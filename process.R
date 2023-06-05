library(tidyverse)
library(stringr)
library(tidytext)

here::here()

load("data.RData")

clean_walls <- clean_walls %>% 
  mutate(token = tolower(token)) %>% 
  filter(!grepl("concept", token)) %>% 
  mutate(token = str_replace_all(token, "wall", ""))

#### BASE ADIST() METHOD ####

walls_dist <- adist(clean_walls$token)
colnames(walls_dist) <- clean_walls$Type
rownames(walls_dist) <- clean_walls$Type

#write.csv2(x = walls_dist, file = "distance_mat.csv")


wall_clust <- hclust(as.dist(walls_dist), method = "ward.D")

df_clust <- tibble(clean_walls, cutree(wall_clust, h =200)) %>% 
  rename(cluster = `cutree(wall_clust, h = 200)`)

#write.csv2(x = df_clust, file = "clusters.csv")

### PLOT ###
plot(wall_clust, main = NULL, sub = NULL, xlab = NULL, ylab= NULL, lwd = 1)
rect.hclust(wall_clust,k= 6,border = 2:6)

plotly::ggplotly(ggdendro::ggdendrogram(wall_clust))

## CIRCULAR PLOT ##

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggtree")

ggtree::ggtree(wall_clust, layout = 'circular')



library(ape)
library(cluster)
plot(as.phylo(wall_clust), type = "fan")


####STRINGDIST METHOD####
library(stringdist)

expand_wall <- expand.grid(clean_walls$token,clean_walls$token) %>% 
  filter(Var1 != Var2) %>% 
  distinct(Var1, Var2, .keep_all = T)

#The Levenshtein distance
wall_lv <- expand_wall %>% 
  mutate(sim = stringsim(as.character(Var1), as.character(Var2), method = "lv")) 
#The Optimal String Alignment distance
wall_osa <- expand_wall %>% 
  mutate(sim = stringsim(as.character(Var1), as.character(Var2), method = "osa")) 
#The longest common substring
wall_lcs <- expand_wall %>% 
  mutate(sim = stringsim(as.character(Var1), as.character(Var2), method = "lcs")) 
#The Jaro distance
wall_jw <- expand_wall %>% 
  mutate(sim = stringsim(as.character(Var1), as.character(Var2), method = "jw")) 

mat_lv <- wall_lv %>%  
  spread(key = Var1, value = as.numeric(sim)) %>%  
  replace(is.na(.),0) %>%
  as.matrix()

mat_lv <- mat_lv[ ,colnames(mat_lv)!="Var2"]
mat_lv[lower.tri(mat_lv, diag = F)] <- 0
mat_lv <- matrix(as.numeric(mat_lv), ncol(mat_lv))

colnames(mat_lv) <- unique(wall_lv$Var1)
rownames(mat_lv) <- colnames(mat_lv)
heatmap(mat_lv)


#### NETWORK GRAPH ####
library(igraph)

###BY MATRIX###
graph_matlv <- graph.adjacency(mat_lv, mode = "undirected", weighted = T)

#matlv_clust <- cluster_edge_betweenness(graph_matlv, weights = E(graph_matlv)$weight, directed = F)
#saveRDS(matlv_clust, "graph_cluster.RDS")

readRDS("graph_cluster.RDS")

plot(matlv_clust, graph_matlv, 
     layout =  layout.fruchterman.reingold, 
     vertex.size = 0.1,  
     vertex.label.cex = 1, 
     vertex.color = "white", 
     vertex.shape = "none")


###BY DATA FRAME###
graph_prep <- wall_lv %>% 
  filter(sim != 0) %>% 
  rename(weight = sim) 

wall_graph <- graph_from_data_frame(graph_prep , directed = F)

plot.igraph(wall_graph)

wall_clust <- cluster_edge_betweenness(wall_graph, weights = E(wall_graph)$weight, directed = F)

edge.betweenness.community(wall_graph, weights = E(wall_graph)$weight, directed = F) %>%  plot()

