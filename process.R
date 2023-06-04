library(tidyverse)
library(stringr)
library(tidytext)

here::here()

load("data.RData")

clean_walls <- clean_walls %>% 
  mutate(token = tolower(token)) %>% 
  filter(!grepl("concept", token)) %>% 
  mutate(token = str_replace_all(token, "wall", ""))


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


library(igraph)

graph_prep <- wall_lv %>% 
  filter(sim != 0) %>% 
  rename(weight = sim) 

wall_graph <- graph_from_data_frame(graph_prep , directed = F)

plot.igraph(wall_graph)


cluster_edge_betweenness(wall_graph, weights = E(wall_graph)$weight, directed = F)

edge.betweenness.community(wall_graph, weights = E(wall_graph)$weight, directed = F) %>%  plot()

# library(ggridges)
# 
# expand_wall %>% 
#   mutate(sim = stringsim(as.character(Var1), as.character(Var2), method = "lv")) %>% 
#   #filter(Var1 == "concrete cast insitu") %>% 
#   ggplot(aes(x = sim, y= Var1)) +
#   geom_density_ridges2()
# 
# expand_wall %>% 
#   mutate(sim = stringsim(as.character(Var1), as.character(Var2), method = "osa")) %>% 
#   group_by(Var1) %>% 
#   filter(sim != 1) %>% 
#   #slice(which.max(sim)) %>% 
#   arrange(-sim) %>% View()
# 
# df_expand <- expand.grid(uniq_token$token,uniq_token$token)
# 
# df_expand %>% 
#   mutate(sim = stringsim(df_expand$Var1,df_expand$Var2, method = "jw")) %>% 
#   group_by(Var1) %>% 
#   filter(sim != 1) %>% 
#   slice(which.max(sim)) %>% 
#   arrange(-sim)
