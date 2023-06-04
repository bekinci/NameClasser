library(tidyverse)
library(stringr)
library(tidytext)

here::here()

files <- list.files(path = "data/",pattern = ".txt")
files <- paste0("data/", files)


df_all <- files %>% 
  map_dfr(read.csv, skip = 2, sep = '\t') 

uniq_wall <- df_all %>% 
  filter(Category == "Walls") %>% 
  distinct(Type) 

clean_walls <- uniq_wall %>% 
  mutate(token = str_replace_all(Type, "_"," ")) %>% 
  mutate(token = str_remove_all(token, "[0-9]+mm")) %>%  
  mutate(token = str_remove_all(token, "[0-9]+MM")) %>% 
  mutate(token = trimws(token, "both")) 


df_token <- uniq_wall %>% 
  mutate(word = str_replace_all(Type, "[[:punct:]]", " ")) %>% 
  mutate(word = str_replace_all(word, "[[:digit:]]", " ")) %>% 
  mutate(word = str_remove_all(word, "_")) %>% 
  mutate(word = str_remove_all(word, "mm")) %>% 
  unnest_tokens(token, word, drop = F) %>% 
  filter(nchar(token) > 2) %>% 
  select(Type, token)

uniq_token <- df_token %>% 
  distinct(token)

save.image(file = "data.RData")