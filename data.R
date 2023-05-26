library(tidyverse)
library(stringr)
library(tidytext)

here::here()

files <- list.files(pattern = ".txt")

df_all <- files %>% 
  map_dfr(read.csv, skip = 2, sep = '\t') 

uniq_wall <- df_all %>% 
  filter(Category == "Walls") %>% 
  distinct(Type)

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


library(stringdist)

expand_wall <- expand.grid(uniq_wall$Type,uniq_wall$Type) 

expand_wall %>% 
  mutate(sim = stringsim(as.character(Var1), as.character(Var2), method = "osa")) %>% 
  group_by(Var1) %>% 
  filter(sim != 1) %>% 
  #slice(which.max(sim)) %>% 
  arrange(-sim) %>% View()

df_expand <- expand.grid(uniq_token$token,uniq_token$token)

df_expand %>% 
  mutate(sim = stringsim(df_expand$Var1,df_expand$Var2, method = "jw")) %>% 
  group_by(Var1) %>% 
  filter(sim != 1) %>% 
  slice(which.max(sim)) %>% 
  arrange(-sim) %>% View()
