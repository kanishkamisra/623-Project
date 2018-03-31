library(tidyverse)
library(jsonlite)
library(tidyjson)
library(widyr)
library(tidytext)

news_articles <- fromJSON("data/1996_dicts_9_sources.json", simplifyDataFrame = T)

tidy_articles <- news_articles %>% 
  as.tibble()

tidy_articles %>%
  select(text, authors) %>%
  unnest(authors) %>%
  count(authors, sort = T)

source_distances <- tidy_articles %>%
  filter(!(source %in% c("wsj", "washingtonpost", "nypost", "bostonglobe"))) %>%
  select(text, source) %>%
  unnest_tokens(word, text) %>%
  count(source, word) %>%
  pairwise_dist(source, word, n) %>%
  multi_scale(item1, item2, distance)

source_distances %>%
  ggplot(aes(V1, V2, color = item)) +
  geom_point() +
  scale_x_continuous(limits = c(-15000, 15000)) +
  scale_y_continuous(limits = c(-15000, 15000)) 



tidy_articles %>%
  filter(!(source %in% c("wsj", "washingtonpost", "nypost", "bostonglobe"))) %>%
  mutate(news_id = row_number()) %>%
  select(news_id, text) %>%
  unnest_tokens(word, text) %>%
  count(news_id, word) %>%
  bind_tf_idf(word, news_id, n) %>%
  pairwise_dist(news_id, word, tf_idf, sparse = T)
  
