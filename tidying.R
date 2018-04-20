library(tidyverse)
library(jsonlite)
library(tidyjson)
library(widyr)
library(tidytext)
library(ggridges)

news_articles <- fromJSON("data/1996_dicts_9_sources.json", simplifyDataFrame = T)

tidy_articles <- news_articles %>% 
  as.tibble()

tidy_articles

tidy_articles %>%
  select(text, authors) %>%
  unnest(authors) %>%
  count(authors, sort = T)

tidy_articles %>%
  select(text, authors, source) %>%
  unnest(authors) %>%
  count(source, authors) %>%
  ggplot(aes(n, source)) +
  geom_density_ridges() + 
  theme_ridges()

tidy_articles %>%
  count(text, source, publish_date) %>%
  arrange(-n)

test <- tidy_articles %>%
  group_by(text, source, publish_date) %>%
  nest(url)
test %>%
  filter(source == "latimes", str_detect(text, "President Trump signed an executive order Monday")) %>%
  unnest() %>%
  pull(url)

duplicate_authors <- tidy_articles %>%
  unnest(authors) %>%
  group_by(text) %>%
  mutate(auth = n_distinct(authors))
duplicate_authors %>%
  filter(auth < 2)


tidy_articles %>%
  distinct(text, source, .keep_all = T) %>%
  mutate(length = map_int(authors, length)) %>%
  filter(length < 2)
  



