## implmentation of Koppel 2011 paper 'Authorship attribution in the wild'

library(tidyverse)
library(tidytext)
library(widyr)
library(jsonlite)
library(lubridate)
library(rvest)
library(scales)
library(ggrepel)
library(recipes)
library(rsample)

options(scipen = 99)

add_rowid <- function(x) {
  x %>% mutate(article_id = row_number())
}

articles_raw <- fromJSON("data/1996_dicts_9_sources.json", simplifyDataFrame = T) %>%
  as.tibble() %>%
  select(text, source, publish_date, authors) %>%
  distinct(text, source, .keep_all = T) %>%
  mutate(length = map_int(authors, length)) %>%
  filter(length < 2) %>%
  unnest(authors) %>%
  rename(author = authors) %>%
  select(-length) %>%
  filter(author != "editorial board") %>%
  mutate(
    text = str_replace_all(tolower(text), paste0(source, "|story highlights|washington"), "")
  )

## 1405 articles
articles_koppel <- articles_raw %>%
  group_by(author) %>%
  mutate(sources = n_distinct(source)) %>%
  ungroup() %>%
  filter(sources > 1) %>%
  group_by(source) %>%
  mutate(authors = n_distinct(author)) %>%
  ungroup() %>%
  filter(authors > 10)

articles_full <- articles_koppel %>%
  group_by(author) %>%
  nest() %>%
  mutate(
    data = map(data, add_rowid)
  ) %>%
  unnest() %>%
  mutate(article_id = paste(author, article_id, sep = "_")) %>%
  select(-publish_date, -sources, -authors, -author)

set.seed(1234)

articles_split <- initial_split(articles_full, strata = "source")

known <- training(articles_split)
unknown <- testing(articles_split)

known %>%
  group_by(article_id) %>%
  unnest_tokens(shingle, text, "character_shingles", n = 4) %>%
  ungroup() %>%
  distinct(shingle)

unknown %>%
  group_by(article_id) %>%
  unnest_tokens(shingle, text, "character_shingles", n = 4) %>%
  ungroup() %>%
  distinct(shingle)

