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

## We take the top ten authors based on number of articles written. 
## Duplicates and multiple authors are ignored.
nested_articles <- tidy_articles %>%
  distinct(text, source, .keep_all = T) %>%
  mutate(length = map_int(authors, length)) %>%
  filter(length < 2) %>%
  unnest(authors) %>%
  select(-summary, -top_words, -type, -length, -id, -url, -title) %>%
  group_by(authors) %>%
  nest() %>%
  mutate(articles = map_int(data, nrow)) %>%
  top_n(10, articles)

laura <- nested_articles %>%
  filter(authors == "jaweed kaleem") %>% 
  pull(data)

laura <- laura[[1]]

laura_tidy <- laura %>%
  mutate(
    text = str_replace(tolower(text), paste0(source, "|story highlights"), ""),
  ) %>%
  select(-source) %>%
  group_by(publish_date) %>%
  unnest_tokens(word, text) %>%
  count(word) %>%
  ungroup()

laura_tidy %>%
  mutate(
    publish_date = lubridate::ymd(publish_date)
  ) %>%
  pairwise_delta(publish_date, word, n, upper = F) %>%
  mutate(
    difference = abs(item1 - item2)
  ) %>%
  group_by(item1) %>%
  filter(difference == min(difference)) %>%
  arrange(item1) %>%
  ggplot(aes(item2, delta, group = 1)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label = item2))

# date_matrix <- laura %>%
#   mutate(
#     text = str_replace(tolower(text), paste0(source, "|story highlights"), "")
#   ) %>%
#   select(-source) %>%
#   group_by(publish_date) %>%
#   unnest_tokens(word, text) %>%
#   count(word) %>%
#   ungroup() %>%
#   spread(word, n, fill = 0) %>%
#   as.data.frame() %>%
#   remove_rownames() %>%
#   column_to_rownames("publish_date") %>%
#   as.matrix()
# 
# dim(date_matrix)
# 
# rownames(colMeans(date_matrix))

median_vector <- laura_tidy %>%
  spread(word, n, fill = 0) %>%
  gather(word, n, -publish_date) %>%
  group_by(word) %>%
  summarize(
    n = median(n)
  ) %>%
  ungroup() %>%
  mutate(publish_date = "median_vector")

bind_rows(
  median_vector,
  laura_tidy
) %>%
  pairwise_delta(publish_date, word, n) %>%
  filter(item2 == "median_vector") %>%
  # group_by(publish_date) %>%
  # mutate(delta_adjusted = mean(delta)) %>%
  ggplot(aes(lubridate::ymd(item1), delta)) + 
  geom_point() + 
  geom_line() + 
  geom_text(aes(label = item1))



