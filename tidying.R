library(tidyverse)
library(jsonlite)
library(tidyjson)
library(widyr)
library(tidytext)
library(ggridges)
library(lubridate)

options(scipen = 99)

distances <- data_frame(
  d = seq(-0.5, 0.5, length = 10),
  s = exp(-d/0.5)
)

distances %>% 
  ggplot(aes(d, s)) + 
  geom_line()

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

tidy_articles %>%
  group_by(text, source, publish_date) %>%
  nest(url) %>%
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
    text = str_replace(tolower(text), paste0(source, "|story highlights"), "")
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
  ggplot(aes(lubridate::ymd(item1), delta)) + 
  geom_point() + 
  geom_line() + 
  geom_text(aes(label = item1))

## Temporal Distances

add_rowid <- function(x) {
  x %>% mutate(author_id = row_number())
}

author_dates <- tidy_articles %>%
  distinct(text, source, .keep_all = T) %>%
  mutate(length = map_int(authors, length)) %>%
  filter(length < 2) %>%
  unnest(authors) %>%
  select(authors, publish_date, text) %>%
  group_by(authors) %>%
  nest() %>%
  mutate(data = map(data, add_rowid)) %>%
  unnest() %>%
  mutate(author_id = paste(authors, author_id, sep = "_")) %>%
  select(-authors)

temporal_difference <- crossing(
  author1 = author_dates$author_id, 
  author2 = author_dates$author_id
) %>%
  inner_join(author_dates %>% select(-text), by = c(author1 = "author_id")) %>%
  inner_join(author_dates %>% select(-text), by = c(author2 = "author_id")) %>%
  filter(author1 != author2) %>%
  transmute(
    author1, author2,
    temporal_difference = abs(ymd(publish_date.x) - ymd(publish_date.y))
  )

temporal_difference

author_deltas <- author_dates %>%
  select(-publish_date) %>%
  unnest_tokens(word, text) %>%
  count(author_id, word) %>%
  group_by(author_id) %>%
  top_n(50) %>%
  mutate(p_word = n/sum(n)) %>%
  ungroup() %>%
  pairwise_delta(author_id, word, p_word, sparse = T, upper = F)

diff_deltas <- author_deltas %>%
  rename(author1 = item1, author2 = item2) %>%
  inner_join(temporal_difference)

diff_deltas %>%
  mutate(similarity = exp(-delta/0.5)) %>%
  mutate(global_mean = mean(delta)) %>%
  group_by(temporal_difference) %>%
  mutate(
    average_similarity = mean(similarity),
    average_distance = mean(delta)
  ) %>%
  ungroup() %>%
  ggplot(aes(temporal_difference, average_distance, group = 1)) + 
  geom_line() + 
  # geom_hline(aes(yintercept = global_mean), linetype = 2, color = "red") +
  labs(
    x = "Difference in Days of Publishing", 
    y = "Average Delta Distance"
  )
ggsave("temporal_distance.png")






