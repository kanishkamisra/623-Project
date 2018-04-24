library(tidyverse)
library(jsonlite)
library(tidyjson)
library(widyr)
library(tidytext)
library(ggridges)
library(lubridate)
library(rvest)
library(scales)
library(ggrepel)

options(scipen = 99)

source("utils.R")

news_raw <- fromJSON("data/1996_dicts_9_sources.json", simplifyDataFrame = T) %>%
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

news_articles <- news_raw %>%
  group_by(author) %>%
  nest() %>%
  mutate(
    data = map(data, add_rowid),
    len = map_int(data, nrow)
  ) %>%
  unnest() %>%
  # filter(len >= 7) %>%
  unite(article_id, author, article_id, sep = "_") %>%
  mutate(publish_date = ymd(publish_date))

author_filter <- news_raw %>%
  group_by(author) %>%
  nest() %>%
  mutate(
    data = map(data, add_rowid),
    len = map_int(data, nrow)
  ) %>%
  unnest() %>%
  filter(len >= 7) %>%
  distinct(author) %>%
  pull(author)
## Get timeline of travel ban
fox_url <- "http://www.foxnews.com/politics/2018/01/20/trump-travel-ban-timeline-legal-journey.html"

tb_timeline <- fox_url %>%
  read_html() %>%
  html_nodes("#wrapper > div > div.page-content > div > main > article > div > div > div.article-body > h3") %>%
  html_text() %>%
  as.tibble() %>%
  mutate(value = gsub("\u00A0", " ", value)) %>%
  separate(value, into = c("date", "announcement"), sep = " – ") %>%
  mutate(
    date = case_when(
      date == "Jan. 19" ~ paste(date, "2018", sep = " "),
      TRUE ~ paste(date, "2017", " ")
    ), 
    date = mdy(date)
  ) %>%
  arrange(date)

tb_timeline2 <- tibble(
  date1 = tb_timeline$date[1:28],
  date2 = tb_timeline$date[2:29]
) %>%
  filter(date1 < ymd("2017-10-15")) %>%
  inner_join(tb_timeline, by = c(date1 = "date")) %>%
  mutate(announcement = fct_relevel(announcement))

tb_timeline3 <- tb_timeline2 %>%
  mutate(
    dates_between = map2(date1, date2, function(x, y) {
      as.character(seq(to = ymd(x), from = ymd(y) - days(1), length = abs(x - y)))
    })
  ) %>%
  unnest() %>%
  mutate(
    dates_between = ymd(dates_between)
  )

timeline <- news_articles %>%
  inner_join(
    tb_timeline3 %>% 
      select(announcement, publish_date = dates_between)
  )

auth_distances <- timeline %>%
  group_by(article_id) %>%
  unnest_tokens(word, text) %>%
  # ungroup() %>%
  count(word) %>%
  group_by(article_id) %>%
  mutate(rel_freq = n/sum(n)) %>%
  ungroup() %>%
  group_by(word) %>%
  mutate(count = sum(n)) %>%
  ungroup() %>%
  filter(count >= quantile(count, 0.9)) %>%
  pairwise_similarity(article_id, word, n) %>%
  inner_join(timeline %>% select(article_id, announcement, publish_date), by = c(item1 = "article_id")) %>%
  inner_join(timeline %>% select(article_id, announcement1 = announcement), by = c(item2 = "article_id")) %>%
  filter(announcement == announcement1) %>%
  select(-announcement1) %>%
  group_by(item1, announcement, publish_date) %>%
  filter(similarity > quantile(similarity, 0.95)) %>%
  ungroup() %>%
  separate(item1, into = c("author", "id"), sep = "_")
  




jaweed <- auth_distances %>%
  filter(author == "jaweed kaleem") %>%
  group_by(announcement) %>%
  mutate(avg_delta = mean(delta)) %>%
  ungroup() 

jaweed %>%
  ggplot(aes(publish_date, avg_delta)) +
  geom_line() +
  geom_rect(data = tb_timeline2, inherit.aes = F, aes(xmin = date1, xmax = date2, ymin = min(jaweed$avg_delta), ymax = max(jaweed$avg_delta), group = announcement, fill = announcement), color = "transparent", alpha = 0.5) +
  theme_minimal()


auth_avg <- auth_distances %>%
  group_by(author, announcement) %>%
  mutate(avg_similarity = mean(similarity)) %>%
  ungroup() %>%
  filter(author %in% author_filter)

auth_avg %>%
  ggplot(aes(publish_date, avg_similarity, group = 1)) + 
  geom_line() + 
  facet_wrap(~author)


all <- auth_distances %>%
  group_by(publish_date) %>%
  mutate(avg_similarity = mean(similarity)) %>%
  ungroup() 

all %>%
  ggplot(aes(publish_date, avg_similarity)) + 
  geom_line()
