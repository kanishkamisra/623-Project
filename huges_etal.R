library(tidyverse)
library(jsonlite)
library(widyr)
library(tidytext)
library(ggridges)
library(lubridate)
library(rvest)
library(scales)
library(ggrepel)
library(spatialEco)

options(scipen = 99)

source("utils.R")

symmetric_kl <- function(object, eps = 10^-4, overlap = T) {
  if(!is.numeric(object))
    stop("object must be a numeric matrix\n")
  z <- matrix(NA, nrow = ncol(object), ncol = ncol(object))
  colnames(z) <- rownames(z) <- colnames(object)
  w <- object < eps
  if(any(w))
    object[w] <- eps
  object <- sweep(object, 2, colSums(object), "/")
  # object <- object/rowSums(object)
  for (i in seq_len(ncol(object) - 1)) {
    for(j in 2:ncol(object)) {
      ok <- (object[, i] > eps) & (object[, j] > eps)
      if(!overlap | any(ok)) {
        z[i, j] <- 0.5*sum(((object[, i] - object[, j]) * log(object[, i]/object[, j])))
        z[j, i] <- 0.5*sum(((object[, j] - object[, i]) * log(object[, j]/object[, i])))
      }
    }
  }
  diag(z) <- 0
  return(z)
}

news_raw <- fromJSON("data/data2178datesfound.json", simplifyDataFrame = T) %>%
  as.tibble() %>%
  select(text, source, publish_date, authors) %>%
  distinct(text, source, .keep_all = T) %>%
  mutate(length = map_int(authors, length)) %>%
  filter(length < 2) %>%
  unnest(authors) %>%
  mutate(authors = str_replace_all(authors, "\\.|3139", " ")) %>%
  rename(author = authors) %>%
  select(-length) %>%
  filter(author != "editorial board", author != "") %>%
  mutate(
    text = str_replace_all(tolower(text), paste0(source, "|story highlights|washington|'|’"), "")
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
  filter(len >= 6) %>%
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
  mutate(announcement = fct_relevel(str_wrap(announcement, width = 50)))

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

timeline

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
  mutate(avg_similarity = mean(similarity)) %>%
  ungroup() 

jaweed %>%
  ggplot(aes(publish_date, avg_similarity)) +
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
  geom_line(size = 1) +
  geom_rect(data = tb_timeline2, inherit.aes = F, aes(xmin = date1, xmax = date2, ymin = min(auth_avg$avg_similarity), ymax = max(auth_avg$avg_similarity), group = announcement, fill = announcement), color = "transparent", alpha = 0.5) +
  scale_fill_discrete(breaks = tb_timeline2$announcement) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  facet_wrap(~author) + 
  theme_bw(base_size = 14) +
  theme(strip.text = element_text(face = "bold")) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(
    x = "Timeline",
    y = "Average Similarity",
    fill = "Announcement"
  )

ggsave("cosine_authors.png", height = 10, width = 20)

all <- auth_distances %>%
  group_by(publish_date) %>%
  mutate(avg_similarity = mean(similarity)) %>%
  ungroup() 

all %>%
  ggplot(aes(publish_date, avg_similarity)) + 
  geom_line()



## KL

test_m <- timeline %>%
  group_by(article_id) %>%
  unnest_tokens(word, text) %>%
  # ungroup() %>%
  count(word) %>%
  group_by(article_id) %>%
  mutate(sum = sum(n), rel_freq = n/sum(n)) %>%
  ungroup() %>%
  group_by(word) %>%
  mutate(count = sum(n)) %>%
  ungroup() %>%
  filter(count >= quantile(count, 0.8)) %>%
  select(article_id, word, rel_freq) %>%
  spread(word, rel_freq, fill = 0) %>%
  as.data.frame() %>%
  remove_rownames() %>%
  column_to_rownames("article_id") %>%
  as.matrix() 

kl_div <- symmetric_kl(t(test_m), eps = 10^-5)

kl_tbl <- as.data.frame(kl_div) %>%
  rownames_to_column("author1") %>%
  as.tibble() %>%
  gather(`aaron blake_1`:zezimak_2, key = "author2", value = "kl_divergence") %>%
  filter(author1 != author2) %>%
  mutate(
    kl_similarity = exp(-2*kl_divergence)
  )

kl_tbl %>%
  ggplot(aes(kl_similarity)) + 
  # geom_histogram(bins = 50) +
  geom_density() +
  scale_y_log10()

kl_tbl %>%
  write_csv("kl_similarity_articles.csv")

nested_kl <- kl_tbl %>%
  inner_join(timeline %>% select(article_id, announcement, publish_date), by = c(author2 = "article_id")) %>%
  inner_join(timeline %>% select(article_id, announcement1 = announcement), by = c(author1 = "article_id")) %>%
  filter(announcement == announcement1) %>%
  select(-announcement1) %>%
  group_by(author2, announcement) %>%
  nest() %>%
  mutate(significant = map(data, function(x) filter(x, kl_similarity > quantile(kl_similarity, 0.95))))

nested_kl$data[[1]] %>%
  filter(kl_similarity > quantile(kl_similarity, 0.95))

auth_similarities <- nested_kl %>%
  unnest(significant) %>%
  separate(author2, into = c("author", "id"), sep = "_")

auth_similarities %>%
  group_by(author, announcement) %>%
  mutate(avg_similarity = mean(kl_similarity)) %>%
  ungroup() %>%
  filter(author %in% author_filter) %>%
  ggplot(aes(publish_date, avg_similarity)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~author) 

avg_sim <- kl_tbl %>%
  inner_join(timeline %>% select(article_id, announcement, publish_date), by = c(author2 = "article_id")) %>%
  inner_join(timeline %>% select(article_id, announcement1 = announcement), by = c(author1 = "article_id")) %>%
  filter(announcement == announcement1) %>%
  select(-announcement1) %>%
  separate(author2, into = c("author", "id"), sep = "_") %>%
  group_by(author, announcement) %>%
  mutate(avg_similarity = mean(kl_similarity)) %>%
  ungroup() %>%
  filter(author %in% author_filter) %>%
  group_by(author) %>%
  mutate(cnt = n()) %>%
  ungroup()

avg_sim %>%
  mutate(author = reorder(author, -cnt)) %>%
  # ggplot(aes(publish_date, kl_similarity)) + 
  ggplot(aes(publish_date, avg_similarity)) +
  # geom_point() +
  geom_rect(data = tb_timeline2, inherit.aes = F, aes(xmin = date1, xmax = date2, ymin = min(avg_sim$avg_similarity), ymax = 0.5, group = announcement, fill = announcement), color = "transparent", alpha = 0.5) +
  geom_line(size = 1, color = "#2E294E") +
  scale_fill_manual(breaks = tb_timeline2$announcement, values = rep(rcartocolor::carto_pal(12, "Prism"), length = 28)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  facet_wrap(~author) +
  scale_y_continuous(limits = c(0, 0.5), minor_breaks = NULL, labels = scales::percent_format()) +
  theme_bw(base_size = 14) +
  theme(strip.text = element_text(face = "bold")) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(
    x = "Timeline",
    y = "Average Similarity",
    fill = "Announcement"
  )

ggsave("average_similarity.png", height = 10, width = 20)


kl_tbl %>%
  inner_join(timeline %>% select(article_id, announcement, publish_date), by = c(author2 = "article_id")) %>%
  inner_join(timeline %>% select(article_id, announcement1 = announcement), by = c(author1 = "article_id")) %>%
  filter(announcement == announcement1) %>%
  select(-announcement1) %>%
  separate(author2, into = c("author", "id"), sep = "_") %>%
  group_by(announcement) %>%
  mutate(avg_similarity = mean(kl_similarity)) %>%
  ungroup() %>%
  # filter(author %in% author_filter) %>%
  # ggplot(aes(publish_date, kl_similarity)) + 
  ggplot(aes(publish_date, avg_similarity)) +
  # geom_point() +
  geom_rect(data = tb_timeline2, inherit.aes = F, aes(xmin = date1, xmax = date2, ymin = min(avg_sim$avg_similarity), ymax = 0.5, group = announcement, fill = announcement), color = "transparent", alpha = 0.5) +
  geom_line(size = 1) +
  # scale_fill_manual(breaks = tb_timeline2$announcement, values = sample(rep(Redmonder::redmonder.pal(10, "qMSOStd"), length = 28), 28)) +
  scale_fill_manual(breaks = tb_timeline2$announcement, values = rep(rcartocolor::carto_pal(12, "Prism"), length = 28)) +
  # rcartocolor::scale_fill_carto_d() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 0.5), minor_breaks = NULL, labels = scales::percent_format()) +
  theme_bw(base_size = 16) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(
    x = "Timeline",
    y = "Average Similarity",
    fill = "Announcement"
  )

ggsave("avg_avg_similarity.png", height = 10, width = 20)

kl_tbl
  

