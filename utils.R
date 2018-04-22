library(widyr)
library(lubridate)
library(tidyverse)

col_name <- function(x, default = stop("Please supply column name", call. = FALSE))
{
  if (is.character(x))
    return(x)
  if (identical(x, quote(expr = )))
    return(default)
  if (is.name(x))
    return(as.character(x))
  if (is.null(x))
    return(x)
  stop("Invalid column specification", call. = FALSE)
}

multi_scale <- function(tbl, item1, item2, value, k = 2) {
  multi_scale_(tbl,
               col_name(substitute(item1)),
               col_name(substitute(item2)),
               col_name(substitute(value)),
               k = 2)
}


multi_scale_ <- function(tbl, item1, item2, value, k = 2) {
  tbl_matrix <- tbl %>%
    spread(item2, col_name(value), fill = 0) %>%
    as.data.frame() %>%
    remove_rownames() %>%
    column_to_rownames("item1") %>%
    as.matrix()
  
  cmdscale(tbl_matrix, k = k) %>%
    as.data.frame() %>%
    rownames_to_column("item") %>%
    as.tibble()
}

add_rowid <- function(x) {
  x %>% mutate(article_id = row_number())
}
