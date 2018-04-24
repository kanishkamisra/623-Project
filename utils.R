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

pairwise_normdelta <- function(tbl, item, feature, value,
                               method = "burrows", ...) {
  pairwise_normdelta_(tbl,
                      col_name(substitute(item)),
                      col_name(substitute(feature)),
                      col_name(substitute(value)),
                      method = method, ...)
}

pairwise_normdelta_ <- function(tbl, item, feature, value, method = "burrows", ...) {
  delta_func <- function(m) {
    
    m <- m/sqrt(rowSums(m^2))
    
    m_scaled <- scale(m)
    
    if(method == "burrows") {
      return(as.matrix(stats::dist(m_scaled, method = "manhattan")/length(m[1,])))
    }
    else if(method == "argamon") {
      return(as.matrix(stats::dist(m_scaled, method = "euclidean")/length(m[1,])))
    }
    else if (method == "cosine") {
      normed <- m_scaled / sqrt(rowSums(m_scaled ^ 2))
      return(1 - normed %*% t(normed))
    }
    else {
      stop("Wrong method! Only method = burrows or method = argamon have been implmented!")
    }
  }
  
  d_func <- squarely_(delta_func, ...)
  
  tbl %>%
    d_func(item, feature, value) %>%
    rename(delta = value)
}