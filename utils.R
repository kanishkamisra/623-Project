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

pairwise_temp <- function(tbl, item, feature, value,
                          method = "euclidean", ...) {
  pairwise_dist_(tbl,
                 col_name(substitute(item)),
                 col_name(substitute(feature)),
                 col_name(substitute(value)),
                 method = method, ...)
}

pairwise_temp_ <- function(tbl, item, feature, value) {
  d_func <- squarely_(function(m) ))
  
  tbl %>%
    d_func(item, feature, value) %>%
    rename(distance = value)
}