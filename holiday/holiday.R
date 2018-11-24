library(rvest)
library(tidyverse)
library(widyr)
library(tidytext)

wikipedia <- c(
  "Christmas Under Wraps", "Switched for Christmas", "The Christmas Train", "Journey Back to Christmas", "A Christmas Detour", 
  "Christmas at Graceland", "My Christmas Love", "The Christmas Cottage", "A Royal Christmas", "My Christmas Dream", "A Christmas Wish",
  "Help for the Holidays", "A Very Merry Mix-Up", "Christmas Under Wraps", "A Christmas Detour",
  "Journey Back to Christmas", "Switched for Christmas", "Christmas at Graceland"
)

lifetime <- read_html("https://www.countryliving.com/life/entertainment/a19456390/lifetime-christmas-movies-2018/") %>%
  html_nodes("strong") %>%
  html_text() 

lifetime <- lifetime[nchar(lifetime) > 2]

hallmark <- read_html("https://www.tvguide.com/news/2018-hallmark-christmas-movie-schedule/") %>%
  html_nodes("p>em") %>%
  html_text()

hallmark <- hallmark[nchar(hallmark) > 4]

hallmark_official <- c()
for (i in -10:24) {
  temp <- read_html(paste0("http://www.hallmarkchannel.com/schedule?mode=&day=", i, "&week=0&tz=EST")) %>%
    html_nodes("a.schedule-daily-table-show-details-title") %>%
    html_text()
  
  hallmark_official <- c(
    hallmark_official,
    stringr::str_remove(temp, " - New 2018")
  )
}


movies <- data_frame(movie = c(
    wikipedia,
    lifetime,
    hallmark,
    hallmark_official
  )) %>%
  distinct(movie)

movies <- movies %>%
  unnest_tokens(word2, movie, drop = F) %>%
  group_by(movie) %>%
  mutate(word1 = lag(word2)) %>%
  ungroup() %>%
  replace_na(list(word1 = "_START")) %>%
  # select(movie, word1, word) %>%
  # unite(pair, prev_word, word, sep = "__") %>%
  mutate(n = n()) %>%
  group_by(word1, word2) %>%
  summarise(freq = n() / first(n)) %>%
  ungroup()
  # separate(pair, c("word1", "word2"), sep = "__")

softmax <- function(x) {
  zsum <- sum(x^2, na.rm = T)
  x^2 / zsum
}
  
m <- movies %>%
  group_by(word1) %>%
  mutate(freq = softmax(freq)) %>%
  ungroup() %>%
  spread(word1, freq)

m[is.na(m)] <- 0

prev_word <- "_START"
title <- c()

for (i in 1:4) {
  weight <- m %>%
    pull(!!enquo(prev_word))
  
  prev_word <- m %>%
    sample_n(1, weight = weight) %>%
    pull(word2)
  
  title <- c(title, prev_word)
}  
(title)
