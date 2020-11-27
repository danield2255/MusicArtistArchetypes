library(tidyverse)

genres <- c("alternative metal,christian rock,new metal,rock")

genres %>%
  str_split("(,|\\s)") %>%
  unlist() %>%
  table()


top_genres <- c("metal", "rock")

genres %>%
  str_detect(top_genres)

find_top_genre <- function(album_genres, top_genres) {
  
  counts <- str_count(album_genres, top_genres)
  
  top_genres[which.max(counts)]
  
  
}




dat <- tibble(
  album = "Whatever",
  genres = genres,
  Genre.1 = NA,
  Genre.2 = NA,
  Genre.3 = NA,
)

dat %>%
  mutate(
    Genre.1 = find_top_genre(genres, top_genres)
  )
