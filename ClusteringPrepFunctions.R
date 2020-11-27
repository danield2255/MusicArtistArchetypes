library(tidyverse)

popularGenreGetter = function(data){#Pass in billboard data and group across year
  dataSub = data %>%
    separate(Week, sep = "-", into = c("Year", "Month", "Day")) %>%
    select(Genre, Year)
  
  byYear = dataSub %>%
    group_by(Year) %>%
    mutate(genresOnCharts = paste0(Genre, collapse = ",")) %>% 
    select(Year, genresOnCharts) %>%
    distinct() 
  
  byYear$popularGenres = lapply(strsplit(byYear$genresOnCharts, ","), function(x) return(names(table(x)[order(-table(x))][1:3]))) %>% 
    sapply(paste, collapse = ",")
  
  byYear = byYear %>%
    select(Year, popularGenres)
  
  return(byYear)
}