library(tidyverse)

#retrieve the most popular genres according to the billboard data for each year 
popularGenreGetter = function(data){#Pass in billboard data 
  dataSub = data %>%
    separate(Week, sep = "-", into = c("Year", "Month", "Day")) %>%
    select(Genre, Year)
  
  #group across year 
  byYear = dataSub %>%
    group_by(Year) %>%
    mutate(genresOnCharts = paste0(Genre, collapse = ",")) %>% 
    select(Year, genresOnCharts) %>%
    distinct() 
  
  #Get the top 3 most frequently noted genres 
  byYear$popularGenres = lapply(strsplit(byYear$genresOnCharts, ","), function(x) return(names(table(x)[order(-table(x))][1:3]))) %>% 
    sapply(paste, collapse = ",")
  
  
  byYear = byYear %>%
    select(Year, popularGenres)
  
  return(byYear)
}