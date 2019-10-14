options(knitr.table.format = "html")
library(tidyverse)
library(tidytext)
library(tools)
library(stringr)
library(kableExtra)
library(gridExtra)

riaaSuffixStrip = function(title){
  new = gsub("\\(Feat.*", "", title)
  return(new)
}

artistPunctuationStrip = function(artist){
  new = artist %>%
    as.character(.)%>%
    sub("\\.", "", .) %>%
    sub(":", "", .) 
  return(new)
}

spotifySuffixStrip = function(title){#this needs fix
  new = title %>% 
    sub(" - Remastered", "", .) %>%
    #sub(" - Radio Edit", "", .)%>%
    sub(" - From.*", "", .) %>%
    sub(" - Official.*", "", .) 
  return(new)
}

stringColStandardizer = function(oldText){
  newText = oldText %>%
    as.character(.)%>%
    tolower(.) %>%
    toTitleCase(.) %>%
    trimws(.)
  return(newText)
}



artistDataJoiner = function(artist){
  drops = c("X", "Day", "Artists", "Artist", "Unnamed..0")
  billboardSub = billboardDf[billboardDf$Artists %like% artist, ]
  spotifySub = spotifyDf[spotifyDf$Artist %like% artist,]
  spotifySub = spotifySub[ , !(names(spotifySub)) %in% drops]
  billboardSub = billboardSub[ , !(names(billboardSub)) %in% drops]
  new1 = join_all(list(billboardSub, spotifySub), by = c("Name","Features" ,"Year", "Month", "Week"), type = "full")
  riaaSub = riaaDf[riaaDf$Artist %like% artist,]
  grammySub = grammyDf[grammyDf$Artist %like% artist,]
  songAttrsSub = songAttrsDf[songAttrsDf$Artist %like% artist,] %>%
    mutate(DurationInSecs = Duration/1000) %>%
    select(-c("Duration"))
  
  songSecsSub = songSecsDf[songSecsDf$Artist %like% artist,]
  riaaSub = riaaSub[ , !(names(riaaSub)) %in% drops]
  grammySub =grammySub[ , !(names(grammySub)) %in% drops]
  songAttrsSub = songAttrsSub[ , !(names(songAttrsSub)) %in% drops]
  songSecsSub =songSecsSub[ , !(names(songSecsSub)) %in% drops]
  
  new2 = join_all(list(new1, riaaSub, grammySub, songAttrsSub, songSecsSub), by = "Name", type = "full") #add songSecsSub later
  return(new2)
}


pop1Calc = function(df){
  ret = sum(df$WeeksOnBillboard / df$BillboardWeekRank, na.rm = TRUE)
  return(ret)
}

pop2Calc = function(df){ 
  ret = sum(1/df$BillboardWeekRank, na.rm = TRUE)
  return(ret)
}

pop3Calc = function(df){
  ret =log(101.1 - min(df$PeakPosBillboard, na.rm = TRUE))
  if (is.nan(ret)){
    ret = NA
  }
  return(ret)
}

pop4Calc1 = function(val){
  if (!is.na(val)){
    ret = log(101.1- val)
  }else{
    ret = val
  }
  return(ret)
}

pop4Calc2 = function(df){
  vals = sapply(df$BillboardWeekRank, pop4Calc1)
  return(mean(vals, na.rm = TRUE))
}

getPopularityMetric = function(df){
  new1 = df %>%
    select(Name, Album, BillboardWeekRank, WeeksOnBillboard, PeakPosBillboard) %>%
    distinct() %>%
    group_by(Name, Album) %>%
    do(data.frame( pop1=pop1Calc(.), pop2 = pop2Calc(.), pop3 = pop3Calc(.), pop4 = pop4Calc2(.))) %>%
    distinct()
  
  new2 = df %>%
    select(Name, ReleaseDate) %>%
    distinct()
  
  new = merge(new1, new2, by="Name", type= "full")
  
  new = new[complete.cases(new[,!names(new) %in% c("Album")]), ] %>%
    distinct() %>%
    arrange(desc(ReleaseDate))

  return(new)
}



countNonBandWriters = function(df, bandMembers){
  if (is.na(df$WritingCredits)){
    nonBandWriters = NA
  } else{
    writers = str_split(df$WritingCredits, ", ") %>% 
      unlist()
    nonBandWriters = sum(!writers %in% bandMembers)
  }
  return(nonBandWriters)
}


getOutsideInfluenceScore = function(df, bandMembers){
  new1 = df %>%
    select(Name, WritingCredits, Album) %>%
    distinct() %>%
    group_by(Name, Album) %>%
    do(data.frame(nonBandMemberWriters = countNonBandWriters(., bandMembers))) %>%
    distinct()

  new2 = df %>%
    select(Name, ReleaseDate) %>%
    distinct()
  
  new = merge(new1, new2, by="Name", type= "full")
  
  new = new[complete.cases(new[,!names(new) %in% c("Album")]), ] %>%
    distinct() %>%
    arrange(desc(ReleaseDate))

  return(new)
}


noContraction = function(lyrics){
  new = lyrics %>% 
    gsub("can't", "cannot", .) %>% #special n't
    gsub("couldn't've", "could not have", .) %>%
    gsub("mustn't've", "must not have", .) %>%
    gsub("who'd've", "who would have", .) %>%
    gsub("why'd", "why did", .) %>%
    gsub("n't", " not", .) %>%
    gsub("'ll", " will", .) %>% 
    gsub("'d", " would", .) %>% 
    gsub("n't", " not", .) %>%
    gsub("'ve", " have", .) %>%
    gsub("'re", " are", .) %>%
    gsub("'cause", "because", .) %>%
    gsub("there's", "there is", .) %>% 
    gsub("everyone's", "everyone is", .) %>% 
    gsub("she's", "she is", .) %>%
    gsub("he's", "he is", .) %>%
    gsub("it's", "it is", .) %>%
    gsub("let's", "let us", .) %>%
    gsub("how's", "how is", .) %>%
    gsub("somebody's", "somebody is", .) %>%
    gsub("someone's", "someone is", .) %>%
    gsub("something's", "something is", .) %>%
    gsub("that's", "that is", .) %>%
    gsub("there's", "there is", .) %>%
    gsub("what's", "what is", .) %>%
    gsub("when's", "when is", .) %>%
    gsub("where's", "where is", .) %>%
    gsub("who's", "who is", .) %>%
    gsub("gonna", "going to", .) %>% 
    gsub("gotta", "got to", .) %>% 
    gsub("gimme", "give me", .)%>% 
    gsub("tryna", "trying to", .) %>%
    gsub("i'm'a", "i am about to", .)%>% 
    gsub("i'm", "i am", .) %>%
    gsub("gimme", "give me", .) %>%
    gsub("y'all", "you all", .)
  return(new)
}


individualLyric = function(df){
  newDf = df %>% 
    unnest_tokens(word, Lyrics) %>%
    anti_join(stop_words) %>%
    distinct()
  return(newDf)
}


getLyricalComplexity = function(df, standard){
  lyricsDf = df %>%
    select(Name, Lyrics) %>%
    distinct() %>% #now have the lyrics of all of the songs 
    mutate(Lyrics = tolower(Lyrics)) %>% #Get all lyrics to be lower case
    mutate(Lyrics = noContraction(Lyrics)) %>% #There are now no contractions besides possessives
    #mutate(unlist(str_split(Lyrics, "\n", n = 1))) %>% ##############NEEED TO BE ABLE TO STRIP THE TITLE
    mutate(Lyrics = gsub("[^a-z ]", " ", Lyrics)) #Remove what is not an english letter
  
  individual = individualLyric(lyricsDf)
  
  fullLyrics = individual %>%
    group_by(Name) %>%
    tally(name = "uniqueNonStop") %>%
    distinct()
  
  totalLyrics = lyricsDf %>%
    unnest_tokens(word, Lyrics) %>%
    group_by(Name) %>% 
    tally(name = "totalWords")
  
  
  avgLenAndSyls = individual %>%
    group_by(Name) %>%
    distinct() %>%
    mutate(wordLength = nchar(word))%>%
    mutate(wordSyllables = nsyllable(word, syllable_dictionary = quanteda::data_int_syllables, use.names = FALSE)) %>%
    select(Name, wordLength, wordSyllables) %>%
    ddply(.,~Name, summarize, avgWordLen = mean(wordLength, na.rm = TRUE), avgSyllables = mean(wordSyllables, na.rm = TRUE))
  
  
  #Get the total words by the duration 
  wordsByTime = df %>%
    select(Name, DurationInSecs) %>%
    distinct() %>%
    full_join(totalLyrics, by = "Name") %>%
    mutate(wordsPerSec = totalWords/DurationInSecs) %>%
    select(Name, wordsPerSec)

  new3 = join_all(list(fullLyrics, totalLyrics, avgLenAndSyls, wordsByTime), by = "Name")
  
  new3$UniqueToTotalRatio = new3$uniqueNonStop/new3$totalWords
  full = new3[complete.cases(c(new3$UniqueToTotalRatio, new3$avgWordLen, new3$avgSyllables)), ] 
  if (standard){
    full[c(2,3,4,5,6,7)] = sapply(full[c( 2,3,4,5,6,7)],scale)
    full$wordsPerSec[is.na(full$wordsPerSec)] = 0
  }else{
    full$wordsPerSec[is.na(full$wordsPerSec)] = mean(full$wordsPerSec, na.rm = TRUE)
  }

  scores = full %>%
    do(data.frame(Name = full$Name, lyricalComplexity = 1.5 *full$avgWordLen +full$avgSyllables + 2* full$UniqueToTotalRatio+ full$wordsPerSec))
  
  datesDf = df %>%
    select(Name, ReleaseDate, Album) %>%
    distinct()
  
  final = merge(scores, datesDf, by="Name", type= "full")
  
  final = final[complete.cases(final[,!names(final) %in% c("Album")]), ] %>%
    distinct() %>%
    arrange(desc(ReleaseDate))
  return(final)
}


countUniqueChords = function(df){
  uniqueChords = unique(unlist(strsplit(paste(paste("-", unlist(df["Progression"]), sep = ""), collapse = ""), "-"))[-1])
  numUniqueChords = length(uniqueChords[uniqueChords != "NA"])
  return(numUniqueChords)
}

checkDifferentEnd = function(df){
  vals = df$EndDifferent
  endDif = sum(!is.na(vals) & vals != "")
  return (endDif)
}


getMusicComplexity = function(df, standard){
  new1 = df %>%
    select(Name, Section, Progression,EndDifferent, DurationInSecs, NumSectionChords, nonDiatonicChords, extendedChords) %>%
    distinct() 
  
  new2 = new1 %>%
    select(Name, nonDiatonicChords, extendedChords) %>%
    group_by(Name) %>%
    summarise_all(sum)
  
  new3 = df %>%
    group_by(Name) %>%
    do(data.frame(numUniqueChords = countUniqueChords(.)))
  
  new4 = new1 %>% 
    group_by(Name) %>%
    do(data.frame(endDif = checkDifferentEnd(.)))
  
  full = join_all(list(new2, new3, new4), by = "Name", type = "full")
  full = full[complete.cases(full), ]
  if (standard){
    full[c(2,3,4,5)] = sapply(full[c( 2,3,4,5)],scale)
  }
  
  scores = full %>%
    do(data.frame(Name = full$Name , musicalComplexity = 2 * (full$nonDiatonicChords) + (full$extendedChords) + 2* (full$numUniqueChords) + (full$endDif)))
  
  datesDf = df %>%
    select(Name, ReleaseDate, Album) %>%
    distinct()
  
  final = merge(scores, datesDf, by="Name", type= "full")
  
  final = final[complete.cases(final[,!names(final) %in% c("Album")]), ] %>%
    distinct() %>%
    arrange(desc(ReleaseDate))
  return(final)
}


fullMetricsDataSet = function(popScores, origScores, lyricComp, musicComp, standard){
  full = join_all(list(as.data.frame(popScores), as.data.frame(origScores), as.data.frame(lyricComp), as.data.frame(musicComp)), by = c("Name", "Album"), type = "full")
  if (standard){
    full[c(3,4,5,6,9,10)] = sapply(full[c(3,4,5,6,9,10)],scale) 
    #Fill musical complexity NAs with 0 because it is standardized
    full$musicalComplexity[is.na(full$musicalComplexity)] = 0
  } else{
    full$musicalComplexity[is.na(full$musicalComplexity)] = max(c(1, mean(full$musicalComplexity, na.rm=TRUE)), na.rm = TRUE)
  }
  full$totalComplexity = full$lyricalComplexity + full$musicalComplexity
  return(full)
}

compareTracks = function(songs, artistDf, metricDf){
  songs = lapply(songs, stringColStandardizer)
  artistDfSub = artistDf[artistDf$Name %in% songs, ] %>%
    select(Name, BillboardWeekRank, WeeksOnBillboard, Year, Month, Week, ReleaseDate, Genre, Features, Songwriter, numWriters, numArtists, Streams, RiaaStatus, Label, GrammyAward, GrammyYear, Explicit, Album, Acousticness, Danceability, Energy, Instrumentalness, Liveness, Loudness, Mode, Popularity, Speechiness, Tempo, TimeSignature, Valence, DurationInSecs) %>%
    distinct()
  
  full = merge(artistDfSub, metricDf, by = c("Name","ReleaseDate"), type = "full")
  full = subset(full, !is.na(full[,3]))
  
  #Here is the weekly contribution to the pop1 score that was calculated earlier.
  #Shows how the contribution to this metric changes over time of a song on the chart
  full$pop1SWeekScore = full$WeeksOnBillboard / full$BillboardWeekRank
  song1 = paste(songs[-length(songs)], collapse = ", ")
  song2 = tail(songs, n = 1)[[1]]
  
  graphic1 = ggplot(full, aes(x=WeeksOnBillboard, y = pop1SWeekScore, color = Name)) +
    geom_point() + geom_line() + labs(title = substitute(paste("Weekly Contribution to Pop1 Score On Billboard Hot 100 from " ,song1, " and ", song2)))
  graphic2 = ggplot(full, aes(x=WeeksOnBillboard, y = BillboardWeekRank, color = Name)) +
    geom_point() + geom_line() + labs(title = substitute(paste("Rank On Billboard Hot 100 for " ,song1 , " and ",song2, " by Week"))) + scale_y_reverse()
  grid.arrange(graphic1, graphic2, ncol = 1, nrow = 2)
  
  
  #Creation of tables to show on kable
  outsideInfTableDf = full %>%
    select(Name, ReleaseDate, Label, nonBandMemberWriters) %>%
    distinct() %>%
    arrange(desc(nonBandMemberWriters))
  
  popTableDf = full %>%
    select(Name, ReleaseDate, pop1, pop2, pop3, pop4, GrammyAward, RiaaStatus) %>%
    distinct() %>%
    arrange(desc(pop1), desc(pop2), desc(pop3), desc(pop4))
  
  complexityTableDf = full %>%
    select(Name, ReleaseDate, totalComplexity) %>%
    distinct() %>%
    arrange(desc(totalComplexity))
  return(list(outsideInfTableDf, popTableDf, complexityTableDf))
}




completeArchDf = function(artist, members, validAlbs, excludeSongs, standard){
  artistDf = artistDataJoiner(artist) %>% 
    filter(!is.na(BillboardWeekRank) & Album %in% c(validAlbs, NA) & !Name %in% excludeSongs)
  archArtistPop = getPopularityMetric(artistDf)
  archArtistInfluence = getOutsideInfluenceScore(artistDf, members)
  lyricalComplexDf = getLyricalComplexity(artistDf, standard)
  musicComplexDf = getMusicComplexity(artistDf, standard)
  artistMetricDf = fullMetricsDataSet(archArtistPop, archArtistInfluence, lyricalComplexDf, musicComplexDf, standard)
  relDateDf = artistDf %>% select(Name, ReleaseDate, Label)
  fullMetric = merge(relDateDf, artistMetricDf, by =c("Name", "ReleaseDate"))
  fullMetric = fullMetric %>% 
    filter(complete.cases(.[,!names(.) %in% c("nonBandMemberWriters", "Label", "Album")])) %>%
    distinct()
  return (fullMetric)
}

singleArtistVisual = function(artist, fullMetric){
  complexGraph = ggplot(fullMetric, aes(x = ReleaseDate, y = totalComplexity)) + geom_point() + geom_smooth(method = "lm", ) + labs(y = "Standardized Song Complexity", x = "Release Date")
  popGraph = ggplot(fullMetric, aes(x = ReleaseDate, y = pop1)) + geom_point() + geom_smooth(method = "lm", ) + labs(y = "Standardized Pop Score", x = "Release Date")
  infGraph = ggplot(fullMetric, aes(x = ReleaseDate, y = nonBandMemberWriters)) + geom_point() + geom_smooth(method = "lm", ) + labs(y = "Number of Non-Artist Writers", x = "Release Date")
  
  grid.arrange(complexGraph, popGraph, infGraph, ncol = 2, nrow = 2, top = paste(artist, "'s Song Popularity, Complexity, and Outside Influence Over Time"))
  summary(lm(fullMetric$totalComplexity~fullMetric$ReleaseDate))
  
}


artistCompare = function(artistDfs){
  fullDf = bind_rows(artistDfs)
  artists = unique(fullDf$Artist)
  artist1 = paste(artists[-length(artists)], collapse = ", ")
  artist2 = tail(artists, n= 1)[[1]]
  #Visualize
  popPlot = ggplot(fullDf, aes(x = ReleaseDate, y = pop1, color = Artist, shape = Artist)) + geom_smooth(method = "lm",se = FALSE)+ geom_point(alpha= 0.5) + labs(x = "Release Date", y = "Popularity Metric (AU)") + theme(axis.title =element_text(size=9))
  compPlot= ggplot(fullDf, aes(x = ReleaseDate, y = lyricalComplexity, color = Artist, shape = Artist)) + geom_smooth(method = "lm", se = FALSE)+ geom_point(alpha= 0.5) + labs(x= "Release Date",y = "Lyrical Complexity Metric (AU)")+ theme(axis.title =element_text(size=9))
  infPlot = ggplot(fullDf, aes(x = ReleaseDate, y = nonBandMemberWriters, color = Artist, shape = Artist)) + geom_smooth(method = "lm", se = FALSE)+ geom_point(alpha= 0.5) + labs(x= "Release Date",y = "Number of Outside Writers") + theme(axis.title =element_text(size=9))
  grid.arrange(popPlot, compPlot, infPlot, ncol = 2, nrow = 2, top = textGrob(paste("Popularity, Lyrical Complexity, and Outside Influence of ", artist1, " and ", artist2, " Songs Over Time"), gp = gpar(fontsize = 10)))
  
  #Statistical Comparison
  linear = lm(pop1 ~ Artist/ReleaseDate-1, data = fullDf)
  summary(linear)
  
}
