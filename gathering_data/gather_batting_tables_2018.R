rm(list=ls())
library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(data.table)
library(plyr)


##Standard Batting##

standard_batting_html <- GET("https://www.baseball-reference.com/leagues/MLB/2018-standard-batting.shtml") %>% read_html()
standard_batting <- GET("https://www.baseball-reference.com/leagues/MLB/2018-standard-batting.shtml") 

standard_batting_teams <- read_html(standard_batting) %>% 
  html_nodes("table") %>%
  html_table %>%
  rbindlist() %>%
  filter(!Tm %in% "Tm")


# from https://stackoverflow.com/questions/43476819/not-able-to-scrape-a-second-table-within-a-page-using-rvest

standard_batting_players <- xml2::xml_find_all(standard_batting_html,"//comment()") %>% {
  raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
  strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                vectorize_all = FALSE)
  lapply(grep("<table", strip_html, value = TRUE), function(i){
    rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
      .[[1]]
  })
} %>% rbindlist() %>%
  filter(!Name %in% "Name")

rm(standard_batting, standard_batting_html)


##Player Value Batting##

player_value_batting_html <- GET("https://www.baseball-reference.com/leagues/MLB/2018-value-batting.shtml") %>% read_html()
player_value_batting <- GET("https://www.baseball-reference.com/leagues/MLB/2018-value-batting.shtml") 

player_value_batting_teams <- read_html(player_value_batting) %>% 
  html_nodes("table") %>%
  html_table %>%
  rbindlist() %>%
  filter(!Tm %in% "Tm")

player_value_batting_players <- xml2::xml_find_all(player_value_batting_html,"//comment()") %>% {
  raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
  strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                vectorize_all = FALSE)
  lapply(grep("<table", strip_html, value = TRUE), function(i){
    rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
      .[[1]]
  })
} %>% rbindlist() %>%
  filter(!Name %in% "Name")

rm(player_value_batting, player_value_batting_html)

batting_teams <- left_join(standard_batting_teams, select(player_value_batting_teams, -G), by =c("Tm", "PA"))

batting_players <- left_join(standard_batting_players, select(player_value_batting_players, -Rk, -Tm, -G, -PA, -starts_with("Pos")), by = c("Name", "Age"))

rm(standard_batting_teams, player_value_batting_teams, standard_batting_players, player_value_batting_players)

##Advanced Batting##

advanced_batting_html <- GET("https://www.baseball-reference.com/leagues/MLB/2018-advanced-batting.shtml") %>% read_html()
advanced_batting <- GET("https://www.baseball-reference.com/leagues/MLB/2018-advanced-batting.shtml") 

advanced_batting_teams <- read_html(advanced_batting) %>% 
  html_nodes("table") %>%
  html_table %>%
  rbindlist() %>%
  filter(!Tm %in% "Tm")

advanced_batting_players <- xml2::xml_find_all(advanced_batting_html,"//comment()") %>% {
  raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
  strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                vectorize_all = FALSE)
  lapply(grep("<table", strip_html, value = TRUE), function(i){
    rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
      .[[1]]
  })
} %>% rbindlist() %>%
  filter(!Name %in% "Name")

rm(advanced_batting, advanced_batting_html)

batting_teams <- left_join(batting_teams, advanced_batting_teams)

batting_players <- left_join(batting_players, advanced_batting_players)

rm(advanced_batting_teams, advanced_batting_players)


##Win Probability Batting##

win_probability_batting_html <- GET("https://www.baseball-reference.com/leagues/MLB/2018-win_probability-batting.shtml") %>% read_html()
win_probability_batting <- GET("https://www.baseball-reference.com/leagues/MLB/2018-win_probability-batting.shtml") 

win_probability_batting_teams <- read_html(win_probability_batting) %>% 
  html_nodes("table") %>%
  html_table %>%
  rbindlist() %>%
  filter(!Tm %in% "Tm")

win_probability_batting_players <- xml2::xml_find_all(win_probability_batting_html,"//comment()") %>% {
  raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
  strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                vectorize_all = FALSE)
  lapply(grep("<table", strip_html, value = TRUE), function(i){
    rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
      .[[1]]
  })
} %>% rbindlist() %>%
  filter(!Name %in% "Name")

rm(win_probability_batting, win_probability_batting_html)

#PHAB = Pinch Hit At Bats
batting_teams <- left_join(batting_teams, select(win_probability_batting_teams, -`R/G`, PHAB = AB, -BtRuns, -BtWins))

batting_players <- left_join(batting_players, select(win_probability_batting_players, -BtRuns, -BtWins, -PA, -Rk, PHAB = AB))

rm(win_probability_batting_teams, win_probability_batting_players)

##Batting Ratios##

ratios_batting_html <- GET("https://www.baseball-reference.com/leagues/MLB/2018-ratio-batting.shtml") %>% read_html()
ratios_batting <- GET("https://www.baseball-reference.com/leagues/MLB/2018-ratio-batting.shtml") 

ratios_batting_teams <- read_html(ratios_batting) %>% 
  html_nodes("table") %>%
  html_table %>%
  rbindlist() %>%
  filter(!Tm %in% "Tm")

#delete % signs from cells
ratios_batting_teams[,2:length(colnames(ratios_batting_teams))] <- data.table(
  sapply(select(ratios_batting_teams, -Tm), function(x) as.numeric(gsub("%", "", x))))


ratios_batting_players <- xml2::xml_find_all(ratios_batting_html,"//comment()") %>% {
  raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
  strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                vectorize_all = FALSE)
  lapply(grep("<table", strip_html, value = TRUE), function(i){
    rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
      .[[1]]
  })
} %>% rbindlist() %>%
  filter(!Name %in% "Name")

ratios_batting_players[,6:length(colnames(ratios_batting_players))] <- data.table(
  sapply(select(ratios_batting_players, -Rk, -Name, -Age, -PA, -Tm), function(x) as.numeric(gsub("%", "", x))))

rm(ratios_batting, ratios_batting_html)


batting_teams <- left_join(batting_teams, select(ratios_batting_teams, -`R/G`), by = "Tm")

batting_players <- left_join(batting_players, select(ratios_batting_players, -Rk, -PA), by = c("Name", "Age", "Tm"))

rm(ratios_batting_teams, ratios_batting_players)

##Baserunning / Misc##

baserunning_batting_html <- GET("https://www.baseball-reference.com/leagues/MLB/2018-baserunning-batting.shtml") %>% read_html()
baserunning_batting <- GET("https://www.baseball-reference.com/leagues/MLB/2018-baserunning-batting.shtml") 

baserunning_batting_teams <- read_html(baserunning_batting) %>% 
  html_nodes("table") %>%
  html_table %>%
  rbindlist() %>%
  filter(!Tm %in% "Tm")

#delete % signs from cells
baserunning_batting_teams[,2:length(colnames(baserunning_batting_teams))] <- data.table(
  sapply(select(baserunning_batting_teams, -Tm), function(x) as.numeric(gsub("%", "", x))))


baserunning_batting_players <- xml2::xml_find_all(baserunning_batting_html,"//comment()") %>% {
  raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
  strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                vectorize_all = FALSE)
  lapply(grep("<table", strip_html, value = TRUE), function(i){
    rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
      .[[1]]
  })
} %>% rbindlist() %>%
  filter(!Name %in% "Name")

#delete % signs from cells
baserunning_batting_players[,6:length(colnames(baserunning_batting_players))] <- data.table(
  sapply(select(baserunning_batting_players, -Rk, -Name, -Age, -PA, -Tm), function(x) as.numeric(gsub("%", "", x))))

rm(baserunning_batting, baserunning_batting_html)


batting_teams <- left_join(batting_teams, select(baserunning_batting_teams, -SB, -CS, -`R/G`))

batting_players <- left_join(batting_players, select(
  baserunning_batting_players, -Rk, -SB, -CS, -PA), by = c("Name", "Age", "Tm"))

rm(baserunning_batting_teams, baserunning_batting_players)



##Pitches##

pitches_batting_html <- GET("https://www.baseball-reference.com/leagues/MLB/2018-pitches-batting.shtml") %>% read_html()
pitches_batting <- GET("https://www.baseball-reference.com/leagues/MLB/2018-pitches-batting.shtml") 

pitches_batting_teams <- read_html(pitches_batting) %>% 
  html_nodes("table") %>%
  html_table %>%
  rbindlist() %>%
  filter(!Tm %in% "Tm")

#delete % signs from cells
pitches_batting_teams[,2:length(colnames(pitches_batting_teams))] <- data.table(
  sapply(select(pitches_batting_teams, -Tm), function(x) as.numeric(gsub("%", "", x))))


pitches_batting_players <- xml2::xml_find_all(pitches_batting_html,"//comment()") %>% {
  raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
  strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                vectorize_all = FALSE)
  lapply(grep("<table", strip_html, value = TRUE), function(i){
    rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
      .[[1]]
  })
} %>% rbindlist() %>%
  filter(!Name %in% "Name")

#delete % signs from cells
pitches_batting_players[,6:length(colnames(pitches_batting_players))] <- data.table(
  sapply(select(pitches_batting_players, -Rk, -Name, -Age, -PA, -Tm), function(x) as.numeric(gsub("%", "", x))))

rm(pitches_batting, pitches_batting_html)


batting_teams <- left_join(batting_teams, select(pitches_batting_teams, -`R/G`, -PA), by = "Tm")

batting_players <- left_join(batting_players, select(
  pitches_batting_players, -Rk, -PA), by = c("Name", "Age", "Tm"))

rm(pitches_batting_teams, pitches_batting_players)

##Make numeric columns##

character_cols_players <- sapply(batting_players, is.character)
character_cols_players[c("Rk", "Name", "Tm", "Lg", "Acquired", "Salary", "Pos Summary")] <- FALSE
batting_players[ ,character_cols_players] <- sapply(batting_players[ ,character_cols_players], as.numeric)
batting_players$Salary <- gsub("\\$", "", batting_players$Salary)
batting_players$Salary <- as.numeric(gsub(",", "", batting_players$Salary))

character_cols_teams <- sapply(batting_teams, is.character)
character_cols_teams[c("Tm", "Salary")] <- FALSE

batting_teams[ ,character_cols_teams] <- sapply(batting_teams[ ,character_cols_teams], as.numeric)
batting_teams$Salary <- gsub("\\$", "", batting_teams$Salary)
batting_teams$Salary <- as.numeric(gsub(",", "", batting_teams$Salary))

rm(character_cols_players, character_cols_teams)

##Make singles Stat##

batting_players <- mutate(batting_players, `1B` = H - `2B` - `3B` - HR)

batting_teams <- mutate(batting_teams, `1B` = H - `2B` - `3B` - HR)

##Make UBB stat##

batting_players <- mutate(batting_players, UBB = BB - IBB)

batting_teams <- mutate(batting_teams, UBB = BB - IBB)


batting_teams <- as.data.table(batting_teams)

batting_players <- as.data.table(batting_players)

##Rename Team Names##

team_labels <- c("ARI" = "Arizona Diamondbacks",
                 "ATL" = "Atlanta Braves",
                 "BAL" = "Baltimore Orioles",
                 "BOS" = "Boston Red Sox",
                 "CHC" = "Chicago Cubs",
                 "CHW" = "Chicago White Sox",
                 "CIN" = "Cincinnati Reds",
                 "CLE" = "Cleveland Indians",
                 "COL" = "Colorado Rockies",
                 "DET" = "Detroit Tigers",
                 "HOU" = "Houston Astros", 
                 "KCR" = "Kansas City Royals",
                 "LAA" = "Los Angeles Angels",
                 "LAD" = "Los Angeles Dodgers",
                 "MIA" = "Miami Marlins",
                 "MIL" = "Milwaukee Brewers",
                 "MIN" = "Minnesota Twins",
                 "NYM" = "New York Mets",
                 "NYY" = "New York Yankees",
                 "OAK" = "Oakland Athletics",
                 "PHI" = "Philadelphia Phillies",
                 "PIT" = "Pittsburgh Pirates",
                 "SDP" = "San Diego Padres",
                 "SEA" = "Seattle Mariners",
                 "SFG" = "San Francisco Giants",
                 "STL" = "St. Louis Cardinals",
                 "TBR" = "Tampa Bay Rays",
                 "TEX" = "Texas Rangers",
                 "TOR" = "Toronto Blue Jays",
                 "WSN" = "Washington Nationals")

batting_teams$Tm <- team_labels[batting_teams$Tm]


##Clean names##

batting_players$Name <- gsub("\u00A0", " ", batting_players$Name)
batting_players$Name_Tm <- paste0(batting_players$Name, " (", batting_players$Tm, ")", sep = "")


##Write to hard disk##

write.csv(batting_players, "~/tables/batting_players_2018.csv", row.names = FALSE)
write.csv(batting_teams, "~/tables/batting_teams_2018.csv", row.names = FALSE)
