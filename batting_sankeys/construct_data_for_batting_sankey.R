setwd("C:/nextcloud/Firma/Projects/baseball_dashboard/")

library(tidyverse)
library(data.table)

batting_teams <- fread("tables/batting_teams_2018.csv")
batting_players <- fread("tables/batting_players_2018.csv")


batting_teams_sankeys_data <- data.table(Name = batting_teams$Tm,
                                         Name_Tm = batting_teams$Tm,
                                         Name_Players = NA,
                                         PA = batting_teams$PA,
                                  SH = batting_teams$SH,
                                  SF = batting_teams$SF,
                                  `1B` = batting_teams$`1B`,
                                  `2B` = batting_teams$`2B`,
                                  `3B` = batting_teams$`3B`,
                                  H = batting_teams$H,
                                  HR = batting_teams$HR,
                                  IBB = batting_teams$IBB,
                                  UBB = batting_teams$UBB,
                                  BB = batting_teams$BB,
                                  HBP = batting_teams$HBP,
                                  XI = batting_teams$XI,
                                  R = batting_teams$R,
                                  SO = batting_teams$SO,
                                  GDP = batting_teams$GDP,
                                  `GO/AO` = batting_teams$`GO/AO`,
                                  CS = batting_teams$CS,
                                  PO = batting_teams$PO,
                                  ROE = batting_teams$ROE,
                                  OOB = batting_teams$OOB,
                                  AB = batting_teams$AB,
                                  SB = batting_teams$SB,
                                  OnBase = batting_teams$OnBase,
                                  Out = batting_teams$Out,
                                  LOB = batting_teams$LOB,
                                  total_GO_FO = batting_teams$total_GO_FO,
                                  GO = batting_teams$GO,
                                  FO = batting_teams$FO)

batting_players_sankeys_data <- data.table(Name = batting_players$Name,
                                         Name_Tm = NA,
                                         Name_Players = batting_players$Name_Players,
                                         PA = batting_players$PA,
                                         SH = batting_players$SH,
                                         SF = batting_players$SF,
                                         `1B` = batting_players$`1B`,
                                         `2B` = batting_players$`2B`,
                                         `3B` = batting_players$`3B`,
                                         H = batting_players$H,
                                         HR = batting_players$HR,
                                         IBB = batting_players$IBB,
                                         UBB = batting_players$UBB,
                                         BB = batting_players$BB,
                                         HBP = batting_players$HBP,
                                         XI = batting_players$XI,
                                         R = batting_players$R,
                                         SO = batting_players$SO,
                                         GDP = batting_players$GDP,
                                         `GO/AO` = batting_players$`GO/AO`,
                                         CS = batting_players$CS,
                                         PO = batting_players$PO,
                                         ROE = batting_players$ROE,
                                         OOB = batting_players$OOB,
                                         AB = batting_players$AB,
                                         SB = batting_players$SB,
                                         OnBase = batting_players$OnBase,
                                         Out = batting_players$Out,
                                         LOB = batting_players$LOB,
                                         total_GO_FO = batting_players$total_GO_FO,
                                         GO = batting_players$GO,
                                         FO = batting_players$FO)
               

batting_teams_sankeys_data <- mutate(batting_teams_sankeys_data, PA = PA + XI)
batting_players_sankeys_data <- mutate(batting_players_sankeys_data, PA = PA + XI)

batting_all_sankeys_data <- rbind(batting_players_sankeys_data, batting_teams_sankeys_data)

batting_all_sankeys_data <- filter(batting_all_sankeys_data, PA > 49)

batting_all_sankeys_data <- filter(batting_all_sankeys_data, !(is.na(Name_Tm) & is.na(Name_Players)))

write.csv(batting_all_sankeys_data, "tables/batting_all_sankeys_data.csv", row.names = FALSE)

