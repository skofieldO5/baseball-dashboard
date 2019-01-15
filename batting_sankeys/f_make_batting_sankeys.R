setwd("C:/Nextcloud/Firma/Projects/baseball_dashboard")

library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(data.table)
library(plyr)
library(networkD3)
library(htmlwidgets)
library(htmltools)



batting_all_sankeys_data <- fread("tables/batting_all_sankeys_data.csv")


make_batting_sankey <- function(searchterm) {
bat <- filter(batting_all_sankeys_data, Name == searchterm)[1,]

##Make Links and Nodes Table##

links <- data.table(
  source = c(rep("PA", 15), "1B", "2B", "3B", "UBB", "IBB", "HBP", "ROE", "XI", rep("OnBase", 4), "CS", "PO", "OOB", 
             "OnBase"),
  target = c("SH", "SF", "SO", "FO", "GO", "GDP", "HR", "1B", "2B", "3B", "UBB", "IBB", "HBP", "ROE", "XI", rep("OnBase", 8),
             "R", "CS", "PO", "OOB", "Out", "Out", "Out", "LOB"),
  group = c("Sac", "Sac", rep("Out", 4), "Run", rep("OnBase", 16), "Run", rep("Out", 6), "LOB"),
  value = c(bat$SH, bat$SF, bat$SO, bat$FO, bat$GO, bat$GDP, bat$HR, bat$`1B`, bat$`2B`, bat$`3B`, bat$UBB, bat$IBB, bat$HBP, bat$ROE,
            bat$XI, bat$`1B`, bat$`2B`, bat$`3B`, bat$UBB, bat$IBB, bat$HBP, bat$ROE, bat$XI, bat$R, bat$CS,
            bat$PO, bat$OOB, bat$CS, bat$PO, bat$OOB, bat$LOB)
  )





##Nodes##

nodes <- data.table(name=c(as.character(links$source), 
                               as.character(links$target)) %>% unique())

#Order Nodes'

nodes_order <- c("PA", "SH", "SF", "SO", "FO", "GO", "GDP", "HR", "1B", "2B", "3B", "UBB", "IBB", "HBP", "ROE", "XI", "OnBase",
                 "R", "CS", "PO", "OOB", "Out", "LOB")

nodes <- nodes[match(nodes_order, nodes$name),]

nodes$group <- as.factor(c("PA", "Sac", "Sac", rep("Out",4), "Run", rep("OnBase", 9), "Run", rep("Out", 4), "LOB"))

nodes$state <- c(rep("Mouseover_test", 23))

nodes$value <- c(bat$PA, bat$SH, bat$SF, bat$SO, bat$FO, bat$GO, bat$GDP, bat$HR, bat$`1B`, bat$`2B`, bat$`3B`,
                 bat$UBB, bat$IBB, bat$HBP, bat$ROE, bat$XI, bat$OnBase, bat$R, bat$CS, bat$PO, bat$OOB, bat$Out,
                 bat$LOB)

##Delete Links and Nodes with no value##

missing_links <- which(links$value < 1 | is.na(links$value))

links <- links[!missing_links]

missing_nodes <- which(nodes$value < 1 | is.na(nodes$value))

nodes <- nodes[!missing_nodes]


links$IDsource=match(links$source, nodes$name)-1 
links$IDtarget=match(links$target, nodes$name)-1





##Calculate Percentages##

nodes_to_percentize <- c(nodes[2:length(nodes$name),1])[[1]]

#percentize function#
percentize <- function(string) {
  nodes[name == string, name := paste(string, " (", round(bat[,string] / bat$PA*100, digits = 2), "%)", sep = "")] 
}

for (i in nodes_to_percentize) {
  percentize(as.character(i))
}


##Write Total PAs into Label##
nodes[1,1] <- paste(nodes[1,1], " Total (", bat$PA, ")", sep ="")


##Make Sankey Diagram##

sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget",
                            Value = "value", NodeID = "name",NodeGroup = "group", LinkGroup = "group",
                            fontSize = 15, colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"), 
              nodePadding = 8.5) 
}
