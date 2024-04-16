library(tidyverse)
library(tidyr)
library(readxl)
library(dplyr)

#Filtering by top 20 rather than the full 32 teams because it removes human variable
#Injuries happen, and the 21-26/32 have tendencies to be filled with players who were injured
#Or backups to not play the entire season. Selecting thetop 20 removes this variable
stats <- read_xlsx("/Users/alexbechakas/Desktop/temp_table.xlsx")


starters <- stats%>%
  group_by(Year)%>%
  arrange(desc(`Pass Yds`))%>%
  filter(row_number()<21)%>%
  arrange(desc(Year))
View(starters)


era<- starters%>%
  group_by(Year)%>%
  summarise(`Year Mean` = mean(`Pass Yds`))
View(era)


merged_yards<-merge(x= starters, y = era, by = "Year", all.x = T)%>%
  select(Year,Player, `Pass Yds`, TD, INT, `Year Mean`)
View(merged_yards)


adj_yards<- merged_yards%>%
  summarise(Player = Player, TD = TD, `APY (Average Passing Yards)` = `Year Mean` ,Yards = `Pass Yds`, `APY+` = round(100*(`Pass Yds`/`Year Mean`),0))%>%
arrange(desc(`APY+`))
View(adj_yards)


points<-starters%>%
  group_by(Year)%>%
  summarise(Year = Year, Player = Player, Yards = `Pass Yds`,TD = TD, INT = INT, Points = ((`Pass Yds`)+(TD*100)-(INT*50)))
View(points)


era_points<- points%>%
  group_by(Year)%>%
  summarise(`Year Mean` = mean(Points))
View(era_points)


merged_points<-merge(x= points, y = era_points, by = "Year", all.x = T)%>%
  select(Year,Player, Points, TD, INT, `Year Mean`)
View(merged_points)


adj_points<- merged_points%>%
  summarise(Year = Year, Player = Player, TD = TD, `AP (Average Points)` = `Year Mean` ,Points = Points, `AP+` = round(100*(Points/`Year Mean`),0))%>%
  arrange(desc(`AP+`))
View(adj_points)

