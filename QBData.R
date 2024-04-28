library(tidyverse)
library(tidyr)
library(readxl)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)

#Filtering by top 20 rather than the full 32 teams because it removes human variable
#Injuries happen, and the 21-26/32 have tendencies to be filled with players who were injured
#Or backups to not play the entire season. Selecting thetop 20 removes this variable


stats <- read_xlsx("/Users/alexbechakas/Desktop/FP_GavinCrowley_AlexBechakas/QBStats.xlsx")


starters <- stats%>%
  group_by(Year)%>%
  arrange(desc(`Pass Yds`))%>%
  filter(row_number()<21)%>%
  arrange(desc(Year))
View(starters)



era<- starters%>%
  group_by(Year)%>%
  filter(Year != 1982)%>%
  summarise(`Year Mean` = mean(`Pass Yds`))
View(era)

temp<-era%>%
  mutate(`Games per Year` = if_else(Year<1982,"14 Games","16 Games"))

temp2<-replace(temp$`Games per Year`,temp$Year>2020,"17 Games")

New_era<-era%>%
  mutate(`Games per Year` = temp2)
View(New_era)

merged_yards<-merge(x= starters, y = era, by = "Year", all.x = T)%>%
  select(Year,Player, `Pass Yds`, TD, INT, `Year Mean`)
View(merged_yards)

adj_yards<- merged_yards%>%
  summarise(Year = Year, Player = Player, TD = TD, INT= INT,`APY (Average Passing Yards)` = `Year Mean` ,Yards = `Pass Yds`, `APY+` = round(100*(`Pass Yds`/`Year Mean`),0))%>%
arrange(desc(`APY+`))
View(adj_yards)


points<-starters%>%
  group_by(Year)%>%
  summarise(Year = Year, Player = Player, Yards = `Pass Yds`,TD = TD, INT = INT, Points = ((`Pass Yds`)+(TD*100)-(INT*50)))
View(points)


  
  
era_points<- points%>%
  group_by(Year)%>%
  filter(Year != 1982)%>%
  summarise(`Year Mean` = mean(Points))
View(era_points)

New_era_points<-era_points%>%
  mutate(`Games per Year` = temp2)
View(New_era_points)

merged_points<-merge(x= points, y = New_era_points, by = "Year", all.x = T)%>%
  select(Year,Player, Points, TD, INT, `Year Mean`,`Games per Year`)
View(merged_points)


adj_points<- merged_points%>%
  summarise(Year = as.character(Year), Player = Player, TD = TD, INT =INT,`AP (Average Points)` = `Year Mean` 
            ,Points = Points, `AP+` = round(100*(Points/`Year Mean`),0),`Games per Year`= `Games per Year`)%>%
  arrange(desc(`AP+`))
View(adj_points)

#Table for showing average yards per year
ggplot(New_era) +
  aes(x = Year, y = `Year Mean`, colour = `Games per Year`) +
  geom_point(shape = "circle", size = 2L) +
  scale_color_brewer(palette = "Set1", direction = 1) +
  labs(x = "Year", y = "Average Passing Yards") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30L,
                              hjust = 0.5),
    axis.title.y = element_text(size = 14L),
    axis.title.x = element_text(size = 14L)
  )

#Same Table as above for Points
ggplot(New_era_points) +
  aes(x = Year, y = `Year Mean`, colour = `Games per Year`) +
  geom_point(shape = "circle", size = 2L) +
  scale_color_brewer(palette = "Set1", direction = 1) +
  labs(x = "Year", y = "Average Points") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30L,
                              hjust = 0.5),
    axis.title.y = element_text(size = 14L),
    axis.title.x = element_text(size = 14L)
  )


#Adjusted yards Table
adj_yard_table<- adj_yards%>%
  select(Year, Player, TD, INT ,`APY+`)
head(adj_yard_table,5)%>%
  kable(caption = "Best AP+ since 1970",
        align = c("l", rep("c", 10))) %>%
  kable_styling(      
    bootstrap_options = c("striped"),      
    font_size = 16)


#Adjusted points Table
adj_points_table<- adj_points%>%
  select(Year, Player, TD, INT ,`AP+`)
  head(adj_points_table,5)%>%
  kable(caption = "Best AP+ since 1970",
        align = c("l", rep("c", 10))) %>%
  kable_styling(      
    bootstrap_options = c("striped"),      
    font_size = 16)
  
  

#Best AP+ Barplot
ggplot(head(adj_points_table, 5)) +
  aes(x = Year, y = `AP+`, fill = Player) +
  geom_col() +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  labs(
    x = "Year",
    y = "AP+",
    title = "Best AP+ since 1970",
    fill = "Name"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30L),
    axis.title.y = element_text(size = 14L),
    axis.title.x = element_text(size = 14L)
  )


  