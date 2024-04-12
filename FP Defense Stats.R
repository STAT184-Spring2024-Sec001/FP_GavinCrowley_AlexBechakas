library(readxl)
library(tidyr)
library(dplyr)
DefenseStats <- read_excel("2023 NFL Defense Stats.xlsx")
View(DefenseStats)

DefS <- DefenseStats %>%
  select(Pos,G,Int,IntTD,PD,FF,FR,FumTD,Comb,Solo,Ast,TFL,QBHits,Sfty)
