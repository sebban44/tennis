library(lubridate)
library(tidyverse)
library(elo)
library(dplyr)
library(tidyr)
library(plotly)
library(implied)

path <- "C:/Users/Seb/OneDrive/Dokument/te_doubles/"

# Get the list of file names
files <- list.files(path, pattern = "*.csv", full.names = TRUE)

# Read the files and bind them together
te_raw <- do.call("rbind", lapply(files, function(x) read.csv(x, sep = ",")))

#check for data only containing doubles matches
te_raw <- te_raw %>% filter(!is.na(team1_p2))

#fix date
te_raw <- te_raw %>% mutate(match_date = dmy(match_date) %>% ymd())

#fix result
te_raw <- te_raw %>% separate(result, into = c("set_home", "set_away"), sep = " : ")

te_raw$set_home <- as.numeric(te_raw$set_home)
te_raw$set_away <- as.numeric(te_raw$set_away)

te_raw <- te_raw %>% distinct(match_date,team1_p1,team1_p2,team2_p1,team2_p2,.keep_all=TRUE)

#remove all matches that are not finished
te_raw <- te_raw %>% filter(set_home > 1)

te_raw <- te_raw %>% arrange(match_date)

#surface filtering
#te_raw <- te_raw %>% filter(surface == "hard" | surface == "indoors" | surface == "clay" | surface == "grass")

matches <- te_raw
colnames(matches)[colnames(matches) == "team1_p1"] <- "winner1"
colnames(matches)[colnames(matches) == "team1_p2"] <- "winner2"
colnames(matches)[colnames(matches) == "team2_p1"] <- "loser1"
colnames(matches)[colnames(matches) == "team2_p2"] <- "loser2"
colnames(matches)[colnames(matches) == "match_date"] <- "date"

# Function to rearrange names
rearrange_name <- function(name) {
  # Split name by spaces
  name_parts <- strsplit(name, " ")[[1]]
  
  # Rearrange and collapse
  rearranged_name <- paste(name_parts[length(name_parts):1], collapse = " ")
  return(rearranged_name)
}

matches$winner1 <- sapply(matches$winner1, rearrange_name)
matches$winner2 <- sapply(matches$winner2, rearrange_name)
matches$loser1 <- sapply(matches$loser1, rearrange_name)
matches$loser2 <- sapply(matches$loser2, rearrange_name)


m1 <- matches %>% 
  dplyr::select(date,name=winner1)
m2 <- matches %>% 
  dplyr::select(date,name=winner2)
m3 <- matches %>% 
  dplyr::select(date,name=loser1)
m4 <- matches %>% 
  dplyr::select(date,name=loser2)

m <- rbind(m1,m2,m3,m4)
m <- m %>% arrange(date,name)
m <- m %>% mutate(n = 1)
m <- m %>% group_by(name) %>% mutate(lastmatch = lag(date)) %>% ungroup()
m <- m %>% group_by(name) %>% mutate(numberofmatches = lag(cumsum(n),default=0)) %>% ungroup()
matches <- inner_join(matches, m, by = c("date", "winner1" = "name"))
matches <- inner_join(matches, m, by = c("date", "winner2" = "name"))
matches <- inner_join(matches, m, by = c("date", "loser1" = "name"))
matches <- inner_join(matches, m, by = c("date", "loser2" = "name"))
matches <- matches %>% distinct(date,winner1,winner2,loser1,loser2,.keep_all = TRUE )

matches <- matches %>% 
  mutate(
    k1 = 250/((numberofmatches.x+5)^0.4),
    k2 = 250/((numberofmatches.y+5)^0.4),
    k3 = 250/((numberofmatches.x.x+5)^0.4),
    k4 = 250/((numberofmatches.y.y+5)^0.4),
    kw = (k1 + k2) / 2,
    kl = (k3 + k4) / 2
  )

clay_doubles <- matches %>% filter(surface == "clay")
hard_doubles <- matches %>% filter(surface == "hard")
grass_doubles <- matches %>% filter(surface == "grass")

setwd("C:\\Users\\Seb\\OneDrive\\Dokument")
Rcpp::sourceCpp("aedoubles2.cpp")
#for absence adjustment use  3
#Rcpp::sourceCpp("aedoubles3.cpp")

#init a player df to hold rankings
players <- data.frame(
  name = "Test",
  ranking = 1500,
  date = as.Date("1900-01-01")
)

doubles_all <- elo_update(players, matches)
doubles_clay <- elo_update(players,clay_doubles)
doubles_hard <- elo_update(players,hard_doubles)
doubles_grass <- elo_update(players,grass_doubles)


doubles_all1 <- doubles_all %>% dplyr::select(date,name=winner1,elo=new.eloA1)
doubles_all2 <- doubles_all %>% dplyr::select(date,name=winner2,elo=new.eloA2)
doubles_all3 <- doubles_all %>% dplyr::select(date,name=loser1,elo=new.eloB1)
doubles_all4 <- doubles_all %>% dplyr::select(date,name=loser2,elo=new.eloB2)
doubles_all_rank <- rbind(doubles_all1,doubles_all2,doubles_all3,doubles_all4)
doubles_all_rank <- doubles_all_rank %>% arrange(name,date)
doubles_all_rank$date <- as.Date(doubles_all_rank$date)

doubles_clay1 <- doubles_clay %>% dplyr::select(date,name=winner1,elo=new.eloA1)
doubles_clay2 <- doubles_clay %>% dplyr::select(date,name=winner2,elo=new.eloA2)
doubles_clay3 <- doubles_clay %>% dplyr::select(date,name=loser1,elo=new.eloB1)
doubles_clay4 <- doubles_clay %>% dplyr::select(date,name=loser2,elo=new.eloB2)
doubles_clay_rank <- rbind(doubles_clay1,doubles_clay2,doubles_clay3,doubles_clay4)
doubles_clay_rank <- doubles_clay_rank %>% arrange(name,date)
doubles_clay_rank$date <- as.Date(doubles_clay_rank$date)

doubles_hard1 <- doubles_hard %>% dplyr::select(date,name=winner1,elo=new.eloA1)
doubles_hard2 <- doubles_hard %>% dplyr::select(date,name=winner2,elo=new.eloA2)
doubles_hard3 <- doubles_hard %>% dplyr::select(date,name=loser1,elo=new.eloB1)
doubles_hard4 <- doubles_hard %>% dplyr::select(date,name=loser2,elo=new.eloB2)
doubles_hard_rank <- rbind(doubles_hard1,doubles_hard2,doubles_hard3,doubles_hard4)
doubles_hard_rank <- doubles_hard_rank %>% arrange(name,date)
doubles_hard_rank$date <- as.Date(doubles_hard_rank$date)

doubles_grass1 <- doubles_grass %>% dplyr::select(date,name=winner1,elo=new.eloA1)
doubles_grass2 <- doubles_grass %>% dplyr::select(date,name=winner2,elo=new.eloA2)
doubles_grass3 <- doubles_grass %>% dplyr::select(date,name=loser1,elo=new.eloB1)
doubles_grass4 <- doubles_grass %>% dplyr::select(date,name=loser2,elo=new.eloB2)
doubles_grass_rank <- rbind(doubles_grass1,doubles_grass2,doubles_grass3,doubles_grass4)
doubles_grass_rank <- doubles_grass_rank %>% arrange(name,date)
doubles_grass_rank$date <- as.Date(doubles_grass_rank$date)


doubles_all_rank <- doubles_all_rank %>%
  group_by(name) %>%
  summarise(elo = tail(elo,n=1),last_match = max(date))

doubles_clay_rank <- doubles_clay_rank %>%
  group_by(name) %>%
  summarise(clay_elo = tail(elo,n=1))

doubles_hard_rank <- doubles_hard_rank %>%
  group_by(name) %>%
  summarise(hard_elo = tail(elo,n=1))

doubles_grass_rank <- doubles_grass_rank %>%
  group_by(name) %>%
  summarise(grass_elo = tail(elo,n=1))

doubles <- left_join(doubles_all_rank,doubles_clay_rank,by=c("name"))
doubles <- left_join(doubles,doubles_hard_rank,by=c("name"))
doubles <- left_join(doubles,doubles_grass_rank,by=c("name"))

doubles <- doubles %>%
  mutate(
    elo = round(elo),
    c_elo = round(ifelse(is.na(clay_elo), elo * 0.5 + 1500 * .5,elo*0.5+clay_elo*0.5)),
    h_elo = round(ifelse(is.na(hard_elo),elo*0.5+1500*0.5,elo * 0.5 + hard_elo * 0.5)),
    g_elo = round(ifelse(is.na(grass_elo),elo*0.5+1500*0.5,elo * 0.5 + grass_elo * 0.5))
  ) %>% dplyr::select(
    name,elo,c_elo,h_elo,g_elo,last_match
  )

doubles <- doubles %>% arrange(desc(elo))
