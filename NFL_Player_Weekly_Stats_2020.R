library(data.table)
library(tidyverse)
library(lubridate)
library(rvest)

library(RPostgreSQL)

con = dbConnect(
  user = "postgres",
  dbDriver("PostgreSQL"),
  dbname = "NFL_2020",
  password = "Oldblue16",
  host = "localhost"
)

wnum = 4

#yrs <- seq(from = 2000,to=2019,by=1)
yrs <- c(2020)
wk <- c(wnum)

#### weekly NFL QB stats 2020 - 2019
dat_NFL_QB <- data.table()
Col_Names <- c('Rk','Player', 'Pos', 'Age','Date','Lg','Tm','H_A', 'Opp','Result','G','Week','Day','Cmp','Att','CmpPer','Yds','TD','Int','Rate', 'Sk', 'Yds_Loss', 'YA','AY_Per_A')

for(j in yrs ){
for(i in wk){
  
  print(c(j,i))

  url <- paste0("https://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=",j,"&year_max=",j,"&season_start=1&season_end=-1&age_min=0&age_max=99&game_type=A&league_id=&team_id=&opp_id=&game_num_min=0&game_num_max=99&week_num_min=",i,"&week_num_max=",i,"&game_day_of_week=&game_location=&game_result=&handedness=&is_active=&is_hof=&c1stat=pass_att&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=pass_rating&from_link=1")

  NFL_QB <- xml2::read_html(url) %>%
    html_nodes(css = 'table') %>%
    html_table(fill = TRUE)
  dat_temp <- as.data.table(NFL_QB[[1]])[,1:24]
  names(dat_temp) <- Col_Names

  dat_temp <-
    dat_temp %>%
    filter(Rk != 'Rk')

  dat_NFL_QB <- rbind(dat_NFL_QB,dat_temp)
}
}

#write.csv(dat_NFL_QB,file = 'NFL_QB_Week.csv',row.names = FALSE)

dat_NFL_QB_SQL <- dat_NFL_QB
names(dat_NFL_QB_SQL) <- stringr::str_to_lower(names(dat_NFL_QB_SQL))
dbWriteTable(con, name = 'dat_nfl_qb', value = dat_NFL_QB_SQL, overwrite = FALSE, append = TRUE)


#### 2020 weekly NFL RB stats
dat_NFL_RB <- data.table()
Col_Names <- c('Rk','Player', 'Pos', 'Age','Date','Lg','Tm','H_A', 'Opp','Result','G','Week','Day','Att','Yds','Yards_Per_Att','TD')

for(j in yrs ){
for(i in wk){
  for(k in c(0,100,200)){
    
    print(c(j,i,k))
  
  url <- paste0("https://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=",j,"&year_max=",j,"&season_start=1&season_end=-1&age_min=0&age_max=99&game_type=A&league_id=&team_id=&opp_id=&game_num_min=0&game_num_max=99&week_num_min=",i,"&week_num_max=",i,"&game_day_of_week=&game_location=&game_result=&handedness=&is_active=&is_hof=&c1stat=rush_att&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rush_yds&from_link=1&offset=",k)
  
  NFL_RB <- xml2::read_html(url) %>%
    html_nodes(css = 'table') %>%
    html_table(fill = TRUE)
  dat_temp <- as.data.table(NFL_RB[[1]])[,1:17]
  names(dat_temp) <- Col_Names
  
  dat_temp <-
    dat_temp %>%
    filter(Rk != 'Rk')
  
  dat_NFL_RB <- rbind(dat_NFL_RB,dat_temp)
  }
}
}


#write.csv(dat_NFL_RB,file = 'NFL_RB_Week.csv',row.names = FALSE)

dat_NFL_RB_SQL <- dat_NFL_RB
names(dat_NFL_RB_SQL) <- stringr::str_to_lower(names(dat_NFL_RB_SQL))
dbWriteTable(con, name = 'dat_nfl_rb', value = dat_NFL_RB_SQL, overwrite = FALSE, append = TRUE)


#### 2019 weekly NFL WR stats
dat_NFL_WR <- data.table()
Col_Names <- c('Rk','Player', 'Pos', 'Age','Date','Lg','Tm','H_A', 'Opp','Result','G','Week','Day','Tgt','Rec','Yds','Yds_Per_Rec','TD','Catch_Per','Y_TGt')

for(j in yrs ){ 
for(i in wk){
  for(k in c(0,100,200,300)){
  
    print(c(j,i,k))
  
    url <- paste0("https://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=",j,"&year_max=",j,"&season_start=1&season_end=-1&age_min=0&age_max=99&game_type=A&league_id=&team_id=&opp_id=&game_num_min=0&game_num_max=99&week_num_min=",i,"&week_num_max=",i,"&game_day_of_week=&game_location=&game_result=&handedness=&is_active=&is_hof=&c1stat=rec&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rec_yds&from_link=1&offset=",k)
  
  NFL_WR <- xml2::read_html(url) %>%
    html_nodes(css = 'table') %>%
    html_table(fill = TRUE)
  dat_temp <- as.data.table(NFL_WR[[1]])[,1:20]
  names(dat_temp) <- Col_Names
  
  dat_temp <-
    dat_temp %>%
    filter(Rk != 'Rk')
  
  dat_NFL_WR <- rbind(dat_NFL_WR,dat_temp)
  }
}
}
#write.csv(dat_NFL_WR,file = 'NFL_WR_Week.csv',row.names = FALSE)

dat_NFL_WR_SQL <- dat_NFL_WR
names(dat_NFL_WR_SQL) <- stringr::str_to_lower(names(dat_NFL_WR_SQL))
dbWriteTable(con, name = 'dat_nfl_wr', value = dat_NFL_WR_SQL, overwrite = FALSE, append = TRUE)


# defense tackles

dat_NFL_Def_Tackles <- data.table()
Col_Names <- c('Rk','Player', 'Pos', 'Age','Date','Lg','Tm','H_A', 'Opp','Result','G','Week','Day','Sk','Solo','Ast','T_Comb','TFL','QBHits')

for(j in yrs ){
for(i in wk){
  for(k in c(0,100,200,300,400)){
  print(c(j,i,k))
    
    url <- paste0("https://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=",j,"&year_max=",j,"&season_start=1&season_end=-1&age_min=0&age_max=99&game_type=A&league_id=&team_id=&opp_id=&game_num_min=0&game_num_max=99&week_num_min=",i,"&week_num_max=",i,"&game_day_of_week=&game_location=&game_result=&handedness=&is_active=&is_hof=&c1stat=tackles_solo&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=sacks&from_link=1&offset=",k)
  
  NFL_Def_Tackles <- xml2::read_html(url) %>%
    html_nodes(css = 'table') %>%
    html_table(fill = TRUE)
  dat_temp <- as.data.table(NFL_Def_Tackles[[1]])[,1:19]
  names(dat_temp) <- Col_Names
  
  dat_temp <-
    dat_temp %>%
    filter(Rk != 'Rk')
  
  dat_NFL_Def_Tackles <- rbind(dat_NFL_Def_Tackles,dat_temp)
  }
}
}

#write.csv(dat_NFL_Def_Tackles,file = 'dat_NFL_Def_Tackles_Week.csv',row.names = FALSE)

dat_NFL_Def_Tackles_SQL <- dat_NFL_Def_Tackles
names(dat_NFL_Def_Tackles_SQL) <- stringr::str_to_lower(names(dat_NFL_Def_Tackles_SQL))
dbWriteTable(con, name = 'dat_nfl_def_tackles', value = dat_NFL_Def_Tackles_SQL, overwrite = FALSE, append = TRUE)

dat_NFL_Def_Int <- data.table()
Col_Names <- c('Rk','Player', 'Pos', 'Age','Date','Lg','Tm','H_A', 'Opp','Result','G','Week','Day','Int','Int_Yds','Int_TD','PD')

for(j in yrs ){
for(i in wk){
  
  for(k in c(0,100,200)){
    print(c(j,i,k))
    
    url <- paste0("https://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=",j,"&year_max=",j,"&season_start=1&season_end=-1&pos%5B%5D=DL&pos%5B%5D=LB&pos%5B%5D=DB&is_starter=E&game_type=R&game_num_min=0&game_num_max=99&week_num_min=",i,"&week_num_max=",i,"&c5val=1.0&order_by=def_int&from_link=1&offset=",k)
  
  NFL_Def_Int <- xml2::read_html(url) %>%
    html_nodes(css = 'table') %>%
    html_table(fill = TRUE)
  dat_temp <- as.data.table(NFL_Def_Int[[1]])[,1:17]
  names(dat_temp) <- Col_Names
  
  dat_temp <-
    dat_temp %>%
    filter(Rk != 'Rk')
  
  dat_NFL_Def_Int <- rbind(dat_NFL_Def_Int,dat_temp)
  }
}
}

#write.csv(dat_NFL_Def_Int,file = 'dat_NFL_Def_Int_Week.csv',row.names = FALSE)

dat_NFL_Def_Int_SQL <- dat_NFL_Def_Int
names(dat_NFL_Def_Int_SQL) <- stringr::str_to_lower(names(dat_NFL_Def_Int_SQL))
dbWriteTable(con, name = 'dat_nfl_def_int', value = dat_NFL_Def_Int_SQL, overwrite = FALSE, append = TRUE)

dat_NFL_Results <- data.table()

for(j in yrs ){
  
  #j <- 2000
  
  url <- paste0("https://www.pro-football-reference.com/years/",j,"/games.htm")
  NFL_Results <- xml2::read_html(url) %>%
    html_nodes(css = 'table') %>%
    html_table(fill = TRUE)

  dat_temp <- as.data.frame(NFL_Results[[1]])
  dat_temp <- dat_temp[,c(1,5,7,9,10,11,12,13,14)]
  
  dat_temp <-
  dat_temp %>%
    rename('Winner'='Winner/tie','Loser'= 'Loser/tie') %>%
    filter(Week != 'Week') %>%
    mutate(Week = as.numeric(Week),
           PtsW = as.numeric(PtsW),
           PtsL = as.numeric(PtsL),
           YdsW = as.numeric(YdsW),
           TOW = as.numeric(TOW),
           YdsL = as.numeric(YdsL),
           TOL = as.numeric(TOL),
           Year = j) %>%
    filter(!is.na(Week)) %>%
    arrange(desc(Week))
  
  #dat_temp[248,]
  
  dat_NFL_Results <- rbind(dat_NFL_Results,dat_temp)

}

Team_Map <- fread('Team_Map.csv')

dat_NFL_Results <-
dat_NFL_Results %>%
  left_join(Team_Map, by = c('Winner'='Team')) %>%
  rename(Win = Tm) %>%
  left_join(Team_Map, by = c('Loser'='Team')) %>%
  rename(Lose = Tm) %>%
  select(-Winner,-Loser) %>%
  mutate(Pt_Diff = PtsW - PtsL)

dat_NFL_Results <-
dat_NFL_Results %>%
  arrange(Year,Week)

dat_NFL_Results <-
dat_NFL_Results %>%
  filter(!is.na(PtsW))

write.csv(dat_NFL_Results,file = 'dat_NFL_Results_Week.csv',row.names = FALSE)

dat_NFL_Results_SQL <- dat_NFL_Results %>%
  filter(Week == wnum)
names(dat_NFL_Results_SQL) <- stringr::str_to_lower(names(dat_NFL_Results_SQL))
dbWriteTable(con, name = 'dat_nfl_results', value = dat_NFL_Results_SQL,overwrite = FALSE, append = TRUE)


url <- "https://www.pro-football-reference.com/years/2020/games.htm"
NFL_Schedule <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)

dat_NFL_Schedule_2020 <- as.data.table(NFL_Schedule[[1]])

names(dat_NFL_Schedule_2020) <- c("Week", "Day", "V1","VisTm","Vis_Pts", "V2","HomeTm", "Home_Pts","Time")

dat_NFL_Schedule_2020 =
dat_NFL_Schedule_2020 %>%
  select(Week,Winner = VisTm, Loser = HomeTm) %>%
  left_join(Team_Map, by = c('Winner'='Team')) %>%
  rename(Win = Tm) %>%
  left_join(Team_Map, by = c('Loser'='Team')) %>%
  rename(Lose = Tm) %>%
  select(-Winner,-Loser) %>%
  filter(Week != "Week") %>%
  mutate(Week = as.numeric(Week))


write.csv(dat_NFL_Schedule_2020,file = 'dat_NFL_Schedule_2020.csv',row.names = FALSE)
#dbWriteTable(con, name = 'dat_nfl_schedule', value = dat_NFL_Schedule_2020)

dat_NFL_Schedule_SQL <- dat_NFL_Schedule_2020
names(dat_NFL_Schedule_SQL) <- stringr::str_to_lower(names(dat_NFL_Schedule_SQL))
dbWriteTable(con, name = 'dat_nfl_schedule', value = dat_NFL_Schedule_SQL)





