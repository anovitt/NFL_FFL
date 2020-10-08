library(RSelenium)
library(rvest)
library(tidyverse)
library(data.table)

wnum = 4

# datTeam = my team
# datRosters = other teams

# open web browser
rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
remDr <- rD[["client"]]

# open web site
remDr$navigate("https://fantasy.espn.com/football/team?leagueId=775&teamId=5&seasonId=2020")


#Sys.sleep(5) # give the page time to fully load
html <- remDr$getPageSource()[[1]]

signals <- read_html(html) %>% # parse HTML
  html_nodes("table") %>% # extract table nodes with class = "tbl_mapReception"
  #.[3] %>% # keep the third of these tables
  #.[[1]] %>% # keep the first element of this list
  html_table(fill=T) # have rvest turn it into a dataframe

datTeam <- as.data.table(signals[[1]])

names(datTeam) <-  unlist(c(datTeam[1,]))
datTeam <- datTeam[2:nrow(datTeam)]

datTeamHold <- datTeam

##############################
# Load other teams
##############################

datRosters <- data.table()

for(i in c(1:4,6:10)){
  #for(i in c(6,7)){
  #i<-7
  print(i)
  url <-paste0("https://fantasy.espn.com/football/team?leagueId=775&teamId=",i,"&seasonId=2020")
  remDr$navigate(url)
  Sys.sleep(5) # give the page time to fully load
  html <- remDr$getPageSource()[[1]]
  
  signals <- read_html(html) %>% # parse HTML
    html_nodes("table") %>% # extract table nodes with class = "tbl_mapReception"
    #.[3] %>% # keep the third of these tables
    #.[[1]] %>% # keep the first element of this list
    html_table(fill=T) # have rvest turn it into a dataframe
  #View(signals)
  
  datTemp <- as.data.table(signals[[1]])
  
  names(datTemp) <-  unlist(c(datTemp[1,]))
  datTemp <- datTemp[2:nrow(datTemp)]
  
  datRosters<-rbind(datRosters, datTemp)
}


teamAbrv = c("Buf","Mia","NE","NYJ","Dal","NYG","Phi","Wsh","Bal","Cin","Cle","Pit","Chi","Det","GB","Min","Hou","Ind","Jax",
  "Ten","Atl","Car","NO","TB","Den","KC","LV","LAC","Ari","LAR","SF","Sea")
length(teamAbrv)

# Pull the team abrivations to clean up player names
teamAbrv =
datRosters %>%
  filter(Player != 'Empty',
         Player != '',
         opp != '--') %>%
  mutate(opp = gsub('@','',opp)) %>%
  select(opp) %>%
  #mutate(opp = stringr::str_to_lower(opp)) %>%
  distinct(opp) %>%
  pull(opp)

length(teamAbrv)

teamAbrv = c("Buf","Mia","NE","NYJ","Dal","NYG","Phi","Wsh","Bal","Cin","Cle","Pit","Chi","Det","GB","Min","Hou","Ind","Jax",
             "Ten","Atl","Car","NO","TB","Den","KC","LV","LAC","Ari","LAR","SF","Sea")
length(teamAbrv)


datRosters =
datRosters %>%
  filter(Player != 'Empty',
         Player != '') %>%
  #mutate(Player2 = Player) %>%
  mutate(LastChar = ifelse(grepl('K',str_sub(Player,-1)),TRUE,FALSE),
         Posi = ifelse(LastChar,str_sub(Player,-1),str_sub(Player,-2,-1))) %>%
  filter(Posi == 'QB'|Posi =='RB'|Posi =='WR'|Posi =='TE') %>%
  mutate(Player = paste(tolower(stringr::str_sub(Player,1,1)),stringr::str_sub(Player, 2, nchar(Player)), sep=""),
         Player = str_remove(Player,paste(teamAbrv,collapse = "|") ),
         #Player = str_remove(Player,'D/ST|FA|IR'),
         #Player = str_remove(Player,'D/ST|D/Suf'),
         opp = gsub('@','',opp)) %>%
  mutate(Player = str_remove(Player,'QB|RB|WR|TE')) %>%
  mutate(Injury = ifelse(grepl('Q',str_sub(Player,-1)),'Q',
                         ifelse(grepl('D',str_sub(Player,-1)),'D',
                                ifelse(grepl('IR',Player),'IR',
                                       ifelse(grepl('O',str_sub(Player,-1)),'O',
                                       'OK'))))
                         ) %>%
  mutate(Player = ifelse(grepl('Q',str_sub(Player,-1)),str_sub(Player,end = -2),
                         ifelse(grepl('D',str_sub(Player,-1)),str_sub(Player,end = -2),
                                ifelse(grepl('O',str_sub(Player,-1)),str_sub(Player,end = -2),
                                       ifelse(grepl('IR',Player),str_sub(Player,end = -3),Player))))) %>%
  mutate( FFL_Team = "Rostered") %>%
  select(Player,Posi,proj,Injury,SCORE,OPRK,`%ST`,`%ROST`,`+/-`,PRK,FPTS,avg,LAST,FFL_Team) %>%
  mutate(Player = str_to_title(Player))


datTeam =
datTeam %>%
  filter(Player != 'Empty',
         Player != '') %>%
  #mutate(Player2 = Player) %>%
  mutate(LastChar = ifelse(grepl('K',str_sub(Player,-1)),TRUE,FALSE),
         Posi = ifelse(LastChar,str_sub(Player,-1),str_sub(Player,-2,-1))) %>%
  filter(Posi == 'QB'|Posi =='RB'|Posi =='WR'|Posi =='TE') %>%
  mutate(Player = paste(tolower(stringr::str_sub(Player,1,1)),stringr::str_sub(Player, 2, nchar(Player)), sep=""),
    Player = str_remove(Player,paste(teamAbrv,collapse = "|") ),
    #Player = str_remove(Player,'D/ST|FA|IR'),
    #Player = str_remove(Player,'D/ST|D/Suf'),
    opp = gsub('@','',opp)) %>%
  mutate(Player = str_remove(Player,'QB|RB|WR|TE')) %>%
  mutate(Injury = ifelse(grepl('Q',str_sub(Player,-1)),'Q',
                         ifelse(grepl('D',str_sub(Player,-1)),'D',
                                ifelse(grepl('IR',Player),'IR',
                                       ifelse(grepl('O',str_sub(Player,-1)),'O',
                                              'OK'))))
  ) %>%
  mutate(Player = ifelse(grepl('Q',str_sub(Player,-1)),str_sub(Player,end = -2),
                         ifelse(grepl('D',str_sub(Player,-1)),str_sub(Player,end = -2),
                                ifelse(grepl('O',str_sub(Player,-1)),str_sub(Player,end = -2),
                                       ifelse(grepl('IR',Player),str_sub(Player,end = -3),Player))))) %>%
  mutate( FFL_Team = "GID") %>%
  select(Player,Posi,proj,Injury,SCORE,OPRK,`%ST`,`%ROST`,`+/-`,PRK,FPTS,avg,LAST,FFL_Team) %>%
  mutate(Player = str_to_title(Player))
  

datRoster <-
  rbind(datRosters,datTeam)

# Fix Derek Carr
datRoster <-
datRoster %>%
  mutate(Player = ifelse(grepl('Derek Rlv',Player),'Derek Carr',Player),
        Player = ifelse(grepl('Gardne',Player),'Gardner Minshew II',Player)) 

### Pull Team data

datNFL <- data.table()

for(i in 1:length(teamAbrv)){

  print(i)
url <-paste0("https://www.espn.com/nfl/team/roster/_/name/",teamAbrv[i],"/")
remDr$navigate(url)
Sys.sleep(5) # give the page time to fully load
html <- remDr$getPageSource()[[1]]

signals <- read_html(html) %>% # parse HTML
  html_nodes("table") %>% # extract table nodes with class = "tbl_mapReception"
  #.[3] %>% # keep the third of these tables
  #.[[1]] %>% # keep the first element of this list
  html_table(fill=T) # have rvest turn it into a dataframe
#View(signals)

datTemp <- rbind(as.data.table(signals[[1]]),
                               as.data.table(signals[[2]]),
                                             as.data.table(signals[[3]]))
datTemp<- 
  datTemp %>%
  mutate(Team = teamAbrv[i])

datNFL <- rbind(datNFL,datTemp)
}


# remove column and player number

datNFL =
datNFL %>%
  select(-V1) %>%
  mutate(Name = stringr::str_remove(Name,'[0-9]+')) %>%
  rename("Player"= "Name") %>%
  mutate(Player = str_to_title(Player))

# Get the player stats
datNFLStatsQB <-data.table()
datNFLStatsRB <-data.table()
datNFLStatsWR <-data.table()
datNFLStatsK <-data.table()



# Player stats
for(i in 1:length(teamAbrv)){
  
  print(i)
  
  url <-paste0("https://www.espn.com/nfl/team/stats/_/name/",teamAbrv[i],"/")
  remDr$navigate(url)
  Sys.sleep(5) # give the page time to fully load
  html <- remDr$getPageSource()[[1]]
  
  signals <- read_html(html) %>% # parse HTML
    html_nodes("table") %>% # extract table nodes with class = "tbl_mapReception"
    #.[3] %>% # keep the third of these tables
    #.[[1]] %>% # keep the first element of this list
    html_table(fill=T) # have rvest turn it into a dataframe
  #View(signals)
  
  datTemp <- cbind(as.data.table(signals[[1]]),
                   as.data.table(signals[[2]]))
  datTemp<- 
    datTemp %>%
    mutate(Team = teamAbrv[i])
  
  datNFLStatsQB <- rbind(datNFLStatsQB,datTemp)
  
  datTemp <- cbind(as.data.table(signals[[3]]),
                   as.data.table(signals[[4]]))
  datTemp<- 
    datTemp %>%
    mutate(Team = teamAbrv[i])
  
  datNFLStatsRB <- rbind(datNFLStatsRB,datTemp)
  
  datTemp <- cbind(as.data.table(signals[[5]]),
                   as.data.table(signals[[6]]))
  datTemp<- 
    datTemp %>%
    mutate(Team = teamAbrv[i])
  
  datNFLStatsWR <- rbind(datNFLStatsWR,datTemp)
  
  datTemp <- cbind(as.data.table(signals[[5]]),
                   as.data.table(signals[[6]]))
  datTemp<- 
    datTemp %>%
    mutate(Team = teamAbrv[i])
  
  datNFLStatsWR <- rbind(datNFLStatsWR,datTemp)
}

pos =  
datNFL%>%
  distinct(POS) %>%
  pull(POS) %>%
  paste(.,collapse = "|")

#datNFLStatsQBTeam =
#datNFLStatsQB %>% 
#  filter(Name == "Total")

#datTeam

datNFLStatsQB =
datNFLStatsQB %>%
  filter(Name != "Total") %>%
  mutate(Posi = stringr::str_sub(Name,-2,-1),
         Name = stringr::str_sub(Name,1,str_length(Name)-2),
         Name = str_trim(Name, side = c("both")),
         Posi = str_trim(Posi, side = c("both"))) %>%
  rename('Player' = 'Name') %>%
  mutate(Player = str_to_title(Player))

#datNFLStatsRBTeam =
#  datNFLStatsRB %>% 
 # filter(Name == "Total")

datNFLStatsRB =
  datNFLStatsRB %>%
  filter(Name != "Total") %>%
  mutate(Posi = stringr::str_sub(Name,-2,-1),
         Name = stringr::str_sub(Name,1,str_length(Name)-2),
         Name = str_trim(Name, side = c("both")),
         Posi = str_trim(Posi, side = c("both"))) %>%
  rename('Player' = 'Name') %>%
  mutate(Player = str_to_title(Player))

#datNFLStatsWRTeam =
#  datNFLStatsWR %>% 
#  filter(Name == "Total")

datNFLStatsWR =
  datNFLStatsWR %>%
  filter(Name != "Total") %>%
  mutate(Posi = stringr::str_sub(Name,-2,-1),
         Name = stringr::str_sub(Name,1,str_length(Name)-2),
         Name = str_trim(Name, side = c("both")),
         Posi = str_trim(Posi, side = c("both"))) %>%
  rename('Player' = 'Name') %>%
  mutate(Player = str_to_title(Player))

datNFLStatsTE <- datNFLStatsWR

# Close
remDr$close()

datTeam %>% write.csv("datTeam.csv", row.names = FALSE)
datRoster %>% write.csv("datRoster.csv", row.names = FALSE)
datNFL  %>% write.csv("datNFL.csv", row.names = FALSE)
datNFLStatsQB  %>% write.csv("datNFLStatsQB.csv", row.names = FALSE)
datNFLStatsRB  %>% write.csv("datNFLStatsRB.csv", row.names = FALSE)
datNFLStatsWR  %>% write.csv("datNFLStatsWR.csv", row.names = FALSE)
datNFLStatsTE  %>% write.csv("datNFLStatsTE.csv", row.names = FALSE)

#datNFLStatsQBTeam  %>% write.csv("datNFLStatsQBTeam.csv", row.names = FALSE)
#datNFLStatsRBTeam  %>% write.csv("datNFLStatsRBTeam.csv", row.names = FALSE)
#datNFLStatsWRTeam  %>% write.csv("datNFLStatsWRTeam.csv", row.names = FALSE)


####
#  create data frame showing rostered, wavier wire, and my team
####

datNFL =
datNFL %>%
  left_join(select(datRoster,Player,FFL_Team),by = 'Player') %>%
  mutate(FFL_Team = ifelse(is.na(FFL_Team),'Wavier',FFL_Team))

#datNFLStatsQB <- fread("datNFLStatsQB.csv")
#datNFLStatsRB <- fread("datNFLStatsRB.csv")
#datNFLStatsWR <- fread("datNFLStatsWR.csv")

datNFLStatsQB =
datNFLStatsQB %>% 
  filter(Posi == 'QB') %>%
  left_join(select(datRoster,Player,FFL_Team),by = 'Player') %>%
  mutate(FFL_Team = ifelse(is.na(FFL_Team),'Wavier',FFL_Team)) %>%
  left_join(select(datRosters,Player,'%ST','%ROST','+/-','PRK','FPTS','avg'),by = "Player")  %>%
  replace(is.na(.), 0) %>%
  mutate(PRK = gsub('--',0,PRK),
         FPTS =gsub('--',0,FPTS),
         avg = gsub('--',0,avg))

datNFLStatsRB =
  datNFLStatsRB %>% 
  filter(Posi == 'RB') %>%
  left_join(select(datRoster,Player,FFL_Team),by = 'Player') %>%
  mutate(FFL_Team = ifelse(is.na(FFL_Team),'Wavier',FFL_Team)) %>%
  left_join(select(datRosters,Player,'%ST','%ROST','+/-','PRK','FPTS','avg'),by = "Player")  %>%
  replace(is.na(.), 0) %>%
  mutate(PRK = gsub('--',0,PRK),
         FPTS =gsub('--',0,FPTS),
         avg = gsub('--',0,avg))


datNFLStatsWR =
  datNFLStatsWR %>% 
  filter(Posi == 'WR') %>%
  left_join(select(datRoster,Player,FFL_Team),by = 'Player') %>%
  mutate(FFL_Team = ifelse(is.na(FFL_Team),'Wavier',FFL_Team)) %>%
  left_join(select(datRosters,Player,'%ST','%ROST','+/-','PRK','FPTS','avg'),by = "Player")  %>%
  replace(is.na(.), 0) %>%
  mutate(PRK = gsub('--',0,PRK),
         FPTS =gsub('--',0,FPTS),
         avg = gsub('--',0,avg))

datNFLStatsTE =
  datNFLStatsTE %>% 
  filter(Posi == 'TE') %>%
  left_join(select(datRoster,Player,FFL_Team),by = 'Player') %>%
  mutate(FFL_Team = ifelse(is.na(FFL_Team),'Wavier',FFL_Team)) %>%
  left_join(select(datRosters,Player,'%ST','%ROST','+/-','PRK','FPTS','avg'),by = "Player")  %>%
  replace(is.na(.), 0) %>%
  mutate(PRK = gsub('--',0,PRK),
         FPTS =gsub('--',0,FPTS),
         avg = gsub('--',0,avg))


#########################################################################################################################
# Position Analysis
#########################################################################################################################

str(datNFLStatsQB)

datNFLStatsQB = as.data.table(datNFLStatsQB)

datGIDQB =
  datNFLStatsQB %>%
  filter(FFL_Team == 'GID') %>%
  mutate(YDS = as.numeric(gsub(',','',YDS))) %>%
  #distinct() %>%
  mutate(FPoints = YDS*0.06+TD*5) 

datNFLStatsQB %>%
  mutate(YDS = as.numeric(gsub(',','',YDS))) %>%
  mutate(FPoints = YDS*0.06+TD*5) %>%
  filter(ATT > 20) %>%
  ggplot() +
  geom_point(aes(x=FPoints,y=Player,color = FFL_Team),size = I(5)) +
  geom_text(data = datGIDQB, aes(x=FPoints,y=Player,label = Player)) +
  ggtitle(label = "Northville GID QB Player Assessment")

datGIDRB =
  datNFLStatsRB %>%
  filter(FFL_Team == 'GID') %>%
  mutate(YDS = as.numeric(gsub(',','',YDS))) %>%
  distinct() %>%
  mutate(FPoints = YDS/10+TD*6) 


datNFLStatsRB %>%
  mutate(YDS = as.numeric(gsub(',','',YDS))) %>%
  mutate(FPoints = YDS/10+TD*6) %>%
  ggplot() +
  geom_point(aes(x=FPoints,y=Player,color = FFL_Team),size = I(5)) +
  geom_text(data = datGIDRB, aes(x=FPoints,y=Player,label = Player)) +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle(label = "Northville GID RB Player Assessment")

datGIDWR =
  datNFLStatsWR %>%
  filter(FFL_Team == 'GID') %>%
  mutate(YDS = as.numeric(gsub(',','',YDS))) %>%
  distinct() %>%
  mutate(FPoints = YDS/10+TD*6 + REC) 

datNFLStatsWR %>%
  mutate(YDS = as.numeric(gsub(',','',YDS))) %>%
  mutate(FPoints = YDS/10+TD*6 + REC) %>%
  ggplot() +
  geom_point(aes(x=FPoints,y=Player,color = FFL_Team),size = I(5)) +
  geom_text(data = datGIDWR, aes(x=FPoints,y=Player,label = Player)) +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle(label = "Northville GID WR Player Assessment")


datGIDTE =
  datNFLStatsTE %>%
  filter(FFL_Team == 'GID') %>%
  mutate(YDS = as.numeric(gsub(',','',YDS))) %>%
  distinct() %>%
  mutate(FPoints = YDS/10+TD*6 + REC) 
  
datNFLStatsTE %>%
  mutate(YDS = as.numeric(gsub(',','',YDS))) %>%
  mutate(FPoints = YDS/10+TD*6 + REC) %>%
  ggplot() +
  geom_point(aes(x=FPoints,y=Player,color = FFL_Team),size = I(5)) +
  geom_text(data = datGIDTE, aes(x=FPoints,y=Player,label = Player)) +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle(label = "Northville GID TE Player Assessment")


############################
#  Weekly Stats
############################

#source("NFL_Player_Weekly_Stats.R")
# set the year and week numbers for team comparisions
#yrs <- seq(from = 2000,to=2019,by=1)
#yrs <- c(2019)
#wk <- c(1:17)

yrs <- c(2020)
wk <- c(1:wnum)

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

#dat_NFL_QB_SQL <- dat_NFL_QB
#names(dat_NFL_QB_SQL) <- stringr::str_to_lower(names(dat_NFL_QB_SQL))
#dbWriteTable(con, name = 'dat_nfl_qb', value = dat_NFL_QB_SQL, overwrite = FALSE, append = TRUE)


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

#dat_NFL_RB_SQL <- dat_NFL_RB
#names(dat_NFL_RB_SQL) <- stringr::str_to_lower(names(dat_NFL_RB_SQL))
#dbWriteTable(con, name = 'dat_nfl_rb', value = dat_NFL_RB_SQL)


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

#dat_NFL_WR_SQL <- dat_NFL_WR
#names(dat_NFL_WR_SQL) <- stringr::str_to_lower(names(dat_NFL_WR_SQL))
#dbWriteTable(con, name = 'dat_nfl_wr', value = dat_NFL_WR_SQL)


# join GID, Rostered, Waiver

# Box plots for player performance.

Game_Played = 
dat_NFL_QB %>%
  filter(Pos == 'QB') %>%
  select(Player,Pos,Week,Cmp,Att,CmpPer,Yds,TD,Int,Rate,Sk,Yds_Loss,AY_Per_A) %>%
  mutate(Player = str_to_title(Player)) %>%
  mutate(Week = as.numeric(Week),
         Cmp = as.numeric(Cmp),
         Att = as.numeric(Att),
         CmpPer = as.numeric(CmpPer),
         Yds = as.numeric(Yds),
         TD = as.numeric(TD),
         Int = as.numeric(Int),
         Sk = as.numeric(Sk),
         Yds_Loss = as.numeric(Yds_Loss),
         AY_Per_A = as.numeric(AY_Per_A)) %>%
  mutate(FPoints = Yds*.06+TD*5) %>%
  filter(FPoints > 2) %>%
  group_by(Player) %>%
  summarise(n=n()) 

dat_NFL_QB %>%
  filter(Pos == 'QB') %>%
  select(Player,Pos,Week,Cmp,Att,CmpPer,Yds,TD,Int,Rate,Sk,Yds_Loss,AY_Per_A) %>%
  mutate(Player = str_to_title(Player)) %>%
  mutate(Week = as.numeric(Week),
         Cmp = as.numeric(Cmp),
         Att = as.numeric(Att),
         CmpPer = as.numeric(CmpPer),
         Yds = as.numeric(Yds),
         TD = as.numeric(TD),
         Int = as.numeric(Int),
         Sk = as.numeric(Sk),
         Yds_Loss = as.numeric(Yds_Loss),
         AY_Per_A = as.numeric(AY_Per_A)) %>%
  mutate(FPoints = Yds*0.06+TD*5) %>%
  left_join(select(datNFL,Player,FFL_Team), by = 'Player') %>%
  mutate(FFL_Team = ifelse(is.na(FFL_Team),"Wavier",FFL_Team)) %>%
  distinct() %>%
  filter(FPoints > 2) %>%
  ggplot() +
  #geom_boxplot(aes(x = FPoints,y= reorder(Player, FPoints, FUN = median),fill=FFL_Team)) +
  geom_boxplot(aes(x = FPoints,y= reorder(Player, FPoints, FUN = median),fill=FFL_Team)) +
  geom_text(data = Game_Played,aes(x = 0, y=Player,label = n)) +
  xlab(label="Fantasy Points") +
  ylab(label="Player")+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle(label = "QB Actual Fantasy Point Assessment")
  

Game_Played = 
    dat_NFL_RB %>%
    filter(Pos == 'RB') %>%
    select(Player,Pos,Week,Att,Yds,TD,Yards_Per_Att) %>%
    mutate(Player = str_to_title(Player)) %>%
    mutate(Week = as.numeric(Week),
           Att = as.numeric(Att),
           Yds = as.numeric(Yds),
           TD = as.numeric(TD),
           Yards_Per_Att = as.numeric(Yards_Per_Att)) %>%
    mutate(FPoints = Yds/10+TD*6) %>%
    filter(FPoints > 2) %>%
    group_by(Player) %>%
    summarise(n=n()) 
  
dat_NFL_RB %>%
    filter(Pos == 'RB') %>%
    select(Player,Pos,Week,Att,Yds,TD,Yards_Per_Att) %>%
    mutate(Player = str_to_title(Player)) %>%
    mutate(Week = as.numeric(Week),
           Att = as.numeric(Att),
           Yds = as.numeric(Yds),
           TD = as.numeric(TD),
           Yards_Per_Att = as.numeric(Yards_Per_Att)) %>%
    mutate(FPoints = Yds/10+TD*7) %>%
    left_join(select(datNFL,Player,FFL_Team), by = 'Player') %>%
    mutate(FFL_Team = ifelse(is.na(FFL_Team),"Wavier",FFL_Team)) %>%
    mutate(FFL_Team = ifelse(grepl('Gurley',Player),'Rostered',FFL_Team)) %>%
    distinct() %>%
    filter(FPoints > 2) %>%
    ggplot() +
    #geom_boxplot(aes(x = FPoints,y=Player,fill=FFL_Team)) +
    geom_boxplot(aes(x = FPoints,y= reorder(Player, FPoints, FUN = median),fill=FFL_Team)) +
    #geom_text(data = Game_Played,aes(x = 0, y=Player,label = n)) +
    xlab(label="Fantasy Points") +
     ylab(label="Player")+
    coord_flip()+
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle(label = "RB Actual Fantasy Point Assessment")

Game_Played = 
  dat_NFL_WR %>%
  filter(Pos == 'WR') %>%
  mutate(Player = str_to_title(Player)) %>%
  mutate(Week = as.numeric(Week),
         Tgt = as.numeric(Tgt),
         Yds = as.numeric(Yds),
         TD = as.numeric(TD),
         Yds_Per_Rec = as.numeric(Yds_Per_Rec),
         Catch_Per = as.numeric(Catch_Per),
         Y_TGt = as.numeric(Y_TGt)) %>%
  mutate(FPoints = Yds/10+TD*6 + Yds*(1/(Yds_Per_Rec))) %>%
  filter(FPoints > 2) %>%
  group_by(Player) %>%
  summarise(n=n()) 


dat_NFL_WR %>%
  filter(Pos == 'WR') %>%
  select(Player,Pos,Week,Tgt,Rec,Yds,TD,Yds_Per_Rec,Catch_Per,Y_TGt) %>%
  mutate(Player = str_to_title(Player)) %>%
  mutate(Week = as.numeric(Week),
         Tgt = as.numeric(Tgt),
         Yds = as.numeric(Yds),
         TD = as.numeric(TD),
         Yds_Per_Rec = as.numeric(Yds_Per_Rec),
         Catch_Per = as.numeric(Catch_Per),
         Y_TGt = as.numeric(Y_TGt)) %>%
  mutate(FPoints = Yds/10+TD*6 + Yds*(1/Yds_Per_Rec)) %>%
  left_join(select(datNFL,Player,FFL_Team), by = 'Player') %>%
  mutate(FFL_Team = ifelse(is.na(FFL_Team),"Wavier",FFL_Team)) %>%
  distinct() %>%
  filter(FPoints > 5) %>%
  ggplot() +
  #geom_boxplot(aes(x = FPoints,y=Player,fill=FFL_Team)) +
  geom_boxplot(aes(x = FPoints,y= reorder(Player, FPoints, FUN = median),fill=FFL_Team)) +
  #geom_text(data = Game_Played,aes(x = 0, y=Player,label = n), size = 3) +
  xlab(label="Fantasy Points") +
  ylab(label="Player")+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle(label = "WR Actual Fantasy Point Assessment")

Game_Played = 
  dat_NFL_WR %>%
  filter(Pos == 'TE') %>%
  mutate(Player = str_to_title(Player)) %>%
  mutate(Week = as.numeric(Week),
         Tgt = as.numeric(Tgt),
         Yds = as.numeric(Yds),
         TD = as.numeric(TD),
         Yds_Per_Rec = as.numeric(Yds_Per_Rec),
         Catch_Per = as.numeric(Catch_Per),
         Y_TGt = as.numeric(Y_TGt)) %>%
  mutate(FPoints = Yds/10+TD*7) %>%
  filter(FPoints > 1) %>%
  group_by(Player) %>%
  summarise(n=n()) 

dat_NFL_WR %>%
  filter(Pos == 'TE') %>%
  select(Player,Pos,Week,Tgt,Rec,Yds,TD,Yds_Per_Rec,Catch_Per,Y_TGt) %>%
  mutate(Player = str_to_title(Player)) %>%
  mutate(Week = as.numeric(Week),
         Tgt = as.numeric(Tgt),
         Yds = as.numeric(Yds),
         TD = as.numeric(TD),
         Yds_Per_Rec = as.numeric(Yds_Per_Rec),
         Catch_Per = as.numeric(Catch_Per),
         Y_TGt = as.numeric(Y_TGt)) %>%
  mutate(FPoints = Yds/10+TD*6 + Yds*(1/Yds_Per_Rec)) %>%
  left_join(select(datNFL,Player,FFL_Team), by = 'Player') %>%
  mutate(FFL_Team = ifelse(is.na(FFL_Team),"Wavier",FFL_Team)) %>%
  distinct() %>%
  filter(FPoints > 2) %>%
  ggplot() +
  #geom_boxplot(aes(x = FPoints,y=Player,fill=FFL_Team)) +
  geom_boxplot(aes(x = FPoints,y= reorder(Player, FPoints, FUN = median),fill=FFL_Team)) +
  #geom_text(data = Game_Played,aes(x = 0, y=Player,label = n)) +
  xlab(label="Fantasy Points") +
  ylab(label="Player")+
  coord_flip()+
  #coord_flip()+
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle(label = "TE Actual Fantasy Point Assessment")

datRosterESPN_Proj =
datRoster %>%
  select(Player,Posi,proj,FFL_Team)

write.csv(datRosterESPN_Proj, file = "datRosterESPN_Proj.csv")
