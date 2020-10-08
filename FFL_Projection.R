library(RSelenium)
library(rvest)
library(tidyverse)
library(data.table)

url <- "https://football.fantasysports.yahoo.com/f1/1033005/players?&sort=AR&sdir=1&status=ALL&pos=QB&stat1=S_PW_4&jsenabled=1"
yahooProjectionQB <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)

url <- "https://football.fantasysports.yahoo.com/f1/1033005/players?&sort=AR&sdir=1&status=ALL&pos=RB&stat1=S_PW_4&jsenabled=1"
yahooProjectionRB <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)


url <- "https://football.fantasysports.yahoo.com/f1/1033005/players?&sort=AR&sdir=1&status=ALL&pos=WR&stat1=S_PW_4&jsenabled=1"
yahooProjectionWR <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)

url <- "https://football.fantasysports.yahoo.com/f1/1033005/players?&sort=AR&sdir=1&status=ALL&pos=TE&stat1=S_PW_4&jsenabled=1"
yahooProjectionTE <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)

url <- "https://fantasy.nfl.com/research/projections#researchProjections=researchProjections%2C%2Fresearch%2Fprojections%253Fposition%253D1%2526statCategory%253DprojectedStats%2526statSeason%253D2020%2526statType%253DweekProjectedStats%2526statWeek%253D4%2Creplace"
nflProjection <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)

url <- "https://fantasy.nfl.com/research/projections#researchProjections=researchProjections%2C%2Fresearch%2Fprojections%253Fposition%253D2%2526statCategory%253DprojectedStats%2526statSeason%253D2020%2526statType%253DweekProjectedStats%2526statWeek%253D4%2Creplace"
nflProjection <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)
url1 == url2

url <- "https://baseball.fantasysports.yahoo.com/b1/165789/players"
batterAvail <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)


fftodayProjection[2]
batterAvail[[2]]

# open web browser
rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
remDr <- rD[["client"]]

# open web site
remDr$navigate("https://www.fftoday.com/rankings/playerwkproj.php?Season=2020&GameWeek=3&PosID=10&LeagueID=26955")


Sys.sleep(5) # give the page time to fully load
html <- remDr$getPageSource()[[1]]

signals <- read_html(html) %>% # parse HTML
  html_nodes("table") %>% # extract table nodes with class = "tbl_mapReception"
  #.[3] %>% # keep the third of these tables
  #.[[1]] %>% # keep the first element of this list
  html_table(fill=T) # have rvest turn it into a dataframe

datTeam <- as.data.table(signals[[1]])

names(datTeam) <-  unlist(c(datTeam[1,]))
datTeam <- datTeam[2:nrow(datTeam)]
