######
# Load data
######

library(data.table)
library(tidyverse)
library(lubridate)
library(RPostgreSQL)

con = dbConnect(
  user = "postgres",
  dbDriver("PostgreSQL"),
  dbname = "NFL_2020",
  password = "Oldblue16",
  host = "localhost"
)

query = function(sql) {
  dbSendQuery(con, sql) %>%
    fetch(n = 1e8) %>%
    as_tibble()
}

wnum = 4

# Run NFL_Player_Weekly_Stats.R to pull data and win / loss
# number of game to calculate the rolling average.

roll_games = 4

# Load Quarterback data.

dat_NFL_QB_Sql = query("
  SELECT
    week,
    tm,
    EXTRACT(YEAR FROM TO_DATE(date,'YYYY-MM-DD')) AS year,
    SUM(cmp) AS cmp,
    SUM(att) AS att_qb,
    SUM(yds) AS yds_qb,
    SUM(td) AS td_qb,
    SUM(int) AS int_off_qb,
    SUM(sk) AS sk_qb
  FROM dat_nfl_qb
  GROUP BY year, tm, week
  ORDER BY year, tm, week
")

dat_NFL_QB =
  dat_NFL_QB_Sql %>%
  mutate(Cmp_QB_Rate = cmp/att_qb,
         Yds_QB_Rate = yds_qb/att_qb,
         TD_QB_Rate = td_qb/att_qb,
         Int_Off_QB_Rate = int_off_qb/att_qb,
         Sk_QB_Rate = sk_qb/att_qb) %>%
  arrange(tm,year,week) %>%
  replace(is.na(.), 0) %>%
  group_by(tm) %>%
  mutate(Cmp_QB_Rate_Roll = zoo::rollapply(Cmp_QB_Rate,roll_games,mean,align='right',fill=Cmp_QB_Rate),
         Yds_QB_Rate_Roll = zoo::rollapply(Yds_QB_Rate,roll_games,mean,align='right',fill=Yds_QB_Rate),
         TD_QB_Rate_Roll = zoo::rollapply(TD_QB_Rate,roll_games,mean,align='right',fill=TD_QB_Rate),
         Int_Off_QB_Rate_Roll = zoo::rollapply(Int_Off_QB_Rate,roll_games,mean,align='right',fill=Int_Off_QB_Rate),
         Sk_QB_Rate_Roll = zoo::rollapply(Sk_QB_Rate,roll_games,mean,align='right',fill=Sk_QB_Rate)) 

# Running back data.

dat_NFL_RB_SQL = query("
  SELECT
    week,
    tm,
    EXTRACT(YEAR FROM TO_DATE(date,'YYYY-MM-DD')) AS year,
    SUM(att) AS att_rb,
    SUM(yds) AS yds_rb,
    SUM(td) AS td_rb
  FROM dat_nfl_rb
  GROUP BY year, tm, week
  ORDER BY year, tm, week
")

dat_NFL_RB =
  dat_NFL_RB_SQL %>%
  mutate(Yds_RB_Rate = yds_rb/att_rb,
         TD_RB_Rate = td_rb/att_rb) %>%
  arrange(tm,year,week) %>%
  replace(is.na(.), 0) %>%
  group_by(tm) %>%
  mutate(Yds_RB_Rate_Roll = zoo::rollapply(Yds_RB_Rate,roll_games,mean,align='right',fill=Yds_RB_Rate),
         TD_RB_Rate_Roll = zoo::rollapply(TD_RB_Rate,roll_games,mean,align='right',fill=TD_RB_Rate))


# Wide receiver
dat_NFL_WR_SQL = query("
  SELECT
    week,
    tm,
    EXTRACT(YEAR FROM TO_DATE(date,'YYYY-MM-DD')) AS year,
    SUM(tgt) AS tgt_wr,
    SUM(yds) AS yds_wr,
    SUM(td) AS td_wr,
    SUM(rec) AS rec_wr
  FROM dat_nfl_wr
  GROUP BY year, tm, week
  ORDER BY year, tm, week
")

dat_NFL_WR =
  dat_NFL_WR_SQL %>%
  mutate(Yds_Tgt_WR_Rate = yds_wr/tgt_wr,
         TD_Tgt_WR_Rate = td_wr/tgt_wr,
         Yds_Rec_WR_Rate = yds_wr/rec_wr,
         TD_Rec_WR_Rate = td_wr/rec_wr) %>%
  arrange(tm,year,week) %>%
  replace(is.na(.), 0) %>%
  group_by(tm) %>%
  mutate(Yds_Tgt_WR_Rate_Roll = zoo::rollapply(Yds_Tgt_WR_Rate,roll_games,mean,align='right',fill=Yds_Tgt_WR_Rate),
         TD_Tgt_WR_Rate_Roll = zoo::rollapply(TD_Tgt_WR_Rate,roll_games,mean,align='right',fill=TD_Tgt_WR_Rate),
         Yds_Rec_WR_Rate_Roll = zoo::rollapply(Yds_Rec_WR_Rate,roll_games,mean,align='right',fill=Yds_Rec_WR_Rate),
         TD_Rec_WR_Rate_Roll = zoo::rollapply(TD_Rec_WR_Rate,roll_games,mean,align='right',fill=TD_Rec_WR_Rate))

# Defence tackles
dat_NFL_Def_Tackles_SQL = query("
  SELECT
    week,
    tm,
    EXTRACT(YEAR FROM TO_DATE(date,'YYYY-MM-DD')) AS year,
    SUM(sk) AS sk_def_rate,
    SUM(solo) solo_def_rate,
    SUM(ast) AS ast_def_rate,
    SUM(t_comb) AS t_comb_def_rate,
    SUM(tfl) AS tfl_def_rate
  FROM dat_nfl_def_tackles
  GROUP BY year, tm, week
  ORDER BY year, tm, week
")

dat_NFL_Def_Tackles =
  dat_NFL_Def_Tackles_SQL %>%
  arrange(tm,year,week) %>%
  replace(is.na(.), 0) %>%
  group_by(tm) %>%
  mutate(Sk_Def_Rate_Roll = zoo::rollapply(sk_def_rate,roll_games,mean,align='right',fill=sk_def_rate),
         Solo_Def_Rate_Roll = zoo::rollapply(solo_def_rate,roll_games,mean,align='right',fill=solo_def_rate),
         Ast_Def_Rate_Roll = zoo::rollapply(ast_def_rate,roll_games,mean,align='right',fill=ast_def_rate),
         T_Comb_Def_Rate_Roll = zoo::rollapply(t_comb_def_rate,roll_games,mean,align='right',fill=t_comb_def_rate),
         TFL_Def_Rate_Roll = zoo::rollapply(tfl_def_rate,roll_games,mean,align='right',fill=tfl_def_rate)) 

# defense interceptions

dat_NFL_Def_Int_SQL = query("
  SELECT
    week,
    tm,
    EXTRACT(YEAR FROM TO_DATE(date,'YYYY-MM-DD')) AS year,
    SUM(int) AS int_def_rate,
    SUM(int_yds) AS int_yds_def_rate,
    SUM(int_td) AS int_td_def_rate,
    SUM(pd) AS pd_def_rate
  FROM dat_nfl_def_int
  GROUP BY year, tm, week
  ORDER BY year, tm, week
")

dat_NFL_Def_Int =
  dat_NFL_Def_Int_SQL %>%
  arrange(tm,year,week) %>%
  replace(is.na(.), 0) %>%
  group_by(tm) %>%
  mutate(Int_Def_Rate_Roll = zoo::rollapply(int_def_rate,roll_games,mean,align='right',fill=int_def_rate),
         Int_Yds_Def_Rate_Roll = zoo::rollapply(int_yds_def_rate,roll_games,mean,align='right',fill=int_yds_def_rate),
         Int_TD_Def_Rate_Roll = zoo::rollapply(int_td_def_rate,roll_games,mean,align='right',fill=int_td_def_rate),
         PD_Def_Rate_Roll = zoo::rollapply(pd_def_rate,roll_games,mean,align='right',fill=pd_def_rate)) 

# inner join to QB data for each teams weekly stats

dat_NFL =
dat_NFL_QB %>%
  inner_join(dat_NFL_RB, by = c('year','tm','week')) %>%
  inner_join(dat_NFL_WR, by = c('year','tm','week')) %>%
  inner_join(dat_NFL_Def_Tackles,by = c('year','tm','week')) %>%
  inner_join(dat_NFL_Def_Int,by = c('year','tm','week')) %>%
  as.data.table()

#  Win team for joining

dat_NFL_W <- dat_NFL
names(dat_NFL_W) <- c(names(dat_NFL)[1:3],paste("W_",names(dat_NFL)[4:56],sep='')) 

dat_NFL_W <-
  dat_NFL_W %>% 
    select(year,tm,week,ends_with('_Roll'))

# Loss team for joining

dat_NFL_L <- dat_NFL
names(dat_NFL_L) <- c(names(dat_NFL)[1:3],paste("L_",names(dat_NFL)[4:56],sep=''))

dat_NFL_L <-
  dat_NFL_L %>%
  select(year,tm,week,ends_with('_Roll')) 

#  Load results from 2000

dat_NFL_Results_SQL = query("
  SELECT
    week,
    year,
    win,
    lose,
    pt_diff
  FROM dat_nfl_results
")

# Join weekly team data for each game


dat_NFL_Results_Game <- 
  rbind(
dat_NFL_Results_SQL %>%
  #filter(Year >= 2005) %>%
  left_join(dat_NFL_W, by = c("win" = "tm", "year", "week")) %>%
  left_join(dat_NFL_L, by = c("lose" = "tm", "year", "week")) %>%
 #drop_na() %>%
   as.data.table(),

dat_NFL_Results_SQL %>%
  rename(lose = win,
         win = lose) %>%
  mutate(pt_diff = -1*pt_diff) %>%
  left_join(dat_NFL_W, by = c("win" = "tm", "year", "week")) %>%
  left_join(dat_NFL_L, by = c("lose" = "tm", "year", "week")) %>%
  #drop_na() %>%
  as.data.table()
) %>%
  arrange(year,win,week) %>%
  mutate(Game_Number = seq(from = 1, to = nrow(.),by=1))

# Use 2019 week 17 to predict 2020 week 1 then update to until week 4

dat_NFL_W_2020 =
dat_NFL_W %>%
  filter(year == 2020,week == wnum)

dat_NFL_L_2020 =
  dat_NFL_L %>%
  filter(year == 2020,week == wnum)

# lOad the 2020 schedule

dat_NFL_Schedule_2020 = query("
  SELECT
    week,
    win,
    lose
  FROM dat_nfl_schedule
")

# join the prior week data for predicting.
dat_NFL_Schedule_2020_Test =
dat_NFL_Schedule_2020 %>% 
  left_join(select(dat_NFL_W_2020,-year,-week), by = c("win" = "tm")) %>%
  left_join(select(dat_NFL_L_2020,-year,-week), by = c("lose" = "tm")) 

### xgboost model
library(xgboost)
library(lme4)
library(Metrics)

dat_NFL_Results_Game =
  as.data.table(dat_NFL_Results_Game)

# Pull rolling average data and point difference for the the model.

features = setdiff(names(dat_NFL_Results_Game), c("week", "year","win","lose","pt_diff","Game_Number"))
dtrain = xgb.DMatrix(as.matrix(dat_NFL_Results_Game[, ..features]), label = dat_NFL_Results_Game$pt_diff)

cauchyobj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  c <- 5000 
  x <-  preds-labels
  grad <- x / (x^2/c^2+1)
  hess <- -c^2*(x^2-c^2)/(x^2+c^2)^2
  return(list(grad = grad, hess = hess))
}

xgb_parameters = 
  list(objective = cauchyobj, 
       eval_metric = "mae",
       booster = "gbtree", 
       eta = 0.02,
       subsample = 0.35,
       colsample_bytree = 0.7,
       num_parallel_tree = 2,
       min_child_weight = 40,
       gamma = 10,
       max_depth = 3)

N = nrow(dat_NFL_Results_Game)
#N = nrow(dat_NFL_Results_Game_Scrape)
fold5list = c(
  rep( 1, floor(N/5) ),
  rep( 2, floor(N/5) ),
  rep( 3, floor(N/5) ),
  rep( 4, floor(N/5) ),
  rep( 5, N - 4*floor(N/5) )
)


### Build cross-validation model, repeated 10-times

iteration_count = c()
smooth_model = list()

for (i in 1:10) {
 
  ### Resample fold split
  set.seed(i)
  print(i)
  folds = list()  
  fold_list = sample(fold5list)
  for (k in 1:5) folds[[k]] = which(fold_list == k)
  
  set.seed(120)
  xgb_cv = 
    xgb.cv(
      params = xgb_parameters,
      data = dtrain,
      #nrounds = 3000,
      nrounds = 300,
      verbose = 0,
      nthread = 12,
      folds = folds,
      early_stopping_rounds = 25,
      maximize = FALSE,
      prediction = TRUE
    )
  iteration_count = c(iteration_count, xgb_cv$best_iteration)
  
  ### Fit a smoothed GAM model on predicted result point differential to get probabilities
  smooth_model[[i]] = smooth.spline(x = xgb_cv$pred, y = ifelse(dat_NFL_Results_Game$pt_diff > 0, 1, 0))
  
}

### Build submission models

submission_model = list()

for (i in 1:10) {
  set.seed(i)
  print(i)
  submission_model[[i]] = 
    xgb.train(
      params = xgb_parameters,
      data = dtrain,
      nrounds = round(iteration_count[i]*1.05),
      verbose = 0,
      nthread = 12,
      maximize = FALSE,
      prediction = TRUE
    )
}

dtest = xgb.DMatrix(as.matrix(dat_NFL_Schedule_2020_Test[, features]))

probs = list()
for (i in 1:10) {
  preds = predict(submission_model[[i]], dtest)
  probs[[i]] = predict(smooth_model[[i]], preds)$y
}
dat_NFL_Schedule_2020_Test$Pred = Reduce("+", probs) / 10
dat_NFL_Schedule_2020_Test$PointDiff = preds

dat_NFL_Schedule_2020_Test %>%
  select(week,win,lose,Pred,PointDiff) %>%
  filter(week == wnum +1)
