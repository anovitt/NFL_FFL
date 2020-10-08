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

# Enter the last week played
wnum = 4

# number of game to calculate the rolling average.
roll_games = 4

# Load Quarterback data.

dat_NFL_QB_Sql = query("
  SELECT
    week,
    player,
    tm,
    EXTRACT(YEAR FROM TO_DATE(date,'YYYY-MM-DD')) AS year,
    SUM(cmp) AS cmp,
    SUM(att) AS att_qb,
    SUM(yds) AS yds_qb,
    SUM(td) AS td_qb,
    SUM(int) AS int_off_qb,
    SUM(sk) AS sk_qb
  FROM dat_nfl_qb
  GROUP BY year, player, tm,week
  ORDER BY year, player, tm,week
")

dat_NFL_QB =
  dat_NFL_QB_Sql %>%
  mutate(Cmp_QB_Rate = cmp/att_qb,
         Yds_QB_Rate = yds_qb/att_qb,
         TD_QB_Rate = td_qb/att_qb,
         Int_Off_QB_Rate = int_off_qb/att_qb,
         Sk_QB_Rate = sk_qb/att_qb) %>%
  arrange(tm,year,week,player) %>%
  replace(is.na(.), 0) %>%
  group_by(player) %>%
  mutate(Cmp_QB_Rate_Roll = zoo::rollapply(Cmp_QB_Rate,roll_games,mean,align='right',fill=Cmp_QB_Rate),
         Yds_QB_Rate_Roll = zoo::rollapply(Yds_QB_Rate,roll_games,mean,align='right',fill=Yds_QB_Rate),
         TD_QB_Rate_Roll = zoo::rollapply(TD_QB_Rate,roll_games,mean,align='right',fill=TD_QB_Rate),
         Int_Off_QB_Rate_Roll = zoo::rollapply(Int_Off_QB_Rate,roll_games,mean,align='right',fill=Int_Off_QB_Rate),
         Sk_QB_Rate_Roll = zoo::rollapply(Sk_QB_Rate,roll_games,mean,align='right',fill=Sk_QB_Rate),
         Att_QB_Roll = zoo::rollapply(att_qb,roll_games,mean,align='right',fill=att_qb)) %>%
  mutate(tm = ifelse(tm == "LVR",'OAK',tm))

# Running back data.

dat_NFL_RB_SQL = query("
  SELECT
    week,
    player,
    tm,
    EXTRACT(YEAR FROM TO_DATE(date,'YYYY-MM-DD')) AS year,
    SUM(att) AS att_rb,
    SUM(yds) AS yds_rb,
    SUM(td) AS td_rb
  FROM dat_nfl_rb
  GROUP BY year, player, tm, week
  ORDER BY year, player, tm, week
")

dat_NFL_RB =
  dat_NFL_RB_SQL %>%
  mutate(Yds_RB_Rate = yds_rb/att_rb,
         TD_RB_Rate = td_rb/att_rb) %>%
  arrange(tm,year,week) %>%
  replace(is.na(.), 0) %>%
  group_by(player) %>%
  mutate(Yds_RB_Rate_Roll = zoo::rollapply(Yds_RB_Rate,roll_games,mean,align='right',fill=Yds_RB_Rate),
         TD_RB_Rate_Roll = zoo::rollapply(TD_RB_Rate,roll_games,mean,align='right',fill=TD_RB_Rate),
         Att_RB_Roll = zoo::rollapply(att_rb,roll_games,mean,align='right',fill=att_rb)) %>%
  mutate(tm = ifelse(tm == "LVR",'OAK',tm))


# Wide receiver
dat_NFL_WR_SQL = query("
  SELECT
    week,
    player,
    tm,
    EXTRACT(YEAR FROM TO_DATE(date,'YYYY-MM-DD')) AS year,
    SUM(tgt) AS tgt_wr,
    SUM(yds) AS yds_wr,
    SUM(td) AS td_wr,
    SUM(rec) AS rec_wr
  FROM dat_nfl_wr
  GROUP BY year, player, tm, week
  ORDER BY year, player, tm, week
")

dat_NFL_WR =
  dat_NFL_WR_SQL %>%
  mutate(Yds_Tgt_WR_Rate = yds_wr/tgt_wr,
         TD_Tgt_WR_Rate = td_wr/tgt_wr,
         Yds_Rec_WR_Rate = yds_wr/rec_wr,
         TD_Rec_WR_Rate = td_wr/rec_wr) %>%
  arrange(tm,year,week) %>%
  replace(is.na(.), 0) %>%
  group_by(player) %>%
  mutate(Yds_Tgt_WR_Rate_Roll = zoo::rollapply(Yds_Tgt_WR_Rate,roll_games,mean,align='right',fill=Yds_Tgt_WR_Rate),
         TD_Tgt_WR_Rate_Roll = zoo::rollapply(TD_Tgt_WR_Rate,roll_games,mean,align='right',fill=TD_Tgt_WR_Rate),
         Yds_Rec_WR_Rate_Roll = zoo::rollapply(Yds_Rec_WR_Rate,roll_games,mean,align='right',fill=Yds_Rec_WR_Rate),
         TD_Rec_WR_Rate_Roll = zoo::rollapply(TD_Rec_WR_Rate,roll_games,mean,align='right',fill=TD_Rec_WR_Rate),
         Tgt_WR_Roll = zoo::rollapply(tgt_wr,roll_games,mean,align='right',fill=tgt_wr),
         Rec_WR_Roll = zoo::rollapply(rec_wr,roll_games,mean,align='right',fill=rec_wr)) %>%
  mutate(tm = ifelse(tm == "LVR",'OAK',tm))

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
         TFL_Def_Rate_Roll = zoo::rollapply(tfl_def_rate,roll_games,mean,align='right',fill=tfl_def_rate)) %>%
  ungroup() %>%
  mutate(tm = ifelse(tm == "LVR",'OAK',tm))

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
         PD_Def_Rate_Roll = zoo::rollapply(pd_def_rate,roll_games,mean,align='right',fill=pd_def_rate)) %>%
  ungroup() %>%
  mutate(tm = ifelse(tm == "LVR",'OAK',tm))

dat_NFL_QB_train =
dat_NFL_QB %>%
  inner_join(dat_NFL_Def_Tackles,by = c('year','tm','week')) %>%
  inner_join(dat_NFL_Def_Int,by = c('year','tm','week')) %>%
  mutate(fpoints = yds_qb * 0.06 + td_qb*5) %>%
  select(player,year,tm,week,fpoints,ends_with('_Roll')) %>%
  as.data.table()

dat_NFL_RB_train =
  dat_NFL_RB %>%
  inner_join(dat_NFL_Def_Tackles,by = c('year','tm','week')) %>%
  inner_join(dat_NFL_Def_Int,by = c('year','tm','week')) %>%
  mutate(fpoints = yds_rb * 0.1 + td_rb*6) %>%
  select(player,year,tm,week,fpoints,ends_with('_Roll')) %>%
  as.data.table()

dat_NFL_WR_train =
  dat_NFL_WR %>%
  inner_join(dat_NFL_Def_Tackles,by = c('year','tm','week')) %>%
  inner_join(dat_NFL_Def_Int,by = c('year','tm','week')) %>%
  mutate(fpoints = yds_wr * 0.1 + td_wr*6 + rec_wr*1) %>%
  select(player,year,tm,week,fpoints,ends_with('_Roll')) %>%
  as.data.table()

# Load the NFL Schedule
dat_NFL_Schedule_2020 <-
rbind(
   query("
  SELECT
    week,
    win,
    lose
  FROM dat_nfl_schedule
") %>%
  as.data.table(),

  query("
  SELECT
    week,
    win,
    lose
  FROM dat_nfl_schedule
") %>%
  rename('lose' = 'win','win' = 'lose') %>%
  as.data.table())

dat_NFL_QB <-
  dat_NFL_QB %>%
  as.data.table()

dat_NFL_RB <-
  dat_NFL_RB %>%
  as.data.table()

dat_NFL_WR <-
  dat_NFL_WR %>%
  as.data.table()

dat_NFL_Def_Tackles <-
  dat_NFL_Def_Tackles %>%
  as.data.table()

dat_NFL_Def_Int <-
  dat_NFL_Def_Int %>%
  as.data.table()

dat_NFL_Schedule_2020_QB_Pred =
dat_NFL_Schedule_2020 %>%
  filter(week == wnum + 1) %>%
  full_join(dat_NFL_QB[week == wnum & year == 2020,], by = c('win' = 'tm')) %>%
  full_join(dat_NFL_Def_Tackles[week == wnum & year == 2020,],by = c('lose'='tm','week.y' = 'week')) %>%
  full_join(dat_NFL_Def_Int[week == wnum & year == 2020,],by = c('lose'='tm','week.y' = 'week')) %>%
  as.data.table()

dat_NFL_Schedule_2020_RB_Pred =
  dat_NFL_Schedule_2020 %>%
  filter(week == wnum + 1) %>%
  full_join(dat_NFL_RB[week == wnum & year == 2020,], by = c('win' = 'tm')) %>%
  full_join(dat_NFL_Def_Tackles[week == wnum & year == 2020,],by = c('lose'='tm','week.y' = 'week')) %>%
  full_join(dat_NFL_Def_Int[week == wnum & year == 2020,],by = c('lose'='tm','week.y' = 'week')) %>%
  as.data.table()

dat_NFL_Schedule_2020_WR_Pred =
  dat_NFL_Schedule_2020 %>%
  filter(week == wnum + 1) %>%
  full_join(dat_NFL_WR[week == wnum & year == 2020,], by = c('win' = 'tm')) %>%
  full_join(dat_NFL_Def_Tackles[week == wnum & year == 2020,],by = c('lose'='tm','week.y' = 'week')) %>%
  full_join(dat_NFL_Def_Int[week == wnum & year == 2020,],by = c('lose'='tm','week.y' = 'week')) %>%
  as.data.table()
  
### xgboost model
library(xgboost)
library(lme4)
library(Metrics)

# Pull rolling average data and point dfifference for the the model.

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

###################################################
# QB
###################################################

features = setdiff(names(dat_NFL_QB_train), c("player",  "week", "year","fpoints","Game_Number","tm"))
dtrain = xgb.DMatrix(as.matrix(dat_NFL_QB_train[, ..features]), label = dat_NFL_QB_train$fpoints)

N = nrow(dat_NFL_QB_train)
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
  smooth_model[[i]] = smooth.spline(x = xgb_cv$pred, y = dat_NFL_QB_train$fpoints)
}

### Build submission models

# Add prediction to training file
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

dtest = xgb.DMatrix(as.matrix(dat_NFL_QB_train[, ..features]))

probs = list()
for (i in 1:10) {
  preds = predict(submission_model[[i]], dtest)
  probs[[i]] = predict(smooth_model[[i]], preds)$y
}

#dat_NFL_Schedule_2020_Test$Pred = Reduce("+", probs) / 10
dat_NFL_QB_train$pred_fpoints = preds

dat_NFL_QB_train %>%
  ggplot() +
  geom_point(aes(x = fpoints, y=pred_fpoints))

#### Prediction file
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

dtest = xgb.DMatrix(as.matrix(dat_NFL_Schedule_2020_QB_Pred[, ..features]))

probs = list()
for (i in 1:10) {
  preds = predict(submission_model[[1]], dtest)
  probs[[i]] = predict(smooth_model[[i]], preds)$y
}

#dat_NFL_Schedule_2020_Test$Pred = Reduce("+", probs) / 10
dat_NFL_Schedule_2020_QB_Pred$pred_fpoints = preds

datRosterESPN_Proj <- fread("datRosterESPN_Proj.csv")

#dat_NFL_Schedule_2020_QB_Pred =
dat_NFL_Schedule_2020_QB_Pred %>%
  mutate(player = str_to_title(player)) %>%
  mutate(preds_fpoints_calc = Cmp_QB_Rate_Roll * Att_QB_Roll * Yds_QB_Rate_Roll*0.06 +  Cmp_QB_Rate_Roll * Att_QB_Roll *TD_QB_Rate_Roll *5) %>%
  full_join(datRosterESPN_Proj,by=c('player' = 'Player')) %>%
  rename(week = week.x, tm = win) %>%
  arrange(desc(pred_fpoints)) %>%
  filter(pred_fpoints > 5) %>%
  pivot_longer(cols =  c('pred_fpoints', 'preds_fpoints_calc', 'proj'),
               names_to = 'pred_type',
               values_to = 'pred_points',
               values_drop_na = TRUE) %>%
  #filter(pos == 'QB') %>%
  ggplot(aes(x=pred_points,y= reorder(player, pred_points, FUN = median))) +
  geom_boxplot(aes(fill = FFL_Team)) +
  xlab(label = paste("Predicted Points: Week =",wnum + 1,sep = " ")) +
  ylab(label = "Player") +
  ggtitle(label = "QB Preditions")



###########################################################################
# RB
###########################################################################

features = setdiff(names(dat_NFL_RB_train), c("player",  "week", "year","fpoints","Game_Number","tm"))
dtrain = xgb.DMatrix(as.matrix(dat_NFL_RB_train[, ..features]), label = dat_NFL_RB_train$fpoints)

N = nrow(dat_NFL_RB_train)
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
  smooth_model[[i]] = smooth.spline(x = xgb_cv$pred, y = dat_NFL_RB_train$fpoints)
}

### Build submission models

# Add prediction to training file
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

dtest = xgb.DMatrix(as.matrix(dat_NFL_RB_train[, ..features]))

probs = list()
for (i in 1:10) {
  preds = predict(submission_model[[i]], dtest)
  probs[[i]] = predict(smooth_model[[i]], preds)$y
}

#dat_NFL_Schedule_2020_Test$Pred = Reduce("+", probs) / 10
dat_NFL_RB_train$pred_fpoints = preds

dat_NFL_RB_train %>%
  ggplot() +
  geom_point(aes(x = fpoints, y=pred_fpoints))

#### Prediciton file
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

dtest = xgb.DMatrix(as.matrix(dat_NFL_Schedule_2020_RB_Pred[, ..features]))

probs = list()
for (i in 1:10) {
  preds = predict(submission_model[[1]], dtest)
  probs[[i]] = predict(smooth_model[[i]], preds)$y
}

#dat_NFL_Schedule_2020_Test$Pred = Reduce("+", probs) / 10
dat_NFL_Schedule_2020_RB_Pred$pred_fpoints = preds

dat_NFL_Schedule_2020_RB_Pred %>%
  mutate(player = str_to_title(player)) %>%
  mutate(preds_fpoints_calc = Att_RB_Roll *  Yds_RB_Rate_Roll*0.1 +  Att_RB_Roll *TD_RB_Rate_Roll *6) %>%
  full_join(datRosterESPN_Proj,by=c('player' = 'Player')) %>%
  rename(week = week.x, tm = win) %>%
  arrange(desc(pred_fpoints)) %>%
  filter(pred_fpoints > 5) %>%
  pivot_longer(cols =  c('pred_fpoints', 'preds_fpoints_calc', 'proj'),
               names_to = 'pred_type',
               values_to = 'pred_points',
               values_drop_na = TRUE) %>%
  #filter(pos == 'QB') %>%
  ggplot(aes(x=pred_points,y= reorder(player, pred_points, FUN = median))) +
  geom_boxplot(aes(fill = FFL_Team)) +
  xlab(label = paste("Predicted Points: Week =",wnum + 1,sep = " ")) +
  ylab(label = "Player") +
  ggtitle(label = "RB Preditions")

####################################################################
# WR
####################################################################

features = setdiff(names(dat_NFL_WR_train), c("player",  "week", "year","fpoints","Game_Number","tm"))
dtrain = xgb.DMatrix(as.matrix(dat_NFL_WR_train[, ..features]), label = dat_NFL_WR_train$fpoints)

N = nrow(dat_NFL_WR_train)
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
  smooth_model[[i]] = smooth.spline(x = xgb_cv$pred, y = dat_NFL_WR_train$fpoints)
}

### Build submission models

# Add prediction to training file
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

dtest = xgb.DMatrix(as.matrix(dat_NFL_WR_train[, ..features]))

probs = list()
for (i in 1:10) {
  preds = predict(submission_model[[i]], dtest)
  probs[[i]] = predict(smooth_model[[i]], preds)$y
}

#dat_NFL_Schedule_2020_Test$Pred = Reduce("+", probs) / 10
dat_NFL_WR_train$pred_fpoints = preds

dat_NFL_WR_train %>%
  ggplot() +
  geom_point(aes(x = fpoints, y=pred_fpoints))

#### Prediciton file
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

dtest = xgb.DMatrix(as.matrix(dat_NFL_Schedule_2020_WR_Pred[, ..features]))

probs = list()
for (i in 1:10) {
  preds = predict(submission_model[[1]], dtest)
  probs[[i]] = predict(smooth_model[[i]], preds)$y
}

#dat_NFL_Schedule_2020_Test$Pred = Reduce("+", probs) / 10
dat_NFL_Schedule_2020_WR_Pred$pred_fpoints = preds

dat_NFL_Schedule_2020_WR_Pred %>%
  mutate(player = str_to_title(player)) %>%
  mutate(preds_fpoints_calc = Rec_WR_Roll *  Yds_Rec_WR_Rate_Roll*0.1 +  Rec_WR_Roll *TD_Rec_WR_Rate_Roll *6 + Rec_WR_Roll*1) %>%
  full_join(datRosterESPN_Proj,by=c('player' = 'Player')) %>%
  rename(week = week.x, tm = win) %>%
  arrange(desc(pred_fpoints)) %>%
  filter(pred_fpoints > 8) %>%
  pivot_longer(cols =  c('pred_fpoints', 'preds_fpoints_calc', 'proj'),
               names_to = 'pred_type',
               values_to = 'pred_points',
               values_drop_na = TRUE) %>%
  #filter(pos == 'WR') %>%
  ggplot(aes(x=pred_points,y= reorder(player, pred_points, FUN = median))) +
  geom_boxplot(aes(fill = FFL_Team)) +
  xlab(label = paste("Predicted Points: Week =",wnum + 1,sep = " ")) +
  ylab(label = "Player") +
  ggtitle(label = "WR Preditions")

