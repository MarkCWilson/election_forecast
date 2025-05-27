## vote_share_predict.R
## Mark C. Wilson <wilson.mark.c@gmail.com>
##
## Purpose: predict vote shares from historical vote data and polling data
##

library(tidyverse)
library(tidymodels)

library("ranger")
library("xgboost")

#library("LiblineaR")

#####################################################
##### fitting models to vote share, history only

# run vote_clean.R first

temp <-vote_data_clean %>% 
  # drop_na() %>%
  mutate(win = case_when(
    DEMOCRAT == pmax(DEMOCRAT, REPUBLICAN, OTHER) ~ "D",
    REPUBLICAN == pmax(DEMOCRAT, REPUBLICAN, OTHER) ~ "R",
    OTHER == pmax(DEMOCRAT, REPUBLICAN, OTHER) ~ "O"),
    lagwin = case_when(
      lagDEM == pmax(lagDEM,lagREP,lagOTH) ~ "D",
      lagREP == pmax(lagDEM,lagREP,lagOTH) ~ "R",
      lagOTH == pmax(lagDEM,lagREP,lagOTH) ~ "O"),
    win = as.factor(win), lagwin = as.factor(lagwin)) %>%
  mutate(demfrac = DEMOCRAT/(totalvotes),
         repfrac = REPUBLICAN/(totalvotes),othfrac = OTHER/totalvotes,
         lagDfrac = lagDEM/(totalvotes), lagRfrac = lagREP/(totalvotes),
         lagOfrac = lagOTH/totalvotes) %>% drop_na()

#set.seed(222)
# split into training and testing 
#data_split <- initial_split(temp2, prop = 3/4)

# create data frames for the two sets
#train_data <- training(data_split)
#test_data  <- testing(data_split)

tempt <- temp %>% select(year:fips | demfrac:lagOfrac  )
train_data <- tempt %>% filter(!year ==2020) 
test_data <- tempt %>% filter(year==2020)

# make recipe
vote_rec_dem <- 
  recipe(demfrac ~ ., data = train_data) %>% 
  update_role(year,fips,repfrac,new_role = "ID") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

vote_rec_rep <- 
  recipe(repfrac ~ ., data = train_data) %>% 
  update_role(year,fips,demfrac,new_role = "ID") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

mods = c("LinR","XG","RF")

party <-rep(c("D", "R"), each = 4)

# linear regression model

mod <- function(m) {
  
if(m=="LinR") 
  {
  meth <- 
  linear_reg(
      mode = "regression",
      engine = "lm",
      penalty = NULL,
      mixture = 1
  )
  }
else if(m=="XG")
  {
  meth <- boost_tree(
    mode = "regression",
    engine = "xgboost",
    mtry = NULL,
    trees = NULL,
    min_n = NULL,
    tree_depth = NULL,
    learn_rate = NULL,
    loss_reduction = NULL,
    sample_size = NULL,
    stop_iter = NULL
  )
  }
  else if (m=="RF")
  {
  meth <- rand_forest(mtry = 2, trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")
  }
  
return(meth)
}

pred <- function(m) {

  # make workflow
  vote_wf_dem <- 
    workflow() %>% 
    add_model(mod(m)) %>% 
    add_recipe(vote_rec_dem)
  
  vote_wf_rep <- 
    workflow() %>% 
    add_model(mod(m)) %>% 
    add_recipe(vote_rec_rep)
  
  # fit model on training data
  vote_fit_dem <- 
    vote_wf_dem %>% 
    fit(data = train_data)
  
  vote_fit_rep <- 
    vote_wf_rep %>% 
    fit(data = train_data)
  
#  vote_fit_dem %>% 
#    extract_fit_parsnip() %>% 
#    tidy() 
  
  # predict and evaluate
  vote_aug_dem <- 
    augment(vote_fit_dem, test_data)
  vote_aug_rep <- 
    augment(vote_fit_rep, test_data)
  vote_pred <- inner_join(vote_aug_dem,vote_aug_rep, 
                              by = join_by(year, demfrac,repfrac,othfrac,fips, lagDfrac, 
                                           lagRfrac)) %>% 
    mutate(predD = .pred.x,predR = .pred.y) %>%
    select(year,fips,predD,predR,demfrac,repfrac)
 
  p_dem <- ggplot(vote_aug_dem, aes(y = demfrac, x = .pred)) + geom_point(color = "blue")
  p_rep <- ggplot(vote_aug_rep, aes(y = repfrac, x = .pred)) + geom_point(color = "red")
  show(p_dem)
  show(p_rep) # present these better
  
  return(vote_pred)
}
 
mets <- function(pred) {
  mets <- metric_set(rsq, rmse, mae, smape)
  algo <- rep(m, each = 8) # how to do generally with number of metrics?
  
  results_vote_dem <- pred %>% mets(demfrac, estimate = predD)
  results_vote_rep <- pred %>% mets(repfrac, estimate = predR)
  
  metrics_vote <- cbind(algo,party,rbind(results_vote_dem, results_vote_rep))
  metrics_vote <-as_tibble(metrics_vote)
  #show(metrics_vote)
  return(metrics_vote)
}

# report results
t1<-tibble(algo="",party="",metric="",estimator="",estimate=0.0)[0,]
for(m in mods) {
  out <- mets(pred(m))
  t1<- rbind(t1,out)
}

show(t1)

#######################################################
##### now fitting models to vote share, history and polling

### add new predictors e.g. swing

sw <- function(v0,vs0,d,str) {
  if(str=="ups") {
    di <- d    
  }
  else if(str=="prop") {
    di <- d*v0/vs0
  }
  else if(str=="pw") {
    ifelse (d>0, di <- d*(1-v0)/(1-vs0), di <- d*v0/vs0)
  }
  return (di) #  Estimate of district level swing
}

### polling data

poll <- cbind(c(2000,2004,2008,2012,2016,2020), 
              c(0.48,0.469,0.521,0.488,0.468, 0.512), 
              c(0.46, 0.489, 0.445,0.481,0.436, 0.440))
colnames(poll) <- c("year","pollDfrac", "pollRfrac")
poll <- as_tibble(poll)

### compute swing

vote_swing <- 
  vote_data_clean %>% group_by(year) %>% 
  summarize(Dtot = sum(DEMOCRAT), Rtot = sum(REPUBLICAN), Otot = sum(OTHER),
            Dfrac = Dtot/(Dtot+Rtot+Otot), 
            Rfrac = Rtot/(Dtot+Rtot+Otot)) 

vote_swing <- merge(vote_swing, poll) %>%
  select(year, Dfrac:pollRfrac) %>% 
  mutate(swingD = Dfrac - lag(Dfrac, n=1),
         swingR = Rfrac - lag(Rfrac, n=1),
         pollswingD = pollDfrac - lag(Dfrac, n=1),
         pollswingR = pollRfrac - lag(Rfrac, n=1))

temp2 <- merge(temp,vote_swing) %>% 
  mutate(predDfrac = lagDfrac + sw(lagDfrac,Dfrac,swingD, "pw"),
         predpollDfrac = lagDfrac + sw(lagDfrac,Dfrac,pollswingD, "pw"),
         predpollDfrac2 = lagDfrac + sw(lagDfrac,Dfrac,pollswingD, "ups"),
         predRfrac = lagRfrac + sw(lagRfrac,Rfrac,swingR, "pw"),
         predpollRfrac = lagRfrac + sw(lagRfrac,Rfrac,pollswingR, "pw"),
         predpollRfrac2 = lagRfrac + sw(lagRfrac,Rfrac,pollswingR, "ups"))

temp2t <- temp2 %>% select(year:fips | win:othfrac 
                           | lagDfrac:lagOfrac | swingD:predpollRfrac2)
train_data <- temp2t %>% filter(year<2020)
test_data <- temp2t %>% filter(year==2020)

# make recipe
vote_rec_dem <- 
  recipe(demfrac ~ ., data = train_data) %>% 
  update_role(year, fips, win, lagwin, repfrac, othfrac, swingD, predpollDfrac, 
              predpollRfrac, new_role = "ID") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

vote_rec_rep <- 
  recipe(repfrac ~ ., data = train_data) %>% 
  update_role(year, fips, win, lagwin, demfrac, othfrac, swingD, predpollRfrac, 
              predpollDfrac, new_role = "ID") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

# report results

t2<-tibble(algo="",party="",.metric="",.estimator="",.estimate=0.0)[0,]
for(m in mods) {
  out <- mets(pred(m))
  t2<- rbind(t2,out)
}

show(t2)