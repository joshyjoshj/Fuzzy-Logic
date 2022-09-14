library(tidyverse)
library(lubridate)
library(janitor)
library(reshape2)
library(RWeka)
library(rJava)

ticker <- "SPY"
data <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/historical-price-full/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker))$historical
data <- data%>%arrange(date)

dates <- data$date


date1 <- date2 <- "2022-08-05"

min_data <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v4/historical-price/%s/%s/minute/%s/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker,15,date1,date2))$results%>%
  mutate(date = substr(formated,1,10),time=substr(formated,12,19))%>%
  rename(high = h, low = l, close = c, open = o)

min_data <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v4/historical-price/%s/%s/day/%s/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker,1,date1,date2))$results%>%
  mutate(date = substr(formated,1,10),time=substr(formated,12,19))%>%
  rename(high = h, low = l, close = c, open = o)

#

##### Fuzzy Funtions ####
b <- function(open,low,high,close){
  out <- abs((open-close)/(high-low))
  out
}

us <- function(open,low,high,close){
  ifelse({open > close},{out <- abs((high-open)/(high-low))},{out <- abs((high-close)/(high-low))})
  out
}

ls <- function(open,low,high,close){
  ifelse({open > close},{out <- abs((close-low)/(high-low))},{out <- abs((open-low)/(high-low))})
  out
}

mu_very_short <- function(x){
  case_when(
    x <= 0.05 ~ 1-20*x,
    x > 0.05 ~ 0
  )
}

mu_short <- function(x){
  case_when(
    x <= 0.1 ~ 10*x,
    (0.1 < x & x <= 0.3) ~ 1,
    (0.3 < x & x <= 0.4) ~ 2.5-5*x,
    0.4 < x ~ 0
  )
}

mu_medium <- function(x){
  case_when(
    (x <= 0.3 | x > 0.7) ~ 0,
    (0.3 < x & x <= 0.4) ~ 10*x-3,
    (0.4 < x & x <= 0.6) ~ 1,
    (0.6 < x & x <= 0.7) ~ 7-10*x
  )
}

mu_long <- function(x){
  case_when(
    x <= 0.5 ~ 0,
    (0.5 < x & x <=0.7) ~ 5*x-2.5,
    (0.7 < x & x <= 0.9) ~ 1,
    (0.9 < x) ~ 10-10*x
  )
}

mu_very_long <- function(x){
  case_when(
    x <= 0.9 ~ 0,
    0.9 < x ~ 10*x-9
  )
}

fuzzy_set <- function(x){
  vs <- mu_very_short(x)
  s <- mu_short(x)
  m <- mu_medium(x)
  l <- mu_long(x)
  vl <- mu_very_long(x)
  set <- c("3" = vs, "4" = s, "5" = m, "6" = l, "7" = vl)
  out <- names(which.max(set))
  out
}


right_linear <- function(x,a,b){
  out <- case_when(
    (x < a) ~ 1,
    (a <= x & x < b) ~ (b-x)/(b-a),
    (x > b) ~ 0
  )
  if(is.nan(out)){out <- 0}
  out
}


left_linear <- function(x,a,b){
  out <- case_when(
    (x < a) ~ 0,
    (a <= x & x < b) ~ (x-a)/(b-a),
    (x > b) ~ 1
  )
  if(is.nan(out)){out <- 0}
  out
}

triangle <- function(x,a,b,c){
  out <- case_when(
    (x < a) ~ 0,
    (a <= x & x <= b) ~ (x-a)/(b-a),
    (b < x & x <= c) ~ (c-x)/(c-b),
    (x > c) ~ 0
  )
  if(is.nan(out)){out <- 0}
  out
}


mu_open_low <- function(open,prev_high,prev_low){
  right_linear(open,prev_low,prev_high)
}

mu_open_equal_low <- function(open,prev_open,prev_close,prev_high,prev_low){
  triangle(open,prev_low,min(prev_open,prev_close),prev_high)
}

mu_open_equal_high <- function(open,prev_open,prev_close,prev_high,prev_low){
  triangle(open,prev_low,max(prev_open,prev_close),prev_high)
}

mu_open_high <- function(open,prev_high,prev_low){
  left_linear(open,prev_low,prev_high)
}

open_style <- function(open,prev_open,prev_close,prev_high,prev_low){
  o_l <- mu_open_low(open,prev_high,prev_low)
  o_e_l <- mu_open_equal_low(open,prev_open,prev_close,prev_high,prev_low)
  o_e_h <- mu_open_equal_high(open,prev_open,prev_close,prev_high,prev_low)
  o_h <- mu_open_high(open,prev_high,prev_low)
  set <- c("8" = o_l, "9" = o_e_l, "10" = o_e_h, "11" = o_h)
  out <- names(which.max(set))
  out
}



mu_close_low <- function(close,prev_high,prev_low){
  right_linear(close,prev_low,prev_high)
}

mu_close_equal_low <- function(close,prev_open,prev_close,prev_high,prev_low){
  triangle(close,prev_low,min(prev_open,prev_close),prev_high)
}

mu_close_equal_high <- function(close,prev_open,prev_close,prev_high,prev_low){
  triangle(close,prev_low,max(prev_open,prev_close),prev_high)
}

mu_close_high <- function(close,prev_high,prev_low){
  left_linear(close,prev_low,prev_high)
}

close_style <- function(close,prev_open,prev_close,prev_high,prev_low){
  c_l <- mu_close_low(close,prev_high,prev_low)
  c_e_l <- mu_close_equal_low(close,prev_open,prev_close,prev_high,prev_low)
  c_e_h <- mu_close_equal_high(close,prev_open,prev_close,prev_high,prev_low)
  c_h <- mu_close_high(close,prev_high,prev_low)
  set <- c("12" = c_l, "13" = c_e_l, "14" = c_e_h, "15" = c_h)
  out <- names(which.max(set))
  out
}


#Body colour

body_colour <- function(open,close){
  case_when(
    (open > close) ~ "16",
    (close > open) ~ "17",
    (open == close) ~ "18"
  )
}

#####


create_fuzzy_vars <- function(data){
  
  data <- data%>%mutate(high = ifelse({high==low},{high+0.01},{high}))
  
  data <- data%>%mutate(b = b(open,low,high,close),us = us(open,low,high,close), ls = ls(open,low,high,close))
  
  #Fuzzying lines
  
  data <- data%>%group_by(time)%>%mutate(k1_b = fuzzy_set(b), k1_us = fuzzy_set(us), k1_ls = fuzzy_set(ls))%>%ungroup()
  
  #Fuzzying line relations
  
  data <- data%>%group_by()%>%mutate(prev_open = lag(open,1), prev_close = lag(close,1), prev_high = lag(high,1), prev_low = lag(low,1))%>%drop_na()%>%ungroup()

  data <- data%>%group_by(time)%>%mutate(k1_open_style = open_style(open,prev_open,prev_close,prev_high,prev_low), k1_close_style = close_style(close,prev_open,prev_close,prev_high,prev_low))%>%ungroup()
  
  #Body Colour 
  
  data <- data%>%mutate(k1_body_colour = body_colour(open,close))
  
  data <- data%>%melt(id.vars = c("time"),measure.vars = c("k1_b","k1_us","k1_ls","k1_open_style","k1_close_style","k1_body_colour"))%>%
    mutate(variable = paste(time,variable,sep="_"))%>%
    select(variable,value)%>%
    spread(variable,value)%>%
    clean_names()
  
  data
  
}

create_fuzzy_vars_2 <- function(data){
  
  data <- data%>%mutate(high = ifelse({high==low},{high+0.01},{high}))
  
  data <- data%>%mutate(b = b(open,low,high,close),us = us(open,low,high,close), ls = ls(open,low,high,close))
  
  #Fuzzying lines
  
  data <- data%>%group_by(date)%>%mutate(k1_b = fuzzy_set(b), k1_us = fuzzy_set(us), k1_ls = fuzzy_set(ls))%>%ungroup()
  
  #Fuzzying line relations
  
  data <- data%>%group_by()%>%mutate(prev_open = lag(open,1), prev_close = lag(close,1), prev_high = lag(high,1), prev_low = lag(low,1))%>%drop_na()%>%ungroup()
  
  data <- data%>%group_by(date)%>%mutate(k1_open_style = open_style(open,prev_open,prev_close,prev_high,prev_low), k1_close_style = close_style(close,prev_open,prev_close,prev_high,prev_low))%>%ungroup()
  
  #Body Colour 
  
  data <- data%>%mutate(k1_body_colour = body_colour(open,close))%>%mutate(day = rev(seq(1,4,1)))
  
  data <- data%>%melt(id.vars = c("day"),measure.vars = c("k1_b","k1_us","k1_ls","k1_open_style","k1_close_style","k1_body_colour"))%>%
    mutate(variable = paste(day,variable,sep="_"))%>%
    select(variable,value)%>%
    spread(variable,value)%>%
    clean_names()
  
  data
  
}

classifier <- function(data){
  o <- data$open[1]
  c <- data$close[nrow(data)]
  h <- max(data$high)
  l <- min(data$low)
  h_t <- which.max(data$high)
  l_t <- which.min(data$low)
  direction <- which.max(c((o-l),(h-o)))-1
  return <- ifelse({c/o < 1},{0},{1})
  time <- ifelse({h_t < l_t},{0},{1})
  score <- mean(c(direction,return,time))
  class <- ifelse({score == 0},{"0"},{ifelse({score == 1},{"1"},{"2"})})
  class
  
}


for (i in 1:(length(dates)-5)) {
  date1 <- dates[i+5]
  date2 <- dates[i+4]
  date3 <- dates[i]
  
  prev_day <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v4/historical-price/%s/%s/minute/%s/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker,240,date2,date2))$results%>%
    mutate(date = substr(formated,1,10),time=substr(formated,12,19))%>%
    rename(high = h, low = l, close = c, open = o)
  if(nrow(prev_day) != 4){next}
  
  
  
  current_day <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v4/historical-price/%s/%s/minute/%s/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker,15,date1,date1))$results%>%
    mutate(date = substr(formated,1,10),time=substr(formated,12,19))%>%
    rename(high = h, low = l, close = c, open = o)
  if(nrow(current_day) != 64){next}
  
  
  
  daily <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v4/historical-price/%s/%s/day/%s/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker,1,date3,date2))$results%>%
    mutate(date = substr(formated,1,10),time=substr(formated,12,19))%>%
    rename(high = h, low = l, close = c, open = o)
  
  if(nrow(daily) != 5){next}
  
  
  prev_day_fuzzy <- create_fuzzy_vars(prev_day)
  colnames(prev_day_fuzzy) <- paste("prev_day", colnames(prev_day_fuzzy), sep = "_")
  
  current_day_fuzzy <- current_day%>%filter(time < "09:30:00")
  current_day_fuzzy <- create_fuzzy_vars(current_day_fuzzy)
  colnames(current_day_fuzzy) <- paste("current_day", colnames(current_day_fuzzy), sep = "_")
  
  daily_fuzzy <- create_fuzzy_vars_2(daily)
  colnames(daily_fuzzy) <- paste("daily", colnames(daily_fuzzy), sep = "_")
  
  class <- classifier(current_day%>%filter("09:30:00"<=time&time<="10:15:00"))
  class_df <- data.frame(class = class,date = date1)
  
  out_df <- bind_cols(class_df,prev_day_fuzzy,current_day_fuzzy,daily_fuzzy)
  
  ifelse({i==1},{out <- out_df},{out <- rbind(out,out_df)})
  
  
}


out[is.na(out)] = 18

out <- as

saveRDS(out,file="Use this tmor.rds")

out <- readRDS("~/Use this tmor.rds")


t <- apply(out,2,strtoi,base = 32)

strtoi(out[,1],32L)
library(tidymodels)

mod_data <- out%>%mutate_if(is.character, as.factor)


mod_data_split <- initial_split(mod_data,strata = class)
mod_data_train <- training(mod_data_split)
mod_data_test <- testing(mod_data_split)

mod_df <- mod_data_train%>%select(-date)

WPM("refresh-cache")
WPM("install-package","alternatingDecisionTrees")


ADT <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")


mod.adt <- ADT(class ~., data = mod_df)


pred <- bind_cols(pred = predict(mod.adt,mod_data_test,type = "prob"),select(mod_data_test,class,date))%>%mutate(e = pred==class)


pred%>%filter(pred != "NA")%>%summarise(accuracy = mean(e))


mod.adt


t <- mod_df%>%select_if(is.factor)
