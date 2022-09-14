library(tidyverse)
library(lubridate)
library(janitor)
library(reshape2)
library(RWeka)
library(rJava)
library(tidymodels)




Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_341')

#### Attempt 1 ####

ticker <- "SPY"
data <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/historical-price-full/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker))$historical
data <- data%>%arrange(date)


data <- data%>%mutate(h1 = lag(high,1), h2 = lag(high,2), h3 = lag(high,3), h4 = lag(high,4), h5 = lag(high,5),
              l1 = lag(low,1), l2 = lag(low,2), l3 = lag(low,3), l4 = lag(low,4), l5 = lag(low,5))%>%
  mutate(h_5 = pmax(h1,h2,h3,h4,h5),l_5 = pmin(l1,l2,l3,l4,l5))%>%
  mutate(rsv = (close - l_5)/(h_5 - l_5)*100)%>%
  select(-h1,-h2,-h3,-h4,-h5,-l1,-l2,-l3,-l4,-l5,-h_5,-l_5)%>%
  mutate(k = 50)%>%mutate(k = 1/3*rsv + 2/3*lag(k,1))%>%
  mutate(d = 50)%>%mutate(d = 1/3*k + 2/3*lag(d,1))%>%
  mutate(x_negt  = (lag(close,3)+lag(close,2)+lag(close,1)+close+lead(close,1)+lead(close,2))/6,
         x_post  = (lag(close,2)+lag(close,1)+close+lead(close,1)+lead(close,2)+lead(close,3))/6)%>%
  mutate(cma  = (x_negt+x_post)/2)%>%
  mutate(trend = ifelse({(lag(cma,6)>lag(cma,5)) & (lag(cma,5)>lag(cma,4)) & (lag(cma,4)>lag(cma,3)) & (lag(cma,3)>lag(cma,2)) & (lag(cma,2)>lag(cma,1)) & (lag(cma,2)>cma)},
                        {"down"},
                        {ifelse({(lag(cma,6)<lag(cma,5)) & (lag(cma,5)<lag(cma,4)) & (lag(cma,4)<lag(cma,3)) & (lag(cma,3)<lag(cma,2)) & (lag(cma,2)<lag(cma,1)) & (lag(cma,2)<cma)},{"up"},{"none"})}))
  





test <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/historical-chart/15min/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker))%>%
  mutate(date1 = substr(date,1,10),time=substr(date,12,19))
data <- test%>%arrange(date1,time)






#Fuzzyizing 

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


data <- data%>%mutate(b = b(open,low,high,close),us = us(open,low,high,close), ls = ls(open,low,high,close))

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
  set <- c("veryshort" = vs, "short" = s, "medium" = m, "long" = l, "verylong" = vl)
  out <- names(which.max(set))
  out
}

names(which.max(fuzzy_set(0.89)))


data%>%group_by(date)%>%mutate(test = fuzzy_set(b))

data <- data%>%mutate(k_b_vs = mu_very_short(b),
                      k_b_s = mu_short(b),
                      k_b_m = mu_medium(b),
                      k_b_l = mu_long(b),
                      k_b_vl = mu_very_long(b),
                      k_us_vs = mu_very_short(us),
                      k_us_s = mu_short(us),
                      k_us_m = mu_medium(us),
                      k_us_l = mu_long(us),
                      k_us_vl = mu_very_long(us),
                      k_ls_vs = mu_very_short(ls),
                      k_ls_s = mu_short(ls),
                      k_ls_m = mu_medium(ls),
                      k_ls_l = mu_long(ls),
                      k_ls_vl = mu_very_long(ls)
                      )

data <- data%>%group_by(date)%>%mutate(k1_b = fuzzy_set(b), k1_us = fuzzy_set(us), k1_ls = fuzzy_set(ls))%>%ungroup()%>%
  mutate(k2_b = lag(k1_b,1), k3_b = lag(k1_b,2), k4_b = lag(k1_b,3), k5_b =lag(k1_b,4),
         k2_us = lag(k1_us,1), k3_us = lag(k1_us,2), k4_us = lag(k1_us,3), k5_us =lag(k1_us,4),
         k2_ls = lag(k1_ls,1), k3_ls = lag(k1_ls,2), k4_ls = lag(k1_ls,3), k5_ls =lag(k1_ls,4))%>%drop_na()






####Modlling 

library(tidymodels)
library(kernlab)



mod_data <- data%>%mutate(sig = ifelse({(high > lag(high,1)) & (high > lag(high,2)) & (high > lag(high,3)) & (high > lag(high,4)) & (high > lag(high,4)) &
    (high > lead(high,1)) & (high > lead(high,2)) & (high > lead(high,3))  & (high > lead(high,4)) & (high > lead(high,5)) },{"Short"},{"na"}))%>%
  mutate(sig=factor(sig))%>%drop_na()

?drop_na


mod_data%>%select(date,sig)%>%view()


library(liblin)


mod_data_split <- initial_split(mod_data,strata = sig)
mod_data_train <- training(mod_data_split)
mod_data_test <- testing(mod_data_split)

tune_spec <- 
  svm_linear(
    cost = tune()
  ) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

tree_grid <- grid_regular(cost(),
                          levels = 5)


mod_data_folds <- vfold_cv(mod_data_train)



tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(sig ~ k1_b+k2_b+k3_b+k4_b+k5_b)

tree_res <- 
  tree_wf %>% 
  tune_grid(
    resamples = mod_data_folds,
    grid = tree_grid
  )


best_tree <- tree_res %>%
  select_best("accuracy")


final_wf <- 
  tree_wf %>% 
  finalize_workflow(best_tree)

final_fit <- 
  final_wf %>%
  last_fit(mod_data_split)

final_fit %>%
  collect_metrics()

final_fit %>%
  collect_predictions() %>% 
  view()

final_tree <- extract_workflow(final_fit)

final_tree %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)


library(vip)

final_tree %>% 
  extract_fit_parsnip() %>% 
  vip()


mod_data%>%select(date,k_b_vs,k_us_vs,k_ls_vs)%>%filter(k_b_vs > 0 | k_ls_vs > 0 | k_us_vs >0)%>%view()





library(e1071)

k_b_vs+k_b_s+k_b_m+k_b_l+k_b_vl

svmfit <- svm(sig ~ k_b_vs+k_b_s+k_b_m+k_b_l+k_b_vl, data = mod_data_train, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit,mod_data_test, k_b_vs ~ k_b_s)

predict(svmfit,mod_data_test)


ggplot(mod_data,mapping=aes(x=k_b_l,y=k_b_s,colour=sig))+
  geom_point()

#### Attempt 2 ####


#Data 1
ticker <- "SPY"
data <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/historical-price-full/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker))$historical
data <- data%>%arrange(date)

#Data 2
test <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/historical-chart/15min/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker))%>%
  mutate(date1 = substr(date,1,10),time=substr(date,12,19))
data <- test%>%arrange(date1,time)


#Data 3

ticker <- "SPY"
data <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/historical-price-full/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker))$historical


dates <- data$date

for (i in 1:length(dates)) {
  date <- dates[1]
  time <- 15
  min_data <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v4/historical-price/%s/%s/minute/%s/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker,time,date,date))$results%>%
    mutate(date = substr(formated,1,10),time=substr(formated,12,19))
  ifelse({i==1},{out <- min_data},{out <- rbind(min_data,out)})
  
}

data <- out%>%rename(open = o,close = c, high = h, low = l)

data <- data%>%group_by(date)%>%mutate(short = ifelse({(high > lag(high,1)) & (high > lag(high,2)) & (high > lag(high,3)) & (high > lag(high,4)) & (high > lag(high,4)) &
    (high > lead(high,1)) & (high > lead(high,2)) & (high > lead(high,3))  & (high > lead(high,4)) & (high > lead(high,5)) & (high > lead(high,6)) &
    (high > lead(high,7)) & (high > lead(high,8))},{"HOD"},{"na"}))%>%
  mutate(short=factor(short))%>%drop_na()%>%ungroup()



data <- data%>%group_by(date)%>%mutate(long = ifelse({(low < lag(low,1)) & (low < lag(low,2)) & (low < lag(low,3)) & (low < lag(low,4)) & (low < lag(low,4)) &
    (low < lead(low,1)) & (low < lead(low,2)) & (low < lead(low,3))  & (low < lead(low,4)) & (low < lead(low,5)) & (low < lead(low,6)) &
    (low < lead(low,7)) & (low < lead(low,8))},{"LOD"},{"na"}))%>%
  mutate(long=factor(long))%>%drop_na()%>%ungroup()



data <- data%>%mutate(x_negt  = (lag(close,3)+lag(close,2)+lag(close,1)+close+lead(close,1)+lead(close,2))/6,
                        x_post  = (lag(close,2)+lag(close,1)+close+lead(close,1)+lead(close,2)+lead(close,3))/6)%>%
  mutate(cma  = (x_negt+x_post)/2)%>%
  mutate(trend = ifelse({(lag(cma,6)>lag(cma,5)) & (lag(cma,5)>lag(cma,4)) & (lag(cma,4)>lag(cma,3)) & (lag(cma,3)>lag(cma,2)) & (lag(cma,2)>lag(cma,1)) & (lag(cma,2)>cma)},
                        {"down"},
                        {ifelse({(lag(cma,6)<lag(cma,5)) & (lag(cma,5)<lag(cma,4)) & (lag(cma,4)<lag(cma,3)) & (lag(cma,3)<lag(cma,2)) & (lag(cma,2)<lag(cma,1)) & (lag(cma,2)<cma)},{"up"},{"none"})}))




data%>%filter(date=="2022-08-15")%>%
  ggplot()+
  geom_segment(aes(x=time,xend=time,y=open,yend=close,colour=long),size=1.5)+
  geom_segment(aes(x=time,xend=time,y=high,yend=low,colour=long))+
  theme(axis.text.x = element_text(angle=90))

#Data 4

tickers <- c("SPY","AAPL","AMZN","NVDA","GOOG")

for (i in 1:length(dates)) for (j in 1:length(tickers)){
  date <- dates[i]
  ticker <- tickers[j]
  time <- 30
  min_data <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v4/historical-price/%s/%s/minute/%s/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker,time,date,date))$results%>%
  mutate(date = substr(formated,1,10),time=substr(formated,12,19))%>%mutate(ticker = ticker)
  ifelse({i==1&j==1},{out <- min_data},{out <- rbind(min_data,out)})
}

data <- out%>%rename(open = o,close = c, high = h, low = l)

data <- data%>%group_by(date,ticker)%>%mutate(short = ifelse({(high > lag(high,1)) & (high > lag(high,2)) & (high > lag(high,3)) & (high > lag(high,4)) & (high > lag(high,4)) &
    (high > lead(high,1)) & (high > lead(high,2)) & (high > lead(high,3))  & (high > lead(high,4)) & (high > lead(high,5)) },{"Short"},{"na"}))%>%
  mutate(short=factor(short))%>%drop_na()%>%ungroup()

saveRDS(out, file="FAANG 15min.rds")

#Data 5 

tickers <- c("SPY","TQQQ","SQQQ","QQQ","AAPL","TSLA","GOOG","NVDA","AMZN")

for (i in 1:length(tickers)) {
  ticker <- tickers[i]
  df <- (jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/historical-price-full/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker))$historical)%>%
    arrange(date)%>%mutate(ticker = ticker)
  ifelse({i==1},{out <- df},{out <- rbind(df,out)})
}

data <- out

data <- data%>%group_by(ticker)%>%mutate(short = ifelse({(high > lag(high,1)) & (high > lag(high,2)) & (high > lag(high,3)) & (high > lag(high,4)) & (high > lag(high,4)) &
    (high > lead(high,1)) & (high > lead(high,2)) & (high > lead(high,3))  & (high > lead(high,4)) & (high > lead(high,5)) },{"Short"},{"na"}))%>%
  mutate(short=factor(short))%>%drop_na()%>%ungroup()


#Data 6 
tickers <- jsonlite::fromJSON("https://financialmodelingprep.com/api/v3/sp500_constituent?apikey=c34a828bdd72b3933d449ea419de9562")$symbol

for (i in 1:length(tickers)) {
  ticker <- tickers[i]
  df <- (jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/historical-price-full/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker))$historical)%>%
    arrange(date)%>%mutate(ticker = ticker)
  ifelse({i==1},{out <- df},{out <- rbind(df,out)})
}

data <- out

data <- data%>%group_by(ticker)%>%mutate(short = ifelse({(high > lag(high,1)) & (high > lag(high,2)) & (high > lag(high,3)) & (high > lag(high,4)) & (high > lag(high,4)) &
    (high > lead(high,1)) & (high > lead(high,2)) & (high > lead(high,3))  & (high > lead(high,4)) & (high > lead(high,5)) },{"Short"},{"na"}))%>%
  mutate(short=factor(short))%>%drop_na()%>%ungroup()
  


#Trend Measures and Reversal Point 

a <- 0
b <- 0.05

#Numbers referencing formulas from paper

library(rlang)

t_1_measure <- function(time){parse_expr(sprintf("(lag(close, %s) - close)/lag(close, %s) * 100 >= a", time, time))}

t_2_measure <- function(time){parse_expr(sprintf("(lead(close, %s) - close)/close * 100 >= b", time))}

t_3_measure <- function(time){parse_expr(sprintf("(close - lag(close, %s))/lag(close, %s) * 100 >= a", time, time))}

t_4_measure <- function(time){parse_expr(sprintf("(close - lead(close, %s))/close * 100 >= b", time))}


#This needs slightly refining

#Possible grouping the clumps of reversal points then assigning the signal to the high/low bar 
#And multiple criteria ie meeeting botht the 10,5 and 2 condtions furthermore no overnight for intraday 



#Plot of current assigned reversal points
data%>%mutate(eq1 = eval(t_1_measure(10)), eq2 = eval(t_2_measure(10)), eq3 = eval(t_3_measure(10)), eq4 = eval(t_4_measure(10)))%>%
  mutate(long = eq1== TRUE & eq2 == TRUE,short = eq3== TRUE & eq4 == TRUE)%>%select(date,long,short,open,high,low,close)%>%
  ggplot()+
  geom_segment(aes(x=date,xend=date,y=open,yend=close,colour=short),size=1.5)+
  geom_segment(aes(x=date,xend=date,y=high,yend=low,colour=short))



#Custom reversal point measure 1 

data <-data%>%mutate(short = ifelse({(high > lag(high,1)) & (high > lag(high,2)) & (high > lag(high,3)) & (high > lag(high,4)) & (high > lag(high,4)) &
    (high > lead(high,1)) & (high > lead(high,2)) & (high > lead(high,3))  & (high > lead(high,4)) & (high > lead(high,5)) },{"Short"},{"na"}))%>%
  mutate(short=factor(short))%>%drop_na()

data%>%
  ggplot()+
  geom_segment(aes(x=date,xend=date,y=open,yend=close,colour=short),size=1.5)+
  geom_segment(aes(x=date,xend=date,y=high,yend=low,colour=short))
  


#Defining the input values of membership functions from paper one as all parameters are present 
#Must be grouped by date/datetime for use in mutate

#Line entities

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
  set <- c("veryshort" = vs, "short" = s, "medium" = m, "long" = l, "verylong" = vl)
  out <- names(which.max(set))
  out
}


data <- data%>%mutate(high = ifelse({high==low},{high+0.01},{high}))

data <- data%>%mutate(b = b(open,low,high,close),us = us(open,low,high,close), ls = ls(open,low,high,close))


#Plot of the fuzzy sets
ggplot(data.frame(x=seq(0,1,0.01)),mapping = aes(x=x))+
  stat_function(fun = mu_very_short,aes(colour= "Very Short"))+
  stat_function(fun = mu_short,aes(colour= "Short"))+
  stat_function(fun = mu_medium,aes(colour= "Medium"))+
  stat_function(fun = mu_long,aes(colour= "Long"))+
  stat_function(fun = mu_very_long,aes(colour= "Very Long"))+
  labs(x = "x",y ="mu(x)", legend = "Type", title = "Fuzzy Sets")+
  theme_bw()

#Open and close styles

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
  set <- c("open_low" = o_l, "open_equal_low" = o_e_l, "open_equal_high" = o_e_h, "open_high" = o_h)
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
  set <- c("close_low" = c_l, "close_equal_low" = c_e_l, "close_equal_high" = c_e_h, "close_high" = c_h)
  out <- names(which.max(set))
  out
}


#Body colour

body_colour <- function(open,close){
  case_when(
    (open > close) ~ "green",
    (close > open) ~ "red"
  )
}


#Data preparation

#Creating line lengths 



data <- data%>%mutate(b = b(open,low,high,close),us = us(open,low,high,close), ls = ls(open,low,high,close))



#Fuzzying lines


data <- data%>%group_by(formated)%>%mutate(k1_b = fuzzy_set(b), k1_us = fuzzy_set(us), k1_ls = fuzzy_set(ls))%>%ungroup()

#Fuzzying line relations

data <- data%>%group_by(date)%>%mutate(prev_open = lag(open,1), prev_close = lag(close,1), prev_high = lag(high,1), prev_low = lag(low,1))%>%drop_na()%>%ungroup()


data <- data%>%group_by(formated)%>%mutate(k1_open_style = open_style(open,prev_open,prev_close,prev_high,prev_low), k1_close_style = close_style(close,prev_open,prev_close,prev_high,prev_low))%>%ungroup()



#Body Colour 

data <- data%>%mutate(k1_body_colour = body_colour(open,close))

#Creating predictors from lagged values
#More can be added very easily

#Fuzzy lines 
data <- data%>%group_by(date)%>%mutate(k2_b = lag(k1_b,1),k2_us = lag(k1_us,1), k2_ls = lag(k1_ls,1),
                      k3_b = lag(k2_b,1),k3_us = lag(k2_us,1), k3_ls = lag(k2_ls,1),
                      k4_b = lag(k3_b,1),k4_us = lag(k3_us,1), k4_ls = lag(k3_ls,1),
                      k5_b = lag(k4_b,1),k5_us = lag(k4_us,1), k5_ls = lag(k4_ls,1),
                      k6_b = lag(k5_b,1),k6_us = lag(k5_us,1), k6_ls = lag(k5_ls,1),
                      k7_b = lag(k6_b,1),k7_us = lag(k6_us,1), k7_ls = lag(k6_ls,1),
                      k8_b = lag(k7_b,1),k8_us = lag(k7_us,1), k8_ls = lag(k7_ls,1),
                      k9_b = lag(k8_b,1),k9_us = lag(k8_us,1), k9_ls = lag(k8_ls,1),
                      k10_b = lag(k9_b,1),k10_us = lag(k9_us,1), k10_ls = lag(k9_ls,1))%>%ungroup()

#line relations 
data <- data%>%group_by(date)%>%mutate(k2_open_style = lag(k1_open_style,1), k2_close_style = lag(k1_close_style,1),
                      k3_open_style = lag(k2_open_style,1), k3_close_style = lag(k2_close_style,1),
                      k4_open_style = lag(k3_open_style,1), k4_close_style = lag(k3_close_style,1),
                      k5_open_style = lag(k4_open_style,1), k5_close_style = lag(k4_close_style,1),
                      k6_open_style = lag(k5_open_style,1), k6_close_style = lag(k5_close_style,1),
                      k7_open_style = lag(k6_open_style,1), k7_close_style = lag(k6_close_style,1),
                      k8_open_style = lag(k7_open_style,1), k8_close_style = lag(k7_close_style,1),
                      k9_open_style = lag(k8_open_style,1), k9_close_style = lag(k8_close_style,1),
                      k10_open_style = lag(k9_open_style,1), k10_close_style = lag(k9_close_style,1))%>%ungroup()

#Body Colour 
data <- data%>%group_by(date)%>%mutate(k2_body_colour = lag(k1_body_colour,1),
                      k3_body_colour = lag(k2_body_colour,1),
                      k4_body_colour = lag(k3_body_colour,1),
                      k5_body_colour = lag(k4_body_colour,1),
                      k6_body_colour = lag(k5_body_colour,1),
                      k7_body_colour = lag(k6_body_colour,1),
                      k8_body_colour = lag(k7_body_colour,1),
                      k9_body_colour = lag(k8_body_colour,1),
                      k10_body_colour = lag(k9_body_colour,1))%>%ungroup()%>%drop_na()


saveRDS(data,"SP500 All Stocks fuzzy daily data.rds")
#Modelling 

mod_data <- data%>%select(k1_b,k1_us,k1_ls,k1_open_style,k1_close_style,k1_body_colour,
                          k2_b,k2_us,k2_ls,k2_open_style,k2_close_style,k2_body_colour,
                          k3_b,k3_us,k3_ls,k3_open_style,k3_close_style,k3_body_colour,
                          k4_b,k4_us,k4_ls,k4_open_style,k4_close_style,k4_body_colour,
                          k5_b,k5_us,k5_ls,k5_open_style,k5_close_style,k5_body_colour,
                          k6_b,k6_us,k6_ls,k6_open_style,k6_close_style,k6_body_colour,
                          k7_b,k7_us,k7_ls,k7_open_style,k7_close_style,k7_body_colour,
                          k8_b,k8_us,k8_ls,k8_open_style,k8_close_style,k8_body_colour,
                          k9_b,k9_us,k9_ls,k9_open_style,k9_close_style,k9_body_colour,
                          k10_b,k10_us,k10_ls,k10_open_style,k10_close_style,k10_body_colour,
                          long,formated)

mod_data <- mod_data%>%mutate_if(is.character, as.factor)

test <- data%>%select(long,formated)

mod_data <- data%>%select(k1_b,k1_us,k1_ls,k1_body_colour,
                          k2_b,k2_us,k2_ls,k2_body_colour,
                          k3_b,k3_us,k3_ls,k3_body_colour,
                          k4_b,k4_us,k4_ls,k4_body_colour,
                          k5_b,k5_us,k5_ls,k5_body_colour,
                          short)%>%drop_na()
library(tidymodels)


mod_data_split <- initial_split(mod_data,strata = long)
mod_data_train_1 <- training(mod_data_split)
mod_data_train <- mod_data_train_1%>%select(-formated)
mod_data_test_1 <- testing(mod_data_split)
mod_data_test <- mod_data_test_1%>%select(-formated)


tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)


mod_data_folds <- vfold_cv(mod_data_train)



tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(short ~.)

tree_res <- 
  tree_wf %>% 
  tune_grid(
    resamples = mod_data_folds,
    grid = tree_grid
  )


best_tree <- tree_res %>%
  select_best("accuracy")


final_wf <- 
  tree_wf %>% 
  finalize_workflow(best_tree)

final_fit <- 
  final_wf %>%
  last_fit(mod_data_split)

final_fit %>%
  collect_metrics()

 final_fit %>%
  collect_predictions() %>% 
  view()




#Alternating decision tree

library(RWeka)
 
WPM("refresh-cache")
WPM("install-package","alternatingDecisionTrees")




#Create interface to Weka's ADTree and print some documentation
ADT <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
rm(ADT)
WOW(ADT)

mod.adt <- ADT(long ~., data = mod_data_train)

ds <- DecisionStump(short ~., data = mod_data_train)

train

mod.adt

pred <- cbind(class = predict(mod.adt,mod_data_test_1),predict(mod.adt,mod_data_test_1,type = "prob"))%>%bind_cols(select(mod_data_test_1,long,formated))

pred2 <- cbind(class = predict(mod.adt,mod_data_train_1),predict(mod.adt,mod_data_train_1,type = "prob"))%>%bind_cols(select(mod_data_train_1,long,formated))


library(caret)


adtree <- train(short ~., data = mod_data_train%>%drop_na(), method = "lssvmLinear")

?train

unique(mod_data_split[["in_id"]][! mod_data_split[["in_id"]] %in% seq(1,nrow(mod_data),1)])

mod_data_split[["in_id"]]


bind_cols(pred = predict(mod.adt,data),data)%>%
  filter(ticker == "TSLA" & year(date)=="2022")%>%
  ggplot()+
  geom_segment(aes(x=date,xend=date,y=open,yend=close,colour=pred),size=1.5)+
  geom_segment(aes(x=date,xend=date,y=high,yend=low,colour=pred))+
  theme(axis.text.x = element_text(angle = 90))


bind_cols(pred = predict(mod.adt,data),data)%>%
  group_by(date)%>%summarise(n = mean(pred=="Short"))%>%
  view()
  

#### Attempt 3 ####

##### Functions
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
  
  #Adding lags as predictors
  
  data <- data%>%mutate(k2_b = lag(k1_b,1),k2_us = lag(k1_us,1), k2_ls = lag(k1_ls,1),
                        k3_b = lag(k2_b,1),k3_us = lag(k2_us,1), k3_ls = lag(k2_ls,1),
                        k4_b = lag(k3_b,1),k4_us = lag(k3_us,1), k4_ls = lag(k3_ls,1),
                        k5_b = lag(k4_b,1),k5_us = lag(k4_us,1), k5_ls = lag(k4_ls,1),
                        k6_b = lag(k5_b,1),k6_us = lag(k5_us,1), k6_ls = lag(k5_ls,1),
                        k7_b = lag(k6_b,1),k7_us = lag(k6_us,1), k7_ls = lag(k6_ls,1),
                        k8_b = lag(k7_b,1),k8_us = lag(k7_us,1), k8_ls = lag(k7_ls,1),
                        k9_b = lag(k8_b,1),k9_us = lag(k8_us,1), k9_ls = lag(k8_ls,1),
                        k10_b = lag(k9_b,1),k10_us = lag(k9_us,1), k10_ls = lag(k9_ls,1))%>%
    mutate(k2_open_style = lag(k1_open_style,1), k2_close_style = lag(k1_close_style,1),
           k3_open_style = lag(k2_open_style,1), k3_close_style = lag(k2_close_style,1),
           k4_open_style = lag(k3_open_style,1), k4_close_style = lag(k3_close_style,1),
           k5_open_style = lag(k4_open_style,1), k5_close_style = lag(k4_close_style,1),
           k6_open_style = lag(k5_open_style,1), k6_close_style = lag(k5_close_style,1),
           k7_open_style = lag(k6_open_style,1), k7_close_style = lag(k6_close_style,1),
           k8_open_style = lag(k7_open_style,1), k8_close_style = lag(k7_close_style,1),
           k9_open_style = lag(k8_open_style,1), k9_close_style = lag(k8_close_style,1),
           k10_open_style = lag(k9_open_style,1), k10_close_style = lag(k9_close_style,1))%>%
    mutate(k2_body_colour = lag(k1_body_colour,1),
           k3_body_colour = lag(k2_body_colour,1),
           k4_body_colour = lag(k3_body_colour,1),
           k5_body_colour = lag(k4_body_colour,1),
           k6_body_colour = lag(k5_body_colour,1),
           k7_body_colour = lag(k6_body_colour,1),
           k8_body_colour = lag(k7_body_colour,1),
           k9_body_colour = lag(k8_body_colour,1),
           k10_body_colour = lag(k9_body_colour,1))%>%drop_na()
  
  
  #Creating y values 
  
  #HOD and LOD
  
  data <- data%>%mutate(hod = ifelse({(high > lag(high,1)) & (high > lag(high,2)) & (high > lag(high,3)) & (high > lag(high,4)) & (high > lag(high,4)) &
      (high > lead(high,1)) & (high > lead(high,2)) & (high > lead(high,3))  & (high > lead(high,4)) & (high > lead(high,5)) & (high > lead(high,6)) &
      (high > lead(high,7)) & (high > lead(high,8))},{"1"},{"0"}))%>%
    mutate(hod=factor(hod))%>%
    mutate(lod = ifelse({(low < lag(low,1)) & (low < lag(low,2)) & (low < lag(low,3)) & (low < lag(low,4)) & (low < lag(low,4)) &
        (low < lead(low,1)) & (low < lead(low,2)) & (low < lead(low,3))  & (low < lead(low,4)) & (low < lead(low,5)) & (low < lead(low,6)) &
        (low < lead(low,7)) & (low < lead(low,8))},{"1"},{"0"}))%>%
    mutate(lod=factor(lod))%>%
    mutate(trend_long = ifelse({lead(high,1) > close & lead(high,2) > close & lead(high,3) > close & lead(high,4) > close & lead(high,5) > close &
        lead(high,6) > close & lead(high,7) > close & lead(high,8) > close & lag(high,1) > close & lag(high,2) > close & lag(high,3) > close & lag(high,4) > close & lag(high,5) > close},{"1"},{"0"}))%>%
    mutate(trend_long=factor(trend_long))%>%
    mutate(trend_short = ifelse({lead(low,1) < close & lead(low,2) < close & lead(low,3) < close & lead(low,4) < close & lead(low,5) < close &
        lead(low,6) < close & lead(low,7) < close & lead(low,8) < close & lag(low,1) < close & lag(low,2) < close & lag(low,3) < close & lag(low,4) < close & lag(low,5) < close},{"1"},{"0"}))%>%
    mutate(trend_short=factor(trend_short))%>%replace(is.na(.),"0")
  
  
  
    
                
  data%>%select(formated,hod,trend_short,lod,trend_long,k1_b,k1_us,k1_ls,k1_open_style,k1_close_style,k1_body_colour,
                k2_b,k2_us,k2_ls,k2_open_style,k2_close_style,k2_body_colour,
                k3_b,k3_us,k3_ls,k3_open_style,k3_close_style,k3_body_colour,
                k4_b,k4_us,k4_ls,k4_open_style,k4_close_style,k4_body_colour,
                k5_b,k5_us,k5_ls,k5_open_style,k5_close_style,k5_body_colour,
                k6_b,k6_us,k6_ls,k6_open_style,k6_close_style,k6_body_colour,
                k7_b,k7_us,k7_ls,k7_open_style,k7_close_style,k7_body_colour,
                k8_b,k8_us,k8_ls,k8_open_style,k8_close_style,k8_body_colour,
                k9_b,k9_us,k9_ls,k9_open_style,k9_close_style,k9_body_colour,
                k10_b,k10_us,k10_ls,k10_open_style,k10_close_style,k10_body_colour)
}


create_fuzzy_vars(min_data%>%rename(high = h, low = l, close = c, open = o))%>%view()




#Daily Data

ticker <- "SPY"
data <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/historical-price-full/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker))$historical
data <- data%>%arrange(date)
dates <- data$date


for (i in 931:length(dates)) {
  date <- dates[i]
  min_data <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v4/historical-price/%s/%s/minute/%s/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker,5,date,date))$results%>%
    mutate(date = substr(formated,1,10),time=substr(formated,12,19))%>%
    rename(high = h, low = l, close = c, open = o)
  df <- create_fuzzy_vars(min_data)
  ifelse({i==1},{out <- df}, out <- rbind(out,df))
  
  
}


saveRDS(out,file = "15minfuzzynumeric.rds")
saveRDS(out,file = "15minfuzzy.rds")

df <- readRDS("~/15minfuzzy.rds")



mod_data <- out%>%mutate_if(is.character, as.factor)%>%filter((year(formated) != "2022") &( month(formated) != 8))

month(out$formated)


mod_data_split <- initial_split(mod_data,strata = lod)
mod_data_train_1 <- training(mod_data_split)
mod_data_train <- mod_data_train_1%>%select(-formated,-hod,-trend_long,-trend_short)
mod_data_test_1 <- testing(mod_data_split)
mod_data_test <- mod_data_test_1%>%select(-formated,-hod,-trend_long,-trend_short)


ADT <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
WOW(ADT)

mod.adt <- ADT(lod ~., data = mod_data_train)




pred <- cbind(class = predict(mod.adt,mod_data_test_1),predict(mod.adt,mod_data_test_1,type = "prob"))%>%bind_cols(select(mod_data_test_1,lod,formated))

pred2 <- cbind(class = predict(mod.adt,mod_data),predict(mod.adt,mod_data,type = "prob"))%>%bind_cols(select(mod_data,trend_long,formated))



#15 min models


df_15 <- readRDS("~/15minfuzzynumeric.rds")

mod_15_data <- df_15%>%mutate_if(is.character, as.factor)

#LOD
lod_mod_15_data_split <- initial_split(mod_15_data,strata = lod)
lod_mod_15_data_train_1 <- training(lod_mod_15_data_split)
lod_mod_15_data_train <- lod_mod_15_data_train_1%>%select(-formated,-hod,-trend_long,-trend_short)
lod_mod_15_data_test_1 <- testing(lod_mod_15_data_split)
lod_mod_15_data_test <- lod_mod_15_data_test_1%>%select(-formated,-hod,-trend_long,-trend_short)

lod_mod_15 <- ADT(lod ~., data = lod_mod_15_data_train)

#Trend Long

trend_long_mod_15_data_split <- initial_split(mod_15_data,strata = trend_long)
trend_long_mod_15_data_train_1 <- training(trend_long_mod_15_data_split)
trend_long_mod_15_data_train <- trend_long_mod_15_data_train_1%>%select(-formated,-hod,-lod,-trend_short)
trend_long_mod_15_data_test_1 <- testing(trend_long_mod_15_data_split)
trend_long_mod_15_data_test <- trend_long_mod_15_data_test_1%>%select(-formated,-hod,-lod,-trend_short)

trend_long_mod_15 <- ADT(trend_long ~., data = trend_long_mod_15_data_train)


#HOD

hod_mod_15_data_split <- initial_split(mod_15_data,strata = hod)
hod_mod_15_data_train_1 <- training(hod_mod_15_data_split)
hod_mod_15_data_train <- hod_mod_15_data_train_1%>%select(-formated,-lod,-trend_long,-trend_short)
hod_mod_15_data_test_1 <- testing(hod_mod_15_data_split)
hod_mod_15_data_test <- hod_mod_15_data_test_1%>%select(-formated,-lod,-trend_long,-trend_short)

hod_mod_15 <- ADT(hod ~., data = hod_mod_15_data_train)


#Trend short

trend_short_mod_15_data_split <- initial_split(mod_15_data,strata = trend_short)
trend_short_mod_15_data_train_1 <- training(trend_short_mod_15_data_split)
trend_short_mod_15_data_train <- trend_short_mod_15_data_train_1%>%select(-formated,-hod,-trend_long,-lod)
trend_short_mod_15_data_test_1 <- testing(trend_short_mod_15_data_split)
trend_short_mod_15_data_test <- trend_short_mod_15_data_test_1%>%select(-formated,-hod,-trend_long,-lod)

trend_short_mod_15 <- ADT(trend_short ~., data = trend_short_mod_15_data_train)










#5 min models


df_5 <- readRDS("~/5minfuzzynumeric.rds")

mod_5_data <- df_5%>%mutate_if(is.character, as.factor)

#LOD
lod_mod_5_data_split <- initial_split(mod_5_data,strata = lod)
lod_mod_5_data_train_1 <- training(lod_mod_5_data_split)
lod_mod_5_data_train <- lod_mod_5_data_train_1%>%select(-formated,-hod,-trend_long,-trend_short)
lod_mod_5_data_test_1 <- testing(lod_mod_5_data_split)
lod_mod_5_data_test <- lod_mod_5_data_test_1%>%select(-formated,-hod,-trend_long,-trend_short)

lod_mod_5 <- ADT(lod ~., data = lod_mod_5_data_train)

#Trend Long

trend_long_mod_5_data_split <- initial_split(mod_5_data,strata = trend_long)
trend_long_mod_5_data_train_1 <- training(trend_long_mod_5_data_split)
trend_long_mod_5_data_train <- trend_long_mod_5_data_train_1%>%select(-formated,-hod,-lod,-trend_short)
trend_long_mod_5_data_test_1 <- testing(trend_long_mod_5_data_split)
trend_long_mod_5_data_test <- trend_long_mod_5_data_test_1%>%select(-formated,-hod,-lod,-trend_short)

trend_long_mod_5 <- ADT(trend_long ~., data = trend_long_mod_5_data_train)


#HOD

hod_mod_5_data_split <- initial_split(mod_5_data,strata = hod)
hod_mod_5_data_train_1 <- training(hod_mod_5_data_split)
hod_mod_5_data_train <- hod_mod_5_data_train_1%>%select(-formated,-lod,-trend_long,-trend_short)
hod_mod_5_data_test_1 <- testing(hod_mod_5_data_split)
hod_mod_5_data_test <- hod_mod_5_data_test_1%>%select(-formated,-lod,-trend_long,-trend_short)

hod_mod_5 <- ADT(hod ~., data = hod_mod_5_data_train)


#Trend short

trend_short_mod_5_data_split <- initial_split(mod_5_data,strata = trend_short)
trend_short_mod_5_data_train_1 <- training(trend_short_mod_5_data_split)
trend_short_mod_5_data_train <- trend_short_mod_5_data_train_1%>%select(-formated,-hod,-trend_long,-lod)
trend_short_mod_5_data_test_1 <- testing(trend_short_mod_5_data_split)
trend_short_mod_5_data_test <- trend_short_mod_5_data_test_1%>%select(-formated,-hod,-trend_long,-lod)

trend_short_mod_5 <- ADT(trend_short ~., data = trend_short_mod_5_data_train)








create_fuzzy_vars_pred <- function(data){
  
  data <- data%>%mutate(high = ifelse({high==low},{high+0.01},{high}))
  
  data <- data%>%mutate(b = b(open,low,high,close),us = us(open,low,high,close), ls = ls(open,low,high,close))
  
  #Fuzzying lines
  
  data <- data%>%group_by(time)%>%mutate(k1_b = fuzzy_set(b), k1_us = fuzzy_set(us), k1_ls = fuzzy_set(ls))%>%ungroup()
  
  #Fuzzying line relations
  
  data <- data%>%group_by()%>%mutate(prev_open = lag(open,1), prev_close = lag(close,1), prev_high = lag(high,1), prev_low = lag(low,1))%>%drop_na()%>%ungroup()
  
  data <- data%>%group_by(time)%>%mutate(k1_open_style = open_style(open,prev_open,prev_close,prev_high,prev_low), k1_close_style = close_style(close,prev_open,prev_close,prev_high,prev_low))%>%ungroup()
  
  #Body Colour 
  
  data <- data%>%mutate(k1_body_colour = body_colour(open,close))
  
  #Adding lags as predictors
  
  data <- data%>%mutate(k2_b = lag(k1_b,1),k2_us = lag(k1_us,1), k2_ls = lag(k1_ls,1),
                        k3_b = lag(k2_b,1),k3_us = lag(k2_us,1), k3_ls = lag(k2_ls,1),
                        k4_b = lag(k3_b,1),k4_us = lag(k3_us,1), k4_ls = lag(k3_ls,1),
                        k5_b = lag(k4_b,1),k5_us = lag(k4_us,1), k5_ls = lag(k4_ls,1),
                        k6_b = lag(k5_b,1),k6_us = lag(k5_us,1), k6_ls = lag(k5_ls,1),
                        k7_b = lag(k6_b,1),k7_us = lag(k6_us,1), k7_ls = lag(k6_ls,1),
                        k8_b = lag(k7_b,1),k8_us = lag(k7_us,1), k8_ls = lag(k7_ls,1),
                        k9_b = lag(k8_b,1),k9_us = lag(k8_us,1), k9_ls = lag(k8_ls,1),
                        k10_b = lag(k9_b,1),k10_us = lag(k9_us,1), k10_ls = lag(k9_ls,1))%>%
    mutate(k2_open_style = lag(k1_open_style,1), k2_close_style = lag(k1_close_style,1),
           k3_open_style = lag(k2_open_style,1), k3_close_style = lag(k2_close_style,1),
           k4_open_style = lag(k3_open_style,1), k4_close_style = lag(k3_close_style,1),
           k5_open_style = lag(k4_open_style,1), k5_close_style = lag(k4_close_style,1),
           k6_open_style = lag(k5_open_style,1), k6_close_style = lag(k5_close_style,1),
           k7_open_style = lag(k6_open_style,1), k7_close_style = lag(k6_close_style,1),
           k8_open_style = lag(k7_open_style,1), k8_close_style = lag(k7_close_style,1),
           k9_open_style = lag(k8_open_style,1), k9_close_style = lag(k8_close_style,1),
           k10_open_style = lag(k9_open_style,1), k10_close_style = lag(k9_close_style,1))%>%
    mutate(k2_body_colour = lag(k1_body_colour,1),
           k3_body_colour = lag(k2_body_colour,1),
           k4_body_colour = lag(k3_body_colour,1),
           k5_body_colour = lag(k4_body_colour,1),
           k6_body_colour = lag(k5_body_colour,1),
           k7_body_colour = lag(k6_body_colour,1),
           k8_body_colour = lag(k7_body_colour,1),
           k9_body_colour = lag(k8_body_colour,1),
           k10_body_colour = lag(k9_body_colour,1))%>%drop_na()
  

  
  data%>%select(formated,k1_b,k1_us,k1_ls,k1_open_style,k1_close_style,k1_body_colour,
                k2_b,k2_us,k2_ls,k2_open_style,k2_close_style,k2_body_colour,
                k3_b,k3_us,k3_ls,k3_open_style,k3_close_style,k3_body_colour,
                k4_b,k4_us,k4_ls,k4_open_style,k4_close_style,k4_body_colour,
                k5_b,k5_us,k5_ls,k5_open_style,k5_close_style,k5_body_colour,
                k6_b,k6_us,k6_ls,k6_open_style,k6_close_style,k6_body_colour,
                k7_b,k7_us,k7_ls,k7_open_style,k7_close_style,k7_body_colour,
                k8_b,k8_us,k8_ls,k8_open_style,k8_close_style,k8_body_colour,
                k9_b,k9_us,k9_ls,k9_open_style,k9_close_style,k9_body_colour,
                k10_b,k10_us,k10_ls,k10_open_style,k10_close_style,k10_body_colour)
}


date <- "2022-08-24"
ticker <- "SPY"

dev.off()

#15 min pred

cur_data_15 <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v4/historical-price/%s/%s/minute/%s/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker,15,date,date))$results%>%
  mutate(date = substr(formated,1,10),time=substr(formated,12,19))%>%
  rename(high = h, low = l, close = c, open = o)

cur_data_fuzzy_15 <- create_fuzzy_vars_pred(cur_data_15)


lod_pred_15 <- data.frame(predict(lod_mod_15,cur_data_fuzzy_15,type = "prob"))%>%rename("lod_0" = "X0", "lod_1"  = "X1")
trend_long_pred_15 <- data.frame(predict(trend_long_mod_15,cur_data_fuzzy_15,type = "prob"))%>%rename("trend_long_0" = "X0", "trend_long_1"  = "X1")
hod_pred_15 <- data.frame(predict(hod_mod_15,cur_data_fuzzy_15,type = "prob"))%>%rename("hod_0" = "X0", "hod_1"  = "X1")
trend_short_pred_15 <- data.frame(predict(trend_short_mod_15,cur_data_fuzzy_15,type = "prob"))%>%rename("trend_short_0" = "X0", "trend_short_1"  = "X1")

preds_15 <- bind_cols(dt = cur_data_fuzzy_15$formated,lod_pred_15,trend_long_pred_15,hod_pred_15,trend_short_pred_15)


preds_15%>%select(dt,lod_1,trend_long_1,hod_1,trend_short_1)%>%reshape2::melt(id.vars = c("dt"))%>%
  ggplot(aes(x = dt,y = value, colour = variable,fill=variable))+
  geom_bar(position="dodge", stat="identity")+
  geom_hline(mapping=aes(yintercept = 0.5),color="red")+
  labs(x = "time", y = "score", title = paste(date, "15 min predictions", sep = " "))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))


#5 min pred

cur_data_5 <- jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v4/historical-price/%s/%s/minute/%s/%s?apikey=c34a828bdd72b3933d449ea419de9562", ticker,5,date,date))$results%>%
  mutate(date = substr(formated,1,10),time=substr(formated,12,19))%>%
  rename(high = h, low = l, close = c, open = o)

cur_data_fuzzy_5 <- create_fuzzy_vars_pred(cur_data_5)


lod_pred_5 <- data.frame(predict(lod_mod_5,cur_data_fuzzy_5,type = "prob"))%>%rename("lod_0" = "X0", "lod_1"  = "X1")
trend_long_pred_5 <- data.frame(predict(trend_long_mod_5,cur_data_fuzzy_5,type = "prob"))%>%rename("trend_long_0" = "X0", "trend_long_1"  = "X1")
hod_pred_5 <- data.frame(predict(hod_mod_5,cur_data_fuzzy_5,type = "prob"))%>%rename("hod_0" = "X0", "hod_1"  = "X1")
trend_short_pred_5 <- data.frame(predict(trend_short_mod_5,cur_data_fuzzy_5,type = "prob"))%>%rename("trend_short_0" = "X0", "trend_short_1"  = "X1")

preds_5 <- bind_cols(dt = cur_data_fuzzy_5$formated,lod_pred_5,trend_long_pred_5,hod_pred_5,trend_short_pred_5)


preds_5%>%select(dt,lod_1,hod_1)%>%reshape2::melt(id.vars = c("dt"))%>%
  ggplot(aes(x = dt,y = value, colour = variable,fill=variable))+
  geom_bar(position="dodge", stat="identity")+
  geom_hline(mapping=aes(yintercept = 0.5),color="red")+
  labs(x = "time", y = "score", title = paste(date, "5 min predictions", sep = " "))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))
  
  
