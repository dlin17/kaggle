library(dplyr)
library(h2o)
library(ggplot2)
library(lubridate)

# load and format the data
train <- read.csv("~/git/kaggle/rossmann/train.csv")
store <- read.csv("~/git/kaggle/rossmann/store.csv")

train = train %>% tbl_df() %>% mutate(Date = ymd(train$Date)) %>%
  mutate(year = factor(year(Date)),
         month = factor(month(Date)),
         week = factor(week(Date))) %>% 
  mutate(Open = as.logical(Open),
         Promo = as.logical(Promo),
         SchoolHoliday = as.logical(SchoolHoliday),
         DayOfWeek = factor(DayOfWeek),
         Store = factor(Store)) %>% arrange(Store, Date)

# investigate store level sales

prior_sales = train %>% filter(Customers > 0, Sales > 0) %>%
  group_by(Store, year, week) %>%
  summarise(w_sales = log1p(sum(Sales)), 
            w_cust = (mean(log1p(Customers))),
            wd_sales = mean(log1p(Sales))) %>% ungroup() %>%
  mutate(year = factor(as.numeric(as.character(year)) + 1))

prior2_sales = prior_sales %>% mutate(year = factor(as.numeric(as.character(year)) + 1))

colnames(prior2_sales) = c('Store','year','week','w2_sales','w2_cust','w2d_sales')

hist_sales = prior_sales %>% inner_join(prior2_sales, by = c('Store','year','week')) %>%
  filter(year == '2015') %>%
  mutate(w_growth = w_sales - w2_sales,
         wc_growth = w_cust - w2_cust,
         wd_growth = wd_sales - w2d_sales,
         year = factor(year))

store$Store = factor(store$Store)

half_year = train %>% filter(month %in% c(1,2,3,4,5,6)) %>% group_by(Store, year) %>%
  summarise(h_sales = log1p(sum(Sales)), 
            hd_sales = mean(log1p(Sales))) 

train = train %>%
  mutate(logC = log1p(Customers), logS = log1p(Sales), date = lubridate::day(Date)) %>% 
  # inner_join(annual_stores, by = c('Store','year')) %>%
  inner_join(store, by = 'Store') # %>% filter(year == 2015) %>%
  inner_join(prior_sales, by = c('Store','week','year'))
  # inner_join(hist_sales, by = c('Store','year','week')) %>% droplevels()

train$year = factor(train$year)


# Load test data
test <- read.csv("~/git/kaggle/rossmann/test.csv")


test = test %>% tbl_df() %>% mutate(Date = ymd(Date)) %>%
  mutate(year = factor(year(Date)),
         month = factor(month(Date)),
         week = factor(week(Date))) %>% 
  mutate(Open = as.logical(Open),
         Promo = as.logical(Promo),
         SchoolHoliday = as.logical(SchoolHoliday),
         DayOfWeek = factor(DayOfWeek),
         Store = factor(Store))

test = test %>%
  mutate(date = lubridate::day(Date)) %>% 
  # left_join(annual_stores, by = c('Store','year')) %>%
  left_join(store, by = 'Store') %>%
  # left_join(weekly_stores, by = c('Store','week'))
  left_join(hist_sales, by = c('Store','year','week'))

# test = test %>% inner_join(half_year, by = c('Store','year'))

test = test %>% mutate(year = factor(year),
                       Store = factor(Store),
                       week = factor(week))

# random forests

h2o.init(nthreads=-1,max_mem_size='6G')

train_rf = select(train, -Date, -contains('Promo2'), 
                  -contains('CompetitionOpen'), -CompetitionDistance,
                  -Date, -Sales, -logC, -Customers, -PromoInterval)


trainHex<-as.h2o(train_rf)

splits = h2o.splitFrame(trainHex, ratios = 0.95)
train_rft = splits[[1]]
valid = splits[[2]]

features<-colnames(train_rf)[!(colnames(train_rf) %in% c("Id","Date","Sales","logC",
                                                         "logS","Customers"))]


rfHex <- h2o.randomForest(x=features,
                          y="logS", 
                          ntrees = 30,
                          # max_depth = 30,
                          validation = valid,
                          nbins_cats = 1115, ## allow it to fit store ID
                          training_frame=train_rft)



# predict

testRf = test %>% select(-Date) %>% mutate(Open = ifelse(is.na(Open), TRUE, Open))

testHex = as.h2o(as.data.frame(testRf))

predictions<-as.data.frame((h2o.predict(model_2015, testHex)))


predictions = tbl_df(predictions)
predictions$predict = expm1(predictions$predict)

testRf$Sales = predictions$predict
testRf = testRf %>% mutate(Sales = ifelse(Open==T,Sales,0))

# export the results

output = tbl_df(select(testRf, Id, Sales))

write.csv(output, file = 'results.csv', row.names = F)

