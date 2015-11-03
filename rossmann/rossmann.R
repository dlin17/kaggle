library(dplyr)
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
         Store = factor(Store))

# investigate store level sales

annual_stores = train %>% filter(Customers > 0, Sales > 0) %>%
  group_by(Store, month) %>% 
  summarise(a_sales = log1p(sum(Sales)), 
            a_cust = (mean(log1p(Customers))),
            ad_sales = mean(log1p(Sales)))

weekly_stores = train %>% filter(Customers > 0, Sales > 0) %>%
  group_by(Store, week) %>% 
  summarise(w_sales = log1p(sum(Sales)), 
            w_cust = (mean(log1p(Customers))),
            wd_sales = mean(log1p(Sales)))

store$Store = as.factor(store$Store)

store = store %>% mutate(logcd = log1p(CompetitionDistance),
                         compopen = ymd(paste(CompetitionOpenSinceYear,
                                              CompetitionOpenSinceMonth,'01')))


growth = train %>% filter(month %in% c(1,2,3,4,5,6)) %>% group_by(Store, year) %>%
  summarise(logS = log(sum(Sales))) %>% spread(year, logS) %>% ungroup() %>%
  mutate(fourgrowth = `2014` / `2013` - 1)

train = train %>%
  mutate(logC = log1p(Customers), logS = log1p(Sales), date = lubridate::day(Date)) %>% 
  inner_join(annual_stores, by = c('Store','month')) %>%
  inner_join(store, by = 'Store') %>%
  mutate(compopeninterval = log1p(as.integer(Date - compopen)))

train = train %>% inner_join(weekly_stores, by = c('Store','week'))

train = train %>% inner_join(growth, by = 'Store')

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
  inner_join(annual_stores, by = c('Store','month')) %>%
  inner_join(store, by = 'Store') %>%
  mutate(compopeninterval = log1p(as.integer(Date - compopen)))

test = test %>% 
  left_join(weekly_stores, by = c('Store','week'))
  
test = test %>% mutate(Store = factor(Store),
                       year = factor(year),
                       month = factor(month),
                       week = factor(week))

# random forests

h2o.init(nthreads=-1,max_mem_size='6G')

train_rf = select(train, -Date, -contains('Promo2'), 
                  -contains('CompetitionOpen'), -CompetitionDistance,
                  -Date, -Sales, -logC, -Customers, -PromoInterval, -compopen)


trainHex<-as.h2o(train_rf)

splits = h2o.splitFrame(trainHex, ratios = 0.95)
train_rft = splits[[1]]
valid = splits[[2]]

features<-colnames(train_rf)[!(colnames(train_rf) %in% c("Id","Date","Sales","logC",
                                                         "logS","Customers"))]


rfHex <- h2o.randomForest(x=features,
                          y="logS", 
                          ntrees = 100,
                          max_depth = 30,
                          validation = valid,
                          nbins_cats = 1115, ## allow it to fit store ID
                          training_frame=train_rft)



# predict

testRf = test %>% select(-Date, -compopen) %>% mutate(Open = ifelse(is.na(Open), TRUE, Open))

testHex = as.h2o(as.data.frame(testRf))

predictions<-as.data.frame((h2o.predict(rfHex_noholiday, testHex)))


predictions = tbl_df(predictions)
predictions$predict = exp(predictions$predict)

testRf$Sales = predictions$predict
testRf = testRf %>% mutate(Sales = ifelse(Open==T,Sales,0))

# export the results

output = tbl_df(select(testRf, Id, Sales))

write.csv(output, file = 'results.csv', row.names = F)

