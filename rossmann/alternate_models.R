
# random forests reduce features

h2o.init(nthreads=-1,max_mem_size='6G')

train_rf = train %>% filter(Sales > 0) %>%
  select(-Date, -contains('Promo2'), 
         -contains('CompetitionOpen'), -CompetitionDistance,
         -Date, -Sales, -logC, -Customers, -PromoInterval, -compopen,
         -Open)


trainHex<-as.h2o(train_rf)

splits = h2o.splitFrame(trainHex, ratios = 0.95)
train_rft = splits[[1]]
valid = splits[[2]]

features<-colnames(train_rf)[!(colnames(train_rf) %in% c("Id","Date","Sales","logC",
                                                         "logS","Customers"))]


rfHex_2 <- h2o.randomForest(x=features,
                          y="logS", 
                          ntrees = 50,
                          max_depth = 30,
                          validation = valid,
                          # nbins_cats = 1115, ## allow it to fit store ID
                          training_frame=train_rft)

# add in store


train_rf = train %>% filter(Open == T) %>%
  select(-Date, -contains('Promo2'), 
         -contains('CompetitionOpen'), -CompetitionDistance,
         -Date, -Sales, -logC, -Customers, -PromoInterval, -compopen,
         -Open)


trainHex<-as.h2o(train_rf)

splits = h2o.splitFrame(trainHex, ratios = 0.95)
train_rft = splits[[1]]
valid = splits[[2]]

features<-colnames(train_rf)[!(colnames(train_rf) %in% c("Id","Date","Sales","logC",
                                                         "logS","Customers"))]


rfHex_3 <- h2o.randomForest(x=features,
                            y="logS", 
                            ntrees = 100,
                            max_depth = 30,
                            validation = valid,
                            nbins_cats = 1115, ## allow it to fit store ID
                            training_frame=train_rft)


# add additional features


h2o.init(nthreads=-1,max_mem_size='6G')

train_rf = train %>% filter(Sales > 0) %>%
  select(-Date, -Open, -Store, -compopen)

trainHex<-as.h2o(train_rf)

splits = h2o.splitFrame(trainHex, ratios = 0.95)
train_rft = splits[[1]]
valid = splits[[2]]

features<-colnames(train_rf)[!(colnames(train_rf) %in% c("Id","Date","Sales","logC",
                                                         "logS","Customers"))]


rfHex_4 <- h2o.randomForest(x=features,
                            y="logS", 
                            ntrees = 50,
                            max_depth = 30,
                            validation = valid,
                            # nbins_cats = 1115, ## allow it to fit store ID
                            training_frame=train_rft)

summary(rfHex_4)

# exclude b and c holidays

h2o.shutdown(prompt = F)
h2o.init(nthreads=-1,max_mem_size='6G')

train_rf = train %>% filter(Sales > 0, StateHoliday %in% c('0','a')) %>%
  select(-contains('Promo2'), 
         -contains('CompetitionOpen'), -CompetitionDistance,
         -Sales, -logC, -Customers, -PromoInterval, -compopen,
         -Open, -`2015`)

training_set = train_rf %>% filter(Date < '2014-07-01') %>%
  select(-Date)

validation_set = train_rf %>% filter(Date > '2014-07-01', Date < '2014-10-01') %>%
  select(-Date)


# train_rft = as.h2o(training_set)
train_rft = as.h2o(select(train_rf, -Date))
valid = as.h2o(validation_set)

features<-colnames(train_rf)[!(colnames(train_rf) %in% c("Id","Date","Sales","logC",
                                                         "logS","Customers"))]


rfHex_noholiday <- h2o.randomForest(x=features,
                                    y="logS", 
                                    ntrees = 50,
                                    max_depth = 30,
                                    validation = valid,
                                    # nbins_cats = 1115, ## allow it to fit store ID
                                    # nfolds = 10,
                                    training_frame=train_rft)

h2o.mse(rfHex_noholiday)
h2o.mse(rfHex_noholiday, valid = T)
h2o.varimp(rfHex_noholiday)