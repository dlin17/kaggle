
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
  select(-Sales, -logC, -Customers,
         -Open, -Date, -contains('Promo2'), -contains('Holiday'))


trainHex<-as.h2o(train_rf)

splits = h2o.splitFrame(trainHex, ratios = 0.95)
train_rft = splits[[1]]
valid = splits[[2]]

features<-colnames(select(train_rf, -logS))

rfHex_store <- h2o.randomForest(x=features,
                                    y="logS", 
                                    ntrees = 30,
                                    # max_depth = 30,
                                    validation = valid,
                                    nbins_cats = 1115, ## allow it to fit store ID
                                    nfolds = 5,
                                    training_frame=train_rft)

h2o.mse(rfHex_store)
h2o.mse(rfHex_store, valid = T)
h2o.varimp(rfHex_store)


# half year

h2o.shutdown(prompt = F)
h2o.init(nthreads=2,max_mem_size='6G')

train_rf = train %>% filter(Sales > 0, StateHoliday %in% c('0','a')) %>% filter(month %in% c(7,8,9)) %>%
  select(-Sales, -logC, -Customers,
         -Open, -Date, -contains('Promo2'), -contains('Holiday'), -Store, -contains('cust'),
         -contains('Competition'), -date, -month, -year)


trainHex<-as.h2o(train_rf)

splits = h2o.splitFrame(trainHex, ratios = 0.95)
train_rft = splits[[1]]
valid = splits[[2]]

features<-colnames(select(train_rf, -logS))

rfHex_half <- h2o.randomForest(x=features,
                                y="logS", 
                                ntrees = 30,
                                # max_depth = 30,
                                validation = valid,
                                # nbins_cats = 1115, ## allow it to fit store ID
                                nfolds = 5,
                                training_frame=train_rft)

h2o.mse(rfHex_half)
h2o.mse(rfHex_half, valid = T)
h2o.varimp(rfHex_half)

# grid search for depth


grid_search = function(depth) {
  model = h2o.randomForest(x=features,
                   y="logS", 
                   ntrees = 30,
                   max_depth = depth,
                   validation = valid,
                   # nbins_cats = 1115, ## allow it to fit store ID
                   # nfolds = 10,
                   training_frame=train_rft)
  
  return (h2o.mse(model), valid = T)
}

