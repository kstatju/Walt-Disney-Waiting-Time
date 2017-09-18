



## Random Forest

set.seed(12325543)
labeldata1 = labeldata
names(labeldata1) = c("SPOSTMIN_PRED", 'LABEL')
smp_size <- floor(0.75 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
valid <- data[-train_ind, ]


train_data   <- as.matrix(train[ , !(names(train) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])
train_label  <- as.matrix(train[,"LABEL"])
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)

valid_data   <- as.matrix(valid[ , !(names(valid) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])
valid_label  <- as.matrix(valid[,"LABEL"])
valid_matrix <- xgb.DMatrix(data = valid_data, label = valid_label)


test_data   <- as.matrix(tdata[ , !(names(tdata) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])

test_label  <- tdata[,"LABEL"]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)


## Random Forest Tune


# Random Search

ntree = c(500,1000,1500,2000)
mtry = c(30, 40, 50, 60)
prem = expand.grid(ntree, mtry)

classwt = table(train$LABEL)/nrow(train)
# fgl.res <- tuneRF(train_data, as.factor(train_label),
#                   mtryStart = 2, ntreeTry = 100, stepFactor= 1.2, classwt = classwt, xtest = valid_data,
#                   ytest = as.factor(valid_label))

ptm = proc.time()
result = list()
for (kk in 1:nrow(prem)){
  model.rf = randomForest(train_data, 
                          as.factor(train_label),
                          xtest = valid_data,
                          ytest = as.factor(valid_label), ntree=prem[kk,1], mtry=prem[kk,2], importance=TRUE,
                          classwt = classwt, keep.forest=TRUE, nodesize = 5, replace = FALSE)
  
  train_pred = predict(model.rf, train_data, type="response")
  valid_pred = predict(model.rf, valid_data, type="response")
  test_pred = predict(model.rf, test_data, type="response")
  
  train_prediction = as.data.frame(cbind(train[c('LABEL', 'SPOSTMIN1', 'SPOSTMIN')],PRED_LABEL = train_pred))
  
  train_prediction = merge(train_prediction, labeldata1, by.x = 'PRED_LABEL', by.y = 'LABEL')
  
  valid_prediction = as.data.frame(cbind(valid[c('LABEL', 'SPOSTMIN1', 'SPOSTMIN')],PRED_LABEL = valid_pred))
  
  valid_prediction = merge(valid_prediction, labeldata1, by.x = 'PRED_LABEL', by.y = 'LABEL')
  
  test_prediction = as.data.frame(cbind(tdata[c('LABEL', 'SPOSTMIN1', 'SPOSTMIN')],PRED_LABEL = test_pred))
  
  test_prediction = merge(test_prediction, labeldata1, by.x = 'PRED_LABEL', by.y = 'LABEL')
  
  train_errortable1 = cbind(matrix(prem[kk,], ncol = 2),as.data.frame(accuracy_error(train_prediction)))
  valid_errortable1 = cbind(matrix(prem[kk,], ncol = 2),as.data.frame(accuracy_error(valid_prediction)))
  test_errortable1 = cbind(matrix(prem[kk,], ncol = 2),as.data.frame(accuracy_error(test_prediction)))
  train_errortable1
  valid_errortable1
  test_errortable1
  result[kk] = list(c(unlist(train_errortable1), unlist(valid_errortable1), unlist(test_errortable1)))
}
ptm1 = proc.time()

print(ptm1 - ptm)



write.csv(result, file = paste0(datapath, "result1.csv", collapse = ''), row.names = FALSE)









# XGBoost Model get the feature real names

set.seed(12325543)
labeldata1 = labeldata
names(labeldata1) = c("SPOSTMIN_PRED", 'LABEL')
smp_size <- floor(0.75 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
valid <- data[-train_ind, ]


train_data   <- as.matrix(train[ , !(names(train) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])
train_label  <- as.matrix(train[,"LABEL"])
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)

valid_data   <- as.matrix(valid[ , !(names(valid) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])
valid_label  <- as.matrix(valid[,"LABEL"])
valid_matrix <- xgb.DMatrix(data = valid_data, label = valid_label)


test_data   <- as.matrix(tdata[ , !(names(tdata) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])

test_label  <- tdata[,"LABEL"]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)

watchlist <- list(eval = valid_matrix, train = train_matrix)

weight = train_data[,'WGT']
ptm <- proc.time()
numberOfClasses = length(unique(train_label))
prem1 = c(.01, 20, 2, .9, .9, 2, 1500) 
cv <- xgb.train(params = list(booster = 'gbtree', eta = prem1[1],
                              max_depth = prem1[2],
                              min_child_weight = prem1[3],
                              subsample = prem1[4],
                              colsample_bytree = prem1[5],
                              gamma = prem1[6], num_class = numberOfClasses,
                              lambda = 0, alpha = 0,nthread = 10,
                              objective = "multi:softprob",
                              eval_metric = "merror"),
                data = train_matrix,
                label = train_label,watchlist = watchlist,
                nround = prem1[7], prediction = TRUE, weight = weight,
                showsd = TRUE, early_stopping_rounds = 50, maximize = TRUE,
                verbose = 0
)


trainnumberOfClasses = length(unique(train_label))

train_pred <- predict(cv, newdata = train_matrix)
train_prediction <- matrix(train_pred, nrow = trainnumberOfClasses,
                           ncol=length(train_pred)/trainnumberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(LABEL1 = train_label, SPOSTMIN = train$SPOSTMIN , SPOSTMIN1 = train$SPOSTMIN1, LABEL = train$LABEL,
         PRED_LABEL = max.col(., "last")-1)

train_prediction = merge(train_prediction, labeldata1, by.x = 'PRED_LABEL', by.y = 'LABEL')


validnumberOfClasses = length(unique(valid_label))

valid_pred <- predict(cv, newdata = valid_matrix)
valid_prediction <- matrix(valid_pred, nrow = validnumberOfClasses,
                           ncol=length(valid_pred)/validnumberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(LABEL1 = valid_label, SPOSTMIN = valid$SPOSTMIN , SPOSTMIN1 = valid$SPOSTMIN1, LABEL = valid$LABEL,
         PRED_LABEL = max.col(., "last")-1)

valid_prediction = merge(valid_prediction, labeldata1, by.x = 'PRED_LABEL', by.y = 'LABEL')


testnumberOfClasses = length(unique(test_label))
test_pred <- predict(cv, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = testnumberOfClasses,
                          ncol=length(test_pred)/testnumberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(LABEL1 = test_label, SPOSTMIN = tdata$SPOSTMIN , SPOSTMIN1 = tdata$SPOSTMIN1,
         PRED_LABEL = max.col(., "last")-1)

test_prediction = merge(test_prediction, labeldata1, by.x = 'PRED_LABEL', by.y = 'LABEL')


train_errortable1 = cbind(matrix(prem1, ncol = 7),as.data.frame(accuracy_error(train_prediction)))
valid_errortable1 = cbind(matrix(prem1, ncol = 7),as.data.frame(accuracy_error(valid_prediction)))
test_errortable1 = cbind(matrix(prem1, ncol = 7),as.data.frame(accuracy_error(test_prediction)))
train_errortable1
valid_errortable1
test_errortable1

ptm1 = proc.time()

print(ptm1 - ptm)


names1 <-  colnames(train_data[, !(colnames(train_data) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])
# compute feature importance matrix
importance_matrix = xgb.importance(feature_names = names1, model = cv)
head(importance_matrix)

# plot
gp = xgb.plot.importance(importance_matrix)
print(gp)


write.csv(importance_matrix, file = paste0(datapath, "importance_matrix.csv", collapse = ''), row.names = FALSE)





# 
# ## XGBOOST TUNE Parameters
# ptm <- proc.time()
# 
# 
# train_data   <- as.matrix(data[ , !(names(data) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])
# train_label  <- as.matrix(data[,"LABEL"])
# train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# 
# 
# cv_folds <- KFold(as.matrix(data[,target.var]), nfolds = 5, 
#                   stratified = TRUE, seed = 50)
# 
# xgb.cv.bayes <- function(max.depth, min_child_weight, subsample, colsample_bytree, gamma, eta, nround){
#   numberOfClasses = length(unique(train_label))
#   cv <- xgb.cv(params = list(booster = 'gbtree', eta = eta,
#                              max_depth = max.depth,
#                              min_child_weight = min_child_weight,
#                              subsample = subsample,
#                              colsample_bytree = colsample_bytree,
#                              gamma = gamma, num_class = numberOfClasses,
#                              lambda = 1, alpha = 0,
#                              objective = "multi:softprob",
#                              eval_metric = "mlogloss"),
#                data = train_matrix,
#                label = train_label,
#                nround = nround, folds = cv_folds, prediction = TRUE, 
#                showsd = TRUE, early_stopping_rounds = 15, maximize = TRUE,
#                verbose = 0
#   )
#   list(Score = as.matrix(-(cv$evaluation_log$test_mlogloss_mean)),
#        Pred = cv$pred)
# }
# 
# xgb.bayes.model <- BayesianOptimization(
#   xgb.cv.bayes,
#   bounds = list(max.depth = c(2L, 20L),
#                 min_child_weight = c(1L, 30L),
#                 subsample = c(0.5, .8),
#                 colsample_bytree = c(0.1, 0.8),
#                 gamma = c(0, 100),
#                 eta = c(1e-4, 0.3), nround = c(50L, 1000L) 
#                 
#   ),
#   init_grid_dt = NULL,
#   init_points = 50,  # number of random points to start search
#   n_iter = 200, # number of iterations after initial random points are set
#   acq = 'ucb', kappa = 2.576, eps = 0.0, verbose = TRUE
# )
# 
# ptm1 = proc.time()
# 
# print(ptm1 - ptm)
# 
# 
# saveRDS(xgb.bayes.model, paste0(datapath, "xgb.bayes.model.rds", collapse = ''))
# my_model <- readRDS(paste0(datapath, "xgb.bayes.model.rds", collapse = ''))
# 
# 

## Interaction 
# 
# intervar = c("DAYMINUTE", "SCHOOL_PCA4", "CAPACITY_PCA2", "AKHOURS", "LAG_Q4", 
#              "MKHOURS", "LAG_Q3", "DAYOFWEEK", "SEASON_PROP", "IAHOURS", 
#              "HSHOURS", "UFHOURS", "LAG_Q1", "DMY_AKCLOSE", "DAYEVENTBA", 
#              "DMY_UFCLOSE", "DMY_IACLOSE")

# interc <- t(apply(test_data[,intervar], 1, combn, 2, prod))
# colnames(interc) <- paste("Inter.V", combn(intervar, 2, paste, collapse="V"), sep="")
# test_data <- as.matrix(cbind(test_data, interc))



## XGBOOST using Train

ptm <- proc.time()
labeldata1 = labeldata
names(labeldata1) = c("SPOSTMIN_PRED", 'LABEL')

train_data   <- as.matrix(data[ , !(names(data) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])
train_label  <- as.matrix(data[,"LABEL"])
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)


test_data   <- as.matrix(tdata[ , !(names(tdata) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])
test_label  <- tdata[,"LABEL"]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)


max.depth = c(12,15,18)
min_child_weight = c(2,3,4)
subsample = c(.7,.8)
colsample_bytree = c(0.9,1)
gamma = c(6,8,10)
eta = c(0.01,0.05,0.1,0.15)
nround = c(1500)
prem = expand.grid(eta, max.depth, min_child_weight, subsample, colsample_bytree, gamma, nround)

watchlist <- list(eval = test_matrix, train = train_matrix)

numberOfClasses = length(unique(train_label))
for (j in (1:dim(prem)[1])){
    cv <- xgb.train(params = list(booster = 'gbtree', eta = prem[j,1],
                               max_depth = prem[j,2],
                               min_child_weight = prem[j,3],
                               subsample = prem[j,4],
                               colsample_bytree = prem[j,5],
                               gamma = prem[j,6], num_class = numberOfClasses,
                               lambda = 1, alpha = 0,
                               objective = "multi:softprob",
                               eval_metric = "merror"),
                 data = train_matrix,
                 label = train_label,watchlist = watchlist,
                 nround = prem[j,7], prediction = TRUE, 
                 showsd = TRUE, early_stopping_rounds = 50, maximize = TRUE,
                 verbose = 0
      )
    
    
    trainnumberOfClasses = length(unique(train_label))
    
    train_pred <- predict(cv, newdata = train_matrix)
    train_prediction <- matrix(train_pred, nrow = trainnumberOfClasses,
                               ncol=length(train_pred)/trainnumberOfClasses) %>%
      t() %>%
      data.frame() %>%
        mutate(LABEL1 = train_label, SPOSTMIN = data$SPOSTMIN , SPOSTMIN1 = data$SPOSTMIN1, LABEL = data$LABEL,
             PRED_LABEL = max.col(., "last")-1)

    train_prediction = merge(train_prediction, labeldata1, by.x = 'PRED_LABEL', by.y = 'LABEL')
    
    
    testnumberOfClasses = length(unique(test_label))
    test_pred <- predict(cv, newdata = test_matrix)
    test_prediction <- matrix(test_pred, nrow = testnumberOfClasses,
                               ncol=length(test_pred)/testnumberOfClasses) %>%
      t() %>%
      data.frame() %>%
      mutate(LABEL1 = test_label, SPOSTMIN = tdata$SPOSTMIN , SPOSTMIN1 = tdata$SPOSTMIN1,
             PRED_LABEL = max.col(., "last")-1)
    
    test_prediction = merge(test_prediction, labeldata1, by.x = 'PRED_LABEL', by.y = 'LABEL')
    
    
    if (j == 1){
      train_errortable = cbind(prem[j,],as.data.frame(accuracy_error(train_prediction)))
      test_errortable = cbind(prem[j,],as.data.frame(accuracy_error(test_prediction)))
    }else{ 
        train_errortable = rbind(train_errortable, cbind(prem[j,],as.data.frame(accuracy_error(train_prediction))))
        test_errortable = rbind(test_errortable, cbind(prem[j,],as.data.frame(accuracy_error(test_prediction))))
      }
}




ptm1 = proc.time()

print(ptm1 - ptm)

write.csv(test_errortable, file = paste0(datapath, "test_errortable_parm1.csv", collapse = ''), row.names = FALSE)
write.csv(train_errortable, file = paste0(datapath, "train_errortable_parm1.csv", collapse = ''), row.names = FALSE)

plot(train_errortable$accuracy, test_errortable$accuracy)
test_errortable[which(test_errortable$accuracy == max(test_errortable$accuracy)),]
train_errortable[which(train_errortable$accuracy == max(train_errortable$accuracy)),]

### XGBOOST Model

ptm <- proc.time()



train_data   <- as.matrix(data[ , !(names(data) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])
train_label  <- data[,"LABEL"]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)


numberOfClasses <- length(unique(data$LABEL))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5
# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params, max.depth = 10, nthread = 10,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)

 bst_model <- xgb.train(params = xgb_params, max.depth = 20, nthread = 10,
                        data = train_matrix, 
                       nrounds = nround,prediction = TRUE)
ptm1 = proc.time()

print(ptm1 - ptm)

train_pred <- predict(bst_model, newdata = train_matrix)
train_prediction <- matrix(train_pred, nrow = numberOfClasses,
                          ncol=length(train_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(LABEL1 = train_label,
         PRED_LABEL = max.col(., "last")-1)


OOF_prediction <- data.frame(train_pred) %>%
  mutate(PRED_LABEL = max.col(., ties.method = "last")-1,
         LABEL1 = train_label )
pred_label = OOF_prediction[, c('PRED_LABEL')]

data = cbind(data, pred_label)
names(labeldata) = c('SPOSTMIN2', "LABEL")
data  = merge(data, labeldata, by.x = 'PRED_LABEL', by.y = 'LABEL')


maecom(data$SPOSTMIN, data$SPOSTMIN2)





# confusion matrix
confusionMatrix(factor(train_prediction$LABEL1), 
                factor(train_prediction$PRED_LABEL),
                mode = "everything")
table(train_prediction$PRED_LABEL)

table(factor(cv_model$pred, levels=min(test):max(test)),factor(train_label, levels=min(test):max(test)))

table(train_prediction$LABEL1, train_prediction$PRED_LABEL)

table(OOF_prediction$label)



tdata[c('LAG_1', 'LAG_2', 'LAG_3', 'LAG_4')] = NULL





delvar = c("WGT", "HOLIDAYM", "LAG_Q0", "DLR_TICKET_SEASON_PROP", "WEEKOFYEAR", 
           "WDW_TICKET_SEASON_PROP",  "YEAR", "PARTYSEASON_WDW_PROP", "MONTHOFYEAR")

intervar = c("DAYMINUTE", "SCHOOL_PCA4", "CAPACITY_PCA2", "AKHOURS", "LAG_Q4", 
             "MKHOURS", "LAG_Q3", "DAYOFWEEK", "SEASON_PROP", "IAHOURS", 
             "HSHOURS", "UFHOURS", "LAG_Q1", "DMY_AKCLOSE", "DAYEVENTBA", 
             "DMY_UFCLOSE", "DMY_IACLOSE")









# unique(data1$SEASON)
# 
# aa = as.data.frame(table(data$SPOSTMIN,data$SEASON))

# ## create n fold data
# 
# nfold = 10
# 
# hist(data1$SPOSTMIN)
# 
# 
# partyvar = c("MKDAYSBEFORENONEVENT", "MKDAYSSINCENONEVENT", "MKEVENTSTREAK", "MKEVENTSTREAK_F")
# 
# data['a'] = rowSums(data[partyvar])
# 
# 
# flds <- createFolds(data$SPOSTMIN, k = nfold, list = TRUE, returnTrain = FALSE)
# names(flds)[1] <- "train"
# 
# ## ACF Plot
# data = data[with(data, order(SATTID, WEEKDAYS, DAYHOUR, DATE)),]
# k = 1
# data$LAG_1 = c(data$SPOSTMIN[1:k], data$SPOSTMIN[1:(nrow(data)-k)])
# k = 2
# data$LAG_2 = c(data$SPOSTMIN[1:k], data$SPOSTMIN[1:(nrow(data)-k)])
# k = 3
# data$LAG_3 = c(data$SPOSTMIN[1:k], data$SPOSTMIN[1:(nrow(data)-k)])
# k = 4
# data$LAG_4 = c(data$SPOSTMIN[1:k], data$SPOSTMIN[1:(nrow(data)-k)])
# acf(data$SPOSTMIN-data$LAG_1)
# hist(data$SPOSTMIN[data$SATTID == "AK07" & data$DAYOFWEEK == 3 & data$DAYHOUR == 14])
# 
# 
# 
# a = data[data$SATTID == "AK07" & data$DAYOFWEEK == 3 & data$DAYHOUR == 14,]
# 
# ## Plot 
# hist(unlist(data[capacityvar[1]]))
# 
# plot(data$CAPACITY_PCA1, data$SPOSTMIN)
# 
# 
# a$aa = log(a$BOOLVAR+1)
# # 
nm = names(data)
for (i in 1:length(nm)){
  ggplot(data, aes(x = DAYOFYEAR*MONTHOFYEAR, y = SPOSTMIN1 , colour = SPOSTMIN )) +
       geom_point() +  facet_wrap( ~  DAYOFWEEK)
}
a = data$BOOLVAR[data$BOOLVAR == 0]

# 


for (i in 1:length(nm)){
  df <- rbind(data.frame(aa="Train", obs=unlist(data[,nm[i]])),
              data.frame(aa="Test", obs=unlist(tdata[,nm[i]])))
  print(
    ggplot(df, aes(obs, fill=aa, colour=aa)) +
      geom_histogram(aes(y=..density..),
                     lwd=0.2, position = "dodge") +
      ggtitle(nm[i]))
}
