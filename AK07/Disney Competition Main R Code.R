if(!require(dummies, quietly =T))  {  install.packages("dummies");  require(dummies, quietly =TRUE)}
if(!require(plyr, quietly =T))  {  install.packages("plyr");  require(plyr, quietly =TRUE)}
if(!require(ggplot2, quietly =T))  {  install.packages("ggplot2");  require(ggplot2, quietly =TRUE)}
if(!require(stats, quietly =T))  {  install.packages("stats");  require(stats, quietly =TRUE)}
if(!require(lme4, quietly =T))  {  install.packages("lme4");  require(lme4, quietly =TRUE)}
if(!require(quantreg, quietly =T))  {  install.packages("quantreg");  require(quantreg, quietly =TRUE)}
if(!require(caret, quietly =T))  {  install.packages("caret");  require(caret, quietly =TRUE)}
if(!require(xgboost, quietly =T))  {  install.packages("xgboost");  require(xgboost, quietly =TRUE)}
if(!require(data.table, quietly =T))  {  install.packages("data.table");  require(data.table, quietly =TRUE)}
if(!require(lubridate, quietly =T))  {  install.packages("lubridate");  require(lubridate, quietly =TRUE)}
if(!require(archdata, quietly =T))  {  install.packages("archdata");  require(archdata, quietly =TRUE)}
if(!require(dplyr, quietly =T))  {  install.packages("dplyr");  require(dplyr, quietly =TRUE)}
if(!require(rBayesianOptimization, quietly =T))  {  install.packages("rBayesianOptimization");  require(rBayesianOptimization, quietly =TRUE)}
if(!require(e1071, quietly =T))  {  install.packages("e1071");  require(e1071, quietly =TRUE)}
if(!require(reshape2, quietly =T))  {  install.packages("reshape2");  require(reshape2, quietly =TRUE)}
if(!require(randomForest, quietly =T))  {  install.packages("randomForest");  require(randomForest, quietly =TRUE)}



datapath = "C:/Users/ka746940/Desktop/WaltDisney Competition/Data/Disney Touring Plans Data/"
codepath = "C:/Users/ka746940/Desktop/WaltDisney Competition/R Code/"

dataname = c("AK07")
test_data_name = c("AK07_predict")
#dataname = c("AK07", "EP09", "HS20", "MK04")

function_file_name =  "Disney Competition R Function.R"

source(paste0(codepath, function_file_name, collapse = ''))
source(paste0(codepath, "Disney Competition Test data R Code.R", collapse = ''))
source(paste0(codepath, "Disney Competition Train data R Code.R", collapse = ''))


# Train Data Preparation

train_data = train_data_prep(data = NULL, dataname = dataname, 
                            test_data_prop = 0.75, test = FALSE,
                            datapath = datapath, codepath = codepath, seed = 123)

data = train_data$Train_Data

## Test data Preparation
test_data = test_data_prep(data = NULL, dataname = test_data_name, datapath = datapath, codepath = codepath)

# Model Fit for Train data

fit  = randomforeset_func(data, prem = c(ntree = 1000, mtry = 43, nodesize = 3), target = "SPOSTMIN1", seed = 12354)

# Predict Train data and find the accuracy and MSE, MAE, and Average Over and under Prediction

pred = pred_func_rf(data, model = fit$model)
as.data.frame(maeoucom(pred$pred_data[c("SPOSTMIN", "PRED_LABEL")]))

# Predict Test Data

test_data_pred = pred_func_rf(test_data, model = fit$model)

test_data_pred = test_data_pred$pred_data

write.csv(test_data_pred, file = paste0(datapath, "AK07 TEST DATA PREDICTION.csv", collapse = ''), row.names = FALSE)

# Save fitted Model and Read fitted Model
saveRDS(fit$model, paste0(datapath, "Random_Forest_For_AK07.rds", collapse = ''))
Random_Forest_For_AK07 <- readRDS(paste0(datapath, "Random_Forest_For_AK07.rds", collapse = ''))

#########################################################
























# 
# 
# 
# model_data_9_10  = randomforeset_func(data = data_9_10, 
#                                       prem = c(ntree = 200, mtry = 60, nodesize = 3), 
#                                       target = "SPOSTMIN")
# 
# model_data_11_13  = randomforeset_func(data = data_11_13, 
#                                       prem = c(ntree = 200, mtry = 60, nodesize = 3), 
#                                       target = "SPOSTMIN")
# 
# model_data_14_16  = randomforeset_func(data = data_14_16, 
#                                       prem = c(ntree = 200, mtry = 60, nodesize = 3), 
#                                       target = "SPOSTMIN")
# 
# model_data_17_19  = randomforeset_func(data = data_17_19, 
#                                       prem = c(ntree = 200, mtry = 60, nodesize = 3), 
#                                       target = "SPOSTMIN")
# 
# model_data_20_21  = randomforeset_func(data = data_20_21, 
#                                        prem = c(ntree = 200, mtry = 60, nodesize = 3), 
#                                        target = "SPOSTMIN")
# 
# model_data_22_7  = randomforeset_func(data = data_22_7, 
#                                        prem = c(ntree = 200, mtry = 60, nodesize = 3), 
#                                        target = "SPOSTMIN")
# 
# 
# train_pred_data_9_10= predict(model_data_9_10$func, model_data_9_10$train_data, type="response")
# 
# train_errortable_data_9_10 = cbind(prem,
#                           as.data.frame(maeoucom(cbind(model_data_9_10$train_label, 
#                                                        as.numeric(as.character(train_pred_data_9_10))))))
# 
# 
# train_pred_data_11_13= predict(model_data_11_13$func, model_data_11_13$train_data, type="response")
# 
# train_errortable_data_11_13 = cbind(prem,
#                           as.data.frame(maeoucom(cbind(model_data_11_13$train_label, 
#                                                        as.numeric(as.character(train_pred_data_11_13))))))
# 
# train_pred_data_14_16= predict(model_data_14_16$func, model_data_14_16$train_data, type="response")
# 
# train_errortable_data_14_16 = cbind(prem,
#                           as.data.frame(maeoucom(cbind(model_data_14_16$train_label, 
#                                                        as.numeric(as.character(train_pred_data_14_16))))))
# 
# train_pred_data_17_19= predict(model_data_17_19$func, model_data_17_19$train_data, type="response")
# 
# train_errortable_data_17_19 = cbind(prem,
#                           as.data.frame(maeoucom(cbind(model_data_17_19$train_label, 
#                                                        as.numeric(as.character(train_pred_data_17_19))))))
# 
# train_pred_data_20_21= predict(model_data_20_21$func, model_data_20_21$train_data, type="response")
# 
# train_errortable_data_20_21 = cbind(prem,
#                           as.data.frame(maeoucom(cbind(model_data_20_21$train_label, 
#                                                        as.numeric(as.character(train_pred_data_20_21))))))
# 
# train_pred_data_22_7= predict(model_data_22_7$func, model_data_22_7$train_data, type="response")
# 
# train_errortable_data_22_7 = cbind(prem,
#                           as.data.frame(maeoucom(cbind(model_data_22_7$train_label, 
#                                                        as.numeric(as.character(train_pred_data_22_7))))))
# 
# train_errortable_data_9_10
# train_errortable_data_11_13
# train_errortable_data_14_16
# train_errortable_data_17_19
# train_errortable_data_20_21
# train_errortable_data_22_7
# ptm1 = proc.time()
# print(ptm1 - ptm)
# 
# #####
# 
# set.seed(12325543)
# predvar = "SPOSTMIN1"
# train_data   <- as.matrix(data_12_14[ , !(names(data_12_14) %in% c('SPOSTMIN1', 'SPOSTMIN'))])
# train_label  <- as.matrix(data_12_14[,predvar])
# train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# 
# prem = matrix(c(10, 60, 5), ncol = 3)
# colnames(prem) = c('ntree', 'mtry', 'nodesize')
# classwt = table(data_12_14[predvar])/nrow(data_12_14)
# kk = 1
# ptm = proc.time()
# result = list()
# model.rf_data_12_14 = randomForest(train_data, 
#                           as.factor(train_label),
#                           ntree=prem[kk,1], mtry=prem[kk,2], importance=TRUE,
#                           classwt = classwt, keep.forest=TRUE, nodesize = prem[kk,3], replace = FALSE)
# 
# train_pred_data_12_14 = predict(model.rf_2, train_data, type="response")
# 
# train_errortable1 = cbind(prem,
#                           as.data.frame(maeoucom(cbind(data_12_14['SPOSTMIN'], as.numeric(as.character(train_pred_data_12_14))))))
# 
# train_errortable1
# ptm1 = proc.time()
# print(ptm1 - ptm)
# 
# 
# 
# ###########################################################################
# ## Random Forest
# 
# set.seed(12325543)
# predvar = "SPOSTMIN1"
# train_data   <- as.matrix(data[ , !(names(data) %in% c('SPOSTMIN1', 'SPOSTMIN'))])
# train_label  <- as.matrix(data[,predvar])
# train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# 
# test_data   <- as.matrix(tdata[ , !(names(tdata) %in% c('SPOSTMIN1', 'SPOSTMIN'))])
# test_label  <- tdata[,predvar]
# test_matrix <- xgb.DMatrix(data = test_data, label = test_label)
# 
# 
# ## Random Forest Tune
# 
# 
# # Random Search
# 
# classwt = table(data[predvar])/nrow(data)
# #classwt = (1/(table(data[predvar])/nrow(data)))/sum(1/(table(data[predvar])/nrow(data)))
# #classwt = rep(1/length(unique(unlist(data[predvar]))),length(unique(unlist(data[predvar]))))
# 
# prem = matrix(c(10, 74, 10), ncol = 3)
# colnames(prem) = c('ntree', 'mtry', 'nodesize')
# classwt = table(data[predvar])/nrow(data)
# kk = 1
# ptm = proc.time()
# result = list()
#   model.rf_1 = randomForest(train_data, 
#                           as.factor(train_label),
#                           ytest = as.factor(test_label), ntree=prem[kk,1], mtry=prem[kk,2], importance=TRUE,
#                           classwt = classwt, keep.forest=TRUE, nodesize = prem[kk,3], replace = FALSE)
#   
#   train_pred = predict(model.rf, train_data, type="response")
#   test_pred = predict(model.rf, test_data, type="response")
#   
#   train_errortable1 = cbind(prem,
#                             as.data.frame(maeoucom(cbind(data['SPOSTMIN'], as.numeric(as.character(train_pred))))))
# 
#   test_errortable1 = cbind(prem,
#                            as.data.frame(maeoucom(cbind(tdata['SPOSTMIN'], as.numeric(as.character(test_pred))))))
#   
#   train_errortable1
#   test_errortable1
# 
# ptm1 = proc.time()
# 
# print(ptm1 - ptm)
# 
# 
# saveRDS(model.rf, paste0(datapath, "Random_Forest_For_EP09_1.rds", collapse = ''))
# Random_Forest_For_EP09 <- readRDS(paste0(datapath, "Random_Forest_For_EP09_1.rds", collapse = ''))
# 
# 
# 
