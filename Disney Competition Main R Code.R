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



datapath = "C:/Users/ka746940/Desktop/UCF/Competition/WaltDisney Competition/Data/Disney Touring Plans Data/"
codepath = "C:/Users/ka746940/Desktop/UCF/Competition/WaltDisney Competition/R Code/"
dataname = c("AK07")
#dataname = c("AK07", "EP09", "HS20", "MK04")

function_file_name =  "Disney Competition R Function.R"

source(paste0(codepath, function_file_name, collapse = ''))
source(paste0(codepath, "Disney Competition Test data R Code.R", collapse = ''))
source(paste0(codepath, "Disney Competition Train data R Code.R", collapse = ''))



train_test = train_data_prep (data = NULL, test_data = NULL, dataname = dataname, 
                            test_data_name = NULL, create_test_data = TRUE,
                            test_data_prop = 0.75,
                            datapath = datapath, codepath = codepath, combind = FALSE, seed = 123)

data = train_test$Train_Data
tdata = train_test$Test_Data
data1 = train_test$data1

labeldata = read.csv(paste0(datapath, "labeldata.csv", collapse = ''), stringsAsFactors = FALSE)

## Random Forest

set.seed(12325543)
labeldata1 = labeldata
names(labeldata1) = c("SPOSTMIN_PRED", 'LABEL')
smp_size <- floor(0.75 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
valid <- data[-train_ind, ]

predvar = "SPOSTMIN1"
train_data   <- as.matrix(train[ , !(names(train) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])
train_label  <- as.matrix(train[,predvar])
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)

valid_data   <- as.matrix(valid[ , !(names(valid) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])
valid_label  <- as.matrix(valid[,predvar])
valid_matrix <- xgb.DMatrix(data = valid_data, label = valid_label)


test_data   <- as.matrix(tdata[ , !(names(tdata) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])

test_label  <- tdata[,predvar]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)


## Random Forest Tune


# Random Search

ntree = c(500,1000,1500,2000)
mtry = c(30, 40, 50, 60)
prem = expand.grid(ntree, mtry)

classwt = table(train[predvar])/nrow(train)
# fgl.res <- tuneRF(train_data, as.factor(train_label),
#                   mtryStart = 2, ntreeTry = 100, stepFactor= 1.2, classwt = classwt, xtest = valid_data,
#                   ytest = as.factor(valid_label))

prem = matrix(c(1000, 101, 5), ncol = 3)
colnames(prem) = c('ntree', 'mtry', 'nodesize')
kk = 1
ptm = proc.time()
result = list()
for (kk in 1:nrow(prem)){
  model.rf = randomForest(train_data, 
                          as.factor(train_label),
                          xtest = valid_data,
                          ytest = as.factor(valid_label), ntree=prem[kk,1], mtry=prem[kk,2], importance=TRUE,
                          classwt = classwt, keep.forest=TRUE, nodesize = prem[kk,3], replace = FALSE)
  
  train_pred = predict(model.rf, train_data, type="response")
  valid_pred = predict(model.rf, valid_data, type="response")
  test_pred = predict(model.rf, test_data, type="response")
  
  train_prediction = as.data.frame(cbind(train[c('LABEL', 'SPOSTMIN1', 'SPOSTMIN')],PRED_LABEL = train_pred))
  
  valid_prediction = as.data.frame(cbind(valid[c('LABEL', 'SPOSTMIN1', 'SPOSTMIN')],PRED_LABEL = valid_pred))

  test_prediction = as.data.frame(cbind(tdata[c('LABEL', 'SPOSTMIN1', 'SPOSTMIN')],PRED_LABEL = test_pred))
  

  
  if (predvar == 'LABEL'){
    train_prediction = merge(train_prediction, labeldata1, by.x = 'PRED_LABEL', by.y = 'LABEL')
    valid_prediction = merge(valid_prediction, labeldata1, by.x = 'PRED_LABEL', by.y = 'LABEL')
    test_prediction = merge(test_prediction, labeldata1, by.x = 'PRED_LABEL', by.y = 'LABEL')
    accuracyvar = "SPOSTMIN_PRED"
  }else{
    accuracyvar = "PRED_LABEL"
  }
  
  train_errortable1 = cbind(prem,
                            as.data.frame(accuracy_error(train_prediction,
                                                         confivar = c(predvar, "PRED_LABEL"),
                                                         accuracyvar = accuracyvar)))
  valid_errortable1 = cbind(prem,
                           as.data.frame(accuracy_error(valid_prediction,
                                                        confivar = c(predvar, "PRED_LABEL"),
                                                        accuracyvar = accuracyvar)))
  test_errortable1 = cbind(prem,
                           as.data.frame(accuracy_error(test_prediction,
                                                        confivar = c(predvar, "PRED_LABEL"),
                                                        accuracyvar = accuracyvar)))
  
  train_errortable1
  valid_errortable1
  test_errortable1
  result[kk] = list(c(unlist(train_errortable1), unlist(valid_errortable1), unlist(test_errortable1)))
}
ptm1 = proc.time()

print(ptm1 - ptm)

imp  = importance(model.rf)


write.csv(imp, file = paste0(datapath, "imp1.csv", collapse = ''))
