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

dataname = c("MK04")
test_data_name = c("MK04_predict")
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
test_data[is.na(test_data)] = 0

maxx = max(test_data$INDEX)

data["INDEX"] = c((maxx+1):(maxx+nrow(data)))

combine_data = rbind(data, test_data)


aa = combine_data[ , !(names(combine_data) %in% c('INDEX','SPOSTMIN1', 'SPOSTMIN', "DAYHOURGROUP"))]
nn = names(aa)
for(i in 1:length(nn)){
  a = hist(unlist(aa[nn[i]]), plot = FALSE)$breaks
  a = c(-Inf, a, Inf)
  bb = cut(unlist(aa[nn[i]]), a, labels = FALSE)
  aa[nn[i]] = bb
}

combine_data = as.data.frame(cbind(combine_data[c('INDEX','SPOSTMIN1', 'SPOSTMIN', "DAYHOURGROUP")], aa))

combine_data = apply(combine_data, 2, as.factor)
data = as.data.frame(combine_data[1:163254,])
test_data = as.data.frame(combine_data[163255:nrow(combine_data),])
data['INDEX'] = NULL
# Model Fit for Train data

fit  = randomforeset_func(data, prem = c(ntree = 500, mtry = 44, nodesize = 3), target = "SPOSTMIN1", seed = 12354)

# Predict Train data and find the accuracy and MSE, MAE, and Average Over and under Prediction

pred = pred_func_rf(data, model = fit$model)
as.data.frame(maeoucom(pred$pred_data[c("SPOSTMIN", "PRED_LABEL")]))

# Predict Test Data

test_data_pred = pred_func_rf(test_data, model = fit$model)

test_data_pred = test_data_pred$pred_data

# Save fitted Model and Read fitted Model
saveRDS(fit$model, paste0(datapath, "Random_Forest_For_MK04.rds", collapse = ''))
Random_Forest_For_MK04 <- readRDS(paste0(datapath, "Random_Forest_For_MK04.rds", collapse = ''))


test=  read.csv(paste0(datapath, test_data_name[1], ".CSV", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
test1=  read.csv(paste0(datapath, "MK04 TEST DATA PREDICTION.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")


test_pred = cbind(test1[c("INDEX", "PRED_LABEL")], test)

write.csv(test_pred, file = paste0(datapath, "MK04 TEST DATA PREDICTION.csv", collapse = ''), row.names = FALSE)
