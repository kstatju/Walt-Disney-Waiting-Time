
if(!require(dummies, quietly =T))  {  install.packages("dummies");  require(dummies, quietly =TRUE)}
if(!require(plyr, quietly =T))  {  install.packages("plyr");  require(plyr, quietly =TRUE)}
if(!require(ggplot2, quietly =T))  {  install.packages("ggplot2");  require(ggplot2, quietly =TRUE)}
if(!require(stats, quietly =T))  {  install.packages("stats");  require(stats, quietly =TRUE)}
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
#dataname = c("AK07.csv")
dataname = c("AK07.csv", "EP09.csv", "HS20.csv", "MK04.csv")

function_file_name =  "Disney Competition R Function.R"

source(paste0(codepath, function_file_name, collapse = ''))
source(paste0(codepath, "Disney Competition Test data R Code.R", collapse = ''))

for (i in 1:length(dataname)){
  data1 =  read.csv(paste0(datapath, dataname[i], collapse = ''), stringsAsFactors = FALSE)
  if (i==1) data = data1
  else data = rbind(data, data1)
}
rm(data1, i)
data[data==""] = NA
data1 = as.data.frame(data)
data = data1

### Split data into training and test
set.seed(12325543)
smp_size <- floor(0.75 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]


data = train
rm(train)

## Missing variable 

miss = misper(data)
miss = miss[miss >90]



### change Time variable

data$DATE <- as.Date(data$DATE, origin = "1899-12-30")
data$TIMEPART = as.POSIXct(data$TIMEPART * 86400, origin = "1970-01-01", tz = "UTC")
data$MKCLOSE = round(strptime(as.POSIXct(data$MKCLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
data$IACLOSE = round(strptime(as.POSIXct(data$IACLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
data$UFCLOSE = round(strptime(as.POSIXct(data$UFCLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
data$HSCLOSE = round(strptime(as.POSIXct(data$HSCLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
data$AKCLOSE = round(strptime(as.POSIXct(data$AKCLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
data$TIMEPARTROUND = round(strptime(data$TIMEPART, format="%Y-%m-%d %H:%M:%S"), units="hours")


## Create New variables Hour of Day and Weekdays

data['DAYHOUR'] = as.numeric(format(as.POSIXct(data$TIMEPARTROUND,format="%H:%M"),"%H"))
data["DAYMINUTE"] = as.numeric(format(as.POSIXct(data$TIMEPART,format="%H:%M"),"%M"))
data['WEEKDAYS'] = weekdays(data$DATE)


data$SPOSTMIN1 = ifelse(data$SPOSTMIN == 0, 5, data$SPOSTMIN)
data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 == 15, 20, data$SPOSTMIN1)
data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 == 25, 30, data$SPOSTMIN1)
data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 == 35, 40, data$SPOSTMIN1)
data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 == 45, 50, data$SPOSTMIN1)
data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 == 55, 60, data$SPOSTMIN1)
data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 == 65, 70, data$SPOSTMIN1)
data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 == 75, 80, data$SPOSTMIN1)
data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 == 85, 90, data$SPOSTMIN1)
data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 == 95, 100, data$SPOSTMIN1)
data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 == 105, 110, data$SPOSTMIN1)
data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 >= 115, 120, data$SPOSTMIN1)

data["SUMHOUR"] = rowSums(data[,c("MKHOURS","HSHOURS","AKHOURS","IAHOURS","UFHOURS")])
data["DAYEVENTBA"] = rowSums(data[,c("MKDAYSBEFORENONEVENT", "MKDAYSSINCENONEVENT", "MKEVENTSTREAK", "MKEVENTSTREAK_F")])


## Dummy variable for Alphanumeric
data$DLR_TICKET_SEASON[is.na(data$DLR_TICKET_SEASON)] = "Unknown"
data$WDW_TICKET_SEASON[is.na(data$WDW_TICKET_SEASON)] = "Unknown"

carvar = c("SATTID", "PARTYSEASON_WDW", "SEASON", "DLR_TICKET_SEASON", "WDW_TICKET_SEASON", "NEWNESS")
data = create_dummy(data = data, carvar = carvar, cn_1 = 0)




season = as.data.frame(sort(table(data$SEASON))/nrow(data)*100)
names(season) = c("SEASON", 'SEASON_PROP')
write.csv(season, file = paste0(datapath, "season.csv", collapse = ''), row.names = FALSE)
data = merge(data, season, by = "SEASON")

DLR_TICKET_SEASON = as.data.frame(sort(table(data$DLR_TICKET_SEASON))/nrow(data)*100)
names(DLR_TICKET_SEASON) = c("DLR_TICKET_SEASON", 'DLR_TICKET_SEASON_PROP')
write.csv(DLR_TICKET_SEASON, file = paste0(datapath, "DLR_TICKET_SEASON.csv", collapse = ''), row.names = FALSE)
data = merge(data, DLR_TICKET_SEASON, by = "DLR_TICKET_SEASON")

WDW_TICKET_SEASON = as.data.frame(sort(table(data$WDW_TICKET_SEASON))/nrow(data)*100)
names(WDW_TICKET_SEASON) = c("WDW_TICKET_SEASON", 'WDW_TICKET_SEASON_PROP')
write.csv(WDW_TICKET_SEASON, file = paste0(datapath, "WDW_TICKET_SEASON.csv", collapse = ''), row.names = FALSE)
data = merge(data, WDW_TICKET_SEASON, by = "WDW_TICKET_SEASON")


PARTYSEASON_WDW = as.data.frame(sort(table(data$PARTYSEASON_WDW))/nrow(data)*100)
names(PARTYSEASON_WDW) = c("PARTYSEASON_WDW", 'PARTYSEASON_WDW_PROP')
write.csv(PARTYSEASON_WDW, file = paste0(datapath, "PARTYSEASON_WDW.csv", collapse = ''), row.names = FALSE)
data = merge(data, PARTYSEASON_WDW, by = "PARTYSEASON_WDW")

if (length(unique(data$NEWNESS)) > 1){
  NEWNESS = as.data.frame(sort(table(data$NEWNESS))/nrow(data)*100)
  names(NEWNESS) = c("NEWNESS", 'NEWNESS_PROP')
  write.csv(NEWNESS, file = paste0(datapath, "NEWNESS.csv", collapse = ''), row.names = FALSE)
  data = merge(data, NEWNESS, by = "NEWNESS")
}



closetime = c("MKCLOSE", "UFCLOSE", "IACLOSE" , "HSCLOSE", "AKCLOSE")



data$DMY_MKCLOSE = ifelse(((hour(data$MKCLOSE)-1) <= hour(data$TIMEPARTROUND)) & data$SATTID == "MK04", 1,0)
data$DMY_IACLOSE = ifelse(((hour(data$IACLOSE)-1) <= hour(data$TIMEPARTROUND)), 1,0)
data$DMY_UFCLOSE = ifelse(((hour(data$UFCLOSE)-1) <= hour(data$TIMEPARTROUND)), 1,0)
data$DMY_HSCLOSE = ifelse(((hour(data$HSCLOSE)-1) <= hour(data$TIMEPARTROUND)) & data$SATTID == "HS20", 1,0)
data$DMY_AKCLOSE = ifelse(((hour(data$AKCLOSE)-1) <= hour(data$TIMEPARTROUND)) & data$SATTID == "AK07", 1,0)



## Lag variable
lagtime = as.data.frame(as.list(aggregate(SPOSTMIN ~ SATTID + WEEKDAYS + DAYHOUR, data = data, function(x) agfunc(x))))
names(lagtime) = c("SATTID", "WEEKDAYS", "DAYHOUR", "LAG_M", "LAG_Q0", "LAG_Q1", "LAG_Q2", "LAG_Q3", "LAG_Q4")
write.csv(lagtime, file = paste0(datapath, "lagtime.csv", collapse = ''), row.names = FALSE)

data = merge(data, lagtime, by = c("SATTID", "WEEKDAYS", "DAYHOUR"))

# data = data[order(data$SATTID, data$WEEKDAYS, data$DAYHOUR, data$DATE),]
# 
# data$LAG_1 =lag(data$SPOSTMIN)
# data$LAG_1[1] = data$SPOSTMIN[1]
# data$LAG_2 =lag(data$SPOSTMIN, 2)
# data$LAG_2[1:2] = data$SPOSTMIN[1:2]
# data$LAG_3 =lag(data$SPOSTMIN, 3)
# data$LAG_3[1:3] = data$SPOSTMIN[1:3]
# data$LAG_4 =lag(data$SPOSTMIN, 4)
# data$LAG_4[1:4] = data$SPOSTMIN[1:4]
# data$LAG_5 =lag(data$SPOSTMIN, 5)
# data$LAG_5[1:5] = data$SPOSTMIN[1:5]
# data$LAG_6 =lag(data$SPOSTMIN, 6)
# data$LAG_6[1:6] = data$SPOSTMIN[1:6]
# data$LAG_7 =lag(data$SPOSTMIN, 7)
# data$LAG_7[1:7] = data$SPOSTMIN[1:7]
# data$LAG_8 =lag(data$SPOSTMIN, 8)
# data$LAG_8[1:8] = data$SPOSTMIN[1:8]


## Create sum of School Vars

schoolvar = c("INSESSION", "INSESSION_ENROLLMENT", "INSESSION_WDW", "INSESSION_DLR", "INSESSION_SQRT_WDW", 
              "INSESSION_SQRT_DLR", "INSESSION_REGWDW", "INSESSION_CALIFORNIA", "INSESSION_DC", 
              "INSESSION_DRIVE1_FL", "INSESSION_DRIVE2_FL", "INSESSION_DRIVE_CA", "INSESSION_FLORIDA",
              "INSESSION_MARDI_GRAS", "INSESSION_MIDWEST", "INSESSION_NY_NJ", "INSESSION_NY_NJ_PA",
              "INSESSION_NEW_ENGLAND", "INSESSION_NEW_JERSEY", "INSESSION_NOTHWEST", 
              "INSESSION_PLANES", "INSESSION_SOCAL", "INSESSION_SOUTHWEST")

data[schoolvar][is.na(data[schoolvar])] = 0

smeanvec = colMeans(data[schoolvar])
ssdmat = sqrt(diag(var(data[schoolvar])))
write.csv(smeanvec, file = paste0(datapath, "smeanvec.csv", collapse = ''), row.names = FALSE)
write.csv(ssdmat, file = paste0(datapath, "ssdmat.csv", collapse = ''), row.names = FALSE)

data[schoolvar] = scale(data[schoolvar])
pca1 = prcomp(data[schoolvar])
data[c("SCHOOL_PCA1", "SCHOOL_PCA2", "SCHOOL_PCA3", "SCHOOL_PCA4")] = pca1$x[,1:4]
srotaction = pca1$rotation[,1:4]
write.csv(srotaction, file = paste0(datapath, "srotaction.csv", collapse = ''), row.names = FALSE)

data$SCHOOL = rowSums(data[schoolvar])

# data = data[with(data, order(SATTID, WEEKDAYS, DAYHOUR, DATE)),]
# data = cbind(data, ma(data[schoolvar], n=10, combind = FALSE))


booleanvar = c("MKEMHMORN", "MKEMHMYEST", "MKEMHMTOM", "MKEMHEVE", "MKEMHEYEST", 
               "MKEMHETOM", "EPEMHMORN", "EPEMHMYEST", "EPEMHMTOM", "EPEMHEVE", 
               "EPEMHEYEST", "EPEMHETOM", "HSEMHMORN", "HSEMHMYEST", "HSEMHMTOM", 
               "HSEMHEVE", "HSEMHEYEST", "HSEMHETOM", "AKEMHMORN", "AKEMHMYEST", 
               "AKEMHMTOM", "AKEMHEVE", "AKEMHEYEST", "AKEMHETOM")
boolmeanve = data.frame(varname= character(), mean= numeric(), stringsAsFactors = FALSE)
for (i in 1:length(booleanvar)){
  mmean = mean(data[,booleanvar[i]])*100
  boolmeanve[i,] = list(varname = as.character(booleanvar[i]), mean = mmean)
  data[,booleanvar[i]] = ifelse(data[,booleanvar[i]] == 1, mmean, 0)
}

boolmeanve = as.data.frame(boolmeanve)
write.csv(boolmeanve, file = paste0(datapath, "boolmeanve.csv", collapse = ''), row.names = FALSE)


data['BOOLVAR'] = rowSums(data[booleanvar])


capacityvar = c("CAPACITYLOST_MK", "CAPACITYLOST_EP", "CAPACITYLOST_HS", 
                "CAPACITYLOST_AK", "CAPACITYLOSTWGT_MK", "CAPACITYLOSTWGT_EP", 
                "CAPACITYLOSTWGT_HS", "CAPACITYLOSTWGT_AK", "EP09CAPACITY", "HS20CAPACITY")


meanvec = colMeans(data[capacityvar])
sdmat = sqrt(diag(var(data[capacityvar])))
write.csv(meanvec, file = paste0(datapath, "meanvec.csv", collapse = ''), row.names = FALSE)
write.csv(sdmat, file = paste0(datapath, "sdmat.csv", collapse = ''), row.names = FALSE)

data[capacityvar] = scale(data[capacityvar])
pca = prcomp(data[capacityvar])
data[c("CAPACITY_PCA1", "CAPACITY_PCA2")] = pca$x[,1:2]

rotaction = pca$rotation[,1:2]
write.csv(rotaction, file = paste0(datapath, "rotaction.csv", collapse = ''), row.names = FALSE)

# data = data[with(data, order(SATTID, WEEKDAYS, DAYHOUR, DATE)),]
# data = cbind(data, ma(data[capacityvar], n=10, combind = FALSE))


### Create label for xgboost
unilabel = sort(unique(data$SPOSTMIN1))
labeldata = as.data.frame(cbind(unilabel, c(0:(length(unilabel)-1))))
names(labeldata) = c("SPOSTMIN1", 'LABEL')
write.csv(labeldata, file = paste0(datapath, "labeldata.csv", collapse = ''), row.names = FALSE)

data = merge(data, labeldata, by = 'SPOSTMIN1')





## Remove Variable

carvar = c("SATTID", "DLR_TICKET_SEASON", "WDW_TICKET_SEASON", "SEASON", 
           "PARTYSEASON_WDW", "WEEKDAYS", "NEWNESS")
data[carvar] = NULL

data['DATE'] = NULL


remvar = c("IAUEPRICE", "UFUEPRICE", "UOR2PARKUEPRICE", "HOLIDAYN", "HOLIDAYJ", 'SUNSET', 'OVERLAY' )
data[remvar] = NULL


capacityvar = c("CAPACITYLOST_MK", "CAPACITYLOST_EP", "CAPACITYLOST_HS",
                "CAPACITYLOST_AK", "CAPACITYLOSTWGT_MK", "CAPACITYLOSTWGT_EP",
                "CAPACITYLOSTWGT_HS", "CAPACITYLOSTWGT_AK", "EP09CAPACITY", "HS20CAPACITY")
data[capacityvar] = NULL


tiemvar1 = c('TIMEPART', 'IAOPEN', 'IACLOSE', 'UFOPEN', 'UFCLOSE', 'MKOPEN',
             'MKCLOSE', 'HSOPEN', 'HSCLOSE',
             'AKOPEN', 'AKCLOSE', 'TIMEPARTROUND')
data[tiemvar1] = NULL



booleanvar = c("MKEMHMORN", "MKEMHMYEST", "MKEMHMTOM", "MKEMHEVE", "MKEMHEYEST",
               "MKEMHETOM", "EPEMHMORN", "EPEMHMYEST", "EPEMHMTOM", "EPEMHEVE",
               "EPEMHEYEST", "EPEMHETOM", "HSEMHMORN", "HSEMHMYEST", "HSEMHMTOM",
               "HSEMHEVE", "HSEMHEYEST", "HSEMHETOM", "AKEMHMORN", "AKEMHMYEST",
               "AKEMHMTOM", "AKEMHEVE", "AKEMHEYEST", "AKEMHETOM")
data[booleanvar] = NULL


schoolvar = c("INSESSION", "INSESSION_ENROLLMENT", "INSESSION_WDW", "INSESSION_DLR", "INSESSION_SQRT_WDW",
              "INSESSION_SQRT_DLR", "INSESSION_REGWDW", "INSESSION_CALIFORNIA", "INSESSION_DC",
              "INSESSION_DRIVE1_FL", "INSESSION_DRIVE2_FL", "INSESSION_DRIVE_CA", "INSESSION_FLORIDA",
              "INSESSION_MARDI_GRAS", "INSESSION_MIDWEST", "INSESSION_NY_NJ", "INSESSION_NY_NJ_PA",
              "INSESSION_NEW_ENGLAND", "INSESSION_NEW_JERSEY", "INSESSION_NOTHWEST",
              "INSESSION_PLANES", "INSESSION_SOCAL", "INSESSION_SOUTHWEST")
data[schoolvar] = NULL

delvar = c("WGT", "HOLIDAYM", "LAG_Q0", "DLR_TICKET_SEASON_PROP", "WEEKOFYEAR",
           "WDW_TICKET_SEASON_PROP",  "YEAR", "PARTYSEASON_WDW_PROP", "MONTHOFYEAR")
data[delvar] = NULL

dayevet = c("MKDAYSBEFORENONEVENT", "MKDAYSSINCENONEVENT", "MKEVENTSTREAK", "MKEVENTSTREAK_F")
data[dayevet] = NULL


hourvar = c("MKHOURS","HSHOURS","AKHOURS","IAHOURS","UFHOURS")
data[hourvar] = NULL



varname = names(data)
write.csv(varname, file = paste0(datapath, "varname.csv", collapse = ''), row.names = FALSE)

allmeanvec = colMeans(data[ , !(names(data) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])
allsdmat = sqrt(diag(var(data[ , !(names(data) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])))
write.csv(allmeanvec, file = paste0(datapath, "allmeanvec.csv", collapse = ''), row.names = FALSE)
write.csv(allsdmat, file = paste0(datapath, "allsdmat.csv", collapse = ''), row.names = FALSE)

#data[ , !(names(data) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))] = scale(data[ , !(names(data) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])

tdata = test_data_prep(data = test, datapath = datapath, codepath = codepath, combind = FALSE)





## Random Forest

classwt = table(train$LABEL)/nrow(train)
# fgl.res <- tuneRF(train[, !(colnames(train) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))], as.factor(train$LABEL), 
#                   mtryStart = 2, ntreeTry = 100, stepFactor= 2)


model.rf = randomForest(train_data, 
                        as.factor(train_label),
                        xtest = valid_data,
                        ytest = as.factor(valid_label), ntree=1000, mtry=61, importance=TRUE,
                        classwt = classwt, keep.forest=TRUE)

train_pred = predict(model.rf, train_data, type="response")
valid_pred = predict(model.rf, valid_data, type="response")
test_pred = predict(model.rf, test_data, type="response")

train_prediction = as.data.frame(cbind(train[c('LABEL', 'SPOSTMIN1', 'SPOSTMIN')],PRED_LABEL = train_pred))

train_prediction = merge(train_prediction, labeldata1, by.x = 'PRED_LABEL', by.y = 'LABEL')

valid_prediction = as.data.frame(cbind(valid[c('LABEL', 'SPOSTMIN1', 'SPOSTMIN')],PRED_LABEL = valid_pred))

valid_prediction = merge(valid_prediction, labeldata1, by.x = 'PRED_LABEL', by.y = 'LABEL')

test_prediction = as.data.frame(cbind(tdata[c('LABEL', 'SPOSTMIN1', 'SPOSTMIN')],PRED_LABEL = test_pred))

test_prediction = merge(test_prediction, labeldata1, by.x = 'PRED_LABEL', by.y = 'LABEL')

train_errortable1 = cbind(matrix(prem1, ncol = 7),as.data.frame(accuracy_error(train_prediction)))
valid_errortable1 = cbind(matrix(prem1, ncol = 7),as.data.frame(accuracy_error(valid_prediction)))
test_errortable1 = cbind(matrix(prem1, ncol = 7),as.data.frame(accuracy_error(test_prediction)))
train_errortable1
valid_errortable1
test_errortable1

ptm1 = proc.time()

print(ptm1 - ptm)












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
#   ggplot(data, aes(x = MKEMHMORN, y = SPOSTMIN , colour = SPOSTMIN )) +
#        geom_point() +  facet_wrap( ~  DAYOFWEEK)
#  
# a = data$BOOLVAR[data$BOOLVAR == 0]

# 
# nm = names(data)
# 
# for (i in 1:length(nm)){
#   df <- rbind(data.frame(aa="Train", obs=unlist(data[,nm[i]])),
#               data.frame(aa="Test", obs=unlist(tdata[,nm[i]])))
#   print(
#     ggplot(df, aes(obs, fill=aa, colour=aa)) +
#       geom_histogram(aes(y=..density..),
#                      lwd=0.2, position = "dodge") +
#       ggtitle(nm[i]))
# }
