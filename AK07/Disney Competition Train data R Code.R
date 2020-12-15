

train_data_prep <- function(data = NULL, dataname = NULL, 
                            test_data_prop = NULL, test = TRUE,
                            datapath, codepath, seed = NULL){
  datapath = datapath
  codepath = codepath
  if ((!is.null(data) & !is.null(dataname))){
    message("Either input Train Data or Train Data File name")
    return()
  }
  
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
  
  
  
  function_file_name =  "Disney Competition R Function.R"
  
  source(paste0(codepath, function_file_name, collapse = ''))
  source(paste0(codepath, "Disney Competition Test data R Code.R", collapse = ''))
  
  
  if(is.null(data) & !is.null(dataname)){
    for (i in 1:length(dataname)){
      data1 =  read.csv(paste0(datapath, dataname[i], ".CSV", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
      if (i==1) {data = data1
      }else {data = rbind(data, data1)}
    }
    rm(data1, i)
    data = as.data.frame(data)
    
  }else if(!is.null(data) & is.null(dataname)){
    data = data
    data = as.data.frame(data)
  }else print("You must input either Data File or list of File Name but not both")
  
  #############################################
  uniqSATTID = unique(data$SATTID)
  uniqNEWNESS = unique(data$NEWNESS)
  if(uniqSATTID == 'AK07'){
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
  }else if(uniqSATTID == 'EP09'){
    data$SPOSTMIN1 = ifelse(data$SPOSTMIN == 0, 5, data$SPOSTMIN)
    data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 >= 115 & data$SPOSTMIN1 <=130, 120, data$SPOSTMIN1)
    data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 > 130, 150, data$SPOSTMIN1)
  }else if(uniqSATTID == 'HS20'){
    data$SPOSTMIN1 = ifelse(data$SPOSTMIN == 0, 5, data$SPOSTMIN)
    data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 == 125, 130, data$SPOSTMIN1)
    data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 == 135, 130, data$SPOSTMIN1)
    data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 >= 140, 150, data$SPOSTMIN1)
  }else if(uniqSATTID == 'MK04'){
    data$SPOSTMIN1 = ifelse(data$SPOSTMIN == 0, 5, data$SPOSTMIN)
    data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 == 85, 90, data$SPOSTMIN1)
    data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 == 95, 100, data$SPOSTMIN1)
    data$SPOSTMIN1 = ifelse(data$SPOSTMIN1 >= 105, 110, data$SPOSTMIN1)
  }
  nnrow = nrow(data)
  ### Split data into training and test
  if (test){
    set.seed(ifelse(!is.null(seed), seed, proc.time()))
    test_data_prop = ifelse(!is.null(test_data_prop), test_data_prop, 0.75)
    smp_size <- floor(test_data_prop * nnrow)
    train_ind <- sample(seq_len(nnrow), size = smp_size)
    
    train <- data[train_ind, ]
    test_data <- data[-train_ind, ]
    data = train
    rm(train, smp_size, train_ind)
  }
  
  
  ## Missing variable 
  
  # miss = misper(test_data)
  # miss = miss[miss >0]
  
  
  
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
  data["DAYMINUTE"] = round_any(as.numeric(format(as.POSIXct(data$TIMEPART,format="%H:%M"),"%M")), 5, f = round)
  data['WEEKDAYS'] = weekdays(data$DATE)
  data["SUMHOUR"] = rowSums(data[,c("MKHOURS","HSHOURS","AKHOURS","IAHOURS","UFHOURS")],na.rm = TRUE)
  data["DAYEVENTBA"] = rowSums(data[,c("MKDAYSBEFORENONEVENT", "MKDAYSSINCENONEVENT", "MKEVENTSTREAK", "MKEVENTSTREAK_F")],na.rm = TRUE)
  
  data["DAYHOURGROUP"] = ifelse(data$DAYHOUR >= 9 & data$DAYHOUR <= 21, data$DAYHOUR, 22)
  
  
  ## Dummy variable for Alphanumeric
  
  savefilename = NULL
  carvar = c("HOLIDAYN", "HOLIDAYJ", "PARTYSEASON_WDW", "SEASON", "NEWNESS")
  data[carvar][data[,carvar]==""] = as.character("NOT KNOWN")

  
  data = create_dummy(data = data, carvar = 'DAYOFWEEK', cn_1 = 0)
  
 
  carvarlist = NULL
 for(i in 1:length(carvar)){
  if(length(unique(unlist(data[carvar[i]]))) > 1){
    data[carvar][data[,carvar]==""] = as.character("NOT KNOWN")
    carvarlist = append(carvarlist, carvar[i])
    mmen = as.data.frame(as.list(aggregate(as.formula(paste("SPOSTMIN ~ DAYHOURGROUP +", carvar[i])), data = data,  mean)))
    names(mmen) = c("DAYHOURGROUP", carvar[i], paste0(carvar[i],"_mean", collapse = ""))
    write.csv(mmen, file = paste0(datapath, carvar[i], ".csv", collapse = ''), row.names = FALSE)
    savefilename = append(savefilename, paste0(carvar[i], ".csv", collapse = ''))
    data = merge(data, mmen, by = c("DAYHOURGROUP", carvar[i]), all.x = TRUE)
  }
 } 
  write.csv(carvarlist, file = paste0(datapath, "carvarlist.csv", collapse = ''), row.names = FALSE)

  
  data[is.na(data)] = 0
  
  closetime = c("MKCLOSE", "UFCLOSE", "IACLOSE" , "HSCLOSE", "AKCLOSE")
  
  if(any(uniqSATTID %in% "MK04")){
    data$DMY_MKCLOSE = ifelse(((hour(data$MKCLOSE)-1) <= hour(data$TIMEPARTROUND)) & data$SATTID == "MK04", 1,0)
  }
  data$DMY_IACLOSE = ifelse(((hour(data$IACLOSE)-1) <= hour(data$TIMEPARTROUND)), 1,0)
  data$DMY_UFCLOSE = ifelse(((hour(data$UFCLOSE)-1) <= hour(data$TIMEPARTROUND)), 1,0)
  
  if(any(uniqSATTID %in% "HS20")){
    data$DMY_HSCLOSE = ifelse(((hour(data$HSCLOSE)-1) <= hour(data$TIMEPARTROUND)) & data$SATTID == "HS20", 1,0)
  }
  if(any(uniqSATTID %in% "AK07")){
    data$DMY_AKCLOSE = ifelse(((hour(data$AKCLOSE)-1) <= hour(data$TIMEPARTROUND)) & data$SATTID == "AK07", 1,0)
  } 
  
  
  ## Lag variable
  lagtime = as.data.frame(as.list(aggregate(SPOSTMIN ~ WEEKDAYS + DAYHOURGROUP, data = data, function(x) agfunc(x))))
  names(lagtime) = c("WEEKDAYS", "DAYHOURGROUP","LAG_M", "LAG_Q0", "LAG_Q1", "LAG_Q2", "LAG_Q3", "LAG_Q4")
  write.csv(lagtime, file = paste0(datapath, "lagtime.csv", collapse = ''), row.names = FALSE)
  savefilename = append(savefilename, "lagtime.csv")
  data = merge(data, lagtime, by = c("WEEKDAYS", "DAYHOURGROUP"), all.x = TRUE)
  
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
  savefilename = append(savefilename, "smeanvec.csv")
  write.csv(ssdmat, file = paste0(datapath, "ssdmat.csv", collapse = ''), row.names = FALSE)
  savefilename = append(savefilename, "ssdmat.csv")
  data[schoolvar] = scale(data[schoolvar])
  pca1 = prcomp(data[schoolvar])
  data[c("SCHOOL_PCA1", "SCHOOL_PCA2", "SCHOOL_PCA3", "SCHOOL_PCA4")] = pca1$x[,1:4]
  srotaction = pca1$rotation[,1:4]
  write.csv(srotaction, file = paste0(datapath, "srotaction.csv", collapse = ''), row.names = FALSE)
  savefilename = append(savefilename, "srotaction.csv")
  data$SCHOOL = rowSums(data[schoolvar],na.rm = TRUE)
  
  booleanvar = c("MKEMHMORN", "MKEMHMYEST", "MKEMHMTOM", "MKEMHEVE", "MKEMHEYEST", 
                 "MKEMHETOM", "EPEMHMORN", "EPEMHMYEST", "EPEMHMTOM", "EPEMHEVE", 
                 "EPEMHEYEST", "EPEMHETOM", "HSEMHMORN", "HSEMHMYEST", "HSEMHMTOM", 
                 "HSEMHEVE", "HSEMHEYEST", "HSEMHETOM", "AKEMHMORN", "AKEMHMYEST", 
                 "AKEMHMTOM", "AKEMHEVE", "AKEMHEYEST", "AKEMHETOM")
  data['BOOLVAR_DMY'] = rowSums(data[booleanvar],na.rm = TRUE)
  
  booleanvar = c("MKEMHMORN", "MKEMHMYEST", "MKEMHMTOM", "MKEMHEVE", "MKEMHEYEST",
                 "MKEMHETOM", "EPEMHMORN", "EPEMHMYEST", "EPEMHMTOM", "EPEMHEVE",
                 "EPEMHEYEST", "EPEMHETOM", "HSEMHMORN", "HSEMHMYEST", "HSEMHMTOM",
                 "HSEMHEVE", "HSEMHEYEST", "HSEMHETOM", "AKEMHMORN", "AKEMHMYEST",
                 "AKEMHMTOM", "AKEMHEVE", "AKEMHEYEST", "AKEMHETOM", 'BOOLVAR_DMY')
  boolvar1 = NULL
   for(i in 1:length(booleanvar)){
    if(length(unique(unlist(data[booleanvar[i]]))) > 1){
      boolvar1 = append(boolvar1, booleanvar[i])
      mmen = as.data.frame(as.list(aggregate(as.formula(paste("SPOSTMIN ~ WEEKDAYS + DAYHOURGROUP +", booleanvar[i])), data = data,  mean)))
      names(mmen) = c("WEEKDAYS","DAYHOURGROUP", booleanvar[i], paste0(booleanvar[i],"_mean", collapse = ""))
      write.csv(mmen, file = paste0(datapath, booleanvar[i], ".csv", collapse = ''), row.names = FALSE)
      savefilename = append(savefilename, paste0(booleanvar[i], ".csv", collapse = ''))
      data = merge(data, mmen, by = c("WEEKDAYS", "DAYHOURGROUP", booleanvar[i]), all.x = TRUE)
    }
  }

  write.csv(boolvar1, file = paste0(datapath, "boolvar1.csv", collapse = ''), row.names = FALSE)
  savefilename = append(savefilename, "boolvar1.csv")

  boolvar1 = boolvar1[!(boolvar1 %in% c('BOOLVAR_DMY', "HOLIDAYM", "HOLIDAYPX"))]
  boolvar1 = paste(boolvar1, "_mean",sep = "")
  data['BOOLVAR'] = rowSums(data[boolvar1],na.rm = TRUE)
  

  
  meanvecbb = colMeans(data[boolvar1])
  sdmatbb = sqrt(diag(var(data[boolvar1])))
  write.csv(meanvecbb, file = paste0(datapath, "meanvecbb.csv", collapse = ''), row.names = FALSE)
  savefilename = append(savefilename, "meanvecbb.csv")
  write.csv(sdmatbb, file = paste0(datapath, "sdmatbb.csv", collapse = ''), row.names = FALSE)
  savefilename = append(savefilename, "sdmatbb.csv")
  data[boolvar1] = scale(data[boolvar1])
  pca = prcomp(data[boolvar1])
  data[c("BOOLVAR_PCA1", "BOOLVAR_PCA2")] = pca$x[,1:2]

  rotactionbb = pca$rotation[,1:2]
  write.csv(rotactionbb, file = paste0(datapath, "rotactionbb.csv", collapse = ''), row.names = FALSE)
  savefilename = append(savefilename, "rotactionbb.csv")
  
  capacityvar = c("CAPACITYLOST_MK", "CAPACITYLOST_EP", "CAPACITYLOST_HS",
                  "CAPACITYLOST_AK", "CAPACITYLOSTWGT_MK", "CAPACITYLOSTWGT_EP",
                  "CAPACITYLOSTWGT_HS", "CAPACITYLOSTWGT_AK", "EP09CAPACITY", "HS20CAPACITY")


  meanvec = colMeans(data[capacityvar])
  sdmat = sqrt(diag(var(data[capacityvar])))
  write.csv(meanvec, file = paste0(datapath, "meanvec.csv", collapse = ''), row.names = FALSE)
  savefilename = append(savefilename, "meanvec.csv")
  write.csv(sdmat, file = paste0(datapath, "sdmat.csv", collapse = ''), row.names = FALSE)
  savefilename = append(savefilename, "sdmat.csv")
  data[capacityvar] = scale(data[capacityvar])
  pca = prcomp(data[capacityvar])
  data[c("CAPACITY_PCA1", "CAPACITY_PCA2")] = pca$x[,1:2]

  rotaction = pca$rotation[,1:2]
  write.csv(rotaction, file = paste0(datapath, "rotaction.csv", collapse = ''), row.names = FALSE)
  savefilename = append(savefilename, "rotaction.csv")

  timevar11 = c("MONTHOFYEAR", "DAYMINUTE",  "DAYEVENTBA")
  
  
  for(i in 1:length(timevar11)){
    if(length(unique(unlist(data[timevar11[i]]))) > 1){
      mmen = as.data.frame(as.list(aggregate(as.formula(paste("SPOSTMIN ~ WEEKDAYS + DAYHOURGROUP +", timevar11[i])), data = data,  mean)))
      names(mmen) = c("WEEKDAYS",  'DAYHOURGROUP', timevar11[i], paste0(timevar11[i],"_mean", collapse = ""))
      write.csv(mmen, file = paste0(datapath, timevar11[i], ".csv", collapse = ''), row.names = FALSE)
      savefilename = append(savefilename, paste0(timevar11[i], ".csv", collapse = ''))
      data = merge(data, mmen, by = c("WEEKDAYS", "DAYHOURGROUP", timevar11[i]), all.x = TRUE)
    }
  }
  

  ## Remove Variable
  
  carvar = c("SATTID", "DLR_TICKET_SEASON", "WDW_TICKET_SEASON", "SEASON", 
             "PARTYSEASON_WDW", "WEEKDAYS", "NEWNESS", "WGT")
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
               'AKOPEN', 'AKCLOSE', 'TIMEPARTROUND', 'DAYHOUR')
  data[tiemvar1] = NULL
  
  

  booleanvar = c("MKEMHMORN", "MKEMHMYEST", "MKEMHMTOM", "MKEMHEVE", "MKEMHEYEST",
                 "MKEMHETOM", "EPEMHMORN", "EPEMHMYEST", "EPEMHMTOM", "EPEMHEVE",
                 "EPEMHEYEST", "EPEMHETOM", "HSEMHMORN", "HSEMHMYEST", "HSEMHMTOM",
                 "HSEMHEVE", "HSEMHEYEST", "HSEMHETOM", "AKEMHMORN", "AKEMHMYEST",
                 "AKEMHMTOM", "AKEMHEVE", "AKEMHEYEST", "AKEMHETOM")
  data[booleanvar] = NULL
  data[boolvar1] = NULL

  schoolvar = c("INSESSION", "INSESSION_ENROLLMENT", "INSESSION_WDW", "INSESSION_DLR", "INSESSION_SQRT_WDW",
                "INSESSION_SQRT_DLR", "INSESSION_REGWDW", "INSESSION_CALIFORNIA", "INSESSION_DC",
                "INSESSION_DRIVE1_FL", "INSESSION_DRIVE2_FL", "INSESSION_DRIVE_CA", "INSESSION_FLORIDA",
                "INSESSION_MARDI_GRAS", "INSESSION_MIDWEST", "INSESSION_NY_NJ", "INSESSION_NY_NJ_PA",
                "INSESSION_NEW_ENGLAND", "INSESSION_NEW_JERSEY", "INSESSION_NOTHWEST",
                "INSESSION_PLANES", "INSESSION_SOCAL", "INSESSION_SOUTHWEST")
  data[schoolvar] = NULL

  delvar = c("YEAR")
  data[delvar] = NULL

  dayevet = c("MKDAYSBEFORENONEVENT", "MKDAYSSINCENONEVENT", "MKEVENTSTREAK", "MKEVENTSTREAK_F")
  data[dayevet] = NULL
  
  
  hourvar = c("MKHOURS","HSHOURS","AKHOURS","IAHOURS","UFHOURS")
  data[hourvar] = NULL
  
  
  timevar11 = c("DAYOFWEEK", "DAYOFYEAR", "WEEKOFYEAR")
  
  data[timevar11] = NULL
  
  allmeanvecmiss = as.data.frame(colMeans(data[ , !(names(data) %in% c('SPOSTMIN1', 'SPOSTMIN'))]))
  names(allmeanvecmiss) = c("Mean")
  write.csv(allmeanvecmiss, file = paste0(datapath, "allmeanvecmiss.csv", collapse = ''))
  
  impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
  data11 <- apply(data[ , !(names(data) %in% c('SPOSTMIN1', 'SPOSTMIN'))], 2, impute.mean)
  data = cbind(data[c('SPOSTMIN1', 'SPOSTMIN')], data11)
  
  
  varname = names(data)
  write.csv(varname, file = paste0(datapath, "varname.csv", collapse = ''), row.names = FALSE)
  savefilename = append(savefilename, "varname.csv")
  write.csv(savefilename, file = paste0(datapath, "savefilename.csv", collapse = ''), row.names = FALSE)
  

  if (test){
    return(list(Train_Data = data, Test_Data = test_data))
  }else {
    return(list(Train_Data = data))
  }
}