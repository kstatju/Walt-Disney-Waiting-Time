test_data_prep <- function(data = NULL, dataname = NULL, datapath, codepath, combind= FALSE){
  datapath = datapath
  codepath = codepath
  
  function_file_name =  "Disney Competition R Function.R"
  
  source(paste0(codepath, function_file_name, collapse = ''))
  lagtime = read.csv(paste0(datapath, 'lagtime.csv', collapse = ''), stringsAsFactors = FALSE)
  labeldata = read.csv(paste0(datapath, "labeldata.csv", collapse = ''), stringsAsFactors = FALSE)
  
  
  smeanvec = read.csv(paste0(datapath, "smeanvec.csv", collapse = ''), stringsAsFactors = FALSE)
  ssdmat = read.csv(paste0(datapath, "ssdmat.csv", collapse = ''), stringsAsFactors = FALSE)
  srotaction = read.csv(paste0(datapath, "srotaction.csv", collapse = ''), stringsAsFactors = FALSE)
  meanvec = read.csv(paste0(datapath, "meanvec.csv", collapse = ''), stringsAsFactors = FALSE)
  sdmat = read.csv(paste0(datapath, "sdmat.csv", collapse = ''), stringsAsFactors = FALSE)
  rotaction = read.csv(paste0(datapath, "rotaction.csv", collapse = ''), stringsAsFactors = FALSE)
  
  season = read.csv(paste0(datapath, "season.csv", collapse = ''), stringsAsFactors = FALSE)
  DLR_TICKET_SEASON = read.csv(paste0(datapath, "DLR_TICKET_SEASON.csv", collapse = ''), stringsAsFactors = FALSE)
  WDW_TICKET_SEASON = read.csv(paste0(datapath, "WDW_TICKET_SEASON.csv", collapse = ''), stringsAsFactors = FALSE)
  PARTYSEASON_WDW = read.csv(paste0(datapath, "PARTYSEASON_WDW.csv", collapse = ''), stringsAsFactors = FALSE)
  NEWNESS = read.csv(paste0(datapath, "NEWNESS.csv", collapse = ''), stringsAsFactors = FALSE)
  boolmeanve = read.csv(paste0(datapath, "boolmeanve.csv", collapse = ''), stringsAsFactors = FALSE)
  allmeanvec = read.csv(paste0(datapath, "allmeanvec.csv", collapse = ''), stringsAsFactors = FALSE)
  allsdmat = read.csv(paste0(datapath, "allsdmat.csv", collapse = ''), stringsAsFactors = FALSE)
  varname = read.csv(paste0(datapath, "varname.csv", collapse = ''), stringsAsFactors = FALSE)
  
  
  if(is.null(data) & !is.null(dataname)){
    dataname = dataname

    for (i in 1:length(dataname)){
      tdata1 =  read.csv(paste0(datapath, dataname[i], collapse = ''), stringsAsFactors = FALSE)
      if (i==1) tdata = tdata1
      else tdata = rbind(tdata, tdata1)
    }
    rm(tdata1, i)
    tdata[tdata==""] = NA
    tdata = as.data.frame(tdata)
    
  }else if(!is.null(data) & is.null(dataname)){
    tdata = data
    tdata[tdata==""] = NA
    tdata = as.data.frame(tdata)
    }else print("You must input either Data File or list of File Name but not both")
  #######################
  
  
  
  #################
  
  
  ### change Time variable
  
  tdata$DATE <- as.Date(tdata$DATE, origin = "1899-12-30")
  tdata$TIMEPART = as.POSIXct(tdata$TIMEPART * 86400, origin = "1970-01-01", tz = "UTC")
  tdata$MKCLOSE = round(strptime(as.POSIXct(tdata$MKCLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
  tdata$IACLOSE = round(strptime(as.POSIXct(tdata$IACLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
  tdata$UFCLOSE = round(strptime(as.POSIXct(tdata$UFCLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
  tdata$HSCLOSE = round(strptime(as.POSIXct(tdata$HSCLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
  tdata$AKCLOSE = round(strptime(as.POSIXct(tdata$AKCLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
  tdata$TIMEPARTROUND = round(strptime(tdata$TIMEPART, format="%Y-%m-%d %H:%M:%S"), units="hours")
  
  
  
  ## Create New variables Hour of Day and Weekdays
  
  tdata['DAYHOUR'] = as.numeric(format(as.POSIXct(tdata$TIMEPARTROUND,format="%H:%M"),"%H"))
  tdata["DAYMINUTE"] = as.numeric(format(as.POSIXct(tdata$TIMEPART,format="%H:%M"),"%M"))
  tdata['WEEKDAYS'] = weekdays(tdata$DATE)
  
  
  
  tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN == 0, 5, tdata$SPOSTMIN)
  tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 == 15, 20, tdata$SPOSTMIN1)
  tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 == 25, 30, tdata$SPOSTMIN1)
  tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 == 35, 40, tdata$SPOSTMIN1)
  tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 == 45, 50, tdata$SPOSTMIN1)
  tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 == 55, 60, tdata$SPOSTMIN1)
  tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 == 65, 70, tdata$SPOSTMIN1)
  tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 == 75, 80, tdata$SPOSTMIN1)
  tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 == 85, 90, tdata$SPOSTMIN1)
  tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 == 95, 100, tdata$SPOSTMIN1)
  tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 == 105, 110, tdata$SPOSTMIN1)
  tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 >= 115, 120, tdata$SPOSTMIN1)
  
  
  tdata["SUMHOUR"] = rowSums(tdata[,c("MKHOURS","HSHOURS","AKHOURS","IAHOURS","UFHOURS")])

  tdata["DAYEVENTBA"] = rowSums(tdata[,c("MKDAYSBEFORENONEVENT", "MKDAYSSINCENONEVENT", "MKEVENTSTREAK", "MKEVENTSTREAK_F")])
  
  
  ## Dummy variable for Alphanumeric
  uniqSATTID = unique(tdata$SATTID)
  uniqNEWNESS = unique(tdata$NEWNESS)
  tdata$DLR_TICKET_SEASON[is.na(tdata$DLR_TICKET_SEASON)] = "Unknown"
  tdata$WDW_TICKET_SEASON[is.na(tdata$WDW_TICKET_SEASON)] = "Unknown"
  
  if(length(uniqSATTID) > 1){
    carvar = c("SATTID")
    tdata = create_dummy(data = tdata, carvar = carvar, cn_1 = 0)
  }
  
  if(length(uniqNEWNESS) > 1){
    carvar = c("NEWNESS")
    tdata = create_dummy(data = tdata, carvar = carvar, cn_1 = 0)
  }
  

  #carvar = c("PARTYSEASON_WDW", "SEASON", "DLR_TICKET_SEASON", "WDW_TICKET_SEASON")
  # tdata = create_dummy(data = tdata, carvar = carvar, cn_1 = 0)
  

  tdata = merge(tdata, season, by = "SEASON")

  tdata = merge(tdata, DLR_TICKET_SEASON, by = "DLR_TICKET_SEASON")

  tdata = merge(tdata, WDW_TICKET_SEASON, by = "WDW_TICKET_SEASON")

  tdata = merge(tdata, PARTYSEASON_WDW, by = "PARTYSEASON_WDW")

  if (length(uniqNEWNESS) > 1){
    tdata = merge(tdata, NEWNESS, by = "NEWNESS")
  }
  
  
  
  closetime = c("MKCLOSE", "UFCLOSE", "IACLOSE" , "HSCLOSE", "AKCLOSE")
  
  
  if(any(uniqSATTID %in% "MK04")){
    tdata$DMY_MKCLOSE = ifelse(((hour(tdata$MKCLOSE)-1) <= hour(tdata$TIMEPARTROUND)) & tdata$SATTID == "MK04", 1,0)
  }
  tdata$DMY_IACLOSE = ifelse(((hour(tdata$IACLOSE)-1) <= hour(tdata$TIMEPARTROUND)), 1,0)
  tdata$DMY_UFCLOSE = ifelse(((hour(tdata$UFCLOSE)-1) <= hour(tdata$TIMEPARTROUND)), 1,0)
  
  if(any(uniqSATTID %in% "HS20")){
    tdata$DMY_HSCLOSE = ifelse(((hour(tdata$HSCLOSE)-1) <= hour(tdata$TIMEPARTROUND)) & tdata$SATTID == "HS20", 1,0)
  }
  if(any(uniqSATTID %in% "AK07")){
    tdata$DMY_AKCLOSE = ifelse(((hour(tdata$AKCLOSE)-1) <= hour(tdata$TIMEPARTROUND)) & tdata$SATTID == "AK07", 1,0)
  } 
  
  ## Lag variable
  
  tdata = merge(tdata, lagtime, by = c("SATTID", "WEEKDAYS", "DAYHOUR"))
  
  # tdata = tdata[order(tdata$SATTID, tdata$WEEKDAYS, tdata$DAYHOUR, tdata$DATE),]
  # 
  # tdata$LAG_1 = lag(tdata$SPOSTMIN)
  # tdata$LAG_1[1] = tdata$SPOSTMIN[1]
  # tdata$LAG_2 =lag(tdata$SPOSTMIN, 2)
  # tdata$LAG_2[1:2] = tdata$SPOSTMIN[1:2]
  # tdata$LAG_3 =lag(tdata$SPOSTMIN, 3)
  # tdata$LAG_3[1:3] = tdata$SPOSTMIN[1:3]
  # tdata$LAG_4 =lag(tdata$SPOSTMIN, 4)
  # tdata$LAG_4[1:4] = tdata$SPOSTMIN[1:4]
  # tdata$LAG_5 =lag(tdata$SPOSTMIN, 5)
  # tdata$LAG_5[1:5] = tdata$SPOSTMIN[1:5]
  # tdata$LAG_6 =lag(tdata$SPOSTMIN, 6)
  # tdata$LAG_6[1:6] = tdata$SPOSTMIN[1:6]
  # tdata$LAG_7 =lag(tdata$SPOSTMIN, 7)
  # tdata$LAG_7[1:7] = tdata$SPOSTMIN[1:7]
  # tdata$LAG_8 =lag(tdata$SPOSTMIN, 8)
  # tdata$LAG_8[1:8] = tdata$SPOSTMIN[1:8]
  
  
  ## Create sum of School Vars
  schoolvar = c("INSESSION", "INSESSION_ENROLLMENT", "INSESSION_WDW", "INSESSION_DLR", "INSESSION_SQRT_WDW", 
                "INSESSION_SQRT_DLR", "INSESSION_REGWDW", "INSESSION_CALIFORNIA", "INSESSION_DC", 
                "INSESSION_DRIVE1_FL", "INSESSION_DRIVE2_FL", "INSESSION_DRIVE_CA", "INSESSION_FLORIDA",
                "INSESSION_MARDI_GRAS", "INSESSION_MIDWEST", "INSESSION_NY_NJ", "INSESSION_NY_NJ_PA",
                "INSESSION_NEW_ENGLAND", "INSESSION_NEW_JERSEY", "INSESSION_NOTHWEST", 
                "INSESSION_PLANES", "INSESSION_SOCAL", "INSESSION_SOUTHWEST")
  
  tdata[schoolvar][is.na(tdata[schoolvar])] = 0

  tdata[schoolvar] = scale(tdata[schoolvar],center = unlist(smeanvec), scale = unlist(ssdmat))
   
  tdata[c("SCHOOL_PCA1", "SCHOOL_PCA2", "SCHOOL_PCA3", "SCHOOL_PCA4")] = as.matrix(tdata[schoolvar]) %*% as.matrix(srotaction)

  tdata$SCHOOL = rowSums(tdata[schoolvar])
  
  # tdata = tdata[with(tdata, order(SATTID, WEEKDAYS, DAYHOUR, DATE)),]
  # tdata = cbind(tdata, ma(tdata[schoolvar], n=10, combind = FALSE))
  # 
  
  booleanvar = c("MKEMHMORN", "MKEMHMYEST", "MKEMHMTOM", "MKEMHEVE", "MKEMHEYEST",
                 "MKEMHETOM", "EPEMHMORN", "EPEMHMYEST", "EPEMHMTOM", "EPEMHEVE",
                 "EPEMHEYEST", "EPEMHETOM", "HSEMHMORN", "HSEMHMYEST", "HSEMHMTOM",
                 "HSEMHEVE", "HSEMHEYEST", "HSEMHETOM", "AKEMHMORN", "AKEMHMYEST",
                 "AKEMHMTOM", "AKEMHEVE", "AKEMHEYEST", "AKEMHETOM")

  tdata['BOOLVAR_DMY'] = rowSums(tdata[booleanvar])

  for (i in 1:length(schoolvar)){

    tdata[,booleanvar[i]] = ifelse(tdata[,booleanvar[i]] == 1, boolmeanve$mean[which(boolmeanve$varname == as.character(booleanvar[i]))], 0)
  }

  tdata['BOOLVAR'] = rowSums(tdata[booleanvar])

  

  capacityvar = c("CAPACITYLOST_MK", "CAPACITYLOST_EP", "CAPACITYLOST_HS",
                  "CAPACITYLOST_AK", "CAPACITYLOSTWGT_MK", "CAPACITYLOSTWGT_EP",
                  "CAPACITYLOSTWGT_HS", "CAPACITYLOSTWGT_AK", "EP09CAPACITY", "HS20CAPACITY")

  tdata[capacityvar] = scale(tdata[capacityvar],center = unlist(meanvec), scale = unlist(sdmat))

  tdata[c("CAPACITY_PCA1", "CAPACITY_PCA2")] = as.matrix(tdata[capacityvar]) %*% as.matrix(rotaction)

  # tdata = tdata[with(tdata, order(SATTID, WEEKDAYS, DAYHOUR, DATE)),]
  # tdata = cbind(tdata, ma(tdata[capacityvar], n=10, combind = FALSE))
  # 

  
  ### Create label for xgboost
  
  tdata = merge(tdata, labeldata, by = 'SPOSTMIN1')
  
  
  
  ## Remove Variable
  
  carvar = c("SATTID", "DLR_TICKET_SEASON", "WDW_TICKET_SEASON", "SEASON", 
             "PARTYSEASON_WDW", "WEEKDAYS", "NEWNESS")
  tdata[carvar] = NULL
  
  tdata['DATE'] = NULL
  
  remvar = c("IAUEPRICE", "UFUEPRICE", "UOR2PARKUEPRICE", "HOLIDAYN", "HOLIDAYJ", 'SUNSET', 'OVERLAY' )
  tdata[remvar] = NULL
  
  # capacityvar = c("CAPACITYLOST_MK", "CAPACITYLOST_EP", "CAPACITYLOST_HS",
  #                 "CAPACITYLOST_AK", "CAPACITYLOSTWGT_MK", "CAPACITYLOSTWGT_EP",
  #                 "CAPACITYLOSTWGT_HS", "CAPACITYLOSTWGT_AK", "EP09CAPACITY", "HS20CAPACITY")
  # tdata[capacityvar] = NULL

  
  tiemvar1 = c('TIMEPART', 'IAOPEN', 'IACLOSE', 'UFOPEN', 'UFCLOSE', 'MKOPEN',
               'MKCLOSE', 'HSOPEN', 'HSCLOSE',
               'AKOPEN', 'AKCLOSE', 'TIMEPARTROUND')
  tdata[tiemvar1] = NULL
  

  
  # 
  # booleanvar = c("MKEMHMORN", "MKEMHMYEST", "MKEMHMTOM", "MKEMHEVE", "MKEMHEYEST",
  #                "MKEMHETOM", "EPEMHMORN", "EPEMHMYEST", "EPEMHMTOM", "EPEMHEVE",
  #                "EPEMHEYEST", "EPEMHETOM", "HSEMHMORN", "HSEMHMYEST", "HSEMHMTOM",
  #                "HSEMHEVE", "HSEMHEYEST", "HSEMHETOM", "AKEMHMORN", "AKEMHMYEST",
  #                "AKEMHMTOM", "AKEMHEVE", "AKEMHEYEST", "AKEMHETOM")
  # tdata[booleanvar] = NULL
  # 
  # 
  # schoolvar = c("INSESSION", "INSESSION_ENROLLMENT", "INSESSION_WDW", "INSESSION_DLR", "INSESSION_SQRT_WDW",
  #               "INSESSION_SQRT_DLR", "INSESSION_REGWDW", "INSESSION_CALIFORNIA", "INSESSION_DC",
  #               "INSESSION_DRIVE1_FL", "INSESSION_DRIVE2_FL", "INSESSION_DRIVE_CA", "INSESSION_FLORIDA",
  #               "INSESSION_MARDI_GRAS", "INSESSION_MIDWEST", "INSESSION_NY_NJ", "INSESSION_NY_NJ_PA",
  #               "INSESSION_NEW_ENGLAND", "INSESSION_NEW_JERSEY", "INSESSION_NOTHWEST",
  #               "INSESSION_PLANES", "INSESSION_SOCAL", "INSESSION_SOUTHWEST")
  # tdata[schoolvar] = NULL
  # 
  # # delvar = c("WGT", "HOLIDAYM", "LAG_Q0", "DLR_TICKET_SEASON_PROP", "WEEKOFYEAR",
  # #            "WDW_TICKET_SEASON_PROP",  "YEAR", "PARTYSEASON_WDW_PROP", "MONTHOFYEAR")
  delvar = c("YEAR")
  tdata[delvar] = NULL
  # 
  # # hourvar = c("MKHOURS","HSHOURS","AKHOURS","IAHOURS","UFHOURS")
  # # tdata[hourvar] = NULL
  # 
  # dayevet = c("MKDAYSBEFORENONEVENT", "MKDAYSSINCENONEVENT", "MKEVENTSTREAK", "MKEVENTSTREAK_F")
  # tdata[dayevet] = NULL

  tdata = tdata[unlist(varname)]
  # tdata[ , !(names(tdata) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))] = scale(tdata[ , !(names(tdata) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))],
  #                                                                            center = unlist(allmeanvec), 
  #                                                                            scale = unlist(allsdmat))
  # 

  return(tdata)
}














# 
# 
# 
# 
# 
# 
# 
# 
# ## XGBOOST Model
# test_data   <- as.matrix(tdata[ , !(names(tdata) %in% c('LABEL', 'SPOSTMIN1', 'SPOSTMIN'))])
# test_label  <- tdata[,"LABEL"]
# test_matrix <- xgb.DMatrix(data = test_data, label = test_label)
# 
# # Predict hold-out test set
# test_pred <- predict(bst_model, newdata = test_matrix)
# test_prediction <- matrix(test_pred, nrow = bst_model$params$num_class,
#                           ncol=length(test_pred)/bst_model$params$num_class) %>%
#   t() %>%
#   data.frame() %>%
#   mutate(LABEL1 = test_label,
#          PRED_LABEL = max.col(., "last")-1)
# 
# tdata = cbind(tdata, test_prediction[, c('LABEL1', 'PRED_LABEL')])
# names(labeldata) = c('SPOSTMIN2', "LABEL")
# tdata  = merge(tdata, labeldata, by.x = 'PRED_LABEL', by.y = 'LABEL')
# 
# 
# maecom(tdata$SPOSTMIN, tdata$SPOSTMIN2)
# 
# 
# table(test_prediction$LABEL1, test_prediction$PRED_LABEL)
# # confusion matrix of test set
# confusionMatrix(factor(test_prediction$LABEL1),
#                 factor(test_prediction$PRED_LABEL),
#                 mode = "everything")
# length(unique(test_label))
# 
# 
