test_data_prep <- function(data = NULL, dataname = NULL, datapath, codepath){
  datapath = datapath
  codepath = codepath
  
  function_file_name =  "Disney Competition R Function.R"
  
  source(paste0(codepath, function_file_name, collapse = ''))
  lagtime = read.csv(paste0(datapath, 'lagtime.csv', collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
  smeanvec = read.csv(paste0(datapath, "smeanvec.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
  ssdmat = read.csv(paste0(datapath, "ssdmat.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
  srotaction = read.csv(paste0(datapath, "srotaction.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
  meanvec = read.csv(paste0(datapath, "meanvec.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
  sdmat = read.csv(paste0(datapath, "sdmat.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
  rotaction = read.csv(paste0(datapath, "rotaction.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
  varname = read.csv(paste0(datapath, "varname.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
  
  
  if(is.null(data) & !is.null(dataname)){
    for (i in 1:length(dataname)){
      tdata1 =  read.csv(paste0(datapath, dataname[i],'.csv', collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
      if (i==1) tdata = tdata1
      else tdata = rbind(tdata, tdata1)
    }
    rm(tdata1, i)
    tdata = as.data.frame(tdata)
    
  }else if(!is.null(data) & is.null(dataname)){
    tdata = data
    tdata = as.data.frame(tdata)
    }else print("You must input either Data File or list of File Name but not both")
  #######################
  
  tdata["INDEX"] = c(1:nrow(tdata))
  
  #################
  
  
  ### change Time variable
  uniqSATTID = unique(tdata$SATTID)
  uniqNEWNESS = unique(tdata$NEWNESS)
  if(uniqSATTID == 'AK07'){
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
  }else if(uniqSATTID == 'EP09'){
    tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN == 0, 5, tdata$SPOSTMIN)
    tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 >= 115 & tdata$SPOSTMIN1 <=130, 120, tdata$SPOSTMIN1)
    tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 > 130, 150, tdata$SPOSTMIN1)
  }else if(uniqSATTID == 'HS20'){
    tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN == 0, 5, tdata$SPOSTMIN)
    tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 == 125, 130, tdata$SPOSTMIN1)
    tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 == 135, 130, tdata$SPOSTMIN1)
    tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 >= 140, 150, tdata$SPOSTMIN1)
  }else if(uniqSATTID == 'MK04'){
    tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN == 0, 5, tdata$SPOSTMIN)
    tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 == 85, 90, tdata$SPOSTMIN1)
    tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 == 95, 100, tdata$SPOSTMIN1)
    tdata$SPOSTMIN1 = ifelse(tdata$SPOSTMIN1 >= 105, 110, tdata$SPOSTMIN1)
  }
  
  
  ## Create New variables Hour of Day and Weekdays
  ### change Time variable
  
  tdata$DATE <- as.Date(tdata$DATE, origin = "1899-12-30")
  tdata$TIMEPART = as.POSIXct(tdata$TIMEPART * 86400, origin = "1970-01-01", tz = "UTC")
  tdata$MKCLOSE = round(strptime(as.POSIXct(tdata$MKCLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
  tdata$IACLOSE = round(strptime(as.POSIXct(tdata$IACLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
  tdata$UFCLOSE = round(strptime(as.POSIXct(tdata$UFCLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
  tdata$HSCLOSE = round(strptime(as.POSIXct(tdata$HSCLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
  tdata$AKCLOSE = round(strptime(as.POSIXct(tdata$AKCLOSE * 86400, origin = "1970-01-01", tz = "UTC"), format="%Y-%m-%d %H:%M:%S"), units="hours")
  tdata$TIMEPARTROUND = round(strptime(tdata$TIMEPART, format="%Y-%m-%d %H:%M:%S"), units="hours")
  
  
  
  tdata['DAYHOUR'] = as.numeric(format(as.POSIXct(tdata$TIMEPARTROUND,format="%H:%M"),"%H"))
  tdata["DAYMINUTE"] = round_any(as.numeric(format(as.POSIXct(tdata$TIMEPART,format="%H:%M"),"%M")), 5, f = round)
  tdata['WEEKDAYS'] = weekdays(tdata$DATE)
  tdata["SUMHOUR"] = rowSums(tdata[,c("MKHOURS","HSHOURS","AKHOURS","IAHOURS","UFHOURS")], na.rm = TRUE)
  tdata["DAYEVENTBA"] = rowSums(tdata[,c("MKDAYSBEFORENONEVENT", "MKDAYSSINCENONEVENT", "MKEVENTSTREAK", "MKEVENTSTREAK_F")], na.rm = TRUE)

  tdata["DAYHOURGROUP"] = ifelse(tdata$DAYHOUR <= 10 | tdata$DAYHOUR == 23, 9, tdata$DAYHOUR)
  
  tdata$DAYHOURGROUP= ifelse(tdata$DAYHOURGROUP == 11 |
                              tdata$DAYHOURGROUP == 12, 10, tdata$DAYHOURGROUP)
  tdata$DAYHOURGROUP= ifelse(tdata$DAYHOURGROUP == 13 |
                              tdata$DAYHOURGROUP == 14, 11, tdata$DAYHOURGROUP)
  
  tdata$DAYHOURGROUP= ifelse(tdata$DAYHOURGROUP == 15 |
                              tdata$DAYHOURGROUP == 16, 12, tdata$DAYHOURGROUP)
  tdata$DAYHOURGROUP= ifelse(tdata$DAYHOURGROUP == 17 |
                              tdata$DAYHOURGROUP == 18, 13, tdata$DAYHOURGROUP)
  tdata$DAYHOURGROUP= ifelse(tdata$DAYHOURGROUP == 19 |
                              tdata$DAYHOURGROUP == 20, 14, tdata$DAYHOURGROUP)
  tdata$DAYHOURGROUP= ifelse(tdata$DAYHOURGROUP == 21 |
                              tdata$DAYHOURGROUP == 22 , 15, tdata$DAYHOURGROUP)
  
  
  ## Dummy variable for Alphanumeric
  uniqSATTID = unique(tdata$SATTID)
  uniqNEWNESS = unique(tdata$NEWNESS)
  carvar = c("HOLIDAYN", "HOLIDAYJ", "PARTYSEASON_WDW", "SEASON", "NEWNESS")
  tdata[carvar][tdata[,carvar]==""] = as.character("NOT KNOWN")
  
  
  tdata = create_dummy(data = tdata, carvar = 'DAYOFWEEK', cn_1 = 0)
  carvar = as.character(unlist(read.csv(paste0(datapath, "carvarlist.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")))
  
  filelist =  unlist(read.csv(paste0(datapath, "savefilename.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA"))
  for(i in 1:length(carvar)){
    filename1 = paste0(carvar[i], ".csv", collapse = '')
    if(filename1 %in% filelist){
      readmeantable = read.csv(paste0(datapath, carvar[i], ".csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
      tdata = merge(tdata, readmeantable, by = c("DAYHOURGROUP", carvar[i]), all.x = TRUE)
    }
  }
  
  #tdata[is.na(tdata)] = 0
  
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
  
  tdata = merge(tdata, lagtime, by = c("WEEKDAYS","DAYHOURGROUP"), all.x = TRUE)

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

  tdata$SCHOOL = rowSums(tdata[schoolvar], na.rm = TRUE)

  booleanvar = c("MKEMHMORN", "MKEMHMYEST", "MKEMHMTOM", "MKEMHEVE", "MKEMHEYEST",
                 "MKEMHETOM", "EPEMHMORN", "EPEMHMYEST", "EPEMHMTOM", "EPEMHEVE",
                 "EPEMHEYEST", "EPEMHETOM", "HSEMHMORN", "HSEMHMYEST", "HSEMHMTOM",
                 "HSEMHEVE", "HSEMHEYEST", "HSEMHETOM", "AKEMHMORN", "AKEMHMYEST",
                 "AKEMHMTOM", "AKEMHEVE", "AKEMHEYEST", "AKEMHETOM")
  tdata[booleanvar][is.na(tdata[booleanvar])] = 0

  tdata['BOOLVAR_DMY'] = rowSums(tdata[booleanvar], na.rm = TRUE)
  
  booleanvar = c("MKEMHMORN", "MKEMHMYEST", "MKEMHMTOM", "MKEMHEVE", "MKEMHEYEST",
                 "MKEMHETOM", "EPEMHMORN", "EPEMHMYEST", "EPEMHMTOM", "EPEMHEVE",
                 "EPEMHEYEST", "EPEMHETOM", "HSEMHMORN", "HSEMHMYEST", "HSEMHMTOM",
                 "HSEMHEVE", "HSEMHEYEST", "HSEMHETOM", "AKEMHMORN", "AKEMHMYEST",
                 "AKEMHMTOM", "AKEMHEVE", "AKEMHEYEST", "AKEMHETOM", 'BOOLVAR_DMY')

  booleanvar1 =as.character(unlist(read.csv(paste0(datapath, "boolvar1.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")))
  for(i in 1:length(booleanvar1)){
      readmeantable =read.csv(paste0(datapath, booleanvar1[i], ".csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
      tdata = merge(tdata, readmeantable, by = c("WEEKDAYS", "DAYHOURGROUP", booleanvar1[i]), all.x = TRUE)
  }
  
  booleanvar1 = booleanvar1[!(booleanvar1 %in% c('BOOLVAR_DMY', "HOLIDAYM", "HOLIDAYPX"))]
  booleanvar1 = paste(booleanvar1, "_mean",sep = "")
  tdata['BOOLVAR'] = rowSums(tdata[booleanvar1], na.rm = TRUE)

  meanvecbb = read.csv(paste0(datapath, 'meanvecbb.csv', collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
  sdmatbb = read.csv(paste0(datapath, "sdmatbb.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
  rotactionbb = read.csv(paste0(datapath, "rotactionbb.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")

  tdata[booleanvar1] = scale(tdata[booleanvar1],center = unlist(meanvecbb), scale = unlist(sdmatbb))

  tdata[c("BOOLVAR_PCA1", "BOOLVAR_PCA2")] = as.matrix(tdata[booleanvar1]) %*% as.matrix(rotactionbb)

  
  
  capacityvar = c("CAPACITYLOST_MK", "CAPACITYLOST_EP", "CAPACITYLOST_HS",
                  "CAPACITYLOST_AK", "CAPACITYLOSTWGT_MK", "CAPACITYLOSTWGT_EP",
                  "CAPACITYLOSTWGT_HS", "CAPACITYLOSTWGT_AK", "EP09CAPACITY", "HS20CAPACITY")

  tdata[capacityvar] = scale(tdata[capacityvar],center = unlist(meanvec), scale = unlist(sdmat))

  tdata[c("CAPACITY_PCA1", "CAPACITY_PCA2")] = as.matrix(tdata[capacityvar]) %*% as.matrix(rotaction)

  timevar11 = c("WEEKOFYEAR", "MONTHOFYEAR", "DAYMINUTE", 
                "DAYEVENTBA")
  
  for(i in 1:length(timevar11)){
    filename1 = paste0(timevar11[i], ".csv", collapse = '')
    if(filename1 %in% filelist){
      readmeantable =read.csv(paste0(datapath, timevar11[i], ".csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
      tdata = merge(tdata, readmeantable, by = c("WEEKDAYS", "DAYHOURGROUP", timevar11[i]), all.x = TRUE)
    }
  }
  
  ## Remove Variable
  
  carvar = c("SATTID", "DLR_TICKET_SEASON", "WDW_TICKET_SEASON", "SEASON", 
             "PARTYSEASON_WDW", "WEEKDAYS", "NEWNESS", "SUNSET_WDW", "MINSSUNSETWDW", "WEATHER_WDWHIGH",
             "WEATHER_WDWLOW", "WEATHER_WDWPRECIP", "WGT")
  tdata[carvar] = NULL
  
  tdata['DATE'] = NULL
  
  remvar = c("IAUEPRICE", "UFUEPRICE", "UOR2PARKUEPRICE", "HOLIDAYN", "HOLIDAYJ", 'SUNSET', 'OVERLAY' )
  tdata[remvar] = NULL
  
  capacityvar = c("CAPACITYLOST_MK", "CAPACITYLOST_EP", "CAPACITYLOST_HS",
                  "CAPACITYLOST_AK", "CAPACITYLOSTWGT_MK", "CAPACITYLOSTWGT_EP",
                  "CAPACITYLOSTWGT_HS", "CAPACITYLOSTWGT_AK", "EP09CAPACITY", "HS20CAPACITY")
  tdata[capacityvar] = NULL

  
  tiemvar1 = c('TIMEPART', 'IAOPEN', 'IACLOSE', 'UFOPEN', 'UFCLOSE', 'MKOPEN',
               'MKCLOSE', 'HSOPEN', 'HSCLOSE',
               'AKOPEN', 'AKCLOSE', 'TIMEPARTROUND', 'DAYHOUR')
  tdata[tiemvar1] = NULL
  
  booleanvar = c("MKEMHMORN", "MKEMHMYEST", "MKEMHMTOM", "MKEMHEVE", "MKEMHEYEST",
                 "MKEMHETOM", "EPEMHMORN", "EPEMHMYEST", "EPEMHMTOM", "EPEMHEVE",
                 "EPEMHEYEST", "EPEMHETOM", "HSEMHMORN", "HSEMHMYEST", "HSEMHMTOM",
                 "HSEMHEVE", "HSEMHEYEST", "HSEMHETOM", "AKEMHMORN", "AKEMHMYEST",
                 "AKEMHMTOM", "AKEMHEVE", "AKEMHEYEST", "AKEMHETOM")
  tdata[booleanvar] = NULL
  tdata[booleanvar1] = NULL

  schoolvar = c("INSESSION", "INSESSION_ENROLLMENT", "INSESSION_WDW", "INSESSION_DLR", "INSESSION_SQRT_WDW",
                "INSESSION_SQRT_DLR", "INSESSION_REGWDW", "INSESSION_CALIFORNIA", "INSESSION_DC",
                "INSESSION_DRIVE1_FL", "INSESSION_DRIVE2_FL", "INSESSION_DRIVE_CA", "INSESSION_FLORIDA",
                "INSESSION_MARDI_GRAS", "INSESSION_MIDWEST", "INSESSION_NY_NJ", "INSESSION_NY_NJ_PA",
                "INSESSION_NEW_ENGLAND", "INSESSION_NEW_JERSEY", "INSESSION_NOTHWEST",
                "INSESSION_PLANES", "INSESSION_SOCAL", "INSESSION_SOUTHWEST")
  tdata[schoolvar] = NULL

  delvar = c("YEAR")
  tdata[delvar] = NULL

  hourvar = c("MKHOURS","HSHOURS","AKHOURS","IAHOURS","UFHOURS")
  tdata[hourvar] = NULL

  dayevet = c("MKDAYSBEFORENONEVENT", "MKDAYSSINCENONEVENT", "MKEVENTSTREAK", "MKEVENTSTREAK_F")
  tdata[dayevet] = NULL
  
  timevar11 = c("DAYOFWEEK", "DAYOFYEAR", "WEEKOFYEAR")
  
  tdata[timevar11] = NULL
  
  
  allmeanvecmiss = read.csv(paste0(datapath, "allmeanvecmiss.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA")
  names(allmeanvecmiss) = c("Var", "Mean")
  tdata11 <- tdata[ , !(names(tdata) %in% c('SPOSTMIN1', 'SPOSTMIN', "INDEX"))]
  vn = names(tdata11)
  for (i in 1:length(vn)){
    menaa =allmeanvecmiss[which(allmeanvecmiss$Var == vn[i]),2]
    tdata11[vn[i]] = replace(tdata11[vn[i]], is.na(tdata11[vn[i]]), menaa)
  }

  tdata = cbind(tdata[c("INDEX", 'SPOSTMIN1', 'SPOSTMIN')], tdata11)
  
  
  # aa = tdata[ , !(names(tdata) %in% c('SPOSTMIN1', 'SPOSTMIN', "DAYHOURGROUP"))]
  # nn = names(aa)
  # for(i in 1:length(nn)){
  #   a = unlist(read.csv(paste0(datapath, nn[i], "11.csv", collapse = ''), stringsAsFactors = FALSE, na.strings = "NA"))
  #   bb = cut(unlist(aa[nn[i]]), a, labels = FALSE)
  #   aa[nn[i]] = bb
  # }
  # 
  # tdata = as.data.frame(cbind(tdata[c('SPOSTMIN1', 'SPOSTMIN', "DAYHOURGROUP")], aa))
  
  aa= names(tdata[ , !(names(tdata) %in% c('INDEX'))])
  
  if(any(!(unlist(varname) %in% aa))){
    print(unlist(varname)[!(unlist(varname) %in% aa)])
    print("These variables not in test data")
  }
  tdata = tdata[c("INDEX", unlist(varname))]

  return(tdata)
}

