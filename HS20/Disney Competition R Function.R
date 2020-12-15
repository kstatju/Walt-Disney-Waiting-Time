
## Random Forest training fucntion

randomforeset_func <- function(data, prem, target = "SPOSTMIN1", seed = 12354){
  data1 = data
  model <- vector("list",14)
  trin = list()
  label = list()
  for(i in 1:14){
    # if(i != 14){
    data = data1[data1$DAYHOURGROUP == (i+8),]
    # } else {
    #   data = data1[(data1$DAYHOUR >= 22 & data1$DAYHOUR <=23) | (data1$DAYHOUR >= 0 & data1$DAYHOUR <=8),]
    # }
    if (nrow(data) > 0){
      set.seed(seed)
      predvar = target
      train_data   <- as.matrix(data[ , !(names(data) %in% c('SPOSTMIN1', 'SPOSTMIN'))])
      train_label  <- as.matrix(data[,predvar])
      
      prem = prem
      classwt = table(data[predvar])/nrow(data)
      ptm = proc.time()
      result = list()
      model[[i]] = randomForest(train_data,
                                as.factor(train_label),
                                ntree=prem[1], mtry=prem[2], importance=TRUE,
                                classwt = classwt, keep.forest=TRUE, nodesize = prem[3], replace = FALSE)
      trin[[i]] = train_data
      label[[i]] = train_label
    }else{
      trin[[i]] = NULL
      label[[i]] = NULL
    }
  }
  return(list(model = model, prem = prem, train_data = train_data, train_label = train_label))
}


### Predtion function

pred_func_rf <- function(test_data, model){
  if(!require(randomForest, quietly =T))  {  install.packages("randomForest");  require(randomForest, quietly =TRUE)}
  test_data["INDEX"] = c(1:nrow(test_data))
  test_data1 = test_data
  
  for (i in 1:14){
    # if(i != 14){
    test_data = test_data1[test_data1$DAYHOURGROUP == (i+8),]
    # print(nrow(test_data))
    # } else {
    #   test_data = test_data1[(test_data1$DAYHOUR >= 22 & test_data1$DAYHOUR <=23) | 
    #                       (test_data1$DAYHOUR >= 0 & test_data1$DAYHOUR <=8),]
    #   print(nrow(test_data))
    # }
    
    if(nrow(test_data) > 0){
      
      pred = predict(model[[i]], test_data, type="response")
      test_data["PRED_LABEL"] = as.numeric(as.character(pred))
      if(i == 1){
        pred_data = test_data
      }else{
        pred_data = rbind(pred_data, test_data)
      }
    }
  }
  
  pred_data = pred_data[order(pred_data$INDEX),]
  return(list(pred_data = pred_data, test_data = test_data))
}


## Moving Agerage

ma <- function(arr, n=15, combind = FALSE){
  ncl = ncol(arr)
  name = names(arr)
  name1 = paste("MA_", name, sep = "")

  for (k in 1:ncl){
    res2 = unlist(arr[name[k]])
    res = NULL
    for (j in 1:n-1){
      res[j] = mean(res2[1:j])
    }
    for(i in n:nrow(arr)){
      res[i] = mean(res2[(i-n):i])
    }
    res = as.data.frame(res)
    names(res) = name1[k]
    if (k == 1){
      res1 = res
    }else {res1 = cbind(res1, res) }
  }
  if (combind){
    return(cbind(arr, res1))
  }else {
    return(res1)
    }
}

## Accuracy and Error of Classification
accuracy_error <- function(data, confivar = c("SPOSTMIN1", "PRED_LABEL"),accuracyvar = c("PRED_LABEL")){
  tab = as.data.frame(table(unlist(data[confivar[1]]),unlist(data[confivar[2]])))
  v1 = unique(tab$Var1)
  v2 = unique(tab$Var2)
  
  lv1 = length(v1)
  lv2 = length(v2)
  if(lv1 != lv2){
    notin = v1[which(!(v1 %in% v2))]
    for (i in 1:length(notin)){
      if(i == 1){
        newtab = data.frame(Var1 = as.factor(c(0:(lv1-1))),Var2 = as.factor(rep(as.numeric(as.character(notin[i])), lv1)), Freq = rep(0, lv1))
      }else {
        newtab = rbind(newtab, data.frame(Var1 = as.factor(c(0:(lv1-1))),Var2 = as.factor(rep(as.numeric(as.character(notin[i])), lv1)), Freq = rep(0, lv1)))
      }
    }
    tab = rbind(tab, newtab)
  }
  
  tab = as.matrix(acast(tab, Var1 ~ Var2, value.var="Freq"))
  
  accuracy = sum(diag(tab))/sum(tab)
  error = 1-accuracy
  mse = maeoucom(as.numeric(as.character(unlist(data["SPOSTMIN"]))),as.numeric(as.character(unlist(data[accuracyvar[1]]))))
  
  return(list(accuracy = accuracy, error = error, MAE = mse$MAE, 
              MSE = mse$MSE, AvgOverEstimate = mse$AvgOverEstimate, 
              AvgUnderEstimate = mse$AvgUnderEstimate))
}



# missing percent

misper <- function(data, var = NULL){
  if (is.null(var)){
    var = names(data)
  }
  return(colSums(is.na(data[var]))*100/nrow(data))
}


## Create dummy variable for alphanumeric

create_dummy <- function(data, carvar, cn_1 = 1){
  if(!require(dummies, quietly =T))  {  install.packages("dummies");  require(dummies, quietly =TRUE)}
  for (i in 1:length(carvar)){
    df1 = as.data.frame(dummies::dummy(unlist(data[carvar[i]]), sep = "_"))
    a = sub(".*_", "DUMMYV_", names(df1))
    names(df1) = paste(a,"_", carvar[i], sep = "")
    data <- cbind(data, df1)
  }
  rm(df1)
  return(data)
}


## create lag variable

agfunc <- function(x){
  a = c(mean(x), quantile(unlist(x), probs = seq(0,1,.25)) )
  names(a) = c('mean', 'min', 'q1', 'q2', 'q3', 'q4')
  return(a)
  
} 


## average Over and Under Estimate, MAE, MSE

maeoucom <- function(y, pred = NULL){
  if (dim(y)[2] == 2 & is.null(pred)){
    dif = y[,1] - y[,2]
  } else dif = y-pred
  oem = mean(dif[which(dif < 0)])
  uem = mean(dif[which(dif > 0)])
  ncorrect = sum(dif == 0)
  MAE = mean(abs(dif))
  MSE = mean(dif**2)
  return(list(MAE = MAE, MSE = MSE, AvgOverEstimate = oem, AvgUnderEstimate = uem, NCorrectEstimate = ncorrect))
}