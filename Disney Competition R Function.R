
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
  for (i in 1: length(carvar)){
    df1 = as.data.frame(dummy(unlist(data[carvar[i]]), sep = "_"))
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

maeoucom <- function(y, pred){
  dif = y-pred
  oem = mean(dif[which(dif < 0)])
  uem = mean(dif[which(dif > 0)])
  MAE = mean(abs(dif))
  MSE = mean(dif**2)
  return(list(MAE = MAE, MSE = MSE, AvgOverEstimate = oem, AvgUnderEstimate = uem))
}