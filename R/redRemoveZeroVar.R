
redRemoveZeroVar <- function(fo, nearZero = F){
  require(caret)

  if(class(fo) != "ForecastingObject"){
    warning("This function should get ForecastingObject as parameter. Please use buildForecastingObject.")
    return(NULL)
  }

  nzv <- nearZeroVar(fo$train, saveMetrics= TRUE)
  toRemove <-  row.names(nzv[nzv$zeroVar==T,])

  if(nearZero){
    toRemove <-  row.names(nzv[nzv$nzv==T,])
    fo$description <- append(fo$description,"And after that near zero variance variables were removed!")
  }else{
    fo$description <- append(fo$description,"And after that zero variance variables were removed!")
  }

  if(length(toRemove) > 0){
    keepVars <- setdiff(names(fo$train),toRemove)
    fo$train <- fo$train[,keepVars]
    fo$forecast<- fo$forecast[,keepVars]
    fo$trainFull<- fo$trainFull[,keepVars]
  }
    fo
}

# fo <- buildForecastingObject(dfTrain = "C:\\Users\\Tomek\\Desktop\\kaggle\\houses\\backup\\fullData_ai.csv", dfForecast = "C:\\Users\\Tomek\\Desktop\\kaggle\\houses\\backup\\fullData_ai.csv", yName="SalePrice")
