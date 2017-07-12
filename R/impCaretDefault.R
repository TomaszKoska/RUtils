impCaretDefault <- function(fo, modelName = "rf", trainControl = NULL, variableAutoselection = F, variablesToImpute = NULL,forbiddenVariables = c(),verbose=F){
  require(caret)

  if(class(fo) != "ForecastingObject"){
  warning("This function should get ForecastingObject as parameter. Please use buildForecastingObject.")
  return(NULL)
  }
  if(is.null(trainControl)){
    trainControl <- trainControl(method="repeatedcv", number=10, repeats=5,verboseIter = verbose)
  }


  x<-apply(fo$train,2,pMiss)

# print(variablesToImpute)
  if(!is.null(variablesToImpute)){
    needImputation<-variablesToImpute
  }else{
    needImputation <- names(x[x>0])
  }



  if(variableAutoselection){
    forbiddenVariables <- append(forbiddenVariables, names(x[x>0]))
  }

  forbiddenVariables <- append(forbiddenVariables, fo$yName)
  allowedVariables <- setdiff(names(fo$train),forbiddenVariables)

  xs<-fo$train[,allowedVariables]

  for(n in needImputation){
    frml <- paste(c(n," ~ ."),collapse = "",sep = "")
    frml <- as.formula(frml)
    model <- train(frml, data = xs,method = modelName,trControl=trainControl)
    fo$train[is.na(fo$train[,n]),n] <- predict(model, newdata = fo$train)[is.na(fo$train[,n])]
    fo$forecast[is.na(fo$forecast[,n]),n] <- predict(model, newdata = fo$forecast)[is.na(fo$forecast[,n])]
    fo$trainFull[is.na(fo$trainFull[,n]),n] <- predict(model, newdata = fo$trainFull)[is.na(fo$trainFull[,n])]
  }

  fo$description <- append(fo$description, "And then imputed missing data with caretDefault")
  fo
}
#
# fo$train[1:10,1:2] <- NA
# fo$forecast[1:10,1:2] <- NA
#
# fo <- impCaretDefault(fo,modelName = "ridge")
#

