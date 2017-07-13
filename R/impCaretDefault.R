impCaretDefault <- function(fo, modelName = "rf", trainControl = NULL, variableAutoselection = T, variablesToImpute = NULL,forbiddenVariables = c(),verbose=F){
  # fo is fo
  # modelName  = same as in caret
  # trainControl = can be changed, but if null we will set default cv 3x3
  # autoselection = removes variables with na from variables
  # variablesToImpute = we can select one, if left NULL we will impute all with missing observations
  # forbiddenVariables = vector of names of variables which shoud not be used (e.g. Id)
  # verbose = should we print?



  require(caret)

  if(class(fo) != "ForecastingObject"){
  warning("This function should get ForecastingObject as parameter. Please use buildForecastingObject.")
  return(NULL)
  }
  if(is.null(trainControl)){
    trainControl <- trainControl(method="repeatedcv", number=3, repeats=3,verboseIter = verbose)
  }


  x<-apply(fo$train,2,pMiss)

# print(variablesToImpute)
  if(!is.null(variablesToImpute)){
    needImputation<-variablesToImpute
  }else{
    needImputation <- names(x[x>0])
  }
  needImputation <- setdiff(needImputation,fo$yName)

  if(length(needImputation)<=0){
    print("No variables to impute!")
    return(fo)
  }


  if(variableAutoselection){
    forbiddenVariables <- append(forbiddenVariables, names(x[x>0]))
  }

  forbiddenVariables <- append(forbiddenVariables, fo$yName)
  allowedVariables <- setdiff(names(fo$train),forbiddenVariables)

  xs<-fo$train[,allowedVariables]

  for(n in needImputation){
    print(paste(c("Let's start with ", n),collapse = ""))
    frml <- paste(c(n," ~ ."),collapse = "",sep = "")
    frml <- as.formula(frml)
    print(frml)
    g<-cbind(fo$train[,n],xs)
    # head(g)
    # str(g)
    names(g)[1]<-n
    model <- train(frml, data = g,method = modelName,trControl=trainControl)
    fo$train[is.na(fo$train[,n]),n] <- predict(model, newdata = fo$train)[is.na(fo$train[,n])]
    fo$forecast[is.na(fo$forecast[,n]),n] <- predict(model, newdata = fo$forecast)[is.na(fo$forecast[,n])]
    fo$trainFull[is.na(fo$trainFull[,n]),n] <- predict(model, newdata = fo$trainFull)[is.na(fo$trainFull[,n])]
  }

  fo$description <- append(fo$description, "And then imputed missing data with caretDefault")
  fo
}


# fo$train[1:10,1:2] <- NA
# fo$forecast[1:10,1:2] <- NA
#
# fo <- impCaretDefault(fo,modelName = "ridge")
#



