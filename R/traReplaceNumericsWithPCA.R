traReplaceNumericsWithPCA <- function(fo, choosenVariables=NULL, forbiddenVariables=c(), keepPC=0, cheatingMode=F){

  # choosenVariables - if null the function will transform all numeric variables
  # keepPC will be implemented some time in the future

  if(class(fo) != "ForecastingObject"){
    warning("This function should get ForecastingObject as parameter. Please use buildForecastingObject.")
    return(NULL)
  }

  x<-sapply(fo$train, class)

  if(is.null(choosenVariables)){
    choosenVariables<-names(x[x=="numeric"|x=="integer"])
  }

  #jeszcze sprawdzenie czy choosen są numericami
  notNumerics <- x[x!="numeric" & x !="integer"]
  choosenVariables<-setdiff(choosenVariables,notNumerics)
  #wywalenie zabronionych
  choosenVariables<-setdiff(choosenVariables,forbiddenVariables)
  #wywalenie zmiennej celu
  choosenVariables<-setdiff(choosenVariables,fo$yName)

  print(paste(choosenVariables,collapse = ","))

  cheatDf <-rbind(fo$train,fo$forecast)
  xs <- fo$train[,choosenVariables]

  if(cheatingMode){
    xs <-  cheatDf[,choosenVariables]
  }


  xTrain <- fo$train[,choosenVariables]
  xForecast <- fo$forecast[,choosenVariables]
  xTrainFull <- fo$trainFull[,choosenVariables]

  myPCA <- prcomp(xs,
                  center = TRUE,
                  scale = TRUE)

  a<- predict(myPCA, newdata=xTrain)
  b<- predict(myPCA, newdata=xForecast)
  c<- predict(myPCA, newdata=xTrainFull)

  fo$train[,choosenVariables] <-NULL
  fo$forecast[,choosenVariables]<-NULL
  fo$trainFull[,choosenVariables]<-NULL

  fo$train <- cbind(fo$train,a)
  fo$forecast<- cbind(fo$forecast,b)
  fo$trainFull<- cbind(fo$trainFull,c)

  fo$description <- append(fo$description,"And after that we did PCA!")


  fo


}
