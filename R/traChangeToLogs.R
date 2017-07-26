traChangeToLogs <- function(fo, choosenVariables=NULL, forbiddenVariables=c()){
# choosenVariables - if null the function will transform all numeric variables

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


  #wywalenie zmiennych, które mają wartości poniżej 0 lub 0
  areThereZeros<-apply((fo$trainFull[,choosenVariables]+1),2,min,na.rm = T)
  areThereZeros<-names(areThereZeros[areThereZeros<=0])
  areThereZerosForecast<-apply((fo$forecast[,choosenVariables]+1),2,min,na.rm = T)
  areThereZerosForecast<-names(areThereZerosForecast[areThereZerosForecast<=0])
  choosenVariables<-setdiff(choosenVariables,areThereZeros)
  choosenVariables<-setdiff(choosenVariables,areThereZerosForecast)


  if(length(append(areThereZeros,areThereZerosForecast)) > 0){
    print("These variables can't be transformed to logs: ")
    print(paste(append(areThereZeros,areThereZerosForecast),collapse = ", "))
  }

  choosenVariables<-setdiff(choosenVariables,areThereZeros)




  #czy są w ogóle jakieś choosen?
  if(length(choosenVariables)>0){
    for(n in choosenVariables){
      fo$train[,n] <- log(fo$train[,n]+1)
      fo$trainFull[,n] <- log(fo$trainFull[,n]+1)
      fo$forecast[,n] <- log(fo$forecast[,n]+1)
    }
  }

  fo$description <- append(fo$description,"And after that some variables changed to logs!")


  fo
}
# class(fo$trainFull$LotFrontage)
# class(fo$trainFull$LotArea)
#
# fo3 <- buildForecastingObject(iris,iris,"Species")
#
# str(fo3$train)
# summary(fo3$train)
# fo3 <- traChangeToLogs(fo3)
# str(fo3$train)
# summary(fo3$train)
# fo2 <- traChangeToLogs(fo,forbiddenVariables = c("Id"))
# str(fo$train)
# str(fo2$train)
