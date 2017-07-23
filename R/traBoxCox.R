traBoxCox <- function(fo, lambda = 0.1, choosenVariables=NULL, forbiddenVariables=c()){
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


  #czy są w ogóle jakieś choosen?
  if(length(choosenVariables)>0){
    for(n in choosenVariables){
      if(lambda != 0){
        fo$train[,n] <- ((fo$train[,n])^lambda -1)/lambda
        fo$trainFull[,n] <- ((fo$trainFull[,n])^lambda -1)/lambda
        fo$forecast[,n] <- ((fo$forecast[,n])^lambda -1)/lambda
      }
      else{
        fo$train[,n] <- log(fo$train[,n])
        fo$trainFull[,n] <- log(fo$trainFull[,n])
        fo$forecast[,n] <- log(fo$forecast[,n])
      }

    }
  }

  fo$description <- append(fo$description,"And after that some variables were transformed using box cox!")


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
