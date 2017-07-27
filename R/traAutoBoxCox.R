traAutoBoxCox <- function(fo, choosenVariables=NULL, forbiddenVariables=c(), lambdasToCheck = seq(-20,20,0.01), tolerance = 0.05){
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


  for(n in choosenVariables){

    baseCorrelation <- cor(fo$train[,n],fo$train[,fo$yName])
    bestLambda <- NULL

    for(lambda in lambdasToCheck){
      value <- ((fo$train[,n])^lambda -1)/lambda

      if(sum(is.finite(value)) >= length(value)){
        testedCorrelation <- 0
        if(sd(value) > 0){
          testedCorrelation <- cor(value,fo$train[,fo$yName])
        }

         if(!is.nan(testedCorrelation)){
            if(abs(testedCorrelation) > (1+tolerance) * abs(baseCorrelation)){
              bestLambda <- lambda
            }
         }

      }
    }
    if(!is.null(bestLambda)){
      if(bestLambda != 0){
        fo$train[,n] <- ((fo$train[,n])^bestLambda -1)/bestLambda
        fo$trainFull[,n] <- ((fo$trainFull[,n])^bestLambda -1)/bestLambda
        fo$forecast[,n] <- ((fo$forecast[,n])^bestLambda -1)/bestLambda
      }
      else{
        fo$train[,n] <- log(fo$train[,n])
        fo$trainFull[,n] <- log(fo$trainFull[,n])
        fo$forecast[,n] <- log(fo$forecast[,n])
      }

      print(paste(c(n," transformujemy z lambda ", bestLambda),collapse = ""))
    }else{
      print(paste(c(n," bez zmian!"),collapse = ""))
    }
  }

  fo


}
