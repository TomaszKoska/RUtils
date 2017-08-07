traNumericAutoTransformer <- function(fo, choosenVariables=NULL, forbiddenVariables=c(), functionsToTest = c("log","exp","square"),verbose=F, cheatingMode=F, tolerance = 0.05){
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

  #tutaj przejście przez wszystkie zmienne
    #dla każdej zmiennej przejście przez wszystkie funkcje
    #sprawdzenie czy są inf, -inf lub na
      #jesli tak, odrzucamy to rozwiązanie
      #jeśli nie to mierzymy korelację
    #wybranie funkcji z najwięszką korelacją jako rozwiązania

  cheatDf <- rbind(fo$trainFull,fo$forecast)

  df <- fo$train
  if(cheatingMode){
    df <- cheatDf #podmieniam tylko jeśli cheating mode!
  }

  for(n in choosenVariables){

    baseCorrelation <- cor(df[,n],df[,fo$yName])
    bestFunctionForNow <- ""

    for(f in functionsToTest){
      fun<-eval(parse(text=f))
      value <- sapply(X=df[,n],FUN = fun)
      if(sum(is.finite(value)) >= length(value)){
        testedCorrelation <- cor(value,df[,fo$yName])
        if(abs(testedCorrelation) > (1+tolerance)*abs(baseCorrelation)){
          bestFunctionForNow <- f
        }
      }
    }
    if(bestFunctionForNow != ""){
      if(verbose){
        print(bestFunctionForNow)
        print(n)
      }
      fun<-eval(parse(text=bestFunctionForNow))
      df[,n] <- sapply(X=df[,n],FUN = fun)
      if(verbose){
        print(summary(df[,n]))
      }

      fo$train[,n] <- sapply(X=fo$train[,n],FUN = fun)
      fo$trainFull[,n] <- sapply(X=fo$trainFull[,n],FUN = fun)
      fo$forecast[,n] <- sapply(X=fo$forecast[,n],FUN = fun)
      print(paste(c(n," zmieni się na ", bestFunctionForNow),collapse = ""))
    }else{
      print(paste(c(n," bez zmian!"),collapse = ""))
    }
  }

  fo


}
