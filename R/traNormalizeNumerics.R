traNormalizeNumerics <- function(fo, forbiddenVariables=c(),lowerP = 0.25, upperP = 0.75, verbose = F, cheatingMode=F){

  if(class(fo) != "ForecastingObject"){
    warning("This function should get ForecastingObject as parameter. Please use buildForecastingObject.")
    return(NULL)
  }

  #jedź przez wszystkie zmienne
    #jeśli nie numeric i nie forbidden - normalizuj
  cheatDf <-rbind(fo$train,fo$forecast)

  variablesToCheck <- setdiff(names(fo$trainFull),forbiddenVariables)
  variablesToCheck <- setdiff(variablesToCheck,c(fo$yName))
  if(length(variablesToCheck) > 0){

    if(verbose){
      # print("cheat")
      # print(summary(cheatDf[,variablesToCheck]))
      # print("trainFull")
      # print(summary(fo$trainFull[,variablesToCheck]))
    }

    for(n in variablesToCheck){
      if(class(fo$trainFull[,n])=="integer" || class(fo$trainFull[,n])=="numeric" ){

        if(verbose){
          print(n)
        }


        if(cheatingMode){
          lower <- quantile(cheatDf,lowerP)
          upper <- quantile(cheatDf,upperP)
          toCalculation <- cheatDf[cheatDf[,n] >= lower,]
          toCalculation <- cheatDf[toCalculation[,n] <= upper,]
        }else{
          lower <- quantile(fo$train[,n],lowerP)
          upper <- quantile(fo$train[,n],upperP)
          toCalculation <- fo$trainFull[fo$trainFull[,n] >= lower,]
          toCalculation <- fo$trainFull[toCalculation[,n] <= upper,]
        }




        avg <- mean(toCalculation[,n],na.rm = T)
        stddev <- sd(toCalculation[,n],na.rm = T)
        if(stddev==0){
          stddev <- 1 #if stddev is 0, then we will just center the var
        }

        if(verbose){
          print("liczymy z tego:")
          print(head(toCalculation[,n],100))
          print("policzyliśmy to:")
          print(paste(c(n,lower,upper,avg,stddev),collapse = "|"))
        }



        fo$trainFull[,n] <- (fo$trainFull[,n]-avg)/stddev
        fo$train[,n] <- (fo$train[,n]-avg)/stddev
        fo$forecast[,n] <- (fo$forecast[,n]-avg)/stddev

        if(verbose){
          print(n)
          print(summary(fo$trainFull[,n]))
        }
      }
    }
    fo$description <- append(fo$description, "And then we standardized some variables!")
  }
  fo
}
