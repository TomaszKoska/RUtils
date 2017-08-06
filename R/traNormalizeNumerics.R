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
    for(n in variablesToCheck){
      if(class(fo$trainFull[,n])=="integer" || class(fo$trainFull[,n])=="numeric" ){

        if(verbose){
          print(n)
          print("cheat")
          print(summary(cheatDf))
          print("trainFull")
          print(summary(fo$trainFull))

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

        if(verbose){
          print(paste(c(n,lower,upper,avg,stddev),collapse = "|"))
          print(lower)
          print(upper)
          # print(head(toCalculation[,n],200))
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

#
# fo3 <- buildForecastingObject(iris,iris,"Species")
# str(fo3$train)
# summary(fo3$train)
# fo3 <- traNormalizeNumerics(fo3)
# str(fo3$train)
# summary(fo3$train)
#
#
#
# fo3 <- buildForecastingObject(iris,iris,"Species")
# fo3$train[1:10,1:2] <- NA
# fo3$forecast[1:10,1:2] <- NA
# str(fo3$train)
# summary(fo3$train)
# fo3 <- impCaretDefault(fo3)
# fo3 <- outNumericStdDev(fo3,1.5)
# fo3 <- traNormalizeNumerics(fo3)
# str(fo3$train)
# summary(fo3$train)
#
#
#
#
#
# str(fo$train)
# fo2 <- fo
# fo2$train[1:10,1:5] <- NA
# fo2 <- impCaretDefault(fo2)
# fo2 <- outNumericStdDev(fo2,2)
# fo2 <- traNormalizeNumerics(fo2)
# str(fo2$train)

