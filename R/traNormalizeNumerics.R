traNormalizeNumerics <- function(fo, forbiddenVariables=c(),lowerP = 0.25, upperP = 0.75, verbose = F){

  if(class(fo) != "ForecastingObject"){
    warning("This function should get ForecastingObject as parameter. Please use buildForecastingObject.")
    return(NULL)
  }

  #jedź przez wszystkie zmienne
    #jeśli nie numeric i nie forbidden - normalizuj
  variablesToCheck <- setdiff(names(fo$trainFull),forbiddenVariables)
  variablesToCheck <- setdiff(names(fo$trainFull),c(fo$yName))
  if(length(variablesToCheck) > 0){
    for(n in variablesToCheck){
      if(class(fo$trainFull[,n])=="integer" || class(fo$trainFull[,n])=="numeric" ){



        lower <- quantile(fo$train[,n],lowerP)
        upper <-quantile(fo$train[,n],upperP)

        if(verbose){
          print(paste(c(n,lower,upper),collapse = "|"))
        }

        toCalculation <- fo$trainFull[fo$trainFull[,n] >= lower,]
        toCalculation <- fo$trainFull[toCalculation <= upper,]

        avg <- mean(toCalculation[,n],na.rm = T)
        stddev <- sd(toCalculation[,n],na.rm = T)

        fo$trainFull[,n] <- (fo$trainFull[,n]-avg)/stddev
        fo$train[,n] <- (fo$train[,n]-avg)/stddev
        fo$forecast[,n] <- (fo$forecast[,n]-avg)/stddev
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

