outNumericStdDev <- function(fo, howManyDevsAway = 2, ignoredVariables = c(), maxRemovedPercentage= 0.01, cheatingMode=F,verbose=F,ignoreZeros=F){
#usuwa wszystkie obserwacje dla których dowolna zmienna numeryczna przyjmuje wartość oddaloną o
#zadaną liczbę odchyleń standardowych od średniej


  if(class(fo) != "ForecastingObject"){
    warning("This function should get ForecastingObject as parameter. Please use buildForecastingObject.")
    return(NULL)
  }



  if(cheatingMode){
    df <-rbind(fo$train,fo$forecast)
  }else{
    df <- fo$train
  }


  ignoredVariables <- append(ignoredVariables,fo$yName)

  checkedVars <- setdiff(names(fo$train), ignoredVariables)
  toRemove<-c()
  toRemove2<-c()
  for(n in checkedVars){
    if(class(fo$train[,n])!="factor" && class(fo$train[,n])!="character" && class(fo$train[,n])!="logical"){

      avg <- mean(df[,n])
      stddev <- sd(df[,n])

      if(ignoreZeros){
        avg <- mean(df[df[,n]!=0,n])
        stddev <- sd(df[df[,n]!=0,n])
      }

      upper.bound <- avg + stddev*howManyDevsAway
      lower.bound <- avg - stddev*howManyDevsAway

      if(verbose){
        print(paste(c(n," upper: ", upper.bound, " lower: ", lower.bound, " avg: ", avg, " stddev: ", stddev),collapse=""))
      }
      x<-sum(fo$train[,n] > upper.bound | fo$train[,n] < lower.bound )
      # print("x")
      # print(x)
      if(ifelse(!is.na(x),x,Inf) < maxRemovedPercentage*nrow(fo$train) && x >0){
        # print("coś będzie usunięte")
        toRemove <- append(toRemove,which(fo$train[,n] > upper.bound | fo$train[,n] < lower.bound))
        # toRemove2 <- append(toRemove,which(fo$forecast[,n] > upper.bound | fo$forecast[,n] < lower.bound))
        # print(length(toRemove))
      }
    }
  }
  if(length(toRemove) > 0){
  toRemove<-sort(unique(toRemove))
  # toRemove2<-sort(unique(toRemove2))
  fo$train <- fo$train[-toRemove,]
  # fo$forecast <- fo$forecast[-toRemove2,]
  }


  fo$description <- append(fo$description, "And then removed outliers")

  fo
}
#
# fo <- buildForecastingObject(iris,iris,"Species")
# fo
# str(fo)
# fo <- outNumericStdDev(fo,howManyDevsAway =5,maxRemovedPercentage = 100)
# str(fo)
