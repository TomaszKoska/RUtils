outNumericStdDev <- function(fo, howManyDevsAway = 2, ignoredVariables = c(), maxRemovedPercentage= 0.01){
#usuwa wszystkie obserwacje dla których dowolna zmienna numeryczna przyjmuje wartość oddaloną o
#zadaną liczbę odchyleń standardowych od średniej


  if(class(fo) != "ForecastingObject"){
    warning("This function should get ForecastingObject as parameter. Please use buildForecastingObject.")
    return(NULL)
  }

  ignoredVariables <- append(ignoredVariables,fo$yName)

  checkedVars <- setdiff(names(fo$train), ignoredVariables)
  toRemove<-c()
  toRemove2<-c()
  for(n in checkedVars){
    if(class(fo$train[,n])!="factor" && class(fo$train[,n])!="character" && class(fo$train[,n])!="logical"){
      avg <- mean(fo$train[,n])
      stddev <- sd(fo$train[,n])
      upper.bound <- avg + stddev*howManyDevsAway
      lower.bound <- avg - stddev*howManyDevsAway
      # print("avg")
      # print(avg)
      # print("std")
      # print(stddev)
      # print("up")
      # print(upper.bound)
      # print("lo")
      # print(lower.bound)
      # print(fo$train[,n] )
      x<-sum(fo$train[,n] > upper.bound | fo$train[,n] < lower.bound )
      # print("x")
      # print(x)
      if(ifelse(!is.na(x),x,Inf) < maxRemovedPercentage*nrow(fo$train) && x >0){
        # print("coś będzie usunięte")
        toRemove <- append(toRemove,which(fo$train[,n] > upper.bound | fo$train[,n] < lower.bound))
        toRemove2 <- append(toRemove,which(fo$forecast[,n] > upper.bound | fo$forecast[,n] < lower.bound))
        # print(length(toRemove))
      }
    }
  }
  if(length(toRemove) > 0){
  toRemove<-sort(unique(toRemove))
  toRemove2<-sort(unique(toRemove2))
  fo$train <- fo$train[-toRemove,]
  fo$forecast <- fo$forecast[-toRemove2,]
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
