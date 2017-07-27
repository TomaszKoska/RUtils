traZbijacz <- function(fo,minCount=10,verbose=F){


  if(class(fo) != "ForecastingObject"){
    warning("This function should get ForecastingObject as parameter. Please use buildForecastingObject.")
    return(NULL)
  }


  df <- rbind(fo$trainFull,fo$forecast)
  inTrain <- rep(FALSE,nrow(fo$trainFull)+nrow(fo$forecast))
  inTrain[1:nrow(fo$trainFull)]<-TRUE

  for(n in names(df)){ #Dla każdego factora
    if(verbose) {print(n)}
    if(class(df[,n])=="factor" && n != fo$yName){
      df[,n] <- droplevels(df[,n])
      if(verbose) {print(table(df[,n]))}
      counts<-sort(table(df[,n]))
      levelsToRemove<- names(counts[counts<minCount])

      if(length(levelsToRemove)>0){
        #policzmy mediany dla wszystkich poziomów
        medians<-aggregate(df[inTrain,fo$yName], list(df[inTrain,n]), median)
        medians<-medians[order(medians[,2]),]
        names(medians)<-c("level","median")

        for(ltr in levelsToRemove){ #Dla każdego poziomu
          #wyszukaj najpodobniejszy poziom
          thisMedian <- medians[medians$level==ltr,"median"]

          distances <- as.data.frame(medians$level)
          distances$distance <- abs(medians$median - thisMedian)
          names(distances)<-c("level","distance")
          distances <-distances[!(distances$level %in% levelsToRemove),]
          distances <-distances[order(distances[,2]),]
          newLevelName <- distances[1,"level"]
          df[df[,n]==ltr,n] <- newLevelName
          df[,n]<-droplevels(df[,n])
          if(length(levels(df[,n]))==1){ #usuń zmienne z jednym poziomem!
            df[,n] <-NULL
          }
        }
      }
    }
  }
# print(dim(df))
# print(dim(df[inTrain,]))
# print(dim(df[!inTrain,]))

  fo$trainFull <- df[inTrain,]
  fo$train <- fo$trainFull
  fo$forecast <- df[!inTrain,]
  fo$description <- append(fo$description, "And then we used zbijacz!")
  fo
}
