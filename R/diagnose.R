diagnose <- function(fo,minObs=10){
  #funkcja weźmie fo i sprawdzi:
  # w każdym data secie
  #wszystko to co diagnoseHelper robi
  print("Train dataset")
  diagnoseHelper(fo$train,fo$yName,minObs)
  print("")
  print("")
  print("")
  print("Full train dataset")
  diagnoseHelper(fo$trainFull,fo$yName,minObs)
  print("")
  print("")
  print("")
  print("Forecast dataset")
  diagnoseHelper(fo$forecast,fo$yName,minObs)
}

diagnoseHelper <- function(df,yName,minObs){
  require(caret)
  # 1. ile jest wierszy?
  # 2. ile jest kolumn?
  # 3. w ilu wierszach brakuje obserwacji?
  # 4. czy istnieją zmienne które nie mają wariancji (i które to)?
  # 5. czy istniją zmienne, w których level jest nieużywany? <- TODO!!!!
  df <- df[,setdiff(names(df),yName)]
  print(paste(c("There is ", nrow(df), " rows!"),collapse=""))
  print(paste(c("There is ", ncol(df), " columns!"),collapse=""))
  x<- apply(df,2,pMiss)
  x<-x[x>0]
  if(length(x)>0){
    for(a in 1:length(x)){
      print(paste(c("There is ", x[a], " missing values in ", names(x)[a] ," variable!"),collapse=""))
    }
  }else{
    print("No missing values!")
  }

  x <- nearZeroVar(df, saveMetrics= TRUE)
  x <- x[x$zeroVar==TRUE,]

  for(a in row.names(x)){
    print(paste(c("Variable ", a ," has no variance!"),collapse=""))
  }

  x<- sapply(df, class)
  x<-names(x[x=="factor"])
  a<-apply(df[,x],2,table)

  i<-0
  for(v in a){
    i<-i+1
    # print(v)
    y <-v[v<=minObs]
    if(length(y)>0){
      # print(x)
      print(paste(c("Variable ", x[i] ," has some rare levels!"),collapse=""))

    }
  }
}

