pollutantmean <- function(directory, pollutant, id = 1:332){
  growing_list = numeric()
  for(i in id){
    if(i<10){
      i <- paste("00",i,sep="")
    }
    else if(i<100 && i>9){
      i <- paste("0",i,sep="")
    }
    else{
      i <- paste(i,sep="")
    }
    temp <- read.table(paste(directory,"/",i,".csv",sep=""),header=TRUE, sep=",")
    good <- !is.na(temp[,pollutant])
    current_list <- temp[good,][,pollutant]
    growing_list <- c(current_list,growing_list)
  }
  mean(growing_list)
}