complete <- function(directory,id=1:332){
  data_frame = data.frame()
  for(i in id){
    original_i <- i
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
    sum_of_complete <- sum(complete.cases(temp))
    data_frame <- rbind(data_frame,c(original_i,sum_of_complete))
  } 
  names(data_frame) <- c("id","nobs")
  data_frame
}