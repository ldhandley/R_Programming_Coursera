corr <- function(directory,threshold=0){
  growing_data_frame = data.frame() ##this will be a dataframe containing all nitrate & sulfate data that we can correlate in the end
  for(i in 1:332){
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
    if(complete(directory,original_i)$nobs > threshold){
      growing_data_frame <- rbind(growing_data_frame,c(temp$sulfate,temp$nitrate))
    }
  } 
  names(growing_data_frame) <- c("sulfate","nitrate")
  growing_data_frame
}

##NEED TO GET RID OF NA VALUES BEFORE ADDING TO GROWING_DATA_FRAME