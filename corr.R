corr <- function(directory,threshold=0){
  threshhold_list <- vector()
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
      threshhold_list <- append(threshhold_list, i) ##builds list of files that meets threshhold requirement
    }
  } 
  cor_list <- numeric(length=0)
  for(j in threshhold_list){
    temp <- read.table(paste(directory,"/",j,".csv",sep=""),header=TRUE, sep=",")
    cor_list <- append(cor_list, cor(x=temp$sulfate,y=temp$nitrate,use="complete.obs"))
  }
  cor_list
}