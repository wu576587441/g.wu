#-----set pathway-----#
setwd(""C:/Users/data")
#-----packages------#
library("irr")
#--Load data- "\" or "\\"
seg1 <- read.table(header=T,file="C:/Users/data/seg1.csv",sep=",")
seg2 <- read.table(header=T,file="C:/Users/data/seg2.csv",sep=",")
seg3 <- read.table(header=T,file="C:/Users/data/seg3.csv",sep=",")
seg1 <- seg1[,1:1339]
seg2 <- seg2[,1:1339]
seg3 <- seg3[,1:1339]
#----12ICC----
iccs1 <- matrix(nrow = 1328,ncol = 1)
for (i in (2 : 1329))
{
  print(i)
  iccs1[i]<- icc(data.frame(seg1[,i], seg2[,i]), "twoway")$value
}
write.table(iccs1,"icc12.csv", sep = ",")
# add features name by hands
icc12 <- read.table()#--edited table
#-----13icc------
iccs2 <- matrix(nrow = 1328,ncol = 1)
for (i in (2 : 1329))
{
  print(i)
  iccs2[i]<- icc(data.frame(seg1[,i], seg3[,i]), "twoway")$value
}
write.table(iccs2,"icc13.csv", sep = ",")
# add features name by hands
icc13 <- read.table()#--edited table
#----histogram------
hist12 <- hist(icc12$Value, xlab = "ICC", main= "Histogram of intra-class correlation coefficients", col="lightblue", border="red",xlim = c(0, 1.0),ylim=c(0,1500))
hist13 <- hist(icc13$Value, xlab = "ICC", main= "Histogram of inter-class correlation coefficients", col="lightblue",border="red",xlim = c(0, 1.0),ylim=c(0,1500))
#----move out icc<0.8
icc12out <- subset(icc12,abs(Value)<0.8)
icc13out <- subset(icc13,abs(Value)<0.8)
icc12outfeatures<-icc12out$General_DicomLocation
icc13outfeatures<-icc13out$General_DicomLocation
iccout_features<-union(icc12outfeatures, icc13outfeatures)
#---creat a new table moved out icc<0.8
