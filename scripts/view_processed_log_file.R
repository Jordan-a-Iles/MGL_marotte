library(zoo);
library(xts);

#Number of minutes to skip
skip <- 10

#Load the file - this could take a LONG time
filename <- file.choose();
cm_raw <- as.xts(read.zoo(filename,sep=',',tz='',header=T,format='%Y-%m-%d %H:%M:%OS'));

#Subsample to make plotting quicker
cm <- align.time( cm_raw[endpoints(cm_raw, "minutes", skip)], n=60*skip );

#Plot variables
plot(cm[,1],main="speed");
readline("Press return to continue");
plot(cm[,2],main="heading");
readline("Press return to continue");
plot(cm[,7],main="battery");
readline("Press return to continue");
plot(cm[,8],main="temperature");
