library(zoo);
library(xts);

#Number of minutes to skip
skip <- 10

#Load the file - this could take a LONG time
filename <- file.choose();
cm_raw <- as.xts(read.zoo(filename,sep=',',tz='',skip=9,header=T,format='%Y-%m-%d %H:%M:%OS'));

#Subsample to make plotting quicker
cm <- align.time( cm_raw[endpoints(cm_raw, "minutes", skip)], n=60*skip )

#Plot variables
plot(cm$acc_x);
readline("Press return to continue");
plot(cm$acc_y);
readline("Press return to continue");
plot(cm$acc_z);
readline("Press return to continue");
plot(cm$mag_x);
readline("Press return to continue");
plot(cm$mag_y);
readline("Press return to continue");
plot(cm$mag_z);
readline("Press return to continue");
plot(cm$batt);
readline("Press return to continue");
plot(cm$temp);

plot(cm_raw$acc_x);
