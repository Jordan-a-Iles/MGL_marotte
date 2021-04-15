# Libraries
library(zoo);
library(xts);
library(Hmisc);

# Number of minutes to skip (set doSkip to T to enable)
doSkip = F;
skip <- 10;

# Major tick locations (hours, weeks, months etc.);
tickLoc <- "weeks"
# Date format (see https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)
tickFmt <- "%d %b"

# List all CSV files
filenames <- dir(pattern='\\.csv$', ignore.case=T);

# Create directory
dir.create("graphs", showWarnings = FALSE);

# Loop through files and create graphs
for(filename in filenames) {
  # Load file
  cm <- as.xts(read.zoo(filename,sep=',',tz='',header=T,format='%Y-%m-%d %H:%M:%OS'));
  # Subsample (makes plotting quicker for very large files)
  if (doSkip) {
    cm <- align.time( cm[endpoints(cm, "minutes", skip)], n=60*skip );
  }
  
  # Output file
  filebase = paste("graphs/", substr(filename,1,nchar(filename)-4), sep="");
  
  # Colours based on speed and heading
  heading <- coredata(cm[,2])
  headingClr <- hsv(heading/360,1,1);
  speed <- coredata(cm[,1])
  speedClr <- rgb(speed / max(speed),1-speed / max(speed),0);
  
  # Plot variables
  # Speed
  ylimits <- c(0, max(cm[,1]));
  plot(cm[,1],main="Speed",ylab="Speed (m/s)",xlab="Time",ylim=ylimits,major.ticks=tickLoc, major.format=tickFmt, minor.ticks=F);
  dev.copy(png,filename=paste(filebase,"_speed.png",sep=""),width = 1000, height = 500);
  dev.off();
  
  # Heading
  plot(cm[,2],type="p",main="Heading",ylab="Heading (degrees from North)",yaxt="n", xlab="Time", ylim=c(360,0),major.ticks=tickLoc, major.format=tickFmt, minor.ticks=F,pch=20, col=speedClr);
  axis(side = 2, at = c(0,90,180,270,360));
  dev.copy(png,filename=paste(filebase,"_heading.png",sep=""),width = 1000, height = 500);
  dev.off();
  
  ylimits <- c(1.5,3.3);
  plot(cm[,7],main="Battery",ylab="Battery (V)",xlab="Time",ylim=ylimits,major.ticks=tickLoc, major.format=tickFmt, minor.ticks=F);
  dev.copy(png,filename=paste(filebase,"_batt.png",sep=""),width = 1000, height = 500);
  dev.off();
  
  ylimits <- c(15,35);
  plot(cm[,8],main="Temperature",ylab="Temperature (C)",xlab="Time",ylim=ylimits,major.ticks=tickLoc, major.format=tickFmt, minor.ticks=F);
  dev.copy(png,filename=paste(filebase,"_temp.png",sep=""),width = 1000, height = 500);
  dev.off();
}
