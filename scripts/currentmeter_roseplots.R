# Roseplots of currentmeter data:
# >for instructions(code usage), refer to supplementary file ""
# > for example data code outputs refer to supplementary xlsx
# > Created by Jamie Johns 2020 (Marine Geophysics Laboratory 2020)

### Section 1: load required packages ##############################################################################################################
library(openair) # if not installed run: install.packages("openair")
library(tidyverse) # if not installed run: install.packages("tidyverse")
library(readxl) # if not installed run: install.packages("readxl")
options(warn=-1) # turn off unnecessary warnings (console) [critical error messages will still print]

#### Section 2: specify data to read #########################################################################################################
# >>>> all parameters in this section are intended for modification by the user

#below: full path to excel file containing all currentmeter:
fullpath_summary="C:/current_meterRose/Example_data.xlsx" #<<< substitude "\" for "/"

# below: name of work-sheet to read data from (excel file)
sheet_data=c("Location 1","Location 2","Location 3","Location 4") #List: reference (name) of datasheets representing individual sites (different currentmeter)

site_area_name="Example Area" #<<<<< applied in plots (title): Overall name for all current meter data (e.g - area[location] containing all currentmeter locations)
label_year_range="March2018-March2019" #<<<<< applied in plots (title): Date range covering all current meter data


# Below: limits applied to remove temperature and speed(m/s) data above and below certain values [converted to blank values]
#         >>> if parameter=NA, condition is skipped (values not converted to blank)
#         Important note: If values are changed below, make sure to modify parameters accordingly in
Temp_upperLIM=36 #Upper limit for temperature (<<< e.g- if =36, Temperatures above 36degC are converted to blank values)
Temp_lowerLIM=NA #lower limit for temperature (<<< e.g - if =NA, don't convert temperature values to blank below a certain value)
Speed_upperLIM=1.2 #Upper limit for speed (<<< e.g- if =1.2, Speed(m/s) values above 1.2m/s are converted to blank values)
Speed_lowerLIM=0.0 #Upper limit for speed (<<< e.g - if =0.0, Speed(m/s) values below 0.0m/s are converted to blank values)


# Section 3: Import data and create needed datasets#############################################################################################
# >>>> Run this section to read data from .xlsx file and create dataset applied in section 4
#     (no settings to specify/modify in this section) - also typical warnings produce from this section (console) notify that unexpected values converted to blank value

is_finished=FALSE
for (J in 1:length(sheet_data)){ # for each Jth datasheet in .xlsx file
  print(sprintf("Obtaining data for site: %s [dataset %s of %s]",sheet_data[J],J,length(sheet_data))) # print datasheet being read (console)
  
  sheet_current=sheet_data[J]
  
  # below obtain data from Jth data sheet
  data_imported <- read_excel(fullpath_summary,sheet=sheet_current, 
                              col_types = c("numeric", "date", "numeric", "numeric", "numeric","numeric", "numeric", "numeric","numeric", "numeric"))
  
  # Below: create dataframe(df) using data of interest ////////////////////////////////////////
  #       >> from imported data (xlsx worksheet), obtain data from columns named: "datetime","speed (m/s)" ,"heading (degrees CW from North)" and "temp (Celsius)"
 
  df <- data.frame(matrix(ncol = 4, nrow = nrow(data_imported)))
  colnames(df) <- c("date","speed(m/s)","Direction(heading)","Temp(degC)")
  df[,1] <- data_imported["datetime"]
  df[,2] <- data_imported["speed (m/s)"]
  df[,3] <- data_imported["heading (degrees CW from North)"] 
  df[,4] <- data_imported["temp (Celsius)"]
  
  
  # For df: convert data to blank values that are above/below specified thresholds [see section 2]//////////////////////////////////////////////
  if(!is.na(Temp_upperLIM)){ #if upperlimit for temperatue is specified(!=NA)
    df$'Temp(degC)'[df$'Temp(degC)'>Temp_upperLIM]=NA
  }

  if(!is.na(Temp_lowerLIM)){  #if lowerlimit for temperatue is specified(!=NA)
    df$'Temp(degC)'[df$'Temp(degC)'<Temp_lowerLIM]=NA
  }
  
  if(!is.na(Speed_upperLIM)){ #if upperlimit for speed is specified(!=NA)
    df$'speed(m/s)'[df$'speed(m/s)'>Speed_upperLIM]=NA
  }

  if(!is.na(Speed_lowerLIM)){ #if lowerlimit for speed is specified(!=NA)
    df$'speed(m/s)'[df$'speed(m/s)'<Speed_lowerLIM]=NA
  }
  
    
  # Below: use data from "df" to create further datasets ////////////////////////////////////////////////////////////
  #       >>> below datasets are temporary (for Jth datasheet of xlsx)
  
  #Wet season dataframe: Nov-March (inclusive) ; Months=c(1,2,3,11,12) [across all years] @@@@@@@@@@@@@@@@@@
  data_current_wet <- selectByDate(df,month = c(1,2,3,11,12))
  data_current_wet[,5]<-"Wet Season(Nov-Mar)"
  data_current_wet[,6]<- sprintf("Site: %s",sheet_data[J])
  
  colnames(data_current_wet) <- c("date","speed(m/s)","Direction(heading)","Temp(degC)","season_type","site_name")
  #dry season: April-Oct (inclusive) ; months=4:10 [across all years] @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  data_current_dry <- selectByDate(df,month = 4:10)
  data_current_dry[,5]<-"Dry Season(Apr-Oct)"
  data_current_dry[,6]<- sprintf("Site: %s",sheet_data[J])
  #Combined data (wet and dry seson)
  colnames(data_current_dry) <- c("date","speed(m/s)","Direction(heading)","Temp(degC)","season_type","site_name")
  data_current_combined<-rbind(data_current_dry,data_current_wet)
  
  # below: datasets used in "section 4"  (combination of all datasheets from xlsx)////////////////////////////////
  if (J==1){ # if reading first datasheet [define data frames]
    data_all=data_current_combined
    data_wet=data_current_wet  
    data_dry=data_current_dry  
  }else{ # for remaining datasheets [append Jth data with existing dataframes]
    data_all<-rbind(data_all,data_current_combined)
    data_wet<-rbind(data_wet,data_current_wet)
    data_dry<-rbind(data_dry,data_current_dry) 
  }
  
  
  if(J==length(sheet_data)) #<<<< if final datasheet was succesfully processed
  {
    is_finished=TRUE
  }
  
}


if(!is_finished) #<<<<<<<<<<< If not all datasheets successfully processed(above)
{
  print('Section 3 did not complete without an error\nPlease check your inputs for Section 2 and then try running section 3 again.')
  #If above text is observed in console, user needs to redo sections 2-3
}




#Section 4: Create and save plots 1-9 ##########################################################################################################
#    >>> to find output location of saved (plots jpg): run command "getwd()"
#      >>>> OR, a custom location can be set via command setwd("C:/folder/subfolder") [must be existing folder]


# Parameters (settings) for customising plots @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
ANGLE_VAL=30 #<<<<<<<<<<< Applied in pollutionrose(..) plots; incremental steps (deg) for binning data by direction(angle); seg(0,360,by=ANGLE_VAL)
data_name=sprintf("%s [%s]",site_area_name,label_year_range)  # main title for plots 1-3
colorsteps_temperature=seq(16, 36, by=2) # For plots 1,4 and 7: colourbar range and increment length ["by="] for Temperature legend
colorsteps_speeds=seq(0, 1.2, by=0.1) # For plots 2,5 and 8: : colourbar range and increment length ["by="] for speed legend
colorlimits_temperature=c(16,36) # For plots 3,6 and 9: min and max value for Temperature colourbar [legend]

speed_bin_int=0.05 # For all plots applying pollutionRose(...): interval (m/s) for binning data by speed
speed_uplim=0.8 # For all plots applying PolarRose(...): axis limit for speed (m/s) in plots [axis limits changed]
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



# ((((( No settings (parameters) need to be specified/modified in remaining lines of the code ))))


while (!is.null(dev.list())) dev.off() #<<<< this line: close all open files for in case there was previous,incomplete run of "section 4")


# Specification of function used to save plots to jpg file (applied throughout section 4) @@@@@@@@@@@@@@@@@@@@@@@@
save_image <- function(file_name,plot_object){ # output resolution and dimensions can be customised below
  invisible(jpeg(file_name,width=4*716,height=4*561,quality = 100,pointsize = 20,res=4*100))
  print(plot_object)
  dev.off()
}



#Plots(1-3): concerning all data (combining wet and dry data)///////////////////////////////////////////////////////////////////////////////////////

# Plot 1 : temperature frequency plot@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
colnames(data_all) <- c("date","ws","wd","Temp(degC)","season_type","site_name") # rename columns: speed->"ws" and direction->"wd" (expected column names needed for pollution rose plots)

# below plot function:
PLOT_OBJ<-pollutionRose(data_all,angle=ANGLE_VAL, pollutant = "Temp(degC)",
              key.position = "right",type="site_name",
              main=data_name,breaks=colorsteps_temperature,
              sub="For each site: Frequency(%) of temperature(degC) by direction (heading)",
              key.footer="temperature\n(degC)",ws.int=speed_bin_int,annotate=FALSE,col="jet") 


file_name<-sprintf("Plot1-%s-all_data(Temp_freq).jpg",site_area_name) #<<<< file name for "plot 1"
save_image(file_name,PLOT_OBJ) # <<<<< save "plot 1" to file

# Plot 2 : speed frequency plot@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

PLOT_OBJ<-pollutionRose(data_all,angle=ANGLE_VAL, pollutant = "ws",
              key.position = "right",type="site_name",
              main=data_name,breaks=colorsteps_speeds,
              sub="For each site: Frequency(%) of speed(m/s) by direction (heading)",
              key.footer="Speed(m/s)",ws.int=speed_bin_int,annotate=FALSE) #plot function


file_name<-sprintf("Plot2-%s-all_data(Speed_freq).jpg",site_area_name)  #<<<< file name for "plot 2"
save_image(file_name,PLOT_OBJ) # <<<<< save "plot 2" to file


# Plot 3 : Bivariate plot of temperature by speed and direction @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
colnames(data_all) <- c("date","speed(m/s)","Direction(heading)","Temp(degC)","season_type","site_name") #unique names for wd and ws (for labelling purpose in figure; "wd" and "ws" name not required for PolarPlot)

# Below: Set text displayed in plots
figure_title_sub="For each site: Average Temperatures(degC) by speed(m/s) and direction(heading)" #bottom text of plot
figure_title_main= sprintf("%s [%s]",site_area_name,label_year_range) # top text of plot
figure_title_key = "Temperature (degC)" #text for colorbar (temperature)


PLOT_OBJ<-polarPlot(data_all,x = "speed(m/s)",
          wd = "Direction(heading)",pollutant="Temp(degC)",main=figure_title_main,type="site_name",
          sub=figure_title_sub,key.head="",key.footer =figure_title_key,key.header = "",
          key.position="bottom",min.bin = 6,limits=colorlimits_temperature,col="jet",mis.col = "transparent",
          ws.int = speed_bin_int,upper=speed_uplim) #<<<<< plot function



file_name<-sprintf("Plot3-%s-all_data(temp_polarplot).jpg",site_area_name) #<<<< file name for "plot 3"
save_image(file_name,PLOT_OBJ) # <<<<< save "plot 3" to file

# (Plots 4-9): Dry and Wet season plots  //////////////////////////////////////////////////////////////////////////////////////////////////

for (K in 1:2){ # If K=1(use dry season data) and =2 (use wet season data)
  
  # Specifiy data used in plots (Kth data)@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  if(K==1){ #if dry season data [plots 4 to 6 created]
    data_use=data_dry
    data_season_name="Dry Season(Apr-Oct)"
    figure_name="dry_data"
    plot_num_offset=0
  }else{ #else, wet season data (K==2) [plots 7 to 9 created]
    data_use=data_wet
    data_season_name="Wet Season(Nov-Mar)"
    figure_name="wet_data"
    plot_num_offset=3 # offset applied to numbering use in plot filenames (output .jpg)
  }
  
  data_name=sprintf("%s: %s [%s]",site_area_name,data_season_name,label_year_range)   # Main title for Kth set of plots
  
  
  
  # Plot 4 and 7 : temperature frequency plot@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  

  colnames(data_use) <- c("date","ws","wd","Temp(degC)","season_type","site_name") # rename columns: speed->"ws" and direction->"wd" (expected column names needed for pollution rose plots)

  PLOT_OBJ<-pollutionRose(data_use,angle=ANGLE_VAL, pollutant = "Temp(degC)",
                          key.position = "right",type="site_name",
                          main=data_name,breaks=colorsteps_temperature,
                          sub="For each site: Frequency(%) of temperature(degC) by direction (heading)",
                          key.footer="temperature\n(degC)",ws.int=speed_bin_int,annotate=FALSE,col="jet") #<<<< plot function
  

  file_name<-sprintf("Plot%s-%s-%s(Temp_freq).jpg",4+plot_num_offset,site_area_name,figure_name) #<<<< file name for "plot 4" and "plot 7"
  save_image(file_name,PLOT_OBJ) #save plot to file
  
  
  # Plot 5 and 8 : speed frequency plot@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  PLOT_OBJ<-pollutionRose(data_use,angle=ANGLE_VAL, pollutant = "ws",
                          key.position = "right",type="site_name",
                          main=data_name,breaks=colorsteps_speeds,
                          sub="For each site: Frequency(%) of speed(m/s) by direction (heading)",
                          key.footer="Speed(m/s)",ws.int=speed_bin_int,annotate=FALSE) #<<< plot function

  file_name<-sprintf("Plot%s-%s-%s(Speed_freq).jpg",5+plot_num_offset,site_area_name,figure_name) #<<<< file name for "plot 5" and "plot 8"
  save_image(file_name,PLOT_OBJ) #<<<< save plot to file
  
  
  # Plot 6 and 9 : Bivariate plot of temperature by speed and direction @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  colnames(data_use) <- c("date","speed(m/s)","Direction(heading)","Temp(degC)","season_type","site_name")  #unique names for wd and ws (for labelling purpose in figure; "wd" and "ws" name not required for PolarPlot)
  
  # Below: Set text displayed in plots 6 and 9
  figure_title_sub="For each site: Average Temperatures(degC) by speed(m/s) and direction(heading)" #bottom text of plot
  figure_title_main= data_name  # top text of plot
  figure_title_key = "Temperature (degC)" #text for colorbar (temperature)

  PLOT_OBJ<-polarPlot(data_use,x = "speed(m/s)",type="site_name",
                      wd = "Direction(heading)",pollutant="Temp(degC)",main=figure_title_main,
                      sub=figure_title_sub,key.head="",key.footer =figure_title_key,key.header = "",
                      key.position="bottom",min.bin = 6,limits=colorlimits_temperature,col="jet",
                      mis.col = "transparent",ws.int = speed_bin_int,upper=speed_uplim) #<<<<<<<< plot function
  

  file_name<-sprintf("Plot%s-%s-%s(temp_polarplot).jpg",6+plot_num_offset,site_area_name,figure_name) #<<<< file name for "plot 6" and "plot 9"
  save_image(file_name,PLOT_OBJ)#<<<< save plot to file
  
}


print(sprintf("Output location of saved plots(jpg): '%s'",getwd()))
      