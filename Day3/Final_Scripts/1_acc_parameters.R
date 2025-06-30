#############
#In this code we segment the accelerometer trips into a set time interval and calculate metrics of XYZ and energy expenditure for each interval.

#This code is based on a code written by Phillip Collins for the paper "Interpreting behaviors from accelerometry: a method combining simplicity and objectivity", and edited by Sarah Saldahna and Leia Navarro for the Seabird Group R class 2022
#If you have any questions please contact Sarah Saldanha (sarahsaldanha9@gmail.com)
#############


#place <- "PC"
place<- "laptop"


if (place == "PC") WD <- "CC:/Users/sarah/Dropbox/PhD/RClass_2025/Data Curation, Handling, and Interpretation of Accelerometery Data/Day3/Data"
if (place == "laptop") WD <- "C:/Users/sarah/Dropbox/PhD/RClass_2025/Data Curation, Handling, and Interpretation of Accelerometery Data/Day3/Data" 


###########
#Libraries#
###########
library(tidyverse) 
library(lubridate)
library(data.table)
library(zoo)

# parallel computing
library(doParallel)
library(foreach)
# milliseconds
op <- options(digits.secs=6)

#----------------------
#Define number of cores
#----------------------

cores <- detectCores()-2

#---------------------------------------------------------------
#Prepare cluster
#---------------------------------------------------------------

cl <- makeCluster(cores)
registerDoParallel(cl)

#---------------------------------
#Set the require segment length
#---------------------------------

freq <- 25# The Frequency of accelerometry data (Hz)
secs <- 2 # The number of seconds over which to calculate the desired metrics.
# The manuscript says to use 1 second intervals, but Phil said "to capture 
# gliding flight as well I've found that a longer period is needed."
numrows <- freq*secs # The number of rows required to calculate metrics over the chosen period. 

#---------------------------------
#Read axy files
#---------------------------------
#---------------------------------
setwd(paste0(WD,"/Axy/raw_axy/"))

axy_files <- list.files(pattern = '.csv')
print(axy_files)

#---------------------------------
# function to calculate mode
#---------------------------------

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
  }

####Lets check out what the raw axy data looks like 


axy_data<-fread(axy_files[1], sep=",")


head(axy_data)


#---------------------------------------------------------------
#Let's run
#---------------------------------------------------------------
#for (j in seq_along(axy_files)){

l=foreach(i=1:length(axy_files), .packages=c("tidyverse", "lubridate", "data.table", "zoo", "dplyr")) %dopar% {
  #i=1
  #---------
  #Read file
  #---------
  axy_data<-fread(axy_files[i], sep=",")%>%
    mutate(
      Timestamp=dmy_hms(Timestamp),
      #--------------
      #Checking units
      #--------------
      #Check units and convert to g if needed. In some trips the data may be in the wrong units...but this is unlikely
      mean_x=mean(X),
      X=ifelse(mean_x>100,X/1000, X),
      Y=ifelse(mean_x>100,Y/1000, Y),
      Z=ifelse(mean_x>100,Z/1000, Z),
      #------------
      #Pitch & Roll 
      #------------
      pitch=atan((X/(sqrt((Y*Y)+(Z*Z)))))*(180/pi),
      roll=atan((Y/(sqrt((X*X)+(Z*Z)))))*(180/pi),
      #-----
      #Depth here we are caluclating the mode of the depth per trip and substracting it. This corrects a bit for differences in sea surface pressure but not completely... The next script works with depth more robustly. na.approx is a linear interpolation
      #-----
      Pressure= na.approx(Pressure, rule = 2),
      Approx_depth_negative_mode= 0.01*(getmode(round(Pressure))-Pressure),
      Hz=round(n()/as.numeric(difftime(max(Timestamp),min(Timestamp),units="secs"))))%>%  
      dplyr::select(-mean_x)
  
  #Label segments, removing the last records that do not make up enough rows
  ###Here assigning the 2 second segments
  row_num <- floor(nrow(axy_data)/numrows)*numrows
  axy_data <- axy_data[1:row_num,]
  axy_data$segID <- rep(1:(row_num/numrows),each = numrows)
  axy_data$bird_seg <- paste(axy_data$TagID, axy_data$segID,sep="_")
  
  #----------
  #Static XYZ
  #----------
  Static<-axy_data%>%
    group_by(bird_seg)%>%
    summarise(
      StaticX=sum(zoo::rollapply(X,numrows,mean,fill=NA),na.rm = T),
      StaticY=sum(zoo::rollapply(Y,numrows,mean,fill=NA),na.rm = T),
      StaticZ=sum(zoo::rollapply(Z,numrows,mean,fill=NA),na.rm = T))%>%
    ungroup()
  
  axy_data=axy_data%>%
    left_join(.,Static,by="bird_seg")
  
  axy_data=as.data.frame(axy_data)
  axy_data$bird_seg=as.factor(axy_data$bird_seg)
  
  sz=axy_data%>%
    mutate(
      ####Calculates DBA for each axis. 
      DynamicX=X-StaticX,
      DynamicY=Y-StaticY,
      DynamicZ=Z-StaticZ,
      #Combines the DBA into ODBA or VeDBA
      ODBA=abs(DynamicX)+abs(DynamicY)+abs(DynamicZ),
      VeDBA=sqrt((DynamicX^2)+(DynamicY^2)+(DynamicZ^2)))%>%
    group_by(bird_seg)%>%
    summarise(
      #id's
      #id=paste0(unique(ring)),
      TagID=paste0(unique(TagID)),
      #start of the seg
      start_seg=min(Timestamp),
      #OBDA/VeDBA
      mean_ODBA=mean(ODBA),
      sd_ODBA=sd(ODBA),
      mean_VeDBA=mean(VeDBA),
      sd_VeDBA=sd(VeDBA),
      #Surge
      meanX_surge=mean(X),
      stdevX_surge=sd(X),
      minX_surge=min(X),
      maxX_surge=max(X),
      sum_positive_X_surge=sum(X[which(X>0)]),
      sum_negative_X_surge=sum(X[which(X<0)]),
      #Sway
      meanY_sway=mean(Y),
      stdevY_sway=sd(Y),
      minY_sway=min(Y),
      maxY_sway=max(Y),
      sum_positive_Y_sway=sum(Y[which(Y>0)]),
      sum_negative_Y_sway=sum(Y[which(Y<0)]),
      #heave
      meanZ_heave=mean(Z),
      stdevZ_heave=sd(Z),
      minZ_heave=min(Z),
      maxZ_heave=max(Z),
      sum_positive_Z_heave=sum(Z[which(Z>0)]),
      sum_negative_Z_heave=sum(Z[which(Z<0)]),
      #pitch
      pitch_mean=mean(pitch),
      pitch_stdev=sd(pitch),
      min_pitch=min(pitch),
      max_pitch=max(pitch),
      sum_positive_pitch=sum(pitch[which(pitch>0)]),
      sum_negative_pitch=sum(pitch[which(pitch<0)]),
      #roll
      roll_mean=mean(roll),
      roll_stdev=sd(roll),
      min_roll=min(roll),
      max_roll=max(roll),
      sum_positive_roll=sum(roll[which(roll>0)]),
      sum_negative_roll=sum(roll[which(roll<0)]),
      #depth
      depth_mean=mean(Approx_depth_negative_mode),
      depth_stdev=sd(Approx_depth_negative_mode),
      min_depth=min(Approx_depth_negative_mode),
      max_depth=max(Approx_depth_negative_mode),
      sum_positive_depth=sum(Approx_depth_negative_mode[which(Approx_depth_negative_mode>0)]),
      sum_negative_depth=sum(Approx_depth_negative_mode[which(Approx_depth_negative_mode<0)]))%>%
    select(TagID,bird_seg,start_seg, everything())%>%
    arrange(start_seg)
  
  sz #PRINT IN FOREACH FUNCTION (do not remove this line if you want to store the info calculated)
  }

#---------------------------------------------------------------
#Write data
#---------------------------------------------------------------

lapply(seq_along(l), function(i){
  fwrite(l[[i]], paste0(WD,"/Data/Outputs/Axy_data_with_metrics/", l[[i]]$TagID[1], "_axymetrics.csv"),row.names=F)
 })




#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

