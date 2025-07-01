
#####In this script we use the diveMove package to correct for drift in TDR data and then calculate summary information on each dive
#This code was written by Sarah Saldahna for the Seabird Group R class 2022
#If you have any questions please contact Sarah Saldanha (sarahsaldanha9@gmail.com)


         
library(tidyr)          
library(lubridate)      
library(zoo)
library(data.table)
library(plotly)
library(diveMove)
library(stringr)



memory.limit(size=5000000000)

###############TDR data analysis
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

####bring in axy data and take only TDR lines

setwd("C:/Users/sarah/Dropbox/PhD/RClass_2025/Data Curation, Handling, and Interpretation of Accelerometery Data/Day3/Data/Axy/raw_axy")
wd<-"C:/Users/sarah/Dropbox/PhD/RClass_2025/Data Curation, Handling, and Interpretation of Accelerometery Data/Day3/"

#data_wdir <- "C:/Users/Sarah Saldanha/Dropbox/PhD/RClasses_2022/Behavioural Annotation from Accelerometry and TDR/Data/Axy/raw_axy"


PHAAET_files <- list.files( pattern = '.csv')
print(PHAAET_files)





axy_PHAAET_files_TDR <- function(file_name){
  #file_name <-PHAAET_files[1]
  
  # First import the data from the file
  
  axy_data<-fread( file_name, header = T)
  

  
  axy_data<-subset(axy_data, !is.na(Pressure))####this where the tdr data
  ####now subset data to the 1sec with Pressure data 
  axy_data$Timestamp_TDR<-floor_date(dmy_hms(axy_data$Timestamp), "sec")



     ####Calculate depth and spread pressure
  axy_data$Approx_depth_negative_mode<-0.01*(getmode(round(axy_data$Pressure))-axy_data$Pressure)
  axy_data$Approx_depth_negative_mean<-0.01*(mean(axy_data$Pressure)-axy_data$Pressure)
  
  
  
  ggplot(data = axy_data[100000:150000], aes(x =Timestamp_TDR, y= Pressure)) +
    geom_line()+
    geom_point()+
    geom_hline(yintercept = mean(axy_data$Pressure), colour = "red") +
    geom_hline(yintercept = getmode(round(axy_data$Pressure)), colour = "green") 
  
  

write.csv(axy_data, paste0(wd, "Data/Outputs/TDR/", unique(axy_data$TagID), "_TDR" ,".csv"), row.names = FALSE)

  
}



lapply(PHAAET_files, axy_PHAAET_files_TDR)



####### Now bring in all the TDR data files, manipulate with moveDive and then merge with complete gps datasets. 



setwd(paste0(wd, "Data/Outputs/TDR/"))



PHAAET_files <- list.files(pattern = '.csv')
print(PHAAET_files)


##Some links to understand the filters better
#https://mran.microsoft.com/snapshot/2017-12-24/web/packages/diveMove/vignettes/diveMove.pdf
#https://cran.r-project.org/web/packages/diveMove/vignettes/diveMove.html


#Not working
#####"8201666_PHAAET_rec04022021_ICima_ninho_39_37_S1_4_TDR.csv" ###30, NO DIVES. if there are no dives in your trip it will fail!!! 

#file_name <-PHAAET_files[1]
TDR_ZERO_CORRECT <- function(file_name){
  #file_name <-PHAAET_files[2]
  
  # First import the data from the file
  
  tdr_data<-fread(file_name, header = T)
  
  
  tdr_data<-subset(tdr_data, !duplicated(Timestamp))
  tdr_data<-subset(tdr_data, !is.na(Timestamp_TDR))
  
  tdr_data$Timestamp_TDR<-tdr_data$Timestamp_TDR
  tdr_data<-subset(tdr_data, !is.na(Timestamp_TDR))
  
  
  
  
  #order by timestamp
  tdr_data$Timestamp_TDR<-ymd_hms( tdr_data$Timestamp_TDR)
  tdr_data$depth<-(tdr_data$Approx_depth_negative_mode*-1)
  
tdr_data <- tdr_data[order(tdr_data$Timestamp_TDR)]
 


####create the Time-Depth recorded data  (dtime = 1 since the data was recorded every second, in concurent data we are keeping all the additional columns)
  tdrX <- createTDR(time=tdr_data$Timestamp_TDR,
                    depth=tdr_data$depth,
                    concurrentData=dplyr::select(tdr_data, Timestamp_TDR, depth),
                    dtime=1, file = file_name,
                    speed=F)
  
  #plotTDR(tdrX)
  
  
  # 
  # to callibrate the depth, we will applya smoothing/filtering mechanism where running quantiles can be applied to depth measurements sequentially, using .depth.filter.
  
  # We will use two filters: the first one being a running median with a narrow window to remove noise from the time series, followed by a running low quantile using a wide time window. The integer vector given as argument k specifies the width of the moving window(s), where ki is the width for the ith filter in units of the sampling interval of the TDR object. Similarly, the integer vector given as argument probs specifies the quantile for each filter, where probsi is the quantile for the ith filter. Smoothing/filtering can be performed within specified minimum and maximum depth bounds using argument depth.bounds2, in cases where surface durations are relatively brief separated by long periods of deep diving. These cases usually require large windows, and using depth bounds helps to stabilize the surface signal. Further details on this method are provided by Luque and Fried (2011).
  
  

  db <- c(-0.25,0.25) ####interval of where we thing the zero is
  dt <- 0.2 #dive threshold (we think that dives should be at minimum 0.2m depth, this will change by species)
  k<-c(3, 60)##length of the two running windows size in seconds
  P <- c(0.5, 0.05)####probs
  
  
  d.filter <- diveMove:::.depthFilter(depth = getDepth(tdrX),
                                      k = k, probs = P,
                                      depth.bounds = db, na.rm = TRUE)
  #plotTDR(tdrX)
  

  png(paste0(wd, "Data/Outputs/Plots/", file_name, "_zero_corrected.png"))
  
  # Creating a plot
  
  plotZOC(tdrX, d.filter)
  # Closing the graphical device
  dev.off() 
  
  dcalib <- calibrateDepth(tdrX, dive.thr = dt, zoc.method = "filter",
                           k = k, probs = P, dive.model="smooth.spline",
                           depth.bounds = db, na.rm = TRUE)
  
  tdrXSumm1 <- diveStats(dcalib)

  
  tdrXSumm1$TagID<-str_sub(file_name, 1, -9)
  
  
  
  
  
  write.csv(tdrXSumm1, paste0(wd, "/Data/Axy/TDR_summary/", unique(tdr_data$TagID), "_TDR_summary" ,".csv"), row.names = FALSE)
  
  
}

lapply(PHAAET_files, TDR_ZERO_CORRECT)



###########Now you can do many things with this, 



# 
all<-readRDS(paste0(wd,"/Data/all_axy_merged_to_GPS.rds"))


#######

setwd(paste0(wd, "/Data/Axy/TDR_summary/"))

FILES <- list.files(pattern = '.csv')
print(FILES)



 
msum <- 
  do.call(rbind,
          lapply(list.files(path = (paste0(wd, "/Data/Axy/TDR_summary/")), pattern = '.csv'), read.csv))

head(msum)



table(is.na(msum$begdesc))

msum$begdesc<-ymd_hms(msum$begdesc)
msum$enddesc<-ymd_hms(msum$enddesc)
msum$begasc<-ymd_hms(msum$begasc)
msum$end_dive<-msum$begdesc + msum$divetim



####check for errors in the dives 

hist(msum$divetim)
subset(msum, divetim>100)
msum<-subset(msum, divetim<100)####check trips with super long dives... this would be issues with the filters 

mean(msum$divetim) ######1.49
sd(msum$divetim)#####2.3


mean(msum$maxdep)#####0.7847
sd(msum$maxdep)####N0.3612
table(msum$TagID)



#mean and sd number of dives per trip
xxx<-msum %>% 
  dplyr::group_by(TagID) %>%
  dplyr::summarize(n())

mean(xxx$'n()') ####26.275
sd(xxx$'n()') ###27.693




####Now bring this together with the all dataset based on the intervals! 

####so we want to loop this, first by matching with the trip ID and the iterval between one position and the next 

head(all)
head(msum)

####for now simplify dataset... 

msum<-select(msum, TagID,begdesc, divetim, maxdep, end_dive)

####add in closest gps timestamp that is before the begdesc. then summarize the number of dives and merge with to the all dataset


###merged with gps
###match with datatable. 

  msum2<-select(msum, begdesc, TagID)
  msum2$join_time <-msum2$begdesc
  
  all2<-select(all, dateTime_gps, TagID)
  all2$join_time <-all2$dateTime_gps
  
  setDT(msum2)
  setDT(all2)
  
  setkey(msum2,TagID, join_time)
  setkey(all2,TagID, join_time)
  
 matched<- all2[msum2, roll = T]
 ###SO THIS MATCHES EACH OF THE DIVES TO ONE GPS POSITION. 
  
  matched<-select(matched, -join_time)
  
  sum_matched<-matched %>%
    group_by(dateTime_gps, TagID)%>%
    summarise(TDR_dives = n()) %>%
    ungroup()
  
  unique(sum_matched$TagID)
  
  sum_matched<-sum_matched[with(sum_matched, order(TagID, dateTime_gps)),]
  
    

  
  ####now merge this with all
  
  
  head(all)
  
  ######not merging properly??? issues  with TagIDs OR DATEIMES
  all<-left_join(all, sum_matched, by=c("dateTime_gps", "TagID"))
  
  
  

  ####now lets set the 0 dives to when there were no dives detected and see how well these dives match up with those detected with axy! 

  
  
  
all$TDRdive01_new<-ifelse(all$TDR_dives>0, 1, 0)
  
  
all$TDR_dives<-ifelse(is.na(all$TDR_dives) , 0, all$TDR_dives)
all$TDRdive01_new<-ifelse(is.na(all$TDRdive01_new) , 0, all$TDRdive01_new)
  
  
  ####how much did this change 
  table(all$TDRdive01_new, all$dive_tdr_01)###comparing old tdr dives 
  table(all$TDRdive01_new, all$dive_01_acc)#### looks like the axy is detecting much more dives then the tdr
  
  

 
  