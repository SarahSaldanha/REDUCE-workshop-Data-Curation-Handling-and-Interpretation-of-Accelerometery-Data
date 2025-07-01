###################################################################################################################################################################################################################################################################################################################In this script we cut the axy data to the dateTimes of the gps trips (removing axy data on land), we also calculate a the pitch and roll and an estimated depth (this is not precise so it should only be used for visualization)

#################################################################################################################################################################################################################################################################################################################


library(dplyr)          
library(tidyr)          
library(stringr)
library(lubridate)
library(data.table)
library(zoo)

op <- options(digits.secs=6)

memory.limit(size=5000000000)

WD <- "C:/Users/sarah/Dropbox/PhD/RClass_2025/Data Curation, Handling, and Interpretation of Accelerometery Data/Day3/Data"

setwd(WD)

###bring in bird data
bird<-read.csv("Bird/bird_info_axys.csv")
bird<-dplyr::select(bird, ring = BirdId, Sex, Colony, date_deployment,GMT_hour_dep= GMT_Hour, date_recovery, GMT_Hour_recover, Breed.Stage, HZ)
bird$dep_dt<-dmy_hms(paste0(bird$date_deployment, " ", bird$GMT_hour_dep))
bird$rec_dt<-dmy_hms(paste0(bird$date_recovery, " ", bird$GMT_Hour_recover))
bird<-dplyr::select(bird, ring, Sex, dep_dt, rec_dt, Breed.Stage, HZ)
#bird<-subset(bird, HZ ==25 )
bird$ring<-as.character(bird$ring)
bird<-dplyr::select(bird, ring, dep_dt, rec_dt)
####bring in GPS cut data. 

masterBirds <- readRDS("Cut_GPS/extractedTrips_allBirds_withContBlocks_Final_April2021.rds")
masterBirds$dateTime<-ymd_hms(masterBirds$dateTime)
masterBirds <- masterBirds[order(masterBirds$refID, masterBirds$tripID2, masterBirds$dateTime), ]
head(masterBirds)

###create a fucntion to calculate the mode for the visual of depth
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


###set your working directory


PHAAET_files <- list.files(path = paste0 (WD, "/Axy/raw_axy/")  ,pattern = '.csv')
print(PHAAET_files)





axy_PHAAET_files <- function(file_name){
# First import the data from the file, for data with both ";" and "," separators

file_name <-PHAAET_files[1]
file.error<- try(fread(paste0 (WD, "/Axy/raw_axy/", file_name), header=T, sep=","), silent= T)
if(length(file.error)==1){axy_data<-fread( paste0 (WD, "/Axy/raw_axy/", file_name), header=T, sep=";")} else {axy_data<-fread(paste0 (WD, "/Axy/raw_axy/", file_name), header=T, sep=",")} #####bring in one file at a time whether it has a seperation of , or ; 
colnames(axy_data) <- c("TagID", "Timestamp", "X", "Y","Z","Activity","Pressure","Temperature","lat","lon", "height.above.msl", "ground.speed", "satellite.count", "hdop", "maximum.signal.strength") ####rename columns
axy_data$Timestamp <- dmy_hms(axy_data$Timestamp)
axy_data$date <- as.Date(axy_data$Timestamp, taxy_data = "GMT")  #### fix the dates


####since there are some errors in the TagID variable, I am creating here a TrackID variable so that I can take the ring and recovery information that is saved in the file names.

# Remove file extension - so we just have the main file name, not the .CSV extension.
file_name_only <- strsplit(file_name,".csv")
#Make this into a unit length vector
TrackId <- unlist(file_name_only)
axy_data <- cbind(TrackId, axy_data)
TID<-unlist(strsplit(axy_data$TrackId, "_"))
axy_data$ring <- TID[1]


###extract recovery date

axy_data$rec_d<-dmy(paste0(str_sub(TID[3], 4, 5), "-", str_sub(TID[3], 6, 7),"-", str_sub(TID[3], 8, 11)))



#####now trim trip to deployment and recovery of GPS

###create dept_dt and rec_dt

axy_data$dep_dt<-subset(bird, ring == unique(axy_data$ring) & ymd(str_sub(rec_dt, 1,10)) == unique(axy_data$rec_d))$dep_dt

axy_data$rec_dt<-subset(bird, ring == unique(axy_data$ring) & ymd(str_sub(rec_dt, 1,10)) == unique(axy_data$rec_d))$rec_dt


axy_data<-subset(axy_data, Timestamp > dep_dt & Timestamp < rec_dt)

###Check units and convert to g if needed. I had one trip that I need to fix by dividing it by 1000 so I added this ifelse statement to the loop. took 20 as the cutoff quite randomly, should be AROUND 0 for x and y and around 1 for Z 
######

axy_data$mean_x<- mean(axy_data$X)

axy_data$X <- ifelse(axy_data$mean_x>100,  axy_data$X/1000, axy_data$X)
axy_data$Y <- ifelse(axy_data$mean_x>100,  axy_data$Y/1000, axy_data$Y)
axy_data$Z <- ifelse(axy_data$mean_x>100,  axy_data$Z/1000, axy_data$Z)

axy_data<- dplyr::select(axy_data, -mean_x,  -TagID, -rec_d)

#Calculate pitch & roll
axy_data$pitch<-atan((axy_data$X/(sqrt((axy_data$Y*axy_data$Y)+(axy_data$Z*axy_data$Z)))))*(180/pi)#Calculate pitch
axy_data$roll<-atan((axy_data$Y/(sqrt((axy_data$X*axy_data$X)+(axy_data$Z*axy_data$Z)))))*(180/pi) #Calculate roll

####Calculate depth and spread pressure
axy_data$Pressure<- na.approx(axy_data$Pressure, rule = 2) ###interpolate  pressure to be every axy recording. rule = 2 to get na.approx to always fill in the blanks
axy_data$Approx_depth_negative_mode<-0.01*(getmode(round(axy_data$Pressure))-axy_data$Pressure)


####GET MINIMUM AND maximum time in each foraging trip to the floored second
####here we are spreading the GPS tracking data to every second so that we can cut out periods on land between trips. (The GPS TRIPS HAVE ALREADY BEEN CUT!!!)
mb_2<-subset(masterBirds, ring == unique(axy_data$ring) ) %>%
  group_by(ring, refID, tripID) %>%
  dplyr::summarize(min_dt= min(dateTime), 
            max_dt= max(dateTime))

vec <- dmy_hms(vector())####create vector with all seconds within each count block

for(i in 1:nrow(mb_2)) {
  print(i)
  trial<-seq(mb_2$min_dt[i], mb_2$max_dt[i], "1 sec")
  vec<-c(vec, trial) 
  rm(trial)
  
}

axy_data$Timestamp2<-floor_date(axy_data$Timestamp, "1s")


axy_cut<-subset(axy_data, Timestamp2 %in% vec )
rm(axy_data)

###turn vector into a dataframe
vec2<-data.frame(vec) %>%
  dplyr::select(Timestamp2 = vec)




vec3<-merge(vec2, mb_2, by.x = "Timestamp2", by.y = "min_dt", all.x = T) 

#str(vec3)
#str(axy_cut$Timestamp2)

vec3 <- vec3 %>% fill(names(vec3), .direction = "down")


str(vec3)

axy_cut<-merge(vec3, axy_cut, by = c("Timestamp2", "ring"))



      
axy_cut <- split(axy_cut, axy_cut$tripID)
  
# use numbers as file names
lapply(names(axy_cut),
       function(x){write.csv(axy_cut[[x]], paste0(WD, "/Axy/cut_axy/", x ,".csv"),
                             row.names = FALSE)}) 


}


lapply(PHAAET_files, axy_PHAAET_files)



