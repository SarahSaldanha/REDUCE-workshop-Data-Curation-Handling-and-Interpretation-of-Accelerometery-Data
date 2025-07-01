#2.1 . Combine all labelled data. 
#2.2. Sample into Different rolling means
#2.3. Resample each rolling mean at different frequencies
#2.4. Calculate training metrics for each dataset


################################################################################
########################2.1 . Combine all labelled data.########################
################################################################################
rm(list=ls())
library(zoo)
library(tidyverse)

setwd("C:/Users/xcarrj/Desktop/acc_course/")
output_data <- paste0("C:/Users/xcarrj/Desktop/acc_course/output/")

#folder where labelled data is stored
files <- list.files("C:/Users/xcarrj/Desktop/acc_course/input/labelled_data", full.names = TRUE)

#read in all labelled acc data filed
tables <- lapply(files, read.csv, header = TRUE)

#bind together
data_full <- do.call(rbind , tables)

#remove uneccesary columns
data_short<-data_full[,c(2,3,4,8,9,11)]

acc.data<-data_short

#change column name
colnames(acc.data)[colnames(acc.data) == "Behavior"] ="behaviour"

#check how many entires per individual
table(acc.data$id)

#combine all
output_file<- paste0(output_data, "acc.data_IDscombined.csv")
write.csv(acc.data, output_file)


#Label each behavioural segment by when the behaviour changes ####
acc.data<-acc.data %>%
  mutate(segID = cumsum(behaviour != lag(behaviour) | row_number() == 1))

#see how many entries 
table(acc.data$segID)


output_file<- paste0(output_data, "acc.data_seglabs.csv")
write.csv(acc.data, output_file)



################################################################################
#######################2.2. Sample into Different rolling means#################
################################################################################

rm(list=ls())
library(zoo)
library(tidyverse)
graphics.off()
output_data <- paste0("C:/Users/xcarrj/Desktop/acc_course/output/")
my.data<-read.csv("C:/Users/xcarrj/Desktop/acc_course/output/acc.data_seglabs.csv")
table(my.data$segID)
Hz<-100

#1 and 2 seconds for 100 Hz data
rolling100hz<-c(100,200)

#loop through to get metrics of different segment lengths
rolling<-rolling100hz
############################################################
#this section of code will trim segments to specific length#
#Dr. Daire Carroll. Daire.carroll@bioenv.gu.se
############################################################
for(r in 1:length(rolling)){
  #################parms begin###############
  attach(my.data)
  bl_len = rolling[r]
  
  #################parms end###############
  
  #################split data begin##############
  
  lab1 = unique(segID)
  
  empty = data.frame(matrix(nrow = 0, ncol = (ncol(my.data)+1)))
  names(empty) = c(names(my.data),"Block_ID")
  Block_ID_v = 1
  
  
  
  for(i in 1:length(lab1)){
    
    seg = my.data[which(my.data$segID == lab1[i]),]
    
    if(nrow(seg)>bl_len){
      
      trm =  floor(nrow(seg)/bl_len)*bl_len
      seg = seg[1:trm,]
      
      Block_ID = rep(NA,nrow(seg))
      
      blks = nrow(seg)/bl_len  
      
      for(j in 1:blks){
        
        Block_ID[(1 + bl_len*(j-1)):(bl_len+bl_len*(j-1))] = Block_ID_v
        Block_ID_v = Block_ID_v + 1
        print(paste("block",Block_ID_v-1,seg[1,]$id,seg[1,]$behaviour,sep = " "))
        
      }
      
      seg = cbind(seg,Block_ID)
      
      empty = rbind(empty,seg)
      
    }
    
    
  }  
  
  #################split data end##############
  
  ############test begin#################
  
  blocks = unique(empty$Block_ID)
  
  hold = c()
  for(i in 1:length(blocks)){
    
    print(length(
      which(
        empty$Block_ID == blocks[i]
      )
    ))
    hold[i] = length(which(empty$Block_ID == blocks[i]))
    
  }
  
  unique(hold)
  
  ############test end#################

  write.csv(empty, paste0(output_data, "training_metrics/100hz/100hzsegmented_accData",bl_len, "seglength_loggers.csv"))
  
  detach(my.data)
  
}



################################################################################
##################2.3.Resample each rolling mean at different frequencies#######
################################################################################
rm(list=ls())
library(zoo)
library(tidyverse)

output_data <- paste0("C:/Users/xcarrj/Desktop/acc_course/output/")


full_freq<-c(100,200)

for(s in 1:length(full_freq)){
  
  input_file <- paste0("C:/Users/xcarrj/Desktop/acc_course/output/training_metrics/100hz/100hzsegmented_accData", full_freq[s],"seglength_loggers.csv")
  
  my.data<-read.csv(input_file)
  
  #add row nums to check
  my.data$rownum<-1:nrow(my.data)
  #format datetime to arrange by datetime just in case
  my.data$time2<-as.POSIXct(my.data$time, "%Y-%m-%d %H:%M:%OS",tz = "UTC")
  
  # #resample
  slice_50<-my.data %>%
    group_by(Block_ID) %>%
    arrange(time2) %>%
    slice(seq(1, n(), by =2))
  
  write.csv(slice_50,paste0(output_data,"training_metrics/50hz/resamp_50hz_", full_freq[s],"_seglength_loggers.csv"))
  
  slice_25<-slice_50 %>%
    group_by(Block_ID) %>%
    arrange(time2) %>%
    slice(seq(1, n(), by =2))
  
  write.csv(slice_25,paste0(output_data,"training_metrics/25hz/resamp_25hz_", full_freq[s],"_seglength_loggers.csv"))
 
}



################################################################################
###################2.4. Calculate training metrics for each dataset#############
################################################################################

rm(list=ls())
library(zoo)
library(tidyverse)

files<-paste0("C:/Users/xcarrj/Desktop/acc_course/output/training_metrics/")


#create dataframe with all parameters in to loop through

hz_rp<-rep(c(100,50,25),2) #sampling frequencies

smooth_wind<-c(rep(c(100),3), rep(c(200),3)) #smoothing windows used

met_df<-as.data.frame(cbind(hz_rp,smooth_wind)) #create ref. dataframe

# now extract metrics
for (m in 1:nrow(met_df)){
  
  #read input data
  if(met_df$hz_rp[m]==100){
    input_file <- paste0(files,"100hz/100hzsegmented_accData", met_df[m,2],"seglength_loggers.csv")
  }else{
    input_file <- paste0(files,met_df[m,1], "hz","/resamp_", met_df[m,1], "hz_", met_df[m,2],"_", "seglength_loggers.csv")
  }
  
  data<-read.csv(input_file)
  
  print(paste("calculating metrics","smooth",met_df[m,2],"freq",met_df[m,1]))
  
  Hz<-met_df[m,1]
  
  runmean<-met_df[m,2]
  
  #Set the require segment length
  
  freq <- Hz# The Frequency of accelerometry data (Hz)
  secs <- runmean/100 # The number of seconds over which to calculate the desired metrics.
  numrows <- freq*secs # The number of rows required to calculate metrics over the chosen period. 
  
  train<-data
  
  
  #adapted from (Clark, 2019; Clark et al., 2022). https://ore.exeter.ac.uk/repository/handle/10871/120152, https://www.int-res.com/abstracts/meps/v701/p145-157/
  
  train$pango_seg <- paste(train$id,train$Block_ID,sep="_") #If there are multiple individuals

  #Calculate metrics ####
  
  #Calculate pitch & roll
  train$pitch <- atan((train$X/(sqrt((train$Y*train$Y)+(train$Z*train$Z)))))*(180/pi)
  train$roll <- atan((train$Y/(sqrt((train$X*train$X)+(train$Z*train$Z)))))*(180/pi) 
  
  #Calculate the metrics for each segment
  pango_seg <- unique(train$pango_seg)
  train_metrics <- as.data.frame(pango_seg)

  #Create blank columns so that NA will appear if the calculations fail If this is not done, 
  #the value for the first iteration of the loop will appear when calculation fail, which'll be wrong
  #Loop will calculate:
  train_metrics$ODBA <- NA
  train_metrics$VeDBA <- NA
  
  train_metrics$meanX_surge <- NA
  train_metrics$stdevX_surge <-NA
  train_metrics$minX_surge <- NA
  train_metrics$maxX_surge <- NA
  
  train_metrics$pitch_mean <- NA
  train_metrics$pitch_stdev <- NA
  train_metrics$roll_mean <- NA
  train_metrics$roll_stdev <- NA
  #train_metrics$yaw_mean <- NA
  #train_metrics$yaw_stdev <- NA
  
  
  train_metrics$meanY_sway <- NA
  train_metrics$stdevY_sway <-NA
  train_metrics$minY_sway <- NA
  train_metrics$maxY_sway <- NA
  
  train_metrics$meanZ_heave <- NA
  train_metrics$stdevZ_heave <-NA
  train_metrics$minZ_heave <- NA
  train_metrics$maxZ_heave <- NA
  
  head(train_metrics)
  
  
  #Loop through each segment
  #This may take a few hours depending on size of data
  for (i in 1:nrow(train_metrics)){
    
    #Extract segment
    seg <- subset(train,pango_seg == train_metrics$pango_seg[i])
    
    #####Calculate ODBA and VeDBA####
    
    #First calculate the static g force
    StaticX <- sum(zoo::rollapply(seg$X,numrows,mean,fill=NA),na.rm = T)
    StaticY <- sum(zoo::rollapply(seg$Y,numrows,mean,fill=NA),na.rm = T)
    StaticZ <- sum(zoo::rollapply(seg$Z,numrows,mean,fill=NA),na.rm = T)
    
    #together should = ~1
    #StaticX + StaticY + StaticZ
    
    ####Calculates DBA for each axis. 
    seg$DynamicX <- seg$X-StaticX
    seg$DynamicY <- seg$Y-StaticY
    seg$DynamicZ <- seg$Z-StaticZ
    
    #Combines the DBA into ODBA or VeDBA
    seg$ODBA  <- abs(seg$DynamicX)+abs(seg$DynamicY)+abs(seg$DynamicZ)
    seg$VeDBA <- sqrt((seg$DynamicX^2)+(seg$DynamicY^2)+(seg$DynamicZ^2))
    
    #Store the mean OBDA/VeDBA for the segment
    train_metrics$ODBA[i]  <- mean(seg$ODBA)
    train_metrics$VeDBA[i] <- mean(seg$VeDBA)
    
    #Calculate and store trhe other metrics
    train_metrics$meanX_surge[i] <- mean(seg$X)
    train_metrics$stdevX_surge[i] <- sd(seg$X)
    train_metrics$minX_surge[i] <- min(seg$X)
    train_metrics$maxX_surge[i] <- max(seg$X)
    
    train_metrics$meanY_sway[i] <- mean(seg$Y)
    train_metrics$stdevY_sway[i] <- sd(seg$Y)
    train_metrics$minY_sway[i] <- min(seg$Y)
    train_metrics$maxY_sway[i] <- max(seg$Y)
    
    train_metrics$meanZ_heave[i] <- mean(seg$Z)
    train_metrics$stdevZ_heave[i] <- sd(seg$Z)
    train_metrics$minZ_heave[i] <- min(seg$Z)
    train_metrics$maxZ_heave[i] <- max(seg$Z)
    
    train_metrics$pitch_mean[i] <- mean(seg$pitch)
    train_metrics$pitch_stdev[i] <- sd(seg$pitch)
    train_metrics$roll_mean[i] <- mean(seg$roll)
    train_metrics$roll_stdev[i] <- sd(seg$roll)
    
  }
  
  #Remove any NAs
  train_metrics <- train_metrics[complete.cases(train_metrics),]
  
  #Match to add the behaviour label
  train_metrics$behaviour <- train$behaviour[match(train_metrics$pango_seg, train$pango_seg)] 

  write.csv(train_metrics, paste0(files,met_df[m,1],"hz/", met_df[m,1], "hztrainingmetrics", met_df[m,2], "seglength.csv"), row.names = F)
  
}