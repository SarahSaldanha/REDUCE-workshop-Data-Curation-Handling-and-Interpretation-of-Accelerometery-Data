# This script takes the files labelled in Framework4 or R, individually names each segment of behaviour and then match in to the accelerometer summaries
#This code was adapted from a script written by Beth Clark. The adaptations were made by Sarah Saldahna for the Seabird Group R class 2022
#If you have any questions please contact Sarah Saldanha (sarahsaldanha9@gmail.com)

#######################################################################################################################################################################################################################################################################



library(data.table)
library(lubridate)

#Setup ####
library(plotly)
library(tidyr)

#clear R
rm(list=ls()) 

#set folder

wd <- "C:/Users/sarah/Dropbox/PhD/RClass_2025/Data Curation, Handling, and Interpretation of Accelerometery Data/Day3/"

#Set the require segment length
seglength <- 50 #50 = 2 seconds for my 25 hz data

#Read in the file that you read in to Framework4 (as Framework4 messes up date format when it saves)
acc.data <- fread(paste0(wd,"Data/Axy/cut_axy/8200501_PHAAET_rec08042021_ICima_ninho_3_37_S1_2.csv"))
acc.data$index <- 1:nrow(acc.data)
head(acc.data)

#Read in the output file from Framework4

acc.labels <- fread(paste0(wd,"Data/Axy/Labelled_trips/8200501_PHAAET_rec08042021_ICima_ninho_3_37_S1_2.csv"))

head(acc.labels)


#For each file saved by framework4, you have to input the meaing of the behaviours
#This will reorder the behaviours after being saved and reloaded as a .bio file (framework4 project file)



#Set the variable to a character format
acc.labels$behaviour <- as.character(acc.labels$Behavior)
table(acc.labels$behaviour)

#To find out the correct labels, in Framework4, go to 
#File > Export Label Summary > Write Label Collection (Summary)
#The behaviours will appear in the correct order, starting at 0
#-1 means no behaviour was recorded
acc.data$behaviour <- plyr::revalue(acc.labels$behaviour, c("0"="land", "1"="ActiveLand", 
                                                      "2"="Takeoff", "3"="flying", 
                                                      "4"="preDive", "5"="Dive", "6" = "water", 
                                                      "-1"="none"))

table(acc.data$behaviour)

#Check it is correct

#Use plotly to make a graph you can zoom and scroll through
plot_ly(x = acc.data$index, 
        y = acc.data$X, 
        mode = 'scatter', 
        text = acc.data$index,
        color = as.factor(acc.data$behaviour))

#Update the labels if needed

###merge this with the 2 second segments of the summary statistics 


acc_sum <- fread(paste0(wd,"Data/Outputs/Axy_data_with_metrics/8200501_PHAAET_rec08042021_ICima_ninho 3_37_S1_axymetrics.csv"))


acc.data<-select(acc.data, Timestamp2, Timestamp, behaviour)
acc.data$Timestamp2<- ymd_hms(acc.data$Timestamp2)

acc_merged<-merge(acc.data, acc_sum, by.x = "Timestamp2", by.y ="start_seg", all.x = T)

###fill all the seconds
acc_merged<- fill(acc_merged, everything(), .direction = "down")
acc_merged<-subset(acc_merged, !is.na(TagID))
behaviour<-acc_merged%>%
  group_by(bird_seg, behaviour)%>%
  dplyr::tally() %>%
  ungroup()

behaviour$prop<-behaviour$n/50



behaviour<-behaviour %>% group_by(bird_seg) %>% dplyr::top_n(1, prop) %>% ungroup()


#####now match most important behaviour per segement


final<-merge(behaviour, acc_sum, by ="bird_seg")




head(final)


subset(final, prop>0.5)  %>%
  ggplot( aes(x=behaviour, y=sd_ODBA)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  )



subset(final, prop>0.5)  %>%
  ggplot( aes(x=behaviour, y=maxZ_heave)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  )

  
