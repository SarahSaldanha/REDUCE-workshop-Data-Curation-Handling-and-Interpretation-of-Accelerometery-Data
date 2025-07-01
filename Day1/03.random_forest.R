#now run random forest
rm(list=ls())
library(caret)
library(ranger)
library(data.table)
library(egg)
library(dplyr)

input_data <- paste0("C:/Users/xcarrj/Desktop/acc_course/output/training_metrics/")

setwd("C:/Users/xcarrj/Desktop/acc_course/")

freq<-rep(c(100,50,25),2) #sampling frequencies

smooth<-c(rep(c(100),3), rep(c(200),3)) #smoothing windows used

accuracy<-NA

forest<-as.data.frame(cbind(smooth,freq, accuracy))

for (f in 1:nrow(forest)){
  #import data
  input_file <- paste0(input_data, forest[f,2],"Hz/",forest[f,2],"hz","trainingmetrics",forest[f,1],"seglength.csv" )
  data<-read.csv(input_file)
  
  print(paste("runnning model","smooth",forest[f,1],"freq",forest[f,2]))
  
  #seperate ID and seg
  data<-data %>% separate(pango_seg, c('id', 'seg'))
  
  #remove seg
  data<-data[,!(names(data) %in% c("seg"))]

  #format behaviour names for RF
  data[data$behaviour== "u-turn",]$behaviour<-"uturn"
  data[data$behaviour== "swimming in place",]$behaviour<-"SIP"
  data[data$behaviour== "assisted resting",]$behaviour<-"still"
  data[data$behaviour== "locate food",]$behaviour<-"locatefood"
  #create dataframe to to summarise num. segs for each individual
  freq<-as.matrix.data.frame(table(data$behaviour,data$id))
  
  row_names <- rownames(table(data$behaviour,data$id))
  col_names <- colnames(table(data$behaviour,data$id))
  dimnames(freq) <- list(row_names, col_names)
  
  freqDF<-as.data.frame(freq)
  
  #how many individuals have less than 10 occurances?
  freqDF$count <- apply(freqDF, 1, function(x) length(which(x<4)))
  
  freqDF$behaviour <- rownames(freqDF)
  
  #now if behaviours have less than 4 occurances of 0/1 then remove
    data<-filter(data, !behaviour %in%  c(freqDF$behaviour[which(freqDF$count>4)]))
  
  data$behaviour<-as.factor(data$behaviour)
  data$id<-as.factor(data$id)
  
  data2<-data
  
  ##split data#. NOTE this will ensure balanced distributions of behaviour classes between the two datasets.
  #the final model will still test on the same individuals. If individuals are more of a concern than behaviour
  #split by individuals
  intrain <- createDataPartition(y = data2$behaviour, p = .70, list = FALSE) #split data 70% training
  training <- data2[intrain,]
  testing <- data2[-intrain,]

  
  #this function allows an individual based k-fold cross validation to be used during model training.
  foldCaret <- function(dat, nm = 10) {
    
    #--------------------------------------------------------
    # Create a folds index to keep cells together during cross-validation
    # The resulting list "folds" is passed to "index" in the "trainControl" function in caret
    # Keeps individuals together during splitting data for cross validation
    
    use.long <- dat
    nm <- nm
    
    if (length(unique(use.long$id)) < nm) {
      nm <- length(unique(use.long$id))
    }
    
    rws <- floor(length(unique(use.long$id))/nm) #number of cells to include in each fold
    cells <- unique(use.long$id)
    folds <- list()
    for (i in 1:nm) {
      samp <- sample(x = cells, size = rws)
      l <- list()
      for (j in 1:length(samp)) {
        k <- which(use.long$id == samp[j])
        l[j] <- list(k)
      }
      l <- unlist(l)
      folds[i] <- list(l)
      for (r in 1:length(samp)) {
        cells <- cells[cells != samp[r]]
      }
    }
    
    names(folds) <- paste0("fold_", c(1:nm)) #train needs list to be named
    
    return(folds)
    
  } #from Reisinger et al., 2021. https://doi.org/10.1111/ddi.13447
  
  folds <- foldCaret(dat = training, nm = 10) #nm=numfolds. NOTE if less than 10 individuals nm changes to num. individuals
  
  #remove ID from training dataset after folds
  training<-subset(training, select = -c(id))
  testing2<-subset(testing, select = -c(id))
  
  ######
  
  #upsampling for unequal group sizes https://topepo.github.io/caret/subsampling-for-class-imbalances.html
  tc <- trainControl(method = "cv",
                     number = length(folds),
                     index = folds,
                     search = "grid",classProbs = TRUE, summaryFunction = multiClassSummary, verboseIter=FALSE, sampling = "up")#was search=random, savePred=T
  
  
  #########find best mtry, predefined ntree##########################
  
  ## find the optimal mtry values. mtry can only be max num of variables putting nito the model
  
  rfGrid <-  data.frame(mtry = c(1:18),
                        splitrule = "gini",
                        min.node.size = c(1))
  
  ntree=1000
  
  set.seed(6)
  
  MOD.rf<-train(behaviour~., data=training, method="ranger", importance ='impurity',#or permutation. #or behaviour~.-id,  
                trControl = tc,tuneGrid = rfGrid, verbose=FALSE, num.trees=ntree)#,metric = "multiROC")
  
  #ggplot(MOD.rf)
  
  tmp <- MOD.rf$results
  tmp <- tmp[order(tmp$Accuracy, decreasing = T),] #normally AUC, here I have changed to accuracy because the sample dataset is too small for AUC
  tmp <- tmp[1,] # Keep only the best score
  
  best.mtry<-tmp[1,1]
  
  ##insert into final model here#
  
  #use best mtry in grid
  rfGrid <-  data.frame(mtry = best.mtry,
                        splitrule = "gini",
                        min.node.size = c(1))
  ntree=1000
  
  opt.rf<-train(behaviour~., data=training, method="ranger", importance ='impurity',#or permutation. #or behaviour~.-id,  
                trControl = tc,tuneGrid = rfGrid, verbose=FALSE, num.trees=ntree ) 
  
  #ggplot(MOD.rf)
  
  # Variable importance
  tmp <- varImp(opt.rf)$importance
  tmp$variable <- row.names(tmp)
  #plot(varImp(opt.rf))
  
  #now test final accuracy from tuned model on testing dataset
  accuracy<-predict(opt.rf, testing2) #, behaviour = "prob" give probability of specific behaviour
  confusion_matrix<-confusionMatrix(accuracy, testing$behaviour)
  
  CM_output1<-as.matrix(confusion_matrix,what="overall")
  CM_output2<-as.matrix(confusion_matrix, what = "classes")
  CM_output3<-as.matrix(confusion_matrix$table)
  
  final_accuracy <- confusion_matrix$overall[1]
  print(final_accuracy)
  forest[f,3]<-final_accuracy
  output_file <- paste0(input_data,"RF_results/",forest[f,2],"Hz","RF",forest[f,1], "smooth" )
  
  saveRDS(opt.rf,paste0(output_file,"_bestRF.RDS"))
  
  
  write.csv(CM_output1, paste0(output_file, "CM_output1.csv"))
  write.csv(CM_output2, paste0(output_file, "CM_output2.csv"))
  write.csv(CM_output3, paste0(output_file,"CM_output3.csv"))
  
  
}


write.csv(forest, paste0(output_file,"RF_accuracy_results.csv"))


forest$smooth<-as.factor(forest$smooth)
forest$accuracy<-as.numeric(forest$accuracy)
forest$freq<-as.factor(forest$freq)

ggplot(forest, aes(x=smooth, y=accuracy, group=freq)) +
  geom_line(aes(color=freq))+
  geom_point(aes(color=freq))+
  theme_article()
