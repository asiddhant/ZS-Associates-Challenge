#Set the working directory to the directory containing the input 4 files
setwd("C:/Users/ADITYA/Desktop/Overall Code/Codes and Data")

#Install the following packages if they are not already installed. if some dependencies
#are not installed, please mail me @siddhantaditya01@gmail.com

# install.packages("caret")
# install.packages("xgboost")
# install.packages("randomForest")
# install.packages("gbm")
# install.packages("doParallel")
# install.packages("gdata")


hosprof=read.csv("HospitalProfiling.csv",stringsAsFactors = FALSE)
hosrev=read.csv("HospitalRevenue.csv",stringsAsFactors = FALSE)
projrev=read.csv("ProjectedRevenue.csv",stringsAsFactors = FALSE)
solution=read.csv("Solution.csv",stringsAsFactors = FALSE)

## Data Cleaning

#Remove Duplicate Entries : Taking the minimum
hosprof$Unique_ID=paste(hosprof$Hospital_ID,hosprof$District_ID,sep="_")
hosprof=hosprof[order(hosprof$Hospital_employees),]
hosprof=hosprof[!duplicated(hosprof$Unique_ID),]
hosprof$Hospital_employees=ifelse(hosprof$Hospital_employees>5000,5000,hosprof$Hospital_employees)
hosprof$Unique_ID=NULL
hosprof=hosprof[order(hosprof$Hospital_ID),]

solution$Hosp_Dist=paste(solution$Hospital_ID,solution$District_ID,sep="_")
hosrev$Hosp_Dist=paste(hosrev$Hospital_ID,hosrev$District_ID,sep="_")
hosprof$Hosp_Dist=paste(hosprof$Hospital_ID,hosprof$District_ID,sep="_")
projrev$Hosp_Dist=paste(projrev$Hospital_ID,projrev$District_ID,sep="_")

Hosp_Dist=unique(c(solution$Hosp_Dist,hosrev$Hosp_Dist,hosprof$Hosp_Dist,projrev$Hosp_Dist))

dataset=matrix(NA,nrow=length(Hosp_Dist)*15,ncol=3)
for (i in 1:length(Hosp_Dist)){
  start=(i-1)*15 + 1
  end=i*15
  dataset[start:end,1:2]=matrix(rep(unlist(strsplit(Hosp_Dist[i],"_")),15),nrow=15,byrow = TRUE)
  dataset[start:end,3]=paste("Instrument",c(1:8,10,11,13,15,18:20),sep=" ")
  print(i)
}
rm(start,end,i)

dataset=as.data.frame(dataset)
names(dataset)=names(solution)[1:3]

dataset$Unique_ID=paste(dataset$Hospital_ID,dataset$District_ID,dataset$Instrument_ID,sep="_")
solution$Unique_ID=paste(solution$Hospital_ID,solution$District_ID,solution$Instrument_ID,sep = "_")
hosrev$Unique_ID=paste(hosrev$Hospital_ID,hosrev$District_ID,hosrev$Instrument_ID,sep="_")
projrev$Unique_ID=paste(projrev$Hospital_ID,projrev$District_ID,projrev$Instrument_ID,sep="_")

dataset=dataset[!dataset$Unique_ID %in% solution$Unique_ID,]

dataset$Buy_or_not=0
for(i in 1:nrow(dataset)){
  if(dataset$Unique_ID[i] %in% hosrev$Unique_ID){
    if(hosrev$Year.Total[hosrev$Unique_ID==dataset$Unique_ID[i]]>0){
      dataset$Buy_or_not[i]=1
    }
  } 
  if(dataset$Unique_ID[i] %in% projrev$Unique_ID){
    dataset$Buy_or_not[i]=1
  }
  print(i)
}

dataset$Hosp_Dist=paste(dataset$Hospital_ID,dataset$District_ID,sep="_")
hospstats=as.data.frame.table(tapply(hosprof$Hospital_employees,hosprof$Hospital_ID,mean))
diststats=as.data.frame.table(tapply(hosprof$Hospital_employees,hosprof$District_ID,mean))

dataset2=merge(dataset,hosprof[,3:4],by="Hosp_Dist",all.x = TRUE)
dataset$NumEmployee=dataset2$Hospital_employees
rm(dataset2)
solution2=merge(solution,hosprof[,3:4],by="Hosp_Dist",all.x=TRUE)
solution$NumEmployee=solution2$Hospital_employees
rm(solution2)
library(randomForest)

dataset$NumEmployee=na.roughfix(dataset$NumEmployee)
solution$NumEmployee=na.roughfix(solution$NumEmployee)

solution$Hospital_ID=as.factor(solution$Hospital_ID)
solution$District_ID=as.factor(solution$District_ID)
solution$Instrument_ID=as.factor(solution$Instrument_ID)

dataset$Hospital_ID=as.factor(dataset$Hospital_ID)
dataset$District_ID=as.factor(dataset$District_ID)
dataset$Instrument_ID=as.factor(dataset$Instrument_ID)

dataset$Buy_or_not=ifelse(dataset$Buy_or_not==1,"Pos","Neg")
dataset$Buy_or_not=as.factor(dataset$Buy_or_not)

regdataset=subset(dataset,dataset$Buy_or_not=="Pos")
regdataset$Amount=0
for ( i in 1:nrow(regdataset)){
  if(regdataset$Unique_ID[i] %in% hosrev$Unique_ID){
    regdataset$Amount[i]=hosrev$Year.Total[hosrev$Unique_ID==regdataset$Unique_ID[i]]
  }
  if(regdataset$Unique_ID[i] %in% projrev$Unique_ID){
    regdataset$Amount[i]=projrev$Annual_Projected_Revenue[projrev$Unique_ID==regdataset$Unique_ID[i]]
  }
  print(i)
}
rm(i)

dataset_big=dataset
dataset=subset(dataset,(dataset$Hosp_Dist %in% hosrev$Hosp_Dist | dataset$Hosp_Dist %in% projrev$Hosp_Dist))


##Clustering
hospstats=as.data.frame.table(tapply(hosprof$Hospital_employees,hosprof$Hospital_ID,mean))
names(hospstats)=c("Hospital_ID","Mean_Patients")
dataset_big_pos=subset(dataset_big,dataset_big$Buy_or_not=="Pos")
hospstats2=as.matrix(table(dataset_big_pos$Hospital_ID,dataset_big_pos$Instrument_ID))
hospstats2=as.data.frame.matrix(hospstats2)
hospstats2$Hospital_ID=row.names(hospstats2)
hospstats2=merge(hospstats,hospstats2,by= "Hospital_ID",all=TRUE)
hospstatsclus=hospstats2[,3:17]
hospstatsclusdist=dist(hospstatsclus,method = "euclidean")
hospstatsclustersmodel=hclust(hospstatsclusdist,method="ward.D")
hospstatsclusters=cutree(hospstatsclustersmodel,k=10)
hospstats2$Cluster1=as.factor(hospstatsclusters)
dataset=merge(dataset,hospstats2[,c("Hospital_ID","Cluster1")],by="Hospital_ID",all.x=TRUE)
dataset_big=merge(dataset_big,hospstats2[,c("Hospital_ID","Cluster1")],by="Hospital_ID",all.x=TRUE)
rm(dataset_big_pos,hospstats,hospstatsclusters,hospstatsclustersmodel,
   hospstatsclus,hospstatsclusdist)
solution=merge(solution,hospstats2[,c("Hospital_ID","Cluster1")],by="Hospital_ID",all.x=TRUE)

## Hospital Clustering based on monthly revenue (2)
hosrevagg=aggregate(hosrev[5:17],by=list(hosrev$Hospital_ID),sum)
names(hosrevagg)[1]="Hospital_ID"
hospstatsclusdist=dist(hosrevagg[,2:13],method = "euclidean")
hospstatsclustersmodel=hclust(hospstatsclusdist,method="ward.D")
hospstatsclusters=cutree(hospstatsclustersmodel,k=10)
hosrevagg$Cluster2=as.factor(hospstatsclusters)
dataset=merge(dataset,hosrevagg[,c("Hospital_ID","Cluster2")],by="Hospital_ID",
              all.x=TRUE)
dataset_big=merge(dataset_big,hosrevagg[,c("Hospital_ID","Cluster2")],by="Hospital_ID",
                  all.x=TRUE)
rm(hospstatsclusters,hospstatsclustersmodel,hospstatsclusdist)
solution=merge(solution,hosrevagg[,c("Hospital_ID","Cluster2")],by="Hospital_ID",
               all.x=TRUE)


##Feature_Eng

hospitalrevmean=aggregate(regdataset$Amount,by=list(regdataset$Hospital_ID),mean)
names(hospitalrevmean)=c("Hospital_ID","HospitalRevenueMean")
hospitalrevmedian=aggregate(regdataset$Amount,by=list(regdataset$Hospital_ID),median)
names(hospitalrevmedian)=c("Hospital_ID","HospitalRevenueMeadian")
regdataset=merge(regdataset,hospitalrevmean,by="Hospital_ID",all.x = TRUE)
regdataset=merge(regdataset,hospitalrevmedian,by="Hospital_ID",all.x = TRUE)
dataset=merge(dataset,hospitalrevmean,by="Hospital_ID",all.x = TRUE)
dataset=merge(dataset,hospitalrevmedian,by="Hospital_ID",all.x = TRUE)
dataset_big=merge(dataset_big,hospitalrevmean,by="Hospital_ID",all.x = TRUE)
dataset_big=merge(dataset_big,hospitalrevmedian,by="Hospital_ID",all.x = TRUE)
solution=merge(solution,hospitalrevmean,by="Hospital_ID",all.x = TRUE)
solution=merge(solution,hospitalrevmedian,by="Hospital_ID",all.x = TRUE)

districtrevmean=aggregate(regdataset$Amount,by=list(regdataset$District_ID),mean)
names(districtrevmean)=c("District_ID","DistrictRevenueMean")
districtrevmedian=aggregate(regdataset$Amount,by=list(regdataset$District_ID),median)
names(districtrevmedian)=c("District_ID","DistrictRevenueMeadian")
regdataset=merge(regdataset,districtrevmean,by="District_ID",all.x = TRUE)
regdataset=merge(regdataset,districtrevmedian,by="District_ID",all.x = TRUE)
dataset=merge(dataset,districtrevmean,by="District_ID",all.x = TRUE)
dataset=merge(dataset,districtrevmedian,by="District_ID",all.x = TRUE)
dataset_big=merge(dataset_big,districtrevmean,by="District_ID",all.x = TRUE)
dataset_big=merge(dataset_big,districtrevmedian,by="District_ID",all.x = TRUE)
solution=merge(solution,districtrevmean,by="District_ID",all.x = TRUE)
solution=merge(solution,districtrevmedian,by="District_ID",all.x = TRUE)

instrumentrevmean=aggregate(regdataset$Amount,by=list(regdataset$Instrument_ID),mean)
names(instrumentrevmean)=c("Instrument_ID","InstrumentRevenueMean")
instrumentrevmedian=aggregate(regdataset$Amount,by=list(regdataset$Instrument_ID),median)
names(instrumentrevmedian)=c("Instrument_ID","InstrumentRevenueMeadian")
regdataset=merge(regdataset,instrumentrevmean,by="Instrument_ID",all.x = TRUE)
regdataset=merge(regdataset,instrumentrevmedian,by="Instrument_ID",all.x = TRUE)
dataset=merge(dataset,instrumentrevmean,by="Instrument_ID",all.x = TRUE)
dataset=merge(dataset,instrumentrevmedian,by="Instrument_ID",all.x = TRUE)
dataset_big=merge(dataset_big,instrumentrevmean,by="Instrument_ID",all.x = TRUE)
dataset_big=merge(dataset_big,instrumentrevmedian,by="Instrument_ID",all.x = TRUE)
solution=merge(solution,instrumentrevmean,by="Instrument_ID",all.x = TRUE)
solution=merge(solution,instrumentrevmedian,by="Instrument_ID",all.x = TRUE)

regdataset$Dist_Inst=paste(regdataset$District_ID,regdataset$Instrument_ID,sep="_")
dataset$Dist_Inst=paste(dataset$District_ID,dataset$Instrument_ID,sep="_")
dataset_big$Dist_Inst=paste(dataset_big$District_ID,dataset_big$Instrument_ID,sep="_")
solution$Dist_Inst=paste(solution$District_ID,solution$Instrument_ID,sep="_")

regdataset$Hosp_Inst=paste(regdataset$Hospital_ID,regdataset$Instrument_ID,sep="_")
dataset$Hosp_Inst=paste(dataset$Hospital_ID,dataset$Instrument_ID,sep="_")
dataset_big$Hosp_Inst=paste(dataset_big$Hospital_ID,dataset_big$Instrument_ID,sep="_")
solution$Hosp_Inst=paste(solution$Hospital_ID,solution$Instrument_ID,sep="_")

hospdistrevmean=aggregate(regdataset$Amount,by=list(regdataset$Hosp_Dist),mean)
names(hospdistrevmean)=c("Hosp_Dist","HospDistRevenueMean")
hospdistrevmedian=aggregate(regdataset$Amount,by=list(regdataset$Hosp_Dist),median)
names(hospdistrevmedian)=c("Hosp_Dist","HospDistRevenueMeadian")
regdataset=merge(regdataset,hospdistrevmean,by="Hosp_Dist",all.x = TRUE)
regdataset=merge(regdataset,hospdistrevmedian,by="Hosp_Dist",all.x = TRUE)
dataset=merge(dataset,hospdistrevmean,by="Hosp_Dist",all.x = TRUE)
dataset=merge(dataset,hospdistrevmedian,by="Hosp_Dist",all.x = TRUE)
dataset_big=merge(dataset_big,hospdistrevmean,by="Hosp_Dist",all.x = TRUE)
dataset_big=merge(dataset_big,hospdistrevmedian,by="Hosp_Dist",all.x = TRUE)
solution=merge(solution,hospdistrevmean,by="Hosp_Dist",all.x = TRUE)
solution=merge(solution,hospdistrevmedian,by="Hosp_Dist",all.x = TRUE)

distinstrevmean=aggregate(regdataset$Amount,by=list(regdataset$Dist_Inst),mean)
names(distinstrevmean)=c("Dist_Inst","DistInstRevenueMean")
distinstrevmedian=aggregate(regdataset$Amount,by=list(regdataset$Dist_Inst),median)
names(distinstrevmedian)=c("Dist_Inst","DistInstRevenueMeadian")
regdataset=merge(regdataset,distinstrevmean,by="Dist_Inst",all.x = TRUE)
regdataset=merge(regdataset,distinstrevmedian,by="Dist_Inst",all.x = TRUE)
dataset=merge(dataset,distinstrevmean,by="Dist_Inst",all.x = TRUE)
dataset=merge(dataset,distinstrevmedian,by="Dist_Inst",all.x = TRUE)
dataset_big=merge(dataset_big,distinstrevmean,by="Dist_Inst",all.x = TRUE)
dataset_big=merge(dataset_big,distinstrevmedian,by="Dist_Inst",all.x = TRUE)
solution=merge(solution,distinstrevmean,by="Dist_Inst",all.x = TRUE)
solution=merge(solution,distinstrevmedian,by="Dist_Inst",all.x = TRUE)

hospinstrevmean=aggregate(regdataset$Amount,by=list(regdataset$Hosp_Inst),mean)
names(hospinstrevmean)=c("Hosp_Inst","HospInstRevenueMean")
hospinstrevmedian=aggregate(regdataset$Amount,by=list(regdataset$Hosp_Inst),median)
names(hospinstrevmedian)=c("Hosp_Inst","HospInstRevenueMeadian")
regdataset=merge(regdataset,hospinstrevmean,by="Hosp_Inst",all.x = TRUE)
regdataset=merge(regdataset,hospinstrevmedian,by="Hosp_Inst",all.x = TRUE)
dataset=merge(dataset,hospinstrevmean,by="Hosp_Inst",all.x = TRUE)
dataset=merge(dataset,hospinstrevmedian,by="Hosp_Inst",all.x = TRUE)
dataset_big=merge(dataset_big,hospinstrevmean,by="Hosp_Inst",all.x = TRUE)
dataset_big=merge(dataset_big,hospinstrevmedian,by="Hosp_Inst",all.x = TRUE)
solution=merge(solution,hospinstrevmean,by="Hosp_Inst",all.x = TRUE)
solution=merge(solution,hospinstrevmedian,by="Hosp_Inst",all.x = TRUE)


hospstats2$NumInstruments=apply(hospstats2[,3:17],1,sum)
names(hospstats2)[c(3:17,19)]=paste("Hosp",names(hospstats2)[c(3:17,19)])
dataset=merge(dataset,hospstats2[,c(1,3:17,19)],by="Hospital_ID",
              all.x=TRUE)
dataset_big=merge(dataset_big,hospstats2[,c(1,3:17,19)],by="Hospital_ID",
                  all.x=TRUE)
solution=merge(solution,hospstats2[,c(1,3:17,19)],by="Hospital_ID",
               all.x=TRUE)


dataset_big_pos=subset(dataset_big,dataset_big$Buy_or_not=="Pos")
avgnohospdistpair=as.data.frame.table(table(dataset_big_pos$Hosp_Dist))
names(avgnohospdistpair)=c("Hosp_Dist","Count")
dataset=merge(dataset,avgnohospdistpair,by="Hosp_Dist",
              all.x=TRUE)
dataset_big=merge(dataset_big,avgnohospdistpair,by="Hosp_Dist",
                  all.x=TRUE)
solution=merge(solution,avgnohospdistpair,by="Hosp_Dist",
               all.x=TRUE)

diststats=as.data.frame.table(tapply(hosprof$Hospital_employees,hosprof$District_ID,mean))
names(diststats)=c("District_ID","Mean_Patients")
dataset_big_pos=subset(dataset_big,dataset_big$Buy_or_not=="Pos")
distcounter=as.matrix(table(dataset_big_pos$District_ID,dataset_big_pos$Instrument_ID))
distcounter=as.data.frame.matrix(distcounter)
distcounter$NumInstruments=apply(distcounter[,1:15],1,sum)
distcounter$District_ID=row.names(distcounter)
names(distcounter)[1:16]=paste("Dist",names(distcounter)[1:16])
dataset=merge(dataset,distcounter,by="District_ID",
              all.x=TRUE)
dataset_big=merge(dataset_big,distcounter,by="District_ID",
                  all.x=TRUE)
solution=merge(solution,distcounter,by="District_ID",
               all.x=TRUE)

dataset=dataset[,c(3,1,6,2,5,4,7,9:56,8)]
dataset_big=dataset_big[,c(3,1,6,2,5,4,7,9:56,8)]
solution=solution[,c(3,1,6,2,5,4,9:57,7,8)]
regdataset$Buy_or_not=NULL
regdataset=regdataset[,c(6:1,7,8,10:21,9)]

library(gdata)
keep(dataset,dataset_big,solution,regdataset,sure=TRUE)

#RoughFixer of Dataset
library(randomForest)
dataset[,-c(4:7)]=na.roughfix(dataset[,-c(4:7)])
dataset_big[,-c(4:7)]=na.roughfix(dataset_big[,-c(4:7)])
solution[,-c(4:7,56:57)]=na.roughfix(solution[,-c(4:7,56:57)])
regdataset[,-c(4:7)]=na.roughfix(regdataset[,-c(4:7)])

##
# fixer=function(data,pivot){
#   pivotlevels=levels(pivot)
#   for(level in pivotlevels)
#     data[pivot==level,]=na.roughfix(data[pivot==level,])
#   return(data)
# }
# 
# dataset[,-c(4:7)]=fixer(dataset[,-c(4:7)],dataset$Instrument_ID)
# dataset_big[,-c(4:7)]=fixer(dataset_big[,-c(4:7)],dataset_big$Instrument_ID)
# solution[,-c(4:7,56:57)]=fixer(solution[,-c(4:7,56:57)],solution$Instrument_ID)
# regdataset[,-c(4:7)]=fixer(regdataset[,-c(4:7)],regdataset$Instrument_ID)


###Modelling

library(caret)
library(doParallel)

levels(dataset$Instrument_ID)

##Instrument1
dataset_ent1=subset(dataset,dataset$Instrument_ID=="Instrument 1")
dataset_big_ent1=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 1")
solution_ent1=subset(solution,solution$Instrument_ID=="Instrument 1")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent1 = train(Buy_or_not~.,
                       data=dataset_ent1[,-c(1,4,5,6,7)],
                       method="xgbTree",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_xgb_ent1=predict(model_xgb_ent1,newdata=solution_ent1,type="prob")
#write.csv(output_xgb_ent1,"ent1.csv",row.names = FALSE)
solution_ent1$Buy_or_not=ifelse(output_xgb_ent1$Pos>0.9,1,0)

solution_ent1$Revenue=0

##Instrument2
dataset_ent2=subset(dataset,dataset$Instrument_ID=="Instrument 2")
dataset_big_ent2=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 2")
solution_ent2=subset(solution,solution$Instrument_ID=="Instrument 2")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent2 = train(Buy_or_not~.,
                       data=dataset_ent2[,-c(1,4,5,6,7)],
                       method="xgbTree",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_xgb_ent2=predict(model_xgb_ent2,newdata=solution_ent2,type="prob")
#write.csv(output_xgb_ent2,"ent2.csv",row.names = FALSE)
solution_ent2$Buy_or_not=ifelse(output_xgb_ent2$Pos>0.9,1,0)

solution_ent2$Revenue=0

##Instrument3

dataset_ent3=subset(dataset,dataset$Instrument_ID=="Instrument 3")
dataset_big_ent3=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 3")
solution_ent3=subset(solution,solution$Instrument_ID=="Instrument 3")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent3 = train(Buy_or_not~.,
                       data=dataset_ent3[,-c(1,4,5,6,7)],
                       method="xgbTree",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_xgb_ent3=predict(model_xgb_ent3,newdata=solution_ent3,type="prob")
#write.csv(output_xgb_ent3,"ent3.csv",row.names = FALSE)
solution_ent3$Buy_or_not=ifelse(output_xgb_ent3$Pos>0.9,1,0)

solution_ent3$Revenue=0

##Instrument4

dataset_ent4=subset(dataset,dataset$Instrument_ID=="Instrument 4")
dataset_big_ent4=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 4")
solution_ent4=subset(solution,solution$Instrument_ID=="Instrument 4")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent4 = train(Buy_or_not~.,
                       data=dataset_ent4[,-c(1,4,5,6,7)],
                       method="xgbTree",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_xgb_ent4=predict(model_xgb_ent4,newdata=solution_ent4,type="prob")
#write.csv(output_xgb_ent4,"ent4.csv",row.names = FALSE)
solution_ent4$Buy_or_not=ifelse(output_xgb_ent4$Pos>0.9,1,0)

solution_ent4$Revenue=0

##Instrument5

dataset_ent5=subset(dataset,dataset$Instrument_ID=="Instrument 5")
dataset_big_ent5=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 5")
solution_ent5=subset(solution,solution$Instrument_ID=="Instrument 5")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent5 = train(Buy_or_not~.,
                       data=dataset_ent5[,-c(1,4,5,6,7)],
                       method="xgbTree",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_xgb_ent5=predict(model_xgb_ent5,newdata=solution_ent5,type="prob")
#write.csv(output_xgb_ent5,"ent5.csv",row.names = FALSE)
solution_ent5$Buy_or_not=ifelse(output_xgb_ent5$Pos>0.9,1,0)

solution_ent5$Revenue=0


##Instrument6

dataset_ent6=subset(dataset,dataset$Instrument_ID=="Instrument 6")
dataset_big_ent6=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 6")
solution_ent6=subset(solution,solution$Instrument_ID=="Instrument 6")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent6 = train(Buy_or_not~.,
                       data=dataset_ent6[,-c(1,4,5,6,7)],
                       method="xgbTree",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_xgb_ent6=predict(model_xgb_ent6,newdata=solution_ent6,type="prob")
#write.csv(output_xgb_ent6,"ent6.csv",row.names = FALSE)
solution_ent6$Buy_or_not=ifelse(output_xgb_ent6$Pos>0.9,1,0)

solution_ent6$Revenue=0


##Instrument7

dataset_ent7=subset(dataset,dataset$Instrument_ID=="Instrument 7")
dataset_big_ent7=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 7")
solution_ent7=subset(solution,solution$Instrument_ID=="Instrument 7")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent7 = train(Buy_or_not~.,
                       data=dataset_ent7[,-c(1,4,5,6,7)],
                       method="xgbTree",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_xgb_ent7=predict(model_xgb_ent7,newdata=solution_ent7,type="prob")
#write.csv(output_xgb_ent7,"ent7.csv",row.names = FALSE)
solution_ent7$Buy_or_not=ifelse(output_xgb_ent7$Pos>0.9,1,0)

solution_ent7$Revenue=0

##Instrument8

dataset_ent8=subset(dataset,dataset$Instrument_ID=="Instrument 8")
dataset_big_ent8=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 8")
solution_ent8=subset(solution,solution$Instrument_ID=="Instrument 8")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent8 = train(Buy_or_not~.,
                       data=dataset_ent8[,-c(1,4,5,6,7)],
                       method="xgbTree",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_xgb_ent8=predict(model_xgb_ent8,newdata=solution_ent8,type="prob")
#write.csv(output_xgb_ent8,"ent8.csv",row.names = FALSE)
solution_ent8$Buy_or_not=ifelse(output_xgb_ent8$Pos>0.9,1,0)

solution_ent8$Revenue=0

##Instrument10

dataset_ent10=subset(dataset,dataset$Instrument_ID=="Instrument 10")
dataset_big_ent10=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 10")
solution_ent10=subset(solution,solution$Instrument_ID=="Instrument 10")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent10 = train(Buy_or_not~.,
                        data=dataset_ent10[,-c(1,4,5,6,7)],
                        method="xgbTree",
                        metric="ROC",
                        #tuneGrid=tuneGr,
                        trControl=trctrl)

stopCluster(cl)

output_xgb_ent10=predict(model_xgb_ent10,newdata=solution_ent10,type="prob")
#write.csv(output_xgb_ent10,"ent10.csv",row.names = FALSE)
solution_ent10$Buy_or_not=ifelse(output_xgb_ent10$Pos>0.9,1,0)

solution_ent10$Revenue=0

##Instrument11

dataset_ent11=subset(dataset,dataset$Instrument_ID=="Instrument 11")
dataset_big_ent11=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 11")
solution_ent11=subset(solution,solution$Instrument_ID=="Instrument 11")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent11 = train(Buy_or_not~.,
                        data=dataset_ent11[,-c(1,4,5,6,7)],
                        method="xgbTree",
                        metric="ROC",
                        #tuneGrid=tuneGr,
                        trControl=trctrl)

stopCluster(cl)

output_xgb_ent11=predict(model_xgb_ent11,newdata=solution_ent11,type="prob")
#write.csv(output_xgb_ent11,"ent11.csv",row.names = FALSE)
solution_ent11$Buy_or_not=ifelse(output_xgb_ent11$Pos>0.9,1,0)

solution_ent11$Revenue=0

##Instrument13

dataset_ent13=subset(dataset,dataset$Instrument_ID=="Instrument 13")
dataset_big_ent13=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 13")
solution_ent13=subset(solution,solution$Instrument_ID=="Instrument 13")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent13 = train(Buy_or_not~.,
                        data=dataset_ent13[,-c(1,4,5,6,7)],
                        method="xgbTree",
                        metric="ROC",
                        #tuneGrid=tuneGr,
                        trControl=trctrl)

stopCluster(cl)

output_xgb_ent13=predict(model_xgb_ent13,newdata=solution_ent13,type="prob")
#write.csv(output_xgb_ent13,"ent13.csv",row.names = FALSE)
solution_ent13$Buy_or_not=ifelse(output_xgb_ent13$Pos>0.9,1,0)

solution_ent13$Revenue=0

##Instrument3

dataset_ent15=subset(dataset,dataset$Instrument_ID=="Instrument 15")
dataset_big_ent15=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 15")
solution_ent15=subset(solution,solution$Instrument_ID=="Instrument 15")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent15 = train(Buy_or_not~.,
                        data=dataset_ent15[,-c(1,4,5,6,7)],
                        method="xgbTree",
                        metric="ROC",
                        #tuneGrid=tuneGr,
                        trControl=trctrl)

stopCluster(cl)

output_xgb_ent15=predict(model_xgb_ent15,newdata=solution_ent15,type="prob")
#write.csv(output_xgb_ent15,"ent15.csv",row.names = FALSE)
solution_ent15$Buy_or_not=ifelse(output_xgb_ent15$Pos>0.9,1,0)

solution_ent15$Revenue=0

##Instrument3

dataset_ent18=subset(dataset,dataset$Instrument_ID=="Instrument 18")
dataset_big_ent18=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 18")
solution_ent18=subset(solution,solution$Instrument_ID=="Instrument 18")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent18 = train(Buy_or_not~.,
                        data=dataset_ent18[,-c(1,4,5,6,7)],
                        method="xgbTree",
                        metric="ROC",
                        #tuneGrid=tuneGr,
                        trControl=trctrl)

stopCluster(cl)

output_xgb_ent18=predict(model_xgb_ent18,newdata=solution_ent18,type="prob")
#write.csv(output_xgb_ent18,"ent18.csv",row.names = FALSE)
solution_ent18$Buy_or_not=ifelse(output_xgb_ent18$Pos>0.9,1,0)

solution_ent18$Revenue=0

##Instrument19

dataset_ent19=subset(dataset,dataset$Instrument_ID=="Instrument 19")
dataset_big_ent19=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 19")
solution_ent19=subset(solution,solution$Instrument_ID=="Instrument 19")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent19 = train(Buy_or_not~.,
                        data=dataset_ent19[,-c(1,4,5,6,7)],
                        method="xgbTree",
                        metric="ROC",
                        #tuneGrid=tuneGr,
                        trControl=trctrl)

stopCluster(cl)

output_xgb_ent19=predict(model_xgb_ent19,newdata=solution_ent19,type="prob")
#write.csv(output_xgb_ent19,"ent19.csv",row.names = FALSE)
solution_ent19$Buy_or_not=ifelse(output_xgb_ent19$Pos>0.9,1,0)

solution_ent19$Revenue=0


##Instrument20

dataset_ent20=subset(dataset,dataset$Instrument_ID=="Instrument 20")
dataset_big_ent20=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 20")
solution_ent20=subset(solution,solution$Instrument_ID=="Instrument 20")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_xgb_ent20 = train(Buy_or_not~.,
                        data=dataset_ent20[,-c(1,4,5,6,7)],
                        method="xgbTree",
                        metric="ROC",
                        #tuneGrid=tuneGr,
                        trControl=trctrl)

stopCluster(cl)

output_xgb_ent20=predict(model_xgb_ent20,newdata=solution_ent20,type="prob")
#write.csv(output_xgb_ent20,"ent20.csv",row.names = FALSE)
solution_ent20$Buy_or_not=ifelse(output_xgb_ent20$Pos>0.9,1,0)

solution_ent20$Revenue=0

solution_ent1$Buy_or_not=ifelse(output_xgb_ent1$Pos>0.9,1,0)
solution_ent2$Buy_or_not=ifelse(output_xgb_ent2$Pos>0.9,1,0)
solution_ent3$Buy_or_not=ifelse(output_xgb_ent3$Pos>0.9,1,0)
solution_ent4$Buy_or_not=ifelse(output_xgb_ent4$Pos>0.9,1,0)
solution_ent5$Buy_or_not=ifelse(output_xgb_ent5$Pos>0.9,1,0)
solution_ent6$Buy_or_not=ifelse(output_xgb_ent6$Pos>0.9,1,0)
solution_ent7$Buy_or_not=ifelse(output_xgb_ent7$Pos>0.8,1,0)
solution_ent8$Buy_or_not=ifelse(output_xgb_ent8$Pos>0.9,1,0)
solution_ent10$Buy_or_not=ifelse(output_xgb_ent10$Pos>0.5,1,0)
solution_ent11$Buy_or_not=ifelse(output_xgb_ent11$Pos>0.5,1,0)
solution_ent13$Buy_or_not=ifelse(output_xgb_ent13$Pos>0.5,1,0)
solution_ent15$Buy_or_not=ifelse(output_xgb_ent15$Pos>0.5,1,0)
solution_ent18$Buy_or_not=ifelse(output_xgb_ent18$Pos>0.5,1,0)
solution_ent19$Buy_or_not=ifelse(output_xgb_ent19$Pos>0.5,1,0)
solution_ent20$Buy_or_not=ifelse(output_xgb_ent20$Pos>0.5,1,0)

solution_ent=rbind(solution_ent1,solution_ent2,solution_ent3,solution_ent4,
                   solution_ent5,solution_ent6,solution_ent7,solution_ent8,
                   solution_ent10,solution_ent11,solution_ent13,solution_ent15,
                   solution_ent18,solution_ent19,solution_ent20)

library(caret)
library(doParallel)

levels(dataset$Instrument_ID)

##Instrument1
dataset_ent1=subset(dataset,dataset$Instrument_ID=="Instrument 1")
dataset_big_ent1=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 1")
solutiong_ent1=subset(solution,solution$Instrument_ID=="Instrument 1")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent1 = train(Buy_or_not~.,
                       data=dataset_ent1[,-c(1,4,5,6,7)],
                       method="gbm",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_gbm_ent1=predict(model_gbm_ent1,newdata=solutiong_ent1,type="prob")
#write.csv(output_gbm_ent1,"ent1.csv",row.names = FALSE)
solutiong_ent1$Buy_or_not=ifelse(output_gbm_ent1$Pos>0.9,1,0)

solutiong_ent1$Revenue=0

##Instrument2
dataset_ent2=subset(dataset,dataset$Instrument_ID=="Instrument 2")
dataset_big_ent2=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 2")
solutiong_ent2=subset(solution,solution$Instrument_ID=="Instrument 2")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent2 = train(Buy_or_not~.,
                       data=dataset_ent2[,-c(1,4,5,6,7)],
                       method="gbm",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_gbm_ent2=predict(model_gbm_ent2,newdata=solutiong_ent2,type="prob")
#write.csv(output_gbm_ent2,"ent2.csv",row.names = FALSE)
solutiong_ent2$Buy_or_not=ifelse(output_gbm_ent2$Pos>0.9,1,0)

solutiong_ent2$Revenue=0

##Instrument3

dataset_ent3=subset(dataset,dataset$Instrument_ID=="Instrument 3")
dataset_big_ent3=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 3")
solutiong_ent3=subset(solution,solution$Instrument_ID=="Instrument 3")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent3 = train(Buy_or_not~.,
                       data=dataset_ent3[,-c(1,4,5,6,7)],
                       method="gbm",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_gbm_ent3=predict(model_gbm_ent3,newdata=solutiong_ent3,type="prob")
#write.csv(output_gbm_ent3,"ent3.csv",row.names = FALSE)
solutiong_ent3$Buy_or_not=ifelse(output_gbm_ent3$Pos>0.9,1,0)

solutiong_ent3$Revenue=0

##Instrument4

dataset_ent4=subset(dataset,dataset$Instrument_ID=="Instrument 4")
dataset_big_ent4=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 4")
solutiong_ent4=subset(solution,solution$Instrument_ID=="Instrument 4")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent4 = train(Buy_or_not~.,
                       data=dataset_ent4[,-c(1,4,5,6,7)],
                       method="gbm",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_gbm_ent4=predict(model_gbm_ent4,newdata=solutiong_ent4,type="prob")
#write.csv(output_gbm_ent4,"ent4.csv",row.names = FALSE)
solutiong_ent4$Buy_or_not=ifelse(output_gbm_ent4$Pos>0.9,1,0)

solutiong_ent4$Revenue=0

##Instrument5

dataset_ent5=subset(dataset,dataset$Instrument_ID=="Instrument 5")
dataset_big_ent5=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 5")
solutiong_ent5=subset(solution,solution$Instrument_ID=="Instrument 5")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent5 = train(Buy_or_not~.,
                       data=dataset_ent5[,-c(1,4,5,6,7)],
                       method="gbm",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_gbm_ent5=predict(model_gbm_ent5,newdata=solutiong_ent5,type="prob")
#write.csv(output_gbm_ent5,"ent5.csv",row.names = FALSE)
solutiong_ent5$Buy_or_not=ifelse(output_gbm_ent5$Pos>0.9,1,0)

solutiong_ent5$Revenue=0


##Instrument6

dataset_ent6=subset(dataset,dataset$Instrument_ID=="Instrument 6")
dataset_big_ent6=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 6")
solutiong_ent6=subset(solution,solution$Instrument_ID=="Instrument 6")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent6 = train(Buy_or_not~.,
                       data=dataset_ent6[,-c(1,4,5,6,7)],
                       method="gbm",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_gbm_ent6=predict(model_gbm_ent6,newdata=solutiong_ent6,type="prob")
#write.csv(output_gbm_ent6,"ent6.csv",row.names = FALSE)
solutiong_ent6$Buy_or_not=ifelse(output_gbm_ent6$Pos>0.9,1,0)

solutiong_ent6$Revenue=0


##Instrument7

dataset_ent7=subset(dataset,dataset$Instrument_ID=="Instrument 7")
dataset_big_ent7=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 7")
solutiong_ent7=subset(solution,solution$Instrument_ID=="Instrument 7")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent7 = train(Buy_or_not~.,
                       data=dataset_ent7[,-c(1,4,5,6,7)],
                       method="gbm",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_gbm_ent7=predict(model_gbm_ent7,newdata=solutiong_ent7,type="prob")
#write.csv(output_gbm_ent7,"ent7.csv",row.names = FALSE)
solutiong_ent7$Buy_or_not=ifelse(output_gbm_ent7$Pos>0.9,1,0)

solutiong_ent7$Revenue=0

##Instrument8

dataset_ent8=subset(dataset,dataset$Instrument_ID=="Instrument 8")
dataset_big_ent8=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 8")
solutiong_ent8=subset(solution,solution$Instrument_ID=="Instrument 8")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent8 = train(Buy_or_not~.,
                       data=dataset_ent8[,-c(1,4,5,6,7)],
                       method="gbm",
                       metric="ROC",
                       #tuneGrid=tuneGr,
                       trControl=trctrl)

stopCluster(cl)

output_gbm_ent8=predict(model_gbm_ent8,newdata=solutiong_ent8,type="prob")
#write.csv(output_gbm_ent8,"ent8.csv",row.names = FALSE)
solutiong_ent8$Buy_or_not=ifelse(output_gbm_ent8$Pos>0.9,1,0)

solutiong_ent8$Revenue=0

##Instrument10

dataset_ent10=subset(dataset,dataset$Instrument_ID=="Instrument 10")
dataset_big_ent10=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 10")
solutiong_ent10=subset(solution,solution$Instrument_ID=="Instrument 10")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent10 = train(Buy_or_not~.,
                        data=dataset_ent10[,-c(1,4,5,6,7)],
                        method="gbm",
                        metric="ROC",
                        #tuneGrid=tuneGr,
                        trControl=trctrl)

stopCluster(cl)

output_gbm_ent10=predict(model_gbm_ent10,newdata=solutiong_ent10,type="prob")
#write.csv(output_gbm_ent10,"ent10.csv",row.names = FALSE)
solutiong_ent10$Buy_or_not=ifelse(output_gbm_ent10$Pos>0.9,1,0)

solutiong_ent10$Revenue=0

##Instrument11

dataset_ent11=subset(dataset,dataset$Instrument_ID=="Instrument 11")
dataset_big_ent11=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 11")
solutiong_ent11=subset(solution,solution$Instrument_ID=="Instrument 11")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent11 = train(Buy_or_not~.,
                        data=dataset_ent11[,-c(1,4,5,6,7)],
                        method="gbm",
                        metric="ROC",
                        #tuneGrid=tuneGr,
                        trControl=trctrl)

stopCluster(cl)

output_gbm_ent11=predict(model_gbm_ent11,newdata=solutiong_ent11,type="prob")
#write.csv(output_gbm_ent11,"ent11.csv",row.names = FALSE)
solutiong_ent11$Buy_or_not=ifelse(output_gbm_ent11$Pos>0.9,1,0)

solutiong_ent11$Revenue=0

##Instrument13

dataset_ent13=subset(dataset,dataset$Instrument_ID=="Instrument 13")
dataset_big_ent13=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 13")
solutiong_ent13=subset(solution,solution$Instrument_ID=="Instrument 13")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent13 = train(Buy_or_not~.,
                        data=dataset_ent13[,-c(1,4,5,6,7)],
                        method="gbm",
                        metric="ROC",
                        #tuneGrid=tuneGr,
                        trControl=trctrl)

stopCluster(cl)

output_gbm_ent13=predict(model_gbm_ent13,newdata=solutiong_ent13,type="prob")
#write.csv(output_gbm_ent13,"ent13.csv",row.names = FALSE)
solutiong_ent13$Buy_or_not=ifelse(output_gbm_ent13$Pos>0.9,1,0)

solutiong_ent13$Revenue=0

##Instrument3

dataset_ent15=subset(dataset,dataset$Instrument_ID=="Instrument 15")
dataset_big_ent15=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 15")
solutiong_ent15=subset(solution,solution$Instrument_ID=="Instrument 15")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent15 = train(Buy_or_not~.,
                        data=dataset_ent15[,-c(1,4,5,6,7)],
                        method="gbm",
                        metric="ROC",
                        #tuneGrid=tuneGr,
                        trControl=trctrl)

stopCluster(cl)

output_gbm_ent15=predict(model_gbm_ent15,newdata=solutiong_ent15,type="prob")
#write.csv(output_gbm_ent15,"ent15.csv",row.names = FALSE)
solutiong_ent15$Buy_or_not=ifelse(output_gbm_ent15$Pos>0.9,1,0)

solutiong_ent15$Revenue=0

##Instrument3

dataset_ent18=subset(dataset,dataset$Instrument_ID=="Instrument 18")
dataset_big_ent18=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 18")
solutiong_ent18=subset(solution,solution$Instrument_ID=="Instrument 18")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent18 = train(Buy_or_not~.,
                        data=dataset_ent18[,-c(1,4,5,6,7)],
                        method="gbm",
                        metric="ROC",
                        #tuneGrid=tuneGr,
                        trControl=trctrl)

stopCluster(cl)

output_gbm_ent18=predict(model_gbm_ent18,newdata=solutiong_ent18,type="prob")
#write.csv(output_gbm_ent18,"ent18.csv",row.names = FALSE)
solutiong_ent18$Buy_or_not=ifelse(output_gbm_ent18$Pos>0.9,1,0)

solutiong_ent18$Revenue=0

##Instrument19

dataset_ent19=subset(dataset,dataset$Instrument_ID=="Instrument 19")
dataset_big_ent19=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 19")
solutiong_ent19=subset(solution,solution$Instrument_ID=="Instrument 19")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent19 = train(Buy_or_not~.,
                        data=dataset_ent19[,-c(1,4,5,6,7)],
                        method="gbm",
                        metric="ROC",
                        #tuneGrid=tuneGr,
                        trControl=trctrl)

stopCluster(cl)

output_gbm_ent19=predict(model_gbm_ent19,newdata=solutiong_ent19,type="prob")
#write.csv(output_gbm_ent19,"ent19.csv",row.names = FALSE)
solutiong_ent19$Buy_or_not=ifelse(output_gbm_ent19$Pos>0.9,1,0)

solutiong_ent19$Revenue=0


##Instrument20

dataset_ent20=subset(dataset,dataset$Instrument_ID=="Instrument 20")
dataset_big_ent20=subset(dataset_big,dataset_big$Instrument_ID=="Instrument 20")
solutiong_ent20=subset(solution,solution$Instrument_ID=="Instrument 20")

cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1)

trctrl = trainControl(method="repeatedcv",number=4,repeats=1,classProbs = TRUE,
                      summaryFunction = twoClassSummary)
# tuneGr= expand.grid(.eta=c(0.001),.max_depth=c(3,5),
#                     .colsample_bytree=0.6,.nrounds=c(10000),.gamma=c(0),
#                     .min_child_weight=1)

model_gbm_ent20 = train(Buy_or_not~.,
                        data=dataset_ent20[,-c(1,4,5,6,7)],
                        method="gbm",
                        metric="ROC",
                        #tuneGrid=tuneGr,
                        trControl=trctrl)

stopCluster(cl)

output_gbm_ent20=predict(model_gbm_ent20,newdata=solutiong_ent20,type="prob")
##write.csv(output_gbm_ent20,"ent20.csv",row.names = FALSE)
solutiong_ent20$Buy_or_not=ifelse(output_gbm_ent20$Pos>0.9,1,0)

solutiong_ent20$Revenue=0

solutiong_ent1$Buy_or_not=ifelse(output_gbm_ent1$Pos>0.8,1,0)
solutiong_ent2$Buy_or_not=ifelse(output_gbm_ent2$Pos>0.9,1,0)
solutiong_ent3$Buy_or_not=ifelse(output_gbm_ent3$Pos>0.9,1,0)
solutiong_ent4$Buy_or_not=ifelse(output_gbm_ent4$Pos>0.9,1,0)
solutiong_ent5$Buy_or_not=ifelse(output_gbm_ent5$Pos>0.9,1,0)
solutiong_ent6$Buy_or_not=ifelse(output_gbm_ent6$Pos>0.9,1,0)
solutiong_ent7$Buy_or_not=ifelse(output_gbm_ent7$Pos>0.9,1,0)
solutiong_ent8$Buy_or_not=ifelse(output_gbm_ent8$Pos>0.9,1,0)
solutiong_ent10$Buy_or_not=ifelse(output_gbm_ent10$Pos>0.5,1,0)
solutiong_ent11$Buy_or_not=ifelse(output_gbm_ent11$Pos>0.5,1,0)
solutiong_ent13$Buy_or_not=ifelse(output_gbm_ent13$Pos>0.5,1,0)
solutiong_ent15$Buy_or_not=ifelse(output_gbm_ent15$Pos>0.5,1,0)
solutiong_ent18$Buy_or_not=ifelse(output_gbm_ent18$Pos>0.5,1,0)
solutiong_ent19$Buy_or_not=ifelse(output_gbm_ent19$Pos>0.5,1,0)
solutiong_ent20$Buy_or_not=ifelse(output_gbm_ent20$Pos>0.5,1,0)

solutiong_ent=rbind(solutiong_ent1,solutiong_ent2,solutiong_ent3,solutiong_ent4,
                    solutiong_ent5,solutiong_ent6,solutiong_ent7,solutiong_ent8,
                    solutiong_ent10,solutiong_ent11,solutiong_ent13,solutiong_ent15,
                    solutiong_ent18,solutiong_ent19,solutiong_ent20)

solutiong_ent=solutiong_ent[order(solutiong_ent$Unique_ID),]
solution_ent=solution_ent[order(solution_ent$Unique_ID),]

solution_ent$Buy_or_not=as.numeric(solution_ent$Buy_or_not|solutiong_ent$Buy_or_not)
  

library(caret)
library(doParallel)
cl <- makeCluster(6)
registerDoParallel(cl)

trctrl=trainControl(method = "repeatedcv",number = 3,repeats = 1)
regmodel=train(Amount~.,
               data = regdataset[,c(8:21)],
               method="xgbTree",
               trControl=trctrl
)

stopCluster(cl)

solution_ent$Revenue=predict(regmodel,newdata=solution_ent)
solution_ent$Revenue[which(solution_ent$Buy_or_not==0)]=0

write.csv(solution_ent[,c("Hospital_ID","District_ID","Instrument_ID","Buy_or_not","Revenue")]
          ,"Solution.csv",row.names=FALSE)

## This will overwrite Solution.csv in the working directory with the outputs.
