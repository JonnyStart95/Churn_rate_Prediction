# Parameter Directory(Reminder)
# -------------------------------------------------------------------------------------------
# nbAdultAvg: Average number of adults in the family from beginning of the client's contract
# Chrono: Month of client arrival
# nbrIsOverData: Number of times the data limit was exceeded
# cashdown: Initial cash down on the phone
# promo=1: The client was invited to the dinner during the pilot
# Lifetime: the lifetime of the client in months
# Churnln3month: Client has left in a 3-month window
# -------------------------------------------------------------------------------------------
streamraw=read.csv("Retention_train.csv")
{#install.packages("plyr")
library("plyr")
library(dplyr)
library(tidyselect)
source("Theme4-functions.R")}# Read the data
{streamraw%>% nrow()
str(streamraw)         # List the column name and the value of each column
head(streamraw)}#First check the data
{Index=streamraw%>%select("ID","IDfamily")

Index_table=data.frame(table(Index$IDfamily))

streamraw <- merge(x=streamraw,y=Index_table,by.x = "IDfamily", by.y = "Var1", all.x = TRUE)}#Set the artificial variable

streamraw$timeSinceLastTechProb[is.na(streamraw$timeSinceLastTechProb)]=100
# Since this customer never meet Techprob before, so we set it as 100 to make in 
# the situation same as the customers who haven't met the Techprob for a long time

streamraw=streamraw%>%
  select(-c("baseMonthlyRateForPhone","phoneBalance","phonePrice"))
# These are nothing to do with the rating of the plan

streamraw$minutesVoice[is.na(streamraw$minutesVoice)]=200
# Set those customers who have unlimited voice to 200


streamraw$timeSinceLastIsOverData[is.na(streamraw$timeSinceLastIsOverData)]=80
#Set those who have never be overdata, timeSinceLastIsOverData=100/80

streamraw$timeSinceLastIsOverVoice[is.na(streamraw$timeSinceLastIsOverVoice)]=30
#Set those who have never be overvooice, timeSinceLastIsOverVoice=200/30

streamraw$timeSinceLastComplaints[is.na(streamraw$timeSinceLastComplaints)]=100
#Set those who have never be complaints, timeSinceLastComplaints=10000/100

streamraw=streamraw%>%
  mutate(isWorkPhone=as.factor(isWorkPhone), unlimitedVoice=as.factor(unlimitedVoice), 
         unlimitedText=as.factor(unlimitedText), promo=as.factor(promo))
# Set the numberial varaibles into factor variables

#streamraw[is.na(streamraw)]=0

# From streamraw------>stream
stream=streamraw%>%
  filter(promo==0)%>%
  select(-c("promo","unlimitedText","ID","IDfamily"))

summary(stream)
         
set.seed(88888)
trainID=sample(1:677933,550000)
train=stream[trainID,]
validate=stream[-trainID,]
# Cleaning the data-------------------------------------------------------------------------------------------

mod1=glm(churnIn3Month~.,family="binomial", data=train)

summary(mod1)
p1=predict(mod1,newdata=validate,type="response")
cbind(p1,validate)[sort.list(p1,decreasing=TRUE)[1:30],]
cbind1=cbind(p1,validate)[sort.list(p1,decreasing=TRUE)[1:10000],]
predict_correction_p1=sum(cbind1$churnIn3Month)/10000

# Finish a p1 normal test regression(Bad outcome, maximum around 0.1699780)--------------------------------------------------------------------------------------------------------------------------

mod2=step(mod1,trace=FALSE)
summary(mod2)
p2=predict(mod2,newdata=validate,type="response")
cbind(p2,validate)[sort.list(p2,decreasing=TRUE)[1:30],]
cbind2=cbind(p2,validate)[sort.list(p2,decreasing=TRUE)[1:10000],]
predict_correction_p2=sum(cbind2$churnIn3Month)/10000

# Data Preparation for backforward Algorithm
train_bestmod=train%>%
  select(nbAdultAvg,chrono,age,gender,isWorkPhone,planType,data,dataAvgConsumption,nbrIsOverData,
         timeSinceLastIsOverData,unlimitedVoice,minutesVoice,voiceAvgConsumption,nbrIsOverVoice,
         timeSinceLastIsOverVoice,textoAvgConsumption,cashDown,timeSinceLastTechProb,
         nbrTechnicalProblems,timeSinceLastComplaints,nbrComplaints,lifeTime,Freq,baseMonthlyRateForPlan,churnIn3Month)
validate_bestmod=validate%>%
  select(nbAdultAvg,chrono,age,gender,isWorkPhone,planType,data,dataAvgConsumption,nbrIsOverData,
         timeSinceLastIsOverData,unlimitedVoice,minutesVoice,voiceAvgConsumption,nbrIsOverVoice,
         timeSinceLastIsOverVoice,textoAvgConsumption,cashDown,timeSinceLastTechProb,
         nbrTechnicalProblems,timeSinceLastComplaints,nbrComplaints,lifeTime,Freq,baseMonthlyRateForPlan,churnIn3Month)
keep=rep(TRUE,23)

bestmod=glm(churnIn3Month~.,family="binomial",data=train_bestmod)
bestkeep=keep
bestp=predict(bestmod,newdata=validate_bestmod,type="response")
bestmod_cbind=cbind(bestp,validate_bestmod)
bestmod_cbind=cbind(bestp,validate_bestmod)[sort.list(bestp,decreasing=TRUE)[1:10000],]
predict_correction_bestp=sum(bestmod_cbind$churnIn3Month)/10000
newbest=TRUE
while(sum(keep)>=2 & newbest){
  newbest=FALSE
  keep=bestkeep
  cat("\nBest model so far:",names(train_bestmod)[bestkeep])
  for(i in (1:23)[keep]){
    cat(".")
    keep[i]=FALSE
    mod=glm(churnIn3Month~.,family="binomial",data=train_bestmod[,c(keep,TRUE,TRUE)])
    p=predict(mod,newdata=validate_bestmod,type="response")
    temporary_cbind=cbind(p,validate_bestmod)
    temporary_cbind=cbind(p,validate_bestmod)[sort.list(p,decreasing=TRUE)[1:10000],]
    predict_correction_p=sum(temporary_cbind$churnIn3Month)/10000
    
    if(predict_correction_bestp<predict_correction_p){
      newbest=TRUE
      bestmod=mod
      bestkeep=keep
      bestp=p
      predict_correction_bestp=predict_correction_p
      bestmod_cbind=temporary_cbind
    }
    keep[i]=TRUE
  }
}


streamrawplus=streamraw%>%
  filter(promo==0)%>%
  select(-c("promo","unlimitedText","ID","IDfamily"))%>%
  select(churnIn3Month,Freq,unlimitedVoice,planType,isWorkPhone,gender,nbrIsOverData,nbrIsOverVoice,everything())
# Changing order of variables to leave the numeric variables last
for(i in 9:25){
  for(j in i:25){
    streamrawplus = streamrawplus %>%
      mutate(!!paste(names(streamrawplus)[[i]],names(streamrawplus)[[j]],sep="_"):=as.numeric(streamrawplus[[i]])*streamrawplus[[j]])
  }
}
# Caculate the correlation between the numeracial variables
streamplus=streamrawplus

trainplus=streamplus[trainID,]
validplus=streamplus[-trainID,]

mod3=glm(churnIn3Month~.,family="binomial",data=trainplus)
p3=predict(mod3,newdata=validplus,type="response")
cbind(p3,validplus)[sort.list(p3,decreasing=TRUE)[1:30],]
cbind3=cbind(p3,validplus)[sort.list(p3,decreasing=TRUE)[1:10000],]
predict_correction_p3=sum(cbind3$churnIn3Month)/10000

mod4=step(mod3,trace=FALSE)
p4=predict(mod4,newdata=validplus,type="response")
cbind(p4,validplus)[sort.list(p4,decreasing=TRUE)[1:30],]
cbind4=cbind(p4,validplus)[sort.list(p4,decreasing=TRUE)[1:10000],]
predict_correction_p4=sum(cbind4$churnIn3Month)/10000

# Get the predictions of the other 3 models

roc(validplus$churnIn3Month,p1)$AUC
roc(validplus$churnIn3Month,p2,lines=TRUE,col="red")$AUC!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
roc(validplus$churnIn3Month,p3,lines=TRUE,lty=2)$AUC
roc(validplus$churnIn3Month,bestp,lines=TRUE,col="red",lty=2)$AUC!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Get the ROC curves for the 4 predictions

#lift(validplus$churnIn3Month,p1)
#lift(validplus$churnIn3Month,p2,lines=TRUE,col="red")!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#lift(validplus$churnIn3Month,p3,lines=TRUE,lty=2)
#lift(validplus$churnIn3Month,p4,lines=TRUE,col="red",lty=2)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Get the lift curve of 4 models

#Confusion Matrix   Prediction=0    Prediction=1
#Truth=0            0               12.5
#Truth=1            0               593.09


#th1=cost(validplus$churnIn3Month,p1,cost=matrix(c(0,0,12.5,-593.09),2,2),k=1000)$ThresholdMin
#th2=cost(validplus$churnIn3Month,p2,cost=matrix(c(0,0,12.5,593.09),2,2),k=1000)$ThresholdMin!!!!!!!!!!!!!!!!!!!!!!
#th3=cost(validplus$churnIn3Month,p3,cost=matrix(c(0,0,12.5,-593.09),2,2),k=1000)$ThresholdMin
#th4=cost(validplus$churnIn3Month,p4,cost=matrix(c(0,0,12.5,593.09),2,2),k=1000)$ThresholdMin!!!!!!!!!!!!!!!!!!!!!!!

#par(mfrow=c(2,2))
#cost(validplus$churnIn3Month,p1,cost=matrix(c(0,0,12.5,-593.09),2,2))$Min
#cost(validplus$churnIn3Month,p2,cost=matrix(c(0,0,12.5,-593.09),2,2))$Min
#cost(validplus$churnIn3Month,p3,cost=matrix(c(0,0,12.5,-593.09),2,2))$Min
#cost(validplus$churnIn3Month,p4,cost=matrix(c(0,0,12.5,-593.09),2,2))$Min

#-Processing the testing data-------------------------------------------------------------------------------------------------------

streamscore=read.csv("Retention_score.csv")
streamscore%>% nrow()
str(streamscore)
summary(streamscore)
Index_score=streamscore%>%select("ID","IDfamily")

Index_table_score=data.frame(table(Index_score$IDfamily))

streamscore <- merge(x=streamscore,y=Index_table_score,by.x = "IDfamily", by.y = "Var1", all.x = TRUE)
#-------------Set the artificial variable

streamscore$timeSinceLastTechProb[is.na(streamscore$timeSinceLastTechProb)]=100
# Since this customer never meet Techprob before, so we set it as 100 to make in 
# the situation same as the customers who haven't met the Techprob for a long time

streamscore$minutesVoice[is.na(streamscore$minutesVoice)]=200
# Set those customers who have unlimited voice to 200


streamscore$timeSinceLastIsOverData[is.na(streamscore$timeSinceLastIsOverData)]=100
#Set those who have never be overdata, timeSinceLastIsOverData=100/80

streamscore$timeSinceLastIsOverVoice[is.na(streamscore$timeSinceLastIsOverVoice)]=30
#Set those who have never be overvooice, timeSinceLastIsOverVoice=200/30

streamscore$timeSinceLastComplaints[is.na(streamscore$timeSinceLastComplaints)]=100
#Set those who have never be complaints, timeSinceLastComplaints=10000/100

streamscore=streamscore%>%
  mutate(isWorkPhone=as.factor(isWorkPhone), unlimitedVoice=as.factor(unlimitedVoice), 
         unlimitedText=as.factor(unlimitedText))

p1_score=predict(mod1,newdata=streamscore,type="response")
p1_score_cbind=cbind(p1_score,streamscore)
p1_score_cbind=p1_score_cbind%>%
  mutate(expectation_value_p1=baseMonthlyRateForPlan*p1_score)
p1_score_cbind=p1_score_cbind[sort.list(p1_score_cbind$expectation_value_p1,decreasing=TRUE)[1:847350],]
p1_score_cbind=p1_score_cbind%>%
  filter(Freq<=1)


p2_score=predict(mod2,newdata=streamscore,type="response")
p2_score_cbind=cbind(p2_score,streamscore)[sort.list(p2_score,decreasing=TRUE)[1:847350],]
p2_score_cbind=p2_score_cbind%>%
  filter(Freq<=3)

bestp_score=predict(bestmod,newdata=streamscore,type="response")
bestp_score_cbind=cbind(bestp_score,streamscore)
bestp_score_cbind=bestp_score_cbind%>%
  mutate(expectation_value_bestp=baseMonthlyRateForPlan*bestp_score)%>%
bestp_score_cbind=bestp_score_cbind[sort.list(bestp_score_cbind$expectation_value_bestp,decreasing=TRUE)[1:847350],]
bestp_score_cbind=bestp_score_cbind%>%
  filter(Freq<=2)

# Freq=1 more likely to leave, so we filter Freq==1

#Set the correlations data for p3 and p4

streamscoreplus=streamscore%>%
  select(-c("unlimitedText"))%>%
  select(ID,IDfamily,Freq,unlimitedVoice,planType,isWorkPhone,gender,nbrIsOverData,nbrIsOverVoice,everything())
# Changing order of variables to leave the numeric variables last
for(i in 10:26){
  for(j in i:26){
    streamscoreplus = streamscoreplus %>%
      mutate(!!paste(names(streamscoreplus)[[i]],names(streamscoreplus)[[j]],sep="_"):=as.numeric(streamscoreplus[[i]])*streamscoreplus[[j]])
  }
}
p3_score=predict(mod3,newdata=streamscoreplus,type="response")
p3_score_cbind=cbind(p3_score,streamscore)
p3_score_cbind=p3_score_cbind%>%
  mutate(expectation_value_p3=baseMonthlyRateForPlan*p3_score)
p3_score_cbind=p3_score_cbind[sort.list(p3_score_cbind$expectation_value_p3,decreasing=TRUE)[1:847350],]
p3_score_cbind=p3_score_cbind%>%
  filter(Freq<=1)





