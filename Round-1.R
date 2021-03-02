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
streamraw=read.csv("Retention-train_fixed.csv")
#install.packages("plyr")
library("plyr")
library(dplyr)
library(tidyselect)
source("Theme4-functions.R")
streamraw%>% nrow()
str(streamraw)         # List the column name and the value of each column
head(streamraw)        # Show the first rows of the dataset

Index=streamraw%>%select("ID","IDfamily")

Index_table=data.frame(table(Index$IDfamily))

streamraw <- merge(x=streamraw,y=Index_table,by.x = "IDfamily", by.y = "Var1", all.x = TRUE)
#-------------Set the artificial variable, Freq means the number of people who are using the plan in the family, simulate the 
# factor of conformity behavior

streamraw$timeSinceLastTechProb[is.na(streamraw$timeSinceLastTechProb)]=100
# Since this customer never meet Techprob before, so we set it as 100 to make in 
# the situation same as the customers who haven't met the Techprob for a long time

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

streamraw[is.na(streamraw)]=0

# From streamraw------>stream
stream=streamraw%>%
  select(-c("unlimitedText","ID","IDfamily"))

summary(stream)

set.seed(88888)
trainID=sample(1:677933,550000)
train=stream[trainID,]
validate=stream[-trainID,]
# Cleaning the data-------------------------------------------------------------------------------------------

mod1=glm(churnIn3Month~.,family="binomial", data=train)
rg1_pro=glm(churnIn3Month~.,family="binomial", data=train%>%filter(promo==1)%>%select(-c("promo")))
rg1_nopro=glm(churnIn3Month~.,family="binomial", data=train%>%filter(promo==0)%>%select(-c("promo")))

summary(mod1)
p1=predict(mod1,newdata=validate,type="response")
cbind(p1,validate)[sort.list(p1,decreasing=TRUE)[1:30],]
cbind1=cbind(p1,validate)[sort.list(p1,decreasing=TRUE)[1:10000],]
predict_correction_p1=sum(cbind1$churnIn3Month)/10000

#-Processing the testing data-------------------------------------------------------------------------------------------------------

streamscore=read.csv("Retention-score-fixed.csv")
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

streamscore[is.na(streamscore)]=0

streamscore=streamscore%>%
  mutate(isWorkPhone=as.factor(isWorkPhone), unlimitedVoice=as.factor(unlimitedVoice), 
         unlimitedText=as.factor(unlimitedText))

p1_score=predict(mod1,newdata=streamscore,type="response")
p1_score_cbind=cbind(p1_score,streamscore)
p1_score_cbind=p1_score_cbind%>%
  mutate(expectation_value_p1=(baseMonthlyRateForPlan+(baseMonthlyRateForPhone+cashDown+phonePrice+phoneBalance)^0.5)*p1_score)
p1_score_cbind=p1_score_cbind[sort.list(p1_score_cbind$expectation_value_p1,decreasing=TRUE)[1:nrow(p1_score_cbind)],]
p1_score_cbind=p1_score_cbind%>%
  filter(Freq<=1)

score_nopro=predict(rg1_nopro,newdata=streamscore,type="response")
score_pro=predict(rg1_pro,newdata=streamscore,type="response")
evaluation_score=2*score_nopro-score_pro-0.6
evaluation_score_cbind=cbind(evaluation_score,streamscore)
evaluation_score_cbind=evaluation_score_cbind[sort.list(evaluation_score_cbind$evaluation_score,decreasing=TRUE)[1:797821],]%>%
  filter(nbAdultAvg==1)




