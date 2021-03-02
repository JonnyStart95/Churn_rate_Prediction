streamraw=read.csv("StreamPromo.csv")
library(dplyr)  
library(tidyselect)
streamraw %>% nrow()
str(streamraw)     # List the column name and the value of each column
head(streamraw)    # Show the first rows of the dataset
streamraw=streamraw%>%        #  %>% to give the all the elements on left side to the right side
  mutate(revenue=as.factor(revenue), region=as.factor(region))  # change the type of the variable in the matrix
summary(streamraw)   # List the statistical characteristic of each column
sum(streamraw$weekslast==52)
nrow(streamraw)
streamraw %>%
  filter(streamraw$weekslast==52)%>%
  select(salesyear)%>%
  summary()

streamraw%>%
  mutate(missing_amount=is.na(amount))%>%
  select(missing_amount,sale)%>%
  table()
  
 stream=streamraw%>%
   filter(train==1)%>%
   select(-c("amount","train","test"))
  
  
  set.seed(20600)  # Make sure if you use the sample to get the same radom number as this one. 20600 is just a index.
  trainID=sample(1:1000,700)  #1:1000 is the range of the random number, 700 is how many numbers we need.
  
  train=stream[trainID,]
  validate=stream[-trainID,]
  
  mod1=glm(sale~.,family ="binomial", data=train)   #Using the train data to find the linear regression of sale, binomial family
  summary(mod1)    # Using the regression to estimate the probability if the person's sale will be 1.
  
  p1=predict(mod1, newdata = validate, type = "response")   # Making the probability predict based on the module and validate data
  cbind(pl, validate)[sort.list(pl,decreasing = TRUE)[1:10],]
  
#ROC curve and AUC#

#full model
#stepwise selection from full model
#full model with interactions and quadratic powers
#stepwise model with interactions and quadratic powers


mod2=step(mod1, trace = FALSE)   #stepwisde method to get the important variables

streamrawplus=streamraw %>%
  select(sale,amount,revenue,region,train,test,everything())

for(i in 7:14){
  for(j in i:14){
    streamrawplus = streamrawplus %>%
      mutate(!!paste(names(streamrawplus)[[i]],names(streamrawplus)[[j]],sep="_"):=as.numeric(streamrawplus[[i]])*streamrawplus[[j]])
  }
}
# Removing squared binary predictors because they are identical to their regular value  ???????

streamrawplus=streamrawplus%>%
  select(-female_female)    #Remove the column of female_female

streamplus=streamrawplus%>%
  filter(train==1)%>%
  select(-c(train,test,amount))
  
trainplus=streamplus[trainID,]
validplus=streamplus[-trainID,]     #Set the new validate and train dataset

mod3=glm(sale~., family = "binomial", data=trainplus) 
mod4=step(mod3, trace = FALSE)     # Then we can get another module, mod3 and mod4

source("Theme4-functions.R")
p2=predict(mod2, newdata = validate, type = "response") # Using the validate data to get prediction2
p3=predict(mod3,newdata = validplus,type= "response") # Using the validplus data and mod3 to get prediction3
p4=predict(mod4,newdata = validplus,type = "response") #Using validplus data and mod4 to get prediction4

roc(validplus$sale,p1)$AUC
roc(validplus$sale,p2,lines=TRUE,col="red")$AUC
roc(validplus$sale,p3,lines=TRUE,lty=2)$AUC
roc(validplus$sale,p4,lines=TRUE,col="red",lty=2)$AUC  

# Draw different lines for ROC curves for predictions using 4 different modules

# To double-confirm we have chosen the best module, we would like to use all the data we have right now.

streamtestplus=streamrawplus%>%
  filter(train==0)%>%
  select(-c("train","test"))

p1test=predict(mod1,newdata = streamtestplus,type = "response")
p2test=predict(mod2,newdata = streamtestplus,type = "response")
p3test=predict(mod3,newdata = streamtestplus,type = "response")
p4test=predict(mod4,newdata = streamtestplus,type = "response")

roc(streamtestplus$sale,p1test)$AUC
roc(streamtestplus$sale,p2test,lines=TRUE,col="red")$AUC
roc(streamtestplus$sale,p3test,lines=TRUE,lty=2)$AUC
roc(streamtestplus$sale,p4test,lines = TRUE,lty=2,col="red")$AUC

#The right balance between sensitivity and specificity depends on the relative cost of
#tendering an offer that is rejected
#not sending an offer that would have lead to a sale

lift(validplus$sale,p1)
lift(validplus$sale,p2,lines = TRUE, col="red")
lift(validplus$sale,p3,lines = TRUE, lty=2)
lift(validplus$sale,p3,lines = TRUE, col="red", lty=2)

# Draw the cumulative lift chart

# Note that the maximum of the lift chart is the reciprocal of target percentage

th4=cost(validplus$sale,p4,cost=matrix(c(0,0,2,-65),2,2),k=1000)$ThresholdMin

# Find the optimal threshold

all=streamtestplus%>%
  mutate(offer=1)%>%
  select(offer,sale, amount)%>%
  summarise_all(sum, na.rm=TRUE)%>%
  mutate(netamount=amount-2*offer)

notallinter=cbind(streamtestplus,p4test)%>%
  mutate(offer=1) %>%
  filter(p4test>th4) %>%  
  select(offer,sale,amount) %>%
  summarise_all(sum,na.rm=TRUE) %>%
  mutate(netamount=amount-2*offer)

cbind(all_clients=all, selected_clients_interactions=notallinter, difference=all-notallinter)

# Compare the difference between 100% sending the offer and sending the offer based on our own result.

par(mfrow=c(2,2))
cost(validplus$sale,p1,cost=matrix(c(0,0,2,-65),2,2))$Min
cost(validplus$sale,p2,cost=matrix(c(0,0,2,-65),2,2))$Min
cost(validplus$sale,p3,cost=matrix(c(0,0,2,-65),2,2))$Min
cost(validate$sale,bestp,cost=matrix(c(0,0,2,-65),2,2))$Min

#Compare 4 different threshold using 4 different forecasting models.

# Ways which can improve the performance of the models

# 1.trying more models – possibly all subsets – on the cost function
# 2.programming a stepwise method that looks at the cost to decide to add or remove variables

# Using the method2

keep=rep(TRUE,10)

bestmod=glm(sale~.,family="binomial",data=train)
bestkeep=keep
bestp=predict(bestmod,newdata=validate,type="response")
bestcost=cost(validate$sale,bestp,cost=matrix(c(0,0,2,-65),2,2),plot=FALSE)$Min
newbest=TRUE
while(sum(keep)>=2 & newbest){
  newbest=FALSE
  keep=bestkeep
  cat("\nBest model so far:",names(train)[bestkeep])
  for(i in (1:10)[keep]){
    cat(".")
    keep[i]=FALSE
    mod=glm(sale~.,family="binomial",data=train[,c(keep,TRUE)])
    p=predict(mod,newdata=validate,type="response")
    costmod=cost(validate$sale,p,cost=matrix(c(0,0,2,-65),2,2),plot=FALSE)$Min
    if(costmod<bestcost){
      newbest=TRUE
      bestmod=mod
      bestkeep=keep
    }  # Condition to justify if this column should be removed, if it is then we can jump out of the internal loop.
    keep[i]=TRUE
  }  # Internal loop to find out that which column should be removed
     
  
} # There will must be one column satify the condition and make the value of newbest=TRUE, if there is not, 
  #  then it can prove that the situation right now is the optimal one, we can jump out the outer loop

p=predict(bestmod,newdata=validate,type="response")
sol=cost(validate$sale,p,cost=matrix(c(0,0,2,-65),2,2),plot=TRUE)
sol$Min
# Get the best predition model and optimal cost and threshold

thbest=sol$ThresholdMin
bestmod
# Get the best cost and display the best model

mod2

ptest=predict(bestmod,newdata=streamtestplus,type="response")

th2=cost(validate$sale,p2,cost=matrix(c(0,0,2,-65),2,2),k=1000,plot=FALSE)$ThresholdMin

notallstepwise=cbind(streamtestplus,p2test)%>%
  mutate(offer=1) %>%
  filter(p2test>th2) %>%  
  select(offer,sale,amount) %>%
  summarise_all(sum,na.rm=TRUE) %>%
  mutate(netamount=amount-2*offer)


notallstepwisecost=cbind(streamtestplus,ptest)%>%
  mutate(offer=1) %>%
  filter(ptest>thbest) %>%  
  select(offer,sale,amount) %>%
  summarise_all(sum,na.rm=TRUE) %>%
  mutate(netamount=amount-2*offer)

rbind(all_clients=all,selected_clients_interactions=notallinter,selected_clients_regular_stepwise=notallstepwise,selected_clients_cost_stepwise=notallstepwisecost)

# Call all the best models we get from different kinds of methods we have used and compare them.

# Two-stage modelling

streamplus=streamrawplus %>%
  filter(train==1)%>%
  select(-c("train","test","sale"))

trainplus=streamplus[trainID,]
validplus=streamplus[-trainID,]

amt1=lm(amount~.,data = trainplus) 
# Function lm will dismiss the missing value of amount for those who didn't have a pay
amt2=step(amt1,trace = FALSE)

M=predict(amt2,newdata=validplus)   
ExpectedSale=p4*M     # M and p4 must have the same size, they are all vectors

sum(ExpectedSale[ExpectedSale>2])  # Get the sum of amount using two stage


Mtest=predict(amt2,newdata=streamtestplus) # Remember that if we wanna compare different models, we must use the same data set.  
ExpectTest=Mtest*p4test     

twostage=cbind(streamtestplus,ExpectTest)%>%
  mutate(offer=1) %>%
  filter(ExpectTest>2) %>%      # Since the cost of sending an offer is 2, so the expectation of amount must be over 2, if not we will lose money if we still send the offer
  select(offer,sale,amount) %>%
  summarise_all(sum,na.rm=TRUE) %>%    #negelect the NA in column
  mutate(netamount=amount-2*offer)

rbind(all_clients=all,selected_clients_interactions=notallinter,selected_clients_regular_stepwise=notallstepwise,selected_clients_cost_stepwise=notallstepwisecost,selected_clients_two_stage=twostage)









