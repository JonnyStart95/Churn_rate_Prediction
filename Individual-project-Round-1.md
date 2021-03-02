---
output:
  html_document: 
    keep_md: yes
    df_print: paged
  pdf_document: default
---
### Retention Data and Customer Intelligence-Round 1
##### MINGLIANG WEI(11274244)
#### Starting Strategy


Our strategy is to find the people who has the most largetst possiblity to leave and invite them to prevent them from leaving.Firstly, I have checked the data and find out that there are so many missing values there. To avoid missing values which will cause a misleading to the regression, we will give the values of the NA obeying the characteristic of those variables who have missing values.(Average, Maximum,Medium based on the real business meaning environment) and changing those factors varibales into factor format.(e.g.)


```r
streamraw$timeSinceLastTechProb[is.na(streamraw$timeSinceLastTechProb)]=100
streamraw$minutesVoice[is.na(streamraw$minutesVoice)]=200
```

Set the artificial variable 'Freq' which means the number of people who are using the plan in each single family to simulate the factor of conformity behavior, summary is shown as follows:


```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   1.000   1.000   1.163   1.000   5.000
```


Seperate the data into the training set and the testing set and do the binary regression to get mod1, then predict the testing data based on the model we get.

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["p1"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["nbAdultAvg"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["chrono"],"name":[3],"type":["int"],"align":["right"]},{"label":["age"],"name":[4],"type":["int"],"align":["right"]},{"label":["gender"],"name":[5],"type":["fctr"],"align":["left"]},{"label":["isWorkPhone"],"name":[6],"type":["fctr"],"align":["left"]},{"label":["planType"],"name":[7],"type":["fctr"],"align":["left"]},{"label":["data"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["dataAvgConsumption"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["nbrIsOverData"],"name":[10],"type":["int"],"align":["right"]},{"label":["timeSinceLastIsOverData"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["unlimitedVoice"],"name":[12],"type":["fctr"],"align":["left"]},{"label":["minutesVoice"],"name":[13],"type":["dbl"],"align":["right"]},{"label":["voiceAvgConsumption"],"name":[14],"type":["dbl"],"align":["right"]},{"label":["nbrIsOverVoice"],"name":[15],"type":["int"],"align":["right"]},{"label":["timeSinceLastIsOverVoice"],"name":[16],"type":["dbl"],"align":["right"]},{"label":["textoAvgConsumption"],"name":[17],"type":["dbl"],"align":["right"]},{"label":["phonePrice"],"name":[18],"type":["dbl"],"align":["right"]},{"label":["cashDown"],"name":[19],"type":["dbl"],"align":["right"]},{"label":["phoneBalance"],"name":[20],"type":["dbl"],"align":["right"]},{"label":["baseMonthlyRateForPlan"],"name":[21],"type":["dbl"],"align":["right"]},{"label":["baseMonthlyRateForPhone"],"name":[22],"type":["dbl"],"align":["right"]},{"label":["timeSinceLastTechProb"],"name":[23],"type":["dbl"],"align":["right"]},{"label":["nbrTechnicalProblems"],"name":[24],"type":["int"],"align":["right"]},{"label":["timeSinceLastComplaints"],"name":[25],"type":["dbl"],"align":["right"]},{"label":["nbrComplaints"],"name":[26],"type":["int"],"align":["right"]},{"label":["lifeTime"],"name":[27],"type":["int"],"align":["right"]},{"label":["churnIn3Month"],"name":[28],"type":["int"],"align":["right"]},{"label":["Freq"],"name":[29],"type":["int"],"align":["right"]}],"data":[{"1":"0.1241081","2":"4","3":"115","4":"33","5":"F","6":"0","7":"bring","8":"9","9":"4.455","10":"0","11":"80","12":"1","13":"200","14":"57.708","15":"0","16":"30","17":"478.232","18":"0","19":"91.16","20":"0","21":"59.3","22":"0","23":"100","24":"0","25":"100","26":"0","27":"3","28":"1","29":"4","_rn_":"624694"},{"1":"0.1240167","2":"4","3":"117","4":"27","5":"F","6":"0","7":"bring","8":"7","9":"2.185","10":"0","11":"80","12":"1","13":"200","14":"30.181","15":"0","16":"30","17":"267.013","18":"0","19":"152.84","20":"0","21":"53.9","22":"0","23":"100","24":"0","25":"100","26":"0","27":"1","28":"1","29":"4","_rn_":"666979"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>
Adding predict_correction_p1 as our index showing the prediction correction rate of our model.


```
## [1] 0.0712
```

We can get the mod2 by using the stepwise of mod1 and by considering the correlations between variables, we can get mod3, after comparing the AUC between all the models, mod1 has the best performence so we choose it as our optimal model. But the prediction correction rate for our optimal model is too small as being a good binory model,after checking the previous dataset. We can get the leaving rate for all the clients.



```r
leaving_rate
```

```
## [1] 0.02714581
```
#### Modifying Strategy
a.We find out that only 2.7% percentage of people will leave in three months, and the most largest possibiliy of mod1 is 12.4% combing the largest 7.12% prediction correction rate from all the models,which means that we don't have a big confidence to find out those who will leave in 3 months.
b.Plus we are not sure if we invite them to come to our dinner event will help to change their mind from leaving, to remedy the weakness of our model, we modify our strategy from inviting those who has the largest possibilities to leave to the modified strategy that finding the expectation money we will lost for each person, it means that we will take the potential value of each customer into consideration.     
c.We will use the equations as follows to caculate the potential value of each customer:   

$PotentialValue=baseMonthlyRateForPlan+(baseMonthlyRateForPhone+cashDown+phonePrice+phoneBalance)^{1/2}$  
$ExpectationLossingValue=PotentialValue*Probability$




```r
p1_score_cbind=p1_score_cbind%>%
  mutate(expectation_value_p1=(baseMonthlyRateForPlan+(baseMonthlyRateForPhone+cashDown+phonePrice+phoneBalance)^0.5)*p1_score)
p1_score_cbind=p1_score_cbind[sort.list(p1_score_cbind$expectation_value_p1,decreasing=TRUE)[1:nrow(p1_score_cbind)],]%>%filter(Freq<=1)
```
Processing the score data and use the equation we mentioned above to predict the expectation money we will lost for each person and find the largest 8000 ones.  
We will filter those people whose Freq is 1 because based on the rule of conformity behavior, the one who has the least degree pf conformity behavior will more likely to leave and then get our results.

