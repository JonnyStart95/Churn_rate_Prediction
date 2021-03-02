set.seed(20190927)
prob=c(.3195,.3120,.2705,.0603,.0189,.0188)
parti=c("PQ","PLQ","CAQ","QS","ON","Others")

# 10,000 repetitions of a poll with 1,000 respondants
# One matrix contains all the samples. Each column represents a different poll
sim=matrix(sample(parti,1000*10000,prob=prob,replace=TRUE),nrow=1000)
for(i in 1:6){
  assign(parti[i],apply(sim==parti[i],2,mean))
}

# a)
mean(PLQ>PQ)

