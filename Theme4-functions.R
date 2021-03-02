
confusion=function(truth,pred){
  # Truth: is a vector of 0,1 (or TRUE,FALSE) for the target variable (ideally) in a validation or test set.
  # pred: is a vector of predictions (coded 0,1 or TRUE,FALSE) for the same set, and in the same order
  # Output: Confusion matrix and fit statistics
  
  a=table(truth,pred,dnn=c("Truth","Prediction"))
  list(
    Confusion=addmargins(a, FUN = list(Total = sum), quiet = TRUE),
    Misclassification=1-sum(diag(a))/sum(a),
    Precision=a[2,2]/sum(a[,2]),
    Sensitivity=a[2,2]/sum(a[2,]),
    Specificity=a[1,1]/sum(a[1,])
  )
  
}


roc=function(truth,p,k=100,plot=TRUE,lines=FALSE,...){
  # Truth: is a vector of 0,1 (or TRUE,FALSE) for the target variable (ideally) in a validation or test set.
  # p: is a vector of predicted probabilites of getting 1 (or TRUE) for the same set, and in the same order
  # k: number of points at which the ROC curve will be evaluated
  # plot: plots the ROC curve (or not)
  # lines: add a line to an existing ROC plot
  # Output: (invisible means it will not be displayed on the console)
  #    ROC = list of points of the ROC curve
  #    AUC = Area under the curve
  
  Curve=rbind(c(0,0),
              t(sapply(quantile(p,(k:1)/(k+1)),function(th){a=confusion(truth,as.numeric(p>th));  c(1-a$Specificity,a$Sensitivity)})),
              c(1,1)
  )
  if(plot&!lines) plot(Curve,xlab="1-Specificity",ylab="Sensitivity",main="ROC curve",xlim=0:1,ylim=0:1,type="l",...)
  if(plot&lines) lines(Curve,...)
  invisible(list(ROC=Curve,AUC=sum(diff(Curve[,1])*(Curve[-1,2]+Curve[-nrow(Curve),2])/2)))
}

lift=function(truth,p,k=100,plot=TRUE,lines=FALSE,...){
  # Truth: is a vector of 0,1 (or TRUE,FALSE) for the target variable (ideally) in a validation or test set.
  # p: is a vector of predicted probabilites of getting 1 (or TRUE) for the same set, and in the same order
  # k: number of points at which the lift chart will be evaluated
  # plot: plots the lift chart (or not)
  # lines: add a line to an existing lift chart
  # Output: (invisible means it will not be displayed on the console)
  #     - list of points of the lift chart
  
  Curve=cbind((1:k)/k,c(sapply(quantile(p,((k-1):1)/k),function(th){confusion(truth,as.numeric(p>=th))$Precision})/mean(truth),1)
  )
  if(plot&!lines) plot(Curve,xlab="Depth",ylab="Cumulative Lift",main="Cumulative Lift Chart",xlim=0:1,type="l",...)
  if(plot&lines) lines(Curve,...)
  invisible(Curve)
}


cost=function(truth,p,cost=matrix(c(0,0,10,-1),2,2),k=100,plot=TRUE,...){
  # Truth: is a vector of 0,1 (or TRUE,FALSE) for the target variable (ideally) in a validation or test set.
  # p: is a vector of predicted probabilites of getting 1 (or TRUE) for the same set, and in the same order
  # cost: a matrix containing the costs for:
  #         matrix(c("Rightly predicting 0","Wrongly predicting 1","Wrongly predicting 1","Rightly predicting 1"),2,2)
  # k: number of points at which the cost will be evaluated
  # plot: plots the cost vs threshold (or not)
  # Output: (invisible means it will not be displayed on the console)
  #    Cost = list of thresholds and costs
  #    Min = Minimum cost achieved
  #    ThresholdMin = Threshold that leads to minimum cost
  
  quant=quantile(p,(1:(k-1))/k)
  Curve=c(   cost[1,2]*sum(truth==0)+cost[2,2]*sum(truth==1),
             sapply(quant,function(th){sum(confusion(truth,as.numeric(p>th))$Confusion[1:2,1:2]*cost)}),
             cost[1,1]*sum(truth==0)+cost[2,1]*sum(truth==1)
  )
  Min=min(Curve)
  ind=which.min(Curve)
  ThresholdMin=c(0,quant,1)[which.min(Curve)]
  
  if(plot){
    plot((k:0)/k,Curve,xlab="Depth",ylab="Cost",main="Cost Profile vs Threshold",sub=paste("Cost",Min,"at threshold",round(ThresholdMin,5)),xlim=0:1,type="l",...)
    abline(h=0,col="gray")
    points((k-ind+1)/k,Curve[ind],pch=20,lwd=2)
  }
  
  invisible(list(Cost=cbind(Depth=(k:0)/k,Cost=Curve),Min=Min,ThresholdMin=ThresholdMin))
}
