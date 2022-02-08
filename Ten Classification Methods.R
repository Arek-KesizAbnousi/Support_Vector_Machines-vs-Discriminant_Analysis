#First, we will program the R function to perform classification using LDA, QDA, RDA, and
#Naïve Bayes.
classification_f <-
  function(newdata,y,X,method=c("LDA","QDA","RDA","NaiveBayes"),alpha=NA)
  {
    classes <- unique(y)
    classes
    m <- array(0,dim = c(length(classes),ncol(X)))
    s_Indiv <- array(0,dim = c(length(classes),ncol(X),ncol(X)))
    s_combined <- cov(X)
    problity <- summary(y)/length(y)
    for(i in 1:length(classes))
    {
      class_C <- X[which(y==classes[i]),]
      m[i,] <- colMeans(class_C)
      s_Indiv[i,,] <- cov(class_C)
    }
    class_pred <- y[1:nrow(newdata)]
    score <- array(dim=length(classes))
    newdata <- as.matrix(newdata)
    for(i in 1:nrow(newdata))
    {
      if(method=="LDA")
      {
        for(j in 1:length((classes)))
        {
          score[j] <- newdata[i,]%*%solve(s_combined)%*%m[j,]-
            (0.5*t(m[j,])%*%solve(s_combined)%*%m[j,]) + log(problity[j])
        }
        class_pred[i] <- classes[which.max(score)]
      }
      if(method=="QDA")
      {
        for(j in 1:length((classes)))
        {
          score[j] <- (-0.5*log(det(s_Indiv[j,,])))-
            (0.5*(t(newdata[i,])-m[j,])%*%solve(s_Indiv[j,,])%*%t(t(newdata[i,])-m[j,]))
          + log(problity[j])
        }
        class_pred[i] <- classes[which.max(score)]
      }
      if(method=="RDA")
      {
        for(j in 1:length((classes)))
        {
          score[j] <- (-0.5*log(det((alpha*s_Indiv[j,,])+((1-alpha)*s_combined))))-
            (0.5*t(newdata[i,]-m[j,])%*%solve((alpha*s_Indiv[j,,])+((1-alpha)*s_combined))
             %*%(newdata[i,]-m[j,]))
          + log(problity[j])
        }
        class_pred[i] <- classes[which.max(score)]
      }
      if(method=="NaiveBayes")
      {
        for(j in 1:length((classes)))
        {
          score[j] <- newdata[i,]%*%solve(diag(diag(s_combined),ncol(X),ncol(X)))%*%m[j,]-
            (0.5*t(m[j,])%*%solve(diag(diag(s_combined),ncol(X),ncol(X)))%*%m[j,])
          +log(problity[j])
        }
        class_pred[i] <- classes[which.max(score)]
      }
    }
    return(class_pred)
  }




# Now, we will use the mentioned methods and compare their performances in classification on #a specific dataset named Sonar.

library(mlbench)
data(Sonar)
dim(Sonar)
levels(Sonar$Class)
head(Sonar)

X <- log(Sonar[,1:60] + 1) # predictors
y <- Sonar$Class



# The Accuracy Matrix ( Resulting Table)

library("e1071")
Resulting_Table <- matrix(0,nrow = 100,ncol = 10)
for(i in 1:100)
{
  set <- sample(1:length(y), 158)
  Resulting_Table[i,1] <- mean(classification_f(X[-set,],y[set],X[set,],method="LDA")==y[-set])
  Resulting_Table[i,2] <- mean(classification_f(X[-set,],y[set],X[set,],method="QDA")==y[-set])
  Resulting_Table[i,3] <- mean(classification_f(X[-set,],y[set],X[set,],method="RDA",alpha = 0)==y[-set])
  Resulting_Table[i,4] <- mean(classification_f(X[-set,],y[set],X[set,],method="RDA",alpha = 0.1)==y[-set])
  Resulting_Table[i,5] <- mean(classification_f(X[-set,],y[set],X[set,],method="RDA",alpha = 0.5)==y[-set])
  Resulting_Table[i,6] <- mean(classification_f(X[-set,],y[set],X[set,],method="RDA",alpha = 0.7)==y[-set])
  Resulting_Table[i,7] <- mean(classification_f(X[-set,],y[set],X[set,],method="RDA",alpha = 0.9)==y[-set])
  Resulting_Table[i,8] <- mean(classification_f(X[-set,],y[set],X[set,],method="NaiveBayes")==y[-set])
  Resulting_Table[i,9] <- mean(predict(svm(X[set,],y[set],kernel="linear"),X[-set,])==y[-set])
  Resulting_Table[i,10] <- mean(predict(svm(X[set,],y[set],kernel="polynomial"),X[-set,])==y[-set])
}
