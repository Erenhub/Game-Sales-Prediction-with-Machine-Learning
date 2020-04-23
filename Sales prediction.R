setwd("C:\\Users\\Toshiba\\Desktop\\R")
sales<-read.csv("videogame.csv", sep = ";", dec = ",")

options(scipen=999)



attach(sales)
summary(sales)
head(sales)
#analysis of predictors
#x variable tells us we have 749 row
#name variable tells there are some names which repeats over time
#its because there are some varibles such as Platform and genre
#and it means we may have same rows with same name however this
#doesnt necesaarily mean they are same,their genre or platform may differ
#there are also some variables such as publisher,rating,
#user and critic scores and global sales,and lastly salelevel which is categoric
#
#ýn platform,genre,year and publisher variable
# we can check mean values and understand the disturibution
#of these variebles,for example genresimulation has the mean 
# 0.0227 while genresports is 0.112 means sport games far more
#popular than simulation games

install.packages("corrplot")

library(corrplot)
gg <- ggplot(sales, aes(x=User_Score, y=Global_Sales)) + 
  geom_point(aes(col=SaleLevel), size=2) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 10), ylim=c(0, 1)) + 
  labs(title="Global vide sales over user scores ", y="global sales", x="user score")
plot(gg)


my_data <- sales[,-c(1,2,41)]  #since rownumber and names has nothing to do with globalsales
#and salelevel is categoric i exclude this columns

corrx <- my_data[c(1,2,8,9,11,12,21,22,31,32,34,35,36,37,38)]
corry <- my_data[c(1,2,8,9,11,12,21,22,31,32,34,35,36,37,38)]

corr<- cor(corrx,corry)
summary(corr)
#seem like global sales has only correlation with critic score
#although userscore and critic score are corelated
#user score has no as much effect as critic scores
corrplot(corr, method="circle",type="upper")




training_data = my_data[1:500,]
testing_data = my_data[500:749,] 

#2 splitted two


lm.fit1=lm(Global_Sales~.,data=training_data)
 sm<-summary(lm.fit1)
 
 names(lm.fit1)
 coef(lm.fit1)
 coef(summary(lm.fit1))
 sm #summary of our lm model,easier to see here.
 # p value of model is 0.000000000022 so it is a good model
 # R-square is 0.4269 means its not bad,1 means perfect model
 # p value is < 0.0000000000000002 means we can reject null hypothesis
 # so our model is significant this model is appliable in real life
  #coeffecients
 #most of coeffecients have high p values and are not meaningful
 #to use in any regression,however we see Critic_Score 0.000000854398296277
 #Critic_Count                                    0.013409051032253660
 #User_Count                                      0.000000000006892331
 #User_Score  (0.0100) are valid 
 
 #estimate means if you increase by 1 unit it will affect globalsales... 
 
 
 #4 prediction comes into play
 
pred.lm=predict(lm.fit1,testing_data,
                        interval="confidence")

pred.lm[1:5] #if yield conditions in 1 row to 5 of all clomuns
summary(pred.lm) #it simply makes predictions
#by just putting new values of testing data in our linear
#model and try to predict how it will affect global sales
 
#mseMULTIPLE<- mean(sm$residuals^2)
#rmseMULTIPLE <- sqrt(mse) #this look for training to me

mseMUL<-mean((pred.lm-testing_data$Global_Sales)^2) # 1.54
rmseMUL <- sqrt(mseMUL) #1.24


#now ridge and lasso
#They shrink the beta coefficient towards zero for unimportant variables.
#ridge 

install.packages("glmnet")
library(glmnet)

x<-model.matrix(Global_Sales~.,my_data)
y<-my_data$Global_Sales


grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

set.seed(1)
cv.out=cv.glmnet(x[1:500,],y[1:500],alpha=0)
plot(cv.out)

bestlam=cv.out$lambda.min
bestlam

ridge.predict=predict(ridge.mod,s=bestlam,newx = x[500:749,])
sqrt(mean((ridge.predict-y[500:749])^2))


#0.944 is mse of our ridge model  rmse is 0.972
  predict(cv.out,type="coefficients",s=bestlam)[1:39,]
#coeffecient in ridge model
  

#lasso

grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)

set.seed(1)
cv.out=cv.glmnet(x[1:500,],y[1:500],alpha=1)
plot(cv.out)

bestlam=cv.out$lambda.min
bestlam

lasso.predict=predict(lasso.mod,s=bestlam,newx = x[500:749,])
 sqrt( mean((lasso.predict-y[500:749])^2))
 
 
#mse for lasso is 0.942 rmse is 0.970

predict(cv.out,type="coefficients",s=bestlam)[1:39,]

#decision trees
install.packages("tree")
library(tree)

#fit a tree based on training data
tree_sales=tree(Global_Sales~.,training_data)
plot(tree_sales)
text(tree_sales,pretty = 0)

tree_predict=predict(tree_sales,testing_data)
testing_global=Global_Sales[500:749]
mean((tree_predict-testing_global)^2) #1.78

#cross-validation for prune the tree
cv_tree=cv.tree(tree_sales)
plot(cv_tree$size,cv_tree$dev,
     type="b",
     xlab="tree size",
     ylab="mse")

which.min(cv_tree$dev)
cv_tree$size[2] # prune to the 7


prune_sales=prune.tree(tree_sales,best=8)
plot(prune_tree)
text(prune_tree,pretty=0)

#fitting tree

prune_tree=prune.tree(tree_sales,best=8)
yhat=predict(prune_tree,newdata=my_data[500:749,])
tree_test=my_data[500:749,"Global_Sales"]
sqrt(mean((yhat-tree_test)^2))
#after a trial i found mse 1.81 rmse is 1.34 for 7 terminal node
#it is better not to prune then (we have same results with first tree)
#which is mse is 1.78...and rmse 1.33












#randomforest-------------------






install.packages("randomForest")
library(randomForest)
set.seed(1)
bag.global=randomForest(Global_Sales~.,data=training_data
                        ,mtry=37,ntree=500,importance=TRUE)
  bag.global
#number of trees are already 500


#Predicting validation set
yhat.bag = predict(bag.global,newdata=sales[500:749,])
global_forest_test=sales[500:749,"Global_Sales"]
plot(yhat.bag, global_forest_test)
abline(0,1)
sqrt(mean((yhat.bag-global_forest_test)^2)) 

#usually we use mtry/3 according to our text book...

#when we use 37 variable mse is 0.96 almost half of tree rmse 0.98

install.packages("tuneRF")

tuneRF(training_data[,-38],training_data[,38],
       stepFactor = 1,
       plot = TRUE,
       ntreeTry=500,
       improve=0.05)

#Fitting random forest
set.seed(1)
rf.global=randomForest(Global_Sales~.,data=training_data,mtry=12,
                       importance=TRUE)
yhat.rf = predict(rf.global,newdata=sales[500:749,])
sqrt(mean((yhat.rf-global_forest_test)^2))  #mse was 1.03 and rmse was 1.01 when mtry was 37
# mtry is 12 here 
importance(rf.global)
varImpPlot(rf.global)

#apperantly user count,score and critis are more important


