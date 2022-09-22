#college Admission project


setwd(choose.dir())

clg = read.csv(file.choose(),header= T)
View(clg)
str(clg)
clg1=clg

#Missing values
missingVal = sum(is.na(clg))
missingVal #no missing values

#changing datatype of target variable
clg$admit = as.factor(clg$admit)
clg$Gender_Male =as.factor(clg$Gender_Male)
clg$Race = as.factor(clg$Race)
clg$rank = as.factor(clg$rank)
clg$ses = as.factor(clg$ses)
clg$gre = as.numeric(clg$gre)
str(clg)


#outliers
boxplot(clg$gre)
out=boxplot.stats(clg$gre)$out
out_dat = which(clg$gre %in% c(out))
clg= clg[-out_dat,]
#After removing outliers in GRE
boxplot(clg$gre)

#outliers in GPA
boxplot(clg$gpa)
out1=boxplot.stats(clg$gpa)$out
out_dat1 = which(clg$gpa %in% c(out1))
clg = clg[-out_dat1,]

boxplot(clg[,-2])



#Distribution of the data
hist(clg$gre,col="GREEN", main="GRE")
hist(clg$gpa,col="PINK", main="GPA")

#Normalize the data
clg$gpa = scale(clg$gpa,center = TRUE,scale = TRUE)
clg$gre = scale(clg$gre,center = TRUE,scale = TRUE)
head(clg)


#splitting the data into training and test dataset
library(caTools)
set.seed(256)

split=sample.split(clg$admit,SplitRatio =0.8)
train.data= subset(clg,split=="TRUE")
test.data = subset(clg,split=="FALSE")
str(train.data)
str(test.data)
View(train.data)



#Logistic Regression

model1<-glm(admit~.,data=train.data,family='binomial')
summary(model1)

model2<-glm(admit~gpa+rank,data=train.data,family='binomial')
summary(model2)

model3<-glm(admit~gpa+gre,data=train.data,family='binomial')
summary(model3)

#Residual deviance is optimal with model2
#considering the significant variables 

admitPredict = predict(model2,test.data,type ="response")
test.data$admit1 = ifelse(admitPredict >0.5,1,0)

#confusion matrix
cf1= table(ActualValue=test.data$admit,
          PredictedValue= test.data$admit1)
#accuracy
accuracy1 = sum(diag(cf1))/sum(cf1)
accuracy1
#accuracy is high with model2 comparatively

########################################################
#SVM model
library(e1071)
svm.model = svm(admit~., 
                  data = train.data, kernel="linear",scale = T)
summary(svm.model)
head(test.data)
test.data= subset(test.data[-8])
p <-  predict(svm.model,test.data[-1],type="class")
p


#confusion matrix
cf2= table(ActualValue=test.data$admit,
           PredictedValue= p)
cf2

#accuracy
accuracy2 = sum(diag(cf2))/sum(cf2)
accuracy2
###########################################################

#KNN
library(caTools)
library(class)

knn.data=knn(train.data, test.data, train.data$admit, k=19)
knn.data

# Confusion Matrix
cf3 =table(test.data$admit, knn.data)
cf3

#Accuracy
accuracy3 =sum(diag(cf3))/sum(cf3)
accuracy3

misClassError = mean(knn.data != test.data$admit)
print(paste('Accuracy =', 1-misClassError))

##########################################################
#Naive bayes
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

NB_Model  = naive_bayes(admit~ gpa+rank,
                         data=train.data, usekernel = T)
summary(NB_Model)
#predict
#Prediction with probability
p  =  predict(NB_Model, 
               newdata = test.data, type = 'prob')
head(cbind(p,test.data))
View(p)

#prediction
p1 =  predict(NB_Model,test.data)

#confusion matrix
cf4 =  table(p1, test.data$admit)
cf4

#accuracy
accuracy4 =sum(diag(cf4))/sum(cf4)
accuracy4
######################################################
#Decission Tree
library(rpart)
library("rpart.plot")


nrow(train.data)
nrow(test.data)
0.03*nrow(train.data)
0.03*nrow(train.data)*3
r.cntrl = rpart.control(minsplit=26, minbucket=9,xval=5)
dec_clf = rpart(admit~.,control=r.cntrl,data=train.data)
rpart.plot(dec_clf)
summary(dec_clf)
predicted_val<-predict(dec_clf,test.data[-1],type="class")
predicted_val

#confusion_matrix
cf5<-table(predicted=predicted_val,actual=test.data$admit)
cf5

#accuracy
accuracy5<-sum(diag(cf5))/sum(cf5)
accuracy5

#####################################################

#KNN is the best model with accuracy of 88.6%

########################################################
#Categorize the grade point average into High, Medium,and Low  

head(clg1)
Descriptive=transform(clg1,Gre_ctg=ifelse(gre<440,"Low",
                      ifelse(gre<580,"Medium","High")))

Gre_ctg
View(Descriptive)
Sum_Desc=aggregate(admit~,Descriptive,FUN=sum)
length_Desc=aggregate(admit~Gre_ctg,Descriptive,FUN=length)
Probability_Table=cbind(Sum_Desc,Recs=length_Desc[,2])
Probability_Table_final=transform(Probability_Table,Probability_Admission=admit/Recs)


ggplot(Probability_Table_final,aes(x=Gre_ctg,y=Probability_Admission))+geom_point()

#cross grid for admission variable with GRE categorized

table(Descriptive$admit,Descriptive$Gre_ctg)
