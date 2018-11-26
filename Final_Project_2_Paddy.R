                     ## Project- Assignment

                    ## Padmanabh Bosamia

                     ## Answer to Question 1

## Setting the path for the directory 

getwd()

library(dplyr)
library (psych)
setwd("C:/Users/bdm/Documents/R")

                  ## Reading the project file

mydata=read.csv("bank-full.csv",header= TRUE, sep =";",stringsAsFactors = FALSE)
View(mydata)


                    ## Question 2
glimpse(mydata)

## finding out their mean,sd etc for age and balance

age_mean=mean(mydata$age)
age_std=sd(mydata$age)
age_summary=summary(mydata$age)
age_summary
class(age_summary)
names(age_summary)
age_1= age_summary["1st Qu."]
age_2= age_summary["3rd Qu."]
ageIQR= age_1 - age_2
ageIQR

balance_mean=mean(mydata$balance)
balance_std=sd(mydata$balance)
balance_summary=summary(mydata$balance)
balance_summary
class(balance_summary)
names(balance_summary)
balance_1= balance_summary["1st Qu."]
balance_2= balance_summary["3rd Qu."]
balanceIQR= balance_1 - balance_2
balanceIQR

## Normal distribution of Age and Balance in : Bank - full.csv
library(ggplot2)
qqline=(mydata$age)
qqnorm(mydata$age)
hist(mydata$age,  20, col="red")

qqnorm(mydata$balance)
qqline(mydata$balance)
hist(mydata$balance,10, col = "green")

##Age follows normal distribution but, balance does not
## Finding out outlier on the distribution
upperage = age_mean + 3*age_std
lowerage = age_mean - 3*age_std
upperbalance = balance_2 + 1.5*balance_std
lowerbalance = balance_1 - 1.5*balance_std
upperage
lowerage
upperbalance
lowerbalance

#finding no of observations lie above or below outlier limits
sum(mydata$age< 9.07)
sum(mydata$age > 72.79)
sum(mydata$balance < -4495)
sum(mydata$balance >5995)

#removing obserevations that lie outside limits for age and balance

mydata1 = subset(mydata, !(age >72.79 | age < 9.07 | balance > 5995 | balance < -4495))
summary(mydata1)
mydata1


## Question 3 - Percentage cross table for job & y and Month and Y seperatel

library(dplyr)
table(mydata1$job)
table(mydata1$y)
table1= round(prop.table(table(mydata1$job,mydata1$y),1),2)
table1

## Reducing categories on job
glimpse(mydata1$job)
mydata1= mydata1 %>%
  mutate(job_1=as.numeric(job %in% c("entrepreneur","housemaid")),
         job_2 =as.numeric(job=="unknown"),
         job_3=as.numeric(job %in% c("admin","blue-collar")),
         job_4= as.numeric(job=="management"),
         job_5= as.numeric(job=="services"),
         Job_6= as.numeric(job==c("retired","unemployed","self-employed"))) %>%
select(-job)
# creating cross percertage for month and y 
table(mydata1$month)
table2=round(prop.table(table(mydata1$month,mydata1$y),1),2)
table2
#reducing no of levels for months
mydata1=mydata1 %>%
  mutate(qt1_month=as.numeric(month %in% c("apr","feb","aug")),
         qt2_month=as.numeric(month=="may"),
         qt3_month=as.numeric(month %in% c("jan","jul","jun")),
         qt4_month=as.numeric(month %in% c("dec","mar","sep","oct")))%>%
select(-month)

##pi chart for variable Education and Y as fill Question 5

library (ggplot2)
table(mydata1$education)
pie=ggplot(mydata1, aes(x = mydata1$education, fill = factor(mydata1$y))) + geom_bar(width = 2)
pie + coord_polar(theta = "y")

## Bonus Question - Identifying the variables as categories and creating
## dummy variables from the function as numeric

for (i in 1:ncol(mydata1)) {
  if (class(mydata1[,i])=="character"){
    if (names(mydata1)[i]!="y"){
      message = paste("No of categories in variable", names(mydata1)[i], ":")
      number = length(unique(mydata1 [,i]))
      print(paste0(message,number))
    }
  }
}

## Creating dummy variables for categorical variables - Question 6 - 

head(mydata1,1)
table (mydata1$education)

mydata1=mydata1%>%
  mutate(edu_1=as.numeric(education =="primary"),
         edu_2=as.numeric(education == "secondary"),
         edu_3=as.numeric(education =="tertiary")) %>%

  select (-education)
##  glimpse(mydata1)
  
table (mydata1$default) 
mydata1=mydata1%>%
  mutate(def_1=as.numeric(default =="no"))%>%
  select (-default)
  
  
table (mydata1$housing)
mydata1=mydata1%>%
  mutate(hou_1=as.numeric(housing =="yes"))%>%
  select (-housing)

table (mydata1$loan)  
mydata1=mydata1%>%
  mutate(loa_1=as.numeric(loan=="no"))%>%
  select (-loan)
  glimpse(mydata1)
  
table (mydata1$contact)
mydata1=mydata1%>%
  mutate(con_1=as.numeric(contact=="cellular"),
         con_2=as.numeric(contact=="telephone"))%>%
  select (-contact)
  glimpse(mydata1)
  
head(mydata1$marital,1)
mydata1=mydata1%>%
  mutate(mar_1=as.numeric(marital=="single"),
          mar_2=as.numeric(marital=="married"))%>%
  select(-marital) 

glimpse(mydata1)
table (mydata1$poutcome)
mydata1=mydata1%>%
  mutate (pou_1=as.numeric(poutcome=="failure"),
          pou_2=as.numeric(poutcome=="success"),
          pou_3=as.numeric(poutcome=="unknown"))%>%
  select(-poutcome)%>%
  na.omit()

glimpse(mydata1)
table(mydata1$y)
mydata1=mydata1%>%
  mutate (y=ifelse(y=="no",0,1),
          y=as.numeric(y))
  glimpse(mydata1)
  

# Project 2 - Padmanabh 
#Answer 1 - Break your data in to random parts train and test, train should contina 70% of the observation. Make
#sure this random sampling is reproducible.
set.seed(2)
s=sample(1:nrow(mydata1),0.7*nrow(mydata1)) #random selection of observations
newdata_trainval = mydata1[s,] #70% observations
newdata_test=mydata1[-s,] # remaining 30%
s1=sample(1:nrow(newdata_trainval),0.7*nrow(newdata_trainval))
mydata1_train=newdata_trainval[s,]
mydata1_train1=newdata_trainval[-s1,]
glimpse(mydata1_train)
na.omit(mydata1_train)
##Answer 2 - Remove predictor variables with VIF>5 from the train data
library (car)
p=lm(y~.,data=mydata1_train)
glimpse(mydata1_train)
vif(p)
formula(p)
## Step by step removal of a variable based on VIF>5 
p=glm(y ~ age + balance + day + duration + campaign + pdays + previous + 
        job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
        qt2_month + qt3_month + qt4_month + edu_1 + edu_2 + edu_3 + 
        def_1 + hou_1 + loa_1 + con_1 + con_2 + mar_1 + mar_2 + pou_1 + 
        pou_2 + pou_3,family = "binomial",data = mydata1_train)
summary(p)

p=glm(y ~ age + balance + day + duration + campaign + pdays + previous + 
        job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
        qt2_month + qt3_month + qt4_month + edu_1 + edu_2 + edu_3 + 
        def_1 + hou_1 + loa_1 + con_1 + con_2 + mar_1 + pou_1 + 
        pou_2 + pou_3,family = "binomial",data = mydata1_train)
summary(p)

p=glm(y ~ age + balance + day + duration + campaign + previous + 
        job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
        qt2_month + qt3_month + qt4_month + edu_1 + edu_2 + edu_3 + 
        def_1 + hou_1 + loa_1 + con_1 + con_2 + mar_1 + pou_1 + 
        pou_2 + pou_3,family = "binomial",data = mydata1_train)
summary(p)

p=glm(y ~ age + balance + day + duration + campaign + previous + 
        job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
        qt2_month + qt3_month + qt4_month + edu_1 +  edu_3 + 
        def_1 + hou_1 + loa_1 + con_1 + con_2 + mar_1 + pou_1 + 
        pou_2 + pou_3,family = "binomial",data = mydata1_train)
summary(p)

p=glm(y ~ age + balance + duration + campaign + previous + 
        job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
        qt2_month + qt3_month + qt4_month + edu_1 +  edu_3 + 
        def_1 + hou_1 + loa_1 + con_1 + con_2 + mar_1 + pou_1 + 
        pou_2 + pou_3,family = "binomial",data = mydata1_train)
summary(p)

p=glm(y ~ age + balance + duration + campaign + previous + 
        job_1 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
        qt2_month + qt3_month + qt4_month + edu_1 +  edu_3 + 
        def_1 + hou_1 + loa_1 + con_1 + con_2 + mar_1 + pou_1 + 
        pou_2 + pou_3,family = "binomial",data = mydata1_train)
summary(p)

p=glm(y ~ age + balance + duration + campaign + previous + 
        job_1 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
        qt2_month + qt3_month + qt4_month + edu_1 +  edu_3 + 
        hou_1 + loa_1 + con_1 + con_2 + mar_1 + pou_1 + 
        pou_2 + pou_3,family = "binomial",data = mydata1_train)
summary(p)

p=glm(y ~ age + balance + duration + campaign + previous + 
        job_1 + job_3 + job_5 + Job_6 + qt1_month + 
        qt2_month + qt3_month + qt4_month + edu_1 +  edu_3 + 
        def_1 + hou_1 + loa_1 + con_1 + con_2 + mar_1 + pou_1 + 
        pou_2 + pou_3,family = "binomial",data = mydata1_train)
summary(p)

p=glm(y ~ age + balance + duration + campaign + previous + 
        job_1 + job_3 + job_5 +  qt1_month + 
        qt2_month + qt3_month + qt4_month + edu_1 +  edu_3 + 
        def_1 + hou_1 + loa_1 + con_1 + con_2 + mar_1 + pou_1 + 
        pou_2 + pou_3,family = "binomial",data = mydata1_train)
summary(p)

p=glm(y ~ age + balance + duration + campaign + previous + 
        job_1 + job_3 + job_5 +  qt1_month + 
        qt2_month + qt3_month + qt4_month + edu_1 +  edu_3 + 
        hou_1 + loa_1 + con_1 + con_2 + mar_1 + pou_1 + 
        pou_2 + pou_3,family = "binomial",data = mydata1_train)
summary(p)

p=glm(y ~ age + balance + duration + campaign + previous + 
        job_1 + job_3 + job_5 +  qt1_month + 
        qt2_month + qt3_month + qt4_month + edu_1 +   
        hou_1 + loa_1 + con_1 + con_2 + mar_1 + pou_1 + 
        pou_2 + pou_3,family = "binomial",data = mydata1_train)
summary(p)

## Answer 3 - Building logistic regression model for the response y with the remaining variables
fit=step(p)# function to remove insignificant variables
formula (fit)
## fitting model to train datasets
fit1=glm(y ~ age + balance + duration + campaign + previous + job_1 + 
           job_3 + job_5 + qt1_month + qt2_month + qt3_month + qt4_month + 
           edu_1 + hou_1 + loa_1 + con_1 + con_2 + mar_1 + pou_1 + pou_2 + 
           pou_3, family = "binomial",data = mydata1_train)


##finding fitting formula on validation datasets
fitval=glm(y~.,family = "binomial",data =mydata1_train1 )
summary(fitval)
formula(fitval)


fitval=glm(y ~ age + balance + day + duration + campaign + pdays + previous + 
             job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
             qt2_month + qt3_month + qt4_month + edu_1 + edu_2 + edu_3 + 
             def_1 + hou_1 + loa_1 + con_1 + con_2 + mar_1 + mar_2 + pou_1 + 
             pou_2 + pou_3,family="binomial",data=mydata1_train1)

fitval=glm(y ~ age + balance + day + duration + campaign + previous + 
             job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
             qt2_month + qt3_month + qt4_month + edu_1 + edu_2 + edu_3 + 
             def_1 + hou_1 + loa_1 + con_1 + con_2 + mar_1 + mar_2 + pou_1 + 
             pou_2 + pou_3,family="binomial",data=mydata1_train1)
summary(fitval)

fitval=glm(y ~ age + balance + day + duration + campaign + previous + 
             job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
             qt2_month + qt3_month + qt4_month + edu_1 + edu_2 + edu_3 + 
             def_1 + hou_1 + loa_1 + con_1 + con_2 + mar_1 + mar_2 +  
             pou_2 + pou_3,family="binomial",data=mydata1_train1)
summary(fitval)
fitval=glm(y ~ age + balance + day + duration + campaign + previous + 
             job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
             qt2_month + qt3_month + qt4_month + edu_1 + edu_2 + edu_3 + 
             def_1 + hou_1 + loa_1 + con_1 + con_2 +  mar_2 +  
             pou_2 + pou_3,family="binomial",data=mydata1_train1)
summary(fitval)

fitval=glm(y ~ age + balance + day + duration + campaign + previous + 
             job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
             qt2_month + qt3_month + qt4_month + edu_1 + edu_3 + 
             def_1 + hou_1 + loa_1 + con_1 + con_2 +  mar_2 +  
             pou_2 + pou_3,family="binomial",data=mydata1_train1)
summary(fitval)

fitval=glm(y ~ balance + day + duration + campaign + previous + 
             job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
             qt2_month + qt3_month + qt4_month + edu_1 + edu_3 + 
             def_1 + hou_1 + loa_1 + con_1 + con_2 +  mar_2 +  
             pou_2 + pou_3,family="binomial",data=mydata1_train1)
summary(fitval)

fitval=glm(y ~ balance + day + duration + campaign + previous + 
             job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
             qt2_month + qt3_month + qt4_month + edu_1 + edu_3 + 
             hou_1 + loa_1 + con_1 + con_2 +  mar_2 +  
             pou_2 + pou_3,family="binomial",data=mydata1_train1)
summary(fitval)
fitval=glm(y ~ balance + day + duration + campaign + previous + 
             job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
             qt3_month + qt4_month + edu_1 + edu_3 + 
             hou_1 + loa_1 + con_1 + con_2 +  mar_2 +  
             pou_2 + pou_3,family="binomial",data=mydata1_train1)
summary(fitval)
fitval=glm(y ~ balance + duration + campaign + previous + 
             job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
             qt3_month + qt4_month + edu_1 + edu_3 + 
             hou_1 + loa_1 + con_1 + con_2 +  mar_2 +  
             pou_2 + pou_3,family="binomial",data=mydata1_train1)
summary(fitval)
fitval=glm(y ~ balance + duration + campaign +  
             job_1 + job_2 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
             qt3_month + qt4_month + edu_1 + edu_3 + 
             hou_1 + loa_1 + con_1 + con_2 +  mar_2 +  
             pou_2 + pou_3,family="binomial",data=mydata1_train1)
summary(fitval)
fitval=glm(y ~ balance + duration + campaign +  
             job_1 + job_3 + job_4 + job_5 + Job_6 + qt1_month + 
             qt4_month + edu_1 + edu_3 + 
             hou_1 + loa_1 + con_1 + con_2 +  mar_2 +  
             pou_2 + pou_3,family="binomial",data=mydata1_train1)
summary(fitval)
fitval=glm(y ~ balance + duration + campaign +  
             job_1 + job_3 + job_4 + Job_6 + qt1_month + 
             qt4_month + edu_3 + 
             hou_1 + loa_1 + con_1 + con_2 +  mar_2 +  
             pou_2 + pou_3,family="binomial",data=mydata1_train1)
summary(fitval)

fitval=step(fitval)

formula(fitval)

## Final Model - Both Validation and Train Datasets
formula(fit)
formula(fitval)
final_fit=glm(y ~ balance + duration + campaign +job_1 + job_3 + qt4_month + hou_1 
              + loa_1+ con_1 + con_2 + pou_2 + pou_3,family = "binomial",data = mydata1_train)
summary(final_fit)
mydata1_train$score=predict(fitval,newdata = mydata1_train,type = "response")
View(mydata1_train$score)
newdata_final=na.omit(mydata1_train)
(newdata_final$score)

##Answer 4 - Visualising how binary responses are behaving with respect to score
library(ggplot2)
ggplot (newdata_final,aes(y=y,x=score,color=factor(y)))+
  geom_point()+geom_jitter()
cutoff=0.2
predicted=as.numeric(newdata_final$score>cutoff)
TP=sum(predicted==1 & newdata_final$y==1)
FP=sum(predicted==1 & newdata_final$y==0)
FN=sum(predicted==0 & newdata_final$y==1)
TN=sum(predicted==0 & newdata_final$y==0)
P=TP+FN
N=TN+FP
total=P+N
cutoff_data=data.frame(cutoff=0,TP=0,FP=0,TN=0)
cutoffs=seq(0,1,length=100)
for (cutoff in cutoffs){
  predicted=as.numeric(newdata_final$score>cutoff)
  TP=sum(predicted==1 & newdata_final$y==1)
  FP=sum(predicted==1 & newdata_final$y==0)
  FN=sum(predicted==0 & newdata_final$y==1)
  TN=sum(predicted==0 & newdata_final$y==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}
cutoff_data=cutoff_data[-1,] # removing dummy from top row
## Now using KS Method
library (dplyr)
cutoff_data=cutoff_data %>%
  mutate(P=TP+FN,N=TN+FP) %>%
  mutate(Sn=TP/P,Sp=TN/N,dist=sqrt((1-Sn)**2+(1-Sp)**2),P=FN+TP,N=TN+FP)%>%
  mutate(KS=abs((TP/P)- (FP/N))) %>%
  select(-P,-N) 
    
library(tidyr)
cutoff_viz=cutoff_data %>%
  select(cutoff,Sn,Sp,dist,KS)%>%
  gather(Criterion,Value,Sn:KS)
## Finding scores on the test
newdata_final$score=predict(final_fit,newdata = newdata_final,type = "response")
## Calculating cutoff using KS method and checking performance of the data
KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))][1]
KS_cutoff

table(newdata_final$y,as.numeric(newdata_final$score>KS_cutoff))
## Answer 5 Forest model on the same train data and report if performance of this model is better
##than logistic regression model on the test data. Get a variable importance plot.
library (randomForest)
class_rf=randomForest(y~.,data = newdata_final)
class_rf
forest.pred=predict(class_rf,newdata = newdata_final)
table(newdata_final$y,forest.pred)
importance(class_rf)
varImpPlot(class_rf)
## Top 6 variables with higest IncodePurity are shown below
## Duration, Balance, Campaign, Day, pdays, pou_2
## Answer 6 - Building logistic regression model with the top 6 variables mentioned above
top6_fit = glm (y ~ balance + duration + age + campaign + pdays + pou_2, family = "binomial",data = newdata_final)
summary(top6_fit)
newdata_final$score=predict(top6_fit,newdata = newdata_final,type = "response")
library(ggplot2)
ggplot(newdata_final,aes(y=y,x=score,color=factor(y)))+
  geom_point()+geom_jitter()
cutoff=0.2
predicted=as.numeric(newdata_final$score>cutoff)
TP=sum(predicted==1 & newdata_final$y==1)
FP=sum(predicted==1 & newdata_final$y==0)
FN=sum(predicted==0 & newdata_final$y==1)
TN=sum(predicted==0 & newdata_final$y==0)
P=TP+FN
N=TN+FP
total=P+N
cutoff_data=data.frame(cutoff=0,TP=0,FP=0,TN=0)
cutoffs=seq(0,1,length=100)
for (cutoff in cutoffs){
  predicted=as.numeric(newdata_final$score>cutoff)
  TP=sum(predicted==1 & newdata_final$y==1)
  FP=sum(predicted==1 & newdata_final$y==0)
  FN=sum(predicted==0 & newdata_final$y==1)
  TN=sum(predicted==0 & newdata_final$y==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}
cutoff_data=cutoff_data[-1,] # removing dummy from top row
## Now using KS Method
library (dplyr)
cutoff_data=cutoff_data %>%
  mutate(P=TP+FN,N=TN+FP) %>%
  mutate(Sn=TP/P,Sp=TN/N,dist=sqrt((1-Sn)**2+(1-Sp)**2),P=FN+TP,N=TN+FP)%>%
  mutate(KS=abs((TP/P)- (FP/N))) %>%
  select(-P,-N) 
library(tidyr)
cutoff_viz=cutoff_data %>%
  select(cutoff,Sn,Sp,dist,KS)%>%
  gather(Criterion,Value,Sn:KS)
## Finding scores on the test
newdata_final$score=predict(top6_fit,newdata = newdata_final,type = "response")
## Calcuating cutoff with KS method and checking performance
KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))][1]
KS_cutoff
## Performance on test data
table(newdata_final$y,as.numeric(newdata_final$score>KS_cutoff))
