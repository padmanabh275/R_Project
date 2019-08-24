getwd()
library(dplyr)
library (psych)
## Reading the project file
library (RCurl)
library(ggpubr)
library(tidyverse)
library(Hmisc)
library(corrplot)


x<-getURL("https://raw.githubusercontent.com/padmanabh275/R_Project/master/datasetnew.csv")
mydata<-read.csv(text = x)
View(mydata)
colnames(mydata)
mydata[,c("X","index")] <- list(NULL)
head(mydata)

## To check for Multicollinerity 

M<-cor(mydata)
head(round(M,2))
# method = "circle""
corrplot(M, method = "circle")

# method = "pie"
corrplot(M, method = "pie")

corrplot(M, method = "number")

# method = "color"
corrplot(M, method = "color")

# Checking to fit vif

names(mydata)
y <- paste("likes","Checkins","Returns","Category", "commBase",
           "comm24","comm48","comm24_1", "diff_24.48","baseTime",
           "length", "shares", "hrs","sun_pub","mon_pub",
           "tue_pub","wed_pub","thu_pub","fri_pub","sat_pub", 
           "sun_base","mon_base","tue_base","wed_base","thu_base",
           "fri_base", "sat_base", sep = "+")

form <- as.formula(paste("output ~", y))
formula<-as.formula(form)
# Feature Selection methods for data
library(mlbench)
library(caret)
varImp(fit)
# calculate correlation matrix
correlationMatrix <- cor(mydata[,1:8])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(output~., data=mydata, method="icr")
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)





set.seed(2)
s=sample(1:nrow(mydata),0.7*nrow(mydata)) #random selection of observations
newdata_train = mydata[s,] #70% observations
newdata_test=mydata[-s,] # remaining 30%


library(MASS)
fit<-lm(output~.,data=newdata_train)
step <- stepAIC(fit, direction="both")
step$anova # display results

fit1<-lm(output~.,data=newdata_test)
step <- stepAIC(fit1, direction="both")
step$anova # display results

## Final Model - Both Validation and Train Datasets
formula(fit)
formula(fit1)
final_fit<-glm(output ~ likes + Checkins + Returns + Category + commBase + comm24 + 
                 comm48 + comm24_1 + diff_24.48 + baseTime + length + shares + 
                 hrs + sun_pub + mon_pub + tue_pub + wed_pub + thu_pub + fri_pub + 
                 sat_pub + sun_base + mon_base + tue_base + wed_base + thu_base + 
                 fri_base + sat_base,family = "binomial",data = newdata_train)


# Other useful functions 
library(car)
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics
# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)


# Assessing Outliers
library(car)
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots 
av.Plots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mydata)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)

# Evaluate Collinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit)
# Ceres plots 
ceresPlots(fit)





##Remove predictor variables with VIF>5 from the train data



glm.model <- glm(formula = form,
                 data = mydata, 
                 family = binomial(link = "logit"),
                 x = TRUE)


