####################
#### Basic Info ####
####################

##To import my dataset directly from the internet
data <- read.csv('https://raw.githubusercontent.com/shiranli/ECO-386-FInal-Project/main/Customer_Segmentation.csv')
##Checks the dimensions
dim(data)
##Shows the first 6 observations
head(data)
##Shows the last six observations.
tail(data)
##See the data in a summarized spreadsheet format in a new tab.
View(data)
##Generate the "six number summary" statistics
summary(data) 
##check the object class
class(data)



#############################
#### Data Clean & Graphs ####
#############################

##rename variables 
names(data)[1:10] <- c("customerid", "age", "education", "yearsemployed", "income", "carddebt"
                      , "otherdebt", "defaulted","address", "debtincomeratio")
str(data)

##create new dataset from starting point
##fill in empty data as NA
cleandata<-data
cleandata$defaulted[cleandata$defaulted!=0 | cleandata$inflation !=1]<-NA
View(cleandata)

##no data lost - same as dim(data)
dim(cleandata) 
str(cleandata)

##Plots
##install GGPLOT2 package
install.packages('ggplot2')

##load the GGPLOT2 library (MUST BE DONE EVERY TIME)
library(ggplot2)
library(plyr) 

##LOADING PREINSTALLED DATA FROM GGPLOT2 LIBRARY
data(cleandata) #LOADS THE DATA
cleandata #VIEW DATA IN CONSOLE OUTPUT
View(cleandata) #VIEW THE DATA IN LONG FORM
class(cleandata) #CHECK CLASS OF THE DATA

#LOADS THE MASS LIBRARY
library(MASS) 
library(mgcv)

##Distribution Graphs
#PLOTS A NONPARAMETRIC DENSITY CURVE
plot_a<-ggplot(cleandata,aes(x=age)) + geom_density()+
  xlab('Age Distribution') +  
  ylab('Probability Density') +
  ggtitle('Probability Density for Age')
plot_a
plot_c<-ggplot(cleandata,aes(x=yearsemployed)) + geom_density()+
  xlab('Years Employed') +  
  ylab('Probability Density') +
  ggtitle('Probability Density for Years Employed')
plot_c
plot_d<-ggplot(cleandata,aes(x=income)) + geom_density()+
  xlab('income per person') +  
  ylab('Probability Density') +
  ggtitle('Probability Density for Income')
plot_d
plot_f<-ggplot(cleandata,aes(x=carddebt)) + geom_density()+
  xlab('Card Debt') +  
  ylab('Probability Density') +
  ggtitle('Probability Density for Card Debt')
plot_f
plot_g<-ggplot(cleandata,aes(x=otherdebt)) + geom_density()+
  xlab('Other Debt') +  
  ylab('Probability Density') +
  ggtitle('Probability Density for Other Debt')
plot_g
plot_h<-ggplot(cleandata,aes(x=debtincomeratio)) + geom_density()+
  xlab('Debt Income Ratio') +  
  ylab('Probability Density') +
  ggtitle('Probability Density for Debt Income Ratio')
plot_h

##HISTOGRAMS AND FREQUENCY POLYGONS
ggplot(cleandata, aes(education)) + geom_histogram(binwidth = 0.5)
ggplot(cleandata, aes(defaulted)) + geom_histogram(binwidth = 0.05)

##create a new data set 
##and delete outliers in order to see relationship between income and other factors
cleandata1<-cleandata
cleandata1$income[cleandata1$income>200]<-NA
cleandata1$carddebt[cleandata1$carddebt>10]<-NA
cleandata1$debtincomeratio[cleandata1$debtincomeratio>30]<-NA

#CREATE A SCATTER PLOT USING THE POINT GEOMETRY
ggplot(cleandata1, aes(x = income, y = age))+
  geom_point(aes(color = "label"))+
  geom_smooth(method = "gam", formula = y ~ s(x))
ggplot(cleandata1, aes(income, yearsemployed))+ 
  geom_point(aes(color = "label"))+
  geom_smooth(method = "gam", formula = y ~ s(x))

##Plot the scatter plot with education levels and defaulted
## compare the relationship
ggplot(cleandata1, aes(x = income, y = carddebt))+
  geom_point(aes(color = "label"))+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  facet_grid(.~education, labeller = label_both)
ggplot(cleandata1, aes(x = income, y = debtincomeratio))+
  geom_point(aes(color = "label"))+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  facet_grid(.~defaulted, labeller = label_both)

#other graphs
ggplot(cleandata, aes(x = income, y = otherdebt)) + geom_violin(aes(fill=income)) 




#########################
#### Regression Task ####
#########################
library(tseries)

#INCORPORATING NONLINEAR AND LOGARITHMIC TRANSFORMATIONS OF INCOME
cleandata$yearsemployed2<-cleandata$yearsemployed^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
cleandata$yearsemployed3<-cleandata$yearsemployed^3 #CUBIC TRANSFORMATION 

#fraction of sample to be used for training
p<-.7

#number of observations (rows) in the dataframe
obs_count<-dim(cleandata)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(123)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

Training <- cleandata[train_ind, ] #pulls random rows for training
Testing <- cleandata[-train_ind, ] #pulls random rows for testing

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)

##BUILDING A LINEAR MODEL TO QUANTIFY THE RELATIONSHIP BETWEEN INCOME AND YEARS EMPLOYED##
M1<-lm(income~yearsemployed, Training)  #model: income = B_0+B_1(year employed)+e
summary(M1) #produces the summary output of the model
confint(M1) #returns upper and lower bounds from the 95% confidence interval for each model parameter
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(M1$fitted.values) #these are the same as the fitted values
M1$coefficients #RETURNS BETA ESTIMATES
hist(M1$residuals) #PLOT THEM!
jarque.bera.test(M1$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(M1, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$income)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$income)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

##BUILD A MULTIVARIATE MODEL TO PREDICT INCOME CONTROLLING FOR BOTH YEARS EMPLOYED AND AGE##
M2<-lm(income~yearsemployed+age, Training) #model: income = B_0+B_1(years employed)+B_2(age)+e
summary(M2)  #returns summary output for model M2
PRED_2_IN <- predict(M2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(M2$fitted.values) #these are the same as the fitted values
M2$coefficients #RETURNS BETA ESTIMATES
hist(M2$residuals) #PLOT THEM!
jarque.bera.test(M2$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(M2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$income)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$income)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

##BUILD A MULTIVARIATE MODEL TO PREDICT INCOME CONTROLLING FOR YEARS EMPLOYED, AGE AND CARD DEBT##
M3<-lm(income~yearsemployed+age+carddebt, Training) #adds card debt to the model M2
summary(M3)  #returns summary output for model M3
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(M3$fitted.values) #these are the same as the fitted values
M3$coefficients #RETURNS BETA ESTIMATES
hist(M3$residuals) #PLOT THEM!
jarque.bera.test(M3$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(M3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$income)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$income)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

##BUILD A MULTIVARIATE MODEL TO PREDICT INCOME CONTROLLING FOR BOTH EDUCATION AND AGE##
M4<-lm(income~age+education, Training) #build a regression using dummy (binary) variables controlling for education
summary(M4) #summary output from model M4
PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(M4$fitted.values) #these are the same as the fitted values
M4$coefficients #RETURNS BETA ESTIMATES
hist(M4$residuals) #PLOT THEM!
jarque.bera.test(M4$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(M4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$income)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$income)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

#RE-BUILD MODEL M6 WITH ONLY THE TRAINING DATA PARTITION
M5<-lm(income~yearsemployed + yearsemployed2, Training)
summary(M5)
PRED_5_IN <- predict(M5, Training) #generate predictions on the (in-sample) training data
View(PRED_5_IN)
View(M5$fitted.values) #these are the same as the fitted values
hist(M5$residuals) #PLOT THEM!
jarque.bera.test(M5$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_5_OUT <- predict(M5, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_5_IN<-sqrt(sum((PRED_5_IN-Training$income)^2)/length(PRED_5_IN))  #computes in-sample error
RMSE_5_OUT<-sqrt(sum((PRED_5_OUT-Testing$income)^2)/length(PRED_5_OUT)) #computes out-of-sample 

RMSE_5_IN #IN-SAMPLE ERROR
RMSE_5_OUT #OUT-OF-SAMPLE ERROR

#build the logistic model with the training partition (need rank to be factor variable)
M6<-lm(income ~ yearsemployed + yearsemployed2 + yearsemployed3, data = Training)
summary(M6)
PRED_6_IN <- predict(M6, Training) #generate predictions on the (in-sample) training data
View(PRED_6_IN)
View(M6$fitted.values) #these are the same as the fitted values
hist(M6$residuals) #PLOT THEM!
jarque.bera.test(M6$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_6_OUT <- predict(M6, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_6_IN<-sqrt(sum((PRED_6_IN-Training$income)^2)/length(PRED_6_IN))  #computes in-sample error
RMSE_6_OUT<-sqrt(sum((PRED_6_OUT-Testing$income)^2)/length(PRED_6_OUT)) #computes out-of-sample 

RMSE_6_IN #IN-SAMPLE ERROR
RMSE_6_OUT #OUT-OF-SAMPLE ERROR

########################################
########### MODEL COMPARISON ###########
########################################

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_IN #FIRST MODEL WITH LINEAR TERM
RMSE_2_IN #SECOND MODEL WITH LINEAR TERM
RMSE_3_IN #THIRD MODEL WITH LINEAR TERM
RMSE_4_IN #FOURTH MODEL WITH LINEAR TERM
RMSE_5_IN #MODEL WITH LINEAR, AND QUADRATIC TERM
RMSE_6_IN #LOGARITHMIC MODEL

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT #FIRST MODEL WITH LINEAR TERM
RMSE_2_OUT #SECOND MODEL WITH LINEAR TERM
RMSE_3_OUT #THIRD MODEL WITH LINEAR TERM
RMSE_4_OUT #FOURTH MODEL WITH LINEAR TERM
RMSE_5_OUT #MODEL WITH LINEAR, AND QUADRATIC TERM
RMSE_6_OUT #LOGARITHMIC MODEL


#############################
#### Classification Task ####
#############################
install.packages('caret', dependencies = TRUE)
library(lattice)
library(caret) #data partioning library and other machine learning tools
library(rpart) #CART library
library(e1071) #svm library
library(randomForest) #random forest

cleandata$defaulted<-factor(cleandata$defaulted) #convert defaulted to factor (categorical) variable
cleandata$education<-factor(cleandata$education) #convert education to factor (categorical) variable
View(cleandata)

#fraction of sample to be used for training
p<-.7

#number of observations (rows) in the dataframe
obs_count<-dim(cleandata)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(123)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

Training <- cleandata[train_ind, ] #pulls random rows for training
Testing <- cleandata[-train_ind, ] #pulls random rows for testing

##CART MODEL:
#rpart package implementation
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)
CART <- train(defaulted~., data = Training, trControl=train_control, tuneLength=10, method = "rpart", na.action = na.omit) #increasing tunelength increases regularization penalty
##the "cv", number = 10 refers to 10-fold cross validation on the training data
plot(CART) #produces plot of cross-validation results
CART$bestTune #returns optimal complexity parameter
confusionMatrix(predict(CART, Testing, na.action = na.omit), Testing$defaulted, positive='1')

#rpart package implementation
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)
CART2 <- train(education~., data = Training, trControl=train_control, tuneLength=10, method = "rpart", na.action = na.pass) #increasing tunelength increases regularization penalty
##the "cv", number = 10 refers to 10-fold cross validation on the training data
plot(CART2) #produces plot of cross-validation results
CART$bestTune #returns optimal complexity parameter
confusionMatrix(predict(CART2, Testing, na.action = na.pass), Testing$education, positive='1')

##RANDOM FOREST MODEL:
#caret package implementation with 10-fold cross validation
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)
RF <- train(defaulted~ ., method="rf", trControl=train_control, preProcess=c("center", "scale"), tuneLength=2, data=Training, na.action = na.omit)
print(RF)
confusionMatrix(predict(RF, Testing), Testing$defaulted, positive='1')

#random forest package implementation
RF2 <- randomForest(education~., Training, na.action = na.omit)
print(RF2)
confusionMatrix(predict(RF2, Testing), Testing$education, positive='1')





