# Installing packages
install.packages('mlbench', dependencies = TRUE)
install.packages('caret', dependencies = TRUE)
install.packages('corrplot', dependencies = TRUE)
install.packages('e1071', dependencies=TRUE)
install.packages('scales', dependencies=TRUE)
install.packages("e1071")
install.packages("caTools")
install.packages("class")
install.packages("caTools")    # For Logistic regression
install.packages("ROCR")       # For ROC curve to evaluate model
install.packages("PerformanceAnalytics")
# Loading package
library(caTools)
library(ROCR) 
library(PerformanceAnalytics)
library(e1071)
library(caTools)
library(class)
library(mlbench)
library(caret)
library(corrplot)
library(scales) # to get percent value
library(e1071)# e1071 library holds the naive bayes classifier
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

data <- read.csv(file.choose(), header = T)# The output is delivered as a data frame
head(data)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Data cleaning - 
# SEX
mean(data$SEX) # There are no NA values in Sex column
unique(data$SEX) # Unique values are - 1, 2
#EDUCATION 
mean(data$EDUCATION) # There are no NA values in Education column
unique(data$EDUCATION) #Unique values are - 2, 1, 3, 5, 4, 6, 0
# 0,5,6 are not documented. so we decided to make it as 4 which is other
data$EDUCATION[data$EDUCATION == 5] <- 4
data$EDUCATION[data$EDUCATION == 6] <- 4
data$EDUCATION[data$EDUCATION == 0] <- 4
#MARRIAGE
mean(data$MARRIAGE) # There are no NA values in Marriage column
unique(data$MARRIAGE) # Unique values are - 1, 2, 3, 0
# 0 is not documented. so we decided to make it as 3 which is other
data$MARRIAGE[data$MARRIAGE == 0] <- 3
#AGE
mean(data$AGE) # There are no NA values in Age column
# Find if there are any null values in Bill_Amt and Pay_Amt
mean(data$BILL_AMT1)
mean(data$BILL_AMT2)
mean(data$BILL_AMT3)
mean(data$BILL_AMT4)
mean(data$BILL_AMT5)
mean(data$BILL_AMT6)
# There are no NA values in Bill_Amt columns
mean(data$PAY_AMT1)
mean(data$PAY_AMT2)
mean(data$PAY_AMT3)
mean(data$PAY_AMT4)
mean(data$PAY_AMT5)
mean(data$PAY_AMT6)
# There are no NA values in Pay_Amt columns
# Remove null  & NA values & blank values
data[!(is.na(data$BILL_AMT1) | data$BILL_AMT1=="" | 
         is.na(data$BILL_AMT2) | data$BILL_AMT2=="" |
            is.na(data$BILL_AMT3) | data$BILL_AMT3=="" |
               is.na(data$BILL_AMT4) | data$BILL_AMT4=="" |
                  is.na(data$BILL_AMT5) | data$BILL_AMT5=="" |
                     is.na(data$BILL_AMT6) | data$BILL_AMT6=="" |
                        is.na(data$PAY_AMT1) | data$PAY_AMT1=="" |
                           is.na(data$PAY_AMT2) | data$PAY_AMT2=="" |
                              is.na(data$PAY_AMT3) | data$PAY_AMT3=="" |
                                 is.na(data$PAY_AMT4) | data$PAY_AMT4=="" |
                                    is.na(data$PAY_AMT5) | data$PAY_AMT5=="" |
                                       is.na(data$PAY_AMT6) | data$PAY_AMT6=="" |
                                          is.na(data$default) | data$default==""),]
# Data Cleaning to calculate correlation (Correlation cannot have categorical value so changing them to numeric values)
# Repayment status
# There is lot of inconsistent data in 6 repayment status columns (pay0->pay6). So, we have decided to calculate the status for each month
# value 1 for 'defaulted' or 0 for 'not defaulted' in repayment status column
# Example - For september - if combined bill amount of all the months is greater than payment amount then it is considered as defaulted, else not defaulted
data$repayment_status_april = with(data, ifelse(((data$BILL_AMT6) - (data$PAY_AMT6)) > 0, 1, 0))
data$repayment_status_may = with(data, ifelse(((data$BILL_AMT6+data$BILL_AMT5)-(data$PAY_AMT6+data$PAY_AMT5)) > 0, 1, 0))
data$repayment_status_june = with(data, ifelse(((data$BILL_AMT6+data$BILL_AMT5+data$BILL_AMT4)-(data$PAY_AMT6+data$PAY_AMT5+data$PAY_AMT4)) > 0, 1, 0))
data$repayment_status_july = with(data, ifelse(((data$BILL_AMT6+data$BILL_AMT5+data$BILL_AMT4+data$BILL_AMT3)-(data$PAY_AMT6+data$PAY_AMT5+data$PAY_AMT4+data$PAY_AMT3)) > 0, 1, 0))
data$repayment_status_august = with(data, ifelse(((data$BILL_AMT6+data$BILL_AMT5+data$BILL_AMT4+data$BILL_AMT3+data$BILL_AMT2)-(data$PAY_AMT6+data$PAY_AMT5+data$PAY_AMT4+data$PAY_AMT3+data$PAY_AMT2)) > 0, 1, 0))
data$repayment_status_september = with(data, ifelse(((data$BILL_AMT6+data$BILL_AMT5+data$BILL_AMT4+data$BILL_AMT3+data$BILL_AMT2+data$BILL_AMT1)-(data$PAY_AMT6+data$PAY_AMT5+data$PAY_AMT4+data$PAY_AMT3+data$PAY_AMT2+data$PAY_AMT1)) > 0, 1, 0))
# Calculate number of months default based on past history
# If bill was paid completely till that month we make the value as '0' else we increase the value by 1. we calculate this in the month end
# Example - For September - If the complete amount is repaid in august we bring the value to 0 indicating it is completely paid else we increase the value by 1
data$default_count_april = with(data,ifelse(data$repayment_status_april == 1,1,0))
data$default_count_may = with(data,ifelse(data$repayment_status_may == 1,data$default_count_april+1,0))
data$default_count_june = with(data,ifelse(data$repayment_status_june == 1,data$default_count_may+1,0))
data$default_count_july = with(data,ifelse(data$repayment_status_july == 1,data$default_count_june+1,0))
data$default_count_august = with(data,ifelse(data$repayment_status_august == 1,data$default_count_july+1,0))
data$default_count_september = with(data,ifelse(data$repayment_status_september == 1,data$default_count_august+1,0))
# checking values of default.payment.next.month 
mean(data$default) # There are no NA values in this column
unique(data$default) # Unique values are 1, 0
data$default_payment_next_month = data$default

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Identifying columns which are highly correlated
clean_data = subset(data, select = (-c(ID, BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,	BILL_AMT5,BILL_AMT6,PAY_AMT1,PAY_AMT2,PAY_AMT3,	
                                       PAY_AMT4	,PAY_AMT5	,PAY_AMT6 , PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6, default)))

correl_clean_data = clean_data[,c(1,2,3,4,5,12,13,14,15,16,17,6,7,8,9,10,11)]

correlationMatrix <- cor(as.matrix(correl_clean_data))
correlationMatrix
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.80, names=TRUE)
highlyCorrelated

# > highlyCorrelated output:
# [1] "default_count_august"    "default_count_july"      "default_count_september" "default_count_june"     
# [5] "default_count_may"       "default_count_april"

# Output correlation plot:
corrplot(correlationMatrix, method = "number",
         tl.pos = "n", mar = c(2, 1, 3, 1)) 

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Building train and test models

clean_data_aftr_feat_sel = subset(data, select = (-c(ID, BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,	BILL_AMT5,BILL_AMT6,PAY_AMT1,PAY_AMT2,PAY_AMT3,	
                                                     PAY_AMT4	,PAY_AMT5	,PAY_AMT6 , PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6, default
                                                     ,default_count_april,default_count_may,default_count_june,default_count_july,default_count_august,default_count_september)))


set.seed(0) # To make sure same random variables are selected even though we run the code multiple times

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), size = nrow(clean_data_aftr_feat_sel), replace = TRUE, prob=c(0.70,0.30))
# Before Feature Selection
# train_data  <- clean_data[sample, ]
# test_data   <- clean_data[!sample, ]
# After Feature selection
train_data  <- clean_data_aftr_feat_sel[sample, ]
test_data   <- clean_data_aftr_feat_sel[!sample, ]

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Model 1: Naive Bayes

naive_bayes_model = naiveBayes(default_payment_next_month~.,data = train_data)
naive_bayes_model

# Building confusion matrix with test data to find the accuracy of our prediction
naive_bayes_predicted_data = predict(naive_bayes_model,test_data)
naive_bayes_actual_data = test_data$default_payment_next_month
naive_bayes_conf_matrix <- table(naive_bayes_actual_data, naive_bayes_predicted_data)
naive_bayes_conf_matrix
naive_bayes_accuracy_of_test_data = percent(sum(diag(naive_bayes_conf_matrix)) / sum(naive_bayes_conf_matrix))
naive_bayes_accuracy_of_test_data


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Model 2: Logistic Regression

glm.default.or.not<-glm(default_payment_next_month~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+repayment_status_september+repayment_status_august+repayment_status_july+repayment_status_june+repayment_status_april,data = train_data,family = "binomial")#build the model to predict default or not
predict.train<-predict(glm.default.or.not,train_data,type="response")# predict base on our model
predict.train.bin=ifelse(predict.train>0.5,1,0)
#table(actuals=train_data$default.payment.next.month,Predictions=predict.train>0.5)
#accuracy_train_data=((16412)/21004)
#accuracy_train_data
conf_matrix<-table(actuals=train_data$default_payment_next_month,Predictions=predict.train>0.5)
accuracy_of_train_data =(sum(diag(conf_matrix)) / sum(conf_matrix))
accuracy_of_train_data
predict.test<-predict(glm.default.or.not,test_data,type="response")# predict base on our model
#predict.train.bin=ifelse(predict.train>0.5,1,0)
conf_matrix<-table(actuals=test_data$default_payment_next_month,Predictions=predict.test>0.5)
accuracy_test_data =(sum(diag(conf_matrix)) / sum(conf_matrix))
percent(accuracy_test_data)



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Model 3: KNN Classifier

# K value - defines how many neighbors will be checked to determine the classification
# Typically chosen as the square root of total entries in the dataset
classifier_knn_model <- knn(train_data, test_data,cl = clean_data_aftr_feat_sel[sample,12], k=173)
classifier_knn_model
classifier_knn_conf_matrix <- table(clean_data_aftr_feat_sel[!sample,12], classifier_knn_model)
classifier_knn_conf_matrix
classifier_knn_accuracy_of_test_data = percent(sum(diag(classifier_knn_conf_matrix)) / sum(classifier_knn_conf_matrix))
classifier_knn_accuracy_of_test_data


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



