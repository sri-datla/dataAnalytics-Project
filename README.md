# dataAnalysis-Project

Dataset information - 

This dataset contains information on default payments, demographic factors, credit data, history of payment, and bill statements of credit card clients in Taiwan from April 2005 to September 2005.

Used the dataset from (https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients) to predict whether the credit card holders will default or not next month.

Variables - 

ID : Unique ID of each customer
LIMIT_BAL : Limit balance means the limit of credit card user have.
SEX: 1= Male ,2 =Female
EDUCATION: 1=Graduate school, 2=University, 3=High school, (4,5,6,7)=Others
MARRIAGE: Marital status (1=married, 2=single, 3=others)
AGE: Years Old
Payment Status: -1=pay duly, 1=payment delay for one month, 2=payment delay for two months,8=payment delay for eight months, 9=payment delay for nine months and above)
Bill Amt – Amount of bill generated first of month
Pay Amt – Amount of money customer has paid by end of the month
Default payment: 1=yes, 0=no (If he default in next month)


Data cleaning - 

Checked for NULL/ NaN/ blank values of each variable
Checked for unique values to make sure there is no inconsistent data
Removed the rows from dataset where payment information is NULL
Identified some inaccurate data in the 6 repayment status columns which are calculated based on past history (pay0->pay6). So, we have decided to calculate that status for each month
Calculation for September month – 
Step 1 – If combined bill amount of the months till September is greater than payment amount made by the customer then it is considered as defaulted (1),else not defaulted (0)
	data$repayment_status_september = with(data, 	ifelse(((data$BILL_AMT6+data$BILL_AMT5+data$BILL_AMT4+data$BILL_AMT3+data$BILL_A	MT2+data$BILL_AMT1)-	(data$PAY_AMT6+data$PAY_AMT5+data$PAY_AMT4+data$PAY_AMT3+data$PAY_AMT2+	data$PAY_AMT1)) > 0, 1, 0))
Step 2 – calculated number of months default by using repayment status
	data$default_count_september = with(data,ifelse(data$repayment_status_september == 	1,data$default_count_august+1,0))



Model 1 - Naive Bayes Classification -

Naïve Bayes classifier is a supervised machine learning model that is used for classification problems which is based on the Bayes theorem.
A training model is created by using naiceBayes() function - naive_bayes_model = naiveBayes(default_payment_next_month~.,data = train_data)
This model is used to predict default status of customers in the test data.
	naive_bayes_predicted_data = predict(naive_bayes_model,test_data)
	naive_bayes_conf_matrix <- table(naive_bayes_actual_data, 	naive_bayes_predicted_data)
Achieved accuracy of 73% 

Then perfomed feature selection using correlation to identify columns which are highly correlated.

correlationMatrix <- 	cor(as.matrix(correl_clean_data))
	highlyCorrelated <- 	findCorrelation(correlationMatrix, 	cutoff=0.80, 	names=TRUE)
  
Performed Naïve Bayes classification again after removing the columns(default count for each month) which are output of highly correlated.

Accuracy improved to 78%

Model 2 - Logistic Regression -

Logistic regression is an example of supervised learning. It is used to calculate or predict the probability of a binary (yes/no) event occurring.
As the data that have to be predicted was categorical data (between 0 and 1 ) not continuous so that we use logistic over the linear regression.

Used GLM model of R in order to do the Logistic regression.
Focused on the variables that are having high correlation between them and looked at the P values while building our model in order to optimize and get maximum accuracy of data.

Achieved accuracy of 78%

Model 3 - KNN Classification - 

k-nearest neighbors algorithm is a supervised learning method used for classification and regression problems.

K value - defines how many neighbors will be checked to determine the classification
K value is typically chosen as the square root of total entries in the dataset

Achieved accuracy of 78%


