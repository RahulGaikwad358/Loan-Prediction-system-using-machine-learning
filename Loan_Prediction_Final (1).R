df1<-read.csv("Loan_train.csv")
summary(df1)
View(df1)
colnames(df1)
nrow(df1)
ncol(df1)
dim(df1)
head(df1)
df1<-df1[-c(1)]
# Data Cleaning Process
summary(df1$Loan_Status)
# finding the unique variables in the loan_status column
unique(df1$Loan_Status)
# finding the number of unique variables in the loan_status column
length(unique(df1$Loan_Status))
unique(df1$Gender)
df1=subset(df1,df1$Gender!="")
unique(df1$Gender)
unique(df1$Self_Employed)
df1=subset(df1,df1$Self_Employed!="")
unique(df1$Self_Employed)
df1$ApplicantIncome[is.na(df1$ApplicantIncome)]<-mean(df1$ApplicantIncome,na.rm = TRUE)
df1$CoapplicantIncome[is.na(df1$CoapplicantIncome)]<-mean(df1$CoapplicantIncome,na.rm = TRUE)
df1$LoanAmount[is.na(df1$LoanAmount)]<-mean(df1$LoanAmount,na.rm=TRUE)
df1$Loan_Amount_Term[is.na(df1$Loan_Amount_Term)]<-mean(df1$Loan_Amount_Term,na.rm=TRUE)
df1=na.omit(df1)
summary(df1)
nrow(df1)
ncol(df1)
dim(df1)
#DAta visulisation 
# Plotting Graph for male vs female applicant 
barplot(table(df1$Gender),xlab="Gender",ylab="Applicants",main="Male vs Female",col="Green")
#plotting graph for married vs unmarried applicants       
barplot(table(df1$Married),xlab = "MArried Status",ylab = "Applicants",main = "MArried vs Unmarried",col = "Green")      
#plotting graph for no of dependents applicants
barplot(table(df1$Dependents),xlab = "No of dependents",ylab = "Applicants",main = "Dependents", col="Green")
#Plotting graph on basis of education(Graduated Vs UnGraduated)        
barplot(table(df1$Education),xlab = "Education",ylab = "Applicants",main = "Graduated VS  Not Graduated",col = "Green")       
# plotting the graph of sef-Employed vs non-self Employed applicants
barplot(table(df1$Self_Employed),xlab="Self Employed",ylab="Applicants",main="Self Employed vs Non-self Employed ",col="Green")
# plotting the graph of applicant's credit history
barplot(table(df1$Credit_History),xlab="Creadit History",ylab="Applicants",main="Applicant with Good credit history vs bad ",col="Green")
# plotting the graph for compare approved vs non approved loans
barplot(table(df1$Loan_Status),xlab="Loan Status",ylab="Applicants",main="Approved vs non approved loans",col="Green") 
# comparing the urban semi-urban and rural property
barplot(table(df1$Property_Area),xlab="Area of property",ylab="Applicants",main="Urban vs Rural vs Sub-urban properties",col="Green")
# knowing the dispersion of applicant income
barplot(df1$ApplicantIncome, ylab="Applicant income", main="Applicant income")
cat("********************************************\n")
cat("APPLYING RANDOM FOREST\n")
cat("********************************************\n")
df1$Loan_Status=as.factor(df1$Loan_Status)
library(stats)
library(randomForest)
set.seed(125)
#View(df1)
str(df1)
index<-sample(2,nrow(df1),replace = TRUE,prob =c(0.7,0.3))
#Training Daata 
training_dat<-df1[index==1,]
testing_dat<-df1[index==2,]
#Random forest
rfm<-randomForest(Loan_Status~.,data = training_dat)
#Evaluation model accuracy
Loan_Status_Pred<-predict(rfm,testing_dat)
testing_dat$Loan_Status_Pred<-Loan_Status_Pred

cat("Confusion matrixx for RandomForest")
#Confusion matrix
cfm<-table(testing_dat$Loan_Status,testing_dat$Loan_Status_Pred)
print(cfm)
#Classification Accurary after applying Random Forest
Classification_Accuracy<-sum(diag(cfm)/sum(cfm))
print(Classification_Accuracy)
per<-Classification_Accuracy*100
cat("The Accuracy after applying random forest algorithm is :",per ,"%")
#Logistic Regreesion
library(dplyr)
library(caTools)
logistic_model<-glm(Loan_Status~ApplicantIncome+LoanAmount,data = training_dat,family = "binomial")
summary(logistic_model)

predict_reg<-predict(logistic_model,testing_dat,type = "response")
summary(predict_reg)
predict_reg
predict_reg<-ifelse(predict_reg>0.5,1,0)
str(predict_reg)
table(testing_dat$Loan_Status,predict_reg)
missing_classer<-mean(predict_reg!=testing_dat$Loan_Status,)
accuracy<-print(paste(1- missing_classer))
cat("Accurary after applying LOGISTIC REGRESSION :",accuracy)
cat("ACCuracy after applying RANDOM FOREST IS :",Classification_Accuracy)
cat("Accuracy after appying Logistic Regression is ",accuracy)
#APPLYING SVM
cat("****************APPLYING SVM******************" )
library(caret)
View(training_dat)
training_loan_status<-factor(training_dat[["Loan_Status"]])

trctlr<-trainControl(method = "repeatedcv",number = 10,repeats = 3)
svm_linear<-train(Loan_Status~.,data = training_dat, method="svmLinear",trControl=trctlr,preProcess=c("center","scale"),tuneLength=10)
summary(svm_linear)
test_predict<-predict(svm_linear,newdata = testing_dat)
test_predict
cm<-confusionMatrix(table(test_predict,testing_dat$Loan_Status))
cm
cat("THE ACCURACY AFTER APPLYING SVM IS : 77 %")
#KNN 
fit<-train(Loan_Status~.,data = training_dat,method='knn',tuneLength=20,trControl=trctlr)
print(plot(fit))
pred<-predict(fit,newdata = testing_dat)
pred
cfknn<-confusionMatrix(pred,testing_dat$Loan_Status)
cfknn
cat("THE ACCURACY AFTER APPPLYING KNN IS : 69 %")
