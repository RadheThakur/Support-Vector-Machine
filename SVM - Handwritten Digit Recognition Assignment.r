#############################################################
# SVM Assignment: Handwritten Digit Recognition
# Submitted By: Radhe Thakur
# Dated: 23 Sep 2018
#############################################################



#############################################################
##     Business Understanding                              ##
#############################################################	

# Data set given is from MNIST database and represents the greyscale 
# levels that is normalized to 28X28 dimensions. There are 785 features  
# recorded for each handwritten image.
# As There are in total 10 Classes (0-9), its a multiclass classification
# Problem having Training(Image) data (60k) and Test Data (10k)

#############################################################
##     Set Up libraries                                    ##
#############################################################


## Function to SetUp Required Libraries
	Setup_Packages<-function(Pkgs.load){
	  
	  Unistalled.Libraries<-Pkgs.load[!(Pkgs.load %in% installed.packages()[,"Package"])] 
	  
	  if(length(Unistalled.Libraries)!=0){
		install.packages(Unistalled.Libraries)
	  }
	  
	  for(pkg in 1:length(Required.Libraries)){
		
		library(Required.Libraries[pkg],character.only = TRUE)
	  } 
	  
	  print(paste("Package(s) ", Pkgs.load ," Loaded Successfully"))
	  
	}

	Required.Libraries<-c("caret","kernlab","dplyr","readr","ggplot2","gridExtra")
	Setup_Packages(Required.Libraries)


#Set Working Directory
	setwd("C:/Users/RadheThakur/Desktop/SVM")

#############################################################
##      Load Data Files                                    ##
#############################################################
	 Train.Data<- read.csv("mnist_train.csv",header = FALSE,stringsAsFactors = FALSE)
	 Test.Data<-read.csv("mnist_test.csv",header = FALSE,stringsAsFactors = FALSE)
	 
# Data Understanding
    
	 dim(Train.Data)
	# 60000   786
	
	 dim(Test.Data) 
	 # 10000   786
	 
	 str(Train.Data) # Numerical columns
# First Column contaings Digit (0-9)

# Include a flag, merge Test and Train data to Understand attributes and properties
# append the identifier(Digit)
  
	  Train.Data$data.flag<-"Train"
	  Test.Data$data.flag<-"Test"
	  Digit.Master.Data<-rbind(Train.Data,Test.Data)
	  colnames(Digit.Master.Data)[1]<-"Digit"

#############################################################
##                Data Cleansing                           ##
#############################################################

# Remove Columns having all same row Values
	  cols.to.Remove<-apply(Digit.Master.Data,2,function(x) length(unique(x)))
	  cols.to.Remove<-names(cols.to.Remove)[which(cols.to.Remove==1)]
	  print(paste(length(cols.to.Remove)," Columns Removed")) #65 COlumns 

# 65 Columns have same value in all the rows(Test and Train)
# Since its a constant and wont add any value in explaining the variability and Prediction.
# Thus, we can remove these columns
  
	Digit.Master.Data<-Digit.Master.Data[ , !names(Digit.Master.Data) %in% cols.to.Remove] 
 
# Check for NA ,Blank and Duplicate

	  sum(is.na(Digit.Master.Data)) # 0, No NA Values, No Treatment Required
	  sum(Digit.Master.Data =="") # 0 ,  No Blank Values no Treatment Required
	  sum(duplicated(Digit.Master.Data))# 0, No Duplicate Value, No Treatment Required

#Convert the Digit to Factor

	Digit.Master.Data$Digit<-as.factor(Digit.Master.Data$Digit)


#Divide the Test and Train data and remove the flag introduced

	  Modified.Train.Data<- Digit.Master.Data[Digit.Master.Data$data.flag=="Train",]
	  Modified.Test.Data<-Digit.Master.Data[Digit.Master.Data$data.flag=="Test",]
	  Modified.Test.Data<-Modified.Test.Data[,-721] #Remove Flag COlumn
	  Modified.Train.Data<-Modified.Train.Data[,-721] #Remove Flag COlumn

#############################################################
###               Sampling of the data                     ##
#############################################################  
  
# The sample should be selected in such a way that its proportional
# to the data available so that ample data is available for proper
# Training 

	set.seed(100)  
  
#Sample1: 
  
#As Per Pareto Principle(80/20 Rule), Since Test data is 10000 Our Training data should be 40k
#which is ~67% of original 60k rows
  
	  Sample.1_Indice<-sample(1: nrow(Modified.Train.Data),(.67*nrow(Modified.Train.Data)))
	  Sample.1_Data<- Modified.Train.Data[Sample.1_Indice,] 
	  print(nrow(Sample.1_Data))

#Sample2: 

# The fraction of patterns reserved for the validation set should be inversely proportional to the square root of the number of free adjustable parameters. 
# We have 785 parameters sqrt of 785 equals 28, which makes 1/28 = 3.5 % is enough for testing 
# That Makes 4% of 70k(Test and Train) equals 2800 rows 
# 80/20 Rule: Makes ~12K rows sufficient for Training which comes around 20% data of 60k (Training Data)

	  Sample.2_Indice<-sample(1: nrow(Modified.Train.Data),(.20*nrow(Modified.Train.Data)))
	  Sample.2_Data<- Modified.Train.Data[Sample.2_Indice,] 
	  print(nrow(Sample.2_Data))
  
#Sample3:

# We will go with twice the testing data 
# We have 10k testing data that makes 20 K Training data which is 1/3 of Training Data 
# which makes around 33% data

	  Sample.3_Indice<-sample(1: nrow(Modified.Train.Data),(.33*nrow(Modified.Train.Data)))
	  Sample.3_Data<- Modified.Train.Data[Sample.3_Indice,] 
	  print(nrow(Sample.3_Data))
	  
	  
	  
	  
#Assumption:
	  
# All different categories of data and its features of a particular digit is sampled and available in the sample selected
# for training
	  
	  
 
#############################################################
####          Distribution Data vs Diff samples            ##
############################################################# 
  
	Master.Data<- ggplot(Modified.Train.Data, aes(x=Digit,fill=Digit))+ geom_bar()+ggtitle("Master Data")+theme(legend.position="none")  
	Sample1<- ggplot(Sample.1_Data, aes(x=Digit,fill=Digit))+ geom_bar()+ggtitle("Sample 1")+theme(legend.position="none")
	Sample2<- ggplot(Sample.2_Data, aes(x=Digit,fill=Digit))+ geom_bar()+ggtitle("Sample 2")+theme(legend.position="none")
	Sample3<- ggplot(Sample.3_Data, aes(x=Digit,fill=Digit))+ geom_bar()+ggtitle("Sample 3")+theme(legend.position = "right")
 
	grid.arrange(Master.Data,Sample1,Sample2,Sample3)

#############################################################
##               Modelling                                 ##
#############################################################   

# Distribution is almost same in all the samples selected wrt to Master Training Data
# and to Overcome Computation Expense we will go with Sample 2 i.e 12k training rows

	Training_data<-Sample.2_Data
	Testing_data<-Modified.Test.Data

#############################################################
##         Linear Model                                    ##
############################################################# 


	SVM.Linear.Model <- ksvm(Training_data$Digit~.,data=Training_data,scale=FALSE,kernel="vanilladot")
	SVM.Linear.Model

	Train.Accu<-predict(SVM.Linear.Model,Training_data)
	confusionMatrix(Train.Accu,Training_data$Digit)
# Overall Statistics
# Accuracy : 1        

	Test.Accu<-predict(SVM.Linear.Model,Testing_data)
	confusionMatrix(Test.Accu,Testing_data$Digit)
# Overall Statistics
# Accuracy : 0.9136  

# Linear Model Training Accuracy is 100% and Test Accuracy is of 92% 
# Drop in Accuracy suggest Over-fitting Problem, Lets Check if it can be solved by non linear models

#############################################################
####          Non Linear Models                			   ##
############################################################# 

	SVM.Radial <- ksvm(Digit~.,data=Training_data,scale=FALSE,kernel="rbfdot")
	SVM.Radial

# Support Vector Machine object of class "ksvm" 
# 
# SV type: C-svc  (classification) 
# parameter : cost C = 1 
# 
# Gaussian Radial Basis kernel function. 
# Hyperparameter : sigma =  1.63088043977761e-07 
# 
# Number of Support Vectors : 4305 

# Predict on Training Accuracy 

	Radial.Train.Accu<-predict(SVM.Radial,Training_data)
	confusionMatrix(Radial.Train.Accu,Training_data$Digit)
# Overall Statistics
# 
# Accuracy : 0.9804  

	Radial.Test.Accu<-predict(SVM.Radial,Testing_data)
	confusionMatrix(Radial.Test.Accu,Testing_data$Digit)

# Overall Statistics
# 
# Accuracy : 0.9611   


#Training Data Accuracy and Test Data Accuracy almost close ~2% difference,  
#Default Parameters enhanced the Accuracy of Model by ~4% 
# Lets Cross validate with 3 different Hyper parameter
#Sigma and C

#############################################################
####          Model Tuning  	               			   ##
############################################################# 
	set.seed(7)
	trainControl <- trainControl(method="cv", number=3)
	metric <- "Accuracy"
	grid <- expand.grid(.sigma=c(1.62985225357094e-07, 2.62985225357094e-07,3.62985225357094e-07), .C=c(1,2,3) )

	fit.svm <- train(Digit~., data=Training_data, method="svmRadial", metric=metric, 
					 tuneGrid=grid, trControl=trainControl)

	print(fit.svm)
# Support Vector Machines with Radial Basis Function Kernel 
# 
# 12000 samples
# 719 predictor
# 10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
# 
# No pre-processing
# Resampling: Cross-Validated (3 fold) 
# Summary of sample sizes: 7998, 8001, 8001 
# Resampling results across tuning parameters:
#   
#   sigma         C  Accuracy   Kappa    
# 1.629852e-07  1  0.9525837  0.9473022
# 1.629852e-07  2  0.9570006  0.9522111
# 1.629852e-07  3  0.9591672  0.9546192
# 2.629852e-07  1  0.9592499  0.9547113
# 2.629852e-07  2  0.9630834  0.9589718
# 2.629852e-07  3  0.9650001  0.9611019
# 3.629852e-07  1  0.9620001  0.9577679
# 3.629852e-07  2  0.9664167  0.9626765
# 3.629852e-07  3  0.9665001  0.9627691
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were sigma = 3.629852e-07 and C = 3.

	plot(fit.svm)

#############################################################
####    Final Model       sigma = 3.629852e-07 and C = 3   ##
############################################################# 

	Final_Model <- ksvm(Digit~.,data=Training_data,
						  kernel="rbfdot",
						  scale=FALSE,
						  C=3,
						  kpar=list(sigma=3.629852e-07))

# Training Accuracy
	 Final.Train.Accu<-predict(Final_Model,Training_data)
	 confusionMatrix(Radial.Train.Accu,Training_data$Digit)
  
# Overall Statistics
# 
# Accuracy : 0.9804          
  
#Test Accuracy
	 Final.Test.Accu<-predict(Final_Model,Testing_data)
	 confusionMatrix(Final.Test.Accu,Testing_data$Digit)
  
# Overall Statistics
# 
# Accuracy : 0.976             
  
  
#############################################################
####                    Conclusion                         ##
############################################################# 
# Non Linearity in the data is very small as affirmed by sigma of order e-07
# Eventhough the Accuracy in the test set has been enhanced by 4% as compared to linear model
# since there is not much difference in Train and Test Accuracy minimizes the
# chances of Overfitting



