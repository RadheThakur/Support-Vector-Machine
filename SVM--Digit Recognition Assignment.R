#############################################################
# SVM Assignment: Handwritten Digit Recognition
# Submitted By: Radhe Thakur
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

#sink(file = "OutPutLog.txt",append = TRUE,type = c("output", "message"))
Start_Time<-as.POSIXct(Sys.time())

print(paste("Execution Started: ",Sys.time()))

## Function to SetUp Required Libraries
Setup_Packages<-function(Pkgs.load){
  
  Unistalled.Libraries<-Pkgs.load[!(Pkgs.load %in% installed.packages()[,"Package"])] 
  
  if(length(Unistalled.Libraries)!=0){
    install.packages(Unistalled.Libraries)
  }
  
  for(pkg in 1:length(Required.Libraries)){
    
    library(Required.Libraries[pkg],character.only = TRUE)
    next 
    
 } 
  print(paste("Package(s) ", Pkgs.load ," Loaded Successfully"))
    
  
}


Required.Libraries<-c("caret","kernlab","dplyr","readr","ggplot2","gridExtra","doParallel","foreach")
Setup_Packages(Required.Libraries)

#Set Working Directory
setwd("C:/Users/RadheThakur/Desktop/SVM")

#############################################################
##      Load Data Files                                    ##
#############################################################
Train.Data<- read.csv("mnist_train.csv",header = FALSE,stringsAsFactors = FALSE)
Test.Data<-read.csv("mnist_test.csv",header = FALSE,stringsAsFactors = FALSE)

print(paste("Data Loaded Successfully in ", difftime(as.POSIXct(Sys.time()),Start_Time,units = "mins")))

# Data Understanding
dim(Train.Data)# 60000   786
dim(Test.Data) # 10000   786
#str(Train.Data) # Numerical columns

# First Column contaings Digit (0-9)
# Include a flag, merge Test and Train data to Understand attributes and properties
# append the identifier(Digit)

Train.Data$data.flag<-"Train"
Test.Data$data.flag<-"Test"
Digit.Master.Data<-rbind(Train.Data,Test.Data)
colnames(Digit.Master.Data)[1]<-"Digit"

# Check for NA ,Blank and Duplicate

sum(is.na(Digit.Master.Data)) # 0, No NA Values, No Treatment Required
sum(Digit.Master.Data =="") # 0 ,  No Blank Values no Treatment Required
sum(duplicated(Digit.Master.Data))# 0, No Duplicate Value, No Treatment Required

#Convert the Digit to Factor

Digit.Master.Data$Digit<-as.factor(Digit.Master.Data$Digit)

#Divide the Test and Train data and remove the flag introduced

Modified.Train.Data<- Digit.Master.Data[Digit.Master.Data$data.flag=="Train",]
Modified.Test.Data<-Digit.Master.Data[Digit.Master.Data$data.flag=="Test",]
Modified.Test.Data<-Modified.Test.Data[,-786] #Remove Flag COlumn
Modified.Train.Data<-Modified.Train.Data[,-786] #Remove Flag COlumn


#############################################################
###               Sampling of the data                     ##
#############################################################  

# The sample should be selected in such a way that its proportional
# to the data available so that ample data is available for proper
# Training 


#Sample1: 

    #As Per Pareto Principle(80/20 Rule), Since Test data is 10000 Our Training data should be 40k
    #which is ~67% of original 60k rows

#Sample2: 

    # The fraction of patterns reserved for the validation set should be inversely proportional to the square root of the number of free adjustable parameters. 
    # We have 785 parameters sqrt of 785 equals 28, which makes 1/28 = 3.5 % is enough for testing 
    # That Makes 4% of 70k(Test and Train) equals 2800 rows 
    # 80/20 Rule: Makes ~12K rows sufficient for Training which comes around 20% data of 60k (Training Data)

#Sample3:

    # We will go with twice the testing data 
    # We have 10k testing data that makes 20 K Training data which is 1/3 of Training Data 
    # which makes around 33% data



#Sample2:
set.seed(100) 
    Sample.3_Indice<-sample(1: nrow(Modified.Train.Data),(.20*nrow(Modified.Train.Data)))
    Sample.3_Data<- Modified.Train.Data[Sample.3_Indice,] 
    print(paste(nrow(Sample.3_Data),"rows selcted for Training"))
    
    
    #Assumption:
    
    # All different categories of data and its features of a particular digit is sampled and available in the sample selected
    # for training
    # Distribution is almost same in all the sample selected wrt to Master Training Data
    # and to Overcome Computation Expense we are going with Sample 3 i.e ~20k training rows
    
    #############################################################
    ####          Distribution Data vs sample                  ##
    ############################################################# 
    
    Master.Data<- ggplot(Modified.Train.Data, aes(x=Digit,fill=Digit))+ geom_bar()+ggtitle("Master Data")+theme(legend.position="none")
    Sample3<- ggplot(Sample.3_Data, aes(x=Digit,fill=Digit))+ geom_bar()+ggtitle("Sample 2")+theme(legend.position="none")
    grid.arrange(Master.Data,Sample3)
    
    print("Training data has same Distribution wrt Train data provided")
    
    #############################################################
    ##               Model Selection                           ##
    #############################################################   
    
    Training_data<-Sample.3_Data
    Testing_data<-Modified.Test.Data
  
    #HyperParametes can be contributing factor so running list of C for optimal Model 
    # 3 Times Cross Validation is performed and Error is the evaluation criteria for Model
    
    #Setting Range of C(Wider Range to find the approximate the global optima the same can be fine tuned)
    # Creating a dataframe containg C value, ketnel, degree and Cross Validation Error
    Model.Acc<-data.frame(matrix(ncol = 4,nrow = 0))
    C.Value=list(.001,.01,.1,10,100,1000)
    Kernel<-c("vanilladot","polydot","rbfdot")
    Dgree<-list(2,3,4)
    colnames(Model.Acc)<-c("Kernel","Degree","C.Value","CV.Err")

    
#Update the dataframe with models cross Validation Error
    
    
      
    for(krnl in Kernel){
        for(hyperParm in C.Value){
            if(krnl!="polydot"){
                SVM.Linear.Model <- ksvm(Training_data$Digit~.,
                                         data=Training_data,
                                         scaled=FALSE,
                                         kernel= krnl,
                                         cross=3,
                                         C=hyperParm)
                Model.Acc<-rbind(Model.Acc,data.frame("Kernel"= krnl,
                                                      "Degree"= NA,
                                                      "C.Value"= as.character(hyperParm),
                                                      "CV.Err"= SVM.Linear.Model@cross))
                
        
            }else{
        
                for(d in Dgree){
                                SVM.polynomial.Model <- ksvm(Training_data$Digit~.,
                                                             data=Training_data,
                                                             scaled=FALSE,
                                                             kernel=krnl,
                                                             cross=3,
                                                             C=hyperParm,
                                                             kpar=list(d))
                                
                                
                                Model.Acc<-rbind(Model.Acc,data.frame("Kernel"=krnl,
                                                                      "Degree"= d,
                                                                      "C.Value"= as.character(hyperParm),
                                                                      "CV.Err"= SVM.polynomial.Model@cross))    
                                
} 
 }
  }
    }
    
  
    print(Model.Acc)
    print(paste("Global Optima approximated in ", difftime(as.POSIXct(Sys.time()),Start_Time,units = "mins")))
    
    #############################################################
    ##               Model Selection                           ##
    #############################################################   
  #   print(Model.Acc)  
  #   Kernel Degree C.Value     CV.Err
  #   1  vanilladot     NA   0.001 0.09008333
  #   2  vanilladot     NA    0.01 0.09291667
  #   3  vanilladot     NA     0.1 0.09033333
  #   4  vanilladot     NA      10 0.09200000
  #   5  vanilladot     NA     100 0.08758333
  #   6  vanilladot     NA    1000 0.09475000
  #   7     polydot      2   0.001 0.03891667
  #   8     polydot      3   0.001 0.04533333
  #   9     polydot      4   0.001 0.06091667
  #   10    polydot      2    0.01 0.03975000
  #   11    polydot      3    0.01 0.04441667
  #   12    polydot      4    0.01 0.05841667
  #   13    polydot      2     0.1 0.03991667
  #   14    polydot      3     0.1 0.04658333
  #   15    polydot      4     0.1 0.06066667
  #   16    polydot      2      10 0.03941667
  #   17    polydot      3      10 0.04441667
  #   18    polydot      4      10 0.06083333
  #   19    polydot      2     100 0.03958333
  #   20    polydot      3     100 0.04383333
  #   21    polydot      4     100 0.06275000
  #   22    polydot      2    1000 0.03900000
  #   23    polydot      3    1000 0.04433333
  #   24    polydot      4    1000 0.06025000
  #   25     rbfdot     NA   0.001 0.89000000
  #   26     rbfdot     NA    0.01 0.28291667
  #   27     rbfdot     NA     0.1 0.07966667
  #   28     rbfdot     NA      10 0.03541667
  #   29     rbfdot     NA     100 0.03425000
  #   30     rbfdot     NA    1000 0.03633333

BIC.Model<-Model.Acc[Model.Acc$CV.Err == min(Model.Acc$CV.Err),]  
    
print(paste("Best Optimum Model ",BIC.Model))    
    
    
  # The Best Model having smallest error is Model 29 rbfdot C= 100 and CV.Errr = 0.03425 
  
#############################################################
##             Model Tuning                                ##
#############################################################  


trainControl <- trainControl(method="cv", number=3)
metric <- "Accuracy"
grid <- expand.grid(.sigma=c(1.63989684931514e-07, 2.63989684931514e-07,3.63989684931514e-07),.C=c(200,400,800))

fit.svm <- train(Digit~., data=Training_data, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)


        # Support Vector Machines with Radial Basis Function Kernel 
        # 
        # 12000 samples
        # 784 predictor
        # 10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
        # 
        # No pre-processing
        # Resampling: Cross-Validated (3 fold) 
        # Summary of sample sizes: 8000, 7999, 8001 
        # Resampling results across tuning parameters:
        #   
        #   sigma         C    Accuracy   Kappa    
        # 1.639897e-07  200  0.9650832  0.9611955
        # 1.639897e-07  400  0.9650832  0.9611955
        # 1.639897e-07  800  0.9650832  0.9611955
        # 2.639897e-07  200  0.9686669  0.9651786
        # 2.639897e-07  400  0.9686669  0.9651786
        # 2.639897e-07  800  0.9686669  0.9651786
        # 3.639897e-07  200  0.9695835  0.9661972
        # 3.639897e-07  400  0.9695835  0.9661972
        # 3.639897e-07  800  0.9695835  0.9661972
        # 
        # Accuracy was used to select the optimal model using the largest value.
        # The final values used for the model were sigma = 3.639897e-07 and C = 200.  
print(c("Execution End: ",Sys.time()))

#############################################################
##   Final Model   sigma = 3.639897e-07 and C = 200.       ##
#############################################################  

Final_Model <- ksvm(Digit~.,data=Training_data,
                    kernel="rbfdot",
                    scaled=FALSE,
                    C=200,
                    kpar=list(sigma=3.639897e-07))



Rad.Acc<-predict(Final_Model,Testing_data)
print(confusionMatrix(Rad.Acc,Testing_data$Digit)$overall[1])

# Accuracy 
# 0.9721 


#############################################################
####                    Assumptions                        ##
############################################################# 

# Since there is no parameter reduction/selection is suggested in assignment, its not performed
# Its highly reccommended considering the number of variables 

# Although sampling should be enough to hadle distribution of training model, Due to COmputational 
# resuources crunch, minimum i.e 20% of sampled data is used for modelling, It is highly reccommended
# to go with ~ 33% of Train data

# Accuracy is the primary scale to measure the Best Model,  
# Polynomial model degree 2 with C= 0.001 , CV Error=0.03891667 almost has same error as 
#compared to radial best model selected with little trade off 
# in accuracy the models complexcity can be reduced by ample amount


#############################################################
####                    Conclusion                         ##
############################################################# 

# Different Ranges of C, Degree(in case of polynomial) and kernel is used to locate the Global
# Optima for hyperparametes measuring 3 fold cross validation Error.

# Best model is suggeested is radial kernel with C= 100 and CV.Errr = 0.03425(which is minium) 

# The hyperparameters is tuned to find the Local Optima which came out to be
# sigma=3.639897e-07  C=200  having accuracy of 0.9695835 

# Finally, model using the refined hyperparameters are used to get final Model and tested for 
# Acuracy which came ~97%

# Non Linearity in the data is very small as affirmed by sigma of order e-07

























  
  