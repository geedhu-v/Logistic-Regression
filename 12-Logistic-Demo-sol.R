  ##################################################
  ### PROG8430                                    ##
  ### Logistic Demonstration                      ## 
  ##################################################
  # Written by Peiyuan
  # ID: 123456789
  ##################################################
  ### Basic Set Up                                ##
  ##################################################
  # Clear plots
  if(!is.null(dev.list())) dev.off()
  # Clear console
  cat("\014") 
  # Clean workspace
  rm(list=ls())
  options(scipen=9)
  
  ##################################################
  ### Install Libraries                           ##
  ##################################################
  #If the library is not already downloaded, download it
  #if(!require(pROC)){install.packages("pROC")}
  library(pROC)
  
  ##################################################
  ### Read data and do preliminary data checks    ##
  ##################################################
  # Read Blood Donation data set
  Blood <- read.csv("C:/Users/Geedhu/Documents/Maths _ Data Analysys/Week 12_Logistic Regression/BloodDonate.csv", header = TRUE, sep = ",")
  head(Blood,8)  #Print a Few Observations to Verify
  
  #Rename for easier interpretation
  names(Blood) <- c("Gender", "Donate", "Recent", "Nbr", "Vol", "Time")
  str(Blood)
  summary(Blood)
  
  # Summary and check frequency for each features
  feature_names <- colnames(Blood)
  frequency_tables <- list()
  for (feature in feature_names) {
    frequency_tables[[feature]] <- table(Blood[[feature]])
    cat(feature)
    print(frequency_tables[[feature]])
    cat("\n")
  }
  
  #Remove "-1" for Recent. -- outlier
  Blood$Recent <- ifelse(Blood$Recent < 0, 0, Blood$Recent)
  
  
  ##################################################
  ### Create Two Datasets: One for factors        ##
  ### One for numeric                             ##
  ##################################################
  Blood_Fac <- Blood
  for(i in 1:ncol(Blood_Fac)) 
    Blood_Fac[,i]=as.factor(Blood_Fac[,i])
  str(Blood_Fac)
  
  # Notice - This next part does not work for character variables
  Blood_Num <- Blood_Fac
  for(i in 1:ncol(Blood_Num)) 
    Blood_Num[,i]=as.numeric(Blood_Fac[,i])
  str(Blood_Num)
  
  ##################################################
  ### Descriptive Analysis                        ##
  ##################################################
  summary(Blood_Fac)
  
  par(mfrow = c(2, 3))
  for (i in 1:ncol(Blood_Fac)) {
    barplot(table(Blood_Fac[,i]), 
            main=names(Blood_Fac)[i],
            border="red", col="orange", density=25)
  }

  ##################################################
  ### Correlation Detection.                      ##
  ##################################################
  round(cor(Blood_Num, method="spearman"),3)   
  #Note here - School is not ordinal so may not give good results

  ##################################################
  ### Building the Model                          ##
  ##################################################
  #Why not MLR? 
  MLR <- lm(formula = Donate ~., data=Blood_Num, na.action=na.omit)
  par(mfrow = c(2, 2))
  plot(MLR)
  
  #Manual (Dropping Vol)
  Don_Man <- glm(formula=Donate ~ .,
                 family="binomial", data=Blood_Fac, na.action=na.omit)
  #family = "binomial": This specifies the type of model being fitted (logistic regression model).
  summary(Don_Man)
 
  Don_Man <- glm(formula=Donate ~ Gender + Recent + Nbr + Time,
                family="binomial", data=Blood_Fac, na.action=na.omit)
  #family = "binomial": This specifies the type of model being fitted (logistic regression model).
  summary(Don_Man)
  
  
  #stepwise
  Don_glm = glm(Donate ~ . ,
                family="binomial", data=Blood_Fac, na.action=na.omit)
  stp_Don_glm <- step(Don_glm)
  summary(stp_Don_glm)
  
  
  
  ##################################################
  ### Evaluations.                                ##
  ##################################################
  ## Check the stepwise Model
  pred <- predict(stp_Don_glm, type="response")   # creates probabilities
  head(pred,20)
  Class_SW <- ifelse(pred > 0.5,"Y","N")           # Classifies probabilities (i.e. >50% then likely to donate)
  head(Class_SW)
  
  ### Note confusion matrix using longley list is not possible as my version is not 
  ### supporting the package to be downloaded for longleylist function to use.
  #library(package_name)
  Conf <- table(Blood_Fac$Donate, Class_SW,dnn=c("Act Donate","Predicted"))  # Creates a Confusion Matrix
  Conf
                                                                          
  TP <- Conf[2,2]
  TN <- Conf[1,1]
  FP <- Conf[1,2]
  FN <- Conf[2,1]
  
  Accuracy = (TP + TN) / (TP + TN + FP + FN)
  Accuracy
  Precision = TP / (TP + FP)
  Precision
  Sensitivity = TP / (TP + FN)
  Sensitivity
  Specificity = TN / (TN + FP)
  Specificity
  
  #ROC Curve (and Area Under the Curve)
  par(mfrow = c(1, 1))
  plot(roc(Blood_Fac$Donate,pred),
       col="red", lwd=2, main='ROC Curve for Logistic, Blood Donation')
  
  auc(Blood_Fac$Donate, pred)
  
  