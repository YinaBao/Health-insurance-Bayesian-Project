# DATS 6450-11 Final Project: Insurance charges prediction
# Bayesian Methods for Data Science
# Group 7: Yina Bao, Tianyi Wang, Xiaochi Ge


# Load the required packages
load.libraries <- c('GGally', 'corrplot', 'ggplot2', 'dplyr','BAS', 'data.table', 'gridExtra')

install.lib <- load.libraries[!load.libraries %in% installed.packages()]

for(libs in install.lib) install.packages(libs)
sapply(load.libraries, require, character = TRUE)

# Original dataset observation
# Load data
Insurance <- read.csv('insurance.csv')

head(Insurance)

# Summary and Structure of the data -- Descriptive Statistics
str(Insurance)
summary(Insurance)
sapply(Insurance, function(x) sum(is.na(x)))

# Convert the categorical data to numeric value
ins <- Insurance
ins$sex <- as.numeric(ins$sex)
ins$smoker <- as.numeric(ins$smoker)

# Drop unnecessary variables
ins$region <- NULL

# Summary statistics plot
insplot <- ins
insplot$group <- ifelse(insplot$charges > mean(insplot$charges), "high", "low")
ggpairs(insplot, aes(color=group, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()

# Split training and testing set
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(ins), size = floor(.75*nrow(ins)), replace = F)
trainingData <- ins[sample, ]
testingData  <- ins[-sample, ]


#######################################################################

dataPrepare <- function(df){
  
  head(df)
  
  # Summary of training set and testing set
  str(df)
  summary(df)
  
  # Convert factor to numeric
  ins <- df
  ins$sex <- as.numeric(ins$sex)
  ins$smoker <- as.numeric(ins$smoker)
  
  # Drop unnecessary variables (region)
  ins$region <- NULL
  return(ins)
}


#######################################################################

EDA <- function(traindata){
  
  # Distribution of Response Variable: charges
  hist(traindata$charges, # histogram
       col="mistyrose", # color
       border="black",
       prob = TRUE, # plot densities
       xlab = "charges",
       main = "Charges Distribution")
  lines(density(traindata$charges), 
        lwd = 2, # line thickness 
        col = "black")
  
  # Correlation plot
  corins <- traindata
  trainCorr = cor(na.omit(corins))
  head(round(trainCorr,2))
  corrplot(trainCorr, method = "number", type="lower", tl.srt=45)
  
  
  trainingPrepared <- traindata
  
  drawBoxPlots <- function(df){
    
    dfMelt <- melt(df, id="charges")
    
    p <- ggplot(dfMelt, aes(factor(value), charges)) 
    p + geom_boxplot() + facet_wrap(~variable, scale="free")  
    
  }
  
  # Normality Test
  
  ggplot(trainingPrepared, aes(charges)) +
    geom_density() +
    scale_x_continuous(breaks = c(0,200000,400000,755000))
  
  
  qqnorm(trainingPrepared$charges,main = "Normal Q-Q Plot",
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         plot.it = TRUE)
  qqline(trainingPrepared$charges)
  
  
  # LogNormal Distribution of charges
  
  hist(log(traindata$charges), # histogram
       col="mistyrose", # color
       border="black",
       prob = TRUE, # plot density
       xlab = "charges",
       main = "Charges Distribution")
  lines(density(log(traindata$charges)),
        lwd = 2, # line thickness
        col = "black")
  
  qqnorm(log(trainingPrepared$charges),main = "Normal Q-Q Plot",
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         plot.it = TRUE)
  qqline(log(trainingPrepared$charges))
  
  # Homoscedesticity Test
  numeric_var <- names(trainingPrepared)[which(sapply(trainingPrepared, is.numeric))]
  
  train_numeric <- trainingPrepared[,numeric_var]
  
  train_numeric_melt <- melt(train_numeric, id="charges")
  
  ggplot(train_numeric_melt,aes(x=value, y=log(charges))) +
    facet_wrap(~variable, scales = "free")+
    geom_point()
  
}

#######################################################################


multiLinearRegression <- function(trainingPrepared,testingPrepared){
  
  # Multiple Linear Regression
  
  chargesML = lm(log(charges) ~ . , data = trainingPrepared)
  
  print(summary(chargesML))
  
  plot(chargesML)
  
  testing_y<- testingPrepared$charges
  predicted_y<- predict(chargesML,testingPrepared)
  MSE<- mean((testing_y-exp(predicted_y))^2,na.rm = T)
  RMSE <- MSE^0.5
  paste("Root Mean Squared Error",RMSE)
}


#######################################################################

bayesianAveraging <- function(trainingPrepared,testingPrepared){
  
  # Bayesian Approach
  
  bma_charges = bas.lm(log(charges) ~ ., data = trainingPrepared, prior = "BIC", 
                       modelprior = uniform(), method = "MCMC")
  
  # Summary result of bayesian linear approach
  print(bma_charges)
  
  print(summary(bma_charges))
  
  # Posterior Mean, Standard Deviation and Posterior Probabilities 
  
  estimatorResults <- data.frame(BMA=double(),BPM=double(),MPM=double(),HPM=double(),stringsAsFactors=FALSE)
  
  for (estimatorName in colnames(estimatorResults)) {
    print(coef(bma_charges,estimator = estimatorName))
  }
  
  
  ## 95% credible intervals for these coefficients
  confint(coef(bma_charges,estimator = estimatorName),level = 0.95)
  
  yPred <- fitted(bma_charges, type = "response", estimator = "BMA")
  
  exp_yPred <- exp(yPred)
  
  plot(trainingPrepared$charges,exp_yPred,col=c('orange'), xlab = "Actual charges",
       ylab = "Predicted charges", main = "Predicted Vs Actual charges", xaxt="n", 
       xlim = c(0,20000), ylim = c(0,30000))
  
  axis(1, at=seq(0,20000,5000), labels = c("0",'5000','10000','15000','20000'))
  
  for (estimatorName in colnames(estimatorResults)) {
    testing_y<- testingPrepared$charges
    y_pred = predict(bma_charges, testingPrepared, estimator=estimatorName)$fit
    MSE <- mean((testing_y-exp(y_pred))^2,na.rm = T)
    RMSE <- MSE^0.5
    print(paste0("Root Mean Square Error ",estimatorName," ",RMSE,sep = ""))
  }
  
  par(mfrow = c(2,3))
  
  par(ask=F)
  
  # Plot posterior
  
  plot(coef(bma_charges,estimator = 'BMA'))
  
}


# Prepare training and testing set
trainingPrepared <- dataPrepare(trainingData)
testingPrepared <- dataPrepare(testingData)
# Exploratory Data Analysis
EDA(trainingPrepared)
# Mulitiple Linear Regression
multiLinearRegression(trainingPrepared,testingPrepared)
# Bayesian Model Averaging
bayesianAveraging(trainingPrepared,testingPrepared)

