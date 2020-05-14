# Name:Fredrick J.sigalla
# Std No.: A00277360

install.packages("class")
library(class)
install.packages("gmodels")
library(gmodels)
install.packages("dplyr")
library(dplyr)
install.packages("C50")
library(C50)
install.packages("rmarkdown")
library(rmarkdown)
install.packages("psych")
library(psych)

# importing contraceptive method choice data set
cmc <- read.csv("data/cmc/cmc_data.csv", stringsAsFactors = FALSE)
str(cmc)
# importing combine cycle power plant data set
cpp <- read.csv("data/cpp/cpp.csv", stringsAsFactors = TRUE, sep = ";")
str(cpp)

# f_normalize - rescale a vector into a normal form of 0 to 1 scale
f_normalize <- function(x) 
{
  x_normalized <- (x - min(x))/diff(range(x))
  return(x_normalized)
}

# f_z_score_standardization - rescale a vector into a normal form by using z-score value
f_z_score_standardization <- function(x)
{	
  x_standardized <- (x - mean(x))/sd(x)
  return(x_standardized)
}

# f_age_range - converts age numeric values into range character values
f_age_range <- function(x)
{
    if(x >= 1 && x <= 14){
    return("Age_Btn_1_14_Incl")
  }
  else if(x >= 15 && x <= 28){
    return("Age_Btn_15_28_Incl")
  }
  else if(x >= 29 && x <= 42){
    return("Age_Btn_29_42_Incl")
  }
  else if(x >= 43 && x <= 56){
    return("Age_Btn_43_56_Incl")
  }
}

# f_num_children - converts number of children numeric values into range character values
f_num_children <- function(x)
{
  if(x >= 0 && x <= 3){
    return("C_Btn_0_3_Incl")
  }
  else if(x >= 4 && x <= 7){
    return("C_Btn_4_7_Incl")
  }
  else if(x >= 8 && x <= 11){
    return("C_Btn_8_11_Incl")
  }
  else if(x >= 12 && x <= 15){
    return("C_Btn_12_15_Incl")
  }
  else if(x >= 16){
    return("C_16_Above_Incl")
  }
}

# giving variable names meaningfull names as described on the data set data dictionary
new_variable_names <- c("Age", "Education", "Husband_Education", "Children", "Religion", "Working", "Husband_Occupation", "Std_Living", "Media_Exposure", "Contraceptive_Method")
names(cmc)

# renaming dataset variable names
names(cmc) <- new_variable_names
str(cmc)

# Contractptive_Method is the target variable
# changing Contraceptive_Method variable to factor

cmc$Contraceptive_Method <- factor(cmc$Contraceptive_Method, levels = c(1, 2, 3), labels = c("No-use", "Long-term", "Short-term"))
str(cmc)

# checking for any incomplete record (null value)
nrow(as.data.frame(complete.cases(cmc)))

# number of complete cases (1472) is equal to total objects (1472), which means there is no null value

# Interogating summary statistics for all numerical features
summary(cmc[-10])
boxplot(cmc$Age, main = "Box-Plot of Women Age", ylab = "Age in Years")
boxplot(cmc$Age, main = "Box-Plot - No. Children", ylab = "Number children")

# all numerical values are normalize by using function f_normalize so as they all lie with 0 to 1 scale
cmc_normalized <- as.data.frame(lapply(cmc[-10], f_normalize))
summary(cmc_normalized) # all numerical values of cmc_normalized are in 0 to 1 scale
str(cmc_normalized)

# creating a vector of target elements
target_variable <- cmc$Contraceptive_Method
str(target_variable)
summary(target_variable)

# checking to see if the objects/rows are randomly distributed before splittng a data set into training
# and testing data sets, specifically the target variable (Contraceptive_Method).
head(target_variable, 30)
tail(target_variable, 30)

table(target_variable[1:1031])
table(target_variable[1032:1472])

# From the above analysis, for almost fair distribution of target variables between training and
# and testing data set, the data set will be distributed in the 70/30 ration, where, 70 percent of the objects
# which is equivalent to 1031 objects will be used for traning the model, and 30 percent, which is equivalent
# to 441 objects, will be used to test the model

# trainin target variable
target_variable_train <- target_variable[1:1031]
round(prop.table(table(target_variable_train)) * 100, 2)

#test target variable
target_variable_test <- target_variable[1032:1472]
round(prop.table(table(target_variable_test)) * 100, 2)

# training data set
cmc_normalized_training <- cmc_normalized[1:1031,]

# testing data set
cmc_normalized_testing <- cmc_normalized[1032:1472,]

# implementing kNN algorithm on the training data set
# k = 32 will be used, which has been obtained by approximating the square root of the number of
# objects in the training data set (1031)
target_variable_pred <- knn(train = cmc_normalized_training, 
                            test = cmc_normalized_testing,
                            cl = target_variable_train, k = 32)


# evaluating model performance
CrossTable(x = target_variable_test, y = target_variable_pred, prop.chisq = FALSE)

# decision tree works best with none numeric data, therefore, all feature are converted in characters/factors

# using f_age_range function to covert age values into range of values
cmc$Age <- as.character(lapply(cmc$Age, f_age_range))
str(cmc)

# using f_num_children to convert number of children into range of values
cmc$Children <- as.character(lapply(cmc$Children, f_num_children))
str(cmc)

# converting all categorical variables to factors
cmc$Education <- factor(cmc$Education, levels = c(1, 2, 3, 4), labels = c("low", "high", "high", "high"))
cmc$Husband_Education <- factor(cmc$Husband_Education, levels = c(1, 2, 3, 4), labels = c("low", "high", "high", "high"))
cmc$Religion <- factor(cmc$Religion, levels = c(0, 1), labels = c("Non-Islam", "Islam"))
cmc$Working <- factor(cmc$Working, levels = c(0, 1), labels = c("Yes", "No"))

cmc$Husband_Occupation <- factor(cmc$Husband_Occupation, levels = c(1, 2, 3, 4), labels = c("1", "2", "3", "4")) #data set doenst have categorical 
# values for the numbers 1 - 4, hence, number 1 - 4 will be converted into factors and treated as character and not integers

cmc$Std_Living <- factor(cmc$Std_Living, levels = c(1, 2, 3, 4), labels = c("low", "high", "high", "high"))
cmc$Media_Exposure <- factor(cmc$Media_Exposure, levels = c(0, 1), labels = c("Good", "Not good"))
cmc$Contraceptive_Method <- factor(cmc$Contraceptive_Method, levels = c(1, 2, 3), labels = c("No-use", "Long-term", "Short-term"))

str(cmc)

# splitting the data set into training and testing data set in the 9:1 ratio respectively
# Contraceptive method in the target feature
round(prop.table(table(cmc$Contraceptive_Method)) * 100, 2)

# training data set
cmc_dt_train <- cmc[1:1325,]
str(cmc_dt_train)

# testing data set
cmc_dt_test <- cmc[1326:1472,]
str(cmc_dt_test)

round(prop.table(table(cmc_dt_train$Contraceptive_Method)) * 100,2)
round(prop.table(table(cmc_dt_test$Contraceptive_Method)) * 100,2)

# training and testing data set doesnt seem to have a good ratio of the target feature
# will randomise the data before splitting the data into train and test data set

set.seed(12345)
cmc_rand <- cmc[order(runif(1472)),]
str(cmc_rand)

round(prop.table(table(cmc_rand$Contraceptive_Method)) * 100,2)

# traning data set after randomization
cmc_rand_train <- cmc_rand[1:1325,]
str(cmc_rand_train)

# test data set after randomization
cmc_rand_test <- cmc_rand[1326:1472,]
str(cmc_rand_test)

round(prop.table(table(cmc_rand_test$Contraceptive_Method)) * 100, 2)
round(prop.table(table(cmc_rand_train$Contraceptive_Method)) * 100, 2)

# C5.0 Decisioin Tree Implementation algorithm will be used to create the model
cmc_dt_model <- C5.0(cmc_rand_train[-10], cmc_rand_train$Contraceptive_Method) 

summary(cmc_dt_model)

# evaluating the model
cmc_dt_pred <- predict(cmc_dt_model, cmc_rand_test[-10]) 

CrossTable(cmc_rand_test$Contraceptive_Method, cmc_dt_pred, prop.chisq = FALSE, 
           prop.c = FALSE, prop.r = FALSE, dnn = c("Actual CMC", "Predicted CMC"))

# checking if boosting can increase accuracy
cmc_dt_model_boosted <- C5.0(cmc_rand_train[-10], cmc_rand_train$Contraceptive_Method, trials = 10)

cmc_dt_pred_boosted <- predict(cmc_dt_model_boosted, cmc_rand_test[-10])

CrossTable(cmc_rand_test$Contraceptive_Method, cmc_dt_pred_boosted, prop.chisq = FALSE, 
           prop.c = FALSE, prop.r = FALSE, dnn = c("Actual CMC", "Predicted CMC"))

# Even after applying boosting, the accuracy remains the same.

# Multiple Regression
str(cpp)

# intuitive columname - this is from the data set documentation
features <- c("Temperature", "Vacuum", "Pressure", "Humidity", "Energy")
colnames(cpp)
colnames(cpp) <- features
colnames(cpp)
str(cpp)

# interogating the data set
# checking for null values
cpp[rowSums(is.na(cpp)) > 0, ]

# statistical summary
summary(cpp)
summary(cpp$Energy)

# checking for outliers in dependent variable - Energy
boxplot(cpp$Energy, main = "Boxplot - Cycle Power Plant", ylab = "Electrical Energy")
# checking for dependent variable normal distribution (Linear Regresson assume the dependent variable is normally distributed)
hist(cpp$Energy, main = "Histogram - Cycle Power Plant", xlab = "Electrical Energy")

# Exploring the correlation among independent variables and dependent variable
str(cpp)
pairs.panels(cpp[c('Temperature','Vacuum','Pressure','Humidity','Energy')])

# Training the model
cpp_model <- lm(cpp$Energy ~ ., data = cpp)
cpp_model
summary(cpp$Energy)

# Evaluating the model
summary(cpp_model)
