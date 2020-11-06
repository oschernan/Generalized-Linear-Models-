############ Insurance Claims Unit 2 - Oscar Hernandez ################################################
####Includes Stand Alone Scoring Program#####

#####Import the necessary packages
library(mice)
library(MASS)
library(gmodels)
library(forcats)
library(ROSE)
library(InformationValue)
library(corrplot)

#####

##### Set working directory and load test data 
setwd("C:/Users/herna_000/Desktop")
train_original <- read.csv("logit_insurance.csv", header=TRUE)
train <- train_original

#####

############ PART 1: DATA EXPLORATION ##########################################################
str(train_original)
summary(train_original)
summary(train)

hist(train$TARGET_AMT)  #a lot of zero values; right, positive skew
boxplot(train$TARGET_AMT)

CrossTable(train$AGE)
hist(train$AGE) #looks normal 
boxplot(train$AGE) #outliers present

hist(train$BLUEBOOK) #positive, right skew
boxplot(train$BLUEBOOK) #outliers present

hist(train$CAR_AGE) #normal but with zero-inflation shape
boxplot(train$CAR_AGE)

hist(train$CLM_FREQ)  # majority of zero values 
boxplot(train$CLM_FREQ)
CrossTable(train$CLM_FREQ)

hist(train$HOMEKIDS) #majority of zero values
boxplot(train$HOMEKIDS)
CrossTable(train$HOMEKIDS)

par(mfrow=c(1,3))
hist(train$HOME_VAL) #normal with zero-inflation shape 
hist(train$OLDCLAIM) 
hist(train$CAR_AGE)
par(mfrow=c(1,1))

hist(train$INCOME) #heavy right, positive skew


boxplot(train$HOME_VAL)
boxplot(train$INCOME) #outliers present

hist(train$KIDSDRIV) #majority of zero values
boxplot(train$KIDSDRIV)

hist(train$MVR_PTS)  #majority of zero values
boxplot(train$MVR_PTS)
CrossTable(train$MVR_PTS)

#signficant amount of outliers; majority of zero values or less than $10000
boxplot(train$OLDCLAIM)

hist(train$TIF) #lot of zero values
boxplot(train$TIF)
CrossTable(train$TIF)

hist(train$TRAVTIME) #normal with some outliers
boxplot(train$TRAVTIME)

hist(train$YOJ) #normal with zero-inlation shape
boxplot(train$YOJ)

CrossTable(train$JOB)


numeric <- subset(train_original, select = c(TARGET_AMT, AGE, CAR_AGE, CLM_FREQ, HOMEKIDS, KIDSDRIV, MVR_PTS, TIF, TRAVTIME, YOJ), na.rm = TRUE)
c <- cor(numeric)
corrplot(c, method = "square")

############ PART 2: DATA PREPARATION ##########################################################

index_column <- data.frame(train$INDEX)

#Make sure that each variable is treated as continuous or categorical 
train$TARGET_FLAG <- as.factor(train$TARGET_FLAG)
train$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", train$BLUEBOOK)))
train$CAR_TYPE <- as.factor(train$CAR_TYPE)
train$CAR_USE <- as.factor(train$CAR_USE)
train$EDUCATION <- as.factor(train$EDUCATION)
train$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", train$HOME_VAL)))
train$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", train$INCOME)))
train$JOB <- as.factor(train$JOB)
train$MSTATUS <- as.factor(train$MSTATUS)
train$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", train$OLDCLAIM)))
train$PARENT1 <- as.factor(train$PARENT1)
train$RED_CAR <- as.factor(ifelse(train$RED_CAR=="yes", 1, 0))
train$REVOKED <- as.factor(train$REVOKED)
train$SEX <- as.factor(train$SEX)
train$URBANICITY <- ifelse(train$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
train$URBANICITY <- as.factor(train$URBANICITY)

#Imput missing values using MICE package
train_temp1 <- mice(train[, !names(train) %in% "INDEX"], m =5, maxit=10, meth = "rf", seed = 500)
train_imp1 <-complete(train_temp1)  #restart here if needed
train_imp1 <- cbind(index_column, train_imp1)
colnames(train_imp1)[colnames(train_imp1)=="train.INDEX"] <- "INDEX"

#Make changes to the variables in the dataset

train_imp1$JOB <- as.character(train_imp1$JOB)
train_imp1$JOB[train_imp1$JOB==""] <- "Missing_Job"
train_imp1$JOB <- as.factor(train_imp1$JOB)

train_imp1$CAR_AGE[train_imp1$CAR_AGE <0 ] <- 0

train_imp1$INCOME <- sqrt(train_imp1$INCOME) #transformed income to its square root
train_imp1$TRAVTIME <- sqrt(train_imp1$TRAVTIME) #transformed travel time to its square root
train_imp1$OLDCLAIM <-sqrt(train_imp1$OLDCLAIM) #transformed oldclaim to its natural log 
train_imp1$BLUEBOOK <-sqrt(train_imp1$BLUEBOOK) #transformed bluebook to its square root 
train_imp1$TARGET_AMT <- log(train_imp1$TARGET_AMT+1) #transformed target amount to its natural log

#Creating factor variable to show if record has or doesn't have points 
train_imp1$MVR_PTS <-ifelse(train_imp1$MVR_PTS == 0, "Zero_Points", "Has_Points")
train_imp1$MVR_PTS <-as.factor(train_imp1$MVR_PTS)

#Creating factor variable to show if kids drive or don't drive  
train_imp1$KIDSDRIV <-ifelse(train_imp1$KIDSDRIV == 0, "z_Kids_NoDrive", "Kids_Drive")
train_imp1$KIDSDRIV <-as.factor(train_imp1$KIDSDRIV)

#Creating factor variable to show if there are kids at home or not 
train_imp1$HOMEKIDS <-ifelse(train_imp1$HOMEKIDS == 0, "z_No_Kids_Home", "Kids_Home")
train_imp1$HOMEKIDS <-as.factor(train_imp1$HOMEKIDS)

summary(train_imp1)


############ PART 3: BUILD MODELS ##############################################################

#Building one model using Stepwise variable selection
#Need to specifiy the upper model and lower models
# Define the upper model as the FULL model 
upper.glm <- glm(TARGET_FLAG ~ .,data=train_imp1[, !names(train_imp1) %in% c("INDEX", "TARGET_AMT")], family = binomial())

# Define the lower model as the Intercept model 
lower.glm <- glm(TARGET_FLAG ~ 1,data=train_imp1[, !names(train_imp1) %in% c("INDEX", "TARGET_AMT")], family = binomial()) 

# Need a simple generalized linear regression model to initialize stepwise selection 
initialize_stepwise.glm <- glm(TARGET_FLAG ~ AGE,data=train_imp1[, !names(train_imp1) %in% c("INDEX", "TARGET_AMT")], family = binomial()) 



#Create stepwise.glm
stepwise.glm <- stepAIC(object=initialize_stepwise.glm,scope=list(upper=formula(upper.glm),lower=~1), direction=c('both'))
summary(stepwise.glm)
train_imp1$StepwisePrediction <- predict(stepwise.glm, type = "response")

#Building second model that is user-defined 
user_defined.glm <- glm(TARGET_FLAG ~ AGE + CAR_USE+CLM_FREQ+HOME_VAL+INCOME+MSTATUS+MVR_PTS+REVOKED+TRAVTIME+URBANICITY,data=train_imp1[, !names(train_imp1) %in% c("INDEX", "TARGET_AMT")], family = binomial())
summary(user_defined.glm)
train_imp1$UserPrediction <- predict(user_defined.glm, type = "response")

#Building third model with just numeric variables
numeric_model.glm <- glm(TARGET_FLAG ~ AGE+BLUEBOOK+CAR_AGE+CLM_FREQ+HOME_VAL+INCOME+OLDCLAIM+TIF+TRAVTIME+YOJ,data=train_imp1[, !names(train_imp1) %in% c("INDEX", "TARGET_AMT")], family = binomial())
summary(numeric_model.glm)
train_imp1$NumericPrediction <- predict(numeric_model.glm, type = "response")

#Building fourth model with just categorical variables 
categorical_model.glm <- glm(TARGET_FLAG ~ CAR_TYPE+CAR_USE+EDUCATION+HOMEKIDS+JOB+KIDSDRIV+MSTATUS+MVR_PTS+PARENT1+RED_CAR+REVOKED+SEX+URBANICITY,data=train_imp1[, !names(train_imp1) %in% c("INDEX", "TARGET_AMT")], family = binomial())
summary(categorical_model.glm)
train_imp1$CategoricalPrediction <- predict(categorical_model.glm, type = "response")
############ PART 4: SELECT MODELS #############################################################

AIC(stepwise.glm)
BIC(stepwise.glm)
AIC(user_defined.glm)
BIC(user_defined.glm)
AIC(numeric_model.glm)
BIC(numeric_model.glm)
AIC(categorical_model.glm)
BIC(categorical_model.glm)

print(-2*logLik(stepwise.glm, REML = TRUE))
print(-2*logLik(user_defined.glm, REML = TRUE))
print(-2*logLik(numeric_model.glm, REML = TRUE))
print(-2*logLik(categorical_model.glm, REML = TRUE))

library(InformationValue)
ks_stat(actuals=train_imp1$TARGET_FLAG, predictedScores=train_imp1$StepwisePrediction)
ks_stat(actuals=train_imp1$TARGET_FLAG, predictedScores=train_imp1$UserPrediction)
ks_stat(actuals=train_imp1$TARGET_FLAG, predictedScores=train_imp1$NumericPrediction)
ks_stat(actuals=train_imp1$TARGET_FLAG, predictedScores=train_imp1$CategoricalPrediction)

par(mfrow=c(1,3))
roc.curve(train_imp1$TARGET_FLAG, train_imp1$StepwisePrediction)
roc.curve(train_imp1$TARGET_FLAG, train_imp1$UserPrediction)
roc.curve(train_imp1$TARGET_FLAG, train_imp1$CategoricalPrediction)
par(mfrow=c(1,1))

roc.curve(train_imp1$TARGET_FLAG, train_imp1$NumericPrediction)
summary(stepwise.glm)



##########################STAND ALONE SCORING PROGRAM################################

test_original <- read.csv("logit_insurance_test.csv", header=TRUE)
test <- test_original

#Make the required imputations 
test_index_column <- data.frame(test$INDEX)
test$TARGET_FLAG <- as.factor(test$TARGET_FLAG)
test$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$BLUEBOOK)))
test$CAR_TYPE <- as.factor(test$CAR_TYPE)
test$CAR_USE <- as.factor(test$CAR_USE)
test$EDUCATION <- as.factor(test$EDUCATION)
test$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$HOME_VAL)))
test$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$INCOME)))
test$JOB <- as.factor(test$JOB)
test$MSTATUS <- as.factor(test$MSTATUS)
test$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$OLDCLAIM)))
test$PARENT1 <- as.factor(test$PARENT1)
test$RED_CAR <- as.factor(ifelse(test$RED_CAR=="yes", 1, 0))
test$REVOKED <- as.factor(test$REVOKED)
test$SEX <- as.factor(test$SEX)
test$URBANICITY <- ifelse(test$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
test$URBANICITY <- as.factor(test$URBANICITY)

#Imput missing values using MICE package
test_temp1 <- mice(test[, !names(test) %in% "INDEX"], m =5, maxit=10, meth = "rf", seed = 500)
test_imp1 <-complete(test_temp1)  
test_imp1 <- cbind(test_index_column, test_imp1)
colnames(test_imp1)[colnames(test_imp1)=="test.INDEX"] <- "INDEX"


test_imp1$JOB <- as.character(test_imp1$JOB)
test_imp1$JOB[test_imp1$JOB==""] <- "Missing_Job"
test_imp1$JOB <- as.factor(test_imp1$JOB)
test_imp1$CAR_AGE[test_imp1$CAR_AGE <0 ] <- 0
test_imp1$INCOME <- sqrt(test_imp1$INCOME) #transformed income to its square root
test_imp1$TRAVTIME <- sqrt(test_imp1$TRAVTIME) #transformed travel time to its square root
test_imp1$OLDCLAIM <-sqrt(test_imp1$OLDCLAIM) #transformed oldclaim to its natural log 
test_imp1$BLUEBOOK <-sqrt(test_imp1$BLUEBOOK) #transformed bluebook to its square root 

test_imp1$MVR_PTS <-ifelse(test_imp1$MVR_PTS == 0, "Zero_Points", "Has_Points")
test_imp1$MVR_PTS <-as.factor(test_imp1$MVR_PTS)
test_imp1$KIDSDRIV <-ifelse(test_imp1$KIDSDRIV == 0, "z_Kids_NoDrive", "Kids_Drive")
test_imp1$KIDSDRIV <-as.factor(test_imp1$KIDSDRIV)
test_imp1$HOMEKIDS <-ifelse(test_imp1$HOMEKIDS == 0, "z_No_Kids_Home", "Kids_Home")
test_imp1$HOMEKIDS <-as.factor(test_imp1$HOMEKIDS)

#Score test data and save predictions as P_TARGET_FLAG
test_imp1$P_TARGET_FLAG <- predict(stepwise.glm, newdata = test_imp1, type="response")

#Utilize Multiple Linear Regression Model to predict TARGET_AMT
upper.lm <- lm(BLUEBOOK ~ .,data=test_imp1[, !names(test_imp1) %in% c("INDEX", "TARGET_FLAG", "TARGET_AMT")])

# Define the lower model as the Intercept model 
lower.lm <- lm(BLUEBOOK ~ 1,data=test_imp1[, !names(test_imp1) %in% c("INDEX", "TARGET_FLAG", "TARGET_AMT")]) 

# Need a simple linear regression model to initialize stepwise selection 
initialize_stepwise.lm <- lm(BLUEBOOK ~ AGE,data=test_imp1[, !names(test_imp1) %in% c("INDEX", "TARGET_FLAG", "TARGET_AMT")]) 

stepwise.lm <- stepAIC(object=initialize_stepwise.lm,scope=list(upper=formula(upper.lm),lower=~1), direction=c('both'))

test_imp1$P_BLUEBOOK <- predict(stepwise.lm, newdata=test_imp1)
test_imp1$P_BLUEBOOK <- test_imp1$P_BLUEBOOK**2

#Predict TARGET_AMT and save as P_TARGET_AMT
test_imp1$P_TARGET_AMT <- test_imp1$P_TARGET_FLAG*test_imp1$P_BLUEBOOK

#Scored Data File
scores <- test_imp1[c("INDEX", "P_TARGET_FLAG", "P_TARGET_AMT")]
write.csv(scores, file = "Scored_InsuranceClaims.csv", row.names = FALSE)
