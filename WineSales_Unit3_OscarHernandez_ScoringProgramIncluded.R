############ Wine Sales Unit 3 - Oscar Hernandez ################################################
############SCORING PROGRAM INCLUDED ############################################################

#####Import the necessary packages
library(mice)
library(MASS)
library(corrplot)
library(gmodels)
library(robustHD)
library(pscl)
#####

##### Set working directory and load test data 
setwd("C:/Users/herna_000/Desktop")
train_wine <- read.csv("Wine_Training.csv", header=TRUE)
original_train_wine <- train_wine
#####

############ PART 1: DATA EXPLORATION ##########################################################
str(original_train_wine)
summary(original_train_wine)

hist(original_train_wine$TARGET) #zero inflated; otherwise normal 
boxplot(original_train_wine$TARGET)

CrossTable(original_train_wine$LabelAppeal)
CrossTable(original_train_wine$STARS)

par(mfrow=c(1,3))
hist(original_train_wine$Alcohol, main="Alcohol")
hist(original_train_wine$AcidIndex, main="AcidIndex")
hist(original_train_wine$TARGET,main="TARGET")
par(mfrow=c(1,1))

hist(original_train_wine$FixedAcidity) #outliers; otherwise normal; lot of values between 5 - 10
boxplot(original_train_wine$FixedAcidity)

hist(original_train_wine$VolatileAcidity) #outliers; otherwise normal; lots of values between 0 - 1 
boxplot(original_train_wine$VolatileAcidity)

hist(original_train_wine$CitricAcid)  #outliers; otherwise normal; lots of values around 0 - 0.5
boxplot(original_train_wine$CitricAcid)

hist(original_train_wine$ResidualSugar) #outliers; otherwise normal; lots of values around 0 - 20 
boxplot(original_train_wine$ResidualSugar)

hist(original_train_wine$Chlorides) #outliers; otherwise normal; lots of values around 0 - 0.2
boxplot(original_train_wine$Chlorides)

hist(original_train_wine$FreeSulfurDioxide)  #outliers; otherwise normal; lots of values around 0 - 100
boxplot(original_train_wine$FreeSulfurDioxide)

hist(original_train_wine$TotalSulfurDioxide)  #outliers; otherwise normal; lots of values around 0 - 200
boxplot(original_train_wine$TotalSulfurDioxide)

hist(original_train_wine$Density)  #outliers; otherwise normal; lots values around 0.98 - 1
boxplot(original_train_wine$Density)

hist(original_train_wine$pH)  #outliers; otherwise normal; lots of values 3 - 3.5 
boxplot(original_train_wine$pH)

hist(original_train_wine$Sulphates)  #outliers; otherwise normal; lots of values around 0 - 1
boxplot(original_train_wine$Sulphates)

hist(original_train_wine$Alcohol)  #outliers; othewise normal; lotsof values around 10 
boxplot(original_train_wine$Alcohol)

hist(original_train_wine$AcidIndex)  #some outliers; not normal; positive skew
boxplot(original_train_wine$AcidIndex)

#Scatterplot
plot(original_train_wine$FixedAcidity, original_train_wine$TARGET)
plot(original_train_wine$VolatileAcidity, original_train_wine$TARGET)
plot(original_train_wine$CitricAcid, original_train_wine$TARGET)
plot(original_train_wine$ResidualSugar, original_train_wine$TARGET)


#Correlation
cor(original_train_wine$FixedAcidity, original_train_wine$TARGET)
cor(original_train_wine$VolatileAcidity, original_train_wine$TARGET)
cor(original_train_wine$CitricAcid, original_train_wine$TARGET)
cor(original_train_wine$Density, original_train_wine$TARGET)
cor(original_train_wine$AcidIndex, original_train_wine$TARGET)
cor(original_train_wine$ResidualSugar, original_train_wine$TARGET, use = "na.or.complete")
cor(original_train_wine$Chlorides, original_train_wine$TARGET, use = "na.or.complete")
cor(original_train_wine$FreeSulfurDioxide, original_train_wine$TARGET, use = "na.or.complete")
cor(original_train_wine$TotalSulfurDioxide, original_train_wine$TARGET, use = "na.or.complete")
cor(original_train_wine$pH, original_train_wine$TARGET, use = "na.or.complete")
cor(original_train_wine$Sulphates, original_train_wine$TARGET, use = "na.or.complete")
cor(original_train_wine$Alcohol, original_train_wine$TARGET, use = "na.or.complete")
cor(original_train_wine$LabelAppeal, original_train_wine$TARGET, use = "na.or.complete")
cor(original_train_wine$STARS, original_train_wine$TARGET, use = "na.or.complete")

wine_train_corr <- cor(original_train_wine[, !names(original_train_wine) %in% c("INDEX")],  use="na.or.complete")
corrplot(wine_train_corr,method="color")

mean(original_train_wine$TARGET)
var(original_train_wine$TARGET)

############ PART 2: DATA PREPARATION ##########################################################
removed_columns <- data.frame(train_wine$INDEX)

#Turn negative values into missing values 
train_wine$Alcohol[train_wine$Alcohol<0] <- NA
train_wine$Chlorides[train_wine$Chlorides<0] <- NA
train_wine$CitricAcid[train_wine$CitricAcid<0] <- NA
train_wine$FixedAcidity[train_wine$FixedAcidity<=0] <- NA
train_wine$FreeSulfurDioxide[train_wine$FreeSulfurDioxide<0] <- NA
train_wine$ResidualSugar[train_wine$ResidualSugar<0] <- NA
train_wine$Sulphates[train_wine$Sulphates<0] <- NA
train_wine$TotalSulfurDioxide[train_wine$TotalSulfurDioxide<0] <- NA
train_wine$VolatileAcidity[train_wine$VolatileAcidity<0] <- NA

#Impute missing values using MICE package
train__wine_temp1 <- mice(train_wine[, !names(train_wine) %in% c("INDEX")], m =5, maxit=10, meth = "rf", seed = 500)
train_wine_imp1 <-complete(train__wine_temp1)  #restart here if needed
train_wine_imp1 <- cbind(removed_columns, train_wine_imp1)

colnames(train_wine_imp1)[colnames(train_wine_imp1)=="train_wine.INDEX"] <- "INDEX"   

str(train_wine_imp1)
summary(train_wine_imp1)

#Convert certain variables into categorical variables 
CrossTable(train_wine_imp1$LabelAppeal)

train_wine_imp1$LabelAppeal[train_wine_imp1$LabelAppeal==-2] <- "Neg_Two"
train_wine_imp1$LabelAppeal[train_wine_imp1$LabelAppeal==-1] <- "Neg_One"
train_wine_imp1$LabelAppeal[train_wine_imp1$LabelAppeal==0] <- "z_Indifferent"
train_wine_imp1$LabelAppeal[train_wine_imp1$LabelAppeal==1] <- "Pos_One"
train_wine_imp1$LabelAppeal[train_wine_imp1$LabelAppeal==2] <- "Pos_Two"

train_wine_imp1$LabelAppeal <- as.factor(train_wine_imp1$LabelAppeal)  

#Recode STARS variable; Turn NA's into "Missing"  
CrossTable(train_wine_imp1$STARS)
train_wine_imp1$STARS <- as.character(train_wine_imp1$STARS)

train_wine_imp1$STARS[train_wine_imp1$STARS==1] <- "One_Star"
train_wine_imp1$STARS[train_wine_imp1$STARS==2] <- "Two_Stars"
train_wine_imp1$STARS[train_wine_imp1$STARS==3] <- "Three_Stars"
train_wine_imp1$STARS[train_wine_imp1$STARS==4] <- "Four_Stars"

train_wine_imp1$STARS <- as.factor(train_wine_imp1$STARS)

#Check percentile values and possibly do windsorizing 
fa_high <- quantile(train_wine_imp1$FixedAcidity, c(.98))
va_high <- quantile(train_wine_imp1$VolatileAcidity, c(.98))
rs_high <- quantile(train_wine_imp1$ResidualSugar, c(.98))
ca_high <- quantile(train_wine_imp1$CitricAcid, c(.98))
c_high <- quantile(train_wine_imp1$Chlorides, c(.98))
fsd_high <- quantile(train_wine_imp1$FreeSulfurDioxide, c(.98))
tsd_high <- quantile(train_wine_imp1$TotalSulfurDioxide, c(.98))
d_high <- quantile(train_wine_imp1$Density, c(.98))
ph_high <- quantile(train_wine_imp1$pH, c(.98))
s_high <- quantile(train_wine_imp1$Sulphates, c(.98))
a_high <- quantile(train_wine_imp1$Alcohol, c(.98))
ai_high <- quantile(train_wine_imp1$AcidIndex, c(.98))

train_wine_imp1$FixedAcidity[train_wine_imp1$FixedAcidity > fa_high] <- fa_high
train_wine_imp1$VolatileAcidity[train_wine_imp1$VolatileAcidity > va_high] <- va_high
train_wine_imp1$ResidualSugar[train_wine_imp1$ResidualSugar>rs_high] <- rs_high
train_wine_imp1$CitricAcid[train_wine_imp1$CitricAcid>ca_high] <- ca_high
train_wine_imp1$Chlorides[train_wine_imp1$Chlorides>c_high] <- c_high
train_wine_imp1$FreeSulfurDioxide[train_wine_imp1$FreeSulfurDioxide>fsd_high] <- fsd_high
train_wine_imp1$TotalSulfurDioxide[train_wine_imp1$TotalSulfurDioxide>tsd_high] <- tsd_high
train_wine_imp1$pH[train_wine_imp1$pH>ph_high] <- ph_high
train_wine_imp1$Sulphates[train_wine_imp1$Sulphates>s_high] <- s_high
train_wine_imp1$Alcohol[train_wine_imp1$Alcohol>a_high] <- a_high
train_wine_imp1$AcidIndex[train_wine_imp1$AcidIndex>ai_high] <- ai_high

#Create new variables 
train_wine_imp1$AcidicWine_Flag <- train_wine_imp1$AcidIndex
train_wine_imp1$AcidicWine_Flag <- ifelse(train_wine_imp1$AcidicWine_Flag >7, "z_AcidicWine", "NonAcidicWine")
train_wine_imp1$AcidicWine_Flag <- as.factor(train_wine_imp1$AcidicWine_Flag)

train_wine_imp1$SaltyWine_Flag <- train_wine_imp1$Chlorides
train_wine_imp1$SaltyWine_Flag <- ifelse(train_wine_imp1$SaltyWine_Flag > 0.2, "z_SaltyWine", "NonSaltyWine")
train_wine_imp1$SaltyWine_Flag <- as.factor(train_wine_imp1$SaltyWine_Flag)

train_wine_imp1$StrongWine_Flag <- train_wine_imp1$Alcohol
train_wine_imp1$StrongWine_Flag <- ifelse(train_wine_imp1$StrongWine_Flag > 15, "z_StrongWine", "WeakWine")
train_wine_imp1$StrongWine_Flag <- as.factor(train_wine_imp1$StrongWine_Flag)

#Square Root Transformation 
train_wine_imp1$VolatileAcidity <- sqrt(train_wine_imp1$VolatileAcidity)
train_wine_imp1$ResidualSugar <- sqrt(train_wine_imp1$ResidualSugar)
train_wine_imp1$CitricAcid <- sqrt(train_wine_imp1$CitricAcid)
train_wine_imp1$Chlorides <- sqrt(train_wine_imp1$Chlorides)
train_wine_imp1$FreeSulfurDioxide <- sqrt(train_wine_imp1$FreeSulfurDioxide)
train_wine_imp1$TotalSulfurDioxide <- sqrt(train_wine_imp1$TotalSulfurDioxide)

#Check variable distribtuions
hist(train_wine_imp1$FixedAcidity)
hist(train_wine_imp1$VolatileAcidity) #sq
hist(train_wine_imp1$ResidualSugar) #sq
hist(train_wine_imp1$CitricAcid) #sq
hist(train_wine_imp1$Chlorides)#sq
hist(train_wine_imp1$FreeSulfurDioxide) #sq
hist(train_wine_imp1$TotalSulfurDioxide) #sq
hist(train_wine_imp1$Density)
hist(train_wine_imp1$pH)
hist(train_wine_imp1$Sulphates)
hist(train_wine_imp1$Alcohol)
hist(train_wine_imp1$AcidIndex)

str(train_wine_imp1)
summary(train_wine_imp1)

exp(-0.79)
############ PART 3: BUILD MODELS ##############################################################

#####MLR Model - Using Stepwise Variable Selection #####
#Need to specifiy the upper model and lower models
# Define the upper model as the FULL model 
upper.lm <- lm(TARGET ~ .,data=train_wine_imp1[, !names(train_wine_imp1) %in% "INDEX"])

# Define the lower model as the Intercept model 
lower.lm <- lm(TARGET ~ 1,data=train_wine_imp1[, !names(train_wine_imp1) %in% "INDEX"]) 

# Need a simple linear regression model to initialize stepwise selection 
initialize_stepwise.lm <- lm(TARGET ~ Density,data=train_wine_imp1[, !names(train_wine_imp1) %in% "INDEX"]) 

#Create stepwise.lm
stepwise.lm <- stepAIC(object=initialize_stepwise.lm,scope=list(upper=formula(upper.lm),lower=~1), direction=c('both'))
summary(stepwise.lm)

#####Poisson Model ######
poisson.glm <- glm(TARGET ~ STARS+ LabelAppeal+ Density+ Alcohol+AcidIndex+ pH+ TotalSulfurDioxide+ Chlorides+ VolatileAcidity+
                     CitricAcid + Sulphates + AcidicWine_Flag + SaltyWine_Flag + StrongWine_Flag + ResidualSugar + FreeSulfurDioxide,
                   family = "poisson"(link="log"), data=train_wine_imp1)
summary(poisson.glm)

#####Negative Binomial Model #####
NBR.glm <- glm.nb(TARGET~ STARS+ LabelAppeal+ Density+ Alcohol+AcidIndex+ pH+ TotalSulfurDioxide+ Chlorides+ VolatileAcidity+
                    CitricAcid + Sulphates + AcidicWine_Flag + SaltyWine_Flag + StrongWine_Flag, ResidualSugar + FreeSulfurDioxide,
                  data=train_wine_imp1)
summary(NBR.glm)

#####ZI Poisson Model#####
zip.glm<-zeroinfl(TARGET~ STARS+ LabelAppeal+ Density+ Alcohol+AcidIndex+  pH+ TotalSulfurDioxide+ Chlorides+ VolatileAcidity+
                    CitricAcid + Sulphates + AcidicWine_Flag + SaltyWine_Flag + StrongWine_Flag +ResidualSugar + FreeSulfurDioxide, 
                  data=train_wine_imp1)
summary(zip.glm)

####ZI NB Model#####
zinb.glm <- zeroinfl(TARGET~ STARS+ LabelAppeal+ Density+ Alcohol+AcidIndex+ pH+ TotalSulfurDioxide+ Chlorides+ VolatileAcidity+
                       CitricAcid+ Sulphates + AcidicWine_Flag + SaltyWine_Flag + StrongWine_Flag, 
                     dist="negbin", EM=TRUE, data=train_wine_imp1)
summary(zinb.glm)


############ PART 4: SELECT MODELS #############################################################

AIC(stepwise.lm)
BIC(stepwise.lm)
print(-2*logLik(stepwise.lm, REML = TRUE))

AIC(poisson.glm)
BIC(poisson.glm)
print(-2*logLik(poisson.glm), REML=TRUE)

AIC(NBR.glm)
BIC(NBR.glm)
print(-2*logLik(NBR.glm), REML=TRUE)

AIC(zip.glm)
AIC(zip.glm, k=log(nrow(train_wine_imp1))) #BIC
print(-2*logLik(zip.glm), REML=TRUE)

AIC(zinb.glm)
AIC(zinb.glm, k=log(nrow(train_wine_imp1))) #BIC
print(-2*logLik(zinb.glm), REML=TRUE)

vuong(poisson.glm, zip.glm)
vuong(NBR.glm, zinb.glm)


##########################STAND ALONE SCORING PROGRAM################################
original_test_wine <- read.csv("Wine_Test.csv", header = TRUE)
test_wine <- original_test_wine

removed_columns_test <- data.frame(test_wine$INDEX)

#Turn negative values into missing values 
test_wine$Alcohol[test_wine$Alcohol<0] <- NA
test_wine$Chlorides[test_wine$Chlorides<0] <- NA
test_wine$CitricAcid[test_wine$CitricAcid<0] <- NA
test_wine$FixedAcidity[test_wine$FixedAcidity<0] <- NA
test_wine$FreeSulfurDioxide[test_wine$FreeSulfurDioxide<0] <- NA
test_wine$ResidualSugar[test_wine$ResidualSugar<0] <- NA
test_wine$Sulphates[test_wine$Sulphates<0] <- NA
test_wine$TotalSulfurDioxide[test_wine$TotalSulfurDioxide<0] <- NA
test_wine$VolatileAcidity[test_wine$VolatileAcidity<0] <- NA

#Impute missing values using MICE package
test_wine_temp1 <- mice(test_wine[, !names(test_wine) %in% c("INDEX")], m =5, maxit=10, meth = "rf", seed = 500)
test_wine_imp1 <-complete(test_wine_temp1)  #restart here if needed
test_wine_imp1 <- cbind(removed_columns_test, test_wine_imp1)

colnames(test_wine_imp1)[colnames(test_wine_imp1)=="test_wine.INDEX"] <- "INDEX"   

#Convert certain variables into categorical variables 

test_wine_imp1$LabelAppeal[test_wine_imp1$LabelAppeal==-2] <- "Neg_Two"
test_wine_imp1$LabelAppeal[test_wine_imp1$LabelAppeal==-1] <- "Neg_One"
test_wine_imp1$LabelAppeal[test_wine_imp1$LabelAppeal==0] <- "z_Indifferent"
test_wine_imp1$LabelAppeal[test_wine_imp1$LabelAppeal==1] <- "Pos_One"
test_wine_imp1$LabelAppeal[test_wine_imp1$LabelAppeal==2] <- "Pos_Two"

test_wine_imp1$LabelAppeal <- as.factor(test_wine_imp1$LabelAppeal)  

#Recode STARS variable; Turn NA's into "Missing" 
test_wine_imp1$STARS <- as.character(test_wine_imp1$STARS)

test_wine_imp1$STARS[test_wine_imp1$STARS==1] <- "One_Star"
test_wine_imp1$STARS[test_wine_imp1$STARS==2] <- "Two_Stars"
test_wine_imp1$STARS[test_wine_imp1$STARS==3] <- "Three_Stars"
test_wine_imp1$STARS[test_wine_imp1$STARS==4] <- "Four_Stars"

test_wine_imp1$STARS <- as.factor(test_wine_imp1$STARS)

#Check percentile values and possibly do windsorizing 
fa_high1 <- quantile(train_wine_imp1$FixedAcidity, c(.98))
va_high1 <- quantile(test_wine_imp1$VolatileAcidity, c(.98))
rs_high1 <- quantile(test_wine_imp1$ResidualSugar, c(.98))
ca_high1 <- quantile(test_wine_imp1$CitricAcid, c(.98))
c_high1 <- quantile(test_wine_imp1$Chlorides, c(.98))
fsd_high1 <- quantile(test_wine_imp1$FreeSulfurDioxide, c(.98))
tsd_high1 <- quantile(test_wine_imp1$TotalSulfurDioxide, c(.98))
d_high1 <- quantile(test_wine_imp1$Density, c(.98))
ph_high1 <- quantile(test_wine_imp1$pH, c(.98))
s_high1 <- quantile(test_wine_imp1$Sulphates, c(.98))
a_high1 <- quantile(test_wine_imp1$Alcohol, c(.98))
ai_high1 <- quantile(test_wine_imp1$AcidIndex, c(.98))

test_wine_imp1$FixedAcidity[test_wine_imp1$FixedAcidity > fa_high1] <- fa_high1
test_wine_imp1$VolatileAcidity[test_wine_imp1$VolatileAcidity > va_high1] <- va_high1
test_wine_imp1$ResidualSugar[test_wine_imp1$ResidualSugar>rs_high1] <- rs_high1
test_wine_imp1$CitricAcid[test_wine_imp1$CitricAcid>ca_high1] <- ca_high1
test_wine_imp1$Chlorides[test_wine_imp1$Chlorides>c_high1] <- c_high1
test_wine_imp1$FreeSulfurDioxide[test_wine_imp1$FreeSulfurDioxide>fsd_high1] <- fsd_high1
test_wine_imp1$TotalSulfurDioxide[test_wine_imp1$TotalSulfurDioxide>tsd_high1] <- tsd_high1
test_wine_imp1$pH[test_wine_imp1$pH>ph_high1] <- ph_high1
test_wine_imp1$Sulphates[test_wine_imp1$Sulphates>s_high1] <- s_high1
test_wine_imp1$Alcohol[test_wine_imp1$Alcohol>a_high1] <- a_high1
test_wine_imp1$AcidIndex[test_wine_imp1$AcidIndex>ai_high1] <- ai_high1

#Create new variables 
test_wine_imp1$AcidicWine_Flag <- test_wine_imp1$AcidIndex
test_wine_imp1$AcidicWine_Flag <- ifelse(test_wine_imp1$AcidicWine_Flag >7, "z_AcidicWine", "NonAcidicWine")
test_wine_imp1$AcidicWine_Flag <- as.factor(test_wine_imp1$AcidicWine_Flag)

test_wine_imp1$SaltyWine_Flag <- test_wine_imp1$Chlorides
test_wine_imp1$SaltyWine_Flag <- ifelse(test_wine_imp1$SaltyWine_Flag > 0.2, "z_SaltyWine", "NonSaltyWine")
test_wine_imp1$SaltyWine_Flag <- as.factor(test_wine_imp1$SaltyWine_Flag)

test_wine_imp1$StrongWine_Flag <- test_wine_imp1$Alcohol
test_wine_imp1$StrongWine_Flag <- ifelse(test_wine_imp1$StrongWine_Flag > 15, "z_StrongWine", "WeakWine")
test_wine_imp1$StrongWine_Flag <- as.factor(test_wine_imp1$StrongWine_Flag) 

#Square Root Transformation 
test_wine_imp1$VolatileAcidity <- sqrt(test_wine_imp1$VolatileAcidity)
test_wine_imp1$ResidualSugar <- sqrt(test_wine_imp1$ResidualSugar)
test_wine_imp1$CitricAcid <- sqrt(test_wine_imp1$CitricAcid)
test_wine_imp1$Chlorides <- sqrt(test_wine_imp1$Chlorides)
test_wine_imp1$FreeSulfurDioxide <- sqrt(test_wine_imp1$FreeSulfurDioxide)
test_wine_imp1$TotalSulfurDioxide <- sqrt(test_wine_imp1$TotalSulfurDioxide)



#Score test data and save predictions as P_TARGET
test_wine_imp1$P_TARGET <- predict(zip.glm, newdata = test_wine_imp1, type="response")

#Scored data file
scores <- test_wine_imp1[c("INDEX", "P_TARGET")]
write.csv(scores, file = "Scored_WineSales.csv", row.names = FALSE)

