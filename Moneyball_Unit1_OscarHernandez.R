############ Moneyball Unit 1 - Oscar Hernandez ################################################

##### Import necessary packages
library(mice)
library(MASS)
library(car)
#####

##### Set working directory and load test data 
setwd("C:/Users/herna_000/Desktop")
moneyball <- read.csv("moneyball.csv", header=TRUE)
moneyball_original <- moneyball
#####

############ PART 1: DATA EXPLORATION ##########################################################
str(moneyball_original)
summary(moneyball_original)

#Inspect the distribution of the response variable 
par(mfrow=c(1,2))
hist(moneyball_original$TARGET_WINS, col = "#A71930", xlab = "TARGET_WINS", main = "Histogram of Wins")
boxplot(moneyball_original$TARGET_WINS, col = "#A71930", main = "Boxplot of Wins")
par(mfrow = c(1,1))

#Check the correlation between the response and each predictor variables
TEAM_BATTING_H.cor <- cor(moneyball_original$TEAM_BATTING_H, moneyball_original$TARGET_WINS)
TEAM_BATTING_2B.cor <-cor(moneyball_original$TEAM_BATTING_2B, moneyball_original$TARGET_WINS) 
TEAM_BATTING_3B.cor <-cor(moneyball_original$TEAM_BATTING_3B, moneyball_original$TARGET_WINS) 
TEAM_BATTING_HR.cor<-cor(moneyball_original$TEAM_BATTING_HR,  moneyball_original$TARGET_WINS)
TEAM_BATTING_BB.cor <-cor(moneyball_original$TEAM_BATTING_BB, moneyball_original$TARGET_WINS) 
TEAM_BATTING_HBP.cor <-cor(moneyball_original$TEAM_BATTING_HBP, moneyball_original$TARGET_WINS, use="na.or.complete") 
TEAM_BATTING_SO.cor <-cor(moneyball_original$TEAM_BATTING_SO, moneyball_original$TARGET_WINS, use="na.or.complete") 
TEAM_BASERUN_SB.cor <-cor(moneyball_original$TEAM_BASERUN_SB, moneyball_original$TARGET_WINS, use="na.or.complete") 
TEAM_BASERUN_CS.cor <-cor(moneyball_original$TEAM_BASERUN_CS, moneyball_original$TARGET_WINS, use="na.or.complete") 
TEAM_FIELDING_E.cor <-cor(moneyball_original$TEAM_FIELDING_E, moneyball_original$TARGET_WINS) 
TEAM_FIELDING_DP.cor <-cor(moneyball_original$TEAM_FIELDING_DP, moneyball_original$TARGET_WINS, use="na.or.complete") 
TEAM_PITCHING_BB.cor <-cor(moneyball_original$TEAM_PITCHING_BB, moneyball_original$TARGET_WINS) 
TEAM_PITCHING_H.cor <-cor(moneyball_original$TEAM_PITCHING_H, moneyball_original$TARGET_WINS) 
TEAM_PITCHING_HR.cor <-cor(moneyball_original$TEAM_PITCHING_HR, moneyball_original$TARGET_WINS) 
TEAM_PITCHING_SO.cor <-cor(moneyball_original$TEAM_PITCHING_SO,moneyball_original$TARGET_WINS, use = "na.or.complete") 

#Check scatterplot of the response and each predictor variable
plot(moneyball_original$TEAM_BATTING_H, moneyball_original$TARGET_WINS, main="Target Wins vs. Batting Hits") #somewhat linear
plot(moneyball_original$TEAM_BATTING_2B, moneyball_original$TARGET_WINS, main="Target Wins vs. Batting 2B") #more linear
plot(moneyball_original$TEAM_BATTING_3B, moneyball_original$TARGET_WINS, main="Target Wins vs. Batting 3B") #somewhat linear
plot(moneyball_original$TEAM_BATTING_HR,  moneyball_original$TARGET_WINS, main="Target Wins vs. Batting HR") #more linear
plot(moneyball_original$TEAM_BATTING_BB, moneyball_original$TARGET_WINS, main="Target Wins vs. Batting BB") #kind of linear
plot(moneyball_original$TEAM_BATTING_HBP, moneyball_original$TARGET_WINS, main="Target Wins vs. Batting HBP") #somewhat linear
plot(moneyball_original$TEAM_BATTING_SO, moneyball_original$TARGET_WINS, main="Target Wins vs. Batting SO") #somewhat linear
plot(moneyball_original$TEAM_BASERUN_SB, moneyball_original$TARGET_WINS, main="Target Wins vs. Baserun SB") #not really linear
plot(moneyball_original$TEAM_BASERUN_CS, moneyball_original$TARGET_WINS, main="Target Wins vs. Baserun CS") #not really linear
plot(moneyball_original$TEAM_FIELDING_E, moneyball_original$TARGET_WINS, main="Target Wins vs. Fielding E") #not linear
plot(moneyball_original$TEAM_FIELDING_DP, moneyball_original$TARGET_WINS, main="Target Wins vs. Fielding DP") #somwhat linear
plot(moneyball_original$TEAM_PITCHING_BB, moneyball_original$TARGET_WINS, main="Target Wins vs. Pitching BB") #not linear
plot(moneyball_original$TEAM_PITCHING_H, moneyball_original$TARGET_WINS, main="Target Wins vs. Pitching H") #not linear
plot(moneyball_original$TEAM_PITCHING_HR, moneyball_original$TARGET_WINS, main="Target Wins vs. Pitching HR") #somewhat linear
plot(moneyball_original$TEAM_PITCHING_SO,moneyball_original$TARGET_WINS, main="Target Wins vs. Pitching SO") #not linear

#Correlation Matrix
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

#Batting correlation matrix
pairs(moneyball_original[2:8], lower.panel=panel.smooth, upper.panel = panel.smooth)

#Pitching correlation
pairs(~ moneyball_original$TARGET_WINS + moneyball_original$TEAM_PITCHING_BB + moneyball_original$TEAM_PITCHING_H + 
        moneyball_original$TEAM_PITCHING_HR + moneyball_original$TEAM_PITCHING_SO, lower.panel = panel.smooth, upper.panel = panel.smooth)

#Calculates the percentage of observations within each variable that's missing
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(moneyball_original,2,pMiss)

############ PART 2: DATA PREPARATION ##########################################################

#Determine the observations that are <= the 5th percentile and >= the 95th percentile for each variable 
#These will be considered outliers and will be imputed 

#Wins
quantile(moneyball_original$TARGET_WINS, 0.95) #104
quantile(moneyball_original$TARGET_WINS, 0.05) #54

#Batting
quantile(moneyball_original$TEAM_BATTING_H, 0.95) #1695.25
quantile(moneyball_original$TEAM_BATTING_H, 0.05) #1281.5
quantile(moneyball_original$TEAM_BATTING_2B, 0.95)#320
quantile(moneyball_original$TEAM_BATTING_2B, 0.05)#167
quantile(moneyball_original$TEAM_BATTING_3B , 0.95)#108
quantile(moneyball_original$TEAM_BATTING_3B , 0.05)#23
quantile(moneyball_original$TEAM_BATTING_HR  , 0.95)#199
quantile(moneyball_original$TEAM_BATTING_HR  , 0.05)#14
quantile(moneyball_original$TEAM_BATTING_BB , 0.95) #670.25
quantile(moneyball_original$TEAM_BATTING_BB , 0.05) #248.25
quantile(moneyball_original$TEAM_BATTING_HBP, 0.95, na.rm = TRUE) #82.5
quantile(moneyball_original$TEAM_BATTING_HBP , 0.05, na.rm = TRUE) #40
quantile(moneyball_original$TEAM_BATTING_SO   , 0.95, na.rm=TRUE) #1103.35
quantile(moneyball_original$TEAM_BATTING_SO   , 0.05, na.rm=TRUE) #359

#Baserun
quantile(moneyball_original$TEAM_BASERUN_SB, 0.95, na.rm=TRUE) #301.8
quantile(moneyball_original$TEAM_BASERUN_SB , 0.05, na.rm=TRUE) #35
quantile(moneyball_original$TEAM_BASERUN_CS , 0.95, na.rm=TRUE) #91
quantile(moneyball_original$TEAM_BASERUN_CS  , 0.05, na.rm=TRUE) #24

#Fielding
quantile(moneyball_original$TEAM_FIELDING_E , 0.95) #716
quantile(moneyball_original$TEAM_FIELDING_E , 0.05) #100
quantile(moneyball_original$TEAM_FIELDING_DP , 0.95, na.rm=TRUE) #186
quantile(moneyball_original$TEAM_FIELDING_DP , 0.05, na.rm=TRUE) #98

#Pitching
quantile(moneyball_original$TEAM_PITCHING_BB , 0.95) #757
quantile(moneyball_original$TEAM_PITCHING_BB , 0.05) #377
quantile(moneyball_original$TEAM_PITCHING_H , 0.95) #2563
quantile(moneyball_original$TEAM_PITCHING_H , 0.05) #1316
quantile(moneyball_original$TEAM_PITCHING_HR, 0.95) #209.25
quantile(moneyball_original$TEAM_PITCHING_HR , 0.05) #18
quantile(moneyball_original$TEAM_PITCHING_SO , 0.95, na.rm=TRUE) #1173
quantile(moneyball_original$TEAM_PITCHING_SO , 0.05, na.rm=TRUE) #421.3

#Impute values that are less than 5th percentile or greater than 95th percentile
#Target Wins
moneyball$TARGET_WINS[moneyball$TARGET_WINS>104] <- NA
moneyball$TARGET_WINS[moneyball$TARGET_WINS<54] <- NA
#Batting
moneyball$TEAM_BATTING_H[moneyball$TEAM_BATTING_H>1695] <- NA  
moneyball$TEAM_BATTING_H[moneyball$TEAM_BATTING_H<1281]  <-NA
moneyball$TEAM_BATTING_2B[moneyball$TEAM_BATTING_2B>320] <- NA  
moneyball$TEAM_BATTING_2B[moneyball$TEAM_BATTING_2B<167] <- NA
moneyball$TEAM_BATTING_3B[moneyball$TEAM_BATTING_3B>108] <- NA 
moneyball$TEAM_BATTING_3B[moneyball$TEAM_BATTING_3B<23] <- NA
moneyball$TEAM_BATTING_HR[moneyball$TEAM_BATTING_HR>199] <- NA  
moneyball$TEAM_BATTING_HR[moneyball$TEAM_BATTING_HR<14] <- NA
moneyball$TEAM_BATTING_BB[moneyball$TEAM_BATTING_BB>670] <- NA 
moneyball$TEAM_BATTING_BB[moneyball$TEAM_BATTING_BB<248] <- NA
moneyball$TEAM_BATTING_HBP[moneyball$TEAM_BATTING_HBP>82] <- NA 
moneyball$TEAM_BATTING_HBP[moneyball$TEAM_BATTING_HBP<40] <- NA
moneyball$TEAM_BATTING_SO[moneyball$TEAM_BATTING_SO>1103] <- NA
moneyball$TEAM_BATTING_SO[moneyball$TEAM_BATTING_SO<359] <- NA

#Baserun
moneyball$TEAM_BASERUN_SB[moneyball$TEAM_BASERUN_SB>301] = NA
moneyball$TEAM_BASERUN_SB[moneyball$TEAM_BASERUN_SB<35] = NA
moneyball$TEAM_BASERUN_CS[moneyball$TEAM_BASERUN_CS>91] = NA
moneyball$TEAM_BASERUN_CS[moneyball$TEAM_BASERUN_CS<24] = NA

#Fielding
moneyball$TEAM_FIELDING_E[moneyball$TEAM_FIELDING_E>716] <- NA
moneyball$TEAM_FIELDING_E[moneyball$TEAM_FIELDING_E<100] <- NA
moneyball$TEAM_FIELDING_DP[moneyball$TEAM_FIELDING_DP>186] <- NA
moneyball$TEAM_FIELDING_DP[moneyball$TEAM_FIELDING_DP<98] <- NA

#Pitching
moneyball$TEAM_PITCHING_BB[moneyball$TEAM_PITCHING_BB>757] <- NA
moneyball$TEAM_PITCHING_BB[moneyball$TEAM_PITCHING_BB<377] <- NA
moneyball$TEAM_PITCHING_H[moneyball$TEAM_PITCHING_H>2563] <- NA
moneyball$TEAM_PITCHING_H[moneyball$TEAM_PITCHING_H<1316] <- NA
moneyball$TEAM_PITCHING_HR[moneyball$TEAM_PITCHING_HR>209] <- NA
moneyball$TEAM_PITCHING_HR[moneyball$TEAM_PITCHING_HR<18] <- NA
moneyball$TEAM_PITCHING_SO[moneyball$TEAM_PITCHING_SO>1173] <- NA
moneyball$TEAM_PITCHING_SO[moneyball$TEAM_PITCHING_SO<421] <- NA

#Impute missing values using MICE package
index_column <- data.frame(moneyball$INDEX)

#using PMM
#don't include Index column
moneyball_temp1<-mice(moneyball[, !names(moneyball) %in% "INDEX"], m=5, maxit=50, meth="pmm", seed = 500)
summary(moneyball_temp1)

moneyball_imp1 <- complete(moneyball_temp1)
moneyball_imp1 <-cbind(index_column, moneyball_imp1)
colnames(moneyball_imp1)[colnames(moneyball_imp1)=="moneyball.INDEX"] <- "INDEX"

#using RF
#don't include Index column
moneyball_temp2<-mice(moneyball[, !names(moneyball) %in% "INDEX"], m=5, maxit=50, meth="rf", seed = 750)
summary(moneyball_temp2)

moneyball_imp2 <- complete(moneyball_temp2)
moneyball_imp2 <-cbind(index_column, moneyball_imp2)

#Check for correlation using the first imputed data frame
cor(moneyball_imp1$TEAM_BATTING_H, moneyball_imp1$TARGET_WINS)
cor(moneyball_imp1$TEAM_BATTING_2B, moneyball_imp1$TARGET_WINS) 
cor(moneyball_imp1$TEAM_BATTING_3B, moneyball_imp1$TARGET_WINS) 
cor(moneyball_imp1$TEAM_BATTING_HR,  moneyball_imp1$TARGET_WINS)
cor(moneyball_imp1$TEAM_BATTING_BB, moneyball_imp1$TARGET_WINS) 
cor(moneyball_imp1$TEAM_BATTING_HBP, moneyball_imp1$TARGET_WINS) 
cor(moneyball_imp1$TEAM_BATTING_SO, moneyball_imp1$TARGET_WINS) 
cor(moneyball_imp1$TEAM_BASERUN_SB, moneyball_imp1$TARGET_WINS) 
cor(moneyball_imp1$TEAM_BASERUN_CS, moneyball_imp1$TARGET_WINS) 
cor(moneyball_imp1$TEAM_FIELDING_E, moneyball_imp1$TARGET_WINS) 
cor(moneyball_imp1$TEAM_FIELDING_DP, moneyball_imp1$TARGET_WINS) 
cor(moneyball_imp1$TEAM_PITCHING_BB, moneyball_imp1$TARGET_WINS) 
cor(moneyball_imp1$TEAM_PITCHING_H, moneyball_imp1$TARGET_WINS) 
cor(moneyball_imp1$TEAM_PITCHING_HR, moneyball_imp1$TARGET_WINS) 
cor(moneyball_imp1$TEAM_PITCHING_SO,moneyball_imp1$TARGET_WINS) 

#Create new variables 
moneyball_imp1$TEAM_BATTING_H_SO_RATIO <- moneyball_imp1$TEAM_BATTING_H/moneyball_imp1$TEAM_BATTING_SO
moneyball_imp1$TEAM_BATTING_1B <- moneyball_imp1$TEAM_BATTING_H-moneyball_imp1$TEAM_BATTING_2B - moneyball_imp1$TEAM_BATTING_3B
-moneyball_imp1$TEAM_BATTING_HR
moneyball_imp1$TEAM_PITCHING_H_SO_RATIO <- moneyball_imp1$TEAM_PITCHING_H/moneyball_imp1$TEAM_PITCHING_SO
moneyball_imp1$TEAM_BATTING_ONBASE <- moneyball_imp1$TEAM_BATTING_H+moneyball_imp1$TEAM_BATTING_BB+moneyball_imp1$TEAM_BATTING_HBP
moneyball_imp1$TEAM_BATTING_ADVANCE_BASES <- moneyball_imp1$TEAM_BATTING_H+moneyball_imp1$TEAM_BATTING_BB+moneyball_imp1$TEAM_BATTING_HBP
+moneyball_imp1$TEAM_BASERUN_SB-moneyball_imp1$TEAM_BASERUN_CS
moneyball_imp1$TEAM_PITCHING_NON_HR <- moneyball_imp1$TEAM_PITCHING_H-moneyball_imp1$TEAM_PITCHING_HR
moneyball_imp1$TEAM_PITCHING_ADVANCE_BASES <- moneyball_imp1$TEAM_PITCHING_H +moneyball_imp1$TEAM_PITCHING_BB-moneyball_imp1$TEAM_PITCHING_SO
-moneyball_imp1$TEAM_FIELDING_DP-moneyball_imp1$TEAM_FIELDING_E

############ PART 3: BUILD MODELS ##############################################################
#Build model using automated variable selection - Stepwise Approach 
#Need to specifiy the upper model and lower models
# Define the upper model as the FULL model 
upper.lm <- lm(TARGET_WINS ~ .,data=moneyball_imp1[, !names(moneyball) %in% "INDEX"])

# Define the lower model as the Intercept model 
lower.lm <- lm(TARGET_WINS ~ 1,data=moneyball_imp1[, !names(moneyball) %in% "INDEX"]) 

# Need a simple linear regression model to initialize stepwise selection 
initialize_stepwise.lm <- lm(TARGET_WINS ~ TEAM_BATTING_H,data=moneyball_imp1[, !names(moneyball) %in% "INDEX"]) 

#Create stepwise.lm
stepwise.lm <- stepAIC(object=initialize_stepwise.lm,scope=list(upper=formula(upper.lm),lower=~1), direction=c('both'))
summary(stepwise.lm)

#Check for normality with residuals 
qqnorm(stepwise.lm$res) 
qqline(stepwise.lm$res) 

#Check for constant variance 
plot(stepwise.lm$fitted.values, stepwise.lm$residuals)

#Build model using  transformed variables along with three highest correlated variables 
transformed_lm <- lm(TARGET_WINS ~ TEAM_BATTING_2B+ +TEAM_BATTING_H +TEAM_BATTING_BB + TEAM_BATTING_H_SO_RATIO + TEAM_BATTING_1B + TEAM_PITCHING_H_SO_RATIO + TEAM_BATTING_ONBASE +
                        TEAM_PITCHING_NON_HR + TEAM_PITCHING_ADVANCE_BASES, data = moneyball_imp1)
summary(transformed_lm)

#Check for normality with residuals 
qqnorm(transformed_lm$res) 
qqline(transformed_lm$res) 

#Check for constant variance 
plot(transformed_lm$fitted.values, transformed_lm$residuals)

#Build model using only batting variables 
batting_lm <- lm(TARGET_WINS ~ TEAM_BATTING_H  +TEAM_BATTING_2B 
              +TEAM_BATTING_3B  +TEAM_BATTING_HR  +TEAM_BATTING_BB  +TEAM_BATTING_HBP  +TEAM_BATTING_SO 
              + TEAM_BATTING_H_SO_RATIO, data = moneyball_imp1)

summary(batting_lm)

#Check for normality with residuals 
qqnorm(batting_lm$res) 
qqline(batting_lm$res) 

#Check for constant variance 
plot(batting_lm$fitted.values, batting_lm$residuals)


#Create forward.lm
forward.lm <- stepAIC(object=lower.lm,scope=list(upper=formula(upper.lm),lower=~1), direction=c('forward'))
summary(forward.lm)

#Create backward.lm
backward.lm <- stepAIC(object=upper.lm,direction=c('backward')) 
summary(backward.lm)


############ PART 4: SELECT MODELS #############################################################

#Look at VIFs
sort(vif(stepwise.lm),decreasing=TRUE)
sort(vif(transformed_lm),decreasing=TRUE) 
sort(vif(batting_lm),decreasing=TRUE)

#Deviance
deviance(stepwise.lm)
deviance(transformed_lm)
deviance(batting_lm)

#AIC and BIC
AIC(stepwise.lm)
AIC(transformed_lm)
AIC(batting_lm)

BIC(stepwise.lm)
BIC(transformed_lm)
BIC(batting_lm)

#MSE AND MAE
mse.stepwise <- mean(stepwise.lm$residuals^2)
mse.stepwise
mae.stepwise <- mean(abs(stepwise.lm$residuals))
mae.stepwise

mse.transformed <- mean(transformed_lm$residuals^2)
mse.transformed
mae.transformed <- mean(abs(transformed_lm$residuals))
mae.transformed

mse.batting <- mean(batting_lm$residuals^2)
mse.batting
mae.batting <- mean(abs(batting_lm$residuals))
mae.batting



