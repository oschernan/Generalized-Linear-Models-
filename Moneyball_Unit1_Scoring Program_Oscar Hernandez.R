########################################### Moneyball Scoring Program ########################################
###Oscar Hernandez####


#This Scoring Program requires the use of the mice package 
#If you do not have it, install by running "install.packages("mice)"
#Load the mice package
library(mice)

#You will need to set your working directory to where the test data file is located
#Type the name of the file in place of "moneyball_test.csv" if it is different. If not, leave as is  
moneyball_test=read.csv("moneyball_test.csv",header=T)


#Make the required imputations 
moneyball_test$TEAM_PITCHING_SO[moneyball_test$TEAM_PITCHING_SO==0] <-NA
moneyball_test$TEAM_PITCHING_H[moneyball_test$TEAM_PITCHING_H>2000] <-NA
moneyball_test$TEAM_FIELDING_E[moneyball_test$TEAM_FIELDING_E>275] <-NA
moneyball_test$TEAM_BASERUN_SB[moneyball_test$TEAM_BASERUN_SB>225] <-NA
moneyball_test$TEAM_BATTING_SO[moneyball_test$TEAM_BATTING_SO>1100] <-NA
moneyball_test$TEAM_PITCHING_BB[moneyball_test$TEAM_PITCHING_BB>725] <-NA

index_column_test <- data.frame(moneyball_test$INDEX)

moneyball_temp_test<-mice(moneyball_test[, !names(moneyball_test) %in% "INDEX"], m=5, maxit=50, meth="pmm", seed = 2000)

moneyball_imp_test <- complete(moneyball_temp_test)
moneyball_test <-cbind(index_column_test, moneyball_imp_test)
colnames(moneyball_test)[colnames(moneyball_test)=="moneyball_test.INDEX"] <- "INDEX"

#Add new variable 
moneyball_test$TEAM_PITCHING_H_SO_RATIO <- moneyball_test$TEAM_PITCHING_H/moneyball_test$TEAM_PITCHING_SO

#Create Predicted Target Wins variable using Champion Model
moneyball_test$P_TARGET_WINS <- 61.836347 + moneyball_test$TEAM_BATTING_H*0.011840 + moneyball_test$TEAM_BATTING_BB*0.064093+
  moneyball_test$TEAM_BASERUN_SB*0.069722 - moneyball_test$TEAM_BATTING_SO*0.028916 - moneyball_test$TEAM_FIELDING_E*0.040508-
  moneyball_test$TEAM_FIELDING_DP*0.082757+moneyball_test$TEAM_BATTING_3B*0.108000 - moneyball_test$TEAM_BASERUN_CS*0.083702+
  moneyball_test$TEAM_BATTING_HR*0.092044-moneyball_test$TEAM_BATTING_2B*0.026777+ moneyball_test$TEAM_PITCHING_H*0.021836 - 
  moneyball_test$TEAM_PITCHING_H_SO_RATIO*4.576801-moneyball_test$TEAM_PITCHING_BB*0.047478 

##This will create subset of data set for the end product "Scored data file"
prediction <- moneyball_test[c("INDEX","P_TARGET_WINS")]

#This will create the Moneyball Scored data file as a CSV file and place it in your working directory 
write.csv(prediction, file = "write.csv", row.names = FALSE)

