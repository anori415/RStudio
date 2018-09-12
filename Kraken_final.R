####Kraken_Experian###

#Working Directory
setwd("~/Dropbox (Delta Defense)/Analytics Team/Kraken Work/Kraken_Reverse_Append")

####Libraries####
library(ggplot2) # Preliminary Analysis
library(tidyr) # Preliminary Analysis
library(cluster) # Cluster Analysis
library(factoextra) # Cluster Analysis
library(gridExtra) # Cluster Analysis
library(corrplot) # Correlation Matrix
library(mice) # Impute Missing Data
library(party) # Random Forest
library(earth) # Mars (earth package)
library(Boruta) # Boruta
library(relaimpo) # Relative Importance
library(devtools) # Information value and Weight of evidence
library(plyr) # Information value and Weight of evidence
library(woe) # Information value and Weight of evidence
library(sqldf) # Information value and Weight of evidence

####Functions####
p <- function(x) {sum(is.na(x))/length(x)*100}

####Data####
data<-read.csv("Kraken_Experian_Final.csv",header = T, na.strings = c("","NA"))
inputData1<-data

####Data Preparation####
names(inputData1) <- c("Tier","TierClass","MosaicHousehold","SubGroup","Group","Children","ProfiabilityScore","DirectMailResponder","DwellingType","HouseholdIncome","HomeValue","Gender","Education","LengthResidence","MaritalStatus","Occupation","State","TCV","MembershipLevel","Days","ProductTCV","Count")  # assign names
numeric.data <- as.data.frame(sapply(inputData1,as.numeric))
inputData2 <- numeric.data
inputData2$Tier <- as.factor(inputData1$Tier)
inputData2$TierClass <- as.factor(inputData1$TierClass)
inputData2$Days <- as.factor(inputData1$Days)
sapply(inputData2,function(x) sum(is.na(x)))
inputData2<-subset(inputData2,select=c("Tier","TierClass","Days","MosaicHousehold","SubGroup","Group","Children","ProfiabilityScore","DirectMailResponder","DwellingType","HouseholdIncome","HomeValue","Gender","Education","LengthResidence","MaritalStatus","Occupation","State"))

#Impute Missing Data
apply(inputData2, 2, p)
#md.pattern(inputData1)
#md.pairs(inputData1)
#marginplot(data[,c('','')])

#Impute
impute <- mice(inputData1[,4:18], m=3, seed = 123 ) # ignore columns that do not have any predictive power
print(impute) #pmm = predictive mean matching & ployreg = multinomial logistic regression (used for factors)

#Complete Data
inputData2 <- complete(impute, 1) # replacing data with implecation option (1, 2, or 3 if m = 3)
inputData2$Tier <- inputData1$Tier
inputData2$TierClass <- as.numeric(inputData1$TierClass)
inputData2$Days <- inputData1$Days
#inputData2<-subset(inputData2,select=c("Tier","TierClass","Days","MosaicHousehold","SubGroup","Group","Children","ProfiabilityScore","DirectMailResponder","DwellingType","HouseholdIncome","HomeValue","Gender","Education","LengthResidence","MaritalStatus","Occupation","State"))
inputData3<-inputData2[,-c(2,3,16,18)]

#Segregate all continuous and categorical variables
inputData2_cont <- inputData3[, c("HomeValue","LengthResidence")] #all continuous vars
inputData2_cat <- inputData3[, c("MosaicHousehold", "Children", "ProfiabilityScore","DirectMailResponder","DwellingType","HouseholdIncome","Gender", "Education", "MaritalStatus", "Occupation","State")] #all catagorical vars

#Create the response data frame
inputData2_response <- data.frame(TierClass=inputData3[, "TierClass"])  # response variable as a dataframe
response2_name <- "TierClass"  # name of response variable
response2 <- inputData3[,response2_name]  # response variable as a vector

####Preliminary Analysis####

# Histogram
numeric.data %>% gather() %>% head()
hist.1<-ggplot(gather(numeric.data), aes(value)) + 
  geom_histogram(bins = 16) + 
  facet_wrap(~key, scales = 'free_x')

####Cluster Analysis####

# data preparation 
df<-inputData2
df.num <- as.data.frame(sapply(df,as.numeric))
#df.num <- df.num[,2:16] # Removing 
#attr(df,"row.names") <- as.character(df$Tier)
#Structuring
sum.df <- aggregate(df.num[c("MosaicHousehold","Children","ProfiabilityScore","DirectMailResponder","DwellingType","HouseholdIncome","HomeValue",
                         "Gender","Education","LengthResidence","MaritalStatus","Occupation","State")],
                    by = df.num[c("Tier", "Group")],
                    FUN = mean, na.rm=TRUE)

sum.df1<-sum.df[-c(1,4,8,17,18),]
sum.df1<-sum.df[,3:15] # Removing Tier & Group because they are factors
#sum.df1<-sum.df1[-c(2,5)] # Removing Childre & DwellingType because No Variation
attr(sum.df1,"row.names")<-c("Tier 1.2","Tier 9.2","Tier 1.4","Tier 9.4","Tier 1.5","Tier 1.6","Tier 9.6","Tier 1.7",
                             "Tier 9.7","Tier 1.8","Tier 9.8","Tier 1.9","Tier 9.9", "Tier 1.11", "Tier 9.11")
test.df <- scale(sum.df1) #Scaling Data Set

#Analysis
distance <- get_dist(test.df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k2 <- kmeans(test.df, centers = 2, nstart = 25) #Clustering in 2 Groups
k3 <- kmeans(test.df, centers = 3, nstart = 25) #Clustering in 2 Groups
k4 <- kmeans(test.df, centers = 4, nstart = 25) #Clustering in 2 Groups
k5 <- kmeans(test.df, centers = 5, nstart = 25) #Clustering in 2 Groups 

str(k2)
k2
str(k3)
k3
str(k4)
k4
str(k5)
k5

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = test.df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = test.df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = test.df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = test.df) + ggtitle("k = 5")

cluster_plot <- grid.arrange(p1, p2, p3, p4, nrow = 2)

####Correlation Matrix####
tier1<-sum.df1[c(1,3,5,6,8,10,12,14),-c(1)]
tier9<-sum.df1[c(2,4,7,9,11,13,15),-c(1)]
par(mai=c(4, 4, 4, 5) + 0.1, pin=c(8,8))

corr_sum.df1=cor(sum.df1,method = "s")
corr_tier1=cor(tier1, method = "s")
corr_tier9=cor(tier9, method = "s")
corrplot(corr_sum.df1, title="Correlation Plot", method = "square", outline = T, 
         addgrid.col = "darkgray", order="hclust", mar = c(4,0,4,0), addrect = 4, 
         rect.col = "black", rect.lwd = 5,cl.pos = "b", tl.col = "indianred4", 
         tl.cex = 1.5, cl.cex = 1.5)

#Correlation Matrix All Variables vs One Another
corrplot(corr_sum.df1,method = "color", outline = T, addgrid.col = "darkgray", 
         order="hclust", addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b", 
         tl.col = "indianred4", tl.cex = 1, cl.cex = 1.5, addCoef.col = "white", 
         number.digits = 2, number.cex = 0.75, col = colorRampPalette(c("darkred","white","midnightblue"))(100))
title(main="Correlation Plot", col.main="firebrick4", col.sub="midnightblue", line=2)

#Tier 1 on Top | Tier 9 on Bottom
ord=hclust(1-as.dist(corr_tier1))$order
corrplot(corr_tier1[ord,ord], outline = T, addgrid.col = "darkgray",cl.pos = "r",
         tl.col = "indianred4", tl.cex = 1, cl.cex = 1.5, mar = c(4,0,4,0),
         type = "upper", tl.pos = "tl", bg="azure2")
corrplot(corr_tier9[ord,ord], outline = T, addgrid.col = "darkgray",cl.pos = "r", 
         tl.col = "indianred4", tl.cex = 1, cl.cex = 1.5, mar = c(4,0,4,0),
         type = "lower", tl.pos = "tl", add=T, bg="azure")
title(main="Correlation Plot by Tier's", col.main="firebrick4", col.sub="midnightblue", line=2)

####Randome Forest Method####
cf2 <- cforest(TierClass ~ . , data= inputData2, control=cforest_unbiased(mtry=2,ntree=50)) # fit the random forest
varimp(cf2) # get variable importance, based on mean decrease in accuracy
varimp(cf2, conditional=TRUE)  # conditional=True, adjusts for correlations between predictors
varimpAUC(cf1)  # more robust towards class imbalance.


####Relative Importance####
lmMod2 <- lm(TierClass ~ . , data = inputData3)  # fit lm() model
relImportance2 <- calc.relimp(lmMod2, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
sort(relImportance2$lmg, decreasing=TRUE)  # relative importance

####Mars (earth package)####
marsModel2 <- earth(TierClass ~ ., data=inputData3) # build model
ev <- evimp (marsModel2) # estimate variable importance
ev
plot(ev)

####Step-wise Regression####
base.mod <- lm(ozone_reading ~ 1 , data= inputData)  # base intercept only model
all.mod <- lm(ozone_reading ~ . , data= inputData) # full model with all predictors
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 

####Boruta####
#Decide if a variable is important or not using Boruta
boruta_output2 <- Boruta(response2 ~ ., data=na.omit(inputData3), doTrace=2)  # perform Boruta search
boruta_signif2 <- names(boruta_output2$finalDecision[boruta_output2$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables

####Information value and Weight of evidence####

#Data#
inputData1<-data

#Data Preparation
names(inputData1) <- c("Tier","TierClass","MosaicHousehold","SubGroup","Group","Children","ProfiabilityScore","DirectMailResponder","DwellingType","HouseholdIncome","HomeValue","Gender","Education","LengthResidence","MaritalStatus","Occupation","State","TCV","MembershipLevel","Days","ProductTCV","Count")  # assign names
numeric.data <- as.data.frame(sapply(inputData1,as.numeric))
inputData1 <- numeric.data
numeric.data$Tier <- as.factor(inputData1$Tier)
numeric.data$TierClass <- as.factor(inputData1$TierClass)
numeric.data$Days <- as.factor(inputData1$Days)
sapply(inputData1,function(x) sum(is.na(x)))
inputData1<-subset(inputData1,select=c("Tier","TierClass","Days","MosaicHousehold","SubGroup","Group","Children","ProfiabilityScore","DirectMailResponder","DwellingType","HouseholdIncome","HomeValue","Gender","Education","LengthResidence","MaritalStatus","Occupation","State"))

#Impute Missing Data
apply(inputData1, 2, p)
#md.pattern(inputData1)
#md.pairs(inputData1)
#marginplot(data[,c('','')])

#Impute
impute <- mice(inputData1[,4:18], m=3, seed = 123 ) # ignore columns that do not have any predictive power
#print(impute) #pmm = predictive mean matching & ployreg = multinomial logistic regression (used for factors)

#Complete Data
inputData2 <- complete(impute, 1) # replacing data with implecation option (1, 2, or 3 if m = 3)
inputData2$Tier <- as.factor(inputData1$Tier)
inputData3<-inputData2[,-c(2,3)]

#Analysis
iv_df <- iv.mult(inputData3, y="Tier", summary=TRUE, verbose=TRUE) # information values summarized
iv <- iv.mult(inputData3, y="Tier", summary=FALSE, verbose=TRUE) # information values not summarized
iv_df_plot<-iv.plot.summary(iv_df) # plot of the information value summary
inputData3_iv <- iv.replace.woe(inputData3, iv, verbose=TRUE)  # add woe variables to original data frame.
