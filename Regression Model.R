#load dependent variables dataset 
data <- read.csv("C:\\Users\\mattn\\Desktop\\DAP\\Regression Model\\Regression\\2018 and 2019 dataset - Copy.csv",
                   header = TRUE, stringsAsFactors = FALSE)

#load and explore dependent variable 
library(readxl)
happiness_score <- read_xlsx("C:\\Users\\mattn\\Desktop\\DAP\\Regression Model\\Regression\\Happiness data 2019.xlsx",
                             col_names = TRUE)

library(dplyr)

happiness_score <- happiness_score %>% filter (HAPPY >0)
library(ggplot2)
ggplot(data=happiness_score, aes(x=HAPPY)) + 
  geom_histogram(aes(y=..density..), colour="navy blue", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

#load dependnet variables for 2018 and clean 
data_2018 <- data[1:310094,c(1,5,6)] #select 2018 data 
data_2018$X2018..YR2018. <- as.numeric(data_2018$X2018..YR2018.)
library(tidyr)
data_2018 <- spread(data_2018, Series.Code, X2018..YR2018.) #spread indicators into multiple columns
View(head(data_2018,15))

#append independent variable 
data_2018_Y <- as.data.frame(data_2018 %>% 
right_join(happiness_score, by=c("Country.Name"= "Country Name")))
#names <- data_2018_Y$Country.Name
#rownames(data_2018_Y) <- names

#identify variables with > 30% missing data and exclude them
library(data.table)
Tdata_2018_Y <- transpose(data_2018_Y) #transpose 
colnames(Tdata_2018_Y) <- rownames(data_2018_Y) #assign column names
rownames(Tdata_2018_Y) <- colnames(data_2018_Y) #assign row names 

pMiss <- function(x){sum(is.na(x))/length(x)*100} #idenify % of missing values for features 
data1 <- as.data.frame(cbind(Tdata_2018_Y, apply(data_2018_Y,2,pMiss))) #add column that calc % of missing values
View(head(data1))
data2 <- data1[data1$`apply(data_2018_Y, 2, pMiss)` <25, ]

data3 <- transpose(data2)
colnames(data3) <- rownames(data2)
rownames(data3) <- colnames(data2)
data3[,c(2:445,447,448)] <- sapply(data3[,c(2:445,447,448)], as.numeric)
View(head(data3))

#explore correlation between independent variables and dependent variable. 
#select independent variables that have strong correlation with dependent variable (|0.5|)
correl_xvars <- as.data.frame(round(cor(data3[,c(2:445,447,448)], use="complete.obs"),2))
View(correl_xvars[correl_xvars$HAPPY < -0.5,])
View(correl_xvars[correl_xvars$HAPPY > 0.5,])

final_data <- subset(data3, select=c(HAPPY, Democracy, SL.EMP.WORK.ZS, SL.IND.EMPL.ZS,
                                     NY.GDP.PCAP.CD, SL.GDP.PCAP.EM.KD,
                                     IC.BUS.DFRN.XQ, SH.DYN.0514,
                                     SH.DYN.MORT, SH.DYN.NMRT, SL.FAM.WORK.ZS,
                                     SL.AGR.EMPL.ZS, SL.EMP.SELF.ZS,
                                     SL.EMP.VULN.ZS, SP.DYN.IMRT.IN,
                                     SP.RUR.TOTL.ZS, SP.URB.GROW,
                                     MS.MIL.XPND.GD.ZS, SL.UEM.TOTL.ZS, Country.Name))
final_data[,1:19] <- sapply(final_data[,1:19], as.numeric)

#check for missing variables 
library(VIM)
library(mice)
aggr(final_data[1:19], col=c('navyblue','red'), #plot missing varibale chart
                      numbers=TRUE, sortVars=TRUE,
                      labels=names(final_data), cex.axis=.5,
                      gap=3, ylab=c("Missing data","Pattern"))

tempData <- mice(final_data[1:19], m=5) #peform mice to impute missing values for rem vars
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)) #one value was not fixed by mice
final_data2 <-replace(complete(tempData,1), TRUE, lapply(complete(tempData,1), NA2mean)) # using mean to impute the missing value 

apply(final_data2,2,pMiss) #check there are no missing vars left 

#Correlation between X vars to exclude multicolineraity 
correl_xvars <- round(cor(final_data2),2) #populate corr table between x vars
View(correl_xvars)
library(corrplot)
corrplot(correl_xvars, tl.cex=0.5) #plot x vars correlation 

#Assumption: strong correlation between X vars if corr coef is >0.7 
#X vars strongly correlated to be excluded 
final_data3 <- subset(final_data2, select=-c(SL.EMP.WORK.ZS, SL.GDP.PCAP.EM.KD,
                                             SH.DYN.0514,SH.DYN.NMRT, SP.DYN.IMRT.IN,
                                             SL.AGR.EMPL.ZS, SL.EMP.SELF.ZS ,
                                             SL.FAM.WORK.ZS, SH.DYN.MORT))

correl_xvars_red <- round(cor(final_data3),2) #test no strong correlation remaining
View(correl_xvars_red)
corrplot(correl_xvars_red, tl.cex=0.5) #plot corrplot for x vars 

#split into train and predict 
library(caTools)
data1 <- sample.split(final_data3,SplitRatio = 0.8)
train <- subset(final_data3,data1==TRUE)
test <- subset(final_data3,data1==FALSE)

#full model - first take 
full_model <- lm(HAPPY~., data=train)
summary(full_model)

new_model <- lm(formula=HAPPY ~ SP.RUR.TOTL.ZS + (MS.MIL.XPND.GD.ZS) + (SL.UEM.TOTL.ZS) + 
                   log(NY.GDP.PCAP.CD), data = train)
summary(new_model)


pred2 <- predict(new_model, test, type="response")
cor (pred2, test$HAPPY)
plot(pred2, test$HAPPY)
abline(a=0, b=1, col="red", lwd=3, lty=2)

ggplot(data=train, aes(x=NY.GDP.PCAP.CD)) + 
  geom_histogram(aes(y=..density..), colour="navy blue", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

new_model2 <- lm(formula=HAPPY ~ SP.RUR.TOTL.ZS + (MS.MIL.XPND.GD.ZS) + (SL.UEM.TOTL.ZS) + 
                  log(NY.GDP.PCAP.CD), data = train)
summary(new_model2)

pred3 <- predict(new_model2, test, type="response")
cor (pred3, test$HAPPY)
plot(pred3, test$HAPPY)

MAE(pred3, test$HAPPY)
abline(a=0, b=1, col="red", lwd=3, lty=2)

#TREE

library(rpart)
library(rpart.plot)
library(MLmetrics)

tree_model <- rpart(HAPPY ~ SP.RUR.TOTL.ZS + (MS.MIL.XPND.GD.ZS) + (SL.UEM.TOTL.ZS) + 
                      log(NY.GDP.PCAP.CD), data= train)

tree_predict <- predict(tree_model, test, type="vector")

summary(tree_model)
summary(tree_predict)
summary(test$HAPPY)

rpart.plot (tree_model, digits =4, fallen.leaves=TRUE,type=3,extra=101)

#measuring model performance 
cor(tree_predict, test$HAPPY)

plot(tree_predict, test$HAPPY)

MAE(tree_predict, test$HAPPY)

mean(test$HAPPY)
MAE(5.4, test$HAPPY)

library(Cubist)
cubist_model <- cubist(x=train[2:10], y=train$HAPPY)
predict_cubist <- predict(cubist_model, test)
MAE(predict_cubist, test$HAPPY)
