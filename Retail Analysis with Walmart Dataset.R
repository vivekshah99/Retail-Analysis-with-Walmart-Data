### LOADING THE REQUIRED LIBRARIES ###

library(dplyr)
library(forecast)
library(tidyr)
library(lubridate)
library(gbm)
library(glmnet)
library(ggplot2)
library(tidyverse)
library(randomForest)
library(Rsconctdply)
library(rsconnect)
library(plotly)
library(corrplot)


### LOADING THE DATASET ###
rm(list = ls())
data <- read.csv("Walmart_Store_sales.csv", header = TRUE)
data1 <- data.frame(data)

### DESCRIPTIVE STATISTICS ###
summary(data) 

###################### PART 1 BASIC STATISTICS TASK ########################

### Which store has maximum sales ###

Sales_Stores <- data1[order(data1$Weekly_Sales, decreasing = TRUE),]
Sales_Stores 
head(Sales_Stores)


### Negative impact on sales, Finding out the holidays having higher sales ###

Nonholidaysales <- data1%>%group_by(Weekly_Sales)%>%filter(Holiday_Flag==0)
Avg_Nonholidaysales <- mean(Nonholidaysales$Weekly_Sales)
Avg_Nonholidaysales

holidaysales <- data1%>%group_by(Weekly_Sales)%>%filter(Holiday_Flag==1)
Avg_holidaysales <- mean(holidaysales$Weekly_Sales)
Avg_holidaysales

filter(data1,Weekly_Sales>Avg_Nonholidaysales &  Holiday_Flag==1)

## converting dates format ##

as.Date(data1$Date, format = "%d-%m-%Y")
data1$Date <- as.Date(data1$Date, format = "%d-%m-%Y")
data1

## converting days format ##

data1$Date <- as.character(data1$Date)
baseline_date <- as.Date('2010-02-05')
data1$Days <- as.numeric(as.Date(data1$Date) - baseline_date)
data1

## splitting data into year/month/day

data1$Date <- as.character(data1$Date) # convert date to cher
d <- strsplit(data1$Date, '-')
d <- as.numeric(unlist(d))
d <- matrix(d, dim(data1)[1], 3, byrow=T)
data1$Year <- d[,1]
data1$Month <- d[,2]
data1$Day <- d[,3]
data1

### Provide a monthly and semester view of sales in units and give insights ###

### Weekly_Sales by month ###

data1%>%group_by(Month)%>%
  summarise(Mean_Weekly_Sales = mean(Weekly_Sales))

### Weekly_Sales by Year ###

data1%>%group_by(Year)%>%
  summarise(Mean_Weekly_Sales = mean(Weekly_Sales))

### STATISTICAL MODEL ###

model1 <- lm (Weekly_Sales ~ CPI, data=data1)
summary(model1)
p_value= 5.438e-09
alpha= 0.05
p_value < alpha 

model2 <- lm (Weekly_Sales ~ Unemployment, data=data1)
summary(model2)
p_value = 2.2e-16
alpha = 0.05
p_value < alpha

model3 <- lm (Weekly_Sales ~ Fuel_Price, data=data1)
summary(model3)
p_value= 0.4478
alpha = 0.05
p_value < alpha

### BUILDING PREDICTION MODELS TO FORECAST DEMAND ###

col.vars <- c('Holiday_Flag','Temperature', 'Fuel_Price', 'CPI', 'Unemployment','Weekly_Sales')
datamodel <- data1[,col.vars]

model4 <- lm (Weekly_Sales ~ ., datamodel)
summary(model4) 
Rsqd1 <- summary(model4)$r.squared
Rsqd1

predicted_y1 <- predict(model4, datamodel)
RMSE1 = sqrt(mean((data1$Weekly_Sales - predicted_y1)^2))
RMSE1

### CONSIDERING WHICH HAVE HIGH IMPACT ON SALES ###

model5 <- lm(Weekly_Sales ~ Unemployment + CPI+ Temperature, datamodel)
summary(model5)
Rsqd2 <- summary(model5)$r.squared
Rsqd2

predicted_y2 <- predict(model5, datamodel)
RMSE2 = sqrt(mean((datamodel$Weekly_Sales - predicted_y2)^2)) 
RMSE2 

model6 <- lm(Weekly_Sales ~ log(Unemployment) + CPI+ Temperature, datamodel)
summary(model6)
Rsqd3 <- summary(model6)$r.squared
Rsqd3

predicted_y3 <- predict(model6, datamodel)
RMSE3 = sqrt(mean((datamodel$Weekly_Sales - predicted_y3)^2))
RMSE3

model7 <- lm(Weekly_Sales ~ Unemployment + CPI+ Temperature + Holiday_Flag , datamodel)
summary(model7)
Rsqd4 <- summary(model7)$r.squared
Rsqd4 

predicted_y2 <- predict(model7, datamodel)
RMSE4 = sqrt(mean((datamodel$Weekly_Sales - predicted_y2)^2)) 
RMSE4

### COMPARING MODELS ###

Rsqd_ <- c(Rsqd1,Rsqd2,Rsqd3,Rsqd4)
RMSE_ <- c(RMSE1,RMSE2,RMSE3,RMSE4)

Model_Validation <- cbind(Rsqd_,RMSE_)
rownames(Model_Validation) <- c("model4 - (all variables)",
                                "model5 - (Weekly_Sales on  CPI  & Temperature)",
                                "model6 - (Weekly_Sales on log(Unemployment) & CPI  & Temperature)",
                                "model7 - (Weekly_Sales on Unemployment + CPI+ Temperature + Holiday_Flag)")
Model_Validation

head(data)
input <- data
plot(x= input$Weekly_Sales,y=input$Store,
     xlab = "WeeklySales",
     ylab = "Stores",
     xlim = c(1400000,3900000),
     ylim = c(0,1),
     main = "Weekly Sales vs Store"
)

### SUBSET CONSTRUCTION FOR CORRELATION PLOT ###

data1$Holiday_Flag [data1$Holiday_Flag == "TRUE"] <- 1
data1$Holiday_Flag [data1$Holiday_Flag == "FALSE"] <- 0
head(data1)
# DEALING WITH "NA" VALUES
data1[is.na(data1)] <- 0

subset1 <- subset(data1$Date,data1$Weekly_Sales<0) 
subset2 <- subset(data1,select = c("Weekly_Sales","Temperature","Fuel_Price","CPI","Unemployment"))

## CORRELATION BETWEEN SALES N HOLIDAY ##

cor(data1$Weekly_Sales,data1$Holiday_Flag,use="everything",method="pearson")
subset1 <- subset(data1$Date,data1$Weekly_Sales<0)

## TIME SERIES ##
fore_data <- ts(data1$Weekly_Sales, start=2010, end=2012,frequency=12)
plot(fore_data)

## CORRELATION PLOT ##

subset2 <- subset(data1, select= c("Weekly_Sales","Temperature","Fuel_Price",
                                       "CPI","Unemployment"))
res <- cor(subset2)
head(res)

library(corrplot)
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE )

### Is there a relation between CPI and Temperature ### 

data1 %>% ggplot(aes(x = CPI, y = Temperature)) + geom_point() + geom_line(aes(y= fitted(lm(Temperature~CPI, data = data1))), colour = "red")

data1 %>% ggplot(aes(x = CPI, y = Weekly_Sales)) + geom_point() + geom_line(aes(y= fitted(lm(Weekly_Sales~CPI, data = data1))), colour = "red")
data1 %>% ggplot(aes(x = Temperature, y = Weekly_Sales)) + geom_point() + geom_line(aes(y= fitted(lm(Weekly_Sales~Temperature, data = data1))), colour = "red")
### Is there a relation between Unemployment and Weekly Sales ### decrease in sales due to increased unemployment

data1 %>% ggplot(aes(x = Unemployment, y = Weekly_Sales)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales~Unemployment, data = data1))), colour = "red")

############ RANDOM FOREST #################

library(randomForest)
set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(0.5,0.5))
train <- data[ind==1,]
test <- data[ind==2,]


rf <- randomForest(train$Weekly_Sales ~ train$Temperature+ train$Fuel_Price +
                     train$CPI + train$Unemployment + train$Holiday_Flag, data = train,ntree = 1000, mtry = 5, nodesize=15, importance=TRUE)
rf

varImpPlot(rf)

hist(treesize(rf), main = "NO. of nodes for the trees", col = "green")

library(caret)
p1<- predict(rf, data = train)
df <- data.frame(data = train$Weekly_Sales,p1)
df
plot(p1,train$Weekly_Sales,main="Random Forest with 85% Accuracy")
abline(0,1)


############ ARIMA MODEL #################


dfile <- read.csv(file.choose(), header = T)
head(dfile, n=10)
tail(dfile,n=10)
library(forecast)
library(tseries)

sales_ts <- ts(dfile$Weekly_Sales, start = 2010, frequency = 52)
autoplot(sales_ts)
adf.test(sales_ts,k=52)
pacf(sales_ts)
##p =0.01

acf(sales_ts)
#q=0.02
Amodel <- Arima(y=sales_ts, order = c(0.01,0,0.02))
Amodel

autoplot(forecast(Amodel, h=52))











