library('ggplot2')
library('corrplot') #used in correlation plot
library('Amelia') #used in missmap
library('dplyr') 

#Read Train and Test data
train <- read.csv('train.csv', header = TRUE)
test  <- read.csv('test.csv', header = TRUE)

#structure of data
glimpse(train) #print dimension and str

summary(train) 

#sapply() list, vector or data frame as input and gives output in vector or matrix.
#Count the number of columns that consists of text data
print(sum(sapply(train[,1:17], typeof) == "character"))

#Count the number of columns that consists of numerical data
print(sum(sapply(train[,1:17], typeof) == "integer"))

# The percentage of data missing in train
print(sum(is.na(train)) / (nrow(train) *ncol(train)))

# The percentage of data missing in test
print(sum(is.na(test)) / (nrow(test) * ncol(test)))

#another way to see for missing values
missmap(train,col=c('yellow','black'),y.at=1,y.labels='',legend=TRUE)

# Check for duplicated rows
#cat("The number of duplicated rows are", nrow(train) - nrow(unique(train)))

numData <- names(train)[which(sapply(train, is.numeric))]
trainNum <- train[numData]

#correlation plot
correlation <- cor(na.omit(trainNum[,-1]))
corrplot(correlation, method="square")

#Data Exploration
print(train$price)
priceIN1L = train$price/100000

#price distribution
hist(priceIN1L,
     main = "Distribution of Price",
     xlab = 'Price in 1L',
     ylab = 'Frequency',
     col = 'blue',
     breaks = 20,
     xlim = c(0,250),
     ylim = c(0,3000)
)

#bedroom distribution
hist(train$bedrooms,
     main = "Distribution of Bedrooms",
     xlab = 'Number of Bedrooms',
     ylab = 'Frequency',
     col = 'blue'
)

#condition distribution
hist(train$condition,
     main = "Distribution of Condition",
     xlab = 'House Condition',
     ylab = 'Frequency',
)

sqrFT <- train$sqft_lot/100

#price by Sqft
plot(x = sqrFT,
     y = priceIN1L,
     xlab = 'Square Feet',
     ylab = 'Price in 1L')

#price by bedroom
plot(priceIN1L,
     train$bedrooms,
     main = "Price by Bedroom",
     xlab = 'Price in 1L',
     ylab = 'Numbers of bedrooms',
     col = 'red')

model <- lm(data=train,price~bedrooms+bathrooms+sqft_living+view+sqft_above+sqft_basement+condition+yr_renovated)

#Round coefficients table 

coeff <- summary(model)$coefficients
coeff <- round(coeff,4)
print(coeff)

#Scatter plot
plot(
  priceIN1L ~ train$yr_built,
  cex = .2,
  main = 'Price by Year',
  xlab = 'Year',
  ylab = 'Price of house in 1L',
  col = 'red',
)

#Analysis of Variance (ANOVA)

priceByDecade <- data.frame(Price = train$price, decade = train$yr_built)

#Earliest year built house
min(priceByDecade$decade)

#Recently year built house
max(priceByDecade$decade)

#distribution by year
hist(priceByDecade$decade,
     main = 'Distribution of houses by year',
     xlab = 'Decade built',
     ylab = 'count')

#prediction test.csv
prediction <- predict(model,test)

# Evaluation RMSE function
RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

rmseCalculate <- round(RMSE(prediction, test$price), digits = 5)
print(rmseCalculate)

#generating output file
prediction[which(is.na(prediction))] <- mean(prediction,na.rm=T)
#divide our sales price with 1Lakh So, our price in predictedData.csv in Lakh
submit <- data.frame(Id=test$id,SalePrice=prediction/100000)
write.csv(submit,file="PredictedData.csv",row.names=F)