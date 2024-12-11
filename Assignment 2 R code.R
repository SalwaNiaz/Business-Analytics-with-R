#Assignment 2 BA with R

library(forecast)
data <- read.csv("BostonHousing1.csv")

#. Fit a multiple linear regression model to the median house MEDV (MEDV) as a function of CRIM,
#CHAS, and RM. Write the equation for predicting the median house MEDV from the predictors
#in the model#

# use first 300 rows of data
house.df <- data[1:300, ]
# select variables for regression
selected.var <- c(1, 4, 6, 13)

# partition data
set.seed(1)  
train.index <- sample(c(1:300), 60)  
train.df <- house.df[train.index, selected.var]
valid.df <- house.df[-train.index, selected.var]

# using lm() to run a linear regression of MEDV on the selected predictors in the
# training set. 
model <- lm(MEDV ~ ., data = train.df)

options(scipen = 999)
summary(model)

#Predict with given values:
data_input <- data.frame(CRIM = 0.1, CHAS = 0, RM = 6)

#Predict the median house price using the model
pMEDV <- predict(model, newdata = data_input); pMEDV

# Assuming we know the actual MEDV for this tract
p.error <- predict(model, newdata = data_input, se.fit = TRUE);p.error 
prediction_error <- p.error$se.fit;prediction_error

#d(ii)

cor_matrix <- cor(data[, c("CRIM", "ZN", "INDUS", "CHAS", "NOX", 
             "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "LSTAT")]); cor_matrix

high_cor <- ifelse(cor_matrix > 0.5 & cor_matrix < 1 
                   | cor_matrix < -0.5 & cor_matrix > -1, cor_matrix, NA); high_cor


#d(iii)

#full model with all variables:
# use first 300 rows of data
house.df1 <- data[1:300,1:13]

# partition data
set.seed(1)  # set seed for reproducing the partition
train.index1 <- sample(c(1:300), 60)  
train.df1 <- house.df1[train.index,]
valid.df1<- house.df1[-train.index,]

model2 <- lm(MEDV ~ ., data = train.df1)

options(scipen = 999)
summary(model2)

#create model with no predictors
house.lm.null <- lm(MEDV~1, data = train.df1)

# use step() to run forward regression.
house.lm.stepf <- step(house.lm.null, scope=list(lower=house.lm.null, upper=model2), direction = "forward")
summary(house.lm.stepf)  

# predicting forward with validation set
house.lm.step.predf <- predict(house.lm.stepf, valid.df1)
accuracy(house.lm.step.predf, valid.df1$MEDV)


# use step() to run backward regression.
house.lm.stepb <- step(model2, direction = "backward")
summary(house.lm.stepb)  

# predicting backward with validation set
house.lm.step.predb <- predict(house.lm.stepb, valid.df1)
accuracy(house.lm.step.predb, valid.df1$MEDV)


# use step() to run 'both' regression.
house.lm.stepfb <- step(model2, direction = "both")
summary(house.lm.stepfb)  

# predicting both with validation set
house.lm.step.predfb <- predict(house.lm.stepfb, valid.df1)
accuracy(house.lm.step.predfb, valid.df1$MEDV)

##lift charts

actual = valid.df1$MEDV

#lift for forward
gain1 = gains(actual, 
              house.lm.step.predf,
              group = 10)

plot(c(0, gain1$cume.pct.of.total*sum(actual))~c(0, gain1$cume.obs), type = "l", 
     xlab = "#Cases", ylab = "Cumulative MEDV", main = "Lift Chart for forwards")
segments(0, 0, nrow(valid.df1), sum(actual), lty = "dashed", col = "red", lwd = 2)

#lift for backwards
gain2 = gains(actual, 
              house.lm.step.predb,
              group = 10)

plot(c(0, gain2$cume.pct.of.total*sum(actual))~c(0, gain2$cume.obs), type = "l", 
     xlab = "#Cases", ylab = "Cumulative MEDV", main = "Lift Chart for backwards")
segments(0, 0, nrow(valid.df1), sum(actual), lty = "dashed", col = "red", lwd = 2)


#lift for both
gain3 = gains(actual, 
              house.lm.step.predfb,
              group = 10)

plot(c(0, gain3$cume.pct.of.total*sum(actual))~c(0, gain3$cume.obs), type = "l", 
     xlab = "#Cases", ylab = "Cumulative MEDV", main = "Lift Chart for both")
segments(0, 0, nrow(valid.df1), sum(actual), lty = "dashed", col = "red", lwd = 2)








