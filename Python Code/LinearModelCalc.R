data1 <- read.csv("fifa_AllFairnessScore.csv")

set.seed(1)

#data1$Country = as.factor(data1$Country)

# Defining training set by taking data until 2014.
train1 <- data1[1:176, ]

# Defining testing set by taking remaining 2018 data
test1 <- data1[177:208, ]

# Create a multiple linear regression model based on given response and predictor variables.
multi_model1 <- lm(FinalPos ~ Fairness*TeamRank, data = train1)

# View summary of model.
summary(multi_model1)

# Create a model using backwards step function.
backwards_multi_model <- step(multi_model1, direction = "backward")

# Assign backwards_log_model to log_model2.
multi_model2 <- backwards_multi_model

# Show summary of the improved model.
summary(multi_model2)


# Predict for testing dataset.
fifa_predicted1 <- predict(multi_model1, newdata = test1)

# Fill vector with actual values from testing dataset.
fifa_test1 <- test1[, "FinalPos"]


# Model to have Fairness & Team Rank Interaction + Fairness & Year
multi_model3 <- lm(FinalPos ~ Fairness*TeamRank + Fairness*Year, data = train1)

# View summary of model.
summary(multi_model3)


#plot(data1$FinalPos~data1$Fairness)
#abline(data1$TeamRank)

# Calculate MSE by taking the mean of squared error difference.
MSE <- mean((fifa_predicted1 - fifa_test1) ^ 2)
print(MSE)

# team rank to predict fairness
multi_model4 <- lm(Fairness ~ TeamRank, data = train1)

summary(multi_model4)



## residuals - fairness adjusted for rank, take average per country


# create plots for models
plot(multi_model2)

plot(multi_model3)

plot(multi_model4)


## Scatter plot
## FINAL POS V FAIRNESS, LINE FOR TEAM RANK



# simulate

