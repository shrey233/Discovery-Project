data1 <- read.csv("fifa_AllFairnessScore.csv")

set.seed(1)

attach(data1)

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


# Calculate MSE by taking the mean of squared error difference.
MSE <- mean((fifa_predicted1 - fifa_test1) ^ 2)
print(MSE)

# Model to have Fairness & Team Rank Interaction + Fairness & Year
multi_model3 <- lm(FinalPos ~ Fairness*TeamRank + Fairness*Year, data = train1)

# View summary of model.
summary(multi_model3)


#plot(data1$FinalPos~data1$Fairness)
#abline(data1$TeamRank)


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


# Plot to show Fairness ~ Team Rank w Line of Best Fit for each Year

library(ggplot2)

data2 <- data1

data2$Year <- as.factor(data2$Year)

ggplot(data2) +
  aes(x = Fairness, y = TeamRank, color = Year) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm")


multi_model5 <- lm(FinalPos ~ TeamRank, data = train1)

summary(multi_model5)

# Predict for testing dataset.
finalpos_predict5 <- predict(multi_model5, newdata = test1)

# Fill vector with actual values from testing dataset.
fifa_test5 <- test1[, "FinalPos"]

MSE5 <- mean((finalpos_predict5 - fifa_test5) ^ 2)
print(MSE5)


predTable5 <- cbind(test1, finalpos_predict5)

################################################################

multi_model6 <- lm(FinalPos ~ TeamRank + Fairness, data = train1)

summary(multi_model6)

# Predict for testing dataset.
finalpos_predict6 <- predict(multi_model6, newdata = test1)

# Fill vector with actual values from testing dataset.
fifa_test6 <- test1[, "FinalPos"]

MSE6 <- mean((finalpos_predict6 - fifa_test6) ^ 2)
print(MSE6)


predTable6 <- cbind(test1, finalpos_predict6)



# simulate
