#Name: PA Gunawardana
#Student ID: D/DBA/21/0045
#Subject: Categorical Data Analysis
#Assignment: Take Home Assignment


#Install packages
library(aod)
library(ggplot2)
library(car)
library(ResourceSelection)
library(pROC)

# Loading the data set
diabdata <- read.csv("C:/Users/Pasan/Desktop/KDU/Year 3/SEM2/CDA - Categorical Data Analysis/Assignment/diabetes.csv")
diabdata

# Data Exploration
# ----------------
str(diabdata)      # Structure of the data set
head(diabdata)     # Display the first few rows
## to get the total number of rows/observations in the data set
nrow(diabdata)
## to get the total number of columns/variables
ncol(diabdata)
## to view the variable names
names(diabdata)
##Basic Summary of the data
summary(diabdata)


# Check for missing values
missing_values <- colSums(is.na(diabdata))
print("Missing Values:")
print(missing_values)

# Check if all values in the 'Age' column are greater than or equal to 21
all_above_or_equal_21 <- all(diabdata$Age >= 21)
# Print the result
print(all_above_or_equal_21)



#Perform the Chi-Square test for categorical Variable 'Blood Pressure'
#H0: Variables are independent
#H1: Variables are not independent
contingency_table <- table(diabdata$Outcome, diabdata$Blood.Pressure)
print(diabdata$Blood.Pressure)
# Print the contingency table with variable names
colnames(contingency_table) <- c("High", "Medium", "Low")
rownames(contingency_table) <- c("No Diabetic", "Diabetic")
print(contingency_table)

chi_square_test <- chisq.test(contingency_table)
# Print the chi-square test results
print(chi_square_test)

#p-value= 0.000165. Therefore we have enough evidence to reject null hypothesis.
#Therefore we can conclude that Blood Pressure is a significant factor when deciding the outcome.



#Before we fit the logistic model, we have to make categorical data as factor variables. 
diabdata$Blood.Pressure <- factor(diabdata$Blood.Pressure)
diabdata$Blood.Pressure <- relevel(diabdata$Blood.Pressure, ref = "Low")

#Fit the Logistic Model 1
logistic_model1 <- glm(Outcome ~ Glucose + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age + Pregnancies + Blood.Pressure, data = diabdata, family = "binomial")
#Summary of the Logitistic Model 1
summary(logistic_model1)
##here R has fitted the model with all the predictors which I have specified. According to this it is visible that the SkinThickness, Insulin, Age and Blood Pressure are not significant.But here we have not specified a model sepection method. Therefore the model accuracy might be very low. This could not be the best model which fit the data.

# Map Outcome to factor with labels
diabdata$Outcome_Label <- factor(diabdata$Outcome, labels = c("No Diabetes", "Diabetes"))


# Scatter plot for Glucose vs Outcome
ggplot(diabdata, aes(x = Glucose, y = Outcome, color = Outcome_Label)) +
  geom_point() +
  ggtitle("Scatter Plot: Glucose vs Outcome") +
  scale_color_manual(values = c("No Diabetes" = "blue", "Diabetes" = "red"), labels = c("No Diabetes", "Diabetes")) +
  theme(legend.position = "top")

# Scatter plot for SkinThickness vs Outcome
ggplot(diabdata, aes(x = SkinThickness, y = Outcome, color = Outcome_Label)) +
  geom_point() +
  ggtitle("Scatter Plot: Skin Thickness vs Outcome") +
  scale_color_manual(values = c("No Diabetes" = "blue", "Diabetes" = "red"), labels = c("No Diabetes", "Diabetes")) +
  theme(legend.position = "top")

# Scatter plot for Insulin vs Outcome
ggplot(diabdata, aes(x = Insulin, y = Outcome, color = Outcome_Label)) +
  geom_point() +
  ggtitle("Scatter Plot: Insulin vs Outcome") +
  scale_color_manual(values = c("No Diabetes" = "blue", "Diabetes" = "red"), labels = c("No Diabetes", "Diabetes")) +
  theme(legend.position = "top")

# Scatter plot for BMI vs Outcome
ggplot(diabdata, aes(x = BMI, y = Outcome, color = Outcome_Label)) +
  geom_point() +
  ggtitle("Scatter Plot: BMI vs Outcome") +
  scale_color_manual(values = c("No Diabetes" = "blue", "Diabetes" = "red"), labels = c("No Diabetes", "Diabetes")) +
  theme(legend.position = "top")

# Scatter plot for DiabetesPedigreeFunction vs Outcome
ggplot(diabdata, aes(x = DiabetesPedigreeFunction, y = Outcome, color = Outcome_Label)) +
  geom_point() +
  ggtitle("Scatter Plot: Diabetes Pedigree Function vs Outcome") +
  scale_color_manual(values = c("No Diabetes" = "blue", "Diabetes" = "red"), labels = c("No Diabetes", "Diabetes")) +
  theme(legend.position = "top")

# Scatter plot for Pregnancies vs Outcome
ggplot(diabdata, aes(x = Pregnancies, y = Outcome, color = Outcome_Label)) +
  geom_point() +
  ggtitle("Scatter Plot: Pregnancies vs Outcome") +
  scale_color_manual(values = c("No Diabetes" = "blue", "Diabetes" = "red"), labels = c("No Diabetes", "Diabetes")) +
  theme(legend.position = "top")

# Scatter plot for Age vs Outcome
ggplot(diabdata, aes(x = Age, y = Outcome, color = Outcome_Label)) +
  geom_point() +
  ggtitle("Scatter Plot: Age vs Outcome") +
  scale_color_manual(values = c("No Diabetes" = "blue", "Diabetes" = "red"), labels = c("No Diabetes", "Diabetes")) +
  theme(legend.position = "top")



# Logistic Regression Modeling
# ----------------------------

#forward selection
#we consider all variables in the dataset as potential predictors
initial_model1 <- glm(Outcome ~ 1, data = diabdata, family = "binomial")
forward_model <- step(initial_model, direction = "forward",scope = formula(~ Glucose + Blood.Pressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age + Pregnancies), data = diabdata)
summary(forward_model)


#backward selection 
initial_model2 <- glm(Outcome~Glucose + Blood.Pressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age + Pregnancies, data = diabdata, family = "binomial")
backward_model <- step(initial_model2, direction = "backward")
summary(backward_model)

#step wise selection method
stepwise_model <- step(glm(Outcome ~ Glucose + Blood.Pressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age + Pregnancies, data = diabdata, family = "binomial"), direction = "both")
summary(stepwise_model)

##The all the 3 methods suggest the same model with Glucose,Insulin,BMI,DiabetesPedigreeFunction and Pregnancies in it. Even if we compare this with the logistic_model1 this the AIC value of the model which extracted by the Stepwise,forward and backward methods are low. Therefore we can continue with the model which extracted with Stepwise,forward or backward method. Lets continue with final model of stepwise method.
##Insulin seems to be not significant when we get the model summary. But it reduces the AIC value of the overall model when it is in the model.


## Calculate Wald confidence intervals
wald_intervals <- confint(stepwise_model)
wald_intervals
summary_with_intervals <- summary(stepwise_model)
summary_with_intervals$coefficients<-cbind(summary_with_intervals$coefficients,
                                          wald_intervals)

# Print the model summary with Wald confidence intervals
print(summary_with_intervals)

##  Model Diagnostics
#---------------------------------

# Get predicted probabilities from the model
predicted_probs <- predict(stepwise_model, type = "response")

# Create a data frame with observed and predicted values
observed_predicted <- data.frame(Observed = diabdata$Outcome, Predicted = 
                                   predicted_probs)
# Perform the Hosmer-Lemeshow test
# HO- Model adequately fit the data
# H1- Model does not adequately fit the data
hosmer_lemeshow_test <- hoslem.test(observed_predicted$Observed, 
                                    observed_predicted$Predicted)
# Print the test result
print(hosmer_lemeshow_test)

#p-value is 0.2906. Therefore we do not have enough evidence to reject H0. Therefore we can conclude that model is adequetly fit the data.


# Create a ROC curve
roc_curve <- roc(diabdata$Outcome, predicted_probs)
# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", print.auc = TRUE)
# Add AUC value to the plot
text(0.7, 0.3, paste("AUC =", round(auc(roc_curve), 2)), col = "blue")

## we get an area under the ROC curve value of 0.84 which can we considered as excellent.

# Create deviance residual plot
plot(stepwise_model, which = 2, col = "blue", main = "Deviance Residuals Plot")

# Perform deviance test
#Hypothesis
#Ho=Model adequately fit the data
#H1=Model does not adequately fit the data

deviance_test <- anova(stepwise_model, test = "Chisq")
print(deviance_test)


####### End of the Code #########






