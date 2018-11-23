install.packages("rcompanion") # To observe the levels of the dummy variables
library(rcompanion)

install.packages("ROSE") # To contact ROC analysis and model comparison
library(ROSE)

# install.packages("pscl") # To find R_sq
library(pscl)

install.packages("remotes")
library(remotes) # To install a depracated package from archive
install_version("epicalc", "2.15.1.0") # To contact a LR test
library(epicalc)

install.packages("extRemes") # To contact a LR test
library(extRemes)

# install.packages('caTools') # To split the dataset to train and test set
library(caTools)

install.packages("corrplot") # To create correlation plots
library(corrplot)

install.packages("corrgram")
library(corrgram)
install.packages("Hmisc")
library(Hmisc)

# Load data_challenge_Script.R
source("data_challenge_Script.R")

# Examine clean_d
summary(trans_d)
str(trans_d)
names(trans_d)

# Create new df to use it for modelling
model_d = trans_d

#### Convert numerics to factors when necessary ###########################################################################
# age is integer
table(model_d$age)
hist(model_d$age) # right skewed
range(model_d$age) # 31-67 years

# alcohol is a character but needs to be a factor with 2 levels: 0-low, 1-high
table(model_d$alcohol)
model_d$alcohol = as.factor(model_d$alcohol)
contrasts(model_d$alcohol)

# mouthwash is integer but needs to be a factor with 2 levels: 0-no mouthwash, 1:use of mouthwash
table(model_d$mouthwash)
model_d$mouthwash = as.factor(model_d$mouthwash)
contrasts(model_d$mouthwash)

# n.fillings is integer
hist(model_d$n.fillings)
range(model_d$n.fillings) # 0-32 fillings

# sex is integer but needs to be a factor with 2 levels: 0:male, 1:female
table(model_d$sex)
model_d$sex = as.factor(model_d$sex)
contrasts(model_d$sex)

# smoke is integer but needs to be a factor with 2 levels: 0:no smoker, 1:smoker
table(model_d$smoke)
model_d$smoke = as.factor(model_d$smoke)
contrasts(model_d$smoke)

# typ2_diabetes is integer but needs to be a factor with 2 levels: 0:no, 1:yes
table(model_d$typ2_diabetes)
model_d$typ2_diabetes = as.factor(model_d$typ2_diabetes)
contrasts(model_d$typ2_diabetes)

# water_fluor is integer but needs to be a factor with 2 levels: 0:no, 1:yes
table(model_d$water_fluor)
model_d$water_fluor = as.factor(model_d$water_fluor)
contrasts(model_d$water_fluor)

# probing_depth is integer
hist(model_d$probing_depth) # probing_depth is right skewed but should not be kept eitherway because of multicollinearity with other variables
range(model_d$probing_depth) # 0-7 

# BMI is integer
hist(model_d$BMI)
range(model_d$BMI) # 23.84978 66.06639

# Flmins is integer
range(model_d$Flmins) # 0-45500
hist(model_d$Flmins, breaks = 50)
nrow(model_d[clean_d$Flmins==0,]) # 89 Zeros
# Flmins is right skewed (tail on the right). No need to log-transform


# gumd_binary is integer but needs to be a factor with 2 levels: 0:no disease, 1:disease
table(model_d$gumd_binary)
model_d$gumd_binary = as.factor(model_d$gumd_binary)
contrasts(model_d$gumd_binary)


#### Correlations in model_d ##############################################################################################
str(model_d)
summary(model_d)
x = rcorr(as.matrix(model_d))

corrplot(x$r)

# Different corrplot 
corrgram(model_d, order = F, upper.panel = panel.cor, main="gumd_binary", cor.method = "spearman")
# Probing_depth is correlated with age, n_fillings. We need to remove it.
# Also age is correlated with n_fillings. Maybe we need to remove the latter.

# Remove probing_depth
model_d$probing_depth = NULL

#### Feature Scaling ########################################################################################
str(model_d)
continuous_vars = model_d[,c(1, 4, 9:10)] # Extract only continuous vars
discrete_vars = model_d[, -c(1, 4, 9:10)] # Extract only discrete vars

scaled_cont_vars = scale(continuous_vars)               # Scale-standardize continuous vars
scaled_cont_vars_df = as.data.frame(scaled_cont_vars)

model_d = cbind(scaled_cont_vars_df, discrete_vars)  # Bind the cnontinuous-scaled and the discrete-unscaled vars
write.csv(model_d, "model_d.csv", row.names = F)

#### BACKWARD ELIMINATION ####################################################################################
#### MODEL 1: All initial variables #########################################################################

# Splitting the dataset into the Training set and Test set

set.seed(123) # have a constant seed to reproduce the same result every time
split = sample.split(model_d$gumd_binary, SplitRatio = 0.75)
training_set = subset(model_d, split == TRUE)
test_set = subset(model_d, split == FALSE)


# Fitting Logistic Regression to the Training set 
model_1 = glm(formula = gumd_binary ~ age + n.fillings + BMI + Flmins + alcohol + mouthwash + sex + smoke + typ2_diabetes + water_fluor,
                 family = binomial,
                 data = training_set)

# See outputs
summary(model_1)
# Save outputs to .txt
sink("model_1.txt")
print(summary(model_1))
sink() 



# Find R_sq

round(pR2(model_1)[4], 3) # McFadden R_sq = 0.422


# Predicting the Test set results
prob_pred_model1 = predict(model_1, type = "response", newdata = test_set[-11])
y_pred = ifelse(prob_pred_model1 > 0.5, 1, 0)


# Making the Confusion matrix
cm = table(test_set[, 11], y_pred > 0.5) 
# True Negatives (TN) = 149
# True Positives (TP) = 56
# False Negatives (FN) = 20
# False Positives (FP) = 21
# Accuracy = (TP + TN) / (TN + TP + FN + FP) - Accurate Positive and Negative Predictions
Accuracy_model1 = round((cm[1, 1] + cm[2, 2]) / (cm[1, 1] + cm[2, 2] + cm[1, 2] + cm[2, 1]), 2) # 0.83
# Precision = TP / Total Predicted Positive (FP + TP) - How precise are the positive predictions 
Precision_model1 = round(cm[2, 2] / (cm[1, 2] + cm[2, 2]), 2) # 0.73
# Recall = TP / Total Actual Positive (TP + FN) - How many of the Actual Positives are predicted accurately as Positives
Recall_model1 = round(cm[2, 2] / (cm[2, 1] + cm[2, 2]), 2) # 0.74
# Recall is extremely important in disease detection. If a sick patient (actual positive)
# is diagnosed as non patient (False Negative), it can have significant impacts to his/her life.

# F1 score = 2 * Precision * Recall / (Precision + Recall)
F1score_model1 = round(2*Precision_model1*Recall_model1 / (Precision_model1 + Recall_model1), 2) # 0.73


# ROC analysis

roc.curve(test_set$gumd_binary, prob_pred_model1) # AUC = 0.897

# Find Log-Likelihood
logLik(model_1) # -264.83 df=11 (11 parameters)




#### MODEL 2: Same as MODEL 1 BUT WITHOUT SEX #########################################################################

# Fitting Logistic Regression to the Training set 
model_2 = glm(formula = gumd_binary ~ age + n.fillings + BMI + Flmins + alcohol + mouthwash + smoke + typ2_diabetes + water_fluor,
              family = binomial,
              data = training_set[-7])

# See outputs
summary(model_2)
# Save outputs to .txt
sink("model_2.txt")
print(summary(model_2))
sink() 

# All variables are statistically significant, except water_fluor (significant at the 90% level)

#### Find R_sq ################################################################

round(pR2(model_2)[4], 3) # McFadden R_sq = 0.382

#### Predicting the Test set results #####################################################
prob_pred_model2 = predict(model_2, type = "response", newdata = test_set[-c(7,11)])
y_pred = ifelse(prob_pred_model2 > 0.5, 1, 0)

# Making the Confusion matrix
cm = table(test_set[, 11], y_pred > 0.5) 
# True Negatives (TN) = 150
# True Positives (TP) = 51
# False Negatives (FN) = 25
# False Positives (FP) = 20
# Accuracy = (TP + TN) / (TN + TP + FN + FP) - Accurate Positive and Negative Predictions
Accuracy_model2 = round((cm[1, 1] + cm[2, 2]) / (cm[1, 1] + cm[2, 2] + cm[1, 2] + cm[2, 1]), 2) # 0.82
# Precision = TP / Total Predicted Positive (FP + TP) - How precise are the positive predictions 
Precision_model2 = round(cm[2, 2] / (cm[1, 2] + cm[2, 2]), 2) # 0.72
# Recall = TP / Total Actual Positive (TP + FN) - How many of the Actual Positives are predicted accurately as Positives
Recall_model2 = round(cm[2, 2] / (cm[2, 1] + cm[2, 2]), 2) # 0.67
# Recall is extremely important in disease detection. If a sick patient (actual positive)
# is diagnosed as non patient (False Negative), it can have significant impacts to his/her life.

# F1 score = 2 * Precision * Recall / (Precision + Recall) - It is the harmonic mean of Precision and Recall
F1score_model2 = round(2*Precision_model2*Recall_model2 / (Precision_model2 + Recall_model2), 2) # 0.69

# ROC analysis
roc.curve(test_set$gumd_binary, prob_pred_model2) # 0.901

# Likelihood Ratio Test
logLik(model_3) # -282.77
lr.test(logLik(model_3), logLik(model_1)) # -2(LL2 - LL1)



#### Stepwise regression ######################################################

# install.packages("leaps")
library(leaps)
library('car')

stepwise_reg = regsubsets(gumd_binary ~ age + n.fillings + BMI + Flmins + alcohol + mouthwash + smoke + typ2_diabetes + water_fluor, data = model_d, nbest = 1) # Find only the best of each variable-number category

summary(stepwise_reg)

subsets(stepwise_reg, statistic = 'rsq')

# Stepwise regression leads to the same final output - All variables are statistically significant except water_fluor