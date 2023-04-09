# **RE6013 Team 7 Project**

### **Prediction of Heart Disease using Patient Data**

# (Extension 1) Prediction of Stroke based on Heart Disease Data (Regression - Logistic, Random Forest, CART)

# (Extension 2) Estimation of Heart Disease Relapse using Patient Data

# Setting up of R notebook

# to check the version of R
R.version.string

# #Install packages IF NEEDED
# install.packages("ggplots2", repos="https://cran.asia/", dependencies = TRUE)
# install.packages("data.table", repos="https://cran.asia/", dependencies = TRUE)
# install.packages("gtExtras", repos="https://cran.asia/", dependencies = TRUE)
#Use github repo instead because R Colabs don't seem to enjoy this CRAN mirror much
remotes::install_github("tidyverse/ggplot2")
remotes::install_github("jthomasmock/gtExtras")
install.packages("rlang", repos = "https://cloud.r-project.org/", dependencies = TRUE)
install.packages("caTools", repos="https://cran.asia/", dependencies = TRUE)
install.packages("MASS", repos="https://cran.asia/", dependencies = TRUE)
install.packages("rpart.plot", repos="https://cran.asia/", dependencies = TRUE)
install.packages("corrplot", repos="https://cran.asia/", dependencies = TRUE)
install.packages("caret", repos="https://cran.asia/", dependencies = TRUE)
install.packages("IRdisplay", repos="https://cran.asia/", dependencies = TRUE)

library("rlang")
packageVersion("rlang")

#Import necessary libraries
library(data.table)
library(ggplot2)
library(grid)
library(gtExtras)
library(IRdisplay)
library(nnet)
library(caTools)
library(MASS)
library(rpart)
library(rpart.plot)
library(caret)

library(corrplot)

#set working directory to current
setwd("./")

#Import csv
url <- "https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/aggregated_4data.csv"
cardio_train_url <- "https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/cardio_train.csv"
diabetes_url <- "https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/diabetes.csv"

#https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/processed.cleveland.csv
#https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/processed.hungarian.csv
#https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/processed.switzerland.csv
#https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/processed.va.csv

fourdata.dt <- fread(url)
kaggledata.dt <- fread(cardio_train_url)
diabetes.dt <- fread(diabetes_url)
sprintf("Data downloaded from %s!", paste(c(url, cardio_train_url, diabetes_url), collapse = ","))
head(fourdata.dt,2)
head(kaggledata.dt,2)
head(diabetes.dt,2)

#Dataset descriptions
sprintf("fourdata.dt is from UCI Machine Learning Repository containing %s entries and %s data attributes",nrow(fourdata.dt), ncol(fourdata.dt))
sprintf("Link is ")
sprintf("kaggledata.dt is from Kaggle containing %s entries and %s data attributes",nrow(kaggledata.dt), ncol(kaggledata.dt))
sprintf("diabetes.dt is from Kaggle containing %s entries and %s data attributes",nrow(diabetes.dt), ncol(diabetes.dt))

# Data Processing

# process the data
# TODO: PLS CHECK -> ASSUME ALL COLUMNS USING AN INTEGER IS CATEGORICAL DATA
fourdata.dt <- fourdata.dt[, lapply(.SD, function(x) if(is.integer(x)) as.factor(x) else x)]
kaggledata.dt <- kaggledata.dt[, lapply(.SD, function(x) if(is.integer(x)) as.factor(x) else x)]
diabetes.dt <- diabetes.dt[, lapply(.SD, function(x) if(is.integer(x)) as.factor(x) else x)]

#Filter out the unique cases
#Kaggle dataset age,height,weight,ap_hi and ap_lo is not categorical data
kaggledata.dt$age <- as.numeric(as.character(kaggledata.dt$age))
kaggledata.dt$height <- as.numeric(as.character(kaggledata.dt$height))
kaggledata.dt$weight <- as.integer(kaggledata.dt$weight)
kaggledata.dt$ap_hi <- as.integer(kaggledata.dt$ap_hi)
kaggledata.dt$ap_lo <- as.integer(kaggledata.dt$ap_lo)
head(kaggledata.dt,2)

#Diabetes dataset Pregnancies, Glucose, BloodPressure, SkinThickness, Insulin and Age is not categorical data
diabetes.dt$Pregnancies <- as.integer(diabetes.dt$Pregnancies)
diabetes.dt$Glucose <- as.integer(diabetes.dt$Glucose)
diabetes.dt$BloodPressure <- as.integer(diabetes.dt$BloodPressure)
diabetes.dt$SkinThickness <- as.integer(diabetes.dt$SkinThickness)
diabetes.dt$Insulin <- as.integer(diabetes.dt$Insulin)
diabetes.dt$Age <- as.integer(diabetes.dt$Age)
head(diabetes.dt,2)

#Final dataset types
head(fourdata.dt,2)
head(kaggledata.dt,2)
head(diabetes.dt,2)

# display the summary of fourdata.dt
summary(fourdata.dt)

# check for missing data in fourdata.dt datable
sum(is.na(fourdata.dt))

# notice that the age colum of kaggledata.dt is measured in days, to convert it to years
kaggledata.dt$years <- as.integer(round(kaggledata.dt$age / 365))
head(kaggledata.dt, 5)

# display the summary of kaggledata.dt
summary(kaggledata.dt)

# check for missing data in kaggle.dt datatable
sum(is.na(kaggledata.dt))

# Data Visualization and Exploratory Data Analysis

### Visualization 1: at what age does the number of people with CVD exceed the number of people without CVD?

# to stretch the ggplots
options(repr.plot.width=15, repr.plot.height=8)

ggplot(data=kaggledata.dt, aes(x=years, fill=cardio)) +
  geom_bar(position='dodge') +
  scale_fill_discrete(labels = c("0" = "No CVD", "1" = "CVD")) +
  labs(title='Bar Graph of Number of People vs Years Categorized by Presence of Cardiovascular Heart Disease', x='Years', y='Count') +
  theme(text = element_text(size = 15)
  )

ggplot(data = kaggledata.dt, aes(x=years , y=cardio, fill=cardio)) +
  geom_boxplot() +
  scale_fill_discrete(labels = c("0" = "No CVD", "1" = "CVD")) +
  labs(title = "Boxplot of Cardiovascular Disease Presence by Age") +
  stat_summary(fun.y="mean", color="black", shape=1 3)

# calculate mean ages of these 2 groups

mean(kaggledata.dt[, mean(years)])
mean(kaggledata.dt[cardio == 0][, mean(years)])
mean(kaggledata.dt[cardio == 1][, mean(years)])

# calculate median ages of these 2 groups

mean(kaggledata.dt[, median(years)])
mean(kaggledata.dt[cardio == 0][, median(years)])
mean(kaggledata.dt[cardio == 1][, median(years)])

### Findings 1

- at the age of 40, the percentage of patients with cardiovascular diseases is about 25%
- beyond the age of 55, the number of patients with cardiovascular diseases exceeds that of the healthy population
- the average age of the population without cardiovascular diseases is about 51.73 while that of the population with carduivascular diseases is 54.95. it shows that the chance of getting cardiovascular disease becomes larger as you get older

Truly alarming!

### Visualization 2: to plot density graphs on the systolic and diastolic blood pressure on groups that have CVD and those who don't


ggplot(data=kaggledata.dt, aes(x=ap_hi, fill=cardio)) +
  geom_density(alpha=0.5) +
  scale_fill_discrete(labels = c("0" = "No CVD", "1" = "CVD")) +
  labs(title='Kernel Density Graph of Blood Pressure', x='Systolic Blood Pressure (ap_hi)', y='Density') +
  theme(text = element_text(size = 15)
  )

ggplot(data=kaggledata.dt, aes(x=ap_lo, fill=cardio)) +
  geom_density(alpha=0.5) +
  scale_fill_discrete(labels = c("0" = "No CVD", "1" = "CVD")) +
  labs(title='Kernel Density Graph of Diastolic Blood Pressure', x='Diastolic Blood Pressure (ap_lo)', y='Density') +
  theme(text = element_text(size = 15)
  )

ggplot(kaggledata.dt, aes(x = ap_hi, y = ap_lo, shape=cardio, color=cardio)) +
    geom_point() +
    stat_smooth(method = "lm",
        col = "#C42126",
        se = FALSE,
        formula= y~x)

# correlation between the ap_hi and ap_lo
cor(kaggledata.dt$ap_lo, kaggledata.dt$ap_hi)

# finding the probability of getting CVD when the patient has higher than average ap_hi and ap_lo values

mean_aphi <- mean(kaggledata.dt$ap_hi)
mean_aplo <- mean(kaggledata.dt$ap_lo)

# create a table called cardio_count that stores the cardio data of those patients with above-than-average blood pressures
cardio_count <- table(kaggledata.dt$cardio[kaggledata.dt$ap_hi > mean_aphi & kaggledata.dt$ap_lo > mean_aplo])
cardio_count / sum(cardio_count) # normalizing the table

### Findings 2

- generally, CVD patients have higher systolic and diastolic blood pressures
- there is a 62.8% correlation between systolic and diastolic blood pressures, this means that generally when systolic pressure increases, diastolic pressure increases as well
- 80.2% of the patients with CVD have higher than average blood pressures

### Visualization 3: exploring other categorical columns based on cardiovascular presence


# to see the number of male and female patients in the dataset
ggplot(data=kaggledata.dt, aes(x=gender, fill=cardio)) +
  geom_bar() +
  scale_fill_discrete(labels = c("0" = "No CVD", "1" = "CVD")) +
  labs(title='Bar Graph of Number of People vs Gender Categorized by Presence of Cardiovascular Heart Disease', x='Gender', y='Count') +
  scale_x_discrete(labels = c("1" = "Female", "2" = "Male")) +
  geom_text(stat='count', aes(label=after_stat(count)), position = position_stack(vjust = 0.5)) +
  theme(text = element_text(size = 15)
  )

# to see the cholesterol levels in patients in the dataset
ggplot(data=kaggledata.dt, aes(x=cholesterol, fill=cardio)) +
  geom_bar() +
  scale_fill_discrete(labels = c("0" = "No CVD", "1" = "CVD")) +
  labs(title='Bar Graph of Number of People vs Cholesterol Level Categorized by Presence of Cardiovascular Heart Disease', x='Cholesterol', y='Count') +
  scale_x_discrete(labels = c("1" = "Normal", "2" = "Above Normal", "3" = "Well Above Normal")) +
  geom_text(stat='count', aes(label=after_stat(count)), position = position_stack(vjust = 0.5)) +
  theme(text = element_text(size = 15)
  )

# to see the glucose levels in patients in the dataset
ggplot(data=kaggledata.dt, aes(x=gluc, fill=cardio)) +
  geom_bar() +
  scale_fill_discrete(labels = c("0" = "No CVD", "1" = "CVD")) +
  labs(title='Bar Graph of Number of People vs Glucose Level Categorized by Presence of Cardiovascular Heart Disease', x='Glucose', y='Count') +
  scale_x_discrete(labels = c("1" = "Normal", "2" = "Above Normal", "3" = "Well Above Normal")) +
  geom_text(stat='count', aes(label=after_stat(count)), position = position_stack(vjust = 0.5)) +
  theme(text = element_text(size = 15)
  )

# to see whether the patients smoke in the dataset
ggplot(data=kaggledata.dt, aes(x=smoke, fill=cardio)) +
  geom_bar() +
  scale_fill_discrete(labels = c("0" = "No CVD", "1" = "CVD")) +
  labs(title='Bar Graph of Number of People vs Smoke Categorized by Presence of Cardiovascular Heart Disease', x='Smoke', y='Count') +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  geom_text(stat='count', aes(label=after_stat(count)), position = position_stack(vjust = 0.5)) +
  theme(text = element_text(size = 15)
  )

# to see whether the patients drink alcohol in the dataset
ggplot(data=kaggledata.dt, aes(x=alco, fill=cardio)) +
  geom_bar() +
  scale_fill_discrete(labels = c("0" = "No CVD", "1" = "CVD")) +
  labs(title='Bar Graph of Number of People vs Alcohol Categorized by Presence of Cardiovascular Heart Disease', x='Alcohol', y='Count') +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  geom_text(stat='count', aes(label=after_stat(count)), position = position_stack(vjust = 0.5)) +
  theme(text = element_text(size = 15)
  )

# to see whether the patients are active in the dataset
ggplot(data=kaggledata.dt, aes(x=active, fill=cardio)) +
  geom_bar() +
  scale_fill_discrete(labels = c("0" = "No CVD", "1" = "CVD")) +
  labs(title='Bar Graph of Number of People vs Active Categorized by Presence of Cardiovascular Heart Disease', x='Active', y='Count') +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  geom_text(stat='count', aes(label=after_stat(count)), position = position_stack(vjust = 0.5)) +
  theme(text = element_text(size = 15)
  )

### Findings 3

- the dataset has 45530 females and 24470 males. about 50% of each gender has cardiovascular disease
- cholesterol level is a huge determinant of whether the patients have cardiovascular disease. it can be seen that there is 27.2% more healthy patients than CVD patients with normal cholesterol level, while there is 51.4% more CVD patients than healthy ones with above normal cholesterol level and 226.3% with well above cholesterol level.
- gluclose level is also another huge determinant of whether patients have cardiovascular disease. 8.08%, 45.7%, 64.6%
- there are more inactive patients with CVD compared to inactive patients without CVD (15.4%), while active patients without CVD is 3.71% more than active patients with CVD. This means that while exercising and maintaining an active lifestyle is good, it does not necessarily mean that you are less likely to have CVD
- smoking and alcohol consumption does not seem to have an adverse effect on whether the patients have CVD or not


names(fourdata.dt)

#Seems like age, sex, chest pain, restecg, thalach are most important

#Start with tables
#Figure out what are important factors for target
categoricalColors <- c("0"="white", "1"="blue")
targetColors <- c("0"="#ff0000", "1"="#00b232")
fourdata.dt[target=="1"] %>% 
  gt() %>% 
  gt_theme_538() %>% 
  gt_color_rows(c("trestbps","chol","thalach","oldpeak","age"), palette=c("white", "blue")) %>%
  data_color(columns = "target", colors = targetColors) %>%
  data_color(columns = c("sex", "fbs", "cp", "restecg", "exang", "slope", "ca", "thal"),colors = categoricalColors) %>%
  tab_header(title = "UCI Machine Learning Dataset") %>%
  as_raw_html() %>%
  display_html()

## Visualization 4: Creating new BMI feature



kaggledata.dt$bmi <- kaggledata.dt$weight/((kaggledata.dt$height/100)**2)
head(kaggledata.dt,2)

# Convert categorical variables to numeric
kaggledata.dt$alco <- as.numeric(as.character(kaggledata.dt$alco))
kaggledata.dt$cholesterol <- as.numeric(as.character(kaggledata.dt$cholesterol))
kaggledata.dt$gluc <- as.numeric(as.character(kaggledata.dt$gluc))
kaggledata.dt$active <- as.numeric(as.character(kaggledata.dt$active))
kaggledata.dt$gluc <- as.numeric(as.character(kaggledata.dt$gluc))
kaggledata.dt$gender <- as.numeric(as.character(kaggledata.dt$gender))
kaggledata.dt$smoke <- as.numeric(as.character(kaggledata.dt$smoke))
kaggledata.dt$cardio <- as.numeric(as.character(kaggledata.dt$cardio))

# Calculate the correlation matrix
corr <- cor(kaggledata.dt[, c("alco", "gender", "smoke", "cardio","gluc", "cholesterol", "active")], use = "complete.obs", method = "pearson")

# Create a correlation plot
#corrplot(corr, type = "lower", order = "hclust", tl.col = "black", addCoef.col = "black")
corrplot(corr, type = "lower", order = "hclust", tl.col = "black",
         col = colorRampPalette(c("white", "red"))(10), 
         addCoef.col = "black", number.cex = 0.8, number.offset = 0.3,
         number.digits = 2, method = "color")

##Conclusion

We have sieved out the following data features that are important:
- Cholesterol
- Glucose

# Split into Train, Validation and Test

set.seed(420420)
data <- kaggledata.dt
split1 <- sample.split(data$cardio, SplitRatio = 0.6)
train_data <- subset(data, split1 == TRUE)
remain_data <- subset(data, split1 == FALSE)

split2 <- sample.split(remain_data$cardio, SplitRatio = 0.5)
valid_data <- subset(remain_data, split2 == TRUE)
test_data <- subset(remain_data, split2 == FALSE)
nrow(kaggledata.dt)
nrow(train_data)
nrow(valid_data)
nrow(test_data)

# # Perform GLM

names(kaggledata.dt)
head(kaggledata.dt,2)

## Trial 1 - Use everything

#Trial 1 - Blindly using all columns to create a logistic regression
heart.restricted.p1 <- glm(cardio ~ gender + height + weight + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + active + years,family = binomial,data = train_data )
summary(heart.restricted.p1)
OR <- exp(coef(heart.restricted.p1))
OR
prob <- predict(heart.restricted.p1, type='response')
threshold <- sum(train_data$cardio==1)/length(train_data$cardio)
pass.hat <- ifelse(prob > threshold, 1, 0)
table(train_data$cardio,pass.hat)
mean(pass.hat == train_data$cardio)

prob_valid <- predict(heart.restricted.p1, newdata = valid_data, type = "response")
threshold <- sum(valid_data$cardio==1)/length(valid_data$cardio)
pass.hat <- ifelse(prob_valid > threshold, 1, 0)
table(valid_data$cardio, pass.hat)
mean(pass.hat == valid_data$cardio)

## Trial 2 - Just chol, gluc, ap_hi, ap_lo and years

#Trial 2 - Restrict to just cholesterol and gluc
heart.restricted.p2 <- glm(cardio ~ cholesterol + gluc + years + ap_hi + ap_lo,family = binomial,data = train_data )
summary(heart.restricted.p2)
prob <- predict(heart.restricted.p2, type='response')
threshold <- sum(train_data$cardio==1)/length(train_data$cardio)
pass.hat2 <- ifelse(prob > threshold, 1, 0)
table(train_data$cardio,pass.hat2)
mean(pass.hat2 == train_data$cardio)

prob_valid <- predict(heart.restricted.p2, newdata = valid_data, type = "response")
threshold <- sum(valid_data$cardio==1)/length(valid_data$cardio)
pass.hat <- ifelse(prob_valid > threshold, 1, 0)
table(valid_data$cardio, pass.hat)
mean(pass.hat == valid_data$cardio)

## Trial 3 - Brute force to find the best model

# "years", "gender", "height", "weight", "ap_hi", "ap_lo", "cholesterol", "gluc", "smoke", "alco", "active"
predictors <- c("ap_hi", "ap_lo", "cholesterol", "gluc", "bmi", "gender","years")
aic_model <- glm(cardio ~ ., data = train_data[,c(predictors,"cardio"), with=FALSE], family = binomial)

best_model <- stepAIC(aic_model, direction = "both", scope = list(lower = ~ 1, upper = ~ .), trace = FALSE)
summary(best_model)

heart.restricted.p3 <- best_model
prob <- predict(heart.restricted.p3, type='response')
threshold <- sum(train_data$cardio==1)/length(train_data$cardio)
pass.hat2 <- ifelse(prob > threshold, 1, 0)
table(train_data$cardio,pass.hat2)
mean(pass.hat2 == train_data$cardio)

prob_valid <- predict(heart.restricted.p3, newdata = valid_data, type = "response")
threshold <- sum(valid_data$cardio==1)/length(valid_data$cardio)
pass.hat <- ifelse(prob_valid > threshold, 1, 0)
table(valid_data$cardio, pass.hat)
mean(pass.hat == valid_data$cardio)

## Test final accuracy using test set

#Test final accuracy based on test set
#For heart.restricted.p1
prob_test <- predict(heart.restricted.p1, newdata = test_data, type = "response")
threshold <- sum(test_data$cardio==1)/length(test_data$cardio)
pass.hat <- ifelse(prob_test > threshold, 1, 0)
table(test_data$cardio, pass.hat)
acc.p1 <-mean(pass.hat == test_data$cardio)
acc.p1

#For heart.restricted.p2
prob_test <- predict(heart.restricted.p2, newdata = test_data, type = "response")
threshold <- sum(test_data$cardio==1)/length(test_data$cardio)
pass.hat <- ifelse(prob_test > threshold, 1, 0)
table(test_data$cardio, pass.hat)
acc.p2 <- mean(pass.hat == test_data$cardio)
acc.p2

#For heart.restricted.p3
prob_test <- predict(heart.restricted.p3, newdata = test_data, type = "response")
threshold <- sum(test_data$cardio==1)/length(test_data$cardio)
pass.hat <- ifelse(prob_test > threshold, 1, 0)
table(test_data$cardio, pass.hat)
acc.p3 <- mean(pass.hat == test_data$cardio)
acc.p3

# Perform Logistic Regression

##Trial 1 - Use everything


heart.restricted.p4 <- multinom(cardio ~ gender + height + weight + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + active + years, data = train_data)
summary(heart.restricted.p4)
OR <- exp(coef(heart.restricted.p4))
z <- summary(heart.restricted.p4)$coefficients/summary(heart.restricted.p4)$standard.errors
pvalue <- (1-pnorm(abs(z),0,1))*2
prob <- predict(heart.restricted.p4, type='prob')
predicted_class <- predict(heart.restricted.p4)
table(train_data$cardio, predicted_class)
mean(predicted_class == train_data$cardio)

prob_valid <- predict(heart.restricted.p4, newdata = valid_data, type = "prob")
predicted_class <- predict(heart.restricted.p4, newdata = valid_data)
table(valid_data$cardio, predicted_class)
mean(predicted_class == valid_data$cardio)

## Trial 2 - Brute force to find the best model

# "years", "gender", "height", "weight", "ap_hi", "ap_lo", "cholesterol", "gluc", "smoke", "alco", "active"
predictors <- c("ap_hi", "ap_lo", "cholesterol", "gluc", "bmi", "gender","years")
aic_model <- multinom(cardio ~ ., data = train_data[,c(predictors,"cardio"), with=FALSE])

best_model <- stepAIC(aic_model, direction = "both", scope = list(lower = ~ 1, upper = ~ .), trace = FALSE)
summary(best_model)

heart.restricted.p5 <- best_model
OR <- exp(coef(heart.restricted.p5))
z <- summary(heart.restricted.p5)$coefficients/summary(heart.restricted.p5)$standard.errors
pvalue <- (1-pnorm(abs(z),0,1))*2
prob <- predict(heart.restricted.p5, type='prob')
predicted_class <- predict(heart.restricted.p5)
table(train_data$cardio, predicted_class)
mean(predicted_class == train_data$cardio)

prob_valid <- predict(heart.restricted.p5, newdata = valid_data, type = "prob")
predicted_class <- predict(heart.restricted.p5, newdata = valid_data)
table(valid_data$cardio, predicted_class)
mean(predicted_class == valid_data$cardio)

## Test final accuracy using test set

#Test final accuracy based on test set
#For heart.restricted.p4
prob_test <- predict(heart.restricted.p4, newdata = test_data, type = "prob")
predicted_class <- predict(heart.restricted.p4, newdata = test_data)
table(test_data$cardio, predicted_class)
acc.p4 <- mean(predicted_class == test_data$cardio)
acc.p4

#For heart.restricted.p5
prob_test <- predict(heart.restricted.p5, newdata = test_data, type = "prob")
predicted_class <- predict(heart.restricted.p5, newdata = test_data)
table(test_data$cardio, predicted_class)
acc.p5 <- mean(predicted_class == test_data$cardio)
acc.p5

#Model Conclusion

# The highest accuracies that we can find are typically the GLM or Logistic Regression Models that use all the available data available. However, we are also able to produce more lightweight models consisting of a smaller set of features but with roughly the same accuracy. This could be more useful when trying to deploy these models in production.
# The accuracies for both GLM and Logistic Regression are around 72.8%

#CART


cart1 <- rpart(cardio ~ ., data=train_data, method ='class', control = rpart.control(minsplit =2, cp= 0))
rpart.plot(cart1, nn=T, main="Maximal Tree in cart1")

print(cart1)

cart_model <- rpart(cardio ~ cholesterol + gluc + smoke + alco + years + active, data=train_data, method="class")

# Visualize the CART model
rpart.plot(cart_model)
printcp(cart_model) # display the results
# plotcp(cart_model) # visualize cross-validation results
summary(cart_model) # detailed summary of splits

predictions <- predict(cart_model, test_data, type="class")
confusion_matrix <- table(predictions, test_data$cardio)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)

cat("Accuracy:", round(accuracy, 3))

# naive preprocessing
data2 <- kaggledata.dt

split2 <- sample.split(data2$cardio, SplitRatio = 0.6)
train_data2 <- subset(data2, split2 == TRUE)
remain_data2 <- subset(data2, split2 == FALSE)


# need to convert continous numeric variables and bin them into categorical variables for cart
train_data2$ap_hi <- as.factor(train_data2$ap_hi)
train_data2$ap_lo <- as.factor(train_data2$ap_lo)
train_data2$weight <- as.factor(train_data2$weight)


split3 <- sample.split(remain_data2$cardio, SplitRatio = 0.5)
valid_data2 <- subset(remain_data2, split3 == TRUE)
test_data2 <- subset(remain_data2, split3 == FALSE)

cart_model <- rpart(cardio ~ gender + weight + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + years + active, data=train_data, method="class")
rpart.plot(cart_model)
printcp(cart_model) # display the results
summary(cart_model) # detailed summary of splits

plotcp(cart_model) # visualize cross-validation results
summary(cart_model) # detailed summary of splits

predictions <- predict(cart_model, test_data, type="class")
confusion_matrix <- table(predictions, test_data$cardio)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
cat("Accuracy:", round(accuracy, 3))

cart_model <- rpart(cardio ~ gender + weight + cholesterol + gluc + smoke + alco + years + active, data=train_data, method="class")
rpart.plot(cart_model)
printcp(cart_model) # display the results
plotcp(cart_model) # visualize cross-validation results
summary(cart_model) # detailed summary of splits

predictions <- predict(cart_model, test_data, type="class")
confusion_matrix <- table(predictions, test_data$cardio)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
cat("Accuracy:", round(accuracy, 3))

# Exportooooooor

# export data
# head(kaggledata.dt,2)
# write.csv(kaggledata.dt, "heartdisease.csv")