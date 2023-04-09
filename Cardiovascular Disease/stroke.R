# # **RE6013 Team 7 Project**

# Prediction of Heart Disease using Patient Data

# ### **(Extension 1) Prediction of Stroke based on Heart Disease Data (Regression - Logistic, Random Forest, CART)**

# (Extension 2) Estimation of Heart Disease Relapse using Patient Data

# Setting up of R notebook

# to check the version of R
R.version.string

# #Install packages IF NEEDED
# install.packages("ggplots2", repos="https://cran.asia/", dependencies = TRUE)
# install.packages("data.table", repos="https://cran.asia/", dependencies = TRUE)
# install.packages("gtExtras", repos="https://cran.asia/", dependencies = TRUE)
#Use github repo instead because R Colabs don't seem to enjoy this CRAN mirror much
remotes::install_github("jthomasmock/gtExtras")

# install packages
install.packages("naniar")
install.packages("skimr")
install.packages("caret")
install.packages("MLmetrics")
install.packages("imbalance")
install.packages("gridExtra")
install.packages("patchwork")
install.packages("corrplot")

#Import necessary libraries
library(data.table)
library(ggplot2)
library(grid)
library(gtExtras)
library(IRdisplay)
library(nnet)



load libraries
library(tidyverse) # metapackage of all tidyverse packages
library(naniar) # handling missing data
library(skimr) # quick overview over the dataset
library(caret) # ML toolkit
library(MLmetrics) # F1 Score
library(imbalance) # algorithms to deal with imbalanced datasets
library(gridExtra) # display plots in grids
library(patchwork) # arrange plots side by side
library(caTools)
library(corrplot)

install.packages("MASS")
library(MASS)

#set working directory to current
setwd("./")

#Import csv
url <- "https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/Cardiovascular%20Disease/aggregated_4data.csv"
cardio_train_url <- "https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/Cardiovascular%20Disease/cardio_train.csv"
diabetes_url <- "https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/Cardiovascular%20Disease/diabetes.csv"
stroke_url <- "https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/Cardiovascular%20Disease/healthcare-dataset-stroke-data.csv"

#https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/Cardiovascular%20Disease/processed.cleveland.csv
#https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/Cardiovascular%20Disease/processed.hungarian.csv
#https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/Cardiovascular%20Disease/processed.switzerland.csv
#https://raw.githubusercontent.com/howry423/Dataset-Analysis-with-R/main/Cardiovascular%20Disease/processed.va.csv

fourdata.dt <- fread(url)
kaggledata.dt <- fread(cardio_train_url)
diabetes.dt <- fread(diabetes_url)
stroke.dt <- fread(stroke_url)
sprintf("Data downloaded from %s!", paste(c(url, cardio_train_url, diabetes_url), collapse = ","))
head(fourdata.dt,2)
head(kaggledata.dt,2)
head(diabetes.dt,2)
head(stroke.dt, 2)

#Dataset descriptions
sprintf("fourdata.dt is from UCI Machine Learning Repository containing %s entries and %s data attributes",nrow(fourdata.dt), ncol(fourdata.dt))
sprintf("Link is ")
sprintf("kaggledata.dt is from Kaggle containing %s entries and %s data attributes",nrow(kaggledata.dt), ncol(kaggledata.dt))
sprintf("diabetes.dt is from Kaggle containing %s entries and %s data attributes",nrow(diabetes.dt), ncol(diabetes.dt))
sprintf("stroke.dt is from Kaggle containing %s entries and %s data attributes",nrow(stroke.dt), ncol(stroke.dt))

# Data Processing

# process the data
fourdata.dt <- fourdata.dt[, lapply(.SD, function(x) if(is.integer(x)) as.factor(x) else x)]
kaggledata.dt <- kaggledata.dt[, lapply(.SD, function(x) if(is.integer(x)) as.factor(x) else x)]
diabetes.dt <- diabetes.dt[, lapply(.SD, function(x) if(is.integer(x)) as.factor(x) else x)]
stroke.dt <- stroke.dt[, lapply(.SD, function(x) if(is.integer(x)) as.factor(x) else x)]

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

# check unique values of categorical values
cat("Gender:")
unique(stroke.dt$gender)
cat("Married:")
unique(stroke.dt$ever_married)
cat("Work type:")
unique(stroke.dt$work_type)
cat("Residence type:")
unique(stroke.dt$Residence_type)
cat("Smoking:")
unique(stroke.dt$smoking_status)

# how many "N/A" values are in my dataset per column?
miss_scan_count(data = stroke.dt, search = list("N/A", "Unknown"))

# There are 201 "N/A" values in the bmi column that likely caused this column to be parsed as character, although it should be numerical. Let's take care of that by replacing those values with actual NAs. Moreover, there are a lot of "Unknown" values in smoking_status which we have to take care of too. The plot below puts this number into perspective.

options(repr.plot.width=15, repr.plot.height=8)

stroke.dt %>%
group_by(smoking_status) %>%
summarise(count = length(smoking_status)) %>%
mutate(smoking_status = factor(smoking_status)) %>%
ggplot(aes(x = fct_reorder(smoking_status, count), y = count, fill = factor(ifelse(smoking_status=="Unknown","Unknown","Known")))) +
geom_col() +
geom_text(aes(label = count, x = smoking_status, y = count), size = 6, hjust = 1.5) +
coord_flip() +
scale_fill_manual(values = c("Unknown" = "red", "Known" = "darkgrey")) +
labs(x = "smoking status")

# We see that we have 1544 unknown values for smoking status and therefore are missing a lot of information in a potentially informative predictor. We will have to deal with this. Lets replace those values with NAs.

# replace the "N/A" in bmi
stroke_data_clean <- replace_with_na(data = stroke.dt, replace = list(bmi = c("N/A"), smoking_status = c("Unknown"))) %>%
    # change bmi to numeric 
    mutate(bmi = as.numeric(bmi))

# check
summary(stroke_data_clean)
unique(stroke_data_clean$smoking_status)

# visualize the missing values
vis_miss(stroke_data_clean, cluster = TRUE)

# check distribution of bmi
ggplot(stroke_data_clean, aes(x = bmi)) +
geom_histogram() +
labs(title = "Distribution of BMI")

# The distribution is right skewed (long tail to the right). Because this is the only variable with missing data (at least of the numerical variables) we can impute the median on the missing data without losing too much information. Note: Imputation can also be done later during the modelling process. caret offers an optional parameter called preprocess. To impute the median you would want preprocess = "medianTmpute". However, I will do that prior to model building here.

# Now, to evaluate imputation

# impute median and bind shadow to evaluate imputation
stroke_data_imp <- bind_shadow(stroke_data_clean) %>% 
impute_median_at(.vars = c("bmi")) %>%
add_label_shadow()

# Explore the median values in bmi in the imputed dataset
ggplot(stroke_data_imp, 
       aes(x = bmi_NA, y = bmi)) + 
geom_boxplot() +
labs(title = "Comparison, no-missing vs. imputed values for BMI")

# use the fill() function to impute those values since the values are supposed to be missing at random.
stroke_data_imp <- impute_median_at(stroke_data_clean, .vars = c("bmi"))

p1 <- ggplot(stroke_data_imp, 
       aes(x = smoking_status, fill = smoking_status)) + 
geom_bar() +
labs(title = "Before filling in NA values in smoking_status") +
theme(legend.position = "none")

# fill imputation based on previous unique value in "smoking_status" column
after <- stroke_data_imp %>% 
fill(smoking_status)
# mode imputation which leads to worse performance of models:
#mutate(across(c(smoking_status)), replace(., is.na(.), "never smoked"))

# Explore the median values in bmi in the imputed dataset
p2 <- ggplot(after, 
       aes(x = smoking_status, fill = smoking_status)) + 
geom_bar() +
labs(title = "After filling in NA values in smoking_status") +
theme(legend.position = "none")

grid.arrange(p1, p2, ncol=2)

convert bmi from a continuous to a factor according to the bmi categories of the CDC. This has improved the random forest model slightly
stroke_data_imp2 <- stroke_data_imp %>%
fill(smoking_status) %>%
#mutate(across(c(smoking_status)), replace(., is.na(.), "never smoked")) %>%
mutate(across(c(hypertension, heart_disease), factor),
      across(where(is.character), as.factor),
      across(where(is.factor), as.numeric),
      stroke = factor(ifelse(stroke == 0, "no", "yes")))

stroke_data_imp2 <- stroke_data_imp2 %>%
mutate(bmi = case_when(bmi < 18.5 ~ "underweight",
                      bmi >= 18.5 & bmi < 25 ~ "normal weight",
                      bmi >= 25 & bmi < 30 ~ "overweight",
                      bmi >= 30 ~ "obese"),
      bmi = factor(bmi, levels = c("underweight", "normal weight", "overweight", "obese"), order = TRUE))

#Final dataset types
head(fourdata.dt,2)
head(kaggledata.dt,2)
head(diabetes.dt,2)
head(stroke_data_imp,2)
head(after, 2)

# display the summary of fourdata.dt
summary(after)

# check for missing data in fourdata.dt datable
sum(is.na(after))

# change variable name
stroke_cleaned <- after

# Data Visualization and Exploratory Data Analysis

# Subset Data into Yes and No (stroke)
Yes <- subset(stroke_cleaned, stroke == '1')
No <- subset(stroke_cleaned, stroke == '0')

### Visualization 1: exploring internal characteristics such as  hypertension, CVD, gender of patients based on stroke presence

# Create stroke counts table
strokecounts <- as.data.frame(table(stroke_cleaned$stroke))

# As 'Female' has the majority counts, replace num to char 
strokecounts$Var1 <- ifelse(strokecounts$Var1 == 0, "No", 'Yes') 

# Bar Chart of Patients With and Without Strokes
ggplot(strokecounts, aes(x = Var1, y = Freq, fill = Var1)) +
        geom_bar(stat = "identity") + theme(legend.position="none") +
        geom_text(aes(label = Freq), vjust = 0) +
        labs(title="Stroke Status of Patients",x ="Stroke", y = "Frequency") +
        theme(plot.title = element_text(hjust = 0.5))

# Create hypertension counts table
hypercounts <- as.data.frame(table(stroke_cleaned$hypertension, stroke_cleaned$stroke))
  
# Replace num to char
hypercounts$Var1 <- ifelse(hypercounts$Var1 == 0, "No", 'Yes')
hypercounts$Var2 <- ifelse(hypercounts$Var2 == 0, "No", 'Yes')

# Replace headers
colnames(hypercounts)[1] <- 'Hypertension'
colnames(hypercounts)[2] <- 'Stroke'

# Bar Chart of Hypertension : No vs. Yes     
ggplot(hypercounts, aes(x = Hypertension, y = Freq, fill = Stroke)) +
        geom_bar(stat = "identity")+
        geom_text(aes(label = Freq), vjust = 0) +
        labs(title="Hypertension Status of Patients",x ="Hypertension", y = "Frequency") +
        theme(plot.title = element_text(hjust = 0.5))

# Create heart disease counts table
heartcounts <- as.data.frame(table(stroke_cleaned$heart_disease, stroke_cleaned$stroke))

# Replace num to char
heartcounts$Var1 <- ifelse(heartcounts$Var1 == 0, "No", 'Yes')
heartcounts$Var2 <- ifelse(heartcounts$Var2 == 0, "No", 'Yes')

# Replace headers
colnames(heartcounts)[1] <- 'Heart_Disease'
colnames(heartcounts)[2] <- 'Stroke'

# Bar Chart of Heart Disease : No vs. Yes     
ggplot(heartcounts, aes(x = Heart_Disease, y = Freq, fill = Stroke)) +
        geom_bar(stat = "identity") + 
        geom_text(aes(label = Freq), vjust = 0) +
        labs(title="Heart Disease Status of Patients",x ="Heart Disease", y = "Frequency") +
        theme(plot.title = element_text(hjust = 0.5))

# Create gender counts table
gendercounts <- as.data.frame(table(stroke_cleaned$gender))
  
# Bar Chart of Gender   
ggplot(gendercounts, aes(x = Var1, y = Freq, fill = Var1)) +
        geom_bar(stat = "identity") + theme(legend.position="none") +
        geom_text(aes(label = Freq), vjust = 0) +
        labs(title="Gender of Patients",x ="Gender", y = "Frequency") +
        theme(plot.title = element_text(hjust = 0.5))

### Findings 1

- the number of patients who have not had strokes is vastly greater than the number of patients who have. Out of 5000 patients, 249 of them (4.98%) have suffered from stroke
- the number of patients without hypertension is vastly greater than the number of patients with hypertension, but the gap is slightly less than the gap seen for stroke victims. 4.13% of patients who do not have hypertension have stroke, while 15.28% of hypertension patients have stroke
- 4.36% of patients who do not have CVD have stroke, while 20.52% of CVD patients have stroke
- there are more female patients than male patients in this dataset. 41.56% more

### Visualization 2: exploring external characteristics of the dataset such as work type, married, residence type, and smoking status

# Create ever married counts table
marriedcounts <- as.data.frame(table(stroke_cleaned$ever_married))

# Bar Chart of Patients Who Have Been Married  
ggplot(marriedcounts, aes(x = Var1, y = Freq, fill = Var1)) +
        geom_bar(stat = "identity") + theme(legend.position="none") +
        geom_text(aes(label = Freq), vjust = 20) +
        labs(title="Bar Chart of Patients Who Have Been Married",x ="Have the Patient been Married", y = "Frequency") +
        theme(plot.title = element_text(hjust = 0.5))

# Bar Chart of Heart Disease : No vs. Yes     
ggplot(stroke_cleaned, aes(x = ever_married, fill = stroke)) +
  geom_bar() +
  scale_fill_discrete(labels = c("0" = "No Stroke", "1" = "Stroke")) +
  labs(title='Bar Graph of Number of People vs Married Categorized by Presence of Stroke', x='Married', y='Count') +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  geom_text(stat='count', aes(label=after_stat(count)), position = position_stack(vjust = 0.5)) +
  theme(text = element_text(size = 15))

# Create work type counts table
workcounts <- as.data.frame(table(stroke_cleaned$work_type))
  
# Bar Chart of Patient Work Type   
ggplot(workcounts, aes(x = Var1, y = Freq, fill = Var1)) +
        geom_bar(stat = "identity") + theme(legend.position="none") +
        geom_text(aes(label = Freq), vjust = 0) +
        labs(title="Patient Work Type",x ="Work Type", y = "Frequency") +
        theme(plot.title = element_text(hjust = 0.5))

# Create residence type counts table
rescounts <- as.data.frame(table(stroke_cleaned$Residence_type))
  
# Bar Chart of Patients Who Have Been Married  
ggplot(rescounts, aes(x = Var1, y = Freq, fill = Var1)) +
        geom_bar(stat = "identity") + theme(legend.position="none") +
        geom_text(aes(label = Freq), vjust = 0) +
        labs(title="Residence Type of the Patients",x ="Residence Type", y = "Frequency") +
        theme(plot.title = element_text(hjust = 0.5))

# Create smoking status counts table
smokecounts <- as.data.frame(table(stroke_cleaned$smoking_status))
  
# Bar Chart of Patients Who Have Been Married  
ggplot(smokecounts, aes(x = Var1, y = Freq, fill = Var1)) +
        geom_bar(stat = "identity") + theme(legend.position="none") +
        geom_text(aes(label = Freq), vjust = 0) +
        labs(title="Smoking Status of Patients",x ="Smoking Status", y = "Frequency") +
        theme(plot.title = element_text(hjust = 0.5))

### Findings 2

- there are approximately even amounts of patients that are working government jobs, are self-employed, and are children. The majority of patients work for private companies, and a small number have never worked.
- roughly double the amount of patients have been married before than those who have not. out of the married patients, 7.02% have stroke. For those who are unmarried, only 1.678% have stroke. This means that marriage contributes to stroke quite significantly
- the patients are nearly evenly distributed between rural and urban residences
- most patients have either never smoked. The data for formerly and currently smokers are similar.

### Visualization 3: exploring personal variables like age, glucose levels and bmi levels among the patients

stroke_cleaned_isolate <- copy(stroke_cleaned)

stroke_cleaned_isolate <- stroke_cleaned_isolate[stroke_cleaned_isolate$stroke == 1]

summary(stroke_cleaned_isolate)

# Histogram of Age with normal distribution overlay
histage <- hist(stroke_cleaned_isolate$age,xlim=c(0,100),
                main="Histogram of Age with Normal Distribution Overlay",
                xlab="Age",las=1)
xfit <- seq(min(stroke_cleaned_isolate$age),max(stroke_cleaned_isolate$age))
yfit <- dnorm(xfit,mean=mean(stroke_cleaned_isolate$age),sd=sd(stroke_cleaned_isolate$age))
yfit <- yfit*diff(histage$mids[1:2])*length(stroke_cleaned_isolate$age)
lines(xfit,yfit,col="red",lwd=2)

# Histogram of Average Glucose Level with normal distribution overlay
histglucose <- hist(stroke_cleaned_isolate$avg_glucose_level,xlim=c(0,300),
                main="Histogram of Avg. Glucose with Normal Distribution Overlay",
                xlab="Avg. Glucose",las=1)
xfit <- seq(min(stroke_cleaned_isolate$avg_glucose_level),max(stroke_cleaned_isolate$avg_glucose_level))
yfit <- dnorm(xfit,mean=mean(stroke_cleaned_isolate$avg_glucose_level),sd=sd(stroke_cleaned_isolate$avg_glucose_level))
yfit <- yfit*diff(histglucose$mids[1:2])*length(stroke_cleaned_isolate$avg_glucose_level)
lines(xfit,yfit,col="red",lwd=2)

# Histogram of BMI with normal distribution overlay
histbmi <- hist(stroke_cleaned_isolate$bmi,xlim=c(0,100),
                main="Histogram of BMI with Normal Distribution Overlay",
                xlab="Body Mass Index",las=1)
xfit <- seq(min(stroke_cleaned_isolate$bmi),max(stroke_cleaned_isolate$bmi))
yfit <- dnorm(xfit,mean=mean(stroke_cleaned_isolate$bmi),sd=sd(stroke_cleaned_isolate$bmi))
yfit <- yfit*diff(histbmi$mids[1:2])*length(stroke_cleaned_isolate$bmi)
lines(xfit,yfit,col="red",lwd=2)

# Histogram of Age with normal distribution overlay
histage <- hist(stroke_cleaned$age,xlim=c(0,100),
                main="Histogram of Age with Normal Distribution Overlay",
                xlab="Age",las=1)
xfit <- seq(min(stroke_cleaned$age),max(stroke_cleaned$age))
yfit <- dnorm(xfit,mean=mean(stroke_cleaned$age),sd=sd(stroke_cleaned$age))
yfit <- yfit*diff(histage$mids[1:2])*length(stroke_cleaned$age)
lines(xfit,yfit,col="red",lwd=2)

# Histogram of Average Glucose Level with normal distribution overlay
histglucose <- hist(stroke_cleaned$avg_glucose_level,xlim=c(0,300),
                main="Histogram of Avg. Glucose with Normal Distribution Overlay",
                xlab="Avg. Glucose",las=1)
xfit <- seq(min(stroke_cleaned$avg_glucose_level),max(stroke_cleaned$avg_glucose_level))
yfit <- dnorm(xfit,mean=mean(stroke_cleaned$avg_glucose_level),sd=sd(stroke_cleaned$avg_glucose_level))
yfit <- yfit*diff(histglucose$mids[1:2])*length(stroke_cleaned$avg_glucose_level)
lines(xfit,yfit,col="red",lwd=2)

# Histogram of BMI with normal distribution overlay
histbmi <- hist(stroke_cleaned$bmi,xlim=c(0,100),
                main="Histogram of BMI with Normal Distribution Overlay",
                xlab="Body Mass Index",las=1)
xfit <- seq(min(stroke_cleaned$bmi),max(stroke_cleaned$bmi))
yfit <- dnorm(xfit,mean=mean(stroke_cleaned$bmi),sd=sd(stroke_cleaned$bmi))
yfit <- yfit*diff(histbmi$mids[1:2])*length(stroke_cleaned$bmi)
lines(xfit,yfit,col="red",lwd=2)

summary(stroke_cleaned)

# ### Findings 3

- the ages of the patients in the study are close to a normal distribution, with mean of 43.23 from the summary() function. Based on the information from the summary() function earlier and the chart above, most patients are around their 40s.
- the average glucose levels of the patients in the study are right skewed, with mean of 106.15 from the summary() function earlier.
- the data for patient Body Mass Index is right skewed, with a mean of 28.86 from the summary() function above after modification.
# 

head(stroke_cleaned, 5)

# factorize the necessary columns into categorical
stroke_cleaned_updated <- stroke_cleaned

stroke_cleaned_updated[, gender := factor(gender, levels=c("Male", "Female", "Other"), labels=c("1", "0", "0"))]
stroke_cleaned_updated[, ever_married:= factor(ever_married, levels=c("Yes", "No"), labels=c("1", "0"))]
stroke_cleaned_updated[, work_type := factor(work_type, levels=c("Private", "Self-employed"), labels=c("1","0"))]
stroke_cleaned_updated[, Residence_type := factor(Residence_type, levels=c("Urban", "Rural"), labels=c("1", "0"))]
stroke_cleaned_updated[, smoking_status := factor(smoking_status, levels=c("formerly smoked", "never smoked", "smokes"), labels=c("1", "0", "2"))]

head(stroke_cleaned_updated, 5)

# Convert categorical variables to numeric
stroke_cleaned_updated$gender <- as.numeric(as.character(stroke_cleaned_updated$gender))
stroke_cleaned_updated$hypertension <- as.numeric(as.character(stroke_cleaned_updated$hypertension))
stroke_cleaned_updated$heart_disease <- as.numeric(as.character(stroke_cleaned_updated$heart_disease))
stroke_cleaned_updated$ever_married <- as.numeric(as.character(stroke_cleaned_updated$ever_married))
stroke_cleaned_updated$work_type <- as.numeric(as.character(stroke_cleaned_updated$work_type))
stroke_cleaned_updated$Residence_type <- as.numeric(as.character(stroke_cleaned_updated$Residence_type))
stroke_cleaned_updated$stroke <- as.numeric(as.character(stroke_cleaned_updated$stroke))

# Calculate the correlation matrix
corr <- cor(stroke_cleaned_updated[, c("gender", "hypertension", "heart_disease","ever_married", "work_type", "Residence_type")], use = "complete.obs", method = "pearson")

# Create a correlation plot
#corrplot(corr, type = "lower", order = "hclust", tl.col = "black", addCoef.col = "black")
corrplot(corr, type = "lower", order = "hclust", tl.col = "black",
         col = colorRampPalette(c("white", "red"))(10), 
         addCoef.col = "black", number.cex = 0.8, number.offset = 0.3,
         number.digits = 2, method = "color")

# # # Split into Train, Validation and Test

stroke_cleaned <- stroke_cleaned[, lapply(.SD, function(x) if(is.character(x)) as.factor(x) else x)]

head(stroke_cleaned, 5)

set.seed(420420)
data <- stroke_cleaned
split1 <- sample.split(data$stroke, SplitRatio = 0.6)
train_data <- subset(data, split1 == TRUE)
remain_data <- subset(data, split1 == FALSE)

split2 <- sample.split(remain_data$stroke, SplitRatio = 0.5)
valid_data <- subset(remain_data, split2 == TRUE)
test_data <- subset(remain_data, split2 == FALSE)
nrow(stroke_cleaned)
nrow(train_data)
nrow(valid_data)
nrow(test_data)

# # # Perform GLM

names(stroke_cleaned)
head(stroke_cleaned,2)

# # ### Trial 1 - Use everything

#Trial 1 - Blindly using all columns to create a logistic regression
stroke.p1 <- glm(stroke ~  hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + bmi + smoking_status ,family = binomial,data = train_data )
summary(stroke.p1)
OR <- exp(coef(stroke.p1))
OR
prob <- predict(stroke.p1, type="response")
threshold <- sum(train_data$stroke==1)/length(train_data$stroke)
pass.hat <- ifelse(prob > threshold, 1, 0)
# length(train_data$stroke)
# length(pass.hat)
# length(prob)
# head(pass.hat, 10)
table(train_data$stroke,pass.hat)
mean(pass.hat == train_data$stroke)

prob_valid <- predict(stroke.p1, newdata = valid_data, type = "response")
threshold <- sum(valid_data$stroke==1)/length(valid_data$stroke)
pass.hat <- ifelse(prob_valid > threshold, 1, 0)
table(valid_data$stroke, pass.hat)
mean(pass.hat == valid_data$stroke)

# # ## Trial 2 - Use Hypertension, heart disease, glucose, and bmi

#Trial 2 - Restrict to just hypertension, heart disease, glucose, and bmi
stroke.p2 <- glm(stroke ~ hypertension + heart_disease + avg_glucose_level + bmi,family = binomial,data = train_data)
summary(stroke.p2)
prob <- predict(stroke.p2, type='response')
threshold <- sum(train_data$stroke==1)/length(train_data$stroke)
pass.hat2 <- ifelse(prob > threshold, 1, 0)
table(train_data$stroke,pass.hat2)
mean(pass.hat2 == train_data$stroke)

prob_valid <- predict(stroke.p2, newdata = valid_data, type = "response")
threshold <- sum(valid_data$stroke==1)/length(valid_data$stroke)
pass.hat <- ifelse(prob_valid > threshold, 1, 0)
table(valid_data$stroke, pass.hat)
mean(pass.hat == valid_data$stroke)

# # ## Trial 3 - Brute force to find the best model

# "years", "gender", "height", "weight", "ap_hi", "ap_lo", "cholesterol", "gluc", "smoke", "alco", "active"
predictors <- c("gender", "hypertension", "heart_disease", "avg_glucose_level", "bmi","smoking_status", "ever_married", "work_type", "Residence_type")
aic_model <- glm(stroke ~ ., data = train_data[,c(predictors,"stroke"), with=FALSE], family = binomial)

best_model <- stepAIC(aic_model, direction = "both", scope = list(lower = ~ 1, upper = ~ .), trace = FALSE)
summary(best_model)

stroke.p3 <- best_model
prob <- predict(stroke.p3, type='response')
threshold <- sum(train_data$stroke==1)/length(train_data$stroke)
pass.hat2 <- ifelse(prob > threshold, 1, 0)
table(train_data$stroke,pass.hat2)
mean(pass.hat2 == train_data$stroke)

prob_valid <- predict(stroke.p3, newdata = valid_data, type = "response")
threshold <- sum(valid_data$stroke==1)/length(valid_data$stroke)
pass.hat <- ifelse(prob_valid > threshold, 1, 0)
table(valid_data$stroke, pass.hat)
mean(pass.hat == valid_data$stroke)

# # ## Test final accuracy using test set

#Test final accuracy based on test set
#For stroke_cleaned.p1
prob_test <- predict(stroke.p1, newdata = test_data, type = "response")
threshold <- sum(test_data$stroke==1)/length(test_data$stroke)
pass.hat <- ifelse(prob_test > threshold, 1, 0)
table(test_data$stroke, pass.hat)
acc.p1 <-mean(pass.hat == test_data$stroke)
acc.p1

#For stroke.p2
prob_test <- predict(stroke.p2, newdata = test_data, type = "response")
threshold <- sum(test_data$stroke==1)/length(test_data$stroke)
pass.hat <- ifelse(prob_test > threshold, 1, 0)
table(test_data$stroke, pass.hat)
acc.p2 <- mean(pass.hat == test_data$stroke)
acc.p2

#For stroke.p3
prob_test <- predict(stroke.p3, newdata = test_data, type = "response")
threshold <- sum(test_data$stroke==1)/length(test_data$stroke)
pass.hat <- ifelse(prob_test > threshold, 1, 0)
table(test_data$stroke, pass.hat)
acc.p3 <- mean(pass.hat == test_data$stroke)
acc.p3

# # Perform Logistic Regression

## Trial 1 - Use everything
# 

stroke.p4 <- multinom(stroke ~ hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + bmi + smoking_status , data = train_data)
summary(stroke.p4)
OR <- exp(coef(stroke.p4))
z <- summary(stroke.p4)$coefficients/summary(stroke.p4)$standard.errors
pvalue <- (1-pnorm(abs(z),0,1))*2
prob <- predict(stroke.p4, type='prob')
predicted_class <- predict(stroke.p4)
table(train_data$stroke, predicted_class)
mean(predicted_class == train_data$stroke)

prob_valid <- predict(stroke.p4, newdata = valid_data, type = "prob")
predicted_class <- predict(stroke.p4, newdata = valid_data)
table(valid_data$stroke, predicted_class)
mean(predicted_class == valid_data$stroke)

# # ## Trial 2 - Use hypertension, heart disease, glucose, and bmi

stroke.p6 <- multinom(stroke ~ hypertension + heart_disease + avg_glucose_level + bmi, data = train_data)
summary(stroke.p6)
OR <- exp(coef(stroke.p6))
z <- summary(stroke.p6)$coefficients/summary(stroke.p6)$standard.errors
pvalue <- (1-pnorm(abs(z),0,1))*2
prob <- predict(stroke.p6, type='prob')
predicted_class <- predict(stroke.p6)
table(train_data$stroke, predicted_class)
mean(predicted_class == train_data$stroke)

prob_valid <- predict(stroke.p6, newdata = valid_data, type = "prob")
predicted_class <- predict(stroke.p6, newdata = valid_data)
table(valid_data$stroke, predicted_class)
mean(predicted_class == valid_data$stroke)

# # ## Brute force to find the best model

# "years", "gender", "height", "weight", "ap_hi", "ap_lo", "cholesterol", "gluc", "smoke", "alco", "active"
predictors <- c("gender", "hypertension", "heart_disease", "avg_glucose_level", "bmi","smoking_status", "ever_married", "work_type", "Residence_type")
aic_model <- multinom(stroke ~ ., data = train_data[,c(predictors,"stroke"), with=FALSE])

best_model <- stepAIC(aic_model, direction = "both", scope = list(lower = ~ 1, upper = ~ .), trace = FALSE)
summary(best_model)

stroke.p5 <- best_model
OR <- exp(coef(stroke.p5))
z <- summary(stroke.p5)$coefficients/summary(stroke.p5)$standard.errors
pvalue <- (1-pnorm(abs(z),0,1))*2
prob <- predict(stroke.p5, type='prob')
predicted_class <- predict(stroke.p5)
table(train_data$stroke, predicted_class)
mean(predicted_class == train_data$stroke)

prob_valid <- predict(stroke.p5, newdata = valid_data, type = "prob")
predicted_class <- predict(stroke.p5, newdata = valid_data)
table(valid_data$stroke, predicted_class)
mean(predicted_class == valid_data$stroke)

# # ## Test final accuracy using test set

#Test final accuracy based on test set
#For stroke.p4
prob_test <- predict(stroke.p4, newdata = test_data, type = "prob")
predicted_class <- predict(stroke.p4, newdata = test_data)
table(test_data$stroke, predicted_class)
acc.p4 <- mean(predicted_class == test_data$stroke)
acc.p4

#For stroke.p6
prob_test <- predict(stroke.p6, newdata = test_data, type = "prob")
predicted_class <- predict(stroke.p6, newdata = test_data)
table(test_data$stroke, predicted_class)
acc.p6 <- mean(predicted_class == test_data$stroke)
acc.p6

#For stroke.p5
prob_test <- predict(stroke.p5, newdata = test_data, type = "prob")
predicted_class <- predict(stroke.p5, newdata = test_data)
table(test_data$stroke, predicted_class)
acc.p5 <- mean(predicted_class == test_data$stroke)
acc.p5

# Exportoooooooor
# head(stroke_cleaned,2)
# write.csv(stroke_cleaned, "stroke.csv")

