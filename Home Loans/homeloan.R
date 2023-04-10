R.version.string

# install.packages("ggplots2", repos = "https://cran.asia/", dependencies = TRUE)
# install.packages("data.table", repos = "https://cran.asia/", dependencies = TRUE)
# install.packages("corrplot", repos = "https://cran.asia/", dependencies = TRUE)
# install.packages("caTools", repos = "https://cran.asia/", dependencies = TRUE)
# install.packages("MASS", repos = "https://cran.asia/", dependencies = TRUE)

library(data.table)
library(ggplot2)
library(corrplot)
library(caTools)
library(MASS)
library(nnet)
library(rpart)
library(rpart.plot)
library(tidyr)

setwd("./")

homeloans.dt <- fread("homeloan3.csv", na.strings = c("NA", "missing", "N/A", -99, "", "m", "M", "na", "."))

# Get summary of dataset
summary(homeloans.dt)
names(homeloans.dt)
head(homeloans.dt, 2)

# Find out NA values in dataset
print("NA Values in homeloan.dt")
sum(is.na(homeloans.dt))
rows_with_na <- apply(is.na(homeloans.dt), 1, any)
homeloans.dt[rows_with_na, c(names(homeloans.dt)[apply(is.na(homeloans.dt), 2, any)])]
nrow(homeloans.dt)

# Clean the dataset of na values
print("NA Values in cleaned homeloan.dt")
homeloans_clean.dt <- homeloans.dt

# Clean Gender
homeloans_clean.dt[is.na(Gender), gender := "non-gender"]
homeloans_clean.dt[Gender == "Male", gender := "male"]
homeloans_clean.dt[Gender == "Female", gender := "female"]
homeloans_clean.dt$gender <- factor(homeloans_clean.dt$gender, levels = c("non-gender", "male", "female"))

# Clean Married, Dependents, Self_Employed, Loan_Amount_Term, Credit_Score
homeloans_clean.dt[is.na(Married), married := "missing"]
homeloans_clean.dt[Married == "Yes", married := "yes"]
homeloans_clean.dt[Married == "No", married := "no"]
homeloans_clean.dt$married <- factor(homeloans_clean.dt$married, levels = c("missing", "yes", "no"))

homeloans_clean.dt$dependents <- homeloans_clean.dt$Dependents
homeloans_clean.dt[is.na(Dependents), dependents := "missing"]
homeloans_clean.dt$dependents <- factor(homeloans_clean.dt$dependents)

homeloans_clean.dt[is.na(Self_Employed), self_employed := "missing"]
homeloans_clean.dt[Self_Employed == "Yes", self_employed := "yes"]
homeloans_clean.dt[Self_Employed == "No", self_employed := "no"]
homeloans_clean.dt$self_employed <- factor(homeloans_clean.dt$self_employed, levels = c("missing", "yes", "no"))

mean_term <- as.integer(mean(homeloans_clean.dt$Loan_Amount_Term, na.rm = TRUE))
homeloans_clean.dt[is.na(Loan_Amount_Term), is_term := "missing"]
homeloans_clean.dt[!is.na(Loan_Amount_Term), is_term := "not missing"]
homeloans_clean.dt$is_term <- factor(homeloans_clean.dt$is_term, levels = c("missing", "not missing"))
homeloans_clean.dt[is.na(Loan_Amount_Term), Loan_Amount_Term := mean_term]

homeloans_clean.dt[is.na(Credit_Score), credit_score := "missing"]
homeloans_clean.dt[Credit_Score == 1, credit_score := "good"]
homeloans_clean.dt[Credit_Score == 0, credit_score := "bad"]
homeloans_clean.dt$credit_score <- factor(homeloans_clean.dt$credit_score, levels = c("missing", "bad", "good"))

# Convert some columns to factor
homeloans_clean.dt$education <- as.factor(homeloans_clean.dt$Education)
homeloans_clean.dt$property_area <- as.factor(homeloans_clean.dt$Property_Area)
homeloans_clean.dt$Loan_Status <- as.factor(homeloans_clean.dt$Loan_Status)

# homeloans_clean.dt <- homeloans_clean.dt[, lapply(.SD, function(x) if (is.character(x)) as.factor(x) else x)]
# homeloans_clean.dt <- na.omit(homeloans.dt)
sum(is.na(homeloans_clean.dt$Loan_Amount_Term))
nrow(homeloans_clean.dt)
str(homeloans_clean.dt)


# Find the number of unique categorical values of each column
print("Unique Values in each column")
unique(homeloans_clean.dt$gender)
unique(homeloans_clean.dt$married)
unique(homeloans_clean.dt$dependents)
unique(homeloans_clean.dt$education)
unique(homeloans_clean.dt$self_employed)
unique(homeloans_clean.dt$is_term)
unique(homeloans_clean.dt$credit_score)
unique(homeloans_clean.dt$property_area)

# Visualization 1: Find out the correlation between features to filter out the most important variables
# Convert categorical variables to numeric
numeric_homeloans.dt <- homeloans_clean.dt[, lapply(.SD, function(x) if (is.factor(x)) as.numeric(x) else x)]

# Calculate correlation matrix & create correlation plot
corr <- cor(numeric_homeloans.dt[, c("gender", "married", "dependents", "education", "self_employed", "ApplicantIncome", "CoapplicantIncome", "LoanAmount", "Loan_Amount_Term", "credit_score", "property_area", "Loan_Status")], use = "complete.obs", method = "pearson")
corrplot(corr,
    type = "lower", order = "hclust", tl.col = "black",
    col = colorRampPalette(c("white", "red"))(10),
    addCoef.col = "black", number.cex = 0.8,
    number.digits = 2, method = "color"
)

# Findings 1:
# - It seems like amongst all the features, the most important features seems to be
# - Credit Score, Coapplicant Income, Marriage Status

# Visualization 2: Explore the correlation between the important variables (categorical)

# see number of good and bad credit score in the dataset
ggplot(data = homeloans_clean.dt, aes(x = credit_score, fill = Loan_Status)) +
    geom_bar() +
    scale_fill_discrete(labels = c("N" = "Not Approved", "Y" = "Approved")) +
    labs(title = "Credit Score Categorized by Approval of Loan", x = "Credit Score", y = "Count") +
    scale_x_discrete(labels = c("missing" = "Missing", "good" = "Good", "bad" = "Bad")) +
    geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5)) +
    theme(text = element_text(size = 15))

ggplot(data = homeloans_clean.dt, aes(x = credit_score, fill = Loan_Status)) +
    geom_bar() +
    scale_fill_discrete(labels = c("N" = "Not Approved", "Y" = "Approved")) +
    labs(title = "Credit Score Categorized by Approval of Loan & Gender", x = "Credit Score", y = "Count") +
    scale_x_discrete(labels = c("missing" = "Missing", "good" = "Good", "bad" = "Bad")) +
    geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5)) +
    facet_grid(. ~ gender) +
    theme(text = element_text(size = 15))

# see number of male and female loan in the dataset
ggplot(data = homeloans_clean.dt, aes(x = gender, fill = Loan_Status)) +
    geom_bar() +
    scale_fill_discrete(labels = c("N" = "Not Approved", "Y" = "Approved")) +
    labs(title = "Gender Categorized by Approval of Loan", x = "Gender", y = "Count") +
    scale_x_discrete(labels = c("male" = "Male", "female" = "Female", "non-gender" = "Non-Gender")) +
    geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5)) +
    theme(text = element_text(size = 15))

# see loan amount term vs loan status
histloanterm <- hist(homeloans_clean.dt$Loan_Amount_Term,
    main = "Histogram of Loan Term with Normal Distribution Overlay",
    xlab = "Loan Amount Term", las = 1,
    ylim = c(0, 550),
)
xfit <- seq(min(homeloans_clean.dt$Loan_Amount_Term), max(homeloans_clean.dt$Loan_Amount_Term))
yfit <- dnorm(xfit, mean = mean(homeloans_clean.dt$Loan_Amount_Term), sd = sd(homeloans_clean.dt$Loan_Amount_Term))
yfit <- yfit * diff(histloanterm$mids[1:2]) * length(homeloans_clean.dt$Loan_Amount_Term)
lines(xfit, yfit, col = "red", lwd = 2)

ggplot(data = homeloans_clean.dt, aes(x = Loan_Amount_Term, fill = Loan_Status)) +
    geom_bar(position = "dodge") +
    scale_fill_discrete(labels = c("N" = "Not Approved", "Y" = "Approved")) +
    labs(title = "Loan Amount Term Categorized by Approval of Loan", x = "Loan Amount Term", y = "Count") +
    theme(text = element_text(size = 15))

# see is_term vs loan status
ggplot(data = homeloans_clean.dt, aes(x = is_term, fill = Loan_Status)) +
    geom_bar() +
    scale_fill_discrete(labels = c("N" = "Not Approved", "Y" = "Approved")) +
    labs(title = "Term filled Categorized by Approval of Loan", x = "Is Term Filled?", y = "Count") +
    scale_x_discrete(labels = c("missing" = "missing", "not missing" = "Not missing")) +
    geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5)) +
    theme(text = element_text(size = 15))

# see applicant income vs loan status
ggplot(data = homeloans_clean.dt[order(ApplicantIncome)], aes(x = ApplicantIncome, fill = gender)) +
    geom_histogram(aes(y = after_stat(count))) +
    labs(title = "Distribution of Applicant Income", x = "Applicant Income", y = "Count") +
    geom_density(alpha = .2) +
    scale_fill_discrete(labels = c("female" = "Female", "male" = "Male"))

# see loan status vs applicant income and coapplicant income
ggplot(data = homeloans_clean.dt, aes(x = Loan_Status, y = ApplicantIncome)) +
    geom_boxplot(position = position_dodge(1)) +
    labs(title = "Loan_Status vs Applicant Income") +
    scale_x_discrete(labels = c("N" = "Not Approved", "Y" = "Approved")) +
    theme(aspect.ratio = 1.5)

ggplot(data = homeloans_clean.dt, aes(x = Loan_Status, y = CoapplicantIncome)) +
    geom_boxplot(position = position_dodge(1)) +
    labs(title = "Loan_Status vs Coapplicant Income") +
    scale_x_discrete(labels = c("N" = "Not Approved", "Y" = "Approved")) +
    theme(aspect.ratio = 1.5)




# Split dataset
set.seed(420420)
data <- homeloans_clean.dt
split <- sample.split(data$Loan_Status, SplitRatio = 0.6)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)
nrow(train_data)
nrow(test_data)

names(homeloans_clean.dt)

# Logistic regreession
# Trial 1 - Blindly using all columns to create a logistic regression
loans.glm.p1 <- multinom(Loan_Status ~ gender + married + dependents + self_employed + is_term + credit_score + education + property_area + Loan_Amount_Term + ApplicantIncome + CoapplicantIncome, data = train_data)
summary(loans.glm.p1)
OR <- exp(coef(loans.glm.p1))
z <- summary(loans.glm.p1)$coefficients / summary(loans.glm.p1)$standard.errors
pvalue <- (1 - pnorm(abs(z), 0, 1)) * 2
prob <- predict(loans.glm.p1, type = "prob")
predicted_class <- predict(loans.glm.p1)
table(train_data$Loan_Status, predicted_class)
mean(predicted_class == train_data$Loan_Status)

# Trial 2 - Brute force to find best model
predictors <- c("gender", "married", "dependents", "self_employed", "is_term", "credit_score", "education", "property_area", "Loan_Amount_Term", "ApplicantIncome", "CoapplicantIncome")
aic_model <- multinom(Loan_Status ~ ., data = train_data[, c(predictors, "Loan_Status"), with = FALSE])

best_model <- stepAIC(aic_model, direction = "both", scope = list(lower = ~1, upper = ~.), trace = FALSE)
summary(best_model)

loans.glm.p2 <- best_model
OR <- exp(coef(loans.glm.p2))
z <- summary(loans.glm.p2)$coefficients / summary(loans.glm.p2)$standard.errors
pvalue <- (1 - pnorm(abs(z), 0, 1)) * 2
prob <- predict(loans.glm.p2, type = "prob")
predicted_class <- predict(loans.glm.p2)
table(train_data$Loan_Status, predicted_class)
mean(predicted_class == train_data$Loan_Status)

# Trial 3 - Select the brute forced features + some additional features
loans.glm.p3 <- multinom(Loan_Status ~ gender + married + dependents + self_employed + is_term + credit_score + education + property_area + Loan_Amount_Term + ApplicantIncome + CoapplicantIncome, data = train_data)
summary(loans.glm.p3)
OR <- exp(coef(loans.glm.p3))
z <- summary(loans.glm.p3)$coefficients / summary(loans.glm.p3)$standard.errors
pvalue <- (1 - pnorm(abs(z), 0, 1)) * 2
prob <- predict(loans.glm.p3, type = "prob")
predicted_class <- predict(loans.glm.p3)
table(train_data$Loan_Status, predicted_class)
mean(predicted_class == train_data$Loan_Status)

# Test final accuracy using test set
print("final acc")
# For loans.glm.p1
prob_test <- predict(loans.glm.p1, newdata = test_data, type = "prob")
predicted_class <- predict(loans.glm.p1, newdata = test_data)
table(test_data$Loan_Status, predicted_class)
acc.p1 <- mean(predicted_class == test_data$Loan_Status)
acc.p1

# For loans.glm.p2
prob_test <- predict(loans.glm.p2, newdata = test_data, type = "prob")
predicted_class <- predict(loans.glm.p2, newdata = test_data)
table(test_data$Loan_Status, predicted_class)
acc.p2 <- mean(predicted_class == test_data$Loan_Status)
acc.p2

# For loans.glm.p3
prob_test <- predict(loans.glm.p3, newdata = test_data, type = "prob")
predicted_class <- predict(loans.glm.p3, newdata = test_data)
table(test_data$Loan_Status, predicted_class)
acc.p3 <- mean(predicted_class == test_data$Loan_Status)
acc.p3

# Model conclusion
# The highest accuracy for GLM seem to be around 83.97% where we selectively choose features via stepwise AIC such as gender, marriage, credit_score, property_area and income

# CART
# Trial 1 -

set.seed(420024)

loans.cart.p1 <- rpart(Loan_Status ~ gender + married + dependents + self_employed + is_term + credit_score + education + property_area + Loan_Amount_Term + ApplicantIncome + CoapplicantIncome, data = train_data, method = "class", control = rpart.control(minsplit = 2, cp = 0))
rpart.plot(loans.cart.p1, nn = T, main = "Maximal Tree in homeloan3.csv")
printcp(loans.cart.p1)
plotcp(loans.cart.p1, main = "Subtrees in homeloan3.csv")

# Let's pick cp of 0.0106809 giving the lowest xerror
cp2 <- 0.011
loans.cart.p2 <- prune(loans.cart.p1, cp = cp2)
printcp(loans.cart.p2)
rpart.plot(loans.cart.p2, nn = T, main = "Pruned Tree with cp = 0.11")

# Calculate the optimal cp
# Compute min CVerror + 1SE in maximal tree loans.cart.p1.
CVerror.cap <- loans.cart.p1$cptable[which.min(loans.cart.p1$cptable[, "xerror"]), "xerror"] + loans.cart.p1$cptable[which.min(loans.cart.p1$cptable[, "xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1
j <- 4
while (loans.cart.p1$cptable[i, j] > CVerror.cap) {
    i <- i + 1
}
cp.opt <- ifelse(i > 1, sqrt(loans.cart.p1$cptable[i, 1] * loans.cart.p1$cptable[i - 1, 1]), 1)
cp.opt

loans.cart.p3 <- prune(loans.cart.p1, cp = cp.opt)
printcp(loans.cart.p2)
rpart.plot(loans.cart.p3, nn = T, main = paste("Pruned Tree with cp =", cp.opt))

# Test final accuracy for test set
# For loans.cart.p1
predict_test <- predict(loans.cart.p1, newdata = test_data, type = "class")
confusion_matrix <- table(predict_test, test_data$Loan_Status)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
confusion_matrix
accuracy

# For loans.cart.p2
predict_test <- predict(loans.cart.p2, newdata = test_data, type = "class")
confusion_matrix <- table(predict_test, test_data$Loan_Status)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
confusion_matrix
accuracy

# For loans.cart.p3
predict_test <- predict(loans.cart.p3, newdata = test_data, type = "class")
confusion_matrix <- table(predict_test, test_data$Loan_Status)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
confusion_matrix
accuracy
