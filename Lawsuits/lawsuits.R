#** RE6013 Lawsuit Homework**

## **Background** ##
# A few years ago, the United States District Court of Houston had a case that arises under Title VII of the Civil Rights Act of 1964, 42 U.S.C. 200e et seq. The plaintiffs in this case were all female doctors at Houston College of Medicine who claimed that the College has engaged in a pattern and practice of discrimination against women in giving promotions and setting salaries. The Lead plaintiff in this action, a pediatrician and an assistant professor, was denied for promotion at the College. The plaintiffs had presented a set of data to show that **female faculty at the school were less likely to be full professors**, **more likely to be assistant professors**, and **earn less money than men**, on average.


# ## **Details** ##
# Who are we: analytics consultant for the female doctors

# Target audience: senior management, court judge and jury (basically non analytics expert/programmer)

# to check the version of R
R.version.string

# Install packages IF NEEDED
install.packages("ggplots2", repos = "https://cran.asia/", dependencies = TRUE)
install.packages("data.table", repos = "https://cran.asia/", dependencies = TRUE)

# Import necessary libraries
library(data.table)
library(ggplot2)

library(grid)

# set working directory to current
setwd("./")

# Import csv
url <- "https://raw.githubusercontent.com/ainthowry/Statistical-Modelling-Analysis-R/main/Lawsuits/Lawsuit.csv"
lawsuit.dt <- fread(url)
sprintf("Data downloaded from %s!", url)
head(lawsuit.dt, 2)

# process the data
lawsuit.dt$Gender <- factor(lawsuit.dt$Gender)
lawsuit.dt$Rank <- factor(lawsuit.dt$Rank)
lawsuit.dt$Dept <- factor(lawsuit.dt$Dept)
lawsuit.dt$Clin <- factor(lawsuit.dt$Clin)
lawsuit.dt$Cert <- factor(lawsuit.dt$Cert)
head(lawsuit.dt, 5)

# add new datas
# add data for pay_raise
lawsuit.dt[, pay_raise := Sal95 - Sal94]
head(lawsuit.dt, 5)

# Split if necessary
# female data.table -> gender == 0
female.dt <- lawsuit.dt[Gender == 0]
# male data.table -> gender == 1
male.dt <- lawsuit.dt[Gender == 1]

# Get summary of lawsuit.dt
summary(lawsuit.dt)

# # **Overview**

# count number of males vs females at rank level
ggplot(data = lawsuit.dt, aes(Rank, fill = Gender)) +
  geom_bar() +
  scale_fill_discrete(labels = c("0" = "Female", "1" = "Male")) +
  scale_x_discrete(labels = c("1" = "Asistant", "2" = "Asociate", "3" = "Prof")) +
  labs(title = "Proportion of Gender Holding Each Rank") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5))

# # # **Argument 1: Female faculty earns less money than the male faculty: across all ranks, and across all salary ranges**

# create a copy of the lawsuit datatable and segment the salary column into ranges starting from 0 to 500k, in blocks of 50k
lawsuit_edit.dt <- copy(lawsuit.dt)
lawsuit_edit.dt$Sal94 <- cut(lawsuit_edit.dt$Sal94, breaks = seq(0, 500000, 50000))

# plot proportion of gender sorted by salary ranges
ggplot(lawsuit_edit.dt, aes(fill = Gender, x = Sal94)) +
  scale_fill_discrete(labels = c("Female", "Male")) +
  labs(title = "Proportion of Gender sorted by Salary Ranges") +
  geom_bar(width = 0.9) +
  scale_x_discrete(labels = c("<50k", "50k-100k", "100k-150k", "150k-200k", "200k-250k", "250k-300k", "300k-350k", "350k-400k", ">400k")) +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5))

# create a copy of the lawsuit datatable and segment the salary column into ranges starting from 0 to 500k, in blocks of 50k
lawsuit_edit.dt <- copy(lawsuit.dt)
lawsuit_edit.dt$Sal95 <- cut(lawsuit_edit.dt$Sal95, breaks = seq(0, 500000, 50000))

# plot proportion of gender sorted by salary ranges
ggplot(lawsuit_edit.dt, aes(fill = Gender, x = Sal95)) +
  scale_fill_discrete(labels = c("Female", "Male")) +
  labs(title = "Proportion of Gender sorted by Salary Ranges") +
  geom_bar(width = 0.9) +
  scale_x_discrete(labels = c("<50k", "50k-100k", "100k-150k", "150k-200k", "200k-250k", "250k-300k", "300k-350k", "350k-400k", "400k-450k", ">450k")) +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5))

# plot proportion of gender for each rank within researchers
researchers.dt <- lawsuit.dt[Clin == "0", ]
ggplot(data = researchers.dt, aes(Rank, fill = Gender)) +
  geom_bar() +
  scale_fill_discrete(labels = c("0" = "Female", "1" = "Male")) +
  scale_x_discrete(labels = c("1" = "Assistant", "2" = "Associate", "3" = "Prof")) +
  labs(title = "Proportion of Gender For Each Rank within Researchers") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5))

# # ## Prate + Research emphasis vs Rank

ggplot(data = researchers.dt, aes(x = Rank, y = Prate, fill = Gender)) +
  geom_boxplot() +
  scale_fill_discrete(labels = c("0" = "Female", "1" = "Male")) +
  scale_x_discrete(labels = c("1" = "Assistant", "2" = "Associate", "3" = "Prof")) +
  labs(title = "Publication rate vs Rank within Researchers")

custom.dt <- researchers.dt[(Rank == "1" & Gender == "0") | (Rank == "2" & Gender == "1"), ]
ggplot(data = custom.dt, aes(x = Prate, y = Exper, shape = Gender, color = Gender)) +
  geom_point() +
  scale_fill_discrete(labels = c("0" = "Female", "1" = "Male")) +
  labs(title = "Experience vs Publication rate for Male Associates and Female Assistants")

# Boxplot rank vs salary94 according to gender
ggplot(data = lawsuit.dt, aes(x = Rank, y = Sal94, fill = Gender)) +
  geom_boxplot(position = position_dodge(1)) +
  labs(title = "Box Plot of Profession vs Salary94") +
  scale_fill_discrete(labels = c("0" = "Female", "1" = "Male")) +
  scale_x_discrete(labels = c("1" = "Asistant", "2" = "Asociate", "3" = "Prof"))

# Boxplot rank vs salary95 according to gender
ggplot(data = lawsuit.dt, aes(x = Rank, y = Sal95, fill = Gender)) +
  geom_boxplot(position = position_dodge(1)) +
  labs(title = "Box Plot of Profession vs Salary95") +
  scale_fill_discrete(labels = c("0" = "Female", "1" = "Male")) +
  scale_x_discrete(labels = c("1" = "Asistant", "2" = "Asociate", "3" = "Prof"))

# **Argument 2: Not only does female faculty earn less, a larger percentage of women also enjoy less pay raise than men! 😠**

# Distribution plot of pay raise
ggplot(data = lawsuit.dt[order(pay_raise)], aes(x = pay_raise, fill = Gender)) +
  geom_histogram(aes(y = after_stat(density))) +
  labs(title = "Distribution of Pay Raise", x = "Pay Raise", y = "Density") +
  # geom_density(alpha = .2) +
  scale_fill_discrete(labels = c("0" = "Female", "1" = "Male"))

# mean pay increase for males is higher than females

ggplot(data = lawsuit.dt, aes(x = Gender, y = pay_raise, fill = Gender)) +
  geom_boxplot() +
  geom_boxplot(position = position_dodge(1)) +
  labs(title = "Pay Increments by Gender") +
  scale_fill_discrete(labels = c("0" = "Female", "1" = "Male")) +
  scale_x_discrete(labels = c("1" = "Male", "0" = "Female"))

mean_female_pay94 <- mean(lawsuit.dt[Gender == 0, Sal94])
mean_female_pay95 <- mean(lawsuit.dt[Gender == 0, Sal95])
mean_male_pay94 <- mean(lawsuit.dt[Gender == 1, Sal94])
mean_male_pay95 <- mean(lawsuit.dt[Gender == 1, Sal95])

# Calculate how much each person is above the mean of their gender
lawsuit.dt[, pay_alpha94 := ifelse(Gender == 0, Sal94 - mean_female_pay94, Sal94 - mean_male_pay94)]
lawsuit.dt[, pay_alpha95 := ifelse(Gender == 0, Sal95 - mean_female_pay95, Sal94 - mean_male_pay95)]
head(lawsuit.dt, 10)

# find correlation between being more capable and their pay alpha
lawsuit_numeric.dt <- sapply(lawsuit.dt, as.numeric)
cor(lawsuit_numeric.dt, use = "complete.obs")

# # **Further Data Exploration**

## Clinical emphasis vs rank
#

ggplot(data = lawsuit.dt, aes(Rank, fill = Clin)) +
  geom_bar() +
  scale_fill_discrete(labels = c("0" = "Clinical Emphasis", "1" = "Research Emphasis")) +
  scale_x_discrete(labels = c("1" = "Assistant", "2" = "Associate", "3" = "Prof")) +
  labs(title = "Proportion of Clinical Emphasis For Each Rank") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5))

# # ## Department vs rank

ggplot(data = lawsuit.dt, aes(Rank, fill = Dept)) +
  geom_bar() +
  scale_fill_discrete(labels = c("1" = "Biochemistry/ Molecular Biology", "2" = "Physiology", "3" = "Genetics", "4" = "Pediatrics", "5" = "Medicine", "6" = "Surgery")) +
  scale_x_discrete(labels = c("1" = "Assistant", "2" = "Associate", "3" = "Prof")) +
  labs(title = "Proportion of Departments For Each Rank") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5))

# summarize within each rank statistics
rank1.dt <- lawsuit.dt[Rank == "1", ]
rank2.dt <- lawsuit.dt[Rank == "2", ]
rank3.dt <- lawsuit.dt[Rank == "3", ]

# percentage of Assistant that belong to each Department
table(rank1.dt$Dept) / nrow(rank1.dt) * 100

# percentage of Associates that belong to each Department
table(rank2.dt$Dept) / nrow(rank2.dt) * 100

# percentage of Profs that belong to each Department
table(rank3.dt$Dept) / nrow(rank3.dt) * 100

depts.dt <- lawsuit.dt[Dept == "1" | Dept == "2" | Dept == "4", ]

ggplot(data = lawsuit.dt, aes(Rank, fill = Gender)) +
  geom_bar() +
  facet_grid(. ~ Dept) +
  scale_fill_discrete(labels = c("0" = "Female", "1" = "Male")) +
  scale_x_discrete(labels = c("1" = "Assistant", "2" = "Associate", "3" = "Prof")) +
  labs(title = "Proportion of Gender For Each Rank within Departments") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5))

# # ## Dept vs Pay

dept6.dt <- lawsuit.dt[Dept == "6", ]
dept6.dt$Sal95 <- cut(dept6.dt$Sal95, breaks = seq(0, 500000, 50000))
# plot proportion of gender sorted by salary ranges
ggplot(dept6.dt, aes(fill = Gender, x = Sal95)) +
  scale_fill_discrete(labels = c("Female", "Male")) +
  labs(title = "Proportion of Gender sorted by Salary Ranges for Surgery Dept") +
  geom_bar(width = 0.9) +
  theme(panel.spacing = unit(2, "lines")) +
  scale_x_discrete(labels = c("<50k", "50k-100k", "100k-150k", "150k-200k", "200k-250k", "250k-300k", "300k-350k", "350k-400k", "400k-450k", ">450k")) +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5))

dept6_prof.dt <- dept6.dt[Rank == "3", ]
dept6_prof.dt

summary(dept6_prof.dt)

researchers.dt$Sal95 <- cut(researchers.dt$Sal95, breaks = seq(0, 500000, 50000))

ggplot(researchers.dt, aes(fill = Gender, x = Sal95)) +
  scale_fill_discrete(labels = c("Female", "Male")) +
  labs(title = "Histogram of Salary Ranges for Researchers") +
  geom_bar(width = 0.9) +
  theme(panel.spacing = unit(2, "lines")) +
  scale_x_discrete(labels = c("<50k", "50k-100k", "100k-150k", "150k-200k", "200k-250k", "250k-300k", "300k-350k", "350k-400k", "400k-450k", ">450k")) +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5))

summary(researchers.dt)

researchers.dt[Rank == "3" & Gender == "0", ]

researchers.dt[Rank == "3" & Dept == "5", ]

researchers.dt[Rank == "3" & Prate >= 6.7, ]
