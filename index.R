library(RCurl)
data <- read.csv("https://raw.githubusercontent.com/dat-analytics/data_assess_1_t2_2022/main/z5364258.csv",encoding ="UTF-8")


# Significant Variables
age <- data[, 1]
education <- data[, 5]
jobLevel <- data[, 9]
jobRotation <- data[, 10]
monthlyIncome <- data[, 14]

overTime <- data[, 17] 
totalWorkingYears <- data[, 21]

yearsAtCompany <- data[, 25]
yearsInCurrentRole <- data[, 26]

performance <- data[, 28]


# Poor Correlation
cor(performance, yearsAtCompany)
cor(performance, jobRotation)
cor(performance, yearsInCurrentRole)
cor(performance, totalWorkingYears)
cor(performance, performance)
cor(jobLevel, performance)


# Strong Correlation
cor(jobLevel, monthlyIncome)

# Box Plot (Training & Performance) --> Good One to Use
input <- data[,c('Training','Performance')]
boxplot( Performance ~ Training, data = data, xlab = "Corporate Training Received",
         ylab = "Individual Performance", main = "Training Correlation With Performance")




