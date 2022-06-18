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



#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\      Box Plots       \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Box Plot (Training & Performance) --> Good One to Use
input <- data[,c('Training','Performance')]
boxplot( Performance ~ Training, data = data, xlab = "Corporate Training Received",
         ylab = "Individual Performance", main = "Training Correlation With Performance")


# Box Plot (Environment Satisfaction & Gender)
input <- data[,c('EnvironmentSatisfaction','Gender')]
boxplot( EnvironmentSatisfaction ~ Gender, data = data, xlab = "Gender",
         ylab = "Environment Satisfaction", main = "Gendered Workplace Satisfaction")

# Can link this correlation with the monthly rate pne on performance and 
# say how monetary incentives dont correlate well with performance rather the focus should be on training etc
# Box Plot (PercentSalaryHike & Performance) --> Good One to Use
input <- data[,c('Performance','PercentSalaryHike')]
boxplot( Performance ~ PercentSalaryHike, data = data, xlab = "PercentSalaryHike Received",
         ylab = "Individual Performance", main = "PercentSalaryHike Impacts on Performance")


# Box Plot (YearsWithCurrManager & Performance) --> Good One to Use
input <- data[,c('Performance','YearsWithCurrManager')]
boxplot( Performance ~ YearsWithCurrManager, data = data, xlab = "YearsWithCurrManager Received",
         ylab = "Individual Performance", main = "YearsWithCurrManager Impacts on Performance")

# Box Plot (BusinessTravel & Performance) --> Good One to Use
input <- data[,c('Performance','BusinessTravel')]
boxplot( Performance ~ BusinessTravel, data = data, xlab = "BusinessTravel Received",
         ylab = "Individual Performance", main = "BusinessTravel Impacts on Performance")



# Box Plot (StockOptionLevel & Performance) --> Good One to Use
input <- data[,c('Performance','StockOptionLevel')]
boxplot( Performance ~ StockOptionLevel, data = data, xlab = "StockOptionLevel Received",
         ylab = "Individual Performance", main = "StockOptionLevel Impacts on Performance")

# Box Plot (Mentoring & Performance) --> Good One to Use
input <- data[,c('Performance','Mentoring')]
boxplot( Performance ~ Mentoring, data = data, xlab = "Mentoring Received",
         ylab = "Individual Performance", main = "Mentoring Impacts on Performance")

# Box Plot (Mentoring & Gender)
input <- data[,c('Performance','Gender')]
boxplot( Performance ~ Gender, data = data, xlab = "Gender",
         ylab = "Individual Performance", main = "Gender Impacts on Performance")

# Box Plot (Age & Performance) --> Good One to Use (what is shows/doesn't show)
input <- data[,c('Performance','Age')]
boxplot( Performance ~ Age, data = data, xlab = "Age",
         ylab = "Individual Performance", main = "Age Impacts on Performance")

# Box Plot (JobInvolvement & Performance) --> Good One to Use (what is shows/doesn't show)
input <- data[,c('Performance','JobInvolvement')]
boxplot( Performance ~ JobInvolvement, data = data, xlab = "JobInvolvement",
         ylab = "Individual Performance", main = "JobInvolvement Impacts on Performance")




# Box Plot (Num Companies Worked & Performance) --> Good One to Use
input <- data[,c('Performance','NumCompaniesWorked')]
boxplot( Performance ~ NumCompaniesWorked, data = data, xlab = "NumCompaniesWorked",
         ylab = "Individual Performance", main = "Impact Of Companies Previously Worked on Performance")

# Box Plot (Overtime & Performance) --> Good One to Use
input <- data[,c('Performance','OverTime')]
boxplot( Performance ~ OverTime, data = data, xlab = "OverTime",
         ylab = "Individual Performance", main = "Impact Of OverTime on Performance")


# Box Plot (RelationshipSatisfaction & Performance) 
input <- data[,c('Performance','RelationshipSatisfaction')]
boxplot( Performance ~ RelationshipSatisfaction, data = data, xlab = "RelationshipSatisfaction",
         ylab = "Individual Performance", main = "Impact Of RelationshipSatisfaction on Performance")

# Box Plot (WorkLifeBalance & Performance) 
input <- data[,c('Performance','WorkLifeBalance')]
boxplot( Performance ~ WorkLifeBalance, data = data, xlab = "WorkLifeBalance",
         ylab = "Individual Performance", main = "Impact Of WorkLifeBalance on Performance")

# Box Plot (YearsInCurrentRole & Performance) 
input <- data[,c('Performance','YearsInCurrentRole')]
boxplot( Performance ~ YearsInCurrentRole, data = data, xlab = "YearsInCurrentRole",
         ylab = "Individual Performance", main = "Impact Of YearsInCurrentRole on Performance")

# Box Plot (FeedbackFromManager & Performance) 
input <- data[,c('Performance','FeedbackFromManager')]
boxplot( Performance ~ FeedbackFromManager, data = data, xlab = "FeedbackFromManager",
         ylab = "Individual Performance", main = "Impact Of FeedbackFromManager on Performance")



# Box Plot (Job Role & Performance) --> Good One to Use // NEED TO FIX STYLE
input <- data[,c('Performance','JobRole')]
boxplot( Performance ~ JobRole, data = data, 

         ylab = "Job Role", 
         xlab = "Individual Performance",
         lab = "Individual Performance", main = "Impact Of JobRole Performance", cex.axis = 0.8,
         horizontal = TRUE, las = 1, par(mar = c(6.1, 18.1, 4.1, 2.1)))


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\      Scatter Plots       \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Scatter Plot (YearsInCurrentRole & Performance) --> Good One to Use
attach(data)
plot(YearsInCurrentRole, Performance, main="YearsInCurrentRole on Performance",
     xlab="YearsInCurrentRole", ylab="Performance", pch=19)

cor(MonthlyIncome, Performance)


# Scatter Plot (Monthly Income & Performance) --> Good One to Use
attach(data)
plot(MonthlyIncome, Performance, main="MonthlyIncome on Performance",
     xlab="Monthly Income", ylab="Performance", pch=19)

cor(MonthlyIncome, Performance)



# Scatter Plot (Stock Option Level & Total Working Years)
attach(data)
plot(StockOptionLevel, TotalWorkingYears, main="Stock Option Level and Total Working Years",
     xlab="Stock Option Level", ylab="TotalWorkingYears", pch=1)

# Scatter Plot (Job Involvement & Performance)
attach(data)
plot(JobInvolvement, Performance, main="Job Involvement & Performance",
     xlab="Job Involvement", ylab="Performance", pch=19)


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\      Bar Plots       \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

gender <- table(data$Gender)
barplot(gender,ylim = c(0,700), 
        main = "Gender Proportion", 
        xlab = "Gender",
        ylab = "Quantity")





#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\      Pie Chart       \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
















