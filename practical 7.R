# Insurance dataset 
# Load the dataset into a data frame first
# refer to notes on Blackboard for discusisons on 
# dummy variables and how they are generated

insurance_data <- read.csv("insurance.csv", na = "")
str(insurance_data)

# several variables need to be converted
# sex - male = 0, female = 1
# Smoker - yes = 1, no = 0
# Region contains 4 categories
# N = 4, so we need n-1 indicator variables
# = 3 indicator variables
# Code variables in alphabetical order
head(insurance_data$region, 15)

# Convert variables as described above
names(insurance_data)
attach(insurance_data)

insurance_data$sex <- factor(sex,
                             levels = c("male", "female"), 
                             ordered = FALSE)

insurance_data$smoker <- factor(smoker, 
                                levels = c("yes", "no"), 
                                ordered = FALSE)

insurance_data$region <- factor(region,  
                                levels = c("northeast", "northwest", "southeast", "southwest"), 
                                ordered = FALSE)

str(insurance_data)

# View the split of categorical variables within the data frame
# to examine balance
table(insurance_data$sex)
table(insurance_data$smoker)
table(insurance_data$region)


# Initial investigation of data variables
# and their correlations
# Be careful of your interpretation of this chart
pairs(insurance_data)
install.packages("psych")
library(psych)

# Seems there could be a positive correlation between 
# smoker and charges, perhaps charges and age
# and BMI and charges
pairs.panels(insurance_data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# if we build the model now, R will automatically split the
# factor variables
# Alternatively we will control this process

# in linear regression, model represented by:
# y = b0 + B1x1 + B2x2 + B3x3..... + e
# where y = insurance charges
# x1 = age of the person
# x2  sex of the person
# x3 = bmi of the person
# x4 = children
# x5 = smoker
# x6 = region
# It is clear that x1, and x3 are continuous and x2, x4, x5, x6 are categorical
# therefore we need to create dummy variables for the categorical
# variables
# Eg for the smoker variable x5
# x5 = 1 if person is smoker
# x5 = 0 if person is non-smoker

# Initial build of th MLR model
# Dummy varaibles created automatically by R
set.seed(1)
model <- lm(formula = charges ~ 
              age + 
              sex + 
              bmi + 
              children + 
              smoker + 
              region, 
            data = insurance_data)

model
summary(model)

#BMI,children, smokerno have an influence
# over the dependent variable
# drop sex variable
#keep region variable because
#it is part of my research question 

names(insurance_data)
insurance_data <- insurance_data[c(1, 3:7)]
names(insurance_data)

insurance_data$bmi <- round(bmi, 1)
insurance_data$charges <- round(charges, 2)

#create the model again 
model2 <- lm(formula = charges ~
               age +
               bmi + 
               children + 
               smoker + 
               region, 
             data = insurance_data)

summary(model2)
#model assumptions
#linearity 
#we can check linear correlation 
#if it exists between dependent and each 
#independent variable 
#only works for continuous data 
attach(insurance_data)
scatter.smooth(x = ages , y = charges , main = "Insurance charges for age",
               ylab = "Insurance charges for age",
               xlab = "Age (years)")

#BMI and children 
scatter.smooth(x = bmi,
               y= charges,
               main = "Insurance charges for age",
               ylab = "Insurance charges for BMI",
               xlab = "BMI" )
scatter.smooth(x = children,
               y = charges,
               main = "Insurance charges for children",
               ylab = "Insurance charegs(,000)",
               xlab = "BMI")

#cant use scatter.smooth for cat data 
attach(insurance_data)
plot(x = smoker,
     y = charges,
     main = "Charges by smoker status",
     xlab = "smoker",
     ylab = "Insurance charges")
cor()
attach(insurance_data)
plot(x = region,
     y = charges,
     main = "Charges by smoker status",
     xlab = "smoker",
     ylab = "Insurance charges")
cor(charges, smoker)

#normality 
with(insurance_data, {
  qqnorm(age, 
         main = "Normality analysis of age data"),
  qqline(age))
})

normality_age_test <- shapiro.test(insurance_data$age)
normality_age_test$p.value
#p-value < 0.05 then data is not normally distributed
#cant check factor variables with this approach 

with(insurance_data, tapply(charges, smoker, shapiro.test))

with(insurance_data, tapply(charges, region, shapiro.test))

#colinearity
install.packages("car")
library(car)
#VIF score should be close to 1 but under 5 
#10+ indicates that vars are not nheeded
#and can be removed from the model
vif(model)
