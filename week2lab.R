# 2. Write the R code to read this data into R.
# Given data is 4 baterries at each tempa and material
battery_life <- c(
  130, 155, 74, 180, 34, 40, 80, 75, 20, 70, 82, 58,
  150, 188,  159, 126, 136, 122, 106, 115, 25, 70, 58, 45,
  138, 110,  168, 160, 174, 120, 150, 139, 96, 104, 82, 60 )

# inrtaction <- Materials + tem+ mat:trmp, data = lifetime
# Create the row and column names
Material_names <- factor(rep(c(1,2,3),each = 12))
Temperature <- factor(rep(c(-10,20,55),each = 4, length(36)))

# creating data frame
battery <- data.frame(Material_names, Temperature)
battery <- cbind(battery, battery_life)

# Display the data frame
print(battery)

# 3. Find the summary statistics for this data. First think about what 
# sort of statistics you should be interested in.
with(battery, tapply(battery_life, list(Material_names, Temperature), mean))

# 4. Now fit a linear model to the battery lifetimes. Investigate any interaction terms.
battery_lm <- lm(battery_life ~ Material_names + Temperature, data = battery)
summary(battery_lm)

# 5. Select the best model based on your analysis.
battery_lm_fit <- lm(battery_life ~ Material_names + Temperature + 
                       Material_names:Temperature, data = battery)
summary(battery_lm_fit)

# 6. Perform appropriate model diagnostics.
library(MASS)
sres <- stdres(battery_lm_fit)
plot(sres ~ battery_lm_fit$fitted.values, xlab = "Fitted Values", 
     ylab = "Standardised residuals")
hist(battery_lm_fit$residuals, xlab = "Residuals")
box()
hist(sres, xlab = "Standardised residuals")
qqnorm(sres)
qqline(sres)
# 7. Produce an interaction plot for the mean battery lifetimes.
with(battery, interaction.plot(Material_names, Temperature, battery_life))
with(battery, interaction.plot(Temperature, Material_names, battery_life))

# 8. Interpret your model.
# 9. Which material would you recommend for the batteries? Justify your selection.


# Exercise 2: Data manipulation Consider the following grouped data on seatbelt 
# use and the severity of injury in an accident.
# 1. Enter the data into R using the variables Injury, SeatBelt and Frequency.
Injury <- gl(n=4, labels = c('fatal', 'severe', 'minor', 'unknown'), k = 3)
SeatBelt <- gl(n=3, labels = c('worn', 'not worn', 'unknown'), k = 1, length=12)

Frequency <- c(35, 6, 15, 1142, 48, 328, 7969, 76, 764, 11404, 24, 38570)
(Accident <- data.frame(Injury, SeatBelt, Frequency))
xtabs(Frequency ~ Injury + SeatBelt)
# 2. Now we want to create date that contains one record for each case. 
Injury_ind <- rep(Injury, Frequency)
head(Injury_ind, 500)

SeatBelt_ind <- rep(SeatBelt, Frequency)
head(SeatBelt_ind, 500)

Accident_ind <- data.frame(Injury_ind, SeatBelt_ind)
colnames(Accident_ind) <- c("Injury", "SeatBelt")
Accident_ind
# That is, we need to create 35 entries corresponding to a fatal injury where 
# the seat belt was worn, 6 for when the seat belt was worn, and 15 for unknown. 
# Similarly for the other levels of injuries. Write a short (2 lines!) of R code 
# to achieve this, and test your code (for example, by producing a table from your
#                                      new data).

# Exercise 3: Fish data The folder Data in the Computer Labs folder contains the data set Fish.txt. 
# Download the file and read the data into R. The variables are:
#  Code: fish species code
#  Weight: weight of the fish in grammes
#  Length1: length from the nose to the beginning of the tail (cm) 
#  Length2: length from nose to notch of tail (cm)
#  Length3: length from nose to the end of the tail (cm)
#  Height: maximum height as a percentage of Length3
#  Width: maximum width as a percentage of Length3
fish <- read.table("Fish.txt", header = T)

# 1. Summarise the data and check for any data errors.
summary(fish)
# 2. You will note a weight of 0. Determine which data record this corresponds to and omit it. 
# Use the commands which and fish1 <- fish[-x,], where x corresponds to the number of the record in error. 
# Check that the record with the error has been removed.
(x <- which(fish$weight == 0))
fish1 <- fish[-x,]
# 3. Note that Code for the species of fish. This is currently numerical and needs to be converted to a 
# factor. Use the code fish1$Code <- factor(fish1$Code). 
# (Note that if Code is left as numerical, the model will estimate a single 
#   coefficient for it. The contribu- tion of this variable will then be linear 
#   in this coefficient. So for example, the effect of a value 2 for Code is 
#   twice that for a value 1. This is not correct.)
fish1$Code <- factor(fish1$Code)

# 4. Fit a linear regression model with Weight as response against the other covariates.
fish_lm <- lm(Weight ~ Length1 + Length2 + Length3 + Height + Width + Code, data = fish)

# 5. Investigate interaction terms in the model.
# 6. Perform model diagnostics. In particular, examine the plot of residuals against fitted values for any patterns (indicating issues with a linear model fit) or change in spread (indicating a violation of homogeneous variance assumption).
# Residual plots
par(mfrow = c(2, 2))
plot(fish_lm, pch = 20)

# 7. By examining plots of the explanatory variables against the response variable, determine an appropriate transformation of data to improve the model for weight against the other morphological measurements.
# Plots of explanatory variables against the response (Weight)
par(mfrow = c(2, 3))
plot(Weight ~ Length1, data = fish, main = "Weight vs. Length1")
plot(Weight ~ Length2, data = fish, main = "Weight vs. Length2")
plot(Weight ~ Length3, data = fish, main = "Weight vs. Length3")
plot(Weight ~ Height, data = fish, main = "Weight vs. Height")
plot(Weight ~ Width, data = fish, main = "Weight vs. Width")

# 8. Fit your selected model.
# For example, if we decide to transform Length1 to log(Length1):
fish$logLength1 <- log(fish$Length1)
fish_lm_selected <- lm(Weight ~ logLength1 + Length2 + Length3 + Height + Width + Code, data = fish)

# 9. Reduce the model removing non-significant variables one by one, until a model with only significant terms is left. Use update command. For example, fish.lm1 <- update(fish.lm .∼.-Length2).
fish_lm_reduced <- update(fish_lm_selected, . ~ . - Length2)
# Repeat the update command for other non-significant variables as necessary.

# 10. Perform model diagnostics. For this, plot a histogram of the residuals and a scatter plot of the residuals against the fitted values. Comment on whether the model assumptions are satisfied.
# Residual histogram
hist(resid(fish_lm_reduced), main = "Histogram of Residuals")

# Residuals vs. Fitted plot
plot(fitted(fish_lm_reduced), resid(fish_lm_reduced), pch = 20,
     main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals")

# 11. Explore the data further and decide how the model can be improved.

# 12. Report your findings on the dependence of the weight of the fish on the explanatory variables.



# Exercise 4: Bank data The folder Data in the Computer Labs folder contains the 
# data set Bank.txt. The female employees are suing the bank for gender 
# discrimation in salary. Download the file and read the data into R. 
# For each employee the Bank Data as the following variables.
#  EducLev: education level, a categorical variable with categories 
# 1 (finished high school), 2 (finished some tertiary education), 
# 3 (obtained a bachelor’s degree), 4 (took some post- graduate courses), 
# 5 (obtained a postgraduate degree).
#  Job Grade: a categorical variable indicating the current job level, 
# the possible levels being 1 (lowest) to 6 (highest).
#  YrHired: year employee was hired.
#  YrBorn: year employee was born.
#  Gender: a categorical variable with values “Female” and “Male”.
#  YrsPrior: number of years of work experience at another bank prior to working 
# at First National.
#  PCJob: a categorical yes/no variable indicating whether the employees current 
# job is PC related.
#  Salary: current salary in thousands of dollar.
bank_data <- read.table("Bank.txt", header = TRUE)
# 1. Summarise the data and check for any data errors.
summary(bank_data)
# 2. Fit a linear model to the Salary. Do not include any interactions.
model1 <- lm(Salary ~ EducLev + JobGrade + YrHired + YrBorn + Gender + YrsPrior + PCJob, data = bank_data)
summary(model1)
# 3. Reduce the model to only significant terms.
model1_reduced <- update(model1, . ~ . - YrHired - YrBorn - YrsPrior - PCJob)
summary(model1_reduced)
# 4. Perform appropriate model diagnostics.
plot(model1_reduced)
# 5. Is there a gender bias in salaries? Justify your decision.

# 6. Now include appropriate interaction terms. You may have to consider which 
# interactions are meaningful.
model2 <- lm(Salary ~ (EducLev + JobGrade + Gender)^2, data = bank_data)
summary(model2)
# 7. Again reduce the model to only significant terms.
model2_reduced <- update(model2, . ~ . - Gender:JobGrade)
summary(model2_reduced)
# 8. Perform model diagnostics.
plot(model2_reduced)
# 9. Under this new model, is there gender bias in salaries? Justify your decision.
anova(model2_reduced)
# 10. Produce a scatterplot of fitted salaries against observed salaries. 
# The plotting character should be Sex (M or F), and the colour code should be 
# by education level.
plot(model2_reduced$fitted.values, bank_data$Salary, pch = ifelse(bank_data$Gender == "Female", "F", "M"), 
     col = bank_data$EducLev, xlab = "Fitted Salaries", ylab = "Observed Salaries")
legend("topright", legend = levels(bank_data$EducLev), col = 1:5, pch = "F", title = "Education Level")
# 11. Comment your findings from the plot.
# 12. What form of discrimination can you detect from your analysis?