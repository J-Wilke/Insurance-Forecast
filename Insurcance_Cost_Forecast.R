# Load necessary libraries
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(relaimpo))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(gganimate))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(effects))




# Set theme_gray() as default for all plots
theme_set(theme_gray())

# Read Data in 
Insurance <- read.csv("C:/Users/jonaw/OneDrive/Dokumente/Coding/20240926_R_Kaggle_Insurance Forecast/insurance.csv")

# Describing Data
summary(Insurance)

# Insurance Cost by Region
ggplot(data = Insurance, aes(region, charges)) + 
  stat_boxplot(geom = "errorbar", width = 0.1) +  
  geom_boxplot(outlier.shape = 19, outlier.color = "black", fill = c("skyblue"), width = 0.5) +  
  ggtitle("Insurance Cost per Region")

# Insurance Cost by Smoking status
ggplot(data = Insurance, aes(smoker, charges)) + 
  stat_boxplot(geom = "errorbar", width = 0.1) +  
  geom_boxplot(outlier.shape = 19, outlier.color = "black", fill = c("lightcoral"), width = 0.5) + 
  ggtitle("Insurance Cost Smokers")

# Insurance Cost by Gender
ggplot(data = Insurance, aes(sex, charges)) + 
  stat_boxplot(geom = "errorbar", width = 0.1) +  
  geom_boxplot(outlier.shape = 19, outlier.color = "black", fill = c("lightgreen"), width = 0.5) +  
  ggtitle("Insurance Cost by Gender")

# Define new column for Obesity
Insurance$bmi30 <- ifelse(Insurance$bmi>=30,"yes", "no")

# Insurance Cost by Obesity
ggplot(data = Insurance, aes(bmi30, charges)) + 
  stat_boxplot(geom = "errorbar", width = 0.1) +  
  geom_boxplot(outlier.shape = 19, outlier.color = "black", fill = c("skyblue"), width = 0.5) +  
  ggtitle("Insurance Cost by Obesity")

# Correlation-Scatterplot Matrix
ggpairs(Insurance[c("age", "bmi", "children", "charges")], 
        lower = list(continuous = wrap("smooth", color = "lightblue")),
        upper = list(continuous = wrap("cor", size = 5))) +
  ggtitle("Scatterplot Matrix of Age, BMI, Children, and Charges")

# 3D Scatterplot
plot_ly(Insurance, x = ~age, y = ~bmi, z = ~charges, color = ~smoker, type = "scatter3d",
        colors = c("skyblue","lightcoral"), mode = "markers", marker = list(size = 2)) %>%
  layout(title = "3D Scatter Plot of Charges by Age, BMI, and Smoker")

# Linear Modell
Insurance_model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = Insurance)
summary(Insurance_model)

# Multikollinearity Check 
vif(Insurance_model)

# Checking Distribution of residuals
qqnorm(residuals(Insurance_model))
qqline(residuals(Insurance_model))

shapiro.test(residuals(Insurance_model))

# Histogram of Residuals
hist(residuals(Insurance_model), main = "Histogram of Residuals", xlab = "Residuals")

# Residuals vs Fitted Plot
plot(Insurance_model, which = 1)

# Model with Age and Smoking
Insurance_model3 <- lm(charges ~ age + smoker, data = Insurance)
summary(Insurance_model3)

# Convert 'smoker' to a factor
Insurance$smoker <- factor(Insurance$smoker, levels = c("no", "yes"))

# Calculate intercepts correctly
intercepts <- c(
  coef(Insurance_model3)["(Intercept)"],
  coef(Insurance_model3)["(Intercept)"] + coef(Insurance_model3)["smokeryes"]
)

# Create a data frame for the lines with correct intercepts
lines.df <- data.frame(
  intercepts = intercepts,
  slopes = rep(coef(Insurance_model3)["age"], 2),
  smoker = levels(Insurance$smoker)
)

# Plot with corrected intercepts and factor levels
ggplot(data = Insurance, aes(x = age, y = charges, color = smoker)) +
  geom_point(alpha = 0.6) +
  geom_abline(aes(intercept = intercepts, slope = slopes, color = smoker), 
              data = lines.df, linewidth = 1) + 
  scale_y_continuous(breaks = seq(0, 65000, 5000)) +
  labs(title = "Insurance Charges by Age and Smoking Status")


# Plot with with Gender
ggplot(data = Insurance, aes(x = age, y = charges, color = smoker)) +
  geom_point(alpha = 0.6) +
  geom_abline(aes(intercept = intercepts, slope = slopes, color = smoker), 
              data = lines.df, linewidth = 1) +
  facet_wrap(~ sex) + 
  scale_y_continuous(breaks = seq(0, 65000, 5000)) +
  labs(title = "Insurance Charges by Age, Smoking Status, and Gender")

# Creating a new Data Points
new_customer <- data.frame(
  age = c(30, 50, 60),
  sex = c("female", "male", "female"),
  bmi = c(25, 35, 28),
  children = c(1, 0, 3),
  smoker = c("no", "yes", "no"),
  region = c("northwest", "southeast", "southwest")
)

# Predicting Insurance Costs
Predicted_charges <- predict(Insurance_model, newdata = new_customer)
Predicted_charges

# Creating a Scenario
age_scenario <- data.frame(  
  age = seq(20, 60, by = 1),
  sex = "male",
  bmi = 30,
  children = 2,
  smoker = "yes",
  region = "northwest")

# Prediction for the Scenarion
Predicted_charges_age <- predict(Insurance_model, newdata = age_scenario)

# Adding Predictes Values Values to the Scenario
age_scenario$Pedicted_charges <- Predicted_charges_age

# Effects on the Linear Model
plot(allEffects(Insurance_model))


# Erstelle ein Gitter von Werten für 'age' und 'bmi'
age_vals <- seq(min(Insurance$age), max(Insurance$age), length.out = 30)
bmi_vals <- seq(min(Insurance$bmi), max(Insurance$bmi), length.out = 30)

# Vorhersagegitter für beide Rauchergruppen erstellen
grid <- expand.grid(age = age_vals, bmi = bmi_vals, children = mean(Insurance$children),
                    sex = "male", smoker = c("no", "yes"), region = "southeast")

# Vorhersagen für beide Rauchergruppen aus dem Modell
grid$charges <- predict(Insurance_model, newdata = grid)

# Separiere die Gitter für Raucher und Nichtraucher
grid_non_smoker <- grid[grid$smoker == "no", ]
grid_smoker <- grid[grid$smoker == "yes", ]

# 3D-Plot mit Vorhersageflächen für Raucher und Nichtraucher
plot_ly() %>%
  # Scatterplot der tatsächlichen Daten
  add_markers(data = Insurance, x = ~age, y = ~bmi, z = ~charges, color = ~smoker,
              marker = list(size = 3), colors = c("skyblue", "coral"),
              name = ~smoker) %>%
  
  # Vorhersagefläche für Nichtraucher
  add_surface(x = ~age_vals, y = ~bmi_vals, z = matrix(grid_non_smoker$charges, nrow = 30, ncol = 30),
              showscale = FALSE, opacity = 0.5, name = "Prediction Surface (Non-Smoker)") %>%
  
  # Vorhersagefläche für Raucher
  add_surface(x = ~age_vals, y = ~bmi_vals, z = matrix(grid_smoker$charges, nrow = 30, ncol = 30),
              showscale = FALSE, opacity = 0.5, name = "Prediction Surface (Smoker)") %>%
  
  # Layout für den 3D-Plot
  layout(scene = list(xaxis = list(title = "Age"),
                      yaxis = list(title = "BMI"),
                      zaxis = list(title = "Charges")),
         title = "3D Scatterplot with Prediction Surfaces for Smoker and Non-Smoker")

