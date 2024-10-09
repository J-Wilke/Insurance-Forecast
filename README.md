# Insurance-Forecast



``` r
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
```

```
##       age            sex                 bmi           children        smoker         
##  Min.   :18.00   Length:1338        Min.   :15.96   Min.   :0.000   Length:1338       
##  1st Qu.:27.00   Class :character   1st Qu.:26.30   1st Qu.:0.000   Class :character  
##  Median :39.00   Mode  :character   Median :30.40   Median :1.000   Mode  :character  
##  Mean   :39.21                      Mean   :30.66   Mean   :1.095                     
##  3rd Qu.:51.00                      3rd Qu.:34.69   3rd Qu.:2.000                     
##  Max.   :64.00                      Max.   :53.13   Max.   :5.000                     
##     region             charges     
##  Length:1338        Min.   : 1122  
##  Class :character   1st Qu.: 4740  
##  Mode  :character   Median : 9382  
##                     Mean   :13270  
##                     3rd Qu.:16640  
##                     Max.   :63770
```

``` r
# Insurance Cost by Region
ggplot(data = Insurance, aes(region, charges)) + 
  stat_boxplot(geom = "errorbar", width = 0.1) +  
  geom_boxplot(outlier.shape = 19, outlier.color = "black", fill = c("skyblue"), width = 0.5) +  
  ggtitle("Insurance Cost per Region")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

``` r
# Insurance Cost by Smoking status
ggplot(data = Insurance, aes(smoker, charges)) + 
  stat_boxplot(geom = "errorbar", width = 0.1) +  
  geom_boxplot(outlier.shape = 19, outlier.color = "black", fill = c("lightcoral"), width = 0.5) + 
  ggtitle("Insurance Cost Smokers")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

``` r
# Insurance Cost by Gender
ggplot(data = Insurance, aes(sex, charges)) + 
  stat_boxplot(geom = "errorbar", width = 0.1) +  
  geom_boxplot(outlier.shape = 19, outlier.color = "black", fill = c("lightgreen"), width = 0.5) +  
  ggtitle("Insurance Cost by Gender")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

``` r
# Define new column for Obesity
Insurance$bmi30 <- ifelse(Insurance$bmi>=30,"yes", "no")

# Insurance Cost by Obesity
ggplot(data = Insurance, aes(bmi30, charges)) + 
  stat_boxplot(geom = "errorbar", width = 0.1) +  
  geom_boxplot(outlier.shape = 19, outlier.color = "black", fill = c("skyblue"), width = 0.5) +  
  ggtitle("Insurance Cost by Obesity")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

``` r
# Correlation-Scatterplot Matrix
ggpairs(Insurance[c("age", "bmi", "children", "charges")], 
        lower = list(continuous = wrap("smooth", color = "lightblue")),
        upper = list(continuous = wrap("cor", size = 5))) +
  ggtitle("Scatterplot Matrix of Age, BMI, Children, and Charges")
```

```
## plot: [1, 1] [===>------------------------------------------------------------] 6% est: 0s
## plot: [1, 2] [=======>--------------------------------------------------------] 12% est: 0s
## plot: [1, 3] [===========>----------------------------------------------------] 19% est: 1s
## plot: [1, 4] [===============>------------------------------------------------] 25% est: 1s
## plot: [2, 1] [===================>--------------------------------------------] 31% est: 1s
## plot: [2, 2] [=======================>----------------------------------------] 38% est: 1s
## plot: [2, 3] [===========================>------------------------------------] 44% est: 0s
## plot: [2, 4] [===============================>--------------------------------] 50% est: 0s
## plot: [3, 1] [===================================>----------------------------] 56% est: 0s
## plot: [3, 2] [=======================================>------------------------] 62% est: 0s
## plot: [3, 3] [===========================================>--------------------] 69% est: 0s
## plot: [3, 4] [===============================================>----------------] 75% est: 0s
## plot: [4, 1] [===================================================>------------] 81% est: 0s
## plot: [4, 2] [=======================================================>--------] 88% est: 0s
## plot: [4, 3] [===========================================================>----] 94% est: 0s
## plot: [4, 4] [================================================================]100% est: 0s
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-5.png)

``` r
# 3D Scatterplot
plot_ly(Insurance, x = ~age, y = ~bmi, z = ~charges, color = ~smoker, type = "scatter3d",
        colors = c("skyblue","lightcoral"), mode = "markers", marker = list(size = 2)) %>%
  layout(title = "3D Scatter Plot of Charges by Age, BMI, and Smoker")
```

```
## Error in loadNamespace(name): es gibt kein Paket namens 'webshot'
```

``` r
# Linear Modell
Insurance_model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = Insurance)
summary(Insurance_model)
```

```
## 
## Call:
## lm(formula = charges ~ age + sex + bmi + children + smoker + 
##     region, data = Insurance)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11304.9  -2848.1   -982.1   1393.9  29992.8 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     -11938.5      987.8 -12.086  < 2e-16 ***
## age                256.9       11.9  21.587  < 2e-16 ***
## sexmale           -131.3      332.9  -0.394 0.693348    
## bmi                339.2       28.6  11.860  < 2e-16 ***
## children           475.5      137.8   3.451 0.000577 ***
## smokeryes        23848.5      413.1  57.723  < 2e-16 ***
## regionnorthwest   -353.0      476.3  -0.741 0.458769    
## regionsoutheast  -1035.0      478.7  -2.162 0.030782 *  
## regionsouthwest   -960.0      477.9  -2.009 0.044765 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6062 on 1329 degrees of freedom
## Multiple R-squared:  0.7509,	Adjusted R-squared:  0.7494 
## F-statistic: 500.8 on 8 and 1329 DF,  p-value: < 2.2e-16
```

``` r
# Multikollinearity Check 
vif(Insurance_model)
```

```
##              GVIF Df GVIF^(1/(2*Df))
## age      1.016822  1        1.008376
## sex      1.008900  1        1.004440
## bmi      1.106630  1        1.051965
## children 1.004011  1        1.002003
## smoker   1.012074  1        1.006019
## region   1.098893  3        1.015841
```

``` r
# Checking Distribution of residuals
qqnorm(residuals(Insurance_model))
qqline(residuals(Insurance_model))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-6.png)

``` r
shapiro.test(residuals(Insurance_model))
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  residuals(Insurance_model)
## W = 0.89894, p-value < 2.2e-16
```

``` r
# Histogram of Residuals
hist(residuals(Insurance_model), main = "Histogram of Residuals", xlab = "Residuals")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-7.png)

``` r
# Residuals vs Fitted Plot
plot(Insurance_model, which = 1)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-8.png)

``` r
# Model with Age and Smoking
Insurance_model3 <- lm(charges ~ age + smoker, data = Insurance)
summary(Insurance_model3)
```

```
## 
## Call:
## lm(formula = charges ~ age + smoker, data = Insurance)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -16088.1  -2046.8  -1336.4   -212.7  28760.0 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2391.63     528.30  -4.527 6.52e-06 ***
## age           274.87      12.46  22.069  < 2e-16 ***
## smokeryes   23855.30     433.49  55.031  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6397 on 1335 degrees of freedom
## Multiple R-squared:  0.7214,	Adjusted R-squared:  0.721 
## F-statistic:  1728 on 2 and 1335 DF,  p-value: < 2.2e-16
```

``` r
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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-9.png)

``` r
# Plot with with Gender
ggplot(data = Insurance, aes(x = age, y = charges, color = smoker)) +
  geom_point(alpha = 0.6) +
  geom_abline(aes(intercept = intercepts, slope = slopes, color = smoker), 
              data = lines.df, linewidth = 1) +
  facet_wrap(~ sex) + 
  scale_y_continuous(breaks = seq(0, 65000, 5000)) +
  labs(title = "Insurance Charges by Age, Smoking Status, and Gender")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-10.png)

``` r
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
```

```
##         1         2         3 
##  4369.525 35458.248 13436.710
```

``` r
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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-11.png)

``` r
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
```

```
## Error in loadNamespace(name): es gibt kein Paket namens 'webshot'
```
