
# Libraries ---------------------------------------------------------------

  library(ggplot2)
  library(stargazer)

# 1. Preliminaries --------------------------------------------------------

## 1.1
  wage_data <- read.csv2("wages2.csv")

## 1.2
  hist(wage_data$WAGE,
       xlab = "WAGE",
       main = "Histogram of Wage of Men in the USA, 1980")
  
  
  ggplot(data = wage_data,
         mapping = aes(x = WAGE)) +
    geom_histogram()
  
## 1.3
  
  lm1 <- lm(formula = WAGE ~ EDUC + EXPER,
            data = wage_data)
  
  summary(lm1)
    # We see significant effects of education and experience on wage on a 0.1 % significance level.
    # All following interpretations are ceteris paribus, approximately, on average:
      # For an additional year of education, we expect monthly wage to increase by 76.22 Dollars.
      # For an additional year of work experience, we expect the monthly wage to increase by 17.64 Dollars.

# 2. Effects of Data Scaling on OLS Statistics ----------------------------
  
## 2.1
  wage_data$WAGETH <- wage_data$WAGE / 1000
  
## 2.2
  lm2 <- lm(formula = WAGETH ~ EDUC + EXPER,
            data = wage_data)
  
  ## What happens to the coefficients?
  lm1$coefficients
  lm2$coefficients
  
  lm1$coefficients / lm2$coefficients
    # All the coefficients scale directly linearly to WAGE: WAGE * 1000 -> coefficients / 1000
  
  ## What about the statistical significance of the coefficients?
  stargazer(lm1, lm2,
            type = "text")
    # No change in significance
  
## 2.3
  wage_data$EDUCM <- wage_data$EDUC * 12
  
## 2.4
  lm3 <- lm(formula = WAGE ~ EDUCM + EXPER,
            data = wage_data)
  
  ## What happens to the coefficients?
  lm1$coefficients
  lm3$coefficients
  
  lm1$coefficients / lm3$coefficients
    # Only the EDUC coefficient scales directly linearly to EDUC: EDUC * 12 -> coefficient of EDUC / 12
  
  ## What about the statistical significance of the coefficients?
  stargazer(lm1, lm3,
            type = "text")
    # No change in significance
  
## 2.5
  
  # Standardisation function
  standardise <- function(variable) {
    (variable - mean(variable)) / sd(variable)
  }
      # Example with simulated data:
      ggplot() + geom_histogram(aes(x = rnorm(100000, mean = 4, sd = 2)),
                                bins = 100,
                                fill = "red") +
                geom_histogram(aes(x = standardise(rnorm(100000, mean = 4, sd = 2))),
                               bins = 100,
                               fill = "green")
  
  wage_data$WAGEZ <- standardise(wage_data$WAGE)
  wage_data$EDUCZ <- standardise(wage_data$EDUC)
  wage_data$EXPERZ <- standardise(wage_data$EXPER)
  
  lm4 <- lm(formula = WAGEZ ~ EDUCZ + EXPERZ,
            data = wage_data)

  summary(lm4)  
    # We see significant effects of education and experience on wage on a 0.1 % significance level.
    # All following interpretations are ceteris paribus, approximately, on average:
      # For a one standard-deviation increase in education (measured in years), we expect an increase in wage of 0.41 standard deviations
      # For a one standard-deviation increase in experience (measured in years), we expect an increase in wage of 0.19 standard deviations  
      # Education has the largest impact on wage

# 3. Functional Form and Choice of Additional Regressors ------------------

  ## 3.1
  lm5 <- lm(formula = log(WAGE) ~ EDUC + EXPER,
            data = wage_data)
  
  summary(lm5)  
  # We see significant effects of education and experience on wage on a 0.1 % significance level.
  # All following interpretations are ceteris paribus, approximately, on average:
    # For an additional year of education, wage is expected to increase by 7.78 % 
    # For an additional year of work experience, wage is expeted to increase by 2 %

  ## 3.2
  lm6 <- lm(formula = log(WAGE) ~ EDUC + EXPER + TENURE + IQ,
            data = wage_data)

  summary(lm6)  
  # We see significant effects of education, experience, tenure and IQ on wage on a 0.1 % significance level.
  # All following interpretations are ceteris paribus, approximately, on average:
    # For an additional year of education, wage is expected to increase by 5.56 % 
    # For an additional year of work experience, wage is expected to increase by 1.54 %
    # For an additional year of tenure, wage is expected to increase by 1.24 %
    # For an addional IQ point, wage is expected to increase by 0.55 %

  ## 3.3 
  summary(lm6)
  # As we can see from the coefficient summary, both IQ and TENURE show effects significant on an 0.1% level.
  
  ## 3.4
  stargazer(lm5, lm6, type = "text")
    # Model 6 explains 18.3 % of the variation in y, whereas model 5 only explains 13.1 %.
  

# F-Test Excercises -------------------------------------------------------

  ## 1. F-Test for overall significance
  lm7 <- lm(formula = WAGE ~ SCORES + EXPER + TENURE + IQ + EDUC + AGE,
            data = wage_data)

  summary(lm7)  
    # The F-Test compares the given model with a model containing only the intercept.
    # As a group, all variables in the model have a significant effect on wage

  ## 2. Hypotheses about groups
  
  # H0: beta4, beta5 and beta6 = 0
  
  lm7 # is our unrestricted model (with all variables)
  
  lm7_res <- lm(formula = WAGE ~ SCORES + EXPER + TENURE,
                data = wage_data)
  
  anova(lm7_res,lm7)
    # With a p-value < 0.01 we can reject our H0 of no group influence.
  
  ## 3. Functional form# Plotting WAGE against AGE and AGE^2
  ggplot(data = wage_data,
         mapping = aes(x = WAGE, y = AGE)) +
    geom_point() +
    geom_jitter() + # add some noise to see the points better
    geom_smooth()
  
  
  # Restricted model without AGE
  lm8_res <- lm(formula = WAGE ~ SCORES + EXPER + TENURE + IQ + EDUC,
            data = wage_data)
  
  lm8     <- lm(formula = WAGE ~ SCORES + EXPER + TENURE + IQ + EDUC + AGE + I(AGE^2),
            data = wage_data)
  
  anova(lm8_res, lm8)
    # With a p-value of 0.64 we cannot reject the H0  
  
  summary(lm8)
  