
# Libraries ---------------------------------------------------------------
  library(ggplot2)
  library(stargazer)

# Data Import -------------------------------------------------------------

  ## Import Dataset
  wage_data <- read.csv2("wages2.csv")
  
  ## Take a peek at the data
  str(wage_data)
  head(wage_data)
  

# Graphical Analysis ------------------------------------------------------

  ## First analyse our variable of interest : WAGE
  hist(wage_data$WAGE)
  
  ggplot(wage_data,
         mapping = aes(x = WAGE)) +
    geom_histogram()
  
  ## Plot WAGE against other Variables
  
  ggplot(wage_data, 
         mapping = aes(x = WAGE, y = TENURE)) +
    geom_point() +
    geom_smooth()
  

# Set up Models -----------------------------------------------------------

  ## Work related factors in the model
  m1 <- formula(WAGE ~ SCORES + EXPER + TENURE)
  m2 <- formula(WAGE ~ SCORES + EXPER + I(TENURE^2))
  
  m1_log <- formula(log(WAGE) ~ SCORES + EXPER + TENURE)


# Estimation --------------------------------------------------------------

  ## Work related factors model
  lm1 <- lm(formula = m1,
            data = wage_data)
  
  lm2 <- lm(formula = m2,
            data = wage_data)
  
  lm_log1 <- lm(formula = m1_log,
                 data = wage_data)
  

# Model Assessment --------------------------------------------------------
  
  # Model Summary
  summary(lm_log1)
  
  # Simple Plots
  hist(lm_log1$residuals, breaks = 30)
  plot(lm1)
  
  ## Plot residuals and normal distribution with ggplot
  
  resid_df <- data.frame(res_lm1 = lm1$residuals,
                         res_lm2 = lm2$residuals,
                         res_lm_log1 = lm_log1$residuals) # dataframe for ggplot
  
  ggplot(data = resid_df, aes(x = res_lm_log1)) +
    geom_density() + # Density curve of the residuals
    stat_function(fun  = dnorm, # Normal Distribution 
                  args = list(mean = mean(resid_df$res_lm_log1),
                              sd   = sd  (resid_df$res_lm_log1)),
                  size = 2
                  )
  

# Model Comparison --------------------------------------------------------

  stargazer(lm1, lm2,
            type = "text")
  
  