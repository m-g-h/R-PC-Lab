

# Loading of several packages we work with


#Install R and R Studio:\\
#https://cran.r-project.org/bin/windows/base/\\
#https://www.rstudio.com/products/rstudio/download/\\


#help on R function: help("nameoffunction") e.g. help("hist") or help("lm")

#install.packages("dplyr")
#install.packages("magrittr")
#install.packages("stargazer")


library(dplyr)
library(magrittr)
library(stargazer)




# Loading of the dataset (csv for English, csv2 for German excel files; 
# cauz English uses commas to seperate and points between numbers while German uses semikolons to seperate and commas between numbers)
data <- read.csv2(file = "wages2.csv",header = TRUE)


# first overview over the data

names(data) 


#exploration and summary all variables
 
dim(data)



summary(data$WAGE)
hist(data$WAGE, main = paste("Histogram of WAGE"))

summary(data$EDUC)
hist(data$EDUC,  main = paste("Histogram of EDUC"))
 
summary(data$EXPER)
hist(data$EXPER, main = paste("Histogram of EXPER"))
 
 

# 1.3 Regression Model 

reg <- lm (WAGE ~ EDUC + EXPER , data = data)

summary(reg)



# 2 Wage scale

data$WAGETH <- data$WAGE/1000

reg2 <- lm (WAGETH ~ EDUC + EXPER , data = data)
summary(reg2) 

# comparison of the original and rescaled model

stargazer (reg,reg2,type = "text")

# 

# z-standardization

#WAGE
mean_wage <- mean(data$WAGE,na.rm = TRUE) 
sd_wage   <- sd(data$WAGE,na.rm = TRUE)   

data$WAGE_Z <- (data$WAGE-mean_wage)/sd_wage 

#EDUCATION
mean_educ <- mean(data$EDUC,na.rm = TRUE) 
sd_educ   <- sd(data$EDUC,na.rm = TRUE)

#recoding education in months

data$EDUCM <- data$EDUC * 12
reg3 <- lm (WAGE ~ EDUCM + EXPER , data = data)
summary(reg3) 

stargazer (reg,reg3,type = "text")

data$EDUC_Z <- (data$EDUC-mean_educ)/sd_educ

#EXPERIENCE
mean_exper <- mean(data$EXPER,na.rm = TRUE) 
sd_exper   <- sd(data$EXPER,na.rm = TRUE)   

data$EXPER_Z <- (data$EXPER-mean_exper )/sd_exper 

#Z-Regression

reg4 <- lm (WAGE_Z ~ EDUC_Z + EXPER_Z , data = data)
summary(reg4) 



# 3 nonlinear relationships

# 3.1 ln(wage)

data$WAGEL <- log(data$WAGE)
hist(data$WAGEL, main = paste("Histogram of log WAGE"), breaks=15)

reg5 <- lm (log(WAGE) ~ EDUC+ EXPER , data = data)
summary(reg5) 

# 3.2 (additional variables)

reg6 <- lm (log(WAGE) ~ EDUC + EXPER + TENURE + IQ , data = data)
summary(reg6)

# 3.3 hypothesis testing

ttest <- function(reg6, coefnum, val){
  co <- coef(summary(reg6))
  tstat <- (co[coefnum+1,1]-val)/co[coefnum+1,2]
  2 * pt(abs(tstat), reg6$df.residual, lower.tail = FALSE)
}

 ttest(reg6, 3,0) #returns p-value
 ttest(reg6, 4,0)
 
 #test whether beta 3 equal 0.01
 ttest(reg6, 3,0.01)
 
 

# 3.4 model comparison

stargazer (reg5,reg6,type="text") # 


# Additional Task: Include the ndummy married as well as exp^2 in the regression and interpret the results

EXPER_Squared=data$EXPER*data$EXPER

reg7<- lm (log(WAGE) ~ EDUC + EXPER + EXPER_Squared+ MARRIED+TENURE + IQ , data = data)
summary(reg7)

