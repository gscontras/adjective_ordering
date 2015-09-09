# Bootstrap 95% CI for R-Squared
library(boot)
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 
# bootstrapping with 1000 replications 
results <- boot(data=o_agr, statistic=rsq, 
                R=10000, formula=response~f_diff)

# view results
results 
plot(results)

# get 95% confidence interval 
boot.ci(results, type="bca")

## for naturalnesss ~ corpus 
# t1* 0.7190124 0.006736156   0.1019942
# 95%   ( 0.3974,  0.8642 )  

## for faultless ~ subjectivity
# t1* 0.8916601 0.002180427  0.02575724
# 95%   ( 0.8125,  0.9276 )  

## for naturalness ~ faultless
# t1* 0.8222903 -0.0002285857  0.04066239
# 95%   ( 0.7217,  0.8860 )  

