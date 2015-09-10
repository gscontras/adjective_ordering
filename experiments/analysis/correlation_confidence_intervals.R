# Bootstrap 95% CI for R-Squared
library(boot)
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 
# bootstrapping with 1000 replications 
results <- boot(data=fs, statistic=rsq, R=10000, formula=subjectivity~response)
# view results
results 
plot(results)
# get 95% confidence interval 
boot.ci(results, type="bca")

## for inferred-distance ~ corpus 
# t1* 0.7190124 0.006736156   0.1019942
# 95%   ( 0.3974,  0.8642 )  

## for naturalness of pred1 ~ corpus
# t1* 0.839568 -0.002518377  0.05390976
# 95%   ( 0.6868,  0.9159 ) 

## for faultless ~ subjectivity
# t1* 0.8916601 0.002180427  0.02575724
# 95%   ( 0.8125,  0.9276 )  

## for subjectivity ~ faultless
# t1* 0.8916601 0.002448418  0.02525995
# 95%   ( 0.815,  0.928 )  

## for naturalness ~ faultless difference (PREDICATE)
# t1* 0.6274071 7.97753e-05  0.02812938
# 95%   ( 0.5658,  0.6772 ) 

## for naturalness ~ faultless difference (CLASS)
# t1* 0.8059274 0.0001572343  0.04437677
# 95%   ( 0.6915,  0.8747 ) 

## for naturalness ~ subjectivity difference (PREDICATE)
# t1* 0.5700732 6.298432e-05  0.03064024
# 95%   ( 0.5030,  0.6241 ) 

## for naturalness ~ subjectivity difference (CLASS)
# t1* 0.7712465 0.0004370731  0.05434795
# 95%   ( 0.6353,  0.8562 ) 