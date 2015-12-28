library(hydroGOF)
library(ggplot2)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/13-subjectivity-expanded")

# Bootstrap 95% CI for R-Squared
library(boot)
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 

source("results/splithalf.R")

s = read.csv("results/subjectivity-expanded_results.csv",header=T)
head(s)
s_agr_pred = aggregate(response~predicate,data=s,mean)
s_agr_class = aggregate(response~class,data=s,mean)

#load in naturalness preferences
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/12-order-preference-expanded/Submiterator-master/order-preference-duplicated.csv",header=T)
head(o)
o <- o[o$makes_sense=="yes",]
o_agr_pred = aggregate(correctresponse~correctpred1*correctclass1,data=o,mean)
o_agr_class = aggregate(correctresponse~correctclass1,data=o,mean)
head(o_agr_pred)

# explainable variance
o$workerID = o$workerid + 1
o$response = o$correctresponse
o$class = o$correctclass
#library(plyr)
prophet(splithalf_class(o, 100), 2) # 0.95 class configuration
prophet(splithalf_correctpred(o, 100), 2) # 0.98 predicate configuration
s$workerID = s$workerid
prophet(splithalf_pred(s, 100), 2) # 0.98

## SUBJECTIVITY
# PREDICATE
o_agr_pred$subjectivity = s_agr_pred$response[match(o_agr_pred$correctpred1,s_agr_pred$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$subjectivity) # r = .72, r2 = .51
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.3174,  0.6583 )  
# PREDICATE WITHOUT CLASS X
o_x = o_agr_pred[o_agr_pred$correctclass1!="X",]
gof(o_x$correctresponse,o_x$subjectivity) # r = .76, r2 = .58
results <- boot(data=o_x, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.4128,  0.6960 )  
# CLASS
o_agr_class$subjectivity = s_agr_class$response[match(o_agr_class$correctclass1,s_agr_class$class)]
gof(o_agr_class$correctresponse,o_agr_class$subjectivity) # r = .86, r2 = .73
results <- boot(data=o_agr_class, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.3629,  0.8717 )

# plot order preference against subjectivity
ggplot(o_agr_pred, aes(x=subjectivity,y=correctresponse,color=correctclass1)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity")+
  ylab("naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
ggsave("results/naturalness-subjectivity.pdf",height=4,width=5.5)



#####
## configuration analysis
#####

# load in order preference
head(o)
o$predicate = o$correct_configuration
o$class = o$correctclass
o$workerID = o$workerid + 1
o$response = o$correctresponse
# get Spearman-Brown prophecy (explainable variance)
prophet(splithalf_class(o, 100), 2) # 0.95 class configuration
prophet(splithalf_pred(o, 100), 2) # 0.65 predicate configuration


##SUBJECTIVITY
# CLASS add in subjectivity difference
o_agr = aggregate(correctresponse~correctclass+correctclass1+correctclass2,data=o,mean)
o_agr$class1_s = s_agr_class$response[match(o_agr$correctclass1,s_agr_class$class)]
o_agr$class2_s = s_agr_class$response[match(o_agr$correctclass2,s_agr_class$class)]
o_agr$s_diff = (o_agr$class1_s-o_agr$class2_s)
#compare subjectivity with order-preference
gof(o_agr$s_diff,o_agr$correctresponse) # r = .83, r2 = .69
results <- boot(data=o_agr, statistic=rsq, R=10000, formula=correctresponse~s_diff)
boot.ci(results, type="bca") # 95%   ( 0.6042,  0.7563 )  

# PREDICATE add in subjectivity difference
o_agr_pred = aggregate(correctresponse~correct_configuration+correctpred1+correctpred2+class,data=o,mean)
o_agr_pred$predicate1_s = s_agr_pred$response[match(o_agr_pred$correctpred1,s_agr_pred$predicate)]
o_agr_pred$predicate2_s = s_agr_pred$response[match(o_agr_pred$correctpred2,s_agr_pred$predicate)]
o_agr_pred$s_diff = (o_agr_pred$predicate1_s-o_agr_pred$predicate2_s)
#compare subjectivity with order-preference
gof(o_agr_pred$s_diff,o_agr_pred$correctresponse) # r = .54, r2 = .29
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~s_diff)
boot.ci(results, type="bca") # 95%   ( 0.5929,  0.6769 )

## plot order preference against subjectivity
# CLASS
ggplot(o_agr, aes(x=s_diff,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity difference")+
  ylab("configuration naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("~/Documents/git/cocolab/adjective_ordering/writing/short-paper/plots/naturalness-subjectivity_class-difference.pdf",height=3,width=3.5)

#PREDICATE
ggplot(o_agr_pred, aes(x=f_diff,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nfaultless difference")+
  ylab("configuration naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("~/Documents/git/cocolab/adjective_ordering/writing/short-paper/plots/naturalness-faultless_difference.pdf",height=3,width=3.5)
