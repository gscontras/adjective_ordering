library(ggplot2)
library(reshape2)
library(lme4)
library(hydroGOF)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/11-faultless-disagreement-noun/Submiterator-master")

#source("../results/splithalf.R")
source("../results/helpers.R") # for bootsSummary() and boot

s = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/subjectivity_no-noun_results.csv")

#d$class <- factor(d$class,levels=c("quality","size","age","texture","color","shape","material"))

s_agr_pred = aggregate(response~predicate,data=s,mean)
s_agr_class = aggregate(response~class,data=s,mean)

#load in order preference
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/10-order-preference-noun/Submiterator-master/naturalness-duplicated.csv",header=T)
head(o)
o_agr_pred = aggregate(correctresponse~predicate*correctclass,data=o,mean)
o_agr_class = aggregate(correctresponse~correctclass,data=o,mean)
head(o_agr_pred)
# explainable variance
o$workerID = o$workerid + 1
o$response = o$correctresponse
o$class = o$correctclass
#library(dplyr)
# needs to not have plyr loaded/cannot use bootsSummary()
#prophet(splithalf_class(o, 100), 2) # 0.99 class configuration
#prophet(splithalf_pred(o, 100), 2) # 0.98 predicate configuration
#f$workerID = f$workerid
#prophet(splithalf_pred(f, 100), 2) # 

## FAULTLESS
# PREDICATE
o_agr_pred$faultless = f_agr_pred$response[match(o_agr_pred$predicate,f_agr_pred$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$faultless) # r = .91, r2 = .84
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~faultless)
boot.ci(results, type="bca") # 95%   ( 0.6392,  0.9120 ) 
# CLASS
o_agr_class$faultless = f_agr_class$response[match(o_agr_class$correctclass,f_agr_class$class)]
gof(o_agr_class$correctresponse,o_agr_class$faultless) # r = .92, r2 = .85
results <- boot(data=o_agr_class, statistic=rsq, R=10000, formula=correctresponse~faultless)
boot.ci(results, type="bca") # 95%   ( 0.1281,  0.9546 )   

## SUBJECTIVITY
# PREDICATE
o_agr_pred$subjectivity = s_agr_pred$response[match(o_agr_pred$predicate,s_agr_pred$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$subjectivity) # r = .92, r2 = .85
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.6443,  0.9270 )  
# CLASS
o_agr_class$subjectivity = f_agr_class$response[match(o_agr_class$correctclass,s_agr_class$class)]
gof(o_agr_class$correctresponse,o_agr_class$subjectivity) # r = .94, r2 = .88
results <- boot(data=o_agr_class, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.3321,  0.9657 )   


# plot order preference against faultless
ggplot(o_agr_pred, aes(x=subjectivity,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity")+
  ylab("naturalness\n")+
  #geom_text(aes(label=predicate),color="black")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("~/Documents/git/cocolab/adjective_ordering/writing/long-paper/plots/naturalness-subjectivity-new-nouns.png",height=3,width=3.5)
