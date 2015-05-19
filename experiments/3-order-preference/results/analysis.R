library(ggplot2)
library(reshape2)
library(lme4)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/3-order-preference/Submiterator-master")

d = read.table("order-preference-trials.tsv",sep="\t",header=T)
head(d)

summary(d)

d$configuration = paste(d$class1,d$class2)

aggregate(response~configuration,data=d,mean)

## get faultles disagreement ratings

f = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/2-faultless-disagreement/Submiterator-master/faultless-disagreement-2-trials.tsv",sep="\t",header=T)
head(f)
f_agr = aggregate(response~class,data=f,mean)
p_agr = aggregate(response~predicate,data=f,mean)

d$class1_f = f_agr$response[match(d$class1,f_agr$class)]
d$class2_f = f_agr$response[match(d$class2,f_agr$class)]

d$pred1_f = p_agr$response[match(d$predicate1,p_agr$predicate)]
d$pred2_f = p_agr$response[match(d$predicate2,p_agr$predicate)]

d$f_ratio = (d$class1_f/d$class2_f)
d$f_diff = (d$class1_f-d$class2_f)
d$p_ratio = (d$pred1_f/d$pred2_f)
d$p_diff = (d$pred1_f-d$pred2_f)

## by class plot

d_s = bootsSummary(data=d, measurevar="response", groupvars=c("f_diff"))

d_s = aggregate(response~f_diff,data=d,mean)

ggplot(d_s, aes(x=f_diff,y=response)) +
  geom_point() +
  ylab("acceptability") +
  xlab("faultless disagreement") +
  ggtitle("by-class plot")
  #geom_smooth()
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=f_diff, width=0.1),position=position_dodge(width=0.9))+
  #geom_abline(slope=1,intercept=0.5)
ggsave("../results/class_plot.pdf")

#ggplot(d, aes(x=f_diff,y=response)) +
 #        geom_point() +
  #geom_smooth()


## by predicate plot

#ggplot(d, aes(x=p_diff,y=response)) +
#  geom_point() +
#  geom_smooth()

p_s = bootsSummary(data=d, measurevar="response", groupvars=c("p_diff"))

ggplot(p_s, aes(x=p_diff,y=response)) +
  geom_point() +
  ylab("acceptability") +
  xlab("faultless disagreement") +
  ggtitle("by-predicate plot")
ggsave("../results/pred_plot.pdf")
