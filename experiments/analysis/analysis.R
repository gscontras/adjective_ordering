library(ggplot2)
library(reshape2)
library(lme4)
library(hydroGOF)
library(plyr)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/analysis")

#load in faultless disagreement
f = read.csv("faultless_results.csv",header=T)
head(f)
f_agr = aggregate(response~predicate+class,data=f,mean)

#load in subjectivity
s = read.csv("subjectivity_results.csv",header=T)
head(s)
s_agr = aggregate(response~predicate+class,data=s,mean)

# get Spearman-Brown prophecy (explainable variance)
f$workerID = f$workerid + 1
prophet(splithalf(f, 1000), 2) # 0.98
s$workerID = s$workerid + 1
prophet(splithalf(s, 1000), 2) # 0.97

#compare faultless and subjectivity
gof(s_agr$response,f_agr$response) # r = .94, r2 = .89

#plot faultless against subjectivity
fs <- f_agr 
fs$subjectivity = NA
fs$subjectivity = s_agr$response[match(fs$predicate,s_agr$predicate)]
head(fs)
ggplot(fs, aes(x=response,y=subjectivity)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab("\nfaultless disagreement")+
  ylab("subjectivity\n")+
  theme_bw()



#load in order-preference
o = read.csv("order-preference_results.csv",header=T)
head(o)
o$class = paste(o$class1,o$class2)
o$predicate = paste(o$class1,o$class2)
#o$predicate = paste(o$predicate1,o$predicate2)
o$workerID = o$workerid + 1

# get Spearman-Brown prophecy (explainable variance)
prophet(splithalf(o, 1000), 2) # 0.94

# add in faultless difference
o_agr = aggregate(response~class+class1+class2,data=o,mean)
o_agr$class1_f = f_agr$response[match(o_agr$class1,f_agr$class)]
o_agr$class2_f = f_agr$response[match(o_agr$class2,f_agr$class)]
o_agr$f_diff = (o_agr$class1_f-o_agr$class2_f)
#compare faultless disagreement with order-preference
gof(o_agr$f_diff,o_agr$response) # r = .91, r2 = .82

# add in subjectivity difference
o_agr$class1_s = s_agr$response[match(o_agr$class1,s_agr$class)]
o_agr$class2_s = s_agr$response[match(o_agr$class2,s_agr$class)]
o_agr$s_diff = (o_agr$class1_s-o_agr$class2_s)
#compare subjectivity with order-preference
gof(o_agr$s_diff,o_agr$response) # r = .91, r2 = .83

ggplot(o_agr, aes(x=f_diff,y=response)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab("\nfaultless disagreement")+
  ylab("acceptability\n")+
  theme_bw()
