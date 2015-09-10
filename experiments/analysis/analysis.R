library(ggplot2)
library(reshape2)
library(lme4)
library(hydroGOF)
library(plyr)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/analysis")

#load in faultless disagreement
f = read.csv("faultless_results.csv",header=T)
head(f)
f_agr_pred = aggregate(response~predicate,data=f,mean)
f_agr_class = aggregate(response~class,data=f,mean)

#load in subjectivity
s = read.csv("subjectivity_results.csv",header=T)
head(s)
s_agr_pred = aggregate(response~predicate,data=s,mean)
s_agr_class = aggregate(response~class,data=s,mean)

# get Spearman-Brown prophecy (explainable variance)
f$workerID = f$workerid + 1
prophet(splithalf(f, 1000), 2) # 0.98
s$workerID = s$workerid + 1
prophet(splithalf(s, 1000), 2) # 0.97

# CLASS compare faultless and subjectivity
gof(s_agr_class$response,f_agr_class$response) # r = .96, r2 = .93

# PREDICATE compare faultless and subjectivity
gof(s_agr_pred$response,f_agr_pred$response) # r = .94, r2 = .89


#plot faultless against subjectivity
fs <- f_agr_pred 
fs$subjectivity = NA
fs$subjectivity = s_agr$response[match(fs$predicate,s_agr$predicate)]
head(fs)
ggplot(fs, aes(x=subjectivity,y=response)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity")+
  ylab("faultless disagreement\n")+
  theme_bw()
ggsave("~/Documents/git/cocolab/adjective_ordering/writing/short-paper/plots/subjectivity-faultless.pdf",height=3,width=3.5)



#load in order-preference
o = read.csv("order-preference-trimmed.csv",header=T)
head(o)
#pred1_agg = bootsSummary(data=o, measurevar="response", groupvars=c("class1"))
#write.csv(pred1_agg,"~/Documents/git/cocolab/adjective_ordering/experiments/analysis/class-naturalness.csv")
pred1_agg = aggregate(response~predicate1*class1,data=o,mean)
head(pred1_agg)
#write.csv(pred1_agg,"~/Documents/git/cocolab/adjective_ordering/experiments/analysis/average-naturalness.csv")
o$class = paste(o$class1,o$class2)
#o$predicate = paste(o$class1,o$class2) # class-level estimate
o$predicate = paste(o$predicate1,o$predicate2) # predicate-level estimate
o$workerID = o$workerid + 1

# get Spearman-Brown prophecy (explainable variance)
prophet(splithalf(o, 100), 2) # 0.94 class configuration

prophet(splithalf(o, 100), 2) # 0.79 predicate configuration

# CLASS add in faultless difference
o_agr = aggregate(response~class+class1+class2,data=o,mean)
o_agr$class1_f = f_agr_class$response[match(o_agr$class1,f_agr_class$class)]
o_agr$class2_f = f_agr_class$response[match(o_agr$class2,f_agr_class$class)]
o_agr$f_diff = (o_agr$class1_f-o_agr$class2_f)
#compare faultless disagreement with order-preference
gof(o_agr$f_diff,o_agr$response) # r = .9, r2 = .81

# PREDICATE add in faultless difference
o_agr_pred = aggregate(response~predicate+predicate1+predicate2,data=o,mean)
o_agr_pred$pred1_f = f_agr_pred$response[match(o_agr_pred$predicate1,f_agr_pred$predicate)]
o_agr_pred$pred2_f = f_agr_pred$response[match(o_agr_pred$predicate2,f_agr_pred$predicate)]
o_agr_pred$f_diff = (o_agr_pred$pred1_f-o_agr_pred$pred2_f)
#compare faultless disagreement with order-preference
gof(o_agr_pred$f_diff,o_agr_pred$response) # r = .79, r2 = .63

# CLASS add in subjectivity difference
o_agr$class1_s = s_agr_class$response[match(o_agr$class1,s_agr_class$class)]
o_agr$class2_s = s_agr_class$response[match(o_agr$class2,s_agr_class$class)]
o_agr$s_diff = (o_agr$class1_s-o_agr$class2_s)
#compare subjectivity with order-preference
gof(o_agr$s_diff,o_agr$response) # r = .88, r2 = .77

# PREDICATE add in subjectivity difference
o_agr_pred$predicate1_s = s_agr_pred$response[match(o_agr_pred$predicate1,s_agr_pred$predicate)]
o_agr_pred$predicate2_s = s_agr_pred$response[match(o_agr_pred$predicate2,s_agr_pred$predicate)]
o_agr_pred$s_diff = (o_agr_pred$predicate1_s-o_agr_pred$predicate2_s)
#compare subjectivity with order-preference
gof(o_agr_pred$s_diff,o_agr_pred$response) # r = .76, r2 = .57


ggplot(o_agr, aes(x=s_diff,y=response)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity difference")+
  ylab("configuration naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
ggsave("~/Documents/git/cocolab/adjective_ordering/writing/short-paper/plots/naturalness-subjectivity.pdf",height=3,width=3.5)

ggplot(o_agr, aes(x=f_diff,y=response)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nfaultless difference")+
  ylab("configuration naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
ggsave("~/Documents/git/cocolab/adjective_ordering/writing/short-paper/plots/naturalness-faultless.pdf",height=3,width=3.5)


################
# compare order preference with corpus


#load in inferred distance from order preference
io = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/inferred_distance_by_adj.csv",header=T)
head(io)
colnames(io) = c("number","predicate","inferred_distance")

# load in mean distance from corpus
c = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/corpus_averages.csv")
head(c)

io$corpus = NA
io$corpus = c$Distance[match(io$predicate,c$Adjective)]
head(io)

io$pred1 = NA
io$pred1 = pred1_agg$response[match(io$predicate,pred1_agg$predicate1)]
head(io)



gof(io$inferred_distance,io$corpus) # r2 = .72

gof(io$corpus,io$pred1) # r2 = .84

ggplot(io, aes(x=pred1,y=corpus)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\naverage rating")+
  ylab("corpus distance\n")+
  theme_bw()
ggsave("~/Documents/git/cocolab/adjective_ordering/writing/short-paper/plots/corpus-naturalness.pdf",height=3,width=3.5)
