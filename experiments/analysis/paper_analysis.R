
setwd("~/Documents/git/cocolab/adjective_ordering/experiments/analysis")

# Bootstrap 95% CI for R-Squared
library(boot)
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 

source("splithalf.R")


######
## Mean results by class plot
######

r = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/results-by-class.csv",header=T)
  
r$expt = factor(r$expt,levels=c("preference","corpus","subjectivity","faultless"))

ggplot(data=r,aes(x=reorder(class,-average,mean),y=average,fill=expt))+
    geom_bar(stat="identity",position=position_dodge(.9),color="black")+
    geom_errorbar(aes(ymin=YMin, ymax=YMax, x=reorder(class,-average,mean), width=0.1),position=position_dodge(.9))+
    xlab("\nadjective class")+
    labs(fill="experiment") +
    ylab("")+
    ylim(0,1)+
    theme_bw()+
    scale_fill_manual(values=c("gray25","gray75","gray50","gray100"))
#ggsave("~/Documents/git/cocolab/adjective_ordering/writing/short-paper/plots/expt_results.pdf",height=2.1,width=6.75)  

######
## Correlation coefficients and CIs
######

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
# combine subjectivity and faultless
sf <- s_agr_pred
sf$faultless = f_agr_pred$response[match(sf$predicate,f_agr_pred$predicate)]
# CLASS compare faultless and subjectivity
gof(s_agr_class$response,f_agr_class$response) # r = .96, r2 = .93
# PREDICATE compare faultless and subjectivity
gof(s_agr_pred$response,f_agr_pred$response) # r = .94, r2 = .89
# get 95% CI
results <- boot(data=sf, statistic=rsq, R=10000, formula=response~faultless)
boot.ci(results, type="bca") # 95%   ( 0.8174,  0.9291 ) 

#load in corpus
c_agr_pred = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/corpus_pred_averages.csv",header=T)
c_agr_class = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/corpus_class_averages.csv",header=T)
#load in naturalness preferences
o_agr_pred = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/average-naturalness.csv",header=T)
# combine naturalness and corpus
oc <- c_agr_pred
oc$naturalness = o_agr_pred$response[match(oc$Adjective,o_agr_pred$predicate1)]
# PREDICATE compare naturalness and corpus distance
gof(oc$naturalness,oc$Distance) # r = .91, r2 = .82
# get 95% CI
results <- boot(data=oc, statistic=rsq, R=10000, formula=naturalness~Distance)
boot.ci(results, type="bca") # 95%   ( 0.6405,  0.9000 )

#####
## compare subjectivity/faultless and naturalness
#####

# load in order preference
o = read.csv("order-preference-trimmed.csv",header=T)
o$class = paste(o$class1,o$class2)
o$predicate = paste(o$predicate1,o$predicate2) # predicate-level estimate
o$workerID = o$workerid + 1
# get Spearman-Brown prophecy (explainable variance)
prophet(splithalf_class(o, 100), 2) # 0.94 class configuration
prophet(splithalf_pred(o, 100), 2) # 0.80 predicate configuration

# CLASS add in faultless difference
o_agr = aggregate(response~class+class1+class2,data=o,mean)
o_agr$class1_f = f_agr_class$response[match(o_agr$class1,f_agr_class$class)]
o_agr$class2_f = f_agr_class$response[match(o_agr$class2,f_agr_class$class)]
o_agr$f_diff = (o_agr$class1_f-o_agr$class2_f)
#compare faultless disagreement with order-preference
gof(o_agr$f_diff,o_agr$response) # r = .9, r2 = .81
results <- boot(data=o_agr, statistic=rsq, R=10000, formula=response~f_diff)
boot.ci(results, type="bca") # 95%   ( 0.6931,  0.8746 )  

# PREDICATE add in faultless difference
o_agr_pred = aggregate(response~predicate+predicate1+predicate2,data=o,mean)
o_agr_pred$pred1_f = f_agr_pred$response[match(o_agr_pred$predicate1,f_agr_pred$predicate)]
o_agr_pred$pred2_f = f_agr_pred$response[match(o_agr_pred$predicate2,f_agr_pred$predicate)]
o_agr_pred$f_diff = (o_agr_pred$pred1_f-o_agr_pred$pred2_f)
#compare faultless disagreement with order-preference
gof(o_agr_pred$f_diff,o_agr_pred$response) # r = .79, r2 = .63
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=response~f_diff)
boot.ci(results, type="bca") # 95%   ( 0.5647,  0.6769 ) 

# CLASS add in subjectivity difference
o_agr$class1_s = s_agr_class$response[match(o_agr$class1,s_agr_class$class)]
o_agr$class2_s = s_agr_class$response[match(o_agr$class2,s_agr_class$class)]
o_agr$s_diff = (o_agr$class1_s-o_agr$class2_s)
#compare subjectivity with order-preference
gof(o_agr$s_diff,o_agr$response) # r = .88, r2 = .77
results <- boot(data=o_agr, statistic=rsq, R=10000, formula=response~s_diff)
boot.ci(results, type="bca") # 95%   ( 0.6419,  0.8570 )

# PREDICATE add in subjectivity difference
o_agr_pred$predicate1_s = s_agr_pred$response[match(o_agr_pred$predicate1,s_agr_pred$predicate)]
o_agr_pred$predicate2_s = s_agr_pred$response[match(o_agr_pred$predicate2,s_agr_pred$predicate)]
o_agr_pred$s_diff = (o_agr_pred$predicate1_s-o_agr_pred$predicate2_s)
#compare subjectivity with order-preference
gof(o_agr_pred$s_diff,o_agr_pred$response) # r = .76, r2 = .57
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=response~s_diff)
boot.ci(results, type="bca") # 95%   ( 0.5045,  0.6249 )

# plot order preference against subjectivity
ggplot(o_agr, aes(x=s_diff,y=response)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity difference")+
  ylab("configuration naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("~/Documents/git/cocolab/adjective_ordering/writing/short-paper/plots/naturalness-subjectivity.pdf",height=3,width=3.5)

# plot order preference against faultless
ggplot(o_agr, aes(x=f_diff,y=response)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nfaultless difference")+
  ylab("configuration naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("~/Documents/git/cocolab/adjective_ordering/writing/short-paper/plots/naturalness-faultless.pdf",height=3,width=3.5)
