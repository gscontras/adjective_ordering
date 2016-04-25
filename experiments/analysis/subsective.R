library(MuMIn)
library(hydroGOF)
library(ggplot2)

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

#load in naturalness preferences
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/naturalness-duplicated.csv",header=T)
head(o)
o_agr_pred = aggregate(correctresponse~predicate*correctclass,data=o,mean)
o_agr_class = aggregate(correctresponse~correctclass,data=o,mean)
head(o_agr_pred)

#load in subsectivity
si = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/subsective-set1.csv",header=T)
#si$subsective <- as.factor(si$subsective)

o_agr_pred$subsective = si$subsective[match(o_agr_pred$predicate,si$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$subsective) # r = 0.91 r2 = 0.82
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subsective)
boot.ci(results, type="bca") # 95%   ( 0.6989,  0.8914 )  

subs.m = lm(correctresponse~as.factor(subsective),data=o_agr_pred)
summary(subs.m) # r2=0.82

#load in faultless disagreement
f = read.csv("faultless_results.csv",header=T)
head(f)
f_agr_pred = aggregate(response~predicate,data=f,mean)
f_agr_class = aggregate(response~class,data=f,mean)

o_agr_pred$faultless = f_agr_pred$response[match(o_agr_pred$predicate,f_agr_pred$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$faultless) # r = .94, r2 = .88
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~faultless)
boot.ci(results, type="bca") # 95%   ( 0.7713,  0.9479 )  

#load in subjectivity
s = read.csv("subjectivity_results.csv",header=T)
head(s)
s_agr_pred = aggregate(response~predicate,data=s,mean)
s_agr_class = aggregate(response~class,data=s,mean)

o_agr_pred$subjectivity = s_agr_pred$response[match(o_agr_pred$predicate,s_agr_pred$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$subjectivity) # r = .90, r2 = .81
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.6818,  0.8865 ) 

subj.m = lm(correctresponse~subjectivity,data=o_agr_pred)
summary(subj.m)

#load in inherentness
inh = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/14-inherentness/results/inherentness.csv",header=T)
inh_agr = aggregate(response~predicate,inh,mean)
o_agr_pred$inherentness = inh_agr$response[match(o_agr_pred$predicate,inh_agr$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$inherentness) # r = -0.04 r2 = 0.00
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~inherentness)
boot.ci(results, type="bca") # 95%   ( 0.000,  0.021 )  

#load in subsective-inherentness
sub2 = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/14-inherentness/results/subjectivity.csv",header=T)
sub2_agr = aggregate(response~predicate,sub2,mean)
o_agr_pred$subjectivity2 = sub2_agr$response[match(o_agr_pred$predicate,sub2_agr$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$subjectivity2) # r = 0.87 r2 = 0.75
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subjectivity2)
boot.ci(results, type="bca") # 95%   ( 0.5321,  0.8352 )  

# split subsective and intersective adjectives
subs = o_agr_pred[o_agr_pred$subsective==1,]
gof(subs$correctresponse,subs$subjectivity) # r = 0.29, r2 = 0.08
int = o_agr_pred[o_agr_pred$subsective==0,]
gof(int$correctresponse,int$subjectivity) # r = 0.54, r2 = 0.29

#compare subsective with faultless and subjectivity
gof(o_agr_pred$subsective,o_agr_pred$faultless) # r = .93, r2 = .86
gof(o_agr_pred$subsective,o_agr_pred$subjectivity) # r = .94, r2 = .89

m.0 = lm(correctresponse~subjectivity+as.factor(subsective),data=o_agr_pred)
m.1 = lm(correctresponse~as.factor(subsective),data=o_agr_pred)
anova(m.1,m.0,test="Chisq")

# plot order preference against subsectivity
ggplot(o_agr_pred, aes(x=subsective,y=subjectivity)) +
#ggplot(o_agr_pred, aes(x=subjectivity,y=correctresponse)) +
#ggplot(o_agr_pred, aes(x=subjectivity2,y=correctresponse)) +
#ggplot(o_agr_pred, aes(x=inherentness,y=correctresponse)) +
#ggplot(o_agr_pred, aes(x=faultless,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  #xlab("\nsubsectivity")+
  #ylab("subjectivity\n")+
  #xlab("\nsubjectivity")+
  xlab("\ninherentness")+
  ylab("naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/subsective/expt1-inherentness-naturalness.pdf",height=3,width=3.5)

#model comparison
o$subsective = si$subsective[match(o$predicate,si$predicate)]
#o$faultless = f_agr_pred$response[match(o$predicate,f_agr_pred$predicate)]
o$subjectivity = s_agr_pred$response[match(o$predicate,s_agr_pred$predicate)]

gof(o$correctresponse,o$subsective)
o.subs = lm(correctresponse~as.factor(subsective),data=o)
summary(o.subs) #r2=0.28
gof(o$correctresponse,o$subjectivity)
o.subj = lm(correctresponse~subjectivity,data=o)
summary(o.subj) #r2=0.27

# split subsective and intersective adjectives
subs = o[o$subsective==1,]
summary(lm(correctresponse~subjectivity,data=subs)) ## **
summary(lmer(correctresponse~subjectivity+(1|workerid),data=subs)) ## **
int = o[o$subsective==0,]
summary(lm(correctresponse~subjectivity,data=int)) ## ***
summary(lmer(correctresponse~subjectivity+(1|workerid),data=int)) ## ***

#m.0 = glm(correctresponse~subsective+faultless+subjectivity,data=o)
#m.1 = glm(correctresponse~faultless+subjectivity,data=o)
m.2 = lm(correctresponse~as.factor(subsective)+subjectivity,data=o)
#m.3 = glm(correctresponse~subsective+faultless,data=o)
m.4 = lm(correctresponse~as.factor(subsective),data=o)
#m.5 = lm(correctresponse~subjectivity,data=o)
#m.6 = glm(correctresponse~faultless,data=o)

r.squaredGLMM(m.4) # 0.2761397
#r.squaredGLMM(m.5) # 0.2725833
#r.squaredGLMM(m.6) # 0.2934775

#anova(m.0,m.3)
#anova(m.5,m.2)
#anova(m.3,m.6)
anova(m.4,m.2)

# plot order preference against subsectivity
ggplot(o, aes(x=subsective,y=correctresponse)) +
  #ggplot(o, aes(x=subjectivity,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubsectivity")+
  #ylab("subjectivity\n")+
  #xlab("\nsubjectivity")+
  ylab("naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()

#######
## configuration analysis
#######

#load in naturalness preferences
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/naturalness-configuration-duplicated.csv",header=T)
head(o)
o_agr_pred = aggregate(correctresponse~correct_configuration*correctpred1*correctpred2,data=o,mean)
#o_agr_class = aggregate(correctresponse~correctclass,data=o,mean)
head(o_agr_pred)

#load in subsectivity
si = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/subsective-set1.csv",header=T)

o_agr_pred$subs1 = si$subsective[match(o_agr_pred$correctpred1,si$predicate)]
o_agr_pred$subs2 = si$subsective[match(o_agr_pred$correctpred2,si$predicate)]
o_agr_pred$subs_diff = o_agr_pred$subs1 - o_agr_pred$subs2
#gof(o_agr_pred$correctresponse,o_agr_pred$subs_diff) 
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subs_diff) # r2 = 0.63
boot.ci(results, type="bca") # 95%   ( 0.5816,  0.6732 )   

#load in subjectivity
s = read.csv("subjectivity_results.csv",header=T)
head(s)
s_agr_pred = aggregate(response~predicate,data=s,mean)
#s_agr_class = aggregate(response~class,data=s,mean)

o_agr_pred$subj1 = s_agr_pred$response[match(o_agr_pred$correctpred1,s_agr_pred$predicate)]
o_agr_pred$subj2 = s_agr_pred$response[match(o_agr_pred$correctpred2,s_agr_pred$predicate)]
o_agr_pred$subj_diff = o_agr_pred$subj1 - o_agr_pred$subj2
gof(o_agr_pred$correctresponse,o_agr_pred$subj_diff) # r = .80, r2 = .64
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subj_diff)
boot.ci(results, type="bca") # 95%   ( 0.5901,  0.6765 ) 

#compare subsective with subjectivity
#gof(o_agr_pred$subj_diff,o_agr_pred$subs_diff) # 
boot(data=o_agr_pred, statistic=rsq, R=10000, formula=subj_diff~subs_diff) # r2 = 0.8927097

# plot order preference against subsectivity
ggplot(o_agr_pred, aes(x=subs_diff,y=correctresponse)) +
  #ggplot(o_agr_pred, aes(x=subj_diff,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubsectivity difference")+
  ylab("naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("~/Documents/git/cocolab/adjective_ordering/writing/short-paper/plots/naturalness-subjectivity-new.pdf",height=3,width=3.5)






###################################
##### NEW NOUNS ####################
###################################

#load in order preference
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/10-order-preference-noun/Submiterator-master/naturalness-duplicated.csv",header=T)
head(o)
o_agr_pred = aggregate(correctresponse~predicate*correctclass,data=o,mean)
#o_agr_class = aggregate(correctresponse~correctclass,data=o,mean)
head(o_agr_pred)

#load in subsectivity
si = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/subsective-set1.csv",header=T)
#si$subsective <- as.factor(si$subsective)
o_agr_pred$subsective = si$subsective[match(o_agr_pred$predicate,si$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$subsective) # r = 0.92 r2 = 0.85
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subsective)
boot.ci(results, type="bca") # 95%   ( 0.7249,  0.9122 )   

#load in subjectivity
setwd("~/Documents/git/cocolab/adjective_ordering/experiments/analysis")
s = read.csv("subjectivity_no-noun_results.csv",header=T)
s_agr_pred = aggregate(response~predicate,data=s,mean)
#s_agr_class = aggregate(response~class,data=s,mean)
o_agr_pred$subjectivity = s_agr_pred$response[match(o_agr_pred$predicate,s_agr_pred$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$subjectivity) # r = .92, r2 = .85
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.7249,  0.9122 )  

# split subsective and intersective adjectives
subs = o_agr_pred[o_agr_pred$subsective==1,]
gof(subs$correctresponse,subs$subjectivity) # r = 0.39, r2 = 0.15
int = o_agr_pred[o_agr_pred$subsective==0,]
gof(int$correctresponse,int$subjectivity) # r = 0.13, r2 = 0.02

#compare subsectivity and subjectivity
gof(o_agr_pred$subsective,o_agr_pred$subjectivity) # r = .97, r2 = .93

# plot order preference against subsectivity
ggplot(o_agr_pred, aes(x=subsective,y=subjectivity)) +
  #ggplot(o_agr_pred, aes(x=subjectivity,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubsectivity")+
  ylab("subjectivity\n")+
  #xlab("\nsubjectivity")+
  #ylab("naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/subsective/expt2-subsective-subjective.pdf",height=3,width=3.5)

#model comparison
o$subsective = si$subsective[match(o$predicate,si$predicate)]
#o$faultless = f_agr_pred$response[match(o$predicate,f_agr_pred$predicate)]
o$subjectivity = s_agr_pred$response[match(o$predicate,s_agr_pred$predicate)]

gof(o$correctresponse,o$subsective)
o.subs = lm(correctresponse~as.factor(subsective),data=o)
summary(o.subs) #r2=0.25
gof(o$correctresponse,o$subjectivity)
o.subj = lm(correctresponse~subjectivity,data=o)
summary(o.subj) #r2=0.25

# split subsective and intersective adjectives
subs = o[o$subsective==1,]
summary(lm(correctresponse~subjectivity,data=subs)) ## ***
summary(lmer(correctresponse~subjectivity+(1|workerid),data=subs)) ## ***
int = o[o$subsective==0,]
summary(lm(correctresponse~subjectivity,data=int)) ## not significant
summary(lmer(correctresponse~subjectivity+(1|workerid),data=int)) ## .






###################################
##### NEW ADJECTIVES ####################
###################################

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/13-subjectivity-expanded")
s = read.csv("results/subjectivity-expanded_results.csv",header=T)
head(s)
s_agr_pred = aggregate(response~predicate,data=s,mean)
#load in naturalness preferences
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/12-order-preference-expanded/Submiterator-master/order-preference-duplicated.csv",header=T)
head(o)
nrow(o) #28380 
o$string = paste(o$correctpred1,o$correctpred2,o$noun)
o <- o[o$makes_sense=="yes",]
nrow(o) #23790
length(unique(o$string)) #23488
length(unique(o$correct_configuration)) #5478
o_agr_pred = aggregate(correctresponse~correctpred1*correctclass1,data=o,mean)
colnames(o_agr_pred) <- c("predicate","class","correctresponse")

#load in subsectivity
si = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/subsective-set2.csv",header=T)

o_agr_pred$subsective = si$subsective[match(o_agr_pred$predicate,si$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$subsective) # r = 0.75, r2 = 0.56
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subsective)
boot.ci(results, type="bca") # 95%   ( 0.4056,  0.6741 )

o_agr_pred$subsectiveF = si$subsectiveF[match(o_agr_pred$predicate,si$predicate)]
o.m = lm(correctresponse~subsectiveF,data=o_agr_pred)
summary(o.m) # r2 = 0.53

#load in subjectivity
s = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/13-subjectivity-expanded/results/subjectivity-expanded_results.csv",header=T)
head(s)
s_agr_pred = aggregate(response~predicate,data=s,mean)

o_agr_pred$subjectivity = s_agr_pred$response[match(o_agr_pred$predicate,s_agr_pred$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$subjectivity) # r = 0.72 r2 = 0.51
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.2888,  0.6064 ) 
o.s = lm(correctresponse~subjectivity,data=o_agr_pred)
summary(o.s) # r2 = 0.51

# split subsective and intersective adjectives
subs = o_agr_pred[o_agr_pred$subsectiveF=="subsective",]
gof(subs$correctresponse,subs$subjectivity) # r = 0.47, r2 = 0.22
int = o_agr_pred[o_agr_pred$subsectiveF=="intersective",]
gof(int$correctresponse,int$subjectivity) # r = 0.37, r2 = 0.14
oth = o_agr_pred[o_agr_pred$subsectiveF=="other",]
gof(oth$correctresponse,oth$subjectivity) # r = 0.05, r2 = 0.00

#compare subsective and intersective
ss.m = lm(subjectivity~subsectiveF,data=o_agr_pred)
summary(ss.m) # r2 = 0.52

# plot order preference against subsectivity
ggplot(o_agr_pred, aes(x=subsectiveF,y=subjectivity)) +
  #ggplot(o_agr_pred, aes(x=subjectivity,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubsectivity")+
  ylab("subjectivity\n")+
  #xlab("\nsubjectivity")+
  #ylab("naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/subsective/expt3-subsective-subjective.pdf",height=3,width=3.5)


#model comparison
o$subsective = si$subsective[match(o$correctpred1,si$predicate)]
o$subsectiveF = si$subsectiveF[match(o$correctpred1,si$predicate)]
o$subjectivity = s_agr_pred$response[match(o$correctpred1,s_agr_pred$predicate)]

m.0 = lm(correctresponse~subsectiveF+subjectivity,data=o)
m.1 = lm(correctresponse~subsectiveF,data=o)
m.2 = lm(correctresponse~subsective+subjectivity,data=o)
m.4 = lm(correctresponse~subsective,data=o)
m.5 = lm(correctresponse~subjectivity,data=o)

r.squaredGLMM(m.0) # 0.1106374
r.squaredGLMM(m.1) # 0.09711561
r.squaredGLMM(m.4) # 0.1044312
r.squaredGLMM(m.5) # 0.09250512

anova(m.0,m.1)
anova(m.0,m.5)
anova(m.4,m.2)
anova(m.2,m.5)
summary(m.2)
summary(m.4)

# split subsective and intersective adjectives
subs = o[o$subsectiveF=="subsective",]
summary(lm(correctresponse~subjectivity,data=subs)) ## ***
summary(lmer(correctresponse~subjectivity+(1|workerid),data=subs)) ## ***
int = o[o$subsectiveF=="intersective",]
summary(lm(correctresponse~subjectivity,data=int)) ## ***
summary(lmer(correctresponse~subjectivity+(1|workerid),data=int)) ## ***
oth = o[o$subsectiveF=="other",]
summary(lm(correctresponse~subjectivity,data=oth)) ## not significant
summary(lmer(correctresponse~subjectivity+(1|workerid),data=oth)) ## not significant



