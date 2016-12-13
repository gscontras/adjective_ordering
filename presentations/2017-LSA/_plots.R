library(hydroGOF)
library(ggplot2)
require(MuMIn)


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
source("helpers.R")



######
## Original experiment plots
######

#load in naturalness preferences
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/naturalness-duplicated.csv",header=T)
head(o)
o_agr_pred = aggregate(correctresponse~predicate*correctclass,data=o,mean)
head(o_agr_pred)
#load in subjectivity
s = read.csv("subjectivity_no-noun_results.csv",header=T)
head(s)
s_agr_pred = aggregate(response~predicate,data=s,mean)
s_agr_class = aggregate(response~class,data=s,mean)
# combine subjectivity and faultless
sf <- s_agr_pred
sf$faultless = f_agr_pred$response[match(sf$predicate,f_agr_pred$predicate)]
# CLASS compare faultless and subjectivity
gof(s_agr_class$response,f_agr_class$response) # r = .98, r2 = .95
# PREDICATE compare faultless and subjectivity
gof(s_agr_pred$response,f_agr_pred$response) # r = .96, r2 = .91
# get 95% CI
#results <- boot(data=sf, statistic=rsq, R=10000, formula=response~faultless)
#boot.ci(results, type="bca") # 95%   ( 0.8649,  0.9429 )  

#####
## compare subjectivity/faultless and naturalness
#####

# PREDICATE
## SUBJECTIVITY
# PREDICATE
o_agr_pred$subjectivity = s_agr_pred$response[match(o_agr_pred$predicate,s_agr_pred$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$subjectivity) # r = .92, r2 = .85
#results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
#boot.ci(results, type="bca") # 95%   ( 0.7525,  0.9026 ) 
# CLASS
o_agr_class$subjectivity = s_agr_class$response[match(o_agr_class$correctclass,s_agr_class$class)]
gof(o_agr_class$correctresponse,o_agr_class$subjectivity) # r = .92, r2 = .85
#results <- boot(data=o_agr_class, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
#boot.ci(results, type="bca") # 95%   ( 0.2336,  0.9608 ) 
# plot order preference against subjectivity
ggplot(o_agr_pred, aes(x=subjectivity,y=correctresponse)) +
  geom_point(color="black") +
  geom_smooth(method=lm,color="black") +
  #geom_text(aes(label=predicate))+
  xlab("\nsubjectivity score")+
  ylab("preferred distance from noun\n")+
  ylim(0,1)+
  #xlim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
  #theme_blackDisplay()
#ggsave("~/Dropbox/Jobs/2015-2016/San Diego/talk/adjective-order-plots/subjectivity-no-noun.pdf",height=3,width=3.5)


expt1 <- o_agr_pred


###################################
##### NEW ADJECTIVES ####################
###################################

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/13-subjectivity-expanded")
s = read.csv("results/subjectivity-expanded_results.csv",header=T)
head(s)
s_agr_pred = aggregate(response~predicate,data=s,mean)
s_agr_class = aggregate(response~class,data=s,mean)
#load in naturalness preferences
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/12-order-preference-expanded/Submiterator-master/order-preference-duplicated.csv",header=T)
head(o)
nrow(o) #28380 
o$string = paste(o$correctpred1,o$correctpred2,o$noun)
o <- o[o$makes_sense=="yes",]
nrow(o) #23790
length(unique(o$string)) #23488
length(unique(o$correct_configuration)) #5478
o_no_sup <- o[o$correctpred1!="best"&o$correctpred1!="biggest"&o$correctpred1!="closest"&o$correctpred1!="last"&o$correctpred2!="best"&o$correctpred2!="biggest"&o$correctpred2!="closest"&o$correctpred2!="last",]
o_no_sup_pred = aggregate(correctresponse~correctpred1*correctclass1,data=o_no_sup,mean)
o_agr_pred = aggregate(correctresponse~correctpred1*correctclass1,data=o,mean)
o_agr_class = aggregate(correctresponse~correctclass1,data=o,mean)
head(o_agr_pred)
# combined preference subjectivity plot
## LINEAR REGRESSION TO FIND OUTLIERS
o_agr_pred$subjectivity = s_agr_pred$response[match(o_agr_pred$correctpred1,s_agr_pred$predicate)]
m = glm(correctresponse~subjectivity,data=o_agr_pred)
summary(m)
r.squaredGLMM(m) #0.51
o_agr_pred$Predicted = fitted(m)
o_agr_pred$Diff = abs(o_agr_pred$correctresponse - o_agr_pred$Predicted)
(2*sd(o_agr_pred$Diff)) #0.1545926
o_agr_pred$outlier = F
o_agr_pred[o_agr_pred$Diff>0.1545926,]$outlier = T
table(o_agr_pred$outlier)
o_agr_pred[o_agr_pred$outlier==T,]$correctpred1
gof(o_agr_pred[o_agr_pred$outlier==F,]$correctresponse,o_agr_pred[o_agr_pred$outlier==F,]$subjectivity) #r2 = 0.74
ggplot(o_agr_pred, aes(x=Predicted,y=correctresponse,color=outlier)) +
  #geom_point(size=1) +
  geom_text(aes(label=correctpred1),size=2)+
  ylab("naturalness\n")+
  xlab("\npredicted naturalness")+
  theme_bw()
#ggsave("results/naturalness-subjectivity-outliers.pdf",height=4,width=5.5)
# partially-labeled plot
o_agr_pred$text = o_agr_pred$correctpred1
o_agr_pred[o_agr_pred$correctpred1!="best"&o_agr_pred$correctpred1!="biggest"&o_agr_pred$correctpred1!="closest"&o_agr_pred$correctpred1!="last"&o_agr_pred$correctpred1!="daily"&o_agr_pred$correctpred1!="current"&o_agr_pred$correctpred1!="solid"&o_agr_pred$correctpred1!="entrepreneurial",]$text <- NA
o_agr_pred$bad = TRUE
o_agr_pred[o_agr_pred$correctpred1!="best"&o_agr_pred$correctpred1!="biggest"&o_agr_pred$correctpred1!="closest"&o_agr_pred$correctpred1!="last"&o_agr_pred$correctpred1!="daily"&o_agr_pred$correctpred1!="current"&o_agr_pred$correctpred1!="solid"&o_agr_pred$correctpred1!="entrepreneurial",]$bad <- FALSE
ggplot(o_agr_pred[o_agr_pred$correctpred1!="best"&o_agr_pred$correctpred1!="biggest"&o_agr_pred$correctpred1!="closest"&o_agr_pred$correctpred1!="last"&o_agr_pred$correctpred1!="daily"&o_agr_pred$correctpred1!="current"&o_agr_pred$correctpred1!="solid"&o_agr_pred$correctpred1!="entrepreneurial",], aes(x=subjectivity,y=correctresponse)) +
  geom_point(color="black") +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity score")+
  ylab("preferred distance from noun\n")+
  ylim(0,1)+
  #xlim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#theme_blackDisplay()
#ggsave("~/Dropbox/Jobs/2015-2016/San Diego/talk/adjective-order-plots/subjectivity-new-adjectives.pdf",height=3,width=3.5)

expt2 <- o_agr_pred[o_agr_pred$correctpred1!="best"&o_agr_pred$correctpred1!="biggest"&o_agr_pred$correctpred1!="closest"&o_agr_pred$correctpred1!="last"&o_agr_pred$correctpred1!="daily"&o_agr_pred$correctpred1!="current"&o_agr_pred$correctpred1!="solid"&o_agr_pred$correctpred1!="entrepreneurial",]

expt1$expt="Expt. 1 (26 adjectives)"
expt1 = subset(expt1, select=c("predicate", "correctclass","correctresponse","subjectivity","expt"))
expt2$expt="Expt. 2 (70 adjectives)"
expt2 = subset(expt2, select=c("correctpred1", "correctclass1","correctresponse","subjectivity","expt"))
colnames(expt2) <- c("predicate", "correctclass","correctresponse","subjectivity","expt")
expts = rbind(expt1,expt2)

ggplot(expts, aes(x=subjectivity,y=correctresponse)) +  
  geom_point(color="black",size=0.5) +
  geom_smooth(method=lm,color="black",size=0.5) +
  xlab("\nsubjectivity score")+
  ylab("preferred distance from noun\n")+
  ylim(0,1)+
  #xlim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()+
  facet_wrap( ~expt, ncol=1)
#ggsave("~/Documents/git/cocolab/adjective_ordering/presentations/LSA/plots/expts1-2.png",width=2.3,height=3.3)






#####################
##### SUBSECTIVITY
#####################

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/analysis")

#load in naturalness preferences
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/naturalness-duplicated.csv",header=T)
head(o)
o_agr_pred = aggregate(correctresponse~predicate*correctclass,data=o,mean)
head(o_agr_pred)
#load in subsectivity
si = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/subsective-set1.csv",header=T)
#si$subsective <- as.factor(si$subsective)
o_agr_pred$subsective = si$subsective[match(o_agr_pred$predicate,si$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$subsective) # r = 0.91 r2 = 0.82
#subs.m = lm(correctresponse~as.factor(subsective),data=o_agr_pred)
#summary(subs.m) # r2=0.82
#load in subjectivity
s = read.csv("subjectivity_no-noun_results.csv",header=T)
head(s)
s_agr_pred = aggregate(response~predicate,data=s,mean)
#s_agr_class = aggregate(response~class,data=s,mean)
o_agr_pred$subjectivity = s_agr_pred$response[match(o_agr_pred$predicate,s_agr_pred$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$subjectivity) # r = .92, r2 = .85
#results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
#boot.ci(results, type="bca") # 95%   ( 0.6818,  0.8865 ) 
subj.m = lm(correctresponse~subjectivity,data=o_agr_pred)
summary(subj.m)
#load in inherentness
inh = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/14-inherentness/results/inherentness.csv",header=T)
inh$naturalness = o_agr_pred$correctresponse[match(o_agr_pred$predicate,inh$predicate)]
inh_agr = aggregate(response~predicate,inh,mean)
o_agr_pred$inherentness = inh_agr$response[match(o_agr_pred$predicate,inh_agr$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$inherentness) # r = -0.04 r2 = 0.00
#results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~inherentness)
#boot.ci(results, type="bca") # 95%   ( 0.000,  0.021 )  

o_agr_pred$subs = "intersective"
o_agr_pred[o_agr_pred$subsective==1,]$subs = "subsective"

# plot order preference against subsectivity and subjectivity
ggplot(o_agr_pred, aes(x=subjectivity,y=correctresponse,shape=subs,color=subs,linetype=subs)) +
  #ggplot(o_agr_pred, aes(x=subjectivity2,y=correctresponse)) +
  #ggplot(o_agr_pred, aes(x=inherentness,y=correctresponse)) +
  #ggplot(o_agr_pred, aes(x=faultless,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm) +
  #xlab("\nsubsectivity")+
  #ylab("subjectivity\n")+
  xlab("\nsubjectivity")+
  #xlab("\ninherentness")+
  ylab("naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  scale_colour_manual(values=c("red","blue"))+
  scale_linetype_manual(values=c("solid","dashed"))+
  theme_bw()+
  theme(legend.title=element_blank())
#ggsave("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/subsective/expt1-subjectivity-subsectivity.pdf",height=3,width=4.5)

expt1 <- o_agr_pred
expt1$expt = "Expt. 1 adjectives"

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
#results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subsective)
#boot.ci(results, type="bca") # 95%   ( 0.4056,  0.6741 )

o_agr_pred$subsectiveF = si$subsectiveF[match(o_agr_pred$predicate,si$predicate)]
o.m = lm(correctresponse~subsectiveF,data=o_agr_pred)
summary(o.m) # r2 = 0.53

#load in subjectivity
s = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/13-subjectivity-expanded/results/subjectivity-expanded_results.csv",header=T)
head(s)
s_agr_pred = aggregate(response~predicate,data=s,mean)

o_agr_pred$subjectivity = s_agr_pred$response[match(o_agr_pred$predicate,s_agr_pred$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$subjectivity) # r = 0.72 r2 = 0.51
#results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
#boot.ci(results, type="bca") # 95%   ( 0.2888,  0.6064 ) 
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
#results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=subjectivity~subsectiveF)
#boot.ci(results, type="bca") # 95%   ( 0.3474,  0.6477 ) 


expt2 <- o_agr_pred[o_agr_pred$predicate!="best"&o_agr_pred$predicate!="biggest"&o_agr_pred$predicate!="closest"&o_agr_pred$predicate!="last"&o_agr_pred$predicate!="daily"&o_agr_pred$predicate!="current"&o_agr_pred$predicate!="solid"&o_agr_pred$predicate!="entrepreneurial",]
expt2$expt = "Expt. 2 adjectives"

expt1 = subset(expt1, select=c("predicate", "correctclass","correctresponse","subsective","subjectivity","subs","expt"))
expt2 = subset(expt2, select=c("predicate", "class","correctresponse","subsective","subjectivity","subsectiveF","expt"))
colnames(expt2) <- c("predicate", "correctclass","correctresponse","subsective","subjectivity","subs","expt")
expts = rbind(expt1,expt2)


# plot order preference against subsectivity and subjectivity
ggplot(expts, aes(x=subjectivity,y=correctresponse,linetype=subs,shape=subs,color=subs)) +
  #ggplot(o_agr_pred, aes(x=subjectivity2,y=correctresponse)) +
  #ggplot(o_agr_pred, aes(x=inherentness,y=correctresponse)) +
  #ggplot(o_agr_pred, aes(x=faultless,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm) +
  #xlab("\nsubsectivity")+
  #ylab("subjectivity\n")+
  xlab("subjectivity score")+
  #xlab("\ninherentness")+
  ylab("preferred distance from noun\n")+
  ylim(0,1)+
  #xlim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  scale_colour_manual(values=c("red","darkgreen","blue"))+
  scale_linetype_manual(values=c("solid","dotted","dashed"))+
  scale_shape_manual(values=c(16,15,17))+
  theme_bw()+
  theme(legend.title=element_blank())+
  facet_wrap( ~expt, ncol=1)+
  theme(legend.position="bottom",legend.direction="vertical",legend.margin=unit(0,"cm"))
#ggsave("~/Documents/git/cocolab/adjective_ordering/presentations/LSA/plots/subsective.png",width=2.3,height=4)







#################
##### inherentness and concept formability
################


### get expt1 from subsectivity above
expt1inh <- expt1

#### add in concept formability
setwd("~/Documents/git/cocolab/adjective_ordering/experiments/9-concept-formability/Submiterator-master")
d = read.table("concept-formability-trials.tsv",sep="\t",header=T)
head(d)
s = read.table("concept-formability-subject_information.tsv",sep="\t",header=T)
head(s)
d$language = s$language[match(d$workerid,s$workerid)]
summary(d)
d = dcast(data=d, workerid + trial + adjective + noun + class + nounclass + language ~ predicate, value.var="response",mean)
colnames(d) = c("workerid","trial","predicate","noun","class","nounclass","language","adj_response","noun_response")
length(unique(d$workerid)) # n=40
## nouns
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/naturalness-duplicated.csv",header=T)
head(o)
o_agr_pred = aggregate(correctresponse~predicate*correctclass,data=o,mean)
o_agr_class = aggregate(correctresponse~correctclass,data=o,mean)
head(d)
d_agr_pred = aggregate(noun_response~predicate,data=d,mean)
d_agr_class = aggregate(noun_response~class,data=d,mean)
# PREDICATE
o_agr_pred$concept = d_agr_pred$noun_response[match(o_agr_pred$predicate,d_agr_pred$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$concept) # r = 0.60, r2 = 0.36
#results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~concept)
#boot.ci(results, type="bca") # 95%   ( 0.0715,  0.6213 )    
# CLASS
o_agr_class$concept = d_agr_class$noun_response[match(o_agr_class$correctclass,d_agr_class$class)]
gof(o_agr_class$correctresponse,o_agr_class$concept) # r = 0.82, r2 = 0.67
#results <- boot(data=o_agr_class, statistic=rsq, R=10000, formula=correctresponse~concept)
#boot.ci(results, type="bca") # 95%   ( 0.0006,  0.8949 )    
# plot order preference against subjectivity
######
## with noun info
######
o_agr_pred = aggregate(correctresponse~predicate*correctclass*noun,data=o,mean)
o_agr_class = aggregate(correctresponse~correctclass*noun,data=o,mean)
head(o_agr_pred)
head(o_agr_class)
o_agr_pred$pred_noun = paste(o_agr_pred$predicate,o_agr_pred$noun)
o_agr_class$class_noun = paste(o_agr_class$correctclass,o_agr_class$noun)
head(d)
d_agr_pred = aggregate(adj_response~predicate*class*noun,data=d,mean)
d_agr_class = aggregate(adj_response~class*noun,data=d,mean)
d_agr_pred$pred_noun = paste(d_agr_pred$predicate,d_agr_pred$noun)
d_agr_class$class_noun = paste(d_agr_class$class,d_agr_class$noun)
o_agr_pred$concept = d_agr_pred$adj_response[match(o_agr_pred$pred_noun,d_agr_pred$pred_noun)]
o_agr_class$concept = d_agr_class$adj_response[match(o_agr_class$class_noun,d_agr_class$class_noun)]
gof(o_agr_pred$correctresponse,o_agr_pred$concept) # r = -0.01., r2 = 0.00
#results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~concept)
#boot.ci(results, type="bca") # 95%   ( 0.0006,  0.8949 )    
# plot order preference against subjectivity
gof(o_agr_class$correctresponse,o_agr_class$concept) # r = -0.20, r2 = 0.04
#ggplot(o_agr_pred, aes(x=concept,y=correctresponse)) +
#  geom_point() +
#  geom_smooth(method=lm,color="black") +
#  xlab("\nadjective probability")+
#  ylab("naturalness\n")+
#  ylim(0,1)+
#  theme_bw()
#ggsave("../results/naturalness-concept-noun-pred.pdf",height=3,width=3.5)
concept <- o_agr_pred

head(expt1inh)
expt1inh$expt = "Inherentness"
expt1inh = subset(expt1inh, select=c("predicate", "correctclass","correctresponse","inherentness","expt"))
head(concept)
concept = subset(concept, select=c("pred_noun", "correctclass","correctresponse","concept"))
concept$expt = "Concept-formability"
colnames(concept) <- c("predicate", "correctclass","correctresponse","inherentness","expt")
expts = rbind(expt1inh,concept)
ggplot(expts, aes(x=inherentness,y=correctresponse)) +
  geom_point(size=0.5) +
  geom_smooth(method=lm,color="black",size=0.5) +
  xlab("")+
  ylab("preferred distance from noun\n")+
  ylim(0,1)+
  theme_bw() +
  facet_wrap( ~expt, ncol=1,scales="free_x")
  #theme(legend.position="bottom",legend.direction="vertical",legend.margin=unit(0,"cm"))
ggsave("~/Documents/git/cocolab/adjective_ordering/presentations/LSA/plots/inherentness-concept.png",width=2.3,height=3.3)
