library(hydroGOF)
library(ggplot2)
library(lme4)
library(lmerTest)
require(MuMIn)

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
o_no_sup <- o[o$correctpred1!="best"&o$correctpred1!="biggest"&o$correctpred1!="closest"&o$correctpred1!="last"&o$correctpred2!="best"&o$correctpred2!="biggest"&o$correctpred2!="closest"&o$correctpred2!="last",]
o_no_sup_pred = aggregate(correctresponse~correctpred1*correctclass1,data=o_no_sup,mean)
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
# PREDICATE WITH ONLY ORIGINAL MATERIALS (there were 11)
o_o = o_agr_pred[o_agr_pred$correctpred1=="blue"|o_agr_pred$correctpred1=="green"|o_agr_pred$correctpred1=="long"|o_agr_pred$correctpred1=="new"|o_agr_pred$correctpred1=="old"|o_agr_pred$correctpred1=="purple"|o_agr_pred$correctpred1=="red"|o_agr_pred$correctpred1=="smooth"|o_agr_pred$correctpred1=="square"|o_agr_pred$correctpred1=="wooden"|o_agr_pred$correctpred1=="yellow",]
gof(o_o$correctresponse,o_o$subjectivity) # r = .95, r2 = .91
results <- boot(data=o_o, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.0548,  0.9680 )    
# PREDICATE WITHOUT SUPERLATIVES (best, biggest, closest, last)
o_no_sup_pred$subjectivity = s_agr_pred$response[match(o_no_sup_pred$correctpred1,s_agr_pred$predicate)]
gof(o_no_sup_pred$correctresponse,o_no_sup_pred$subjectivity) # r = .78, r2 = .61
results <- boot(data=o_no_sup_pred, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.4658,  0.7145 )  
# CLASS
o_agr_class$subjectivity = s_agr_class$response[match(o_agr_class$correctclass1,s_agr_class$class)]
gof(o_agr_class$correctresponse,o_agr_class$subjectivity) # r = .86, r2 = .73
results <- boot(data=o_agr_class, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.3629,  0.8717 )

# plot order preference against subjectivity
ggplot(o_agr_pred, aes(x=subjectivity,y=correctresponse,color=correctclass1)) +
  geom_point() +
  #geom_text(aes(label=correctpred1),color="black")+
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity")+
  ylab("naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("results/naturalness-subjectivity.pdf",height=4,width=5.5)


# plot order preference against subjectivity with text label
ggplot(o_agr_pred, aes(x=subjectivity,y=correctresponse)) +
  geom_point() +
  geom_text(aes(label=correctpred1),color="black")+
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity")+
  ylab("naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("results/naturalness-subjectivity-labelled.pdf",height=4,width=5.5)


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
results <- boot(data=o_agr_pred, statistic=rsq, R=100, formula=correctresponse~s_diff)
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
#ggsave("results/naturalness-subjectivity_class-difference.pdf",height=3,width=3.5)

#PREDICATE
ggplot(o_agr_pred, aes(x=s_diff,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity difference")+
  ylab("configuration naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("results/naturalness-subjectivity_difference.pdf",height=3,width=3.5)

# PREDICATE FACETED BY CLASS CONFIGURATION
ggplot(o_agr_pred, aes(x=s_diff,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity difference")+
  ylab("configuration naturalness\n")+
  ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()+
  facet_wrap(~class)
#ggsave("results/naturalness-subjectivity_class-facet.pdf",height=12,width=17)



#################################################
## REGRESSION ANALYSES
#################################################

#load in subjectivity scores
s = read.csv("results/subjectivity-expanded_results.csv",header=T)
head(s)
s_agr_pred = aggregate(response~predicate,data=s,mean)

#load in length and frequency
lf = read.table("../../corpus_results/data/sampled_adjectives_with_freq.txt",sep="\t",header=T)
head(lf)

#load in naturalness preferences
#o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/12-order-preference-expanded/Submiterator-master/order-preference-duplicated.csv",header=T)
#non-duplicated data
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/12-order-preference-expanded/Submiterator-master/order-preference.csv",header=T)
head(o)
o$correctpred1 <- o$predicate1
o$correctpred2 <- o$predicate2
o$correctresponse <- o$response
o <- o[o$makes_sense=="yes",]

o$freq1 = lf$logProbability[match(o$correctpred1,lf$Adjective)]
o$freq2 = lf$logProbability[match(o$correctpred2,lf$Adjective)]
o$length1 = lf$Length[match(o$correctpred1,lf$Adjective)]
o$length2 = lf$Length[match(o$correctpred2,lf$Adjective)]
o$sub1 = s_agr_pred$response[match(o$correctpred1,s_agr_pred$predicate)]
o$sub2 = s_agr_pred$response[match(o$correctpred2,s_agr_pred$predicate)]
o$sup1 = 0
o[o$correctpred1!="best"&o$correctpred1!="biggest"&o$correctpred1!="closest"&o$correctpred1!="last",]$sup1 = 1
o$sup2 = 0
o[o$correctpred2!="best"&o$correctpred2!="biggest"&o$correctpred2!="closest"&o$correctpred2!="last",]$sup2 = 1

m = lmer(correctresponse~
           slide_number+
           class_configuration+
           #makes_sense+
           length1+
           length2+
           freq1+
           freq2+
           sub1+
           sub2+
           sup1+
           sup2+
           sub1:sup1+
           #sub1:sup2+
           #sub2:sup1+
           sub2:sup2+
           sub1:sub2+
           sup1:sup2+
           sub1:length1+
           #sub1:length2+
           #sub2:length1+
           sub2:length2+
           sub1:freq1+
           #sub1:freq2+
           #sub2:freq1+
           sub2:freq2+
           #sub1:makes_sense+
           #sub2:makes_sense+
           #sup1:makes_sense+
           #sup2:makes_sense+
           (1|workerid)+(1|noun),data=o)
summary(m)

m_no_config = lmer(correctresponse~
           slide_number+
           #class_configuration+
           #makes_sense+
           length1+
           length2+
           freq1+
           freq2+
           sub1+
           sub2+
           sup1+
           sup2+
           sub1:sup1+
           #sub1:sup2+
           #sub2:sup1+
           sub2:sup2+
           sub1:sub2+
           sup1:sup2+
           sub1:length1+
           #sub1:length2+
           #sub2:length1+
           sub2:length2+
           sub1:freq1+
           #sub1:freq2+
           #sub2:freq1+
           sub2:freq2+
           #sub1:makes_sense+
           #sub2:makes_sense+
           #sup1:makes_sense+
           #sup2:makes_sense+
           (1|workerid)+(1|noun),data=o)
summary(m_no_config)

anova(m,m_no_config)

## DIFFERENCE SCORES
o$subDiff = o$sub1 - o$sub2
o$lengthDiff = o$length1 - o$length2
o$freqDiff = o$freq1 - o$freq2

md = lmer(correctresponse~
           slide_number+
           sup1+
           sup2+
           subDiff+
           lengthDiff+
           freqDiff+
          #sup1:sup2+
          #sup1:subDiff+
          #sup2:subDiff+
          #sup1:lengthDiff+
          #sup2:lengthDiff+
          #sup1:freqDiff+
          #sup2:freqDiff+
          subDiff:lengthDiff+
          subDiff:freqDiff+
          lengthDiff:freqDiff+
         (1|workerid)+(1|noun),data=o)
summary(md)

m_sub = lmer(correctresponse~
            #slide_number+
            #sup1+
            #sup2+
            subDiff+
            #lengthDiff+
            #freqDiff+
            #sup1:sup2+
            #sup1:subDiff+
            #sup2:subDiff+
            #sup1:lengthDiff+
            #sup2:lengthDiff+
            #sup1:freqDiff+
            #sup2:freqDiff+
            #subDiff:lengthDiff+
            #subDiff:freqDiff+
            #lengthDiff:freqDiff+
            (1|workerid)+(1|noun),data=o)
summary(m_sub)


## NO SUPERLATIVES
o_no_sup <- o[o$correctpred1!="best"&o$correctpred1!="biggest"&o$correctpred1!="closest"&o$correctpred1!="last"&o$correctpred2!="best"&o$correctpred2!="biggest"&o$correctpred2!="closest"&o$correctpred2!="last",]
#o_no_sup <- o_no_sup[o_no_sup$makes_sense=="yes",]
o_no_sup$subDiff = o_no_sup$sub1 - o_no_sup$sub2
o_no_sup$lengthDiff = o_no_sup$length1 - o_no_sup$length2
o_no_sup$freqDiff = o_no_sup$freq1 - o_no_sup$freq2


md_sup = lmer(correctresponse~
            slide_number+
            subDiff+
            lengthDiff+
            freqDiff+
            subDiff:lengthDiff+
            subDiff:freqDiff+
            lengthDiff:freqDiff+
            (1|workerid)+(1|noun),data=o_no_sup)
summary(md_sup)

md_sup_config = lmer(correctresponse~
                slide_number+
                class_configuration+
                subDiff+
                lengthDiff+
                freqDiff+
                subDiff:lengthDiff+
                subDiff:freqDiff+
                lengthDiff:freqDiff+
                (1|workerid)+(1|noun),data=o_no_sup)
summary(md_sup_config)

anova(md_sup,md_sup_config)


m_no_sup = lmer(correctresponse~
           slide_number+
           #makes_sense+
           length1+
           length2+
           freq1+
           freq2+ 
           sub1+
           sub2+
           #sup1+
           #sup2+
           #sub1:sup1+
           #sub2:sup2+
           sub1:sub2+
           #sup1:sup2+
           #sub1:makes_sense+
           #sub2:makes_sense+
           #sup1:makes_sense+
           #sup2:makes_sense+
           sub1:length1+
           sub2:length2+
           sub1:freq1+
           sub2:freq2+
           (1|workerid)+(1|noun),data=o_no_sup)
summary(m_no_sup)



r.squaredGLMM(md)
r.squaredGLMM(m_sub)
#r.squaredGLMM(md_sup)
#r.squaredGLMM(md_sup_config)

r.squaredGLMM(m)
r.squaredGLMM(m_no_config)
