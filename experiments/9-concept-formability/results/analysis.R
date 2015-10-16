library(ggplot2)
library(reshape2)
library(lme4)
library(hydroGOF)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/9-concept-formability/Submiterator-master")

d = read.table("concept-formability-trials.tsv",sep="\t",header=T)
head(d)
s = read.table("concept-formability-subject_information.tsv",sep="\t",header=T)
head(s)
d$language = s$language[match(d$workerid,s$workerid)]
summary(d)
d = dcast(data=d, workerid + trial + adjective + noun + class + nounclass + language ~ predicate, value.var="response",mean)
colnames(d) = c("workerid","trial","predicate","noun","class","nounclass","language","adj_response","noun_response")


#############################################
## comparing concept-formability and faultless disagreement
#############################################


f = read.table("~/Documents/git/CoCoLab/adjective_ordering/experiments/2-faultless-disagreement/Submiterator-master/faultless-disagreement-2-trials.tsv",sep="\t",header=T)

f = subset(f,select=c("class","predicate","response","nounclass"))
f$experiment = "faultless disagreement"
head(f)
adj = subset(d,select=c("class","predicate","adj_response","nounclass"))
adj$experiment = "concept formability"
colnames(adj) = c("class","predicate","response","nounclass","experiment")
head(adj)
all_d = rbind(adj,f)
head(all_d)

## predicate plot by class
c_s = bootsSummary(data=all_d, measurevar="response", groupvars=c("class","experiment","nounclass"))
class_plot <- ggplot(c_s, aes(x=reorder(class,-response,mean),y=response,fill=nounclass)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  ylab("rating\n")+
  xlab("\nadjective class") +
  facet_wrap(~experiment,scale="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
class_plot
#ggsave("../results/class_faultless_concept.png",height=3)


#############################################
## compare concept-formability and order preference
#############################################


## order preference
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/naturalness-duplicated.csv",header=T)
head(o)
o_agr_pred = aggregate(correctresponse~predicate*correctclass,data=o,mean)
o_agr_class = aggregate(correctresponse~correctclass,data=o,mean)
head(d)
d_agr_pred = aggregate(adj_response~predicate,data=d,mean)
d_agr_class = aggregate(adj_response~class,data=d,mean)
# PREDICATE
o_agr_pred$concept = d_agr_pred$adj_response[match(o_agr_pred$predicate,d_agr_pred$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$concept) # r = -0.28, r2 = 0.08
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~concept)
boot.ci(results, type="bca") # 95%   ( 0.0005,  0.3167 )    
# CLASS
o_agr_class$concept = d_agr_class$adj_response[match(o_agr_class$correctclass,d_agr_class$class)]
gof(o_agr_class$correctresponse,o_agr_class$concept) # r = -0.74, r2 = 0.55
results <- boot(data=o_agr_class, statistic=rsq, R=10000, formula=correctresponse~concept)
boot.ci(results, type="bca") # 95%   ( 0.0001,  0.7918 )   
# plot order preference against subjectivity
ggplot(o_agr_pred, aes(x=concept,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nconcept formability (no noun; r = -0.28)")+
  ylab("naturalness\n")+
  ylim(0,1)+
  theme_bw()
#ggsave("../results/naturalness-concept-no-noun.png",height=3,width=4)

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
gof(o_agr_class$correctresponse,o_agr_class$concept) # r = -0.20, r2 = 0.04

ggplot(o_agr_pred, aes(x=concept,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nconcept formability (w/ noun; r = -0.01)")+
  ylab("naturalness\n")+
  ylim(0,1)+
  theme_bw()
#ggsave("../results/naturalness-concept-noun.png",height=3,width=4)



#############################################
## plot noun vs. adj ratings
#############################################


head(d)
gof(d$adj_response,d$noun_response) # r = 0.36., r2 = 0.13
ggplot(d, aes(x=adj_response,y=noun_response)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nadjective rating (r = 0.36)")+
  ylab("noun rating\n")+
  ylim(0,1)+
  theme_bw()

raw_d = read.table("concept-formability-trials.tsv",sep="\t",header=T)
head(raw_d)
raw_d$noun_adj = paste(raw_d$adjective,raw_d$noun)

agr = raw_d %>%
  group_by(noun_adj,predicate) %>%
  summarise(mean=mean(response))
agr = as.data.frame(agr)

raw_agg = agr %>%
  spread(predicate,mean)
agr = raw_d %>%
  group_by(noun_adj,predicate) %>%
  summarise(ci.low=ci.low(response))
agr = as.data.frame(agr)
agr_cilow = agr %>%
  spread(predicate,ci.low)
head(agr_cilow)
agr = raw_d %>%
  group_by(noun_adj,predicate) %>%
  summarise(ci.high=ci.high(response))
agr = as.data.frame(agr)
agr_cihigh = agr %>%
  spread(predicate,ci.high)
head(agr_cihigh)

row.names(agr_cilow) = agr_cilow$noun_adj
raw_agg$noun_cilow = raw_agg$noun - agr_cilow[as.character(raw_agg$noun_adj),]$noun
raw_agg$adj_cilow = raw_agg$adjective - agr_cilow[as.character(raw_agg$noun_adj),]$adjective
row.names(agr_cihigh) = agr_cihigh$noun_adj
raw_agg$noun_cihigh = raw_agg$noun + agr_cihigh[as.character(raw_agg$noun_adj),]$noun
raw_agg$adj_cihigh = raw_agg$adjective + agr_cihigh[as.character(raw_agg$noun_adj),]$adjective

head(raw_agg)

ggplot(raw_agg, aes(x=adjective,y=noun)) +
  geom_point(size=.25,alpha=.75) +
  geom_smooth(method=lm,color="black") +
  geom_errorbar(aes(ymin=noun_cilow,ymax=noun_cihigh),width=.005,alpha=0.25) +
  geom_errorbarh(aes(xmin=adj_cilow,xmax=adj_cihigh),width=.005,alpha=0.25) +
  xlab("\nadjective rating (r = 0.36)")+
  ylab("noun rating\n")+
  geom_text(aes(label=noun_adj),size=2)+
  ylim(0,1)+
  theme_bw()
ggsave("../results/concept-noun-adj.png")
