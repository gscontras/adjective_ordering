library(ggplot2)
library(lme4)
library(hydroGOF)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/10-order-preference-noun/Submiterator-master")

d = read.table("order-preference-noun-trials.tsv",sep="\t",header=T)
head(d)
s = read.table("order-preference-noun-subject_information.tsv",sep="\t",header=T)
head(s)
d$language = s$language[match(d$workerid,s$workerid)]
unique(d$language)
all <- d
# only native English speakers (n=45)
d = d[d$language!="Spanish/English"&d$language!="vietnamese",]
length(unique(d$workerid))
summary(d)
#write.csv(d,"~/Documents/git/cocolab/adjective_ordering/experiments/analysis/order-preference-trimmed.csv")

#####
## duplicate observations by first predicate
#####

o <- d
o$rightpredicate1 = o$predicate2
o$rightpredicate2 = o$predicate1
o$rightresponse = 1-o$response
agr = o %>% 
        select(predicate1,rightpredicate1,response,rightresponse,workerid,noun,nounclass,class1,class2) %>%
        gather(predicateposition,predicate,predicate1:rightpredicate1,-workerid,-noun,-nounclass,-class1,-class2)
agr$correctresponse = agr$response
agr[agr$predicateposition == "rightpredicate1",]$correctresponse = agr[agr$predicateposition == "rightpredicate1",]$rightresponse
agr$correctclass = agr$class1
agr[agr$predicateposition == "rightpredicate1",]$correctclass = agr[agr$predicateposition == "rightpredicate1",]$class2
head(agr[agr$predicateposition == "rightpredicate1",])
agr$response = NULL
agr$rightresponse = NULL
agr$class1 = NULL
agr$class2 = NULL
nrow(agr) #2496
#write.csv(agr,"naturalness-duplicated.csv")

o <- agr

#o$noun <- factor(o$noun,levels=c("thing","apple","cheese","eyes","hair"))



model.7 = lm(correctresponse~predicate, data=o)
model.11 = lm(correctresponse~predicate+noun:predicate, data=o)
anova(model.7,model.11)
summary(model.7)
summary(model.11)


#model.4 = lmer(correctresponse~predicate + (1+predicate|noun),data=o)
#model.5 = lmer(correctresponse~predicate + (1|noun),data=o)
#anova(model.4,model.5)
#summary(model.4)
#summary(model.5)



####### adjclass 
o_agr_class <- bootsSummary(data=o, measurevar="correctresponse", groupvars=c("correctclass","noun"))
ggplot(data=o_agr_class,aes(x=reorder(correctclass,-correctresponse,mean),y=correctresponse,fill=noun))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(correctclass,-correctresponse,mean), width=0.1),alpha=0.5,position=position_dodge(.9))+
  xlab("\nadjective class")+
  ylab("naturalness rating\n(distance from noun)\n")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()#+
