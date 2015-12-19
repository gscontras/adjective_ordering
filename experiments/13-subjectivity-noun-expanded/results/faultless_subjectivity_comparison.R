library(hydroGOF)

## load in order preferences

d = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/3-order-preference/Submiterator-master/order-preference-trials.tsv",sep="\t",header=T)
head(d)

## load in faultless

f = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/2-faultless-disagreement/Submiterator-master/faultless-disagreement-2-trials.tsv",sep="\t",header=T)
head(f)
#fsub = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/2-faultless-disagreement/Submiterator-master/faultless-disagreement-2-subject_information.tsv",sep="\t",header=T)
f_agr = aggregate(response~class,data=f,mean)
f_agr_pred = aggregate(response~predicate,data=f,mean)
d$class1_f = f_agr$response[match(d$class1,f_agr$class)]
d$class2_f = f_agr$response[match(d$class2,f_agr$class)]
d$f_diff = (d$class1_f-d$class2_f)
d$pred1_f = f_agr_pred$response[match(d$predicate1,f_agr_pred$predicate)]
d$pred2_f = f_agr_pred$response[match(d$predicate2,f_agr_pred$predicate)]
d$f_diff_pred = (d$pred1_f-d$pred2_f)

## load in subjectivity

s = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/7-subjectivity-noun/Submiterator-master/subjectivity-trials.tsv",sep="\t",header=T)
head(s)
ssub = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/7-subjectivity-noun/Submiterator-master/subjectivity-subject_information.tsv",sep="\t",header=T)
s_agr = aggregate(response~class,data=s,mean)
s_agr_pred = aggregate(response~predicate,data=s,mean)
d$class1_s = s_agr$response[match(d$class1,s_agr$class)]
d$class2_s = s_agr$response[match(d$class2,s_agr$class)]
d$s_diff = (d$class1_s-d$class2_s)
d$pred1_s = s_agr_pred$response[match(d$predicate1,s_agr_pred$predicate)]
d$pred2_s = s_agr_pred$response[match(d$predicate2,s_agr_pred$predicate)]
d$s_diff_pred = (d$pred1_s-d$pred2_s)

## combine faultless and subjectivity predicate aggregates
c <- f_agr_pred
colnames(c) = c("predicate","faultless")
head(c)
c$subjectivity = s_agr_pred$response[match(c$predicate,s_agr_pred$predicate)]
head(c)
# calculate R2
gof(c$faultless,c$subjectivity)
# plot faultless against subjectivity
ggplot(c, aes(x=subjectivity,y=faultless)) +
  geom_point()


## combine faultless and subjectivity data
f_trim <- subset(f,select=-c(firstutterance))
f_trim$expt = "faultless"
f_trim$workerid = paste("f",f_trim$workerid)
s$expt = "subjectivity"
s$workerid = paste("s",s$workerid)
fs = rbind(f_trim,s)

fs_s = bootsSummary(data=fs, measurevar="response", groupvars=c("class","expt"))

ggplot(data=fs_s,aes(x=reorder(class,-response,mean),y=response,fill=expt))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,response,mean), width=0.1),position=position_dodge(0.9))+
  xlab("\nadjective class")+
  ylab("response")+
  labs(fill="measure")+
  theme_bw()#+
#theme(axis.text.x=element_text(angle=90,vjust=0.35,hjust=1))
ggsave("~/Documents/git/cocolab/adjective_ordering/experiments/7-subjectivity-noun/results/class_subjectivity.pdf",height=4)


fs_s = bootsSummary(data=fs, measurevar="response", groupvars=c("class"))

ggplot(data=fs_s[fs_s$expt=="faultless",],aes(x=reorder(class,-response,mean),y=response))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,response,mean), width=0.1),position=position_dodge(0.9))+
  xlab("\nadjective class")+
  ylab("response")+
  #labs(fill="measure")+
  theme_bw()#+
#theme(axis.text.x=element_text(angle=90,vjust=0.35,hjust=1))
ggsave("~/Documents/git/cocolab/adjective_ordering/experiments/7-subjectivity-noun/results/class_subjectivity_collapsed.pdf",height=4)



m = lmer(response~f_diff+s_diff+slide_number+(1|workerid),data=d)
summary(m)

## plot 

d_s = aggregate(response~f_diff_pred*s_diff_pred,data=d,mean)

ggplot(d_s, aes(x=s_diff_pred,y=f_diff_pred)) +
  geom_point() +
  #geom_text(aes(label=configuration))+
  ylab("acceptability") +
  xlab("subjectivity") +
  ggtitle("by-class plot")

ggplot(d_s, aes(x=f_diff,y=response,color=s_diff)) +
  geom_point() +
  #geom_text(aes(label=configuration))+
  ylab("acceptability") +
  xlab("subjectivity") +
  ggtitle("by-class plot")