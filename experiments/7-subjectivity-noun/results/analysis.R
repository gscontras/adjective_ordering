library(ggplot2)
library(reshape2)
library(lme4)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/7-subjectivity-noun/Submiterator-master")

d = read.table("subjectivity-trials.tsv",sep="\t",header=T)
head(d)
s = read.table("subjectivity-subject_information.tsv",sep="\t",header=T)
head(s)

d$language = s$language[match(d$workerid,s$workerid)]

#d_s = bootsSummary(data=d, measurevar="response", groupvars=c("class"))
# save data for aggregate plot
#write.csv(d_s,"~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/subjectivity.csv")

aggregate(response~class,data=d,mean)

d$class <- factor(d$class,levels=c("quality","size","age","texture","color","shape","material"))


#### comparing faultless disagreement and subjectivity

f = read.table("~/Documents/git/CoCoLab/adjective_ordering/experiments/2-faultless-disagreement/Submiterator-master/faultless-disagreement-2-trials.tsv",sep="\t",header=T)

f = subset(f,select=c("workerid","class","predicate","slide_number","response"))
f$experiment = "faultless disagreement"
d = subset(d,select=c("workerid","class","predicate","slide_number","response"))
d$experiment = "subjectivity"
all_d = rbind(d,f)


## predicate plot by class
c_s = bootsSummary(data=all_d, measurevar="response", groupvars=c("class","experiment"))
class_plot <- ggplot(c_s, aes(x=reorder(class,-response,mean),y=response,fill=experiment)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  ylab("rating\n")+
  xlab("adjective class") +
  #facet_wrap(~class,scale="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
class_plot
ggsave("../results/class_plot_comparison.pdf",height=3)

## predicate plot by class
p_s = bootsSummary(data=all_d, measurevar="response", groupvars=c("class","predicate","experiment"))
p_s$predicate <- factor(p_s$predicate,ordered=is.ordered(p_s$predicate))
pred_plot <- ggplot(p_s, aes(x=reorder(predicate,-response,mean),y=response,fill=experiment)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(predicate,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  ylab("rating\n")+
  xlab("predicate") +
  facet_wrap(~class,scale="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
pred_plot
ggsave("../results/pred_plot_comparison.pdf",height=5,width=7)








#### just subjectivity


## predicate plot by class
c_s = bootsSummary(data=d, measurevar="response", groupvars=c("class"))
class_plot <- ggplot(c_s, aes(x=reorder(class,-response,mean),y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  ylab("subjectivity\n")+
  xlab("adjective class") +
  #facet_wrap(~class,scale="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
class_plot
ggsave("../results/class_plot.pdf",height=5)

## predicate plot by class
p_s = bootsSummary(data=d, measurevar="response", groupvars=c("class","predicate"))
p_s$predicate <- factor(p_s$predicate,ordered=is.ordered(p_s$predicate))
pred_plot <- ggplot(p_s, aes(x=reorder(predicate,-response,mean),y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(predicate,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  ylab("subjectivity\n")+
  xlab("predicate") +
  facet_wrap(~class,scale="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
pred_plot
ggsave("../results/pred_plot.pdf",height=5)


