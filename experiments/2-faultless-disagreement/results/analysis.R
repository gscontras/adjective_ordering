library(ggplot2)
library(reshape2)
library(lme4)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/2-faultless-disagreement/Submiterator-master")

d = read.table("faultless-disagreement-2-trials.tsv",sep="\t",header=T)
head(d)

summary(d)

aggregate(response~class,data=d,mean)

d$class <- factor(d$class,levels=c("quality","size","age","texture","color","shape","material"))

table(d$class,d$nounclass)

## class plot by noun type
d_s = bootsSummary(data=d, measurevar="response", groupvars=c("class","nounclass"))
class_plot <- ggplot(d_s, aes(x=class,y=response,fill=nounclass)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=class, width=0.1),position=position_dodge(width=0.9))+
  ylab("faultless disagreement\n")+
  xlab("\nverb class") 
class_plot
ggsave("../results/class_plot.pdf")


## predicate plot by class
p_s = bootsSummary(data=d, measurevar="response", groupvars=c("class","predicate"))
p_s$predicate <- factor(p_s$predicate,ordered=is.ordered(p_s$predicate))
pred_plot <- ggplot(p_s, aes(x=reorder(predicate,-response,mean),y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(predicate,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  ylab("faultless disagreement\n")+
  xlab("predicate") +
  facet_wrap(~class,scale="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
pred_plot
ggsave("../results/pred_plot.pdf",height=5)

## predicate plot by class and noun
n_s = bootsSummary(data=d, measurevar="response", groupvars=c("class","predicate","nounclass"))
n_s$predicate <- factor(n_s$predicate,ordered=is.ordered(n_s$predicate))
noun_plot <- ggplot(n_s, aes(x=reorder(predicate,-response,mean),y=response,fill=nounclass)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(predicate,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  ylab("faultless disagreement\n")+
  xlab("predicate") +
  facet_wrap(~class,scale="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
noun_plot
ggsave("../results/noun_plot.pdf",height=5)
