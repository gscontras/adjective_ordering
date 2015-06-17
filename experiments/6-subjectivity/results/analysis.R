library(ggplot2)
library(reshape2)
library(lme4)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/6-subjectivity/Submiterator-master")

d = read.table("subjectivity-trials.tsv",sep="\t",header=T)
head(d)
s = read.table("subjectivity-subject_information.tsv",sep="\t",header=T)
head(s)

d$language = s$language[match(d$workerid,s$workerid)]

aggregate(response~class,data=d,mean)

d$class <- factor(d$class,levels=c("quality","size","age","texture","color","shape","material"))

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
