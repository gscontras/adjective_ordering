library(ggplot2)
library(reshape2)
library(lme4)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/1-faultless-disagreement/Submiterator-master")

d = read.table("faultless-disagreement-trials.tsv",sep="\t",header=T)
head(d)

summary(d)

aggregate(response~class,data=d,mean)

d$class <- factor(d$class,levels=c("quality","size","age","texture","shape","color"))

table(d$class,d$nounclass)

d_s = bootsSummary(data=d, measurevar="response", groupvars=c("class","nounclass"))
class_plot <- ggplot(d_s, aes(x=class,y=response,fill=nounclass)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=class, width=0.1),position=position_dodge(width=0.9))+
  ylab("faultless disagreement\n")+
  xlab("\nverb class") 
class_plot
