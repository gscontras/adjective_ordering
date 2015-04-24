library(ggplot2)
library(reshape2)
library(lme4)

setwd("~/Documents/git/cocolab/collective/experiments/3-faultless-disagreement/Submiterator-master")

f = read.table("faultless-disagreement-trials.tsv",sep="\t",header=T)
head(f)

f = f[f$sense=="Yes",]

f_casted = dcast(data=f, predicate + sense ~ faultless, value.var="response",mean)

# compute comparison score (based on sense ratings)

c_score <- as.data.frame.matrix(table(f$sentence,f$sense))
head(c_score)
c_score$c_ratio = (c_score$Yes/c_score$No)
summary(c_score)
head(c_score)
write.csv(c_score,"sentence-comp-score.csv")

f_casted$faultless_rating = (f_casted$yes/f_casted$no)
f_casted$faultless_rating_norm = (f_casted$yes/(f_casted$no+f_casted$yes))


## read in sentence paraphrase ratings

d = read.table("~/Documents/git/CoCoLab/collective/experiments/2-corpus-based/Submiterator-master/2-corpus-based-trials.tsv",sep="\t",header=T)

s = read.csv("~/Documents/git/CoCoLab/collective/experiments/2-corpus-based/bnc.sentences.csv",header=T)

d = d[!is.na(d$sense)&d$sense=="Yes",]

a = d[d$attested=="True",]

a$sentence = paste(a$noun,a$predicate,sep=" ")

a$faultless = f_casted$faultless_rating[match(a$sentence,f_casted$sentence)]
a$faultless_norm = f_casted$faultless_rating_norm[match(a$sentence,f_casted$sentence)]


## look just at small

s <- a[a$predicate=="small",]
s$noun <- factor(s$noun,levels=c("numbers","rooms","children","classes"))

s_model = lmer(response~noun+slide_number+(1|workerid),data=s[s$sentence_type=="coll",])
summary(s_model)

s_casted = dcast(data=s, noun ~ sentence_type, value.var="response",mean)
s_casted$CI.YMin.coll = s_casted$coll - dcast(data=s, noun ~ sentence_type, value.var="response",ci.low)$coll
s_casted$CI.YMin.dist = s_casted$dist - dcast(data=s, noun ~ sentence_type, value.var="response",ci.low)$dist
s_casted$CI.YMax.coll = s_casted$coll + dcast(data=s, noun ~ sentence_type, value.var="response",ci.high)$coll
s_casted$CI.YMax.dist = s_casted$dist + dcast(data=s, noun ~ sentence_type, value.var="response",ci.high)$dist
s_casted$se.YMin.coll = s_casted$coll - dcast(data=s, noun ~ sentence_type, value.var="response",se)$coll
s_casted$se.YMin.dist = s_casted$dist - dcast(data=s, noun ~ sentence_type, value.var="response",se)$dist
s_casted$se.YMax.coll = s_casted$coll + dcast(data=s, noun ~ sentence_type, value.var="response",se)$coll
s_casted$se.YMax.dist = s_casted$dist + dcast(data=s, noun ~ sentence_type, value.var="response",se)$dist



small_coll_plot <- ggplot(s_casted, aes(x=noun,y=coll)) +
  #geom_point(size=2,alpha=0.35) +
  geom_bar(stat="identity",position=position_dodge(),fill="red")+
  #geom_smooth() +
  geom_errorbar(aes(ymin=CI.YMin.coll, ymax=CI.YMax.coll, x=noun, width=0.1),position=position_dodge(width=0.9)) +
  #geom_errorbar(alpha=.8,aes(ymin=CI.YMin.coll,ymax=CI.YMax.coll)) +
  #geom_errorbarh(alpha=.8,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) 
  #geom_errorbar(alpha=.8,aes(ymin=se.YMin.dist,ymax=se.YMax.dist)) +
  #geom_errorbarh(alpha=.8,aes(xmin=se.YMin.coll,xmax=se.YMax.coll))   
  #geom_abline(intercept=0,slope=1) +
  #geom_text(size=3,alpha=.5,aes(label=sentence),angle=45) +
  ylab("collective endorsement\n") +
  xlab("\nsubject noun")+
  ylim(0,1)
  #xlim(0,1)
ggsave("small_coll_plot.pdf")



s_f <- f[f$predicate=="small",]
s_f <- na.omit(s_f)

s_f_casted = dcast(data=s_f, noun ~ faultless, value.var="response",mean)
s_f_casted$CI.YMin.yes = s_f_casted$yes - dcast(data=s_f, noun ~ faultless, value.var="response",ci.low)$yes
s_f_casted$CI.YMin.no = s_f_casted$no - dcast(data=s_f, noun ~ faultless, value.var="response",ci.low)$no
s_f_casted$CI.YMax.yes = s_f_casted$yes + dcast(data=s_f, noun ~ faultless, value.var="response",ci.high)$yes
s_f_casted$CI.YMax.no = s_f_casted$no + dcast(data=s_f, noun ~ faultless, value.var="response",ci.high)$no
s_f_casted$se.YMin.yes = s_f_casted$yes - dcast(data=s_f, noun ~ faultless, value.var="response",se)$yes
s_f_casted$se.YMin.no = s_f_casted$no - dcast(data=s_f, noun ~ faultless, value.var="response",se)$no
s_f_casted$se.YMax.yes = s_f_casted$yes + dcast(data=s_f, noun ~ faultless, value.var="response",se)$yes
s_f_casted$se.YMax.no = s_f_casted$no + dcast(data=s_f, noun ~ faultless, value.var="response",se)$no

small_fault_plot <- ggplot(s_f_casted, aes(x=yes,y=no,color=noun)) +
  geom_point(size=2,alpha=0.35) +
  #geom_smooth() +
  #geom_errorbar(alpha=.8,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  #geom_errorbarh(alpha=.8,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_errorbar(alpha=.8,aes(ymin=se.YMin.no,ymax=se.YMax.no)) +
  geom_errorbarh(alpha=.8,aes(xmin=se.YMin.yes,xmax=se.YMax.yes))   
#geom_abline(intercept=0,slope=1) +
#geom_text(size=3,alpha=.5,aes(label=sentence),angle=45) +
#ylab("faultless?") +
#xlab("collective?")+
#ylim(0,1) +
#xlim(0,1)
ggsave("small_fault_plot.pdf")




## look just at open

s <- a[a$predicate=="open",]

s_casted = dcast(data=s, noun ~ sentence_type, value.var="response",mean,na.rm=T)
s_casted$CI.YMin.coll = s_casted$coll - dcast(data=s, noun ~ sentence_type, value.var="response",ci.low,na.rm=T)$coll
s_casted$CI.YMin.dist = s_casted$dist - dcast(data=s, noun ~ sentence_type, value.var="response",ci.low,na.rm=T)$dist
s_casted$CI.YMax.coll = s_casted$coll + dcast(data=s, noun ~ sentence_type, value.var="response",ci.high,na.rm=T)$coll
s_casted$CI.YMax.dist = s_casted$dist + dcast(data=s, noun ~ sentence_type, value.var="response",ci.high,na.rm=T)$dist
s_casted$se.YMin.coll = s_casted$coll - dcast(data=s, noun ~ sentence_type, value.var="response",se)$coll
s_casted$se.YMin.dist = s_casted$dist - dcast(data=s, noun ~ sentence_type, value.var="response",se)$dist
s_casted$se.YMax.coll = s_casted$coll + dcast(data=s, noun ~ sentence_type, value.var="response",se)$coll
s_casted$se.YMax.dist = s_casted$dist + dcast(data=s, noun ~ sentence_type, value.var="response",se)$dist

open_coll_plot <- ggplot(s_casted, aes(x=coll,y=dist,color=noun)) +
  geom_point(size=2,alpha=0.35) +
  #geom_smooth() +
  #geom_errorbar(alpha=.8,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  #geom_errorbarh(alpha=.8,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_errorbar(alpha=.8,aes(ymin=se.YMin.dist,ymax=se.YMax.dist)) +
  geom_errorbarh(alpha=.8,aes(xmin=se.YMin.coll,xmax=se.YMax.coll))   
#geom_abline(intercept=0,slope=1) +
#geom_text(size=3,alpha=.5,aes(label=sentence),angle=45) +
#ylab("faultless?") +
#xlab("collective?")+
#ylim(0,1) +
#xlim(0,1)
ggsave("open_coll_plot.pdf")



s_f <- f[f$predicate=="open",]
s_f <- na.omit(s_f)

s_f_casted = dcast(data=s_f, noun ~ faultless, value.var="response",mean,na.rm=T)
s_f_casted$CI.YMin.yes = s_f_casted$yes - dcast(data=s_f, noun ~ faultless, value.var="response",ci.low,na.rm=T)$yes
s_f_casted$CI.YMin.no = s_f_casted$no - dcast(data=s_f, noun ~ faultless, value.var="response",ci.low,na.rm=T)$no
s_f_casted$CI.YMax.yes = s_f_casted$yes + dcast(data=s_f, noun ~ faultless, value.var="response",ci.high,na.rm=T)$yes
s_f_casted$CI.YMax.no = s_f_casted$no + dcast(data=s_f, noun ~ faultless, value.var="response",ci.high,na.rm=T)$no
s_f_casted$se.YMin.yes = s_f_casted$yes - dcast(data=s_f, noun ~ faultless, value.var="response",se)$yes
s_f_casted$se.YMin.no = s_f_casted$no - dcast(data=s_f, noun ~ faultless, value.var="response",se)$no
s_f_casted$se.YMax.yes = s_f_casted$yes + dcast(data=s_f, noun ~ faultless, value.var="response",se)$yes
s_f_casted$se.YMax.no = s_f_casted$no + dcast(data=s_f, noun ~ faultless, value.var="response",se)$no

open_fault_plot <- ggplot(s_f_casted, aes(x=yes,y=no,color=noun)) +
  geom_point(size=2,alpha=0.35) +
  #geom_smooth() +
  #geom_errorbar(alpha=.8,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  #geom_errorbarh(alpha=.8,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_errorbar(alpha=.8,aes(ymin=se.YMin.no,ymax=se.YMax.no)) +
  geom_errorbarh(alpha=.8,aes(xmin=se.YMin.yes,xmax=se.YMax.yes))   
#geom_abline(intercept=0,slope=1) +
#geom_text(size=3,alpha=.5,aes(label=sentence),angle=45) +
#ylab("faultless?") +
#xlab("collective?")+
#ylim(0,1) +
#xlim(0,1)
ggsave("open_fault_plot.pdf")



## look just at full

s <- a[a$predicate=="full",]

s_casted = dcast(data=s, noun ~ sentence_type, value.var="response",mean,na.rm=T)
s_casted$CI.YMin.coll = s_casted$coll - dcast(data=s, noun ~ sentence_type, value.var="response",ci.low,na.rm=T)$coll
s_casted$CI.YMin.dist = s_casted$dist - dcast(data=s, noun ~ sentence_type, value.var="response",ci.low,na.rm=T)$dist
s_casted$CI.YMax.coll = s_casted$coll + dcast(data=s, noun ~ sentence_type, value.var="response",ci.high,na.rm=T)$coll
s_casted$CI.YMax.dist = s_casted$dist + dcast(data=s, noun ~ sentence_type, value.var="response",ci.high,na.rm=T)$dist
s_casted$se.YMin.coll = s_casted$coll - dcast(data=s, noun ~ sentence_type, value.var="response",se)$coll
s_casted$se.YMin.dist = s_casted$dist - dcast(data=s, noun ~ sentence_type, value.var="response",se)$dist
s_casted$se.YMax.coll = s_casted$coll + dcast(data=s, noun ~ sentence_type, value.var="response",se)$coll
s_casted$se.YMax.dist = s_casted$dist + dcast(data=s, noun ~ sentence_type, value.var="response",se)$dist

full_coll_plot <- ggplot(s_casted, aes(x=coll,y=dist,color=noun)) +
  geom_point(size=2,alpha=0.35) +
  #geom_smooth() +
  #geom_errorbar(alpha=.8,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  #geom_errorbarh(alpha=.8,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_errorbar(alpha=.8,aes(ymin=se.YMin.dist,ymax=se.YMax.dist)) +
  geom_errorbarh(alpha=.8,aes(xmin=se.YMin.coll,xmax=se.YMax.coll))   
#geom_abline(intercept=0,slope=1) +
#geom_text(size=3,alpha=.5,aes(label=sentence),angle=45) +
#ylab("faultless?") +
#xlab("collective?")+
#ylim(0,1) +
#xlim(0,1)
ggsave("full_coll_plot.pdf")



s_f <- f[f$predicate=="full",]
s_f <- na.omit(s_f)

s_f_casted = dcast(data=s_f, noun ~ faultless, value.var="response",mean,na.rm=T)
s_f_casted$CI.YMin.yes = s_f_casted$yes - dcast(data=s_f, noun ~ faultless, value.var="response",ci.low,na.rm=T)$yes
s_f_casted$CI.YMin.no = s_f_casted$no - dcast(data=s_f, noun ~ faultless, value.var="response",ci.low,na.rm=T)$no
s_f_casted$CI.YMax.yes = s_f_casted$yes + dcast(data=s_f, noun ~ faultless, value.var="response",ci.high,na.rm=T)$yes
s_f_casted$CI.YMax.no = s_f_casted$no + dcast(data=s_f, noun ~ faultless, value.var="response",ci.high,na.rm=T)$no
s_f_casted$se.YMin.yes = s_f_casted$yes - dcast(data=s_f, noun ~ faultless, value.var="response",se)$yes
s_f_casted$se.YMin.no = s_f_casted$no - dcast(data=s_f, noun ~ faultless, value.var="response",se)$no
s_f_casted$se.YMax.yes = s_f_casted$yes + dcast(data=s_f, noun ~ faultless, value.var="response",se)$yes
s_f_casted$se.YMax.no = s_f_casted$no + dcast(data=s_f, noun ~ faultless, value.var="response",se)$no

open_fault_plot <- ggplot(s_f_casted, aes(x=yes,y=no,color=noun)) +
  geom_point(size=2,alpha=0.35) +
  #geom_smooth() +
  #geom_errorbar(alpha=.8,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  #geom_errorbarh(alpha=.8,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_errorbar(alpha=.8,aes(ymin=se.YMin.no,ymax=se.YMax.no)) +
  geom_errorbarh(alpha=.8,aes(xmin=se.YMin.yes,xmax=se.YMax.yes))   
#geom_abline(intercept=0,slope=1) +
#geom_text(size=3,alpha=.5,aes(label=sentence),angle=45) +
#ylab("faultless?") +
#xlab("collective?")+
#ylim(0,1) +
#xlim(0,1)
ggsave("full_fault_plot.pdf")









a_sent_casted = dcast(data=a, animate + sentence + faultless~ sentence_type, value.var="response",mean,na.rm=T)
a_sent_casted$collective = (a_sent_casted$coll/a_sent_casted$dist)
a_sent_casted$collective_norm = (a_sent_casted$coll/(a_sent_casted$coll+a_sent_casted$dist))
a_sent_casted$faultless_norm = (a_sent_casted$faultless/max(a_sent_casted$faultless))

faultless_collective_plot <- ggplot(a_sent_casted, aes(x=collective_norm,y=faultless_norm)) +
    geom_point(size=2,alpha=0.35,color="red") +
  #geom_smooth() +
  #geom_errorbar(alpha=.3,aes(ymin=CI.YMin.dist,ymax=CI.YMax.dist)) +
  #geom_errorbarh(alpha=.3,aes(xmin=CI.YMin.coll,xmax=CI.YMax.coll)) +  
  geom_abline(intercept=0,slope=1) +
  geom_text(size=3,alpha=.5,aes(label=sentence),angle=45) +
  ylab("faultless?") +
  xlab("collective?")+
  ylim(0,1) +
  xlim(0,1)
faultless_collective_plot

sentence_plot <- ggplot(a_sent_casted, aes(x=coll,y=dist)) +
  geom_text(size=3,alpha=.5,aes(label=sentence),angle=45) +
  geom_smooth()+
  geom_abline(intercept=0,slope=1) +
  ylim(0,1) +
  xlim(0,1)
sentence_plot


#ggsave(filename='faultless_collective_plot.png',plot=faultless_collective_plot,width=10, height=10)


head(a_sent_casted)
cor(a_sent_casted$collective_norm,a_sent_casted$faultless_norm)

a_casted = dcast(data=a, animate + sentence + faultless + workerid +slide_number ~ sentence_type, value.var="response",mean,na.rm=T)
a_casted$collective = ((a_casted$coll+0.01)/(a_casted$dist+0.01))
a_casted <- na.omit(a_casted)


a_casted$collective = (a_casted$coll/(a_casted$dist))
a_casted <- a_casted[a_casted$collective != Inf,]

a_casted[a_casted$collective == Inf,]

a_casted$collective_norm = (a_casted$collective/max(a_casted$collective))
a_casted$collective <- as.numeric(as.character(a_casted$collective))
#a_casted$collective <- factor(a_casted$collective)

m <- lmer(collective ~ faultless + (1|slide_number) + (1|workerid) + (1|animate) , a_casted)
summary(m)

ggplot(a_casted, aes(x=faultless,y=coll)) +
  geom_point() +
  geom_smooth()

aggregate(collective~faultless,data=a_casted,mean)
