library(ggplot2)
library(plyr)
theme_set(theme_bw(18))
setwd("~/cogsci/projects/stanford/projects/adjective_ordering/corpus_results/")
source("rscripts/helpers.r")

#load("data/r.RData")
rs = read.table("data/swbd.tab", sep="\t", header=T, quote="")
rb = read.table("data/bncs.tab", sep="\t", header=T, quote="")
nrow(rb)
head(rb)

bnc_adjfreqs = as.data.frame(table(rb$Adjective))
bnc_adjfreqs = bnc_adjfreqs[order(bnc_adjfreqs[,c("Freq")],decreasing=T),]
bnc_padjfreqs = as.data.frame(table(rb$PrevAdjective))
bnc_padjfreqs = bnc_padjfreqs[order(bnc_padjfreqs[,c("Freq")],decreasing=T),]
head(bnc_padjfreqs)
bnc_ppadjfreqs = as.data.frame(table(rb$PrevPrevAdjective))
bnc_ppadjfreqs = bnc_ppadjfreqs[order(bnc_ppadjfreqs[,c("Freq")],decreasing=T),]
head(bnc_ppadjfreqs)

swbd_adjfreqs = as.data.frame(table(rs$Adjective))
swbd_adjfreqs = swbd_adjfreqs[order(swbd_adjfreqs[,c("Freq")],decreasing=T),]
swbd_padjfreqs = as.data.frame(table(rs$PrevAdjective))
swbd_padjfreqs = swbd_padjfreqs[order(swbd_padjfreqs[,c("Freq")],decreasing=T),]
head(swbd_padjfreqs)
swbd_ppadjfreqs = as.data.frame(table(rs$PrevPrevAdjective))
swbd_ppadjfreqs = swbd_ppadjfreqs[order(swbd_ppadjfreqs[,c("Freq")],decreasing=T),]
head(swbd_ppadjfreqs)
row.names(swbd_adjfreqs) = swbd_adjfreqs$Var1

bnc_adjfreqs$FreqBNC = bnc_adjfreqs$Freq
bnc_adjfreqs$FreqSWBD = swbd_adjfreqs[as.character(bnc_adjfreqs$Var1),]$Freq
bnc_adjfreqs$logBNCFreq = log(bnc_adjfreqs$FreqBNC)
bnc_adjfreqs$logSWBDFreq = log(bnc_adjfreqs$FreqSWBD)
head(bnc_adjfreqs)

ggplot(bnc_adjfreqs,aes(x=logBNCFreq,y=logSWBDFreq)) +
  geom_point() +
  geom_smooth() +
  geom_abline(xintercept=0,slope=1)

ggplot(bnc_adjfreqs,aes(x=FreqBNC,y=FreqSWBD)) +
  geom_point() +
  geom_smooth() 

cor(bnc_adjfreqs$FreqBNC,bnc_adjfreqs$FreqSWBD,use="complete.obs")
cor(bnc_adjfreqs$logBNCFreq,bnc_adjfreqs$logSWBDFreq,use="complete.obs")

r[grep("^red$",r$Adjective,perl=T),]$NP
r[grep("^tall$",r$Adjective,perl=T),]$NP

r[r$PrevPrevAdj == "yes",]

nrow(r[r$PrevAdj == "yes",])
sort(table(rb[rb$PrevAdj == "yes",]$Adjective))

row.names(bnc_adjfreqs) = bnc_adjfreqs$Var1
row.names(bnc_padjfreqs) = bnc_padjfreqs$Var1
row.names(bnc_ppadjfreqs) = bnc_ppadjfreqs$Var1
tentative_finalset = c("blue", "green", "red", "purple", "yellow", "brown","big","tiny","long", "short", "huge", "small","wooden", "hard", "soft", "smooth", "plastic", "metallic", "old", "young", "fresh", "rotten", "open", "closed", "full", "empty","round","square")

d = data.frame(Adjective = factor(x=tentative_finalset))
d$BNCFreqDist1 = bnc_adjfreqs[as.character(d$Adjective),]$Freq
d$BNCFreqDist2 = bnc_padjfreqs[as.character(d$Adjective),]$Freq
d$BNCFreqDist3 = bnc_ppadjfreqs[as.character(d$Adjective),]$Freq
d$logFreqDist1 = log(d$BNCFreqDist1)
d$logFreqDist2 = log(d$BNCFreqDist2)
d$logFreqDist3 = log(d$BNCFreqDist3)

ggplot(d, aes(x=logFreqDist1,y=logFreqDist2)) +
  geom_point() +
  geom_text(aes(label=Adjective,x=logFreqDist1+.2)) 
ggsave(file="graphs/adj_freqs.pdf",height=6)


# get most frequent nouns by adjective
dd = droplevels(subset(rb, Adjective %in% tentative_finalset))
t = as.data.frame(table(dd$Adjective, dd$Noun))
t = droplevels(t[t$Freq != 0,])
colnames(t) = c("Adjective","Noun","Freq")
nrow(t)
head(t)
t = t[order(t[,"Freq"],decreasing=T),]
head(t,70)

# nouns to test in the experiment (subsets of what Koolen et al 2011 and Westerbeek et al 2014 used)
t[t$Noun %in% c("chair","couch","fan","tv","desk","apple","banana","carrot","tomato","cheese"),]

size = c("little","big","tiny","long", "short", "large", "small", "skinny", "wide", "tall", "huge", "heavy")
age = c("old","new", "elderly", "young", "fresh", "rotten")
quality = c("good","bad", "cheap", "expensive")
color = c("white", "blue", "green", "red", "black", "purple", "yellow", "brown", "golden", "orange", "gray", "pink")
material = c("wooden", "hard", "soft", "solid", "smooth", "shiny", "flat","rough", "plastic")
mood = c("sad", "happy")
volume = c("full", "empty", "deep", "shallow")
temperature = c("hot", "cold")
strength = c("strong","weak")
other = c("open", "closed", "dark", "bright")

# data.frame with only the data points for the adjectives that we tested in the faultless disagreement study
adjs = data.frame(Adjective = c('red', 'yellow', 'green', 'blue', 'purple', 'brown', 'big', 'small', 'huge', 'tiny', 'short', 'long', 'wooden', 'plastic', 'metal', 'smooth', 'hard', 'soft', 'old', 'new', 'rotten', 'fresh', 'good', 'bad', 'round', 'square'), Class = c('color', 'color', 'color', 'color', 'color', 'color', 'size', 'size', 'size', 'size', 'size', 'size', 'texture', 'texture', 'texture', 'texture', 'texture', 'texture', 'age', 'age', 'age', 'age', 'quality', 'quality', 'shape', 'shape'))
str(adjs)
row.names(adjs) = adjs$Adjective

# plot mean distance from noun for all adjectives we tested
d_exp = droplevels(rb[rb$Adjective %in% adjs$Adjective | rb$PrevAdjective %in% adjs$Adjective | rb$PrevPrevAdjective %in% adjs$Adjective,])
nrow(d_exp)
table(d_exp$PrevPrevAdj) # only 111 cases with 3 adjs
head(d_exp[d_exp$PrevPrevAdj == "yes",])
d_exp[d_exp$PrevAdjective == "",]$PrevAdjective = NA
d_exp[d_exp$PrevPrevAdjective == "",]$PrevPrevAdjective = NA

gathered = d_exp %>% 
            select(Adjective, PrevAdjective, PrevPrevAdjective) %>% 
            gather(Position, Adjective,  Adjective:PrevPrevAdjective)
gathered = gathered[!is.na(gathered$Adjective),]
nrow(gathered)
head(gathered)
summary(gathered)
gathered$DistanceFromNoun = 1
gathered[gathered$Position == "PrevAdjective",]$DistanceFromNoun = 2
gathered[gathered$Position == "PrevPrevAdjective",]$DistanceFromNoun = 3
unique(paste(gathered$Position,gathered$DistanceFromNoun))
head(gathered)
gathered$Class = adjs[as.character(gathered$Adjective),]$Class
summary(gathered)
# remove all the adjectives that didn't occur in the experiment
gathered = droplevels(gathered[gathered$Adjective %in% adjs$Adjective,])
summary(gathered)


# plot adjective's mean distance from noun by class
agr = aggregate(DistanceFromNoun ~ Class, data=gathered, FUN=mean)
agr$CILow = aggregate(DistanceFromNoun ~ Class, data=gathered,FUN="ci.low")$DistanceFromNoun
agr$CIHigh = aggregate(DistanceFromNoun ~ Class, data=gathered, FUN="ci.high")$DistanceFromNoun
agr$YMin = agr$DistanceFromNoun - agr$CILow
agr$YMax = agr$DistanceFromNoun + agr$CIHigh
agr = agr[order(agr[,c("DistanceFromNoun")],decreasing=T),]
agr$AdjClass = factor(x=as.character(agr$Class),levels=as.character(agr$Class))

ggplot(agr, aes(x=AdjClass,y=DistanceFromNoun)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_hline(yintercept =1)
ggsave("graphs/mean_distance_from_noun_all.pdf")

# plot adjective's mean distance from noun by class, only for cases where there's more than one prenominal modifier
d_subexp = d_exp[d_exp$PrevAdj == "yes" | d_exp$PrevPrevAdj == "yes",]
nrow(d_subexp) #total of 1985 cases instead of 22944...

gathered = d_subexp %>% 
  select(Adjective, PrevAdjective, PrevPrevAdjective) %>% 
  gather(Position, Adjective,  Adjective:PrevPrevAdjective)
gathered = gathered[!is.na(gathered$Adjective),]
nrow(gathered) #total: 4081 cases
head(gathered)
summary(gathered)
gathered$DistanceFromNoun = 1
gathered[gathered$Position == "PrevAdjective",]$DistanceFromNoun = 2
gathered[gathered$Position == "PrevPrevAdjective",]$DistanceFromNoun = 3
unique(paste(gathered$Position,gathered$DistanceFromNoun))
head(gathered)
gathered$Class = adjs[as.character(gathered$Adjective),]$Class
summary(gathered)
# remove all the adjectives that didn't occur in the experiment
gathered = droplevels(gathered[gathered$Adjective %in% adjs$Adjective,])
nrow(gathered) # to plot: 2232 cases
summary(gathered)

agr = aggregate(DistanceFromNoun ~ Class, data=gathered, FUN=mean)
agr$CILow = aggregate(DistanceFromNoun ~ Class, data=gathered,FUN="ci.low")$DistanceFromNoun
agr$CIHigh = aggregate(DistanceFromNoun ~ Class, data=gathered, FUN="ci.high")$DistanceFromNoun
agr$YMin = agr$DistanceFromNoun - agr$CILow
agr$YMax = agr$DistanceFromNoun + agr$CIHigh
agr = agr[order(agr[,c("DistanceFromNoun")],decreasing=T),]
agr$AdjClass = factor(x=as.character(agr$Class),levels=as.character(agr$Class))

ggplot(agr, aes(x=AdjClass,y=DistanceFromNoun)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_hline(yintercept =1)
ggsave("graphs/mean_distance_from_noun_morethanonemodifier.pdf")

# plot adjective's mean distance from noun by class, only for cases where there's more than two prenominal modifiers
d_subexp = d_exp[d_exp$PrevPrevAdj == "yes",]
nrow(d_subexp) #total of 111 cases instead of 22944 or even 1985...

gathered = d_subexp %>% 
  select(Adjective, PrevAdjective, PrevPrevAdjective) %>% 
  gather(Position, Adjective,  Adjective:PrevPrevAdjective)
gathered = gathered[!is.na(gathered$Adjective),]
nrow(gathered) 
head(gathered)
summary(gathered)
gathered$DistanceFromNoun = 1
gathered[gathered$Position == "PrevAdjective",]$DistanceFromNoun = 2
gathered[gathered$Position == "PrevPrevAdjective",]$DistanceFromNoun = 3
unique(paste(gathered$Position,gathered$DistanceFromNoun))
head(gathered)
gathered$Class = adjs[as.character(gathered$Adjective),]$Class
summary(gathered)
# remove all the adjectives that didn't occur in the experiment
gathered = droplevels(gathered[gathered$Adjective %in% adjs$Adjective,])
nrow(gathered) # to plot: 138 cases
summary(gathered)

agr = aggregate(DistanceFromNoun ~ Class, data=gathered, FUN=mean)
agr$CILow = aggregate(DistanceFromNoun ~ Class, data=gathered,FUN="ci.low")$DistanceFromNoun
agr$CIHigh = aggregate(DistanceFromNoun ~ Class, data=gathered, FUN="ci.high")$DistanceFromNoun
agr$YMin = agr$DistanceFromNoun - agr$CILow
agr$YMax = agr$DistanceFromNoun + agr$CIHigh
agr = agr[order(agr[,c("DistanceFromNoun")],decreasing=T),]
agr$AdjClass = factor(x=as.character(agr$Class),levels=as.character(agr$Class))

ggplot(agr, aes(x=AdjClass,y=DistanceFromNoun)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_hline(yintercept =1)
ggsave("graphs/mean_distance_from_noun_morethantwomodifiers.pdf")





