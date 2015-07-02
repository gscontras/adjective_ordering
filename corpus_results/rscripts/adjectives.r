library(ggplot2)
library(plyr)
theme_set(theme_bw(18))
setwd("~/cogsci/projects/stanford/projects/adjective_ordering/corpus_results/")
#for greg
#setwd("~/Documents/git/CoCoLab/adjective_ordering/corpus_results/")
source("rscripts/helpers.r")

#load("data/r.RData")
rs = read.table("data/swbd.tab", sep="\t", header=T, quote="")
rs$Corpus = "swbd"
nrow(rs) # 15'744 cases in swbd
rb = read.table("data/bncs.tab", sep="\t", header=T, quote="")
rb$Corpus = "bncs" 
nrow(rb) # 201'261 cases in bncs
head(rb)
rbw = read.table("data/bncw.tab", sep="\t", header=T, quote="")
rbw$Corpus = "bncw"
nrow(rbw) # 270'695 (already restricted) cases in bncw, over 4 million without restriction
head(rbw)

####### NOT INTERESTING


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


###### ALSO NEED THESE

# data.frame with only the data points for the adjectives that we tested in the faultless disagreement study
adjs = data.frame(Adjective = c('red', 'yellow', 'green', 'blue', 'purple', 'brown', 'big', 'small', 'huge', 'tiny', 'short', 'long', 'wooden', 'plastic', 'metal', 'smooth', 'hard', 'soft', 'old', 'new', 'rotten', 'fresh', 'good', 'bad', 'round', 'square'), Class = c('color', 'color', 'color', 'color', 'color', 'color', 'size', 'size', 'size', 'size', 'size', 'size', 'material', 'material', 'material', 'texture', 'texture', 'texture', 'age', 'age', 'age', 'age', 'quality', 'quality', 'shape', 'shape'))
str(adjs)
row.names(adjs) = adjs$Adjective

nouns = data.frame(Noun = c("apple","banana","carrot","cheese","tomato","orange","cherry","cranberry","grape","grapefruit","pear","pomegranate","raspberry","strawberry","watermelon","beet","pepper","radish","onion","potato","rhubarb","apricot","cantaloupe","fig","kiwi","lemon","mango","nectarine","papaya","peach","persimmon","pumpkin","rutabaga","squash","corn","pineapple","tangerine","cauliflower","garlic","ginger","artichoke","kohlrabi","mushroom","parsnip","shallot","turnip","avocado","lime","pea","arugula","asparagus","broccoli","sprouts","cabbage","bean","celery","cucumber","leek","okra","spinach","zucchini","blackberry","blueberry","currant","plum","raisin","olive","eggplant","apples","bananas","carrots","cheeses","tomatoes","oranges","cherries","cranberries","grapes","grapefruits","pears","pomegranates","raspberries","strawberries","watermelons","beets","peppers","radishes","onions","potatoes","apricots","cantaloupes","figs","kiwis","lemons","mangoes","nectarines","papayas","peaches","persimmons","pumpkins","rutabagas","pineapples","tangerines","artichokes","kohlrabis","mushrooms","parsnips","shallots","turnips","avocadoes","limes","peas","cabbages","beans","celeries","cucumbers","leeks","zucchinis","watercress","blackberries","blueberries","currants","plums","raisins","olives","eggplants","chair","couch","fan","tv","desk","recliner","stool","sofa","bench","seat","bed","futon","hammock","mattress","table","television","bookcase","shelf","bookshelf","cabinet","closet","pantry","chest","drawer","nightstand","sideboard","wardrobe","chairs","couches","fans","tvs","desks","recliners","stools","sofas","benches","seats","beds","futons","hammocks","mattresses","tables","televisions","bookcases","shelves","bookshelves","cabinets","closets","pantries","chests","drawers","nightstands","sideboards","wardrobes"), NounClass = c(rep("food",125),rep("furniture",54)))
row.names(nouns) = nouns$Noun

#######################################################################
########### HERE'S WHERE ALL THE INTERESTING STUFF STARTS #############
#######################################################################

d = rbind(rb,rs)#,rbw)
rb = d
d = merge(rb, rbw, all=T)
nrow(d)
rb=d
# plot mean distance from noun for all adjectives we tested
d_exp = droplevels(rb[rb$Adjective %in% adjs$Adjective | rb$PrevAdjective %in% adjs$Adjective | rb$PrevPrevAdjective %in% adjs$Adjective,])
nrow(d_exp)
table(d_exp$Corpus)
table(d_exp$Corpus,d_exp$PrevAdj)
table(d_exp$Corpus,d_exp$PrevPrevAdj) # only 111 cases with 3 adjs
head(d_exp[d_exp$PrevPrevAdj == "yes",])
d_exp[d_exp$PrevAdjective == "",]$PrevAdjective = NA
d_exp[d_exp$PrevPrevAdjective == "",]$PrevPrevAdjective = NA

gathered = d_exp %>% 
            select(Adjective, PrevAdjective, PrevPrevAdjective, Corpus, Noun) %>% 
            gather(Position, Adjective,  Adjective:PrevPrevAdjective)
gathered = gathered[!is.na(gathered$Adjective),]
gathered$Corpus = as.factor(as.character(gathered$Corpus))
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
nrow(gathered) # 25499 cases

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

#plot by class and corpus
agr = aggregate(DistanceFromNoun ~ Class + Corpus, data=gathered, FUN=mean)
agr$CILow = aggregate(DistanceFromNoun ~ Class + Corpus, data=gathered,FUN="ci.low")$DistanceFromNoun
agr$CIHigh = aggregate(DistanceFromNoun ~ Class + Corpus, data=gathered, FUN="ci.high")$DistanceFromNoun
agr$YMin = agr$DistanceFromNoun - agr$CILow
agr$YMax = agr$DistanceFromNoun + agr$CIHigh
agr = agr[order(agr[,c("DistanceFromNoun")],decreasing=T),]
agr$AdjClass = factor(x=as.character(agr$Class),levels=unique(as.character(agr$Class)))

ggplot(agr, aes(x=AdjClass,y=DistanceFromNoun)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_hline(yintercept =1) +
  facet_wrap(~Corpus)
ggsave("graphs/mean_distance_from_noun_all_bycorpus.pdf")

# plot adjective's mean distance from noun by class, only for cases where there's more than one prenominal modifier -- THIS IS THE STUFF YOU"RE REPORTING IN THE PAPER
d_subexp = d_exp[d_exp$PrevAdj == "yes" | d_exp$PrevPrevAdj == "yes",]
nrow(d_subexp) #total of 35721 cases

gathered = d_subexp %>% 
  select(Adjective, PrevAdjective, PrevPrevAdjective, Corpus, Noun) %>% 
  gather(Position, Adjective,  Adjective:PrevPrevAdjective)
gathered = gathered[!is.na(gathered$Adjective),]
nrow(gathered) #total: 107163 cases
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
nrow(gathered) # to plot: 39199 cases
summary(gathered)

table(gathered$Corpus)

nrow(gathered[gathered$Noun %in% nouns$Noun,])
d_subexp[d_subexp$Noun %in% nouns$Noun,]
bynoun = droplevels(gathered[gathered$Noun %in% nouns$Noun,])
bynoun$NounClass = nouns[as.character(bynoun$Noun),]$NounClass
nrow(bynoun)

sort(table(bynoun$Class),decreasing=T)
round(sort(prop.table(table(bynoun$Class)),decreasing=T),3)

agr = aggregate(DistanceFromNoun ~ NounClass + Class + Adjective, data=bynoun, FUN=mean)
agr$CILow = aggregate(DistanceFromNoun ~ NounClass + Class + Adjective, data=bynoun,FUN="ci.low")$DistanceFromNoun
agr$CIHigh = aggregate(DistanceFromNoun ~ NounClass + Class + Adjective, data=bynoun, FUN="ci.high")$DistanceFromNoun
agr$YMin = agr$DistanceFromNoun - agr$CILow
agr$YMax = agr$DistanceFromNoun + agr$CIHigh
agr = agr[order(agr[,c("DistanceFromNoun")],decreasing=T),]
agr$AdjClass = factor(x=as.character(agr$Class),levels=as.character(agr$Class))
dodge=position_dodge(.9)

ggplot(agr, aes(x=Adjective,y=DistanceFromNoun, fill=NounClass)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  geom_hline(yintercept =1) +
  facet_wrap(~AdjClass,scales="free_x")
ggsave("graphs/mean_distance_from_noun_morethanonemodifier_byadjective_bynounclass.pdf",width=12)
ggsave("graphs/mean_distance_from_noun_morethanonemodifier_byadjective_bynounclass.jpg")

agr = aggregate(DistanceFromNoun ~ NounClass + Class, data=bynoun, FUN=mean)
agr$CILow = aggregate(DistanceFromNoun ~ NounClass + Class, data=bynoun,FUN="ci.low")$DistanceFromNoun
agr$CIHigh = aggregate(DistanceFromNoun ~ NounClass + Class, data=bynoun, FUN="ci.high")$DistanceFromNoun
agr$YMin = agr$DistanceFromNoun - agr$CILow
agr$YMax = agr$DistanceFromNoun + agr$CIHigh
agr = agr[order(agr[,c("DistanceFromNoun")],decreasing=T),]
agr$AdjClass = factor(x=as.character(agr$Class),levels=as.character(agr$Class))
dodge=position_dodge(.9)

ggplot(agr, aes(x=AdjClass,y=DistanceFromNoun, fill=NounClass)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  geom_hline(yintercept =1) 
ggsave("graphs/mean_distance_from_noun_morethanonemodifier_bynounclass.pdf")
ggsave("graphs/mean_distance_from_noun_morethanonemodifier_bynounclass.jpg")



gathered[gathered$Adjective %in% c("wooden","plastic","metal"),]
d_subexp[d_subexp$PrevAdjective %in% c("wooden","plastic","metal"),]
gathered[gathered$Adjective %in% c("soft","hard","smooth"),]
d_subexp[d_subexp$PrevAdjective %in% c("soft","hard","smooth"),]
d_subexp[d_subexp$Adjective %in% c("square","round") | d_subexp$PrevAdjective %in% c("square","round") | d_subexp$PrevPrevAdjective %in% c("square","round"),]$NP

## pairwise comparison

pairwise.t.test(gathered$DistanceFromNoun, gathered$Class, p.adj = "bonf")
round(pairwise.t.test(gathered$DistanceFromNoun, gathered$Class, p.adj = "bonf")$p.value,4)

m = lmer(DistanceFromNoun ~ Class + (1|Noun) + (1|Corpus), data=gathered)
summary(m)

agr = aggregate(DistanceFromNoun ~ Class, data=gathered, FUN=mean)
agr$CILow = aggregate(DistanceFromNoun ~ Class, data=gathered,FUN="ci.low")$DistanceFromNoun
agr$CIHigh = aggregate(DistanceFromNoun ~ Class, data=gathered, FUN="ci.high")$DistanceFromNoun
agr$YMin = agr$DistanceFromNoun - agr$CILow
agr$YMax = agr$DistanceFromNoun + agr$CIHigh
agr = agr[order(agr[,c("DistanceFromNoun")],decreasing=T),]
agr$AdjClass = factor(x=as.character(agr$Class),levels=as.character(agr$Class))

ggplot(agr, aes(x=AdjClass,y=(DistanceFromNoun-1))) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin-1,ymax=YMax-1),width=.1) +
  theme_bw()+
  ylim(0,1) +
  xlab("\nadjective class") +
  ylab("distance from noun\n")
  #geom_hline(yintercept =1)
ggsave("graphs/mean_distance_from_noun_morethanonemodifier.pdf")
ggsave("graphs/corpus_distance_plot.pdf",height=3)

agr = aggregate(DistanceFromNoun ~ Class + Corpus, data=gathered, FUN=mean)
agr$CILow = aggregate(DistanceFromNoun ~ Class + Corpus, data=gathered,FUN="ci.low")$DistanceFromNoun
agr$CIHigh = aggregate(DistanceFromNoun ~ Class + Corpus, data=gathered, FUN="ci.high")$DistanceFromNoun
agr$YMin = agr$DistanceFromNoun - agr$CILow
agr$YMax = agr$DistanceFromNoun + agr$CIHigh
agr = agr[order(agr[,c("DistanceFromNoun")],decreasing=T),]
agr$AdjClass = factor(x=as.character(agr$Class),levels=unique(as.character(agr$Class)))

ggplot(agr, aes(x=AdjClass,y=DistanceFromNoun)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_hline(yintercept =1) +
  facet_wrap(~Corpus)
ggsave("graphs/mean_distance_from_noun_morethanonemodifier_bycorpus.pdf",width=14)
ggsave("graphs/mean_distance_from_noun_morethanonemodifier_bycorpus.jpg",width=10)


agr = aggregate(DistanceFromNoun ~ Class + Adjective, data=gathered, FUN=mean)
agr$CILow = aggregate(DistanceFromNoun ~ Class + Adjective, data=gathered,FUN="ci.low")$DistanceFromNoun
agr$CIHigh = aggregate(DistanceFromNoun ~ Class + Adjective, data=gathered, FUN="ci.high")$DistanceFromNoun
agr$YMin = agr$DistanceFromNoun - agr$CILow
agr$YMax = agr$DistanceFromNoun + agr$CIHigh
agr = agr[order(agr[,c("DistanceFromNoun")],decreasing=T),]
agr$Adj = factor(x=as.character(agr$Adjective),levels=unique(as.character(agr$Adjective)))
#agr$Class <- factor(d$Class,levels=c("quality","size","age","texture","shape","color"))

ggplot(agr, aes(x=Adj,y=DistanceFromNoun)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_hline(yintercept =1) +
  facet_wrap(~Class,scales="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
ggsave("graphs/mean_distance_from_noun_morethanonemodifier_byadj.pdf",width=8,height=7)
ggsave("graphs/mean_distance_from_noun_morethanonemodifier_byadj.jpg",width=8,height=7)

# what are the cases of color occurring far away from the noun?
d_exp[d_exp$PrevAdjective %in% c("red","blue","green","brown","yellow","purple") | d_exp$PrevPrevAdjective %in% c("red","blue","green","brown","yellow","purple"),]$NP


# add frequencies
tmp = d_subexp %>% 
  select(P_Adjective,P_PrevAdjective,P_PrevPrevAdjective,P_Noun,Adjective, PrevAdjective, PrevPrevAdjective, Corpus, Noun) 
head(tmp)
tmp$AdjFreq = paste(tmp$Adjective,tmp$P_Adjective)
tmp$PAdjFreq = paste(tmp$PrevAdjective,tmp$P_PrevAdjective)
tmp$PPAdjFreq = paste(tmp$PrevPrevAdjective,tmp$P_PrevPrevAdjective)

gathere = tmp %>% 
  select(AdjFreq,PAdjFreq,PPAdjFreq,Noun,Corpus,P_Noun) %>%
  gather(Position, AdjFrequency,  AdjFreq:PPAdjFreq)
gathere = gathere[!is.na(gathere$AdjFrequency),]
head(gathere)
gathere$Adjective = sapply(strsplit(as.character(gathere$AdjFrequency)," "), "[", 1)
gathere$Frequency = sapply(strsplit(as.character(gathere$AdjFrequency)," "), "[", 2)
gathere$Adjective = as.factor(as.character(gathere$Adjective))
gathere$Frequency = as.numeric(as.character(gathere$Frequency))
gathere = droplevels(gathere[!is.na(gathere$Frequency),])
summary(gathere)
gathere$DistanceFromNoun = 1
gathere[gathere$Position == "PAdjFreq",]$DistanceFromNoun = 2
gathere[gathere$Position == "PPAdjFreq",]$DistanceFromNoun = 3
unique(paste(gathere$Position,gathere$DistanceFromNoun))
head(gathere)
gathere$Class = adjs[as.character(gathere$Adjective),]$Class
summary(gathere)

agr = aggregate(Frequency ~ Class + DistanceFromNoun, data=gathere, FUN=mean)
agr$CILow = aggregate(Frequency ~ Class + DistanceFromNoun, data=gathere,FUN="ci.low")$Frequency
agr$CIHigh = aggregate(Frequency ~ Class + DistanceFromNoun, data=gathere, FUN="ci.high")$Frequency
agr$YMin = agr$Frequency - agr$CILow
agr$YMax = agr$Frequency + agr$CIHigh
agr = agr[order(agr[,c("Frequency")],decreasing=T),]
agr$Class <- factor(agr$Class,levels=c("quality","size","age","texture","shape","color","material"))
dodge=position_dodge(.9)

ggplot(agr, aes(x=Class,y=Frequency,fill=as.factor(DistanceFromNoun))) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  #facet_wrap(~Class,scales="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
ggsave("graphs/meanfrequency_bydistance_byclass.pdf")


agr = aggregate(Frequency ~ Class + DistanceFromNoun + Corpus, data=gathere, FUN=mean)
agr$CILow = aggregate(Frequency ~ Class + DistanceFromNoun + Corpus, data=gathere,FUN="ci.low")$Frequency
agr$CIHigh = aggregate(Frequency ~ Class + DistanceFromNoun + Corpus, data=gathere, FUN="ci.high")$Frequency
agr$YMin = agr$Frequency - agr$CILow
agr$YMax = agr$Frequency + agr$CIHigh
agr = agr[order(agr[,c("Frequency")],decreasing=T),]
agr$Class <- factor(agr$Class,levels=c("quality","size","age","texture","shape","color","material"))
dodge=position_dodge(.9)

ggplot(agr, aes(x=Class,y=Frequency,fill=as.factor(DistanceFromNoun))) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~Corpus,scales="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))

agr = unique(gathere[!is.na(gathere$Class),c("Adjective","Frequency","Class","Corpus")])
nrow(agr)
ggplot(agr, aes(x=Adjective,y=Frequency,fill=Corpus)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~Class, scales="free_x")
ggsave("graphs/adj_freqs_bycorpus.pdf")

agr = aggregate(Frequency ~ Class + DistanceFromNoun + Adjective, data=gathere, FUN=mean)
agr$CILow = aggregate(Frequency ~ Class + DistanceFromNoun + Adjective, data=gathere,FUN="ci.low")$Frequency
agr$CIHigh = aggregate(Frequency ~ Class + DistanceFromNoun + Adjective, data=gathere, FUN="ci.high")$Frequency
agr$YMin = agr$Frequency - agr$CILow
agr$YMax = agr$Frequency + agr$CIHigh
agr = agr[order(agr[,c("Frequency")],decreasing=T),]
agr$Class <- factor(agr$Class,levels=c("quality","size","age","texture","shape","color","material"))
dodge=position_dodge(.9)

ggplot(agr, aes(x=Adjective,y=Frequency,fill=as.factor(DistanceFromNoun))) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~Class,scales="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))

levels(gathere$Class) = c(levels(gathere$Class), "classless")
gathere[is.na(gathere$Class),]$Class = "classless"
# 
# agr = aggregate(Frequency ~ Class + DistanceFromNoun, data=gathere, FUN=mean)
# agr$CILow = aggregate(Frequency ~ Class + DistanceFromNoun, data=gathere,FUN="ci.low")$Frequency
# agr$CIHigh = aggregate(Frequency ~ Class + DistanceFromNoun, data=gathere, FUN="ci.high")$Frequency
# agr$YMin = agr$Frequency - agr$CILow
# agr$YMax = agr$Frequency + agr$CIHigh
# agr = agr[order(agr[,c("Frequency")],decreasing=T),]
# agr$Class <- factor(agr$Class,levels=c("quality","size","age","texture","shape","color","material"))
# dodge=position_dodge(.9)
# 
# ggplot(agr, aes(x=Class,y=Frequency,fill=as.factor(DistanceFromNoun))) +
#   geom_bar(stat="identity",position=dodge) +
#   geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
#   #facet_wrap(~Class,scales="free_x") +
#   theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
# ggsave("graphs/meanfrequency_bydistance_byclass_all.pdf")




# plot adjective's mean distance from noun by class, only for cases where there's more than two prenominal modifiers
d_subexp = d_exp[d_exp$PrevPrevAdj == "yes",]
nrow(d_subexp) #total of 1778 cases

gathered = d_subexp %>% 
  select(Adjective, PrevAdjective, PrevPrevAdjective, Corpus) %>% 
  gather(Position, Adjective,  Adjective:PrevPrevAdjective)
gathered = gathered[!is.na(gathered$Adjective),]
nrow(gathered)  # 5334 cases
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
nrow(gathered) # to plot: 2091 cases
summary(gathered)

sort(table(gathered$Class),decreasing=T)
round(sort(prop.table(table(gathered$Class)),decreasing=T),2)

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
ggsave("graphs/mean_distance_from_noun_morethantwomodifiers.jpg")





