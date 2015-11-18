library(ggplot2)
theme_set(theme_bw(18))
setwd("~/cogsci/projects/stanford/projects/adjective_ordering/corpus_results/noun_effects")
#for greg
#setwd("~/Documents/git/CoCoLab/adjective_ordering/corpus_results/")
source("rscripts/helpers.r")
load("data/dsub.bnc.RData")
nrow(dsub)

# set corpus to one of "swbd" or "bnc"
corpus = "swbd"
corpus = "bnc"
# try first with swbd because smaller
d = read.table(paste("data/",corpus,".tab",sep=""), sep="\t", header=T, quote="")
totalcases = nrow(d)
adjs = c('red', 'yellow', 'green', 'blue', 'purple', 'brown', 'big', 'small', 'huge', 'tiny', 'short', 'long', 'wooden', 'plastic', 'metal', 'smooth', 'hard', 'soft', 'old', 'new', 'rotten', 'fresh', 'good', 'bad', 'round', 'square')

# Do two different analyses: 
# 1. Compute expected joint frequency from global frequencies of nouns and adjectives.
d = droplevels(subset(d, !is.na(P_Adjective) & !is.na(P_Noun)))
leftcases = nrow(d)
print(paste("cases left after excluding NAs: ",leftcases, " ",leftcases/totalcases,"%"))
d$expJPFromGlobal = d$P_Adjective*d$P_Noun # expected joint probability of adjective and noun
total_word_freq = (as.numeric(as.character(d$FQ_Adjective))/as.numeric(as.character(d$P_Adjective)))[1]
d$JP_Adjective = d$JFQ_Adjective/total_word_freq # probability of adj occurring in AN frame
d$JP_Noun = d$JFQ_Noun/total_word_freq # probability of noun occurring in AN frame

# to compute actual joint probability of adjective and noun, you need to get a table of occurrences of each combination, then divide those numbers by total word count
dsub = droplevels(subset(d, Adjective %in% adjs))
nrow(dsub)
t = as.data.frame(table(dsub$Adjective,dsub$Noun))
colnames(t) = c("Adjective","Noun","Freq")
t = droplevels(subset(t, Freq > 0))
t$Probability = t$Freq/total_word_freq
summary(t)
row.names(t) = paste(t$Adjective,t$Noun)
dsub$JointProbability = t[paste(dsub$Adjective,dsub$Noun),]$Probability

# 2. Compute expected joint frequency from frequencies of nouns and adjectives as occurring in the AN frame.
totalcases = nrow(d)
dsub$JointProbabilityAN = t[paste(dsub$Adjective,dsub$Noun),]$Freq/totalcases
dsub$PAN_Adjective = dsub$JFQ_Adjective/totalcases
dsub$PAN_Noun = dsub$JFQ_Noun/totalcases
dsub$expJPFromAN = dsub$PAN_Adjective*dsub$PAN_Noun

#####################
# PLOTS
#####################

# get unique adj-noun combos
toplot = unique(dsub[,c("Adjective","Noun","expJPFromGlobal","JointProbability","expJPFromAN","JointProbabilityAN")])
row.names(toplot) = paste(toplot$Adjective,toplot$Noun)
nrow(toplot)
toplot$Combination = as.factor(paste(toplot$Adjective,toplot$Noun))
toplot = droplevels(toplot[toplot$Adjective %in% adjs,]) # create plot only for the 26 adjectives we've been using

#lowfreq = droplevels(subset(toplot, expJPFromGlobal < .000001 & JointProbability < .000025))

save(dsub, file="data/dsub.bnc.RData")

# first plotting with global frequency estimates
ggplot(toplot, aes(x=expJPFromGlobal,y=JointProbability)) +
#ggplot(lowfreq, aes(x=expJPFromGlobal,y=JointProbability)) +
  geom_point() +
  geom_text(aes(label=Combination)) +
  geom_abline(intercept=0,slope=1,color="gray60")
ggsave(paste("graphs/exp.vs.emp.global-",corpus,".pdf",sep=""),width=25,height=20)

# then plotting with AN frequency estimates
ggplot(toplot, aes(x=expJPFromAN,y=JointProbabilityAN)) +
  geom_point() +
  geom_text(aes(label=Combination)) +
  geom_abline(intercept=0,slope=1,color="gray60")
ggsave(paste("graphs/exp.vs.emp.AN-",corpus,".pdf",sep=""),width=25,height=20)

# then plotting with AN frequency estimates by adjective
ggplot(toplot, aes(x=expJPFromAN,y=JointProbabilityAN)) +
  geom_point() +
  geom_text(aes(label=Combination)) +
  geom_abline(intercept=0,slope=1,color="gray60") +
  facet_wrap(~Adjective,scales="free")
ggsave(paste("graphs/exp.vs.emp.byadjective.AN-",corpus,".pdf",sep=""),width=40,height=30)

# check correlation between joint probabilities computed from global frequencies vs frequency of occurring in AN frame
ggplot(toplot, aes(x=expJPFromGlobal,y=expJPFromAN)) +
  geom_point() +
  geom_text(aes(label=Combination))
ggsave(paste("graphs/correlation.expjointprobabilities-",corpus,".pdf",sep=""),width=25,height=20)

ggplot(toplot, aes(x=JointProbability,y=JointProbabilityAN)) +
  geom_point() +
  geom_text(aes(label=Combination))
ggsave(paste("graphs/correlation.empjointprobabilities-",corpus,".pdf",sep=""),width=25,height=20)


### compute ratio of empirical vs expected joint probability of AN
dsub$RatioOfEmpToExpJP = dsub$JointProbabilityAN/dsub$expJPFromAN
un = unique(dsub[,c("Adjective","Noun","RatioOfEmpToExpJP")])
summary(un)
agr = un %>%
  group_by(Noun) %>%
  summarise(MeanRatio = mean(RatioOfEmpToExpJP))
agr = as.data.frame(agr)
summary(agr)
agr[agr$MeanRatio < .2,]
agr[agr$MeanRatio > 7000,]



# to see which nouns occur with the most fo the 26 adjectives
adjtest = un %>% 
  group_by(Noun) %>%
  summarise(Freq=length(RatioOfEmpToExpJP))
adjtest = as.data.frame(adjtest)
nrow(adjtest)
max(adjtest$Freq)
ordered = adjtest[order(adjtest[,c("Freq")],decreasing=T),]
head(ordered,20)
ordered[ordered$Noun %in% c("thing","eyes","hair","apple","cheese"),]

# to get the range of ratios for each noun -- to be reported in paper supplement
adjtest = un %>% 
  group_by(Noun) %>%
  summarise(Range=max(RatioOfEmpToExpJP) - min(RatioOfEmpToExpJP),Freq=length(RatioOfEmpToExpJP),MinRatio=min(RatioOfEmpToExpJP),MaxRatio=max(RatioOfEmpToExpJP))
adjtest = as.data.frame(adjtest)
adjtest = droplevels(adjtest[adjtest$Freq > 10,])
nrow(adjtest)
max(adjtest$Range)
min(adjtest$Range)

ordered = adjtest[order(adjtest[,"Range"],decreasing=T),]
ordered$Order = seq(1,nrow(ordered))
head(ordered,30)
ordered[ordered$Noun %in% c("thing","eyes","hair","apple","cheese"),]

# plot range of predicted vs empirical joint probabilities for 10 good-looking nouns
nouns = c("thing","eyes","hair","cheese","apple")
#nouns = ordered$Noun[1:20]
ggplot(toplot[toplot$Noun %in% nouns,], aes(x=expJPFromAN,y=JointProbabilityAN)) +
  geom_point() +
  geom_text(aes(label=Combination)) +
  geom_abline(intercept=0,slope=1,color="gray60") +
  facet_wrap(~Noun,scales="free")
ggsave("graphs/by-noun-jps-bnc.pdf",width=20,height=15)
