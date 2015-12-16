theme_set(theme_bw(18))
#for judith
setwd("~/cogsci/projects/stanford/projects/adjective_ordering/corpus_results/")
#for greg
#setwd("~/Documents/git/CoCoLab/adjective_ordering/corpus_results/")
source("rscripts/helpers.r")

adjs = read.table("data/adjs_to_choose_from.txt",sep="\t", header=T,quote="")
row.names(adjs) = adjs$Adjective
d = read.table("data/swbd-freqs.tab",sep="\t",header=T, quote="")
d$Item_ID = NULL
d = unique(d)
d = droplevels(d[d$Adjective %in% as.character(adjs$Adjective),])
d$Class = adjs[as.character(d$Adjective),]$Class
d$Length = nchar(as.character(d$Adjective))
d$logProbability = log(d$P_Adjective)
head(d)
nrow(d)

agr = d %>%
  group_by(Class) %>%
  summarise(MeanProb=mean(P_Adjective),ci.low=ci.low(P_Adjective),ci.high=ci.high(P_Adjective))
agr = as.data.frame(agr)
agr$YMin = agr$MeanProb - agr$ci.low
agr$YMax = agr$MeanProb + agr$ci.high

ggplot(agr, aes(x=Class,y=MeanProb)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax),width=.25)
ggsave("graphs/adjective-selection/meanprobs_adjectives.pdf",width=12)

agr = d %>%
  group_by(Class) %>%
  summarise(MeanLogProb=mean(logProbability),ci.low=ci.low(logProbability),ci.high=ci.high(logProbability))
agr = as.data.frame(agr)
agr$YMin = agr$MeanLogProb - agr$ci.low
agr$YMax = agr$MeanLogProb + agr$ci.high

ggplot(agr, aes(x=Class,y=MeanLogProb)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax),width=.25)
ggsave("graphs/adjective-selection/meanlogprobs_adjectives.pdf",width=12)

ggplot(d, aes(x=Class)) +
  geom_histogram()
ggsave("graphs/adjective-selection/distribution.pdf",width=12)


smallclasses = as.data.frame(table(d$Class))
smallclasses = droplevels(smallclasses[smallclasses$Freq < 9,])
smallclasses

binned = droplevels(d[! d$Class %in% smallclasses$Var1,]) %>%
  group_by(Class) %>%
  mutate(FrequencyBin=cut_number(logProbability,4,labels=c("1","2","3","4")),LengthBin=cut_number(Length,2,labels=c("short","long")))

binned[binned$Class == "human",]
table(binned$Class,binned$FrequencyBin,binned$LengthBin) # there are two cases in "nationality" (3 short and 4 short) and one case (4 short) in "physical" that aren't covered: we'll just fill in these cases by randomly sampling from the leftovers in those two classes

binned$Combination = paste(binned$FrequencyBin,binned$LengthBin)
table(binned$Combination)


##################################################
# to get the actual final selection of adjectives:
##################################################

# get the base selection that includes random sampling
selection = binned %>%
  group_by(Class,Combination) %>%
  summarise(Adjective=sample(Adjective,1))
selection = as.data.frame(selection)
selection

# add the cases with up to 8 cases (ie where no sampling was necessary)
selection = merge(selection,d[d$Class %in% smallclasses$Var1,],all=T)

# add the "leftovers", given the lack of frequency-length bin coverage documented above
selected_physical = selection[selection$Class == "physical",]$Adjective
selected_nationality = selection[selection$Class == "nationality",]$Adjective
extra_physical = sample(setdiff(d[d$Class == "physical",]$Adjective,selected_physical),1)
#extra_physical
extra_nationality = sample(setdiff(d[d$Class == "nationality",]$Adjective,selected_nationality),2)
#extra_nationality
extra = data.frame(Adjective=c(extra_physical,extra_nationality),Class=c("physical","nationality","nationality"))
selection = merge(selection,extra,all=T)

selection
write.table(selection[,c("Adjective","Class")],file="data/sampled_adjectives.txt",sep="\t",col.names=T,row.names=F,quote=F)
