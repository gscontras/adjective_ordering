library(ggplot2)
library(lme4)
library(hydroGOF)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/12-order-preference-expanded/Submiterator-master")

library(tidyr)
library(dplyr)

#d = read.csv("round4/order-preference-expanded.csv")

#d[sapply(d, is.factor)] <- lapply(d[sapply(d, is.factor)], as.character)
#df[sapply(df, is.factor)] <- lapply(df[sapply(df, is.factor)], as.character)
#e = rbind(df,d)

num_round_dirs = 55
df = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/order-preference-expanded-trials.csv', sep='')) %>%
      mutate(workerid = (workerid + (i-1)*9)))}))
#unique(df$comments)
num_round_dirs = 55
sf = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/order-preference-expanded-subject_information.csv', sep='')) %>%
      mutate(workerid = (workerid + (i-1)*9)))}))
head(sf)
unique(sf$language)
sf[sf$language=="Spanish",]
sf[sf$language=="Indonesian",]
sf[sf$language=="Russian",]
sf[sf$language=="English, Spanish",]
sf[sf$language=="english, spanish",]
sf[sf$language=="English, Chinese",]
sf[sf$language=="Tagalog/Cebuano",]
sf[sf$language=="cantonese",]
sf[sf$language=="",]
sf[sf$language=="English and Spanish",]
sf[sf$language=="korean",]
sf[sf$language=="Tamil",]
sf[sf$language=="vietnamese",]
sf[sf$language=="Filipino",]
sf[sf$language=="Mandarin",]
sf[sf$language=="Indonesian",]
sf[sf$language=="English, Malayalam",]
sf[sf$language=="chinese",]
sf[sf$language=="Chinese",]

# remove non-English
df = df[df$workerid!=40&df$workerid!=75&df$workerid!=209&df$workerid!=103&df$workerid!=169&df$workerid!=333&df$workerid!=385&df$workerid!=415&df$workerid!=116&df$workerid!=194&df$workerid!=365&df$workerid!=355&df$workerid!=409&df$workerid!=419&df$workerid!=34&df$workerid!=434&df$workerid!=124&df$workerid!=179&df$workerid!=166&df$workerid!=209&df$workerid!=307&df$workerid!=411&df$workerid!=480,]

length(unique(df$workerid)) #473 English participants
str(df)
#d = subset(df, select=c("workerid", "noun","nounclass","slide_number","sense","predicate2","predicate1","class2","response","class1","language"))
d = subset(df, select=c("workerid", "noun","nounclass","slide_number","sense","predicate2","predicate1","class2","response","class1"))

summary(d)
d$makes_sense = "yes"
d[!is.na(d$sense),]$makes_sense = "no"

levels(d$class1) <- c("age","color", "dimension","human", "location", "material","nationality","physical","shape","speed","temporal","value","X")
levels(d$predicate1) <- c("biggest",
"large",
"long",
"mini",
"narrow",
"open",
"thick",
"thin",
"civilized",
"creative",
"entrepreneurial",
"playful",
"professional",
"sad",
"selfish",
"strict",
"brazilian",
"english",
"european",
"hispanic",
"international",
"japanese",
"national",
"vietnamese",
"creamy",
"curly",
"frozen",
"lacy",
"smooth",
"solid",
"spicy",
"sweet",
"best",
"exciting",
"favorite",
"lavish",
"plain",
"pleasant",
"prestigious",
"strange",
"designated",
"different",
"individual",
"last",
"mixed",
"potential",
"token",
"unique",
"junior",
"new",
"old",
"old-time",
"senior",
"young",
"black",
"blonde",
"blue",
"green",
"purple",
"red",
"white",
"yellow",
"closest",
"internal",
"overhead",
"corduroy",
"crocheted",
"gold",
"wooden",
"circular",
"square",
"fast",
"slow",
"speedy",
"current",
"daily",
"everyday",
"historical")
levels(d$predicate2) <- c("biggest",
                          "large",
                          "long",
                          "mini",
                          "narrow",
                          "open",
                          "thick",
                          "thin",
                          "civilized",
                          "creative",
                          "entrepreneurial",
                          "playful",
                          "professional",
                          "sad",
                          "selfish",
                          "strict",
                          "brazilian",
                          "english",
                          "european",
                          "hispanic",
                          "international",
                          "japanese",
                          "national",
                          "vietnamese",
                          "creamy",
                          "curly",
                          "frozen",
                          "lacy",
                          "smooth",
                          "solid",
                          "spicy",
                          "sweet",
                          "best",
                          "exciting",
                          "favorite",
                          "lavish",
                          "plain",
                          "pleasant",
                          "prestigious",
                          "strange",
                          "designated",
                          "different",
                          "individual",
                          "last",
                          "mixed",
                          "potential",
                          "token",
                          "unique",
                          "junior",
                          "new",
                          "old",
                          "old-time",
                          "senior",
                          "young",
                          "black",
                          "blonde",
                          "blue",
                          "green",
                          "purple",
                          "red",
                          "white",
                          "yellow",
                          "closest",
                          "internal",
                          "overhead",
                          "corduroy",
                          "crocheted",
                          "gold",
                          "wooden",
                          "circular",
                          "square",
                          "fast",
                          "slow",
                          "speedy",
                          "current",
                          "daily",
                          "everyday",
                          "historical")
levels(d$class2) <- c("color", "dimension","human", "location", "material","nationality","physical","shape","speed","temporal","value","X","age")

#####
## duplicate observations by adjective configuration
#####

o <- d
o$configuration = paste(o$predicate1,o$predicate2)
o$class_configuration = paste(o$class1,o$class2)
o$rightconfiguration = paste(o$predicate2,o$predicate1)
o$right_class_configuration = paste(o$class2,o$class1)
#o$rightpredicate1 = o$predicate2
#o$rightpredicate2 = o$predicate1
o$rightresponse = 1-o$response
agr = o %>% 
  select(configuration,rightconfiguration,response,rightresponse,workerid,makes_sense,noun,nounclass,class_configuration,right_class_configuration,class1,class2,predicate1,predicate2) %>%
  gather(predicateposition,correct_configuration,configuration:rightconfiguration,-workerid,-makes_sense,-noun,-nounclass,-class_configuration,-right_class_configuration,-class1,-class2,-predicate1,-predicate2)
agr$correctresponse = agr$response
agr[agr$predicateposition == "rightconfiguration",]$correctresponse = agr[agr$predicateposition == "rightconfiguration",]$rightresponse
agr$correctclass = agr$class_configuration
agr[agr$predicateposition == "rightconfiguration",]$correctclass = agr[agr$predicateposition == "rightconfiguration",]$right_class_configuration
agr$correctclass1 = agr$class1
agr[agr$predicateposition == "rightconfiguration",]$correctclass1 = agr[agr$predicateposition == "rightconfiguration",]$class2
agr$correctclass2 = agr$class2
agr[agr$predicateposition == "rightconfiguration",]$correctclass2 = agr[agr$predicateposition == "rightconfiguration",]$class1
agr$correctpred1 = agr$predicate1
agr[agr$predicateposition == "rightconfiguration",]$correctpred1 = agr[agr$predicateposition == "rightconfiguration",]$predicate2
agr$correctpred2 = agr$predicate2
agr[agr$predicateposition == "rightconfiguration",]$correctpred2 = agr[agr$predicateposition == "rightconfiguration",]$predicate1
#head(agr[agr$predicateposition == "rightconfiguration",])
agr$response = NULL
agr$rightresponse = NULL
agr$predicate1 = NULL
agr$predicate2 = NULL
agr$class1 = NULL
agr$class2 = NULL
agr$class_configuration = NULL
agr$right_class_configuration = NULL
nrow(agr) #28380
head(agr)
##write.csv(agr,"order-preference-duplicated.csv")

##############################################################
########## END SCRIPT ########################################
##############################################################




all_agg_s = aggregate(correctresponse~correctclass1*makes_sense,data=agr,mean)
#save for aggregate plot
#write.csv(all_agg_s,"~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/preference.csv")


ggplot(data=all_agg_s,aes(x=reorder(correctclass1,-correctresponse,mean),y=correctresponse,fill=makes_sense))+
  geom_bar(stat="identity",position=position_dodge())+
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class1,-adj_preferred_10,mean), width=0.1),alpha=0.5)+
  xlab("\nadjective class")+
  ylab("rating\n")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()#+
#theme(axis.text.x=element_text(angle=90,vjust=0.35,hjust=1))
#ggsave("../results/preferred_class_distance.pdf",height=3,width=11)












#####
## duplicate observations by first predicate
#####

#d = read.csv("order-preference-expanded.csv",header=T)

o <- d
#o <- d[d$sense!="no",]
o$rightpredicate1 = o$predicate2
o$rightpredicate2 = o$predicate1
o$rightresponse = 1-o$response
agr = o %>% 
  select(predicate1,rightpredicate1,response,rightresponse,workerid,noun,nounclass,class1,class2) %>%
  gather(predicateposition,predicate,predicate1:rightpredicate1,-workerid,-noun,-nounclass,-class1,-class2)
agr$correctresponse = agr$response
agr[agr$predicateposition == "rightpredicate1",]$correctresponse = agr[agr$predicateposition == "rightpredicate1",]$rightresponse
agr$correctclass = agr$class1
agr[agr$predicateposition == "rightpredicate1",]$correctclass = agr[agr$predicateposition == "rightpredicate1",]$class2
head(agr[agr$predicateposition == "rightpredicate1",])
agr$response = NULL
agr$rightresponse = NULL
agr$class1 = NULL
agr$class2 = NULL
nrow(agr) #2340
#write.csv(agr,"~/Documents/git/cocolab/adjective_ordering/experiments/analysis/naturalness-duplicated.csv")

## compute class configuration ratio

o$configuration <- paste(o$class1,o$class2)
head(o)
o_agg <- aggregate(response~configuration*class1*class2,data=o,mean)
head(o_agg,20)

o_agg$Ratio = -555
row.names(o_agg) = o_agg$configuration

for (first in levels(o_agg$class1)) {
  for (second in levels(o_agg$class1)[levels(o_agg$class1) != first]) {
    o_agg[paste(first,second),]$Ratio = o_agg[paste(first,second),]$response / o_agg[paste(second, first),]$response
    print(paste(first,second))
  }
}

## compute adjective configuration ratio

o$adj_configuration <- paste(o$predicate1,o$predicate2)
head(o)
o_adj_agg <- aggregate(response~adj_configuration*predicate1*predicate2,data=o,mean)
head(o_adj_agg,20)

o_adj_agg$adj_Ratio = -555
row.names(o_adj_agg) = o_adj_agg$adj_configuration

for (first in levels(o_adj_agg$predicate1)) {
  for (second in levels(o_adj_agg$predicate1)[levels(o_adj_agg$predicate1) != first]) {
    o_adj_agg[paste(first,second),]$adj_Ratio = o_adj_agg[paste(first,second),]$response / o_adj_agg[paste(second, first),]$response
    print(paste(first,second))
  }
}

# add in order preference

d$configuration = paste(d$class1,d$class2)
d$adj_configuration = paste(d$predicate1,d$predicate2)
all_agg <- aggregate(response~configuration*adj_configuration*class1*class2*predicate1*predicate2,data=d,mean)

all_agg$Ratio = o_agg[as.character(all_agg$configuration),]$Ratio
all_agg[is.na(all_agg$Ratio),]$Ratio = -555
all_agg$Preferred = as.factor(ifelse(all_agg$Ratio > 1, "preferred", ifelse(all_agg$Ratio == -555, "single","dispreferred")))

all_agg$adj_Ratio = o_adj_agg[as.character(all_agg$adj_configuration),]$adj_Ratio
all_agg[is.na(all_agg$adj_Ratio),]$adj_Ratio = -555
all_agg$adj_Preferred = as.factor(ifelse(all_agg$adj_Ratio > 1, "preferred", ifelse(all_agg$adj_Ratio == -555, "single","dispreferred")))

nrow(all_agg)
nrow(all_agg[all_agg$Preferred=="preferred",])

all_agg$adj_preferred_10 = 0
all_agg[all_agg$adj_Preferred=="preferred",]$adj_preferred_10 = 1

# get inferred mean distance from noun by adjective for PNAS figure
agg_adj = aggregate(adj_preferred_10~predicate1,data=all_agg,mean)
#write.csv(agg_adj,"~/Documents/git/cocolab/adjective_ordering/experiments/analysis/inferred_distance_by_adj.csv")

pairwise.t.test(all_agg$adj_preferred_10, all_agg$class1, p.adj = "bonf")

all_agg_s = bootsSummary(data=all_agg, measurevar="adj_preferred_10", groupvars=c("class1"))
#save for aggregate plot
#write.csv(all_agg_s,"~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/preference.csv")


ggplot(data=all_agg_s,aes(x=reorder(class1,-adj_preferred_10,mean),y=adj_preferred_10))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class1,-adj_preferred_10,mean), width=0.1),alpha=0.5)+
  xlab("\nadjective class")+
  ylab("distance from noun\n")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()#+
  #theme(axis.text.x=element_text(angle=90,vjust=0.35,hjust=1))
#ggsave("../results/class_distance_by_adj.pdf",height=3)


ggplot(data=all_agg[all_agg$Preferred=="preferred",],aes(x=reorder(configuration,-Ratio,mean),y=Ratio))+
  geom_bar(stat="identity")+
  xlab("\nadjective class configuration")+
  ylab("acceptability ratio\n")+
  #labs("order\npreference")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,vjust=0.35,hjust=1))
#ggsave("../results/order_ratio.pdf",height=4)

## average distance from noun

dist = read.csv("../results/distance.csv",header=T)
dist$distance = (dist$first / 6)+1
head(dist)

ggplot(data=dist,aes(x=reorder(class,-distance,mean),y=distance))+
  geom_bar(stat="identity")+
  xlab("\nadjective class")+
  ylab("distance from noun\n")+
  #labs("order\npreference")+
  theme_bw()
#ggsave("../results/class_distance.pdf",height=4)


ggplot(data=all_agg,aes(x=reorder(configuration,-response,mean),y=response,fill=Preferred))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))





## get faultles disagreement ratings

f = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/2-faultless-disagreement/Submiterator-master/faultless-disagreement-2-trials.tsv",sep="\t",header=T)
head(f)
fs = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/2-faultless-disagreement/Submiterator-master/faultless-disagreement-2-subject_information.tsv",sep="\t",header=T)
head(fs) #
f$language = fs$language[match(f$workerid,fs$workerid)]
# 50 native English participants
f_agr = aggregate(response~class,data=f,mean)
#p_agr = aggregate(response~predicate,data=f,mean)

d$Ratio = o_agg$Ratio[match(d$configuration,o_agg$configuration)]
#all_agg[is.na(all_agg$Ratio),]$Ratio = -555
d$Preferred = as.factor(ifelse(d$Ratio > 1, "preferred", ifelse(d$Ratio == -555, "single","dispreferred")))

d$class1_f = f_agr$response[match(d$class1,f_agr$class)]
d$class2_f = f_agr$response[match(d$class2,f_agr$class)]

#d$pred1_f = p_agr$response[match(d$predicate1,p_agr$predicate)]
#d$pred2_f = p_agr$response[match(d$predicate2,p_agr$predicate)]

#d$f_ratio = (d$class1_f/d$class2_f)
d$f_diff = (d$class1_f-d$class2_f)
#d$p_ratio = (d$pred1_f/d$pred2_f)
#d$p_diff = (d$pred1_f-d$pred2_f)
#d$sentence = paste(d$predicate1,d$predicate2,d$noun)

## by class plot

d_s = bootsSummary(data=d, measurevar="response", groupvars=c("f_diff","Preferred","configuration"))

#d_s = aggregate(response~f_diff,data=d,mean)
#d_s = aggregate(response~f_diff*configuration,data=d,mean)

d_s$smooth_thing = "t"

ggplot(d_s, aes(x=f_diff,y=response,color=Preferred)) +
  geom_point() +
  geom_smooth(data=d_s, aes(f_diff,response,color=smooth_thing),method=lm)+
  #geom_smooth(data=d_s, aes(f_diff,response,color=smooth_thing))+
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=f_diff, width=0.1),alpha=0.5)+
  #geom_text(aes(label=configuration),color="black")+
  ylab("configuration acceptability\n") +
  xlab("\nfaultless disagreement difference") +
  ylim(0,1)+
  scale_x_continuous(breaks=c(-0.5,-0.25,0,0.25,0.5))+
  labs(color="order\npreference")+
  scale_colour_manual(values=c("blue","red","blue"),limits = c("preferred", "dispreferred"))+
  #ggtitle("by-class plot")
  theme_bw()
#ggsave("../results/faultless_order_preference.pdf",width=5.5,height=3.5)
ggsave("~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/comparison1.pdf")

ggplot(d_s, aes(x=f_diff,y=response)) +
  geom_point() +
  geom_smooth(data=d_s, aes(f_diff,response,color=smooth_thing),method=lm)+
  #geom_smooth(data=d_s, aes(f_diff,response,color=smooth_thing))+
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=f_diff, width=0.1),alpha=0.5)+
  #geom_text(aes(label=configuration),color="black")+
  ylab("configuration acceptability\n") +
  xlab("\nsubjectivity difference") +
  ylim(0,1)+
  scale_x_continuous(breaks=c(-0.5,-0.25,0,0.25,0.5))+
  #labs(color="order\npreference")+
  scale_colour_manual(values=c("blue","red","blue"),limits = c("preferred", "dispreferred"))+
  guides(color=FALSE)+
  #ggtitle("by-class plot")
  theme_bw()

ggsave("~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/comparison2.pdf",width=4,height=3)

gof(d_s$f_diff,d_s$response) #r2=.81

## only color-shape has diverging predictions



s = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/6-subjectivity/Submiterator-master/subjectivity-trials.tsv",sep="\t",header=T)
head(s)
s_agr = aggregate(response~class,data=s,mean)
p_agr = aggregate(response~predicate,data=s,mean)

d$class1_s = s_agr$response[match(d$class1,s_agr$class)]
d$class2_s = s_agr$response[match(d$class2,s_agr$class)]

d$pred1_s = p_agr$response[match(d$predicate1,p_agr$predicate)]
d$pred2_s = p_agr$response[match(d$predicate2,p_agr$predicate)]

d$s_ratio = (d$class1_s/d$class2_s)
d$s_diff = (d$class1_s-d$class2_s)
d$p_ratio = (d$pred1_s/d$pred2_s)
d$p_diff = (d$pred1_s-d$pred2_s)
d$sentence = paste(d$predicate1,d$predicate2,d$noun)

## by class plot

#d_s = bootsSummary(data=d, measurevar="response", groupvars=c("s_diff"))

d_s = aggregate(response~s_diff,data=d,mean)
d_s = aggregate(response~f_diff*configuration,data=d,mean)

ggplot(d_s, aes(x=s_diff,y=response)) +
  geom_point() +
  #geom_text(aes(label=configuration))+
  ylab("acceptability") +
  xlab("subjectivity") +
  ggtitle("by-class plot")
ggsave("../results/class_plot_subjectivity.pdf")


## correlations

head(d)
cor(d$response,d$f_diff) # 0.69
cor(d$response,d$s_diff) # 0.68



#ggplot(d, aes(x=f_diff,y=response)) +
 #        geom_point() +
  #geom_smooth()


## by predicate plot

#ggplot(d, aes(x=p_diff,y=response)) +
#  geom_point() +
#  geom_smooth()

p_s = bootsSummary(data=d, measurevar="response", groupvars=c("p_diff"))

p_s = aggregate(response~p_diff*sentence,data=d,mean)

ggplot(p_s, aes(x=p_diff,y=response)) +
  geom_point(alpha=0.25) +
  ylab("acceptability") +
  xlab("faultless disagreement") +
  geom_text(size=2,alpha=0.75,aes(label=sentence),angle=45)+
  ggtitle("by-predicate plot")
ggsave("../results/pred_plot.pdf",width=12,height=10)
