library(ggplot2)
library(lme4)
library(hydroGOF)
library(lmerTest)

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


#d$class1 <- as.character(d$class1)
#d$class2 <- as.character(d$class2)
#d$predicate1 <- as.character(d$predicate1)
#d$predicate2 <- as.character(d$predicate2)

#d$m_sense = 0
#d[d$makes_sense=="yes",]$m_sense = 1
#d$pred12 = paste(d$predicate1,d$predicate2)
#m1 = lm(m_sense~pred12,data=d)
#m2 = lm(m_sense~pred12+noun,data=d)
#anova(m1,m2)

#####
## duplicate observations by adjective configuration
#####

o <- d
o$configuration = paste(o$predicate1,o$predicate2)
o$class_configuration = paste(o$class1,o$class2)
#write.csv(o,"order-preference.csv")
o$rightconfiguration = paste(o$predicate2,o$predicate1)
o$right_class_configuration = paste(o$class2,o$class1)
#o$rightpredicate1 = o$predicate2
#o$rightpredicate2 = o$predicate1
o$rightresponse = 1-o$response
agr = o %>% 
  select(configuration,rightconfiguration,response,rightresponse,workerid,makes_sense,noun,nounclass,class_configuration,right_class_configuration,class1,class2,predicate1,predicate2,slide_number) %>%
  gather(predicateposition,correct_configuration,configuration:rightconfiguration,-workerid,-makes_sense,-noun,-nounclass,-class_configuration,-right_class_configuration,-class1,-class2,-predicate1,-predicate2,-slide_number)
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
########## END DATA PROCESSING ###############################
##############################################################

o <- agr
o = o[o$makes_sense=="yes",]

model.7 = lm(correctresponse~correctpred1, data=o)
model.11 = lm(correctresponse~correctpred1+noun:correctpred1, data=o)
model.10 = lm(correctresponse~noun+correctpred1, data=o)
anova(model.7,model.11)
anova(model.7,model.10)

#> anova(model.7,model.11)
#Analysis of Variance Table
#
#Model 1: correctresponse ~ correctpred1
#Model 2: correctresponse ~ correctpred1 + noun:correctpred1
#Res.Df    RSS   Df Sum of Sq      F Pr(>F)
#1  21269 2810.1                             
#2  11593 1516.7 9676    1293.4 1.0218 0.1343