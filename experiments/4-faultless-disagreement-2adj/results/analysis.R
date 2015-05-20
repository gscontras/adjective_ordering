library(ggplot2)
library(reshape2)
library(lme4)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/4-faultless-disagreement-2adj/Submiterator-master")

d = read.table("faultless-disagreement-trials.tsv",sep="\t",header=T)
head(d)
s = read.table("faultless-disagreement-subject_information.tsv",sep="\t",header=T)
head(s)

summary(d)

d$class1 = as.character(d$class1)
d$configuration = "NA"
d[d$adjnum==1,]$configuration = d[d$adjnum==1,]$class1
d[d$adjnum>1,]$configuration <- paste(d[d$adjnum>1,]$class1,d[d$adjnum>1,]$class2)
head(d)

aggregate(response~class1*adjnum,data=d,mean)

d$class1 <- factor(d$class1,levels=c("quality","size","age","texture","color","shape","material"))

d_s = bootsSummary(data=d, measurevar="response", groupvars=c("configuration","class1","class2","nounclass"))

agg2 = aggregate(response~configuration*class1*class2,data=d,mean)
head(agg2)
agg1 = aggregate(response~configuration*class1,data=d[d$adjnum==1,],mean)
agg1$class2 <- agg1$class1
head(agg1)
agg = rbind(agg1,agg2)
head(agg,20)
agg$configuration <- factor(agg$configuration,ordered=is.ordered(agg$configuration))

class_plot <- ggplot(agg, aes(x=reorder(configuration,-response,mean),y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(configuration,-response,mean), width=0.1),position=position_dodge(width=0.9)) +
  ylab("faultless disagreement\n") +
  xlab("configuration") +
  facet_wrap(~class1,scales="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
class_plot
ggsave("../results/class1.pdf",width=10,height=10)

quality = d[(d$class1=="quality"|d$class2=="quality"),]
quality = subset(quality,select=-c(class1,class2,predicate1,predicate2))
quality <- na.omit(quality)
quality_agg = bootsSummary(data=quality, measurevar="response", groupvars=c("configuration"))
quality_agg$configuration <- factor(quality_agg$configuration,ordered=is.ordered(quality_agg$configuration))
quality_agg$class = "quality"

size = d[(d$class1=="size"|d$class2=="size"),]
size = subset(size,select=-c(class1,class2,predicate1,predicate2))
size <- na.omit(size)
size_agg = bootsSummary(data=size, measurevar="response", groupvars=c("configuration"))
size_agg$configuration <- factor(size_agg$configuration,ordered=is.ordered(size_agg$configuration))
size_agg$class = "size"

age = d[(d$class1=="age"|d$class2=="age"),]
age = subset(age,select=-c(class1,class2,predicate1,predicate2))
age <- na.omit(age)
age_agg = bootsSummary(data=age, measurevar="response", groupvars=c("configuration"))
age_agg$configuration <- factor(age_agg$configuration,ordered=is.ordered(age_agg$configuration))
age_agg$class = "age"

texture = d[(d$class1=="texture"|d$class2=="texture"),]
texture = subset(texture,select=-c(class1,class2,predicate1,predicate2))
texture <- na.omit(texture)
texture_agg = bootsSummary(data=texture, measurevar="response", groupvars=c("configuration"))
texture_agg$configuration <- factor(texture_agg$configuration,ordered=is.ordered(texture_agg$configuration))
texture_agg$class = "texture"

color = d[(d$class1=="color"|d$class2=="color"),]
color = subset(color,select=-c(class1,class2,predicate1,predicate2))
color <- na.omit(color)
color_agg = bootsSummary(data=color, measurevar="response", groupvars=c("configuration"))
color_agg$configuration <- factor(color_agg$configuration,ordered=is.ordered(color_agg$configuration))
color_agg$class = "color"

shape = d[(d$class1=="shape"|d$class2=="shape"),]
shape = subset(shape,select=-c(class1,class2,predicate1,predicate2))
shape <- na.omit(shape)
shape_agg = bootsSummary(data=shape, measurevar="response", groupvars=c("configuration"))
shape_agg$configuration <- factor(shape_agg$configuration,ordered=is.ordered(shape_agg$configuration))
shape_agg$class = "shape"

material = d[(d$class1=="material"|d$class2=="material"),]
material = subset(material,select=-c(class1,class2,predicate1,predicate2))
material <- na.omit(material)
material_agg = bootsSummary(data=material, measurevar="response", groupvars=c("configuration"))
material_agg$configuration <- factor(material_agg$configuration,ordered=is.ordered(material_agg$configuration))
material_agg$class = "material"

all_agg = rbind(quality_agg,size_agg,age_agg,texture_agg,color_agg,shape_agg,material_agg)

all_agg$class <- factor(all_agg$class,levels=c("quality","size","texture","age","color","shape","material"))

all_plot <- ggplot(all_agg, aes(x=reorder(configuration,-response,mean),y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(configuration,-response,mean), width=0.1),position=position_dodge(width=0.9)) +
  ylab("faultless disagreement\n") +
  xlab("configuration") +
  facet_wrap(~class,scales="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
all_plot
ggsave("../results/all_plot.pdf",width=10,height=10)
