

## load in order preferences

d = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/3-order-preference/Submiterator-master/order-preference-trials.tsv",sep="\t",header=T)
head(d)

## load in faultless

f = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/2-faultless-disagreement/Submiterator-master/faultless-disagreement-2-trials.tsv",sep="\t",header=T)
head(f)
f_agr = aggregate(response~class,data=f,mean)
d$class1_f = f_agr$response[match(d$class1,f_agr$class)]
d$class2_f = f_agr$response[match(d$class2,f_agr$class)]
d$f_diff = (d$class1_f-d$class2_f)

## load in subjectivity

s = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/6-subjectivity/Submiterator-master/subjectivity-trials.tsv",sep="\t",header=T)
head(s)
s_agr = aggregate(response~class,data=s,mean)
d$class1_s = s_agr$response[match(d$class1,s_agr$class)]
d$class2_s = s_agr$response[match(d$class2,s_agr$class)]
d$s_diff = (d$class1_s-d$class2_s)

m = lmer(response~f_diff+s_diff+slide_number+(1|workerid),data=d)
summary(m)

## plot 

d_s = aggregate(response~f_diff*s_diff,data=d,mean)

ggplot(d_s, aes(x=s_diff,y=response,color=f_diff)) +
  geom_point() +
  #geom_text(aes(label=configuration))+
  ylab("acceptability") +
  xlab("subjectivity") +
  ggtitle("by-class plot")

ggplot(d_s, aes(x=f_diff,y=response,color=s_diff)) +
  geom_point() +
  #geom_text(aes(label=configuration))+
  ylab("acceptability") +
  xlab("subjectivity") +
  ggtitle("by-class plot")