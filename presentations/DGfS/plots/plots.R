library(ggplot2)
library(reshape2)
library(lme4)

# order preference data
o = read.csv("~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/preference.csv",header=T)
o$expt = "preference"
# faultless data 
f = read.csv("~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/faultless.csv",header=T)
f$expt = "subjectivity"

ggplot(data=all_agg_s,aes(x=reorder(class1,-adj_preferred_10,mean),y=adj_preferred_10))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class1,-adj_preferred_10,mean), width=0.1),alpha=0.5)+
  xlab("\nadjective class")+
  ylab("distance from noun\n")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()