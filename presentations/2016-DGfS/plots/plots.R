library(ggplot2)
library(reshape2)
library(lme4)

# order preference data
o = read.csv("~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/preference.csv",header=T)
o$expt = "preference"
head(o)
o <- subset(o,select=c("class1","adj_preferred_10","bootsci_high","bootsci_low","expt"))
colnames(o) <- c("class","average","YMin","YMax","expt")
# faultless data 
f = read.csv("~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/faultless.csv",header=T)
f$expt = "subjectivity"
head(f)
f <- subset(f,select=c("class","response","bootsci_high","bootsci_low","expt"))
colnames(f) <- c("class","average","YMin","YMax","expt")
# corpus
c = read.csv("~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/corpus.csv",header=T)
c$expt = "corpus"
head(c)
colnames(c) <- c("class","average","YMin","YMax","expt")
c$average = c$average - 1
c$YMin = c$YMin - 1
c$YMax = c$YMax - 1
d = rbind(o,f,c)
head(d)

ggplot(data=d,aes(x=reorder(class,-average,mean),y=average,fill=expt))+
  geom_bar(stat="identity",position=position_dodge(.9))+
  geom_errorbar(aes(ymin=YMin, ymax=YMax, x=reorder(class,-average,mean), width=0.1),position=position_dodge(.9))+
  xlab("\nadjective class")+
  labs(fill="experiment") +
  ylab("")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()+
  theme(legend.position=c(.85, .66))
  #theme(legend.position="bottom")


ggsave("~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/expt_results.pdf",height=2.4,width=4.75)

