theme_set(theme_bw(18))
#for judith
setwd("~/cogsci/projects/stanford/projects/adjective_ordering/corpus_results/")
#for greg
#setwd("~/Documents/git/CoCoLab/adjective_ordering/corpus_results/")
source("rscripts/helpers.r")

greg = read.csv("data/swbd_adjectives.gs.csv", quote="")
judith = read.csv("data/swbd_adjectives.jd.csv", quote="")
row.names(judith) = judith$x
head(judith)

d = greg
d$class_judith = judith[as.character(d$x),]$class
d$class_greg = factor(x=as.character(d$class),levels=c(levels(d$class_judith),"X"))
levels(d$class_judith) = c(levels(d$class_judith),"X")
d[d$class_judith == "",]$class_judith = "X"
d[d$class_greg == "",]$class_greg = "X"
d$class = NULL
#d$x_judith = judith[as.character(d$x),]$x
head(d)
d$Agree = ifelse(d$class_greg == d$class_judith, "agree", "disagree")
d$Combination = as.factor(paste(d$class_greg,d$class_judith))

prop.table(table(d$Agree)) # we agree on 71% overall
table(d$ignore) # ignore 71 cases
prop.table(table(d[d$ignore != "X",]$Agree)) # we agree on 66% of non-ignored cases
table(d[d$ignore != "X" & d$class_judith != "X" & d$class_greg != "X",]$Agree)
prop.table(table(d[d$ignore != "X" & d$class_judith != "X" & d$class_greg != "X",]$Agree)) # we agree on 87% of non-ignored and non-blank cases (total 166 cases, agree on 144)
noignored = droplevels(subset(d[d$ignore != "X",]))
noignored[noignored$Agree == "disagree",]
noignorednoblank = droplevels(d[d$ignore != "X" & d$class_judith != "X" & d$class_greg != "X",])
noignorednoblank[noignorednoblank$Agree == "disagree",]
nrow(noignorednoblank)

# choose one or the other (throw out either only the ignored cases or both the ignored and the blank cases)
comb = as.data.frame(table(noignored$Agree, noignored$Combination))
comb = as.data.frame(table(noignorednoblank$Agree, noignorednoblank$Combination))
colnames(comb) = c("Agree","Combination_g_j","Freq")
head(comb)
nrow(comb)
comb[comb$Var1 == 1 & comb$Freq > 0,]
comb[comb$Var1 == 0 & comb$Freq > 0,]
comb = droplevels(subset(comb, Freq > 0))
comb = comb[order(comb[,c("Freq")],decreasing=T),]
comb$AdjClassCombination_g_j = factor(x=as.character(comb$Combination),levels=as.character(comb$Combination))

ggplot(comb, aes(x=AdjClassCombination_g_j,y=Freq)) +
  geom_bar(stat="identity") +
  facet_wrap(~Agree,scales="free") +
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1))
# choose one or the other depending on whether plot was created without ignored or without ignored&blank cases
ggsave("graphs/agreement_noignored.pdf")
ggsave("graphs/agreement_noignored_noblank.pdf")

write.csv(d[d$ignore != "X" & d$class_judith != "X" & d$class_greg != "X" & d$Agree == "agree",c("x","class_greg")],file="data/agreedupon_adjectives.csv",row.names=F,quote=F)
