library(ggplot2)
library(plyr)
theme_set(theme_bw(18))
#setwd("~/cogsci/projects/stanford/projects/adjective_ordering/corpus_results/")
#for greg
setwd("~/Documents/git/CoCoLab/adjective_ordering/corpus_results/")
source("rscripts/helpers.r")

#load("data/r.RData")
rs = read.table("data/swbd.tab", sep="\t", header=T, quote="")
rs$Corpus = "swbd"
nrow(rs) # 15'744 cases in swbd

length(unique(rs[rs$PrevAdj=="yes",]$Adjective)) # 261 unique Adjective
length(unique(rs[rs$PrevAdj=="yes",]$PrevAdjective)) # 180 unique PrevAdjective
length(unique(rs[rs$PrevAdj=="yes",]$Noun)) # 356 unique Noun

d <- rs[rs$PrevAdj=="yes"&rs$PrevPrevAdj!="yes",]
head(d)
nrow(d) # 544 two adjective cases
## trim non-adjectives
# comparatives: further, other, smaller, bigger, upper, larger, higher, older
# non-adjectives: many, various, only, few, several, whole, most
d <- d[d$PrevAdjective!="many"&d$PrevAdjective!="various"&d$PrevAdjective!="only"&d$PrevAdjective!="few"&d$PrevAdjective!="several"&d$PrevAdjective!="whole"&d$PrevAdjective!="most"&d$PrevAdjective!="last"&d$PrevAdjective!="next"&d$PrevAdjective!="past"&d$PrevAdjective!="first"&d$PrevAdjective!="particular",] 
nrow(d) # 473 two adjective cases
d <- d[d$Adjective!="further"&d$Adjective!="other"&d$Adjective!="smaller"&d$Adjective!="bigger"&d$Adjective!="upper"&d$Adjective!="larger"&d$Adjective!="higher"&d$Adjective!="older"&d$Adjective!="few",]
nrow(d) # 469 two adjective cases
length(unique(d$Adjective)) # 241 unique Adjective
length(unique(d$PrevAdjective)) # 165 unique PrevAdjective
a = unique(d$Adjective)
pa = unique(d$PrevAdjective)
all_as = factor(c(as.character(a),as.character(pa)))
length(unique(all_as)) # 350 unique adjectives
length(unique(d$Noun)) # 295 unique nouns
# $16 for 40 people on 26 adjectives with 10 nouns
# $3181.29 for 360 adjectives
# $243.69 for 100 adjectives

as = unique(all_as)
#write.csv(as,"data/swbd_adjectives.csv")
