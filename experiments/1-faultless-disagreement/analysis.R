library(ggplot2)
library(reshape2)
library(lme4)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/1-faultless-disagreement/Submiterator-master")

d = read.table("faultless-disagreement-trials.tsv",sep="\t",header=T)
head(d)

summary(d)
