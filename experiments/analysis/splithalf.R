# computes Spearman–Brown prophecy formula
prophet <- function(reliability, length) {
  prophecy <- length * reliability / (1 + (length - 1)*reliability)
  return (prophecy)
}

# takes in data frame and number of samples you want to generate N and
# returns average split-half correlation

splithalf_pred <- function(data, N) {
  #data <- o
  #data$workerID = as.factor(as.character(data$workerID))
  #data$workerid=NULL
  #o$workerID = factor(o$workerid,labels=seq(1,45))
  cors <- numeric(0)
  t <- 1
  while (t <= N) {
    # assumes your data frame has a column called workerID, and they go from 1 to nWorkers
    nWorkers <- length(unique(data$workerID))
    indices <- seq_len(nWorkers)
    workers1 <- sample(indices, nWorkers / 2)
    workers2 <- indices[!indices %in% workers1]
    subset1 <- subset(data, workerID %in% workers1)
    subset2 <- subset(data, workerID %in% workers2)
    ##########
    # hard-coded, should modify
    ##########
    subset1.mean <- aggregate(data=subset1, response ~ predicate , FUN=mean)
    subset2.mean <- aggregate(data=subset2, response ~ predicate , FUN=mean)
    subset1.mean <- rename(subset1.mean, replace=c("response" = "mean1"))
    subset2.mean <- rename(subset2.mean, replace=c("response" = "mean2"))
    subset.comp <- join(subset1.mean, subset2.mean, by=c("predicate"))
    subset.comp = na.omit(subset.comp)
    r <- with(subset.comp, cor(mean1, mean2))
    if (!is.na(r)) {
      t <- t+1
      cors <- c(cors, r)
    }
  }
  return(mean(cors))
}

splithalf_class <- function(data, N) {
  #data <- o
  #data$workerID = as.factor(as.character(data$workerID))
  #data$workerid=NULL
  #o$workerID = factor(o$workerid,labels=seq(1,45))
  cors <- numeric(0)
  t <- 1
  while (t <= N) {
    # assumes your data frame has a column called workerID, and they go from 1 to nWorkers
    nWorkers <- length(unique(data$workerID))
    indices <- seq_len(nWorkers)
    workers1 <- sample(indices, nWorkers / 2)
    workers2 <- indices[!indices %in% workers1]
    subset1 <- subset(data, workerID %in% workers1)
    subset2 <- subset(data, workerID %in% workers2)
    ##########
    # hard-coded, should modify
    ##########
    subset1.mean <- aggregate(data=subset1, response ~ class , FUN=mean)
    subset2.mean <- aggregate(data=subset2, response ~ class , FUN=mean)
    subset1.mean <- rename(subset1.mean, replace=c("response" = "mean1"))
    subset2.mean <- rename(subset2.mean, replace=c("response" = "mean2"))
    subset.comp <- join(subset1.mean, subset2.mean, by=c("class"))
    subset.comp = na.omit(subset.comp)
    r <- with(subset.comp, cor(mean1, mean2))
    if (!is.na(r)) {
      t <- t+1
      cors <- c(cors, r)
    }
  }
  return(mean(cors))
}


#prophet(splithalf(o, 100), 2)
