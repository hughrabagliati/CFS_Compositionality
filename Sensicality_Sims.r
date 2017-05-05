library(gridExtra)

library(plyr)
library(lme4)
library(doBy)
library(ggplot2)
library(skewt)
library(fitdistrplus)
library(gamlss)
library(gamlss.dist)
library(lme4)
library(ez)
library(jsonlite)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(doBy)
library(sn)
read_data <- function(path_name){
  list.files(path = path_name,full.names = T, pattern = ".csv") -> file_list
  comp = c()
  for (x in file_list){
    data <- read.csv(x,header = T)
    if ("perceptual.rating.reactiontime" %in% colnames(data)){ 
      data <- subset(data, select = -perceptual.rating.reactiontime)
    }
    if ("X" %in% colnames(data)){ 
      data <- subset(data, select = -X)
      data$rt <- as.character(data$rt)
    }
    comp <- rbind(comp, data)
  }
  return(comp)
}

sense.pop <- read_data("./Expt1_Sensicality Study/data/")

# Make RTs numeric [need to remove timeout "none" responses to 8s]
sense.pop <- subset(sense.pop, rt != "None")
sense.pop$rt <- as.numeric(sense.pop$rt)
sense.pop$Length <- nchar(as.character(sense.pop$prime),allowNA = T)
sense.pop$Condition <- as.character(sense.pop$prime_semantics)
sense.pop[sense.pop$prime_semantics %in% c("Sklar_control_A","Sklar_control_B"),]$Condition <- "Sklar_control"
sense.pop$Condition <- as.factor(sense.pop$Condition)


# Funciton for shuffling sim'd data
shuffle_cond_sim <- function(data,shuffle = TRUE, exclude = FALSE){
  if (shuffle != FALSE){
    data <- data %>%
      group_by(subj) %>%
      mutate(cond = sample(cond))
    data <- data.frame(data)
  }
  if (exclude != FALSE){
    
    # [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
    data <- ddply(data, .(cond), function(d){ 
      by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
      d = subset(d, rt < by_subj_include[2])
    })
  }
   data.summary <- summaryBy(rt ~ subj + cond, data = data, keep.names = T)
  
    return(t.test(data.summary$rt ~ data.summary$cond)$p.value)
  }


#### Simulate what happens with normal and skewed data
# Normal data without exclusions
pvals.norm <- rep(NA,5000)
pvals.norm.excl <- rep(5000)

a = data.frame(subj = rep(1:30, each = 60), cond = rep(rep(c(1,2), each = 30),30))
a <- a %>% group_by(subj) %>%
  dplyr::mutate(rt = ifelse(cond >1, rnorm(30,jitter(0.333),jitter(1)),rnorm(30,jitter(0),jitter(1))))
a <- data.frame(a)
for (i in 1:5000){

  pvals.norm[i] <- shuffle_cond_sim(a, shuffle = TRUE)
  pvals.norm.excl[i] <- shuffle_cond_sim(a, shuffle = TRUE,exclude = TRUE)
}




pvals.skew <- rep(NA,1)
pvals.skew.excl <- rep(NA,1)
pvals.skew.log <- rep(NA,1)
pvals.skew.excl.log <- rep(NA,1)

for (j in 1:1000){
a.skew = data.frame(subj = rep(1:30, each = 60), cond = rep(rep(c(1,2), each = 30),30))
a.skew <- a.skew %>% group_by(subj) %>% 
   # dplyr::mutate(rt = ifelse(cond >1, rst(50,jitter(0.7),jitter(1.5),jitter(5),jitter(2.5)),rst(50,jitter(0.7),jitter(1.5),jitter(5),jitter(2.5))))
  dplyr::mutate(rt = ifelse(cond >1, rexp(50,jitter(0.3)),rexp(50,jitter(0.3))))
a.skew <- data.frame(a.skew)

a.skew.log = data.frame(subj = rep(1:30, each = 60), cond = rep(rep(c(1,2), each = 30),30))
a.skew.log <- a.skew.log %>% group_by(subj) %>% 
  #dplyr::mutate(rt = ifelse(cond >1, rst(50,jitter(0.7),jitter(1.5),jitter(5),jitter(2.5)),rst(50,jitter(0.7),jitter(1.5),jitter(5),jitter(2.5))))
  dplyr::mutate(rt = ifelse(cond >1, rexp(50,jitter(0.3)),rexp(50,jitter(0.3))))
a.skew.log <- data.frame(a.skew.log)
a.skew.log$rt <- log(a.skew.log$rt)


for (i in 1:500){
  pvals.skew <- c(pvals.skew,shuffle_cond_sim(a.skew, shuffle = TRUE))
  pvals.skew.excl <- c(pvals.skew.excl,shuffle_cond_sim(a.skew, shuffle = TRUE,exclude = TRUE))
  pvals.skew.log <- c(pvals.skew.log,shuffle_cond_sim(a.skew.log, shuffle = TRUE))
  pvals.skew.excl.log <- c(pvals.skew.excl.log,shuffle_cond_sim(a.skew.log, shuffle = TRUE,exclude = TRUE))
}
}
#shuffle.data <- data.frame(expt = "Normal Distr. Simulations", facet = "untransformed data", cond = "Do not exclude by condition", pvals = pvals.norm)
#shuffle.data <- rbind(shuffle.data, data.frame(expt = "Normal Distr. Simulations", facet = "untransformed data", cond = "Exclude by condition", pvals = pvals.norm.excl))


# Normal data with exclusions

shuffle.data <- data.frame(expt = "a. Exponential Simulations", facet = "untransformed data",cond = "Do not exclude by condition", pvals = pvals.skew)
shuffle.data <- rbind(shuffle.data, data.frame(expt = "a. Exponential Simulations", facet = "untransformed data",cond = "Exclude by condition", pvals = pvals.skew.excl))
shuffle.data <- rbind(shuffle.data, data.frame(expt = "b. Exponential Simulations", facet = "log transformed data", cond = "Do not exclude by condition", pvals = pvals.skew.log))
shuffle.data <- rbind(shuffle.data, data.frame(expt = "b. Exponential Simulations", facet = "log transformed data", cond = "Exclude by condition", pvals = pvals.skew.excl.log))

ggplot(shuffle.data,aes(x=pvals,..density.., col = cond))+ facet_wrap(expt~facet)+geom_freqpoly(alpha = 1, lwd = 1.5)+xlab("p values")

##########################################################################################################################
#
# Let's  analyze  the Sklar trials with exclusions


shuffle_cond <- function(sense.pop.sklar,shuffle = TRUE){
  if (shuffle == TRUE){
    sense.pop.sklar <- sense.pop.sklar %>%
    group_by(SubjNo) %>%
    mutate(Condition = sample(Condition))
  sense.pop.sklar <- data.frame(sense.pop.sklar)
  }
  # Remove outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be < 3sd above group mean)
  Acc <- summaryBy(match. + rt ~ sense.pop.sklar$SubjNo, data = sense.pop.sklar, keep.names = T)
  sense.pop.sklar <- subset(sense.pop.sklar, SubjNo %in% Acc[Acc$match. > 0.9,]$SubjNo)
  
  sense.pop.sklar <- subset(sense.pop.sklar, SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo)
  
  # Remove incorrect trials
  sense.pop.sklar <- subset(sense.pop.sklar, match. == 1)
  
  # Remove outliers by subject (less than 3sd from participant mean -- note that this is not a symmetric exclusion criterion)
  sense.pop.sklar <- ddply(sense.pop.sklar, .(SubjNo), function(d){ 
    by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
    d = subset(d, rt < by_subj_include[2])
  })

  # [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
  sense.pop.sklar <- ddply(sense.pop.sklar, .(Condition), function(d){ 
    by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
    d = subset(d, rt < by_subj_include[2])
  })
  
  # Remove RTs < 200ms
  sense.pop.sklar <- subset(sense.pop.sklar, rt > 0.2)
  
  # T test (Sklar style)
  sense.pop.sklar$log.rt <- log(sense.pop.sklar$rt)
  sense.pop.sklar.summary <- summaryBy(rt + log.rt~ SubjNo + Condition, data = subset(sense.pop.sklar,  Condition %in% c("Sklar_violation", "Sklar_control")), keep.names = T)
  t <- t.test(rt ~ Condition, data = sense.pop.sklar.summary, paired = T)
  return(t)
}

sense.pop.sklar <- subset(sense.pop, Condition %in% c("Sklar_control", "Sklar_violation")) 

pvals.sklar <- rep(NA,5000)

for (i in 1:5000){
  t <- shuffle_cond(sense.pop.sklar)
  pvals.sklar[i] <- t$p.value
}
paste("resample p = ", length(pvals.sklar[shuffle_cond(sense.pop.sklar,FALSE)$p.value > pvals.sklar])/length(pvals.sklar))





##########################################################################################################################
#
# Let's  analyze  the Sklar trials without the exclusions

shuffle_cond_no_excl <- function(sense.pop.sklar){
  sense.pop.sklar <- sense.pop.sklar %>%
    group_by(SubjNo) %>%
    mutate(Condition = sample(Condition))
  sense.pop.sklar <- data.frame(sense.pop.sklar)
  # Remove outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be < 3sd above group mean)
  Acc <- summaryBy(match. + rt ~ SubjNo, data = subset(sense.pop.sklar), keep.names = T)
  sense.pop.sklar <- subset(sense.pop.sklar, SubjNo %in% Acc[Acc$match. > 0.9,]$SubjNo)
  
  sense.pop.sklar <- subset(sense.pop.sklar, SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo)
  
  # Remove incorrect trials
  sense.pop.sklar <- subset(sense.pop.sklar, match. == 1)
  
  # Remove outliers by subject (less than 3sd from participant mean -- note that this is not a symmetric exclusion criterion)
  sense.pop.sklar <- ddply(sense.pop.sklar, .(SubjNo), function(d){ 
    by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
    d = subset(d, rt < by_subj_include[2])
  })
  
  # [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
  #sense.pop.sklar <- ddply(sense.pop.sklar, .(Condition), function(d){ 
  #  by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
  #  d = subset(d, rt < by_subj_include[2])
  #})
  
  # Remove RTs < 200ms
  sense.pop.sklar <- subset(sense.pop.sklar, rt > 0.2)
  
  # T test (Sklar style)
  sense.pop.sklar$log.rt <- log(sense.pop.sklar$rt)
  sense.pop.sklar.summary <- summaryBy(rt + log.rt~ SubjNo + Condition, data = subset(sense.pop.sklar,  Condition %in% c("Sklar_violation", "Sklar_control")), keep.names = T)
  t <- t.test(rt ~ Condition, data = sense.pop.sklar.summary, paired = T)
  return(t)
}
sense.pop.sklar <- subset(sense.pop, Condition %in% c("Sklar_control", "Sklar_violation")) 

pvals.sklar.noexcl <- rep(NA,5000)

for (i in 1:5000){
  t <- shuffle_cond_no_excl(sense.pop.sklar)
  pvals.sklar.noexcl[i] <- t$p.value
  }

ggplot(shuffle.data,aes(x=pvals,..density.., col = cond))+ facet_wrap(~facet)+geom_freqpoly(alpha = 1, lwd = 1.5)+xlab("p values")




##########################################################################################################################
#
# Let's  analyze  the Sklar trials with exclusions using LOG RTs

shuffle_cond.log <- function(sense.pop.sklar){
  sense.pop.sklar <- sense.pop.sklar %>%
    group_by(SubjNo) %>%
    mutate(Condition = sample(Condition))
  sense.pop.sklar <- data.frame(sense.pop.sklar)
  # Remove outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be < 3sd above group mean)
  Acc <- summaryBy(match. + rt ~ SubjNo, data = subset(sense.pop.sklar), keep.names = T)
  sense.pop.sklar <- subset(sense.pop.sklar, SubjNo %in% Acc[Acc$match. > 0.9,]$SubjNo)
  
  sense.pop.sklar <- subset(sense.pop.sklar, SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo)
  
  # Remove incorrect trials
  sense.pop.sklar <- subset(sense.pop.sklar, match. == 1)
  
  # Remove outliers by subject (less than 3sd from participant mean -- note that this is not a symmetric exclusion criterion)
  sense.pop.sklar <- ddply(sense.pop.sklar, .(SubjNo), function(d){ 
    by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
    d = subset(d, rt < by_subj_include[2])
  })
  
  # [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
  sense.pop.sklar <- ddply(sense.pop.sklar, .(Condition), function(d){ 
    by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
    d = subset(d, rt < by_subj_include[2])
  })
  
  # Remove RTs < 200ms
  sense.pop.sklar <- subset(sense.pop.sklar, rt > log(0.2))
  
  # T test (Sklar style)
  sense.pop.sklar.summary <- summaryBy(rt ~ SubjNo + Condition, data = subset(sense.pop.sklar,  Condition %in% c("Sklar_violation", "Sklar_control")), keep.names = T)
  t <- t.test(rt ~ Condition, data = sense.pop.sklar.summary, paired = T)
  return(t)
}
sense.pop.sklar <- subset(sense.pop, Condition %in% c("Sklar_control", "Sklar_violation")) 
sense.pop.sklar$rt <- log(sense.pop.sklar$rt)
pvals.sklar.log <- rep(NA,5000)

for (i in 1:5000){
  t <- shuffle_cond.log(sense.pop.sklar)
  pvals.sklar.log[i] <- t$p.value
}


ggplot(shuffle.data,aes(x=pvals,..density.., col = cond))+ facet_wrap(~facet)+geom_freqpoly(alpha = 1, lwd = 1.5)+xlab("p values")


##########################################################################################################################
#
# Let's  analyze  the Sklar trials without exclusions using LOG RTs


shuffle_cond.log.excl <- function(sense.pop.sklar){
  sense.pop.sklar <- sense.pop.sklar %>%
    group_by(SubjNo) %>%
    mutate(Condition = sample(Condition))
  sense.pop.sklar <- data.frame(sense.pop.sklar)
  # Remove outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be < 3sd above group mean)
  Acc <- summaryBy(match. + rt ~ SubjNo, data = subset(sense.pop.sklar), keep.names = T)
  sense.pop.sklar <- subset(sense.pop.sklar, SubjNo %in% Acc[Acc$match. > 0.9,]$SubjNo)
  
  sense.pop.sklar <- subset(sense.pop.sklar, SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo)
  
  # Remove incorrect trials
  sense.pop.sklar <- subset(sense.pop.sklar, match. == 1)
  
  # Remove outliers by subject (less than 3sd from participant mean -- note that this is not a symmetric exclusion criterion)
  sense.pop.sklar <- ddply(sense.pop.sklar, .(SubjNo), function(d){ 
    by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
    d = subset(d, rt < by_subj_include[2])
  })
  
  # [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
  #sense.pop.sklar <- ddply(sense.pop.sklar, .(Condition), function(d){ 
  #  by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
  #   d = subset(d, rt < by_subj_include[2])
  #  })
  
  # Remove RTs < 200ms
  sense.pop.sklar <- subset(sense.pop.sklar, rt > log(0.2))
  
  # T test (Sklar style)
  sense.pop.sklar.summary <- summaryBy(rt ~ SubjNo + Condition, data = subset(sense.pop.sklar,  Condition %in% c("Sklar_violation", "Sklar_control")), keep.names = T)
  t <- t.test(rt ~ Condition, data = sense.pop.sklar.summary, paired = T)
  return(t)
}

sense.pop.sklar <- subset(sense.pop, Condition %in% c("Sklar_control", "Sklar_violation")) 
sense.pop.sklar$rt <- log(sense.pop.sklar$rt)
pvals.sklar.log.excl <- rep(NA,5000)

for (i in 1:5000){
  t <- shuffle_cond.log.excl(sense.pop.sklar)
  pvals.sklar.log.excl[i] <- t$p.value
}
ggplot(shuffle.data,aes(x=pvals,..density.., col = cond))+facet_wrap(~facet)+ geom_freqpoly(alpha = 1, lwd = 1.5)+xlab("p values")

### And for Experiment 2

Test_Import = function(path_name,expt){
  library(jsonlite)
  
  list.files(path = path_name,full.names = T, pattern = expt) -> file_list
  comp = c()
  for (x in file_list){
    file_name = x
    d <- read.csv(file_name, header = T)
    comp = rbind(comp,d)
    print(x)
  }
  comp$rt <- comp$ReactionTime_column
  comp$Acc <- comp$Valid_column
  comp$SubjNo <- comp$subject_column
  return(comp)
}


##########
# Sklar Sem
sklar.sem <- Test_Import("./Sklar_Direct_Rep/data","sklar_sem")
sklar.sem$Length <- nchar(as.character(sklar.sem$Stim_column))
sklar.sem$Length  <- (sklar.sem$Length  - mean(sklar.sem$Length,na.rm= T))/sd(sklar.sem$Length,na.rm= T)
sklar.sem <- sklar.sem[sklar.sem$Type_column != "Sklar_filler",]
sklar.sem$Condition = "Control"
sklar.sem[sklar.sem$Type_column == "Sklar_violation",]$Condition <- "Violation"

#Mark timeouts as incorrect, for exclusion
sklar.sem[sklar.sem$Acc < 0,]$Acc <- 0


### Experiment 2 vanilla
expt2.shuffle_cond <- function(sklar.sem,shuffle = TRUE){
  if (shuffle == TRUE){
    sklar.sem <- sklar.sem %>%
      group_by(SubjNo) %>%
      mutate(Condition = sample(Condition))
    sklar.sem <- data.frame(sklar.sem)
  }
#sklar.sem$rt <- log(sklar.sem$rt)
# Rsemve outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be > (!!, see by trial exclusion below) 3sd from group mean [ie we rsemve extra-fast participants)
Acc <- summaryBy(Acc + rt ~ SubjNo, data = sklar.sem, keep.names = T)

sklar.sem <- sklar.sem[sklar.sem$SubjNo %in% Acc[Acc$Acc > 0.9,]$SubjNo,]
# Also exclude

sklar.sem <- sklar.sem[sklar.sem$SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo,]
#Rsemve incorrect trials
sklar.sem <- sklar.sem[sklar.sem$Acc ==1,]

# Rsemve RTs +/-3sd from each subject's mean 
sklar.sem <- ddply(sklar.sem, .(SubjNo), function(d){ 
  include = mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
  d = subset(d, rt > include[1] & rt < include[2])
})
# [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
sklar.sem <- ddply(sklar.sem, .(Condition), function(d){ 
  by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
  d = subset(d, rt < by_subj_include[2])
})
# Rsemve RTs < 200ms & > 10
sklar.sem <- subset(sklar.sem, rt > 0.2)#log(0.2))
sklar.sem <- subset(sklar.sem, rt <= 10)#log(10))

sklar.sem.sum <- summaryBy(rt ~ SubjNo + Condition, data = sklar.sem, keep.names = T)
t <- t.test(rt ~ Condition, data = sklar.sem.sum, paired = T)
return(t)
}

expt2.pvals.sklar <- rep(NA,5000)

for (i in 1:5000){
  t <- expt2.shuffle_cond(sklar.sem)
  expt2.pvals.sklar[i] <- t$p.value
}

ggplot(shuffle.data,aes(x=pvals,..density.., col = cond))+ facet_wrap(~facet)+geom_freqpoly(alpha = 1, lwd = 1.5)+xlab("p values")

### Experiment 2 remove by subj and condition exclusion
expt2.shuffle_cond.excl <- function(sklar.sem,shuffle = TRUE){
  if (shuffle == TRUE){
    sklar.sem <- sklar.sem %>%
      group_by(SubjNo) %>%
      mutate(Condition = sample(Condition))
    sklar.sem <- data.frame(sklar.sem)
  }
  #sklar.sem$rt <- log(sklar.sem$rt)
  # Rsemve outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be > (!!, see by trial exclusion below) 3sd from group mean [ie we rsemve extra-fast participants)
  Acc <- summaryBy(Acc + rt ~ SubjNo, data = sklar.sem, keep.names = T)
  
  sklar.sem <- sklar.sem[sklar.sem$SubjNo %in% Acc[Acc$Acc > 0.9,]$SubjNo,]
  # Also exclude
  
  sklar.sem <- sklar.sem[sklar.sem$SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo,]
  #Rsemve incorrect trials
  sklar.sem <- sklar.sem[sklar.sem$Acc ==1,]
  
  # Rsemve RTs +/-3sd from each subject's mean 
  sklar.sem <- ddply(sklar.sem, .(SubjNo), function(d){ 
    include = mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
    d = subset(d, rt > include[1] & rt < include[2])
  })
  # [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
 # sklar.sem <- ddply(sklar.sem, .(Condition), function(d){ 
#    by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
 #   d = subset(d, rt < by_subj_include[2])
#  })
  # Rsemve RTs < 200ms & > 10
  sklar.sem <- subset(sklar.sem, rt > 0.2)#log(0.2))
  sklar.sem <- subset(sklar.sem, rt <= 10)#log(10))
  
  sklar.sem.sum <- summaryBy(rt ~ SubjNo + Condition, data = sklar.sem, keep.names = T)
  t <- t.test(rt ~ Condition, data = sklar.sem.sum, paired = T)
  return(t)
}

expt2.pvals.sklar.excl <- rep(NA,5000)

for (i in 1:5000){
  t <- expt2.shuffle_cond.excl(sklar.sem)
  expt2.pvals.sklar.excl[i] <- t$p.value
}

ggplot(shuffle.data,aes(x=pvals,..density.., col = cond))+ facet_wrap(~facet)+geom_freqpoly(alpha = 1, lwd = 1.5)+xlab("p values")


### Experiment 2 log vanilla
expt2.shuffle_cond.log <- function(sklar.sem,shuffle = TRUE){
  if (shuffle == TRUE){
    sklar.sem <- sklar.sem %>%
      group_by(SubjNo) %>%
      mutate(Condition = sample(Condition))
    sklar.sem <- data.frame(sklar.sem)
  }
  sklar.sem$rt <- log(sklar.sem$rt)
  # Rsemve outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be > (!!, see by trial exclusion below) 3sd from group mean [ie we rsemve extra-fast participants)
  Acc <- summaryBy(Acc + rt ~ SubjNo, data = sklar.sem, keep.names = T)
  
  sklar.sem <- sklar.sem[sklar.sem$SubjNo %in% Acc[Acc$Acc > 0.9,]$SubjNo,]
  # Also exclude
  
  sklar.sem <- sklar.sem[sklar.sem$SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo,]
  #Rsemve incorrect trials
  sklar.sem <- sklar.sem[sklar.sem$Acc ==1,]
  
  # Rsemve RTs +/-3sd from each subject's mean 
  sklar.sem <- ddply(sklar.sem, .(SubjNo), function(d){ 
    include = mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
    d = subset(d, rt > include[1] & rt < include[2])
  })
  # [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
  sklar.sem <- ddply(sklar.sem, .(Condition), function(d){ 
    by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
    d = subset(d, rt < by_subj_include[2])
  })
  # Rsemve RTs < 200ms & > 10
  sklar.sem <- subset(sklar.sem, rt > log(0.2))
  sklar.sem <- subset(sklar.sem, rt <= log(10))
  
  sklar.sem.sum <- summaryBy(rt ~ SubjNo + Condition, data = sklar.sem, keep.names = T)
  t <- t.test(rt ~ Condition, data = sklar.sem.sum, paired = T)
  return(t)
}

expt2.pvals.sklar.log <- rep(NA,5000)

for (i in 1:5000){
  t <- expt2.shuffle_cond.log(sklar.sem)
  expt2.pvals.sklar.log[i] <- t$p.value
}

ggplot(shuffle.data,aes(x=pvals,..density.., col = cond))+ facet_wrap(~facet)+geom_freqpoly(alpha = 1, lwd = 1.5)+xlab("p values")

### Experiment 2 log remove by subj and condition exclusion
expt2.shuffle_cond.excl.log <- function(sklar.sem,shuffle = TRUE){
  if (shuffle == TRUE){
    sklar.sem <- sklar.sem %>%
      group_by(SubjNo) %>%
      mutate(Condition = sample(Condition))
    sklar.sem <- data.frame(sklar.sem)
  }
  sklar.sem$rt <- log(sklar.sem$rt)
  # Rsemve outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be > (!!, see by trial exclusion below) 3sd from group mean [ie we rsemve extra-fast participants)
  Acc <- summaryBy(Acc + rt ~ SubjNo, data = sklar.sem, keep.names = T)
  
  sklar.sem <- sklar.sem[sklar.sem$SubjNo %in% Acc[Acc$Acc > 0.9,]$SubjNo,]
  # Also exclude
  
  sklar.sem <- sklar.sem[sklar.sem$SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo,]
  #Rsemve incorrect trials
  sklar.sem <- sklar.sem[sklar.sem$Acc ==1,]
  
  # Rsemve RTs +/-3sd from each subject's mean 
  sklar.sem <- ddply(sklar.sem, .(SubjNo), function(d){ 
    include = mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
    d = subset(d, rt > include[1] & rt < include[2])
  })
  # [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
  # sklar.sem <- ddply(sklar.sem, .(Condition), function(d){ 
  #    by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
  #   d = subset(d, rt < by_subj_include[2])
  #  })
  # Rsemve RTs < 200ms & > 10
  sklar.sem <- subset(sklar.sem, rt > log(0.2))
  sklar.sem <- subset(sklar.sem, rt <= log(10))
  
  sklar.sem.sum <- summaryBy(rt ~ SubjNo + Condition, data = sklar.sem, keep.names = T)
  t <- t.test(rt ~ Condition, data = sklar.sem.sum, paired = T)
  return(t)
}

expt2.pvals.sklar.excl.log <- rep(NA,5000)

for (i in 1:5000){
  t <- expt2.shuffle_cond.excl.log(sklar.sem)
  expt2.pvals.sklar.excl.log[i] <- t$p.value
}


shuffle.data <- rbind(shuffle.data, data.frame(expt = "c. Study 1", facet = "untransformed data",
                                               cond = "Do not exclude by condition", pvals = pvals.sklar.noexcl))
shuffle.data <- rbind(shuffle.data, data.frame(expt = "c. Study 1", facet = "untransformed data",
                                               cond = "Exclude by condition", pvals = pvals.sklar))
shuffle.data <- rbind(shuffle.data, data.frame(expt = "d. Study 1", facet = "log transformed data",
                                               cond = "Exclude by condition", pvals = pvals.sklar.log))
shuffle.data <- rbind(shuffle.data, data.frame(expt = "d. Study 1", facet = "log transformed data",
                                               cond = "Do not exclude by condition", pvals = pvals.sklar.log.excl))
shuffle.data <- rbind(shuffle.data, data.frame(expt = "e. Study 6", facet = "untransformed data",
                                               cond = "Exclude by condition", pvals = expt2.pvals.sklar))
shuffle.data <- rbind(shuffle.data, data.frame(expt = "e. Study 6", facet = "untransformed data",
                                               cond = "Do not exclude by condition", pvals = expt2.pvals.sklar.excl))
shuffle.data <- rbind(shuffle.data, data.frame(expt = "f. Study 6", facet = "log transformed data",
                                               cond = "Exclude by condition", pvals = expt2.pvals.sklar.log))
shuffle.data <- rbind(shuffle.data, data.frame(expt = "f. Study 6", facet = "log transformed data",
                                               cond = "Do not exclude by condition", pvals = expt2.pvals.sklar.excl.log))

shuffle.data$cond <- ordered(shuffle.data$cond, levels = c("Exclude by condition","Do not exclude by condition"))

ggplot(shuffle.data,aes(x=pvals,..density.., lty = cond))+ facet_wrap(expt~facet, ncol = 2)+
  geom_freqpoly(alpha = 1, lwd = 1.5)+xlab("p values")+
  theme(legend.position=c(0.26,0.95),legend.background = element_rect(fill=alpha('white', 0)),legend.title=element_blank(),legend.key = element_rect(colour = NA))+
  xlim(c(0,1))

# Comparison against a uniform distribution
ks.test(subset(shuffle.data, expt == "a. Exponential Simulations" & facet == "untransformed data" & cond == "Exclude by condition")$pvals, "punif",0,1)
ks.test(subset(shuffle.data, expt == "a. Exponential Simulations" & facet == "untransformed data" & cond != "Exclude by condition")$pvals, "punif",0,1)
ks.test(subset(shuffle.data, expt == "b. Exponential Simulations" & facet != "untransformed data" & cond == "Exclude by condition")$pvals, "punif",0,1)
ks.test(subset(shuffle.data, expt == "b. Exponential Simulations" & facet != "untransformed data" & cond != "Exclude by condition")$pvals, "punif",0,1)

# Comparison between distribitions
ks.test(subset(shuffle.data, expt == "a. Exponential Simulations" & facet == "untransformed data" & cond == "Exclude by condition")$pvals, 
        subset(shuffle.data, expt == "a. Exponential Simulations" & facet == "untransformed data" & cond != "Exclude by condition")$pvals)
ks.test(subset(shuffle.data, expt == "b. Exponential Simulations" & facet != "untransformed data" & cond == "Exclude by condition")$pvals, 
        subset(shuffle.data, expt == "b. Exponential Simulations" & facet != "untransformed data" & cond != "Exclude by condition")$pvals)

# For Study 1
ks.test(subset(shuffle.data, expt == "c. Study 1" & facet == "untransformed data" & cond == "Exclude by condition")$pvals, 
      subset(shuffle.data, expt == "c. Study 1" & facet == "untransformed data" & cond != "Exclude by condition")$pvals)
ks.test(subset(shuffle.data, expt == "d. Study 1" & facet == "log transformed data" & cond == "Exclude by condition")$pvals, 
        subset(shuffle.data, expt == "d. Study 1" & facet == "log transformed data" & cond != "Exclude by condition")$pvals)

# For Study 6
ks.test(subset(shuffle.data, expt == "e. Study 6" & facet == "untransformed data" & cond == "Exclude by condition")$pvals, 
        subset(shuffle.data, expt == "e. Study 6" & facet == "untransformed data" & cond != "Exclude by condition")$pvals)
ks.test(subset(shuffle.data, expt == "f. Study 6" & facet == "log transformed data" & cond == "Exclude by condition")$pvals, 
        subset(shuffle.data, expt == "f. Study 6" & facet == "log transformed data" & cond != "Exclude by condition")$pvals)

