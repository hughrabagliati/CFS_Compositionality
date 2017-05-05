
# Read in and re-analyze Sklar et al's findings

library(tidyr)
library(doBy)
library(dplyr)
library(plyr)
analyze_expt1 <- function(sklar.expt1, log = FALSE, by_cond = TRUE,recip = FALSE){
  if(log == TRUE){
    sklar.expt1$rt <- log(sklar.expt1$rt)
  }
  if(recip == TRUE){
    sklar.expt1$rt <- 1/sklar.expt1$rt
  }
  
  # In our script, we have this section to remove outliers by subject based on accuracy and RT
  # But in the Sklar data, that participant has already been removed
  # Remove outlier subjects by  RT but not by Acc because that isn't coded here (mean acc must be > 0.9, mean rt must be < 3sd above group mean)
#  Acc <- summaryBy(rt ~ Subject, data = subset(sklar.expt1), keep.names = T,na.rm = T)
  
#  sklar.expt1 <- subset(sklar.expt1, Subject %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$Subject)
  
  if(recip == FALSE){
  # Remove outliers by subject (less than 3sd from participant mean -- note that this is not a symmetric exclusion criterion)
  sklar.expt1 <- ddply(sklar.expt1, .(Subject), function(d){ 
    by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
    d = subset(d, rt <= by_subj_include[2])
  }) } else if(recip == TRUE){
    sklar.expt1 <- ddply(sklar.expt1, .(Subject), function(d){ 
      by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
      d = subset(d, rt >= by_subj_include[1])
    })
  }
  
  if(recip == FALSE){
  if(by_cond == TRUE){
  # [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
  sklar.expt1 <- ddply(sklar.expt1, .(Condition), function(d){ 
    by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
    d = subset(d, rt <= by_subj_include[2])
  })
  } } else if(recip == TRUE){
    if(by_cond == TRUE){
      # [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
      sklar.expt1 <- ddply(sklar.expt1, .(Condition), function(d){ 
        by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
        d = subset(d, rt >= by_subj_include[1])
      })   
  }
  }
  
  # Remove RTs < 200ms
  if(log == TRUE){
    sklar.expt1 <- subset(sklar.expt1, rt > log(200))
  } else if(recip == TRUE) {
    sklar.expt1 <- subset(sklar.expt1, rt < (1/200))
  } else {
    sklar.expt1 <- subset(sklar.expt1, rt > 200)
  }
  
  # T test (Sklar style)
  sklar.expt1.summary <- summaryBy(rt ~ Subject + Condition, data = sklar.expt1, keep.names = T, na.rm = T)
  t.test(rt ~ Condition, data = sklar.expt1.summary, paired = T,var.equal = T)
  ttest = t.test(rt ~ Condition, data = sklar.expt1.summary, paired = T)
  correlation = cor.test(subset(sklar.expt1.summary, Condition == "Control")$rt,subset(sklar.expt1.summary, Condition != "Control")$rt)
  return(list(ttest = ttest, 
              summary = summaryBy(rt ~  Condition, data = sklar.expt1.summary, keep.names = T, na.rm = T, FUN = c(mean,sd)),
              correlation = correlation,
              effect_size = ttest$statistic * sqrt(2*(1-correlation$estimate)/(ttest$parameter+1))
              ))
#              subjects = sklar.expt1.summary))
}



sklar.expt1_wide <- read.csv("Sklar_Expt1_Wide.csv")
wide_colums = paste("X",c(1:20,22:29,31:33),sep ="")
# For some reason trials 21, 25 and 30 are missing
sklar.expt1 <- gather_(sklar.expt1_wide, "Subject", "rt", wide_colums)

sklar.expt1 <- subset(sklar.expt1, haf != "fil4")

sklar.expt1$Condition <- as.factor(ifelse(sklar.expt1$haf == "haf4", "Violation", "Control"))

skewness(((sklar.expt1$rt)), na.rm = T)
skewness(log((sklar.expt1$rt)), na.rm = T)
skewness((1/sklar.expt1$rt), na.rm = T)

analyze_expt1(sklar.expt1, log = FALSE, by_cond = TRUE)
analyze_expt1(sklar.expt1, log = FALSE, by_cond = FALSE)
analyze_expt1(sklar.expt1, log = TRUE, by_cond = TRUE)
analyze_expt1(sklar.expt1, log = TRUE, by_cond = FALSE)

analyze_expt1(sklar.expt1, log = FALSE, by_cond = TRUE,recip = TRUE)
analyze_expt1(sklar.expt1, log = FALSE, by_cond = FALSE,recip = TRUE)


#from column 5 to 25
sklar.expt2_wide <- read.csv("Sklar_Expt2_Wide.csv")
sklar.expt2 <- gather(sklar.expt2_wide, Subject, rt, 5:25)

sklar.expt2 <- subset(sklar.expt2, haf != "fil4")
sklar.expt2$Condition <- as.factor(ifelse(sklar.expt2$haf == "haf4", "Violation", "Control"))

skewness(((sklar.expt2$rt)), na.rm = T)
skewness(log((sklar.expt2$rt)), na.rm = T)
skewness((1/sklar.expt2$rt), na.rm = T)

analyze_expt1(sklar.expt2, log = FALSE, by_cond = TRUE)
analyze_expt1(sklar.expt2, log = FALSE, by_cond = FALSE)
analyze_expt1(sklar.expt2, log = TRUE, by_cond = TRUE)
analyze_expt1(sklar.expt2, log = TRUE, by_cond = FALSE)

analyze_expt1(sklar.expt2, log = FALSE, by_cond = TRUE,recip = TRUE)
analyze_expt1(sklar.expt2, log = FALSE, by_cond = FALSE,recip = TRUE)

