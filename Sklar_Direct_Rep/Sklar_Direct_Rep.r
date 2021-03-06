library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(lme4)
library(ez)
library(jsonlite)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(doBy)
## for bootstrapping 95% confidence intervals -- from Mike Frank https://github.com/langcog/KTE/blob/master/mcf.useful.R
library(bootstrap)
# From Mike Frank
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}





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
# Sklar Emo
sklar.emo <- Test_Import("./data","sklar_emo")
sklar.emo.demogs <- summaryBy(subject_age_column  ~ subject_column+ subject_gender_column, data = sklar.emo )
mean(sklar.emo.demogs$subject_age_column.mean)
range(sklar.emo.demogs$subject_age_column.mean)
length(sklar.emo.demogs[sklar.emo.demogs$subject_gender_column == "f",]$subject_gender_column)


sklar.emo$Length <- nchar(as.character(sklar.emo$Stim_column))

#Mark timeouts as incorrect, for exclusion
sklar.emo[sklar.emo$Acc < 0,]$Acc <- 0

# Remove outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be > (!!, see by trial exclusion below) 3sd from group mean [ie we remove extra-fast participants)
Acc <- summaryBy(Acc + rt ~ SubjNo, data = sklar.emo, keep.names = T)
sklar.emo <- sklar.emo[sklar.emo$SubjNo %in% Acc[Acc$Acc > 0.9,]$SubjNo,]
sklar.emo <- sklar.emo[sklar.emo$SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo,]
#Remove incorrect trials
sklar.emo <- sklar.emo[sklar.emo$Acc ==1,]
# Remove RTs +/-3sd from each subject's mean 
sklar.emo <- ddply(sklar.emo, .(SubjNo), function(d){ 
  include = mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
  d = subset(d, rt > include[1] & rt < include[2])
})

# Remove RTs < 200ms & > 1000ms
sklar.emo <- subset(sklar.emo, rt > 0.2)
sklar.emo <- subset(sklar.emo, rt <= 10)

# First standardize the MeanAffectivity score
sklar.emo$MeanAffectivity_column <- (sklar.emo$MeanAffectivity_column - mean(sklar.emo$MeanAffectivity_column, na.rm = T))/sd(sklar.emo$MeanAffectivity_column, na.rm = T)

# Lin Reg (sklar style)
sklar.emo$log.rt <- log(sklar.emo$rt)
sklar.emo.sum <- summaryBy(rt + log.rt~ Stim_column + MeanAffectivity_column, data = sklar.emo, keep.names = T)
summary(lm(rt ~ MeanAffectivity_column, data = sklar.emo.sum))
summary(lm(log.rt ~ MeanAffectivity_column, data = sklar.emo.sum))

#############
# Edinburgh Emo Expt

edin.emo <- Test_Import("./data","edin_emo")
edin.emo$Length <- nchar(as.character(edin.emo$Stim_column))
#Mark timeouts as incorrect, for exclusion
edin.emo[edin.emo$Acc < 0,]$Acc <- 0
# Remove outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be > (!!, see by trial exclusion below) 3sd from group mean [ie we remove extra-fast participants)
Acc <- summaryBy(Acc + rt ~ SubjNo, data = edin.emo, keep.names = T)
edin.emo <- edin.emo[edin.emo$SubjNo %in% Acc[Acc$Acc > 0.9,]$SubjNo,]
edin.emo <- edin.emo[edin.emo$SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo,]
#Remove incorrect trials
edin.emo <- edin.emo[edin.emo$Acc ==1,]
# Remove RTs +/-3sd from each subject's mean 
edin.emo <- ddply(edin.emo, .(SubjNo), function(d){ 
  include = mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
  d = subset(d, rt > include[1] & rt < include[2])
})

# Remove RTs < 200ms & > 10s
edin.emo <- subset(edin.emo, rt > 0.2)
edin.emo <- subset(edin.emo, rt <= 10)

# First standardize the MeanAffectivity score
edin.emo$MeanAffectivity_column <- (edin.emo$MeanAffectivity_column - mean(edin.emo$MeanAffectivity_column, na.rm = T))/sd(edin.emo$MeanAffectivity_column, na.rm = T)

# Lin Reg (edin style)
edin.emo$log.rt <- log(edin.emo$rt)
edin.emo.sum <- summaryBy(rt + log.rt~ Stim_column + MeanAffectivity_column, data = edin.emo, keep.names = T)
summary(lm(rt ~ MeanAffectivity_column, data = edin.emo.sum))
summary(lm(log.rt ~ MeanAffectivity_column, data = edin.emo.sum))

# Lin Reg (sklar style)
edin.emo$log.rt <- log(edin.emo$rt)
edin.emo.sum.factorial <- summaryBy(rt + log.rt ~ SubjNo + Type_column, data = edin.emo, keep.names = T)
t.test(rt ~ Type_column, data = edin.emo.sum.factorial, paired = T)
t.test(log.rt ~ Type_column, data = edin.emo.sum.factorial, paired = T)

# lmer (Rabag style)
edin.emo.sum.raw <- summary(lmer(rt ~ Type_column + (1+Type_column|SubjNo)+ (1|Stim_column), data = edin.emo))
print(edin.emo.sum.raw)
print(paste("p value = ", 2*pnorm(-abs(coef(edin.emo.sum.raw)[,3]))))

edin.emo.sum.log <- summary(lmer(log.rt ~ Type_column + (1+Type_column|SubjNo)+ (1|Stim_column), data = edin.emo))
print(edin.emo.sum.log)
print(paste("p value = ", 2*pnorm(-abs(coef(edin.emo.sum.log)[,3]))))

sense.graph <- summaryBy(rt ~ Type_column + SubjNo, data = edin.emo, keep.names = T)
sense.graph <- summaryBy(rt ~ Type_column, data = sense.graph, FUN = c(mean,sd))
sense.graph$SE <- sense.graph$rt.sd/sqrt(length(unique(emo.pop.new$SubjNo)))
sense.graph$rt.mean <- sense.graph$rt.mean * 1000
sense.graph$SE <- sense.graph$SE * 1000

dodge <- position_dodge(width=0.9)
ggplot(sense.graph, aes(Type_column,rt.mean, fill = Type_column)) +
  geom_bar(stat = "identity",  position = dodge) +
  geom_errorbar(aes(ymax = sense.graph$rt.mean +
                      sense.graph$SE, ymin = sense.graph$rt.mean - sense.graph$SE), width=0.25, position = dodge) +
  labs(fill = "Sentence Type") + 
  theme(axis.text.x = element_text(colour = "black", size = 12)) +
  ylab("Response Time (ms)") +
  xlab("") + 
  ylim(c(0,2000))
##################
# Emo Graphs

sklar.emo.sum$Experiment <- "Experiment 3c\n(Expt. 2a Replication)\nTwo Word Phrases"
edin.emo.sum$Experiment <- "Experiment 3d\n(Expt. 2b Replication)\nReversible Sentences"

graph <- rbind(sklar.emo.sum,edin.emo.sum)

graph <- na.omit(graph)
graph$Experiment <- ordered(graph$Experiment, levels = c("Experiment 3c\n(Expt. 2a Replication)\nTwo Word Phrases", "Experiment 3d\n(Expt. 2b Replication)\nReversible Sentences"))

graph$rt <- graph$rt * 1000

emo.graph <- ggplot(graph, aes(x=MeanAffectivity_column, y=rt)) +
  geom_point(size=2) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE) + facet_grid(.~Experiment) + labs(y = "Response Time (ms)", x = "Standardized Valence Rating")+ 
  theme(strip.text.x = element_text(size = 12))


##########
# Sklar Sem
sklar.sem <- Test_Import("./data","sklar_sem")
sklar.sem$Length <- nchar(as.character(sklar.sem$Stim_column))
sklar.sem$Length  <- (sklar.sem$Length  - mean(sklar.sem$Length,na.rm= T))/sd(sklar.sem$Length,na.rm= T)
sklar.sem <- sklar.sem[sklar.sem$Type_column != "Sklar_filler",]
sklar.sem$Condition = "Control"
sklar.sem[sklar.sem$Type_column == "Sklar_violation",]$Condition <- "Violation"

#Mark timeouts as incorrect, for exclusion
sklar.sem[sklar.sem$Acc < 0,]$Acc <- 0

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
sklar.sem <- subset(sklar.sem, rt > 0.2)
sklar.sem <- subset(sklar.sem, rt <= 10)

# Lin Reg (sklar style)
sklar.sem$log.rt <- log(sklar.sem$rt)
sklar.sem.sum <- summaryBy(rt + log.rt~ SubjNo + Condition, data = sklar.sem, keep.names = T)
t.test(rt ~ Condition, data = sklar.sem.sum, paired = T)
t.test(log.rt ~ Condition, data = sklar.sem.sum, paired = T)

# lmer (Rabag style)
sklar.sem.sum.raw <- summary(lmer(rt ~ Condition + (1+Condition|SubjNo)+ (1|Stim_column), data = sklar.sem))
print(sklar.sem.sum.raw)
print(paste("p value = ", 2*pnorm(-abs(coef(sklar.sem.sum.raw)[,3]))))

sklar.sem.sum.log <- summary(lmer(log(rt) ~ Condition + (1+Condition|SubjNo)+ (1|Stim_column), data = sklar.sem))
print(sklar.sem.sum.log)
print(paste("p value = ", 2*pnorm(-abs(coef(sklar.sem.sum.log)[,3]))))

##########
# Edin Sem
edin.sem <- Test_Import("./data","edin_sem")
edin.sem$Length <- nchar(as.character(edin.sem$Stim_column))
edin.sem$Length <- (edin.sem$Length - mean(edin.sem$Length))/sd(edin.sem$Length)
edin.sem$Condition = "Violation"
edin.sem[edin.sem$Type_column == "Sensible",]$Condition <- "Control"

#Mark timeouts as incorrect, for exclusion
edin.sem[edin.sem$Acc < 0,]$Acc <- 0


# Rsemve outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be > (!!, see by trial exclusion below) 3sd from group mean [ie we rsemve extra-fast participants)
Acc <- summaryBy(Acc + rt ~ SubjNo, data = edin.sem, keep.names = T)
edin.sem <- edin.sem[edin.sem$SubjNo %in% Acc[Acc$Acc > 0.9,]$SubjNo,]
edin.sem <- edin.sem[edin.sem$SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo,]
#Rsemve incorrect trials
edin.sem <- edin.sem[edin.sem$Acc ==1,]
# Rsemve RTs +/-3sd from each subject's mean 
edin.sem <- ddply(edin.sem, .(SubjNo), function(d){ 
  include = mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
  d = subset(d, rt > include[1] & rt < include[2])
})
# [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
edin.sem <- ddply(edin.sem, .(Condition), function(d){ 
  by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
  d = subset(d, rt < by_subj_include[2])
})
# Rsemve RTs < 200ms & > 10
edin.sem <- subset(edin.sem, rt > 0.2)
edin.sem <- subset(edin.sem, rt <= 10)

# Lin Reg (sklar style)
edin.sem$log.rt <- log(edin.sem$rt)
edin.sem.sum <- summaryBy(rt + log.rt ~ SubjNo + Condition, data = edin.sem, keep.names = T)
t.test(rt ~ Condition, data = edin.sem.sum, paired = T)
t.test(log.rt ~ Condition, data = edin.sem.sum, paired = T)

# lmer (Rabag style)
edin.sem.sum.raw <- summary(lmer(rt ~ Condition + (1+Condition|SubjNo)+ (1|Stim_column), data = edin.sem))
print(edin.sem.sum.raw)
print(paste("p value = ", 2*pnorm(-abs(coef(edin.sem.sum.raw)[,3]))))

edin.sem.sum.log <- summary(lmer(log.rt ~ Condition + (1+Condition|SubjNo)+ (1|Stim_column), data = edin.sem))
print(edin.sem.sum.log)
print(paste("p value = ", 2*pnorm(-abs(coef(edin.sem.sum.log)[,3]))))

###########
# Sem Graphs

sense.sklar.graph <- summaryBy(rt ~ Condition + SubjNo, data = sklar.sem, keep.names = T)
sense.sklar.graph <- summaryBy(rt ~ Condition, data = sense.sklar.graph, FUN = c(mean,ci.low,ci.high,sd))
sense.sklar.graph$SE <- sense.sklar.graph$rt.sd/sqrt(length(unique(sklar.sem$SubjNo)))
sense.sklar.graph$Experiment <- "Experiment 3a\n(Expt. 1a Replication)\nIncongruent Phrases"

sense.new.graph <- summaryBy(rt ~ Condition + SubjNo, data = edin.sem, keep.names = T)
sense.new.graph <- summaryBy(rt ~ Condition, data = sense.new.graph, FUN = c(mean,ci.low,ci.high,sd))
sense.new.graph$SE <- sense.new.graph$rt.sd/sqrt(length(unique(edin.sem$SubjNo)))
sense.new.graph$Experiment <- "Experiment 3b\n(Expt. 1b Replication)\nReversible Sentences"

sense.graph <- rbind(sense.sklar.graph,sense.new.graph)
sense.graph$Cond_Graph <- "Violation"
sense.graph[sense.graph$Condition %in% c("Control"),]$Cond_Graph <- "Control"
sense.graph$Cond_Graph <- ordered(sense.graph$Cond_Graph, levels = c("Violation","Control")) 
sense.graph$rt.mean <- sense.graph$rt.mean * 1000
sense.graph$SE <- sense.graph$SE * 1000
sense.graph$rt.ci.high <- sense.graph$rt.ci.high * 1000
sense.graph$rt.ci.low <- sense.graph$rt.ci.low * 1000

dodge <- position_dodge(width=0.9)
sem.graph <- ggplot(sense.graph, aes(Experiment,rt.mean, fill = Cond_Graph)) +
  geom_bar(stat = "identity",  position = dodge) +
  geom_errorbar(aes(ymax = sense.graph$rt.mean +
                      sense.graph$rt.ci.high, ymin = sense.graph$rt.mean - sense.graph$rt.ci.low), width=0.25, position = dodge) +
  labs(fill = "Sentence Type") + 
  theme(axis.text.x = element_text(colour = "black", size = 12), legend.position = c(0.85,0.85), legend.background = element_rect(fill=alpha('white',0))) +
  ylab("Response Time (ms)") +
  xlab("") + 
  ylim(c(0,2500))#+
  #scale_fill_brewer(palette = "Set1")

a <- grid.arrange(sem.graph, emo.graph, nrow = 1, widths = 4:5)
