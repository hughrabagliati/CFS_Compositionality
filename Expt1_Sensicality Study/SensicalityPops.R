library(plyr)
library(lme4)
library(doBy)
library(ggplot2)

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

sense.pop <- read_data("./data/")

# Make RTs numeric [need to remove timeout "none" responses to 8s]
sense.pop <- subset(sense.pop, rt != "None")
sense.pop$rt <- as.numeric(sense.pop$rt)
sense.pop$Length <- nchar(as.character(sense.pop$prime),allowNA = T)
sense.pop$Condition <- as.character(sense.pop$prime_semantics)
sense.pop[sense.pop$prime_semantics %in% c("Sklar_control_A","Sklar_control_B"),]$Condition <- "Sklar_control"
sense.pop$Condition <- as.factor(sense.pop$Condition)


# Note that this analysis includes all of the inclusion criteria discussed by Sklar et al. 

##########################################################################################################################
#
# Let's first analyze for the Sklar trials
sense.pop.sklar <- subset(sense.pop, Condition %in% c("Sklar_control", "Sklar_violation")) 

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
sense.pop.sklar <- subset(sense.pop.sklar, rt > 0.2)

# T test (Sklar style)
sense.pop.sklar.summary <- summaryBy(rt ~ SubjNo + Condition, data = subset(sense.pop.sklar,  Condition %in% c("Sklar_violation", "Sklar_control")), keep.names = T)
t.test(rt ~ Condition, data = sense.pop.sklar.summary, paired = T)
t.test(log(rt) ~ Condition, data = sense.pop.sklar.summary, paired = T)

# Bayes factor -- minimum effect of 0.01, maximum of 0.06, our effect = -0.03519684 and our SE = -0.03/-1.7874=  0.01678416

# lmer (Rabag style)
sense.pop.sklar.raw <- summary(lmer(rt ~ Condition + (1+Condition|SubjNo)+ (1|prime), data = subset(sense.pop.sklar,  Condition %in% c("Sklar_violation", "Sklar_control"))))
print(sense.pop.sklar.raw)
print(paste("p value = ", 2*pnorm(-abs(coef(sense.pop.sklar.raw)[,3]))))

sense.pop.sklar.log <- summary(lmer(log(rt) ~ Condition + (1+Condition|SubjNo)+ (1|prime), data = subset(sense.pop.sklar,  Condition %in% c("Sklar_violation", "Sklar_control"))))
print(sense.pop.sklar.log)
print(paste("p value = ", 2*pnorm(-abs(coef(sense.pop.sklar.log)[,3]))))


##########################################################################################################################
#
# Let's now analyze for the new trials
sense.pop.new <- subset(sense.pop, Condition %in% c("Sensible", "Non-sensible")) 

# Remove outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be > (!!, see by trial exclusion below) 3sd from group mean)
Acc <- summaryBy(match. + rt ~ SubjNo, data = subset(sense.pop.new), keep.names = T)
sense.pop.new <- subset(sense.pop.new, SubjNo %in% Acc[Acc$match. > 0.9,]$SubjNo)
sense.pop.new <- subset(sense.pop.new, SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo)

# Remove incorrect trials
sense.pop.new <- subset(sense.pop.new, match. == 1)

# Remove outliers by subject (less than 3sd from participant mean -- note that this is not a symmetric exclusion criterion)
sense.pop.new <- ddply(sense.pop.new, .(SubjNo), function(d){ 
	by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
	d = subset(d, rt < by_subj_include[2])
	})
	
# [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
sense.pop.new <- ddply(sense.pop.new, .(Condition), function(d){ 
	by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
	d = subset(d, rt < by_subj_include[2])
	})
# Remove RTs < 200ms
sense.pop.new <- subset(sense.pop.new, rt > 0.2)

# T test (Sklar style)
sense.pop.new.summary <- summaryBy(rt ~ SubjNo + Condition, data = subset(sense.pop.new,  Condition %in% c("Non-sensible","Sensible")), keep.names = T)
t.test(rt ~ Condition, data = sense.pop.new.summary, paired = T)
t.test(log(rt) ~ Condition, data = sense.pop.new.summary, paired = T)

# Bayes factor -- minimum effect of 0.01, maximum of 0.06, our effect = -0.0002327283 and our SE = -0.03/-0.02217=  0.01049744


# lmer (rabag style)
sense.pop.new.raw <-summary(lmer(rt ~ Condition + (1+Condition|SubjNo)+ (1|prime), data = subset(sense.pop.new,  Condition %in% c("Non-sensible","Sensible"))))
print(sense.pop.new.raw)
print(paste("p value = ", 2*pnorm(-abs(coef(sense.pop.new.raw)[,3]))))

sense.pop.new.log <- summary(lmer(log(rt) ~ Condition + (1+Condition|SubjNo)+ (1|prime), data = subset(sense.pop.new,  Condition %in% c("Non-sensible","Sensible"))))
print(sense.pop.new.log)
print(paste("p value = ", 2*pnorm(-abs(coef(sense.pop.new.log)[,3]))))

###########################################################################################################################
#
# Finally -- a quick test if longer stims are perceived faster than shorter, 

sense.pop.length <- sense.pop

# Remove outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be > (!!, see by trial exclusion below) 3sd from group mean)
Acc <- summaryBy(match. + rt ~ SubjNo, data = subset(sense.pop.length), keep.names = T)
sense.pop.length <- subset(sense.pop.length, SubjNo %in% Acc[Acc$match. > 0.9,]$SubjNo)
sense.pop.length <- subset(sense.pop.length, SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo)

# Remove incorrect trials
sense.pop.length <- subset(sense.pop.length, match. == 1)

# Remove outliers by subject (less than 3sd from participant mean -- note that this is not a symmetric exclusion criterion)
sense.pop.length <- ddply(sense.pop.length, .(SubjNo), function(d){ 
	by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
	d = subset(d, rt < by_subj_include[2])
	})
	

# Remove RTs < 200ms
sense.pop.length <- subset(sense.pop.length, rt > 0.2)

# Standardize lenth
sense.pop.length$Length <- (sense.pop.length$Length - mean(sense.pop.length$Length, na.rm = T))/sd(sense.pop.length$Length, na.rm = T)
sense.pop.length.raw <- summary(lmer(rt ~ Length + (1+Length|SubjNo), data = sense.pop.length))
print(sense.pop.length.raw)
print(paste("p value = ", 2*pnorm(-abs(coef(sense.pop.length.raw)[,3]))))


###########################################################################################################################
#
# Graphs

sense.sklar.graph <- summaryBy(rt ~ Condition + SubjNo, data = sense.pop.sklar, keep.names = T)
sense.sklar.graph <- summaryBy(rt ~ Condition, data = sense.sklar.graph, FUN = c(mean,sd))
sense.sklar.graph$SE <- sense.sklar.graph$rt.sd/sqrt(length(unique(sense.pop.sklar$SubjNo)))
sense.sklar.graph$Experiment <- "Experiment 1a"

sense.new.graph <- summaryBy(rt ~ Condition + SubjNo, data = sense.pop.new, keep.names = T)
sense.new.graph <- summaryBy(rt ~ Condition, data = sense.new.graph, FUN = c(mean,sd))
sense.new.graph$SE <- sense.new.graph$rt.sd/sqrt(length(unique(sense.pop.new$SubjNo)))
sense.new.graph$Experiment <- "Experiment 1b"

sense.graph <- rbind(sense.sklar.graph,sense.new.graph)
sense.graph$Cond_Graph <- "Violation"
sense.graph[sense.graph$Condition %in% c("Sklar_control", "Sensible"),]$Cond_Graph <- "Control"
sense.graph$Cond_Graph <- ordered(sense.graph$Cond_Graph, levels = c("Violation", "Control")) 
sense.graph$rt.mean <- sense.graph$rt.mean * 1000
sense.graph$SE <- sense.graph$SE * 1000

dodge <- position_dodge(width=0.9)
qplot(sense.graph$Experiment, sense.graph$rt.mean, geom = "bar", stat = "identity", fill = sense.graph$Cond_Graph, ylab = "Reaction Time (ms)", xlab = "", position = dodge, ylim = c(0,2000)) +  geom_errorbar(aes(ymax = sense.graph$rt.mean + sense.graph$SE, ymin = sense.graph$rt.mean - sense.graph$SE), width=0.25, position = dodge) + labs(fill = "Sentence Type") + theme(axis.text.x = element_text(colour = "black", size = 12))