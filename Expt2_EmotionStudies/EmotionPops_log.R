library(plyr)
library(lme4)
library(doBy)

emo.pop <- read.csv("all_data_with_w1w2_ratings.csv")

# Make RTs numeric
emo.pop <- subset(emo.pop, rt != "None")

emo.pop$rt <- as.numeric(as.character(emo.pop$rt))
emo.pop$Length <- nchar(as.character(emo.pop$prime),allowNA = T)

# First standardize the MeanAffectivity score
emo.pop$MeanAffectivity <- (emo.pop$MeanAffectivity - mean(emo.pop$MeanAffectivity, na.rm = T))/sd(emo.pop$MeanAffectivity, na.rm = T)

##########################################################################################################################
#
# Sklar Experiment first
emo.pop.sklar <- subset(emo.pop, prime_semantics %in% c("Negative phrase","Neutral phrase"))
emo.pop.sklar$rt <- log(emo.pop.sklar$rt)
# Remove outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be > (!!, see by trial exclusion below) 3sd from group mean [ie we remove extra-fast participants)
Acc <- summaryBy(match. + rt ~ SubjNo, data = subset(emo.pop.sklar), keep.names = T)
emo.pop.sklar <- subset(emo.pop.sklar, SubjNo %in% Acc[Acc$match. > 0.9,]$SubjNo)
emo.pop.sklar <- subset(emo.pop.sklar, SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo)

# Remove incorrect trials
emo.pop.sklar <- subset(emo.pop.sklar, match. == 1)

# Remove RTs +/-3sd from each subject's mean 
emo.pop.sklar <- ddply(emo.pop.sklar, .(SubjNo), function(d){ 
  include = mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
  d = subset(d, rt > include[1] & rt < include[2])
})

# Remove RTs < 200ms
emo.pop.sklar <- subset(emo.pop.sklar, rt > -1.6)

# First standardize the MeanAffectivity score
emo.pop.sklar$MeanAffectivity <- (emo.pop.sklar$MeanAffectivity - mean(emo.pop.sklar$MeanAffectivity, na.rm = T))/sd(emo.pop.sklar$MeanAffectivity, na.rm = T)

# Lin Reg (sklar style)
emo.pop.sklar$log.rt <- log(emo.pop.sklar$rt)
emo.pop.sklar.sum <- summaryBy(rt + log.rt~ prime + MeanAffectivity, data = emo.pop.sklar, keep.names = T)
summary(lm(rt ~ MeanAffectivity, data = emo.pop.sklar.sum))
summary(lm(log.rt ~ MeanAffectivity, data = emo.pop.sklar.sum))


# note that you can calculate BF by estimating the sample SE from Sklar's 
# regression coefficient (0.356) and his t stat 2.523. t = b/se therefore se = b/t = 0.1411019. Therefore SD = SE * sqrt(N = 46) = 0.9495262
# This is done using Z.D.'s BF calculator http://www.lifesci.sussex.ac.uk/home/Zoltan_Dienes/inference/Bayes.htm
# We know the original coef and the t, and we are testing against a 0 effect. If Sklar is right, we should get data
# that falls within the normal distribution around his effect [ie don't use uniform option].
# Note that Sklar's original coef appears to be a Beta, i.e., a standardized coefficient, so first
# NOTE THAT YOU HAVE TO UNCOMMENT OUT THE LINES BELOW TO RUN THIS CODE. 
#
# <-- UNCOMMENT emo.pop.sklar.sum$rt_stand <- (emo.pop.sklar.sum$rt - mean(emo.pop.sklar.sum$rt, na.rm = T))/sd(emo.pop.sklar.sum$rt, na.rm = T)
# <-- UNCOMMENT summary(lm(rt_stand ~ MeanAffectivity, data = emo.pop.sklar.sum))
#
# Sample estimate is -0.03966 and sample SE is 0.02627 [from regression above], Theory M = 0.356, Theory SD = 0.9495262
# BF is therefore 0.08

# lmer (rabag style)
emo.sklar.lmer.raw <- summary(lmer(rt ~ MeanAffectivity + (1+MeanAffectivity|SubjNo)+ (1|prime), data = subset(emo.pop.sklar, prime_semantics %in% c("Negative phrase","Neutral phrase"))))
print(emo.sklar.lmer.raw)
print(paste("p value = ", 2*pnorm(-abs(coef(emo.sklar.lmer.raw)[2,3]))))
emo.sklar.lmer.log <- summary(lmer(log.rt ~ MeanAffectivity + (1+MeanAffectivity|SubjNo)+ (1|prime), data = subset(emo.pop.sklar, prime_semantics %in% c("Negative phrase","Neutral phrase"))))
print(emo.sklar.lmer.log)
print(paste("p value = ", 2*pnorm(-abs(coef(emo.sklar.lmer.log)[2,3]))))

emo.pop.sklar$Length <- (emo.pop.sklar$Length - mean(emo.pop.sklar$Length))/sd(emo.pop.sklar$Length)
summary(glmer(rt ~ MeanAffectivity+Length + (1+MeanAffectivity|SubjNo)+ (1|prime), data = emo.pop.sklar, family = "inverse.gaussian"(link="log")))


##########################################################################################################################
#
# New reversed sentences Experiment next 
emo.pop.new <- subset(emo.pop, prime_semantics %in% c("Negative sentence","Neutral sentence"))
emo.pop.new$rt <- log(emo.pop.new$rt)
# Remove outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be > (!!, see by trial exclusion below) 3sd from group mean [ie we remove extra-fast participants)
Acc <- summaryBy(match. + rt ~ SubjNo, data = subset(emo.pop.new), keep.names = T)
emo.pop.new <- subset(emo.pop.new, SubjNo %in% Acc[Acc$match. > 0.9,]$SubjNo)
emo.pop.new <- subset(emo.pop.new, SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo)

# Remove incorrect trials
emo.pop.new <- subset(emo.pop.new, match. == 1)

emo.pop.new <- ddply(emo.pop.new, .(SubjNo), function(d){ 
  include = mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
  d = subset(d, rt > include[1] & rt < include[2])
})

# Remove RTs < 200ms
emo.pop.new <- subset(emo.pop.new, rt > -1.6)

#  standardize the MeanAffectivity score
emo.pop.new$MeanAffectivity <- (emo.pop.new$MeanAffectivity - mean(emo.pop.new$MeanAffectivity, na.rm = T))/sd(emo.pop.new$MeanAffectivity, na.rm = T)
emo.pop.new$log.rt <- log(emo.pop.new$rt)
emo.pop.new$Length <- (emo.pop.new$Length - mean(emo.pop.new$Length))/sd(emo.pop.new$Length)
emo.pop.new$Cond <- "Violation"
emo.pop.new[emo.pop.new$prime_semantics == "Neutral sentence",]$Cond <- "Control"
emo.pop.new$Cond <- as.factor(emo.pop.new$Cond)
contrasts(emo.pop.new$Cond)[1] <- -1

emo.pop.new.sum <- summaryBy(rt + log.rt ~ prime + MeanAffectivity+Cond, data = emo.pop.new, keep.names = T)
summary(lm(rt ~ MeanAffectivity, data = emo.pop.new.sum))
summary(lm(log.rt ~ MeanAffectivity, data = emo.pop.new.sum))

# note that you can calculate BF by estimating the sample SE from Sklar's 
# regression coefficient (0.356) and his t stat 2.523. t = b/se therefore se = b/t = 0.1411019. Therefore SD = SE * sqrt(N = 46) = 0.9495262
# This is done using Z.D.'s BF calculator http://www.lifesci.sussex.ac.uk/home/Zoltan_Dienes/inference/Bayes.htm
# We know the original coef and the t, and we are testing against a 0 effect. If Sklar is right, we should get data
# that falls within the normal distribution around his effect [ie don't use uniform option].
# Note that Sklar's original coef appears to be a Beta, i.e., a standardized coefficient, so first
# NOTE THAT YOU HAVE TO UNCOMMENT OUT THE LINES BELOW TO RUN THIS CODE. 
#
# <-- UNCOMMENT emo.pop.new.sum$rt_stand <- (emo.pop.new.sum$rt - mean(emo.pop.new.sum$rt, na.rm = T))/sd(emo.pop.new.sum$rt, na.rm = T)
# <-- UNCOMMENT summary(lm(rt_stand ~ MeanAffectivity, data = emo.pop.new.sum))
#
# Sample estimate is -0.01737 and sample SE is 0.03435 [from regression above], Theory M = 0.356, Theory SD = 0.9495262
# BF is therefore 0.08

# lmer (rabag style)
emo.new.lmer.raw <- summary(lmer(rt ~ MeanAffectivity + (1+MeanAffectivity|SubjNo)+ (1+MeanAffectivity|PairID), data = subset(emo.pop.new, prime_semantics %in% c("Negative sentence","Neutral sentence"))))
print(emo.new.lmer.raw)
print(paste("p value = ", 2*pnorm(-abs(coef(emo.new.lmer.raw)[2,3]))))
emo.new.lmer.log <- summary(lmer(log.rt ~ MeanAffectivity + (1+MeanAffectivity|SubjNo)+ (1+MeanAffectivity|PairID), data = subset(emo.pop.new, prime_semantics %in% c("Negative sentence","Neutral sentence"))))
print(emo.new.lmer.log)
print(paste("p value = ", 2*pnorm(-abs(coef(emo.new.lmer.log)[2,3]))))

summary(glmer(rt ~ Cond+Length + (1+Cond|SubjNo)+ (1|prime), data = emo.pop.new, family = "inverse.gaussian"(link="log")))

###########################################################################################################################
#
# Finally, Hebrew Experiment
emo.pop.hebr <- subset(emo.pop, prime_semantics %in% c("Hebrew"))
emo.pop.hebr$rt <- log(emo.pop.hebr$rt)
# Remove outlier subjects by Accuracy and RT (mean acc must be > 0.9, mean rt must be > (!!, see by trial exclusion below) 3sd from group mean [ie we remove extra-fast participants)
Acc <- summaryBy(match. + rt ~ SubjNo, data = subset(emo.pop.hebr), keep.names = T)
emo.pop.hebr <- subset(emo.pop.hebr, SubjNo %in% Acc[Acc$match. > 0.9,]$SubjNo)
emo.pop.hebr <- subset(emo.pop.hebr, SubjNo %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$SubjNo)

# Remove incorrect trials
emo.pop.hebr <- subset(emo.pop.hebr, match. == 1)

emo.pop.hebr <- ddply(emo.pop.hebr, .(SubjNo), function(d){ 
  include = mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
  d = subset(d, rt > include[1] & rt < include[2])
})

# Remove RTs < 200ms
emo.pop.hebr <- subset(emo.pop.hebr, rt > -1.6)

#  standardize the MeanAffectivity score
emo.pop.hebr$MeanAffectivity <- (emo.pop.hebr$MeanAffectivity - mean(emo.pop.hebr$MeanAffectivity, na.rm = T))/sd(emo.pop.hebr$MeanAffectivity, na.rm = T)
emo.pop.hebr$log.rt <- log(emo.pop.hebr$rt)
emo.pop.hebr.sum <- summaryBy(rt + log.rt~ prime + MeanAffectivity + Contrast, data = emo.pop.hebr, keep.names = T)
emo.pop.hebr.sum$Contrast <- as.factor(emo.pop.hebr.sum$Contrast)
contrasts(emo.pop.hebr.sum$Contrast)[1] <- -1
summary(lm(rt ~ MeanAffectivity*Contrast, data = emo.pop.hebr.sum))
summary(lm(log.rt ~ MeanAffectivity*Contrast, data = emo.pop.hebr.sum))



# lmer (rabag style)
emo.pop.hebr$Contrast2 <- as.factor(emo.pop.hebr$Contrast)
contrasts(emo.pop.hebr$Contrast2)[1] <- -1
emo.contr.lmer.raw <- summary(lmer(rt ~ Contrast2*MeanAffectivity + (1+Contrast2*MeanAffectivity|SubjNo)+ (1+Contrast2|prime), data = subset(emo.pop.hebr, prime_semantics %in% c("Hebrew"))))
print(emo.contr.lmer.raw)
print(paste("p value = ", 2*pnorm(-abs(coef(emo.contr.lmer.raw)[,3]))))
emo.contr.lmer.log <- summary(lmer(log.rt ~ Contrast2*MeanAffectivity + (1+Contrast2*MeanAffectivity|SubjNo)+ (1+Contrast2|prime), data = subset(emo.pop.hebr, prime_semantics %in% c("Hebrew"))))
print(emo.contr.lmer.log)
print(paste("p value = ", 2*pnorm(-abs(coef(emo.contr.lmer.log)[,3]))))


##########################################################################################################################



# We can also check if this depends on perceptual rating


# Finally -- a quick test if English is perceived faster than Hebrew (following Jiang et al 07, 
emo.pop$rt <- log(emo.pop$rt)
emo.pop$Lang <- "English"
emo.pop[emo.pop$prime_semantics %in% c("Hebrew"),]$Lang <- "Hebrew"
lang <- summaryBy(rt  ~ Lang + SubjNo, data = subset(emo.pop, Contrast == 50),keep.names = T)
summaryBy(rt ~ Lang , data = subset(emo.pop, Contrast == 50),keep.names = T, FUN = c(mean,sd))
emo.pop.lang <- summary(lmer(rt ~ Lang +Length+ (1+Lang+Length|SubjNo), data = subset(emo.pop, Contrast == 50)))
print(summary(emo.pop.lang))
print(paste("p value = ", 2*pnorm(-abs(coef(emo.pop.lang)[,3]))))


##########################################################################################################################
#
# Graphs

emo.pop.hebr.sum <- summaryBy(rt + log.rt ~ prime + MeanAffectivity + Contrast, data = emo.pop.hebr, keep.names = T)
emo.pop.hebr.sum$Experiment <- "Experiment 2c"

emo.pop.sklar.sum$Experiment <- "Experiment 2a"
emo.pop.sklar.sum$Contrast <- 50
emo.pop.new.sum$Experiment <- "Experiment 2b"
emo.pop.new.sum$Contrast <- 50

graph <- rbind(emo.pop.sklar.sum,emo.pop.new.sum,emo.pop.hebr.sum)

graph <- na.omit(graph)
graph$Experiment <- ordered(graph$Experiment, levels = c("Experiment 2a", "Experiment 2b", "Experiment 2c"))
graph$Contrast <- ordered(graph$Contrast, levels = c("50","80"), labels = c("50%","80%"))


graph$rt <- graph$rt * 1000

ggplot(graph, aes(x=MeanAffectivity, y=rt, shape = Contrast)) +
  geom_point() +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE) + facet_grid(.~Experiment) + labs(y = "Reaction Time (ms)", x = "Standardized Valence Rating")


sense.graph <- summaryBy(rt ~ Cond + SubjNo, data = emo.pop.new, keep.names = T)
sense.graph <- summaryBy(rt ~ Cond, data = sense.graph, FUN = c(mean,sd))
sense.graph$SE <- sense.graph$rt.sd/sqrt(length(unique(emo.pop.new$SubjNo)))
sense.graph$rt.mean <- sense.graph$rt.mean * 1000
sense.graph$SE <- sense.graph$SE * 1000
sense.graph$Cond <- ordered(sense.graph$Cond, levels =c("Violation","Control"))
dodge <- position_dodge(width=0.9)
ggplot(sense.graph, aes(Cond,rt.mean, fill = Cond)) +
  geom_bar(stat = "identity",  position = dodge) +
  geom_errorbar(aes(ymax = sense.graph$rt.mean +
                      sense.graph$SE, ymin = sense.graph$rt.mean - sense.graph$SE), width=0.25, position = dodge) +
  labs(fill = "Sentence Type") + 
  theme(axis.text.x = element_text(colour = "black", size = 12)) +
  ylab("Reaction Time (ms)") +
  xlab("") + 
  ylim(c(0,2000))
