d = data.frame(Subject = rep(NA, times = length(unique(sense.pop.new$SubjNo))*2), Sensible = NA, Mu = NA, Sigma = NA, Tau = NA)
index = 1
for (k in unique(sense.pop.new$Condition)){
	for (j in unique(subset(sense.pop.new, Condition == k)$SubjNo)){
			a = timefit(subset(sense.pop.new, Condition == k & SubjNo == j )$rt)
			
			d$Subject[index] <- j
			d$Condition[index] <- k
			d$Mu[index] <- a@par[1]
			d$Sigma[index] <- a@par[2]
			d$Tau[index] <- a@par[3]
			index <- index + 1

			}
		}
	
re.summary <- summaryBy(Mu + Sigma+ Tau ~ Condition, data = d, FUN = c(mean))

