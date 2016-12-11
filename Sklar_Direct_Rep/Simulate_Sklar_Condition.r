library(plyr)
library(doBy)
library(retimes)

mu <- 0.6566 
sigma <- 0.0974 
tau <- 2.6765
subj_range<- data.frame(subj = seq(10,100,by = 2),rt.p.noexcl = NA, log_rt.p.noexcl = NA, rt.p.excl1 = NA,rt.p.excl2 = NA,rt.p.excl3 = NA,rt.p.excl4 = NA,rt.p.full = NA)

for (l in subj_range$subj){
	print(l)
no.sim <- 10000
no.subj <- l
no.conditions <- 2
no.trials <- 24
sim = rep(1:no.sim, each = no.subj*no.conditions*no.trials)
subj = rep(1:no.subj,each = no.conditions*no.trials)
cond = rep(rep(1:no.conditions, each =no.trials), no.subj)

data <- data.frame(sim = sim, subj = subj, cond = cond, rt = NA)
pval.noexcludes <- data.frame(sim = 1:no.sim,pval_rt = NA,pval_log_rt=NA)
pval.sklarvalues  <- data.frame(sim = 1:no.sim,pval_rt_full = NA,pval_rt_excl1 = NA,pval_rt_excl2 = NA,pval_rt_excl3 = NA,pval_rt_excl4 = NA,pval_log_rt=NA)

# for (k in 1:no.sim){
	# for (i in 1:no.subj){
		# mu.subj <- jitter(mu)
		# sigma.subj <- jitter(sigma)
		# tau.subj <- jitter(tau)
		
		# for (j in 1:no.conditions){
			# data[data$sim == k & data$subj == i & data$cond == j,]$rt <- rexgauss(no.trials, mu.subj,sigma.subj,tau.subj)
		# }
	# }
# }

# for (i in 1:no.sim){
# # Rsemve RTs +/-3sd from each subject's mean 
# data_subset <- ddply(data[data$sim ==i,], .(subj), function(d){ 
  # include = mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
  # d = subset(d, rt > include[1] & rt < include[2])
# })
# # [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
# data_subset <- ddply(data_subset, .(cond), function(d){ 
  # by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
  # d = subset(d, rt < by_subj_include[2])
# })
# # Rsemve RTs < 200ms & > 10
# data_subset <- subset(data_subset, rt > 0.2)
# data_subset <- subset(data_subset, rt <= 10)
# data_subset$cond_comb <- 1
# data_subset[data_subset$cond %in% c(2,3),]$cond_comb <- 2
# data_subset$log_rt <- log(data_subset$rt)
# data_subset_test <- summaryBy(rt +log_rt~ subj + cond_comb, data = data_subset, keep.names = T)
# pval[pval$sim == i,]$pval_rt <- t.test(rt ~ cond_comb, data = data_subset_test, paired = T)$p.value
# pval[pval$sim == i,]$pval_log_rt <- t.test(log_rt ~ cond_comb, data = data_subset_test, paired = T)$p.value

# }

# pval.sklarvalues <- pval



#######
# Without Sklar exclusions
sim = rep(1:no.sim, each = no.subj*no.conditions*no.trials)
subj = rep(1:no.subj,each = no.conditions*no.trials)
cond = rep(rep(1:no.conditions, each =no.trials), no.subj)

pval <- data.frame(sim = 1:no.sim,pval_rt = NA,pval_log_rt = NA)

# for (k in 1:no.sim){
	# if(k %in% c(200,400,600,800)){print(i)}
	# for (i in 1:no.subj){
		# mu.subj <- jitter(mu)
		# sigma.subj <- jitter(sigma)
		# tau.subj <- jitter(tau)
		
		# for (j in 1:no.conditions){
			# data[data$sim == k & data$subj == i & data$cond == j,]$rt <- rexgauss(no.trials, mu.subj,sigma.subj,tau.subj)
		# }
	# }
# }

for (i in 1:no.sim){
	if(i %in% c(20000,40000,60000,80000)){print(i)}
	data <- data.frame(subj = subj, cond = cond, rt = NA)

	for (k in 1:no.subj){
		mu.subj <- jitter(mu)
		sigma.subj <- jitter(sigma)
		tau.subj <- jitter(tau)
		
		for (j in 1:no.conditions){
			data[data$subj == k & data$cond == j,]$rt <- rexgauss(no.trials, mu.subj,sigma.subj,tau.subj)
		}
	}

	data_subset <- data
	data_subset$cond_comb <- 1
	data_subset[data_subset$cond %in% c(2,3),]$cond_comb <- 2
	data_subset$log_rt <- log(data_subset$rt)
	data_subset_test <- summaryBy(rt +log_rt~ subj + cond_comb, data = data_subset, keep.names = T)
	pval.noexcludes[pval.noexcludes$sim == i,]$pval_rt <- t.test(rt ~ cond_comb, data = data_subset_test, paired = T)$p.value
	pval.noexcludes[pval.noexcludes$sim == i,]$pval_log_rt <- t.test(log_rt ~ cond_comb, data = data_subset_test, paired = T)$p.value
	
	# Exclude 3sd from 
	Acc <- summaryBy( rt ~ subj, data = data_subset, keep.names = T)
	data_subset <- data_subset[data_subset$subj %in% Acc[Acc$rt < (mean(Acc$rt) + (3*sd(Acc$rt))),]$subj,]
	data_subset_test <- summaryBy(rt ~ subj + cond_comb, data = data_subset, keep.names = T)
	pval.sklarvalues[pval.sklarvalues $sim == i,]$pval_rt_excl1 <- t.test(rt ~ cond_comb, data = data_subset_test, paired = T)$p.value

	# Rsemve RTs +/-3sd from each subject's mean 
	data_subset <- ddply(data_subset, .(subj,cond_comb), function(d){ 
	  include = mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
	  d = subset(d, rt > include[1] & rt < include[2])
	})
	data_subset_test <- summaryBy(rt ~ subj + cond_comb, data = data_subset, keep.names = T)
	pval.sklarvalues[pval.sklarvalues $sim == i,]$pval_rt_excl2 <- t.test(rt ~ cond_comb, data = data_subset_test, paired = T)$p.value
	# [for Expt 1 only] Remove outliers by condition (less than 3sd from condition mean -- note that this is not a symmetric exclusion criterion)
	data_subset <- ddply(data_subset, .(cond), function(d){ 
	  by_subj_include <- mean(d$rt, na.rm = T) + 3*c(-1,1)*sd(d$rt,na.rm = T)
	  d = subset(d, rt < by_subj_include[2])
	})
	data_subset_test <- summaryBy(rt ~ subj + cond_comb, data = data_subset, keep.names = T)
	pval.sklarvalues[pval.sklarvalues $sim == i,]$pval_rt_excl3 <- t.test(rt ~ cond_comb, data = data_subset_test, paired = T)$p.value

	# Rsemve RTs < 200ms & > 10
	data_subset <- subset(data_subset, rt > 0.2)
	data_subset_test <- summaryBy(rt ~ subj + cond_comb, data = data_subset, keep.names = T)
	pval.sklarvalues[pval.sklarvalues $sim == i,]$pval_rt_excl4 <- t.test(rt ~ cond_comb, data = data_subset_test, paired = T)$p.value
	data_subset <- subset(data_subset, rt <= 10)

	data_subset$cond_comb <- 1
	data_subset[data_subset$cond %in% c(2,3),]$cond_comb <- 2
	data_subset$log_rt <- log(data_subset$rt)
	data_subset_test <- summaryBy(rt +log_rt~ subj + cond_comb, data = data_subset, keep.names = T)
	pval.sklarvalues[pval.sklarvalues $sim == i,]$pval_rt_full <- t.test(rt ~ cond_comb, data = data_subset_test, paired = T)$p.value
	pval.sklarvalues[pval.sklarvalues $sim == i,]$pval_log_rt <- t.test(log_rt ~ cond_comb, data = data_subset_test, paired = T)$p.value


}

subj_range[subj_range$subj == l,]$rt.p.noexcl <- length(pval.noexcludes[pval.noexcludes$pval_rt<=0.05,]$pval_rt)/length(pval.noexcludes$pval_rt)
subj_range[subj_range$subj == l,]$rt.p.excl1 <- length(pval.sklarvalues[pval.sklarvalues$pval_rt_excl1 <=0.05,]$pval_rt_excl1)/length(pval.sklarvalues$pval_rt_excl1)
subj_range[subj_range$subj == l,]$rt.p.excl2 <- length(pval.sklarvalues[pval.sklarvalues$pval_rt_excl2 <=0.05,]$pval_rt_excl2)/length(pval.sklarvalues$pval_rt_excl2)
subj_range[subj_range$subj == l,]$rt.p.excl3 <- length(pval.sklarvalues[pval.sklarvalues$pval_rt_excl3<=0.05,]$pval_rt_excl3)/length(pval.sklarvalues$pval_rt_excl3)
subj_range[subj_range$subj == l,]$rt.p.excl4 <- length(pval.sklarvalues[pval.sklarvalues$pval_rt_excl4 <=0.05,]$pval_rt_excl4)/length(pval.sklarvalues$pval_rt_excl4)
subj_range[subj_range$subj == l,]$rt.p.full <- length(pval.sklarvalues[pval.sklarvalues$pval_rt_full <=0.05,]$pval_rt_full)/length(pval.sklarvalues$pval_rt_full)
}