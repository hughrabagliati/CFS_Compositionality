library(doBy)

emo.stims <- read.csv("all_data_with_w1w2_ratings.csv")
sklar.emo.stims <- subset(emo.stims, prime_semantics %in% c("Negative phrase","Neutral phrase"))
sklar.emo.stims <- summaryBy(MeanAffectivity + W1.score + W2.score ~ Condition + prime + prime_semantics, data = sklar.emo.stims, keep.names = T,na.rm = T)

mean(c(sklar.emo.stims$W1.score,sklar.emo.stims$W2.score))
sd(c(sklar.emo.stims$W1.score,sklar.emo.stims$W2.score))


new.emo.stims <- subset(emo.stims, prime_semantics %in% c("Negative sentence","Neutral sentence"))
new.emo.stims <- summaryBy(MeanAffectivity + W1.score + W2.score ~ Condition + prime + prime_semantics, data = new.emo.stims, keep.names = T,na.rm = T)

mean(c(new.emo.stims$W1.score,new.emo.stims$W2.score))
sd(c(new.emo.stims$W1.score,new.emo.stims$W2.score))