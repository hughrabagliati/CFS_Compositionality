library(doBy)

emo.stims <- read.csv("all_data_with_w1w2_ratings.csv")
emo.stims <- subset(emo.stims, prime_semantics %in% c("Negative phrase","Neutral phrase"))
emo.stims <- summaryBy(MeanAffectivity + W1.score + W2.score ~ Condition + prime + prime_semantics, data = emo.stims, keep.names = T,na.rm = T)