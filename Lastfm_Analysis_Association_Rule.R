### *** Play counts *** ###
lastfm <- read.csv("C:/DataMining/Data/lastfm.csv")
lastfm[1:19,]
length(lastfm$user) ## 289,955 records in the file
lastfm$user <- factor(lastfm$user)
levels(lastfm$user) ## 15,000 users
levels(lastfm$artist) ## 1,004 artists
library(arules) ## a-rules package for association rules
## Computational environment for mining association rules and
## frequent item sets
## we need to manipulate the data a bit for arules
playlist <- split(x=lastfm[,"artist"],f=lastfm$user) ## split into a list of
users
playlist <- lapply(playlist,unique) ## remove artist duplicates
playlist[1:2]


## the first two listeners (1 and 3) listen to the following bands

playlist <- as(playlist,"transactions")

## view this as a list of "transactions"
## transactions is a data class defined in arules

itemFrequency(playlist)
## lists the support of the 1,004 bands
## number of times band is listed to on the shopping trips of 15,000 users
## computes the rel freq each artist mentioned by the 15,000 users
itemFrequencyPlot(playlist,support=.08,cex.names=1.5)
## plots the item frequencies (only bands with > % support)
## Finally, we build the association rules
## only rules with support > 0.01 and confidence > .50
## so it canít be a super rare band
musicrules <- apriori(playlist,parameter=list(support=.01,confidence=.5))
inspect(musicrules)
## let's filter by lift > 5.
## Among those associations with support > 0.01 and confidence > .50,
## only show those with lift > 5
inspect(subset(musicrules, subset=lift > 5))
## lastly, order by confidence to make it easier to understand
inspect(sort(subset(musicrules, subset=lift > 5), by="confidence"))