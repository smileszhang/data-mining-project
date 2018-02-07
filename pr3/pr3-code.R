dat1<-df2
dat2<-df4

dat1$PseudoID <- NULL
dat1$Name <- NULL
dat1$Date <- NULL
dat1$Agency <- NULL ### we have AgencyName
dat1$Schedule <- NULL ### we have fulltime
dat1$NSFTP <- NULL ### ???
dat1$Education <- as.numeric(as.character(dat1$Education))
dat1$Age <- as.numeric(substr(dat1$Age, start = 1, stop = 2))
dat1$LOS <- as.numeric(sub("-.*|\\+|<", "", dat1$LOS))

dat2$PseudoID <- NULL
dat2$Name <- NULL
dat2$Date <- NULL
dat2$Agency <- NULL ### we have AgencyName
dat2$Schedule <- NULL ### we have fulltime
dat2$NSFTP <- NULL ### ???
dat2$Education <- as.numeric(as.character(dat2$Education))
dat2$Age <- as.numeric(substr(dat2$Age, start = 1, stop = 2))
dat2$LOS <- as.numeric(sub("-.*|\\+|<", "", dat2$LOS))


for(i in which(sapply(dat1, FUN = function(i) is.numeric(i))))
  dat1[[i]] <- discretize(dat1[[i]], method = "frequency")

for(i in which(sapply(dat2, FUN = function(i) is.numeric(i))))
  dat2[[i]] <- discretize(dat2[[i]], method = "frequency")

save(dat1, file = "2005_03_discretized.rda")
save(dat2, file = "2013_03_discretized.rda")

summary(dat1)
trans1 <- as(dat1, "transactions")
trans2 <- as(dat2, "transactions")

summary(trans1)
itemFrequencyPlot(trans1, topN = 40)
itemFrequencyPlot(trans2, topN = 40)

#rules1
rules1 <- apriori(trans1, parameter = list(supp = .01, conf = .8))

rulsummary(rules1)
inspect(head(rules1, by = "lift"))
plot(rules1, engine = "html")
plot(rules1, method="grouped")
#plot(rules1, method="grouped", interactive=TRUE)

plot(rules1, method = "graph", engine = "html")
barplot(table(size(rules1)), xlab="itemset size", ylab="count")

#concise maximum/closed
s

#rules2
rules2 <- apriori(trans2, parameter = list(supp = .01, conf = .8))

summary(rules2)
inspect(head(rules2, by = "lift"))
plot(rules2, engine = "html")

plot(rules2, method = "graph", engine = "html")

barplot(table(size(rules2)), xlab="itemset size", ylab="count")

#concise closed/maximum
rules22 <- apriori(trans2, parameter=list(target="frequent", support=0.01))

rules2_max<-rules22[is.maximal(rules22)]
inspect(head(sort(rules2_max, by="support")))
rules2_closed <- rules22[is.closed(rules22)]
inspect(head(sort(rules2_closed,by="support")))

barplot(c(
  frequent=length(rules22),
  closed=length(rules2_closed),
  maximal=length(rules2_max)
), ylab="count", xlab="itemsets")


#how many rules in trans
m <- match(rules1, rules2)
sum(!is.na(m))/min(length(rules1),length(rules2))

r <- sample(rules1, 100)
q <- interestMeasure(r, measure = c("supp", "confidence", "lift"),
                     transactions = trans2, reuse = FALSE)
diff <- (quality(r)[,-4] - q)/quality(r)[,-4]
diff
inspect(r[which(diff$supp > 0.2 & diff$supp!=1)])
inspect(r[which(diff$supp < -0.1)])
inspect(r[which(diff$lift > 0.1)])
inspect(r[which(diff$lift < -0.1)])
