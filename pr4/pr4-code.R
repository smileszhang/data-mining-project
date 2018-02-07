dat1<-df4

dat1$PseudoID <- NULL
dat1$Name <- NULL
dat1$Date <- NULL
dat1$Schedule <- NULL ### we have fulltime
dat1$NSFTP <- NULL ### ???
dat1$Education <- as.numeric(as.character(dat1$Education))
dat1$Age <- as.numeric(substr(dat1$Age, start = 1, stop = 2))
dat1$LOS <- as.numeric(sub("-.*|\\+|<", "", dat1$LOS))
dat1$SupervisoryStatus <- as.numeric(as.character(dat1$SupervisoryStatus))

dat3 <- dat1[,c("Age", "LOS", "Education", "SupervisoryStatus", "Pay")]
dim(dat3)
#remove NA values
take <- complete.cases(dat3)
dat3 <- dat3[take,]
dim(dat3)
take2<-complete.cases(dat1)
dat1<-dat1[take2,]

#fix supervisorystatus
dat3$SupervisoryStatus <- 8-dat3$SupervisoryStatus
# cluster using kmeans
dat3 <- scale(dat3)
summary(dat3)
sample_ID <- sample(1:nrow(dat3), size = 10000)
dat4 <- dat3[sample_ID, ]
dat<-dat1[sample_ID,]
summary(dat4)
set.seed(1000)
k <- 3
km <- kmeans(dat4, centers = k)
str(km)
km$centers
install.packages("GGally")
library(GGally)

ggparcoord(cbind(data.frame(km$centers), data.frame(id = as.character(1:k))), columns = 1:ncol(km$centers), groupColumn = 'id')

#cluster using hiea

d <- dist(dat4)
hc <- hclust(d, method="complete")
plot(hc)
rect.hclust(hc, k=3)
plot(as.dendrogram(hc), leaflab="none") # plot dendrogram without leaf labels


#find optimal clusters-wss
ks <- 2:10
WSS <- sapply(ks, FUN=function(k) {
  kmeans(dat4, centers=k, nstart=5)$tot.withinss
})
plot(ks, WSS, type="l")
abline(v=4, col="red", lty=2)

#find optimal clusters-sil
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(dat4, centers=k, nstart=5)$cluster)$avg.silwidth
})
plot(ks, ASW, type="l")

ks[which.max(ASW)]

abline(v=ks[which.max(ASW)], col="red", lty=2)
#DUNN index
DI <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(dat4, centers=k, nstart=5)$cluster)$dunn
})
plot(ks, DI, type="l")
ks[which.max(DI)]
abline(v=ks[which.max(DI)], col="red", lty=2)
#gap
k <- clusGap(dat4, FUN = kmeans,  nstart = 10, K.max = 10)
k
plot(k)

#quality
fpc::cluster.stats(d, km$cluster)

sapply(list(
  km=km$cluster,
  hc_compl=cluster_complete,
  m=m$classification),
  FUN=function(x)
    fpc::cluster.stats(d, x))[c("within.cluster.ss","avg.silwidth"),]

windows()
plot(silhouette(km$cluster, d))


pimage(d)
pimage(d, order=order(km$cluster))

#observation 
dat_cluster_1 <- dat[km$cluster == 1,]
dat_cluster_2 <- dat[km$cluster == 2,]
dat_cluster_3 <- dat[km$cluster == 3,]
dat_cluster_4 <- dat[km$cluster == 4,]

summary(dat_cluster_1[,c("Age", "Education", "LOS", "Agency", "Pay")])
summary(dat_cluster_2[,c("Age", "Education", "LOS", "Agency", "Pay")])
summary(dat_cluster_3[,c("Age", "Education", "LOS", "Agency", "Pay")])
summary(dat_cluster_4[,c("Age", "Education", "LOS", "Agency", "Pay")])

head(sort(table(dat_cluster_1$Agency)/nrow(dat_cluster_1), decreasing = TRUE))
head(sort(table(dat_cluster_2$Agency)/nrow(dat_cluster_1), decreasing = TRUE))

lift_cluster_1 <- sort((table(dat_cluster_1$Agency)/nrow(dat_cluster_1)) /
                         (table(dat$Agency)/nrow(dat)), decreasing = TRUE)
head(lift_cluster_1, n = 10)

barplot(rev(head(lift_cluster_1, n=20)), horiz = TRUE, las = 2, xlab = "Lift")

lift_cluster_2 <- sort((table(dat_cluster_2$Agency)/nrow(dat_cluster_2)) /
                         (table(dat$Agency)/nrow(dat)), decreasing = TRUE)
head(lift_cluster_2, n = 10)
barplot(rev(head(lift_cluster_2, n=20)), horiz = TRUE, las = 2, xlab = "Lift")

lift_cluster_3 <- sort((table(dat_cluster_3$Agency)/nrow(dat_cluster_3)) /
                         (table(dat$Agency)/nrow(dat)), decreasing = TRUE)
head(lift_cluster_3, n = 10)
barplot(rev(head(lift_cluster_3, n=20)), horiz = TRUE, las = 2, xlab = "Lift")

lift_cluster_4 <- sort((table(dat_cluster_4$Agency)/nrow(dat_cluster_4)) /
                         (table(dat$Agency)/nrow(dat)), decreasing = TRUE)
head(lift_cluster_4, n = 10)
barplot(rev(head(lift_cluster_4, n=20)), horiz = TRUE, las = 2, xlab = "Lift")
