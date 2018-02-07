dat5=dat1[take,]
agency_data <-
  aggregate(cbind(Age, Education, Grade, LOS, Pay, SupervisoryStatus) ~ Agency, data = dat5, FUN = median)
head(agency_data)
agency_size <- as.data.frame(table(dat5$Agency))
colnames(agency_size) <- c("Agency", "size")
head(agency_size)
agency_data <- merge(agency_data, agency_size)
head(agency_data)

rownames(agency_data) <- agency_data$Agency
agency_data <- agency_data[,-1]

d <- dist(scale(agency_data))
cl <- hclust(d)
windows()
plot(cl)
rect.hclust(cl, k=2)


agency_large <- subset(agency_data, subset = size > 5000)

d <- dist(scale(agency_large))
cl <- hclust(d)
plot(cl)

d <- dist(scale(agency_large[,colnames(agency_large) != "size"]))
cl <- hclust(d)
windows()
plot(cl)
rect.hclust(cl, k=4)

cluster_complete <- cutree(cl, k=2)

#single-cluster

cl <- hclust(d, method="single")
plot(cl)
rect.hclust(cl, k=2)

cluster_single<-cutree(cl,k=2)
#kmeans
km1 <- kmeans(agency_data, centers=2, nstart=10)


#quality
fpc::cluster.stats(d,cluster_complete)
fpc::cluster.stats(d,cluster_single)

windows()
plot(silhouette(cluster_complete, d))

sapply(list(
  km1$cluster,
  hc_compl=cluster_complete),
  FUN=function(x)
    fpc::cluster.stats(d, x))[c("within.cluster.ss","avg.silwidth"),]




set.seed(1234)
ks <- 2:10

#ASW
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d,cutree(cl, k=k))$avg.silwidth
})
windows()
plot(ks, ASW, type="l")
ks[which.max(ASW)]
abline(v=ks[which.max(ASW)], col="red", lty=2)

#DUNN
DI <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d,cutree(cl, k=k))$dunn
})
plot(ks, DI, type="l")
ks[which.max(DI)]
abline(v=ks[which.max(DI)], col="red", lty=2)

#PCA
pc <- prcomp(scale(agency_data))
biplot(pc, col = c("grey", "red"))

#distance
pimage(d, order=order(cluster_complete))
