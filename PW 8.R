genExp <- read.table('hw6.counts.txt', check.names = F)

colN <- apply(genExp, 1, function(x) sum(x) > 10*ncol(genExp))
#apply(genExp, 1, function(x) sum(x) > 10*ncol(genExp))
N <- genExp[colN,]

# нормировка  на размер библиотек
N1 <- as.data.frame(apply(N, 2, function(x) x/sum(x)))

#приведение к стандартному
NS <- (apply(N, 1, function(x) (x-mean(x))/sd(x)))

Dist1 <- dist(NS, method = 'euclidean')
ClusterModel <- hclust(Dist1, method = 'complete')
plot(ClusterModel)

Mod2Clus <- cutree(ClusterModel, k = 2)
    plot(Mod2Clus)
Mod4Clus <- cutree(ClusterModel, k = 4)
    plot(Mod4Clus)
Mod6Clus <- cutree(ClusterModel, k = 6)
    plot(Mod6Clus)

    
meta <-  strsplit(colnames(genExp),'_',fixed = TRUE)
meta <- data.frame(Tis=sapply(meta, '[',1), Age=as.numeric(sapply(meta, '[',2)))

#Ткани    
chisq.test(table(Mod2Clus, meta$Ti))$p.value #нет взаимодействия

#Возраста
NLev <- length(unique(meta$Age))
ModNLevClus <- cutree(ClusterModel, k = NLev)
    plot(ModNLevClus)
chisq.test(table(ModNLevClus, meta$Age))$p.value

#MDS
options(stringsAsFactors = FALSE)
d = read.table("hw6.counts.txt",check.names = FALSE)
d = as.matrix(d)
d = d[apply(d,1,mean)>=10,]
d = sweep(d,2,apply(d,2,sum),'/')
#3
d1=sweep(d,1,apply(d,1,mean))
d1=sweep(d1,1,apply(d,1,sd),'/')

mds = cmdscale(1-cor(d))

meta = strsplit(colnames(d),'_',fixed = TRUE)
meta = data.frame(
  tissue=sapply(meta, '[',1),
  age=as.numeric(sapply(meta, '[',2)))
meta$cex = log(meta$age)
meta$cex = meta$cex-min(meta$cex)
meta$cex = 0.5 + meta$cex/max(meta$cex)*2
meta$col = NA
f = meta$tissue == 'brain'
meta$col[f] = rgb(2.5,2.5-meta$cex[f],2.5-meta$cex[f],maxColorValue = 2.5)
f = meta$tissue == 'liver'
meta$col[f] = rgb(2.5-meta$cex[f],2.5,2.5-meta$cex[f],maxColorValue = 2.5)



mds = cmdscale(1-cor(d1))
plot(mds,cex=meta$cex,col=meta$col,pch=Mod2Clus*2)







meta = strsplit(colnames(NS),'_',fixed = TRUE)
meta = data.frame(
  tissue=sapply(meta, '[',1),
  age=as.numeric(sapply(meta, '[',2)))

meta$cex = log(meta$age)
meta$cex = meta$cex-min(meta$cex)
meta$cex = 0.5 + meta$cex/max(meta$cex)*2
meta$col = NA
f = meta$tissue == 'brain'
meta$col[f] = rgb(2.5,2.5-meta$cex[f],2.5-meta$cex[f],maxColorValue = 2.5)
f = meta$tissue == 'liver'
meta$col[f] = rgb(2.5-meta$cex[f],2.5,2.5-meta$cex[f],maxColorValue = 2.5)

meta$pch = as.numeric(c4)

plot(mds,cex=meta$cex,col=meta$col,pch=meta$pch)









meta$col = NA
f = meta$Ti == 'brain'
meta$col[f] = rgb(2.5,2.5-meta$cex[f],2.5-meta$cex[f],maxColorValue = 2.5)
f = meta$Ti == 'liver'
meta$col[f] = rgb(2.5-meta$cex[f],2.5,2.5-meta$cex[f],maxColorValue = 2.5)
meta$pch = c(1:113)
meta$pch = ifelse(meta$Ti=='brain',19,10)
cr = cor(normstand,use = 'pair',method = 'p')
mds = cmdscale(1-cr,k=4)

plot(mds[,1], mds[,2], main='MDS', type = 'p',
     col =rgb( 1-colPCA1, 
               1-colPCA, 
               1:k1/1:k1-1), 
     cex = 30*brAge/sum(brAge), lwd = 3, pch = pchPCA)
lines(mds[,3], mds[,4], main='MDS', type = 'p', 
      col = rgb( 1-colPCA1, 
                 1-1:k1/1:k1, 
                 colPCA), 
      cex = 30*brAge/sum(brAge), lwd = 1, pch = pchPCA+5)
