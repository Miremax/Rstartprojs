source("https://bioconductor.org/biocLite.R")
biocLite("edgeR")
library(edgeR)

setwd('F:/YandexDisk/Образование/HSE/Term1/R programming/HomeWork 8')

genExp <- read.table('hw6.counts.txt', check.names = F)
meta <-  strsplit(colnames(genExp),'_',fixed = TRUE)
meta <- data.frame(Ti=sapply(meta, '[',1), Age=as.numeric(sapply(meta, '[',2)))
A1 <- meta$Age**0.25
A2 <- meta$Age**0.5
Ti <- as.factor(meta$Ti)

#Из лекции
E = DGEList(genExp)
E = calcNormFactors(E, method = 'RLE')
design = model.matrix(~ A1 + A2 + Ti:A1 + Ti:A2)
E = estimateGLMCommonDisp(E,design)
E = estimateGLMTrendedDisp(E,design)
E = estimateGLMTagwiseDisp(E,design)
strict.disp = pmax(E$tagwise.dispersion, E$trended.dispersion, E$common.dispersion)
glm = glmFit(E, design, dispersion = strict.disp)
glm

pv.A1 = glmLRT(glm,coef = 2)
pv.A2 = glmLRT(glm,coef = 3)
pv.TiA1 = glmLRT(glm,coef = 4)
pv.TiA2 = glmLRT(glm,coef = 5)

#С минимальными pValue
pValue <- (cbind(pv.A1$table$PValue, pv.A2$table$PValue, pv.TiA1$table$PValue, pv.TiA2$table$PValue)) 
inAllFactorsMinpValue <- apply(pValue, 1, function(x) min(x))
MinpValueGenes <- genExp[order(inAllFactorsMinpValue),]
MinpValue2000Genes <- MinpValueGenes[1:2000,]

#cо значимой экспрессией
colN <- apply(MinpValue2000Genes, 1, function(x) sum(x) > 10*ncol(genExp))
sum(colN) #значимо экспрессируются из 2000
N <- MinpValue2000Genes[colN,]

# нормировка  на размер библиотек
N <- as.data.frame(apply(N, 2, function(x) x/sum(x)))

#приведение к стандартному
NS <- as.data.frame(t(apply(N, 1, function(x) (x-mean(x))/sd(x))))

#На 6 кластеров
Dist1 <- as.dist(1 - cor(t(NS), method = 'spearman'))
ClusterModel <- hclust(Dist1, method = 'complete')
  plot(ClusterModel)

Mod6Clus <- cutree(ClusterModel, k = 6)
  plot(Mod6Clus)


meansOfClusters <-  numeric(length(unique(Mod6Clus)))
#Средняя (нормированная стандартизованнная) экспрессия генов по кластерам
for (i in unique(Mod6Clus)){
  meansOfClusters[i] <- mean(sapply(NS[as.vector(Mod6Clus)==i,] , mean))
}
  plot(meansOfClusters)
  
  
  par(mfrow=c(2,3))
  at = c(12,14,20,30,50,80)
  for(c in 1:max(Mod6Clus)){
    t = apply(NS[Mod6Clus==c,],2,mean)
    plot(meta$Age^0.25,t,col=ifelse(meta$Ti=='brain','red','blue'),pch=19,main=paste0('c',c,' (',sum(Mod6Clus==c),')'),xaxt='n',xlab='Age (days)',ylab='mean norm expression')
    axis(1,at^0.25,at)
  }
  