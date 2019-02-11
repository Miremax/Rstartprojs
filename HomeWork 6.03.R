#genExp <- read.table('counts5.txt')
setwd('F:/YandexDisk/Образование/HSE/Term1/R programming/HomeWork 8')
genExp <- read.table('hw6.counts.txt')

# удаление с малой экспрессией 
colN <- apply(genExp, 1, function(x) sum(x) > 10*ncol(genExp))
nExp <- genExp[colN,]

# нормировка  на размер библиотек
norm <- as.data.frame(apply(nExp, 2, function(x) x/sum(x)))

#приведение к стандартному
normstand <- t(apply(norm, 1, function(x) (x-mean(x))/sd(x)))

      #проверка, что sd=1, mean=0
sum(apply(normstand, 1, function(x) (mean(x) < 0.001 && mean(x) > -0.001)), na.rm = T)
sum(apply(normstand, 1, function(x) (sd(x) < 1.001 && sd(x) > 0.999)), na.rm = T)
      
      #удаление cтрок без вариативности
colN1 <- apply(normstand, 1, function(x) (sd(x) < 1.001 && sd(x) > 0.999))
normstand <- normstand[colN1,]

#PCA
pcaObj <- prcomp(t(normstand))

#График
k1 <- length( genExp[1,])
#выделим ткани
brNum <- grep("brain", names(genExp))
livNum <- grep("liver", names(genExp))

#выделим возраста
b2 <- unlist(strsplit(names(genExp)[brNum], '_'))
brAge <- b2[-grep("brain", b2)]
b2 <- unlist(strsplit(names(genExp)[-brNum], '_'))
livAge <- b2[-grep("liver", b2)]

#Зачем возраста с двумя точками?! Что вообще этим закодировано? 
#Будем считать возрастом первую цифру.
brAge <- unlist(lapply(brAge, function(x) as.numeric(substr(x,1,2))))
livAge <- unlist(lapply(livAge, function(x) as.numeric(substr(x,1,2))))

#Цвета
colPCA <- c(1:k1/1:k1)
colPCA1 <- colPCA
colPCA[brNum] <- 12*brAge/sum(brAge)
colPCA1[livNum] <- 12*livAge/sum(livAge)
#точки
pchPCA <- c(1:k1)
pchPCA[brNum] <- 4
pchPCA[livNum] <- 3


plot(pcaObj$x[,2]~pcaObj$x[,1], main='PCA', type = 'p', 
     col = rgb( 1-colPCA1, 
                1-colPCA, 
                1:k1/1:k1-1), 
     cex = 30*brAge/sum(brAge), lwd = 2, pch = pchPCA+6) 
# PCA [1]~[2]

lines(pcaObj$x[,3], pcaObj$x[,4],main='PCA', type = 'p', 
      col = rgb(1-colPCA1, 
                1-1:k1/1:k1, 
                colPCA), 
      cex = 30*brAge/sum(brAge), lwd = 1, pch = pchPCA) 
# PCA [3]~[4]

#MDS из лекции
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
#видно расхождение по тканям

#heatmap
heatmap(cr,col = rev(heat.colors(100)),
        distfun = function(x){as.dist(1-x)}, 
        symm = T, 
        ColSideColors = rgb( 1-colPCA1,
                             1-1:k1/1:k1,
                             colPCA), 
        RowSideColors = rgb( 1-colPCA1, 
                             1-colPCA, 
                             1:k1/1:k1-1))


#Изменения в генах
pv <-  apply(normstand, 1, function(x) t.test(x[brNum], x[livNum])$p.value)
pvAdj <- p.adjust(pv, method = 'BH')

#ищу аналог FDR=0.05 в старом массиве pv:
FDRRev <- max(pv[pvAdj <= 0.05], na.rm = T) 
FDRRev

# Число генов, значимо меняющих экспрессию 
c(sum(pvAdj < 0.05), sum(pv < FDRRev)) #вроде совпадает

#Пермутации
#Число генов в случайных пермутациях с применением pvadj и просто pvalue (в сравнении с полученным FDRRev)
for(i in 1:3){
  normedPerm <- apply (normstand[,sample(1:113)], 1, function(x) t.test(x[1:56], x[57:113])$p.value)
  print(sum(p.adjust(normedPerm, method = 'BH') < 0.05, na.rm = T))
  print(sum(normedPerm < FDRRev, na.rm = T))
}
#Видим разницу в количестве генов. FDRRev был актуален только для нашего сета
#для случайных сетов он с треском проваливается



