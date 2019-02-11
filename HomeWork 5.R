genExp <- read.table('counts5.txt')

# нормировка
rsum <- t(apply(genExp, 2, function(x) sum(x)))
normed <- t(apply(genExp, 1, function(x) x/rsum))

#p-values
pv <-  apply(normed, 1, function(x) t.test(x[1:4], x[5:8])$p.value)
pvAdj <- p.adjust(pv, method = 'BH')

#FDR ищу по смыслу как аналог 0.05 в новом нормированном по BH массиве pvAdj:
pvBorderAdj <- max(pvAdj[pv <= 0.05], na.rm = T)
#Или же по формуле из лекции:
FDR=0.05*sum(pv > 0, na.rm = T)/sum(pv < 0.05, na.rm = T) #если убрать NA, 0.496
c(FDR, pvBorderAdj)
# в обоих случаях FDR = 0.496

# Число генов, значимо меняющих экспрессию после обработки ДГМО
sum(pvAdj<0.05, na.rm = T)

#Число генов в случайных пермутациях с применением pvadj и просто pvalue
#Их разница, видимо, должна продемонстрировать нам пользy pvadj
for(i in 1:100){
  normedPerm <- apply (normed[,sample(1:8)], 1, function(x) t.test(x[1:4], x[5:8])$p.value)
  print(sum(p.adjust(normedPerm, method = 'BH') < 0.05, na.rm = T))
  print(sum(normedPerm < 0.05, na.rm = T))
}




