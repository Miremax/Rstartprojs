setwd('F:/YandexDisk/Образование/HSE/Term1/R programming/HomeWork 7')

genExp1 <- read.table('hw6.counts.txt', check.names = F)

#выделим ткани
brNum <- grep("brain", names(genExp1)) #Номера столбцов мозга
livNum <- grep("liver", names(genExp1))  #номера столбцов печени
#сделаем фактор "ткани" (Ti)
Tissue <- integer(length(genExp1[1,]))
Tissue[livNum] <- 'liver'
Tissue[brNum] <- 'brain'
Ti <- as.factor(Tissue)

#выделим возраста
#BRAIN
b2 <- unlist(strsplit(names(genExp1)[brNum], '_'))
brAge <- b2[-grep("brain", b2)]
brAge <- unlist(lapply(brAge, function(x) as.numeric(substr(x,1,4))))
#LIVER
b2 <- unlist(strsplit(names(genExp1)[-brNum], '_'))
livAge <- b2[-grep("liver", b2)]
livAge <- unlist(lapply(livAge, function(x) as.numeric(substr(x,1,4))))
#Сделаем факторы возраста в соответствии с форматами задания (A1, A2)
Age <-  c(brAge, livAge)
A1 <- Age**0.25
A2 <- Age**0.5

    #Номера столбцов, соответствующие возрастам
    # ageNum <- lapply(unique(brAge), function(x) grep(x, names(genExp)))  
    #оказалась не нужна, но я над ней долго думал, пусть останется

#Нормировка на размер библиотек
genExp <-  as.data.frame(apply(genExp1, 2, function(x) x/sum(x)))

#Без взаимодействий (в задании не указано)
pvAnova <- apply(genExp, 1, function(x) anova(lm(x ~ Ti+A1+A2), test = 't.test')$`Pr(>F)`) 
pvAnovaAdj <- t(apply(pvAnova, 1, function(x) p.adjust(x, method = 'BH'))) 
#Со взаимодействиями, как в классе
pvAnovaRel <- apply(genExp, 1, function(x) anova(lm(x ~ Ti+A1+A2+Ti:A1+Ti:A2), test = 't.test')$`Pr(>F)`)
pvAnovaRelAdj <- t(apply(pvAnovaRel, 1, function(x) p.adjust(x, method = 'BH')))

# Чему равен порог на p.value для разницы между тканями
#И по смыслу, и по формуле, вывод обоих:
#Без взаимодействий
pvTiBorder <- max(pvAnovaAdj[1,][pvAnova[1,] <= 0.05], na.rm = T) 
pvTiBorderFormula <- 0.05*sum(pvAnova[1,] > 0, na.rm = T)/sum(pvAnova[1,] < 0.05, na.rm = T)
c(pvTiBorder, pvTiBorderFormula)
#Со взаимодействиями
pvTiBorderRel <- max(pvAnovaRelAdj[1,][pvAnova[1,] <= 0.05], na.rm = T) 
pvTiBorderRelFormula <- 0.05*sum(pvAnovaRel[1,] > 0, na.rm = T)/sum(pvAnovaRel[1,] < 0.05, na.rm = T)
c(pvTiBorderRel, pvTiBorderRelFormula)

#Сколько генов имеют значимые отличия в экспрессии между тканямu
c(sum(pvAnovaAdj[1,] <= 0.05, na.rm = T), sum(pvAnovaRelAdj[1,] <= 0.05, na.rm = T))
#меняют экспрессию с возрастом
pvAgeBoth <- sum(pvAnovaAdj[2,] <= 0.05 | pvAnovaAdj[3,] <= 0.05, na.rm = T)
pvAgeRelBoth <- sum(pvAnovaRelAdj[2,] <= 0.05 | pvAnovaRelAdj[3,] <= 0.05, na.rm = T) #от одного из представлений возраста
c(pvAgeBoth, pvAgeRelBoth)
#гены, которые меняют экспрессию с возрастом по-разному в разных тканях
sum(pvAnovaRelAdj[4,] <= 0.05 | pvAnovaRelAdj[5,] <= 0.05, na.rm = T)



#Пермутации
N1 <- length(genExp[1,])
for(i in 1:100){
  pvPerm <- apply (genExp[,sample(1:N1)], 1, function(x) anova(lm(x ~ Ti+A1+A2+Ti:A1+Ti:A2), test = 't.test')$`Pr(>F)`)
  
  print("Доля ложных в экспрессии между тканямu") 
  #ложных больше, чем настоящих, как можно долю-то посчитать, если настоящих часто 0 будет и доля от них - бесконечность?
  print(sum(pvPerm[1,] <= 0.05, na.rm = T)) 
  print(sum(p.adjust(pvPerm[1,], method = 'BH') <= 0.05, na.rm = T))
  
  print("Доля ложных в экспрессии между возрастом")
  print(sum(pvPerm[2,] <= 0.05 | pvPerm[3,] <= 0.05, na.rm = T)) 
  print(sum(p.adjust(pvPerm[2,], method = 'BH') <= 0.05 | p.adjust(pvPerm[3,], method = 'BH') <= 0.05, na.rm = T))
  
  print("Доля ложно-изменчивых в экспрессии в связи с возрастом и в разных тканях")
  print(sum(pvPerm[4,] <= 0.05 | pvPerm[5,] <= 0.05, na.rm = T))
  print(sum(p.adjust(pvPerm[4,], method = 'BH') <= 0.05 | p.adjust(pvPerm[5,], method = 'BH') <= 0.05, na.rm = T))  
          
}
