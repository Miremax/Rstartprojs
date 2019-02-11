N = 10000
NormExp = matrix(rnorm(5*N), ncol = 5)
CancerExp = matrix(append(rnorm(0.05*5*N, mean = 3),rnorm(0.95*5*N)), ncol = 5, byrow = T)
Exp = cbind(NormExp, CancerExp)
pValue = apply(Exp, 1, function(x) t.test(x[1:5],x[6:10])$p.value)

#Сколько генов значимо при уровне значимости равном 0.05?
length(pValue[pValue<=0.05])

#Чему равна доля ложно-положительных?
length(pValue[pValue[(0.05*N+1):N]<=0.05])/length(pValue[pValue<=0.05])

#5.Какому уровню значимости соответствует FDR=0.05 (поправка BH).
pValue[round(p.adjust(pValue, method = 'BH'), digits = 2)==0.05]

#Сколько генов значимо в этом случае. Чему равен реальный FDR?
pvBH = p.adjust(pValue, method = 'BH')
length(pvBH[pvBH <= 0.05])

#6.Ответьте на вопросы предыдущего пункта, но с использованием поправки Боннферони
pvBonf = p.adjust(pValue, method = 'BH')
length(pvBonf[pvBonf <= 0.05])

#7.Постройте распределение размера эффекта (разница средних между больными и здоровыми) для генов из пятого пункта. 
#Соответствует ли распределение вашему ожиданию?
Exp1 = data.frame(cbind(Exp, pvBH))
Exp2 = Exp1[Exp1$pvBH<=0.05,]
meandif = apply(Exp2,1, function(x) mean(x[6:10] - mean(x[1:5])))
hist(meandif, breaks = 50)

