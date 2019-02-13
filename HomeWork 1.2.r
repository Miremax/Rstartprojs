df <- read.table('hw1.txt')
mean(df$CPY14)
mean(df$RMD4)
mean(df$htVWQ)
#Различия есть :)
#Построим графически
plot(density(df$CPY14), col='red',  lwd =4, ylim=c(0,0.007))
lines(density(df$RMD4), col='blue', lwd =4)
lines(density(df$htVWQ), col='yellow', lwd =4)
legend("topright", title = "Гены", c("CPY14","RMD4","htVWQ"), fill=c("blue","red","yellow"))
#

t.test(df$CPY14,df$RMD4)$p.value
#cравнение средних, статистически значимые различия обнаружены
wilcox.test(df$CPY14,df$RMD4)$p.value
#тест Уилкоксона подтверждает, что выборки различны
t.test(df$htVWQ,df$RMD4)$p.value
#cравнение средних, статистически значимые различия обнаружены
wilcox.test(df$htVWQ,df$RMD4)$p.value
#тест Уилкоксона подтверждает, что выборки различны
t.test(df$htVWQ,df$CPY14)$p.value
#cравнение средних, они похожи, т.к. p-value недостаточно мало
wilcox.test(df$htVWQ,df$CPY14)$p.value
# хоть p.value уже ближе к критическим 0.05, но все же этот тест 
#говорит о различности наших выборок

cor(df$CPY14,df$RMD4)
#коэффициент корелляции высок, они связаны
cor(df$htVWQ,df$RMD4)
#корелляция отрицательна, коэф-т низок. Связи нет
cor(df$htVWQ,df$CPY14)
#корелляция отрицательна, коэф-т низок. Связи нет