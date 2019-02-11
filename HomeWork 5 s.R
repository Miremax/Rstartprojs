d1=read.table('counts5.txt')
#надо отнормировать риды, на экспрессию, считаем, как доли ридов в каждом образце

#maxct1=cbind(sum(d1[,1]),max(d1[,2]),max(d1[,3]),max(d1[,4]),max(d1[,5]),max(d1[,6]),max(d1[,7]),max(d1[,8]))

sum1=t(apply(d1,2,function(x) sum(x)))

tmaxct01=t(apply(d1,1,function(x) x/sum1))

#tmaxct01=t(apply(d1,1,function(x) x/maxct1))
pValue = apply(tmaxct01, 1, function(x) t.test(x[1:4],x[5:8])$p.value)
pvBH=p.adjust(pValue,method = 'BH', n = length(pValue))

FDR=0.05
Coll0=length(pvBH[pvBH<=FDR])
