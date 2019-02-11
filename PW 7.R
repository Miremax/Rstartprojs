genExp <- read.table('hw6.counts.txt', check.names = F)
NNum <- grep("XLOC_111636", rownames(genExp))
XLout <- colSums(genExp) - genExp[NNum,]
XLself <- genExp[NNum,]
T1 <- cbind(t(XLself), t(XLout))
  
  
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
brAge <- unlist(lapply(brAge, function(x) as.numeric(substr(x,1,4))))
livAge <- unlist(lapply(livAge, function(x) as.numeric(substr(x,1,4))))

Age <-  c(brAge, livAge)
Tissue <- integer(k1)
Tissue[livNum] <- 'liver'
Tissue[brNum] <- 'brain'
A1 <- Age**0.25
A2 <- Age**0.5
Ti <- as.factor(Tissue)
m <- glm(as.matrix(T1) ~ (Ti+A1+A2+Ti:A1+Ti:A2), family = 'quasibinomial')
#m <- glm(as.matrix(T1) ~ (as.factor(Tissue)+Age))
anova(m, test = 'Chisq')
d = predict(m, type = 'resp', newdata = list(Ti[livNum],A1[livNum],A2[livNum]))
plot(d   )
