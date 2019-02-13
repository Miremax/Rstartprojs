#df1 <- read.csv2('One2.csv')
df2 <- read.csv2('Two2.csv', stringsAsFactors = F)
df3 <- read.csv2('One3.csv', stringsAsFactors = F)

#First df
#Уберем всех клиентов без даты приема
nums <- (df3[,5] == '')
df4 <- df3[as.logical(1-as.numeric(nums)),]

k1 <- as.character(df4[,5])
yea1 <- unlist(lapply(k1, function(x) unlist(strsplit(x, '.', fixed = T))[3]))
df4$months <- unlist(lapply(k1, function(x) unlist(strsplit(x, '.', fixed = T))[2]))
df4$dates <- unlist(lapply(k1, function(x) unlist(strsplit(x, '.', fixed = T))[1]))
df4$years <- unlist(lapply(yea1, function(x) unlist(strsplit(x, ' ', fixed = T))[1]))


df4$daysfrommil <- (as.numeric(df4$years)-2000)*365 + as.numeric(df4$months)*30 + as.numeric(df4$dates)
df4 <- df4[ order(df4[,1], df4$daysfrommil), ]

df1 <- df4

#Second df
nums <- (df3[,5] == '')
df4 <- df2[as.logical(1-as.numeric(nums)),]

k1 <- as.character(df4[,5])
yea1 <- unlist(lapply(k1, function(x) unlist(strsplit(x, '.', fixed = T))[3]))
df4$months <- unlist(lapply(k1, function(x) unlist(strsplit(x, '.', fixed = T))[2]))
df4$dates <- unlist(lapply(k1, function(x) unlist(strsplit(x, '.', fixed = T))[1]))
df4$years <- unlist(lapply(yea1, function(x) unlist(strsplit(x, ' ', fixed = T))[1]))

df4$daysfrommil <- (as.numeric(df4$years)-2000)*365 + as.numeric(df4$months)*30 + as.numeric(df4$dates)
df4 <- df4[ order(df4[,1], df4$daysfrommil), ]


sum(1 - as.numeric(df4[,1] == df1[,1])) == 0
# проверили на совпадаемость

#почистим немного память
#remove(df2,df3,k1,yea1)

#слепили в один датасет
df6 <- cbind(df1, df4)
names(df6)
df7 <- df6[,!duplicated(names(df6))]

#давление
df7$ADgolH <- as.numeric(unlist(lapply(df7$АД_голень, function(x) unlist(strsplit(x, '/', fixed = T))[1])))
df7$ADgolL <- as.numeric(unlist(lapply(df7$АД_голень, function(x) unlist(strsplit(x, '/', fixed = T))[2])))

df7$ADplH <- as.numeric(unlist(lapply(df7$АД_плечо, function(x) unlist(strsplit(x, '/', fixed = T))[1])))
df7$ADplL <- as.numeric(unlist(lapply(df7$АД_плечо, function(x) unlist(strsplit(x, '/', fixed = T))[2])))

#работа с возрастом
df7$YO <- 2018 - as.numeric(unlist(lapply(df7[,2], function(x) unlist(strsplit(x, '.', fixed = T))[3])))

#первичность обращений
n <- length(df7[,1])
df7$dif <- c(130, df7$daysfrommil[2:n]-df7$daysfrommil[1:(n-1)])
df7$sameCard <- c(T, df7[2:n,1] == df7[1:(n-1),1])
df8 <- df7[(df7$dif > 120) | (df7$sameCard == F),]

#c 2015 по 2017
df8 <- df8[df8$years>2014,]
df8 <- df8[is.na(df8[,1])==F,]


#sum(df8$years>2014, na.rm = T)
#sum(df8$years<2018, na.rm = T)

#почистим немного память
remove(df6,df1,df4)


#Сделаем числами
#apply(df8, 2, function(x) x <- as.numeric(x))

# женщин в один, мужчин в другой
dfm <- df8[df8[,4]=='М',]
dfw <- df8[df8[,4]=='Ж',]

#снова года
m1845 <- dfm[dfm$YO>17,][dfm$YO<46,]#[is.na(dfm$YO)==F,]
m1845 <- m1845[is.na(m1845$YO)==F,]

m55 <- dfm[dfm$YO>55,]
m55 <- m55[is.na(m55$YO)==F,]


#женские года
w1845 <- dfw[dfw$YO>17,][dfw$YO<46,]#[is.na(dfw$YO)==F,]
w1845 <- w1845[is.na(w1845$YO)==F,]

w55 <- dfw[dfw$YO>55,]
w55 <- w55[is.na(w55$YO)==F,]

#почистим немного память
#      remove(dfm,dfw,df7,df8,nums)

library(ggplot2)

names(w55)

sts <- c(36, 35, 29, 32, 30, 39, 28, 13, 15, 22, 21, 45, 46)



GraphBuilding <- function(annum, df, naz){
  
  tiff(tiff, filename=naz, width=2*ncolGr, height=40, res=80, units='in')
  layout(matrix(c(1:168),ncol=ncolGr))
  mzav <- matrix(data = 0, nrow = length(annum), ncol = length(annum))
  mzav
  for (i in annum){
    print(i)
    d1 <- sd(df[,i], na.rm = T)*0.5
    m1 <- mean(df[,i], na.rm = T)
    for (j in annum){
      if (j != i){
        
        #pvs
        pvz <- t.test(df[,j][df[,i] > m1+d1], df[,j][df[,i] < m1-d1])$p.value
        pv <- t.test(df[,j][df[,i] > m1+d1], df[,j][df[,i] < m1-d1])$p.value<0.05
        
        # матрицa
        mzav[which(annum == i), which(annum == j)] <- mzav[which(annum == i), which(annum == j)] + ifelse(pv, 1, 0)
        
        #график
        n1 <- c(paste('до', as.character(round(m1-d1, 2))), paste('от', as.character(round(m1+d1, 2))))
        
        #new (may) graph
        #plot(df[,j]~df[,i])
        #end of may graph
        
        plot(df[,j]~df[,i],
             col = ifelse(pv, 'red', 'green'),
             main = paste(ifelse(pv, 'Зависимы, pv =', 'pv =') ,as.character(round(pvz, digits = 4))),
             xlab=names(df)[i],
             ylab=names(df)[j],
             names = n1)
        
        fit <- glm(df[,j]~df[,i])
        #co <- coef(fit)
        abline(fit, col="blue", lwd=2)
        #lines(df[,j]~df[,i])
        #legend('topright', legend = paste(names(df)[i], 'до', as.character(round(m1-d1, 2)),'и от', as.character(round(m1+d1, 2))), cex = 0.75)
      }
    }
    
  }
  
  
  
  #end of march:
  for (i in 1:length(annum)){
    for (j in 1:length(annum)){
      if (mzav[i,j] + mzav[j,i] == 2){
        mzav[i,j] = 2
        mzav[j,i] = 2
      }
    }
  }
  mz1 <- as.data.frame(mzav)
  names(mz1) <- names(df)[annum]
  rownames(mz1) <- names(df)[annum]
  #mz1[mz1 == 2] <- '«'
  #mz1[mz1 == 1] <- '¬'
  #mz1[mz1 == 0] <- ''
  
  print(mz1)
  
  dev.off()
}



GraphBuilding(sts, w55, 'Women 55 plus.tiff')
GraphBuilding(sts, w1845, 'Women 18-45.tiff')
GraphBuilding(sts, m55, 'Men 55 plus.tiff')
GraphBuilding(sts, m1845, 'Men 18-45.tiff')

df98 <- rbind(df8[df8$YO<40,],df8[df8$YO>59,])

hist(df8$YO, breaks = 15,
     xlab='Возраст пациентов',
     ylab='Количество пациентов',
     names = n1)

GraphBuilding2 <- function(annum, df, naz, naz2){
  k=0
  ncolGr = length(annum)-1
  jpeg( filename=naz, width=ncolGr*5, height=ncolGr*5, res=160, units='in', q=70)
  layout(t(matrix(c(1:(4*ncolGr*(ncolGr+1))),nrow=2*ncolGr)))
  mzav <- matrix(data = 0, nrow = length(annum), ncol = length(annum))
  mzav
  for (i in annum){
    
 #   if ((i==annum[length(annum)/2]) & (k==0)){
  #    k=1
  #    dev.off()
     # naz2='f2.tiff'      
 #     tiff(tiff, filename=naz2, width=ncolGr*5, height=ncolGr*5, res=100, units='in')
  #    layout(t(matrix(c(1:(4*ncolGr*(ncolGr+1))),nrow=2*ncolGr)))
  #  }
    print(i)
    d1 <- sd(df[,i], na.rm = T)*0.5
    m1 <- mean(df[,i], na.rm = T)
    for (j in annum){
      if (j != i){
        
        #pvs
        pvz <- t.test(df[,j][df[,i] > m1+d1], df[,j][df[,i] < m1-d1])$p.value
        pv <- t.test(df[,j][df[,i] > m1+d1], df[,j][df[,i] < m1-d1])$p.value<0.05
        
        # матрицa
        mzav[which(annum == i), which(annum == j)] <- mzav[which(annum == i), which(annum == j)] + ifelse(pv, 1, 0)
        
        #график
        n1 <- c(paste('до', as.character(round(m1-d1, 2))), paste('от', as.character(round(m1+d1, 2))))
        
        #new (may) graph
        #plot(df[,j]~df[,i])
        #end of may graph
        
        plot(df[,j][df[,4]=='М'][df$YO>55]~df[,i][df[,4]=='М'][df$YO>55],
             col = 'orange',
             #col = ifelse(df[,4]=='М', ifelse(df$YO>50, 'magenta', 'maroon'), ifelse(df$YO>50, 'green', 'blue')),
             #main = c(as.character(sum(df[,j]$YO<45, na.rm = T)), as.character(sum(df[,i]$YO>55, na.rm = T)) ),
             main = paste( 'M, ', 'от 55, ' ,as.character(length(df[,j][df[,4]=='Ж'][df$YO>55])), ' чел'),
             #main = paste(ifelse(pv, 'Зависимы, pv =', 'pv =') ,as.character(round(pvz, digits = 4))),
             xlab=names(df)[i],
             ylab=names(df)[j],
             names = n1)
        fit1 <- glm(df[,j][df[,4]=='М'][df$YO>50]~df[,i][df[,4]=='М'][df$YO>50])
        abline(fit1, col='orange', lwd=2)
        
        plot(df[,j][df[,4]=='М'][df$YO<50]~df[,i][df[,4]=='М'][df$YO<50],
             col = 'maroon',
             #col = ifelse(df[,4]=='М', ifelse(df$YO>50, 'magenta', 'maroon'), ifelse(df$YO>50, 'green', 'blue')),
             #main = c(as.character(sum(df[,j]$YO<45, na.rm = T)), as.character(sum(df[,i]$YO>55, na.rm = T)) ),
             main = paste( 'М, ', 'до 45, ' ,as.character(length(df[,j][df[,4]=='М'][df$YO<50])), ' чел'),
             xlab=names(df)[i],
             ylab=names(df)[j],
             names = n1)
        fit2 <- glm(df[,j][df[,4]=='М'][df$YO<50]~df[,i][df[,4]=='М'][df$YO<50])
        abline(fit2, col='maroon', lwd=2)
        
        
        plot(df[,j][df[,4]=='Ж'][df$YO>55]~df[,i][df[,4]=='Ж'][df$YO>55],
             col = 'green',
             #col = ifelse(df[,4]=='М', ifelse(df$YO>50, 'magenta', 'maroon'), ifelse(df$YO>50, 'green', 'blue')),
             #main = c(as.character(sum(df[,j]$YO<45, na.rm = T)), as.character(sum(df[,i]$YO>55, na.rm = T)) ),
             main = paste( 'Ж, ', 'от 55, ' ,as.character(length(df[,j][df[,4]=='Ж'][df$YO>55])), ' чел'),
             #main = paste(ifelse(pv, 'Зависимы, pv =', 'pv =') ,as.character(round(pvz, digits = 4))),
             xlab=names(df)[i],
             ylab=names(df)[j],
             names = n1)
        fit3 <- glm(df[,j][df[,4]=='Ж'][df$YO>50]~df[,i][df[,4]=='Ж'][df$YO>50])
        abline(fit3, col='green', lwd=2)
        
        plot(df[,j][df[,4]=='Ж'][df$YO<45]~df[,i][df[,4]=='Ж'][df$YO<45],
             col = 'blue',
             #col = ifelse(df[,4]=='М', ifelse(df$YO>50, 'magenta', 'maroon'), ifelse(df$YO>50, 'green', 'blue')),
             #main = c(as.character(sum(df[,j]$YO<45, na.rm = T)), as.character(sum(df[,i]$YO>55, na.rm = T)) ),
             main = paste( 'Ж, ', 'до 45, ' ,as.character(length(df[,j][df[,4]=='Ж'][df$YO>45])), ' чел'),
             #main = paste(ifelse(pv, 'Зависимы, pv =', 'pv =') ,as.character(round(pvz, digits = 4))),
             xlab=names(df)[i],
             ylab=names(df)[j],
             names = n1)
        fit4 <- glm(df[,j][df[,4]=='Ж'][df$YO<45]~df[,i][df[,4]=='Ж'][df$YO<45])
        abline(fit4, col='blue', lwd=2)
        #lines(df[,j]~df[,i])
        #legend('topright', legend = paste(names(df)[i], 'до', as.character(round(m1-d1, 2)),'и от', as.character(round(m1+d1, 2))), cex = 0.75)
      }
    }
    
  }
  
  
  
  #end of march:
  for (i in 1:length(annum)){
    for (j in 1:length(annum)){
      if (mzav[i,j] + mzav[j,i] == 2){
        mzav[i,j] = 2
        mzav[j,i] = 2
      }
    }
  }
  mz1 <- as.data.frame(mzav)
  names(mz1) <- names(df)[annum]
  rownames(mz1) <- names(df)[annum]
  #mz1[mz1 == 2] <- '«'
  #mz1[mz1 == 1] <- '¬'
  #mz1[mz1 == 0] <- ''
  
  print(mz1)
  
  dev.off()
}

GraphBuilding2(sts, df8, 'res7-2.jpg', "res2.tiff")


