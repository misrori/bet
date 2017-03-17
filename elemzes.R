library(data.table)
library(randomForest)

#https://www.bet.hu/oldalak/adatletoltes
#otp, mol, any, fhb, mtelekom

adat <- fread('adatok.csv')
adat <- adat[,1:4, with==F]
names(adat) <- c('ticker','date','close', 'amount')
adat$date <-as.Date(adat$date, format = "%Y.%m.%d") 
adat <- adat[, valtozas1:=(close/shift(close, 1L, type = "lag")-1)*100, by=ticker ]
adat <- adat[complete.cases(adat)]

mol <- adat[adat$ticker=="MOL",][1:580,]

df_mol <-data.frame() 

for(i in 5:nrow(mol)){
    t<- data.frame(x4=mol$valtozas1[i-4], x4a=mol$amount[i-4],x3=mol$valtozas1[i-3], x3a=mol$amount[i-3],x2=mol$valtozas1[i-2], x2a=mol$amount[i-2], x1=mol$valtozas1[i-1], x1a=mol$amount[i-1], target=mol$valtozas1[i])
    df_mol <- rbind(df_mol, t)
   }


mod1 <- lm(target~.,data = df_mol[1:470,])
 
eredmyeny <- predict(mod1, df_mol[570:578,] )

aa<- data.frame(df_mol[570:578,]$target, eredmyeny)

md <- randomForest(target ~ ., data = df_mol[1:470,], ntree = 500)
md
plot(md)
phat <- predict(md, df_mol[470:578,] )
f <- data.frame(df_mol[470:578,]$target, phat)
names(f) <- c("act", "pred")

md <- randomForest(cnt ~ ., data = d_train, ntree = 200, importance = TRUE)
varImpPlot(md)

# 
# my_match <- which(adat$valtozas1>2)
# 
# my_rows <-NULL 
# for(i in my_match){
#   t <-c(i-2,i-1, i) 
#   my_rows<- c(my_rows, t)
# }
# 
# 
# hasznos<- adat[my_rows,]
# mol <- hasznos[hasznos$ticker=='MOL',]





# 
# x2 <- NULL
# x1 <-NULL
# target <- NULL
# df_mol <-data.frame() 
# 
# for(i in seq(1,nrow(mol), by=3)){
#   t<- data.frame(x2=mol$valtozas1[i], x2a=mol$amount[i], x1=mol$valtozas1[i+1], x1a=mol$amount[i+1], target=mol$valtozas1[i+2])
#   df_mol <- rbind(df_mol, t)
# }
# 
# 
# mod1 <- lm(target~.,data = df_mol)
# 
# predict(mod1, x2=0.0, x2a = 43000, x1=0.2, x2a=3400 )
# 
# 
# 
# 
# 
# str(hasznos)
# 
