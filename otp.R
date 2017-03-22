library(data.table)
library(randomForest)

#https://www.bet.hu/oldalak/adatletoltes
#otp, mol, any, fhb, mtelekom

adat <- data.table(fread('historikus_2012_03_22__2017_03_21_.csv'))

adat <- adat[,c(1,2,6,7), with=F]
names(adat) <- c('ticker','date','close', 'amount')
adat$date <-as.Date(adat$date, format = "%Y.%m.%d") 
adat <- adat[, valtozas1:=(close/shift(close, 1L, type = "lag")-1)*100, by=ticker ]



bux <- data.table(fread('historikus_bux_2012_03_22__2017_03_21_.csv'))
bux <- bux[, c(2,6)]
names(bux)<- c('date', 'bux')
bux$date <-as.Date(bux$date, format = "%Y.%m.%d") 
bux <- bux[, bux_valt:=(bux/shift(bux, 1L, type = "lag")-1)*100 ]
bux<- bux[, c(1,3)]


adat <- merge(adat, bux, by='date')
adat <- adat[complete.cases(adat)]
setorder(adat, ticker, date)
adat <- adat[ticker=="OTP",]
adat <- adat[,-2]


train_df <-data.frame() 

for(i in 4:nrow(adat)){
  t<- data.frame(x3=adat$valtozas1[i-3], x3a=adat$amount[i-3],x2=adat$valtozas1[i-2], x2a=adat$amount[i-2],
                 x1=adat$valtozas1[i-1], adat=adat$amount[i-1], x3_bux=adat$bux_valt[i-3], 
                 x2_bux=adat$bux_valt[i-2], x1_bux=adat$bux_valt[i-1], target=adat$valtozas1[i])
  train_df <- rbind(train_df, t)
}


train_df$target <- ifelse(train_df$target>1.5, 1,0)
train_df$target <- as.factor(train_df$target)

md <- randomForest(target ~ ., data = train_df[1:1000,], ntree = 500)


md
plot(md)
phat <- predict(md, train_df[1000:1235,] )
f <- data.frame(train_df[1000:1235,]$target, phat)
names(f) <- c("act", "pred")
View(f)
varImpPlot(md)

d_mol_test <- adat[ti]
for(i in 4:nrow(adat)){
  t<- data.frame(x3=adat$valtozas1[i-3], x3a=adat$amount[i-3],x2=adat$valtozas1[i-2], x2a=adat$amount[i-2],
                 x1=adat$valtozas1[i-1], adat=adat$amount[i-1], x3_bux=adat$bux_valt[i-3], 
                 x2_bux=adat$bux_valt[i-2], x1_bux=adat$bux_valt[i-1], target=adat$valtozas1[i])
  train_df <- rbind(train_df, t)
}





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

