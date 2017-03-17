library(data.table)

#https://www.bet.hu/oldalak/adatletoltes
#otp, mol, any, fhb, mtelekom

adat <- fread('adatok.csv')
adat <- adat[,1:4, with==F]
names(adat) <- c('ticker','date','close', 'amount')
adat$date <-as.Date(adat$date, format = "%Y.%m.%d") 
adat <- adat[, valtozas1:=(close/shift(close, 1L, type = "lag")-1)*100, by=ticker ]
adat <- adat[complete.cases(adat)]


my_match <- which(adat$valtozas1>2)

my_rows <-NULL 
for(i in my_match){
  t <-c(i-2,i-1, i) 
  my_rows<- c(my_rows, t)
}


hasznos<- adat[my_rows,]
mol <- hasznos[hasznos$ticker=='MOL',]

x2 <- NULL
x1 <-NULL
target <- NULL
df_mol <-data.frame() 

for(i in seq(1,nrow(mol), by=3)){
  t<- data.frame(x2=mol$valtozas1[i], x2a=mol$amount[i], x1=mol$valtozas1[i+1], x1a=mol$amount[i+1], target=mol$valtozas1[i+2])
  df_mol <- rbind(df_mol, t)
}







str(hasznos)

