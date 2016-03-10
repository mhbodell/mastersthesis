################################################
##### normal-gamma, non-imformative priors #####
################################################
ninter=3000

############# M ###################

library(rjags)
data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)

datM = na.omit(polls[,c('M','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datM$house = factor(datM$house)
datM = datM[order(datM$collectPeriodFrom),]
datM$collectPeriodFrom = as.Date(datM$collectPeriodFrom)
datM$collectPeriodTo = as.Date(datM$collectPeriodTo)
datM$PublDate = as.Date(datM$PublDate)
orig.date = as.Date("2006-09-16") #day before election 2011
end.date = as.Date('2014-09-14') #election day 2014

elec = data.frame(rbind(c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0),
                        c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                        c(0.2333,0.0542,0.0457,0.0611,0.3101,0.0689,0.0572,0.1286)))
colnames(elec) = c("M","L","KD","C","S","MP","V","SD")
row.names(elec) = c("2002","2006","2010","2012")
elec$Date = c(as.Date("2002-09-12"), as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14'))
n=c((0.801*6722*1000),(0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000))
## http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf

dfM = data.frame(M=elec$M*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datM = rbind(datM, dfM)
datM = datM[order(datM$collectPeriodFrom),]
datM = datM[-which(datM$collectPeriodFrom>end.date),]
datM = datM[-which(datM$collectPeriodFrom<orig.date),]

datM$collectPeriodFrom.num = julian(datM$collectPeriodFrom,origin=orig.date) #days since origin
datM$collectPeriodTo.num = julian(datM$collectPeriodTo,origin=orig.date) #days since origin
datM$fieldDate.num = floor((datM$collectPeriodFrom.num + datM$collectPeriodTo.num) / 2)
datM$M = datM$M/100

jags_M ='
model{

#observed model
for(i in 1:npolls){
M[i] ~ dnorm(xM[day[i]],precM[i])
}

#dynamic model 
for(i in 2:nperiods){
xM[i] ~ dnorm(xM[i-1],phiM)
}

## priors
phiM ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
}
'

pM = (1 / (datM$M*(1-datM$M)/datM$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_M = list(M = datM$M, precM = pM, xM = c(rnorm(1,elec[2,1], 0.00001),rep(NA,end.date - orig.date-1)),
              day = datM$fieldDate.num, npolls = nrow(datM), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_M,con="kalman_M.bug")

system.time(jags_mod_M <- jags.model("kalman_M.bug", data = data_M))

system.time(outM <- coda.samples(jags_mod_M,variable.names = c("xM", "M"), n.iter = ninter, n.thin = 1))
#system.time(outM_jags <- jags.samples(jags_mod_M,variable.names = c("xM", "M","phiM" ), n.iter = 5000, n.thin = 10))
sumM = summary(outM)
cred_intM = HPDinterval(outM[,which(regexpr("xM", row.names(sumM$statistics))==1)], 0.95)
#sumM$quantiles[elec.day[3],c(1,5)] 

############ L ###############

datL = na.omit(polls[,c('FP','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
colnames(datL)[1] = "L"
datL$house = factor(datL$house)
datL = datL[order(datL$collectPeriodFrom),]
datL$collectPeriodFrom = as.Date(datL$collectPeriodFrom)
datL$collectPeriodTo = as.Date(datL$collectPeriodTo)
datL$PublDate = as.Date(datL$PublDate)
orig.date = as.Date("2006-09-16") #day before election 2011
end.date = as.Date('2014-09-14') #election day 2014

elec = data.frame(rbind(c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0),
                        c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                        c(0.2333,0.0542,0.0457,0.0611,0.3101,0.0689,0.0572,0.1286)))
colnames(elec) = c("M","L","KD","C","S","MP","V","SD")
row.names(elec) = c("2002","2006","2010","2012")
elec$Date = c(as.Date("2002-09-12"), as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14'))
n=c((0.801*6722*1000),(0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000))
## http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf

dfL = data.frame(L=elec$L*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datL = rbind(datL, dfL)
datL = datL[order(datL$collectPeriodFrom),]
datL = datL[-which(datL$collectPeriodFrom>end.date),]
datL = datL[-which(datL$collectPeriodFrom<orig.date),]

datL$collectPeriodFrom.num = julian(datL$collectPeriodFrom,origin=orig.date) #days since origin
datL$collectPeriodTo.num = julian(datL$collectPeriodTo,origin=orig.date) #days since origin
datL$fieldDate.num = floor((datL$collectPeriodFrom.num + datL$collectPeriodTo.num) / 2)
datL$L = datL$L/100

jags_L ='
model{

#observed model
for(i in 1:npolls){
L[i] ~ dnorm(xL[day[i]],precL[i])
}

#dynamic model 
for(i in 2:nperiods){
xL[i] ~ dnorm(xL[i-1],phiL)
}

## priors
phiL ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
}
'

pL = (1 / (datL$L*(1-datL$L)/datL$n)) #binomial
#pL = (1 / (datL$L*(1-datL$L)*datL$n)) #multinomial
data_L = list(L = datL$L, precL = pL, xL = c(rnorm(1,elec[2,2], 0.00001),rep(NA,end.date - orig.date-1)),
              day = datL$fieldDate.num, npolls = nrow(datL), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_L,con="kalman_L.bug")

system.time(jags_mod_L <- jags.model("kalman_L.bug", data = data_L))

system.time(outL <- coda.samples(jags_mod_L,variable.names = c("xL", "L"), n.iter = ninter, n.thin = 1))
#system.time(outL_jags <- jags.samples(jags_mod_L,variable.names = c("xL", "L","phiL" ), n.iter = 5000, n.thin = 10))
sumL = summary(outL)
cred_intL = HPDinterval(outL[,which(regexpr("xL", row.names(sumL$statistics))==1)], 0.95)
#sumL$quantiles[elec.day[3],c(1,5)] 

############ KD ###############

datKD = na.omit(polls[,c('KD','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datKD$house = factor(datKD$house)
datKD = datKD[order(datKD$collectPeriodFrom),]
datKD$collectPeriodFrom = as.Date(datKD$collectPeriodFrom)
datKD$collectPeriodTo = as.Date(datKD$collectPeriodTo)
datKD$PublDate = as.Date(datKD$PublDate)
orig.date = as.Date("2006-09-16") #day before election 2011
end.date = as.Date('2014-09-14') #election day 2014

elec = data.frame(rbind(c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0),
                        c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                        c(0.2333,0.0542,0.0457,0.0611,0.3101,0.0689,0.0572,0.1286)))
colnames(elec) = c("M","L","KD","C","S","MP","V","SD")
row.names(elec) = c("2002","2006","2010","2012")
elec$Date = c(as.Date("2002-09-12"), as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14'))
n=c((0.801*6722*1000),(0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000))
## http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf

dfKD = data.frame(KD=elec$KD*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datKD = rbind(datKD, dfKD)
datKD = datKD[order(datKD$collectPeriodFrom),]
datKD = datKD[-which(datKD$collectPeriodFrom>end.date),]
datKD = datKD[-which(datKD$collectPeriodFro<orig.date),]

datKD$collectPeriodFrom.num = julian(datKD$collectPeriodFrom,origin=orig.date) #days since origin
datKD$collectPeriodTo.num = julian(datKD$collectPeriodTo,origin=orig.date) #days since origin
datKD$fieldDate.num = floor((datKD$collectPeriodFrom.num + datKD$collectPeriodTo.num) / 2)
datKD$KD = datKD$KD/100

jags_KD ='
model{

#observed model
for(i in 1:npolls){
KD[i] ~ dnorm(xKD[day[i]],precKD[i])
}

#dynamic model 
for(i in 2:nperiods){
xKD[i] ~ dnorm(xKD[i-1],phiKD)
}

## priors
phiKD ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
}
'

pKD = (1 / (datKD$KD*(1-datKD$KD)/datKD$n)) #binomial
#pKD = (1 / (datKD$KD*(1-datKD$KD)*datKD$n)) #multinomial
data_KD = list(KD = datKD$KD, precKD = pKD, xKD = c(rnorm(1,elec[2,3], 0.00001),rep(NA,end.date - orig.date-1)),
               day = datKD$fieldDate.num, npolls = nrow(datKD), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_KD,con="kalman_KD.bug")

system.time(jags_mod_KD <- jags.model("kalman_KD.bug", data = data_KD))

system.time(outKD <- coda.samples(jags_mod_KD,variable.names = c("xKD", "KD"), n.iter = ninter, n.thin = 1))
#system.time(outKD_jags <- jags.samples(jags_mod_KD,variable.names = c("xKD", "KD","phiKD" ), n.iter = 5000, n.thin = 10))
sumKD = summary(outKD)
cred_intKD = HPDinterval(outKD[,which(regexpr("xKD", row.names(sumKD$statistics))==1)], 0.95)
#sumKD$quantiles[elec.day[3],c(1,5)] 

############ C ###############
datC = na.omit(polls[,c('C','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datC$house = factor(datC$house)
datC = datC[order(datC$collectPeriodFrom),]
datC$collectPeriodFrom = as.Date(datC$collectPeriodFrom)
datC$collectPeriodTo = as.Date(datC$collectPeriodTo)
datC$PublDate = as.Date(datC$PublDate)
orig.date = as.Date("2006-09-161") #day before election 2011
end.date = as.Date('2014-09-14') #election day 2014

elec = data.frame(rbind(c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0),
                        c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                        c(0.2333,0.0542,0.0457,0.0611,0.3101,0.0689,0.0572,0.1286)))
colnames(elec) = c("M","L","KD","C","S","MP","V","SD")
row.names(elec) = c("2002","2006","2010","2012")
elec$Date = c(as.Date("2002-09-12"), as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14'))
n=c((0.801*6722*1000),(0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000))
## http://www.statistiCatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf

dfC = data.frame(C=elec$C*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datC = rbind(datC, dfC)
datC = datC[order(datC$collectPeriodFrom),]
datC = datC[-which(datC$collectPeriodFrom>end.date),]
datC = datC[-which(datC$collectPeriodFrom<orig.date),]

datC$collectPeriodFrom.num = julian(datC$collectPeriodFrom,origin=orig.date) #days since origin
datC$collectPeriodTo.num = julian(datC$collectPeriodTo,origin=orig.date) #days since origin
datC$fieldDate.num = floor((datC$collectPeriodFrom.num + datC$collectPeriodTo.num) / 2)
datC$C = datC$C/100

jags_C ='
model{

#observed model
for(i in 1:npolls){
C[i] ~ dnorm(xC[day[i]],precC[i])
}

#dynamic model 
for(i in 2:nperiods){
xC[i] ~ dnorm(xC[i-1],phiC)
}

## priors
phiC ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
}
'

pC = (1 / (datC$C*(1-datC$C)/datC$n)) #binomial
#pC = (1 / (datC$C*(1-datC$C)*datC$n)) #multinomial
data_C = list(C = datC$C, precC = pC, xC = c(rnorm(1,elec[2,4], 0.00001),rep(NA,end.date - orig.date-1)),
              day = datC$fieldDate.num, npolls = nrow(datC), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_C,con="kalman_C.bug")

system.time(jags_mod_C <- jags.model("kalman_C.bug", data = data_C))

system.time(outC <- coda.samples(jags_mod_C,variable.names = c("xC", "C"), n.iter = ninter, n.thin = 1))
#system.time(outC_jags <- jags.samples(jags_mod_C,variable.names = c("xC", "C","phiC" ), n.iter = 5000, n.thin = 10))
sumC = summary(outC)
cred_intC = HPDinterval(outC[,which(regexpr("xC", row.names(sumC$statistics))==1)], 0.95)
#sumC$quantiles[elec.day[3],c(1,5)] 

############ S ###############

datS = na.omit(polls[,c('S','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datS$house = factor(datS$house)
datS = datS[order(datS$collectPeriodFrom),]
datS$collectPeriodFrom = as.Date(datS$collectPeriodFrom)
datS$collectPeriodTo = as.Date(datS$collectPeriodTo)
datS$PublDate = as.Date(datS$PublDate)
orig.date = as.Date("2006-09-16") #day before election 2011
end.date = as.Date('2014-09-14') #election day 2014

elec = data.frame(rbind(c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0),
                        c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                        c(0.2333,0.0542,0.0457,0.0611,0.3101,0.0689,0.0572,0.1286)))
colnames(elec) = c("M","L","KD","C","S","MP","V","SD")
row.names(elec) = c("2002","2006","2010","2012")
elec$Date = c(as.Date("2002-09-12"), as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14'))
n=c((0.801*6722*1000),(0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000))
## http://www.statistiSatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf

dfS = data.frame(S=elec$S*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datS = rbind(datS, dfS)
datS = datS[order(datS$collectPeriodFrom),]
datS = datS[-which(datS$collectPeriodFrom>end.date),]
datS = datS[-which(datS$collectPeriodFrom<orig.date),]

datS$collectPeriodFrom.num = julian(datS$collectPeriodFrom,origin=orig.date) #days since origin
datS$collectPeriodTo.num = julian(datS$collectPeriodTo,origin=orig.date) #days since origin
datS$fieldDate.num = floor((datS$collectPeriodFrom.num + datS$collectPeriodTo.num) / 2)
datS$S = datS$S/100

jags_S ='
model{

#observed model
for(i in 1:npolls){
S[i] ~ dnorm(xS[day[i]],precS[i])
}

#dynamic model 
for(i in 2:nperiods){
xS[i] ~ dnorm(xS[i-1],phiS)
}

## priors
phiS ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
}
'

pS = (1 / (datS$S*(1-datS$S)/datS$n)) #binomial
#pS = (1 / (datS$S*(1-datS$S)*datS$n)) #multinomial
data_S = list(S = datS$S, precS = pS, xS = c(rnorm(1,elec[2,5], 0.00001),rep(NA,end.date - orig.date-1)),
              day = datS$fieldDate.num, npolls = nrow(datS), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_S,con="kalman_S.bug")

system.time(jags_mod_S <- jags.model("kalman_S.bug", data = data_S))

system.time(outS <- coda.samples(jags_mod_S,variable.names = c("xS", "S"), n.iter = ninter, n.thin = 1))
#system.time(outS_jags <- jags.samples(jags_mod_S,variable.names = c("xS", "S","phiS" ), n.iter = 5000, n.thin = 10))
sumS = summary(outS)
cred_intS = HPDinterval(outS[,which(regexpr("xS", row.names(sumS$statistics))==1)], 0.95)
#sumS$quantiles[elec.day[3],c(1,5)] 

############ MP ###############

datMP = na.omit(polls[,c('MP','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datMP$house = factor(datMP$house)
datMP = datMP[order(datMP$collectPeriodFrom),]
datMP$collectPeriodFrom = as.Date(datMP$collectPeriodFrom)
datMP$collectPeriodTo = as.Date(datMP$collectPeriodTo)
datMP$PublDate = as.Date(datMP$PublDate)
orig.date = as.Date("2006-09-16") #day before election 2011
end.date = as.Date('2014-09-14') #election day 2014

elec = data.frame(rbind(c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0),
                        c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                        c(0.2333,0.0542,0.0457,0.0611,0.3101,0.0689,0.0572,0.1286)))
colnames(elec) = c("M","L","KD","C","S","MP","V","SD")
row.names(elec) = c("2002","2006","2010","2012")
elec$Date = c(as.Date("2002-09-12"), as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14'))
n=c((0.801*6722*1000),(0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000))
## http://www.statistiMPatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf

dfMP = data.frame(MP=elec$MP*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datMP = rbind(datMP, dfMP)
datMP = datMP[order(datMP$collectPeriodFrom),]
datMP = datMP[-which(datMP$collectPeriodFrom>end.date),]
datMP = datMP[-which(datMP$collectPeriodFrom<orig.date),]

datMP$collectPeriodFrom.num = julian(datMP$collectPeriodFrom,origin=orig.date) #days since origin
datMP$collectPeriodTo.num = julian(datMP$collectPeriodTo,origin=orig.date) #days since origin
datMP$fieldDate.num = floor((datMP$collectPeriodFrom.num + datMP$collectPeriodTo.num) / 2)
datMP$MP = datMP$MP/100

jags_MP ='
model{

#observed model
for(i in 1:npolls){
MP[i] ~ dnorm(xMP[day[i]],precMP[i])
}

#dynamic model 
for(i in 2:nperiods){
xMP[i] ~ dnorm(xMP[i-1],phiMP)
}

## priors
phiMP ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
}
'

pMP = (1 / (datMP$MP*(1-datMP$MP)/datMP$n)) #binomial
#pMP = (1 / (datMP$MP*(1-datMP$MP)*datMP$n)) #multinomial
data_MP = list(MP = datMP$MP, precMP = pMP, xMP = c(rnorm(1,elec[2,6], 0.00001),rep(NA,end.date - orig.date-1)),
               day = datMP$fieldDate.num, npolls = nrow(datMP), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_MP,con="kalman_MP.bug")

system.time(jags_mod_MP <- jags.model("kalman_MP.bug", data = data_MP))

system.time(outMP <- coda.samples(jags_mod_MP,variable.names = c("xMP", "MP"), n.iter = ninter, n.thin = 1))
#system.time(outMP_jags <- jags.samples(jags_mod_MP,variable.names = c("xMP", "MP","phiMP" ), n.iter = 5000, n.thin = 10))
sumMP = summary(outMP)
cred_intMP = HPDinterval(outMP[,which(regexpr("xMP", row.names(sumMP$statistics))==1)], 0.95)
#sumMP$quantiles[elec.day[3],c(1,5)] 

############ V ###############

datV = na.omit(polls[,c('V','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datV$house = factor(datV$house)
datV = datV[order(datV$collectPeriodFrom),]
datV$collectPeriodFrom = as.Date(datV$collectPeriodFrom)
datV$collectPeriodTo = as.Date(datV$collectPeriodTo)
datV$PublDate = as.Date(datV$PublDate)
orig.date = as.Date("2006-09-16") #day before election 2011
end.date = as.Date('2014-09-14') #election day 2014

elec = data.frame(rbind(c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0),
                        c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                        c(0.2333,0.0542,0.0457,0.0611,0.3101,0.0689,0.0572,0.1286)))
colnames(elec) = c("M","L","KD","C","S","MP","V","SD")
row.names(elec) = c("2002","2006","2010","2012")
elec$Date = c(as.Date("2002-09-12"), as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14'))
n=c((0.801*6722*1000),(0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000))
## http://www.statistiVatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf

dfV = data.frame(V=elec$V*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datV = rbind(datV, dfV)
datV = datV[order(datV$collectPeriodFrom),]
datV = datV[-which(datV$collectPeriodFrom>end.date),]
datV = datV[-which(datV$collectPeriodFrom<orig.date),]

datV$collectPeriodFrom.num = julian(datV$collectPeriodFrom,origin=orig.date) #days since origin
datV$collectPeriodTo.num = julian(datV$collectPeriodTo,origin=orig.date) #days since origin
datV$fieldDate.num = floor((datV$collectPeriodFrom.num + datV$collectPeriodTo.num) / 2)
datV$V = datV$V/100

jags_V ='
model{

#observed model
for(i in 1:npolls){
V[i] ~ dnorm(xV[day[i]],precV[i])
}

#dynamic model 
for(i in 2:nperiods){
xV[i] ~ dnorm(xV[i-1],phiV)
}

## priors
phiV ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
}
'

pV = (1 / (datV$V*(1-datV$V)/datV$n)) #binomial
#pV = (1 / (datV$V*(1-datV$V)*datV$n)) #multinomial
data_V = list(V = datV$V, precV = pV, xV = c(rnorm(1,elec[2,7], 0.00001),rep(NA,end.date - orig.date-1)),
              day = datV$fieldDate.num, npolls = nrow(datV), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_V,con="kalman_V.bug")

system.time(jags_mod_V <- jags.model("kalman_V.bug", data = data_V))

system.time(outV <- coda.samples(jags_mod_V,variable.names = c("xV", "V"), n.iter = ninter, n.thin = 1))
#system.time(outV_jags <- jags.samples(jags_mod_V,variable.names = c("xV", "V","phiV" ), n.iter = 5000, n.thin = 10))
sumV = summary(outV)
cred_intV = HPDinterval(outV[,which(regexpr("xV", row.names(sumV$statistics))==1)], 0.95)
#sumV$quantiles[elec.day[3],c(1,5)] 

############ SD ###############

datSD = na.omit(polls[,c('SD','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datSD$house = factor(datSD$house)
datSD = datSD[order(datSD$collectPeriodFrom),]
datSD$collectPeriodFrom = as.Date(datSD$collectPeriodFrom)
datSD$collectPeriodTo = as.Date(datSD$collectPeriodTo)
datSD$PublDate = as.Date(datSD$PublDate)
orig.date = as.Date("2006-09-16") #day before election 2006
end.date = as.Date('2014-09-14') #election day 2014

elec = data.frame(rbind(c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0),
                        c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                        c(0.2333,0.0542,0.0457,0.0611,0.3101,0.0689,0.0572,0.1286)))
colnames(elec) = c("M","L","KD","C","S","MP","V","SD")
row.names(elec) = c("2002","2006","2010","2012")
elec$Date = c(as.Date("2002-09-12"), as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14'))
n=c((0.801*6722*1000),(0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000))
## http://www.statistiSDatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf

dfSD = data.frame(SD=elec$SD*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datSD = rbind(datSD, dfSD)
datSD = datSD[order(datSD$collectPeriodFrom),]
datSD = datSD[-which(datSD$collectPeriodFrom>end.date),]
datSD = datSD[-which(datSD$collectPeriodFrom<orig.date),]

datSD$collectPeriodFrom.num = julian(datSD$collectPeriodFrom,origin=orig.date) #days since origin
datSD$collectPeriodTo.num = julian(datSD$collectPeriodTo,origin=orig.date) #days since origin
datSD$fieldDate.num = floor((datSD$collectPeriodFrom.num + datSD$collectPeriodTo.num) / 2)
datSD$SD = datSD$SD/100

jags_SD ='
model{

#observed model
for(i in 1:npolls){
SD[i] ~ dnorm(xSD[day[i]],precSD[i])
}

#dynamic model 
for(i in 2:nperiods){
xSD[i] ~ dnorm(xSD[i-1],phiSD)
}

## priors
phiSD ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
}
'

pSD = (1 / (datSD$SD*(1-datSD$SD)/datSD$n)) #binomial
#pSD = (1 / (datSD$SD*(1-datSD$SD)*datSD$n)) #multinomial
data_SD = list(SD = datSD$SD, precSD = pSD, xSD = c(rnorm(1,elec[2,8], 0.00001),rep(NA,end.date - orig.date-1)),
               day = datSD$fieldDate.num, npolls = nrow(datSD), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_SD,con="kalman_SD.bug")

system.time(jags_mod_SD <- jags.model("kalman_SD.bug", data = data_SD))

system.time(outSD <- coda.samples(jags_mod_SD,variable.names = c("xSD", "SD"), n.iter = ninter, n.thin = 1))
#system.time(outSD_jags <- jags.samples(jags_mod_SD,variable.names = c("xSD", "SD","phiSD" ), n.iter = 5000, n.thin = 10))
sumSD = summary(outSD)
cred_intSD = HPDinterval(outSD[,which(regexpr("xSD", row.names(sumSD$statistics))==1)], 0.95)
#sumSD$quantiles[elec.day[3],c(1,5)] 

#########################################################
################# model evaluation ######################
#########################################################
meanM = sumM$statistics[which(regexpr("xM", row.names(sumM$statistics))==1),1]
meanL = sumL$statistics[which(regexpr("xL", row.names(sumL$statistics))==1),1]
meanKD = sumKD$statistics[which(regexpr("xKD", row.names(sumKD$statistics))==1),1]
meanC = sumC$statistics[which(regexpr("xC", row.names(sumC$statistics))==1),1]
meanS = sumS$statistics[which(regexpr("xS", row.names(sumS$statistics))==1),1]
meanMP = sumMP$statistics[which(regexpr("xMP", row.names(sumMP$statistics))==1),1]
meanV = sumV$statistics[which(regexpr("xV", row.names(sumV$statistics))==1),1]
meanSD = sumSD$statistics[which(regexpr("xSD", row.names(sumSD$statistics))==1),1]

rsimM = outM[sample(1:ninter,1),]
rsimL = outL[sample(1:ninter,1),]
rsimKD = outKD[sample(1:ninter,1),]
rsimC = outC[sample(1:ninter,1),]
rsimS = outS[sample(1:ninter,1),]
rsimMP = outMP[sample(1:ninter,1),]
rsimV = outV[sample(1:ninter,1),]
rsimSD = outSD[sample(1:ninter,1),]



###### resiudals - posterior distribution #####

resM = datM$M-rsimM[[1]][which(regexpr("xM", row.names(sumM$statistics))==1)][datM$fieldDate.num]
resL = datL$L-rsimL[[1]][which(regexpr("xL", row.names(sumL$statistics))==1)][datL$fieldDate.num]
resKD = datKD$KD-rsimKD[[1]][which(regexpr("xKD", row.names(sumKD$statistics))==1)][datKD$fieldDate.num]
resC = datC$C-rsimC[[1]][which(regexpr("xC", row.names(sumC$statistics))==1)][datC$fieldDate.num]
resS = datS$S-rsimS[[1]][which(regexpr("xS", row.names(sumS$statistics))==1)][datS$fieldDate.num]
resMP = datMP$MP-rsimMP[[1]][which(regexpr("xMP", row.names(sumMP$statistics))==1)][datMP$fieldDate.num]
resV = datV$V-rsimV[[1]][which(regexpr("xV", row.names(sumV$statistics))==1)][datV$fieldDate.num]
resSD = datSD$SD-rsimSD[[1]][which(regexpr("xSD", row.names(sumSD$statistics))==1)][datSD$fieldDate.num]


####### y^rep #####
yrepM = sapply(1:nrow(datM), function(s) rnorm(10000,rsimM[[1]][datM$fieldDate.num][s], 1/pM[s] ))
yrepL = sapply(1:nrow(datL), function(s) rnorm(10000,rsimL[[1]][datL$fieldDate.num][s], 1/pL[s] ))
yrepKD = sapply(1:nrow(datKD), function(s) rnorm(10000,rsimKD[[1]][datKD$fieldDate.num][s], 1/pKD[s] ))
yrepC = sapply(1:nrow(datC), function(s) rnorm(10000,rsimC[[1]][datC$fieldDate.num][s], 1/pC[s] ))
yrepS = sapply(1:nrow(datS), function(s) rnorm(10000,rsimS[[1]][datS$fieldDate.num][s], 1/pS[s] ))
yrepMP = sapply(1:nrow(datMP), function(s) rnorm(10000,rsimMP[[1]][datMP$fieldDate.num][s], 1/pMP[s] ))
yrepV = sapply(1:nrow(datV), function(s) rnorm(10000,rsimV[[1]][datV$fieldDate.num][s], 1/pV[s] ))
yrepSD = sapply(1:nrow(datSD), function(s) rnorm(10000,rsimSD[[1]][datSD$fieldDate.num][s], 1/pSD[s] ))

####### y^rep min ##### 

par(mfrow=c(3,3))
min_repM = apply(yrepM,2,min)
min_M = min(datM$M)
hist(min_repM, main="M", col="blue", xlab="Minimum value in replicated data", las=1)
abline(v=min_M, lty=1, lwd=2)
sum(ifelse(min_repM>min_M,1,0))/length(datM$M) 

min_repL = apply(yrepL,2,min)
min_L = min(datL$L)
hist(min_repL, main="L", col="lightblue3", xlab="Minimum value in replicated data", las=1)
abline(v=min_L, lty=1, lwd=2)
sum(ifelse(min_repL>min_L,1,0))/length(datL$L) 

min_repKD = apply(yrepKD,2,min)
min_KD = min(datKD$KD)
hist(min_repKD, main="KD", col="darkblue", xlab="Minimum value in replicated data", las=1)
abline(v=min_KD, lty=1, lwd=2)
sum(ifelse(min_repKD>min_KD,1,0))/length(datKD$KD) 

min_repC = apply(yrepC,2,min)
min_C = min(datC$C)
hist(min_repC, main="C", col="chartreuse3", xlab="Minimum value in replicated data", las=1)
abline(v=min_C, lty=1, lwd=2)
sum(ifelse(min_repC>min_C,1,0))/length(datC$C) 

min_repS = apply(yrepS,2,min)
min_S = min(datS$S)
hist(min_repS, main="S", col="red", xlab="Minimum value in replicated data", las=1)
abline(v=min_S, lty=1, lwd=2)
sum(ifelse(min_repS>min_S,1,0))/length(datS$S) 

min_repMP = apply(yrepMP,2,min)
min_MP = min(datMP$MP)
hist(min_repMP, main="MP", col="forestgreen", xlab="Minimum value in replicated data", las=1)
abline(v=min_MP, lty=1, lwd=2)
sum(ifelse(min_repMP>min_MP,1,0))/length(datMP$MP) 

min_repV = apply(yrepV,2,min)
min_V = min(datV$V)
hist(min_repV, main="V", col="darkred", xlab="Minimum value in replicated data", las=1)
abline(v=min_V, lty=1, lwd=2)
sum(ifelse(min_repV>min_V,1,0))/length(datV$V) 


min_repSD = apply(yrepSD,2,min)
min_SD = min(datSD$SD)
hist(min_repSD, main="SD", col="skyblue3", xlab="Minimum value in replicated data", las=1)
abline(v=min_SD, lty=1, lwd=2)
sum(ifelse(min_repSD>min_SD,1,0))/length(datSD$SD) 
par(mfrow=c(1,1))

####### y^rep max ##### 
par(mfrow=c(3,3))
max_repM = apply(yrepM,2,max)
max_M = max(datM$M)
hist(max_repM, main="M", col="blue", xlab="Maximum value in replicated data", las=1)
abline(v=max_M, lty=1, lwd=2)
sum(ifelse(max_repM<max_M,1,0))/length(datM$M) 

max_repL = apply(yrepL,2,max)
max_L = max(datL$L)
hist(max_repL, main="L", col="lightblue3", xlab="Maximum observation in replicated data", las=1)
abline(v=max_L, lty=1, lwd=2)
sum(ifelse(max_repL<max_L,1,0))/length(datL$L) 

max_repKD = apply(yrepKD,2,max)
max_KD = max(datKD$KD)
hist(max_repKD, main="KD", col="darkblue", xlab="Maximum observation in replicated data", las=1)
abline(v=max_KD, lty=1, lwd=2)
sum(ifelse(max_repKD<max_KD,1,0))/length(datKD$KD) 

max_repC = apply(yrepC,2,max)
max_C = max(datC$C)
hist(max_repC, main="C", col="chartreuse3", xlab="Maximum observation in replicated data", las=1)
abline(v=max_C, lty=1, lwd=2)
sum(ifelse(max_repC<max_C,1,0))/length(datC$C) 

max_repS = apply(yrepS,2,max)
max_S = max(datS$S)
hist(max_repS, main="S", col="red", xlab="Maximum value in replicated data", las=1)
abline(v=max_S, lty=1, lwd=2)
sum(ifelse(max_repS<max_S,1,0))/length(datS$S) 

max_repMP = apply(yrepMP,2,max)
max_MP = max(datMP$MP)
hist(max_repMP, main="MP", col="forestgreen", xlab="Maximum value in replicated data", las=1)
abline(v=max_MP, lty=1, lwd=2)
sum(ifelse(max_repMP<max_MP,1,0))/length(datMP$MP) 

max_repV = apply(yrepV,2,max)
max_V = max(datV$V)
hist(max_repV, main="V", col="darkred", xlab="Maximum value in replicated data", las=1)
abline(v=max_V, lty=1, lwd=2)
sum(ifelse(max_repV<max_V,1,0))/length(datV$V) 

max_repSD = apply(yrepSD,2,max)
max_SD = max(datSD$SD)
hist(max_repSD, main="SD", col="skyblue3", xlab="Maximum observation in replicated data", las=1)
abline(v=max_SD, lty=1, lwd=2)
sum(ifelse(max_repSD<max_SD,1,0))/length(datSD$SD) 
par(mfrow=c(1,1))

####### y^rep mean #########

par(mfrow=c(3,3))
mean_repM = apply(yrepM,2,mean)
mean_M = mean(datM$M)
hist(mean_repM, main="Histogram of mean replicated M proportion", col="blue", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_M, lty=1, lwd=2)
sum(ifelse(mean_repM<mean_M,1,0))/length(datM$M) 

mean_repL = apply(yrepL,2,mean)
mean_L = mean(datL$L)
hist(mean_repL, main="Histogram of mean replicated L proportion", col="lightblue3", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_L, lty=1, lwd=2)
sum(ifelse(mean_repL<mean_L,1,0))/length(datL$L) 

mean_repKD = apply(yrepKD,2,mean)
mean_KD = mean(datKD$KD)
hist(mean_repKD, main="Histogram of mean replicated KD proportion", col="darkblue", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_KD, lty=1, lwd=2)
sum(ifelse(mean_repKD<mean_KD,1,0))/length(datKD$KD) 

mean_repC = apply(yrepC,2,mean)
mean_C = mean(datC$C)
hist(mean_repC, main="Histogram of mean replicated C proportion", col="chartreuse3", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_C, lty=1, lwd=2)
sum(ifelse(mean_repC<mean_C,1,0))/length(datC$C) 

mean_repS = apply(yrepS,2,mean)
mean_S = mean(datS$S)
hist(mean_repS, main="Histogram of mean replicated S proportion", col="red", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_S, lty=1, lwd=2)
sum(ifelse(mean_repS<mean_S,1,0))/length(datS$S) 

mean_repMP = apply(yrepMP,2,mean)
mean_MP = mean(datMP$MP)
hist(mean_repMP, main="Histogram of mean replicated MP proportion", col="forestgreen", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_MP, lty=1, lwd=2)
sum(ifelse(mean_repMP<mean_MP,1,0))/length(datMP$MP) 

mean_repV = apply(yrepV,2,mean)
mean_V = mean(datV$V)
hist(mean_repV, main="Histogram of mean replicated V proportion", col="darkred", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_V, lty=1, lwd=2)
sum(ifelse(mean_repV<mean_V,1,0))/length(datV$V) 

mean_repSD = apply(yrepSD,2,mean)
mean_SD = mean(datSD$SD)
hist(mean_repSD, main="Histogram of mean replicated SD proportion", col="skyblue3", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_SD, lty=1, lwd=2)
sum(ifelse(mean_repSD<mean_SD,1,0))/length(datSD$SD) 
par(mfrow=c(1,1))


####### y^rep mean vs y plot ######
par(mfrow=c(1,2))
plot(datM$M*100, apply(yrep,2,mean)*100, xlab="Observed y", ylab="Expected y", main="Basic model", las=1, col="blue3")
abline(a=0, b=1)
plot(datM$M*100, apply(yrep_house,2,mean)*100, xlab="Observed y", ylab="Expected y", main="Basic + house model" ,las=1, col="blue3")
abline(a=0, b=1)
par(mfrow=c(1,1))


####### y^rep mean vs y plot ######






#################################################
##################### PLOTS #####################
#################################################
#orig.date = as.Date("2002-09-11") #day before election 2011
#end.date = as.Date('2014-09-14') #election day 2014

#### plots M #####

library(ggplot2)
meanM = sumM$statistics[which(regexpr("xM", row.names(sumM$statistics))==1),1]
low2 = (meanM - 1.96 * sumM$statistics[which(regexpr("xM", row.names(sumM$statistics))==1),2])*100
high2 = (meanM + 1.96 * sumM$statistics[which(regexpr("xM", row.names(sumM$statistics))==1),2])*100
df = data.frame(xM = meanM , low=cred_intM[[1]][,1]*100, high=cred_intM[[1]][,2]*100,
                time=seq(as.Date('2006-09-16'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1)))),
                low2=low2, high2=high2)
ggplot(df) +
  aes(x = time, y = xM*100) +
  geom_line(col="blue", alpha=1)  +
  #geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="blue") + 
  ggtitle(paste("M")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-16'),by='days',
                                   length=length(c(0.2623,rep(NA,end.date - orig.date-1))))[datM$fieldDate.num], 
                             y=datM$M*100, house=datM$house), aes(x=x, y=y), alpha = 1, color="blue", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for M", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

#### plots L #####

library(ggplot2)
meanL = sumL$statistics[which(regexpr("xL", row.names(sumL$statistics))==1),1]
low2 = (meanL - 1.96 * sumL$statistics[which(regexpr("xL", row.names(sumL$statistics))==1),2])*100
high2 = (meanL + 1.96 * sumL$statistics[which(regexpr("xL", row.names(sumL$statistics))==1),2])*100
df = data.frame(xL = meanL , low=cred_intL[[1]][,1]*100, high=cred_intL[[1]][,2]*100,
                time=seq(as.Date('2006-09-16'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1)))),
                low2=low2, high2=high2)
ggplot(df) +
  aes(x = time, y = xL*100) +
  geom_line(col="lightblue3", alpha=1)  +
  #geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="lightblue3") +
  ggtitle(paste("L")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-16'),by='days',
                                   length=length(c(0.2623,rep(NA,end.date - orig.date-1))))[datL$fieldDate.num], 
                             y=datL$L*100, house=datL$house), aes(x=x, y=y), alpha = 1, color="lightblue3", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for L", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())



#### plots KD #####

library(ggplot2)
meanKD = sumKD$statistics[which(regexpr("xKD", row.names(sumKD$statistics))==1),1]
low2 = (meanKD - 1.96 * sumKD$statistics[which(regexpr("xKD", row.names(sumKD$statistics))==1),2])*100
high2 = (meanKD + 1.96 * sumKD$statistics[which(regexpr("xKD", row.names(sumKD$statistics))==1),2])*100
df = data.frame(xKD = meanKD , low=cred_intKD[[1]][,1]*100, high=cred_intKD[[1]][,2]*100,
                time=seq(as.Date('2006-09-16'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1)))),
                low2=low2, high2=high2)
ggplot(df) +
  aes(x = time, y = xKD*100) +
  geom_line(col="darkblue", alpha=1)  +
 # geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="darkblue") +
  ggtitle(paste("KD")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-16'),by='days',
                                   length=length(c(0.2623,rep(NA,end.date - orig.date-1))))[datKD$fieldDate.num], 
                             y=datKD$KD*100, house=datKD$house), aes(x=x, y=y), alpha = 1, color="darkblue", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for KD", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())


#### plots C #####

library(ggplot2)
meanC = sumC$statistics[which(regexpr("xC", row.names(sumC$statistics))==1),1]
low2 = (meanC - 1.96 * sumC$statistics[which(regexpr("xC", row.names(sumC$statistics))==1),2])*100
high2 = (meanC + 1.96 * sumC$statistics[which(regexpr("xC", row.names(sumC$statistics))==1),2])*100
df = data.frame(xC = meanC , low=cred_intC[[1]][,1]*100, high=cred_intC[[1]][,2]*100,
                time=seq(as.Date('2006-09-16'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1)))),
                low2=low2, high2=high2)
ggplot(df) +
  aes(x = time, y = xC*100) +
  geom_line(col="chartreuse3", alpha=1)  +
  #geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="chartreuse3") +
  ggtitle(paste("C")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-16'),by='days',
             length=length(c(0.2623,rep(NA,end.date - orig.date-1))))[datC$fieldDate.num], 
             y=datC$C*100, house=datC$house), aes(x=x, y=y), alpha = 1, color="chartreuse3", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for C", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())


#### plots S #####

library(ggplot2)
meanS = sumS$statistics[which(regexpr("xS", row.names(sumS$statistics))==1),1]
low2 = (meanS - 1.96 * sumS$statistics[which(regexpr("xS", row.names(sumS$statistics))==1),2])*100
high2 = (meanS + 1.96 * sumS$statistics[which(regexpr("xS", row.names(sumS$statistics))==1),2])*100
df = data.frame(xS = meanS , low=cred_intS[[1]][,1]*100, high=cred_intS[[1]][,2]*100,
                time=seq(as.Date('2006-09-16'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1)))),
                low2=low2, high2=high2)
ggplot(df) +
  aes(x = time, y = xS*100) +
  geom_line(col="red", alpha=1)  +
  #geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="red") +
  ggtitle(paste("S")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-16'),by='days',
                                   length=length(c(0.2623,rep(NA,end.date - orig.date-1))))[datS$fieldDate.num], 
                             y=datS$S*100, house=datS$house), aes(x=x, y=y), alpha = 1, color="red", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for S", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())


#### plots MP #####

library(ggplot2)
meanMP = sumMP$statistics[which(regexpr("xMP", row.names(sumMP$statistics))==1),1]
low2 = (meanMP - 1.96 * sumMP$statistics[which(regexpr("xMP", row.names(sumMP$statistics))==1),2])*100
high2 = (meanMP + 1.96 * sumMP$statistics[which(regexpr("xMP", row.names(sumMP$statistics))==1),2])*100
df = data.frame(xMP = meanMP , low=cred_intMP[[1]][,1]*100, high=cred_intMP[[1]][,2]*100,
                time=seq(as.Date('2006-09-16'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1)))),
                low2=low2, high2=high2)
ggplot(df) +
  aes(x = time, y = xMP*100) +
  geom_line(col="forestgreen", alpha=1)  +
#  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="forestgreen") +
  ggtitle(paste("MP")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-16'),by='days',
                                   length=length(c(0.2623,rep(NA,end.date - orig.date-1))))[datMP$fieldDate.num], 
                             y=datMP$MP*100, house=datMP$house), aes(x=x, y=y), alpha = 1, color="forestgreen", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for MP", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

#### plots V #####

library(ggplot2)
meanV = sumV$statistics[which(regexpr("xV", row.names(sumV$statistics))==1),1]
low2 = (meanV - 1.96 * sumV$statistics[which(regexpr("xV", row.names(sumV$statistics))==1),2])*100
high2 = (meanV + 1.96 * sumV$statistics[which(regexpr("xV", row.names(sumV$statistics))==1),2])*100
df = data.frame(xV = meanV , low=cred_intV[[1]][,1]*100, high=cred_intV[[1]][,2]*100,
                time=seq(as.Date('2006-09-16'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1)))),
                low2=low2, high2=high2)
ggplot(df) +
  aes(x = time, y = xV*100) +
  geom_line(col="darkred", alpha=1)  +
  #geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="darkred") +
  ggtitle(paste("V")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-16'),by='days',
                                   length=length(c(0.2623,rep(NA,end.date - orig.date-1))))[datV$fieldDate.num], 
                             y=datV$V*100, house=datV$house), aes(x=x, y=y), alpha = 1, color="darkred", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for V", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())


#### plots SD #####

library(ggplot2)
meanSD = sumSD$statistics[which(regexpr("xSD", row.names(sumSD$statistics))==1),1]
low2 = (meanSD - 1.96 * sumSD$statistics[which(regexpr("xSD", row.names(sumSD$statistics))==1),2])*100
high2 = (meanSD + 1.96 * sumSD$statistics[which(regexpr("xSD", row.names(sumSD$statistics))==1),2])*100
df = data.frame(xSD = meanSD , low=cred_intSD[[1]][,1]*100, high=cred_intSD[[1]][,2]*100,
                time=seq(as.Date('2006-09-16'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1)))),
                low2=low2, high2=high2)
ggplot(df) +
  aes(x = time, y = xSD*100) +
  geom_line(col="skyblue3", alpha=1)  +
 # geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="skyblue3") +
  ggtitle(paste("SD")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-16'),by='days',
                                   length=length(c(0.2623,rep(NA,end.date - orig.date-1))))[datSD$fieldDate.num], 
                             y=datSD$SD*100, house=datSD$house), aes(x=x, y=y), alpha = 1, color="skyblue3", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for SD", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())








