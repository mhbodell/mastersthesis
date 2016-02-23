data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)
parties = c("M", "FP", "C", "KD", "S", "V", "MP", "SD")

predData = polls[, c(parties, "n", "house")]
predData[, parties] = predData[, parties]/100
predData$time = as.Date(polls$collectPeriodTo)
predData$publDate = as.Date(polls$PublDate)
predData$periodFrom = as.Date(polls$collectPeriodFrom)
predData$periodTo = as.Date(polls$collectPeriodTo)
predData$house = factor(predData$house)

predData = predData[!(is.na(predData$periodFrom) | is.na(predData$periodTo)), ]
predData = predData[!(is.na(predData$house) | is.na(predData$n)), ]
predData = predData[!(is.na(predData$publDate)), ]
predData = predData[-which(rowSums(predData[, 1:8], na.rm = TRUE) == 1),]

predData = predData[predData$time > as.Date("2006-09-17") & !is.na(predData$time),]
polls2 = predData[order(predData$periodFrom),]


orig.date = as.Date("2006-09-17")
end.date = as.Date(polls2$periodTo[length(polls2$periodTo)])

polls2$collectPeriodFrom.num = julian(polls2$periodFrom, origin=orig.date) #days since origin
polls2$collectPeriodTo.num = julian(polls2$periodTo,origin=orig.date) #days since origin
polls2$fieldDate.num = floor((polls2$collectPeriodFrom.num + polls2$collectPeriodTo.num) / 2)

polls2$precM = 1 / (polls2$M*(1-polls2$M)/polls2$n)
polls2$precL = 1 / (polls2$FP*(1-polls2$FP)/polls2$n)
polls2$precC = 1 / (polls2$C*(1-polls2$C)/polls2$n)
polls2$precKD = 1 / (polls2$KD*(1-polls2$KD)/polls2$n)
polls2$precS = 1 / (polls2$S*(1-polls2$S)/polls2$n)
polls2$precMP = 1 / (polls2$MP*(1-polls2$MP)/polls2$n)
polls2$precV = 1 / (polls2$V*(1-polls2$V)/polls2$n)
polls2$precSD = 1 / (polls2$SD*(1-polls2$SD)/polls2$n)
polls2$precSD = ifelse(is.na(polls2$precSD),1000000,polls2$SD) #to get precisions

jags_uni ='
model{

#observed model
for(i in 1:npolls){
M[i] ~ dnorm(xM[day[i]], precM[i])
L[i] ~ dnorm(xL[day[i]], precL[i])
C[i] ~ dnorm(xC[day[i]], precC[i])
KD[i] ~ dnorm(xKD[day[i]], precKD[i])
S[i] ~ dnorm(xS[day[i]], precS[i])
MP[i] ~ dnorm(xMP[day[i]], precMP[i])
V[i] ~ dnorm(xV[day[i]], precV[i])
SD[i] ~ dnorm(xSD[day[i]], precSD[i])
}

#dynamic model 
for(i in 2:nperiods){
xM[i] ~ dnorm(xM[i-1], phiM)
xL[i] ~ dnorm(xL[i-1], phiL)
xC[i] ~ dnorm(xC[i-1], phiC)
xKD[i] ~ dnorm(xKD[i-1], phiKD)
xS[i] ~ dnorm(xS[i-1], phiS)
xMP[i] ~ dnorm(xMP[i-1], phiMP)
xV[i] ~ dnorm(xV[i-1], phiV)
xSD[i] ~ dnorm(xSD[i-1], phiSD)
}

## priors
omega ~ dgamma(1, 1)
phiM <- 1/pow(omega,2)
phiL <- 1/pow(omega,2)
phiC <- 1/pow(omega,2)
phiKD <- 1/pow(omega,2)
phiS <- 1/pow(omega,2)
phiMP <- 1/pow(omega,2)
phiV <- 1/pow(omega,2)
phiSD <- 1/pow(omega,2)
}
'

data_uni = list(M = polls2$M, L = polls2$L, C = polls2$C, KD = polls2$KD,
                S = polls2$S, MP = polls2$MP, V = polls2$V, SD = polls2$SD,
                precM = polls2$precM, precL = polls2$precL, precC = polls2$precC, precKD = polls2$precKD,
                precS = polls2$precS, precMP = polls2$precMP, precV = polls2$precV, precSD = polls2$precSD,
                xM = c(0.2623,rep(NA,end.date - orig.date-1)), xL = c(0.0754,rep(NA,end.date - orig.date-1)),
                xC = c(0.0788,rep(NA,end.date - orig.date-1)), xKD = c(0.0659,rep(NA,end.date - orig.date-1)),
                xS = c(0.3499,rep(NA,end.date - orig.date-1)), xMP = c(0.0524,rep(NA,end.date - orig.date-1)),
                xV = c(0.0585,rep(NA,end.date - orig.date-1)), xSD = c(0,rep(NA,end.date - orig.date-1)),
                day = polls2$fieldDate.num, npolls = nrow(polls2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_uni,con="kalman_uni.bug")

system.time(jags_mod_uni <- jags.model("kalman_uni.bug", data = data_uni))

system.time(out_uni <- coda.samples( jags_mod_uni,variable.names = c("xM", "xL", "xC", "xKD",
                                                                     "xS", "xMP", "xV", "xSD"), n.iter = 1000,n.thin = 10))
sum_uni = summary(out_uni)

ind = c(3425, 2*3425, 3*3425, 4*3425, 5*3425, 6*3425, 7*3425, 8*3425)
sum_uni$statistics[1:2,1]

plot(1:length(sum_uni$statistics[1:ind[1],1]),sum_uni$statistics[1:ind[1],1], type="l")
points(polls2$fieldDate.num,polls2$C)

######## M ###########
library(rjags)
data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)

datM = na.omit(polls[,c('M','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datM$house = factor(datM$house)
datM = datM[order(datM$collectPeriodFrom),]
datM$collectPeriodFrom = as.Date(datM$collectPeriodFrom)
datM$collectPeriodTo = as.Date(datM$collectPeriodTo)
datM$PublDate = as.Date(datM$PublDate)
orig.date = as.Date("2006-09-17")
end.date = as.Date(datM$collectPeriodTo[length(datM$collectPeriodTo)])
datM =  datM[-which(datM$collectPeriodFrom<orig.date),]

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
omega ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiM <- 1/pow(omega,2)
}
'

pM = (1 / (datM$M*(1-datM$M)/datM$n))
data_M = list(M = datM$M, precM = pM, xM = c(0.2623,rep(NA,end.date - orig.date-1)),
              day = datM$fieldDate.num, npolls = nrow(datM), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_M,con="kalman_M.bug")
                 
system.time(jags_mod_M <- jags.model("kalman_M.bug", data = data_M))
                 
system.time(outM <- coda.samples(jags_mod_M,variable.names = c("xM", "M" ), n.iter = 5000, n.thin = 100))
sumM = summary(outM)
cred_intM = HPDinterval(outM[,760:length(sumM$statistics[,1])], 0.95)

###### L ######
datL = na.omit(polls[,c('FP','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datL = datL[order(datL$collectPeriodFrom),]
datL$collectPeriodFrom = as.Date(datL$collectPeriodFrom)
datL$collectPeriodTo = as.Date(datL$collectPeriodTo)
datL$PublDate = as.Date(datL$PublDate)
orig.date = as.Date("2006-09-17")
end.date = as.Date(datL$collectPeriodTo[length(datL$collectPeriodTo)])
datL =  datL[-which(datL$collectPeriodFrom<orig.date),]
datL$collectPeriodFrom.num = julian(datL$collectPeriodFrom,origin=orig.date) #days since origin
datL$collectPeriodTo.num = julian(datL$collectPeriodTo,origin=orig.date) #days since origin
datL$fieldDate.num = floor((datL$collectPeriodFrom.num + datL$collectPeriodTo.num) / 2)
datL$L = datL$FP/100

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
omega ~ dgamma(1, 1)
phiL <- 1/pow(omega,2)
}
'
pL = (1 / (datL$L*(1-datL$L)/datL$n))
data_L = list(L = datL$L, precL = pL, xL = c(0.0754,rep(NA,end.date - orig.date-1)),
              day = datL$fieldDate.num, npolls = nrow(datL), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_L,con="kalman_L.bug")
                 
system.time(jags_mod_L <- jags.model("kalman_L.bug", data = data_L))
                 
system.time(outL <- coda.samples( jags_mod_L,variable.names = c("xL", "L" ), n.iter = 5000, n.thin = 100))
sumL = summary(outL)
cred_intL = HPDinterval(outL[,760:length(sumL$statistics[,1])], 0.95)


###### KD ######
datKD = na.omit(polls[,c('KD','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datKD = datKD[order(datKD$collectPeriodFrom),]
datKD$collectPeriodFrom = as.Date(datKD$collectPeriodFrom)
datKD$collectPeriodTo = as.Date(datKD$collectPeriodTo)
datKD$PublDate = as.Date(datKD$PublDate)
orig.date = as.Date("2006-09-17")
end.date = as.Date(datKD$collectPeriodTo[length(datKD$collectPeriodTo)])
datKD =  datKD[-which(datKD$collectPeriodFrom<orig.date),]
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
omega ~ dgamma(1, 1)
phiKD <- 1/pow(omega,2)
}
'
pKD = (1 / (datKD$KD*(1-datKD$KD)/datKD$n))
data_KD = list(KD = datKD$KD, precKD = pKD, xKD = c(0.0659,rep(NA,end.date - orig.date-1)),
              day = datKD$fieldDate.num, npolls = nrow(datKD), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_KD,con="kalman_KD.bug")
                 
system.time(jags_mod_KD <- jags.model("kalman_KD.bug", data = data_KD))
                 
system.time(outKD <- coda.samples( jags_mod_KD,variable.names = c("xKD", "KD" ), n.iter = 5000, n.thin = 100))
sumKD = summary(outKD)
cred_intKD = HPDinterval(outKD[,760:length(sumKD$statistics[,1])], 0.95)

###### C ######
datC = na.omit(polls[,c('C','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datC = datC[order(datC$collectPeriodFrom),]
datC$collectPeriodFrom = as.Date(datC$collectPeriodFrom)
datC$collectPeriodTo = as.Date(datC$collectPeriodTo)
datC$PublDate = as.Date(datC$PublDate)
orig.date = as.Date("2006-09-17")
end.date = as.Date(datC$collectPeriodTo[length(datC$collectPeriodTo)])
datC =  datC[-which(datC$collectPeriodFrom<orig.date),]
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
omega ~ dgamma(1, 1)
phiC <- 1/pow(omega,2)
}
'
pC = (1 / (datC$C*(1-datC$C)/datC$n))
data_C = list(C = datC$C, precC = pC, xC = c(0.0788,rep(NA,end.date - orig.date-1)),
              day = datC$fieldDate.num, npolls = nrow(datC), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_C,con="kalman_C.bug")

system.time(jags_mod_C <- jags.model("kalman_C.bug", data = data_C))

system.time(outC <- coda.samples( jags_mod_C,variable.names = c("xC", "C" ), n.iter = 5000, n.thin = 100))
sumC = summary(outC)
cred_intC = HPDinterval(outC[,760:length(sumC$statistics[,1])], 0.95)


###### S ######
datS = na.omit(polls[,c('S','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datS = datS[order(datS$collectPeriodFrom),]
datS$collectPeriodFrom = as.Date(datS$collectPeriodFrom)
datS$collectPeriodTo = as.Date(datS$collectPeriodTo)
datS$PublDate = as.Date(datS$PublDate)
orig.date = as.Date("2006-09-17")
end.date = as.Date(datS$collectPeriodTo[length(datS$collectPeriodTo)])
datS =  datS[-which(datS$collectPeriodFrom<orig.date),]
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
omega ~ dgamma(1, 1)
phiS <- 1/pow(omega,2)
}
'
pS = (1 / (datS$S*(1-datS$S)/datS$n))
data_S = list(S = datS$S, precS = pS, xS = c(0.3499,rep(NA,end.date - orig.date-1)),
              day = datS$fieldDate.num, npolls = nrow(datS), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_S,con="kalman_S.bug")

system.time(jags_mod_S <- jags.model("kalman_S.bug", data = data_S))

system.time(outS <- coda.samples( jags_mod_S,variable.names = c("xS", "S" ), n.iter = 5000, n.thin = 100))
sumS = summary(outS)
cred_intS = HPDinterval(outS[,760:length(sumS$statistics[,1])], 0.95)

###### MP ######
datMP = na.omit(polls[,c('MP','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datMP = datMP[order(datMP$collectPeriodFrom),]
datMP$collectPeriodFrom = as.Date(datMP$collectPeriodFrom)
datMP$collectPeriodTo = as.Date(datMP$collectPeriodTo)
datMP$PublDate = as.Date(datMP$PublDate)
orig.date = as.Date("2006-09-17")
end.date = as.Date(datMP$collectPeriodTo[length(datMP$collectPeriodTo)])
datMP =  datMP[-which(datMP$collectPeriodFrom<orig.date),]
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
omega ~ dgamma(1, 1)
phiMP <- 1/pow(omega,2)
}
'
pMP = (1 / (datMP$MP*(1-datMP$MP)/datMP$n))
data_MP = list(MP = datMP$MP, precMP = pMP, xMP = c(0.0524,rep(NA,end.date - orig.date-1)),
               day = datMP$fieldDate.num, npolls = nrow(datMP), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_MP,con="kalman_MP.bug")

system.time(jags_mod_MP <- jags.model("kalman_MP.bug", data = data_MP))

system.time(outMP <- coda.samples( jags_mod_MP,variable.names = c("xMP", "MP" ), n.iter = 5000, n.thin = 100))
sumMP = summary(outMP)
cred_intMP = HPDinterval(outMP[,760:length(sumMP$statistics[,1])], 0.95)

###### V ######
datV = na.omit(polls[,c('V','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datV = datV[order(datV$collectPeriodFrom),]
datV$collectPeriodFrom = as.Date(datV$collectPeriodFrom)
datV$collectPeriodTo = as.Date(datV$collectPeriodTo)
datV$PublDate = as.Date(datV$PublDate)
orig.date = as.Date("2006-09-17")
end.date = as.Date(datV$collectPeriodTo[length(datV$collectPeriodTo)])
datV =  datV[-which(datV$collectPeriodFrom<orig.date),]
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
omega ~ dgamma(1, 1)
phiV <- 1/pow(omega,2)
}
'
pV = (1 / (datV$V*(1-datV$V)/datV$n))
data_V = list(V = datV$V, precV = pV, xV = c(0.0585,rep(NA,end.date - orig.date-1)),
              day = datV$fieldDate.num, npolls = nrow(datV), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_V,con="kalman_V.bug")

system.time(jags_mod_V <- jags.model("kalman_V.bug", data = data_V))

system.time(outV <- coda.samples( jags_mod_V,variable.names = c("xV", "V" ), n.iter = 5000, n.thin = 100))
sumV = summary(outV)
cred_intV = HPDinterval(outV[,760:length(sumV$statistics[,1])], 0.95)

###### SD ######
datSD = na.omit(polls[,c('SD','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datSD = datSD[order(datSD$collectPeriodFrom),]
datSD$collectPeriodFrom = as.Date(datSD$collectPeriodFrom)
datSD$collectPeriodTo = as.Date(datSD$collectPeriodTo)
datSD$PublDate = as.Date(datSD$PublDate)
orig.date = as.Date("2006-09-17")
end.date = as.Date(datSD$collectPeriodTo[length(datSD$collectPeriodTo)])
datSD =  datSD[-which(datSD$collectPeriodFrom<orig.date),]
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
omega ~ dgamma(1, 1)
phiSD <- 1/pow(omega,2)
}
'
pSD = (1 / (datSD$SD*(1-datSD$SD)/datSD$n))
data_SD = list(SD = datSD$SD, precSD = pSD, xSD = c(0.0293,rep(NA,end.date - orig.date-1)),
               day = datSD$fieldDate.num, npolls = nrow(datSD), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_SD,con="kalman_SD.bug")

system.time(jags_mod_SD <- jags.model("kalman_SD.bug", data = data_SD))

system.time(outSD <- coda.samples( jags_mod_SD,variable.names = c("xSD", "SD" ), n.iter = 5000, n.thin = 100))
sumSD = summary(outSD)
cred_intSD = HPDinterval(outSD[,751:length(sumSD$statistics[,1])], 0.95)

