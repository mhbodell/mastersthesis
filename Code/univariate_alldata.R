################################################
##### normal-gamma, non-imformative priors #####
#####  using available all available data  #####
################################################


############# M ###################

library(rjags, lib="C:/Users/mirhu86/Documents/packages")
data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)

datM = na.omit(polls[,c('M','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datM$house = factor(datM$house)
datM = datM[order(datM$collectPeriodFrom),]
datM$collectPeriodFrom = as.Date(datM$collectPeriodFrom)
datM$collectPeriodTo = as.Date(datM$collectPeriodTo)
datM$PublDate = as.Date(datM$PublDate)


elec = data.frame(rbind(c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0),
                        c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057)))

colnames(elec) = c("M","L","KD","C","S","MP","V","SD")
row.names(elec) = c("2002","2006","2010") #"2014"
elec$Date = c(as.Date("2002-09-12"), as.Date('2006-09-17'),as.Date('2010-09-23')) #,as.Date('2014-09-14')
n=c((0.801*6722*1000),(0.82*6892*1000),(0.846*7124*1000)) #,(0.858*7330*1000)
## http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf

dfM = data.frame(M=elec$M*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datM = rbind(datM, dfM)
datM = datM[order(datM$collectPeriodFrom),]
orig.date= as.Date(datM$collectPeriodFrom[1]-1)
end.date= datM$collectPeriodTo[length(datM$collectPeriodTo)]

datM$collectPeriodFrom.num = julian(datM$collectPeriodFrom,origin=orig.date) #days since origin
datM$collectPeriodTo.num = julian(datM$collectPeriodTo,origin=orig.date) #days since origin
datM$fieldDate.num = floor((datM$collectPeriodFrom.num + datM$collectPeriodTo.num) / 2)
datM$M = datM$M/100

all_jags_M ='
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
xM[1] ~ dunif(0,1)
epsM ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiM <-1/epsM
}
'


pM = (1 / (datM$M*(1-datM$M)/datM$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
all_data_M = list(M = datM$M, precM = pM, xM = rep(NA,end.date - orig.date),
              day = datM$fieldDate.num, npolls = nrow(datM), nperiods = as.numeric(end.date - orig.date))
writeLines(all_jags_M,con="kalman_M_allData.bug")

system.time(jags_mod_all_M <- jags.model("kalman_M_allData.bug", data = all_data_M, n.chain=3))

ninter=10000

system.time(all_outM <- coda.samples(jags_mod_all_M,variable.names = c("xM", "M"), n.iter = ninter, n.thin = 100))
#system.time(outM_jags <- jags.samples(jags_mod_M,variable.names = c("xM", "M","phiM" ), n.iter = 5000, n.thin = 10))
all_sumM = summary(all_outM)
all_cred_intM = HPDinterval(all_outM[,which(regexpr("xM", row.names(all_sumM$statistics))==1)], 0.95)
#sumM$quantiles[elec.day[3],c(1,5)] 

############ L #################
datL = na.omit(polls[,c('FP','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
colnames(datL)[1] <- 'L' 
datL$house = factor(datL$house)
datL = datL[order(datL$collectPeriodFrom),]
datL$collectPeriodFrom = as.Date(datL$collectPeriodFrom)
datL$collectPeriodTo = as.Date(datL$collectPeriodTo)
datL$PublDate = as.Date(datL$PublDate)
dfL = data.frame(L=elec$L*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datL = rbind(datL, dfL)
datL = datL[order(datL$collectPeriodFrom),]
orig.date= as.Date(datL$collectPeriodFrom[1]-1)
end.date= datL$collectPeriodTo[length(datL$collectPeriodTo)]
datL$collectPeriodFrom.num = julian(datL$collectPeriodFrom,origin=orig.date) #days since origin
datL$collectPeriodTo.num = julian(datL$collectPeriodTo,origin=orig.date) #days since origin
datL$fieldDate.num = floor((datL$collectPeriodFrom.num + datL$collectPeriodTo.num) / 2)
datL$L = datL$L/100

all_jags_L ='
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
xL[1] ~ dunif(0,1)
epsL ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiL <- 1/epsL
}
'

pL = (1 / (datL$L*(1-datL$L)/datL$n)) #binomial
#pL = (1 / (datL$L*(1-datL$L)*datL$n)) #multinomial
all_data_L = list(L = datL$L, precL = pL, xL = rep(NA,end.date - orig.date),
                  day = datL$fieldDate.num, npolls = nrow(datL), nperiods = as.numeric(end.date - orig.date))
writeLines(all_jags_L,con="all_kalman_L.bug")

system.time(all_jags_mod_L <- jags.model("all_kalman_L.bug", data = all_data_L, n.chain=3))

system.time(all_outL <- coda.samples(all_jags_mod_L,variable.names = c("xL", "L"), n.iter = ninter, n.thin = 100))
all_sumL = summary(all_outL)
all_cred_intL = HPDinterval(all_outL[,which(regexpr("xL", row.names(all_sumL$statistics))==1)], 0.95)

########### KD #############

datKD = na.omit(polls[,c('KD','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datKD$house = factor(datKD$house)
datKD = datKD[order(datKD$collectPeriodFrom),]
datKD$collectPeriodFrom = as.Date(datKD$collectPeriodFrom)
datKD$collectPeriodTo = as.Date(datKD$collectPeriodTo)
datKD$PublDate = as.Date(datKD$PublDate)
dfKD = data.frame(KD=elec$KD*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datKD = rbind(datKD, dfKD)
datKD = datKD[order(datKD$collectPeriodFrom),]
orig.date= as.Date(datKD$collectPeriodFrom[1]-1)
end.date= datKD$collectPeriodTo[length(datKD$collectPeriodTo)]
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
xKD[1] ~ dunif(0,1)
epsKD ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiKD <- 1/epsKD
}
'

pKD = (1 / (datKD$KD*(1-datKD$KD)/datKD$n)) #binomial
#pKD = (1 / (datKD$KD*(1-datKD$KD)*datKD$n)) #multinomial
all_data_KD = list(KD = datKD$KD, precKD = pKD, xKD = rep(NA,end.date - orig.date),
                   day = datKD$fieldDate.num, npolls = nrow(datKD), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_KD,con="all_kalman_KD.bug")

system.time(all_jags_mod_KD <- jags.model("all_kalman_KD.bug", data = all_data_KD, n.chain=3))

system.time(all_outKD <- coda.samples(all_jags_mod_KD,variable.names = c("xKD", "KD"), n.iter = ninter, n.thin = 100))
all_sumKD = summary(all_outKD)
all_cred_intKD = HPDinterval(all_outKD[,which(regexpr("xKD", row.names(all_sumKD$statistics))==1)], 0.95)


########### C #############

datC = na.omit(polls[,c('C','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datC$house = factor(datC$house)
datC = datC[order(datC$collectPeriodFrom),]
datC$collectPeriodFrom = as.Date(datC$collectPeriodFrom)
datC$collectPeriodTo = as.Date(datC$collectPeriodTo)
datC$PublDate = as.Date(datC$PublDate)
dfC = data.frame(C=elec$C*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datC = rbind(datC, dfC)
datC = datC[order(datC$collectPeriodFrom),]
orig.date= as.Date(datC$collectPeriodFrom[1]-1)
end.date= datC$collectPeriodTo[length(datC$collectPeriodTo)]
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
xC[1] ~ dunif(0,1)
epsC ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiC <- 1/epsC
}
'

pC = (1 / (datC$C*(1-datC$C)/datC$n)) #binomial
#pC = (1 / (datC$C*(1-datC$C)*datC$n)) #multinomial
all_data_C = list(C = datC$C, precC = pC, xC = rep(NA,end.date - orig.date),
                  day = datC$fieldDate.num, npolls = nrow(datC), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_C,con="all_kalman_C.bug")

system.time(all_jags_mod_C <- jags.model("all_kalman_C.bug", data = all_data_C, n.chain=3))

system.time(all_outC <- coda.samples(all_jags_mod_C,variable.names = c("xC", "C"), n.iter = ninter, n.thin = 100))
all_sumC = summary(all_outC)
all_cred_intC = HPDinterval(all_outC[,which(regexpr("xC", row.names(all_sumC$statistics))==1)], 0.95)


########### S #############

datS = na.omit(polls[,c('S','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datS$house = factor(datS$house)
datS = datS[order(datS$collectPeriodFrom),]
datS$collectPeriodFrom = as.Date(datS$collectPeriodFrom)
datS$collectPeriodTo = as.Date(datS$collectPeriodTo)
datS$PublDate = as.Date(datS$PublDate)
dfS = data.frame(S=elec$S*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datS = rbind(datS, dfS)
datS = datS[order(datS$collectPeriodFrom),]
orig.date= as.Date(datS$collectPeriodFrom[1]-1)
end.date= datS$collectPeriodTo[length(datS$collectPeriodTo)]
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
xS[1] ~ dunif(0,1)
epsS ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiS <- 1/epsS
}
'

pS = (1 / (datS$S*(1-datS$S)/datS$n)) #binomial
#pS = (1 / (datS$S*(1-datS$S)*datS$n)) #multinomial
all_data_S = list(S = datS$S, precS = pS, xS = rep(NA,end.date - orig.date),
                  day = datS$fieldDate.num, npolls = nrow(datS), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_S,con="all_kalman_S.bug")

system.time(all_jags_mod_S <- jags.model("all_kalman_S.bug", data = all_data_S, n.chain=3))

system.time(all_outS <- coda.samples(all_jags_mod_S,variable.names = c("xS", "S"), n.iter = ninter, n.thin = 100))
all_sumS = summary(all_outS)
all_cred_intS = HPDinterval(all_outS[,which(regexpr("xS", row.names(all_sumS$statistics))==1)], 0.95)

########### MP #############

datMP = na.omit(polls[,c('MP','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datMP$house = factor(datMP$house)
datMP = datMP[order(datMP$collectPeriodFrom),]
datMP$collectPeriodFrom = as.Date(datMP$collectPeriodFrom)
datMP$collectPeriodTo = as.Date(datMP$collectPeriodTo)
datMP$PublDate = as.Date(datMP$PublDate)
dfMP = data.frame(MP=elec$MP*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datMP = rbind(datMP, dfMP)
datMP = datMP[order(datMP$collectPeriodFrom),]
orig.date= as.Date(datMP$collectPeriodFrom[1]-1)
end.date= datMP$collectPeriodTo[length(datMP$collectPeriodTo)]
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
xMP[1] ~ dunif(0,1)
epsMP ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiMP <- 1/epsMP
}
'

pMP = (1 / (datMP$MP*(1-datMP$MP)/datMP$n)) #binomial
#pMP = (1 / (datMP$MP*(1-datMP$MP)*datMP$n)) #multinomial
all_data_MP = list(MP = datMP$MP, precMP = pMP, xMP = rep(NA,end.date - orig.date),
                   day = datMP$fieldDate.num, npolls = nrow(datMP), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_MP,con="all_kalman_MP.bug")

system.time(all_jags_mod_MP <- jags.model("all_kalman_MP.bug", data = all_data_MP, n.chain=3))

system.time(all_outMP <- coda.samples(all_jags_mod_MP,variable.names = c("xMP", "MP"), n.iter = ninter, n.thin = 100))
all_sumMP = summary(all_outMP)
all_cred_intMP = HPDinterval(all_outMP[,which(regexpr("xMP", row.names(all_sumMP$statistics))==1)], 0.95)

########### V #############

datV = na.omit(polls[,c('V','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datV$house = factor(datV$house)
datV = datV[order(datV$collectPeriodFrom),]
datV$collectPeriodFrom = as.Date(datV$collectPeriodFrom)
datV$collectPeriodTo = as.Date(datV$collectPeriodTo)
datV$PublDate = as.Date(datV$PublDate)
dfV = data.frame(V=elec$V*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datV = rbind(datV, dfV)
datV = datV[order(datV$collectPeriodFrom),]
orig.date= as.Date(datV$collectPeriodFrom[1]-1)
end.date= datV$collectPeriodTo[length(datV$collectPeriodTo)]
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
xV[1] ~ dunif(0,1)
epsV ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiV <- 1/epsV
}
'

pV = (1 / (datV$V*(1-datV$V)/datV$n)) #binomial
#pV = (1 / (datV$V*(1-datV$V)*datV$n)) #multinomial
all_data_V = list(V = datV$V, precV = pV, xV = rep(NA,end.date - orig.date),
                  day = datV$fieldDate.num, npolls = nrow(datV), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_V,con="all_kalman_V.bug")

system.time(all_jags_mod_V <- jags.model("all_kalman_V.bug", data = all_data_V, n.chain=3))

system.time(all_outV <- coda.samples(all_jags_mod_V,variable.names = c("xV", "V"), n.iter = ninter, n.thin = 100))
all_sumV = summary(all_outV)
all_cred_intV = HPDinterval(all_outV[,which(regexpr("xV", row.names(all_sumV$statistics))==1)], 0.95)

########### SD #############

datSD = na.omit(polls[,c('SD','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datSD$house = factor(datSD$house)
datSD = datSD[order(datSD$collectPeriodFrom),]
datSD$collectPeriodFrom = as.Date(datSD$collectPeriodFrom)
datSD$collectPeriodTo = as.Date(datSD$collectPeriodTo)
datSD$PublDate = as.Date(datSD$PublDate)
elecSD = data.frame(elec[-1,])
dfSD = data.frame(SD=elecSD$SD*100, collectPeriodFrom=elecSD$Date, collectPeriodTo=elecSD$Date, n=n[-1], PublDate=elecSD$Date, house="Election")
datSD = rbind(datSD, dfSD)
datSD = datSD[order(datSD$collectPeriodFrom),]
orig.date = as.Date(datSD$collectPeriodFrom[1]-1)
end.date = datSD$collectPeriodTo[length(datSD$collectPeriodTo)]
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
xSD[1] ~ dunif(0,1)
epsSD ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiSD <- 1/epsSD
}
'

pSD = (1 / (datSD$SD*(1-datSD$SD)/datSD$n)) #binomial
#pSD = (1 / (datSD$SD*(1-datSD$SD)*datSD$n)) #multinomial
all_data_SD = list(SD = datSD$SD, precSD = pSD, xSD = rep(NA,end.date - orig.date),
                   day = datSD$fieldDate.num, npolls = nrow(datSD), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_SD,con="all_kalman_SD.bug")

system.time(all_jags_mod_SD <- jags.model("all_kalman_SD.bug", data = all_data_SD, n.chain=3))

system.time(all_outSD <- coda.samples(all_jags_mod_SD,variable.names = c("xSD", "SD"), n.iter = ninter, n.thin = 100))
all_sumSD = summary(all_outSD)
all_cred_intSD = HPDinterval(all_outSD[,which(regexpr("xSD", row.names(all_sumSD$statistics))==1)], 0.95)



######################################################################
sumM = all_sumM
outM = all_outM
sumL = all_sumL
outL = all_outL
sumKD = all_sumKD
outKD = all_outKD
sumC = all_sumC
outC = all_outC
sumS = all_sumS
outS = all_outS
sumMP = all_sumMP
outMP = all_outMP
sumV = all_sumV
outV = all_outV
sumSD = all_sumSD
outSD = all_outSD


meanM = sumM$statistics[which(regexpr("xM", row.names(sumM$statistics))==1),1]
meanL = sumL$statistics[which(regexpr("xL", row.names(sumL$statistics))==1),1]
meanKD = sumKD$statistics[which(regexpr("xKD", row.names(sumKD$statistics))==1),1]
meanC = sumC$statistics[which(regexpr("xC", row.names(sumC$statistics))==1),1]
meanS = sumS$statistics[which(regexpr("xS", row.names(sumS$statistics))==1),1]
meanMP = sumMP$statistics[which(regexpr("xMP", row.names(sumMP$statistics))==1),1]
meanV = sumV$statistics[which(regexpr("xV", row.names(sumV$statistics))==1),1]
meanSD = sumSD$statistics[which(regexpr("xSD", row.names(sumSD$statistics))==1),1]


set.seed(901207)
i = sample(1:3,1)
rsimM = outM[[i]]
rsimL = outL[[i]]
rsimKD = outKD[[i]]
rsimC = outC[[i]]
rsimS = outS[[i]]
rsimMP = outMP[[i]]
rsimV = outV[[i]]
rsimSD = outSD[[i]]
nsim = dim(rsimM)[1]
simxM = rsimM[,which(regexpr("xM", colnames(rsimM))==1)]
simxL = rsimL[,which(regexpr("xL", colnames(rsimL))==1)]
simxKD = rsimKD[,which(regexpr("xKD", colnames(rsimKD))==1)]
simxC = rsimC[,which(regexpr("xC", colnames(rsimC))==1)]
simxS = rsimS[,which(regexpr("xS", colnames(rsimS))==1)]
simxMP = rsimMP[,which(regexpr("xMP", colnames(rsimMP))==1)]
simxV = rsimV[,which(regexpr("xV", colnames(rsimV))==1)]
simxSD = rsimSD[,which(regexpr("xSD", colnames(rsimSD))==1)]
varM = matrix (NA, nrow=nsim, ncol=nrow(datM))
varL = matrix (NA, nrow=nsim, ncol=nrow(datL))
varKD = matrix (NA, nrow=nsim, ncol=nrow(datKD))
varC = matrix (NA, nrow=nsim, ncol=nrow(datC))
varS = matrix (NA, nrow=nsim, ncol=nrow(datS))
varMP = matrix (NA, nrow=nsim, ncol=nrow(datMP))
varV = matrix (NA, nrow=nsim, ncol=nrow(datV))
varSD = matrix (NA, nrow=nsim, ncol=nrow(datSD))
for(i in 1:nrow(varM)){
  varM[i,] = 1/pM
  varL[i,] = 1/pL
  varKD[i,] = 1/pKD
  varC[i,] = 1/pC
  varS[i,] = 1/pS
  varMP[i,] = 1/pMP
  varV[i,] = 1/pV
  varSD[i,] = 1/pSD
}


####### y^rep #####


yrepM = sapply(1:nsim, function(s) rnorm(length(datM$fieldDate.num),simxM[s,datM$fieldDate.num], varM[s,]))
yrepL = sapply(1:nsim, function(s) rnorm(length(datM$fieldDate.num),simxL[s,datL$fieldDate.num], varL[s,] ))
yrepKD = sapply(1:nsim, function(s) rnorm(length(datM$fieldDate.num),simxKD[s,datKD$fieldDate.num], varKD[s,] ))
yrepC = sapply(1:nsim, function(s) rnorm(length(datM$fieldDate.num),simxC[s,datC$fieldDate.num], varC[s,] ))
yrepS = sapply(1:nsim, function(s) rnorm(length(datM$fieldDate.num),simxS[s,datS$fieldDate.num], varS[s,] ))
yrepMP = sapply(1:nsim, function(s) rnorm(length(datM$fieldDate.num),simxMP[s,datMP$fieldDate.num], varMP[s,] ))
yrepV = sapply(1:nsim, function(s) rnorm(length(datM$fieldDate.num),simxV[s,datV$fieldDate.num], varV[s,] ))
yrepSD = sapply(1:nsim, function(s) rnorm(length(datM$fieldDate.num),simxSD[s,datSD$fieldDate.num], varSD[s,] ))


####### y^rep 2 #######

#y^rep|y~N((A^2/(A^2+1))*y,1+(A^2/(A^2+1))), A^2 comes from theta~N(0,A^2)
#p_bM = NULL

#for(i in 1:100){
#  yrepM = sapply(1:nsim, function(s) rnorm(length(datM$fieldDate.num),simxM[s,datM$fieldDate.num], varM[s,]))
#  min_repM = apply(yrepM,2,min)
#  min_M = min(datM$M)
#  p_bM[i] = sum(ifelse(min_repM>=min_M,1,0))/length(min_repM)
#}
#hist(p_bM, breaks=30)
####### y^rep min ##### 

par(mfrow=c(3,3))
min_repM = apply(yrepM,2,min)
min_M = min(datM$M)
hist(min_repM, main="M", col="blue", xlab="Minimum value in replicated data", las=1)
abline(v=min_M, lty=1, lwd=2)
sum(ifelse(min_repM>=min_M,1,0))/length(min_repM) 

min_repL = apply(yrepL,2,min)
min_L = min(datL$L)
hist(min_repL, main="L", col="lightblue3", xlab="Minimum value in replicated data", las=1)
abline(v=min_L, lty=1, lwd=2)
sum(ifelse(min_repL>=min_L,1,0))/length(min_repL) 

min_repKD = apply(yrepKD,2,min)
min_KD = min(datKD$KD)
hist(min_repKD, main="KD", col="darkblue", xlab="Minimum value in replicated data", las=1)
abline(v=min_KD, lty=1, lwd=2)
sum(ifelse(min_repKD>=min_KD,1,0))/length(min_repKD) 

min_repC = apply(yrepC,2,min)
min_C = min(datC$C)
hist(min_repC, main="C", col="chartreuse3", xlab="Minimum value in replicated data", las=1)
abline(v=min_C, lty=1, lwd=2)
sum(ifelse(min_repC>=min_C,1,0))/length(min_repC) 

min_repS = apply(yrepS,2,min)
min_S = min(datS$S)
hist(min_repS, main="S", col="red", xlab="Minimum value in replicated data", las=1)
abline(v=min_S, lty=1, lwd=2)
sum(ifelse(min_repS>=min_S,1,0))/length(min_repS) 

min_repMP = apply(yrepMP,2,min)
min_MP = min(datMP$MP)
hist(min_repMP, main="MP", col="forestgreen", xlab="Minimum value in replicated data", las=1)
abline(v=min_MP, lty=1, lwd=2)
sum(ifelse(min_repMP>=min_MP,1,0))/length(min_repMP) 

min_repV = apply(yrepV,2,min)
min_V = min(datV$V)
hist(min_repV, main="V", col="darkred", xlab="Minimum value in replicated data", las=1)
abline(v=min_V, lty=1, lwd=2)
sum(ifelse(min_repV>=min_V,1,0))/length(min_repV) 


min_repSD = apply(yrepSD,2,min)
min_SD = min(datSD$SD)
hist(min_repSD, main="SD", col="skyblue3", xlab="Minimum value in replicated data", las=1)
abline(v=min_SD, lty=1, lwd=2)
sum(ifelse(min_repSD>=min_SD,1,0))/length(min_repSD) 
par(mfrow=c(1,1))

####### y^rep max ##### 
par(mfrow=c(3,3))
max_repM = apply(yrepM,2,max)
max_M = max(datM$M)
hist(max_repM, main="M", col="blue", xlab="Maximum value in replicated data", las=1)
abline(v=max_M, lty=1, lwd=2)
sum(ifelse(max_repM>=max_M,1,0))/length(max_repM) 

max_repL = apply(yrepL,2,max)
max_L = max(datL$L)
hist(max_repL, main="L", col="lightblue3", xlab="Maximum observation in replicated data", las=1)
abline(v=max_L, lty=1, lwd=2)
sum(ifelse(max_repL>=max_L,1,0))/length(max_repL) 

max_repKD = apply(yrepKD,2,max)
max_KD = max(datKD$KD)
hist(max_repKD, main="KD", col="darkblue", xlab="Maximum observation in replicated data", las=1)
abline(v=max_KD, lty=1, lwd=2)
sum(ifelse(max_repKD>=max_KD,1,0))/length(max_repKD) 

max_repC = apply(yrepC,2,max)
max_C = max(datC$C)
hist(max_repC, main="C", col="chartreuse3", xlab="Maximum observation in replicated data", las=1)
abline(v=max_C, lty=1, lwd=2)
sum(ifelse(max_repC>=max_C,1,0))/length(max_repC) 

max_repS = apply(yrepS,2,max)
max_S = max(datS$S)
hist(max_repS, main="S", col="red", xlab="Maximum value in replicated data", las=1)
abline(v=max_S, lty=1, lwd=2)
sum(ifelse(max_repS>=max_S,1,0))/length(max_repS) 

max_repMP = apply(yrepMP,2,max)
max_MP = max(datMP$MP)
hist(max_repMP, main="MP", col="forestgreen", xlab="Maximum value in replicated data", las=1)
abline(v=max_MP, lty=1, lwd=2)
sum(ifelse(max_repMP<max_MP,1,0))/length(max_repMP) 

max_repV = apply(yrepV,2,max)
max_V = max(datV$V)
hist(max_repV, main="V", col="darkred", xlab="Maximum value in replicated data", las=1)
abline(v=max_V, lty=1, lwd=2)
sum(ifelse(max_repV>=max_V,1,0))/length(max_repV) 

max_repSD = apply(yrepSD,2,max)
max_SD = max(datSD$SD)
hist(max_repSD, main="SD", col="skyblue3", xlab="Maximum observation in replicated data", las=1)
abline(v=max_SD, lty=1, lwd=2)
sum(ifelse(max_repSD>=max_SD,1,0))/length(max_repSD) 
par(mfrow=c(1,1))

####### y^rep mean #########

par(mfrow=c(3,3))
mean_repM = apply(yrepM,2,mean)
mean_M = mean(datM$M)
hist(mean_repM, main="M", col="blue", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_M, lty=1, lwd=2)
sum(ifelse(mean_repM>=mean_M,1,0))/length(mean_repM) 

mean_repL = apply(yrepL,2,mean)
mean_L = mean(datL$L)
hist(mean_repL, main="L", col="lightblue3", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_L, lty=1, lwd=2)
sum(ifelse(mean_repL>=mean_L,1,0))/length(mean_repL) 

mean_repKD = apply(yrepKD,2,mean)
mean_KD = mean(datKD$KD)
hist(mean_repKD, main="KD", col="darkblue", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_KD, lty=1, lwd=2)
sum(ifelse(mean_repKD>=mean_KD,1,0))/length(mean_repKD) 

mean_repC = apply(yrepC,2,mean)
mean_C = mean(datC$C)
hist(mean_repC, main="C", col="chartreuse3", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_C, lty=1, lwd=2)
sum(ifelse(mean_repC>=mean_C,1,0))/length(mean_repC) 

mean_repS = apply(yrepS,2,mean)
mean_S = mean(datS$S)
hist(mean_repS, main="S", col="red", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_S, lty=1, lwd=2)
sum(ifelse(mean_repS>=mean_S,1,0))/length(mean_repS) 

mean_repMP = apply(yrepMP,2,mean)
mean_MP = mean(datMP$MP)
hist(mean_repMP, main="MP", col="forestgreen", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_MP, lty=1, lwd=2)
sum(ifelse(mean_repMP>=mean_MP,1,0))/length(mean_repMP) 

mean_repV = apply(yrepV,2,mean)
mean_V = mean(datV$V)
hist(mean_repV, main="V", col="darkred", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_V, lty=1, lwd=2)
sum(ifelse(mean_repV>=mean_V,1,0))/length(mean_repV) 

mean_repSD = apply(yrepSD,2,mean)
mean_SD = mean(datSD$SD)
hist(mean_repSD, main="SD", col="skyblue3", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_SD, lty=1, lwd=2)
sum(ifelse(mean_repSD>=mean_SD,1,0))/length(mean_repSD) 
par(mfrow=c(1,1))


#################################################
##################### PLOTS #####################
#################################################

dfM = data.frame(party = meanM[-c(1:as.numeric(datM$collectPeriodFrom[2]-datM$collectPeriodFrom[1]))] , 
                 low=all_cred_intM[[1]][-c(1:as.numeric(datM$collectPeriodFrom[2]-datM$collectPeriodFrom[1])),1]*100,
                 high=all_cred_intM[[1]][-c(1:as.numeric(datM$collectPeriodFrom[2]-datM$collectPeriodFrom[1])),2]*100,
                 time=seq(as.Date(datM$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datM$collectPeriodFrom[2]-1))))),
                 party2 = rep("M", length(c(rep(NA,end.date - as.Date(datM$collectPeriodFrom[2]-1))))))

dfL = data.frame(party = meanL[-c(1:as.numeric(datL$collectPeriodFrom[2]-datL$collectPeriodFrom[1]))] , 
                 low=all_cred_intL[[1]][-c(1:as.numeric(datL$collectPeriodFrom[2]-datL$collectPeriodFrom[1])),1]*100, 
                 high=all_cred_intL[[1]][-c(1:as.numeric(datL$collectPeriodFrom[2]-datL$collectPeriodFrom[1])),2]*100,
                 time=seq(as.Date(datL$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datL$collectPeriodFrom[2]-1))))),
                 party2 = rep("L", length(c(rep(NA,end.date - as.Date(datL$collectPeriodFrom[2]-1))))))

dfKD = data.frame(party = meanKD[-c(1:as.numeric(datKD$collectPeriodFrom[2]-datKD$collectPeriodFrom[1]))] , 
                  low=all_cred_intKD[[1]][-c(1:as.numeric(datKD$collectPeriodFrom[2]-datKD$collectPeriodFrom[1])),1]*100, 
                  high=all_cred_intKD[[1]][-c(1:as.numeric(datKD$collectPeriodFrom[2]-datKD$collectPeriodFrom[1])),2]*100,
                  time=seq(as.Date(datKD$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datKD$collectPeriodFrom[2]-1))))),
                  party2 = rep("KD", length(c(rep(NA,end.date - as.Date(datKD$collectPeriodFrom[2]-1))))))

dfC = data.frame(party = meanC[-c(1:as.numeric(datC$collectPeriodFrom[2]-datC$collectPeriodFrom[1]))] , 
                 low=all_cred_intC[[1]][-c(1:as.numeric(datC$collectPeriodFrom[2]-datC$collectPeriodFrom[1])),1]*100,
                 high=all_cred_intC[[1]][-c(1:as.numeric(datC$collectPeriodFrom[2]-datC$collectPeriodFrom[1])),2]*100,
                 time=seq(as.Date(datC$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datC$collectPeriodFrom[2]-1))))),
                 party2 = rep("C", length(c(rep(NA,end.date - as.Date(datC$collectPeriodFrom[2]-1))))))

dfS = data.frame(party = meanS[-c(1:as.numeric(datS$collectPeriodFrom[2]-datS$collectPeriodFrom[1]))] , 
                 low=all_cred_intS[[1]][-c(1:as.numeric(datS$collectPeriodFrom[2]-datS$collectPeriodFrom[1])),1]*100, 
                 high=all_cred_intS[[1]][-c(1:as.numeric(datS$collectPeriodFrom[2]-datS$collectPeriodFrom[1])),2]*100,
                 time=seq(as.Date(datS$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datS$collectPeriodFrom[2]-1))))),
                 party2 = rep("S", length(c(rep(NA,end.date - as.Date(datS$collectPeriodFrom[2]-1))))))

dfMP = data.frame(party = meanMP[-c(1:as.numeric(datMP$collectPeriodFrom[2]-datMP$collectPeriodFrom[1]))] , 
                  low=all_cred_intMP[[1]][-c(1:as.numeric(datMP$collectPeriodFrom[2]-datMP$collectPeriodFrom[1])),1]*100, 
                  high=all_cred_intMP[[1]][-c(1:as.numeric(datMP$collectPeriodFrom[2]-datMP$collectPeriodFrom[1])),2]*100,
                  time=seq(as.Date(datMP$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datMP$collectPeriodFrom[2]-1))))),
                  party2 = rep("MP", length(c(rep(NA,end.date - as.Date(datMP$collectPeriodFrom[2]-1))))))

dfV = data.frame(party = meanV[-c(1:as.numeric(datV$collectPeriodFrom[2]-datV$collectPeriodFrom[1]))] , 
                 low=all_cred_intV[[1]][-c(1:as.numeric(datV$collectPeriodFrom[2]-datV$collectPeriodFrom[1])),1]*100, 
                 high=all_cred_intV[[1]][-c(1:as.numeric(datV$collectPeriodFrom[2]-datV$collectPeriodFrom[1])),2]*100,
                 time=as.Date(datV$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datV$collectPeriodFrom[2]-1)))),
                 party2 = rep("V", length(c(rep(NA,end.date - as.Date(datV$collectPeriodFrom[2]-1))))))

dfSD = data.frame(party = meanSD[-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1]))] , 
                  low=all_cred_intSD[[1]][-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1])),1]*100, 
                  high=all_cred_intSD[[1]][-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1])),2]*100,
                  time=seq(as.Date(datSD$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datSD$collectPeriodFrom[2]-1))))),
                  party2 = rep("SD", length(c(rep(NA,end.date - as.Date(datSD$collectPeriodFrom[2]-1))))))


pointsM = pointsM = data.frame(x=seq(as.Date(datM$collectPeriodFrom[1]-1),by='days',length=datM$fieldDate.num[length(datM$fieldDate.num)])[datM$fieldDate.num[-1]], 
                               y=datM$M[-1]*100, house=datM$house[-1], party=rep("M",datM$fieldDate.num[length(datM$fieldDate.num)])[datM$fieldDate.num[-1]])
pointsL = data.frame(x=seq(as.Date(datL$collectPeriodFrom[1]-1),by='days',length=datL$fieldDate.num[length(datL$fieldDate.num)])[datL$fieldDate.num[-1]], 
                     y=datL$L[-1]*100, house=datL$house[-1], party=rep("L",datL$fieldDate.num[length(datL$fieldDate.num)])[datL$fieldDate.num[-1]])

pointsKD = data.frame(x=seq(as.Date(datKD$collectPeriodFrom[1]-1),by='days',length=datKD$fieldDate.num[length(datKD$fieldDate.num)])[datKD$fieldDate.num[-1]], 
                      y=datKD$KD[-1]*100, house=datKD$house[-1], party=rep("KD",datKD$fieldDate.num[length(datKD$fieldDate.num)])[datKD$fieldDate.num[-1]])

pointsC = data.frame(x=seq(as.Date(datC$collectPeriodFrom[1]-1),by='days',length=datC$fieldDate.num[length(datC$fieldDate.num)])[datC$fieldDate.num[-1]], 
                     y=datC$C[-1]*100, house=datC$house[-1], party=rep("C",datC$fieldDate.num[length(datC$fieldDate.num)])[datC$fieldDate.num[-1]])

pointsS = data.frame(x=seq(as.Date(datS$collectPeriodFrom[1]-1),by='days',length=datS$fieldDate.num[length(datS$fieldDate.num)])[datS$fieldDate.num[-1]], 
                     y=datS$S[-1]*100, house=datS$house[-1], party=rep("S",datS$fieldDate.num[length(datS$fieldDate.num)])[datS$fieldDate.num[-1]])

pointsMP = data.frame(x=seq(as.Date(datMP$collectPeriodFrom[1]-1),by='days',length=datMP$fieldDate.num[length(datMP$fieldDate.num)])[datMP$fieldDate.num[-1]], 
                      y=datMP$MP[-1]*100, house=datMP$house[-1], party=rep("MP",datMP$fieldDate.num[length(datMP$fieldDate.num)])[datMP$fieldDate.num[-1]])

pointsV = data.frame(x=seq(as.Date(datV$collectPeriodFrom[1]-1),by='days',length=datV$fieldDate.num[length(datV$fieldDate.num)])[datV$fieldDate.num[-1]], 
                     y=datV$V[-1]*100, house=datV$house[-1], party=rep("V",datV$fieldDate.num[length(datV$fieldDate.num)])[datV$fieldDate.num[-1]])

pointsSD = data.frame(x=seq(as.Date(datSD$collectPeriodFrom[1]-1),by='days',length=datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]], 
                      y=datSD$SD[-1]*100, house=datSD$house[-1], party=rep("SD",datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]])





cols = c("blue", "lightblue3", "darkblue","chartreuse3","red","forestgreen","darkred","skyblue3" )

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))}
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



gM <- ggplot(dfM) +
  aes(x = time, y = party*100) +
  geom_line(col="blue", alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="blue") + 
  geom_point(data=pointsM, aes(x=x, y=y), alpha = 1, color="blue", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for M (%)")) +
  theme_bw() +
  facet_wrap( ~ party2, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gL <- ggplot(dfL) +
  aes(x = time, y = party*100) +
  geom_line(col="lightblue3", alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="lightblue3") + 
  geom_point(data=pointsL, aes(x=x, y=y), alpha = 1, color="lightblue3", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for L (%)")) +
  theme_bw() +
  facet_wrap( ~ party2, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gKD <- ggplot(dfKD) +
  aes(x = time, y = party*100) +
  geom_line(col="darkblue", alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="darkblue") + 
  geom_point(data=pointsKD, aes(x=x, y=y), alpha = 1, color="darkblue", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for KD (%)")) +
  theme_bw() +
  facet_wrap( ~ party2, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gC <- ggplot(dfC) +
  aes(x = time, y = party*100) +
  geom_line(col="chartreuse3", alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="chartreuse3") + 
  geom_point(data=pointsC, aes(x=x, y=y), alpha = 1, color="chartreuse3", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for C (%)")) +
  theme_bw() +
  facet_wrap( ~ party2, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gS <- ggplot(dfS) +
  aes(x = time, y = party*100) +
  geom_line(col="red", alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="red") + 
  geom_point(data=pointsS, aes(x=x, y=y), alpha = 1, color="red", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for S (%)")) +
  theme_bw() +
  facet_wrap( ~ party2, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gMP <- ggplot(dfMP) +
  aes(x = time, y = party*100) +
  geom_line(col="forestgreen", alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="forestgreen") + 
  geom_point(data=pointsMP, aes(x=x, y=y), alpha = 1, color="forestgreen", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for MP (%)")) +
  theme_bw() +
  facet_wrap( ~ party2, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gV <- ggplot(dfV) +
  aes(x = time, y = party*100) +
  geom_line(col="darkred", alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="darkred") + 
  geom_point(data=pointsV, aes(x=x, y=y), alpha = 1, color="darkred", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for V (%)")) +
  theme_bw() +
  facet_wrap( ~ party2, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gSD <- ggplot(dfSD) +
  aes(x = time, y = party*100) +
  geom_line(col="skyblue3", alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="skyblue3") + 
  geom_point(data=pointsSD, aes(x=x, y=y), alpha = 1, color="skyblue3", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for SD (%)")) +
  theme_bw() +
  facet_wrap( ~ party2, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())


multiplot(gM, gL, gKD, gC, gS, gMP, gS, gSD, cols=2)

############################################
################# MSE ######################
############################################

lM= list()
lL= list()
lC= list()
lKD= list()
lS= list()
lMP= list()
lV= list()
lSD= list()
for (i in 1:3){
  lM[[i]] = outM[[i]][,which(regexpr("xM", colnames(outM[[i]]))==1)]
  lM[[i]] = lM[[i]][,datM$fieldDate.num]
  lL[[i]] = outL[[i]][,which(regexpr("xL", colnames(outL[[i]]))==1)]
  lL[[i]] = lL[[i]][,datL$fieldDate.num]
  lKD[[i]] = outKD[[i]][,which(regexpr("xKD", colnames(outKD[[i]]))==1)]
  lKD[[i]] = lKD[[i]][,datKD$fieldDate.num]
  lC[[i]] = outC[[i]][,which(regexpr("xC", colnames(outC[[i]]))==1)]
  lC[[i]] = lC[[i]][,datC$fieldDate.num]
  lS[[i]] = outS[[i]][,which(regexpr("xS", colnames(outS[[i]]))==1)]
  lS[[i]] = lS[[i]][,datS$fieldDate.num]
  lMP[[i]] = outMP[[i]][,which(regexpr("xMP", colnames(outMP[[i]]))==1)]
  lMP[[i]] = lMP[[i]][,datMP$fieldDate.num]
  lV[[i]] = outV[[i]][,which(regexpr("xV", colnames(outV[[i]]))==1)]
  lV[[i]] = lV[[i]][,datV$fieldDate.num]
  lSD[[i]] = outSD[[i]][,which(regexpr("xSD", colnames(outSD[[i]]))==1)]
  lSD[[i]] = lSD[[i]][,datSD$fieldDate.num]
}

mseM = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
mseL = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
mseKD = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
mseC = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
mseS = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
mseMP = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
mseV = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
mseSD = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
for(i in 1:3){
  for (j in 1:nrow(lM[[i]])){
    mseM[j,i] = sum((datM$M-lM[[i]][j,])^2)/nrow(datM)
    mseL[j,i] = sum((datL$L-lL[[i]][j,])^2)/nrow(datL)
    mseKD[j,i] = sum((datKD$KD-lKD[[i]][j,])^2)/nrow(datKD)
    mseC[j,i] = sum((datC$C-lC[[i]][j,])^2)/nrow(datC)
    mseS[j,i] = sum((datS$S-lS[[i]][j,])^2)/nrow(datS)
    mseMP[j,i] = sum((datMP$MP-lMP[[i]][j,])^2)/nrow(datMP)
    mseV[j,i] = sum((datV$V-lV[[i]][j,])^2)/nrow(datV)
    mseSD[j,i] = sum((datSD$SD-lSD[[i]][j,])^2)/nrow(datSD)
  }
}

par(mfrow=c(3,3))
hist(rbind(mseM[,1], mseM[,2], mseM[,3]), main="M", xlab="MSE", col="blue")
abline(v=mean(mseM))
hist(rbind(mseL[,1], mseL[,2], mseL[,3]), main="L", xlab="MSE", col="lightblue3")
abline(v=mean(mseL))
hist(rbind(mseKD[,1], mseKD[,2], mseKD[,3]), main="KD", xlab="MSE", col="darkblue")
abline(v=mean(mseKD))
hist(rbind(mseC[,1], mseC[,2], mseC[,3]), main="C", xlab="MSE", col="chartreuse3")
abline(v=mean(mseC))
hist(rbind(mseS[,1], mseS[,2], mseS[,3]), main="S", xlab="MSE", col="red")
abline(v=mean(mseS))
hist(rbind(mseMP[,1], mseMP[,2], mseMP[,3]), main="MP", xlab="MSE", col="forestgreen")
abline(v=mean(mseMP))
hist(rbind(mseV[,1], mseV[,2], mseV[,3]), main="V", xlab="MSE", col="darkred")
abline(v=mean(mseV))
hist(rbind(mseSD[,1], mseSD[,2], mseSD[,3]), main="SD", xlab="MSE", col="skyblue3")
abline(v=mean(mseSD))
par(mfrow=c(1,1))



mean(mseM)
mean(mseL)
mean(mseKD)
mean(mseC)
mean(mseS)
mean(mseMP)
mean(mseV)
mean(mseSD)


t=sample(1:3, 1)
s=sample(1:10000,1)

MSE.M = sum((datM$M-lM[[t]][s,])^2)/nrow(datM)
MSE.L = sum((datL$L-lL[[t]][s,])^2)/nrow(datL)
MSE.KD = sum((datKD$KD-lKD[[t]][s,])^2)/nrow(datKD)
MSE.C = sum((datC$C-lC[[t]][s,])^2)/nrow(datC)
MSE.S = sum((datS$S-lS[[t]][s,])^2)/nrow(datS)
MSE.MP = sum((datMP$MP-lMP[[t]][s,])^2)/nrow(datMP)
MSE.V = sum((datV$V-lV[[t]][s,])^2)/nrow(datV)
MSE.SD = sum((datSD$SD-lSD[[t]][s,])^2)/nrow(datSD)

############################################################
#######################   MAE   ############################
############################################################


maeM = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
maeL = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
maeKD = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
maeC = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
maeS = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
maeMP = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
maeV = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
maeSD = matrix(NA, nrow=nrow(lM[[1]]), ncol=3)
for(i in 1:3){
  for (j in 1:nrow(lM[[i]])){
    maeM[j,i] = sum(abs(datM$M-lM[[i]][j,]))/nrow(datM)
    maeL[j,i] = sum(abs(datL$L-lL[[i]][j,]))/nrow(datL)
    maeKD[j,i] = sum(abs(datKD$KD-lKD[[i]][j,]))/nrow(datKD)
    maeC[j,i] = sum(abs(datC$C-lC[[i]][j,]))/nrow(datC)
    maeS[j,i] = sum(abs(datS$S-lS[[i]][j,]))/nrow(datS)
    maeMP[j,i] = sum(abs(datMP$MP-lMP[[i]][j,]))/nrow(datMP)
    maeV[j,i] = sum(abs(datV$V-lV[[i]][j,]))/nrow(datV)
    maeSD[j,i] = sum(abs(datSD$SD-lSD[[i]][j,]))/nrow(datSD)
  }
}

par(mfrow=c(3,3))
hist(rbind(maeM[,1], maeM[,2], maeM[,3]), main="M", xlab="MAE", col="blue")
abline(v=mean(maeM))
hist(rbind(maeL[,1], maeL[,2], maeL[,3]), main="L", xlab="MAE", col="lightblue3")
abline(v=mean(maeL))
hist(rbind(maeKD[,1], maeKD[,2], maeKD[,3]), main="KD", xlab="MAE", col="darkblue")
abline(v=mean(maeKD))
hist(rbind(maeC[,1], maeC[,2], maeC[,3]), main="C", xlab="MAE", col="chartreuse3")
abline(v=mean(maeC))
hist(rbind(maeS[,1], maeS[,2], maeS[,3]), main="S", xlab="MAE", col="red")
abline(v=mean(maeS))
hist(rbind(maeMP[,1], maeMP[,2], maeMP[,3]), main="MP", xlab="MAE", col="forestgreen")
abline(v=mean(maeMP))
hist(rbind(maeV[,1], maeV[,2], maeV[,3]), main="V", xlab="MAE", col="darkred")
abline(v=mean(maeV))
hist(rbind(maeSD[,1], maeSD[,2], maeSD[,3]), main="SD", xlab="MAE", col="skyblue3")
abline(v=mean(maeSD))
par(mfrow=c(1,1))

mean(maeM)
mean(maeL)
mean(maeKD)
mean(maeC)
mean(maeS)
mean(maeMP)
mean(maeV)
mean(maeSD)
#t=sample(1:3, 1)
#s=sample(1:10000,1)

MAE.M = sum(abs(datM$M-lM[[t]][s,]))/nrow(datM)
MAE.L = sum(abs(datL$L-lL[[t]][s,]))/nrow(datL)
MAE.KD = sum(abs(datKD$KD-lKD[[t]][s,]))/nrow(datKD)
MAE.C = sum(abs(datC$C-lC[[t]][s,]))/nrow(datC)
MAE.S = sum(abs(datS$S-lS[[t]][s,]))/nrow(datS)
MAE.MP = sum(abs(datMP$MP-lMP[[t]][s,]))/nrow(datMP)
MAE.V = sum(abs(datV$V-lV[[t]][s,]))/nrow(datV)
MAE.SD = sum(abs(datSD$SD-lSD[[t]][s,]))/nrow(datSD)

MAE.M;MAE.L;MAE.KD;MAE.C;MAE.S;MAE.MP;MAE.V;MAE.SD

