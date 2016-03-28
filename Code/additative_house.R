

niter=10000
#################### additative house effect #################################
orig.date= as.Date(datM$collectPeriodFrom[1]-1)
end.date= datM$collectPeriodTo[length(datM$collectPeriodTo)]

jags_M_house ='
model{

#measurement model
for(i in 1:npolls){
M[i] ~ dnorm(xM[day[i]]+houseM[org[i]], precM[i])
}

#dynamic model
for(i in 2:nperiods){
xM[i] ~ dnorm(xM[i-1],phiM)
}

## priors
xM[1] ~ dunif(0,1) 
epsM ~ dgamma(1,1)
phiM <- 1/epsM

for(i in 1:(nhouses-1)){
houseM[i] ~ dnorm(0,1)
}
houseM[12] <- 0

}
'

pM = (1 / (datM$M*(1-datM$M)/datM$n))
data_M_house = list(M = datM$M, precM = pM, xM = rep(NA,end.date - orig.date),
                    day = datM$fieldDate.num, npolls = nrow(datM), nperiods = as.numeric(end.date - orig.date),
                    nhouses = length(levels(as.factor(datM$house))), org=as.numeric(as.factor(datM$house)))

writeLines(jags_M_house,con="kalman_M_house.bug")
system.time(jags_mod_M_house <- jags.model("kalman_M_house.bug", data = data_M_house, n.chain=3))

system.time(outM_house <- coda.samples(jags_mod_M_house,variable.names = c("xM", "M", "houseM"), n.iter = niter, thin = 5))
sumM_house = summary(outM_house)
house_credM = HPDinterval(outM_house[,which(regexpr("xM", row.names(sumM_house$statistics))==1)], 0.95)
house_effectsM = rbind(sumM_house$statistics[which(regexpr("house", row.names(sumM_house$statistics))==1),1])
colnames(house_effectsM) = levels(datM$house)
house_effectsM

heM = ifelse(datM$house==colnames(house_effectsM)[1],house_effectsM[1],NA)
for(i in 2:length(colnames(house_effectsM))){
  ins = colnames(house_effectsM)[i]
  for(j in 1:nrow(datM)){
    if(datM[j,'house']==ins){
      heM[j] = house_effectsM[i]
    } else {}
  }
}

############ L ###############
orig.date= as.Date(datL$collectPeriodFrom[1]-1)
end.date= datL$collectPeriodTo[length(datL$collectPeriodTo)]

jags_L_house ='
model{

#measurement model
for(i in 1:npolls){
L[i] ~ dnorm(xL[day[i]]+houseL[org[i]], precL[i])}

#dynamic model
for(i in 2:nperiods){
xL[i] ~ dnorm(xL[i-1],phiL)}

## priors
xL[1] ~ dunif(0,1) ##weak prior?
epsL ~ dgamma(1,1)
phiL <- 1/epsL
for(i in 1:(nhouses-1)){
houseL[i] ~ dnorm(0,1)
}
houseL[12] <- 0
}
'

pL = (1 / (datL$L*(1-datL$L)/datL$n)) #binomial
#pL = (1 / (datL$L*(1-datL$L)*datL$n)) #multinomial
data_L_house = list(L = datL$L, precL = pL, xL = rep(NA,end.date - orig.date),
                    day = datL$fieldDate.num, npolls = nrow(datL), nperiods = as.numeric(end.date - orig.date),
                    nhouses = length(levels(as.factor(datL$house))), org=as.numeric(as.factor(datL$house)))
writeLines(jags_L_house, con="kalman_L_house.bug")

system.time(jags_L_house <- jags.model("kalman_L_house.bug", data = data_L_house))

system.time(outL_house <- coda.samples(jags_L_house,variable.names = c("xL", "L", "houseL"), n.iter = niter, thin = 5))
sumL_house = summary(outL_house)
cred_intL_house = HPDinterval(outL_house[,which(regexpr("xL", row.names(sumL_house$statistics))==1)], 0.95)

house_effectsL = rbind(sumL_house$statistics[which(regexpr("house", row.names(sumL_house$statistics))==1),1])
colnames(house_effectsL) = levels(datL$house)
house_effectsL

heL = ifelse(datL$house==colnames(house_effectsL)[1],house_effectsL[1],NA)
for(i in 2:length(colnames(house_effectsL))){
  ins = colnames(house_effectsL)[i]
  for(j in 1:nrow(datL)){
    if(datL[j,'house']==ins){
      heL[j] = house_effectsL[i]
    } else {}
  }
}

######### KD #############

orig.date = as.Date(datKD$collectPeriodFrom[1]-1)
end.date = datKD$collectPeriodTo[length(datKD$collectPeriodTo)]

jags_KD_house ='
model{

#measurement model
for(i in 1:npolls){
KD[i] ~ dnorm(xKD[day[i]]+houseKD[org[i]], precKD[i])
}

#dynamic model
for(i in 2:nperiods){
xKD[i] ~ dnorm(xKD[i-1],phiKD)
}

## priors
xKD[1] ~ dunif(0,1) ##weak prior?
epsKD ~ dgamma(1,1)
phiKD <- 1/epsKD

for(i in 1:(nhouses-1)){
houseKD[i] ~ dnorm(0,1)
}
houseKD[12] <- 0
}
'

pKD = (1 / (datKD$KD*(1-datKD$KD)/datKD$n)) #binomial
#pL = (1 / (datL$L*(1-datL$L)*datL$n)) #multinomial
data_KD_house = list(KD = datKD$KD, precKD = pKD, xKD = rep(NA,end.date - orig.date),
                     day = datKD$fieldDate.num, npolls = nrow(datKD), nperiods = as.numeric(end.date - orig.date),
                     nhouses = length(levels(as.factor(datKD$house))), org=as.numeric(as.factor(datKD$house)))
writeLines(jags_KD_house, con="kalman_KD_house.bug")

system.time(jags_KD_house <- jags.model("kalman_KD_house.bug", data = data_KD_house))

system.time(outKD_house <- coda.samples(jags_KD_house,variable.names = c("xKD", "KD", "houseKD"), n.iter = niter, thin = 5))
sumKD_house = summary(outKD_house)
cred_intKD_house = HPDinterval(outKD_house[,which(regexpr("xKD", row.names(sumKD_house$statistics))==1)], 0.95)

house_effectsKD = rbind(sumKD_house$statistics[which(regexpr("house", row.names(sumL_house$statistics))==1),1])
colnames(house_effectsKD) = levels(datKD$house)
house_effectsKD

heKD = ifelse(datKD$house==colnames(house_effectsKD)[1],house_effectsKD[1],NA)
for(i in 2:length(colnames(house_effectsKD))){
  ins = colnames(house_effectsKD)[i]
  for(j in 1:nrow(datKD)){
    if(datKD[j,'house']==ins){
      heKD[j] = house_effectsKD[i]
    } else {}
  }
}

######### C #############

orig.date = as.Date(datC$collectPeriodFrom[1]-1)
end.date = datC$collectPeriodTo[length(datC$collectPeriodTo)]

jags_C_house ='
model{

#measurement model
for(i in 1:npolls){
C[i] ~ dnorm(xC[day[i]]+houseC[org[i]], precC[i])
}

#dynamic model
for(i in 2:nperiods){
xC[i] ~ dnorm(xC[i-1],phiC)
}

## priors
xC[1] ~ dunif(0,1) ##weak prior?
epsC ~ dgamma(1,1)
phiC <- 1/epsC

for(i in 1:c(nhouses-1)){
houseC[i] ~ dnorm(0,1)
}
houseC[12] <- 0
}
'

pC = (1 / (datC$C*(1-datC$C)/datC$n)) #binomial
#pL = (1 / (datL$L*(1-datL$L)*datL$n)) #multinomial
data_C_house = list(C = datC$C, precC = pC, xC = rep(NA,end.date - orig.date),
                    day = datC$fieldDate.num, npolls = nrow(datC), nperiods = as.numeric(end.date - orig.date),
                    nhouses = length(levels(as.factor(datC$house))), org=as.numeric(as.factor(datC$house)))
writeLines(jags_C_house, con="kalman_C_house.bug")

system.time(jags_C_house <- jags.model("kalman_C_house.bug", data = data_C_house))

system.time(outC_house <- coda.samples(jags_C_house,variable.names = c("xC", "C", "houseC"), n.iter = niter, thin = 5))
sumC_house = summary(outC_house)
cred_intC_house = HPDinterval(outC_house[,which(regexpr("xC", row.names(sumC_house$statistics))==1)], 0.95)

house_effectsC = rbind(sumC_house$statistics[which(regexpr("house", row.names(sumC_house$statistics))==1),1])
colnames(house_effectsC) = levels(datC$house)
house_effectsC

heC = ifelse(datC$house==colnames(house_effectsC)[1],house_effectsC[1],NA)
for(i in 2:length(colnames(house_effectsC))){
  ins = colnames(house_effectsC)[i]
  for(j in 1:nrow(datC)){
    if(datC[j,'house']==ins){
      heC[j] = house_effectsC[i]
    } else {}
  }
}

######### S #############

orig.date = as.Date(datS$collectPeriodFrom[1]-1)
end.date = datS$collectPeriodTo[length(datS$collectPeriodTo)]

jags_S_house ='
model{

#measurement model
for(i in 1:npolls){
S[i] ~ dnorm(xS[day[i]]+houseS[org[i]], precS[i])
}

#dynamic model
for(i in 2:nperiods){
xS[i] ~ dnorm(xS[i-1],phiS)
}

## priors
xS[1] ~ dunif(0,1) ##weak prior?
epsS ~ dgamma(1,1)
phiS <- 1/epsS

for(i in 1:(nhouses-1)){
houseS[i] ~ dnorm(0,1)
}
houseS[12] <- 0
}
'

pS = (1 / (datS$S*(1-datS$S)/datS$n)) #binomial
#pL = (1 / (datL$L*(1-datL$L)*datL$n)) #multinomial
data_S_house = list(S = datS$S, precS = pS, xS = rep(NA,end.date - orig.date),
                    day = datS$fieldDate.num, npolls = nrow(datS), nperiods = as.numeric(end.date - orig.date),
                    nhouses = length(levels(as.factor(datS$house))), org=as.numeric(as.factor(datS$house)))
writeLines(jags_S_house, con="kalman_S_house.bug")

system.time(jags_S_house <- jags.model("kalman_S_house.bug", data = data_S_house))

system.time(outS_house <- coda.samples(jags_S_house,variable.names = c("xS", "S", "houseS"), n.iter = niter, thin = 5))
sumS_house = summary(outS_house)
cred_intS_house = HPDinterval(outS_house[,which(regexpr("xS", row.names(sumS_house$statistics))==1)], 0.95)

house_effectsS = rbind(sumS_house$statistics[which(regexpr("house", row.names(sumS_house$statistics))==1),1])
colnames(house_effectsS) = levels(datS$house)
house_effectsS

heS = ifelse(datS$house==colnames(house_effectsS)[1],house_effectsS[1],NA)
for(i in 2:length(colnames(house_effectsS))){
  ins = colnames(house_effectsS)[i]
  for(j in 1:nrow(datS)){
    if(datS[j,'house']==ins){
      heS[j] = house_effectsS[i]
    } else {}
  }
}



######### MP #############

orig.date = as.Date(datMP$collectPeriodFrom[1]-1)
end.date = datMP$collectPeriodTo[length(datMP$collectPeriodTo)]

jags_MP_house ='
model{

#measurement model
for(i in 1:npolls){
MP[i] ~ dnorm(xMP[day[i]]+houseMP[org[i]], precMP[i])
}

#dynamic model
for(i in 2:nperiods){
xMP[i] ~ dnorm(xMP[i-1],phiMP)
}

## priors
xMP[1] ~ dunif(0,1) ##weak prior?
epsMP ~ dgamma(1,1)
phiMP <- 1/epsMP

for(i in 1:(nhouses-1)){
houseMP[i] ~ dnorm(0,1)
}
houseMP[12] <- 0
}
'

pMP = (1 / (datMP$MP*(1-datMP$MP)/datMP$n)) #binomial
#pL = (1 / (datL$L*(1-datL$L)*datL$n)) #multinomial
data_MP_house = list(MP = datMP$MP, precMP = pMP, xMP = rep(NA,end.date - orig.date),
                     day = datMP$fieldDate.num, npolls = nrow(datMP), nperiods = as.numeric(end.date - orig.date),
                     nhouses = length(levels(as.factor(datMP$house))), org=as.numeric(as.factor(datMP$house)))
writeLines(jags_MP_house, con="kalman_MP_house.bug")

system.time(jags_MP_house <- jags.model("kalman_MP_house.bug", data = data_MP_house))

system.time(outMP_house <- coda.samples(jags_MP_house,variable.names = c("xMP", "MP", "houseMP"), n.iter = niter, thin = 5))
sumMP_house = summary(outMP_house)
cred_intMP_house = HPDinterval(outMP_house[,which(regexpr("xMP", row.names(sumMP_house$statistics))==1)], 0.95)

house_effectsMP = rbind(sumMP_house$statistics[which(regexpr("house", row.names(sumMP_house$statistics))==1),1])
colnames(house_effectsMP) = levels(datMP$house)
house_effectsMP

heMP = ifelse(datMP$house==colnames(house_effectsMP)[1],house_effectsMP[1],NA)
for(i in 2:length(colnames(house_effectsMP))){
  ins = colnames(house_effectsMP)[i]
  for(j in 1:nrow(datMP)){
    if(datMP[j,'house']==ins){
      heMP[j] = house_effectsMP[i]
    } else {}
  }
}


######### V #############

orig.date = as.Date(datV$collectPeriodFrom[1]-1)
end.date = datV$collectPeriodTo[length(datV$collectPeriodTo)]

jags_V_house ='
model{

#measurement model
for(i in 1:npolls){
V[i] ~ dnorm(xV[day[i]]+houseV[org[i]], precV[i])
}

#dynamic model
for(i in 2:nperiods){
xV[i] ~ dnorm(xV[i-1],phiV)
}

## priors
xV[1] ~ dunif(0,1) ##weak prior?
epsV ~ dgamma(1,1)
phiV <- 1/epsV

for(i in 1:(nhouses-1)){
houseV[i] ~ dnorm(0,1)
}
houseV[12] <- 0
}
'

pV = (1 / (datV$V*(1-datV$V)/datV$n)) #binomial
#pL = (1 / (datL$L*(1-datL$L)*datL$n)) #multinomial
data_V_house = list(V = datV$V, precV = pV, xV = rep(NA,end.date - orig.date),
                    day = datV$fieldDate.num, npolls = nrow(datV), nperiods = as.numeric(end.date - orig.date),
                    nhouses = length(levels(as.factor(datV$house))), org=as.numeric(as.factor(datV$house)))
writeLines(jags_V_house, con="kalman_V_house.bug")

system.time(jags_V_house <- jags.model("kalman_V_house.bug", data = data_V_house))

system.time(outV_house <- coda.samples(jags_V_house,variable.names = c("xV", "V", "houseV"), n.iter = niter, thin = 5))
sumV_house = summary(outV_house)
cred_intV_house = HPDinterval(outV_house[,which(regexpr("xV", row.names(sumV_house$statistics))==1)], 0.95)

house_effectsV = rbind(sumV_house$statistics[which(regexpr("house", row.names(sumV_house$statistics))==1),1])
colnames(house_effectsV) = levels(datV$house)
house_effectsV

heV = ifelse(datV$house==colnames(house_effectsV)[1],house_effectsV[1],NA)
for(i in 2:length(colnames(house_effectsV))){
  ins = colnames(house_effectsV)[i]
  for(j in 1:nrow(datV)){
    if(datV[j,'house']==ins){
      heV[j] = house_effectsV[i]
    } else {}
  }
}



######### SD #############

orig.date = as.Date(datSD$collectPeriodFrom[1]-1)
end.date = datSD$collectPeriodTo[length(datSD$collectPeriodTo)]

jags_SD_house ='
model{

#measurement model
for(i in 1:npolls){
SD[i] ~ dnorm(xSD[day[i]]+houseSD[org[i]], precSD[i])
}

#dynamic model
for(i in 2:nperiods){
xSD[i] ~ dnorm(xSD[i-1],phiSD)
}

## priors
xSD[1] ~ dunif(0,1) ##weak prior?
epsSD ~ dgamma(1,1)
phiSD <- 1/epsSD

for(i in 1:(nhouses-1)){
houseSD[i] ~ dnorm(0,1)
}
houseSD[12] <- 0
}
'

pSD = (1 / (datSD$SD*(1-datSD$SD)/datSD$n)) #binomial
#pL = (1 / (datL$L*(1-datL$L)*datL$n)) #multinomial
data_SD_house = list(SD = datSD$SD, precSD = pSD, xSD = rep(NA,end.date - orig.date),
                     day = datSD$fieldDate.num, npolls = nrow(datSD), nperiods = as.numeric(end.date - orig.date),
                     nhouses = length(levels(as.factor(datSD$house))), org=as.numeric(as.factor(datSD$house)))
writeLines(jags_SD_house, con="kalman_SD_house.bug")

system.time(jags_SD_house <- jags.model("kalman_SD_house.bug", data = data_SD_house))
niter=ninter
system.time(outSD_house <- coda.samples(jags_SD_house,variable.names = c("xSD", "SD", "houseSD"), n.iter = niter, thin = 5))
sumSD_house = summary(outSD_house)
cred_intSD_house = HPDinterval(outSD_house[,which(regexpr("xSD", row.names(sumSD_house$statistics))==1)], 0.95)

house_effectsSD = rbind(sumSD_house$statistics[which(regexpr("house", row.names(sumSD_house$statistics))==1),1])
colnames(house_effectsSD) = levels(datSD$house)
house_effectsSD

heSD = ifelse(datSD$house==colnames(house_effectsSD)[1],house_effectsSD[1],NA)
for(i in 2:length(colnames(house_effectsSD))){
  ins = colnames(house_effectsSD)[i]
  for(j in 1:nrow(datSD)){
    if(datSD[j,'house']==ins){
      heSD[j] = house_effectsSD[i]
    } else {}
  }
}

################## PLOTS ####################

meanM_house = sumM_house$statistics[which(regexpr("xM", row.names(sumM_house$statistics))==1),1]
meanL_house = sumL_house$statistics[which(regexpr("xL", row.names(sumL_house$statistics))==1),1]
meanKD_house = sumKD_house$statistics[which(regexpr("xKD", row.names(sumKD_house$statistics))==1),1]
meanC_house = sumC_house$statistics[which(regexpr("xC", row.names(sumC_house$statistics))==1),1]
meanS_house = sumS_house$statistics[which(regexpr("xS", row.names(sumS_house$statistics))==1),1]
meanMP_house= sumMP_house$statistics[which(regexpr("xMP", row.names(sumMP_house$statistics))==1),1]
meanV_house = sumV_house$statistics[which(regexpr("xV", row.names(sumV_house$statistics))==1),1]
meanSD_house = sumSD_house$statistics[which(regexpr("xSD", row.names(sumSD_house$statistics))==1),1]


dfM = data.frame(party = meanM_house[-c(1:as.numeric(datM$collectPeriodFrom[2]-datM$collectPeriodFrom[1]))] ,
                 low=house_credM[[1]][-c(1:as.numeric(datM$collectPeriodFrom[2]-datM$collectPeriodFrom[1])),1]*100,
                 high=house_credM[[1]][-c(1:as.numeric(datM$collectPeriodFrom[2]-datM$collectPeriodFrom[1])),2]*100,
                 time=seq(as.Date(datM$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datM$collectPeriodFrom[2]-1))))),
                 party2 = rep("M", length(c(rep(NA,end.date - as.Date(datM$collectPeriodFrom[2]-1))))))

dfL = data.frame(party = meanL_house[-c(1:as.numeric(datL$collectPeriodFrom[2]-datL$collectPeriodFrom[1]))] ,
                 low=cred_intL_house[[1]][-c(1:as.numeric(datL$collectPeriodFrom[2]-datL$collectPeriodFrom[1])),1]*100,
                 high=cred_intL_house[[1]][-c(1:as.numeric(datL$collectPeriodFrom[2]-datL$collectPeriodFrom[1])),2]*100,
                 time=seq(as.Date(datL$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datL$collectPeriodFrom[2]-1))))),
                 party2 = rep("L", length(c(rep(NA,end.date - as.Date(datL$collectPeriodFrom[2]-1))))))

dfKD = data.frame(party = meanKD_house[-c(1:as.numeric(datKD$collectPeriodFrom[2]-datKD$collectPeriodFrom[1]))] ,
                  low=cred_intKD_house[[1]][-c(1:as.numeric(datKD$collectPeriodFrom[2]-datKD$collectPeriodFrom[1])),1]*100,
                  high=cred_intKD_house[[1]][-c(1:as.numeric(datKD$collectPeriodFrom[2]-datKD$collectPeriodFrom[1])),2]*100,
                  time=seq(as.Date(datKD$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datKD$collectPeriodFrom[2]-1))))),
                  party2 = rep("KD", length(c(rep(NA,end.date - as.Date(datKD$collectPeriodFrom[2]-1))))))

dfC = data.frame(party = meanC_house[-c(1:as.numeric(datC$collectPeriodFrom[2]-datC$collectPeriodFrom[1]))] ,
                 low=cred_intC_house[[1]][-c(1:as.numeric(datC$collectPeriodFrom[2]-datC$collectPeriodFrom[1])),1]*100,
                 high=cred_intC_house[[1]][-c(1:as.numeric(datC$collectPeriodFrom[2]-datC$collectPeriodFrom[1])),2]*100,
                 time=seq(as.Date(datC$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datC$collectPeriodFrom[2]-1))))),
                 party2 = rep("C", length(c(rep(NA,end.date - as.Date(datC$collectPeriodFrom[2]-1))))))

dfS = data.frame(party = meanS_house[-c(1:as.numeric(datS$collectPeriodFrom[2]-datS$collectPeriodFrom[1]))] ,
                 low=cred_intS_house[[1]][-c(1:as.numeric(datS$collectPeriodFrom[2]-datS$collectPeriodFrom[1])),1]*100,
                 high=cred_intS_house[[1]][-c(1:as.numeric(datS$collectPeriodFrom[2]-datS$collectPeriodFrom[1])),2]*100,
                 time=seq(as.Date(datS$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datS$collectPeriodFrom[2]-1))))),
                 party2 = rep("S", length(c(rep(NA,end.date - as.Date(datS$collectPeriodFrom[2]-1))))))

dfMP = data.frame(party = meanMP_house[-c(1:as.numeric(datMP$collectPeriodFrom[2]-datMP$collectPeriodFrom[1]))] ,
                  low=cred_intMP_house[[1]][-c(1:as.numeric(datMP$collectPeriodFrom[2]-datMP$collectPeriodFrom[1])),1]*100,
                  high=cred_intMP_house[[1]][-c(1:as.numeric(datMP$collectPeriodFrom[2]-datMP$collectPeriodFrom[1])),2]*100,
                  time=seq(as.Date(datMP$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datMP$collectPeriodFrom[2]-1))))),
                  party2 = rep("MP", length(c(rep(NA,end.date - as.Date(datMP$collectPeriodFrom[2]-1))))))

dfV = data.frame(party = meanV_house[-c(1:as.numeric(datV$collectPeriodFrom[2]-datV$collectPeriodFrom[1]))] ,
                 low=cred_intV_house[[1]][-c(1:as.numeric(datV$collectPeriodFrom[2]-datV$collectPeriodFrom[1])),1]*100,
                 high=cred_intV_house[[1]][-c(1:as.numeric(datV$collectPeriodFrom[2]-datV$collectPeriodFrom[1])),2]*100,
                 time=seq(as.Date(datV$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datV$collectPeriodFrom[2]-1))))),
                 party2 = rep("V", length(c(rep(NA,end.date - as.Date(datV$collectPeriodFrom[2]-1))))))

dfSD = data.frame(party = meanSD_house[-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1]))] ,
                  low=cred_intSD_house[[1]][-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1])),1]*100,
                  high=cred_intSD_house[[1]][-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1])),2]*100,
                  time=seq(as.Date(datSD$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datSD$collectPeriodFrom[2]-1))))),
                  party2 = rep("SD", length(c(rep(NA,end.date - as.Date(datSD$collectPeriodFrom[2]-1))))))



pointsM =  data.frame(x=seq(as.Date(datM$collectPeriodFrom[1]-1),by='days',length=datM$fieldDate.num[length(datM$fieldDate.num)])[datM$fieldDate.num[-1]],
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

library(ggplot2)
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

##################################################
############## MODEL EVALUATION ##################
##################################################
set.seed(901207)
i = sample(1:3,1)
rsimM_house = outM_house[[i]]
rsimL_house = outL_house[[i]]
rsimKD_house = outKD_house[[i]]
rsimC_house = outC_house[[i]]
rsimS_house = outS_house[[i]]
rsimMP_house = outMP_house[[i]]
rsimV_house = outV_house[[i]]
rsimSD_house = outSD_house[[i]]
nsim = dim(rsimM_house)[1]
simxM_house = rsimM_house[,which(regexpr("xM", colnames(rsimM_house))==1)]
simxL_house = rsimL_house[,which(regexpr("xL", colnames(rsimL_house))==1)]
simxKD_house = rsimKD_house[,which(regexpr("xKD", colnames(rsimKD_house))==1)]
simxC_house = rsimC_house[,which(regexpr("xC", colnames(rsimC_house))==1)]
simxS_house = rsimS_house[,which(regexpr("xS", colnames(rsimS_house))==1)]
simxMP_house = rsimMP_house[,which(regexpr("xMP", colnames(rsimMP_house))==1)]
simxV_house = rsimV_house[,which(regexpr("xV", colnames(rsimV_house))==1)]
simxSD_house = rsimSD_house[,which(regexpr("xSD", colnames(rsimSD_house))==1)]
varM = matrix (NA, nrow=nsim, ncol=nrow(datM));varL = matrix (NA, nrow=nsim, ncol=nrow(datL))
varKD = matrix (NA, nrow=nsim, ncol=nrow(datKD));varC = matrix (NA, nrow=nsim, ncol=nrow(datC))
varS = matrix (NA, nrow=nsim, ncol=nrow(datS));varMP = matrix (NA, nrow=nsim, ncol=nrow(datMP))
varV = matrix (NA, nrow=nsim, ncol=nrow(datV));varSD = matrix (NA, nrow=nsim, ncol=nrow(datSD))
for(i in 1:nrow(varM)){
  varM[i,] = 1/pM;varL[i,] = 1/pL;varKD[i,] = 1/pKD; varC[i,] = 1/pC
  varS[i,] = 1/pS;varMP[i,] = 1/pMP;varV[i,] = 1/pV;varSD[i,] = 1/pSD
}


hoefM = matrix (NA, nrow=nsim, ncol=nrow(datM));hoefL = matrix (NA, nrow=nsim, ncol=nrow(datL))
hoefKD = matrix (NA, nrow=nsim, ncol=nrow(datKD));hoefC = matrix (NA, nrow=nsim, ncol=nrow(datC))
hoefS = matrix (NA, nrow=nsim, ncol=nrow(datS));hoefMP = matrix (NA, nrow=nsim, ncol=nrow(datMP))
hoefV = matrix (NA, nrow=nsim, ncol=nrow(datV));hoefSD = matrix (NA, nrow=nsim, ncol=nrow(datSD))
for(i in 1:nrow(varM)){
  hoefM[i,] = heM;hoefL[i,] = heL;hoefKD[i,] = heKD; hoefC[i,] = heC
  hoefS[i,] = heS;hoefMP[i,] = heMP;hoefV[i,] = heV; hoefSD[i,] = heSD
}


####### y^rep #####

testMin = matrix(NA,ncol=8, nrow=100)
testMax = matrix(NA,ncol=8, nrow=100)
testMean = matrix(NA,ncol=8, nrow=100)
colnames(testMin) = colnames(testMax) = colnames(testMean) = c("M","L","KD","C","S","MP","V","SD") 
for(i in 1:100){
yrepM_house = sapply(1:nsim, function(s) rnorm(length(datM$fieldDate.num),simxM_house[s,datM$fieldDate.num]+hoefM[s,], varM[s,]))
yrepL_house = sapply(1:nsim, function(s) rnorm(length(datL$fieldDate.num),simxL_house[s,datL$fieldDate.num]+hoefL[s,], varL[s,] ))
yrepKD_house = sapply(1:nsim, function(s) rnorm(length(datKD$fieldDate.num),simxKD_house[s,datKD$fieldDate.num]+hoefKD[s,], varKD[s,] ))
yrepC_house = sapply(1:nsim, function(s) rnorm(length(datC$fieldDate.num),simxC_house[s,datC$fieldDate.num]+hoefC[s,], varC[s,] ))
yrepS_house = sapply(1:nsim, function(s) rnorm(length(datS$fieldDate.num),simxS_house[s,datS$fieldDate.num]+hoefS[s,], varS[s,] ))
yrepMP_house = sapply(1:nsim, function(s) rnorm(length(datMP$fieldDate.num),simxMP_house[s,datMP$fieldDate.num]+hoefMP[s,], varMP[s,] ))
yrepV_house = sapply(1:nsim, function(s) rnorm(length(datV$fieldDate.num),simxV_house[s,datV$fieldDate.num]+hoefV[s,], varV[s,] ))
yrepSD_house = sapply(1:nsim, function(s) rnorm(length(datSD$fieldDate.num),simxSD_house[s,datSD$fieldDate.num]+hoefSD[s,], varSD[s,] ))

min_repM_house = apply(yrepM_house,2,min)
min_repL_house = apply(yrepL_house,2,min)
min_repKD_house = apply(yrepKD_house,2,min)
min_repC_house = apply(yrepC_house,2,min)
min_repS_house = apply(yrepS_house,2,min)
min_repMP_house = apply(yrepMP_house,2,min)
min_repV_house = apply(yrepV_house,2,min)
min_repSD_house = apply(yrepSD_house,2,min)
testMin[i,] = c(sum(ifelse(min_repM_house>= min(datM$M),1,0))/length(min_repM_house),sum(ifelse(min_repL_house>=min(datL$L),1,0))/length(min_repL_house),sum(ifelse(min_repKD_house>=min(datKD$KD),1,0))/length(min_repKD_house),
                sum(ifelse(min_repC_house>=min(datC$C),1,0))/length(min_repC_house),sum(ifelse(min_repS_house>=min(datS$S),1,0))/length(min_repS_house),sum(ifelse(min_repMP_house>=min(datMP$MP),1,0))/length(min_repMP_house),
                sum(ifelse(min_repV_house>=min(datV$V),1,0))/length(min_repV_house),sum(ifelse(min_repSD_house>=min(datSD$SD),1,0))/length(min_repSD_house))
max_repM_house = apply(yrepM_house,2,max)
max_repL_house = apply(yrepL_house,2,max)
max_repKD_house = apply(yrepKD_house,2,max)
max_repC_house = apply(yrepC_house,2,max)
max_repS_house = apply(yrepS_house,2,max)
max_repMP_house = apply(yrepMP_house,2,max)
max_repV_house = apply(yrepV_house,2,max)
max_repSD_house = apply(yrepSD_house,2,max)
testMax[i,] = c(sum(ifelse(max_repM_house>= max(datM$M),1,0))/length(max_repM_house),sum(ifelse(max_repL_house>=max(datL$L),1,0))/length(max_repL_house),sum(ifelse(max_repKD_house>=max(datKD$KD),1,0))/length(max_repKD_house),
                sum(ifelse(max_repC_house>=max(datC$C),1,0))/length(max_repC_house),sum(ifelse(max_repS_house>=max(datS$S),1,0))/length(max_repS_house),sum(ifelse(max_repMP_house>=max(datMP$MP),1,0))/length(max_repMP_house),
                sum(ifelse(max_repV_house>=max(datV$V),1,0))/length(max_repV_house),sum(ifelse(max_repSD_house>=max(datSD$SD),1,0))/length(max_repSD_house))

mean_repM_house = apply(yrepM_house,2,mean)
mean_repL_house = apply(yrepL_house,2,mean)
mean_repKD_house = apply(yrepKD_house,2,mean)
mean_repC_house = apply(yrepC_house,2,mean)
mean_repS_house = apply(yrepS_house,2,mean)
mean_repMP_house = apply(yrepMP_house,2,mean)
mean_repV_house = apply(yrepV_house,2,mean)
mean_repSD_house = apply(yrepSD_house,2,mean)
testMean[i,] = c(sum(ifelse(mean_repM_house>= mean(datM$M),1,0))/length(mean_repM_house),sum(ifelse(mean_repL_house>=mean(datL$L),1,0))/length(mean_repL_house),sum(ifelse(mean_repKD_house>=mean(datKD$KD),1,0))/length(mean_repKD_house),
                 sum(ifelse(mean_repC_house>=mean(datC$C),1,0))/length(mean_repC_house),sum(ifelse(mean_repS_house>=mean(datS$S),1,0))/length(mean_repS_house),sum(ifelse(mean_repMP_house>=mean(datMP$MP),1,0))/length(mean_repMP_house),
                 sum(ifelse(mean_repV_house>=mean(datV$V),1,0))/length(mean_repV_house),sum(ifelse(mean_repSD_house>=mean(datSD$SD),1,0))/length(mean_repSD_house))

}

for(i in 1:8){
print(paste(colnames(testMin)[i],":"))
print(paste("Min:",mean(testMin[,i]), sep=" "))
print(paste("Max:",mean(testMax[,i]), sep=" "))
print(paste("Mean:",mean(testMean[,i]), sep=" "))
}
####### y^rep min #####

par(mfrow=c(3,3))
min_repM_house = apply(yrepM_house,2,min)
min_M = min(datM$M)
hist(min_repM_house, main="M", col="blue", xlab="Minimum value in replicated data", las=1)
abline(v=min_M, lty=1, lwd=2)
sum(ifelse(min_repM_house>=min_M,1,0))/length(min_repM_house)

min_repL_house = apply(yrepL_house,2,min)
min_L = min(datL$L)
hist(min_repL_house, main="L", col="lightblue3", xlab="Minimum value in replicated data", las=1)
abline(v=min_L, lty=1, lwd=2)
sum(ifelse(min_repL_house>=min_L,1,0))/length(min_repL_house)

min_repKD_house = apply(yrepKD_house,2,min)
min_KD = min(datKD$KD)
hist(min_repKD_house, main="KD", col="darkblue", xlab="Minimum value in replicated data", las=1)
abline(v=min_KD, lty=1, lwd=2)
sum(ifelse(min_repKD_house>=min_KD,1,0))/length(min_repKD_house)

min_repC_house = apply(yrepC_house,2,min)
min_C = min(datC$C)
hist(min_repC_house, main="C", col="chartreuse3", xlab="Minimum value in replicated data", las=1)
abline(v=min_C, lty=1, lwd=2)
sum(ifelse(min_repC_house>=min_C,1,0))/length(min_repC_house)

min_repS_house = apply(yrepS_house,2,min)
min_S = min(datS$S)
hist(min_repS_house, main="S", col="red", xlab="Minimum value in replicated data", las=1)
abline(v=min_S, lty=1, lwd=2)
sum(ifelse(min_repS_house>=min_S,1,0))/length(min_repS_house)

min_repMP_house = apply(yrepMP_house,2,min)
min_MP = min(datMP$MP)
hist(min_repMP_house, main="MP", col="forestgreen", xlab="Minimum value in replicated data", las=1)
abline(v=min_MP, lty=1, lwd=2)
sum(ifelse(min_repMP_house>=min_MP,1,0))/length(min_repMP_house)

min_repV_house = apply(yrepV_house,2,min)
min_V = min(datV$V)
hist(min_repV_house, main="V", col="darkred", xlab="Minimum value in replicated data", las=1)
abline(v=min_V, lty=1, lwd=2)
sum(ifelse(min_repV_house>=min_V,1,0))/length(min_repV_house)

min_repSD_house = apply(yrepSD_house,2,min)
min_SD = min(datSD$SD)
hist(min_repSD_house, main="SD", col="skyblue3", xlab="Minimum value in replicated data", las=1)
abline(v=min_SD, lty=1, lwd=2)
sum(ifelse(min_repSD_house>=min_SD,1,0))/length(min_repSD_house)
par(mfrow=c(1,1))

####### y^rep max #####
par(mfrow=c(3,3))
max_repM_house = apply(yrepM_house,2,max)
max_M = max(datM$M)
hist(max_repM_house, main="M", col="blue", xlab="Maximum value in replicated data", las=1)
abline(v=max_M, lty=1, lwd=2)
sum(ifelse(max_repM_house>=max_M,1,0))/length(max_repM_house)

max_repL_house = apply(yrepL_house,2,max)
max_L = max(datL$L)
hist(max_repL_house, main="L", col="lightblue3", xlab="Maximum observation in replicated data", las=1)
abline(v=max_L, lty=1, lwd=2)
sum(ifelse(max_repL_house>=max_L,1,0))/length(max_repL_house)

max_repKD_house = apply(yrepKD_house,2,max)
max_KD = max(datKD$KD)
hist(max_repKD_house, main="KD", col="darkblue", xlab="Maximum observation in replicated data", las=1)
abline(v=max_KD, lty=1, lwd=2)
sum(ifelse(max_repKD_house>=max_KD,1,0))/length(max_repKD_house)

max_repC_house = apply(yrepC_house,2,max)
max_C = max(datC$C)
hist(max_repC_house, main="C", col="chartreuse3", xlab="Maximum observation in replicated data", las=1)
abline(v=max_C, lty=1, lwd=2)
sum(ifelse(max_repC_house>=max_C,1,0))/length(max_repC_house)

max_repS_house = apply(yrepS_house,2,max)
max_S = max(datS$S)
hist(max_repS_house, main="S", col="red", xlab="Maximum value in replicated data", las=1)
abline(v=max_S, lty=1, lwd=2)
sum(ifelse(max_repS_house>=max_S,1,0))/length(max_repS_house)

max_repMP_house = apply(yrepMP_house,2,max)
max_MP = max(datMP$MP)
hist(max_repMP_house, main="MP", col="forestgreen", xlab="Maximum value in replicated data", las=1)
abline(v=max_MP, lty=1, lwd=2)
sum(ifelse(max_repMP_house<max_MP,1,0))/length(max_repMP_house)

max_repV_house = apply(yrepV_house,2,max)
max_V = max(datV$V)
hist(max_repV_house, main="V", col="darkred", xlab="Maximum value in replicated data", las=1)
abline(v=max_V, lty=1, lwd=2)
sum(ifelse(max_repV_house>=max_V,1,0))/length(max_repV_house)

max_repSD_house = apply(yrepSD_house,2,max)
max_SD = max(datSD$SD)
hist(max_repSD_house, main="SD", col="skyblue3", xlab="Maximum observation in replicated data", las=1)
abline(v=max_SD, lty=1, lwd=2)
sum(ifelse(max_repSD_house>=max_SD,1,0))/length(max_repSD_house)
par(mfrow=c(1,1))


####### y^rep mean #########
par(mfrow=c(3,3))
mean_repM_house = apply(yrepM_house,2,mean)
mean_M = mean(datM$M)
hist(mean_repM_house, main="M", col="blue", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_M, lty=1, lwd=2)
sum(ifelse(mean_repM_house>=mean_M,1,0))/length(mean_repM_house)

mean_repL_house = apply(yrepL_house,2,mean)
mean_L = mean(datL$L)
hist(mean_repL_house, main="L", col="lightblue3", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_L, lty=1, lwd=2)
sum(ifelse(mean_repL_house>=mean_L,1,0))/length(mean_repL_house)

mean_repKD_house = apply(yrepKD_house,2,mean)
mean_KD = mean(datKD$KD)
hist(mean_repKD_house, main="KD", col="darkblue", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_KD, lty=1, lwd=2)
sum(ifelse(mean_repKD_house>=mean_KD,1,0))/length(mean_repKD_house)

mean_repC_house = apply(yrepC_house,2,mean)
mean_C = mean(datC$C)
hist(mean_repC_house, main="C", col="chartreuse3", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_C, lty=1, lwd=2)
sum(ifelse(mean_repC_house>=mean_C,1,0))/length(mean_repC_house)

mean_repS_house = apply(yrepS_house,2,mean)
mean_S = mean(datS$S)
hist(mean_repS_house, main="S", col="red", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_S, lty=1, lwd=2)
sum(ifelse(mean_repS_house>=mean_S,1,0))/length(mean_repS_house)

mean_repMP_house = apply(yrepMP_house,2,mean)
mean_MP = mean(datMP$MP)
hist(mean_repMP_house, main="MP", col="forestgreen", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_MP, lty=1, lwd=2)
sum(ifelse(mean_repMP_house>=mean_MP,1,0))/length(mean_repMP_house)

mean_repV_house = apply(yrepV_house,2,mean)
mean_V = mean(datV$V)
hist(mean_repV_house, main="V", col="darkred", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_V, lty=1, lwd=2)
sum(ifelse(mean_repV_house>=mean_V,1,0))/length(mean_repV_house)

mean_repSD_house = apply(yrepSD_house,2,mean)
mean_SD = mean(datSD$SD)
hist(mean_repSD_house, main="SD", col="skyblue3", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_SD, lty=1, lwd=2)
sum(ifelse(mean_repSD_house>=mean_SD,1,0))/length(mean_repSD_house)
par(mfrow=c(1,1))

############################################
################# MSE ######################
############################################

mseM_house = mseL_house = mseKD_house = mseC_house = mseS_house = mseMP_house = mseV_house = mseSD_house = NULL

for (j in 1:dim(yrepM_house)[2]){
  mseM_house[j] = sum((datM$M-yrepM_house[,j])^2)/length(datM$M)
  mseL_house[j] = sum((datL$L-yrepL_house[,j])^2)/length(datL$L)
  mseKD_house[j] = sum((datKD$KD-yrepKD_house[,j])^2)/length(datKD$KD)
  mseC_house[j] = sum((datC$C-yrepC_house[,j])^2)/length(datC$C)
  mseS_house[j] = sum((datS$S-yrepS_house[,j])^2)/length(datS$S)
  mseMP_house[j] = sum((datMP$MP-yrepMP_house[,j])^2)/length(datMP$MP)
  mseV_house[j] = sum((datV$V-yrepV_house[,j])^2)/length(datV$V)
  mseSD_house[j] = sum((datSD$SD-yrepSD_house[,j])^2)/length(datSD$SD)
}

par(mfrow=c(3,3))
hist(mseM_house, main="M", xlab="MSE", col="blue")
abline(v=mean(mseM_house))
hist(mseL_house, main="L", xlab="MSE", col="lightblue3")
abline(v=mean(mseL_house))
hist(mseKD_house, main="KD", xlab="MSE", col="darkblue")
abline(v=mean(mseKD_house))
hist(mseC_house, main="C", xlab="MSE", col="chartreuse3")
abline(v=mean(mseC_house))
hist(mseS_house, main="S", xlab="MSE", col="red")
abline(v=mean(mseS_house))
hist(mseMP_house, main="MP", xlab="MSE", col="forestgreen")
abline(v=mean(mseMP_house))
hist(mseV_house, main="V", xlab="MSE", col="darkred")
abline(v=mean(mseV_house))
hist(mseSD_house, main="SD", xlab="MSE", col="skyblue3")
abline(v=mean(mseSD_house))
par(mfrow=c(1,1))

mean(mseM_house);mean(mseL_house);mean(mseKD_house);mean(mseC_house);mean(mseS_house);mean(mseMP_house);mean(mseV_house);mean(mseSD_house)

############################################################
#######################   MAE   ############################
############################################################


maeM_house =maeL_house =maeKD_house = maeC_house = maeS_house = maeMP_house = maeV_house =maeSD_house = NULL
for (j in 1:dim(yrepM_house)[2]){
  maeM_house[j] = sum(abs(datM$M-yrepM_house[,j]))/length(datM$M)
  maeL_house[j] = sum(abs(datL$L-yrepL_house[,j]))/length(datL$L)
  maeKD_house[j] = sum(abs(datKD$KD-yrepKD_house[,j]))/length(datKD$KD)
  maeC_house[j] = sum(abs(datC$C-yrepC_house[,j]))/length(datC$C)
  maeS_house[j] = sum(abs(datS$S-yrepS_house[,j]))/length(datS$S)
  maeMP_house[j] = sum(abs(datMP$MP-yrepMP_house[,j]))/length(datMP$MP)
  maeV_house[j] = sum(abs(datV$V-yrepV_house[,j]))/length(datV$V)
  maeSD_house[j] = sum(abs(datSD$SD-yrepSD_house[,j]))/length(datSD$SD)
}

par(mfrow=c(3,3))
hist(maeM_house, main="M", xlab="MAE", col="blue")
abline(v=mean(maeM_house))
hist(maeL_house, main="L", xlab="MAE", col="lightblue3")
abline(v=mean(maeL_house))
hist(maeKD_house, main="KD", xlab="MAE", col="darkblue")
abline(v=mean(maeKD_house))
hist(maeC_house, main="C", xlab="MAE", col="chartreuse3")
abline(v=mean(maeC_house))
hist(maeS_house, main="S", xlab="MAE", col="red")
abline(v=mean(maeS_house))
hist(maeMP_house, main="MP", xlab="MAE", col="forestgreen")
abline(v=mean(maeMP_house))
hist(maeV_house, main="V", xlab="MAE", col="darkred")
abline(v=mean(maeV_house))
hist(maeSD_house, main="SD", xlab="MAE", col="skyblue3")
abline(v=mean(maeSD_house))
par(mfrow=c(1,1))

mean(maeM_house);mean(maeL_house);mean(maeKD_house);mean(maeC_house);mean(maeS_house);mean(maeMP_house);mean(maeV_house);mean(maeSD_house)


################################################
####### predicting election outcome 2010 #######
################################################


############# M ###################
orig.date = as.Date(datM$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datM = datM[-which(datM$collectPeriodTo>end.date),]

datM$collectPeriodFrom.num = julian(datM$collectPeriodFrom,origin=orig.date)
datM$collectPeriodTo.num = julian(datM$collectPeriodTo,origin=orig.date)
datM$fieldDate.num = floor((datM$collectPeriodFrom.num + datM$collectPeriodTo.num) / 2)
datM = datM[-nrow(datM),]

library(rjags)
e2010jags_M_house ='
model{

#measurement model
for(i in 1:npolls){
M[i] ~ dnorm(xM[day[i]]+houseM[org[i]], precM[i])
}

#dynamic model
for(i in 2:nperiods){
xM[i] ~ dnorm(xM[i-1],phiM)
}

## priors
xM[1] ~ dunif(0,1) ##weak prior?
epsM ~ dgamma(1,1)
phiM <- 1/epsM

for(i in 1:(nhouses-1)){
houseM[i] ~ dnorm(0,1)
}
houseM[12] <- 0
}
'

pM = (1 / (datM$M*(1-datM$M)/datM$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
e2010data_M_house = list(M = datM$M, precM = pM, xM = rep(NA,end.date - orig.date),
                         day = datM$fieldDate.num, npolls = nrow(datM), nperiods = as.numeric(end.date - orig.date),  
                         nhouses = length(levels(as.factor(datM$house))), org=as.numeric(as.factor(datM$house)))
writeLines(e2010jags_M_house,con="kalman_M_e10_house.bug")

system.time(jags_mod_e2010_house<- jags.model("kalman_M_e10_house.bug", data = e2010data_M_house, n.chain=3))
system.time(e2010outM_house <- coda.samples(jags_mod_e2010_house,variable.names = c("xM", "M", "houseM"), n.iter = niter, thin = 5))
e2010sumM_house = summary(e2010outM_house)
e2010cred_intM_house = HPDinterval(e2010outM_house[,which(regexpr("xM", row.names(e2010sumM_house$statistics))==1)], 0.95)

############# L ###################
orig.date = as.Date(datL$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datL = datL[-which(datL$collectPeriodTo>end.date),]

datL$collectPeriodFrom.num = julian(datL$collectPeriodFrom,origin=orig.date)
datL$collectPeriodTo.num = julian(datL$collectPeriodTo,origin=orig.date)
datL$fieldDate.num = floor((datL$collectPeriodFrom.num + datL$collectPeriodTo.num) / 2)
datL = datL[-nrow(datL),]

e2010jags_L_house ='
model{

#measurement model
for(i in 1:npolls){
L[i] ~ dnorm(xL[day[i]]+houseL[org[i]], precL[i])
}

#dynamic model
for(i in 2:nperiods){
xL[i] ~ dnorm(xL[i-1],phiL)
}

## priors
xL[1] ~ dunif(0,1) ##weak prior?
epsL ~ dgamma(1,1)
phiL <- 1/epsL

for(i in 1:(nhouses-1)){
houseL[i] ~ dnorm(0,1)
}
houseL[12] <- 0
}
'

pL = (1 / (datL$L*(1-datL$L)/datL$n)) #binomial
#pL = (1 / (datL$L*(1-datL$L)*datL$n)) #multinomial
e2010data_L_house = list(L = datL$L, precL = pL, xL = rep(NA,end.date - orig.date),
                         day = datL$fieldDate.num, npolls = nrow(datL), nperiods = as.numeric(end.date - orig.date),  
                         nhouses = length(levels(as.factor(datL$house))), org=as.numeric(as.factor(datL$house)))
writeLines(e2010jags_L_house,con="kalman_L_e10_house.bug")

system.time(jags_mod_L_e2010_house<- jags.model("kalman_L_e10_house.bug", data = e2010data_L_house, n.chain=3))
system.time(e2010outL_house <- coda.samples(jags_mod_L_e2010_house,variable.names = c("xL", "L", "houseL"), n.iter = niter, thin = 5))
e2010sumL_house = summary(e2010outL_house)
e2010cred_intL_house = HPDinterval(e2010outL_house[,which(regexpr("xL", row.names(e2010sumL_house$statistics))==1)], 0.95)


############# KD ###################
orig.date = as.Date(datKD$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datKD = datKD[-which(datKD$collectPeriodTo>end.date),]

datKD$collectPeriodFrom.num = julian(datKD$collectPeriodFrom,origin=orig.date)
datKD$collectPeriodTo.num = julian(datKD$collectPeriodTo,origin=orig.date)
datKD$fieldDate.num = floor((datKD$collectPeriodFrom.num + datKD$collectPeriodTo.num) / 2)
datKD = datKD[-nrow(datKD),]

e2010jags_KD_house ='
model{

#measurement model
for(i in 1:npolls){
KD[i] ~ dnorm(xKD[day[i]]+houseKD[org[i]], precKD[i])
}

#dynamic model
for(i in 2:nperiods){
xKD[i] ~ dnorm(xKD[i-1],phiKD)
}

## priors
xKD[1] ~ dunif(0,1) ##weak prior?
epsKD ~ dgamma(1,1)
phiKD <- 1/epsKD

for(i in 1:(nhouses-1)){
houseKD[i] ~ dnorm(0,1)
}
houseKD[12] <- 0
}
'

pKD = (1 / (datKD$KD*(1-datKD$KD)/datKD$n)) #binomial
#pKD = (1 / (datKD$KD*(1-datKD$KD)*datKD$n)) #multinomial
e2010data_KD_house = list(KD = datKD$KD, precKD = pKD, xKD = rep(NA,end.date - orig.date),
                          day = datKD$fieldDate.num, npolls = nrow(datKD), nperiods = as.numeric(end.date - orig.date),  
                          nhouses = length(levels(as.factor(datKD$house))), org=as.numeric(as.factor(datKD$house)))
writeLines(e2010jags_KD_house,con="kalman_KD_e10_house.bug")

system.time(jags_mod_KD_e2010_house<- jags.model("kalman_KD_e10_house.bug", data = e2010data_KD_house, n.chain=3))
system.time(e2010outKD_house <- coda.samples(jags_mod_KD_e2010_house,variable.names = c("xKD", "KD", "houseKD"), n.iter = niter, thin = 5))
e2010sumKD_house = summary(e2010outKD_house)
e2010cred_intKD_house = HPDinterval(e2010outKD_house[,which(regexpr("xKD", row.names(e2010sumKD_house$statistics))==1)], 0.95)

############# C ###################
orig.date = as.Date(datC$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datC = datC[-which(datC$collectPeriodTo>end.date),]

datC$collectPeriodFrom.num = julian(datC$collectPeriodFrom,origin=orig.date)
datC$collectPeriodTo.num = julian(datC$collectPeriodTo,origin=orig.date)
datC$fieldDate.num = floor((datC$collectPeriodFrom.num + datC$collectPeriodTo.num) / 2)
datC = datC[-nrow(datC),]

e2010jags_C_house ='
model{

#measurement model
for(i in 1:npolls){
C[i] ~ dnorm(xC[day[i]]+houseC[org[i]], precC[i])
}

#dynamic model
for(i in 2:nperiods){
xC[i] ~ dnorm(xC[i-1],phiC)
}

## priors
xC[1] ~ dunif(0,1) ##weak prior?
epsC ~ dgamma(1,1)
phiC <- 1/epsC

for(i in 1:(nhouses-1)){
houseC[i] ~ dnorm(0,1)
}
houseC[12] <- 0
}
'

pC = (1 / (datC$C*(1-datC$C)/datC$n)) #binomial
#pC = (1 / (datC$C*(1-datC$C)*datC$n)) #multinomial
e2010data_C_house = list(C = datC$C, precC = pC, xC = rep(NA,end.date - orig.date),
                         day = datC$fieldDate.num, npolls = nrow(datC), nperiods = as.numeric(end.date - orig.date),  
                         nhouses = length(levels(as.factor(datC$house))), org=as.numeric(as.factor(datC$house)))
writeLines(e2010jags_C_house,con="kalman_C_e10_house.bug")

system.time(jags_mod_C_e2010_house<- jags.model("kalman_C_e10_house.bug", data = e2010data_C_house, n.chain=3))
system.time(e2010outC_house <- coda.samples(jags_mod_C_e2010_house,variable.names = c("xC", "C", "houseC"), n.iter = niter, thin = 5))
e2010sumC_house = summary(e2010outC_house)
e2010cred_intC_house = HPDinterval(e2010outC_house[,which(regexpr("xC", row.names(e2010sumC_house$statistics))==1)], 0.95)


############# S ###################
orig.date = as.Date(datS$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datS = datS[-which(datS$collectPeriodTo>end.date),]

datS$collectPeriodFrom.num = julian(datS$collectPeriodFrom,origin=orig.date)
datS$collectPeriodTo.num = julian(datS$collectPeriodTo,origin=orig.date)
datS$fieldDate.num = floor((datS$collectPeriodFrom.num + datS$collectPeriodTo.num) / 2)
datS = datS[-nrow(datS),]

e2010jags_S_house ='
model{

#measurement model
for(i in 1:npolls){
S[i] ~ dnorm(xS[day[i]]+houseS[org[i]], precS[i])
}

#dynamic model
for(i in 2:nperiods){
xS[i] ~ dnorm(xS[i-1],phiS)
}

## priors
xS[1] ~ dunif(0,1) ##weak prior?
epsS ~ dgamma(1,1)
phiS <- 1/epsS

for(i in 1:(nhouses-1)){
houseS[i] ~ dnorm(0,1)
}
houseS[12] <- 0
}
'

pS = (1 / (datS$S*(1-datS$S)/datS$n)) #binomial
#pS = (1 / (datS$S*(1-datS$S)*datS$n)) #multinomial
e2010data_S_house = list(S = datS$S, precS = pS, xS = rep(NA,end.date - orig.date),
                         day = datS$fieldDate.num, npolls = nrow(datS), nperiods = as.numeric(end.date - orig.date),  
                         nhouses = length(levels(as.factor(datS$house))), org=as.numeric(as.factor(datS$house)))
writeLines(e2010jags_S_house,con="kalman_S_e10_house.bug")

system.time(jags_mod_S_e2010_house<- jags.model("kalman_S_e10_house.bug", data = e2010data_S_house, n.chain=3))
system.time(e2010outS_house <- coda.samples(jags_mod_S_e2010_house,variable.names = c("xS", "S", "houseS"), n.iter = niter, thin = 5))
e2010sumS_house = summary(e2010outS_house)
e2010cred_intS_house = HPDinterval(e2010outS_house[,which(regexpr("xS", row.names(e2010sumS_house$statistics))==1)], 0.95)

############# MP ###################
orig.date = as.Date(datMP$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datMP = datMP[-which(datMP$collectPeriodTo>end.date),]

datMP$collectPeriodFrom.num = julian(datMP$collectPeriodFrom,origin=orig.date)
datMP$collectPeriodTo.num = julian(datMP$collectPeriodTo,origin=orig.date)
datMP$fieldDate.num = floor((datMP$collectPeriodFrom.num + datMP$collectPeriodTo.num) / 2)
datMP = datMP[-nrow(datMP),]

e2010jags_MP_house ='
model{

#measurement model
for(i in 1:npolls){
MP[i] ~ dnorm(xMP[day[i]]+houseMP[org[i]], precMP[i])
}

#dynamic model
for(i in 2:nperiods){
xMP[i] ~ dnorm(xMP[i-1],phiMP)
}

## priors
xMP[1] ~ dunif(0,1) ##weak prior?
epsMP ~ dgamma(1,1)
phiMP <- 1/epsMP

for(i in 1:(nhouses-1)){
houseMP[i] ~ dnorm(0,1)
}
houseMP[12] <- 0
}
'

pMP = (1 / (datMP$MP*(1-datMP$MP)/datMP$n)) #binomial
#pMP = (1 / (datMP$MP*(1-datMP$MP)*datMP$n)) #multinomial
e2010data_MP_house = list(MP = datMP$MP, precMP = pMP, xMP = rep(NA,end.date - orig.date),
                          day = datMP$fieldDate.num, npolls = nrow(datMP), nperiods = as.numeric(end.date - orig.date),  
                          nhouses = length(levels(as.factor(datMP$house))), org=as.numeric(as.factor(datMP$house)))
writeLines(e2010jags_MP_house,con="kalman_MP_e10_house.bug")

system.time(jags_mod_MP_e2010_house<- jags.model("kalman_MP_e10_house.bug", data = e2010data_MP_house, n.chain=3))
system.time(e2010outMP_house <- coda.samples(jags_mod_MP_e2010_house,variable.names = c("xMP", "MP", "houseMP"), n.iter = niter, thin = 5))
e2010sumMP_house = summary(e2010outMP_house)
e2010cred_intMP_house = HPDinterval(e2010outMP_house[,which(regexpr("xMP", row.names(e2010sumMP_house$statistics))==1)], 0.95)

############# V ###################
orig.date = as.Date(datV$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datV = datV[-which(datV$collectPeriodTo>end.date),]

datV$collectPeriodFrom.num = julian(datV$collectPeriodFrom,origin=orig.date)
datV$collectPeriodTo.num = julian(datV$collectPeriodTo,origin=orig.date)
datV$fieldDate.num = floor((datV$collectPeriodFrom.num + datV$collectPeriodTo.num) / 2)
datV = datV[-nrow(datV),]

e2010jags_V_house ='
model{

#measurement model
for(i in 1:npolls){
V[i] ~ dnorm(xV[day[i]]+houseV[org[i]], precV[i])
}

#dynamic model
for(i in 2:nperiods){
xV[i] ~ dnorm(xV[i-1],phiV)
}

## priors
xV[1] ~ dunif(0,1) ##weak prior?
epsV ~ dgamma(1,1)
phiV <- 1/epsV

for(i in 1:(nhouses-1)){
houseV[i] ~ dnorm(0,1)
}
houseV[12] <- 0
}
'

pV = (1 / (datV$V*(1-datV$V)/datV$n)) #binomial
#pV = (1 / (datV$V*(1-datV$V)*datV$n)) #multinomial
e2010data_V_house = list(V = datV$V, precV = pV, xV = rep(NA,end.date - orig.date),
                         day = datV$fieldDate.num, npolls = nrow(datV), nperiods = as.numeric(end.date - orig.date),  
                         nhouses = length(levels(as.factor(datV$house))), org=as.numeric(as.factor(datV$house)))
writeLines(e2010jags_V_house,con="kalman_V_e10_house.bug")

system.time(jags_mod_V_e2010_house<- jags.model("kalman_V_e10_house.bug", data = e2010data_V_house, n.chain=3))
system.time(e2010outV_house <- coda.samples(jags_mod_V_e2010_house,variable.names = c("xV", "V", "houseV"), n.iter = niter, thin = 5))
e2010sumV_house = summary(e2010outV_house)
e2010cred_intV_house = HPDinterval(e2010outV_house[,which(regexpr("xV", row.names(e2010sumV_house$statistics))==1)], 0.95)

############# SD ###################
orig.date = as.Date(datSD$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datSD = datSD[-which(datSD$collectPeriodTo>end.date),]

datSD$collectPeriodFrom.num = julian(datSD$collectPeriodFrom,origin=orig.date)
datSD$collectPeriodTo.num = julian(datSD$collectPeriodTo,origin=orig.date)
datSD$fieldDate.num = floor((datSD$collectPeriodFrom.num + datSD$collectPeriodTo.num) / 2)
datSD = datSD[-nrow(datSD),]

e2010jags_SD_house ='
model{

#measurement model
for(i in 1:npolls){
SD[i] ~ dnorm(xSD[day[i]]+houseSD[org[i]], precSD[i])
}

#dynamic model
for(i in 2:nperiods){
xSD[i] ~ dnorm(xSD[i-1],phiSD)
}

## priors
xSD[1] ~ dunif(0,1) ##weak prior?
epsSD ~ dgamma(1,1)
phiSD <- 1/epsSD

for(i in 1:(nhouses-1)){
houseSD[i] ~ dnorm(0,1)
}
houseSD[12] <- 0
}
'

pSD = (1 / (datSD$SD*(1-datSD$SD)/datSD$n)) #binomial
#pSD = (1 / (datSD$SD*(1-datSD$SD)*datSD$n)) #multinomial
e2010data_SD_house = list(SD = datSD$SD, precSD = pSD, xSD = rep(NA,end.date - orig.date),
                          day = datSD$fieldDate.num, npolls = nrow(datSD), nperiods = as.numeric(end.date - orig.date),  
                          nhouses = length(levels(as.factor(datSD$house))), org=as.numeric(as.factor(datSD$house)))
writeLines(e2010jags_SD_house,con="kalman_SD_e10_house.bug")

system.time(jags_mod_SD_e2010_house<- jags.model("kalman_SD_e10_house.bug", data = e2010data_SD_house, n.chain=3))
system.time(e2010outSD_house <- coda.samples(jags_mod_SD_e2010_house,variable.names = c("xSD", "SD", "houseSD"), n.iter = niter, thin = 5))
e2010sumSD_house = summary(e2010outSD_house)
e2010cred_intSD_house = HPDinterval(e2010outSD_house[,which(regexpr("xSD", row.names(e2010sumSD_house$statistics))==1)], 0.95)

#######################################################
sumM = e2010sumM_house;outM = e2010outM_house;sumL = e2010sumL_house;outL = e2010outL_house;sumKD = e2010sumKD_house
outKD = e2010outKD_house;sumC = e2010sumC_house;outC = e2010outC_house;sumS = e2010sumS_house;outS = e2010outS_house
sumMP = e2010sumMP_house;outMP = e2010outMP_house;sumV = e2010sumV_house;outV = e2010outV_house;sumSD = e2010sumSD_house;outSD = e2010outSD_house

meanM = sumM$statistics[which(regexpr("xM", row.names(sumM$statistics))==1),1];meanL = sumL$statistics[which(regexpr("xL", row.names(sumL$statistics))==1),1]
meanKD = sumKD$statistics[which(regexpr("xKD", row.names(sumKD$statistics))==1),1];meanC = sumC$statistics[which(regexpr("xC", row.names(sumC$statistics))==1),1]
meanS = sumS$statistics[which(regexpr("xS", row.names(sumS$statistics))==1),1];meanMP = sumMP$statistics[which(regexpr("xMP", row.names(sumMP$statistics))==1),1]
meanV = sumV$statistics[which(regexpr("xV", row.names(sumV$statistics))==1),1];meanSD = sumSD$statistics[which(regexpr("xSD", row.names(sumSD$statistics))==1),1]

meanM[length(meanM)]; e2010cred_intM_house[[1]][nrow(e2010cred_intM_house[[1]]),];meanL[length(meanL)]; e2010cred_intL_house[[1]][nrow(e2010cred_intL_house[[1]]),]
meanKD[length(meanKD)]; e2010cred_intKD_house[[1]][nrow(e2010cred_intKD_house[[1]]),];meanC[length(meanC)]; e2010cred_intC_house[[1]][nrow(e2010cred_intC_house[[1]]),]
meanS[length(meanS)]; e2010cred_intS_house[[1]][nrow(e2010cred_intS_house[[1]]),];meanMP[length(meanMP)]; e2010cred_intMP_house[[1]][nrow(e2010cred_intMP_house[[1]]),]
meanV[length(meanV)]; e2010cred_intV_house[[1]][nrow(e2010cred_intV_house[[1]]),];meanSD[length(meanSD)]; e2010cred_intSD_house[[1]][nrow(e2010cred_intSD_house[[1]]),]

meanM[length(meanM)]-elec[3,1];meanL[length(meanL)]-elec[3,2];meanKD[length(meanKD)]-elec[3,3]
meanC[length(meanC)]-elec[3,4];meanS[length(meanS)]-elec[3,5];meanMP[length(meanMP)]-elec[3,6]
meanV[length(meanV)]-elec[3,7];meanSD[length(meanSD)]-elec[3,8]

################################################
####### predicting election outcome 2014 #######
########## kör om all univariate data ##########
################################################


############# M ###################
orig.date = as.Date(datM$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datM = datM[-which(datM$collectPeriodTo>end.date),]

datM$collectPeriodFrom.num = julian(datM$collectPeriodFrom,origin=orig.date)
datM$collectPeriodTo.num = julian(datM$collectPeriodTo,origin=orig.date)
datM$fieldDate.num = floor((datM$collectPeriodFrom.num + datM$collectPeriodTo.num) / 2)
datM = datM[-nrow(datM),]

e2014jags_M_house ='
model{

#measurement model
for(i in 1:npolls){
M[i] ~ dnorm(xM[day[i]]+houseM[org[i]], precM[i])
}

#dynamic model
for(i in 2:nperiods){
xM[i] ~ dnorm(xM[i-1],phiM)
}

## priors
xM[1] ~ dunif(0,1) ##weak prior?
epsM ~ dgamma(1,1)
phiM <- 1/epsM

for(i in 1:(nhouses-1)){
houseM[i] ~ dnorm(0,1)
}
houseM[12] <- 0
}
'

pM = (1 / (datM$M*(1-datM$M)/datM$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
e2014data_M_house = list(M = datM$M, precM = pM, xM = rep(NA,end.date - orig.date),
                         day = datM$fieldDate.num, npolls = nrow(datM), nperiods = as.numeric(end.date - orig.date),  
                         nhouses = length(levels(as.factor(datM$house))), org=as.numeric(as.factor(datM$house)))
writeLines(e2014jags_M_house,con="kalman_M_e14_house.bug")

system.time(jags_mod_M_e2014_house<- jags.model("kalman_M_e14_house.bug", data = e2014data_M_house, n.chain=3))
system.time(e2014outM_house <- coda.samples(jags_mod_M_e2014_house,variable.names = c("xM", "M", "houseM"), n.iter = niter, thin = 5))
e2014sumM_house = summary(e2014outM_house)
e2014cred_intM_house = HPDinterval(e2014outM_house[,which(regexpr("xM", row.names(e2014sumM_house$statistics))==1)], 0.95)


############# L ###################
orig.date = as.Date(datL$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datL = datL[-which(datL$collectPeriodTo>end.date),]

datL$collectPeriodFrom.num = julian(datL$collectPeriodFrom,origin=orig.date)
datL$collectPeriodTo.num = julian(datL$collectPeriodTo,origin=orig.date)
datL$fieldDate.num = floor((datL$collectPeriodFrom.num + datL$collectPeriodTo.num) / 2)
datL = datL[-nrow(datL),]

e2014jags_L_house ='
model{

#measurement model
for(i in 1:npolls){
L[i] ~ dnorm(xL[day[i]]+houseL[org[i]], precL[i])
}

#dynamic model
for(i in 2:nperiods){
xL[i] ~ dnorm(xL[i-1],phiL)
}

## priors
xL[1] ~ dunif(0,1) ##weak prior?
epsL ~ dgamma(1,1)
phiL <- 1/epsL

for(i in 1:(nhouses-1)){
houseL[i] ~ dnorm(0,1)
}
houseL[12] <- 0
}
'

pL = (1 / (datL$L*(1-datL$L)/datL$n)) #binomial
#pL = (1 / (datL$L*(1-datL$L)*datL$n)) #multinomial
e2014data_L_house = list(L = datL$L, precL = pL, xL = rep(NA,end.date - orig.date),
                         day = datL$fieldDate.num, npolls = nrow(datL), nperiods = as.numeric(end.date - orig.date),  
                         nhouses = length(levels(as.factor(datL$house))), org=as.numeric(as.factor(datL$house)))
writeLines(e2014jags_L_house,con="kalman_L_e14_house.bug")

system.time(jags_mod_L_e2014_house<- jags.model("kalman_L_e14_house.bug", data = e2014data_L_house, n.chain=3))
system.time(e2014outL_house <- coda.samples(jags_mod_L_e2014_house,variable.names = c("xL", "L", "houseL"), n.iter = niter, thin = 5))
e2014sumL_house = summary(e2014outL_house)
e2014cred_intL_house = HPDinterval(e2014outL_house[,which(regexpr("xL", row.names(e2014sumL_house$statistics))==1)], 0.95)



############# KD ###################
orig.date = as.Date(datKD$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datKD = datKD[-which(datKD$collectPeriodTo>end.date),]

datKD$collectPeriodFrom.num = julian(datKD$collectPeriodFrom,origin=orig.date)
datKD$collectPeriodTo.num = julian(datKD$collectPeriodTo,origin=orig.date)
datKD$fieldDate.num = floor((datKD$collectPeriodFrom.num + datKD$collectPeriodTo.num) / 2)
datKD = datKD[-nrow(datKD),]

e2014jags_KD_house ='
model{

#measurement model
for(i in 1:npolls){
KD[i] ~ dnorm(xKD[day[i]]+houseKD[org[i]], precKD[i])
}

#dynamic model
for(i in 2:nperiods){
xKD[i] ~ dnorm(xKD[i-1],phiKD)
}

## priors
xKD[1] ~ dunif(0,1) ##weak prior?
epsKD ~ dgamma(1,1)
phiKD <- 1/epsKD

for(i in 1:(nhouses-1)){
houseKD[i] ~ dnorm(0,1)
}
houseKD[12] <- 0
}
'

pKD = (1 / (datKD$KD*(1-datKD$KD)/datKD$n)) #binomial
#pKD = (1 / (datKD$KD*(1-datKD$KD)*datKD$n)) #multinomial
e2014data_KD_house = list(KD = datKD$KD, precKD = pKD, xKD = rep(NA,end.date - orig.date),
                          day = datKD$fieldDate.num, npolls = nrow(datKD), nperiods = as.numeric(end.date - orig.date),  
                          nhouses = length(levels(as.factor(datKD$house))), org=as.numeric(as.factor(datKD$house)))
writeLines(e2014jags_KD_house,con="kalman_KD_e14_house.bug")

system.time(jags_mod_KD_e2014_house<- jags.model("kalman_KD_e14_house.bug", data = e2014data_KD_house, n.chain=3))
system.time(e2014outKD_house <- coda.samples(jags_mod_KD_e2014_house,variable.names = c("xKD", "KD", "houseKD"), n.iter = niter, thin = 5))
e2014sumKD_house = summary(e2014outKD_house)
e2014cred_intKD_house = HPDinterval(e2014outKD_house[,which(regexpr("xKD", row.names(e2014sumKD_house$statistics))==1)], 0.95)



############# C ###################
orig.date = as.Date(datC$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datC = datC[-which(datC$collectPeriodTo>end.date),]

datC$collectPeriodFrom.num = julian(datC$collectPeriodFrom,origin=orig.date)
datC$collectPeriodTo.num = julian(datC$collectPeriodTo,origin=orig.date)
datC$fieldDate.num = floor((datC$collectPeriodFrom.num + datC$collectPeriodTo.num) / 2)
datC = datC[-nrow(datC),]

e2014jags_C_house ='
model{

#measurement model
for(i in 1:npolls){
C[i] ~ dnorm(xC[day[i]]+houseC[org[i]], precC[i])
}

#dynamic model
for(i in 2:nperiods){
xC[i] ~ dnorm(xC[i-1],phiC)
}

## priors
xC[1] ~ dunif(0,1) ##weak prior?
epsC ~ dgamma(1,1)
phiC <- 1/epsC

for(i in 1:(nhouses-1)){
houseC[i] ~ dnorm(0,1)
}
houseC[12] <- 0
}
'

pC = (1 / (datC$C*(1-datC$C)/datC$n)) #binomial
#pC = (1 / (datC$C*(1-datC$C)*datC$n)) #multinomial
e2014data_C_house = list(C = datC$C, precC = pC, xC = rep(NA,end.date - orig.date),
                         day = datC$fieldDate.num, npolls = nrow(datC), nperiods = as.numeric(end.date - orig.date),  
                         nhouses = length(levels(as.factor(datC$house))), org=as.numeric(as.factor(datC$house)))
writeLines(e2014jags_C_house,con="kalman_C_e14_house.bug")

system.time(jags_mod_C_e2014_house<- jags.model("kalman_C_e14_house.bug", data = e2014data_C_house, n.chain=3))
system.time(e2014outC_house <- coda.samples(jags_mod_C_e2014_house,variable.names = c("xC", "C", "houseC"), n.iter = niter, thin = 5))
e2014sumC_house = summary(e2014outC_house)
e2014cred_intC_house = HPDinterval(e2014outC_house[,which(regexpr("xC", row.names(e2014sumC_house$statistics))==1)], 0.95)


############# S ###################
orig.date = as.Date(datS$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datS = datS[-which(datS$collectPeriodTo>end.date),]

datS$collectPeriodFrom.num = julian(datS$collectPeriodFrom,origin=orig.date)
datS$collectPeriodTo.num = julian(datS$collectPeriodTo,origin=orig.date)
datS$fieldDate.num = floor((datS$collectPeriodFrom.num + datS$collectPeriodTo.num) / 2)
datS = datS[-nrow(datS),]

e2014jags_S_house ='
model{

#measurement model
for(i in 1:npolls){
S[i] ~ dnorm(xS[day[i]]+houseS[org[i]], precS[i])
}

#dynamic model
for(i in 2:nperiods){
xS[i] ~ dnorm(xS[i-1],phiS)
}

## priors
xS[1] ~ dunif(0,1) ##weak prior?
epsS ~ dgamma(1,1)
phiS <- 1/epsS

for(i in 1:(nhouses-1)){
houseS[i] ~ dnorm(0,1)
}
houseS[12] <- 0
}
'

pS = (1 / (datS$S*(1-datS$S)/datS$n)) #binomial
#pS = (1 / (datS$S*(1-datS$S)*datS$n)) #multinomial
e2014data_S_house = list(S = datS$S, precS = pS, xS = rep(NA,end.date - orig.date),
                          day = datS$fieldDate.num, npolls = nrow(datS), nperiods = as.numeric(end.date - orig.date),  
                          nhouses = length(levels(as.factor(datS$house))), org=as.numeric(as.factor(datS$house)))
writeLines(e2014jags_S_house,con="kalman_S_e14_house.bug")

system.time(jags_mod_S_e2014_house<- jags.model("kalman_S_e14_house.bug", data = e2014data_S_house, n.chain=3))
system.time(e2014outS_house <- coda.samples(jags_mod_S_e2014_house,variable.names = c("xS", "S", "houseS"), n.iter = niter, thin = 5))
e2014sumS_house = summary(e2014outS_house)
e2014cred_intS_house = HPDinterval(e2014outS_house[,which(regexpr("xS", row.names(e2014sumS_house$statistics))==1)], 0.95)


############# MP ###################
orig.date = as.Date(datMP$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datMP = datMP[-which(datMP$collectPeriodTo>end.date),]

datMP$collectPeriodFrom.num = julian(datMP$collectPeriodFrom,origin=orig.date)
datMP$collectPeriodTo.num = julian(datMP$collectPeriodTo,origin=orig.date)
datMP$fieldDate.num = floor((datMP$collectPeriodFrom.num + datMP$collectPeriodTo.num) / 2)
datMP = datMP[-nrow(datMP),]

e2014jags_MP_house ='
model{

#measurement model
for(i in 1:npolls){
MP[i] ~ dnorm(xMP[day[i]]+houseMP[org[i]], precMP[i])
}

#dynamic model
for(i in 2:nperiods){
xMP[i] ~ dnorm(xMP[i-1],phiMP)
}

## priors
xMP[1] ~ dunif(0,1) ##weak prior?
epsMP ~ dgamma(1,1)
phiMP <- 1/epsMP

for(i in 1:(nhouses-1)){
houseMP[i] ~ dnorm(0,1)
}
houseMP[12] <- 0
}
'

pMP = (1 / (datMP$MP*(1-datMP$MP)/datMP$n)) #binomial
#pMP = (1 / (datMP$MP*(1-datMP$MP)*datMP$n)) #multinomial
e2014data_MP_house = list(MP = datMP$MP, precMP = pMP, xMP = rep(NA,end.date - orig.date),
                          day = datMP$fieldDate.num, npolls = nrow(datMP), nperiods = as.numeric(end.date - orig.date),  
                          nhouses = length(levels(as.factor(datMP$house))), org=as.numeric(as.factor(datMP$house)))
writeLines(e2014jags_MP_house,con="kalman_MP_e14_house.bug")

system.time(jags_mod_MP_e2014_house<- jags.model("kalman_MP_e14_house.bug", data = e2014data_MP_house, n.chain=3))
system.time(e2014outMP_house <- coda.samples(jags_mod_MP_e2014_house,variable.names = c("xMP", "MP", "houseMP"), n.iter = niter, thin = 5))
e2014sumMP_house = summary(e2014outMP_house)
e2014cred_intMP_house = HPDinterval(e2014outMP_house[,which(regexpr("xMP", row.names(e2014sumMP_house$statistics))==1)], 0.95)


############# V ###################
orig.date = as.Date(datV$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datV = datV[-which(datV$collectPeriodTo>end.date),]

datV$collectPeriodFrom.num = julian(datV$collectPeriodFrom,origin=orig.date)
datV$collectPeriodTo.num = julian(datV$collectPeriodTo,origin=orig.date)
datV$fieldDate.num = floor((datV$collectPeriodFrom.num + datV$collectPeriodTo.num) / 2)
datV = datV[-nrow(datV),]

e2014jags_V_house ='
model{

#measurement model
for(i in 1:npolls){
V[i] ~ dnorm(xV[day[i]]+houseV[org[i]], precV[i])
}

#dynamic model
for(i in 2:nperiods){
xV[i] ~ dnorm(xV[i-1],phiV)
}

## priors
xV[1] ~ dunif(0,1) ##weak prior?
epsV ~ dgamma(1,1)
phiV <- 1/epsV

for(i in 1:(nhouses-1)){
houseV[i] ~ dnorm(0,1)
}
houseV[12] <- 0
}
'

pV = (1 / (datV$V*(1-datV$V)/datV$n)) #binomial
#pV = (1 / (datV$V*(1-datV$V)*datV$n)) #multinomial
e2014data_V_house = list(V = datV$V, precV = pV, xV = rep(NA,end.date - orig.date),
                          day = datV$fieldDate.num, npolls = nrow(datV), nperiods = as.numeric(end.date - orig.date),  
                          nhouses = length(levels(as.factor(datV$house))), org=as.numeric(as.factor(datV$house)))
writeLines(e2014jags_V_house,con="kalman_V_e14_house.bug")

system.time(jags_mod_V_e2014_house<- jags.model("kalman_V_e14_house.bug", data = e2014data_V_house, n.chain=3))
system.time(e2014outV_house <- coda.samples(jags_mod_V_e2014_house,variable.names = c("xV", "V", "houseV"), n.iter = niter, thin = 5))
e2014sumV_house = summary(e2014outV_house)
e2014cred_intV_house = HPDinterval(e2014outV_house[,which(regexpr("xV", row.names(e2014sumV_house$statistics))==1)], 0.95)



############# SD ###################
orig.date = as.Date(datSD$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datSD = datSD[-which(datSD$collectPeriodTo>end.date),]

datSD$collectPeriodFrom.num = julian(datSD$collectPeriodFrom,origin=orig.date)
datSD$collectPeriodTo.num = julian(datSD$collectPeriodTo,origin=orig.date)
datSD$fieldDate.num = floor((datSD$collectPeriodFrom.num + datSD$collectPeriodTo.num) / 2)
datSD = datSD[-nrow(datSD),]

e2014jags_SD_house ='
model{

#measurement model
for(i in 1:npolls){
SD[i] ~ dnorm(xSD[day[i]]+houseSD[org[i]], precSD[i])
}

#dynamic model
for(i in 2:nperiods){
xSD[i] ~ dnorm(xSD[i-1],phiSD)
}

## priors
xSD[1] ~ dunif(0,1) ##weak prior?
epsSD ~ dgamma(1,1)
phiSD <- 1/epsSD

for(i in 1:(nhouses-1)){
houseSD[i] ~ dnorm(0,1)
}
houseSD[12] <- 0
}
'

pSD = (1 / (datSD$SD*(1-datSD$SD)/datSD$n)) #binomial
#pSD = (1 / (datSD$SD*(1-datSD$SD)*datSD$n)) #multinomial
e2014data_SD_house = list(SD = datSD$SD, precSD = pSD, xSD = rep(NA,end.date - orig.date),
                          day = datSD$fieldDate.num, npolls = nrow(datSD), nperiods = as.numeric(end.date - orig.date),  
                          nhouses = length(levels(as.factor(datSD$house))), org=as.numeric(as.factor(datSD$house)))
writeLines(e2014jags_SD_house,con="kalman_SD_e14_house.bug")

system.time(jags_mod_SD_e2014_house<- jags.model("kalman_SD_e14_house.bug", data = e2014data_SD_house, n.chain=3))
system.time(e2014outSD_house <- coda.samples(jags_mod_SD_e2014_house,variable.names = c("xSD", "SD", "houseSD"), n.iter = niter, thin = 5))
e2014sumSD_house = summary(e2014outSD_house)
e2014cred_intSD_house = HPDinterval(e2014outSD_house[,which(regexpr("xSD", row.names(e2014sumSD_house$statistics))==1)], 0.95)

#################################

sumM = e2014sumM_house;outM = e2014outM_house;sumL = e2014sumL_house;outL = e2014outL_house;sumKD = e2014sumKD_house
outKD = e2014outKD_house;sumC = e2014sumC_house;outC = e2014outC_house;sumS = e2014sumS_house;outS = e2014outS_house
sumMP = e2014sumMP_house;outMP = e2014outMP_house;sumV = e2014sumV_house;outV = e2014outV_house;sumSD = e2014sumSD_house;outSD = e2014outSD_house

meanM = sumM$statistics[which(regexpr("xM", row.names(sumM$statistics))==1),1];meanL = sumL$statistics[which(regexpr("xL", row.names(sumL$statistics))==1),1]
meanKD = sumKD$statistics[which(regexpr("xKD", row.names(sumKD$statistics))==1),1];meanC = sumC$statistics[which(regexpr("xC", row.names(sumC$statistics))==1),1]
meanS = sumS$statistics[which(regexpr("xS", row.names(sumS$statistics))==1),1];meanMP = sumMP$statistics[which(regexpr("xMP", row.names(sumMP$statistics))==1),1]
meanV = sumV$statistics[which(regexpr("xV", row.names(sumV$statistics))==1),1];meanSD = sumSD$statistics[which(regexpr("xSD", row.names(sumSD$statistics))==1),1]

meanM[length(meanM)]; e2014cred_intM_house[[1]][nrow(e2014cred_intM_house[[1]]),];meanL[length(meanL)]; e2014cred_intL_house[[1]][nrow(e2014cred_intL_house[[1]]),]
meanKD[length(meanKD)]; e2014cred_intKD_house[[1]][nrow(e2014cred_intKD_house[[1]]),];meanC[length(meanC)]; e2014cred_intC_house[[1]][nrow(e2014cred_intC_house[[1]]),]
meanS[length(meanS)]; e2014cred_intS_house[[1]][nrow(e2014cred_intS_house[[1]]),];meanMP[length(meanMP)]; e2014cred_intMP_house[[1]][nrow(e2014cred_intMP_house[[1]]),]
meanV[length(meanV)]; e2014cred_intV_house[[1]][nrow(e2014cred_intV_house[[1]]),];meanSD[length(meanSD)]; e2014cred_intSD_house[[1]][nrow(e2014cred_intSD_house[[1]]),]

meanM[length(meanM)]-elec[4,1];meanL[length(meanL)]-elec[4,2];meanKD[length(meanKD)]-elec[4,3]
meanC[length(meanC)]-elec[4,4];meanS[length(meanS)]-elec[4,5];meanMP[length(meanMP)]-elec[4,6]
meanV[length(meanV)]-elec[4,7];meanSD[length(meanSD)]-elec[4,8]






