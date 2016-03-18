dateDiff = datM[,3] - datM[,2]
dateDiff2 = dateDiff+1
nDay = datM$n/as.numeric(dateDiff2)
MSmooth = rep(datM$M,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datM$house, dateDiff2)
orig.date = as.Date("2006-09-16") #day before election 2011
end.date = as.Date('2014-09-14') #election day 2014

ee = list()
for(i in 1:nrow(datM)){
  ee[[i]] = seq(datM[i,2], datM[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datM)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datM2 = data.frame(M = MSmooth, fieldDate=dateSmooth, fieldDate.num = dateSmooth.num, n = nSmooth, house = houseSmooth)

jags_M2 ='
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

epsM ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiM <- 1/epsM
}
'

pM2 = (1 / (datM2$M*(1-datM2$M)/datM2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_M2 = list(M = datM2$M, precM = pM2, xM = c(rnorm(1,elec[2,1], 0.00001),rep(NA,end.date - orig.date-1)),
               day = datM2$fieldDate.num, npolls = nrow(datM2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_M2,con="kalman_M2.bug")

system.time(jags_mod_M2 <- jags.model("kalman_M2.bug", data = data_M2, n.chains = 3))

ninter = 10000
system.time(outM2 <- coda.samples(jags_mod_M2,variable.names = c("xM", "M"), n.iter = ninter, n.thin = 1000))
sumM2 = summary(outM2)
cred_intM2 = HPDinterval(outM2[,which(regexpr("xM", row.names(sumM2$statistics))==1)], 0.95)
#sumM$quantiles[elec.day[3],c(1,5)] 



############### L ################
dateDiffL = datL[,3] - datL[,2]
dateDiffL2 = dateDiffL+1
nDayL = datL$n/as.numeric(dateDiffL2)
LSmooth = rep(datL$L,dateDiffL2)
LnSmooth = rep(nDayL,dateDiffL2)
houseSmoothL = rep(datL$house, dateDiffL2)
orig.date = as.Date("2006-09-16") #day before election 2011
end.date = as.Date('2014-09-14') #election day 2014

eeL = list()
for(i in 1:nrow(datL)){
  eeL[[i]] = seq(datL[i,2], datL[i,3], by="days")
}

dateSmoothL = eeL[[1]]
for(i in 2:nrow(datL)){
  dateSmoothL = c(dateSmoothL, eeL[[i]])
}

dateSmooth.numL = julian(dateSmoothL,origin=orig.date)
datL2 = data.frame(L = LSmooth, fieldDate=dateSmoothL, fieldDate.num = dateSmooth.numL, n = LnSmooth, house = houseSmoothL)

jags_L2 ='
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

epsL ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiL <- 1/epsL
}
'

pL2 = (1 / (datL2$L*(1-datL2$L)/datL2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_L2 = list(L = datL2$L, precL = pL2, xL = c(rnorm(1,elec[2,2], 0.00001),rep(NA,end.date - orig.date-1)),
               day = datL2$fieldDate.num, npolls = nrow(datL2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_L2,con="kalman_L2.bug")

system.time(jags_mod_L2 <- jags.model("kalman_L2.bug", data = data_L2, n.chains = 3))

system.time(outL2 <- coda.samples(jags_mod_L2,variable.names = c("xL", "L"), n.iter = ninter, n.thin = 100))
sumL2 = summary(outL2)
cred_intL2 = HPDinterval(outL2[,which(regexpr("xL", row.names(sumL2$statistics))==1)], 0.95)


############ KD ###############

dateDiffKD = datKD[,3] - datKD[,2]
dateDiffKD2 = dateDiffKD+1
nDayKD = datKD$n/as.numeric(dateDiffKD2)
KDSmooth = rep(datKD$KD,dateDiffKD2)
KDnSmooth = rep(nDayKD,dateDiffKD2)
houseSmoothKD = rep(datKD$house, dateDiffKD2)

eeKD = list()
for(i in 1:nrow(datKD)){
  eeKD[[i]] = seq(datKD[i,2], datKD[i,3], by="days")
}

dateSmoothKD = eeKD[[1]]
for(i in 2:nrow(datKD)){
  dateSmoothKD = c(dateSmoothKD, eeKD[[i]])
}

dateSmooth.numKD = julian(dateSmoothKD,origin=orig.date)
datKD2 = data.frame(KD = KDSmooth, fieldDate=dateSmoothKD, fieldDate.num = dateSmooth.numKD, n = KDnSmooth, house = houseSmoothKD)

jags_KD2 ='
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

epsKD ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiKD <- 1/epsKD
}
'

pKD2 = (1 / (datKD2$KD*(1-datKD2$KD)/datKD2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_KD2 = list(KD = datKD2$KD, precKD = pKD2, xKD = c(rnorm(1,elec[2,3], 0.00001),rep(NA,end.date - orig.date-1)),
               day = datKD2$fieldDate.num, npolls = nrow(datKD2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_KD2,con="kalman_KD2.bug")

system.time(jags_mod_KD2 <- jags.model("kalman_KD2.bug", data = data_KD2, n.chains = 3))

system.time(outKD2 <- coda.samples(jags_mod_KD2,variable.names = c("xKD", "KD"), n.iter = ninter, n.thin = 100))
sumKD2 = summary(outKD2)
cred_intKD2 = HPDinterval(outKD2[,which(regexpr("xKD", row.names(sumKD2$statistics))==1)], 0.95)

############ C ###############
dateDiffC = datC[,3] - datC[,2]
dateDiffC2 = dateDiffC+1
nDayC = datC$n/as.numeric(dateDiffC2)
CSmooth = rep(datC$C,dateDiffC2)
CnSmooth = rep(nDayC,dateDiffC2)
houseSmoothC = rep(datC$house, dateDiffC2)

eeC = list()
for(i in 1:nrow(datC)){
  eeC[[i]] = seq(datC[i,2], datC[i,3], by="days")
}

dateSmoothC = eeC[[1]]
for(i in 2:nrow(datC)){
  dateSmoothC = c(dateSmoothC, eeC[[i]])
}

dateSmooth.numC = julian(dateSmoothC,origin=orig.date)
datC2 = data.frame(C = CSmooth, fieldDate=dateSmoothC, fieldDate.num = dateSmooth.numC, n = CnSmooth, house = houseSmoothC)

jags_C2 ='
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

epsC ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiC <- 1/epsC
}
'

pC2 = (1 / (datC2$C*(1-datC2$C)/datC2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_C2 = list(C = datC2$C, precC = pC2, xC = c(rnorm(1,elec[2,4], 0.00001),rep(NA,end.date - orig.date-1)),
                day = datC2$fieldDate.num, npolls = nrow(datC2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_C2,con="kalman_C2.bug")

system.time(jags_mod_C2 <- jags.model("kalman_C2.bug", data = data_C2, n.chains = 3))

system.time(outC2 <- coda.samples(jags_mod_C2,variable.names = c("xC", "C"), n.iter = ninter, n.thin = 100))
sumC2 = summary(outC2)
cred_intC2 = HPDinterval(outC2[,which(regexpr("xC", row.names(sumC2$statistics))==1)], 0.95)

############ S ###############

dateDiffS = datS[,3] - datS[,2]
dateDiffS2 = dateDiffS+1
nDayS = datS$n/as.numeric(dateDiffS2)
SSmooth = rep(datS$S,dateDiffS2)
SnSmooth = rep(nDayS,dateDiffS2)
houseSmoothS = rep(datS$house, dateDiffS2)

eeS = list()
for(i in 1:nrow(datS)){
  eeS[[i]] = seq(datS[i,2], datS[i,3], by="days")
}

dateSmoothS = eeS[[1]]
for(i in 2:nrow(datS)){
  dateSmoothS = c(dateSmoothS, eeS[[i]])
}

dateSmooth.numS = julian(dateSmoothS,origin=orig.date)
datS2 = data.frame(S = SSmooth, fieldDate=dateSmoothS, fieldDate.num = dateSmooth.numS, n = SnSmooth, house = houseSmoothS)

jags_S2 ='
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

epsS ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiS <- 1/epsS
}
'

pS2 = (1 / (datS2$S*(1-datS2$S)/datS2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_S2 = list(S = datS2$S, precS = pS2, xS = c(rnorm(1,elec[2,5], 0.00001),rep(NA,end.date - orig.date-1)),
                day = datS2$fieldDate.num, npolls = nrow(datS2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_S2,con="kalman_S2.bug")

system.time(jags_mod_S2 <- jags.model("kalman_S2.bug", data = data_S2, n.chains = 3))

system.time(outS2 <- coda.samples(jags_mod_S2,variable.names = c("xS", "S"), n.iter = ninter, n.thin = 100))
sumS2 = summary(outS2)
cred_intS2 = HPDinterval(outS2[,which(regexpr("xS", row.names(sumS2$statistics))==1)], 0.95)

############ MP ###############

dateDiffMP = datMP[,3] - datMP[,2]
dateDiffMP2 = dateDiff+1
nDayMP = datMP$n/as.numeric(dateDiffMP2)
MPSmooth = rep(datMP$MP,dateDiffMP2)
MPnSmooth = rep(nDayMP,dateDiffMP2)
houseSmoothMP = rep(datMP$house, dateDiffMP2)

eeMP = list()
for(i in 1:nrow(datMP)){
  eeMP[[i]] = seq(datMP[i,2], datMP[i,3], by="days")
}

dateSmoothMP = eeMP[[1]]
for(i in 2:nrow(datMP)){
  dateSmoothMP = c(dateSmoothMP, eeMP[[i]])
}

dateSmooth.numMP = julian(dateSmoothMP,origin=orig.date)
datMP2 = data.frame(MP = MPSmooth, fieldDate=dateSmoothMP, fieldDate.num = dateSmooth.numMP, n = MPnSmooth, house = houseSmoothMP)

jags_MP2 ='
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

epsMP ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiMP <- 1/epsMP
}
'

pMP2 = (1 / (datMP2$MP*(1-datMP2$MP)/datMP2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_MP2 = list(MP = datMP2$MP, precMP = pMP2, xMP = c(rnorm(1,elec[2,6], 0.00001),rep(NA,end.date - orig.date-1)),
                day = datMP2$fieldDate.num, npolls = nrow(datMP2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_MP2,con="kalman_MP2.bug")

system.time(jags_mod_MP2 <- jags.model("kalman_MP2.bug", data = data_MP2, n.chains = 3))

system.time(outMP2 <- coda.samples(jags_mod_MP2,variable.names = c("xMP", "MP"), n.iter = ninter, n.thin = 100))
sumMP2 = summary(outMP2)
cred_intMP2 = HPDinterval(outMP2[,which(regexpr("xMP", row.names(sumMP2$statistics))==1)], 0.95)

############ V ###############

dateDiffV = datV[,3] - datV[,2]
dateDiffV2 = dateDiffV+1
nDayV = datV$n/as.numeric(dateDiffV2)
VSmooth = rep(datV$V,dateDiffV2)
VnSmooth = rep(nDayV,dateDiffV2)
houseSmoothV = rep(datV$house, dateDiffV2)

eeV = list()
for(i in 1:nrow(datV)){
  eeV[[i]] = seq(datV[i,2], datV[i,3], by="days")
}

dateSmoothV = eeV[[1]]
for(i in 2:nrow(datV)){
  dateSmoothV = c(dateSmoothV, eeV[[i]])
}

dateSmooth.numV = julian(dateSmoothV,origin=orig.date)
datV2 = data.frame(V = VSmooth, fieldDate=dateSmoothV, fieldDate.num = dateSmooth.numV, n = VnSmooth, house = houseSmoothV)

jags_V2 ='
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

epsV ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiV <- 1/epsV
}
'

pV2 = (1 / (datV2$V*(1-datV2$V)/datV2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_V2 = list(V = datV2$V, precV = pV2, xV = c(rnorm(1,elec[2,7], 0.00001),rep(NA,end.date - orig.date-1)),
                day = datV2$fieldDate.num, npolls = nrow(datV2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_V2,con="kalman_V2.bug")

system.time(jags_mod_V2 <- jags.model("kalman_V2.bug", data = data_V2, n.chains = 3))

system.time(outV2 <- coda.samples(jags_mod_V2,variable.names = c("xV", "V"), n.iter = ninter, n.thin = 100))
sumV2 = summary(outV2)
cred_intV2 = HPDinterval(outV2[,which(regexpr("xV", row.names(sumV2$statistics))==1)], 0.95)

############ SD ###############

dateDiffSD = datSD[,3] - datSD[,2]
dateDiffSD2 = dateDiffSD+1
nDaySD = datSD$n/as.numeric(dateDiffSD2)
SDSmooth = rep(datSD$SD,dateDiffSD2)
SDnSmooth = rep(nDaySD,dateDiffSD2)
houseSmoothSD = rep(datSD$house, dateDiffSD2)

eeSD = list()
for(i in 1:nrow(datSD)){
  eeSD[[i]] = seq(datSD[i,2], datSD[i,3], by="days")
}

dateSmoothSD = eeSD[[1]]
for(i in 2:nrow(datSD)){
  dateSmoothSD = c(dateSmoothSD, eeSD[[i]])
}

dateSmooth.numSD = julian(dateSmoothSD,origin=orig.date)
datSD2 = data.frame(SD = SDSmooth, fieldDate=dateSmoothSD, fieldDate.num = dateSmooth.numSD, n = SDnSmooth, house = houseSmoothSD)

jags_SD2 ='
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

epsSD ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiSD <- 1/epsSD
}
'

pSD2 = (1 / (datSD2$SD*(1-datSD2$SD)/datSD2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_SD2 = list(SD = datSD2$SD, precSD = pSD2, xSD = c(rnorm(1,elec[2,8], 0.00001),rep(NA,end.date - orig.date-1)),
                day = datSD2$fieldDate.num, npolls = nrow(datSD2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_SD2,con="kalman_SD2.bug")

system.time(jags_mod_SD2 <- jags.model("kalman_SD2.bug", data = data_SD2, n.chains = 3))

system.time(outSD2 <- coda.samples(jags_mod_SD2,variable.names = c("xSD", "SD"), n.iter = ninter, n.thin = 100))
sumSD2 = summary(outSD2)
cred_intSD2 = HPDinterval(outSD2[,which(regexpr("xSD", row.names(sumSD2$statistics))==1)], 0.95)