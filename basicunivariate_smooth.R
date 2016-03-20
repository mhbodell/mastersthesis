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
plot(outM2)


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



#####################################################
################ MODEL EVALUATION ###################
#####################################################

meanM2 = sumM2$statistics[which(regexpr("xM", row.names(sumM2$statistics))==1),1]
meanL2 = sumL2$statistics[which(regexpr("xL", row.names(sumL2$statistics))==1),1]
meanKD2 = sumKD2$statistics[which(regexpr("xKD", row.names(sumKD2$statistics))==1),1]
meanC2 = sumC2$statistics[which(regexpr("xC", row.names(sumC2$statistics))==1),1]
meanS2 = sumS2$statistics[which(regexpr("xS", row.names(sumS2$statistics))==1),1]
meanMP2 = sumMP2$statistics[which(regexpr("xMP", row.names(sumMP2$statistics))==1),1]
meanV2 = sumV2$statistics[which(regexpr("xV", row.names(sumV2$statistics))==1),1]
meanSD2 = sumSD2$statistics[which(regexpr("xSD", row.names(sumSD2$statistics))==1),1]


set.seed(901207)
i = sample(1:3,1)
#rsimM2 = outM2[[i]]
#rsimL2 = outL2[[i]]
#rsimKD2 = outKD2[[i]]
#rsimC2 = outC2[[i]]
#rsimS2 = outS2[[i]]
#rsimMP2 = outMP2[[i]]
rsimV2 = outV2[[i]]
rsimSD2 = outSD2[[i]]
nsim = dim(rsimM2)[1]
#simxM2 = rsimM2[,which(regexpr("xM", colnames(rsimM2))==1)]
#simxL2 = rsimL2[,which(regexpr("xL", colnames(rsimL2))==1)]
#simxKD2 = rsimKD2[,which(regexpr("xKD", colnames(rsimKD2))==1)]
#simxC2 = rsimC2[,which(regexpr("xC", colnames(rsimC2))==1)]
#simxS2 = rsimS2[,which(regexpr("xS", colnames(rsimS2))==1)]
#simxMP2 = rsimMP2[,which(regexpr("xMP", colnames(rsimMP2))==1)]
simxV2 = rsimV2[,which(regexpr("xV", colnames(rsimV2))==1)]
simxSD2 = rsimSD2[,which(regexpr("xSD", colnames(rsimSD2))==1)]

#varM2 = matrix (NA, nrow=nsim, ncol=nrow(datM2))
#varL2 = matrix (NA, nrow=nsim, ncol=nrow(datL2))
#varKD2 = matrix (NA, nrow=nsim, ncol=nrow(datKD2))
#varC2 = matrix (NA, nrow=nsim, ncol=nrow(datC2))
#varS2 = matrix (NA, nrow=nsim, ncol=nrow(datS2))
#varMP2 = matrix (NA, nrow=nsim, ncol=nrow(datMP2))
varV2 = matrix (NA, nrow=nsim, ncol=nrow(datV2))
varSD2 = matrix (NA, nrow=nsim, ncol=nrow(datSD2))

for(i in 1:nrow(varM2)){
 # varM2[i,] = 1/pM2
  #varL2[i,] = 1/pL2
  #varKD2[i,] = 1/pKD2
  #varC2[i,] = 1/pC2
  #varS2[i,] = 1/pS2
  #varMP2[i,] = 1/pMP2
  varV2[i,] = 1/pV2
  varSD2[i,] = 1/pSD2
}

#yrepM2 = sapply(1:nsim, function(s) rnorm(500,simxM2[s,datM2$fieldDate.num], varM2[s,]))
#yrepL2 = sapply(1:nsim, function(s) rnorm(500,simxL2[s,datL2$fieldDate.num], varL2[s,]))
#yrepKD2 = sapply(1:nsim, function(s) rnorm(500,simxKD2[s,datKD2$fieldDate.num], varKD2[s,]))
#yrepC2 = sapply(1:nsim, function(s) rnorm(500,simxC2[s,datC2$fieldDate.num], varL2[s,]))
#yrepS2 = sapply(1:nsim, function(s) rnorm(500,simxS2[s,datS2$fieldDate.num], varS2[s,]))
#yrepMP2 = sapply(1:nsim, function(s) rnorm(500,simxMP2[s,datMP2$fieldDate.num], varMP2[s,]))
yrepV2 = sapply(1:nsim, function(s) rnorm(500,simxV2[s,datV2$fieldDate.num], varV2[s,]))
yrepSD2 = sapply(1:nsim, function(s) rnorm(500,simxSD2[s,datSD2$fieldDate.num], varSD2[s,]))

par(mfrow=c(3,3))
min_repM2 = apply(yrepM2,2,min)
min_M2 = min(datM2$M)
hist(min_repM2, main="M", col="blue", xlab="Minimum value in replicated data", las=1)
abline(v=min_M2, lty=1, lwd=2)
sum(ifelse(min_repM2>=min_M2,1,0))/length(min_repM2) 

min_repL2 = apply(yrepL2,2,min)
min_L2 = min(datL2$L)
hist(min_repL2, main="L", col="lightblue3", xlab="Minimum value in replicated data", las=1)
abline(v=min_L2, lty=1, lwd=2)
sum(ifelse(min_repL2>=min_L2,1,0))/length(min_repL2) 

min_repKD2 = apply(yrepKD2,2,min)
min_KD2 = min(datKD2$KD)
hist(min_repKD2, main="KD", col="darkblue", xlab="Minimum value in replicated data", las=1)
abline(v=min_KD2, lty=1, lwd=2)
sum(ifelse(min_repKD2>=min_KD2,1,0))/length(min_repKD2) 

min_repC2 = apply(yrepC2,2,min)
min_C2 = min(datC2$C)
hist(min_repC2, main="C", col="chartreuse3", xlab="Minimum value in replicated data", las=1)
abline(v=min_C2, lty=1, lwd=2)
sum(ifelse(min_repC2>=min_C2,1,0))/length(min_repC2) 

min_repS2 = apply(yrepS2,2,min)
min_S2 = min(datS2$S)
hist(min_repS2, main="S", col="red", xlab="Minimum value in replicated data", las=1)
abline(v=min_S2, lty=1, lwd=2)
sum(ifelse(min_repS2>=min_S2,1,0))/length(min_repS2) 

min_repMP2 = apply(yrepMP2,2,min)
min_MP2 = min(datMP2$MP)
hist(min_repMP2, main="MP", col="forestgreen", xlab="Minimum value in replicated data", las=1)
abline(v=min_MP2, lty=1, lwd=2)
sum(ifelse(min_repMP2>=min_MP2,1,0))/length(min_repMP2) 

min_repV2 = apply(yrepV2,2,min)
min_V2 = min(datV2$V)
hist(min_repV2, main="V", col="darkred", xlab="Minimum value in replicated data", las=1)
abline(v=min_V2, lty=1, lwd=2)
sum(ifelse(min_repV2>=min_V2,1,0))/length(min_repV2) 

min_repSD2 = apply(yrepSD2,2,min)
min_SD2 = min(datSD2$SD)
hist(min_repSD2, main="SD", col="skyblue3", xlab="Minimum value in replicated data", las=1)
abline(v=min_SD2, lty=1, lwd=2)
sum(ifelse(min_repSD2>=min_SD2,1,0))/length(min_repSD2) 
par(mfrow=c(1,1))


par(mfrow=c(3,3))
max_repM2 = apply(yrepM2,2,max)
max_M2 = max(datM2$M)
hist(max_repM2, main="M", col="blue", xlab="Maximum value in replicated data", las=1)
abline(v=max_M2, lty=1, lwd=2)
sum(ifelse(max_repM2>=max_M2,1,0))/length(max_repM2) 

max_repL2 = apply(yrepL2,2,max)
max_L2 = max(datL2$L)
hist(min_repL2, main="L", col="lightblue3", xlab="Maximum value in replicated data", las=1)
abline(v=max_L2, lty=1, lwd=2)
sum(ifelse(max_repL2>=max_L2,1,0))/length(max_repL2) 

max_repKD2 = apply(yrepKD2,2,max)
max_KD2 = max(datKD2$KD)
hist(max_repKD2, main="KD", col="darkblue", xlab="Maximum value in replicated data", las=1)
abline(v=max_KD2, lty=1, lwd=2)
sum(ifelse(max_repKD2>=max_KD2,1,0))/length(max_repKD2) 

max_repC2 = apply(yrepC2,2,max)
max_C2 = max(datC2$C)
hist(max_repC2, main="C", col="chartreuse3", xlab="Maximum value in replicated data", las=1)
abline(v=max_C2, lty=1, lwd=2)
sum(ifelse(max_repC2>=max_C2,1,0))/length(min_repC2)  

max_repS2 = apply(yrepS2,2,max)
max_S2 = max(datS2$S)
hist(max_repS2, main="S", col="red", xlab="Maximum value in replicated data", las=1)
abline(v=max_S2, lty=1, lwd=2)
sum(ifelse(max_repS2>=max_S2,1,0))/length(max_repS2) 

max_repMP2 = apply(yrepMP2,2,max)
max_MP2 = max(datMP2$MP)
hist(max_repMP2, main="MP", col="forestgreen", xlab="Maximum value in replicated data", las=1)
abline(v=max_MP2, lty=1, lwd=2)
sum(ifelse(max_repMP2>=max_MP2,1,0))/length(max_repMP2) 

max_repV2 = apply(yrepV2,2,max)
max_V2 = max(datV2$V)
hist(max_repV2, main="V", col="darkred", xlab="Maximum value in replicated data", las=1)
abline(v=max_V2, lty=1, lwd=2)
sum(ifelse(max_repV2>=max_V2,1,0))/length(max_repV2) 

max_repSD2 = apply(yrepSD2,2,max)
max_SD2 = max(datSD2$SD)
hist(max_repSD2, main="SD", col="skyblue3", xlab="Maximum value in replicated data", las=1)
abline(v=max_SD2, lty=1, lwd=2)
sum(ifelse(max_repSD2>=max_SD2,1,0))/length(max_repSD2) 
par(mfrow=c(1,1))


par(mfrow=c(3,3))
mean_repM2 = apply(yrepM2,2,mean)
mean_M2 = mean(datM2$M)
hist(mean_repM2, main="M", col="blue", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_M2, lty=1, lwd=2)
sum(ifelse(mean_repM2>=mean_M2,1,0))/length(mean_repM2) 

mean_repL2 = apply(yrepL2,2,mean)
mean_L2 = mean(datL2$L)
hist(mean_repL2, main="L", col="lightblue3", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_L2, lty=1, lwd=2)
sum(ifelse(mean_repL2>=mean_L2,1,0))/length(mean_repL2) 

mean_repKD2 = apply(yrepKD2,2,mean)
mean_KD2 = mean(datKD2$KD)
hist(mean_repKD2, main="KD", col="darkblue", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_KD2, lty=1, lwd=2)
sum(ifelse(mean_repKD2>=mean_KD2,1,0))/length(mean_repKD2) 

mean_repC2 = apply(yrepC2,2,mean)
mean_C2 = mean(datC2$C)
hist(mean_repC2, main="C", col="chartreuse3", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_C2, lty=1, lwd=2)
sum(ifelse(mean_repC2>=mean_C2,1,0))/length(mean_repC2) 

mean_repS2 = apply(yrepS2,2,mean)
mean_S2 = mean(datS2$S)
hist(mean_repS2, main="S", col="red", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_S2, lty=1, lwd=2)
sum(ifelse(mean_repS2>=mean_S2,1,0))/length(mean_repS2) 

mean_repMP2 = apply(yrepMP2,2,mean)
mean_MP2 = mean(datMP2$MP)
hist(mean_repMP2, main="MP", col="forestgreen", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_MP2, lty=1, lwd=2)
sum(ifelse(mean_repMP2>=mean_MP2,1,0))/length(mean_repMP2) 

mean_repV2 = apply(yrepV2,2,mean)
mean_V2 = mean(datV2$V)
hist(mean_repV2, main="V", col="darkred", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_V2, lty=1, lwd=2)
sum(ifelse(mean_repV2>=mean_V2,1,0))/length(mean_repV2) 

mean_repSD2 = apply(yrepSD2,2,mean)
mean_SD2 = mean(datSD2$SD)
hist(mean_repSD2, main="SD", col="skyblue3", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_SD2, lty=1, lwd=2)
sum(ifelse(mean_repSD2>=mean_SD2,1,0))/length(mean_repSD2) 
par(mfrow=c(1,1))


dfM2 = data.frame(party = meanM2 , low=cred_intM2[[1]][,1]*100, high=cred_intM2[[1]][,2]*100,
                 time=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date)))),
                 party2 = rep("M", length(c(rep(NA,end.date - orig.date)))))
dfL2 = data.frame(party = meanL2 , low=cred_intL2[[1]][,1]*100, high=cred_intL2[[1]][,2]*100,
                   time=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date)))),
                   party2 = rep("L", length(c(rep(NA,end.date - orig.date)))))
dfKD2 = data.frame(party = meanKD2 , low=cred_intKD2[[1]][,1]*100, high=cred_intKD2[[1]][,2]*100,
                  time=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date)))),
                  party2 = rep("KD", length(c(rep(NA,end.date - orig.date)))))
dfC2 = data.frame(party = meanC2 , low=cred_intC2[[1]][,1]*100, high=cred_intC2[[1]][,2]*100,
                   time=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date)))),
                   party2 = rep("C", length(c(rep(NA,end.date - orig.date)))))
dfS2 = data.frame(party = meanS2 , low=cred_intS2[[1]][,1]*100, high=cred_intS2[[1]][,2]*100,
                   time=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date)))),
                   party2 = rep("S", length(c(rep(NA,end.date - orig.date)))))
dfMP2 = data.frame(party = meanMP2 , low=cred_intMP2[[1]][,1]*100, high=cred_intMP2[[1]][,2]*100,
                   time=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date)))),
                   party2 = rep("MP", length(c(rep(NA,end.date - orig.date)))))
dfV2 = data.frame(party = meanV2 , low=cred_intV2[[1]][,1]*100, high=cred_intV2[[1]][,2]*100,
                   time=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date)))),
                   party2 = rep("V", length(c(rep(NA,end.date - orig.date)))))
dfSD2 = data.frame(party = meanSD2 , low=cred_intSD2[[1]][,1]*100, high=cred_intSD2[[1]][,2]*100,
                   time=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date)))),
                   party2 = rep("SD", length(c(rep(NA,end.date - orig.date)))))

pointsM2 = data.frame(x=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date))))[datM$fieldDate.num], 
                     y=datM$M*100, house=datM$house, party=rep("M",length(c(rep(NA,end.date - orig.date))))[datM$fieldDate.num])
pointsL2 = data.frame(x=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date))))[datL$fieldDate.num], 
                       y=datL$L*100, house=datL$house, party=rep("L",length(c(rep(NA,end.date - orig.date))))[datL$fieldDate.num])
pointsKD2 = data.frame(x=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date))))[datKD$fieldDate.num], 
                      y=datKD$KD*100, house=datKD$house, party=rep("KD",length(c(rep(NA,end.date - orig.date))))[datKD$fieldDate.num])
pointsC2 = data.frame(x=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date))))[datC$fieldDate.num], 
                       y=datC$C*100, house=datC$house, party=rep("C",length(c(rep(NA,end.date - orig.date))))[datC$fieldDate.num])
pointsS2 = data.frame(x=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date))))[datS$fieldDate.num], 
                       y=datS$S*100, house=datS$house, party=rep("S",length(c(rep(NA,end.date - orig.date))))[datS$fieldDate.num])
pointsMP2 = data.frame(x=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date))))[datMP$fieldDate.num], 
                       y=datMP$MP*100, house=datMP$house, party=rep("MP",length(c(rep(NA,end.date - orig.date))))[datMP$fieldDate.num])
pointsV2 = data.frame(x=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date))))[datV$fieldDate.num], 
                       y=datV$V*100, house=datV$house, party=rep("V",length(c(rep(NA,end.date - orig.date))))[datV$fieldDate.num])
pointsSD2 = data.frame(x=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date))))[datSD$fieldDate.num], 
                       y=datSD$SD*100, house=datSD$house, party=rep("SD",length(c(rep(NA,end.date - orig.date))))[datSD$fieldDate.num])

points = rbind(pointsM2, pointsL2, pointsKD2, pointsC2, pointsS2, pointsMP2, pointsV2, pointsSD2)

cols = c("blue", "lightblue3", "darkblue","chartreuse3","red","forestgreen","darkred","skyblue3" )
list.df = rbind(dfM2, dfL2, dfKD2, dfC2, dfS2, dfMP2, dfV2, dfSD2) 


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

k=1
g2 =list()
for(i in unique(list.df$party2)){
  g <- ggplot(list.df[list.df$party2==i,]) +
    aes(x = time, y = party*100) +
    geom_line(col=cols[k], alpha=1)  +
    geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill=cols[k]) + 
    geom_point(data=points[points$party==i,], aes(x=x, y=y), alpha = 1, color=cols[k], shape=1, size=1) +    
    labs(x="Date", y=paste("Support for",i, "(%)")) +
    theme_bw() +
    facet_wrap( ~ party2, ncol=1, nrow=1)+
    theme(axis.text = element_text(size = 9),
          legend.key = element_rect(fill = "white"),
          legend.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "lightgrey"),
          panel.grid.minor = element_blank())
  g2[[k]] = g
  k=k+1
}

multiplot(g2[[1]],g2[[2]], g2[[3]], g2[[4]],
          g2[[5]],g2[[6]], g2[[7]], g2[[8]], cols=2)
