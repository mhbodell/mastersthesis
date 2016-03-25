dateDiff = datM[,3] - datM[,2]
dateDiff2 = dateDiff+1
nDay = datM$n/as.numeric(dateDiff2)
MSmooth = rep(datM$M,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datM$house, dateDiff2)
orig.date= as.Date(datM$collectPeriodFrom[1]-1)
end.date= datM$collectPeriodTo[length(datM$collectPeriodTo)]

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
M[i] ~ dnorm(xM[day[i]],precM[i])}

#dynamic model 
for(i in 2:nperiods){
xM[i] ~ dnorm(xM[i-1],phiM)}
## priors
xM[1] ~ dunif(0,1)
epsM ~ dgamma(1,1) ## hyperparameters in gamma affects the smoothness of the curve
phiM <- 1/epsM
}
'

pM2 = (1 / (datM2$M*(1-datM2$M)/datM2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_M2 = list(M = datM2$M, precM = pM2, xM = rep(NA,end.date - orig.date),
               day = datM2$fieldDate.num, npolls = nrow(datM2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_M2,con="kalman_M2.bug")

system.time(jags_mod_M2 <- jags.model("kalman_M2.bug", data = data_M2, n.chains = 3))

ninter = 10000
system.time(outM2 <- coda.samples(jags_mod_M2,variable.names = c("xM", "M"), n.iter = ninter, thin = 20))
sumM2 = summary(outM2)
cred_intM2 = HPDinterval(outM2[,which(regexpr("xM", row.names(sumM2$statistics))==1)], 0.95)


############### L ################
dateDiffL = datL[,3] - datL[,2]
dateDiffL2 = dateDiffL+1
nDayL = datL$n/as.numeric(dateDiffL2)
LSmooth = rep(datL$L,dateDiffL2)
LnSmooth = rep(nDayL,dateDiffL2)
houseSmoothL = rep(datL$house, dateDiffL2)
orig.date= as.Date(datL$collectPeriodFrom[1]-1)
end.date= datL$collectPeriodTo[length(datL$collectPeriodTo)]

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
xM[1] ~ dunif(0,1)
epsL ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiL <- 1/epsL
}
'

pL2 = (1 / (datL2$L*(1-datL2$L)/datL2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_L2 = list(L = datL2$L, precL = pL2, xL = rep(NA,end.date - orig.date),
               day = datL2$fieldDate.num, npolls = nrow(datL2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_L2,con="kalman_L2.bug")

system.time(jags_mod_L2 <- jags.model("kalman_L2.bug", data = data_L2, n.chains = 3))

system.time(outL2 <- coda.samples(jags_mod_L2,variable.names = c("xL", "L"), n.iter = ninter, thin = 2))
sumL2 = summary(outL2)
cred_intL2 = HPDinterval(outL2[,which(regexpr("xL", row.names(sumL2$statistics))==1)], 0.95)


############ KD ###############

dateDiffKD = datKD[,3] - datKD[,2]
dateDiffKD2 = dateDiffKD+1
nDayKD = datKD$n/as.numeric(dateDiffKD2)
KDSmooth = rep(datKD$KD,dateDiffKD2)
KDnSmooth = rep(nDayKD,dateDiffKD2)
houseSmoothKD = rep(datKD$house, dateDiffKD2)
orig.date= as.Date(datKD$collectPeriodFrom[1]-1)
end.date= datKD$collectPeriodTo[length(datKD$collectPeriodTo)]

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

xKD[1] <-dunif(0,1)
epsKD ~ dgamma(1, 1) 
phiKD <- 1/epsKD
}
'

pKD2 = (1 / (datKD2$KD*(1-datKD2$KD)/datKD2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_KD2 = list(KD = datKD2$KD, precKD = pKD2, xKD = rep(NA,end.date - orig.date),
               day = datKD2$fieldDate.num, npolls = nrow(datKD2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_KD2,con="kalman_KD2.bug")

system.time(jags_mod_KD2 <- jags.model("kalman_KD2.bug", data = data_KD2, n.chains = 3))

system.time(outKD2 <- coda.samples(jags_mod_KD2,variable.names = c("xKD", "KD"), n.iter = ninter, thin = 2))
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
orig.date= as.Date(datC$collectPeriodFrom[1]-1)
end.date= datC$collectPeriodTo[length(datC$collectPeriodTo)]

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

xC[1] ~dunif(0,1)
epsC ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiC <- 1/epsC
}
'

pC2 = (1 / (datC2$C*(1-datC2$C)/datC2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_C2 = list(C = datC2$C, precC = pC2, xC = rep(NA,end.date - orig.date),
                day = datC2$fieldDate.num, npolls = nrow(datC2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_C2,con="kalman_C2.bug")

system.time(jags_mod_C2 <- jags.model("kalman_C2.bug", data = data_C2, n.chains = 3))

system.time(outC2 <- coda.samples(jags_mod_C2,variable.names = c("xC", "C"), n.iter = ninter, thin = 2))
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
orig.date= as.Date(datS$collectPeriodFrom[1]-1)
end.date= datS$collectPeriodTo[length(datS$collectPeriodTo)]

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

xS[1] <- dunif(0,1)
epsS ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiS <- 1/epsS
}
'

pS2 = (1 / (datS2$S*(1-datS2$S)/datS2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_S2 = list(S = datS2$S, precS = pS2, xS = rep(NA,end.date - orig.date),
                day = datS2$fieldDate.num, npolls = nrow(datS2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_S2,con="kalman_S2.bug")

system.time(jags_mod_S2 <- jags.model("kalman_S2.bug", data = data_S2, n.chains = 3))

system.time(outS2 <- coda.samples(jags_mod_S2,variable.names = c("xS", "S"), n.iter = ninter, thin = 2))
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
orig.date= as.Date(datMP$collectPeriodFrom[1]-1)
end.date= datMP$collectPeriodTo[length(datMP$collectPeriodTo)]

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

xMP[1] ~dunif(0,1)
epsMP ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiMP <- 1/epsMP
}
'

pMP2 = (1 / (datMP2$MP*(1-datMP2$MP)/datMP2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_MP2 = list(MP = datMP2$MP, precMP = pMP2, xMP = rep(NA,end.date - orig.date),
                day = datMP2$fieldDate.num, npolls = nrow(datMP2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_MP2,con="kalman_MP2.bug")

system.time(jags_mod_MP2 <- jags.model("kalman_MP2.bug", data = data_MP2, n.chains = 3))

system.time(outMP2 <- coda.samples(jags_mod_MP2,variable.names = c("xMP", "MP"), n.iter = ninter, thin = 2))
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
orig.date= as.Date(datV$collectPeriodFrom[1]-1)
end.date= datV$collectPeriodTo[length(datV$collectPeriodTo)]

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

xV ~ dunif(0,1)
epsV ~ dgamma(1, 1) ## hyperparameters in gamma affects the smoothness of the curve
phiV <- 1/epsV
}
'

pV2 = (1 / (datV2$V*(1-datV2$V)/datV2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_V2 = list(V = datV2$V, precV = pV2, xV = rep(NA,end.date - orig.date),
                day = datV2$fieldDate.num, npolls = nrow(datV2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_V2,con="kalman_V2.bug")

system.time(jags_mod_V2 <- jags.model("kalman_V2.bug", data = data_V2, n.chains = 3))

system.time(outV2 <- coda.samples(jags_mod_V2,variable.names = c("xV", "V"), n.iter = ninter, thin = 2))
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
orig.date = as.Date(datSD$collectPeriodFrom[1]-1)
end.date = datSD$collectPeriodTo[length(datSD$collectPeriodTo)]

jags_SD2 ='
model{
#observed model
for(i in 1:npolls){
SD[i] ~ dnorm(xSD[day[i]],precSD[i])}

#dynamic model 
for(i in 2:nperiods){
xSD[i] ~ dnorm(xSD[i-1],phiSD)
}
## priors

xSD[1] ~ dunif(0,1)
epsSD ~ dgamma(1,1) ## hyperparameters in gamma affects the smoothness of the curve
phiSD <- 1/epsSD
}
'

pSD2 = (1 / (datSD2$SD*(1-datSD2$SD)/datSD2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_SD2 = list(SD = datSD2$SD, precSD = pSD2, xSD = rep(NA,end.date - orig.date),
                day = datSD2$fieldDate.num, npolls = nrow(datSD2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_SD2,con="kalman_SD2.bug")

system.time(jags_mod_SD2 <- jags.model("kalman_SD2.bug", data = data_SD2, n.chains = 3))

system.time(outSD2 <- coda.samples(jags_mod_SD2,variable.names = c("xSD", "SD"), n.iter = ninter, thin = 2))
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
rsimM2 = outM2[[i]];rsimL2 = outL2[[i]];rsimKD2 = outKD2[[i]];rsimC2 = outC2[[i]]
rsimS2 = outS2[[i]];rsimMP2 = outMP2[[i]];rsimV2 = outV2[[i]];rsimSD2 = outSD2[[i]]
nsim = dim(rsimM2)[1]
simxM2 = rsimM2[,which(regexpr("xM", colnames(rsimM2))==1)];simxL2 = rsimL2[,which(regexpr("xL", colnames(rsimL2))==1)]
simxKD2 = rsimKD2[,which(regexpr("xKD", colnames(rsimKD2))==1)];simxC2 = rsimC2[,which(regexpr("xC", colnames(rsimC2))==1)]
simxS2 = rsimS2[,which(regexpr("xS", colnames(rsimS2))==1)];simxMP2 = rsimMP2[,which(regexpr("xMP", colnames(rsimMP2))==1)]
simxV2 = rsimV2[,which(regexpr("xV", colnames(rsimV2))==1)]
simxSD2 = rsimSD2[,which(regexpr("xSD", colnames(rsimSD2))==1)]

varM2 = matrix (NA, nrow=nsim, ncol=nrow(datM2));varL2 = matrix (NA, nrow=nsim, ncol=nrow(datL2))
varKD2 = matrix (NA, nrow=nsim, ncol=nrow(datKD2));varC2 = matrix (NA, nrow=nsim, ncol=nrow(datC2))
varS2 = matrix (NA, nrow=nsim, ncol=nrow(datS2));varMP2 = matrix (NA, nrow=nsim, ncol=nrow(datMP2))
varV2 = matrix (NA, nrow=nsim, ncol=nrow(datV2));varSD2 = matrix (NA, nrow=nsim, ncol=nrow(datSD2))

for(i in 1:nrow(varM2)){
  varM2[i,] = 1/pM2
  varL2[i,] = 1/pL2
  varKD2[i,] = 1/pKD2
  varC2[i,] = 1/pC2
  varS2[i,] = 1/pS2
  varMP2[i,] = 1/pMP2
  varV2[i,] = 1/pV2
  varSD2[i,] = 1/pSD2
}

yrepM2 = sapply(1:nsim, function(s) rnorm(nrow(datM2),simxM2[s,datM2$fieldDate.num], varM2[s,]))
yrepL2 = sapply(1:nsim, function(s) rnorm(nrow(datL2),simxL2[s,datL2$fieldDate.num], varL2[s,]))
yrepKD2 = sapply(1:nsim, function(s) rnorm(nrow(datC2),simxKD2[s,datKD2$fieldDate.num], varKD2[s,]))
yrepC2 = sapply(1:nsim, function(s) rnorm(nrow(datKD2),simxC2[s,datC2$fieldDate.num], varL2[s,]))
yrepS2 = sapply(1:nsim, function(s) rnorm(nrow(datS2),simxS2[s,datS2$fieldDate.num], varS2[s,]))
yrepMP2 = sapply(1:nsim, function(s) rnorm(nrow(datMP2),simxMP2[s,datMP2$fieldDate.num], varMP2[s,]))
yrepV2 = sapply(1:nsim, function(s) rnorm(nrow(datV2),simxV2[s,datV2$fieldDate.num], varV2[s,]))
yrepSD2 = sapply(1:nsim, function(s) rnorm(nrow(datSD2),simxSD2[s,datSD2$fieldDate.num], varSD2[s,]))

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

################ plots ###############
dfM2 = data.frame(party = meanM2[-c(1:as.numeric(datM$collectPeriodFrom[2]-datM$collectPeriodFrom[1]))] , 
                 low=cred_intM2[[1]][-c(1:as.numeric(datM$collectPeriodFrom[2]-datM$collectPeriodFrom[1])),1]*100,
                 high=cred_intM2[[1]][-c(1:as.numeric(datM$collectPeriodFrom[2]-datM$collectPeriodFrom[1])),2]*100,
                 time=seq(as.Date(datM$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datM$collectPeriodFrom[2]-1))))),
                 party2 = rep("M", length(c(rep(NA,end.date - as.Date(datM$collectPeriodFrom[2]-1))))))

dfL2 = data.frame(party = meanL2[-c(1:as.numeric(datL$collectPeriodFrom[2]-datL$collectPeriodFrom[1]))] , 
                 low=cred_intL2[[1]][-c(1:as.numeric(datL$collectPeriodFrom[2]-datL$collectPeriodFrom[1])),1]*100, 
                 high=cred_intL2[[1]][-c(1:as.numeric(datL$collectPeriodFrom[2]-datL$collectPeriodFrom[1])),2]*100,
                 time=seq(as.Date(datL$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datL$collectPeriodFrom[2]-1))))),
                 party2 = rep("L", length(c(rep(NA,end.date - as.Date(datL$collectPeriodFrom[2]-1))))))

dfKD2 = data.frame(party = meanKD2[-c(1:as.numeric(datKD$collectPeriodFrom[2]-datKD$collectPeriodFrom[1]))] , 
                  low=cred_intKD2[[1]][-c(1:as.numeric(datKD$collectPeriodFrom[2]-datKD$collectPeriodFrom[1])),1]*100, 
                  high=cred_intKD2[[1]][-c(1:as.numeric(datKD$collectPeriodFrom[2]-datKD$collectPeriodFrom[1])),2]*100,
                  time=seq(as.Date(datKD$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datKD$collectPeriodFrom[2]-1))))),
                  party2 = rep("KD", length(c(rep(NA,end.date - as.Date(datKD$collectPeriodFrom[2]-1))))))

dfC2 = data.frame(party = meanC2[-c(1:as.numeric(datC$collectPeriodFrom[2]-datC$collectPeriodFrom[1]))] , 
                 low=cred_intC2[[1]][-c(1:as.numeric(datC$collectPeriodFrom[2]-datC$collectPeriodFrom[1])),1]*100,
                 high=cred_intC2[[1]][-c(1:as.numeric(datC$collectPeriodFrom[2]-datC$collectPeriodFrom[1])),2]*100,
                 time=seq(as.Date(datC$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datC$collectPeriodFrom[2]-1))))),
                 party2 = rep("C", length(c(rep(NA,end.date - as.Date(datC$collectPeriodFrom[2]-1))))))

dfS2 = data.frame(party = meanS2[-c(1:as.numeric(datS$collectPeriodFrom[2]-datS$collectPeriodFrom[1]))] , 
                 low=cred_intS2[[1]][-c(1:as.numeric(datS$collectPeriodFrom[2]-datS$collectPeriodFrom[1])),1]*100, 
                 high=cred_intS2[[1]][-c(1:as.numeric(datS$collectPeriodFrom[2]-datS$collectPeriodFrom[1])),2]*100,
                 time=seq(as.Date(datS$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datS$collectPeriodFrom[2]-1))))),
                 party2 = rep("S", length(c(rep(NA,end.date - as.Date(datS$collectPeriodFrom[2]-1))))))

dfMP2 = data.frame(party = meanMP2[-c(1:as.numeric(datMP$collectPeriodFrom[2]-datMP$collectPeriodFrom[1]))] , 
                  low=cred_intMP2[[1]][-c(1:as.numeric(datMP$collectPeriodFrom[2]-datMP$collectPeriodFrom[1])),1]*100, 
                  high=cred_intMP2[[1]][-c(1:as.numeric(datMP$collectPeriodFrom[2]-datMP$collectPeriodFrom[1])),2]*100,
                  time=seq(as.Date(datMP$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datMP$collectPeriodFrom[2]-1))))),
                  party2 = rep("MP", length(c(rep(NA,end.date - as.Date(datMP$collectPeriodFrom[2]-1))))))

dfV2 = data.frame(party = meanV2[-c(1:as.numeric(datV$collectPeriodFrom[2]-datV$collectPeriodFrom[1]))] , 
                 low=cred_intV2[[1]][-c(1:as.numeric(datV$collectPeriodFrom[2]-datV$collectPeriodFrom[1])),1]*100, 
                 high=cred_intV2[[1]][-c(1:as.numeric(datV$collectPeriodFrom[2]-datV$collectPeriodFrom[1])),2]*100,
                 time=as.Date(datV$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datV$collectPeriodFrom[2]-1)))),
                 party2 = rep("V", length(c(rep(NA,end.date - as.Date(datV$collectPeriodFrom[2]-1))))))

dfSD2 = data.frame(party = meanSD2[-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1]))] , 
                  low=cred_intSD2[[1]][-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1])),1]*100, 
                  high=cred_intSD2[[1]][-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1])),2]*100,
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



gM2 <- ggplot(dfM2) +
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

gL2 <- ggplot(dfL2) +
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

gKD2 <- ggplot(dfKD2) +
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

gC2 <- ggplot(dfC2) +
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

gS2 <- ggplot(dfS2) +
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

gMP2 <- ggplot(dfMP2) +
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

gV2 <- ggplot(dfV2) +
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

gSD2 <- ggplot(dfSD2) +
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


multiplot(gM2, gL2, gKD2, gC2, gS2, gMP2, gS2, gSD2, cols=2)

############################################
################# MSE ######################
############################################


mseM2 = mseL2 = mseKD2 = mseC2 = mseS2 = mseMP2 = mseV2 = mseSD2 = NULL


for (j in 1:dim(yrepM2)[2]){
  mseM2[j] = sum((datM2$M-yrepM2[,j])^2)/length(datM2$M)
  mseL2[j] = sum((datL2$L-yrepL2[,j])^2)/length(datL2$L)
  mseKD2[j] = sum((datKD2$KD-yrepKD2[,j])^2)/length(datKD2$KD)
  mseC2[j] = sum((datC2$C-yrepC2[,j])^2)/length(datC2$C)
  mseS2[j] = sum((datS2$S-yrepS2[,j])^2)/length(datS2$S)
  mseMP2[j] = sum((datMP2$MP-yrepMP2[,j])^2)/length(datMP2$MP)
  mseV2[j] = sum((datV2$V-yrepV2[,j])^2)/length(datV2$V)
  mseSD2[j] = sum((datSD2$SD-yrepSD2[,j])^2)/length(datSD2$SD)
}


par(mfrow=c(3,3))
hist(mseM2, main="M", xlab="MSE", col="blue")
abline(v=mean(mseM2))
hist(mseL2, main="L", xlab="MSE", col="lightblue3")
abline(v=mean(mseL2))
hist(mseKD2, main="KD", xlab="MSE", col="darkblue")
abline(v=mean(mseKD2))
hist(mseC2, main="C", xlab="MSE", col="chartreuse3")
abline(v=mean(mseC2))
hist(mseS2, main="S", xlab="MSE", col="red")
abline(v=mean(mseS2))
hist(mseMP2, main="MP", xlab="MSE", col="forestgreen")
abline(v=mean(mseMP2))
hist(mseV2, main="V", xlab="MSE", col="darkred")
abline(v=mean(mseV2))
hist(mseSD2, main="SD", xlab="MSE", col="skyblue3")
abline(v=mean(mseSD2))
par(mfrow=c(1,1))



mean(mseM2);mean(mseL2);mean(mseKD2);mean(mseC2);mean(mseS2);mean(mseMP2);mean(mseV2);mean(mseSD2)


############################################################
#######################   MAE   ############################
############################################################


maeM2 =maeL2 =maeKD2 = maeC2 = maeS2 = maeMP2 = maeV2 =maeSD2 = NULL
for (j in 1:dim(yrepM2)[2]){
  maeM2[j] = sum(abs(datM2$M-yrepM2[,j]))/length(datM2$M)
  maeL2[j] = sum(abs(datL2$L-yrepL2[,j]))/length(datL2$L)
  maeKD2[j] = sum(abs(datKD2$KD-yrepKD2[,j]))/length(datKD2$KD)
  maeC2[j] = sum(abs(datC2$C-yrepC2[,j]))/length(datC2$C)
  maeS2[j] = sum(abs(datS2$S-yrepS2[,j]))/length(datS2$S)
  maeMP2[j] = sum(abs(datMP2$MP-yrepMP2[,j]))/length(datMP2$MP)
  maeV2[j] = sum(abs(datV2$V-yrepV2[,j]))/length(datV2$V)
  maeSD2[j] = sum(abs(datSD2$SD-yrepSD2[,j]))/length(datSD2$SD)
}


par(mfrow=c(3,3))
hist(maeM2, main="M", xlab="MAE", col="blue")
abline(v=mean(maeM2))
hist(maeL2, main="L", xlab="MAE", col="lightblue3")
abline(v=mean(maeL2))
hist(maeKD2, main="KD", xlab="MAE", col="darkblue")
abline(v=mean(maeKD2))
hist(maeC2, main="C", xlab="MAE", col="chartreuse3")
abline(v=mean(maeC2))
hist(maeS2, main="S", xlab="MAE", col="red")
abline(v=mean(maeS2))
hist(maeMP2, main="MP", xlab="MAE", col="forestgreen")
abline(v=mean(maeMP2))
hist(maeV2, main="V", xlab="MAE", col="darkred")
abline(v=mean(maeV2))
hist(maeSD2, main="SD", xlab="MAE", col="skyblue3")
abline(v=mean(maeSD2))
par(mfrow=c(1,1))

mean(maeM2);mean(maeL2);mean(maeKD2);mean(maeC2);mean(maeS2);mean(maeMP2);mean(maeV2);mean(maeSD2)

################################################
####### predicting election outcome 2010 #######
################################################

################## M #####################

orig.date = as.Date(datM$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
e10_datM2 = datM2[-which(datM2$fieldDate>end.date),]
e10_datM2 = e10_datM2[-nrow(e10_datM2),]

library(rjags)
e2010jags_M ='
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

pM = (1 / (e10_datM2$M*(1-e10_datM2$M)/e10_datM2$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
e2010data_M = list(M = e10_datM2$M, precM = pM, xM = rep(NA,end.date - orig.date),
                   day = e10_datM2$fieldDate.num, npolls = nrow(e10_datM2), nperiods = as.numeric(end.date - orig.date))
writeLines(e2010jags_M,con="kalman_M_allData.bug")

system.time(jags_mod_e2010M <- jags.model("kalman_M_allData.bug", data = e2010data_M, n.chain=3))
ninter=10000
system.time(e2010outM <- coda.samples(jags_mod_e2010M,variable.names = c("xM", "M"), n.iter = ninter, thin = 5))

e2010sumM = summary(e2010outM)
e2010cred_intM = HPDinterval(e2010outM[,which(regexpr("xM", row.names(e2010sumM$statistics))==1)], 0.95)


############ L #################
orig.date = as.Date(datL$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
e10_datL2 = datL2[-which(datL2$fieldDate>end.date),]
e10_datL2 = e10_datL2[-nrow(e10_datL2),]

e2010jags_L ='
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

pL = (1 / (e10_datL2$L*(1-e10_datL2$L)/e10_datL2$n)) #binomial
e2010data_L = list(L = e10_datL2$L, precL = pL, xL = rep(NA,end.date - orig.date),
                   day = e10_datL2$fieldDate.num, npolls = nrow(e10_datL2), nperiods = as.numeric(end.date - orig.date))
writeLines(e2010jags_L,con="e2010kalman_L.bug")

system.time(e2010jags_mod_L <- jags.model("e2010kalman_L.bug", data = e2010data_L, n.chain=3))

system.time(e2010outL <- coda.samples(e2010jags_mod_L,variable.names = c("xL", "L"), n.iter = ninter, thin = 5))
e2010sumL = summary(e2010outL)
e2010cred_intL = HPDinterval(e2010outL[,which(regexpr("xL", row.names(e2010sumL$statistics))==1)], 0.95)

########### KD #############
orig.date = as.Date(datKD$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
e10_datKD2 = datKD2[-which(datKD2$fieldDate>end.date),]
e10_datKD2 = e10_datKD2[-nrow(e10_datKD2),]


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

pKD = (1 / (e10_datKD2$KD*(1-e10_datKD2$KD)/e10_datKD2$n)) #binomial
#pKD = (1 / (datKD$KD*(1-datKD$KD)*datKD$n)) #multinomial
e2010data_KD = list(KD = e10_datKD2$KD, precKD = pKD, xKD = rep(NA,end.date - orig.date),
                    day = e10_datKD2$fieldDate.num, npolls = nrow(e10_datKD2), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_KD,con="e2010kalman_KD.bug")

system.time(e2010jags_mod_KD <- jags.model("e2010kalman_KD.bug", data = e2010data_KD, n.chain=3))

system.time(e2010outKD <- coda.samples(e2010jags_mod_KD,variable.names = c("xKD", "KD"), n.iter = ninter, thin = 5))
e2010sumKD = summary(e2010outKD)
e2010cred_intKD = HPDinterval(e2010outKD[,which(regexpr("xKD", row.names(e2010sumKD$statistics))==1)], 0.95)


########### C #############
orig.date = as.Date(datC$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
e10_datC2 = datC2[-which(datC2$fieldDate>end.date),]
e10_datC2 = e10_datC2[-nrow(e10_datC2),]

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
pC = (1 / (e10_datC2$C*(1-e10_datC2$C)/e10_datC2$n)) #binomial
#pC = (1 / (e10_datC2$C*(1-e10_datC2$C)*e10_datC2$n)) #multinomial
e2010data_C = list(C = e10_datC2$C, precC = pC, xC = rep(NA,end.date - orig.date),
                   day = e10_datC2$fieldDate.num, npolls = nrow(e10_datC2), nperiods = as.numeric(end.date - orig.date))

writeLines(jags_C,con="e2010kalman_C.bug")

system.time(e2010jags_mod_C <- jags.model("e2010kalman_C.bug", data = e2010data_C, n.chain=3))

system.time(e2010outC <- coda.samples(e2010jags_mod_C,variable.names = c("xC", "C"), n.iter = ninter, thin = 5))
e2010sumC = summary(e2010outC)
e2010cred_intC = HPDinterval(e2010outC[,which(regexpr("xC", row.names(e2010sumC$statistics))==1)], 0.95)


########### S #############
orig.date = as.Date(datS$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
e10_datS2 = datS2[-which(datS2$fieldDate>end.date),]
e10_datS2 = e10_datS2[-nrow(e10_datS2),]

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
pS = (1 / (e10_datS2$S*(1-e10_datS2$S)/e10_datS2$n)) #binomial
#pS = (1 / (datS$S*(1-datS$S)*datS$n)) #multinomial
e2010data_S = list(S = e10_datS2$S, precS = pS, xS = rep(NA,end.date - orig.date),
                   day = e10_datS2$fieldDate.num, npolls = nrow(e10_datS2), nperiods = as.numeric(end.date - orig.date))

writeLines(jags_S,con="e2010kalman_S.bug")

system.time(e2010jags_mod_S <- jags.model("e2010kalman_S.bug", data = e2010data_S, n.chain=3))

system.time(e2010outS <- coda.samples(e2010jags_mod_S,variable.names = c("xS", "S"), n.iter = ninter, thin = 5))
e2010sumS = summary(e2010outS)
e2010cred_intS = HPDinterval(e2010outS[,which(regexpr("xS", row.names(e2010sumS$statistics))==1)], 0.95)

########### MP #############
orig.date = as.Date(datMP$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
e10_datMP2 = datMP2[-which(datMP2$fieldDate>end.date),]
e10_datMP2 = e10_datMP2[-nrow(e10_datMP2),]

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
pMP = (1 / (e10_datMP2$MP*(1-e10_datMP2$MP)/e10_datMP2$n)) #binomial
#pMP = (1 / (datMP$MP*(1-datMP$MP)*datMP$n)) #multinomial
e2010data_MP = list(MP = e10_datMP2$MP, precMP = pMP, xMP = rep(NA,end.date - orig.date),
                    day = e10_datMP2$fieldDate.num, npolls = nrow(e10_datMP2), nperiods = as.numeric(end.date - orig.date))

writeLines(jags_MP,con="e2010kalman_MP.bug")
system.time(e2010jags_mod_MP <- jags.model("e2010kalman_MP.bug", data = e2010data_MP, n.chain=3))

system.time(e2010outMP <- coda.samples(e2010jags_mod_MP,variable.names = c("xMP", "MP"), n.iter = ninter, thin = 5))
e2010sumMP = summary(e2010outMP)
e2010cred_intMP = HPDinterval(e2010outMP[,which(regexpr("xMP", row.names(e2010sumMP$statistics))==1)], 0.95)

########### V #############
orig.date = as.Date(datV$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
e10_datV2 = datV2[-which(datV2$fieldDate>end.date),]
e10_datV2 = e10_datV2[-nrow(e10_datV2),]

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

pV = (1 / (e10_datV2$V*(1-e10_datV2$V)/e10_datV2$n)) #binomial
#pV = (1 / (datV$V*(1-datV$V)*datV$n)) #multinomial
e2010data_V = list(V = e10_datV2$V, precV = pV, xV = rep(NA,end.date - orig.date),
                   day = e10_datV2$fieldDate.num, npolls = nrow(e10_datV2), nperiods = as.numeric(end.date - orig.date))

writeLines(jags_V,con="e2010kalman_V.bug")
system.time(e2010jags_mod_V <- jags.model("e2010kalman_V.bug", data = e2010data_V, n.chain=3))

system.time(e2010outV <- coda.samples(e2010jags_mod_V,variable.names = c("xV", "V"), n.iter = ninter, thin = 5))
e2010sumV = summary(e2010outV)
e2010cred_intV = HPDinterval(e2010outV[,which(regexpr("xV", row.names(e2010sumV$statistics))==1)], 0.95)

########### SD #############
orig.date = as.Date(datSD$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
e10_datSD2 = datSD2[-which(datSD2$fieldDate>end.date),]
e10_datSD2 = e10_datSD2[-nrow(e10_datSD2),]

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
pSD = (1 / (e10_datSD2$SD*(1-e10_datSD2$SD)/e10_datSD2$n)) #binomial
#pSD = (1 / (datSD$SD*(1-datSD$SD)*datSD$n)) #multinomial
e2010data_SD = list(SD = e10_datSD2$SD, precSD = pSD, xSD = rep(NA,e10_datSD2$fieldDate.num[length(e10_datSD2$fieldDate.num)]),
                    day = e10_datSD2$fieldDate.num, npolls = nrow(e10_datSD2), nperiods = as.numeric(end.date - orig.date))

writeLines(jags_SD,con="e2010kalman_SD.bug")
system.time(e2010jags_mod_SD <- jags.model("e2010kalman_SD.bug", data = e2010data_SD, n.chain=3))

system.time(e2010outSD <- coda.samples(e2010jags_mod_SD,variable.names = c("xSD", "SD"), n.iter = ninter, thin = 5))
e2010sumSD = summary(e2010outSD)
e2010cred_intSD = HPDinterval(e2010outSD[,which(regexpr("xSD", row.names(e2010sumSD$statistics))==1)], 0.95)

#################################

sumM = e2010sumM;outM = e2010outM;sumL = e2010sumL;outL = e2010outL;sumKD = e2010sumKD
outKD = e2010outKD;sumC = e2010sumC;outC = e2010outC;sumS = e2010sumS;outS = e2010outS
sumMP = e2010sumMP;outMP = e2010outMP;sumV = e2010sumV;outV = e2010outV;sumSD = e2010sumSD;outSD = e2010outSD

meanM = sumM$statistics[which(regexpr("xM", row.names(sumM$statistics))==1),1];meanL = sumL$statistics[which(regexpr("xL", row.names(sumL$statistics))==1),1]
meanKD = sumKD$statistics[which(regexpr("xKD", row.names(sumKD$statistics))==1),1];meanC = sumC$statistics[which(regexpr("xC", row.names(sumC$statistics))==1),1]
meanS = sumS$statistics[which(regexpr("xS", row.names(sumS$statistics))==1),1];meanMP = sumMP$statistics[which(regexpr("xMP", row.names(sumMP$statistics))==1),1]
meanV = sumV$statistics[which(regexpr("xV", row.names(sumV$statistics))==1),1];meanSD = sumSD$statistics[which(regexpr("xSD", row.names(sumSD$statistics))==1),1]

meanM[length(meanM)]; e2010cred_intM[[1]][nrow(e2010cred_intM[[1]]),];meanL[length(meanL)]; e2010cred_intL[[1]][nrow(e2010cred_intL[[1]]),]
meanKD[length(meanKD)]; e2010cred_intKD[[1]][nrow(e2010cred_intKD[[1]]),];meanC[length(meanC)]; e2010cred_intC[[1]][nrow(e2010cred_intC[[1]]),]
meanS[length(meanS)]; e2010cred_intS[[1]][nrow(e2010cred_intS[[1]]),];meanMP[length(meanMP)]; e2010cred_intMP[[1]][nrow(e2010cred_intMP[[1]]),]
meanV[length(meanV)]; e2010cred_intV[[1]][nrow(e2010cred_intV[[1]]),];meanSD[length(meanSD)]; e2010cred_intSD[[1]][nrow(e2010cred_intSD[[1]]),]

meanM[length(meanM)]-elec[4,1];meanL[length(meanL)]-elec[4,2];meanKD[length(meanKD)]-elec[4,3]
meanC[length(meanC)]-elec[4,4];meanS[length(meanS)]-elec[4,5];meanMP[length(meanMP)]-elec[4,6]
meanV[length(meanV)]-elec[4,7];meanSD[length(meanSD)]-elec[4,8]