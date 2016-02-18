data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", dec = ".", header = TRUE)
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

data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)

datM = na.omit(polls[,c('M','collectPeriodFrom','collectPeriodTo','n', 'PublDate')])
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
M[i] ~ dnorm(alphaM[day[i]],precM[i])
}
                 
#dynamic model 
for(i in 2:nperiods){
alphaM[i] ~ dnorm(alphaM[i-1],phiM)
}
                 
## priors
omega ~ dgamma(1, 1)
phiM <- 1/pow(omega,2)
}
'
pM = (1 / (datM$M*(1-datM$M)/datM$n))
data_M = list(M = M, precM = pM, alphaM = c(0.2623,rep(NA,end.date - orig.date-1)),
              day = datM$fieldDate.num, npolls = nrow(datM), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_M,con="kalman_M.bug")
                 
system.time(jags_mod_M <- jags.model("kalman_M.bug", data = data_M))
                 
system.time(outM <- coda.samples( jags_mod_M,variable.names = c("alphaM", "M" ), n.iter = 5000, n.thin = 100))
sumM = summary(outM)
                 #str(sumM)
                 plot(yM, type="l")