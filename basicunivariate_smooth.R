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
system.time(outM2 <- coda.samples(jags_mod_M2,variable.names = c("xM", "M"), n.iter = ninter, n.thin = 100))
#system.time(outM_jags <- jags.samples(jags_mod_M,variable.names = c("xM", "M","phiM" ), n.iter = 5000, n.thin = 10))
sumM = summary(outM2)
cred_intM = HPDinterval(outM2[,which(regexpr("xM", row.names(sumM$statistics))==1)], 0.95)
#sumM$quantiles[elec.day[3],c(1,5)] 
outM[,which(regexpr("xM", row.names(sumM2$statistics))==1)]
outM[,which(regexpr("xM", row.names(sumM$statistics))==1)]
plot()