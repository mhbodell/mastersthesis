#################################################
#################### NOVUS ######################
#################################################

library(dplyr)
orig.date = as.Date(datM$collectPeriodFrom[1]-1)
end.date = as.Date(datM$collectPeriodFrom[length(datM$collectPeriodFrom)]) #
df4 = df

dateDiff = df[,3] - datM3[,2]
dateDiff2 = dateDiff+1
nDay = datM3$n/as.numeric(dateDiff2)
MSmooth = rep(datM3$M,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datM3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datM3)){
  ee[[i]] = seq(datM3[i,2], datM3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datM3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datM2 = data.frame(M = MSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datM2 = datM2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(M*n), sample.size=sum(n), prop=votes/sample.size)

new.datM3 = new.datM2[order(new.datM2$year),]


jags_M.novus ='
model{
#observed model
for(i in 1:npolls){
xM[i] ~ dnorm(M[i],precM[i])
}
}
'

pM.novus = (1 / (new.datM3$prop*(1-new.datM3$prop)/new.datM3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_M.novus = list(precM = pM.novus, M = new.datM3$prop, xM= rep(NA, length(new.datM3$prop)),npolls = length(new.datM3$prop))
writeLines(jags_M.novus,con="kalman_M_novus.bug")

system.time(jags_mod_M_novus <- jags.model("kalman_M_novus.bug", data = data_M.novus, n.chain=3))
ninter=10000

system.time(outM.novus <- coda.samples(jags_mod_M_novus,variable.names = c("xM"), n.iter = ninter,thin = 5)) #
sumM.novus = summary(outM.novus)
cred_intM.novus = HPDinterval(outM.novus[,which(regexpr("xM", row.names(sumM.novus$statistics))==1)], 0.95)


############# L ###############
orig.date = as.Date(datL$collectPeriodFrom[1]-1)
end.date = as.Date(datL$collectPeriodFrom[length(datL$collectPeriodFrom)]) #
datL3 = datL

dateDiff = datL3[,3] - datL3[,2]
dateDiff2 = dateDiff+1
nDay = datL3$n/as.numeric(dateDiff2)
LSmooth = rep(datL3$L,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datL3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datL3)){
  ee[[i]] = seq(datL3[i,2], datL3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datL3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datL2 = data.frame(L = LSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datL2 = datL2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(L*n), sample.size=sum(n), prop=votes/sample.size)

new.datL3 = new.datL2[order(new.datL2$year),]


jags_L.novus ='
model{
#observed model
for(i in 1:npolls){
xL[i] ~ dnorm(L[i],precL[i])
}
}
'

pL.novus = (1 / (new.datL3$prop*(1-new.datL3$prop)/new.datL3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_L.novus = list(precL = pL.novus, L = new.datL3$prop, xL= rep(NA, length(new.datL3$prop)),npolls = length(new.datL3$prop))
writeLines(jags_L.novus,con="kalman_L_novus.bug")

system.time(jags_mod_L_novus <- jags.model("kalman_L_novus.bug", data = data_L.novus, n.chain=3))
ninter=10000

system.time(outL.novus <- coda.samples(jags_mod_L_novus,variable.names = c("xL"), n.iter = ninter,thin = 5)) #
sumL.novus = summary(outL.novus)
cred_intL.novus = HPDinterval(outL.novus[,which(regexpr("xL", row.names(sumL.novus$statistics))==1)], 0.95)

############# KD ###############
orig.date = as.Date(datKD$collectPeriodFrom[1]-1)
end.date = as.Date(datKD$collectPeriodFrom[length(datKD$collectPeriodFrom)]) #
datKD3 = datKD

dateDiff = datKD3[,3] - datKD3[,2]
dateDiff2 = dateDiff+1
nDay = datKD3$n/as.numeric(dateDiff2)
KDSmooth = rep(datKD3$KD,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datKD3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datKD3)){
  ee[[i]] = seq(datKD3[i,2], datKD3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datKD3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datKD2 = data.frame(KD = KDSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datKD2 = datKD2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(KD*n), sample.size=sum(n), prop=votes/sample.size)

new.datKD3 = new.datKD2[order(new.datKD2$year),]


jags_KD.novus ='
model{
#observed model
for(i in 1:npolls){
xKD[i] ~ dnorm(KD[i],precKD[i])
}
}
'

pKD.novus = (1 / (new.datKD3$prop*(1-new.datKD3$prop)/new.datKD3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_KD.novus = list(precKD = pKD.novus, KD = new.datKD3$prop, xKD= rep(NA, length(new.datKD3$prop)),npolls = length(new.datKD3$prop))
writeLines(jags_KD.novus,con="kalman_KD_novus.bug")

system.time(jags_mod_KD_novus <- jags.model("kalman_KD_novus.bug", data = data_KD.novus, n.chain=3))
ninter=10000

system.time(outKD.novus <- coda.samples(jags_mod_KD_novus,variable.names = c("xKD"), n.iter = ninter,thin = 5)) #
sumKD.novus = summary(outKD.novus)
cred_intKD.novus = HPDinterval(outKD.novus[,which(regexpr("xKD", row.names(sumKD.novus$statistics))==1)], 0.95)

############# C ###############
orig.date = as.Date(datC$collectPeriodFrom[1]-1)
end.date = as.Date(datC$collectPeriodFrom[length(datC$collectPeriodFrom)]) #
datC3 = datC

dateDiff = datC3[,3] - datC3[,2]
dateDiff2 = dateDiff+1
nDay = datC3$n/as.numeric(dateDiff2)
CSmooth = rep(datC3$C,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datC3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datC3)){
  ee[[i]] = seq(datC3[i,2], datC3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datC3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datC2 = data.frame(C = CSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datC2 = datC2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(C*n), sample.size=sum(n), prop=votes/sample.size)

new.datC3 = new.datC2[order(new.datC2$year),]


jags_C.novus ='
model{
#observed model
for(i in 1:npolls){
xC[i] ~ dnorm(C[i],precC[i])
}
}
'

pC.novus = (1 / (new.datC3$prop*(1-new.datC3$prop)/new.datC3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_C.novus = list(precC = pC.novus, C = new.datC3$prop, xC= rep(NA, length(new.datC3$prop)),npolls = length(new.datC3$prop))
writeLines(jags_C.novus,con="kalman_C_novus.bug")

system.time(jags_mod_C_novus <- jags.model("kalman_C_novus.bug", data = data_C.novus, n.chain=3))
ninter=10000

system.time(outC.novus <- coda.samples(jags_mod_C_novus,variable.names = c("xC"), n.iter = ninter,thin = 5)) #
sumC.novus = summary(outC.novus)
cred_intC.novus = HPDinterval(outC.novus[,which(regexpr("xC", row.names(sumC.novus$statistics))==1)], 0.95)

############# S ###############
orig.date = as.Date(datS$collectPeriodFrom[1]-1)
end.date = as.Date(datS$collectPeriodFrom[length(datS$collectPeriodFrom)]) #
datS3 = datS

dateDiff = datS3[,3] - datS3[,2]
dateDiff2 = dateDiff+1
nDay = datS3$n/as.numeric(dateDiff2)
SSmooth = rep(datS3$S,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datS3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datS3)){
  ee[[i]] = seq(datS3[i,2], datS3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datS3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datS2 = data.frame(S = SSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datS2 = datS2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(S*n), sample.size=sum(n), prop=votes/sample.size)

new.datS3 = new.datS2[order(new.datS2$year),]


jags_S.novus ='
model{
#observed model
for(i in 1:npolls){
xS[i] ~ dnorm(S[i],precS[i])
}
}
'

pS.novus = (1 / (new.datS3$prop*(1-new.datS3$prop)/new.datS3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_S.novus = list(precS = pS.novus, S = new.datS3$prop, xS= rep(NA, length(new.datS3$prop)),npolls = length(new.datS3$prop))
writeLines(jags_S.novus,con="kalman_S_novus.bug")

system.time(jags_mod_S_novus <- jags.model("kalman_S_novus.bug", data = data_S.novus, n.chain=3))
ninter=10000

system.time(outS.novus <- coda.samples(jags_mod_S_novus,variable.names = c("xS"), n.iter = ninter,thin = 5)) #
sumS.novus = summary(outS.novus)
cred_intS.novus = HPDinterval(outS.novus[,which(regexpr("xS", row.names(sumS.novus$statistics))==1)], 0.95)

############# MP ###############
orig.date = as.Date(datMP$collectPeriodFrom[1]-1)
end.date = as.Date(datMP$collectPeriodFrom[length(datMP$collectPeriodFrom)]) #
datMP3 = datMP

dateDiff = datMP3[,3] - datMP3[,2]
dateDiff2 = dateDiff+1
nDay = datMP3$n/as.numeric(dateDiff2)
MPSmooth = rep(datMP3$MP,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datMP3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datMP3)){
  ee[[i]] = seq(datMP3[i,2], datMP3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datMP3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datMP2 = data.frame(MP = MPSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datMP2 = datMP2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(MP*n), sample.size=sum(n), prop=votes/sample.size)

new.datMP3 = new.datMP2[order(new.datMP2$year),]


jags_MP.novus ='
model{
#observed model
for(i in 1:npolls){
xMP[i] ~ dnorm(MP[i],precMP[i])
}
}
'

pMP.novus = (1 / (new.datMP3$prop*(1-new.datMP3$prop)/new.datMP3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_MP.novus = list(precMP = pMP.novus, MP = new.datMP3$prop, xMP= rep(NA, length(new.datMP3$prop)),npolls = length(new.datMP3$prop))
writeLines(jags_MP.novus,con="kalman_MP_novus.bug")

system.time(jags_mod_MP_novus <- jags.model("kalman_MP_novus.bug", data = data_MP.novus, n.chain=3))
ninter=10000

system.time(outMP.novus <- coda.samples(jags_mod_MP_novus,variable.names = c("xMP"), n.iter = ninter,thin = 5)) #
sumMP.novus = summary(outMP.novus)
cred_intMP.novus = HPDinterval(outMP.novus[,which(regexpr("xMP", row.names(sumMP.novus$statistics))==1)], 0.95)

############# V ###############
orig.date = as.Date(datV$collectPeriodFrom[1]-1)
end.date = as.Date(datV$collectPeriodFrom[length(datV$collectPeriodFrom)]) #
datV3 = datV

dateDiff = datV3[,3] - datV3[,2]
dateDiff2 = dateDiff+1
nDay = datV3$n/as.numeric(dateDiff2)
VSmooth = rep(datV3$V,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datV3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datV3)){
  ee[[i]] = seq(datV3[i,2], datV3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datV3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datV2 = data.frame(V = VSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datV2 = datV2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(V*n), sample.size=sum(n), prop=votes/sample.size)

new.datV3 = new.datV2[order(new.datV2$year),]


jags_V.novus ='
model{
#observed model
for(i in 1:npolls){
xV[i] ~ dnorm(V[i],precV[i])
}
}
'

pV.novus = (1 / (new.datV3$prop*(1-new.datV3$prop)/new.datV3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_V.novus = list(precV = pV.novus, V = new.datV3$prop, xV= rep(NA, length(new.datV3$prop)),npolls = length(new.datV3$prop))
writeLines(jags_V.novus,con="kalman_V_novus.bug")

system.time(jags_mod_V_novus <- jags.model("kalman_V_novus.bug", data = data_V.novus, n.chain=3))
ninter=10000

system.time(outV.novus <- coda.samples(jags_mod_V_novus,variable.names = c("xV"), n.iter = ninter,thin = 5)) #
sumV.novus = summary(outV.novus)
cred_intV.novus = HPDinterval(outV.novus[,which(regexpr("xV", row.names(sumV.novus$statistics))==1)], 0.95)


############# SD ###############
orig.date = as.Date(datSD$collectPeriodFrom[1]-1)
end.date = as.Date(datSD$collectPeriodFrom[length(datSD$collectPeriodFrom)]) #
datSD3 = datSD

dateDiff = datSD3[,3] - datSD3[,2]
dateDiff2 = dateDiff+1
nDay = datSD3$n/as.numeric(dateDiff2)
SDSmooth = rep(datSD3$SD,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datSD3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datSD3)){
  ee[[i]] = seq(datSD3[i,2], datSD3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datSD3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datSD2 = data.frame(SD = SDSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datSD2 = datSD2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(SD*n), sample.size=sum(n), prop=votes/sample.size)

new.datSD3 = new.datSD2[order(new.datSD2$year),]


jags_SD.novus ='
model{
#observed model
for(i in 1:npolls){
xSD[i] ~ dnorm(SD[i],precSD[i])
}
}
'

pSD.novus = (1 / (new.datSD3$prop*(1-new.datSD3$prop)/new.datSD3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_SD.novus = list(precSD = pSD.novus, SD = new.datSD3$prop, xSD= rep(NA, length(new.datSD3$prop)),npolls = length(new.datSD3$prop))
writeLines(jags_SD.novus,con="kalman_SD_novus.bug")

system.time(jags_mod_SD_novus <- jags.model("kalman_SD_novus.bug", data = data_SD.novus, n.chain=3))
ninter=10000

system.time(outSD.novus <- coda.samples(jags_mod_SD_novus,variable.names = c("xSD"), n.iter = ninter,thin = 5)) #
sumSD.novus = summary(outSD.novus)
cred_intSD.novus = HPDinterval(outSD.novus[,which(regexpr("xSD", row.names(sumSD.novus$statistics))==1)], 0.95)





################################
############ PLOTS #############
################################

rrr = NULL
for(i in 1: nrow(new.datM3)){
  rr = paste(new.datM3[i,'year'], new.datM3[i,'month'], sep="-", collapse="")
  rrr[i] = paste(rr, "-15", sep="", colapse="")
}

dfM.Novus = data.frame(party = sumM.novus$statistics[-1,1]*100, time=as.Date(rrr[-1]),
                       low=cred_intM.novus[[1]][-1,1]*100, high=cred_intM.novus[[1]][-1,2]*100)
pointsM =  data.frame(x=seq(as.Date(datM$collectPeriodFrom[1]-1),by='days',length=datM$fieldDate.num[length(datM$fieldDate.num)])[datM$fieldDate.num[-1]],
                      y=datM$M[-1]*100, house=datM$house[-1], party=rep("M",datM$fieldDate.num[length(datM$fieldDate.num)])[datM$fieldDate.num[-1]])

rrr = NULL
for(i in 1: nrow(new.datL3)){
  rr = paste(new.datL3[i,'year'], new.datL3[i,'month'], sep="-", collapse="")
  rrr[i] = paste(rr, "-15", sep="", colapse="")
}
dfL.Novus = data.frame(party = sumL.novus$statistics[-1,1]*100, time=as.Date(rrr[-1]),
                       low=cred_intL.novus[[1]][-1,1]*100, high=cred_intL.novus[[1]][-1,2]*100)
pointsL =  data.frame(x=seq(as.Date(datL$collectPeriodFrom[1]-1),by='days',length=datL$fieldDate.num[length(datL$fieldDate.num)])[datL$fieldDate.num[-1]],
                      y=datL$L[-1]*100, house=datL$house[-1], party=rep("L",datL$fieldDate.num[length(datL$fieldDate.num)])[datL$fieldDate.num[-1]])

rrr = NULL
for(i in 1: nrow(new.datKD3)){
  rr = paste(new.datKD3[i,'year'], new.datKD3[i,'month'], sep="-", collapse="")
  rrr[i] = paste(rr, "-15", sep="", colapse="")
}
dfKD.Novus = data.frame(party = sumKD.novus$statistics[-1,1]*100, time=as.Date(rrr[-1]),
                        low=cred_intKD.novus[[1]][-1,1]*100, high=cred_intKD.novus[[1]][-1,2]*100)
pointsKD =  data.frame(x=seq(as.Date(datKD$collectPeriodFrom[1]-1),by='days',length=datKD$fieldDate.num[length(datKD$fieldDate.num)])[datKD$fieldDate.num[-1]],
                       y=datKD$KD[-1]*100, house=datKD$house[-1], party=rep("KD",datKD$fieldDate.num[length(datKD$fieldDate.num)])[datKD$fieldDate.num[-1]])

rrr = NULL
for(i in 1: nrow(new.datC3)){
  rr = paste(new.datC3[i,'year'], new.datC3[i,'month'], sep="-", collapse="")
  rrr[i] = paste(rr, "-15", sep="", colapse="")
}
dfC.Novus = data.frame(party = sumC.novus$statistics[-1,1]*100, time=as.Date(rrr[-1]),
                       low=cred_intC.novus[[1]][-1,1]*100, high=cred_intC.novus[[1]][-1,2]*100)
pointsC =  data.frame(x=seq(as.Date(datC$collectPeriodFrom[1]-1),by='days',length=datC$fieldDate.num[length(datC$fieldDate.num)])[datC$fieldDate.num[-1]],
                      y=datC$C[-1]*100, house=datC$house[-1], party=rep("C",datC$fieldDate.num[length(datC$fieldDate.num)])[datC$fieldDate.num[-1]])
rrr = NULL
for(i in 1: nrow(new.datS3)){
  rr = paste(new.datS3[i,'year'], new.datS3[i,'month'], sep="-", collapse="")
  rrr[i] = paste(rr, "-15", sep="", colapse="")
}
dfS.Novus = data.frame(party = sumS.novus$statistics[-1,1]*100, time=as.Date(rrr[-1]),
                       low=cred_intS.novus[[1]][-1,1]*100, high=cred_intS.novus[[1]][-1,2]*100)
pointsS =  data.frame(x=seq(as.Date(datS$collectPeriodFrom[1]-1),by='days',length=datS$fieldDate.num[length(datS$fieldDate.num)])[datS$fieldDate.num[-1]],
                      y=datS$S[-1]*100, house=datS$house[-1], party=rep("S",datS$fieldDate.num[length(datS$fieldDate.num)])[datS$fieldDate.num[-1]])
rrr = NULL
for(i in 1: nrow(new.datMP3)){
  rr = paste(new.datMP3[i,'year'], new.datMP3[i,'month'], sep="-", collapse="")
  rrr[i] = paste(rr, "-15", sep="", colapse="")
}
dfMP.Novus = data.frame(party = sumMP.novus$statistics[-1,1]*100, time=as.Date(rrr[-1]),
                        low=cred_intMP.novus[[1]][-1,1]*100, high=cred_intMP.novus[[1]][-1,2]*100)
pointsMP =  data.frame(x=seq(as.Date(datMP$collectPeriodFrom[1]-1),by='days',length=datMP$fieldDate.num[length(datMP$fieldDate.num)])[datMP$fieldDate.num[-1]],
                       y=datMP$MP[-1]*100, house=datMP$house[-1], party=rep("MP",datMP$fieldDate.num[length(datMP$fieldDate.num)])[datMP$fieldDate.num[-1]])

rrr = NULL
for(i in 1: nrow(new.datV3)){
  rr = paste(new.datV3[i,'year'], new.datV3[i,'month'], sep="-", collapse="")
  rrr[i] = paste(rr, "-15", sep="", colapse="")
}
dfV.Novus = data.frame(party = sumV.novus$statistics[-1,1]*100, time=as.Date(rrr[-1]),
                       low=cred_intV.novus[[1]][-1,1]*100, high=cred_intV.novus[[1]][-1,2]*100)
pointsV =  data.frame(x=seq(as.Date(datV$collectPeriodFrom[1]-1),by='days',length=datV$fieldDate.num[length(datV$fieldDate.num)])[datV$fieldDate.num[-1]],
                      y=datV$V[-1]*100, house=datV$house[-1], party=rep("V",datV$fieldDate.num[length(datV$fieldDate.num)])[datV$fieldDate.num[-1]])
rrr = NULL
for(i in 1: nrow(new.datSD3)){
  rr = paste(new.datSD3[i,'year'], new.datSD3[i,'month'], sep="-", collapse="")
  rrr[i] = paste(rr, "-15", sep="", colapse="")
}
dfSD.Novus = data.frame(party = sumSD.novus$statistics[-1,1]*100, time=as.Date(rrr[-1]),
                        low=cred_intSD.novus[[1]][-1,1]*100, high=cred_intSD.novus[[1]][-1,2]*100)
pointsSD =  data.frame(x=seq(as.Date(datSD$collectPeriodFrom[1]-1),by='days',length=datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]],
                       y=datSD$SD[-1]*100, house=datSD$house[-1], party=rep("SD",datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]])

library(ggplot2)
gM.Novus <- ggplot(dfM.Novus) +
  aes(x = time, y = party) +
  geom_line(colour = "blue") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="blue") +
  geom_point(data=pointsM, aes(x=x, y=y), alpha = 1, color="blue", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for M (%)")) +
  ggtitle("M") +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gL.Novus <-  ggplot(dfL.Novus) +
  aes(x = time, y = party) +
  geom_line(colour = "lightblue3") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="lightblue3") +
  geom_point(data=pointsL, aes(x=x, y=y), alpha = 1, color="lightblue3", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for L (%)")) +
  ggtitle("L") +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gKD.Novus <- ggplot(dfKD.Novus) +
  aes(x = time, y = party) +
  geom_line(colour = "darkblue") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="darkblue") +
  geom_point(data=pointsKD, aes(x=x, y=y), alpha = 1, color="darkblue", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for KD (%)")) +
  ggtitle("KD") +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gC.Novus <- ggplot(dfC.Novus) +
  aes(x = time, y = party) +
  geom_line(colour = "chartreuse3") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="chartreuse3") +
  geom_point(data=pointsC, aes(x=x, y=y), alpha = 1, color="chartreuse3", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for C (%)")) +
  ggtitle("C") +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gS.Novus <- ggplot(dfS.Novus) +
  aes(x = time, y = party) +
  geom_line(colour = "red") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="red") +
  geom_point(data=pointsS, aes(x=x, y=y), alpha = 1, color="red", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for S (%)")) +
  ggtitle("S") +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gMP.Novus <- ggplot(dfMP.Novus) +
  aes(x = time, y = party) +
  geom_line(colour = "forestgreen") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="forestgreen") +
  geom_point(data=pointsMP, aes(x=x, y=y), alpha = 1, color="forestgreen", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for MP (%)")) +
  ggtitle("MP") +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gV.Novus <- ggplot(dfV.Novus) +
  aes(x = time, y = party) +
  geom_line(colour = "darkred") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="darkred") +
  geom_point(data=pointsV, aes(x=x, y=y), alpha = 1, color="darkred", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for V (%)")) +
  ggtitle("V") +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gSD.Novus <- ggplot(dfSD.Novus) +
  aes(x = time, y = party) +
  geom_line(colour = "skyblue3") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="skyblue3") +
  geom_point(data=pointsSD, aes(x=x, y=y), alpha = 1, color="skyblue3", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for SD (%)")) +
  ggtitle("SD") +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

multiplot(gM.Novus, gL.Novus, gKD.Novus, gC.Novus, gS.Novus, gMP.Novus, gV.Novus, gSD.Novus, cols=2)


##################################################
############## MODEL EVALUATION ##################
##################################################

str()
set.seed(901207)
i = sample(1:3,1)
rsimM.novus = outM.novus[[i]]
rsimL.novus = outL.novus[[i]]
rsimKD.novus = outKD.novus[[i]]
rsimC.novus = outC.novus[[i]]
rsimS.novus = outS.novus[[i]]
rsimMP.novus = outMP.novus[[i]]
rsimV.novus = outV.novus[[i]]
rsimSD.novus = outSD.novus[[i]]
nsim = dim(rsimM.novus)[1]
simxM.novus = rsimM.novus[,which(regexpr("xM", colnames(rsimM.novus))==1)]
simxL.novus = rsimL.novus[,which(regexpr("xL", colnames(rsimL.novus))==1)]
simxKD.novus = rsimKD.novus[,which(regexpr("xKD", colnames(rsimKD.novus))==1)]
simxC.novus = rsimC.novus[,which(regexpr("xC", colnames(rsimC.novus))==1)]
simxS.novus = rsimS.novus[,which(regexpr("xS", colnames(rsimS.novus))==1)]
simxMP.novus = rsimMP.novus[,which(regexpr("xMP", colnames(rsimMP.novus))==1)]
simxV.novus = rsimV.novus[,which(regexpr("xV", colnames(rsimV.novus))==1)]
simxSD.novus = rsimSD.novus[,which(regexpr("xSD", colnames(rsimSD.novus))==1)]
varM = matrix (NA, nrow=nsim, ncol=nrow(datM));varL = matrix (NA, nrow=nsim, ncol=nrow(datL))
varKD = matrix (NA, nrow=nsim, ncol=nrow(datKD));varC = matrix (NA, nrow=nsim, ncol=nrow(datC))
varS = matrix (NA, nrow=nsim, ncol=nrow(datS));varMP = matrix (NA, nrow=nsim, ncol=nrow(datMP))
varV = matrix (NA, nrow=nsim, ncol=nrow(datV));varSD = matrix (NA, nrow=nsim, ncol=nrow(datSD))
for(i in 1:nrow(varM)){
  varM[i,] = 1/pM;varL[i,] = 1/pL;varKD[i,] = 1/pKD; varC[i,] = 1/pC
  varS[i,] = 1/pS;varMP[i,] = 1/pMP;varV[i,] = 1/pV;varSD[i,] = 1/pSD
}


####### y^rep #####

testMin.novus = matrix(NA,ncol=8, nrow=100)
testMax.novus = matrix(NA,ncol=8, nrow=100)
testMean.novus = matrix(NA,ncol=8, nrow=100)
colnames(testMin) = colnames(testMax) = colnames(testMean) = c("M","L","KD","C","S","MP","V","SD") 
for(i in 1:100){
  yrepM.novus = sapply(1:nsim, function(s) rnorm(length(new.datM3$prop),simxM.novus[s,], varM[s,]))
  yrepL.novus = sapply(1:nsim, function(s) rnorm(length(new.datL3$prop),simxL.novus[s,], varL[s,] ))
  yrepKD.novus = sapply(1:nsim, function(s) rnorm(length(new.datKD3$prop),simxKD.novus[s,], varKD[s,] ))
  yrepC.novus = sapply(1:nsim, function(s) rnorm(length(new.datC3$prop),simxC.novus[s,], varC[s,] ))
  yrepS.novus = sapply(1:nsim, function(s) rnorm(length(new.datS3$prop),simxS.novus[s,], varS[s,] ))
  yrepMP.novus = sapply(1:nsim, function(s) rnorm(length(new.datMP3$prop),simxMP.novus[s,], varMP[s,] ))
  yrepV.novus = sapply(1:nsim, function(s) rnorm(length(new.datV3$prop),simxV.novus[s,], varV[s,] ))
  yrepSD.novus = sapply(1:nsim, function(s) rnorm(length(new.datSD3$prop),simxSD.novus[s,], varSD[s,] ))
  
  min_repM.novus = apply(yrepM.novus,2,min)
  min_repL.novus = apply(yrepL.novus,2,min)
  min_repKD.novus = apply(yrepKD.novus,2,min)
  min_repC.novus = apply(yrepC.novus,2,min)
  min_repS.novus = apply(yrepS.novus,2,min)
  min_repMP.novus = apply(yrepMP.novus,2,min)
  min_repV.novus = apply(yrepV.novus,2,min)
  min_repSD.novus = apply(yrepSD.novus,2,min)
  testMin.novus[i,] = c(sum(ifelse(min_repM.novus>= min(datM$M),1,0))/length(min_repM.novus),sum(ifelse(min_repL.novus>=min(datL$L),1,0))/length(min_repL.novus),sum(ifelse(min_repKD.novus>=min(datKD$KD),1,0))/length(min_repKD.novus),
                  sum(ifelse(min_repC.novus>=min(datC$C),1,0))/length(min_repC.novus),sum(ifelse(min_repS.novus>=min(datS$S),1,0))/length(min_repS.novus),sum(ifelse(min_repMP.novus>=min(datMP$MP),1,0))/length(min_repMP.novus),
                  sum(ifelse(min_repV.novus>=min(datV$V),1,0))/length(min_repV.novus),sum(ifelse(min_repSD.novus>=min(datSD$SD),1,0))/length(min_repSD.novus))
  max_repM.novus = apply(yrepM.novus,2,max)
  max_repL.novus = apply(yrepL.novus,2,max)
  max_repKD.novus = apply(yrepKD.novus,2,max)
  max_repC.novus = apply(yrepC.novus,2,max)
  max_repS.novus = apply(yrepS.novus,2,max)
  max_repMP.novus = apply(yrepMP.novus,2,max)
  max_repV.novus = apply(yrepV.novus,2,max)
  max_repSD.novus = apply(yrepSD.novus,2,max)
  testMax.novus[i,] = c(sum(ifelse(max_repM.novus>= max(datM$M),1,0))/length(max_repM.novus),sum(ifelse(max_repL.novus>=max(datL$L),1,0))/length(max_repL.novus),sum(ifelse(max_repKD.novus>=max(datKD$KD),1,0))/length(max_repKD.novus),
                  sum(ifelse(max_repC.novus>=max(datC$C),1,0))/length(max_repC.novus),sum(ifelse(max_repS.novus>=max(datS$S),1,0))/length(max_repS.novus),sum(ifelse(max_repMP.novus>=max(datMP$MP),1,0))/length(max_repMP.novus),
                  sum(ifelse(max_repV.novus>=max(datV$V),1,0))/length(max_repV.novus),sum(ifelse(max_repSD.novus>=max(datSD$SD),1,0))/length(max_repSD.novus))
  
  mean_repM.novus = apply(yrepM.novus,2,mean)
  mean_repL.novus = apply(yrepL.novus,2,mean)
  mean_repKD.novus = apply(yrepKD.novus,2,mean)
  mean_repC.novus = apply(yrepC.novus,2,mean)
  mean_repS.novus = apply(yrepS.novus,2,mean)
  mean_repMP.novus = apply(yrepMP.novus,2,mean)
  mean_repV.novus = apply(yrepV.novus,2,mean)
  mean_repSD.novus = apply(yrepSD.novus,2,mean)
  testMean.novus[i,] = c(sum(ifelse(mean_repM.novus>= mean(datM$M),1,0))/length(mean_repM.novus),sum(ifelse(mean_repL.novus>=mean(datL$L),1,0))/length(mean_repL.novus),sum(ifelse(mean_repKD.novus>=mean(datKD$KD),1,0))/length(mean_repKD.novus),
                   sum(ifelse(mean_repC.novus>=mean(datC$C),1,0))/length(mean_repC.novus),sum(ifelse(mean_repS.novus>=mean(datS$S),1,0))/length(mean_repS.novus),sum(ifelse(mean_repMP.novus>=mean(datMP$MP),1,0))/length(mean_repMP.novus),
                   sum(ifelse(mean_repV.novus>=mean(datV$V),1,0))/length(mean_repV.novus),sum(ifelse(mean_repSD.novus>=mean(datSD$SD),1,0))/length(mean_repSD.novus))
  
}

for(i in 1:8){
  print(paste(colnames(testMin.novus)[i],":"))
  print(paste("Min:",mean(testMin.novus[,i]), sep=" "))
  print(paste("Max:",mean(testMax.novus[,i]), sep=" "))
  print(paste("Mean:",mean(testMean.novus[,i]), sep=" "))
}


################################################
####### predicting election outcome 2010 #######
################################################


############# M ###################
orig.date = as.Date(datM$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datM3 = datM[-which(datM$collectPeriodTo>end.date),]
datM3 = datM3[-nrow(datM3 ),]


dateDiff = datM3[,3] - datM3[,2]
dateDiff2 = dateDiff+1
nDay = datM3$n/as.numeric(dateDiff2)
MSmooth = rep(datM3$M,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datM3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datM3)){
  ee[[i]] = seq(datM3[i,2], datM3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datM3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datM2 = data.frame(M = MSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datM2 = datM2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(M*n), sample.size=sum(n), prop=votes/sample.size)

new.datM3 = new.datM2[order(new.datM2$year),]


jags_M.novus ='
model{
#observed model
for(i in 1:npolls){
xM[i] ~ dnorm(M[i],precM[i])
}
}
'

pM.novus = (1 / (new.datM3$prop*(1-new.datM3$prop)/new.datM3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_M.novus = list(precM = pM.novus, M = new.datM3$prop, xM= rep(NA, length(new.datM3$prop)),npolls = length(new.datM3$prop))
writeLines(jags_M.novus,con="kalman_M_novus.bug")

system.time(jags_mod_M_novus <- jags.model("kalman_M_novus.bug", data = data_M.novus, n.chain=3))
ninter=10000

system.time(e10_outM.novus <- coda.samples(jags_mod_M_novus,variable.names = c("xM"), n.iter = ninter,thin = 5)) #
e10_sumM.novus = summary(e10_outM.novus)
e10_cred_intM.novus = HPDinterval(e10_outM.novus[,which(regexpr("xM", row.names(e10_sumM.novus$statistics))==1)], 0.95)

################# L ###############

orig.date = as.Date(datL$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datL3 = datL[-which(datL$collectPeriodTo>end.date),]
datL3 = datL3[-nrow(datL3 ),]


dateDiff = datL3[,3] - datL3[,2]
dateDiff2 = dateDiff+1
nDay = datL3$n/as.numeric(dateDiff2)
LSmooth = rep(datL3$L,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datL3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datL3)){
  ee[[i]] = seq(datL3[i,2], datL3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datL3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datL2 = data.frame(L = LSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datL2 = datL2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(L*n), sample.size=sum(n), prop=votes/sample.size)

new.datL3 = new.datL2[order(new.datL2$year),]


jags_L.novus ='
model{
#observed model
for(i in 1:npolls){
xL[i] ~ dnorm(L[i],precL[i])
}
}
'

pL.novus = (1 / (new.datL3$prop*(1-new.datL3$prop)/new.datL3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_L.novus = list(precL = pL.novus, L = new.datL3$prop, xL= rep(NA, length(new.datL3$prop)),npolls = length(new.datL3$prop))
writeLines(jags_L.novus,con="kalman_L_novus.bug")

system.time(jags_mod_L_novus <- jags.model("kalman_L_novus.bug", data = data_L.novus, n.chain=3))
ninter=10000

system.time(e10_outL.novus <- coda.samples(jags_mod_L_novus,variable.names = c("xL"), n.iter = ninter,thin = 5)) #
e10_sumL.novus = summary(e10_outL.novus)
e10_cred_intL.novus = HPDinterval(e10_outL.novus[,which(regexpr("xL", row.names(e10_sumL.novus$statistics))==1)], 0.95)


################# KD ###############

orig.date = as.Date(datKD$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datKD3 = datKD[-which(datKD$collectPeriodTo>end.date),]
datKD3 = datKD3[-nrow(datKD3 ),]


dateDiff = datKD3[,3] - datKD3[,2]
dateDiff2 = dateDiff+1
nDay = datKD3$n/as.numeric(dateDiff2)
KDSmooth = rep(datKD3$KD,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datKD3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datKD3)){
  ee[[i]] = seq(datKD3[i,2], datKD3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datKD3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datKD2 = data.frame(KD = KDSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datKD2 = datKD2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(KD*n), sample.size=sum(n), prop=votes/sample.size)

new.datKD3 = new.datKD2[order(new.datKD2$year),]


jags_KD.novus ='
model{
#observed model
for(i in 1:npolls){
xKD[i] ~ dnorm(KD[i],precKD[i])
}
}
'

pKD.novus = (1 / (new.datKD3$prop*(1-new.datKD3$prop)/new.datKD3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_KD.novus = list(precKD = pKD.novus, KD = new.datKD3$prop, xKD= rep(NA, length(new.datKD3$prop)),npolls = length(new.datKD3$prop))
writeLines(jags_KD.novus,con="kalman_KD_novus.bug")

system.time(jags_mod_KD_novus <- jags.model("kalman_KD_novus.bug", data = data_KD.novus, n.chain=3))
ninter=10000

system.time(e10_outKD.novus <- coda.samples(jags_mod_KD_novus,variable.names = c("xKD"), n.iter = ninter,thin = 5)) #
e10_sumKD.novus = summary(e10_outKD.novus)
e10_cred_intKD.novus = HPDinterval(e10_outKD.novus[,which(regexpr("xKD", row.names(e10_sumKD.novus$statistics))==1)], 0.95)


################# C ###############

orig.date = as.Date(datC$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datC3 = datC[-which(datC$collectPeriodTo>end.date),]
datC3 = datC3[-nrow(datC3 ),]


dateDiff = datC3[,3] - datC3[,2]
dateDiff2 = dateDiff+1
nDay = datC3$n/as.numeric(dateDiff2)
CSmooth = rep(datC3$C,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datC3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datC3)){
  ee[[i]] = seq(datC3[i,2], datC3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datC3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datC2 = data.frame(C = CSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datC2 = datC2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(C*n), sample.size=sum(n), prop=votes/sample.size)

new.datC3 = new.datC2[order(new.datC2$year),]


jags_C.novus ='
model{
#observed model
for(i in 1:npolls){
xC[i] ~ dnorm(C[i],precC[i])
}
}
'

pC.novus = (1 / (new.datC3$prop*(1-new.datC3$prop)/new.datC3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_C.novus = list(precC = pC.novus, C = new.datC3$prop, xC= rep(NA, length(new.datC3$prop)),npolls = length(new.datC3$prop))
writeLines(jags_C.novus,con="kalman_C_novus.bug")

system.time(jags_mod_C_novus <- jags.model("kalman_C_novus.bug", data = data_C.novus, n.chain=3))
ninter=10000

system.time(e10_outC.novus <- coda.samples(jags_mod_C_novus,variable.names = c("xC"), n.iter = ninter,thin = 5)) #
e10_sumC.novus = summary(e10_outC.novus)
e10_cred_intC.novus = HPDinterval(e10_outC.novus[,which(regexpr("xC", row.names(e10_sumC.novus$statistics))==1)], 0.95)


################# S ###############

orig.date = as.Date(datS$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datS3 = datS[-which(datS$collectPeriodTo>end.date),]
datS3 = datS3[-nrow(datS3 ),]


dateDiff = datS3[,3] - datS3[,2]
dateDiff2 = dateDiff+1
nDay = datS3$n/as.numeric(dateDiff2)
SSmooth = rep(datS3$S,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datS3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datS3)){
  ee[[i]] = seq(datS3[i,2], datS3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datS3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datS2 = data.frame(S = SSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datS2 = datS2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(S*n), sample.size=sum(n), prop=votes/sample.size)

new.datS3 = new.datS2[order(new.datS2$year),]


jags_S.novus ='
model{
#observed model
for(i in 1:npolls){
xS[i] ~ dnorm(S[i],precS[i])
}
}
'

pS.novus = (1 / (new.datS3$prop*(1-new.datS3$prop)/new.datS3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_S.novus = list(precS = pS.novus, S = new.datS3$prop, xS= rep(NA, length(new.datS3$prop)),npolls = length(new.datS3$prop))
writeLines(jags_S.novus,con="kalman_S_novus.bug")

system.time(jags_mod_S_novus <- jags.model("kalman_S_novus.bug", data = data_S.novus, n.chain=3))
ninter=10000

system.time(e10_outS.novus <- coda.samples(jags_mod_S_novus,variable.names = c("xS"), n.iter = ninter,thin = 5)) #
e10_sumS.novus = summary(e10_outS.novus)
e10_cred_intS.novus = HPDinterval(e10_outS.novus[,which(regexpr("xS", row.names(e10_sumS.novus$statistics))==1)], 0.95)

################# MP ###############

orig.date = as.Date(datMP$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datMP3 = datMP[-which(datMP$collectPeriodTo>end.date),]
datMP3 = datMP3[-nrow(datMP3 ),]


dateDiff = datMP3[,3] - datMP3[,2]
dateDiff2 = dateDiff+1
nDay = datMP3$n/as.numeric(dateDiff2)
MPSmooth = rep(datMP3$MP,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datMP3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datMP3)){
  ee[[i]] = seq(datMP3[i,2], datMP3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datMP3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datMP2 = data.frame(MP = MPSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datMP2 = datMP2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(MP*n), sample.size=sum(n), prop=votes/sample.size)

new.datMP3 = new.datMP2[order(new.datMP2$year),]


jags_MP.novus ='
model{
#observed model
for(i in 1:npolls){
xMP[i] ~ dnorm(MP[i],precMP[i])
}
}
'

pMP.novus = (1 / (new.datMP3$prop*(1-new.datMP3$prop)/new.datMP3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_MP.novus = list(precMP = pMP.novus, MP = new.datMP3$prop, xMP= rep(NA, length(new.datMP3$prop)),npolls = length(new.datMP3$prop))
writeLines(jags_MP.novus,con="kalman_MP_novus.bug")

system.time(jags_mod_MP_novus <- jags.model("kalman_MP_novus.bug", data = data_MP.novus, n.chain=3))
ninter=10000

system.time(e10_outMP.novus <- coda.samples(jags_mod_MP_novus,variable.names = c("xMP"), n.iter = ninter,thin = 5)) #
e10_sumMP.novus = summary(e10_outMP.novus)
e10_cred_intMP.novus = HPDinterval(e10_outMP.novus[,which(regexpr("xMP", row.names(e10_sumMP.novus$statistics))==1)], 0.95)

################# V ###############

orig.date = as.Date(datV$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datV3 = datV[-which(datV$collectPeriodTo>end.date),]
datV3 = datV3[-nrow(datV3 ),]


dateDiff = datV3[,3] - datV3[,2]
dateDiff2 = dateDiff+1
nDay = datV3$n/as.numeric(dateDiff2)
VSmooth = rep(datV3$V,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datV3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datV3)){
  ee[[i]] = seq(datV3[i,2], datV3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datV3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datV2 = data.frame(V = VSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datV2 = datV2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(V*n), sample.size=sum(n), prop=votes/sample.size)

new.datV3 = new.datV2[order(new.datV2$year),]


jags_V.novus ='
model{
#observed model
for(i in 1:npolls){
xV[i] ~ dnorm(V[i],precV[i])
}
}
'

pV.novus = (1 / (new.datV3$prop*(1-new.datV3$prop)/new.datV3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_V.novus = list(precV = pV.novus, V = new.datV3$prop, xV= rep(NA, length(new.datV3$prop)),npolls = length(new.datV3$prop))
writeLines(jags_V.novus,con="kalman_V_novus.bug")

system.time(jags_mod_V_novus <- jags.model("kalman_V_novus.bug", data = data_V.novus, n.chain=3))
ninter=10000

system.time(e10_outV.novus <- coda.samples(jags_mod_V_novus,variable.names = c("xV"), n.iter = ninter,thin = 5)) #
e10_sumV.novus = summary(e10_outV.novus)
e10_cred_intV.novus = HPDinterval(e10_outV.novus[,which(regexpr("xV", row.names(e10_sumV.novus$statistics))==1)], 0.95)

################# SD ###############

orig.date = as.Date(datSD$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[3])
datSD3 = datSD[-which(datSD$collectPeriodTo>end.date),]
datSD3 = datSD3[-nrow(datSD3 ),]


dateDiff = datSD3[,3] - datSD3[,2]
dateDiff2 = dateDiff+1
nDay = datSD3$n/as.numeric(dateDiff2)
SDSmooth = rep(datSD3$SD,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datSD3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datSD3)){
  ee[[i]] = seq(datSD3[i,2], datSD3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datSD3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datSD2 = data.frame(SD = SDSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datSD2 = datSD2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(SD*n), sample.size=sum(n), prop=votes/sample.size)

new.datSD3 = new.datSD2[order(new.datSD2$year),]


jags_SD.novus ='
model{
#observed model
for(i in 1:npolls){
xSD[i] ~ dnorm(SD[i],precSD[i])
}
}
'

pSD.novus = (1 / (new.datSD3$prop*(1-new.datSD3$prop)/new.datSD3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_SD.novus = list(precSD = pSD.novus, SD = new.datSD3$prop, xSD= rep(NA, length(new.datSD3$prop)),npolls = length(new.datSD3$prop))
writeLines(jags_SD.novus,con="kalman_SD_novus.bug")

system.time(jags_mod_SD_novus <- jags.model("kalman_SD_novus.bug", data = data_SD.novus, n.chain=3))
ninter=10000

system.time(e10_outSD.novus <- coda.samples(jags_mod_SD_novus,variable.names = c("xSD"), n.iter = ninter,thin = 5)) #
e10_sumSD.novus = summary(e10_outSD.novus)
e10_cred_intSD.novus = HPDinterval(e10_outSD.novus[,which(regexpr("xSD", row.names(e10_sumSD.novus$statistics))==1)], 0.95)


###############################################
sumM = e10_sumM.novus;outM = e10_outM.novus;sumL = e10_sumL.novus;outL = e10_outL.novus;sumKD = e10_sumKD.novus
outKD = e10_outKD.novus;sumC = e10_sumC.novus;outC = e10_outC.novus;sumS = e10_sumS.novus;outS = e10_outS.novus
sumMP = e10_sumMP.novus;outMP = e10_outMP.novus;sumV = e10_sumV.novus;outV = e10_outV.novus;sumSD = e10_sumSD.novus;outSD = e10_outSD.novus

meanM = sumM$statistics[which(regexpr("xM", row.names(sumM$statistics))==1),1];meanL = sumL$statistics[which(regexpr("xL", row.names(sumL$statistics))==1),1]
meanKD = sumKD$statistics[which(regexpr("xKD", row.names(sumKD$statistics))==1),1];meanC = sumC$statistics[which(regexpr("xC", row.names(sumC$statistics))==1),1]
meanS = sumS$statistics[which(regexpr("xS", row.names(sumS$statistics))==1),1];meanMP = sumMP$statistics[which(regexpr("xMP", row.names(sumMP$statistics))==1),1]
meanV = sumV$statistics[which(regexpr("xV", row.names(sumV$statistics))==1),1];meanSD = sumSD$statistics[which(regexpr("xSD", row.names(sumSD$statistics))==1),1]

meanM[length(meanM)]; e10_cred_intM.novus[[1]][nrow(e10_cred_intM.novus[[1]]),];meanL[length(meanL)]; e10_cred_intL.novus[[1]][nrow(e10_cred_intL.novus[[1]]),]
meanKD[length(meanKD)]; e10_cred_intKD.novus[[1]][nrow(e10_cred_intKD.novus[[1]]),];meanC[length(meanC)]; e10_cred_intC.novus[[1]][nrow(e10_cred_intC.novus[[1]]),]
meanS[length(meanS)]; e10_cred_intS.novus[[1]][nrow(e10_cred_intS.novus[[1]]),];meanMP[length(meanMP)]; e10_cred_intMP.novus[[1]][nrow(e10_cred_intMP.novus[[1]]),]
meanV[length(meanV)]; e10_cred_intV.novus[[1]][nrow(e10_cred_intV.novus[[1]]),];meanSD[length(meanSD)]; e10_cred_intSD.novus[[1]][nrow(e10_cred_intSD.novus[[1]]),]

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
datM3 = datM[-which(datM$collectPeriodTo>end.date),]
datM3 = datM3[-nrow(datM3 ),]


dateDiff = datM3[,3] - datM3[,2]
dateDiff2 = dateDiff+1
nDay = datM3$n/as.numeric(dateDiff2)
MSmooth = rep(datM3$M,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datM3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datM3)){
  ee[[i]] = seq(datM3[i,2], datM3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datM3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datM2 = data.frame(M = MSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datM2 = datM2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(M*n), sample.size=sum(n), prop=votes/sample.size)

new.datM3 = new.datM2[order(new.datM2$year),]


jags_M.novus ='
model{
#observed model
for(i in 1:npolls){
xM[i] ~ dnorm(M[i],precM[i])
}
}
'

pM.novus = (1 / (new.datM3$prop*(1-new.datM3$prop)/new.datM3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_M.novus = list(precM = pM.novus, M = new.datM3$prop, xM= rep(NA, length(new.datM3$prop)),npolls = length(new.datM3$prop))
writeLines(jags_M.novus,con="kalman_M_novus.bug")

system.time(jags_mod_M_novus <- jags.model("kalman_M_novus.bug", data = data_M.novus, n.chain=3))
ninter=10000

system.time(e14_outM.novus <- coda.samples(jags_mod_M_novus,variable.names = c("xM"), n.iter = ninter,thin = 5)) #
e14_sumM.novus = summary(e14_outM.novus)
e14_cred_intM.novus = HPDinterval(e14_outM.novus[,which(regexpr("xM", row.names(e14_sumM.novus$statistics))==1)], 0.95)


################# L ###############

orig.date = as.Date(datL$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datL3 = datL[-which(datL$collectPeriodTo>end.date),]
datL3 = datL3[-nrow(datL3 ),]


dateDiff = datL3[,3] - datL3[,2]
dateDiff2 = dateDiff+1
nDay = datL3$n/as.numeric(dateDiff2)
LSmooth = rep(datL3$L,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datL3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datL3)){
  ee[[i]] = seq(datL3[i,2], datL3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datL3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datL2 = data.frame(L = LSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datL2 = datL2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(L*n), sample.size=sum(n), prop=votes/sample.size)

new.datL3 = new.datL2[order(new.datL2$year),]


jags_L.novus ='
model{
#observed model
for(i in 1:npolls){
xL[i] ~ dnorm(L[i],precL[i])
}
}
'

pL.novus = (1 / (new.datL3$prop*(1-new.datL3$prop)/new.datL3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_L.novus = list(precL = pL.novus, L = new.datL3$prop, xL= rep(NA, length(new.datL3$prop)),npolls = length(new.datL3$prop))
writeLines(jags_L.novus,con="kalman_L_novus.bug")

system.time(jags_mod_L_novus <- jags.model("kalman_L_novus.bug", data = data_L.novus, n.chain=3))
ninter=10000

system.time(e14_outL.novus <- coda.samples(jags_mod_L_novus,variable.names = c("xL"), n.iter = ninter,thin = 5)) #
e14_sumL.novus = summary(e14_outL.novus)
e14_cred_intL.novus = HPDinterval(e14_outL.novus[,which(regexpr("xL", row.names(e14_sumL.novus$statistics))==1)], 0.95)


################# KD ###############

orig.date = as.Date(datKD$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datKD3 = datKD[-which(datKD$collectPeriodTo>end.date),]
datKD3 = datKD3[-nrow(datKD3 ),]


dateDiff = datKD3[,3] - datKD3[,2]
dateDiff2 = dateDiff+1
nDay = datKD3$n/as.numeric(dateDiff2)
KDSmooth = rep(datKD3$KD,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datKD3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datKD3)){
  ee[[i]] = seq(datKD3[i,2], datKD3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datKD3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datKD2 = data.frame(KD = KDSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datKD2 = datKD2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(KD*n), sample.size=sum(n), prop=votes/sample.size)

new.datKD3 = new.datKD2[order(new.datKD2$year),]


jags_KD.novus ='
model{
#observed model
for(i in 1:npolls){
xKD[i] ~ dnorm(KD[i],precKD[i])
}
}
'

pKD.novus = (1 / (new.datKD3$prop*(1-new.datKD3$prop)/new.datKD3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_KD.novus = list(precKD = pKD.novus, KD = new.datKD3$prop, xKD= rep(NA, length(new.datKD3$prop)),npolls = length(new.datKD3$prop))
writeLines(jags_KD.novus,con="kalman_KD_novus.bug")

system.time(jags_mod_KD_novus <- jags.model("kalman_KD_novus.bug", data = data_KD.novus, n.chain=3))
ninter=10000

system.time(e14_outKD.novus <- coda.samples(jags_mod_KD_novus,variable.names = c("xKD"), n.iter = ninter,thin = 5)) #
e14_sumKD.novus = summary(e14_outKD.novus)
e14_cred_intKD.novus = HPDinterval(e14_outKD.novus[,which(regexpr("xKD", row.names(e14_sumKD.novus$statistics))==1)], 0.95)


################# C ###############

orig.date = as.Date(datC$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datC3 = datC[-which(datC$collectPeriodTo>end.date),]
datC3 = datC3[-nrow(datC3 ),]


dateDiff = datC3[,3] - datC3[,2]
dateDiff2 = dateDiff+1
nDay = datC3$n/as.numeric(dateDiff2)
CSmooth = rep(datC3$C,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datC3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datC3)){
  ee[[i]] = seq(datC3[i,2], datC3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datC3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datC2 = data.frame(C = CSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datC2 = datC2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(C*n), sample.size=sum(n), prop=votes/sample.size)

new.datC3 = new.datC2[order(new.datC2$year),]


jags_C.novus ='
model{
#observed model
for(i in 1:npolls){
xC[i] ~ dnorm(C[i],precC[i])
}
}
'

pC.novus = (1 / (new.datC3$prop*(1-new.datC3$prop)/new.datC3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_C.novus = list(precC = pC.novus, C = new.datC3$prop, xC= rep(NA, length(new.datC3$prop)),npolls = length(new.datC3$prop))
writeLines(jags_C.novus,con="kalman_C_novus.bug")

system.time(jags_mod_C_novus <- jags.model("kalman_C_novus.bug", data = data_C.novus, n.chain=3))
ninter=10000

system.time(e14_outC.novus <- coda.samples(jags_mod_C_novus,variable.names = c("xC"), n.iter = ninter,thin = 5)) #
e14_sumC.novus = summary(e14_outC.novus)
e14_cred_intC.novus = HPDinterval(e14_outC.novus[,which(regexpr("xC", row.names(e14_sumC.novus$statistics))==1)], 0.95)


################# S ###############

orig.date = as.Date(datS$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datS3 = datS[-which(datS$collectPeriodTo>end.date),]
datS3 = datS3[-nrow(datS3 ),]


dateDiff = datS3[,3] - datS3[,2]
dateDiff2 = dateDiff+1
nDay = datS3$n/as.numeric(dateDiff2)
SSmooth = rep(datS3$S,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datS3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datS3)){
  ee[[i]] = seq(datS3[i,2], datS3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datS3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datS2 = data.frame(S = SSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datS2 = datS2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(S*n), sample.size=sum(n), prop=votes/sample.size)

new.datS3 = new.datS2[order(new.datS2$year),]


jags_S.novus ='
model{
#observed model
for(i in 1:npolls){
xS[i] ~ dnorm(S[i],precS[i])
}
}
'

pS.novus = (1 / (new.datS3$prop*(1-new.datS3$prop)/new.datS3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_S.novus = list(precS = pS.novus, S = new.datS3$prop, xS= rep(NA, length(new.datS3$prop)),npolls = length(new.datS3$prop))
writeLines(jags_S.novus,con="kalman_S_novus.bug")

system.time(jags_mod_S_novus <- jags.model("kalman_S_novus.bug", data = data_S.novus, n.chain=3))
ninter=10000

system.time(e14_outS.novus <- coda.samples(jags_mod_S_novus,variable.names = c("xS"), n.iter = ninter,thin = 5)) #
e14_sumS.novus = summary(e14_outS.novus)
e14_cred_intS.novus = HPDinterval(e14_outS.novus[,which(regexpr("xS", row.names(e14_sumS.novus$statistics))==1)], 0.95)

################# MP ###############

orig.date = as.Date(datMP$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datMP3 = datMP[-which(datMP$collectPeriodTo>end.date),]
datMP3 = datMP3[-nrow(datMP3 ),]


dateDiff = datMP3[,3] - datMP3[,2]
dateDiff2 = dateDiff+1
nDay = datMP3$n/as.numeric(dateDiff2)
MPSmooth = rep(datMP3$MP,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datMP3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datMP3)){
  ee[[i]] = seq(datMP3[i,2], datMP3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datMP3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datMP2 = data.frame(MP = MPSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datMP2 = datMP2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(MP*n), sample.size=sum(n), prop=votes/sample.size)

new.datMP3 = new.datMP2[order(new.datMP2$year),]


jags_MP.novus ='
model{
#observed model
for(i in 1:npolls){
xMP[i] ~ dnorm(MP[i],precMP[i])
}
}
'

pMP.novus = (1 / (new.datMP3$prop*(1-new.datMP3$prop)/new.datMP3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_MP.novus = list(precMP = pMP.novus, MP = new.datMP3$prop, xMP= rep(NA, length(new.datMP3$prop)),npolls = length(new.datMP3$prop))
writeLines(jags_MP.novus,con="kalman_MP_novus.bug")

system.time(jags_mod_MP_novus <- jags.model("kalman_MP_novus.bug", data = data_MP.novus, n.chain=3))
ninter=10000

system.time(e14_outMP.novus <- coda.samples(jags_mod_MP_novus,variable.names = c("xMP"), n.iter = ninter,thin = 5)) #
e14_sumMP.novus = summary(e14_outMP.novus)
e14_cred_intMP.novus = HPDinterval(e14_outMP.novus[,which(regexpr("xMP", row.names(e14_sumMP.novus$statistics))==1)], 0.95)

################# V ###############

orig.date = as.Date(datV$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datV3 = datV[-which(datV$collectPeriodTo>end.date),]
datV3 = datV3[-nrow(datV3 ),]


dateDiff = datV3[,3] - datV3[,2]
dateDiff2 = dateDiff+1
nDay = datV3$n/as.numeric(dateDiff2)
VSmooth = rep(datV3$V,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datV3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datV3)){
  ee[[i]] = seq(datV3[i,2], datV3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datV3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datV2 = data.frame(V = VSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datV2 = datV2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(V*n), sample.size=sum(n), prop=votes/sample.size)

new.datV3 = new.datV2[order(new.datV2$year),]


jags_V.novus ='
model{
#observed model
for(i in 1:npolls){
xV[i] ~ dnorm(V[i],precV[i])
}
}
'

pV.novus = (1 / (new.datV3$prop*(1-new.datV3$prop)/new.datV3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_V.novus = list(precV = pV.novus, V = new.datV3$prop, xV= rep(NA, length(new.datV3$prop)),npolls = length(new.datV3$prop))
writeLines(jags_V.novus,con="kalman_V_novus.bug")

system.time(jags_mod_V_novus <- jags.model("kalman_V_novus.bug", data = data_V.novus, n.chain=3))
ninter=10000

system.time(e14_outV.novus <- coda.samples(jags_mod_V_novus,variable.names = c("xV"), n.iter = ninter,thin = 5)) #
e14_sumV.novus = summary(e14_outV.novus)
e14_cred_intV.novus = HPDinterval(e14_outV.novus[,which(regexpr("xV", row.names(e14_sumV.novus$statistics))==1)], 0.95)

################# SD ###############

orig.date = as.Date(datSD$collectPeriodFrom[1]-1)
end.date = as.Date(elec$Date[4])
datSD3 = datSD[-which(datSD$collectPeriodTo>end.date),]
datSD3 = datSD3[-nrow(datSD3 ),]


dateDiff = datSD3[,3] - datSD3[,2]
dateDiff2 = dateDiff+1
nDay = datSD3$n/as.numeric(dateDiff2)
SDSmooth = rep(datSD3$SD,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datSD3$house, dateDiff2)

ee = list()
for(i in 1:nrow(datSD3)){
  ee[[i]] = seq(datSD3[i,2], datSD3[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datSD3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datSD2 = data.frame(SD = SDSmooth, fieldDate=dateSmooth, fieldDate.num = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.datSD2 = datSD2 %>%
  group_by(month, year) %>%
  summarise(votes = sum(SD*n), sample.size=sum(n), prop=votes/sample.size)

new.datSD3 = new.datSD2[order(new.datSD2$year),]


jags_SD.novus ='
model{
#observed model
for(i in 1:npolls){
xSD[i] ~ dnorm(SD[i],precSD[i])
}
}
'

pSD.novus = (1 / (new.datSD3$prop*(1-new.datSD3$prop)/new.datSD3$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_SD.novus = list(precSD = pSD.novus, SD = new.datSD3$prop, xSD= rep(NA, length(new.datSD3$prop)),npolls = length(new.datSD3$prop))
writeLines(jags_SD.novus,con="kalman_SD_novus.bug")

system.time(jags_mod_SD_novus <- jags.model("kalman_SD_novus.bug", data = data_SD.novus, n.chain=3))
ninter=10000

system.time(e14_outSD.novus <- coda.samples(jags_mod_SD_novus,variable.names = c("xSD"), n.iter = ninter,thin = 5)) #
e14_sumSD.novus = summary(e14_outSD.novus)
e14_cred_intSD.novus = HPDinterval(e14_outSD.novus[,which(regexpr("xSD", row.names(e14_sumSD.novus$statistics))==1)], 0.95)


###############################################
sumM = e14_sumM.novus;outM = e14_outM.novus;sumL = e14_sumL.novus;outL = e14_outL.novus;sumKD = e14_sumKD.novus
outKD = e14_outKD.novus;sumC = e14_sumC.novus;outC = e14_outC.novus;sumS = e14_sumS.novus;outS = e14_outS.novus
sumMP = e14_sumMP.novus;outMP = e14_outMP.novus;sumV = e14_sumV.novus;outV = e14_outV.novus;sumSD = e14_sumSD.novus;outSD = e14_outSD.novus

meanM = sumM$statistics[which(regexpr("xM", row.names(sumM$statistics))==1),1];meanL = sumL$statistics[which(regexpr("xL", row.names(sumL$statistics))==1),1]
meanKD = sumKD$statistics[which(regexpr("xKD", row.names(sumKD$statistics))==1),1];meanC = sumC$statistics[which(regexpr("xC", row.names(sumC$statistics))==1),1]
meanS = sumS$statistics[which(regexpr("xS", row.names(sumS$statistics))==1),1];meanMP = sumMP$statistics[which(regexpr("xMP", row.names(sumMP$statistics))==1),1]
meanV = sumV$statistics[which(regexpr("xV", row.names(sumV$statistics))==1),1];meanSD = sumSD$statistics[which(regexpr("xSD", row.names(sumSD$statistics))==1),1]

meanM[length(meanM)]; e14_cred_intM.novus[[1]][nrow(e14_cred_intM.novus[[1]]),];meanL[length(meanL)]; e14_cred_intL.novus[[1]][nrow(e14_cred_intL.novus[[1]]),]
meanKD[length(meanKD)]; e14_cred_intKD.novus[[1]][nrow(e14_cred_intKD.novus[[1]]),];meanC[length(meanC)]; e14_cred_intC.novus[[1]][nrow(e14_cred_intC.novus[[1]]),]
meanS[length(meanS)]; e14_cred_intS.novus[[1]][nrow(e14_cred_intS.novus[[1]]),];meanMP[length(meanMP)]; e14_cred_intMP.novus[[1]][nrow(e14_cred_intMP.novus[[1]]),]
meanV[length(meanV)]; e14_cred_intV.novus[[1]][nrow(e14_cred_intV.novus[[1]]),];meanSD[length(meanSD)]; e14_cred_intSD.novus[[1]][nrow(e14_cred_intSD.novus[[1]]),]

meanM[length(meanM)]-elec[4,1];meanL[length(meanL)]-elec[4,2];meanKD[length(meanKD)]-elec[4,3]
meanC[length(meanC)]-elec[4,4];meanS[length(meanS)]-elec[4,5];meanMP[length(meanMP)]-elec[4,6]
meanV[length(meanV)]-elec[4,7];meanSD[length(meanSD)]-elec[4,8]
