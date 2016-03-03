#### compare with and without house effects ####
####        to find evaluation methods      ####
################################################

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
phiM ~ dgamma(800000, 1) ## hyperparameters in gamma affects the smoothness of the curve
}
'
pM = (1 / (datM$M*(1-datM$M)/datM$n)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_M = list(M = datM$M, precM = pM, xM = c(0.2623,rep(NA,end.date - orig.date-1)),
              day = datM$fieldDate.num, npolls = nrow(datM), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_M,con="kalman_M.bug")

system.time(jags_mod_M <- jags.model("kalman_M.bug", data = data_M))

system.time(outM <- coda.samples(jags_mod_M,variable.names = c("xM", "M"), n.iter = 3000, n.thin = 1))
#system.time(outM_jags <- jags.samples(jags_mod_M,variable.names = c("xM", "M","phiM" ), n.iter = 5000, n.thin = 10))
sumM = summary(outM)
cred_intM = HPDinterval(outM[,which(regexpr("xM", row.names(sumM$statistics))==1)], 0.95)
#sumM$quantiles[elec.day[3],c(1,5)] 
#################### house #################################
jags_M_house ='
model{

for(i in 1:npolls){
M[i] ~ dnorm(xM[day[i]]+houseM[org[i]], precM[i])
}

for(i in 2:nperiods){
xM[i] ~ dnorm(xM[i-1],phiM)
}

## sum-to-zero constraint on house effects
#houseM[1] <- -sum(houseM[2:nhouses])

## priors
phiM ~ dgamma(800000,1)

for(i in 1:nhouses){
houseM[i] ~ dgamma(1,1)
} 
}
'
data_M_house = list(M = datM$M, precM = pM, xM = c(0.2623,rep(NA,end.date - orig.date-1)), #,0.2333
              day = datM$fieldDate.num, npolls = nrow(datM), nperiods = as.numeric(end.date - orig.date),
              nhouses = length(levels(as.factor(datM$house))), org=as.numeric(as.factor(datM$house)))
writeLines(jags_M_house,con="kalman_M_house.bug")
system.time(jags_mod_M_house <- jags.model("kalman_M_house.bug", data = data_M_house))
system.time(outM_coda_house <- coda.samples(jags_mod_M_house,variable.names = c("xM", "M", "houseM"), n.iter = 3000, n.thin = 1))
#system.time(outM_jags_house <- jags.samples(jags_mod_M_house,variable.names = c("xM", "M", "houseM" ,"phiM"), n.iter = 5000, n.thin = 10))
sumM_coda_house = summary(outM_coda_house)

heM = rbind(sumM_coda_house$statistics[which(regexpr("house", row.names(sumM_coda_house$statistics))==1),1])
colnames(heM) = levels(datM$house)
heM


cred_intM_house = HPDinterval(outM_coda_house[,which(regexpr("xM", row.names(sumM_coda_house$statistics))==1)], 0.95)
meanM_house = sumM_coda_house$statistics[which(regexpr("xM", row.names(sumM_coda_house$statistics))==1),1]


#################### house 2 #################################
jags_M_house2 ='
model{

for(i in 1:npolls){
M[i] ~ dnorm(xM[day[i]], houseM[org[i]]*precM[i])
}

for(i in 2:nperiods){
xM[i] ~ dnorm(xM[i-1],phiM)
}

## sum-to-zero constraint on house effects
#houseM[1] <- -sum(houseM[2:nhouses])

## priors
phiM ~ dgamma(800000,1)

for(i in 1:nhouses){
houseM[i] ~ dgamma(1,1)
} 
}
'

data_M_house = list(M = datM$M, precM = pM, xM = c(0.2623,rep(NA,end.date - orig.date-1)), #,0.2333
                    day = datM$fieldDate.num, npolls = nrow(datM), nperiods = as.numeric(end.date - orig.date),
                    nhouses = length(levels(as.factor(datM$house))), org=as.numeric(as.factor(datM$house)))
writeLines(jags_M_house2,con="kalman_M_house2.bug")
system.time(jags_mod_M_house2 <- jags.model("kalman_M_house2.bug", data = data_M_house))
system.time(outM_coda_house2 <- coda.samples(jags_mod_M_house2,variable.names = c("xM", "M","houseM"), n.iter = 3000, n.thin = 1))
#system.time(outM_jags_house <- jags.samples(jags_mod_M_house,variable.names = c("xM", "M", "houseM" ,"phiM"), n.iter = 5000, n.thin = 10))
sumM_coda_house2 = summary(outM_coda_house2)

sumM_coda_house2$statistic[which(regexpr("house", row.names(sumM_coda_house2$statistics))==1),]
heM = rbind(sumM_coda_house$statistics[which(regexpr("house", row.names(sumM_coda_house2$statistics))==1),1])
colnames(heM) = levels(datM$house)
heM

cred_intM_house2 = HPDinterval(outM_coda_house2[,which(regexpr("xM", row.names(sumM_coda_house2$statistics))==1)], 0.95)
meanM_house2 = sumM_coda_house2$statistics[which(regexpr("xM", row.names(sumM_coda_house2$statistics))==1),1]

plot(meanM_house2, type="l", ylim=c(0.2,0.35))
lines(meanM_house, type="l")
lines(meanM, type="l")


############# election results vs estimation - HPD intervals ###############
elec = data.frame(rbind(c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                        c(0.2333,0.0542,0.0457,0.0611,0.3101,0.0689,0.0572,0.1286)))
colnames(elec) = c("M","L","KD","C","S","MP","V","SD")
elec.date = c(as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14'))
elec.day = julian(elec.date, origin=orig.date)
elec

## 2010 ##
elec[2,1]
HPDinterval(outM[,elec.day[2]], 0.95)
HPDinterval(outM[,elec.day[2]], 0.95)[[1]][2]-HPDinterval(outM[,elec.day[2]], 0.95)[[1]][1]

HPDinterval(outM_coda_house[,elec.day[2]], 0.95) 
HPDinterval(outM_coda_house[,elec.day[2]], 0.95)[[1]][2]-HPDinterval(outM_coda_house[,elec.day[2]], 0.95)[[1]][1]
# both underestimates, shoudl add house effects on house model but can't

## 2014 ##
elec[3,1]
HPDinterval(outM[,elec.day[3]], 0.95)
HPDinterval(outM[,elec.day[3]], 0.95)[[1]][2]-HPDinterval(outM[,elec.day[3]], 0.95)[[1]][1]

HPDinterval(outM_coda_house[,elec.day[3]]+, 0.95) 
HPDinterval(outM_coda_house[,elec.day[3]], 0.95)[[1]][2]-HPDinterval(outM_coda_house[,elec.day[3]], 0.95)[[1]][1]
# both overestimates, non-house model wider interval

outM_coda_house[,elec.day[3]] +
###### resiudals - posterior distribution #####

sumM$statistics[760:nrow(sumM$statistics),1][datM$fieldDate.num]
sumM_coda_house$statistics[760:nrow(sumM_coda_house$statistics),1][datM$fieldDate.num]
heM
heM2 = ifelse(datM$house=="Demoskop",heM[1],datM$house)
for(i in 2:11){
  heM2 = ifelse(heM2==i,heM[i],heM2)
}

res = datM$M-sumM$statistics[760:nrow(sumM$statistics),1][datM$fieldDate.num]
res_house = datM$M-(sumM_coda_house$statistics[760:nrow(sumM_coda_house$statistics),1][datM$fieldDate.num]+heM2)


plot(density(res), main="Residuals between estimated vote intention and polls results", lwd=2, ylim=c(0,35), cex.main=0.9, las=1)
lines(density(res_house), lty=2, lwd=2)
legend(y=32, x=0.02, lty=c(1,2), legend=c("basic","basic+house"), lwd=c(2,2))

####### y^rep #####

outM[1,760]

yrep = sapply(1:nrow(datM), function(s) rnorm(1000,meanM[datM$fieldDate.num][s], 1/pM[s] ))
yrep_house = sapply(1:nrow(datM), function(s) rnorm(1000,(meanM_house[datM$fieldDate.num][s]+heM2[s]), 1/pM[s] ))

####### y^rep min ##### 
min_rep = apply(yrep,2,min)
min_rep_house = apply(yrep_house,2,min)
min_y = min(datM$M)
hist(min_rep, main="Histogram of minimum repblicated party proportion")
hist(min_rep_house, main="Histogram of minimum repblicated party proportion")
plot(density(min_rep), ylim=c(0,11))
lines(density(min_rep_house))
abline(v=min_y)

sum(ifelse(min_rep>min_y,1,0))/length(datM$M) 
sum(ifelse(min_rep_house>min_y,1,0))/length(datM$M)

####### y^rep max ##### 
max_rep = apply(yrep,2, max)
max_rep_house = apply(yrep_house,2,max)
max_y = max(datM$M)
plot(density(max_rep), ylim=c(0,11))
lines(density(max_rep_house))
abline(v=max_y)
### without house effects are wider tailed, two modes?

sum(ifelse(max_rep>max_y,1,0))/length(datM$M) 
sum(ifelse(max_rep_house>max_y,1,0))/length(datM$M)

####### y^rep mean vs y plot ######
par(mfrow=c(1,2))
plot(datM$M*100, apply(yrep,2,mean)*100, xlab="Observed y", ylab="Expected y", main="Basic model", las=1, col="blue3")
abline(a=0, b=1)
plot(datM$M*100, apply(yrep_house,2,mean)*100, xlab="Observed y", ylab="Expected y", main="Basic + house model" ,las=1, col="blue3")
abline(a=0, b=1)
par(mfrow=c(1,1))

df2 = data.frame(orig=datM$M, exp = apply(yrep,2,mean))
library(ggplot2)
ggplot(df2) +
  geom_point(aes(x=orig*100, y=exp*100), col="blue3")+
  geom_point(data = data.frame(or=datM$M*100,ex=apply(yrep_house,2,mean)*100), aes(x=or, y=ex, col="lightblue")) +
  ggtitle("Observed vs Expected proportion of vote intention for M") +
  labs(x="Observed y", y="Expected y") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1) + 
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.position="bottom",
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

####### y^rep mean vs y plot ######


################### visual inspection ###################
library(ggplot2)
meanM = sumM$statistics[760:length(sumM$statistics[,1]),1]
low2 = (meanM - 1.96 * sumM$statistics[760:length(sumM$statistics[,1]),2])*100
high2 = (meanM + 1.96 * sumM$statistics[760:length(sumM$statistics[,1]),2])*100
df = data.frame(xM = meanM , low=cred_intM[[1]][,1]*100, high=cred_intM[[1]][,2]*100,
                time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1)))),
                low2=low2, high2=high2)
ggplot(df) +
  aes(x = time, y = xM*100) +
  geom_line(col="blue", alpha=1)  +
  geom_line(data = data.frame(xM = meanM_house , low_house=cred_intM_house[[1]][,1]*100, high_house=cred_intM_house[[1]][,2]*100,
                             time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1))))),
                             col="red", alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="blue") +
  geom_ribbon(data = data.frame(xM = meanM_house , low_house=cred_intM_house[[1]][,1]*100, high_house=cred_intM_house[[1]][,2]*100,
  time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1))))),aes(ymin=low_house, ymax=high_house), alpha=0.3, fill="red") +
  ggtitle(paste("M")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-17'),by='days',
                                   length=length(c(0.2623,rep(NA,end.date - orig.date-1))))[datM$fieldDate.num], 
                             y=datM$M*100, house=datM$house), aes(x=x, y=y), alpha = 1, color="blue", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for M", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())


