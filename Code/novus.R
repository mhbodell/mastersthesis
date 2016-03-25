#################################################
#################### NOVUS ######################
#################################################
library(dplyr)

dateDiff = datM[,3] - datM[,2]
dateDiff2 = dateDiff+1
nDay = datM$n/as.numeric(dateDiff2)
MSmooth = rep(datM$M,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datM$house, dateDiff2)
orig.date = as.Date(datM$collectPeriodFrom[1]-1)
end.date = as.Date(datM$collectPeriodFrom[length(datM$collectPeriodFrom)])

as.numeric(as.factor(months(dateSmooth)))
year(dateSmooth)

ee = list()
for(i in 1:nrow(datM)){
  ee[[i]] = seq(datM[i,2], datM[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datM)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datM2 = data.frame(M = MSmooth, fieldDate=dateSmooth, fieldDate.num = dateSmooth.num, n = nSmooth, house = houseSmooth, year= y, month = m)

new.datM2 = datM2 %>%
            group_by(month, year) %>%
            summarise(votes = sum(M*n), sample.size=sum(n), prop=votes/sample.size)

tt = NULL
for(i in 1:nrow(new.datM2)){
  yr = new.datM2[i,'year']
  mt = new.datM2[i,'month']
  for(p in 1:nrow(datM2)){
  if(datM2[p,'month']==mt && datM2[p,'year']==yr){
   tt[p] = as.numeric(new.datM2[i,'prop'])
  }
  }
}

datM2$monthM = tt


library(rjags, lib="C:/Users/mirhu86/Documents/packages")
jags_M.novus ='
model{
#observed model
for(i in 1:npolls){
M[i] ~ dnorm(xM[i],precM[i])

xM[i]~dunif(0,1)
}

}
'

pM.novus = (1 / (new.datM2$monthM*(1-new.datM2$monthM)/new.datM2$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_M.novus = list( precM = pM.novus, M = new.datM2$prop, npolls = nrow(new.datM2))
writeLines(jags_M.novus,con="kalman_M_novus.bug")

system.time(jags_mod_M_novus <- jags.model("kalman_M_novus.bug", data = data_M.novus, n.chain=3))

ninter=10000

system.time(outM.novus <- coda.samples(jags_mod_M_novus,variable.names = c( "xM"), n.iter = ninter, n.thin = 100))
sumM.novus = summary(outM.novus)
cred_intM.novus = HPDinterval(outM.novus[,which(regexpr("xM", row.names(sumM.novus$statistics))==1)], 0.95)


plot(sumM.novus$statistics[,1]*100, type="l", ylim=c(20,50))

dfM.Novus = data.frame(party = new.datM2$prop , time=seq(as.Date('2006-09-16'),by='months',length=length(seq(to = end.date, from = orig.date, by='month'))+1),
                 party2 = rep("M", length=length(seq(to = end.date, from = orig.date, by='month'))+1), low=cred_intM.novus[[1]][,1]*100, high=cred_intM.novus[[1]][,2]*100)
pointsM.novus = data.frame(x=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date))))[datM$fieldDate.num], 
                     y=datM$M*100, house=datM$house, party=rep("M",length(c(rep(NA,end.date - orig.date))))[datM$fieldDate.num])

library(ggplot2)
ggplot(dfM.Novus) +
  aes(x = time, y = party*100) +
  geom_point(data=pointsM.novus, aes(x=x, y=y), color="blue", alpha = 1, shape=19, size=1.5) +    
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="blue") +
  geom_line(col="blue")+
  labs(x="Date", y=paste("Support for M (%)")) +
  theme_bw() +
  facet_wrap( ~ party2, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())




############## KD ####################
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

new.datL2 = datL2 %>%
  mutate(month = format(fieldDate, "%m"), year = format(fieldDate, "%Y")) %>%
  group_by(month, year) %>%
  summarise(votes = sum(L*n), sample.size=sum(n), prop=votes/sample.size)


jags_L.novus ='
model{
#observed model
for(i in 1:npolls){
L[i] ~ dnorm(xL[i],precL[i])


}

}
'

pL.novus = (1 / (new.datL2$prop*(1-new.datL2$prop)/new.datL2$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_L.novus = list( precL = pL.novus, xL = new.datL2$prop, npolls = nrow(new.datL2))
writeLines(jags_L.novus,con="kalman_L_novus.bug")

system.time(jags_mod_L_novus <- jags.model("kalman_L_novus.bug", data = data_L.novus, n.chain=3))

ninter=10000

system.time(outL.novus <- coda.samples(jags_mod_L_novus,variable.names = c( "L"), n.iter = ninter, n.thin = 100))
sumM.novus = summary(outL.novus)
cred_intM.novus = HPDinterval(outM.novus[,which(regexpr("M", row.names(sumM.novus$statistics))==1)], 0.95)


plot(sumM.novus$statistics[,1]*100, type="l", ylim=c(20,50))

dfM.Novus = data.frame(party = new.datM2$prop , time=seq(as.Date('2006-09-16'),by='months',length=length(seq(to = end.date, from = orig.date, by='month'))+1),
                       party2 = rep("M", length=length(seq(to = end.date, from = orig.date, by='month'))+1), low=cred_intM.novus[[1]][,1]*100, high=cred_intM.novus[[1]][,2]*100)
pointsM.novus = data.frame(x=seq(as.Date('2006-09-16'),by='days',length=length(c(rep(NA,end.date - orig.date))))[datM$fieldDate.num], 
                           y=datM$M*100, house=datM$house, party=rep("M",length(c(rep(NA,end.date - orig.date))))[datM$fieldDate.num])

library(ggplot2)
ggplot(dfM.Novus) +
  aes(x = time, y = party*100) +
  geom_point(data=pointsM.novus, aes(x=x, y=y), color="blue", alpha = 1, shape=19, size=1.5) +    
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="blue") +
  geom_line(col="blue")+
  labs(x="Date", y=paste("Support for M (%)")) +
  theme_bw() +
  facet_wrap( ~ party2, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())
