### model using only election results ###

elec = data.frame(rbind(c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
             c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
             c(0.2333,0.0542,0.0457,0.0611,0.3101,0.0689,0.0572,0.1286)))
colnames(elec) = c("M","L","KD","C","S","MP","V","SD")
row.names(elec) = c("2006","2010","2012")
elec$Date = c(as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14'))
orig.date = as.Date('2006-09-16') #day before election
end.date = as.Date('2014-09-15') #day after election
elec$Day = julian(elec$Date, origin=orig.date)
elec$n = rep(6000000,nrow(elec))

library(rjags)

jags_M ='
model{

#observed model
for(i in 1:npolls){
M[i] ~ dnorm(xM[day[i]],precM[i])
}

#dynamic model 
for(i in 2:nperiods-1){
xM[i] ~ dnorm(xM[i-1],phiM)
}

## priors
omega ~ dgamma(1, 10000000) ## hyperparameters in gamma affects the smoothness of the curve
phiM <- 1/pow(omega,2)
}
'

pM = (1 / (elec$M*(1-elec$M)/elec$n))
data_M = list(M = elec$M, precM = pM, xM = c(0.262,rep(NA,elec$Day[2]-1),0.3006,rep(NA,elec$Day[3]-elec$Day[2]-1),0.2333),
              day = elec$Day, npolls = nrow(elec), nperiods = as.numeric(end.date - orig.date))
writeLines(jags_M,con="kalman_M2.bug")

system.time(jags_mod_M <- jags.model("kalman_M2.bug", data = data_M))

system.time(outM <- coda.samples(jags_mod_M,variable.names = c("xM", "M" ), n.iter = 2000, n.thin = 100))
sumM = summary(outM)
cred_intM = HPDinterval(outM[,4:length(sumM$statistics[,1])], 0.95)
meanM = sumM$statistics[4:length(sumM$statistics[,1]),1]

df = data.frame(xM = meanM , low=cred_intM[[1]][,1]*100, high=cred_intM[[1]][,2]*100,
                time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.262,rep(NA,elec$Day[2]-1),0.3006,rep(NA,elec$Day[3]-elec$Day[2]-1),0.2333))))
ggplot(df) +
  aes(x = time, y = xM*100) +
  geom_line(col="blue", alpha=1)  +
  #geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.2, fill="blue3") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.3, fill="blue") +
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




