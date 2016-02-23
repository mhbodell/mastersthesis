
meanM_rstan = NULL
HPDint_M_rstan = NULL
sdM_rstan = NULL
for(i in 1:as.numeric(end.date - orig.date)){
  meanM_rstan[i] = mean(unlist(fitData_M@sim$samples[[1]][i]))
  sdM_rstan[i] = sd(unlist(fitData_M@sim$samples[[1]][i]))
  HPDint_M_rstan = rbind(HPDint_M_rstan,HPDinterval(mcmc(unlist(fitData_M@sim$samples[[1]][i])), 0.95))
}

colMeans(samples_M$xMi)

library(ggplot2)
str(sumM)
plot(colMeans(samples_M$xMi), type="l")
plot(datM$M)
meanM = colMeans(samples_M$xMi)
#meanM= meanM_rstan
library("coda")
cred_intM = HPDinterval(mcmc(samples_M$xMi), 0.95)
low2 = (meanM - (1.96 * sdM_rstan))*100
high2 = (meanM + (1.96 * sdM_rstan))*100
df = data.frame(xM = meanM , low=cred_intM[,1]*100, high=cred_intM[,2]*100,
                time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1)))))
ggplot(df) +
  aes(x = time, y = xM*100) +
  geom_line(col="blue", alpha=1)  +
  #geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.2, fill="blue3") +
  #geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="blue") +
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

########## L ######

meanL_rstan = NULL
HPDint_L_rstan = NULL
sdL_rstan = NULL
for(i in 1:as.numeric(end.date - orig.date)){
  meanL_rstan[i] = mean(unlist(fitData_L@sim$samples[[1]][i]))
  sdL_rstan[i] = sd(unlist(fitData_L@sim$samples[[1]][i]))
  HPDint_L_rstan = rbind(HPDint_L_rstan,HPDinterval(mcmc(unlist(fitData_L@sim$samples[[1]][i])), 0.95))
}

colMeans(samples_L$xLi)

library(ggplot2)
plot(colMeans(samples_L$xLi), type="l")
plot(datL$L, type="l")
meanL = colMeans(samples_L$xLi)
#meanM= meanM_rstan
library("coda")
cred_intL = HPDinterval(mcmc(samples_L$xLi), 0.95)
low2 = (meanL - (1.96 * sdL_rstan))*100
high2 = (meanL + (1.96 * sdL_rstan))*100
df = data.frame(xL = meanL , low=cred_intL[,1]*100, high=cred_intL[,2]*100,
                time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.0754,rep(NA,end.date - orig.date-1)))))
ggplot(df) +
  aes(x = time, y = xL*100) +
  geom_line(col="lightblue3", alpha=1)  +
  #geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.2, fill="lightblue3") +
  #geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="lightblue3") +
  ggtitle(paste("L")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-17'),by='days',
                                   length=length(c(0.0754,rep(NA,end.date - orig.date-1))))[datL$fieldDate.num], 
                             y=datL$L*100, house=datL$house), aes(x=x, y=y), alpha = 1, color="lightblue3", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for L", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())


########## KD ######


library(ggplot2)
plot(colMeans(samples_KD$xKDi), type="l")
plot(datKD$KD, type="l")
meanKD = colMeans(samples_KD$xKDi)
#meanM= meanM_rstan
cred_intKD = HPDinterval(mcmc(samples_KD$xKDi), 0.95)
df = data.frame(xKD = meanKD , low=cred_intKD[,1]*100, high=cred_intKD[,2]*100,
                time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.0659,rep(NA,end.date - orig.date-1)))))
ggplot(df) +
  aes(x = time, y = xKD*100) +
  geom_line(col="darkblue", alpha=1)  +
  #geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.2, fill="darkblue") +
  #geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="darkblue") +
  ggtitle(paste("KD")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-17'),by='days',
                                   length=length(c(0.0659,rep(NA,end.date - orig.date-1))))[datKD$fieldDate.num], 
                             y=datKD$KD*100, house=datKD$house), aes(x=x, y=y), alpha = 1, color="darkblue", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for KD", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())
