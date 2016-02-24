#### plot M ####
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

#### plot L ####
library(ggplot2)
meanL = sumL$statistics[760:length(sumL$statistics[,1]),1]
low2 = (meanL - 1.96 * sumL$statistics[760:length(sumL$statistics[,1]),2])*100
high2 = (meanL + 1.96 * sumL$statistics[760:length(sumL$statistics[,1]),2])*100
dfL = data.frame(xL = meanL , low=cred_intL[[1]][,1]*100, high=cred_intL[[1]][,2]*100,
                time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.0754,rep(NA,end.date - orig.date-1)))),
                low2=low2, high2=high2)
ggplot(dfL) +
  aes(x = time, y = xL*100) +
  geom_line(col="lightblue4", alpha=1)  +
  #geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.2, fill="lightblue3") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.4, fill="lightblue3") +
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

#### plot KD ####
library(ggplot2)
meanKD = sumKD$statistics[760:length(sumKD$statistics[,1]),1]
low2 = (meanKD - 1.96 * sumKD$statistics[760:length(sumKD$statistics[,1]),2])*100
high2 = (meanKD + 1.96 * sumKD$statistics[760:length(sumKD$statistics[,1]),2])*100
dfKD = data.frame(xKD = meanKD , low=cred_intKD[[1]][,1]*100, high=cred_intKD[[1]][,2]*100,
                  time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.0754,rep(NA,end.date - orig.date-1)))),
                  low2=low2, high2=high2)
ggplot(dfKD) +
  aes(x = time, y = xKD*100) +
  geom_line(col="darkblue", alpha=1)  +
  #geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.2, fill="darkblue") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.4, fill="darkblue") +
  ggtitle(paste("KD")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-17'),by='days',
                                   length=length(c(0.0754,rep(NA,end.date - orig.date-1))))[datKD$fieldDate.num], 
                             y=datKD$KD*100, house=datKD$house), aes(x=x, y=y), alpha = 1, color="darkblue", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for KD", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "darkgrey"),
        panel.grid.minor = element_blank())

#### plot C ####
library(ggplot2)
meanC = sumC$statistics[760:length(sumC$statistics[,1]),1]
low2 = (meanC - 1.96 * sumC$statistics[760:length(sumC$statistics[,1]),2])*100
high2 = (meanC + 1.96 * sumC$statistics[760:length(sumC$statistics[,1]),2])*100
dfC = data.frame(xC = meanC , low=cred_intC[[1]][,1]*100, high=cred_intC[[1]][,2]*100,
                 time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.0754,rep(NA,end.date - orig.date-1)))),
                 low2=low2, high2=high2)
ggplot(dfC) +
  aes(x = time, y = xC*100) +
  geom_line(col="chartreuse3", alpha=1)  +
  #geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.2, fill="darkgreen") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.3, fill="chartreuse3") +
  ggtitle(paste("C")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-17'),by='days',
                                   length=length(c(0.0788,rep(NA,end.date - orig.date-1))))[datC$fieldDate.num], 
                             y=datC$C*100, house=datC$house), aes(x=x, y=y), alpha = 1, color="chartreuse3", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for C", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "darkgrey"),
        panel.grid.minor = element_blank())

#### plot S ####
library(ggplot2)
meanS = sumS$statistics[760:length(sumS$statistics[,1]),1]
low2 = (meanS - 1.96 * sumS$statistics[760:length(sumS$statistics[,1]),2])*100
high2 = (meanS + 1.96 * sumS$statistics[760:length(sumS$statistics[,1]),2])*100
dfS = data.frame(xS = meanS , low=cred_intS[[1]][,1]*100, high=cred_intS[[1]][,2]*100,
                 time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.0754,rep(NA,end.date - orig.date-1)))),
                 low2=low2, high2=high2)
ggplot(dfS) +
  aes(x = time, y = xS*100) +
  geom_line(col="red", alpha=1)  +
  #geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.2, fill="red") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.4, fill="red") +
  ggtitle(paste("S")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-17'),by='days',
                                   length=length(c(0.3499,rep(NA,end.date - orig.date-1))))[datS$fieldDate.num], 
                             y=datS$S*100, house=datS$house), aes(x=x, y=y), alpha = 1, color="red", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for S", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "darkgrey"),
        panel.grid.minor = element_blank())


##### plot MP #####
library(ggplot2)
meanMP = sumMP$statistics[760:length(sumMP$statistics[,1]),1]
low2 = (meanMP - 1.96 * sumMP$statistics[760:length(sumMP$statistics[,1]),2])*100
high2 = (meanMP + 1.96 * sumMP$statistics[760:length(sumMP$statistics[,1]),2])*100
dfMP = data.frame(xMP = meanMP , low=cred_intMP[[1]][,1]*100, high=cred_intMP[[1]][,2]*100,
                  time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.0754,rep(NA,end.date - orig.date-1)))),
                  low2=low2, high2=high2)
ggplot(dfMP) +
  aes(x = time, y = xMP*100) +
  geom_line(col="forestgreen", alpha=1)  +
  #geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.2, fill="forestgreen") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.4, fill="forestgreen") +
  ggtitle(paste("MP")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-17'),by='days',
                                   length=length(c(0.0524,rep(NA,end.date - orig.date-1))))[datMP$fieldDate.num], 
                             y=datMP$MP*100, house=datMP$house), aes(x=x, y=y), alpha = 1, color="forestgreen", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for MP", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "darkgrey"),
        panel.grid.minor = element_blank())

##### plot V #####
library(ggplot2)
meanV = sumV$statistics[760:length(sumV$statistics[,1]),1]
low2 = (meanV - 1.96 * sumV$statistics[760:length(sumV$statistics[,1]),2])*100
high2 = (meanV + 1.96 * sumV$statistics[760:length(sumV$statistics[,1]),2])*100
dfV = data.frame(xV = meanV , low=cred_intV[[1]][,1]*100, high=cred_intV[[1]][,2]*100,
                 time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.0754,rep(NA,end.date - orig.date-1)))),
                 low2=low2, high2=high2)
ggplot(dfV) +
  aes(x = time, y = xV*100) +
  geom_line(col="darkred", alpha=1)  +
  #geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.2, fill="darkred") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.4, fill="darkred") +
  ggtitle(paste("V")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-17'),by='days',
                                   length=length(c(0.0585,rep(NA,end.date - orig.date-1))))[datV$fieldDate.num], 
                             y=datV$V*100, house=datV$house), aes(x=x, y=y), alpha = 1, color="darkred", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for V", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "darkgrey"),
        panel.grid.minor = element_blank())

#### plot SD ####
library(ggplot2)
meanSD = sumSD$statistics[751:length(sumSD$statistics[,1]),1]
low2 = (meanSD - 1.96 * sumSD$statistics[751:length(sumSD$statistics[,1]),2])*100
high2 = (meanSD + 1.96 * sumSD$statistics[751:length(sumSD$statistics[,1]),2])*100
dfSD = data.frame(xSD = meanSD , low=cred_intSD[[1]][,1]*100, high=cred_intSD[[1]][,2]*100,
                  time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.0293,rep(NA,end.date - orig.date-1)))),
                  low2=low2, high2=high2)
ggplot(dfSD) +
  aes(x = time, y = xSD*100) +
  geom_line(col="skyblue3", alpha=1)  +
  #geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.2, fill="skyblue3") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.4, fill="skyblue3") +
  ggtitle(paste("SD")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-17'),by='days',
                                   length=length(c(0.0293,rep(NA,end.date - orig.date-1))))[datSD$fieldDate.num], 
                             y=datSD$SD*100, house=datSD$house), aes(x=x, y=y), alpha = 1, color="skyblue3", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for SD", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "darkgrey"),
        panel.grid.minor = element_blank())




