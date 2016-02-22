#### plot M ####
library(ggplot2)
str(sumM)
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
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="blue") +
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
library(ggplot2)
meanL = sumL$statistics[760:length(sumL$statistics[,1]),1]
low2 = (meanL - 1.96 * sumL$statistics[760:length(sumL$statistics[,1]),2])*100
high2 = (meanL + 1.96 * sumL$statistics[760:length(sumL$statistics[,1]),2])*100
dfL = data.frame(xL = meanL , low=cred_intL[[1]][,1]*100, high=cred_intL[[1]][,2]*100,
                time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.0754,rep(NA,end.date - orig.date-1)))),
                low2=low2, high2=high2)
ggplot(dfL) +
  aes(x = time, y = xL*100) +
  geom_line(col="lightblue", alpha=1)  +
  #geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.2, fill="lightblue3") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.4, fill="lightblue") +
  ggtitle(paste("L")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-17'),by='days',
                                   length=length(c(0.0754,rep(NA,end.date - orig.date-1))))[datL$fieldDate.num], 
                             y=datL$L*100, house=datL$house), aes(x=x, y=y), alpha = 1, color="lightblue", shape=1, size=1) +    
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


