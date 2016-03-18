#########################################################
################# model evaluation ######################
#########################################################
meanM = sumM$statistics[which(regexpr("xM", row.names(sumM$statistics))==1),1]
meanM2 = sumM2$statistics[which(regexpr("xM", row.names(sumM2$statistics))==1),1]

set.seed(901207)
i = sample(1:3,1)
j = sample(1:nrow(outM[[i]]),1)
j2 = sample(1:nrow(outM2[[i]]),1)
rsimM = outM[[i]][j,]
rsimM2 = outM2[[i]][j2,]


rsimM.2 = rsimM[which(regexpr("xM", names(rsimM))==1)]
rsimM2.2 = rsimM2[which(regexpr("xM", names(rsimM2))==1)]


####### y^rep #####
yrepM = sapply(1:nrow(datM), function(s) rnorm(10000,rsimM.2[datM$fieldDate.num][s], 1/pM[s] ))
yrepM2 = sapply(1:nrow(datM2), function(s) rnorm(10000,rsimM2.2[datM2$fieldDate.num][s], 1/pM2[s]))


##
hist(datM$M*100)
hist(yrepM[1,]*100)
hist(yrepM2[1,]*100)



####### y^rep min ##### 

par(mfrow=c(1,2))
min_repM = apply(yrepM,1,min)
min_M = min(datM$M)
hist(min_repM, main="M - original", col="blue", xlab="Minimum value in replicated data", las=1)
abline(v=min_M, lty=1, lwd=2)
sum(ifelse(min_repM>=min_M,1,0))/length(min_repM) 

min_repM2 = apply(yrepM2,1,min)
min_M2 = min(datM2$M)
hist(min_repM2, main="M - smooth", col="blue", xlab="Minimum value in replicated data", las=1)
abline(v=min_M2, lty=1, lwd=2)
sum(ifelse(min_repM2>=min_M2,1,0))/length(min_repM2) 

par(mfrow=c(1,1))

####### y^rep max ##### 
par(mfrow=c(1,2))
max_repM = apply(yrepM,1,max)
max_M = max(datM$M)
hist(max_repM, main="M - original", col="blue", xlab="Maximum value in replicated data", las=1)
abline(v=max_M, lty=1, lwd=2)
sum(ifelse(max_repM>=max_M,1,0))/length(max_repM) 

max_repM2 = apply(yrepM2,1,max)
max_M2 = max(datM2$M)
hist(max_repM2, main="M - smooth ", col="blue", xlab="Maximum value in replicated data", las=1)
abline(v=max_M2, lty=1, lwd=2)
sum(ifelse(max_repM2>=max_M2,1,0))/length(max_repM2) 
par(mfrow=c(1,1))

####### y^rep mean #########

par(mfrow=c(1,2))
mean_repM = apply(yrepM,1,mean)
mean_M = mean(datM$M)
hist(mean_repM, main="M - original", col="blue", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_M, lty=1, lwd=2)
sum(ifelse(mean_repM>=mean_M,1,0))/length(mean_repM) 

mean_repM2 = apply(yrepM2,1,mean)
mean_M2 = mean(datM2$M)
hist(mean_repM2, main="M - smooth", col="blue", xlab="Mean value of observation in replicated data", las=1)
abline(v=mean_M2, lty=1, lwd=2)
sum(ifelse(mean_repM2>=mean_M2,1,0))/length(mean_repM2) 

par(mfrow=c(1,1))

########################## gg-plot ####################
library(ggplot2)

low = (meanM - 1.96 * sumM$statistics[which(regexpr("xM", row.names(sumM$statistics))==1),2])*100
high = (meanM + 1.96 * sumM$statistics[which(regexpr("xM", row.names(sumM$statistics))==1),2])*100
df = data.frame(xM = meanM , low=cred_intM[[1]][,1]*100, high=cred_intM[[1]][,2]*100,
                time=seq(as.Date('2006-09-16'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1)))),
                low2=low, high2=high)
ggplot(df) +
  aes(x = time, y = xM*100) +
  geom_line(col="blue", alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="blue") + 
  ggtitle(paste("M")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-16'),by='days',
                                   length=length(c(0.2623,rep(NA,end.date - orig.date-1))))[datM$fieldDate.num], 
                             y=datM$M*100, house=datM$house), aes(x=x, y=y), alpha = 1, color="blue", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for M", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())


low2 = (meanM2 - 1.96 * sumM2$statistics[which(regexpr("xM", row.names(sumM2$statistics))==1),2])*100
high2 = (meanM2 + 1.96 * sumM2$statistics[which(regexpr("xM", row.names(sumM2$statistics))==1),2])*100
df2 = data.frame(xM = meanM2 , low=cred_intM2[[1]][,1]*100, high=cred_intM2[[1]][,2]*100,
                time=seq(as.Date('2006-09-16'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1)))),
                low2=low2, high2=high2)
ggplot(df2) +
  aes(x = time, y = xM*100) +
  geom_line(col="blue", alpha=1)  +
  geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.2, fill="blue") + 
  ggtitle(paste("M")) +
  geom_point(data=data.frame(x=seq(as.Date('2006-09-16'),by='days',
                                   length=length(c(0.2623,rep(NA,end.date - orig.date-1))))[datM$fieldDate.num], 
                             y=datM$M*100, house=datM$house), aes(x=x, y=y), alpha = 1, color="blue", shape=1, size=1) +    
  labs(x="Date", y=paste("Support for M", "(%)")) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

