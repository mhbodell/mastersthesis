x = s = NULL
x[1] = 0.5
for (i in 2:100){
  x[i] <- rnorm(1,x[i-1], 0.02)
}
s
x

period1 = c(12,43)
n = 1000
nperiod = sample(12:43, 1000, TRUE)
y1 = NULL
for (i in 12:43){
  ind = which(nperiod==i)
  y1[i] = rbinom(1,length(ind), x[i])
}
poll1 = sum(na.omit(y1)) / n

period2 = c(8,14)
n = 1000
y2 = NULL
nperiod = sample(8:14, 1000, TRUE)
for (i in 8:14){
  ind = which(nperiod==i)
  y2[i] = rbinom(1,length(ind), x[i])
}
poll2 = sum(na.omit(y2)) / n

period3 = c(30,37)
n = 1000
y3 = NULL
nperiod = sample(30:37, 1000, TRUE)
for (i in 30:37){
  y3[i] = rbinom(1,nperiod[i], x[i])
}
poll3 = sum(na.omit(y3)) / n


period4 = c(60,65)
n = 1000
y4 = NULL
nperiod = sample(60:65, 1000, TRUE)
for (i in 60:65){
  ind = which(nperiod==i)
  y4[i] = rbinom(1,length(ind), x[i])
}
poll4 = sum(na.omit(y4)) / n

period5 = c(64,75)
n = 1000
y5 = NULL
nperiod = sample(64:75, 1000, TRUE)
for (i in 64:75){
  ind = which(nperiod==i)
  y5[i] = rbinom(1,length(ind), x[i])
}
poll5 = sum(na.omit(y5)) / n

period6 = c(72,81)
n = 1000
y6 = NULL
nperiod = sample(72:81, 1000, TRUE)
for (i in 72:81){
  ind = which(nperiod==i)
  y6[i] = rbinom(1,length(ind), x[i])
}
poll6 = sum(na.omit(y6)) / n

period7 = c(5,14)
n = 1000
y7 = NULL
nperiod = sample(5:14, 1000, TRUE)
for (i in 5:14){
  ind = which(nperiod==i)
  y7[i] = rbinom(1,length(ind), x[i])
}
poll7 = sum(na.omit(y7)) / n


period8 = c(84,90)
n = 1000
y8 = NULL
nperiod = sample(84:90, 1000, TRUE)
for (i in 84:90){
  ind = which(nperiod==i)
  y8[i] = rbinom(1,length(ind), x[i])
}
poll8 = sum(na.omit(y8)) / n

period9 = c(23,30)
n = 1000
y9 = NULL
nperiod = sample(23:30, 1000, TRUE)
for (i in 23:30){
  ind = which(nperiod==i)
  y9[i] = rbinom(1,length(ind), x[i])
}
poll9 = sum(na.omit(y9)) / n


period10 = c(57,64)
n = 1000
y10 = NULL
nperiod = sample(57:64, 1000, TRUE)
for (i in 57:64){
  ind = which(nperiod==i)
  y10[i] = rbinom(1,length(ind), x[i])
}
poll10 = sum(na.omit(y10)) / n

period11 = c(90,100)
n = 1000
y11 = NULL
nperiod = sample(90:100, 1000, TRUE)
for (i in 90:100){
  ind = which(nperiod==i)
  y11[i] = rbinom(1,length(ind), x[i])
}
poll11 = sum(na.omit(y11)) / n

period12 = c(50,60)
n = 1000
y12 = NULL
nperiod = sample(50:60, 1000, TRUE)
for (i in 50:60){
  ind = which(nperiod==i)
  y12[i] = rbinom(1,length(ind), x[i])
}
poll12 = sum(na.omit(y12)) / n


poll.dat = data.frame(y=c(poll1,poll2,poll3,poll4,poll5,poll6,poll7,poll8,poll9,poll10, poll11, poll12))
st = c(period1[1],period2[1],period3[1],period4[1],period5[1],period6[1],period7[1],period8[1],period9[1],period10[1],period11[1],period12[1])
en = c(period1[2],period2[2],period3[2],period4[2],period5[2],period6[2],period7[2],period8[2],period9[2],period10[2],period11[2],period12[2])
org = as.Date("2015-01-01")
da =seq(org, by="days", length=100)
poll.dat$startDate = as.Date(da[st])
poll.dat$endDate = as.Date(da[en])

#### data pre-processing 1 ####
pd = poll.dat[order(poll.dat$startDate),]
orig.date = org
end.date = pd$endDate[length(pd$endDate)]
dayssinceorigStart = julian(pd$startDate, origin=orig.date) 
dayssinceorigEnd = julian(pd$endDate, origin=orig.date) 
pd$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
pd$n = rep(1000, 12)
prec = 1 / (pd$y*(1-pd$y)/pd$n)
prec

#### data pre-processing 2 ####


dateDiff = pd[,'endDate'] -pd[,'startDate']
dateDiff2 = dateDiff+1
nDay = pd$n/as.numeric(dateDiff2)
pSmooth = rep(pd$y,dateDiff2)
nSmooth = rep(nDay,dateDiff2)

ee = list()
for(i in 1:nrow(pd)){
  ee[[i]] = seq(pd[i,'startDate'], pd[i,'endDate'], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(pd)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
pd2 = data.frame(y = pSmooth,Date = dateSmooth.num, n = nSmooth)
head(pd2)
prec2 = 1 / (pd2$y*(1-pd2$y)/pd2$n)

####
library(rjags)
jags_dlm ='
model{
#observed model
for(i in 1:npolls){
y[i] ~ dnorm(x[day[i]],prec[i])
}
#dynamic model
for(i in 2:nperiods){
x[i] ~ dnorm(x[i-1],1/eps)
}
## priors
init.m ~ dbeta(1,1) 
init.v ~ dgamma(0.01,0.01)
x[1] ~ dnorm(init.m, init.v)
eps ~ dgamma(1,1)

}
'

ns = list(y = pd$y, prec = prec , npolls=nrow(pd), nperiods=100, day=pd$Date)
writeLines(jags_dlm,con="jags_dlm.bug")
system.time(jags_ns <- jags.model("jags_dlm.bug", data = ns, n.chain=3))
ninter=100000
system.time(out_ns <- coda.samples(jags_ns,variable.names = c("x", "eps"), n.iter = ninter, thin = 5))
sum_ns = summary(out_ns)
out_ns = out_ns[,which(regexpr("x", row.names(sum_ns$statistics))==1)]
sum_ns_x = sum_ns$statistics[which(regexpr("x", row.names(sum_ns$statistics))==1),]
out_ens = out_ns[,which(regexpr("eps", row.names(sum_ns$statistics))==1)]
plot(out_ens)

smo = list(y = pd2$y, prec = prec2 , npolls=nrow(pd2), nperiods=100, day=pd2$Date)
writeLines(jags_dlm,con="jags_dlm.bug")
system.time(jags_smo <- jags.model("jags_dlm.bug", data = smo, n.chain=3))
ninter=100000
system.time(out_smo <- coda.samples(jags_smo,variable.names = c("x", "eps"), n.iter = ninter, thin = 5))
sum_smo = summary(out_smo)
out_smo = out_smo[,which(regexpr("x", row.names(sum_smo$statistics))==1)]
sum_smo_x = sum_smo$statistics[which(regexpr("x", row.names(sum_smo$statistics))==1),]
out_es = out_smo[,which(regexpr("eps", row.names(sum_smo$statistics))==1)]
plot(out_es)


####

poll.dat$length = as.numeric(poll.dat$endDate-poll.dat$startDate)
poll.dat$n = rep(1000,12)
poll.dat$y2 = poll.dat$y
head(poll.dat)
z=matrix(0,nrow=100,ncol=nrow(poll.dat))

n2=NULL
for(i in 1:nrow(poll.dat)){
  n2[i] <- round(dat_3$n[i]/dat_3$k[i])
  for(j in 1:dat_3$k[i]){
    z[j,i] <- rnorm(1,x[dat_3$day[i]+j],((x[dat_3$day[i]+j]*(1-x[dat_3$day[i]+j]))/n2[i]))
  }
  y[i] <- dnorm(1,(sum(z[1:k[i],i]*n2[i])/poll.dat$n[i]), prec[i])
}

for(i in 2:nperiods){
  x[i] ~ dnorm(x[i-1],1/eps)
}

init.m ~ dbeta(1,1) 
init.v ~ dgamma(0.01,0.01)
x[1] ~ dnorm(init.m, init.v)
eps ~ dgamma(1,1)


jags_dlm2 ='
model{
#observed model

for(i in 1:npolls){
  n2[i] <- round(n[i]/k[i])
for(l in 1:nparties){
  for(j in 1:k[i]){
    z[j,i,l] ~ dnorm(x[day[i]+j,l],1/((x[day[i]+j,l]*(1-x[day[i]+j,l]))/n2[i]))
  }
  y[i,l] ~ dnorm((sum(z[1:k[i],i,l]*n2[i])/n[i]), prec[i,l])
}
}

#dynamic model
for(j in 1:nparties){
for(i in 2:nperiods){
x[i,j] ~ dnorm(x[i-1,j],1/eps[j])
}
}

for(i in 1:nparties){
init.m[i] ~ dbeta(1,1)
init.v[i] ~ dgamma(0.01,0.01)
x[1,i] ~ dnorm(init.m[i],init.v[i])
eps[i] ~ dgamma(1,1)
}

}
'

head(poll.dat)
head(pd2)
poll.dat  = poll.dat[order(poll.dat$startDate),]
y=as.matrix(poll.dat[,c('y','y2')])
prec3 = matrix(NA, ncol=2, nrow=nrow(y))
prec3[,1] = 1 / (y[,1]*(1-y[,1])/pd$n)
prec3[,2] = 1 / (y[,2]*(1-y[,2])/pd$n)
dat_3 = list(y = y,  npolls=nrow(poll.dat), nparties=2,prec=prec3, nperiods=100, day=which(da%in%poll.dat$startDate)-1,
          n=poll.dat$n, k=poll.dat$length ,z=array(NA,dim=c(100,nrow(y),ncol(y))))

writeLines(jags_dlm2,con="jags_dlm.bug2")
system.time(jags_3 <- jags.model("jags_dlm.bug2", data = dat_3, n.chain=3))
ninter=10000
system.time(out_3 <- coda.samples(jags_3,variable.names = c("x", "eps","z"), n.iter = ninter, thin = 5))
sum_3 = summary(out_3)
out_3s = out_3[,which(regexpr("x", row.names(sum_3$statistics))==1)]
sum_3x = sum_3$statistics[which(regexpr("x", row.names(sum_3$statistics))==1),]
out_3e = out_3[,which(regexpr("eps", row.names(sum_3$statistics))==1)]
plot(out_3e)

str(sum_3)

low_3 = apply((rbind(out_3s[[1]],out_3s[[2]],out_3s[[3]])),2, function(x) sort(x)[round((dim(out_3[[1]])[1]*3)*0.05)])
high_3 = apply((rbind(out_3s[[1]],out_3s[[2]],out_3s[[3]])),2, function(x) sort(x)[round((dim(out_3[[1]])[1]*3)*0.95)])
mean_3 = apply((rbind(out_3s[[1]],out_3s[[2]],out_3s[[3]])),2, mean)
apply((rbind(out_3z[[1]],out_3z[[2]],out_3z[[3]])),2, mean)


nsim = dim(out_ns[[1]])[1]*3
perc5 = round(nsim*0.05)
perc95 = round(nsim*0.95)
mse_ns = low = high  = NULL
for(i in 1:100){
  mse_ns[i] = sum((rbind(out_ns[[1]],out_ns[[2]],out_ns[[3]])[,i]-x[i])^2)
}
low_ns = apply((rbind(out_ns[[1]],out_ns[[2]],out_ns[[3]])),2, function(x) sort(x)[perc5])
high_ns = apply((rbind(out_ns[[1]],out_ns[[2]],out_ns[[3]])),2, function(x) sort(x)[perc95])
nsim = dim(out_smo[[1]])[1]*3
mse_smo  = NULL
for(i in 1:100){
    mse_smo[i] = sum((rbind(out_smo[[1]],out_smo[[2]],out_smo[[3]])[,i]-x[i])^2)
}
low_smo = apply((rbind(out_smo[[1]],out_smo[[2]],out_smo[[3]])),2, function(x) sort(x)[perc5])
high_smo = apply((rbind(out_smo[[1]],out_smo[[2]],out_smo[[3]])),2, function(x) sort(x)[perc95])
nsim = dim(out_3[[1]])[1]*3
mse_3  = NULL
for(i in 1:100){
  mse_3[i] = sum((rbind(out_3[[1]],out_3[[2]],out_3[[3]])[,i]-x[i])^2)
}
mean(mse_ns)
mean(mse_smo)
mean(mse_3)
data.frame(low=bands$low_ns,true=x,high=bands$high_ns)
data.frame(low=low_smo,true=x,high=high_smo)

low_ns = bands$low_ns
high_ns = bands$high_ns
dfp = data.frame(Original=x, Technique_1=sum_ns_x[,1],
                 Technique_2=sum_smo_x[,1], Technique_3=mean_3, Time=da)
bands =  data.frame(low_ns=low_ns, high_ns=high_ns, low_smo=low_smo, 
                    high_smo=high_smo, high_3=high_3, low_3=low_3, x=da)

#dfp = data.frame(Original=x, Technique_1=sum_ns_x[,1],
#                 Technique_2=sum_smo_x[,1],  Time=da)
#bands =  data.frame(low_ns=low_ns, high_ns=high_ns, low_smo=low_smo, 
#                    high_smo=high_smo, x=da)

bands$high_ns
po = data.frame(poll = poll.dat$y, day=seq(org,by='days',length=100)[pd$Date])
library(ggplot2)
library(reshape)
d_sub = melt(dfp, id=c("Time"))
p = ggplot(d_sub) +
  geom_line(aes(x=Time, y=value, colour=variable)) +
  ylab("x")+
  xlab("")+
  geom_point(data=po, aes(x=day,y=poll), size=0.6) +
  geom_ribbon(data=bands, aes(x=da,ymin=low_ns, ymax=high_ns), fill="hotpink", alpha=0.3) +
  geom_ribbon(data=bands, aes(x=da,ymin=low_smo, ymax=high_smo), fill="blue", alpha=0.3) +
  scale_colour_manual("",values=c("black","hotpink","blue"))+
  expand_limits(y=c(0,1)) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())
p

dateSmooth
eee = data.frame(pd2[1:10,], x = dateSmooth[1:10])
head(poll.dat)
period1[2]-period1[1]
p = ggplot(d_sub) +
  geom_line(aes(x=Time, y=value, colour=variable)) +
  ylab("x")+
  xlab("")+
  geom_point(data=po, aes(x=day,y=poll), size=0.6) +
  geom_ribbon(data=bands, aes(x=da,ymin=low_3, ymax=high_3), fill="orange", alpha=0.3) +
  geom_ribbon(data=bands, aes(x=da,ymin=low_ns, ymax=high_ns), fill="hotpink", alpha=0.3) +
  geom_ribbon(data=bands, aes(x=da,ymin=low_smo, ymax=high_smo), fill="blue", alpha=0.3) +
  scale_colour_manual("",values=c("black","hotpink","blue","orange"))+
  expand_limits(y=c(0,1)) +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())
p

eee = data.frame(pd2[1:10,], x = dateSmooth[1:10])
eee2 = data.frame(pd2[11:17,], x = dateSmooth[11:17])
eee3 = data.frame(pd2[18:49,], x = dateSmooth[18:49])
eee4 = data.frame(pd2[18:49,], x = dateSmooth[18:49])
eee5 = data.frame(pd2[50:57,], x = dateSmooth[50:57])
eee6 = data.frame(pd2[58:65,], x = dateSmooth[58:65])
eee7 = data.frame(pd2[66:76,], x = dateSmooth[66:76])
eee8 = data.frame(pd2[77:84,], x = dateSmooth[77:84])
eee9 = data.frame(pd2[85:90,], x = dateSmooth[85:90])
eee10 = data.frame(pd2[91:102,], x = dateSmooth[91:102])
eee11 = data.frame(pd2[103:112,], x = dateSmooth[103:112])
eee12 = data.frame(pd2[113:119,], x = dateSmooth[113:119])
eee13 = data.frame(pd2[120:130,], x = dateSmooth[120:130])


p+
  geom_line(data=eee, aes(y=y,x=x), linetype=3, col="grey28") +
  geom_line(data=eee2, aes(y=y,x=x), linetype=3, col="grey28") +
  geom_line(data=eee3, aes(y=y,x=x), linetype=3, col="grey28") +
  geom_line(data=eee4, aes(y=y,x=x), linetype=3, col="grey28") +
  geom_line(data=eee5, aes(y=y,x=x), linetype=3, col="grey28") +
  geom_line(data=eee6, aes(y=y,x=x), linetype=3, col="grey28") +
  geom_line(data=eee7, aes(y=y,x=x), linetype=3, col="grey28") +
  geom_line(data=eee8, aes(y=y,x=x), linetype=3, col="grey28") +
  geom_line(data=eee9, aes(y=y,x=x), linetype=3, col="grey28") +
  geom_line(data=eee10, aes(y=y,x=x), linetype=3, col="grey28") +
  geom_line(data=eee11, aes(y=y,x=x), linetype=3, col="grey28") +
  geom_line(data=eee12, aes(y=y,x=x), linetype=3, col="grey28") +
  geom_line(data=eee13, aes(y=y,x=x), linetype=3, col="grey28") 
  



