data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)
names(polls)
polls2 = na.omit(polls[,-c(11,12)])
polls2 = polls2[order(polls2$collectPeriodFrom),]
polls2$collectPeriodFrom = as.Date(polls2$collectPeriodFrom)
polls2$collectPeriodTo = as.Date(polls2$collectPeriodTo)
polls2$PublDate = as.Date(polls2$PublDate)
orig.date = as.Date(polls2$collectPeriodFrom[1])
end.date = as.Date(polls2$collectPeriodTo[length(polls2$collectPeriodTo)])
polls2$collectPeriodFrom.num = julian(polls2$collectPeriodFrom,origin=orig.date) #days since origin
polls2$collectPeriodTo.num = julian(polls2$collectPeriodTo,origin=orig.date) #days since origin
polls2$fieldDate.num = floor((polls2$collectPeriodFrom.num + polls2$collectPeriodTo.num) / 2)

M = polls2$M/100

#### dlm - 1 party ####
library(dlm)
?dlm
#yt=FFt*thetat+vt, vt~N(0,V)
#thetat=GGtthetat-1+wt, wt~N(0,W)
mod = dlmModPoly(1, m0=c(0.27))
m_ts = as.ts(M)
out_dlm = dlmFilter(m_ts,mod)

### plotta- kolla ada (ggplot2)
plot(M)
lines(out_dlm$f, col="hotpink")
hist(out_dlm$f, breaks=20)
plot(density(out_dlm$f))

mod = dlmModPoly(2, m0=c(0.27)
m_ts = as.ts(M)
out_dlm = dlmFilter(m_ts,mod)

plot(m_ts)
lines(out_dlm$f, col="hotpink")
hist(out_dlm$f, breaks=20)
plot(density(out_dlm$f))


#### jags - 1 party ####
library(rjags)

jags_M ='
model{

#observed model
for(i in 1:npolls){
M[i] ~ dnorm(alphaM[day[i]],precM[i])
}

#dynamic model 
for(i in 2:nperiods){
alphaM[i] ~ dnorm(alphaM[i-1],phiM)
}

## priors
omega ~ dgamma(1, 1)
phiM <- 1/pow(omega,2)
alphaM[1] <- 0.27
}
'
pM = (1 / (3*(polls2$M/100*(1-polls2$M/100)/polls2$n)))
yM = polls2$M/100
data_M = list(M = yM, precM = pM, alphaM = rep(NA,end.date - orig.date),
             day = polls2$fieldDate.num, npolls = nrow(polls2), 
             nperiods = as.numeric(end.date - orig.date))
writeLines(jags_M,con="kalman_M.bug")

system.time(jags_mod_M <- jags.model("kalman_M.bug", data = data_M))
#update(jags_mod_M,n.iter = 100)

system.time(outM <- coda.samples( jags_mod_M,variable.names = c("alphaM", "M" ), n.iter = 1000,n.thin = 10))
sumM <- summary(outM)
#str(sumM)
plot(yM, type="l")
#lines(sumM$statistics[1:755,1], col="hotpink") #exactly the input

plot(sumM$statistics[756:length(sumM$statistics[,1]),1], col="hotpink", type="l")
str(sumM$statistics)
#### rstan - 1 party ####
library(rstan)
stan_M <- '
data {
int<lower=0> npolls;
int<lower=0> nperiods; 
int<lower=0> date[npolls];
real y[npolls];
real myvar[npolls];
} 

parameters {
real<lower=0,upper=1> alphaM[nperiods];
real omega;
}

transformed parameters {
real tau;
tau <- pow(omega, 2);
}

model { 
alphaM[1] ~ normal(0.27,0.0001);
for (t in 2:(nperiods)) {
alphaM[t] ~ normal(alphaM[t-1],tau);
}

for (i in 1:npolls) {
y[i]  ~ normal(alphaM[date[i]],myvar[i]);
}
omega ~ gamma(1,1);
}
'

test_M = stan_model(model_code=stan_M)
Mstan=list(npolls=nrow(polls2), nperiods=as.numeric(end.date - orig.date), y=yM, 
           myvar=polls2$M/100*(1-polls2$M/100)/polls2$n,  date=polls2$fieldDate.num)
fitData_M=sampling(test_M, data=Mstan, burnin=10, warmup=100, iter=1000, chains=1);
str(fitData_M)
print(fitData_M, digits_summary=3)
samples_M = extract(fitData_M, c("alphaM"))
plot(density(colMeans(samples_M$alphaM)))

plot(polls2$M, type="l")
lines(1:as.numeric(end.date - orig.date),colMeans(samples_M$alphaM), type="l", col="hotpink")

#### dlm - 2 party ####



#### jags - 2 party ####
jags_M ='
model{

#observed model
for(i in 1:npolls){
M[i] ~ dnorm(alphaM[day[i]],precM[i])
}

#dynamic model 
for(i in 2:nperiods){
alphaM[i] ~ dnorm(alphaM[i-1],phiM)
}

## priors
omega ~ dnorm(0.01, 0.01)
phiM <- 1/pow(omega,2)
alphaM[1] <- 0.27
}
'
#### rstan - 2 party ####