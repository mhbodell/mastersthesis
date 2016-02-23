#### rstan - M ####
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
real<lower=0.000001> omega;
}

transformed parameters {
real tau;
tau <- pow(omega, 2);
}

model { 
alphaM[1] ~ normal(0.27,0.0001);

for (t in 2:(nperiods)) {
alphaM[t] ~ normal(alphaM[t-1], tau);
}

for (i in 1:npolls) {
y[i]  ~ normal(alphaM[date[i]],myvar[i]);
}
omega ~ gamma(1,1);
}
'


#rgamma(1000,10,10)
test_M = stan_model(model_code=stan_M)
Mstan=list(npolls=nrow(datM), nperiods=as.numeric(end.date - orig.date), y=datM$M, 
           myvar=(datM$M*(1-datM$M)/datM$n),date=datM$fieldDate.num)
fitData_M=sampling(test_M, data=Mstan,warmup=300, chain=1 ,thin=10 ,iter=3000); #thin=10,
#traceplot(fitData_M)
#print(fitData_M, digits=3)
samples_M = extract(fitData_M, c("alphaM"))


##### M2 ######

stan_M <- '
data {
int<lower=0> nPolls;
int<lower=0> nPeriods; 
int<lower=0> date[nPolls];
real yM[nPolls];
real kappa;
real myvarM[nPolls];
} 
parameters {
real<lower=0,upper=1> xMi0[nPeriods];
real<lower=0, upper=kappa> omega;
}
transformed parameters {
real xMi[nPeriods];
real tau;
tau <- pow(omega, 2);
xMi[1] <- 0.2623;
for (i in 2:nPeriods) {
xMi[i] <- xMi0[i];
}

}
model {
// observation model
for (i in 1:nPolls) {
yM[i]  ~ normal(xMi[date[i]],myvarM[i]);
}

// transition model
// First two periods: take from known value 
xMi0[1] ~ normal(xMi[1],tau);
xMi0[2] ~ normal(xMi[1],tau);
for (t in 3:(nPeriods)) {
xMi0[t] ~ normal(xMi0[t-1],tau);
}
xMi0[nPeriods] ~ normal(xMi[nPeriods],0.000001);
omega ~ uniform(0,kappa);
}
'
test_M = stan_model(model_code=stan_M)

Mstan=list(nPolls=nrow(datM), nPeriods=as.numeric(end.date - orig.date), yM=datM$M, 
           myvarM=(datM$M*(1-datM$M)/datM$n),  
           date=datM$fieldDate.num, kappa=0.01)

fitData_M=sampling(test_M, data=Mstan, warmup=100 ,iter=2000, thin=10,chains=1);
samples_M = extract(fitData_M, c("xMi"))
colMeans(samples_M$xMi)

##### L ######
library("rstan")
stan_L <- '
data {
int<lower=0> nPolls;
int<lower=0> nPeriods; 
int<lower=0> date[nPolls];
real yL[nPolls];
real kappa;
real myvarL[nPolls];
} 
parameters {
real<lower=0,upper=1> xLi0[nPeriods];
real<lower=0, upper=kappa> omega;
}
transformed parameters {
real xLi[nPeriods];
real tau;
tau <- pow(omega, 2);
xLi[1] <- 0.0754;
for (i in 2:nPeriods) {
xLi[i] <- xLi0[i];
}

}
model {
// observation model
for (i in 1:nPolls) {
yL[i]  ~ normal(xLi[date[i]],myvarL[i]);
}

// transition model
// First two periods: take from known value 
xLi0[1] ~ normal(xLi[1],tau);
xLi0[2] ~ normal(xLi[1],tau);
for (t in 3:(nPeriods)) {
xLi0[t] ~ normal(xLi0[t-1],tau);
}
xLi0[nPeriods] ~ normal(xLi[nPeriods],0.000001);
omega ~ uniform(0,kappa);
}
'
test_L = stan_model(model_code=stan_L)

Lstan=list(nPolls=nrow(datL), nPeriods=as.numeric(end.date - orig.date), yL=datL$L, 
           myvarL=(datL$L*(1-datL$L)/datL$n),  
           date=datL$fieldDate.num, kappa=0.005)

fitData_L=sampling(test_L, data=Lstan, warmup=100 ,iter=1000, thin=10,chains=1);
samples_L = extract(fitData_L, c("xLi"))
colMeans(samples_L$xLi)

##### KD ######
library("rstan")
stan_KD <- '
data {
int<lower=0> nPolls;
int<lower=0> nPeriods; 
int<lower=0> date[nPolls];
real yKD[nPolls];
real kappa;
real myvarKD[nPolls];
} 
parameters {
real<lower=0,upper=1> xKDi0[nPeriods];
real<lower=0, upper=kappa> omega;
}
transformed parameters {
real xKDi[nPeriods];
real tau;
tau <- pow(omega, 2);
xKDi[1] <- 0.0659;
for (i in 2:nPeriods) {
xKDi[i] <- xKDi0[i];
}

}
model {
// observation model
for (i in 1:nPolls) {
yKD[i]  ~ normal(xKDi[date[i]],myvarKD[i]);
}

// transition model
// First two periods: take from known value 
xKDi0[1] ~ normal(xKDi[1],tau);
xKDi0[2] ~ normal(xKDi[1],tau);
for (t in 3:(nPeriods)) {
xKDi0[t] ~ normal(xKDi0[t-1],tau);
}
xKDi0[nPeriods] ~ normal(xKDi[nPeriods],0.000001);
omega ~ uniform(0,kappa);
}
'

test_KD = stan_model(model_code=stan_KD)

KDstan=list(nPolls=nrow(datKD), nPeriods=as.numeric(end.date - orig.date), yKD=datKD$KD, 
           myvarKD=(datKD$KD*(1-datKD$KD)/datKD$n),  
           date=datKD$fieldDate.num, kappa=0.005)

fitData_KD=sampling(test_KD, data=KDstan, warmup=100 ,iter=1000, thin=10,chains=1);
samples_KD = extract(fitData_KD, c("xKDi"))
colMeans(samples_KD$xKDi)

