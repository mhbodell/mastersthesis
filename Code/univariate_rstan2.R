library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

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
real<lower=0.00001> s[npolls];
}

transformed parameters {
real<lower=0> tau;
tau <- 1/omega; ##inverse_gamma

}

model { 
alphaM[1] ~ normal(0.27,0.00000001);
omega ~ gamma(800000,1);

for (t in 2:(nperiods)) {
alphaM[t] ~ normal(alphaM[t-1], tau);
}

for (i in 1:npolls) {
y[i]  ~ normal(alphaM[date[i]], s[i]*myvar[i]);
}
}

generated quantities{
real<lower=0, upper=1> Pred[npolls]; #generating posterior predictive
real<lower=0, upper=1> PredM[npolls]; #predictive mean
for(i in 1:npolls) {
PredM[i] <-alphaM[date[i]];
Pred[i]<-normal_rng(PredM[i], myvar[i]);
}
}

'
#rgamma(1000,10,10)
test_M = stan_model(model_code=stan_M)
Mstan=list(npolls=nrow(datM), nperiods=as.numeric(end.date - orig.date), y=datM$M, 
           myvar=(datM$M*(1-datM$M)/datM$n),date=datM$fieldDate.num)
fitData_M=sampling(test_M, data=Mstan, warmup=300, chain=1 ,iter=3000)#, 
                #  control=list(adapt_delta=0.95)); #thin=10,
traceplot(fitData_M)
samples_M = extract(fitData_M, c("alphaM",  "tau", "Pred","PredM"))
#str(fitData_M)
colMeans(samples_M$alphaM)
plot(density(samples_M$Pred[,ncol(samples_M$Pred)]))
print(fitData_M)
samples_M$tau

lapply(get_sampler_params(fitData_M, inc_warmup = TRUE), summary, digits = 2)

#####
hist(unlist(fitData_M@sim$samples[[1]][length(c(0.2623,rep(NA,end.date - orig.date-1)))]))
#true values election night 0.2333

####
meanM = colMeans(samples_M$alphaM)
plot(seq(as.Date('2006-09-17'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1)))),meanM, type="l", ylim=c())
plot(seq(as.Date('2006-09-17'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1))))[datM$fieldDate.num],datM$M)
library(coda)
cred_intM = HPDinterval(mcmc(samples_M$alphaM), 0.95)

df = data.frame(xM = meanM , low=cred_intM[,1]*100, high=cred_intM[,2]*100,
                time=seq(as.Date('2006-09-17'),by='days',length=length(c(0.2623,rep(NA,end.date - orig.date-1)))))
library(ggplot2)
ggplot(df) +
  aes(x = time, y = xM*100) +
  geom_line(col="blue", alpha=1)  +
  #geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.2, fill="blue3") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="blue") 
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



