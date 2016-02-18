### Libraries
library(pscl)
library(rjags)

data(AustralianElectionPolling,package="pscl")
dat <- AustralianElectionPolling



dat$startDate <- as.Date(dat$startDate)
dat$endDate <- as.Date(dat$endDate)
### Start at 2004 elex.
orig.date <- as.Date("2004-10-09")
### End at 2007 elex.
end.date <- as.Date("2007-11-24")

dat$startDate.num <- julian(dat$startDate,origin=orig.date)
dat$endDate.num <- julian(dat$endDate,origin=orig.date)
dat$fieldDate.num <- floor((dat$startDate.num + dat$endDate.num) / 2)

dat$y <- dat$ALP / 100

## Jags will use precision
### Stan will use variance
dat$var <- dat$y*(1-dat$y)/dat$sampleSize
dat$prec <- 1 / dat$var

forJags <- list(y = dat$y,
	date = dat$fieldDate.num,
	nPolls = nrow(dat),
	kappa = 0.01,
	myvar = dat$var,
	prec = dat$prec,
	xi = c(0.376,rep(NA,end.date - orig.date - 2),0.434),
	nPeriods = as.numeric(end.date - orig.date))

jags_code <- '
	model {
		for (i in 1:nPolls) {
			mu[i] <- xi[date[i]]
			y[i] ~ dnorm(mu[i],prec[i])
		}
		for (t in 2:(nPeriods-1)) {
			xi[t] ~ dnorm(xi[t-1],tau)
		}

		omega ~ dunif(0,kappa)
		tau <- 1/pow(omega,2)
	}
'

writeLines(jags_code,con="kalman.bug")


system.time(jags.mod <- jags.model("kalman.bug",
	data = forJags))
update(jags.mod,n.iter = 10000)
system.time(out <- coda.samples(jags.mod,variable.names = c("xi"),
	n.iter = 10000,
	n.thin = 10))

holder <- summary(out)
plot(1:forJags$nPeriods,holder$statistics[,1],
	xlab = "Day",
	ylab = "ALP vote intention",type="l")

stan_code <- '
	data {
		int<lower=0> nPolls;
		int<lower=0> nPeriods; //
		int<lower=0> date[nPolls];
		real y[nPolls];
		real kappa;
		real myvar[nPolls];
	} 
	parameters {
		real<lower=0,upper=1> xi0[nPeriods];
		real omega;
	}
	transformed parameters {
		real xi[nPeriods];
		real tau;

		tau <- pow(omega, 2);

		xi[1] <- 0.376;
		xi[nPeriods] <- 0.434;
		for (i in 2:(nPeriods-1)) {
			xi[i] <- xi0[i];
		}
	}
	model {
		// observation model
		for (i in 1:nPolls) {
			y[i]  ~ normal(xi[date[i]],myvar[i]);
		}

		// transition model
		// First two periods: take from known value 
		xi0[1] ~ normal(xi[1],tau);
		xi0[2] ~ normal(xi[1],tau);
		for (t in 3:(nPeriods-1)) {
			xi0[t] ~ normal(xi0[t-1],tau);
		}
		xi0[nPeriods] ~ normal(xi[nPeriods],.000001);
		omega ~ uniform(0,kappa);
	}
'

## Stan will complain if we leave partial data in
forJags$xi <- NULL

## Init func with dramatic step change in latent support
my.initfunc <- function() {
	xi0 <- c(rep(0.376,floor(forJags$nPeriods/2)),
		rep(0.434,ceiling(forJags$nPeriods/2)))
	omega <- runif(1,0,.01)
	list(xi0 = xi0,omega=omega)
}

system.time(fit <- stan(model_code = stan_code,
	data = forJags,
	chains = 1,
	init = my.initfunc,
	iter = 20000,
	thin = 10))

samples <- extract(fit, c("xi"))
plot(1:forJags$nPeriods,colMeans(samples$xi),
	xlab = "Day",
	ylab = "ALP vote intention",type="l")
