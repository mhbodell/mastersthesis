library(rjags, lib="C:/Users/mirhu86/Documents/packages")
data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)

O = NULL
for(i in 1:nrow(polls)){
  O[i] = ifelse(sum(polls[i,3:10], na.rm=TRUE)==100,0, sum(polls[i,11:12], na.rm=TRUE))
}

y =  polls[3:10]
y$O = O
y$startDate = as.Date(polls$collectPeriodFrom)
y$endDate = as.Date(polls$collectPeriodTo)
y$house = as.factor(polls$house)
no_n = which(is.na(polls[,'n']))
y = y[-no_n,]
y$n = polls$n[-no_n]

y[,1:9] = y[,1:9]/100
df = na.omit(y[,-9])

elec = data.frame(rbind( c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                         c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                         c(0.2333, 0.0542, 0.0457, 0.0611, 0.3101, 0.0689, 0.0572, 0.1286))) #c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0.00000001),

colnames(elec) = c("M","L","KD","C","S","MP","V","SD")
row.names(elec) = c("2006","2010","2014") #"2002",
elec$Date = c( as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14')) #as.Date("2002-09-12"),
n=c((0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000)) #(0.801*6722*1000),
## http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf
colnames(df)[2] <- "L"
df2 = data.frame(M=elec$M, L=elec$L,C=elec$C,KD=elec$KD,S=elec$S, V=elec$V,
                 MP=elec$MP,SD=elec$SD, startDate=elec$Date, endDate=elec$Date,
                 house="Election" ,n=n)
df = rbind(df, df2)
df = df[order(df$startDate),]
orig.date = df$startDate[1]-1
end.date = df$endDate[length(df$endDate)]
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
head(df)

library(rstan, lib="C:/Users/mirhu86/Documents/packages")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

ssm <-'
data {
  int<lower = 0> T;
  int<lower = 0> nperiods;
  int<lower = 0> day[T];
  int<lower = 0> N_y;
  vector[N_y] y[T];
  int<lower = 0> rr;
  cov_matrix[N_y] sm;
}
parameters {
  vector[N_y] x[nperiods];
  cov_matrix[N_y] Sigma_y;
  cov_matrix[N_y] Sigma_x;
}
model {
  for (t in 1:T) {
    y[t] ~ multi_normal(x[day[t]], Sigma_y);
  }

  for (t in 2:nperiods) {
    x[t] ~ multi_normal(x[t - 1], Sigma_x);
  }
  
  Sigma_y ~ inv_wishart(rr, sm);
  Sigma_x ~ inv_wishart(rr, sm);
}
'

T <- nrow(df)
N_y <- 8
y <- df[,1:8]
rr<- 8
ee = matrix(0,8,8)
diag(ee) = 1
sm <-  ee
nperiods = max(df$Date)
day = df$Date

model <- stan_model(model_code=ssm)
fit <- sampling(model, data = list(y =y, T=T,N_y, sm=sm, nperiods=nperiods, day=day ),  pars = c("x","Sigma_y","Sigma_x"),
                chains = 1, iter = 1000, warmup = 100, thin = 5)
str(fit)
print(fit)
eFit = extract(fit)
fit@sim$samples$x


str(eFit)
str(eFit$y_dash)
eFit$y_dash[,,1]
plot(apply(eFit$x[,,1],2,mean),type="l")
points(df[,1])

###### smooth #####
library(rstan, lib="C:/Users/mirhu86/Documents/packages")

ssm <-'
data {
int<lower = 0> T;
int<lower = 0> N_y;
vector[N_y] y[T];
int<lower = 0> rr;
cov_matrix[N_y] sm;
}
parameters {
vector[N_y] x[T];
cov_matrix[N_y] Sigma_y;
cov_matrix[N_y] Sigma_x;
}
model {
for (t in 1:T) {
y[t] ~ multi_normal(x[t], Sigma_y);
}

for (t in 2:T) {
x[t] ~ multi_normal(x[t - 1], Sigma_x);
}

Sigma_y ~ inv_wishart(rr, sm);
Sigma_x ~ inv_wishart(rr, sm);
}
'

data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)


O = NULL
for(i in 1:nrow(polls)){
  O[i] = ifelse(sum(polls[i,3:10], na.rm=TRUE)==100,0, sum(polls[i,11:12], na.rm=TRUE))
}

y =  polls[3:10]
y$O = O
y$startDate = as.Date(polls$collectPeriodFrom)
y$endDate = as.Date(polls$collectPeriodTo)
y$house = as.factor(polls$house)
no_n = which(is.na(polls[,'n']))
y = y[-no_n,]
y$n = polls$n[-no_n]

y[,1:9] = y[,1:9]/100
df = na.omit(y[,-9])

elec = data.frame(rbind( c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                         c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                         c(0.2333, 0.0542, 0.0457, 0.0611, 0.3101, 0.0689, 0.0572, 0.1286))) #c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0.00000001),

colnames(elec) = partynames = c("M","L","KD","C","S","MP","V","SD")
row.names(elec) = c("2006","2010","2014") #"2002",
elec$Date = c( as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14')) #as.Date("2002-09-12"),
n=c((0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000)) #(0.801*6722*1000),
## http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf
colnames(df)[2] <- "L"
df2 = data.frame(M=elec$M, L=elec$L,C=elec$C,KD=elec$KD,S=elec$S, V=elec$V,
                 MP=elec$MP,SD=elec$SD, startDate=elec$Date, endDate=elec$Date,
                 house="Election" ,n=n)
df = rbind(df, df2)
df = df[order(df$startDate),]
orig.date = df$startDate[1]-1
end.date = df$endDate[length(df$endDate)]
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
head(df)
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)

dateDiff = df[,'endDate'] - df[,'startDate']
dateDiff2 = dateDiff+1
nDay = df$n/as.numeric(dateDiff2)

pSmooth = matrix(NA, ncol=length(partynames), nrow=sum(dateDiff2))
colnames(pSmooth) = partynames
j = 1
for(i in partynames){
  pSmooth[,j] = rep(df[,i],dateDiff2)
  j= j+1
}

nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(df$house, dateDiff2)

ee = list()
for(i in 1:nrow(df)){
  ee[[i]] = seq(df[i,'startDate'], df[i,'endDate'], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(df)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
df2 = data.frame(M = pSmooth[,'M'],L = pSmooth[,'L'], KD= pSmooth[,'KD'], C = pSmooth[,'C'],
                 S = pSmooth[,'S'], V = pSmooth[,'V'], MP =  pSmooth[,'MP'], SD = pSmooth[,'SD'],
                 Date = dateSmooth.num, n = nSmooth, house = houseSmooth)


prec2 = matrix(NA,ncol=8, nrow=nrow(df2))
colnames(prec2) = colnames(df2)[1:8]
for(i in 1:8){
  for(j in 1:nrow(df2)){
    prec2[j,i] = ifelse(is.na(df2[j,i]), NA ,(1 / (df2[j,i]*(1-df2[j,i])/df2[i,'n'])))
  }
}



T <- nrow(df2)
N_y <- 8
y <- df2[,1:8]
rr<- 8
ee = matrix(0,8,8)
diag(ee) = 1
sm <-  ee


model <- stan_model(model_code=ssm)

set_cppo(mode='fast')
fit2 <- sampling(model, data = list(y =y, T=T,N_y, sm=sm ),  pars = c("x","Sigma_y","Sigma_x"),
                chains = 3, iter = 1000, warmup = 100, thin = 5, cores=3)
?set_cppo
eFit2 = extract(fit2)

plot(apply(eFit2$y_dash[,,1],2,mean),type="l")
points(df[,1])

