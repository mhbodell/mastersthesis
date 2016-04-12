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

orig.date = df$startDate[1]-7
end.date = df$endDate[length(df$endDate)]
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
head(df)
df$Week = as.numeric(as.Date(df$startDate)-orig.date) %/% 7
df$Week

Y = df[c("M","L","KD","C","S","MP","V","SD")] * df$n
Y = sapply(Y, function(x) round(x,0))

data = list(nperiod = max(df$Date),nhouses = length(levels(as.factor(df$house))),
            npolls = nrow(df),  nparties = ncol(Y), y = Y, day = df$Date, org = as.numeric(as.factor(df$house)),
            n = rowSums(Y), house = matrix(NA, ncol=ncol(Y), nrow=length(levels(as.factor(df$house)))))

multi_diri <- '
model {
#observed model
for(i in 1:npolls) { 
y[i, 1:nparties] ~ dmulti(x[day[i], 1:nparties] + house[org[i], 1:nparties], n[i])
}

#dynamic model
for(i in 2:nperiod) {
  Alpha[i, 1:nparties] <- x[i-1,  1:nparties] * 10000
  x[i, 1:nparties] ~ ddirch(Alpha[i, 1:nparties])
}

for (i in 1:nparties) { 
  alpha[i] ~ dunif(10, 100) 
}
x[1, 1:nparties] ~ ddirch(alpha[])

for (i in 1:nparties) { 
  house[12, i] <- 0
  for(j in 1:(nhouses-1)) { 
  house[j, i] ~ dnorm(0, 0.01)
  }
}

for (i in 2:nparties) { 
house[1, i] <- -sum( house[2:nhouses, i] )
}
for(i in 1:nhouses) { 
house[i, 1] <- -sum( house[i, 2:nparties] )
}


## -- vague normal priors for house effects - centred on zero
for (party in 2:PARTIES) { # for each party (cols)
for(house in 2:HOUSECOUNT) { #  (rows)
houseEffect[house, party] ~ dnorm(0, pow(0.1, -2))
}
}


}
'

writeLines(multi_diri, con="DM-model")
system.time(jags_DM <- jags.model("DM-model", data = data, n.adapt=3000, n.chain=3))
update(jags_DM,1000)
ninter = 10000
system.time(outDM <- coda.samples(jags_DM,variable.names = c("y", "x","house"), n.iter = ninter, thin = 5))
system.time(sumDM <-  summary(outDM))
x = sumDM$statistics[which(regexpr("x",row.names(sumDM$statistics))==1),]
#library(coda)
#HPD_DM = HPDinterval(as.mcmc(sumDM$statistics[which(regexpr("walk",row.names(sumDM$statistics))==1),1]),0.95)
plot(x[1:3481,1]*100, type="l")
points(df$Date,df$M*100)

plot(x[(3481+1):(3481*2),1]*100, type="l")
points(df$Date,df[,2]*100)
