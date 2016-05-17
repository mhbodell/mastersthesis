library(rjags, lib="C:/Users/mirhu86/Documents/packages")
#library(rjags)
data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)

y =  polls[3:10]
y$startDate = as.Date(polls$collectPeriodFrom)
y$endDate = as.Date(polls$collectPeriodTo)
y$house = as.factor(polls$house)
no_n = which(is.na(polls[,'n']))
y = y[-no_n,]
y$n = polls$n[-no_n]
y = y[-which(is.na(y[,'SD'])),]
y[,1:8] = y[,1:8]/100
y = y[-which(is.na(y[,'startDate'])),]
y$O = 1-rowSums(y[,1:8])
y = y[-which(y$O==0),]
df = y

elec = data.frame(rbind(c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293, 1-sum(c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293))),
                        c(0.3006,0.0706,0.0560,0.0656,0.3066,0.0734,0.0560,0.0570, 1-sum(c(0.3006,0.0706,0.0560,0.0656,0.3066,0.0734,0.0560,0.0570))),
                        c(0.2333, 0.0542, 0.0457, 0.0611, 0.3101, 0.0689, 0.0572, 0.1286, 1-sum(c(0.2333, 0.0542, 0.0457, 0.0611, 0.3101, 0.0689, 0.0572, 0.1286)))))

colnames(elec) = c("M","L","KD","C","S","MP","V","SD","O")
row.names(elec) = c("2006","2010","2014") #"2002",
elec$Date = c( as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14')) #as.Date("2002-09-12"),
n=c((0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000)) #(0.801*6722*1000),
## http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf
colnames(df)[2] <- "L"
df2 = data.frame(M=elec$M, L=elec$L,C=elec$C,KD=elec$KD,S=elec$S, V=elec$V,
                 MP=elec$MP,SD=elec$SD, O=elec$O, startDate=elec$Date, endDate=elec$Date,
                 house="Election" ,n=n)
df = rbind(df, df2)
df = df[order(df$startDate),]

orig.date = df$startDate[1]-2
end.date = df$endDate[length(df$endDate)]
df$length = as.numeric(df$endDate - df$startDate)+1

partynames = c("M","L","KD","C","S","MP","V","SD","O")
df$Day = julian(df$startDate,orig.date)-1
df$house = factor(df$house, levels=c("Demoskop","Inizio", "Ipsos" ,"Novus" ,"SCB" ,"Sentio" ,
                                     "Sifo" ,"Skop" ,"SVT", "United Minds", "YouGov","Election"))

data = list(nperiod = df$Day[nrow(df)]+df$length[nrow(df)]+1,k=df$length,
            npolls = nrow(df),  nparties = 9, y = df[,partynames], day = df$Day,
            n = df$n, org = as.numeric(df$house), b = matrix(NA, ncol=9, nrow=nrow(df)),
            z=array(NA, dim=c(df$Day[nrow(df)]+df$length[nrow(df)]+1,9,nrow(df))),
            nhouses = length(levels(as.factor(df$house))))

diri_diri_lm <- '
model {
#observed model
for(i in 1:npolls) { 
n2[i] <- round(n[i]/k[i])
for(j in 1:k[i]){z[j,1:nparties,i] <- x[day[i]+j, 1:nparties] }
for(l in 1:nparties){b[i,l] <- sum(z[1:k[i],l,i]*n2[i])/n[i]}
b2[i,1:nparties] <- b[i,1:nparties] + house[org[i],1:nparties]
y[i, 1:nparties] ~ ddirch(b2[i,1:nparties]*n[i])
}
#dynamic model
for(i in 2:nperiod) {
conc[i] ~ dnorm(conc[i-1], sigd)
x[i, 1:nparties] ~ ddirch(x[i-1,  1:nparties] * conc)
}
for (i in 1:nparties) { init[i] ~ dunif(100, 1000)}
x[1, 1:nparties] ~ ddirch(init[])


for (i in 2:nparties) { house[1, i] <- -sum(house[2:nhouses, i])}
for(i in 1:(nhouses-1)) { house[i, 1] <- -sum(house[i, 2:nparties])}

for(i in 1:nparties){house[12,i]<-0}


for (i in 2:nparties) { 
for(j in 2:(nhouses-1)) { house[j, i] ~ dnorm(0,sigh)}
}


conc[1]~dgamma(1,0.0001)
sigd~dgamma(1,1)
sigh~dgamma(1,1)
}
'
writeLines(diri_diri_lm,con="diri_diri_lm.bug")
system.time(jags_ddlmh <- jags.model("diri_diri_lm.bug", data = data, n.chain=3, n.adapt=100000))
ninter=20000
system.time(add_outh <- coda.samples(jags_ddlmh,variable.names = c("x","conc","house", "sigd"), n.iter = ninter, thin = 10, burnin=5000))
system.time(sum_addh <- summary(add_outh))

add_out = add_outh
sum_add = sum_addh
addout_x2 = add_out[,which(regexpr("x", row.names(sum_add$statistics))==1)]
out_conc = add_out[,which(regexpr("conc", row.names(sum_add$statistics))==1)]
addhouse = add_out[,which(regexpr("house", row.names(sum_add$statistics))==1)]
map_house = matrix(NA, nrow=length(levels(as.factor(df$house))), ncol=9)
j=1
for(i in seq(1,dim(addhouse[[1]])[2],12)){
  map_house[,j] = apply(as.matrix(addhouse[,seq(i,i+11,1)]),2,mean)
  j = j+1
}
row.names(map_house) = levels(as.factor(df$house))
colnames(map_house) = partynames
map_house 

hist(rbind(out_conc[[1]],out_conc[[2]],out_conc[[3]]), breaks=30, main="", xlab="Concentration parameter", freq=FALSE, las=1)
lines(density(rbind(out_conc[[1]],out_conc[[2]],out_conc[[3]])), lwd=2, col="purple")
abline(v=mean(rbind(out_conc[[1]],out_conc[[2]],out_conc[[3]])), lwd=2)
text(x=mean(rbind(out_conc[[1]],out_conc[[2]],out_conc[[3]]))+10,y=max(density(rbind(out_conc[[1]],out_conc[[2]],out_conc[[3]]))$y) ,labels=round(mean(rbind(out_conc[[1]],out_conc[[2]],out_conc[[3]]))), cex=0.7)

nperiods = df$Day[nrow(df)]+df$length[nrow(df)] + 1
nsim = dim(add_out[[1]])[1]*3
mean_add2 = matrix(NA, ncol=9, nrow=nperiods)
conmat = matrix(NA, ncol=9, nrow=nperiods)
ind.start = 1
ind.end = nperiods
high_add2 = matrix(NA, ncol=9, nrow=nperiods)
low_add2 = matrix(NA, ncol=9, nrow=nperiods)
percentile5 = round(nsim*0.05)
percentile95 = round(nsim*0.95)
states_add2 = list()
for(i in 1:9){
  ind.start[i+1] = i*nperiods+1
  ind.end[i+1] =  ind.start[i+1]+nperiods-1
  mean_add2[,i] = apply(rbind(addout_x2[[1]][,ind.start[i]:ind.end[i]],addout_x2[[2]][,ind.start[i]:ind.end[i]],addout_x2[[3]][,ind.start[i]:ind.end[i]]),2,mean)
  low_add2[,i] = apply(rbind(addout_x2[[1]][,ind.start[i]:ind.end[i]],addout_x2[[2]][,ind.start[i]:ind.end[i]],addout_x2[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile5])
  high_add2[,i] = apply(rbind(addout_x2[[1]][,ind.start[i]:ind.end[i]],addout_x2[[2]][,ind.start[i]:ind.end[i]],addout_x2[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile95])
  states_add2[[i]] = rbind(addout_x2[[1]][,ind.start[i]:ind.end[i]],addout_x2[[2]][,ind.start[i]:ind.end[i]],addout_x2[[3]][,ind.start[i]:ind.end[i]])
}
str(states_add2)
colnames(mean_add2) = partynames