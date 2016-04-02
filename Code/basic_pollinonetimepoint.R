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
y$house = polls$house
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
orig.date = df$startDate[2]-1
end.date = df$endDate[length(df$endDate)]
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
head(df)


prec=matrix(NA,ncol=8, nrow=nrow(df))
for(i in 1:8){
  for(j in 1:nrow(df)){
    prec[j,i] = ifelse(is.na(df[j,i]), NA ,(1 / (df[j,i]*(1-df[j,i])/df[i,'n'])))
  }
}

jags_dlm ='
model{

#observed model
for(j in 1:nparties){
  for(i in 1:npolls){
    y[i,j] ~ dnorm(x[day[i],j],prec[i,j])
  }
}

#dynamic model
for(j in 1:nparties){
  for(i in 2:nperiods){
    x[i,j] ~ dnorm(x[i-1,j],phi[j])
  }
}

## priors
for(i in 1:nparties){
  init.m[i] ~ dunif(0,1)
  init.v[i] ~ dgamma(0.01,0.01)
  x[1,i] ~ dnorm(init.m[i],init.v[i])
  eps[i] ~ dgamma(10,10)
  phi[i] <- 1/eps[i]
}

}
'
y = as.matrix(df[,1:8])
all_data = list(y = y, prec = prec , x = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date)),
                nparties=ncol(y), npolls=nrow(y), nperiods=as.numeric(end.date-orig.date), 
                phi = NULL, day=df$Date )
writeLines(jags_dlm,con="jags_dlm.bug")
system.time(jags_all <- jags.model("jags_dlm.bug", data = all_data, n.chain=3))

ninter=20000

system.time(all_out <- coda.samples(jags_all,variable.names = c("x", "phi"), n.iter = ninter, thin = 5, burnin=2000))
sum_all = summary(all_out)
out_x = all_out[,which(regexpr("x", row.names(sum_all$statistics))==1)]
sum_x = sum_all$statistics[which(regexpr("x", row.names(sum_all$statistics))==1),]
out_phi = all_out[,which(regexpr("phi", row.names(sum_all$statistics))==1)]
par(mfrow=c(3,3))
for(i in 1: ncol(out_phi[[1]])){plot(out_phi[,i])}
par(mfrow=c(1,1))  

mean_basic = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
ind.start = 1
ind.end = as.numeric(end.date-orig.date)
high_basic = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
low_basic = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
states_basic = list()
for(i in 1:ncol(y)){
  ind.start[i+1] = i*as.numeric(end.date-orig.date)+1
  ind.end[i+1] =  ind.start[i+1]+as.numeric(end.date-orig.date)-1
  mean_basic[,i] = sum_x[ind.start[i]:ind.end[i],1]
  low_basic[,i] = mean_basic[,i] - 1.96 * sum_x[ind.start[i]:ind.end[i],2]
  high_basic[,i] = mean_basic[,i] + 1.96 * sum_x[ind.start[i]:ind.end[i],2]
  states_basic[[i]] = out_x[,ind.start[i]:ind.end[i]]
}

##################################################################
############# POSTERIOR PREDICTIVE CHECKING ######################
##################################################################

set.seed(901207)
i = sample(1:3,1)
rChain = list()
rChain[[1]] = states_basic[[1]][i];rChain[[2]] = states_basic[[2]][i];rChain[[3]] = states_basic[[3]][i]
rChain[[4]] = states_basic[[4]][i];rChain[[5]] = states_basic[[5]][i];rChain[[6]] = states_basic[[6]][i]
rChain[[7]] = states_basic[[7]][i];rChain[[8]] = states_basic[[8]][i]
str(rChain)


testMin.basic = matrix(NA,ncol=8, nrow=100)
testMax.basic = matrix(NA,ncol=8, nrow=100)
testMean.basic = matrix(NA,ncol=8, nrow=100)
colnames(testMin.basic) = colnames(testMax.basic) = colnames(testMean.basic) = c("M","L","KD","C","S","MP","V","SD") 

for(i in 1:100){
  for(j in 1:ncol(y)){
  yrep_basic = sapply(1:nsim, function(s) rnorm(length(df$Date),unlist(rChain[[j]][s,df$Date]), 1/prec[,j]))
  min_rep = apply(yrep_basic,2,min)
  testMin.basic[i,j] = sum(ifelse(min_rep>= min(df[,j]),1,0))/length(min_rep) 
  max_rep = apply(yrep_basic,2,max)
  testMax.basic[i,j] = sum(ifelse(max_rep>= max(df[,j]),1,0))/length(max_rep)
  mean_rep = apply(yrep_basic,2,mean)
  testMean.basic[i,j] = sum(ifelse(mean_rep>= mean(df[,j]),1,0))/length(mean_rep)
  }
}
for(i in 1:ncol(y)){
  print(paste(colnames(testMin.basic)[i],":"))
  print(paste("Min:",mean(testMin.basic[,i]), sep=" "))
  print(paste("Max:",mean(testMax.basic[,i]), sep=" "))
  print(paste("Mean:",mean(testMean.basic[,i]), sep=" "))
}

data_cb = 

#################################################
################# PLOTS #########################
#################################################


basic_plot = list()

y = as.matrix(df[,1:8])
head(y)
cols = c("blue","lightblue3","chartreuse3","darkblue","red","darkred","forestgreen","skyblue3")
library(ggplot2)
for(i in 1:ncol(mean_basic)){

plot_df = data.frame(party = mean_basic[,i] ,  low=low_basic[,i]*100, high=high_basic[,i]*100, time=seq(orig.date,by='days',
                 length=as.numeric(end.date-orig.date)), party2 = rep(colnames(y)[i], as.numeric(end.date-orig.date)))
points = data.frame(x=seq(orig.date,by='days',length=as.numeric(end.date-orig.date))[df$Date], 
                               y=df[,i]*100, house=df$house, party=rep(colnames(y)[i],length(df$Date[length(df$Date)][df$Date])))
basic_plot[[i]] <- ggplot(plot_df) +
  aes(x = time, y = party*100) +
  geom_line(col=cols[i], alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill=cols[i]) + 
  geom_point(data=points, aes(x=x, y=y), alpha = 1, color=cols[i], shape=16, size=1) +    
  labs(x="Date", y=paste("Support for", sep=" ", unique(plot_df$party2), paste("(%)", sep=" "), collapse="")) +
  theme_bw() +
  facet_wrap( ~ party2, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))}
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot(basic_plot[[1]], basic_plot[[2]], basic_plot[[3]], basic_plot[[4]], 
          basic_plot[[5]], basic_plot[[6]], basic_plot[[7]], basic_plot[[8]], cols=2)


####################################################
############ PREDICT 2010 ELECTION #################
####################################################

polls = repmis::source_data(data_url, sep = ",", header = TRUE)


O = NULL
for(i in 1:nrow(polls)){
  O[i] = ifelse(sum(polls[i,3:10], na.rm=TRUE)==100,0, sum(polls[i,11:12], na.rm=TRUE))
}

y =  polls[3:10]
y$O = O
y$startDate = as.Date(polls$collectPeriodFrom)
y$endDate = as.Date(polls$collectPeriodTo)
y$house = polls$house
no_n = which(is.na(polls[,'n']))
y = y[-no_n,]
y$n = polls$n[-no_n]

y[,1:9] = y[,1:9]/100
df = na.omit(y[,-9])

elec = data.frame(rbind(c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0.00000001),
                        c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057))) #c(0.2333, 0.0542, 0.0457, 0.0611, 0.3101, 0.0689, 0.0572, 0.1286))

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
orig.date = df$startDate[2]-1
end.date = elec$Date[2]
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
df = df[-which(df$endDate>=end.date),]
tail(df)


prec=matrix(NA,ncol=8, nrow=nrow(df))
for(i in 1:8){
  for(j in 1:nrow(df)){
    prec[j,i] = ifelse(is.na(df[j,i]), NA ,(1 / (df[j,i]*(1-df[j,i])/df[i,'n'])))
  }
}


jags_dlm ='
model{

#observed model
for(j in 1:nparties){
for(i in 1:npolls){
y[i,j] ~ dnorm(x[day[i],j],prec[i,j])
}
}

#dynamic model
for(j in 1:nparties){
for(i in 2:nperiods){
x[i,j] ~ dnorm(x[i-1,j],phi[j])
}
}

## priors
for(i in 1:nparties){
init.m[i] ~ dunif(0,1)
init.v[i] ~ dgamma(0.01,0.01)
x[1,i] ~ dnorm(init.m[i],init.v[i])
eps[i] ~ dgamma(1,1)
phi[i] <- 1/eps[i]
}

}
'
y = as.matrix(df[,1:8])
all_data = list(y = y, prec = prec , x = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date)),
                nparties=ncol(y), npolls=nrow(y), nperiods=as.numeric(end.date-orig.date), 
                phi = NULL, day=df$Date )
writeLines(jags_dlm,con="jags_dlm.bug")
system.time(jags_all <- jags.model("jags_dlm.bug", data = all_data, n.chain=3))

ninter=10000

system.time(all_out2010 <- coda.samples(jags_all,variable.names = c("x", "phi"), n.iter = ninter, thin = 5))
sum_all2010 = summary(all_out2010)
out_x2010 = all_out2010[,which(regexpr("x", row.names(sum_all2010$statistics))==1)]
sum_x2010 = sum_all2010$statistics[which(regexpr("x", row.names(sum_all2010$statistics))==1),]
out_phi2010 = all_out2010[,which(regexpr("phi", row.names(sum_all2010$statistics))==1)]
par(mfrow=c(3,3))
for(i in 1: ncol(out_phi2010[[1]])){plot(out_phi2010[,i])}
par(mfrow=c(1,1))  

mean_basic2010 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
ind.start = 1
ind.end = as.numeric(end.date-orig.date)
high_basic2010 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
low_basic2010 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
states_basic2010 = list()
for(i in 1:ncol(y)){
  ind.start[i+1] = i*as.numeric(end.date-orig.date)+1
  ind.end[i+1] =  ind.start[i+1]+as.numeric(end.date-orig.date)-1
  mean_basic2010[,i] = sum_x2010[ind.start[i]:ind.end[i],1]
  low_basic2010[,i] = mean_basic2010[,i] - 1.96 * sum_x2010[ind.start[i]:ind.end[i],2]
  high_basic2010[,i] = mean_basic2010[,i] + 1.96 * sum_x2010[ind.start[i]:ind.end[i],2]
  states_basic2010[[i]] = out_x2010[,ind.start[i]:ind.end[i]]
}

matrix(NA, ncol=4, nrow=8)


mean_basic2010[nrow(mean_basic),1];


e2010cred_intM[[1]][nrow(e2010cred_intM[[1]]),];meanL[length(meanL)]; e2010cred_intL[[1]][nrow(e2010cred_intL[[1]]),]
meanKD[length(meanKD)]; e2010cred_intKD[[1]][nrow(e2010cred_intKD[[1]]),];meanC[length(meanC)]; e2010cred_intC[[1]][nrow(e2010cred_intC[[1]]),]
meanS[length(meanS)]; e2010cred_intS[[1]][nrow(e2010cred_intS[[1]]),];meanMP[length(meanMP)]; e2010cred_intMP[[1]][nrow(e2010cred_intMP[[1]]),]
meanV[length(meanV)]; e2010cred_intV[[1]][nrow(e2010cred_intV[[1]]),];meanSD[length(meanSD)]; e2010cred_intSD[[1]][nrow(e2010cred_intSD[[1]]),]

meanM[length(meanM)]-elec[4,1];meanL[length(meanL)]-elec[4,2];meanKD[length(meanKD)]-elec[4,3]
meanC[length(meanC)]-elec[4,4];meanS[length(meanS)]-elec[4,5];meanMP[length(meanMP)]-elec[4,6]
meanV[length(meanV)]-elec[4,7];meanSD[length(meanSD)]-elec[4,8]

