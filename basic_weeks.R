library(rjags, lib="C:/Users/mirhu86/Documents/packages")
data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)
colnames(polls)[4] = 'L'

O = NULL
for(i in 1:nrow(polls)){
  O[i] = ifelse(sum(polls[i,3:10], na.rm=TRUE)==100,0, sum(polls[i,11:12], na.rm=TRUE))
}
partynames =  c("M","L","KD","C","S","MP","V","SD")
y =  polls[,partynames]
y$O = O
y$startDate = as.Date(polls$collectPeriodFrom)
y$endDate = as.Date(polls$collectPeriodTo)
y$house = polls$house
no_n = which(is.na(polls[,'n']))
y = y[-no_n,]
y$n = polls$n[-no_n]

y[,1:9] = y[,1:9]/100
df = na.omit(y[,-9])

elec = data.frame(rbind(c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                        c(0.2333, 0.0542, 0.0457, 0.0611, 0.3101, 0.0689, 0.0572, 0.1286)))
colnames(elec) = partynames
row.names(elec) = c("2006","2010","2014") #"2002",
elec$Date = c( as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14')) #as.Date("2002-09-12"),
n=c((0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000)) #(0.801*6722*1000),
## http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf
df2 = data.frame(M=elec$M, L=elec$L,C=elec$C,KD=elec$KD,S=elec$S, V=elec$V,
                 MP=elec$MP,SD=elec$SD, startDate=elec$Date, endDate=elec$Date,
                 house="Election" ,n=n)
df = rbind(df, df2)
df = df[order(df$startDate),]
orig.date = as.Date(df$startDate[1]-7)
end.date = df$endDate[length(df$endDate)]

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
Week = as.numeric(as.Date(dateSmooth)-orig.date) %/% 7
df2 = data.frame(M = pSmooth[,'M'],L = pSmooth[,'L'], KD= pSmooth[,'KD'], C = pSmooth[,'C'],
                 S = pSmooth[,'S'], V = pSmooth[,'V'], MP =  pSmooth[,'MP'], SD = pSmooth[,'SD'],
                 Date = dateSmooth.num, n = nSmooth, house = houseSmooth,Week =Week)
head(df2)
prec2 = matrix(NA,ncol=8, nrow=nrow(df2))
colnames(prec2) = colnames(df2)[1:8]
for(i in 1:8){
  for(j in 1:nrow(df2)){
    prec2[j,i] = ifelse(is.na(df2[j,i]), NA ,(1 / (df2[j,i]*(1-df2[j,i])/df2[i,'n'])))
  }
}
head(prec2)

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
y2 = as.matrix(df2[,1:8])
colnames(y2) = colnames(df2[,1:8])
all_data2 = list(y = y2, prec = prec2 , x = matrix(NA, ncol=ncol(y2), nrow=max(df2$Week)),
                 nparties=ncol(y2), npolls=nrow(y2), nperiods=max(df2$Week), day=df2$Week )
writeLines(jags_dlm,con="jags_dlm.bug")
system.time(jags_all2 <- jags.model("jags_dlm.bug", data = all_data2, n.chain=3))

ninter=10000
system.time(all_out2 <- coda.samples(jags_all2,variable.names = c("x", "eps"), n.iter = ninter, thin = 5))
sum_all2 = summary(all_out2)

out_x2 = all_out2[,which(regexpr("x", row.names(sum_all2$statistics))==1)]
out_phi2 = all_out2[,which(regexpr("eps", row.names(sum_all2$statistics))==1)]
par(mfrow=c(3,3))
for(i in 1: ncol(out_phi2[[1]])){plot(out_phi2[,i], main=colnames(y2)[i])}
par(mfrow=c(1,1))  

nperiods=max(df2$Week)
nsim = dim(out_x2[[1]])[1]*3
mean_basic2 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
ind.start = 1
ind.end = nperiods
high_basic2 = high_basic22 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
low_basic2 = low_basic22 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
percentile5 = round(nsim*0.05)
percentile95 = round(nsim*0.95)
states_basic2 = list()
for(i in 1:ncol(y2)){
  ind.start[i+1] = i*nperiods+1
  ind.end[i+1] =  ind.start[i+1]+nperiods-1
  mean_basic2[,i] = apply(rbind(out_x2[[1]][,ind.start[i]:ind.end[i]],out_x2[[2]][,ind.start[i]:ind.end[i]],out_x2[[3]][,ind.start[i]:ind.end[i]]),2,mean)
  low_basic2[,i] = apply(rbind(out_x2[[1]][,ind.start[i]:ind.end[i]],out_x2[[2]][,ind.start[i]:ind.end[i]],out_x2[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile5])
  high_basic2[,i] = apply(rbind(out_x2[[1]][,ind.start[i]:ind.end[i]],out_x2[[2]][,ind.start[i]:ind.end[i]],out_x2[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile95])
  low_basic22[,i] = mean_basic2[,i] + (1.96*apply(rbind(out_x2[[1]][,ind.start[i]:ind.end[i]],out_x2[[2]][,ind.start[i]:ind.end[i]],out_x2[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sd(x)/sqrt(length(x))))
  high_basic22[,i] = mean_basic2[,i] - (1.96*apply(rbind(out_x2[[1]][,ind.start[i]:ind.end[i]],out_x2[[2]][,ind.start[i]:ind.end[i]],out_x2[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sd(x)/sqrt(length(x))))
  states_basic2[[i]] = rbind(out_x2[[1]][,ind.start[i]:ind.end[i]],out_x2[[2]][,ind.start[i]:ind.end[i]],out_x2[[3]][,ind.start[i]:ind.end[i]])
}

colnames(mean_basic2) = colnames(y2)

##################################################################
############# POSTERIOR PREDICTIVE CHECKING ######################
##################################################################


rChain = list()
rChain[[colnames(y2)[1]]] = states_basic2[[1]];rChain[[colnames(y2)[2]]] = states_basic2[[2]];rChain[[colnames(y2)[3]]] = states_basic2[[3]]
rChain[[colnames(y2)[4]]] = states_basic2[[4]];rChain[[colnames(y2)[5]]] = states_basic2[[5]];rChain[[colnames(y2)[6]]] = states_basic2[[6]]
rChain[[colnames(y2)[7]]] = states_basic2[[7]];rChain[[colnames(y2)[8]]] = states_basic2[[8]]
str(rChain)

df3 = df[,colnames(y2)]
df3$startDate = df$startDate
df3$endDate = df$endDate
df3$n = df$n
df3$Date = df$Date

dayssinceorigStart = julian(df3$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df3$endDate, origin=orig.date) 
df3$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
prec=matrix(NA,ncol=8, nrow=nrow(df3))
df3$Week = as.numeric(as.Date(df3$startDate)-orig.date) %/% 7
colnames(prec) = colnames(df3)[1:8]

for(i in 1:8){
  for(j in 1:nrow(df3)){
    prec[j,i] = ifelse(is.na(df3[j,i]), NA ,((df3[j,i]*(1-df3[j,i])/df3[i,'n'])))
  }
}
colnames(prec) = colnames(df3[,1:8])
yrepM = yrepL = yrepKD = yrepC = yrepS = yrepMP = yrepV = yrepSD = matrix(NA, nrow=dim(rChain[[1]])[1], ncol=nrow(df3))


for(i in 1:dim(rChain[[1]])[1]){
  for(j in 1:nrow(df3)){
    yrepM[i,j] = rnorm(1, rChain[['M']][i,df3$Week[j]], prec[j,'M'])
    yrepL[i,j] = rnorm(1, rChain[['L']][i,df3$Week[j]], prec[j,'L'])
    yrepKD[i,j] = rnorm(1, rChain[['KD']][i,df3$Week[j]], prec[j,'KD'])
    yrepC[i,j] = rnorm(1, rChain[['C']][i,df3$Week[j]], prec[j,'C'])
    yrepS[i,j] = rnorm(1, rChain[['S']][i,df3$Week[j]], prec[j,'S'])
    yrepMP[i,j] = rnorm(1, rChain[['MP']][i,df3$Week[j]], prec[j,'MP'])
    yrepV[i,j] = rnorm(1, rChain[['V']][i,df3$Week[j]], prec[j,'V'])
    yrepSD[i,j] = rnorm(1, rChain[['SD']][i,df3$Week[j]], prec[j,'SD'])
  }
}

yreps = list(M=yrepM,L=yrepL,KD=yrepKD,C=yrepC,S=yrepS,MP=yrepMP,V=yrepV,SD=yrepSD)
bayespval = matrix(NA,nrow=8, ncol=6)
rownames(bayespval) = partynames
colnames(bayespval) = c("Min","Max","Mean","Var","Neg.val","Above1")
for(i in partynames){
  bayespval[i,1] = sum(ifelse(apply(yreps[[i]],1,min)>min(df3[,i]),1,0))/nsim
  bayespval[i,2] = sum(ifelse(apply(yreps[[i]],1,max)>max(df3[,i]),1,0))/nsim
  bayespval[i,3] = sum(ifelse(apply(yreps[[i]],1,mean)>mean(df3[,i]),1,0))/nsim
  bayespval[i,4] = sum(ifelse(apply(yreps[[i]],1,var)>var(df3[,i]),1,0))/nsim
  bayespval[i,5] = sum(ifelse(yreps[[i]]<0,1,0))/(dim(yreps[[i]])[1]*dim(yreps[[i]])[2])
  bayespval[i,6] = sum(ifelse(yreps[[i]]>1,1,0))/(dim(yreps[[i]])[1]*dim(yreps[[i]])[2])
}
bayespval

dat_low2 = matrix(NA, ncol=8, nrow=nrow(df3))
dat_high2 = matrix(NA, ncol=8, nrow=nrow(df3))
colnames(dat_low2) = colnames(dat_high2) = partynames
for(i in partynames){
  dat_low2[,i] = apply(yreps[[i]],2,function(x) sort(x)[percentile5])
  dat_high2[,i] = apply(yreps[[i]],2,function(x) sort(x)[percentile95])
}
colnames(dat_high2) = colnames(dat_low2) = partynames
dat_high2 = dat_high2[,colnames(mean_basic2)]
dat_low2 = dat_low2[,colnames(mean_basic2)]
#################################################
################# PLOTS #########################
#################################################

basic_plot2 = list()

head(y2)
df3$house = df$house
head(df3)
cols = c("blue","lightblue3","darkblue","chartreuse3","red","darkred","forestgreen","skyblue3")
library(ggplot2)
for(i in 1:ncol(mean_basic2)){
  
  plot_df = data.frame(party = mean_basic2[,i] ,  low=low_basic2[,i]*100, high=high_basic2[,i]*100,
                       time=seq(orig.date,by='weeks', length=nperiods), party2 = rep(colnames(y2)[i], nperiods))
  points = data.frame(x=seq(orig.date,by='days',length=as.numeric(end.date-orig.date))[df3$Date], 
                      y=df3[,i]*100, house=df3$house, party=rep(colnames(y)[i],length(df3$Date[length(df3$Date)][df3$Date])),
                      high_dat=dat_high2[,i]*100, low_dat=dat_low2[,i]*100 )
  basic_plot2[[i]] <-  ggplot(plot_df) +
    aes(x = time, y = party*100) +
    geom_line(col=cols[i], alpha=1)  +
    geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill=cols[i]) + 
    geom_ribbon(data=points, aes(x=x, ymin=low_dat, ymax=high_dat), alpha=0.5, fill=cols[i], inherit.aes = FALSE) +
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


multiplot(basic_plot2[[1]], basic_plot2[[2]], basic_plot2[[3]], basic_plot2[[4]], 
          basic_plot2[[5]], basic_plot2[[6]], basic_plot2[[7]], basic_plot2[[8]], cols=2)

####################################################
############ PREDICT 2010 ELECTION #################
####################################################
data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)
colnames(polls)[4] = 'L'

O = NULL
for(i in 1:nrow(polls)){
  O[i] = ifelse(sum(polls[i,3:10], na.rm=TRUE)==100,0, sum(polls[i,11:12], na.rm=TRUE))
}
partynames =  c("M","L","KD","C","S","MP","V","SD")
y =  polls[,partynames]
y$O = O
y$startDate = as.Date(polls$collectPeriodFrom)
y$endDate = as.Date(polls$collectPeriodTo)
y$house = polls$house
no_n = which(is.na(polls[,'n']))
y = y[-no_n,]
y$n = polls$n[-no_n]

y[,1:9] = y[,1:9]/100
df = na.omit(y[,-9])

elec = data.frame(rbind(c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                        c(0.2333, 0.0542, 0.0457, 0.0611, 0.3101, 0.0689, 0.0572, 0.1286)))
colnames(elec) = partynames
row.names(elec) = c("2006","2010","2014") #"2002",
elec$Date = c( as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14')) #as.Date("2002-09-12"),
n=c((0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000)) #(0.801*6722*1000),
## http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf
df2 = data.frame(M=elec$M, L=elec$L,C=elec$C,KD=elec$KD,S=elec$S, V=elec$V,
                 MP=elec$MP,SD=elec$SD, startDate=elec$Date, endDate=elec$Date,
                 house="Election" ,n=n)
df = rbind(df, df2)
df = df[order(df$startDate),]
orig.date = df$startDate[1]-7
end.date = elec$Date[2]
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
df = df[-which(df$endDate>=end.date),]
tail(df)


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
Week = as.numeric(as.Date(dateSmooth)-orig.date) %/% 7
df2 = data.frame(M = pSmooth[,'M'],L = pSmooth[,'L'], KD= pSmooth[,'KD'], C = pSmooth[,'C'],
                 S = pSmooth[,'S'], V = pSmooth[,'V'], MP =  pSmooth[,'MP'], SD = pSmooth[,'SD'],
                 Date = dateSmooth.num, n = nSmooth, house = houseSmooth, Week=Week)


prec2 = matrix(NA,ncol=8, nrow=nrow(df2))
colnames(prec2) = colnames(df2)[1:8]
for(i in 1:8){
  for(j in 1:nrow(df2)){
    prec2[j,i] = ifelse(is.na(df2[j,i]), NA ,(1 / (df2[j,i]*(1-df2[j,i])/df2[i,'n'])))
  }
}


y2 = as.matrix(df2[,1:8])
all_data22010 = list(y = y2, prec = prec2 , x = matrix(NA, ncol=ncol(y2), nrow=max(df2$Week)),
                     nparties=ncol(y2), npolls=nrow(y2), nperiods=max(df2$Week), 
                     phi = NULL, day=df2$Week )
writeLines(jags_dlm,con="jags_dlm.bug")
system.time(jags_all2010<- jags.model("jags_dlm.bug", data = all_data22010, n.chain=3))

ninter = 10000
system.time(all_out22010 <- coda.samples(jags_all2010,variable.names = c("x", "eps"), n.iter = ninter, thin = 5))
sum_all22010 = summary(all_out22010)
out_x22010 = all_out22010[,which(regexpr("x", row.names(sum_all22010$statistics))==1)]
out_phi22010 = all_out22010[,which(regexpr("eps", row.names(sum_all22010$statistics))==1)]
par(mfrow=c(3,3))
for(i in 1: ncol(out_phi22010[[1]])){plot(out_phi22010[,i])}
par(mfrow=c(1,1))  

nperiods=max(df2$Week)
nsim = dim(out_x22010[[1]])[1]*3
mean_basic22010 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
ind.start = 1
ind.end = nperiods
high_basic22010 = high_basic222010 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
low_basic22010 = low_basic222010 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
percentile5 = round(nsim*0.05)
percentile95 = round(nsim*0.95)
states_basic22010= list()
colnames(mean_basic22010) = colnames(low_basic22010) =colnames(high_basic22010) = partynames
for(i in 1:ncol(y2)){
  ind.start[i+1] = i*nperiods+1
  ind.end[i+1] =  ind.start[i+1]+nperiods-1
  mean_basic22010[,i] = apply(rbind(out_x22010[[1]][,ind.start[i]:ind.end[i]],out_x22010[[2]][,ind.start[i]:ind.end[i]],out_x22010[[3]][,ind.start[i]:ind.end[i]]),2,mean)
  low_basic22010[,i] = apply(rbind(out_x22010[[1]][,ind.start[i]:ind.end[i]],out_x22010[[2]][,ind.start[i]:ind.end[i]],out_x22010[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile5])
  high_basic22010[,i] = apply(rbind(out_x22010[[1]][,ind.start[i]:ind.end[i]],out_x22010[[2]][,ind.start[i]:ind.end[i]],out_x22010[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile95])
}


pred22010 = matrix(NA, ncol=5, nrow=8)
row.names(pred22010) = colnames(df2)[1:8]
colnames(pred22010) = c("Elec_res","MAP","Low","High","Diff")
elec2 = elec[,colnames(df2)[1:8]]
for(i in 1:ncol(y2)){
  pred22010[i,] =  cbind(elec2[2,i],mean_basic22010[nrow(mean_basic22010),i],low_basic22010[nrow(low_basic22010),i],high_basic22010[nrow(high_basic22010),i],(mean_basic22010[nrow(mean_basic22010),i]-elec2[2,i]))
}
pred22010


####################################################
############ PREDICT 2014 ELECTION #################
####################################################

O = NULL
for(i in 1:nrow(polls)){
  O[i] = ifelse(sum(polls[i,3:10], na.rm=TRUE)==100,0, sum(polls[i,11:12], na.rm=TRUE))
}
y =  polls[,partynames]
y$O = O
y$startDate = as.Date(polls$collectPeriodFrom)
y$endDate = as.Date(polls$collectPeriodTo)
y$house = polls$house
no_n = which(is.na(polls[,'n']))
y = y[-no_n,]
y$n = polls$n[-no_n]

y[,1:9] = y[,1:9]/100
df = na.omit(y[,-9])
elec = data.frame(rbind(c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                        c(0.2333, 0.0542, 0.0457, 0.0611, 0.3101, 0.0689, 0.0572, 0.1286)))
colnames(elec) = partynames
row.names(elec) = c("2006","2010","2014") #"2002",
elec$Date = c( as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14')) #as.Date("2002-09-12"),
n=c((0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000)) #(0.801*6722*1000),
## http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf
df2 = data.frame(M=elec$M, L=elec$L,C=elec$C,KD=elec$KD,S=elec$S, V=elec$V,
                 MP=elec$MP,SD=elec$SD, startDate=elec$Date, endDate=elec$Date,
                 house="Election" ,n=n)
df = rbind(df, df2)
df = df[order(df$startDate),]
orig.date = df$startDate[1]-7
end.date = elec$Date[3]
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
df = df[-which(df$endDate>=end.date),]
tail(df)

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
Week = as.numeric(as.Date(dateSmooth)-orig.date) %/% 7
df2 = data.frame(M = pSmooth[,'M'],L = pSmooth[,'L'], KD= pSmooth[,'KD'], C = pSmooth[,'C'],
                 S = pSmooth[,'S'], V = pSmooth[,'V'], MP =  pSmooth[,'MP'], SD = pSmooth[,'SD'],
                 Date = dateSmooth.num, n = nSmooth, house = houseSmooth, Week=Week)
prec2 = matrix(NA,ncol=8, nrow=nrow(df2))
colnames(prec2) = colnames(df2)[1:8]
for(i in 1:8){
  for(j in 1:nrow(df2)){
    prec2[j,i] = ifelse(is.na(df2[j,i]), NA ,(1 / (df2[j,i]*(1-df2[j,i])/df2[i,'n'])))
  }
}

y2 = as.matrix(df2[,1:8])
all_data22014 = list(y = y2, prec = prec2 , x = matrix(NA, ncol=ncol(y2), nrow=max(df2$Week)),
                     nparties=ncol(y2), npolls=nrow(y2), nperiods=max(df2$Week), 
                     phi = NULL, day=df2$Week)
writeLines(jags_dlm,con="jags_dlm.bug")
system.time(jags_all22014<- jags.model("jags_dlm.bug", data = all_data22014, n.chain=3))
system.time(all_out22014 <- coda.samples(jags_all22014,variable.names = c("x", "eps"), n.iter = ninter, thin = 5))
sum_all22014 = summary(all_out22014)
out_x22014 = all_out22014[,which(regexpr("x", row.names(sum_all22014$statistics))==1)]
out_phi22014 = all_out22014[,which(regexpr("eps", row.names(sum_all22014$statistics))==1)]
par(mfrow=c(3,3))
for(i in 1: ncol(out_phi22014[[1]])){plot(out_phi22014[,i])}
par(mfrow=c(1,1))  

nperiods=max(df2$Week)
nsim = dim(out_x2[[1]])[1]*3
mean_basic22014 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
ind.start = 1
ind.end = nperiods
high_basic22014 = high_basic222014 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
low_basic22014 = low_basic222014 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
percentile5 = round(nsim*0.05)
percentile95 = round(nsim*0.95)
states_basic22014= list()
colnames(mean_basic22014) = colnames(low_basic22014) =colnames(high_basic22014) = partynames
for(i in 1:ncol(y2)){
  ind.start[i+1] = i*nperiods+1
  ind.end[i+1] =  ind.start[i+1]+nperiods-1
  mean_basic22014[,i] = apply(rbind(out_x22014[[1]][,ind.start[i]:ind.end[i]],out_x22014[[2]][,ind.start[i]:ind.end[i]],out_x22014[[3]][,ind.start[i]:ind.end[i]]),2,mean)
  low_basic22014[,i] = apply(rbind(out_x22014[[1]][,ind.start[i]:ind.end[i]],out_x22014[[2]][,ind.start[i]:ind.end[i]],out_x22014[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile5])
  high_basic22014[,i] = apply(rbind(out_x22014[[1]][,ind.start[i]:ind.end[i]],out_x22014[[2]][,ind.start[i]:ind.end[i]],out_x22014[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile95])
}


pred22014 = matrix(NA, ncol=5, nrow=8)
row.names(pred22014) = colnames(df2)[1:8]
colnames(pred22014) = c("Elec_res","MAP","Low","High","Diff")
elec2 = elec[,colnames(df2)[1:8]]
for(i in 1:ncol(y2)){
  pred22014[i,] =  cbind(elec2[3,i],mean_basic22014[nrow(mean_basic22014),i],low_basic22014[nrow(low_basic22014),i],high_basic22014[nrow(high_basic22014),i],(mean_basic22014[nrow(mean_basic22014),i]-elec2[3,i]))
}
pred22014

