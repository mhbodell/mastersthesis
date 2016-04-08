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

colnames(elec) = partynames =c("M","L","KD","C","S","MP","V","SD")
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
head(df2)


prec2 = matrix(NA,ncol=8, nrow=nrow(df2))
colnames(prec2) = colnames(df2)[1:8]
for(i in 1:8){
  for(j in 1:nrow(df2)){
    prec2[j,i] = ifelse(is.na(df2[j,i]), NA ,(1 / (df2[j,i]*(1-df2[j,i])/df2[i,'n'])))
  }
}

jags_multhouse ='
model{
#measurement model
for(j in 1:nparties){
  for(i in 1:npolls){
    y[i,j] ~ dnorm(x[day[i],j], house[j,org[i]]*prec[i,j])
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
x[1,i] ~ dnorm(init.m[i], init.v[i])
eps[i] ~ dgamma(1,1)
phi[i] <- 1/eps[i]
ee[i, 12] <- 1
house[i,12] <- 1/ee[i,12]
for(k in 1:(nhouses-1)){
ee[i,k] ~ dgamma(1, 1)
house[i,k] <- 1/ee[i,k]
}
}

}
'
y = as.matrix(df2[,1:8])
#factor(df$house,levels(df$house)[c(2,1,3,4,5,6,7,8,9,10,11,12)])
data_multhouse = list(y = y, prec = prec2, x = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date)),
                     nparties = ncol(y), day = df2$Date, npolls = nrow(df2), nperiods = as.numeric(end.date - orig.date),
                     nhouses = length(levels(as.factor(df2$house))), org=as.numeric(as.factor(df2$house)),
                     house = matrix(NA,ncol=length(levels(as.factor(df2$house))), nrow=ncol(y)))
writeLines(jags_multhouse,con="jags_multhouse.bug")
system.time(jags_multhouse <- jags.model("jags_multhouse.bug", data = data_multhouse, n.chain=3))


ninter=10000
system.time(mult_out <- coda.samples(jags_multhouse,variable.names = c("x", "phi", "house"), n.iter = ninter, thin = 5))
system.time(sum_mult <- summary(mult_out))
multout_x = mult_out[,which(regexpr("x", row.names(sum_mult$statistics))==1)]
multsum_x = sum_mult$statistics[which(regexpr("x", row.names(sum_mult$statistics))==1),]
multout_phi = mult_out[,which(regexpr("phi", row.names(sum_mult$statistics))==1)]
par(mfrow=c(3,3))
for(i in 1: ncol(multout_phi[[1]])){plot(multout_phi[,i])}
par(mfrow=c(1,1))  

multhouse = mult_out[,which(regexpr("house", row.names(sum_mult$statistics))==1)]
map_house = matrix(NA, ncol=length(levels(as.factor(df$house))), nrow=ncol(y))
j=1
for(i in seq(1,dim(multhouse[[1]])[2],8)){
  map_house[,j] = apply(as.matrix(multhouse[,seq(i,i+7,1)]),2,mean)
  j = j+1
}
colnames(map_house) = levels(as.factor(df$house))
row.names(map_house) = colnames(y)
map_house

mean_mult = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
ind.start = 1
ind.end = as.numeric(end.date-orig.date)
high_mult = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
low_mult = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
states_mult = list()
for(i in 1:ncol(y)){
  ind.start[i+1] = i*as.numeric(end.date-orig.date)+1
  ind.end[i+1] =  ind.start[i+1]+as.numeric(end.date-orig.date)-1
  mean_mult[,i] = multsum_x[ind.start[i]:ind.end[i],1]
  low_mult[,i] = mean_mult[,i] - 1.96 * multsum_x[ind.start[i]:ind.end[i],2]
  high_mult[,i] = mean_mult[,i] + 1.96 * multsum_x[ind.start[i]:ind.end[i],2]
  states_mult[[i]] = multout_x[,ind.start[i]:ind.end[i]]
}


##################################################################
############# POSTERIOR PREDICTIVE CHECKING ######################
##################################################################

set.seed(901207)
i = sample(1:3,1)
rChain = list()
rChain[[1]] = states_mult[[1]][i];rChain[[2]] = states_mult[[2]][i];rChain[[3]] = states_mult[[3]][i]
rChain[[4]] = states_mult[[4]][i];rChain[[5]] = states_mult[[5]][i];rChain[[6]] = states_mult[[6]][i]
rChain[[7]] = states_mult[[7]][i];rChain[[8]] = states_mult[[8]][i]
str(rChain)

he = matrix(NA, ncol=ncol(prec2),nrow=nrow(df))
for(k in 1:ncol(prec2)){
  for(i in 1:length(colnames(map_house))){
    ins = colnames(map_house)[i]
    for(j in 1:nrow(df)){
      if(df[j,'house']==ins){
        he[j,k] = map_house[k,i]
      } else{}
    }
  }
}

df3 = df[,colnames(y)]
df3$startDate = df$startDate
df3$endDate = df$endDate
df3$n = df$n
df3$Date = df$Date
df3$house = df$house

dayssinceorigStart = julian(df3$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df3$endDate, origin=orig.date) 
df3$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)

prec = matrix(NA,ncol=8, nrow=nrow(df3))
colnames(prec) = colnames(df3)[1:8]
for(i in 1:8){
  for(j in 1:nrow(df3)){
    prec[j,i] = ifelse(is.na(df3[j,i]), NA ,(1 / (df3[j,i]*(1-df2[j,i])/df3[i,'n'])))
  }
}


testMin_mult = matrix(NA,ncol=8, nrow=100)
testMax_mult = matrix(NA,ncol=8, nrow=100)
testMean_mult = matrix(NA,ncol=8, nrow=100)
testVar_mult = matrix(NA,ncol=8, nrow=100)
negval = matrix(NA,ncol=8, nrow=100)
above1 = matrix(NA,ncol=8, nrow=100)
colnames(testMin_mult) = colnames(testMax_mult) = colnames(testMean_mult) = partynames
nsim = dim(mult_out[[1]])[1]
for(i in 1:100){
  for(j in 1:ncol(y)){
    yrep_mult = sapply(1:nsim, function(s) rnorm(length(df3$Date),unlist(rChain[[j]][s,df3$Date]), 1/(prec[,j])*he[,j])))
    min_rep = apply(yrep_mult,2,min)
    testMin_mult[i,j] = sum(ifelse(min_rep>= min(df[,j]),1,0))/length(min_rep) 
    max_rep = apply(yrep_mult,2,max)
    testMax_mult[i,j] = sum(ifelse(max_rep>= max(df[,j]),1,0))/length(max_rep)
    mean_rep = apply(yrep_mult,2,mean)
    testMean_mult[i,j] = sum(ifelse(mean_rep>= mean(df[,j]),1,0))/length(mean_rep)
    var_rep = apply(yrep_mult,2,var)
    testVar_mult[i,j] = sum(ifelse(var_rep>= var(df[,j]),1,0))/length(var_rep)
    negval[i,j] = sum(ifelse(yrep_mult<0,1,0))/(dim(yrep_mult)[1]*dim(yrep_mult)[2])
    above1[i,j] = sum(ifelse(yrep_mult>1,1,0))/(dim(yrep_mult)[1]*dim(yrep_mult)[2])
  }
}

for(i in 1:ncol(y)){
  print(paste(colnames(testMin_mult)[i],":"))
  print(paste("Min:",mean(testMin_mult[,i]), sep=" "))
  print(paste("Max:",mean(testMax_mult[,i]), sep=" "))
  print(paste("Mean:",mean(testMean_mult[,i]), sep=" "))
  print(paste("Var:",mean(testVar_mult[,i]), sep=" "))
  print(paste("Neg numb:",mean(negval[,i]), sep=" "))
  print(paste("Above 1:",mean(above1[,i]), sep=" "))
}

dat_cb = list()
for(i in 1:ncol(y)){
  dat_cb[[i]] = sapply(1:nsim, function(s) rnorm(length(df3$Date),unlist(rChain[[i]][s,df3$Date]), (1/prec[,i])*he[,i]))
}
str(dat_cb)




dat_low = matrix(NA, ncol=ncol(y), nrow=nrow(df))
dat_high = matrix(NA, ncol=ncol(y), nrow=nrow(df))
for(i in 1:ncol(y)){
  dat_low[,i] = apply(dat_cb[[i]], 1, mean) - (apply(dat_cb[[i]], 1, sd)*1.96)
  dat_high[,i] = apply(dat_cb[[i]], 1, mean) + (apply(dat_cb[[i]], 1, sd)*1.96)
}



#################################################
################# PLOTS #########################
#################################################


mult_plot = list()

y = as.matrix(df[,1:8])
head(y)
cols = c("blue","lightblue3","chartreuse3","darkblue","red","darkred","forestgreen","skyblue3")
library(ggplot2)
for(i in 1:ncol(mean_mult)){
  
  plot_df = data.frame(party = mean_mult[,i] ,  low=low_mult[,i]*100, high=high_mult[,i]*100, time=seq(orig.date,by='days',
                                                                                                    length=as.numeric(end.date-orig.date)), party2 = rep(colnames(y)[i], as.numeric(end.date-orig.date)))
  points = data.frame(x=seq(orig.date,by='days',length=as.numeric(end.date-orig.date))[df$Date], 
                      y=df[,i]*100, house=df$house, party=rep(colnames(y)[i],length(df$Date[length(df$Date)][df$Date])),
                      high_dat=dat_high[,i]*100, low_dat=dat_low[,i]*100)
  mult_plot[[i]] <- ggplot(plot_df) +
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


multiplot(mult_plot[[1]], mult_plot[[2]], mult_plot[[3]], mult_plot[[4]], 
          mult_plot[[5]], mult_plot[[6]], mult_plot[[7]], mult_plot[[8]], cols=2)


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
end.date = as.Date(elec$Date[2])
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
df = df[-which(df$Date>=as.numeric(end.date-orig.date)),]
head(df)


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
head(df2)


prec2 = matrix(NA,ncol=8, nrow=nrow(df2))
colnames(prec2) = colnames(df2)[1:8]
for(i in 1:8){
  for(j in 1:nrow(df2)){
    prec2[j,i] = ifelse(is.na(df2[j,i]), NA ,(1 / (df2[j,i]*(1-df2[j,i])/df2[i,'n'])))
  }
}


y = as.matrix(df2[,1:8])
#factor(df$house,levels(df$house)[c(2,1,3,4,5,6,7,8,9,10,11,12)])
jags_multhouse ='
model{
#measurement model
for(j in 1:nparties){
for(i in 1:npolls){
y[i,j] ~ dnorm(x[day[i],j], house[j,org[i]]*prec[i,j])
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
x[1,i] ~ dnorm(init.m[i], init.v[i])
eps[i] ~ dgamma(1,1)
phi[i] <- 1/eps[i]
ee[i, 12] <- 1
house[i,12] <- 1/ee[i,12]
for(k in 1:(nhouses-1)){
ee[i,k] ~ dgamma(1, 1)
house[i,k] <- 1/ee[i,k]
}
}

}
'
writeLines(jags_multhouse,con="jags_multhouse.bug")
data_multhouse_2010 = list(y = y, prec = prec2, x = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date)),
                          nparties = ncol(y), day = df2$Date, npolls = nrow(df2), nperiods = as.numeric(end.date - orig.date),
                          nhouses = length(levels(as.factor(df2$house))), org=as.numeric(as.factor(df2$house)),
                          house = matrix(NA,ncol=length(levels(as.factor(df2$house))), nrow=ncol(y)))

system.time(jags_multhouse_2010 <- jags.model("jags_multhouse.bug", data = data_multhouse_2010, n.chain=3))


ninter=10000
system.time(mult_out_2010 <- coda.samples(jags_multhouse_2010,variable.names = c("x", "phi", "house"), n.iter = ninter, thin = 5, burnin=2000))
system.time(sum_mult_2010 <- summary(mult_out_2010))
multout_x_2010 = mult_out_2010[,which(regexpr("x", row.names(sum_mult_2010$statistics))==1)]
multsum_x2010 = sum_mult_2010$statistics[which(regexpr("x", row.names(sum_mult_2010$statistics))==1),]
mean_mult2010 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
ind.start = 1
ind.end = as.numeric(end.date-orig.date)
high_mult2010 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
low_mult2010 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
states_mult2010 = list()
for(i in 1:ncol(y)){
  ind.start[i+1] = i*as.numeric(end.date-orig.date)+1
  ind.end[i+1] =  ind.start[i+1]+as.numeric(end.date-orig.date)-1
  mean_mult2010[,i] = multsum_x2010[ind.start[i]:ind.end[i],1]
  low_mult2010[,i] = mean_mult2010[,i] - 1.96 * multsum_x2010[ind.start[i]:ind.end[i],2]
  high_mult2010[,i] = mean_mult2010[,i] + 1.96 * multsum_x2010[ind.start[i]:ind.end[i],2]
  states_mult2010[[i]] = multout_x_2010[,ind.start[i]:ind.end[i]]
}

predmult2010 = matrix(NA, nrow=ncol(y), ncol=5)
for(i in 1:ncol(y)){
  predmult2010[i,] = c(elec[2,i],mean_mult2010[nrow(mean_mult2010),i],low_mult2010[nrow(mean_mult2010),i],high_mult2010[nrow(mean_mult2010),i],mean_mult2010[nrow(mean_mult2010),i]-elec[2,i])
}
colnames(predmult2010) = c("Elec res","MAP","Low", "High", "Diff")
row.names(predmult2010) = colnames(y)
predmult2010


####################################################
############ PREDICT 2014 ELECTION #################
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
end.date = as.Date(elec$Date[3])
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
df = df[-which(df$Date>=as.numeric(end.date-orig.date)),]
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
df2 = data.frame(M = pSmooth[,'M'],L = pSmooth[,'L'], KD= pSmooth[,'KD'], C = pSmooth[,'C'],
                 S = pSmooth[,'S'], V = pSmooth[,'V'], MP =  pSmooth[,'MP'], SD = pSmooth[,'SD'],
                 Date = dateSmooth.num, n = nSmooth, house = houseSmooth)
head(df2)


prec2 = matrix(NA,ncol=8, nrow=nrow(df2))
colnames(prec2) = colnames(df2)[1:8]
for(i in 1:8){
  for(j in 1:nrow(df2)){
    prec2[j,i] = ifelse(is.na(df2[j,i]), NA ,(1 / (df2[j,i]*(1-df2[j,i])/df2[i,'n'])))
  }
}

jags_multhouse ='
model{
#measurement model
for(j in 1:nparties){
for(i in 1:npolls){
y[i,j] ~ dnorm(x[day[i],j], house[j,org[i]]*prec[i,j])
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
x[1,i] ~ dnorm(init.m[i], init.v[i])
eps[i] ~ dgamma(1,1)
phi[i] <- 1/eps[i]
ee[i, 12] <- 1
house[i,12] <- 1/ee[i,12]
for(k in 1:(nhouses-1)){
ee[i,k] ~ dgamma(1, 1)
house[i,k] <- 1/ee[i,k]
}
}

}
'
y = as.matrix(df2[,1:8])
writeLines(jags_multhouse,con="jags_multhouse.bug")
data_multhouse_2014 = list(y = y, prec = prec2, x = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date)),
                          nparties = ncol(y), day = df2$Date, npolls = nrow(df2), nperiods = as.numeric(end.date - orig.date),
                          nhouses = length(levels(as.factor(df2$house))), org=as.numeric(as.factor(df2$house)),
                          house = matrix(NA,ncol=length(levels(as.factor(df2$house))), nrow=ncol(y)))

system.time(jags_multhouse_2014 <- jags.model("jags_multhouse.bug", data = data_multhouse_2014, n.chain=3))


ninter=10000
system.time(mult_out_2014 <- coda.samples(jags_multhouse_2014,variable.names = c("x", "phi", "house"), n.iter = ninter, thin = 5, burnin=2000))
system.time(sum_mult_2014 <- summary(mult_out_2014))
multout_x_2014 = mult_out_2014[,which(regexpr("x", row.names(sum_mult_2014$statistics))==1)]
multsum_x2014 = sum_mult_2014$statistics[which(regexpr("x", row.names(sum_mult_2014$statistics))==1),]
mean_mult2014 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
ind.start = 1
ind.end = as.numeric(end.date-orig.date)
high_mult2014 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
low_mult2014 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
states_mult2014 = list()
for(i in 1:ncol(y)){
  ind.start[i+1] = i*as.numeric(end.date-orig.date)+1
  ind.end[i+1] =  ind.start[i+1]+as.numeric(end.date-orig.date)-1
  mean_mult2014[,i] = multsum_x2014[ind.start[i]:ind.end[i],1]
  low_mult2014[,i] = mean_mult2014[,i] - (1.96 * multsum_x2014[ind.start[i]:ind.end[i],2])
  high_mult2014[,i] = mean_mult2014[,i] + (1.96 * multsum_x2014[ind.start[i]:ind.end[i],2])
  states_mult2014[[i]] = multout_x_2014[,ind.start[i]:ind.end[i]]
}

predmult2014 = matrix(NA, nrow=ncol(y), ncol=5)
for(i in 1:ncol(y)){
  predmult2014[i,] = c(elec[3,i],mean_mult2014[nrow(mean_mult2014),i],low_mult2014[nrow(mean_mult2014),i],high_mult2014[nrow(mean_mult2014),i],mean_mult2014[nrow(mean_mult2014),i]-elec[3,i])
}
colnames(predmult2014) = c("Elec res","MAP","Low", "High", "Diff")
row.names(predmult2014) = colnames(y)
predmult2014
