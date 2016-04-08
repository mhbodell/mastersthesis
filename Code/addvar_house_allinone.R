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



jags_addhouse ='
model{
#measurement model
for(j in 1:nparties){
for(i in 1:npolls){
y[i,j] ~ dnorm(x[day[i],j], prec[i,j]+house[j,org[i]])
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
house[i,12] <- 0
for(k in 1:(nhouses-1)){
house[i,k] ~ dnorm(1, 1)
}
}

}
'

y = as.matrix(df2[,1:8])
#factor(df$house,levels(df$house)[c(2,1,3,4,5,6,7,8,9,10,11,12)])
data_addhouse = list(y = y, prec = prec2, x = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date)),
                     nparties = ncol(y), day = df2$Date, npolls = nrow(df2), nperiods = as.numeric(end.date - orig.date),
                     nhouses = length(levels(as.factor(df2$house))), org=as.numeric(as.factor(df2$house)),
                     house = matrix(NA,ncol=length(levels(as.factor(df2$house))), nrow=ncol(y)))
writeLines(jags_addhouse,con="jags_addhouse.bug")
system.time(jags_addhouse <- jags.model("jags_addhouse.bug", data = data_addhouse, n.chain=3))


ninter=10000
system.time(add_out <- coda.samples(jags_addhouse,variable.names = c("x", "phi", "house"), n.iter = ninter, thin = 5))
system.time(sum_add <- summary(add_out))
addout_x = add_out[,which(regexpr("x", row.names(sum_add$statistics))==1)]
addsum_x = sum_add$statistics[which(regexpr("x", row.names(sum_add$statistics))==1),]
addout_phi = add_out[,which(regexpr("phi", row.names(sum_add$statistics))==1)]
par(mfrow=c(3,3))
for(i in 1: ncol(addout_phi[[1]])){plot(addout_phi[,i])}
par(mfrow=c(1,1))  

addhouse = add_out[,which(regexpr("house", row.names(sum_add$statistics))==1)]
map_house = matrix(NA, ncol=length(levels(as.factor(df2$house))), nrow=ncol(y))
j=1
for(i in seq(1,dim(addhouse[[1]])[2],8)){
  map_house[,j] = apply(as.matrix(addhouse[,seq(i,i+7,1)]),2,mean)
  j = j+1
}
colnames(map_house) = levels(as.factor(df2$house))
row.names(map_house) = colnames(y)
map_house

mean_add = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
ind.start = 1
ind.end = as.numeric(end.date-orig.date)
high_add = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
low_add = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
states_add = list()
for(i in 1:ncol(y)){
  ind.start[i+1] = i*as.numeric(end.date-orig.date)+1
  ind.end[i+1] =  ind.start[i+1]+as.numeric(end.date-orig.date)-1
  mean_add[,i] = addsum_x[ind.start[i]:ind.end[i],1]
  low_add[,i] = mean_add[,i] - 1.96 * addsum_x[ind.start[i]:ind.end[i],2]
  high_add[,i] = mean_add[,i] + 1.96 * addsum_x[ind.start[i]:ind.end[i],2]
  states_add[[i]] = addout_x[,ind.start[i]:ind.end[i]]
}


##################################################################
############# POSTERIOR PREDICTIVE CHECKING ######################
##################################################################

set.seed(901207)
i = sample(1:3,1)
rChain = list()
rChain[[1]] = states_add[[1]][i];rChain[[2]] = states_add[[2]][i];rChain[[3]] = states_add[[3]][i]
rChain[[4]] = states_add[[4]][i];rChain[[5]] = states_add[[5]][i];rChain[[6]] = states_add[[6]][i]
rChain[[7]] = states_add[[7]][i];rChain[[8]] = states_add[[8]][i]

testMin_add = matrix(NA,ncol=8, nrow=100)
testMax_add = matrix(NA,ncol=8, nrow=100)
testMean_add = matrix(NA,ncol=8, nrow=100)
testVar_add = matrix(NA,ncol=8, nrow=100)
neg.numb_add =  matrix(NA,ncol=8, nrow=100)
above1_add =  matrix(NA,ncol=8, nrow=100)

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

prec = matrix(NA,ncol=8, nrow=nrow(df))
colnames(prec) = colnames(df3)[1:8]
for(i in 1:8){
  for(j in 1:nrow(df)){
    prec[j,i] = ifelse(is.na(df3[j,i]), NA ,(1 / (df3[j,i]*(1-df3[j,i])/df3[i,'n'])))
  }
}


colnames(testMin_add) = colnames(testMax_add) = colnames(testMean_add) =colnames(neg.numb_add) = colnames(above1_add)=  c("M","L","KD","C","S","MP","V","SD") 
nsim = dim(add_out[[1]])[1]
for(i in 1:100){
  for(j in 1:ncol(y)){
    yrep_add = sapply(1:nsim, function(s) rnorm(length(df3$Date),unlist(rChain[[j]][s,df3$Date]), 1/prec[,j]+1/he[,j]))
    min_rep = apply(yrep_add,2,min)
    testMin_add[i,j] = sum(ifelse(min_rep>= min(df3[,j]),1,0))/length(min_rep) 
    max_rep = apply(yrep_add,2,max)
    testMax_add[i,j] = sum(ifelse(max_rep>= max(df3[,j]),1,0))/length(max_rep)
    mean_rep = apply(yrep_add,2,mean)
    testMean_add[i,j] = sum(ifelse(mean_rep>= mean(df3[,j]),1,0))/length(mean_rep)
    var_rep = apply(yrep_add,2,var)
    testVar_add[i,j] = sum(ifelse(var_rep>= var(df3[,j]),1,0))/length(var_rep)
    neg.numb_add[i,j] = sum(ifelse(yrep_add<0,1,0))/(dim(yrep_add)[1]*dim(yrep_add)[2])
    above1_add[i,j] = sum(ifelse(yrep_add>1,1,0))/(dim(yrep_add)[1]*dim(yrep_add)[2])
  }
}

for(i in 1:ncol(y)){
  print(paste(colnames(testMin_add)[i],":"))
  print(paste("Min:",mean(testMin_add[,i]), sep=" "))
  print(paste("Max:",mean(testMax_add[,i]), sep=" "))
  print(paste("Mean:",mean(testMean_add[,i]), sep=" "))
  print(paste("Var:",mean(testVar_add[,i]), sep=" "))
  print(paste("Neg numb:",mean(neg.numb_add[,i]), sep=" "))
  print(paste("Above 1:",mean(above1_add[,i]), sep=" "))
}

dat_cb = list()
for(i in 1:ncol(y)){
  dat_cb[[i]] = sapply(1:nsim, function(s) rnorm(length(df3$Date),unlist(rChain[[i]][s,df3$Date]), 1/prec[,i]+he[,i]))
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


add_plot = list()

y = as.matrix(df2[,1:8])
head(y)
cols = c("blue","lightblue3","chartreuse3","darkblue","red","darkred","forestgreen","skyblue3")
library(ggplot2)
for(i in 1:ncol(mean_add)){
  
  plot_df = data.frame(party = mean_add[,i] ,  low=low_add[,i]*100, high=high_add[,i]*100, time=seq(orig.date,by='days',
                                                                                                    length=as.numeric(end.date-orig.date)), party2 = rep(colnames(y)[i], as.numeric(end.date-orig.date)))
  points = data.frame(x=seq(orig.date,by='days',length=as.numeric(end.date-orig.date))[df3$Date], 
                      y=df3[,i]*100, house=df3$house, party=rep(colnames(y)[i],length(df3$Date[length(df3$Date)][df3$Date])),
                      high_dat=dat_high[,i]*100, low_dat=dat_low[,i]*100 )
  add_plot[[i]] <- ggplot(plot_df) +
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


multiplot(add_plot[[1]], add_plot[[2]], add_plot[[3]], add_plot[[4]], 
          add_plot[[5]], add_plot[[6]], add_plot[[7]], add_plot[[8]], cols=2)


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


prec2 = matrix(NA,ncol=8, nrow=nrow(df2))
colnames(prec2) = colnames(df2)[1:8]
for(i in 1:8){
  for(j in 1:nrow(df2)){
    prec2[j,i] = ifelse(is.na(df2[j,i]), NA ,(1 / (df2[j,i]*(1-df2[j,i])/df2[i,'n'])))
  }
}




jags_addhouse ='
model{
#measurement model
for(j in 1:nparties){
for(i in 1:npolls){
y[i,j] ~ dnorm(x[day[i],j], prec[i,j]+house[j,org[i]])
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
house[i,12] <- 0
for(k in 1:(nhouses-1)){
house[i,k] ~ dnorm(0, 0.01)
}
}

}
'

y = as.matrix(df2[,1:8])
df = df2
writeLines(jags_addhouse,con="jags_addhouse.bug")
data_addhouse_2010 = list(y = y, prec = prec2, x = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date)),
                          nparties = ncol(y), day = df$Date, npolls = nrow(df), nperiods = as.numeric(end.date - orig.date),
                          nhouses = length(levels(as.factor(df$house))), org=as.numeric(as.factor(df$house)),
                          house = matrix(NA,ncol=length(levels(as.factor(df$house))), nrow=ncol(y)))

system.time(jags_addhouse_2010 <- jags.model("jags_addhouse.bug", data = data_addhouse_2010, n.chain=3))


ninter=10000
system.time(add_out_2010 <- coda.samples(jags_addhouse_2010,variable.names = c("x", "phi", "house"), n.iter = ninter, thin = 5))
system.time(sum_add_2010 <- summary(add_out_2010))
addout_x_2010 = add_out_2010[,which(regexpr("x", row.names(sum_add_2010$statistics))==1)]
addsum_x2010 = sum_add_2010$statistics[which(regexpr("x", row.names(sum_add_2010$statistics))==1),]
mean_add2010 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
ind.start = 1
ind.end = as.numeric(end.date-orig.date)
high_add2010 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
low_add2010 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
states_add2010 = list()
for(i in 1:ncol(y)){
  ind.start[i+1] = i*as.numeric(end.date-orig.date)+1
  ind.end[i+1] =  ind.start[i+1]+as.numeric(end.date-orig.date)-1
  mean_add2010[,i] = addsum_x2010[ind.start[i]:ind.end[i],1]
  low_add2010[,i] = mean_add2010[,i] - 1.96 * addsum_x2010[ind.start[i]:ind.end[i],2]
  high_add2010[,i] = mean_add2010[,i] + 1.96 * addsum_x2010[ind.start[i]:ind.end[i],2]
  states_add2010[[i]] = addout_x_2010[,ind.start[i]:ind.end[i]]
}

predadd2010 = matrix(NA, nrow=ncol(y), ncol=5)
for(i in 1:ncol(y)){
  predadd2010[i,] = c(elec[2,i],mean_add2010[nrow(mean_add2010),i],low_add2010[nrow(mean_add2010),i],high_add2010[nrow(mean_add2010),i],mean_add2010[nrow(mean_add2010),i]-elec[2,i])
}
colnames(predadd2010) = c("Elec res","MAP","Low", "High", "Diff")
row.names(predadd2010) = colnames(y)
predadd2010


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


prec2 = matrix(NA,ncol=8, nrow=nrow(df2))
colnames(prec2) = colnames(df2)[1:8]
for(i in 1:8){
  for(j in 1:nrow(df2)){
    prec2[j,i] = ifelse(is.na(df2[j,i]), NA ,(1 / (df2[j,i]*(1-df2[j,i])/df2[i,'n'])))
  }
}




jags_addhouse ='
model{
#measurement model
for(j in 1:nparties){
for(i in 1:npolls){
y[i,j] ~ dnorm(x[day[i],j]+house[j,org[i]], prec[i,j])
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
house[i,12] <- 0
for(k in 1:(nhouses-1)){
house[i,k] ~ dnorm(0, 0.01)
}
}

}
'

y = as.matrix(df2[,1:8])
df = df2
writeLines(jags_addhouse,con="jags_addhouse.bug")
data_addhouse_2014 = list(y = y, prec = prec2, x = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date)),
                          nparties = ncol(y), day = df$Date, npolls = nrow(df), nperiods = as.numeric(end.date - orig.date),
                          nhouses = length(levels(as.factor(df$house))), org=as.numeric(as.factor(df$house)),
                          house = matrix(NA,ncol=length(levels(as.factor(df$house))), nrow=ncol(y)))

system.time(jags_addhouse_2014 <- jags.model("jags_addhouse.bug", data = data_addhouse_2014, n.chain=3))


ninter=10000
system.time(add_out_2014 <- coda.samples(jags_addhouse_2014,variable.names = c("x", "phi", "house"), n.iter = ninter, thin = 5))
system.time(sum_add_2014 <- summary(add_out_2014))
addout_x_2014 = add_out_2014[,which(regexpr("x", row.names(sum_add_2014$statistics))==1)]
addsum_x2014 = sum_add_2014$statistics[which(regexpr("x", row.names(sum_add_2014$statistics))==1),]
mean_add2014 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
ind.start = 1
ind.end = as.numeric(end.date-orig.date)
high_add2014 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
low_add2014 = matrix(NA, ncol=ncol(y), nrow=as.numeric(end.date-orig.date))
states_add2014 = list()
for(i in 1:ncol(y)){
  ind.start[i+1] = i*as.numeric(end.date-orig.date)+1
  ind.end[i+1] =  ind.start[i+1]+as.numeric(end.date-orig.date)-1
  mean_add2014[,i] = addsum_x2014[ind.start[i]:ind.end[i],1]
  low_add2014[,i] = mean_add2014[,i] - 1.96 * addsum_x2014[ind.start[i]:ind.end[i],2]
  high_add2014[,i] = mean_add2014[,i] + 1.96 * addsum_x2014[ind.start[i]:ind.end[i],2]
  states_add2014[[i]] = addout_x_2014[,ind.start[i]:ind.end[i]]
}

predadd2014 = matrix(NA, nrow=ncol(y), ncol=5)
for(i in 1:ncol(y)){
  predadd2014[i,] = c(elec[3,i],mean_add2014[nrow(mean_add2014),i],low_add2014[nrow(mean_add2014),i],high_add2014[nrow(mean_add2014),i],mean_add2014[nrow(mean_add2014),i]-elec[3,i])
}
colnames(predadd2014) = c("Elec res","MAP","Low", "High", "Diff")
row.names(predadd2014) = colnames(y)
predadd2014
