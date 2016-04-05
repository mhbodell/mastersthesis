library(rjags, lib="C:/Users/mirhu86/Documents/packages")
data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)


O = NULL
for(i in 1:nrow(polls)){
  O[i] = ifelse(sum(polls[i,3:10], na.rm=TRUE)==100,0, sum(polls[i,11:12], na.rm=TRUE))
}

y =  polls[3:10]
y$O = O
y$startDate = as.Date(polls$collectPeriodFrom)-1
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

partynames =  c("M","L","KD","C","S","MP","V","SD")
colnames(elec) = partynames
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
orig.date = as.Date(df$startDate[1]-1)
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
all_data2 = list(y = y2, prec = prec2 , x = matrix(NA, ncol=ncol(y2), nrow=as.numeric(end.date-orig.date)),
                nparties=ncol(y2), npolls=nrow(y2), nperiods=as.numeric(end.date-orig.date), day=df2$Date )

system.time(jags_all2 <- jags.model("jags_dlm.bug", data = all_data2, n.chain=3))

ninter=10000

system.time(all_out2 <- coda.samples(jags_all2,variable.names = c("x", "phi"), n.iter = ninter, thin = 5))
sum_all2 = summary(all_out2)
#str(sum_all)
#str(outM)
#str(sum_all$statistics[])
out_x2 = all_out2[,which(regexpr("x", row.names(sum_all2$statistics))==1)]
sum_x2 = sum_all2$statistics[which(regexpr("x", row.names(sum_all2$statistics))==1),]
out_phi2 = all_out2[,which(regexpr("phi", row.names(sum_all2$statistics))==1)]
par(mfrow=c(3,3))
for(i in 1: ncol(out_phi2[[1]])){plot(out_phi2[,i])}
par(mfrow=c(1,1))  

#str(out_x)
#dim(out_phi)
mean_basic2 = matrix(NA, ncol=ncol(y2), nrow=as.numeric(end.date-orig.date))
ind.start = 1
ind.end = as.numeric(end.date-orig.date)
high_basic2 = matrix(NA, ncol=ncol(y2), nrow=as.numeric(end.date-orig.date))
low_basic2 = matrix(NA, ncol=ncol(y2), nrow=as.numeric(end.date-orig.date))
states_basic2 = list()
for(i in 1:ncol(y2)){
  ind.start[i+1] = i*as.numeric(end.date-orig.date)+1
  ind.end[i+1] =  ind.start[i+1]+as.numeric(end.date-orig.date)-1
  mean_basic2[,i] = sum_x2[ind.start[i]:ind.end[i],1]
  low_basic2[,i] = mean_basic2[,i] - 1.96 * sum_x2[ind.start[i]:ind.end[i],2]
  high_basic2[,i] = mean_basic2[,i] + 1.96 * sum_x2[ind.start[i]:ind.end[i],2]
  states_basic2[[i]] = out_x2[,ind.start[i]:ind.end[i]]
}


##################################################################
############# POSTERIOR PREDICTIVE CHECKING ######################
##################################################################


set.seed(901207)
i = sample(1:3,1)
rChain = list()
rChain[[1]] = states_basic2[[1]][i];rChain[[2]] = states_basic2[[2]][i];rChain[[3]] = states_basic2[[3]][i]
rChain[[4]] = states_basic2[[4]][i];rChain[[5]] = states_basic2[[5]][i];rChain[[6]] = states_basic2[[6]][i]
rChain[[7]] = states_basic2[[7]][i];rChain[[8]] = states_basic2[[8]][i]
str(rChain)
nsim = dim(all_out2[[i]])[1]

testMin.basic2 = matrix(NA,ncol=8, nrow=100)
testMax.basic2 = matrix(NA,ncol=8, nrow=100)
testMean.basic2 = matrix(NA,ncol=8, nrow=100)
colnames(testMin.basic2) = colnames(testMax.basic2) = colnames(testMean.basic2) = c("M","L","KD","C","S","MP","V","SD") 
neg.numb2 =  matrix(NA,ncol=8, nrow=100)
above12 =  matrix(NA,ncol=8, nrow=100)

for(i in 1:100){
  for(j in 1:ncol(y2)){
    yrep_basic = sapply(1:nsim, function(s) rnorm(length(df2$Date),unlist(rChain[[j]][s,df2$Date]), 1/prec2[,j]))
    min_rep = apply(yrep_basic,2,min)
    testMin.basic2[i,j] = sum(ifelse(min_rep>= min(df2[,j]),1,0))/length(min_rep) 
    max_rep = apply(yrep_basic,2,max)
    testMax.basic2[i,j] = sum(ifelse(max_rep>= max(df2[,j]),1,0))/length(max_rep)
    mean_rep = apply(yrep_basic,2,mean)
    testMean.basic2[i,j] = sum(ifelse(mean_rep>= mean(df2[,j]),1,0))/length(mean_rep)
    neg.numb2[i,j] = sum(ifelse(yrep_basic<0,1,0))/length(mean_rep)
    above12[i,j] = sum(ifelse(yrep_basic>0,1,0))/length(mean_rep)
  }
}
for(i in 1:ncol(y)){
  print(paste(colnames(testMin.basic2)[i],":"))
  print(paste("Min:",mean(testMin.basic2[,i]), sep=" "))
  print(paste("Max:",mean(testMax.basic2[,i]), sep=" "))
  print(paste("Mean:",mean(testMean.basic2[,i]), sep=" "))
  print(paste("Negative values:",mean(neg.numb2[,i]), sep=" "))
  print(paste("Above 1:",mean(above12[,i]), sep=" "))
}

dat_cb2 = list()
for(i in 1:ncol(y2)){
  dat_cb2[[i]] = sapply(1:nsim, function(s) rnorm(length(df2$Date),unlist(rChain[[i]][s,d2f$Date]), 1/prec2[,i]))
}


dat_low2 = matrix(NA, ncol=ncol(y), nrow=nrow(y))
dat_high2 = matrix(NA, ncol=ncol(y), nrow=nrow(y))
for(i in 1:ncol(y2)){
  dat_low2[,i] = apply(dat_cb2[[i]], 1, mean) - (apply(dat_cb2[[i]], 1, sd)*1.96)
  dat_high2[,i] = apply(dat_cb2[[i]], 1, mean) + (apply(dat_cb2[[i]], 1, sd)*1.96)
}

test2 =list()
for(j in 1:ncol(y)){
  test2[[j]] = sapply(1:nsim, function(s) rnorm(length(df2$Date),unlist(rChain[[j]][s,df2$Date]), 1/prec[2,j]))
}  

for(j in 1:ncol(y)){
  test[[j]][1,]
}



#################################################
################# PLOTS #########################
#################################################

### M        L        C       KD        S         V       MP        SD ##
basic_plot2 = list()

y2 = as.matrix(df2[,1:8])
head(y2)
cols = c("blue","lightblue3","chartreuse3","darkblue","red","darkred","forestgreen","skyblue3")
library(ggplot2)
for(i in 1:ncol(mean_basic2)){
  
  plot_df = data.frame(party = mean_basic2[,i] ,  low=low_basic2[,i]*100, high=high_basic2[,i]*100, time=seq(orig.date,by='days',
                                                                                                          length=as.numeric(end.date-orig.date)), party2 = rep(colnames(y2)[i], as.numeric(end.date-orig.date)))
  points = data.frame(x=seq(orig.date,by='days',length=as.numeric(end.date-orig.date))[df2$Date], 
                      y=df2[,i]*100, house=df2$house, party=rep(colnames(y2)[i],length(df2$Date[length(df2$Date)][df2$Date])),
                      high_dat=dat_high2[,i]*100, low_dat=dat_low2[,i]*100 )
  basic_plot2[[i]] <- ggplot(plot_df2) +
    aes(x = time, y = party*100) +
    geom_line(col=cols[i], alpha=2)  +
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
orig.date = df$startDate
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


y2 = as.matrix(df2[,1:8])
all_data22010 = list(y = y2, prec = prec2 , x = matrix(NA, ncol=ncol(y2), nrow=as.numeric(end.date-orig.date)),
                nparties=ncol(y2), npolls=nrow(y2), nperiods=as.numeric(end.date-orig.date), 
                phi = NULL, day=df2$Date )
writeLines(jags_dlm,con="jags_dlm.bug")
system.time(jags_all2010<- jags.model("jags_dlm.bug", data = all_data22010, n.chain=3))

ninter=10000

system.time(all_out22010 <- coda.samples(jags_all22010,variable.names = c("x", "phi"), n.iter = ninter, thin = 5))
sum_all22010 = summary(all_out22010)
out_x22010 = all_out22010[,which(regexpr("x", row.names(sum_all22010$statistics))==1)]
sum_x22010 = sum_all22010$statistics[which(regexpr("x", row.names(sum_all22010$statistics))==1),]
out_phi22010 = all_out22010[,which(regexpr("phi", row.names(sum_all22010$statistics))==1)]
par(mfrow=c(3,3))
for(i in 1: ncol(out_phi22010[[1]])){plot(out_phi22010[,i])}
par(mfrow=c(1,1))  

mean_basic22010 = matrix(NA, ncol=ncol(y2), nrow=as.numeric(end.date-orig.date))
ind.start = 1
ind.end = as.numeric(end.date-orig.date)
high_basic22010 = matrix(NA, ncol=ncol(y2), nrow=as.numeric(end.date-orig.date))
low_basic22010 = matrix(NA, ncol=ncol(y2), nrow=as.numeric(end.date-orig.date))
states_basic22010 = list()
for(i in 1:ncol(y2)){
  ind.start[i+1] = i*as.numeric(end.date-orig.date)+1
  ind.end[i+1] =  ind.start[i+1]+as.numeric(end.date-orig.date)-1
  mean_basic22010[,i] = sum_x22010[ind.start[i]:ind.end[i],1]
  low_basic22010[,i] = mean_basic22010[,i] - 1.96 * sum_x22010[ind.start[i]:ind.end[i],2]
  high_basic22010[,i] = mean_basic22010[,i] + 1.96 * sum_x22010[ind.start[i]:ind.end[i],2]
  states_basic22010[[i]] = out_x22010[,ind.start[i]:ind.end[i]]
}

pred22010 = matrix(NA, ncol=5, nrow=8)
row.names(pred22010) = c("M","L","KD","C","S","MP","V","SD")
colnames(pred22010) = c("Elec_res","MAP","Low","High","Diff")
for(i in 1:ncol(y2)){
  pred22010[i,] =  cbind(elec[2,i],mean_basic22010[nrow(mean_basic22010),i],low_basic22010[nrow(low_basic22010),i],high_basic22010[nrow(high_basic22010),i],(mean_basic22010[nrow(mean_basic22010),i]-elec[2,i]))
}

pred22010


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
orig.date = df$startDate[1]
end.date = elec$Date[3]
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
df = df[-which(df$endDate>=end.date),]
tail(df)


ateDiff = df[,'endDate'] - df[,'startDate']
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



y2 = as.matrix(df2[,1:8])
all_data22014 = list(y = y2, prec = prec2 , x = matrix(NA, ncol=ncol(y2), nrow=as.numeric(end.date-orig.date)),
                    nparties=ncol(y2), npolls=nrow(y2), nperiods=as.numeric(end.date-orig.date), 
                    phi = NULL, day=df2$Date )
writeLines(jags_dlm,con="jags_dlm.bug")
system.time(jags_all22014<- jags.model("jags_dlm.bug", data = all_data22014, n.chain=3))

ninter=10000

system.time(all_out22014 <- coda.samples(jags_all22014,variable.names = c("x", "phi"), n.iter = ninter, thin = 5))
sum_all22014 = summary(all_out22014)
out_x22014 = all_out22014[,which(regexpr("x", row.names(sum_all22014$statistics))==1)]
sum_x22014 = sum_all22014$statistics[which(regexpr("x", row.names(sum_all22014$statistics))==1),]
out_phi22014 = all_out22014[,which(regexpr("phi", row.names(sum_all22014$statistics))==1)]
par(mfrow=c(3,3))
for(i in 1: ncol(out_phi22014[[1]])){plot(out_phi22014[,i])}
par(mfrow=c(1,1))  

mean_basic22014 = matrix(NA, ncol=ncol(y2), nrow=as.numeric(end.date-orig.date))
ind.start = 1
ind.end = as.numeric(end.date-orig.date)
high_basic22014 = matrix(NA, ncol=ncol(y2), nrow=as.numeric(end.date-orig.date))
low_basic22014 = matrix(NA, ncol=ncol(y2), nrow=as.numeric(end.date-orig.date))
states_basic22014 = list()
for(i in 1:ncol(y2)){
  ind.start[i+1] = i*as.numeric(end.date-orig.date)+1
  ind.end[i+1] =  ind.start[i+1]+as.numeric(end.date-orig.date)-1
  mean_basic22014[,i] = sum_x22014[ind.start[i]:ind.end[i],1]
  low_basic22014[,i] = mean_basic22014[,i] - 1.96 * sum_x22014[ind.start[i]:ind.end[i],2]
  high_basic22014[,i] = mean_basic22014[,i] + 1.96 * sum_x22014[ind.start[i]:ind.end[i],2]
  states_basic22014[[i]] = out_x22014[,ind.start[i]:ind.end[i]]
}

pred22014 = matrix(NA, ncol=5, nrow=8)
row.names(pred22014) = c("M","L","KD","C","S","MP","V","SD")
colnames(pred22014) = c("Elec_res","MAP","Low","High","Diff")
for(i in 1:ncol(y2)){
  pred22014[i,] =  cbind(elec[2,i],mean_basic22014[nrow(mean_basic22014),i],low_basic22014[nrow(low_basic22014),i],high_basic22014[nrow(high_basic22014),i],(mean_basic22014[nrow(mean_basic22014),i]-elec[2,i]))
}

pred22014

