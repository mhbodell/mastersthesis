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

elec = data.frame(rbind(c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3050,0.0716,0.0568,0.0656,0.311,0.0745,0.069,0.0578),
                        c(0.2432, 0.0565, 0.0477, 0.0637, 0.3234, 0.0718, 0.0596, 0.1341)))#c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0.00000001),

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
partynames = c("M","L","KD","C","S","MP","V","SD")

Y = df[,partynames] * df$n
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
  Alpha[i, 1:nparties] <- x[i-1,  1:nparties] * 1000
  x[i, 1:nparties] ~ ddirch(Alpha[i, 1:nparties])
}

for (i in 1:nparties) { 
  alpha[i] ~ dunif(100, 1000) 
}
x[1, 1:nparties] ~ ddirch(alpha[])

for (i in 1:nparties) { 
  house[12, i] <- 0
}

for (i in 2:nparties) { # for each party - house effects across houses sum to zero
house[1, i] <- -sum(house[2:nhouses, i] )
}

for(i in 1:(nhouses-1)) { # for each house - house effects across the parties sum to zero
house[i, 1] <- -sum(house[i, 2:nparties] )
}

for (i in 2:nparties) { 
for(j in 2:(nhouses-1)) { 
house[j, i] ~ dnorm(0, 0.01)
}
}

}
'

writeLines(multi_diri, con="DM-model")
system.time(jags_DM <- jags.model("DM-model", data = data, n.adapt=100, n.chain=3))
#update(jags_DM,1000)
ninter = 1000
system.time(outDM <- coda.samples(jags_DM,variable.names = c("y", "x","house"), n.iter = ninter, thin = 20, burnin=10))
system.time(sumDM <-  summary(outDM))
addout_x= outDM[,which(regexpr("x",row.names(sumDM$statistics))==1),]

addhouse = outDM[,which(regexpr("house", row.names(sumDM$statistics))==1)]
map_house = matrix(NA, nrow=length(levels(as.factor(df$house))), ncol=8)
j=1
for(i in seq(1,dim(addhouse[[1]])[2],12)){
  map_house[,j] = apply(as.matrix(addhouse[,seq(i,i+11,1)]),2,mean)
  j = j+1
}
row.names(map_house) = levels(as.factor(df$house))
colnames(map_house) = colnames(y[1,8])
map_house 

str(addout_x[[1]])

nperiods=max(df[,'Date'])
nsim = dim(outDM[[1]])[1]*3
mean_add = matrix(NA, ncol=8, nrow=nperiods)
ind.start = 1
ind.end = nperiods
high_add = high_add2 = matrix(NA, ncol=8, nrow=nperiods)
low_add = low_add2 = matrix(NA, ncol=8, nrow=nperiods)
percentile5 = round(nsim*0.05)
percentile95 = round(nsim*0.95)
states_add = list()
for(i in 1:8){
  ind.start[i+1] = i*nperiods+1
  ind.end[i+1] =  ind.start[i+1]+nperiods-1
  mean_add[,i] = apply(rbind(addout_x[[1]][,ind.start[i]:ind.end[i]],addout_x[[2]][,ind.start[i]:ind.end[i]],addout_x[[3]][,ind.start[i]:ind.end[i]]),2,mean)
  low_add[,i] = apply(rbind(addout_x[[1]][,ind.start[i]:ind.end[i]],addout_x[[2]][,ind.start[i]:ind.end[i]],addout_x[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile5])
  high_add[,i] = apply(rbind(addout_x[[1]][,ind.start[i]:ind.end[i]],addout_x[[2]][,ind.start[i]:ind.end[i]],addout_x[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile95])
  states_add[[i]] = rbind(addout_x[[1]][,ind.start[i]:ind.end[i]],addout_x[[2]][,ind.start[i]:ind.end[i]],addout_x[[3]][,ind.start[i]:ind.end[i]])
}

apply(rbind(addout_x[[1]][,ind.start[i]:ind.end[i]],addout_x[[2]][,ind.start[i]:ind.end[i]],addout_x[[3]][,ind.start[i]:ind.end[i]]),2,mean)
plot(mean_add[,6])
plot(y[,3])
mean_add[,6]

##################################################################
############# POSTERIOR PREDICTIVE CHECKING ######################
##################################################################


rChain = list()
rChain[[colnames(y)[1]]] = states_add[[1]];rChain[[colnames(y)[2]]] = states_add[[2]];rChain[[colnames(y)[3]]] = states_add[[3]]
rChain[[colnames(y)[4]]] = states_add[[4]];rChain[[colnames(y)[5]]] = states_add[[5]];rChain[[colnames(y)[6]]] = states_add[[6]]
rChain[[colnames(y)[7]]] = states_add[[7]];rChain[[colnames(y)[8]]] = states_add[[8]]
str(rChain)

df3 = df[,colnames(df)[1:8]]
df3$startDate = df$startDate
df3$endDate = df$endDate
df3$n = df$n
df3$Date = df$Date

dayssinceorigStart = julian(df3$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df3$endDate, origin=orig.date) 
df3$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)


he = matrix(NA, ncol=ncol(prec),nrow=nrow(df))
for(k in 1:ncol(prec)){
  for(i in 1:length(row.names(map_house))){
    ins = row.names(map_house)[i]
    for(j in 1:nrow(df)){
      if(df[j,'house']==ins){
        he[j,k] = map_house[i,k]
      } else{}
    }
  }
}
colnames(he) = colnames(df3[,1:8])

yrep1 = matrix(NA, nrow=nrow(df3), ncol=8)
colnames(yrep1) = partynames
Yrep = list()
for(i in 1:dim(rChain[[1]])[1]){
  for(j in 1:nrow(df3)){
    M = ifelse(rChain[['M']][i,df3$Date[j]]+he[j,'M']<0,0,rChain[['M']][i,df3$Date[j]]+he[j,'M'])
    L = ifelse(rChain[['FP']][i,df3$Date[j]]+he[j,'L']<0,0,rChain[['FP']][i,df3$Date[j]]+he[j,'L'])
    KD = ifelse(rChain[['KD']][i,df3$Date[j]]+he[j,'KD']<0,0,rChain[['KD']][i,df3$Date[j]]+he[j,'KD'])
    C = ifelse(rChain[['C']][i,df3$Date[j]]+he[j,'C']<0,0,rChain[['C']][i,df3$Date[j]]+he[j,'C'])
    S = ifelse(rChain[['S']][i,df3$Date[j]]+he[j,'S']<0,0,rChain[['S']][i,df3$Date[j]]+he[j,'S'])
    MP = ifelse(rChain[['MP']][i,df3$Date[j]]+he[j,'MP']<0,0,rChain[['MP']][i,df3$Date[j]]+he[j,'MP'])
    V = ifelse(rChain[['V']][i,df3$Date[j]]+he[j,'V']<0,0,rChain[['V']][i,df3$Date[j]]+he[j,'V'])
    SD = ifelse(rChain[['SD']][i,df3$Date[j]]+he[j,'SD']<0,0,rChain[['SD']][i,df3$Date[j]]+he[j,'SD'])
    
    yrep1[j,] = (t(rmultinom(1,df$n[j],c(M,L,KD,C,S,MP,V,SD))))/df$n[j]
  }
  Yrep[[i]] = yrep1
}

bpmin = matrix(NA,ncol=8,nrow=dim(rChain[[1]])[1])
bpmax = matrix(NA,ncol=8,nrow=dim(rChain[[1]])[1])
bpmean = matrix(NA,ncol=8,nrow=dim(rChain[[1]])[1])
bpvar = matrix(NA,ncol=8,nrow=dim(rChain[[1]])[1])
for(i in 1:dim(rChain[[1]])[1]){
  bpmin[i,] = ifelse(apply(Yrep[[i]],2,min)>apply(df3[,1:8],2,min),1,0)
  bpmax[i,] = ifelse(apply(Yrep[[i]],2,max)>apply(df3[,1:8],2,max),1,0)
  bpmean[i,] = ifelse(apply(Yrep[[i]],2,mean)>apply(df3[,1:8],2,mean),1,0)
  bpvar[i,] = ifelse(apply(Yrep[[i]],2,var)>apply(df3[,1:8],2,var),1,0)
}


bp = cbind(apply(bpmin,2,sum)/nrow(bpmin),apply(bpmax,2,sum)/nrow(bpmax),apply(bpmean,2,sum)/nrow(bpmean),apply(bpvar,2,sum)/nrow(bpvar))
row.names(bp) = partynames
colnames(bp ) =c("min","max","mean","var")
for(i in 1:8)


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
dat_high2 = dat_high2[,colnames(y2)]
dat_low2 = dat_low2[,colnames(y2)]
bandsL = rep(dat_low2[,1], dateDiff2)
bandsH = rep(dat_low2[,1], dateDiff2)
#################################################
################# PLOTS #########################
#################################################

basic_plot2 = list()

head(y2)
df3$house = df$house
cols = c("blue","lightblue3","darkblue","chartreuse3","red","darkred","forestgreen","skyblue3")
library(ggplot2)
for(i in 1:ncol(mean_add)){
  
  plot_df = data.frame(party = mean_add[,i] ,  low=low_add[,i]*100, high=high_add[,i]*100,
                       time=seq(orig.date,by='days', length=max(df$Date)), party2 = rep(colnames(y2)[i], max(df$Date)))
  points = data.frame(x=seq(orig.date,by='days',length=max(df$Date))[df3$Date], 
                      y=df3[,i]*100, house=df3$house, party=rep(colnames(y)[i],length(df3$Date[length(df3$Date)][df3$Date])),
                      high_dat=dat_high2[,i]*100, low_dat=dat_low2[,i]*100)
  basic_plot2[[i]] <-  ggplot(plot_df) +
    aes(x = time, y = party*100) +
    geom_line(col=cols[i], alpha=1)  +
    geom_ribbon(aes(ymin=low, ymax=high), alpha=0.4, fill=cols[i]) + 
    #geom_line(data=points, aes(x=x, y=high_dat),col=cols[i], alpha=1)  +
    #geom_line(data=points, aes(x=x, y=low_dat),col=cols[i], alpha=1)  +
    #geom_arean(data=points, aes(x=x, ymin=low_dat, ymax=high_dat), alpha=0.5, fill=cols[i], inherit.aes = FALSE) +
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
                        c(0.3050,0.0716,0.0568,0.0656,0.311,0.0745,0.069,0.0578),
                        c(0.2432, 0.0565, 0.0477, 0.0637, 0.3234, 0.0718, 0.0596, 0.1341)))
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
orig.date = df$startDate[1]-1
end.date = elec$Date[2]
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
df = df[-which(df$endDate>=end.date),]
tail(df)

prec=matrix(NA,ncol=8, nrow=nrow(df))
colnames(prec) = colnames(df)[1:8]
for(i in 1:8){
  prec[,i] = (1 / (df[,i]*(1-df[,i])/df[,'n']))
  
}


jags_addhouse ='
model{
#measurement model
for(j in 1:nparties){
for(i in 1:npolls){
y[i,j] ~ dnorm(x[day[i],j]+house[org[i],j], prec[i,j])
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
house[12,i] <- 0
}

for (i in 2:nparties) { # for each party - house effects across houses sum to zero
house[1, i] <- -sum(house[2:nhouses, i] )
}

for(i in 1:(nhouses-1)) { # for each house - house effects across the parties sum to zero
house[i, 1] <- -sum(house[i, 2:nparties] )
}

for (i in 2:nparties) { 
for(j in 2:(nhouses-1)) { 
house[j, i] ~ dnorm(0, 0.01)
}
}
}
'
y2 = as.matrix(df[,1:8])
df$house = factor(df$house, levels=c("Demoskop","Inizio", "Ipsos" ,"Novus" ,"SCB" ,"Sentio" ,
                                     "Sifo" ,"Skop" ,"SVT", "United Minds", "YouGov","Election"))

all_data22010 = list(y = y2, prec = prec, x = matrix(NA, ncol=ncol(y2), nrow=max(df$Date)),
                     nparties = ncol(y2), day = df$Date, npolls = nrow(df), nperiods = max(df$Date),
                     nhouses = length(levels(as.factor(df$house))), org=as.numeric(df$house),
                     house = matrix(NA,nrow=length(levels(as.factor(df$house))), ncol=ncol(y2)))
writeLines(jags_addhouse,con="jags_addhouse.bug")
system.time(jags_addhouse2010 <- jags.model("jags_addhouse.bug", data = all_data22010, n.chain=3))
ninter=40000

system.time(all_out22010 <- coda.samples(jags_addhouse2010,variable.names = c("x", "eps"), n.iter = ninter, thin = 20, burnin=5000))
sum_all22010 = summary(all_out22010)
add_out2010 = all_out22010[,which(regexpr("x", row.names(sum_all22010$statistics))==1)]
out_phi22010 = all_out22010[,which(regexpr("eps", row.names(sum_all22010$statistics))==1)]
par(mfrow=c(3,3))
for(i in 1: ncol(out_phi22010[[1]])){plot(out_phi22010[,i])}
par(mfrow=c(1,1))  

nperiods=max(df[,'Date'])
nsim = dim(add_out2010[[1]])[1]*3
mean_add2010 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
ind.start = 1
ind.end = nperiods
high_add2010 = high_add22010 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
low_add2010 = low_add22010 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
percentile5 = round(nsim*0.05)
percentile95 = round(nsim*0.95)
states_add2010= list()
colnames(mean_add2010) = colnames(low_add2010) =colnames(high_add2010) = partynames
for(i in 1:ncol(y2)){
  ind.start[i+1] = i*nperiods+1
  ind.end[i+1] =  ind.start[i+1]+nperiods-1
  mean_add2010[,i] = apply(rbind(add_out2010[[1]][,ind.start[i]:ind.end[i]],add_out2010[[2]][,ind.start[i]:ind.end[i]],add_out2010[[3]][,ind.start[i]:ind.end[i]]),2,mean)
  low_add2010[,i] = apply(rbind(add_out2010[[1]][,ind.start[i]:ind.end[i]],add_out2010[[2]][,ind.start[i]:ind.end[i]],add_out2010[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile5])
  high_add2010[,i] = apply(rbind(add_out2010[[1]][,ind.start[i]:ind.end[i]],add_out2010[[2]][,ind.start[i]:ind.end[i]],add_out2010[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile95])
}


pred22010 = matrix(NA, ncol=5, nrow=8)
row.names(pred22010) = colnames(mean_add2010)[1:8]
colnames(pred22010) = c("Elec_res","MAP","Low","High","Diff")
elec2 = elec[,colnames(mean_add2010)[1:8]]
for(i in 1:ncol(y2)){
  pred22010[i,] =  cbind(elec2[2,i],mean_add2010[nrow(mean_add2010),i],low_add2010[nrow(low_add2010),i],high_add2010[nrow(high_add2010),i],(mean_add2010[nrow(mean_add2010),i]-elec2[2,i]))
}
pred22010

head(mean_add2010)
head(elec2)

mse_elec2010 = matrix(NA, ncol=1, nrow=8)
row.names(mse_elec2010) = row.names(pred22010)
for(i in 1:8){
  mse_elec2010[i,]  = sum((mean_add2010[,i]-elec2[2,i])^2)/nrow(mean_add2010)
}
mse_elec2010

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
                        c(0.3050,0.0716,0.0568,0.0656,0.311,0.0745,0.069,0.0578),
                        c(0.2432, 0.0565, 0.0477, 0.0637, 0.3234, 0.0718, 0.0596, 0.1341)))
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
orig.date = df$startDate[1]-1
end.date = elec$Date[3]
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
df = df[-which(df$endDate>=end.date),]
tail(df)
prec=matrix(NA,ncol=8, nrow=nrow(df))
colnames(prec) = colnames(df)[1:8]

for(i in 1:8){
  prec[,i] = (1 / (df[,i]*(1-df[,i])/df[,'n']))
  
}

jags_addhouse ='
model{
#measurement model
for(j in 1:nparties){
for(i in 1:npolls){
y[i,j] ~ dnorm(x[day[i],j]+house[org[i],j], prec[i,j])
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
house[12,i] <- 0
}

for (i in 2:nparties) { # for each party - house effects across houses sum to zero
house[1, i] <- -sum(house[2:nhouses, i] )
}

for(i in 1:(nhouses-1)) { # for each house - house effects across the parties sum to zero
house[i, 1] <- -sum(house[i, 2:nparties] )
}

for (i in 2:nparties) { 
for(j in 2:(nhouses-1)) { 
house[j, i] ~ dnorm(0, 0.01)
}
}
}
'

y2 = as.matrix(df[,1:8])
df$house = factor(df$house, levels=c("Demoskop","Inizio", "Ipsos" ,"Novus" ,"SCB" ,"Sentio" ,
                                     "Sifo" ,"Skop" ,"SVT", "United Minds", "YouGov","Election"))

all_data22014 = list(y = y2, prec = prec, x = matrix(NA, ncol=ncol(y2), nrow=max(df$Date)),
                     nparties = ncol(y2), day = df$Date, npolls = nrow(df2), nperiods = max(df$Date),
                     nhouses = length(levels(as.factor(df$house))), org=as.numeric(df$house),
                     house = matrix(NA,nrow=length(levels(as.factor(df$house))), ncol=ncol(y2)))
writeLines(jags_addhouse,con="jags_addhouse.bug")

system.time(jags_all22014<- jags.model("jags_addhouse.bug", data = all_data22014, n.chain=3))
system.time(all_out22014 <- coda.samples(jags_all22014,variable.names = c("x", "eps"), n.iter = ninter, thin = 20, burnin=5000))

sum_all22014 = summary(all_out22014)
add_out2014 = all_out22014[,which(regexpr("x", row.names(sum_all22014$statistics))==1)]
out_phi22014 = all_out22014[,which(regexpr("eps", row.names(sum_all22014$statistics))==1)]
par(mfrow=c(3,3))
for(i in 1: ncol(out_phi22014[[1]])){plot(out_phi22014[,i])}
par(mfrow=c(1,1))  

nperiods=max(df[,'Date'])
nsim = dim(add_out2014[[1]])[1]*3
mean_add2014 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
ind.start = 1
ind.end = nperiods
high_add2014 = high_add22014 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
low_add2014 = low_add22014 = matrix(NA, ncol=ncol(y2), nrow=nperiods)
percentile5 = round(nsim*0.05)
percentile95 = round(nsim*0.95)
states_add2014= list()
colnames(mean_add2014) = colnames(low_add2014) =colnames(high_add2014) = partynames
for(i in 1:ncol(y2)){
  ind.start[i+1] = i*nperiods+1
  ind.end[i+1] =  ind.start[i+1]+nperiods-1
  mean_add2014[,i] = apply(rbind(add_out2014[[1]][,ind.start[i]:ind.end[i]],add_out2014[[2]][,ind.start[i]:ind.end[i]],add_out2014[[3]][,ind.start[i]:ind.end[i]]),2,mean)
  low_add2014[,i] = apply(rbind(add_out2014[[1]][,ind.start[i]:ind.end[i]],add_out2014[[2]][,ind.start[i]:ind.end[i]],add_out2014[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile5])
  high_add2014[,i] = apply(rbind(add_out2014[[1]][,ind.start[i]:ind.end[i]],add_out2014[[2]][,ind.start[i]:ind.end[i]],add_out2014[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile95])
}

pred22014 = matrix(NA, ncol=5, nrow=8)
row.names(pred22014) = colnames(df)[1:8]
colnames(pred22014) = c("Elec_res","MAP","Low","High","Diff")
elec2 = elec[,colnames(df)[1:8]]
for(i in 1:ncol(y2)){
  pred22014[i,] =  cbind(elec2[3,i],mean_add2014[nrow(mean_add2014),i],low_add2014[nrow(low_add2014),i],high_add2014[nrow(high_add2014),i],(mean_add2014[nrow(mean_add2014),i]-elec2[3,i]))
}
pred22014

mse_elec2014 = matrix(NA, ncol=1, nrow=8)
row.names(mse_elec2014) = row.names(pred22014)
for(i in 1:8){
  mse_elec2014[i,]  = sum((mean_add2014[,i]-elec2[3,i])^2)/nrow(mean_add2014)
}
mse_elec2014


add_out2014=addout_x 


