library(dplyr)

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

df3 = df[order(df$Date),]

dateDiff = df3[,'endDate'] - df3[,'startDate']
dateDiff2 = dateDiff+1
nDay = df3$n/as.numeric(dateDiff2)
MSmooth = rep(df3$M,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(df3$house, dateDiff2)

ee = list()
for(i in 1:nrow(df3)){
  ee[[i]] = seq(df3[i,'startDate'], df3[i,'endDate'], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(df3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}
partynames = c("M","L","KD","C","S","MP","V","SD")
pSmooth = matrix(NA, ncol=length(partynames), nrow=sum(dateDiff2))
colnames(pSmooth) = partynames
j = 1
for(i in partynames){
  pSmooth[,j] = rep(df3[,i],dateDiff2)
  j= j+1
}


y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
dat2 = data.frame(M = pSmooth[,'M'],L = pSmooth[,'L'], KD= pSmooth[,'KD'], C = pSmooth[,'C'],
                  S = pSmooth[,'S'], V = pSmooth[,'V'], MP =  pSmooth[,'MP'], SD = pSmooth[,'SD'],
                  Date = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.dat2 = dat2 %>%
  group_by(month, year) %>%
  summarise(Mv = sum(M*n),Lv = sum(L*n), KDv = sum(KD*n), Cv = sum(C*n),
            Sv = sum(S*n), Vv= sum(V*n), MPv = sum(MP*n) , SDv = sum(SD*n),sample.size=sum(n),
            propM=Mv/sample.size, propL=Lv/sample.size, propKD=KDv/sample.size, propC=Cv/sample.size,
            propS=Sv/sample.size, propV=Vv/sample.size, propMP=MPv/sample.size, propSD=SDv/sample.size)

new.dat3 = new.dat2[order(new.dat2$year),]
new.dat3

jags.novus ='
model{
#observed model
for(j in 1:nparties){
for(i in 1:npolls){
x[i,j] ~ dnorm(y[i,j],prec[i,j])
}
}
}
'


prec=matrix(NA,ncol=8, nrow=nrow(new.dat3))
colnames(prec) = colnames(new.dat3[,12:19])
for(i in 12:19){
  for(j in 1:nrow(new.dat3)){
    prec[j,(i-11)] = as.numeric(ifelse(is.na(new.dat3[j,i]), NA ,(1 / (new.dat3[j,i]*(1-new.dat3[j,i])/new.dat3[i,'sample.size']))))
  }
}

y = data.frame(new.dat3[,12:19])
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data.novus = list(prec = prec, y = y, npolls = length(new.dat3$propM), nparties=ncol(y))
writeLines(jags.novus,con="novus.bug")

system.time(jags_novus <- jags.model("novus.bug", data = data.novus, n.chain=3))
ninter=10000

system.time(out.novus <- coda.samples(jags_novus,variable.names = c("x"), n.iter = ninter,thin = 5)) #
sum.novus = summary(out.novus)
out.novus_x = out.novus[,which(regexpr("x", row.names(sum.novus$statistics))==1)]
sum.novus_x = sum.novus$statistics[which(regexpr("x", row.names(sum.novus$statistics))==1),]

mean.novus = matrix(NA, ncol=ncol(y), nrow=nrow(new.dat3))
ind.start = 1
ind.end = nrow(new.dat3)
high.novus = matrix(NA, ncol=ncol(y), nrow=nrow(new.dat3))
low.novus = matrix(NA, ncol=ncol(y), nrow=nrow(new.dat3))
states.novus= list()
for(i in 1:ncol(y)){
  ind.start[i+1] = i*nrow(new.dat3)+1
  ind.end[i+1] =  ind.start[i+1]+as.numeric(nrow(new.dat3))-1
  mean.novus[,i] = sum.novus_x[ind.start[i]:ind.end[i],1]
  low.novus[,i] = mean.novus[,i] - 1.96 * sum.novus_x[ind.start[i]:ind.end[i],2]
  high.novus[,i] = mean.novus[,i] + 1.96 * sum.novus_x[ind.start[i]:ind.end[i],2]
  states.novus[[i]] = out.novus_x[,ind.start[i]:ind.end[i]]
}



##########################################################
################ POSTERIOR PREDICTIVE ####################
##########################################################
set.seed(901207)
i = sample(1:3,1)
rChain = list()
rChain[[1]] = states.novus[[1]][i];rChain[[2]] = states.novus[[2]][i];rChain[[3]] = states.novus[[3]][i]
rChain[[4]] = states.novus[[4]][i];rChain[[5]] = states.novus[[5]][i];rChain[[6]] = states.novus[[6]][i]
rChain[[7]] = states.novus[[7]][i];rChain[[8]] = states.novus[[8]][i]

rrr = NULL
for(i in 1: nrow(new.dat3)){
  rr = paste(new.dat3[i,'year'], new.dat3[i,'month'], sep="-", collapse="")
  rrr[i] = paste(rr, "-15", sep="", colapse="")
}

testMin.novus = matrix(NA,ncol=8, nrow=100)
testMax.novus = matrix(NA,ncol=8, nrow=100)
testMean.novus = matrix(NA,ncol=8, nrow=100)
testVar.novus = matrix(NA,ncol=8, nrow=100)
negval.novus = matrix(NA,ncol=8, nrow=100)
above1.novus = matrix(NA,ncol=8, nrow=100)
colnames(testMin.novus) = colnames(testMax.novus) = colnames(testMean.novus) = c("M","L","KD","C","S","MP","V","SD")
nsim = dim(out.novus[[1]])[1]

for(i in 1:100){
  for(j in 1:ncol(y)){
    yrep.novus = sapply(1:nsim, function(s) rnorm(length(rrr),unlist(rChain[[j]][s,]), 1/prec[,j]))
    min_rep = apply(yrep.novus,2,min)
    testMin.novus[i,j] = sum(ifelse(min_rep>= min(df[,j]),1,0))/length(min_rep)
    max_rep = apply(yrep.novus,2,max)
    testMax.novus[i,j] = sum(ifelse(max_rep>= max(df[,j]),1,0))/length(max_rep)
    mean_rep = apply(yrep.novus,2,mean)
    testMean.novus[i,j] = sum(ifelse(mean_rep>= mean(df[,j]),1,0))/length(mean_rep)
    var_rep = apply(yrep.novus,2,var)
    testVar.novus[i,j] = sum(ifelse(var_rep>= var(df[,j]),1,0))/length(var_rep)
    negval.novus[i,j] = sum(ifelse(yrep.novus<0,1,0))/(dim(yrep.novus)[1]*dim(yrep.novus)[2])
    above1.novus[i,j] = sum(ifelse(yrep.novus>1,1,0))/(dim(yrep.novus)[1]*dim(yrep.novus)[2])
  }
}

for(i in 1:ncol(y)){
  print(paste(colnames(testMin.novus)[i],":"))
  print(paste("Min:",mean(testMin.novus[,i]), sep=" "))
  print(paste("Max:",mean(testMax.novus[,i]), sep=" "))
  print(paste("Mean:",mean(testMean.novus[,i]), sep=" "))
  print(paste("Var:",mean(testVar.novus[,i]), sep=" "))
  print(paste("Neg. Val:",mean(negval.novus[,i]), sep=" "))
  print(paste("Above 1:",mean(above1.novus[,i]), sep=" "))
}


dat_cb = list()
for(i in 1:ncol(y2)){
  dat_cb[[i]] = sapply(1:nsim, function(s) rnorm(length(rrr),unlist(rChain[[i]][s,]), 1/prec[,i]))
}


dat_low = matrix(NA, ncol=ncol(y), nrow=nrow(y))
dat_high = matrix(NA, ncol=ncol(y), nrow=nrow(y))
for(i in 1:ncol(y2)){
  dat_low[,i] = apply(dat_cb[[i]], 1, mean) - (apply(dat_cb[[i]], 1, sd)*1.96)
  dat_high[,i] = apply(dat_cb[[i]], 1, mean) + (apply(dat_cb[[i]], 1, sd)*1.96)
}

################################################
################### PLOTS ######################
################################################

rrr = NULL
for(i in 1: nrow(new.dat3)){
  rr = paste(new.dat3[i,'year'], new.dat3[i,'month'], sep="-", collapse="")
  rrr[i] = paste(rr, "-15", sep="", colapse="")
}

basic_plot = list()
df4 = df3[,c("M","L","KD","C","S","V","MP","SD","startDate","endDate","house","n","Date")]
cols = c("blue","lightblue3","darkblue","chartreuse3","red","darkred","forestgreen","skyblue3")
library(ggplot2)
for(i in 1:ncol(mean.novus)){
  plot_df  = data.frame(party = mean.novus[,i]*100, time=as.Date(rrr),
                        low=low.novus[,i]*100, high=high.novus[,i]*100, party2=rep(colnames(df4)[i],nrow(mean.novus)),
                         high2 = dat_high[,i]*100, low2=dat_low[,i]*100)
  points =  data.frame(x=seq(df$startDate[1]-1,by='days',length=df4$Date[length(df4$Date)])[df4$Date],
                       y=df4[,i]*100, house=df4$house, party=rep(colnames(df4))[i],length(df4$Date[length(df4$Date)][df4$Date]))
  basic_plot[[i]] <- ggplot(plot_df) +
    aes(x = time, y = party) +
    geom_line(col=cols[i], alpha=1)  +
    geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill=cols[i]) +
    geom_ribbon(aes(ymin=low2, ymax=high2), alpha=0.5, fill=cols[i]) +
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




#############################################
########## 2010 ELECTION ####################
#############################################

library(dplyr)

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
end.date = as.Date(elec$Date[2])
dayssinceorigStart = julian(df$startDate, origin=orig.date)
dayssinceorigEnd = julian(df$endDate, origin=orig.date)
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
df = df[-which(df$startDate>=end.date),]
df = df[-which(df$endDate>=end.date),]
tail(df)

df3 = df[order(df$Date),]

dateDiff = df3[,'endDate'] - df3[,'startDate']
dateDiff2 = dateDiff+1
nDay = df3$n/as.numeric(dateDiff2)
MSmooth = rep(df3$M,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(df3$house, dateDiff2)

ee = list()
for(i in 1:nrow(df3)){
  ee[[i]] = seq(df3[i,'startDate'], df3[i,'endDate'], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(df3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}
partynames = c("M","L","KD","C","S","MP","V","SD")
pSmooth = matrix(NA, ncol=length(partynames), nrow=sum(dateDiff2))
colnames(pSmooth) = partynames
j = 1
for(i in partynames){
  pSmooth[,j] = rep(df3[,i],dateDiff2)
  j= j+1
}


y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
dat2 = data.frame(M = pSmooth[,'M'],L = pSmooth[,'L'], KD= pSmooth[,'KD'], C = pSmooth[,'C'],
                  S = pSmooth[,'S'], V = pSmooth[,'V'], MP =  pSmooth[,'MP'], SD = pSmooth[,'SD'],
                  Date = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.dat2 = dat2 %>%
  group_by(month, year) %>%
  summarise(Mv = sum(M*n),Lv = sum(L*n), KDv = sum(KD*n), Cv = sum(C*n),
            Sv = sum(S*n), Vv= sum(V*n), MPv = sum(MP*n) , SDv = sum(SD*n),sample.size=sum(n),
            propM=Mv/sample.size, propL=Lv/sample.size, propKD=KDv/sample.size, propC=Cv/sample.size,
            propS=Sv/sample.size, propV=Vv/sample.size, propMP=MPv/sample.size, propSD=SDv/sample.size)

new.dat3 = new.dat2[order(new.dat2$year),]
tail(new.dat3)

jags.novus ='
model{
#observed model
for(j in 1:nparties){
for(i in 1:npolls){
x[i,j] ~ dnorm(y[i,j],prec[i,j])
}
}
}
'

prec=matrix(NA,ncol=8, nrow=nrow(new.dat3))
colnames(prec) = colnames(new.dat3[,12:19])
for(i in 12:19){
  for(j in 1:nrow(new.dat3)){
    prec[j,(i-11)] = as.numeric(ifelse(is.na(new.dat3[j,i]), NA ,(1 / (new.dat3[j,i]*(1-new.dat3[j,i])/new.dat3[i,'sample.size']))))
  }
}

y = data.frame(new.dat3[,12:19])
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data.novus = list(prec = prec, y = y, npolls = length(new.dat3$propM), nparties=ncol(y))
writeLines(jags.novus,con="novus.bug")

system.time(jags_novus <- jags.model("novus.bug", data = data.novus, n.chain=3))
ninter=10000

system.time(out.novus <- coda.samples(jags_novus,variable.names = c("x"), n.iter = ninter,thin = 5)) #
sum.novus = summary(out.novus)
out.novus_x = out.novus[,which(regexpr("x", row.names(sum.novus$statistics))==1)]
sum.novus_x = sum.novus$statistics[which(regexpr("x", row.names(sum.novus$statistics))==1),]

mean.novus2010 = matrix(NA, ncol=ncol(y), nrow=nrow(new.dat3))
ind.start = 1
ind.end = nrow(new.dat3)
high.novus2010 = matrix(NA, ncol=ncol(y), nrow=nrow(new.dat3))
low.novus2010 = matrix(NA, ncol=ncol(y), nrow=nrow(new.dat3))
states.novus= list()
for(i in 1:ncol(y)){
  ind.start[i+1] = i*nrow(new.dat3)+1
  ind.end[i+1] =  ind.start[i+1]+as.numeric(nrow(new.dat3))-1
  mean.novus2010[,i] = sum.novus_x[ind.start[i]:ind.end[i],1]
  low.novus2010[,i] = mean.novus2010[,i] - (1.96 * sum.novus_x[ind.start[i]:ind.end[i],2])
  high.novus2010[,i] = mean.novus2010[,i] + (1.96 * sum.novus_x[ind.start[i]:ind.end[i],2])
}

pred2010 = matrix(NA, ncol=5, nrow=8)
row.names(pred2010) = c("M","L","KD","C","S","MP","V","SD")
colnames(pred2010) = c("Elec_res","MAP","Low","High","Diff")
for(i in 1:ncol(y)){
  pred2010[i,] =  cbind(elec[2,i],mean.novus2010[nrow(mean.novus2010),i],low.novus2010[nrow(low.novus2010),i],high.novus2010[nrow(high.novus2010),i],(mean.novus2010[nrow(mean.novus2010),i]-elec[2,i]))
}

pred2010


#############################################
########## 2014 ELECTION ####################
#############################################

library(dplyr)

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
end.date = as.Date(elec$Date[3])
dayssinceorigStart = julian(df$startDate, origin=orig.date)
dayssinceorigEnd = julian(df$endDate, origin=orig.date)
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
df = df[-which(df$startDate>=end.date),]
df = df[-which(df$endDate>=end.date),]
head(df)

df3 = df[order(df$Date),]

dateDiff = df3[,'endDate'] - df3[,'startDate']
dateDiff2 = dateDiff+1
nDay = df3$n/as.numeric(dateDiff2)
MSmooth = rep(df3$M,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(df3$house, dateDiff2)

ee = list()
for(i in 1:nrow(df3)){
  ee[[i]] = seq(df3[i,'startDate'], df3[i,'endDate'], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(df3)){
  dateSmooth = c(dateSmooth, ee[[i]])
}
partynames = c("M","L","KD","C","S","MP","V","SD")
pSmooth = matrix(NA, ncol=length(partynames), nrow=sum(dateDiff2))
colnames(pSmooth) = partynames
j = 1
for(i in partynames){
  pSmooth[,j] = rep(df3[,i],dateDiff2)
  j= j+1
}


y = NULL
m =  NULL
for(i in 1:length(dateSmooth)){
  y[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[1])
  m[i] = as.numeric(unlist(strsplit(as.character(dateSmooth[i]), "-"))[2])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
dat2 = data.frame(M = pSmooth[,'M'],L = pSmooth[,'L'], KD= pSmooth[,'KD'], C = pSmooth[,'C'],
                  S = pSmooth[,'S'], V = pSmooth[,'V'], MP =  pSmooth[,'MP'], SD = pSmooth[,'SD'],
                  Date = as.numeric(dateSmooth.num), n = nSmooth, house = houseSmooth, year= y, month = m)

new.dat2 = dat2 %>%
  group_by(month, year) %>%
  summarise(Mv = sum(M*n),Lv = sum(L*n), KDv = sum(KD*n), Cv = sum(C*n),
            Sv = sum(S*n), Vv= sum(V*n), MPv = sum(MP*n) , SDv = sum(SD*n),sample.size=sum(n),
            propM=Mv/sample.size, propL=Lv/sample.size, propKD=KDv/sample.size, propC=Cv/sample.size,
            propS=Sv/sample.size, propV=Vv/sample.size, propMP=MPv/sample.size, propSD=SDv/sample.size)

new.dat3 = new.dat2[order(new.dat2$year),]
new.dat3

jags.novus ='
model{
#observed model
for(j in 1:nparties){
for(i in 1:npolls){
x[i,j] ~ dnorm(y[i,j],prec[i,j])
}
}
}
'

prec=matrix(NA,ncol=8, nrow=nrow(new.dat3))
colnames(prec) = colnames(new.dat3[,12:19])
for(i in 12:19){
  for(j in 1:nrow(new.dat3)){
    prec[j,(i-11)] = as.numeric(ifelse(is.na(new.dat3[j,i]), NA ,(1 / (new.dat3[j,i]*(1-new.dat3[j,i])/new.dat3[i,'sample.size']))))
  }
}

y = data.frame(new.dat3[,12:19])
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data.novus = list(prec = prec, y = y, npolls = length(new.dat3$propM), nparties=ncol(y))
writeLines(jags.novus,con="novus.bug")

system.time(jags_novus <- jags.model("novus.bug", data = data.novus, n.chain=3))
ninter=10000

system.time(out.novus <- coda.samples(jags_novus,variable.names = c("x"), n.iter = ninter,thin = 5)) #
sum.novus = summary(out.novus)
out.novus_x = out.novus[,which(regexpr("x", row.names(sum.novus$statistics))==1)]
sum.novus_x = sum.novus$statistics[which(regexpr("x", row.names(sum.novus$statistics))==1),]

mean.novus2014 = matrix(NA, ncol=ncol(y), nrow=nrow(new.dat3))
ind.start = 1
ind.end = nrow(new.dat3)
high.novus2014 = matrix(NA, ncol=ncol(y), nrow=nrow(new.dat3))
low.novus2014 = matrix(NA, ncol=ncol(y), nrow=nrow(new.dat3))
states.novus= list()
for(i in 1:ncol(y)){
  ind.start[i+1] = i*nrow(new.dat3)+1
  ind.end[i+1] =  ind.start[i+1]+as.numeric(nrow(new.dat3))-1
  mean.novus2014[,i] = sum.novus_x[ind.start[i]:ind.end[i],1]
  low.novus2014[,i] = mean.novus2014[,i] - (1.96 * sum.novus_x[ind.start[i]:ind.end[i],2])
  high.novus2014[,i] = mean.novus2014[,i] + (1.96 * sum.novus_x[ind.start[i]:ind.end[i],2])
}

pred2014 = matrix(NA, ncol=5, nrow=8)
row.names(pred2014) = c("M","L","KD","C","S","MP","V","SD")
colnames(pred2014) = c("Elec_res","MAP","Low","High","Diff")
for(i in 1:ncol(y)){
  pred2014[i,] =  cbind(elec[3,i],mean.novus2014[nrow(mean.novus2014),i],low.novus2014[nrow(low.novus2014),i],high.novus2014[nrow(high.novus2014),i],(mean.novus2014[nrow(mean.novus2014),i]-elec[3,i]))
}

pred2014