library(rjags, lib="C:/Users/mirhu86/Documents/packages")
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
head(df)
tail(df)
orig.date = df$startDate[1]-2
end.date = df$endDate[length(df$endDate)]
df$length = as.numeric(df$endDate - df$startDate)+1
head(df)
partynames = c("M","L","KD","C","S","MP","V","SD","O")
df$Day = julian(df$startDate,orig.date)-1

Y = df[,partynames] * df$n
Y = sapply(Y, function(x) round(x,0))
df$house = factor(df$house, levels=c("Demoskop","Inizio", "Ipsos" ,"Novus" ,"SCB" ,"Sentio" ,
                                     "Sifo" ,"Skop" ,"SVT", "United Minds", "YouGov","Election"))

df$Date[nrow(df)]
data = list(nperiod = 3500,k=df$length,
            npolls = nrow(df),  nparties = ncol(Y), y = Y, day = df$Day,
            n = rowSums(Y), b=matrix(NA,ncol=9, nrow=nrow(df)),
            z=array(NA, dim=c(3500,9,nrow(df))))
tail(df)


diri_diri_lm <- '
model {
#observed model
for(i in 1:npolls) { 
n2[i] <- round(n[i]/k[i])
for(j in 1:k[i]){
z[j,1:nparties,i] <- x[day[i]+j, 1:nparties] 
}
for(l in 1:nparties){
b[i,l] <- sum((z[1:k[i],l,i]*n2[i])/n[i])

}
b2[i,1:nparties] <- b[i,1:nparties] * n[i]
y[i, 1:nparties] ~ ddirch(b2[i,1:nparties])

}
#dynamic model
for(i in 2:nperiod) {
Alpha[i, 1:nparties] <- x[i-1,  1:nparties]* conc
x[i, 1:nparties] ~ ddirch(Alpha[i, 1:nparties])
}
for (i in 1:nparties) { 
alpha[i] ~ dunif(100, 1000) 
}

x[1, 1:nparties] ~ ddirch(alpha[])
conc ~ dgamma(1,1)

}
'
writeLines(diri_diri_lm,con="diri_diri_lm.bug")
system.time(jags_ddlm <- jags.model("diri_diri_lm.bug", data = data, n.chain=3, n.adapt=10000)) 
ninter=30000
system.time(add_out <- coda.samples(jags_ddlm,variable.names = c("x","conc"), n.iter = ninter, thin = 15, burnin=3000))
system.time(sum_add <- summary(add_out))
addout_x2 = add_out[,which(regexpr("x", row.names(sum_add$statistics))==1)]
out_conc = add_out[,which(regexpr("conc", row.names(sum_add$statistics))==1)]
            
hist(rbind(out_conc[[1]],out_conc[[2]],out_conc[[3]]), breaks=30, main="", xlab="Concentration parameter", freq=FALSE, las=1)
lines(density(rbind(out_conc[[1]],out_conc[[2]],out_conc[[3]])), lwd=2, col="purple")
abline(v=mean(rbind(out_conc[[1]],out_conc[[2]],out_conc[[3]])), lwd=2)
text(x=mean(rbind(out_conc[[1]],out_conc[[2]],out_conc[[3]]))+6,y=0.008 ,labels=round(mean(rbind(out_conc[[1]],out_conc[[2]],out_conc[[3]]))), cex=0.7)
            

nperiods = 3500
nsim = 6000
mean_add2 = matrix(NA, ncol=9, nrow=nperiods)
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
##################################################################
############# POSTERIOR PREDICTIVE CHECKING ######################
##################################################################
partynames = c("M","L","KD","C","S","MP","V","SD","O")

rChain = list()
rChain[[partynames[1]]] = states_add2[[1]];rChain[[partynames[2]]] = states_add2[[2]];rChain[[partynames[3]]] = states_add2[[3]]
rChain[[partynames[4]]] = states_add2[[4]];rChain[[partynames[5]]] = states_add2[[5]];rChain[[partynames[6]]] = states_add2[[6]]
rChain[[partynames[7]]] = states_add2[[7]];rChain[[partynames[8]]] = states_add2[[8]];rChain[[partynames[9]]] = states_add2[[9]]
str(rChain)


df$Start = julian(df$startDate,orig.date)

yrep1 = array(NA, dim=c(nrow(df), 9,6000))
library(gtools)
for(i in 1:dim(rChain[[1]])[1]){
  for(j in 1:nrow(df)){
    k = df$length[j]
    n2 = round(df$n[j]/df$length[j])
    M = sum(rChain[['M']][i,c(df$Start[j]:(df$Start[j]+k))]*n2)/df$n[j]
    L = sum(rChain[['L']][i,c(df$Start[j]:(df$Start[j]+k))]*n2)/df$n[j]
    KD = sum(rChain[['KD']][i,c(df$Start[j]:(df$Start[j]+k))]*n2)/df$n[j]
    C = sum(rChain[['C']][i,c(df$Start[j]:(df$Start[j]+k))]*n2)/df$n[j]
    S = sum(rChain[['S']][i,c(df$Start[j]:(df$Start[j]+k))]*n2)/df$n[j]
    MP = sum(rChain[['MP']][i,c(df$Start[j]:(df$Start[j]+k))]*n2)/df$n[j]
    V = sum(rChain[['V']][i,c(df$Start[j]:(df$Start[j]+k))]*n2)/df$n[j]
    SD = sum(rChain[['SD']][i,c(df$Start[j]:(df$Start[j]+k))]*n2)/df$n[j]
    O = sum(rChain[['O']][i,c(df$Start[j]:(df$Start[j]+k))]*n2)/df$n[j]
    
    alphas = c(M,L,KD,C,S,MP,V,SD,O) * df$n[j]
    
    yrep1[j,,i] = rdirichlet(1,alphas)
    
  }
}


bayespval = matrix(NA,nrow=9, ncol=6)
rownames(bayespval) = partynames
colnames(bayespval) = c("Min","Max","Mean","Var","Neg.val","Above1")

for(i in c(1:8,13)){
  bayespval[i,1] = sum(ifelse(apply(yrep1[,i,],2,min)>=min(df[,i]),1,0))/nsim
  bayespval[i,2] = sum(ifelse(apply(yrep1[,i,],2,max)>=max(df[,i]),1,0))/nsim
  bayespval[i,3] = sum(ifelse(apply(yrep1[,i,],2,mean)>=mean(df[,i]),1,0))/nsim
  bayespval[i,4] = sum(ifelse(apply(yrep1[,i,],2,var)>=var(df[,i]),1,0))/nsim
  bayespval[i,5] = sum(ifelse(yrep1[,i,]<0,1,0))/(dim(yrep1[,i,])[1]*dim(yrep1[,i,])[2])
  bayespval[i,6] = sum(ifelse(yrep1[,i,]>1,1,0))/(dim(yrep1[,i,])[1]*dim(yrep1[,i,])[2])
}
bayespval


#################################################
################# PLOTS #########################
#################################################

basic_plot2 = list()

head(y)
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)

cols = c("blue","lightblue3","darkblue","chartreuse3","red","forestgreen","darkred","skyblue3","yellow3")
library(ggplot2)
for(i in 1:9){
  pp = partynames[i]
  plot_df = data.frame(party = mean_add2[,i] ,  low=low_add2[,i]*100, high=high_add2[,i]*100,
                       time=seq(orig.date,by='days', length=nperiods), party2 = rep(partynames[i], nperiods))
  points = data.frame(x=seq(orig.date,by='days',length=nperiods)[df$Date], 
                      y=df[,pp]*100,  party=rep(partynames[i],length(df$Date[length(df$Date)][df$Date])))
  # ,high_dat=dat_high2[,i]*100, low_dat=dat_low2[,i]*100 )
  basic_plot2[[i]] <-  ggplot(plot_df) +
    aes(x = time, y = party*100) +
    geom_line(col=cols[i], alpha=1)  +
    geom_ribbon(aes(ymin=low, ymax=high), alpha=0.4, fill=cols[i]) + 
    #geom_ribbon(data=points, aes(x=x, ymin=low_dat, ymax=high_dat), alpha=0.5, fill=cols[i], inherit.aes = FALSE) +
    geom_point(data=points, aes(x=x, y=y), alpha = 1, color=cols[i], shape=16, size=1) +    
    labs(x="Date", y=paste("Support for", sep=" ", unique(plot_df$party2), paste("(%)", sep=" "), collapse="")) +
    facet_wrap( ~ party2, ncol=1, nrow=1)+
    theme_bw() +
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

basic_plot2[[1]]
basic_plot2[[2]]
basic_plot2[[3]]
basic_plot2[[4]]
basic_plot2[[5]]
basic_plot2[[6]]
basic_plot2[[7]]
basic_plot2[[8]]
basic_plot2[[9]]
multiplot(basic_plot2[[1]], basic_plot2[[2]], basic_plot2[[3]], basic_plot2[[4]], 
          basic_plot2[[5]], basic_plot2[[6]], basic_plot2[[7]], basic_plot2[[8]], basic_plot2[[9]], cols=3)



####################################################
############ PREDICT 2010 ELECTION #################
####################################################
library(rjags, lib="C:/Users/mirhu86/Documents/packages")
data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)
colnames(polls)[4] = 'L'

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
partynames =  c("M","L","KD","C","S","MP","V","SD","O")
row.names(elec) = c("2006","2010","2014") #"2002",
elec$Date = c( as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14')) #as.Date("2002-09-12"),
n=c((0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000)) #(0.801*6722*1000),
## http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf
df2 = data.frame(M=elec$M, L=elec$L,C=elec$C,KD=elec$KD,S=elec$S, V=elec$V,
                 MP=elec$MP,SD=elec$SD,O=elec$O, startDate=elec$Date, endDate=elec$Date,
                 house="Election" ,n=n)
df = rbind(df, df2)
df = df[order(df$startDate),]
orig.date = df$startDate[1]-2
end.date = elec$Date[2]
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
df$length = as.numeric(df$endDate - df$startDate)+1
df = df[-which(df$endDate>=end.date),]
tail(df)

partynames = c("M","L","KD","C","S","MP","V","SD","O")
df$Day = julian(df$startDate,orig.date)-1

Y = df[,partynames] * df$n
Y = sapply(Y, function(x) round(x,0))
df$house = factor(df$house, levels=c("Demoskop","Inizio", "Ipsos" ,"Novus" ,"SCB" ,"Sentio" ,
                                     "Sifo" ,"Skop" ,"SVT", "United Minds", "YouGov","Election"))
data = list(nperiod = df$Date[nrow(df)]+as.numeric(julian(elec$Date[2], df$endDate[nrow(df)])),k=df$length,
            npolls = nrow(df),  nparties = ncol(Y), y = Y, day = df$Day,
            n = rowSums(Y), b=matrix(NA,ncol=9, nrow=nrow(df)),
            z=array(NA, dim=c(df$Date[nrow(df)]+as.numeric(julian(elec$Date[2], df$endDate[nrow(df)])),9,nrow(df))))

tail(df)


diri_diri_lm <- '
model {
#observed model
for(i in 1:npolls) { 
n2[i] <- round(n[i]/k[i])
for(j in 1:k[i]){
z[j,1:nparties,i] <- x[day[i]+j, 1:nparties] 
}
for(l in 1:nparties){
b[i,l] <- sum((z[1:k[i],l,i]*n2[i])/n[i])

}
b2[i,1:nparties] <- b[i,1:nparties] * n[i]
y[i, 1:nparties] ~ ddirch(b2[i,1:nparties])

}
#dynamic model
for(i in 2:nperiod) {
Alpha[i, 1:nparties] <- x[i-1,  1:nparties]* conc
x[i, 1:nparties] ~ ddirch(Alpha[i, 1:nparties])
}
for (i in 1:nparties) { 
alpha[i] ~ dunif(100, 1000) 
}

x[1, 1:nparties] ~ ddirch(alpha[])
conc ~ dgamma(1,1)

}
'

writeLines(diri_diri_lm,con="diri_diri_lm.bug")
system.time(jags_ddlm <- jags.model("diri_diri_lm.bug", data = data, n.chain=3, n.adapt=10000)) 
ninter=30000
system.time(all_out22010 <- coda.samples(jags_ddlm,variable.names = c("x","conc"), n.iter = ninter, thin = 15, burnin=3000))
system.time(sum_add22010 <- summary(all_out22010))
add_out2010 = all_out22010[,which(regexpr("x", row.names(sum_add22010$statistics))==1)]

nperiods= df$Date[nrow(df)]+as.numeric(julian(elec$Date[2], df$endDate[nrow(df)]))
nsim = dim(add_out2010[[1]])[1]*3
mean_add2010 = matrix(NA, ncol=9, nrow=nperiods)
ind.start = 1
ind.end = nperiods
high_add2010 = matrix(NA, ncol=9, nrow=nperiods)
low_add2010 =  matrix(NA, ncol=9, nrow=nperiods)
percentile5 = round(nsim*0.05)
percentile95 = round(nsim*0.95)
states_add2010= list()
colnames(mean_add2010) = colnames(low_add2010) =colnames(high_add2010) = partynames
for(i in 1:9){
  ind.start[i+1] = i*nperiods+1
  ind.end[i+1] =  ind.start[i+1]+nperiods-1
  mean_add2010[,i] = apply(rbind(add_out2010[[1]][,ind.start[i]:ind.end[i]],add_out2010[[2]][,ind.start[i]:ind.end[i]],add_out2010[[3]][,ind.start[i]:ind.end[i]]),2,mean)
  low_add2010[,i] = apply(rbind(add_out2010[[1]][,ind.start[i]:ind.end[i]],add_out2010[[2]][,ind.start[i]:ind.end[i]],add_out2010[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile5])
  high_add2010[,i] = apply(rbind(add_out2010[[1]][,ind.start[i]:ind.end[i]],add_out2010[[2]][,ind.start[i]:ind.end[i]],add_out2010[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile95])
}

tail(df)

pred22010 = matrix(NA, ncol=5, nrow=9)
row.names(pred22010) = partynames
colnames(pred22010) = c("Elec_res","MAP","Low","High","Diff")

elec2 = elec[,partynames]
for(i in 1:9){
  pred22010[i,] =  cbind(elec2[2,i],mean_add2010[df$Date[nrow(df)]+as.numeric(julian(elec$Date[2], df$endDate[nrow(df)])),i],low_add2010[df$Date[nrow(df)]+as.numeric(julian(elec$Date[2], df$endDate[nrow(df)])),i],high_add2010[nrow(high_add2010),i],(mean_add2010[df$Date[nrow(df)]+as.numeric(julian(elec$Date[2], df$endDate[nrow(df)])),i]-elec2[2,i]))
}
pred22010




rmse_elec2010 = matrix(NA, ncol=1, nrow=9)
row.names(rmse_elec2010) = row.names(pred22010)
ind.start = 1
ind.end = nperiods
for(i in 1:9){
  ind.start[i+1] = i*nperiods+1
  ind.end[i+1] =  ind.start[i+1]+nperiods-1
  rmse_elec2010[i,]  = sqrt((sum((rbind(add_out2010[[1]][,ind.start[i]:ind.end[i]],add_out2010[[2]][,ind.start[i]:ind.end[i]],
                                       add_out2010[[3]][,ind.start[i]:ind.end[i]])[,df$Date[nrow(df)]+as.numeric(julian(elec$Date[2], df$endDate[nrow(df)]))]-elec2[2,i])^2)/nsim))
}
rmse_elec2010

####################################################
############ PREDICT 2014 ELECTION #################
####################################################

library(rjags, lib="C:/Users/mirhu86/Documents/packages")
data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)
colnames(polls)[4] = 'L'

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
df2 = data.frame(M=elec$M, L=elec$L,C=elec$C,KD=elec$KD,S=elec$S, V=elec$V,
                 MP=elec$MP,SD=elec$SD, O=elec$O,startDate=elec$Date, endDate=elec$Date, 
                 house="Election" ,n=n)
df = rbind(df, df2)
df = df[order(df$startDate),]
orig.date = df$startDate[1]-2
end.date = elec$Date[3]
dayssinceorigStart = julian(df$startDate, origin=orig.date) 
dayssinceorigEnd = julian(df$endDate, origin=orig.date) 
df$Date = floor((dayssinceorigStart + dayssinceorigEnd ) / 2)
df$length = as.numeric(df$endDate - df$startDate)+1
df = df[-which(df$endDate>=end.date),]
tail(df)

partynames = c("M","L","KD","C","S","MP","V","SD","O")
df$Day = julian(df$startDate,orig.date)-1

Y = df[,partynames] * df$n
Y = sapply(Y, function(x) round(x,0))
df$house = factor(df$house, levels=c("Demoskop","Inizio", "Ipsos" ,"Novus" ,"SCB" ,"Sentio" ,
                                     "Sifo" ,"Skop" ,"SVT", "United Minds", "YouGov","Election"))

data = list(nperiod = df$Date[nrow(df)]+as.numeric(julian(elec$Date[3], df$endDate[nrow(df)])),k=df$length,
            npolls = nrow(df),  nparties = ncol(Y), y = Y, day = df$Day,
            n = rowSums(Y), b=matrix(NA,ncol=9, nrow=nrow(df)),
            z=array(NA, dim=c(df$Date[nrow(df)]+as.numeric(julian(elec$Date[3], df$endDate[nrow(df)])),9,nrow(df))))


diri_diri_lm <- '
model {
#observed model
for(i in 1:npolls) { 
n2[i] <- round(n[i]/k[i])
for(j in 1:k[i]){
z[j,1:nparties,i] <- x[day[i]+j, 1:nparties] 
}
for(l in 1:nparties){
b[i,l] <- sum((z[1:k[i],l,i]*n2[i])/n[i])

}
b2[i,1:nparties] <- b[i,1:nparties] * n[i]
y[i, 1:nparties] ~ ddirch(b2[i,1:nparties])

}
#dynamic model
for(i in 2:nperiod) {
Alpha[i, 1:nparties] <- x[i-1,  1:nparties]* conc
x[i, 1:nparties] ~ ddirch(Alpha[i, 1:nparties])
}
for (i in 1:nparties) { 
alpha[i] ~ dunif(100, 1000) 
}

x[1, 1:nparties] ~ ddirch(alpha[])
conc ~ dgamma(1,1)

}
'
writeLines(diri_diri_lm,con="diri_diri_lm.bug")
system.time(jags_ddlm <- jags.model("diri_diri_lm.bug", data = data, n.chain=3, n.adapt=10000)) 
ninter=30000
system.time(all_out22014 <- coda.samples(jags_ddlm,variable.names = c("x","conc"), n.iter = ninter, thin = 15, burnin=3000))
sum_all22014 = summary(all_out22014)
add_out2014 = all_out22014[,which(regexpr("x", row.names(sum_all22014$statistics))==1)]


nperiods=df$Date[nrow(df)]+as.numeric(julian(elec$Date[3], df$endDate[nrow(df)]))
nsim = dim(all_out22014[[1]])[1]*3
mean_add2014 = matrix(NA, ncol=9, nrow=nperiods)
ind.start = 1
ind.end = nperiods
high_add2014 = high_add22014 = matrix(NA, ncol=9, nrow=nperiods)
low_add2014 = low_add22014 = matrix(NA, ncol=9, nrow=nperiods)
percentile5 = round(nsim*0.05)
percentile95 = round(nsim*0.95)
states_add2014= list()
colnames(mean_add2014) = colnames(low_add2014) =colnames(high_add2014) = partynames
for(i in 1:9){
  ind.start[i+1] = i*nperiods+1
  ind.end[i+1] =  ind.start[i+1]+nperiods-1
  mean_add2014[,i] = apply(rbind(add_out2014[[1]][,ind.start[i]:ind.end[i]],add_out2014[[2]][,ind.start[i]:ind.end[i]],add_out2014[[3]][,ind.start[i]:ind.end[i]]),2,mean)
  low_add2014[,i] = apply(rbind(add_out2014[[1]][,ind.start[i]:ind.end[i]],add_out2014[[2]][,ind.start[i]:ind.end[i]],add_out2014[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile5])
  high_add2014[,i] = apply(rbind(add_out2014[[1]][,ind.start[i]:ind.end[i]],add_out2014[[2]][,ind.start[i]:ind.end[i]],add_out2014[[3]][,ind.start[i]:ind.end[i]]),2, function(x) sort(x)[percentile95])
}

df$Date[nrow(df)]+as.numeric(julian(elec$Date[2], df$endDate[nrow(df)]))

pred22014 = matrix(NA, ncol=5, nrow=9)
row.names(pred22014) = partynames
colnames(pred22014) = c("Elec_res","MAP","Low","High","Diff")
elec2 = elec[,partynames]
for(i in 1:9){
  pred22014[i,] =    cbind(elec2[3,i],mean_add2014[df$Date[nrow(df)]+as.numeric(julian(elec$Date[3], df$endDate[nrow(df)])),i],low_add2014[df$Date[nrow(df)]+as.numeric(julian(elec$Date[3], df$endDate[nrow(df)])),i],high_add2014[df$Date[nrow(df)]+as.numeric(julian(elec$Date[3], df$endDate[nrow(df)])),i],(mean_add2014[df$Date[nrow(df)]+as.numeric(julian(elec$Date[3], df$endDate[nrow(df)])),i]-elec2[3,i]))
  
}
pred22014

ind.start = 1
ind.end = nperiods
rmse_elec2014 = matrix(NA, ncol=1, nrow=9)
row.names(rmse_elec2014) = row.names(pred22014)
for(i in 1:9){
  ind.start[i+1] = i*nperiods+1
  ind.end[i+1] =  ind.start[i+1]+nperiods-1
  rmse_elec2014[i,]  = sqrt((sum((rbind(add_out2014[[1]][,ind.start[i]:ind.end[i]],add_out2014[[2]][,ind.start[i]:ind.end[i]],
                                       add_out2014[[3]][,ind.start[i]:ind.end[i]])[,df$Date[nrow(df)]+as.numeric(julian(elec$Date[3], df$endDate[nrow(df)]))]-elec2[3,i])^2)/nsim))
  
}
mse_elec2014

################
