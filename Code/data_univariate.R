######################################################
################# UNIVARIATE DATA ####################
######################################################

library(rjags, lib="C:/Users/mirhu86/Documents/packages")
data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)

datM = na.omit(polls[,c('M','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datM$house = factor(datM$house)
datM = datM[order(datM$collectPeriodFrom),]
datM$collectPeriodFrom = as.Date(datM$collectPeriodFrom)
datM$collectPeriodTo = as.Date(datM$collectPeriodTo)
datM$PublDate = as.Date(datM$PublDate)


elec = data.frame(rbind(c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0),
                        c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                        c(0.2333, 0.0542, 0.0457, 0.0611, 0.3101, 0.0689, 0.0572, 0.1286)))

colnames(elec) = c("M","L","KD","C","S","MP","V","SD")
row.names(elec) = c("2002","2006","2010","2014") #
elec$Date = c(as.Date("2002-09-12"), as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14')) #
n=c((0.801*6722*1000),(0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000)) #
## http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf

dfM = data.frame(M=elec$M*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datM = rbind(datM, dfM)
datM = datM[order(datM$collectPeriodFrom),]
orig.date= as.Date(datM$collectPeriodFrom[1]-1)
end.date= datM$collectPeriodTo[length(datM$collectPeriodTo)]

datM$collectPeriodFrom.num = julian(datM$collectPeriodFrom,origin=orig.date) #days since origin
datM$collectPeriodTo.num = julian(datM$collectPeriodTo,origin=orig.date) #days since origin
datM$fieldDate.num = floor((datM$collectPeriodFrom.num + datM$collectPeriodTo.num) / 2)
datM$M = datM$M/100


datL = na.omit(polls[,c('FP','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
colnames(datL)[1] <- 'L' 
datL$house = factor(datL$house)
datL = datL[order(datL$collectPeriodFrom),]
datL$collectPeriodFrom = as.Date(datL$collectPeriodFrom)
datL$collectPeriodTo = as.Date(datL$collectPeriodTo)
datL$PublDate = as.Date(datL$PublDate)
dfL = data.frame(L=elec$L*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datL = rbind(datL, dfL)
datL = datL[order(datL$collectPeriodFrom),]
orig.date= as.Date(datL$collectPeriodFrom[1]-1)
end.date= datL$collectPeriodTo[length(datL$collectPeriodTo)]
datL$collectPeriodFrom.num = julian(datL$collectPeriodFrom,origin=orig.date) #days since origin
datL$collectPeriodTo.num = julian(datL$collectPeriodTo,origin=orig.date) #days since origin
datL$fieldDate.num = floor((datL$collectPeriodFrom.num + datL$collectPeriodTo.num) / 2)
datL$L = datL$L/100

datKD = na.omit(polls[,c('KD','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datKD$house = factor(datKD$house)
datKD = datKD[order(datKD$collectPeriodFrom),]
datKD$collectPeriodFrom = as.Date(datKD$collectPeriodFrom)
datKD$collectPeriodTo = as.Date(datKD$collectPeriodTo)
datKD$PublDate = as.Date(datKD$PublDate)
dfKD = data.frame(KD=elec$KD*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datKD = rbind(datKD, dfKD)
datKD = datKD[order(datKD$collectPeriodFrom),]
orig.date= as.Date(datKD$collectPeriodFrom[1]-1)
end.date= datKD$collectPeriodTo[length(datKD$collectPeriodTo)]
datKD$collectPeriodFrom.num = julian(datKD$collectPeriodFrom,origin=orig.date) #days since origin
datKD$collectPeriodTo.num = julian(datKD$collectPeriodTo,origin=orig.date) #days since origin
datKD$fieldDate.num = floor((datKD$collectPeriodFrom.num + datKD$collectPeriodTo.num) / 2)
datKD$KD = datKD$KD/100


datC = na.omit(polls[,c('C','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datC$house = factor(datC$house)
datC = datC[order(datC$collectPeriodFrom),]
datC$collectPeriodFrom = as.Date(datC$collectPeriodFrom)
datC$collectPeriodTo = as.Date(datC$collectPeriodTo)
datC$PublDate = as.Date(datC$PublDate)
dfC = data.frame(C=elec$C*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datC = rbind(datC, dfC)
datC = datC[order(datC$collectPeriodFrom),]
orig.date= as.Date(datC$collectPeriodFrom[1]-1)
end.date= datC$collectPeriodTo[length(datC$collectPeriodTo)]
datC$collectPeriodFrom.num = julian(datC$collectPeriodFrom,origin=orig.date) #days since origin
datC$collectPeriodTo.num = julian(datC$collectPeriodTo,origin=orig.date) #days since origin
datC$fieldDate.num = floor((datC$collectPeriodFrom.num + datC$collectPeriodTo.num) / 2)
datC$C = datC$C/100


datS = na.omit(polls[,c('S','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datS$house = factor(datS$house)
datS = datS[order(datS$collectPeriodFrom),]
datS$collectPeriodFrom = as.Date(datS$collectPeriodFrom)
datS$collectPeriodTo = as.Date(datS$collectPeriodTo)
datS$PublDate = as.Date(datS$PublDate)
dfS = data.frame(S=elec$S*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datS = rbind(datS, dfS)
datS = datS[order(datS$collectPeriodFrom),]
orig.date= as.Date(datS$collectPeriodFrom[1]-1)
end.date= datS$collectPeriodTo[length(datS$collectPeriodTo)]
datS$collectPeriodFrom.num = julian(datS$collectPeriodFrom,origin=orig.date) #days since origin
datS$collectPeriodTo.num = julian(datS$collectPeriodTo,origin=orig.date) #days since origin
datS$fieldDate.num = floor((datS$collectPeriodFrom.num + datS$collectPeriodTo.num) / 2)
datS$S = datS$S/100

datMP = na.omit(polls[,c('MP','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datMP$house = factor(datMP$house)
datMP = datMP[order(datMP$collectPeriodFrom),]
datMP$collectPeriodFrom = as.Date(datMP$collectPeriodFrom)
datMP$collectPeriodTo = as.Date(datMP$collectPeriodTo)
datMP$PublDate = as.Date(datMP$PublDate)
dfMP = data.frame(MP=elec$MP*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datMP = rbind(datMP, dfMP)
datMP = datMP[order(datMP$collectPeriodFrom),]
orig.date= as.Date(datMP$collectPeriodFrom[1]-1)
end.date= datMP$collectPeriodTo[length(datMP$collectPeriodTo)]
datMP$collectPeriodFrom.num = julian(datMP$collectPeriodFrom,origin=orig.date) #days since origin
datMP$collectPeriodTo.num = julian(datMP$collectPeriodTo,origin=orig.date) #days since origin
datMP$fieldDate.num = floor((datMP$collectPeriodFrom.num + datMP$collectPeriodTo.num) / 2)
datMP$MP = datMP$MP/100


datV = na.omit(polls[,c('V','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datV$house = factor(datV$house)
datV = datV[order(datV$collectPeriodFrom),]
datV$collectPeriodFrom = as.Date(datV$collectPeriodFrom)
datV$collectPeriodTo = as.Date(datV$collectPeriodTo)
datV$PublDate = as.Date(datV$PublDate)
dfV = data.frame(V=elec$V*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datV = rbind(datV, dfV)
datV = datV[order(datV$collectPeriodFrom),]
orig.date= as.Date(datV$collectPeriodFrom[1]-1)
end.date= datV$collectPeriodTo[length(datV$collectPeriodTo)]
datV$collectPeriodFrom.num = julian(datV$collectPeriodFrom,origin=orig.date) #days since origin
datV$collectPeriodTo.num = julian(datV$collectPeriodTo,origin=orig.date) #days since origin
datV$fieldDate.num = floor((datV$collectPeriodFrom.num + datV$collectPeriodTo.num) / 2)
datV$V = datV$V/100


datSD = na.omit(polls[,c('SD','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datSD$house = factor(datSD$house)
datSD = datSD[order(datSD$collectPeriodFrom),]
datSD$collectPeriodFrom = as.Date(datSD$collectPeriodFrom)
datSD$collectPeriodTo = as.Date(datSD$collectPeriodTo)
datSD$PublDate = as.Date(datSD$PublDate)
elecSD = data.frame(elec[-1,])
dfSD = data.frame(SD=elecSD$SD*100, collectPeriodFrom=elecSD$Date, collectPeriodTo=elecSD$Date, n=n[-1], PublDate=elecSD$Date, house="Election")
datSD = rbind(datSD, dfSD)
datSD = datSD[order(datSD$collectPeriodFrom),]
orig.date = as.Date(datSD$collectPeriodFrom[1]-1)
end.date = datSD$collectPeriodTo[length(datSD$collectPeriodTo)]
datSD$collectPeriodFrom.num = julian(datSD$collectPeriodFrom,origin=orig.date) #days since origin
datSD$collectPeriodTo.num = julian(datSD$collectPeriodTo,origin=orig.date) #days since origin
datSD$fieldDate.num = floor((datSD$collectPeriodFrom.num + datSD$collectPeriodTo.num) / 2)
datSD$SD = datSD$SD/100