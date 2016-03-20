#################################################
#################### LOESS ######################
#################################################

data_url = "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls = repmis::source_data(data_url, sep = ",", header = TRUE)

datM = na.omit(polls[,c('M','collectPeriodFrom','collectPeriodTo','n', 'PublDate', 'house')])
datM$house = factor(datM$house)
datM = datM[order(datM$collectPeriodFrom),]
datM$collectPeriodFrom = as.Date(datM$collectPeriodFrom)
datM$collectPeriodTo = as.Date(datM$collectPeriodTo)
datM$PublDate = as.Date(datM$PublDate)
orig.date = as.Date("2006-09-16") #day before election 2011
end.date = as.Date('2014-09-14') #election day 2014

elec = data.frame(rbind(c(0.152,0.133,0.091,0.061,0.398,0.046,0.083,0),
                        c(0.2623,0.0754,0.0659,0.0788,0.3499,0.0524,0.0585,0.0293),
                        c(0.3006,0.0706,0.056,0.0656,0.3066,0.0734,0.056,0.057),
                        c(0.2333,0.0542,0.0457,0.0611,0.3101,0.0689,0.0572,0.1286)))
colnames(elec) = c("M","L","KD","C","S","MP","V","SD")
row.names(elec) = c("2002","2006","2010","2012")
elec$Date = c(as.Date("2002-09-12"), as.Date('2006-09-17'),as.Date('2010-09-23'),as.Date('2014-09-14'))
n=c((0.801*6722*1000),(0.82*6892*1000),(0.846*7124*1000),(0.858*7330*1000))
## http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0105__ME0105C/ME0105T01/table/tableViewLayout1/?rxid=36cc544d-3cba-4ced-88f6-6410c83ac9bf

dfM = data.frame(M=elec$M*100, collectPeriodFrom=elec$Date, collectPeriodTo=elec$Date, n=n, PublDate=elec$Date, house="Election")
datM = rbind(datM, dfM)
datM = datM[order(datM$collectPeriodFrom),]
datM = datM[-which(datM$collectPeriodFrom>end.date),]
datM = datM[-which(datM$collectPeriodFrom<orig.date),]

datM$collectPeriodFrom.num = julian(datM$collectPeriodFrom,origin=orig.date) 
datM$collectPeriodTo.num = julian(datM$collectPeriodTo,origin=orig.date) 
datM$fieldDate.num = floor((datM$collectPeriodFrom.num + datM$collectPeriodTo.num) / 2)
datM$M = datM$M/100