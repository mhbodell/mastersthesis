#### inspection of covariance ####

polls$PublDate = as.Date(polls$PublDate)
ind2000 = which((polls$PublDate)<as.Date("2001-01-01"))
ind2001 = which((polls$PublDate)>as.Date("2001-01-01") & (polls$PublDate)<as.Date("2002-01-01") )
ind2002 = which((polls$PublDate)>as.Date("2002-01-01") & (polls$PublDate)<as.Date("2003-01-01") )
ind2003 = which((polls$PublDate)>as.Date("2003-01-01") & (polls$PublDate)<as.Date("2004-01-01") )
ind2004 = which((polls$PublDate)>as.Date("2004-01-01") & (polls$PublDate)<as.Date("2005-01-01") )
ind2005 = which((polls$PublDate)>as.Date("2005-01-01") & (polls$PublDate)<as.Date("2006-01-01") )
ind2006 = which((polls$PublDate)>as.Date("2006-01-01") & (polls$PublDate)<as.Date("2007-01-01") )
ind2007 = which((polls$PublDate)>as.Date("2007-01-01") & (polls$PublDate)<as.Date("2008-01-01") )
ind2008 = which((polls$PublDate)>as.Date("2008-01-01") & (polls$PublDate)<as.Date("2009-01-01") )
ind2009 = which((polls$PublDate)>as.Date("2009-01-01") & (polls$PublDate)<as.Date("2010-01-01") )
ind2010 = which((polls$PublDate)>as.Date("2010-01-01") & (polls$PublDate)<as.Date("2011-01-01") )
ind2011 = which((polls$PublDate)>as.Date("2011-01-01") & (polls$PublDate)<as.Date("2012-01-01") )
ind2012 = which((polls$PublDate)>as.Date("2012-01-01") & (polls$PublDate)<as.Date("2013-01-01") )
ind2013 = which((polls$PublDate)>as.Date("2013-01-01") & (polls$PublDate)<as.Date("2014-01-01") )
ind2014 = which((polls$PublDate)>as.Date("2014-01-01") & (polls$PublDate)<as.Date("2015-01-01") )
ind2015 = which((polls$PublDate)>as.Date("2015-01-01") & (polls$PublDate)<as.Date("2016-01-01") )
ind2016 = which((polls$PublDate)>as.Date("2016-01-01"))

inds = list(ind2000, ind2001, ind2002, ind2003, ind2004, ind2005, ind2006,
            ind2007, ind2008, ind2009, ind2010, ind2011, ind2012, ind2013,
            ind2014, ind2015, ind2016) 	 

POLLS = polls[,3:10]
colnames(POLLS) = c("M","L","C","KD","S","V","MP","SD")

pairs((POLLS[ind2000,-ncol(POLLS)]), font.labels=0.2, upper.panel=NULL, main="2000")
pairs((POLLS[ind2001,-ncol(POLLS)]), font.labels=0.2, upper.panel=NULL, main="2001")
pairs((POLLS[ind2002,-ncol(POLLS)]), font.labels=0.2, upper.panel=NULL, main="2002")
pairs((POLLS[ind2003,-ncol(POLLS)]), font.labels=0.2, upper.panel=NULL, main="2003")
pairs((POLLS[ind2004,-ncol(POLLS)]), font.labels=0.2, upper.panel=NULL, main="2004")
pairs((POLLS[ind2005,-ncol(POLLS)]), font.labels=0.2, upper.panel=NULL, main="2005")
pairs((POLLS[ind2006,]), font.labels=0.2, upper.panel=NULL, main="2006")
pairs((POLLS[ind2007,]), font.labels=0.2, upper.panel=NULL, main="2007")
pairs((POLLS[ind2008,]), font.labels=0.2, upper.panel=NULL, main="2008")
pairs((POLLS[ind2009,]), font.labels=0.2, upper.panel=NULL, main="2009")
pairs((POLLS[ind2010,]), font.labels=0.2, upper.panel=NULL, main="2010")
pairs((POLLS[ind2011,]), font.labels=0.2, upper.panel=NULL, main="2011")
pairs((POLLS[ind2012,]), font.labels=0.2, upper.panel=NULL, main="2012")
pairs((POLLS[ind2013,]), font.labels=0.2, upper.panel=NULL, main="2013")
pairs((POLLS[ind2014,]), font.labels=0.2, upper.panel=NULL, main="2014")
pairs((POLLS[ind2015,]), font.labels=0.2, upper.panel=NULL, main="2015")
pairs((POLLS[ind2016,]), font.labels=0.2, upper.panel=NULL, main="2016")

pairs(POLLS, font.labels=0.1, cex.labels=1.5, upper.panel=NULL, las=1)

plot(POLLS, dataList=NULL, lineColors=NULL, lineStyles=NULL, lineWidths=2, verbose=FALSE, cex.main=1.2, cex.lab=1, cex.axis=1, cex.legend=1, par.zoom=1)
plot(POLLS[,1:2], dataList=NULL, lineColors=NULL, lineStyles=NULL, lineWidths=2, verbose=FALSE, cex.main=1.2, cex.lab=1, cex.axis=1, cex.legend=1, par.zoom=1)


######## DENSITIES ########

Lab.palette <- colorRampPalette(c("white", "lightgreen","yellow", "orange", "red"), space = "Lab")

dev.off()
layout(matrix(c(1,0,0,0,0,0,0,
                2,8,0,0,0,0,0,
                3,9,14,0,0,0,0,
                4,10,15,19,0,0,0,
                5,11,16,20,23,0,0,
                6,12,17,21,24,26,0,
                7,13,18,22,25,27,28), nrow=7, ncol=7, byrow = TRUE))

par(mgp=c(0.2,1,0),mai = c(0.05, 0.15, 0.15, 0.15))  

smoothScatter(x = POLLS[,1], y=POLLS[,2], colramp = Lab.palette, nrpoints=0, xlab="", ylab=colnames(POLLS)[2], xaxt='n',yaxt='n', main=colnames(POLLS)[1], font.lab=2)
smoothScatter(x = POLLS[,1], y=POLLS[,3], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(POLLS)[3], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = POLLS[,1], y=POLLS[,4], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(POLLS)[4], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = POLLS[,1], y=POLLS[,5], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(POLLS)[5], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = POLLS[,1], y=POLLS[,6], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(POLLS)[6], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = POLLS[,1], y=POLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(POLLS)[7], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = POLLS[,1], y=POLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(POLLS)[8], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = POLLS[,2], y=POLLS[,3], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n', main=colnames(POLLS)[2])
smoothScatter(x = POLLS[,2], y=POLLS[,4], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,2], y=POLLS[,5], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,2], y=POLLS[,6], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,2], y=POLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,2], y=POLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,3], y=POLLS[,4], colramp = Lab.palette, nrpoints=0,main=colnames(POLLS)[3],xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,3], y=POLLS[,5], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,3], y=POLLS[,6], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,3], y=POLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,3], y=POLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,4], y=POLLS[,5], colramp = Lab.palette, nrpoints=0,main=colnames(POLLS)[4], xlab="",ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,4], y=POLLS[,6], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,4], y=POLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,4], y=POLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,5], y=POLLS[,6], colramp = Lab.palette, nrpoints=0,main=colnames(POLLS)[5], xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,5], y=POLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,5], y=POLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,6], y=POLLS[,7], colramp = Lab.palette, nrpoints=0,main=colnames(POLLS)[6], xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,6], y=POLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,7], y=POLLS[,8], colramp = Lab.palette, nrpoints=0,main=colnames(POLLS)[7], xlab="", ylab="", xaxt='n',yaxt='n')


############### cov from estimations - basic DLM #####################

estPOLLS = data.frame(M=meanM[datM$fieldDate.num], L=meanL[datL$fieldDate.num], KD=meanKD[datKD$fieldDate.num], C=meanC[datC$fieldDate.num], S=meanS[datS$fieldDate.num], V=meanV[datV$fieldDate.num], MP=meanMP[datMP$fieldDate.num], SD=meanSD[datSD$fieldDate.num])
colnames(estPOLLS) = c("M","L","C","KD","S","V","MP","SD")


dev.off()
layout(matrix(c(1,0,0,0,0,0,0,
                2,8,0,0,0,0,0,
                3,9,14,0,0,0,0,
                4,10,15,19,0,0,0,
                5,11,16,20,23,0,0,
                6,12,17,21,24,26,0,
                7,13,18,22,25,27,28), nrow=7, ncol=7, byrow = TRUE))

par(mgp=c(0.2,1,0),mai = c(0.05, 0.2, 0.2, 0.15))  


smoothScatter(x = estPOLLS[,1], y=estPOLLS[,2], colramp = Lab.palette, nrpoints=0, xlab="", ylab=colnames(estPOLLS)[2], xaxt='n',yaxt='n', main=colnames(estPOLLS)[1], font.lab=2)
smoothScatter(x = estPOLLS[,1], y=estPOLLS[,3], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(estPOLLS)[3], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = estPOLLS[,1], y=estPOLLS[,4], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(estPOLLS)[4], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = estPOLLS[,1], y=estPOLLS[,5], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(estPOLLS)[5], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = estPOLLS[,1], y=estPOLLS[,6], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(estPOLLS)[6], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = estPOLLS[,1], y=estPOLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(estPOLLS)[7], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = estPOLLS[,1], y=estPOLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(estPOLLS)[8], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = estPOLLS[,2], y=estPOLLS[,3], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n', main=colnames(estPOLLS)[2])
smoothScatter(x = estPOLLS[,2], y=estPOLLS[,4], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,2], y=estPOLLS[,5], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,2], y=estPOLLS[,6], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,2], y=estPOLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,2], y=estPOLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,3], y=estPOLLS[,4], colramp = Lab.palette, nrpoints=0,main=colnames(estPOLLS)[3],xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,3], y=estPOLLS[,5], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,3], y=estPOLLS[,6], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,3], y=estPOLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,3], y=estPOLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,4], y=estPOLLS[,5], colramp = Lab.palette, nrpoints=0,main=colnames(estPOLLS)[4], xlab="",ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,4], y=estPOLLS[,6], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,4], y=estPOLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,4], y=estPOLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,5], y=estPOLLS[,6], colramp = Lab.palette, nrpoints=0,main=colnames(estPOLLS)[5], xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,5], y=estPOLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,5], y=estPOLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,6], y=estPOLLS[,7], colramp = Lab.palette, nrpoints=0,main=colnames(estPOLLS)[6], xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,6], y=estPOLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,7], y=estPOLLS[,8], colramp = Lab.palette, nrpoints=0,main=colnames(estPOLLS)[7], xlab="", ylab="", xaxt='n',yaxt='n')


############### cov from estimations - basic DLM smooth data#####################

estPOLLS = data.frame(M=meanM2[datM2$fieldDate.num], L=meanL2[datL2$fieldDate.num], KD=meanKD2[datKD2$fieldDate.num], C=meanC2[datC2$fieldDate.num], 
                      S=meanS2[datS2$fieldDate.num], V=meanV2[datV2$fieldDate.num], MP=meanMP2[datMP2$fieldDate.num], SD=ifelse((datKD2$fieldDate%in%datSD2$fieldDate)==TRUE,meanSD2,NA))
colnames(estPOLLS) = c("M","L","C","KD","S","V","MP","SD")


datKD2$fieldDate[which(!(datKD2$fieldDate%in%datSD2$fieldDate))]
meanM2.2 = ifelse((datKD2$fieldDate%in%datSD2$fieldDate)==TRUE,meanM2,NA)

c(datKD2$fieldDate[which(!(datKD2$fieldDate%in%datSD2$fieldDate))],datSD2$fieldDate)
length(datKD2$fieldDate)

dev.off()
layout(matrix(c(1,0,0,0,0,0,0,
                2,8,0,0,0,0,0,
                3,9,14,0,0,0,0,
                4,10,15,19,0,0,0,
                5,11,16,20,23,0,0,
                6,12,17,21,24,26,0,
                7,13,18,22,25,27,28), nrow=7, ncol=7, byrow = TRUE))

par(mgp=c(0.2,1,0),mai = c(0.05, 0.2, 0.2, 0.15))  


smoothScatter(x = estPOLLS[,1], y=estPOLLS[,2], colramp = Lab.palette, nrpoints=0, xlab="", ylab=colnames(estPOLLS)[2], xaxt='n',yaxt='n', main=colnames(estPOLLS)[1], font.lab=2)
smoothScatter(x = estPOLLS[,1], y=estPOLLS[,3], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(estPOLLS)[3], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = estPOLLS[,1], y=estPOLLS[,4], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(estPOLLS)[4], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = estPOLLS[,1], y=estPOLLS[,5], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(estPOLLS)[5], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = estPOLLS[,1], y=estPOLLS[,6], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(estPOLLS)[6], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = estPOLLS[,1], y=estPOLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(estPOLLS)[7], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = estPOLLS[,1], y=estPOLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab=colnames(estPOLLS)[8], xaxt='n',yaxt='n', font.lab=2)
smoothScatter(x = estPOLLS[,2], y=estPOLLS[,3], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n', main=colnames(estPOLLS)[2])
smoothScatter(x = estPOLLS[,2], y=estPOLLS[,4], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,2], y=estPOLLS[,5], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,2], y=estPOLLS[,6], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,2], y=estPOLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,2], y=estPOLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,3], y=estPOLLS[,4], colramp = Lab.palette, nrpoints=0,main=colnames(estPOLLS)[3],xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,3], y=estPOLLS[,5], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,3], y=estPOLLS[,6], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,3], y=estPOLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,3], y=estPOLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,4], y=estPOLLS[,5], colramp = Lab.palette, nrpoints=0,main=colnames(estPOLLS)[4], xlab="",ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,4], y=estPOLLS[,6], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,4], y=estPOLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,4], y=estPOLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,5], y=estPOLLS[,6], colramp = Lab.palette, nrpoints=0,main=colnames(estPOLLS)[5], xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,5], y=estPOLLS[,7], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,5], y=estPOLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,6], y=estPOLLS[,7], colramp = Lab.palette, nrpoints=0,main=colnames(estPOLLS)[6], xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,6], y=estPOLLS[,8], colramp = Lab.palette, nrpoints=0,xlab="", ylab="", xaxt='n',yaxt='n')
smoothScatter(x = estPOLLS[,7], y=estPOLLS[,8], colramp = Lab.palette, nrpoints=0,main=colnames(estPOLLS)[7], xlab="", ylab="", xaxt='n',yaxt='n')
