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

smoothScatter(POLLS[,1],POLLS[,2], colorramp=colorRampPalette(c("light green", "yellow", "orange", "red"))(100))
Lab.palette <- colorRampPalette(c("white", "lightgreen","yellow", "orange", "red"), space = "Lab")
smoothScatter(x = POLLS[,1], y=POLLS[,2], colramp = Lab.palette, nrpoints=0)
smoothScatter(POLLS[,1], colramp = Lab.palette, nrpoints=0)


smoothScatter(x = POLLS[,2], y=POLLS[,1], colramp = Lab.palette, nrpoints=0, ylab=colnames(POLLS)[1], xlab=colnames(POLLS)[2], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,3], y=POLLS[,1], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[1], xlab=colnames(POLLS)[3], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,4], y=POLLS[,1], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[1], xlab=colnames(POLLS)[4], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,5], y=POLLS[,1], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[1], xlab=colnames(POLLS)[5], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,6], y=POLLS[,1], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[1], xlab=colnames(POLLS)[6], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,7], y=POLLS[,1], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[1], xlab=colnames(POLLS)[7], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,8], y=POLLS[,1], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[1], xlab=colnames(POLLS)[8], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,3], y=POLLS[,2], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[2], xlab=colnames(POLLS)[3], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,4], y=POLLS[,2], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[2], xlab=colnames(POLLS)[4], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,5], y=POLLS[,2], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[2], xlab=colnames(POLLS)[5], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,6], y=POLLS[,2], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[2], xlab=colnames(POLLS)[6], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,7], y=POLLS[,2], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[2], xlab=colnames(POLLS)[7], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,8], y=POLLS[,2], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[2], xlab=colnames(POLLS)[8], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,4], y=POLLS[,3], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[3], xlab=colnames(POLLS)[4], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,5], y=POLLS[,3], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[3], xlab=colnames(POLLS)[5], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,6], y=POLLS[,3], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[3], xlab=colnames(POLLS)[6], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,7], y=POLLS[,3], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[3], xlab=colnames(POLLS)[7], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,8], y=POLLS[,3], colramp = Lab.palette, nrpoints=0,ylab=colnames(POLLS)[3], xlab=colnames(POLLS)[8], xaxt='n',yaxt='n')


polls.elec = POLLS[c(ind2002, ind2006, ind2010, ind2014),]
layout(matrix(c(1,0,0,0,0,0,0,
                2,8,0,0,0,0,0,
                3,9,14,0,0,0,0,
                4,10,15,0,0,0,0,
                5,11,16,19,0,0,0,
                6,12,17,20,22,0,0,
                7,13,18,21,23,27,28), nrow=7, ncol=7, byrow = TRUE))


pairs((POLLS[ind2002,-ncol(POLLS)]), font.labels=0.2, upper.panel=NULL, main="2002")
pairs((POLLS[ind2006,]), font.labels=0.2, upper.panel=NULL, main="2006")
pairs((POLLS[ind2010,]), font.labels=0.2, upper.panel=NULL, main="2010")
pairs((POLLS[ind2014,]), font.labels=0.2, upper.panel=NULL, main="2014")


pairs((POLLS[ind2006,]), font.labels=0.2, upper.panel=NULL, main="2006")
pairs((POLLS[ind2014,]), font.labels=0.2, upper.panel=NULL, main="2014")


#### visual ####

# S vs MP #
par(mfrow=c(3,3))
y=2000
for (i in inds){
  plot(polls$S[i],polls$MP[i], las=1, main=y, ylab="MP", xlab="S", pch=16)
  y=y+1
}

# S vs V #
par(mfrow=c(3,3))
y=2000
for (i in inds){
  plot(polls$S[i] ,polls$V[i], las=1, main=y, ylab="V", xlab="S", pch=16)
  y=y+1
}

# S vs M #

# S vs FP #

# S vs C #

# S vs M #

# S vs M #


library(ggplot2)
for (i in inds){
  ggplot(polls ,aes(x = S, y = MP)) +
    geom_point() +
    ggtitle("S vs MP") +
    labs(x="S", y="MP") +
    theme_bw() +
    theme(axis.text = element_text(size = 9),
          legend.key = element_rect(fill = "white"),
          legend.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "lightgrey"),
          panel.grid.minor = element_blank())
}

### over all correlation ##
cor(polls[,3:10], use="pairwise.complete.obs")


### correlation breaked down on year ###
cor.list = list()
j=1
for(i in inds){
  cor.list[[j]] = cor(polls[i,3:10], use="pairwise.complete.obs")
  j = j+1
}

####################
par(mgp=c(0.2,1,0),mai = c(0.15, 0.15, 0.15, 0.15))  
smoothScatter(x = POLLS[,1], y=POLLS[,2], colramp = Lab.palette, nrpoints=0, xlab=colnames(POLLS)[1], ylab=colnames(POLLS)[2], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,1], y=POLLS[,3], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[1], ylab=colnames(POLLS)[3], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,1], y=POLLS[,4], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[1], ylab=colnames(POLLS)[4], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,1], y=POLLS[,5], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[1], ylab=colnames(POLLS)[5], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,1], y=POLLS[,6], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[1], ylab=colnames(POLLS)[6], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,1], y=POLLS[,7], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[1], ylab=colnames(POLLS)[7], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,1], y=POLLS[,8], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[1], ylab=colnames(POLLS)[8], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,2], y=POLLS[,3], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[2], ylab=colnames(POLLS)[3], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,2], y=POLLS[,4], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[2], ylab=colnames(POLLS)[4], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,2], y=POLLS[,5], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[2], ylab=colnames(POLLS)[5], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,2], y=POLLS[,6], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[2], ylab=colnames(POLLS)[6], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,2], y=POLLS[,7], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[2], ylab=colnames(POLLS)[7], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,2], y=POLLS[,8], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[2], ylab=colnames(POLLS)[8], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,3], y=POLLS[,4], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[3], ylab=colnames(POLLS)[4], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,3], y=POLLS[,5], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[3], ylab=colnames(POLLS)[5], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,3], y=POLLS[,6], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[3], ylab=colnames(POLLS)[6], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,3], y=POLLS[,7], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[3], ylab=colnames(POLLS)[7], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,3], y=POLLS[,8], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[3], ylab=colnames(POLLS)[8], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,4], y=POLLS[,5], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[4], ylab=colnames(POLLS)[5], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,4], y=POLLS[,6], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[4], ylab=colnames(POLLS)[6], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,4], y=POLLS[,7], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[4], ylab=colnames(POLLS)[7], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,4], y=POLLS[,8], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[4], ylab=colnames(POLLS)[8], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,5], y=POLLS[,6], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[5], ylab=colnames(POLLS)[6], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,5], y=POLLS[,7], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[5], ylab=colnames(POLLS)[7], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,5], y=POLLS[,8], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[5], ylab=colnames(POLLS)[8], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,6], y=POLLS[,7], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[6], ylab=colnames(POLLS)[7], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,6], y=POLLS[,8], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[6], ylab=colnames(POLLS)[8], xaxt='n',yaxt='n')
smoothScatter(x = POLLS[,7], y=POLLS[,8], colramp = Lab.palette, nrpoints=0,xlab=colnames(POLLS)[7], ylab=colnames(POLLS)[8], xaxt='n',yaxt='n')


layout(matrix(c(1,2,3,4,5,6,7,
                0,8,9,10,11,12,13,
                0,0,14,15,16,17,18,
                0,0,0,19,20,21,22), nrow=4, ncol=7, byrow = TRUE))


par(mgp=c(3,1,0))
layout(matrix(c(1,2,3,4,5,6,7,
                0,8,9,10,11,12,13,
                0,0,14,15,16,17,18,
                0,0,0,19,20,21,22,
                0,0,0,0,23,24,25,
                0,0,0,0,0,26,27,
                0,0,0,0,0,0,28), nrow=7, ncol=7, byrow = TRUE))

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



