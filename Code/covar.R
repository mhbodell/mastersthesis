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

polls.elec = POLLS[c(ind2002, ind2006, ind2010, ind2014),]


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
