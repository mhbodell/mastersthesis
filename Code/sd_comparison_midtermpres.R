library(ggplot2)

#### novus #####
rrr = NULL
for(i in 1: nrow(new.datSD3)){
  rr = paste(new.datSD3[i,'year'], new.datSD3[i,'month'], sep="-", collapse="")
  rrr[i] = paste(rr, "-15", sep="", colapse="")
}
dfSD.Novus = data.frame(party = sumSD.novus$statistics[-1,1]*100, time=as.Date(rrr[-1]),
                        low=cred_intSD.novus[[1]][-1,1]*100, high=cred_intSD.novus[[1]][-1,2]*100)
pointsSD =  data.frame(x=seq(as.Date(datSD$collectPeriodFrom[1]-1),by='days',length=datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]],
                       y=datSD$SD[-1]*100, house=datSD$house[-1], party=rep("SD",datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]])
gSD_novus <- ggplot(dfSD.Novus) +
  aes(x = time, y = party) +
  geom_line(colour = "skyblue3") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="skyblue3") +
  geom_point(data=pointsSD, aes(x=x, y=y), alpha = 1, color="skyblue3", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for SD (%)")) +
  ggtitle("Naive benchmark model") +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())




### all data ####
sumSD = all_sumSD;outSD = all_outSD
meanSD = sumSD$statistics[which(regexpr("xSD", row.names(sumSD$statistics))==1),1]

dfSD = data.frame(party = meanSD[-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1]))] , 
                  low=all_cred_intSD[[1]][-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1])),1]*100, 
                  high=all_cred_intSD[[1]][-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1])),2]*100,
                  time=seq(as.Date(datSD$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datSD$collectPeriodFrom[2]-1))))),
                  party2 = rep("SD", length(c(rep(NA,end.date - as.Date(datSD$collectPeriodFrom[2]-1))))))
pointsSD = data.frame(x=seq(as.Date(datSD$collectPeriodFrom[1]-1),by='days',length=datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]], 
                      y=datSD$SD[-1]*100, house=datSD$house[-1], party=rep("SD",datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]])

gSD <- ggplot(dfSD) +
  aes(x = time, y = party*100) +
  geom_line(col="skyblue3", alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="skyblue3") + 
  geom_point(data=pointsSD, aes(x=x, y=y), alpha = 1, color="skyblue3", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for SD (%)")) +
  theme_bw() +
  ggtitle("Basic dynamic linear model") +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

##### smooth data #####
meanSD2 = sumSD2$statistics[which(regexpr("xSD", row.names(sumSD2$statistics))==1),1]

dfSD2 = data.frame(party = meanSD2[-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1]))] , 
                   low=cred_intSD2[[1]][-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1])),1]*100, 
                   high=cred_intSD2[[1]][-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1])),2]*100,
                   time=seq(as.Date(datSD$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datSD$collectPeriodFrom[2]-1))))),
                   party2 = rep("SD", length(c(rep(NA,end.date - as.Date(datSD$collectPeriodFrom[2]-1))))))
pointsSD = data.frame(x=seq(as.Date(datSD$collectPeriodFrom[1]-1),by='days',length=datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]], 
                      y=datSD$SD[-1]*100, house=datSD$house[-1], party=rep("SD",datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]])

gSD2_smooth <- ggplot(dfSD2) +
  aes(x = time, y = party*100) +
  geom_line(col="skyblue3", alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="skyblue3") + 
  geom_point(data=pointsSD, aes(x=x, y=y), alpha = 1, color="skyblue3", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for SD (%)")) +
  theme_bw() +
  ggtitle("Basic dynamic linear model with smooth data") +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())


#### additative house effect - all data###
meanSD_house = sumSD_house$statistics[which(regexpr("xSD", row.names(sumSD_house$statistics))==1),1]
dfSD_house = data.frame(party = meanSD_house[-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1]))] ,
                  low=cred_intSD_house[[1]][-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1])),1]*100,
                  high=cred_intSD_house[[1]][-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1])),2]*100,
                  time=seq(as.Date(datSD$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datSD$collectPeriodFrom[2]-1))))),
                  party2 = rep("SD", length(c(rep(NA,end.date - as.Date(datSD$collectPeriodFrom[2]-1))))))
pointsSD = data.frame(x=seq(as.Date(datSD$collectPeriodFrom[1]-1),by='days',length=datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]],
                      y=datSD$SD[-1]*100, house=datSD$house[-1], party=rep("SD",datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]])
gSD_house <- ggplot(dfSD_house) +
  aes(x = time, y = party*100) +
  geom_line(col="skyblue3", alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="skyblue3") +
  geom_point(data=pointsSD, aes(x=x, y=y), alpha = 1, color="skyblue3", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for SD (%)")) +
  theme_bw() +
  ggtitle("Dynamic linear model with additive house effects on mean") +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())



###### multiplicative house effect ######

meanSD_house2 = sumSD_house2$statistics[which(regexpr("xSD", row.names(sumSD_house2$statistics))==1),1]

dfSD_house2 = data.frame(party = meanSD_house2[-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1]))] ,
                         low=house2_credSD[[1]][-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1])),1]*100,
                         high=house2_credSD[[1]][-c(1:as.numeric(datSD$collectPeriodFrom[2]-datSD$collectPeriodFrom[1])),2]*100,
                         time=seq(as.Date(datSD$collectPeriodFrom[2]-1),by='days',length=length(c(rep(NA,end.date - as.Date(datSD$collectPeriodFrom[2]-1))))),
                         party2 = rep("M", length(c(rep(NA,end.date - as.Date(datSD$collectPeriodFrom[2]-1))))))
pointsSD =  data.frame(x=seq(as.Date(datSD$collectPeriodFrom[1]-1),by='days',length=datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]],
                       y=datSD$SD[-1]*100, house2=datSD$house[-1], party=rep("SD",datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]])
library(ggplot2)
gSD_house2 <- ggplot(dfSD_house2) +
  aes(x = time, y = party*100) +
  geom_line(col="skyblue3", alpha=1)  +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="skyblue3") +
  geom_point(data=pointsSD, aes(x=x, y=y), alpha = 1, color="skyblue3", shape=16, size=1) +    
  labs(x="Date", y=paste("Support for SD (%)")) +
  ggtitle("Dynamic linear model with multiplicative house effects on variance") +
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())




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


multiplot(gSD_novus, gSD,gSD2_smooth, gSD_house, gSD_house2,  cols=2)
