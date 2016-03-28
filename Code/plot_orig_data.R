#################################################
################# DATA PLOTS ####################
#################################################


pointsM = pointsM = data.frame(x=seq(as.Date(datM$collectPeriodFrom[1]-1),by='days',length=datM$fieldDate.num[length(datM$fieldDate.num)])[datM$fieldDate.num[-1]], 
                               y=datM$M[-1]*100, house=datM$house[-1], party=rep("M",datM$fieldDate.num[length(datM$fieldDate.num)])[datM$fieldDate.num[-1]])
pointsL = data.frame(x=seq(as.Date(datL$collectPeriodFrom[1]-1),by='days',length=datL$fieldDate.num[length(datL$fieldDate.num)])[datL$fieldDate.num[-1]], 
                     y=datL$L[-1]*100, house=datL$house[-1], party=rep("L",datL$fieldDate.num[length(datL$fieldDate.num)])[datL$fieldDate.num[-1]])

pointsKD = data.frame(x=seq(as.Date(datKD$collectPeriodFrom[1]-1),by='days',length=datKD$fieldDate.num[length(datKD$fieldDate.num)])[datKD$fieldDate.num[-1]], 
                      y=datKD$KD[-1]*100, house=datKD$house[-1], party=rep("KD",datKD$fieldDate.num[length(datKD$fieldDate.num)])[datKD$fieldDate.num[-1]])

pointsC = data.frame(x=seq(as.Date(datC$collectPeriodFrom[1]-1),by='days',length=datC$fieldDate.num[length(datC$fieldDate.num)])[datC$fieldDate.num[-1]], 
                     y=datC$C[-1]*100, house=datC$house[-1], party=rep("C",datC$fieldDate.num[length(datC$fieldDate.num)])[datC$fieldDate.num[-1]])

pointsS = data.frame(x=seq(as.Date(datS$collectPeriodFrom[1]-1),by='days',length=datS$fieldDate.num[length(datS$fieldDate.num)])[datS$fieldDate.num[-1]], 
                     y=datS$S[-1]*100, house=datS$house[-1], party=rep("S",datS$fieldDate.num[length(datS$fieldDate.num)])[datS$fieldDate.num[-1]])

pointsMP = data.frame(x=seq(as.Date(datMP$collectPeriodFrom[1]-1),by='days',length=datMP$fieldDate.num[length(datMP$fieldDate.num)])[datMP$fieldDate.num[-1]], 
                      y=datMP$MP[-1]*100, house=datMP$house[-1], party=rep("MP",datMP$fieldDate.num[length(datMP$fieldDate.num)])[datMP$fieldDate.num[-1]])

pointsV = data.frame(x=seq(as.Date(datV$collectPeriodFrom[1]-1),by='days',length=datV$fieldDate.num[length(datV$fieldDate.num)])[datV$fieldDate.num[-1]], 
                     y=datV$V[-1]*100, house=datV$house[-1], party=rep("V",datV$fieldDate.num[length(datV$fieldDate.num)])[datV$fieldDate.num[-1]])

pointsSD = data.frame(x=seq(as.Date(datSD$collectPeriodFrom[1]-1),by='days',length=datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]], 
                      y=datSD$SD[-1]*100, house=datSD$house[-1], party=rep("SD",datSD$fieldDate.num[length(datSD$fieldDate.num)])[datSD$fieldDate.num[-1]])


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


library(ggplot2)
gM <- ggplot(pointsM) +
  aes(x = x, y = y, color = house) +
  geom_point(shape=16, size=2, show_guide=FALSE) +    
  labs(x="Date", y=paste("Support for M (%)")) +
  theme_bw() +
  facet_wrap( ~ party, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())



gL <- ggplot(pointsL) +
  aes(x = x, y = y, color = house) +
  geom_point( shape=16, size=2, show_guide=FALSE) +    
  labs(x="Date", y=paste("Support for L (%)")) +
  theme_bw() +
  facet_wrap( ~ party, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gKD <- ggplot(pointsKD) +
  aes(x = x, y = y, color = house) +
  geom_point( shape=16, size=2, show_guide=FALSE) +    
  labs(x="Date", y=paste("Support for KD (%)")) +
  theme_bw() +
  facet_wrap( ~ party, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gC <- ggplot(pointsC) +
  aes(x = x, y = y, color = house) +
  geom_point(shape=16, size=2, show_guide=FALSE) +    
  labs(x="Date", y=paste("Support for C (%)")) +
  theme_bw() +
  facet_wrap( ~ party, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gS <- ggplot(pointsS) +
  aes(x = x, y = y, color = house) +
  geom_point( shape=16, size=2, show_guide=FALSE) +    
  labs(x="Date", y=paste("Support for S (%)")) +
  theme_bw() +
  facet_wrap( ~ party, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gMP <- ggplot(pointsMP) +
  aes(x = x, y = y, color = house) +
  geom_point(shape=16, size=2, show_guide=FALSE) +    
  labs(x="Date", y=paste("Support for MP (%)")) +
  theme_bw() +
  facet_wrap( ~ party, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gV <- ggplot(pointsV) +
  aes(x = x, y = y, color = house) +
  geom_point(shape=16, size=2, show_guide=FALSE) +    
  labs(x="Date", y=paste("Support for V (%)")) +
  theme_bw() +
  facet_wrap( ~ party, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())

gSD <- ggplot(pointsSD) +
  aes(x = x, y = y, color = house) +
  geom_point(shape=16, size=2, show_guide=FALSE) +    
  labs(x="Date", y=paste("Support for SD (%)")) +
  theme_bw() +
  facet_wrap( ~ party, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())


multiplot(gM, gL, gKD, gC, gS, gMP, gV, gSD, cols=2)


ggplot(pointsSD) +
  aes(x = x, y = y, color = house) +
  geom_point(shape=16, size=2) +    
  labs(x="Date", y=paste("Support for SD (%)")) +
  theme_bw() +
  facet_wrap( ~ party, ncol=1, nrow=1)+
  theme(axis.text = element_text(size = 9),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())