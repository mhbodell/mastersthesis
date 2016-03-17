
##### political scale plot ####

png('printsGreat2.png', width = 8, height = 6, units = 'in', res = 300)
plot(c(14,37,39,62,66,69,73,82), y=c(1,1,1,1,1,1,1,1),axes=FALSE, 
     ylab="",xlab="", pch=c(21,21,21,21,21,21,21,21), cex=1.5, 
     bg=c("darkred","red","forestgreen","chartreuse3","lightblue3","darkblue","skyblue3", "blue3"))
text(c("","V","S","MP","C","L","KD","SD","M",""),x=c(0,14,37,39,62,66,69,73,82,100), 
     y=c(1.05,1.05,1.05,1.05,1.05,1.05,1.05,1.05), cex=0.75)
text(c("","14","37","39","62","66","69","73","82",""),x=c(0,14,37,39,62,66,69,73,82,100), 
     y=c(.95,.95,.95,.95,.95,.95,.95,.95), cex=0.70)
abline(h=1)
dev.off()


#### voter mobility

netlossgain = cbind(c(NA,-37, -28, -2, 0, -4, -2, 8),
      c(37,NA,-49,-18,-43, -97,-4,63),
      c(28, 49,NA,-8,-26,-37,-2,12),
      c(2,18,8,NA,-18, -69,2, 22),
      c(0,43,26,18,NA,-53,10,28),
      c(4,97,37,69,53,NA,45,156),
      c(2,4,2,-2,-10,-45,NA,22),
      c(-8,-63,-12,-22,-28,-156,-22,NA))
colnames(netlossgain) = c("V","S","MP","C","L","M","KD","SD")
row.names(netlossgain) = c("V","S","MP","C","L","M","KD","SD")
netlossgain
