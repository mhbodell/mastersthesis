#################################################
#################### NOVUS ######################
#################################################
library(dplyr)

new.datM2 = datM2 %>%
            mutate(month = format(fieldDate, "%m"), year = format(fieldDate, "%Y")) %>%
            group_by(month, year) %>%
            summarise(votes = sum(M*n), sample.size=sum(n), prop=votes/sample.size)

M.novus = new.datM2$prop

library(rjags, lib="C:/Users/mirhu86/Documents/packages")
jags_M.novus ='
model{
#observed model
for(i in 1:npolls){
M[i] ~ dnorm(xM[i],precM[i])


}

}
'

pM.novus = (1 / (new.datM2$prop*(1-new.datM2$prop)/new.datM2$sample.size)) #binomial
#pM = (1 / (datM$M*(1-datM$M)*datM$n)) #multinomial
data_M.novus = list( precM = pM.novus, xM = new.datM2$prop, npolls = nrow(new.datM2))
writeLines(jags_M.novus,con="kalman_M_novus.bug")

system.time(jags_mod_M_novus <- jags.model("kalman_M_novus.bug", data = data_M.novus, n.chain=3))

ninter=10000


orig.date = as.Date("2006-09-16") #day before election 2011
end.date = as.Date('2014-09-14') #election day 2014
 

system.time(outM.novus <- coda.samples(jags_mod_M_novus,variable.names = c( "M"), n.iter = ninter, n.thin = 100))
sumM.novus = summary(outM.novus)
cred_intM.novus = HPDinterval(outM.novus[,which(regexpr("M", row.names(sumM.novus$statistics))==1)], 0.95)


plot(sumM.novus$statistics[,1]*100, type="l", ylim=c(20,50))

dfM.Novus = data.frame(party = new.datM2$prop , time=seq(as.Date('2006-09-16'),by='months',length=length(seq(to = end.date, from = orig.date, by='month'))),
                 party2 = rep("M", length=length(seq(to = end.date, from = orig.date, by='month'))))
