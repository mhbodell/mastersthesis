#################################################
#################### NOVUS ######################
#################################################
library(dplyr)

new.datM2 = datM2 %>%
            mutate(month = format(fieldDate, "%m"), year = format(fieldDate, "%Y")) %>%
            group_by(month, year) %>%
            summarise(votes = sum(M*n), sample.size=sum(n), prop=votes/sample.size)

M.novus = new.datM2$prop


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

system.time(outM.novus <- coda.samples(jags_mod_M_novus,variable.names = c( "M"), n.iter = ninter, n.thin = 100))
sumM.novus = summary(outM.novus)
cred_intM.novus = HPDinterval(outM.novus[,which(regexpr("M", row.names(sumM.novus$statistics))==1)], 0.95)


plot(sumM.novus$statistics[,1]*100, type="l", ylim=c(20,50))
