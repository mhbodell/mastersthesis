parties = c("M", "S", "C", "KD", "FP", "V", "MP", "SD")
predData = polls[, c(parties, "n", "house")]
predData[, parties] = predData[, parties]/100
predData$time = as.Date(polls$collectPeriodTo)
predData$publDate = as.Date(polls$PublDate)
predData$periodFrom = as.Date(polls$collectPeriodFrom)
predData$periodTo = as.Date(polls$collectPeriodTo)
predData$house = factor(predData$house)
predData = predData[!(is.na(predData$periodFrom) | is.na(predData$periodTo)), ]
predData = predData[!(is.na(predData$house) | is.na(predData$n)), ]
predData = predData[!(is.na(predData$publDate)), ]
predData = predData[-which(rowSums(predData[, 1:8], na.rm = TRUE) == 1), ]
orig.date = as.Date('2010-09-23') ##valdagen 2006
end.date = as.Date("2014-09-14") ##valdagen 2014
predData =  predData[which(predData$periodFrom>orig.date & predData$periodTo<end.date) ,]
df = predData
df$Week = as.numeric(predData$periodFrom-predData$periodFrom[length(predData$periodFrom)]) %/% 7
df = df[order(df$Week), ]
df = df[-which(df$Week<1),]
NUMPOLLS = nrow(df)
PERIOD = max(df$Week)
HOUSECOUNT = length(levels(df$house)) # a factor
HOUSENAMES = levels(df$house)
PARTYNAMES = parties
PARTIES = length(PARTYNAMES)
primaryVotes = df[PARTYNAMES] * df$n
primaryVotes = sapply(primaryVotes, function(x) round(x,0))
day0 = min(as.Date(df$time)) - 1

## Assume Coalition gets preferences as follows:
## - 16.97% of the Green vote [was 16.97 in 2013 and 21.16 in 2010]
## - 53.3% of the Other vote [was 53.3 in 2013 and 58.26 in 2010]
## See: Antony Green - http://blogs.abc.net.au/antonygreen/2013/11/
##   preference-flows-at-the-2013-federal-election.html
#preference_flows <- c(1.0, 0.0, 0.1697, 0.533)

data = list(PERIOD = PERIOD,
            HOUSECOUNT = HOUSECOUNT,
            NUMPOLLS = NUMPOLLS,
            PARTIES = PARTIES,
            primaryVotes = primaryVotes,
            pollWeek = df$Week,
            house = as.integer(df$house),
            # manage rounding issues with df$Sample ...
            n = rowSums(primaryVotes),
            preference_flows = preference_flows)


model <- '
model {

#### -- observational model
for(poll in 1:NUMPOLLS) { # for each poll result - rows
adjusted_poll[poll, 1:PARTIES] <- walk[pollWeek[poll], 1:PARTIES] +
houseEffect[house[poll], 1:PARTIES]
primaryVotes[poll, 1:PARTIES] ~ dmulti(adjusted_poll[poll, 1:PARTIES], n[poll])
}

#### -- temporal model (a weekly walk where this week is much like last week)
tightness <- 10000 # KLUDGE: tightness of fit parameter selected by trial and error
for(week in 2:PERIOD) {
# Note: use math not a distribution to generate the multinomial ...
multinomial[week, 1:PARTIES] <- walk[week-1,  1:PARTIES] * tightness
walk[week, 1:PARTIES] ~ ddirch(multinomial[week, 1:PARTIES])
}

## -- weakly informative priors for first week in the temporal model
for (party in 1:2) { # for each major party
alpha[party] ~ dunif(200, 400) # majors between 25% and 60%
}
for (party in 3:PARTIES) { # for each minor party
alpha[party] ~ dunif(10, 250) # minors between 1% and 25%
}
walk[1, 1:PARTIES] ~ ddirch(alpha[])


#### -- sum-to-zero constraints on house effects
for (party in 2:PARTIES) { # for each party ...
# house effects across houses sum to zero
# NOTE: ALL MUST SUM TO ZERO
houseEffect[1, party] <- -sum( houseEffect[2:HOUSECOUNT, party] )
}
for(house in 1:HOUSECOUNT) { # for each house ...
# house effects across the parties sum to zero
houseEffect[house, 1] <- -sum( houseEffect[house, 2:PARTIES] )
}
# but note, we do not apply a double constraint to houseEffect[1, 1]
monitorHouseEffectOneSumParties <- sum(houseEffect[1, 1:PARTIES])
monitorHouseEffectOneSumHouses <- sum(houseEffect[1:HOUSECOUNT, 1])

## -- vague normal priors for house effects - centred on zero
for (party in 2:PARTIES) { # for each party (cols)
for(house in 2:HOUSECOUNT) { #  (rows)
houseEffect[house, party] ~ dnorm(0, pow(0.1, -2))
}
}
}
'

writeLines(model,con="DM-model")
system.time(jags_mod_DM <- jags.model("DM-model", data = data))
system.time(outDM <- coda.samples(jags_mod_DM,variable.names = c("houseEffect", "primaryVotes", "walk" ), n.iter = 3000, n.thin = 100))
sumDM =  summary(outDM)
walks = sumDM$statistics[which(regexpr("walk",row.names(sumDM$statistics))==1),]
library(coda)
HPD_DM = HPDinterval(as.mcmc(sumDM$statistics[which(regexpr("walk",row.names(sumDM$statistics))==1),1]),0.95)


df2=data.frame(M=walks[1:205,1]*100, week=c(1:PERIOD), low=HPD_DM[[1]][1], high=HPD_DM[[1]][2])

library(ggplot2)
ggplot(df2) +
  aes(x=week, y=M) +
  geom_line(col="blue3", alpha=1) +
  geom_point(data = data.frame(x=df$Week, y=df$M*100), aes(x=x, y=y), col="blue3") +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.3, fill="blue3")




