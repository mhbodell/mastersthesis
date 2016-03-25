dateDiff = datM[,3] - datM[,2]
dateDiff2 = dateDiff+1
nDay = datM$n/as.numeric(dateDiff2)
MSmooth = rep(datM$M,dateDiff2)
nSmooth = rep(nDay,dateDiff2)
houseSmooth = rep(datM$house, dateDiff2)
orig.date= as.Date(datM$collectPeriodFrom[1]-1)
end.date= datM$collectPeriodTo[length(datM$collectPeriodTo)]

ee = list()
for(i in 1:nrow(datM)){
  ee[[i]] = seq(datM[i,2], datM[i,3], by="days")
}

dateSmooth = ee[[1]]
for(i in 2:nrow(datM)){
  dateSmooth = c(dateSmooth, ee[[i]])
}

dateSmooth.num = julian(dateSmooth,origin=orig.date)
datM2 = data.frame(M = MSmooth, fieldDate=dateSmooth, fieldDate.num = dateSmooth.num, n = nSmooth, house = houseSmooth)

dateDiffL = datL[,3] - datL[,2]
dateDiffL2 = dateDiffL+1
nDayL = datL$n/as.numeric(dateDiffL2)
LSmooth = rep(datL$L,dateDiffL2)
LnSmooth = rep(nDayL,dateDiffL2)
houseSmoothL = rep(datL$house, dateDiffL2)
orig.date= as.Date(datL$collectPeriodFrom[1]-1)
end.date= datL$collectPeriodTo[length(datL$collectPeriodTo)]

eeL = list()
for(i in 1:nrow(datL)){
  eeL[[i]] = seq(datL[i,2], datL[i,3], by="days")
}

dateSmoothL = eeL[[1]]
for(i in 2:nrow(datL)){
  dateSmoothL = c(dateSmoothL, eeL[[i]])
}

dateSmooth.numL = julian(dateSmoothL,origin=orig.date)
datL2 = data.frame(L = LSmooth, fieldDate=dateSmoothL, fieldDate.num = dateSmooth.numL, n = LnSmooth, house = houseSmoothL)

dateDiffKD = datKD[,3] - datKD[,2]
dateDiffKD2 = dateDiffKD+1
nDayKD = datKD$n/as.numeric(dateDiffKD2)
KDSmooth = rep(datKD$KD,dateDiffKD2)
KDnSmooth = rep(nDayKD,dateDiffKD2)
houseSmoothKD = rep(datKD$house, dateDiffKD2)
orig.date= as.Date(datKD$collectPeriodFrom[1]-1)
end.date= datKD$collectPeriodTo[length(datKD$collectPeriodTo)]

eeKD = list()
for(i in 1:nrow(datKD)){
  eeKD[[i]] = seq(datKD[i,2], datKD[i,3], by="days")
}

dateSmoothKD = eeKD[[1]]
for(i in 2:nrow(datKD)){
  dateSmoothKD = c(dateSmoothKD, eeKD[[i]])
}

dateSmooth.numKD = julian(dateSmoothKD,origin=orig.date)
datKD2 = data.frame(KD = KDSmooth, fieldDate=dateSmoothKD, fieldDate.num = dateSmooth.numKD, n = KDnSmooth, house = houseSmoothKD)

dateDiffC = datC[,3] - datC[,2]
dateDiffC2 = dateDiffC+1
nDayC = datC$n/as.numeric(dateDiffC2)
CSmooth = rep(datC$C,dateDiffC2)
CnSmooth = rep(nDayC,dateDiffC2)
houseSmoothC = rep(datC$house, dateDiffC2)

eeC = list()
for(i in 1:nrow(datC)){
  eeC[[i]] = seq(datC[i,2], datC[i,3], by="days")
}

dateSmoothC = eeC[[1]]
for(i in 2:nrow(datC)){
  dateSmoothC = c(dateSmoothC, eeC[[i]])
}

dateSmooth.numC = julian(dateSmoothC,origin=orig.date)
datC2 = data.frame(C = CSmooth, fieldDate=dateSmoothC, fieldDate.num = dateSmooth.numC, n = CnSmooth, house = houseSmoothC)
orig.date= as.Date(datC$collectPeriodFrom[1]-1)
end.date= datC$collectPeriodTo[length(datC$collectPeriodTo)]


dateDiffS = datS[,3] - datS[,2]
dateDiffS2 = dateDiffS+1
nDayS = datS$n/as.numeric(dateDiffS2)
SSmooth = rep(datS$S,dateDiffS2)
SnSmooth = rep(nDayS,dateDiffS2)
houseSmoothS = rep(datS$house, dateDiffS2)

eeS = list()
for(i in 1:nrow(datS)){
  eeS[[i]] = seq(datS[i,2], datS[i,3], by="days")
}

dateSmoothS = eeS[[1]]
for(i in 2:nrow(datS)){
  dateSmoothS = c(dateSmoothS, eeS[[i]])
}

dateSmooth.numS = julian(dateSmoothS,origin=orig.date)
datS2 = data.frame(S = SSmooth, fieldDate=dateSmoothS, fieldDate.num = dateSmooth.numS, n = SnSmooth, house = houseSmoothS)
orig.date= as.Date(datS$collectPeriodFrom[1]-1)
end.date= datS$collectPeriodTo[length(datS$collectPeriodTo)]


dateDiffMP = datMP[,3] - datMP[,2]
dateDiffMP2 = dateDiff+1
nDayMP = datMP$n/as.numeric(dateDiffMP2)
MPSmooth = rep(datMP$MP,dateDiffMP2)
MPnSmooth = rep(nDayMP,dateDiffMP2)
houseSmoothMP = rep(datMP$house, dateDiffMP2)

eeMP = list()
for(i in 1:nrow(datMP)){
  eeMP[[i]] = seq(datMP[i,2], datMP[i,3], by="days")
}

dateSmoothMP = eeMP[[1]]
for(i in 2:nrow(datMP)){
  dateSmoothMP = c(dateSmoothMP, eeMP[[i]])
}

dateSmooth.numMP = julian(dateSmoothMP,origin=orig.date)
datMP2 = data.frame(MP = MPSmooth, fieldDate=dateSmoothMP, fieldDate.num = dateSmooth.numMP, n = MPnSmooth, house = houseSmoothMP)
orig.date= as.Date(datMP$collectPeriodFrom[1]-1)
end.date= datMP$collectPeriodTo[length(datMP$collectPeriodTo)]


dateDiffV = datV[,3] - datV[,2]
dateDiffV2 = dateDiffV+1
nDayV = datV$n/as.numeric(dateDiffV2)
VSmooth = rep(datV$V,dateDiffV2)
VnSmooth = rep(nDayV,dateDiffV2)
houseSmoothV = rep(datV$house, dateDiffV2)

eeV = list()
for(i in 1:nrow(datV)){
  eeV[[i]] = seq(datV[i,2], datV[i,3], by="days")
}

dateSmoothV = eeV[[1]]
for(i in 2:nrow(datV)){
  dateSmoothV = c(dateSmoothV, eeV[[i]])
}

dateSmooth.numV = julian(dateSmoothV,origin=orig.date)
datV2 = data.frame(V = VSmooth, fieldDate=dateSmoothV, fieldDate.num = dateSmooth.numV, n = VnSmooth, house = houseSmoothV)
orig.date= as.Date(datV$collectPeriodFrom[1]-1)
end.date= datV$collectPeriodTo[length(datV$collectPeriodTo)]


dateDiffSD = datSD[,3] - datSD[,2]
dateDiffSD2 = dateDiffSD+1
nDaySD = datSD$n/as.numeric(dateDiffSD2)
SDSmooth = rep(datSD$SD,dateDiffSD2)
SDnSmooth = rep(nDaySD,dateDiffSD2)
houseSmoothSD = rep(datSD$house, dateDiffSD2)

eeSD = list()
for(i in 1:nrow(datSD)){
  eeSD[[i]] = seq(datSD[i,2], datSD[i,3], by="days")
}

dateSmoothSD = eeSD[[1]]
for(i in 2:nrow(datSD)){
  dateSmoothSD = c(dateSmoothSD, eeSD[[i]])
}

dateSmooth.numSD = julian(dateSmoothSD,origin=orig.date)
datSD2 = data.frame(SD = SDSmooth, fieldDate=dateSmoothSD, fieldDate.num = dateSmooth.numSD, n = SDnSmooth, house = houseSmoothSD)
orig.date = as.Date(datSD$collectPeriodFrom[1]-1)
end.date = datSD$collectPeriodTo[length(datSD$collectPeriodTo)]