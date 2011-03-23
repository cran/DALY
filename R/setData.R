setData <-
function(n){

ar <- c(1,5,2,6,3,7,4,8)

countDist <- c(TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
propDist <- c(TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE)
timeDist <- c(TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE)

selectDist <- cbind(countDist, propDist, timeDist, timeDist, propDist, propDist, countDist, timeDist)

getPop <- getData(pop,"pop")

assign(paste("data",n,sep=""), tktoplevel(padx=15,pady=15), envir=.GlobalEnv)
drawWindow(get(paste("data",n,sep="")), get(paste("outcome",n,"Name",sep="")), n) 
for (i in 1:8) assign(paste("cbDistr",txtLbl[i],n,sep=""),
defineCombo(get(paste("Frame",ar[i],sep="")), get(paste("dist",txtLbl[i],n,sep="")), distributions[selectDist[,i]]),
envir = .GlobalEnv)
for (i in 1:8) assign(paste("cbStrat",txtLbl[i],n,sep=""),
defineCombo(get(paste("Frame",ar[i],sep="")), get(paste("strat",txtLbl[i],n,sep="")), stratifications),
envir = .GlobalEnv)

for (i in 1:8) assign(paste("table",txtLbl[i],n,sep=""), defineTable(get(paste("Frame",ar[i],sep="")), get(paste(txtlbl[i],n,sep=""))), envir = .GlobalEnv)
bindCombo(get(paste("cbDistrInc",n,sep="")), get(paste("cbDistrOns",n,sep="")), get(paste("cbDistrDWt",n,sep="")), get(paste("cbDistrMrt",n,sep="")),
get(paste("cbDistrTrt",n,sep="")), get(paste("cbDistrDur",n,sep="")), get(paste("cbDistrDWn",n,sep="")), get(paste("cbDistrLxp",n,sep="")),
get(paste("distInc",n,sep="")), get(paste("distOns",n,sep="")), get(paste("distDWt",n,sep="")), get(paste("distMrt",n,sep="")),
get(paste("distTrt",n,sep="")), get(paste("distDur",n,sep="")), get(paste("distDWn",n,sep="")),get(paste("distLxp",n,sep="")), 
get(paste("cbStratInc",n,sep="")), get(paste("cbStratOns",n,sep="")), get(paste("cbStratDWt",n,sep="")), get(paste("cbStratMrt",n,sep="")),
get(paste("cbStratTrt",n,sep="")), get(paste("cbStratDur",n,sep="")), get(paste("cbStratDWn",n,sep="")), get(paste("cbStratLxp",n,sep="")),
get(paste("stratInc",n,sep="")), get(paste("stratOns",n,sep="")), get(paste("stratDWt",n,sep="")), get(paste("stratMrt",n,sep="")),
get(paste("stratTrt",n,sep="")), get(paste("stratDur",n,sep="")), get(paste("stratDWn",n,sep="")), get(paste("stratLxp",n,sep="")),
get(paste("tableInc",n,sep="")), get(paste("tableOns",n,sep="")),get(paste("tableDWt",n,sep="")), get(paste("tableMrt",n,sep="")),
get(paste("tableTrt",n,sep="")), get(paste("tableDur",n,sep="")), get(paste("tableDWn",n,sep="")), get(paste("tableLxp",n,sep="")),
get(paste("inc",n,sep="")), get(paste("ons",n,sep="")), get(paste("DWt",n,sep="")), get(paste("mrt",n,sep="")),
get(paste("trt",n,sep="")), get(paste("dur",n,sep="")), get(paste("DWn",n,sep="")), get(paste("lxp",n,sep="")) )
drawTables(get(paste("tableInc",n,sep="")), get(paste("tableOns",n,sep="")), get(paste("tableDWt",n,sep="")),
get(paste("tableMrt",n,sep="")), get(paste("tableTrt",n,sep="")), get(paste("tableDur",n,sep="")),
get(paste("tableDWn",n,sep="")), get(paste("tableLxp",n,sep="")), get(paste("cbDistrInc",n,sep="")),
get(paste("cbDistrOns",n,sep="")), get(paste("cbDistrDWt",n,sep="")), get(paste("cbDistrMrt",n,sep="")),
get(paste("cbDistrTrt",n,sep="")), get(paste("cbDistrDur",n,sep="")), get(paste("cbDistrDWn",n,sep="")),
get(paste("cbDistrLxp",n,sep="")), get(paste("cbStratInc",n,sep="")), get(paste("cbStratOns",n,sep="")),
get(paste("cbStratDWt",n,sep="")), get(paste("cbStratMrt",n,sep="")), get(paste("cbStratTrt",n,sep="")),
get(paste("cbStratDur",n,sep="")), get(paste("cbStratDWn",n,sep="")), get(paste("cbStratLxp",n,sep="")))

}

