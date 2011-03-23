getDALY <-
function(button.call=FALSE){
sendPop <- c(getData(pop,"pop"))

if (is.na(sum(sendPop))){
tkmessageBox(title="Error",message="Population table contains\nnon-numeric value(s)",icon="error",type="ok")
stop("Population table contains non-numeric value(s).", call. = FALSE)
} else if (sum(sendPop)==0){
tkmessageBox(title="Error",message="Population table is empty",icon="error",type="ok")
stop("Population table is empty.", call. = FALSE)
}

if (length(tclvalue(drTcl))==0){
tkmessageBox(title="Error",message="Please enter a Discount Rate",icon="error",type="ok")
stop("Please enter a Discount Rate.", call. = FALSE)
} else {
sendDR <- tclvalue(drTcl)
}

sendDR <- gsub("%","",sendDR)
sendDR <- gsub(",",".",sendDR)
DRchars <- array(dim=nchar(sendDR))
for (i in 1:dim(DRchars)) DRchars[i] <- any(c(c(0:9),".") == substr(sendDR,i,i))

if (any(DRchars == FALSE)){
tkmessageBox(title="Error",message="Discount Rate contains non-numeric value",icon="error",type="ok")
stop("Discount Rate contains non-numeric value.", call. = FALSE)
}

listDst <- NULL
for (i in 1:8) for (j in 1:8) listDst[j+(i-1)*8] <- tclvalue(get(paste("dist",txtLbl[j],i,sep="")))
sendDst <- array(dim=length(listDst))
for (i in 1:length(listDst)) sendDst[i] <- which(distributions==listDst[i])

listStr <- NULL
for (i in 1:8) for (j in 1:8) listStr[j+(i-1)*8] <- tclvalue(get(paste("strat",txtLbl[j],i,sep="")))
sendStr <- array(dim=length(listStr))
for (i in 1:length(listStr)) sendStr[i] <- which(stratifications==listStr[i])

sendStrAge <- array(dim=length(sendStr))
for (i in 1:length(sendStr)) sendStrAge[i] <- c(1,2,0,0)[sendStr[i]]

sendStrSex <- array(dim=length(sendStr))
for (i in 1:length(sendStr)) sendStrSex[i] <- ifelse(sendStr[i]==1 || sendStr[i]==3, 5, 0)

sendInc <- c(getData(inc1,"data"),getData(inc2,"data"),getData(inc3,"data"),getData(inc4,"data"),
getData(inc5,"data"),getData(inc6,"data"),getData(inc7,"data"),getData(inc8,"data"))
sendTrt <- c(getData(trt1,"data"),getData(trt2,"data"),getData(trt3,"data"),getData(trt4,"data"),
getData(trt5,"data"),getData(trt6,"data"),getData(trt7,"data"),getData(trt8,"data"))
sendOns <- c(getData(ons1,"data"),getData(ons2,"data"),getData(ons3,"data"),getData(ons4,"data"),
getData(ons5,"data"),getData(ons6,"data"),getData(ons7,"data"),getData(ons8,"data"))
sendDur <- c(getData(dur1,"data"),getData(dur2,"data"),getData(dur3,"data"),getData(dur4,"data"),
getData(dur5,"data"),getData(dur6,"data"),getData(dur7,"data"),getData(dur8,"data"))
sendDWt <- c(getData(DWt1,"data"),getData(DWt2,"data"),getData(DWt3,"data"),getData(DWt4,"data"),
getData(DWt5,"data"),getData(DWt6,"data"),getData(DWt7,"data"),getData(DWt8,"data"))
sendDWn <- c(getData(DWn1,"data"),getData(DWn2,"data"),getData(DWn3,"data"),getData(DWn4,"data"),
getData(DWn5,"data"),getData(DWn6,"data"),getData(DWn7,"data"),getData(DWn8,"data"))
sendMrt <- c(getData(mrt1,"data"),getData(mrt2,"data"),getData(mrt3,"data"),getData(mrt4,"data"),
getData(mrt5,"data"),getData(mrt6,"data"),getData(mrt7,"data"),getData(mrt8,"data"))
sendDth <- c(getData(lxp1,"data"),getData(lxp2,"data"),getData(lxp3,"data"),getData(lxp4,"data"),
getData(lxp5,"data"),getData(lxp6,"data"),getData(lxp7,"data"),getData(lxp8,"data"))
sendLxp <- getLifeExp()
sendAW  <- ifelse(tclvalue(awTcl)=="Yes",1,0)
sendIT  <- as.integer(tclvalue(it))
outcomes <- array(0,dim=8)
for (i in 0:7) outcomes[i+1] <- as.integer((sum(sendInc[((30*i)+1):(30*(i+1))])!=0 || sum(sendMrt[((30*i)+1):(30*(i+1))])!=0)) * (i+1)
outcomes <- outcomes[outcomes!=0]

# delete any seed & generate new seed
if (exists(".Random.seed", envir=.GlobalEnv)) remove(.Random.seed, envir=.GlobalEnv)
runif(1)

output <- .C("getMC", returnMrt=as.integer(vector("numeric",sendIT*length(outcomes))),
YLD=as.double(vector("numeric",sendIT*length(outcomes))),
YLL=as.double(vector("numeric",sendIT*length(outcomes))), IT=as.integer(sendIT),
AW=as.integer(sendAW), DR=as.double(sendDR), OC=as.integer(outcomes), nOC=as.integer(length(outcomes)),
getDist=as.integer(sendDst), getStrat=as.integer(sendStr),
getStrAge=as.integer(sendStrAge), getStrSex=as.integer(sendStrSex), getPop=as.double(sendPop),
getDur=as.double(sendDur), getOns=as.double(sendOns), getInc=as.double(sendInc),
getTrt=as.double(sendTrt), getMrt=as.double(sendMrt), getDWt=as.double(sendDWt),
getDWn=as.double(sendDWn), getDth=as.double(sendDth), getLxp=as.double(sendLxp))

YLD  <- matrix(output$YLD,ncol=length(outcomes))
YLL  <- matrix(output$YLL,ncol=length(outcomes))
DALY <- YLD+YLL
Deaths <- matrix(output$returnMrt,ncol=length(outcomes))

if (length(outcomes) > 1){

for (i in 1:8){
if (any(outcomes==i)){
assign(paste("DALY",i,sep=""), YLL[,i]+YLD[,i])
assign(paste("YLD",i,sep=""), YLD[,i])
assign(paste("YLL",i,sep=""), YLL[,i])
assign(paste("Deaths",i,sep=""), Deaths[,i])
} else {
assign(paste("DALY",i,sep=""), NULL)
assign(paste("YLD",i,sep=""), NULL)
assign(paste("YLL",i,sep=""), NULL)
assign(paste("Deaths",i,sep=""), NULL)
}
}

for (i in 1:8) assign(paste("o",i,sep=""), list(DALY=get(paste("DALY",i,sep="")),
YLL=get(paste("YLL",i,sep="")), YLD=get(paste("YLD",i,sep="")),
Deaths=get(paste("Deaths",i,sep="")), name=tclvalue(get(paste("outcome",i,"Name",sep="")))))

output <- list(DALY=rowSums(DALY), YLD=rowSums(YLD), YLL=rowSums(YLL), Deaths=rowSums(Deaths),
name=tclvalue(diseaseName),
outcome1=o1, outcome2=o2, outcome3=o3, outcome4=o4,
outcome5=o5, outcome6=o6, outcome7=o7, outcome8=o8)

class(output$outcome1) <- "daly.output"; class(output$outcome2) <- "daly.output"
class(output$outcome3) <- "daly.output"; class(output$outcome4) <- "daly.output"
class(output$outcome5) <- "daly.output"; class(output$outcome6) <- "daly.output"
class(output$outcome7) <- "daly.output"; class(output$outcome8) <- "daly.output"

for (i in 1:8) if (!any(outcomes==i)) output[5+i] <- NULL

} else {

output <- list(DALY=rowSums(DALY), YLD=rowSums(YLD), YLL=rowSums(YLL), Deaths=rowSums(Deaths), name=tclvalue(diseaseName))

}

class(output) <- "daly.output"

if (button.call && tclvalue(valOutput) == "Basic") print(output)
if (button.call && tclvalue(valOutput) == "Advanced") summary(output)
if (button.call && tclvalue(valHist) == 1) hist(output)

return(output)
}

