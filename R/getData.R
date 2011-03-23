getData <-
function(tclArray,type){
if (type=="pop") rArray <- array(0,dim=c(5,2))
if (type=="data") rArray <- array(0,dim=c(5,6))
dimList <- names(tclArray)[names(tclArray)!="active"]
xlist0 <- as.integer(substr(dimList,3,3))
ylist0 <- as.integer(substr(dimList,1,1))
xlist <- xlist0[xlist0!=0&ylist0!=0]
ylist <- ylist0[xlist0!=0&ylist0!=0]
if(length(xlist)>0) for (i in 1:length(xlist)) rArray[ylist[i],xlist[i]] <- as.double(tclvalue(tclArray[[ylist[i],xlist[i]]]))

return(rArray)
}

