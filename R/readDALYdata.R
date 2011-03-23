readDALYdata <-
function(){

reset()

fileName <- tclvalue(tkgetOpenFile(filetypes="{{Text files} {.txt}}"))
if(fileName!=""){
readData <- read.delim(fileName,header=FALSE)
ncols <- dim(readData)[2]
nrowO <- ((30/ncols)+1)*8+1
nrowD <- (30/ncols)+1

for (y in 1:5) for (x in 1:2) if (!is.na(as.double(as.character(readData[y+1,x])))) pop[[y,x]] <- as.double(as.character(readData[y+1,x]))
if(!is.na(as.character(readData[7,2]))) tclvalue(diseaseName) <- as.character(readData[7,2])

for(i in 1:8){
loadData("outcome", 0, 0, get(paste("outcome",i,"Name",sep="")), readData[7+nrowO*(i-1)+1,2])
for(j in 1:8) loadData("data", get(paste("dist",txtLbl[j],i,sep="")), get(paste("strat",txtLbl[j],i,sep="")), get(paste(txtlbl[j],i,sep="")), readData, i, j, nrowO, nrowD)
}

tkmessageBox(message="Data successfully loaded", title="DALY calculator")
}
}

