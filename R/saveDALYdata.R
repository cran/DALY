saveDALYdata <-
function(){

fileName <- tclvalue(tkgetSaveFile(initialfile=paste("DALY_Data_",tclvalue(diseaseName),".txt",sep=""),filetypes="{{Text files} {.txt}}"))

if(fileName!=""){
if (substr(fileName,nchar(fileName)-2,nchar(fileName))!="txt") fileName = paste(fileName,".txt",sep="")

write(c("POPULATION",rep("",5)),file=fileName,ncolumns=6,sep="\t")
for (i in 1:5) write(getData(pop,"pop")[i,],file=fileName,ncolumns=6,sep="\t",append=TRUE)

write(c("DISEASE",tclvalue(diseaseName)),file=fileName,ncolumns=6,sep="\t",append=TRUE)

for (i in 1:8){
write(c(paste("OUTCOME",i,sep=""),tclvalue(get(paste("outcome",i,"Name",sep="")))),file=fileName,ncolumns=6,sep="\t",append=TRUE)
for (j in 1:8) writeData(paste(txtLBL[j],i,sep=""),get(paste("dist",txtLbl[j],i,sep="")),get(paste("strat",txtLbl[j],i,sep="")),get(paste(txtlbl[j],i,sep="")),fileName)
}

tkmessageBox(message=paste("Data successfully saved to\n\"",fileName,"\"",sep=""), title="DALY calculator")

}
}

