writeData <-
function(txt,dist,strat,data,file){
write(c(txt,tclvalue(dist),tclvalue(strat)),file=file,ncolumns=6,sep="\t",append=TRUE)
for (i in 1:5) write(getData(data,"data")[i,],file=file,ncolumns=6,sep="\t",append=TRUE)
}

