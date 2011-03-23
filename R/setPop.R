setPop <-
function(){
popTT <<- tktoplevel(padx=15,pady=15)
tkwm.title(popTT, "Population")

popTop <- tkframe(popTT,padx=5,pady=5)
popBottom <- tkframe(popTT,padx=5,pady=5)
save.but <- tkbutton(popBottom,text="save data and close window",width=30,command=function() tkdestroy(popTT))

tablePop <<- tkwidget(popTop,"table",resizeborders="none",selectmode="extended",multiline="0",rowseparator="\n",colseparator="\t",
variable=pop,rows=6,cols=3,titlerows=1,titlecols=1,colwidth=15,background="white")

tkgrid(tablePop);tkgrid(save.but)
tkgrid(popTop);tkgrid(popBottom)
}

