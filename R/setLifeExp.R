setLifeExp <-
function(){
leWindow <- tktoplevel(padx=15,pady=10)
tkwm.title(leWindow, "Life Expectancy")

leTop <- tkframe(leWindow,padx=5,pady=10)
leBottom <- tkframe(leWindow,padx=5,pady=10)
reset.but <- tkbutton(leBottom,text="set standard life expectancy",width=25,command=setStdLE)
save.but  <- tkbutton(leBottom,text="save data and close window",width=30,command=function() tkdestroy(leWindow))

tableLE <- tkwidget(leTop,"table",resizeborders="none",selectmode="extended",multiline="0",rowseparator="\n",colseparator="\t",
variable=LE,rows=22,cols=3,titlerows=1,titlecols=1,colwidth=12,background="white")

tkgrid(tableLE);tkgrid(reset.but)
tkgrid(tklabel(leBottom,text="")); tkgrid(save.but)
tkgrid(leTop);tkgrid(leBottom)
}

