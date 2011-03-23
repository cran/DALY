DALYoptions <-
function(){
setWindow <- tktoplevel(padx=15,pady=15)
tkwm.title(setWindow , "Options")

set1 <- tkframe(setWindow,padx=5,pady=5,relief="groove",borderwidth=2)
set2 <- tkframe(setWindow,padx=5,pady=5,relief="groove",borderwidth=2)
set3 <- tkframe(setWindow,padx=5,pady=5)

itEntry <- tkentry(set1,width=8,textvariable=it)
tkgrid(tklabel(set1,text="Iterations   "),itEntry)
tkgrid(set1,sticky="ew")

tkgrid(tklabel(setWindow,text=""))

radioBasic <- tkradiobutton(set2,variable=valOutput,value="Basic")
radioAdvnc <- tkradiobutton(set2,variable=valOutput,value="Advanced")
checkHist <- tkcheckbutton(set2,variable=valHist)
tkgrid(tklabel(set2,text="Output",font=tkfont.create(weight="bold",size="9")),columnspan=2,sticky="w")
tkgrid(radioBasic, tklabel(set2,text="Basic (total)"),sticky="w")
tkgrid(radioAdvnc, tklabel(set2,text="Advanced (total + outcome-wise)"),sticky="w")
tkgrid(checkHist, tklabel(set2,text="DALY histogram"),sticky="w")
tkgrid(set2,sticky="ew")

tkgrid(tklabel(setWindow,text=""))

save.but  <- tkbutton(set3,text="save settings and close window",width=30,command=function() tkdestroy(setWindow))
tkgrid(save.but,sticky="ew")
tkgrid(set3)
}

