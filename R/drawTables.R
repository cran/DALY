drawTables <-
function(tblInc,tblOns,tblDWt,tblMrt,tblTrt,tblDur,tblDWn,tblLxp,
cbdInc,cbdOns,cbdDWt,cbdMrt,cbdTrt,cbdDur,cbdDWn,cbdLxp,
cbsInc,cbsOns,cbsDWt,cbsMrt,cbsTrt,cbsDur,cbsDWn,cbsLxp){
label <- tklabel(Frame1,text="  incidence (cases/1000 persons/year)",bg="white",width="36",anchor="w")
tkgrid(label,cbdInc,cbsInc); tkgrid.configure(cbdInc,sticky="e"); tkgrid.configure(cbsInc,sticky="e")
tkgrid(tblInc, columnspan=3); tkgrid(Frame1); tkgrid(tklabel(Left, text=""))
label <- tklabel(Frame2,text="  onset (age in years)",bg="white",width="36",anchor="w")
tkgrid(label,cbdOns,cbsOns); tkgrid.configure(cbdOns,sticky="e"); tkgrid.configure(cbsOns,sticky="e")
tkgrid(tblOns, columnspan=3); tkgrid(Frame2); tkgrid(tklabel(Left, text=""))
label <- tklabel(Frame3,text="  DW treated (range [0-1])",bg="white",width="36",anchor="w")
tkgrid(label,cbdDWt,cbsDWt); tkgrid.configure(cbdDWt,sticky="e"); tkgrid.configure(cbsDWt,sticky="e")
tkgrid(tblDWt, columnspan=3); tkgrid(Frame3); tkgrid(tklabel(Left, text=""))
label <- tklabel(Frame4,text="  mortality (deaths/1000 persons/year)",bg="white",width="36",anchor="w")
tkgrid(label,cbdMrt,cbsMrt); tkgrid.configure(cbdMrt,sticky="e"); tkgrid.configure(cbsMrt,sticky="e")
tkgrid(tblMrt, columnspan=3); tkgrid(Frame4)

label <- tklabel(Frame5,text="  treatment (proportion [0-1])",bg="white",width="36",anchor="w")
tkgrid(label,cbdTrt,cbsTrt); tkgrid.configure(cbdTrt,sticky="e"); tkgrid.configure(cbsTrt,sticky="e")
tkgrid(tblTrt, columnspan=3); tkgrid(Frame5); tkgrid(tklabel(Right, text=""))
label <- tklabel(Frame6,text="  duration (years)",bg="white",width="36",anchor="w")
tkgrid(label,cbdDur,cbsDur); tkgrid.configure(cbdDur,sticky="e"); tkgrid.configure(cbsDur,sticky="e")
tkgrid(tblDur, columnspan=3); tkgrid(Frame6); tkgrid(tklabel(Right, text=""))
label <- tklabel(Frame7,text="  DW untreated (range [0-1])",bg="white",width="36",anchor="w")
tkgrid(label,cbdDWn,cbsDWn); tkgrid.configure(cbdDWn,sticky="e"); tkgrid.configure(cbsDWn,sticky="e")
tkgrid(tblDWn, columnspan=3); tkgrid(Frame7); tkgrid(tklabel(Right, text=""))
label <- tklabel(Frame8,text="  average age at death (age in years)",bg="white",width="36",anchor="w")
tkgrid(label,cbdLxp,cbsLxp); tkgrid.configure(cbdLxp,sticky="e"); tkgrid.configure(cbsLxp,sticky="e")
tkgrid(tblLxp, columnspan=3); tkgrid(Frame8)

tkgrid(Left,Right); tkgrid(save.but)
tkgrid(Top); tkgrid(Bottom)
}

