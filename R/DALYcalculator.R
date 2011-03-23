DALYcalculator <-
function(){

# Initiate Tcl/Tk widgets

tclRequire("Tktable")
tclRequire("BWidget")

# Set some variables

rm(list=ls())

LE <<- tclArray()
valOutput <<- tclVar("Basic")
valHist <<- tclVar("1")
setVariables()
setStdLE()

# Create main DALY Calculator window

main <<- tktoplevel(padx=10,pady=10)
tkwm.title(main, "DALY Calculator")

Frame1 <- tkframe(main,padx=80,pady=5,relief="groove",borderwidth=2)
Frame2 <- tkframe(main,padx=15,pady=10,relief="groove",borderwidth=2)
Frame3 <- tkframe(main,padx=62,pady=5,relief="groove",borderwidth=2)
Frame4 <- tkframe(main,pady=10)

DALYmenu <- tkmenu(main)
tkconfigure(main, menu=DALYmenu)
fileMenu <- tkmenu(DALYmenu,tearoff=FALSE)
setsMenu <- tkmenu(DALYmenu,tearoff=FALSE)
helpMenu <- tkmenu(DALYmenu,tearoff=FALSE)
examplesMenu <- tkmenu(helpMenu,tearoff=FALSE)
tkadd(fileMenu,"command",label="Load DALY data from file...",command=readDALYdata)
tkadd(fileMenu,"command",label="Save DALY data to file...",command=saveDALYdata)
tkadd(fileMenu,"separator")
tkadd(fileMenu,"command",label="Reset DALY Calculator",command=reset)
tkadd(fileMenu,"separator")
tkadd(fileMenu,"command",label="Exit",command=function() tkdestroy(main))
tkadd(DALYmenu,"cascade",label="File",menu=fileMenu)
tkadd(setsMenu,"command",label="Life Expectancy table...",command=setLifeExp)
tkadd(setsMenu,"command",label="Options...",command=DALYoptions)
tkadd(DALYmenu,"cascade",label="Settings",menu=setsMenu)
tkadd(helpMenu,"cascade",label="Load examples",menu=examplesMenu)
tkadd(examplesMenu,"command",label="Neurocysticercosis, Cameroon (Praet et al., 2009)",command=function() setDALYexample(1))
tkadd(examplesMenu,"command",label="Toxoplasmosis, the Netherlands (Kortbeek et al., 2009)",command=function() setDALYexample(2))
tkadd(helpMenu,"separator")
tkadd(helpMenu,"command",label="Html help",command=function() openHelpFile("DALYcalculator"))
tkadd(helpMenu,"command",label="DALY Calculator manual (PDF)",command=DALYmanual)
tkadd(helpMenu,"separator")
tkadd(helpMenu,"command",label="Package description",command=function() openHelpFile("DALY-package"))
tkadd(helpMenu,"command",label="DALY Calculator Info",command=function() tkmessageBox(message="DALY Calculator v1.0 (2011-03-23)\n\nhttp://studwww.ugent.be/~bdvleess/DALYcalculator\n\nDeveloped and maintained by:\n   Brecht Devleesschauwer <brecht.devleesschauwer@ugent.be>\n\nWith contributions from:\n   Arie Havelaar <arie.havelaar@rivm.nl>\n   Juanita Haagsma <j.haagsma@erasmusmc.nl>\n   Nicolas Praet <npraet@itg.be>\n   Niko Speybroeck <niko.speybroeck@uclouvain.be>",title="Information"))
tkadd(DALYmenu,"cascade",label="Help",menu=helpMenu)

setPop.but <- tkbutton(Frame1,text="set population",width=14,padx=5,pady=5,command=setPop)
outcome1.but <- tkbutton(Frame2,text="set data",padx=2,command=function() setData(1))
outcome2.but <- tkbutton(Frame2,text="set data",padx=2,command=function() setData(2))
outcome3.but <- tkbutton(Frame2,text="set data",padx=2,command=function() setData(3))
outcome4.but <- tkbutton(Frame2,text="set data",padx=2,command=function() setData(4))
outcome5.but <- tkbutton(Frame2,text="set data",padx=2,command=function() setData(5))
outcome6.but <- tkbutton(Frame2,text="set data",padx=2,command=function() setData(6))
outcome7.but <- tkbutton(Frame2,text="set data",padx=2,command=function() setData(7))
outcome8.but <- tkbutton(Frame2,text="set data",padx=2,command=function() setData(8))
calculate.but <- tkbutton(Frame4,text="CALCULATE DALYs",width=37,padx=5,pady=10,command=function() getDALY(button.call=TRUE))

diseaseEntry <- tkentry(Frame2,width=20,textvariable=diseaseName)
outcome1Entry <- tkentry(Frame2,width=20,textvariable=outcome1Name)
outcome2Entry <- tkentry(Frame2,width=20,textvariable=outcome2Name)
outcome3Entry <- tkentry(Frame2,width=20,textvariable=outcome3Name)
outcome4Entry <- tkentry(Frame2,width=20,textvariable=outcome4Name)
outcome5Entry <- tkentry(Frame2,width=20,textvariable=outcome5Name)
outcome6Entry <- tkentry(Frame2,width=20,textvariable=outcome6Name)
outcome7Entry <- tkentry(Frame2,width=20,textvariable=outcome7Name)
outcome8Entry <- tkentry(Frame2,width=20,textvariable=outcome8Name)

awList <- c("Yes","No")
awCombo <- tkwidget(Frame3,"ComboBox",editable=FALSE,values=awList,textvariable=awTcl,width=5)
drEntry <- tkentry(Frame3,width=7,textvariable=drTcl)

tkgrid(setPop.but)
tkgrid(Frame1)

bold <- tkfont.create(weight="bold",size="11")
tkgrid(tklabel(Frame2,text="disease ",font=bold),diseaseEntry)
tkgrid(tklabel(Frame2,text="outcome 1 "),outcome1Entry,outcome1.but)
tkgrid(tklabel(Frame2,text="outcome 2 "),outcome2Entry,outcome2.but)
tkgrid(tklabel(Frame2,text="outcome 3 "),outcome3Entry,outcome3.but)
tkgrid(tklabel(Frame2,text="outcome 4 "),outcome4Entry,outcome4.but)
tkgrid(tklabel(Frame2,text="outcome 5 "),outcome5Entry,outcome5.but)
tkgrid(tklabel(Frame2,text="outcome 6 "),outcome6Entry,outcome6.but)
tkgrid(tklabel(Frame2,text="outcome 7 "),outcome7Entry,outcome7.but)
tkgrid(tklabel(Frame2,text="outcome 8 "),outcome8Entry,outcome8.but)
tkgrid(Frame2,pady=10)

tkgrid(tklabel(Frame3,text="age weighting "),awCombo)
tkgrid(tklabel(Frame3,text="discount rate (%) "),drEntry)
tkgrid(Frame3)

tkgrid(calculate.but)
tkgrid(Frame4)

}

