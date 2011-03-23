drawWindow <-
function(window,outcomeName,this){
tkwm.title(window , paste("Data:",ifelse(tclvalue(diseaseName)!="",tclvalue(diseaseName),"disease"),">",ifelse(tclvalue(outcomeName)!="",tclvalue(outcomeName),paste("outcome",this))))
tkwm.geometry(window,"+0+0")

Top <<- tkframe(window)
 Left  <<- tkframe(Top,padx=15,pady=15)
 Right <<- tkframe(Top,padx=15,pady=15)
  Frame1 <<- tkframe(Left,relief="groove",borderwidth=2,bg="white")
  Frame2 <<- tkframe(Left,relief="groove",borderwidth=2,bg="white")
  Frame3 <<- tkframe(Left,relief="groove",borderwidth=2,bg="white")
  Frame4 <<- tkframe(Left,relief="groove",borderwidth=2,bg="white")
  Frame5 <<- tkframe(Right,relief="groove",borderwidth=2,bg="white")
  Frame6 <<- tkframe(Right,relief="groove",borderwidth=2,bg="white")
  Frame7 <<- tkframe(Right,relief="groove",borderwidth=2,bg="white")
  Frame8 <<- tkframe(Right,relief="groove",borderwidth=2,bg="white")
Bottom <<- tkframe(window,padx=0,pady=5)

save.but <<- tkbutton(Bottom,text="save data and close window",width=30,command=function() tkdestroy(window))
}

