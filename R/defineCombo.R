defineCombo <-
function(frame,var,list){
w <- 17
if (length(list)==4) w <- 12
combo <- tkwidget(frame,"ComboBox",editable=FALSE,values=list,textvariable=var,width=w)
return(combo)
}

