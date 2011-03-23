setVariables <-
function(){

ageGroups<- c("0-4","5-14","15-44","45-59","60+")
fixed<- c("Age Group","Male","Female")
txtLBL<<- c("INCIDENCE","TREATMENT","ONSET","DURATION","DWtreated","DWuntreated","MORTALITY","AvgAgeDeath")
txtLbl<<- c("Inc","Trt","Ons","Dur","DWt","DWn","Mrt","Lxp")
txtlbl<<- c("inc","trt","ons","dur","DWt","DWn","mrt","lxp")
distributions <<- c("Beta-Pert","Beta","Gamma","Normal","LogNormal.geom","LogNormal.arithm","Uniform","Fixed")
stratifications <<- c("Age and Sex","Age","Sex","None")
stdM<<- c(80.00, 79.36, 75.38, 70.40, 65.41, 60.44, 55.47, 50.51, 45.57, 40.64, 35.77, 30.99, 26.32, 21.81, 17.50, 13.58, 10.17, 7.45, 5.24, 3.54, 2.31)
stdF<<- c(82.50, 81.84, 77.95, 72.99, 68.02, 63.08, 58.17, 53.27, 48.38, 43.53, 38.72, 33.99, 29.37, 24.83, 20.44, 16.20, 12.28, 8.90, 6.22, 4.25, 2.89)
ages <<- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)

pop <<- tclArray()
for(x in 0:2) pop[[0,x]] <- fixed[x+1]
for(y in 1:5) pop[[y,0]] <- ageGroups[y]

if (!exists("LE")) LE <<- tclArray()
LE[[0,0]] <- "Age"
for(x in 1:2)  LE[[0,x]] <- fixed[x+1]
for(y in 1:21) LE[[y,0]] <- ages[y]

for (i in 1:8){
for (j in 1:8){
assign(paste(txtlbl[j], i, sep=""), tclArray(), envir = .GlobalEnv)
assign(paste("strat", txtLbl[j], i, sep=""), tclVar(stratifications[1]), envir = .GlobalEnv)
}
assign(paste("dist", txtLbl[1], i, sep=""), tclVar(distributions[3]), envir = .GlobalEnv)
assign(paste("dist", txtLbl[2], i, sep=""), tclVar(distributions[2]), envir = .GlobalEnv)
assign(paste("dist", txtLbl[3], i, sep=""), tclVar(distributions[8]), envir = .GlobalEnv)
assign(paste("dist", txtLbl[4], i, sep=""), tclVar(distributions[8]), envir = .GlobalEnv)
assign(paste("dist", txtLbl[5], i, sep=""), tclVar(distributions[2]), envir = .GlobalEnv)
assign(paste("dist", txtLbl[6], i, sep=""), tclVar(distributions[2]), envir = .GlobalEnv)
assign(paste("dist", txtLbl[7], i, sep=""), tclVar(distributions[3]), envir = .GlobalEnv)
assign(paste("dist", txtLbl[8], i, sep=""), tclVar(distributions[8]), envir = .GlobalEnv)
}

for (y in 1:5){
inc1[[y,0]] = inc2[[y,0]] = inc3[[y,0]] = inc4[[y,0]] = inc5[[y,0]] = inc6[[y,0]] = inc7[[y,0]] = inc8[[y,0]] <- ageGroups[y]
trt1[[y,0]] = trt2[[y,0]] = trt3[[y,0]] = trt4[[y,0]] = trt5[[y,0]] = trt6[[y,0]] = trt7[[y,0]] = trt8[[y,0]] <- ageGroups[y]
ons1[[y,0]] = ons2[[y,0]] = ons3[[y,0]] = ons4[[y,0]] = ons5[[y,0]] = ons6[[y,0]] = ons7[[y,0]] = ons8[[y,0]] <- ageGroups[y]
dur1[[y,0]] = dur2[[y,0]] = dur3[[y,0]] = dur4[[y,0]] = dur5[[y,0]] = dur6[[y,0]] = dur7[[y,0]] = dur8[[y,0]] <- ageGroups[y]
DWt1[[y,0]] = DWt2[[y,0]] = DWt3[[y,0]] = DWt4[[y,0]] = DWt5[[y,0]] = DWt6[[y,0]] = DWt7[[y,0]] = DWt8[[y,0]] <- ageGroups[y]
DWn1[[y,0]] = DWn2[[y,0]] = DWn3[[y,0]] = DWn4[[y,0]] = DWn5[[y,0]] = DWn6[[y,0]] = DWn7[[y,0]] = DWn8[[y,0]] <- ageGroups[y]
mrt1[[y,0]] = mrt2[[y,0]] = mrt3[[y,0]] = mrt4[[y,0]] = mrt5[[y,0]] = mrt6[[y,0]] = mrt7[[y,0]] = mrt8[[y,0]] <- ageGroups[y]
lxp1[[y,0]] = lxp2[[y,0]] = lxp3[[y,0]] = lxp4[[y,0]] = lxp5[[y,0]] = lxp6[[y,0]] = lxp7[[y,0]] = lxp8[[y,0]] <- ageGroups[y]
}

if(!exists("diseaseName"))  diseaseName  <<- tclVar();tclvalue(diseaseName)  <- ""
if(!exists("outcome1Name")) outcome1Name <<- tclVar();tclvalue(outcome1Name) <- ""
if(!exists("outcome2Name")) outcome2Name <<- tclVar();tclvalue(outcome2Name) <- ""
if(!exists("outcome3Name")) outcome3Name <<- tclVar();tclvalue(outcome3Name) <- ""
if(!exists("outcome4Name")) outcome4Name <<- tclVar();tclvalue(outcome4Name) <- ""
if(!exists("outcome5Name")) outcome5Name <<- tclVar();tclvalue(outcome5Name) <- ""
if(!exists("outcome6Name")) outcome6Name <<- tclVar();tclvalue(outcome6Name) <- ""
if(!exists("outcome7Name")) outcome7Name <<- tclVar();tclvalue(outcome7Name) <- ""
if(!exists("outcome8Name")) outcome8Name <<- tclVar();tclvalue(outcome8Name) <- ""

if(!exists("it"))it <<- tclVar();tclvalue(it) <- 20000
if(!exists("awTcl")) awTcl <<- tclVar();tclvalue(awTcl) <- "No"
if(!exists("drTcl")) drTcl <<- tclVar();tclvalue(drTcl) <- "0"

}

