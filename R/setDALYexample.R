setDALYexample <-
function(example){

reset()

if(example==1){
setStdLE()
tclvalue(diseaseName) <- "Neurocysticercosis"
tclvalue(outcome1Name) <- "Epilepsy"
tclvalue(awTcl) <- "Yes"
tclvalue(drTcl) <- 3
tclvalue(distTrt1) = tclvalue(distDWt1) = tclvalue(distDWn1) <- "Beta"
tclvalue(distOns1) = tclvalue(distDur1) <- "Fixed"
tclvalue(distInc1) = tclvalue(distMrt1) <- "Gamma"
tclvalue(stratTrt1) = tclvalue(stratMrt1) <- "None"
for (y in 1:5){
pop[[y,1]] <- c(397229, 686600, 1073342, 210474, 129081)[y]
pop[[y,2]] <- c(408673, 706380, 1104265, 216538, 132800)[y]
trt1[[y,1]] = trt1[[y,3]] <- 267
trt1[[y,2]] = trt1[[y,4]] <- 733
ons1[[y,1]] = ons1[[y,2]] <- c(2.5, 9.95, 26.99, 51.94, 73.6)[y]
dur1[[y,1]] <- c(1.4, 2.0, 3.6, 2.8, 1.6)[y]
dur1[[y,2]] <- c(1.6, 3.1, 5.9, 6.0, 2.8)[y]
DWt1[[y,1]] = DWt1[[y,3]] <- 1.5
DWt1[[y,2]] = DWt1[[y,4]] <- c(35, 21.6, 21.6, 21.6, 21.6)[y]
DWn1[[y,1]] = DWn1[[y,3]] <- 3
DWn1[[y,2]] = DWn1[[y,4]] <- c(27.3, 17, 17, 17, 17)[y]
lxp1[[y,1]] = lxp1[[y,2]] <- c(2.5, 9.95, 26.99, 51.94, 73.6)[y]
inc1[[y,1]] = inc1[[y,3]] = 47.3
inc1[[y,2]] = c(6.671,9.522,17.141,13.321,7.617)[y]
inc1[[y,4]] = c(7.617,14.756,28.093,28.574,13.321)[y]
}
mrt1[[1,1]] <- 3.049
mrt1[[1,2]] <- 12.321
}

if(example==2){
tclvalue(diseaseName) <- "Toxoplasmosis"
tclvalue(outcome1Name) <- "Fetal loss"
tclvalue(outcome2Name) <- "Chorioretinitis"
tclvalue(outcome3Name) <- "Intracranial calcification"
tclvalue(outcome4Name) <- "Hydrocephalus"
tclvalue(outcome5Name) <- "CNS abnormalities"
tclvalue(outcome6Name) <- "Neonatal death"
tclvalue(outcome7Name) <- "Chorioretinitis later in life"
pop[[1,1]] = pop[[1,2]] <- 194000/2

tclvalue(distMrt1) <- "Beta-Pert"; tclvalue(stratMrt1) <- "Age"
tclvalue(distInc1) = tclvalue(distTrt1) = tclvalue(distDWt1) <- "Fixed"
tclvalue(distDWn1) = tclvalue(distOns1) = tclvalue(distDur1) <- "Fixed"
tclvalue(distLxp1) <- "Fixed"; tclvalue(stratLxp1) <- "Age"
tclvalue(stratInc1) = tclvalue(stratTrt1) = tclvalue(stratDWt1) <- "Age"
tclvalue(stratDWn1) = tclvalue(stratOns1) = tclvalue(stratDur1) <- "Age"
mrt1[[1,1]] <- round((6*(10.7/194) - 5.0/194 - 34.1/194)/4,6)
mrt1[[1,2]] <- round(5.0/194,6); mrt1[[1,3]] <- round(34.1/194,6)
lxp1[[1,1]] <- 0;	for (i in 2:5) lxp1[[i,1]] <- NULL

tclvalue(distInc2) <- "Beta-Pert"; tclvalue(stratInc2) <- "Age"
tclvalue(distTrt2) = tclvalue(distDWt2) = tclvalue(distDWn2) <- "Fixed"
tclvalue(distMrt2) = tclvalue(distOns2) = tclvalue(distDur2) <- "Fixed"
tclvalue(distLxp2) <- "Fixed"; tclvalue(stratLxp2) <- "Age"
tclvalue(stratTrt2) = tclvalue(stratDWt2) = tclvalue(stratDWn2) <- "Age"
tclvalue(stratMrt2) = tclvalue(stratOns2) = tclvalue(stratDur2) <- "Age"
inc2[[1,1]] <- round((6*(50.3/194) - 25.6/194 - 81.4/194)/4,6)
inc2[[1,2]] <- round(25.6/194,6); inc2[[1,3]] <- round(81.4/194,6)
trt2[[1,1]] <- 1; DWt2[[1,1]] <- 0.08; ons2[[1,1]] <- 0; dur2[[1,1]] <- 79
for (i in 1:5) lxp2[[i,1]] <- NULL

tclvalue(distInc3) <- "Beta-Pert"; tclvalue(stratInc3) <- "Age"
tclvalue(distTrt3) = tclvalue(distDWt3) = tclvalue(distDWn3) <- "Fixed"
tclvalue(distMrt3) = tclvalue(distOns3) = tclvalue(distDur3) <- "Fixed"
tclvalue(distLxp3) <- "Fixed"; tclvalue(stratLxp3) <- "Age"
tclvalue(stratTrt3) = tclvalue(stratDWt3) = tclvalue(stratDWn3) <- "Age"
tclvalue(stratMrt3) = tclvalue(stratOns3) = tclvalue(stratDur3) <- "Age"
inc3[[1,1]] <- round((6*(42.7/194) - 17.0/194 - 65.2/194)/4,6)
inc3[[1,2]] <- round(17.0/194,6); inc3[[1,3]] <- round(65.2/194,6)
trt3[[1,1]] <- 1; DWt3[[1,1]] <- 0.01; ons3[[1,1]] <- 0; dur3[[1,1]] <- 79
for (i in 1:5) lxp3[[i,1]] <- NULL

tclvalue(distInc4) <- "Beta-Pert"; tclvalue(stratInc4) <- "Age"
tclvalue(distTrt4) = tclvalue(distDWt4) = tclvalue(distDWn4) <- "Fixed"
tclvalue(distMrt4) = tclvalue(distOns4) = tclvalue(distDur4) <- "Fixed"
tclvalue(distLxp4) <- "Fixed"; tclvalue(stratLxp4) <- "Age"
tclvalue(stratTrt4) = tclvalue(stratDWt4) = tclvalue(stratDWn4) <- "Age"
tclvalue(stratMrt4) = tclvalue(stratOns4) = tclvalue(stratDur4) <- "Age"
inc4[[1,1]] <- round((6*(7.7/194) - 2.13/194 - 16.3/194)/4,6)
inc4[[1,2]] <- round(2.13/194,6); inc4[[1,3]] <- round(16.3/194,6)
trt4[[1,1]] <- 1; DWt4[[1,1]] <- 0.36; ons4[[1,1]] <- 0; dur4[[1,1]] <- 79
for (i in 1:5) lxp4[[i,1]] <- NULL

tclvalue(distInc5) <- "Beta-Pert"; tclvalue(stratInc5) <- "Age"
tclvalue(distTrt5) = tclvalue(distDWt5) = tclvalue(distDWn5) <- "Fixed"
tclvalue(distMrt5) = tclvalue(distOns5) = tclvalue(distDur5) <- "Fixed"
tclvalue(distLxp5) <- "Fixed"; tclvalue(stratLxp5) <- "Age"
tclvalue(stratTrt5) = tclvalue(stratDWt5) = tclvalue(stratDWn5) <- "Age"
tclvalue(stratMrt5) = tclvalue(stratOns5) = tclvalue(stratDur5) <- "Age"
inc5[[1,1]] <- round((6*(11.6/194) - 2.13/194 - 32.6/194)/4,6)
inc5[[1,2]] <- round(2.13/194,6); inc5[[1,3]] <- round(32.6/194,6)
trt5[[1,1]] <- 1; DWt5[[1,1]] <- 0.36; ons5[[1,1]] <- 0; dur5[[1,1]] <- 79
for (i in 1:5) lxp5[[i,1]] <- NULL

tclvalue(distMrt6) <- "Beta-Pert"; tclvalue(stratMrt6) <- "Age"
tclvalue(distInc6) = tclvalue(distTrt6) = tclvalue(distDWt6) <- "Fixed"
tclvalue(distDWn6) = tclvalue(distOns6) = tclvalue(distDur6) <- "Fixed"
tclvalue(distLxp6) <- "Fixed"; tclvalue(stratLxp6) <- "Age"
tclvalue(stratInc6) = tclvalue(stratTrt6) = tclvalue(stratDWt6) <- "Age"
tclvalue(stratDWn6) = tclvalue(stratOns6) = tclvalue(stratDur6) <- "Age"
mrt6[[1,1]] <- round((6*(2.7/194) - 0.85/194 - 6.51/194)/4,6)
mrt6[[1,2]] <- round(0.85/194,6); mrt6[[1,3]] <- round(6.51/194,6)
lxp6[[1,1]] <- 0;	for (i in 2:5) lxp6[[i,1]] <- NULL

tclvalue(distInc7) <- "Beta-Pert"; tclvalue(stratInc7) <- "Age"
tclvalue(distTrt7) = tclvalue(distDWt7) = tclvalue(distDWn7) <- "Fixed"
tclvalue(distMrt7) = tclvalue(distOns7) = tclvalue(distDur7) <- "Fixed"
tclvalue(distLxp7) <- "Fixed"; tclvalue(stratLxp7) <- "Age"
tclvalue(stratTrt7) = tclvalue(stratDWt7) = tclvalue(stratDWn7) <- "Age"
tclvalue(stratMrt7) = tclvalue(stratOns7) = tclvalue(stratDur7) <- "Age"
inc7[[1,1]] <- round((6*(62.4/194) - 10.7/194 - 280.4/194)/4,6)
inc7[[1,2]] <- round(10.7/194,6); inc7[[1,3]] <- round(280.4/194,6)
trt7[[1,1]] <- 1; DWt7[[1,1]] <- 0.08; ons7[[1,1]] <- 10; dur7[[1,1]] <- 69
for (i in 1:5) lxp7[[i,1]] <- NULL
}
}