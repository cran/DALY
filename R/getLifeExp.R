getLifeExp <-
function(){

for (i in 1:21) if( is.null(LE[[i,1]]) || is.null(LE[[i,2]]) ) stop("Life Expectancy table contains empty cell(s).", call. = FALSE)
for (i in 1:21) if( is.na(as.double(LE[[i,1]])) || is.na(as.double(LE[[i,2]])) ) stop("Life Expectancy table contains non-numeric value(s).", call. = FALSE)

lineM <- array(dim=0)
lineF <- array(dim=0)

for (i in 1:20){
lineM <- c(lineM, seq(as.double(LE[[i,1]]),as.double(LE[[i+1,1]]),length.out=(10*(ages[i+1]-ages[i])+1)))
lineF <- c(lineF, seq(as.double(LE[[i,2]]),as.double(LE[[i+1,2]]),length.out=(10*(ages[i+1]-ages[i])+1)))
lineM <- lineM[-length(lineM)]
lineF <- lineF[-length(lineF)]
}

lineM <- c(lineM, as.double(LE[[21,1]]))
lineF <- c(lineF, as.double(LE[[21,2]]))

return(c(lineM,lineF))
}

