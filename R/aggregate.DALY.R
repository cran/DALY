## S3 'aggregate' method for object of class 'DALY'
## aggregate by outcomes, by age/sex, or by both

aggregate.DALY <-
function(x, by = NULL, ...){
  ## Evaluate 'by'
  any.by <- c("total", "outcome", "class")
  dput <- "c(\"total\", \"outcome\", \"class\")"
  if (is.null(by) || is.na(by))
    stop(paste("'by' should be any of", dput))
  if (!any(tolower(by) == any.by))
    stop(paste("'by' should be any of", dput))

  ## Sum of lists
  lsum <-
  function(l, sub){
    s <- l[[1]][sub][[1]]
	if (nOutcomes > 1)
	  for (i in seq(2, nOutcomes))
        s <- s + l[[i]][sub][[1]]
	return(s)
  }
  
  ## Helper objects
  nOutcomes <- length(x) - 2
  nameList <- c("DALY", "YLD", "YLL", "Cases", "Deaths")
  nameLIST <- c("DALY", "YLD", "YLL", "INC", "MRT")
  
  ## Aggregate by outcome
  if (by == "outcome"){
    out <- vector("list", nOutcomes)
	out["pop"] <- x["pop"]
	out["name"] <- x["name"]
	for (i in seq(nOutcomes)){
	  out[[i]] <- vector("list", 5)
	  names(out[[i]]) <- nameList
	  out[[i]]["name"] <- x[[i]]["name"]
	  for (j in seq(5)){
	    out[[i]][[j]] <- rowSums(x[[i]][[j]])
	  }
	}
  }

  ## Aggregate by class
  if (by == "class"){
    out <- vector("list", 5)
	names(out) <- nameList
    out["pop"] <- x["pop"]
	out["name"] <- x["name"]
	for (i in seq(5)){
	  out[[i]] <- lsum(x, nameLIST[i])
	}
  }

  ## Aggregate by outcome & class
  if (by == "total"){
    y <- aggregate(x, by = "class")
	out <- vector("list", 5)
	names(out) <- nameList
    out["pop"] <- x["pop"]
	out["name"] <- x["name"]
	for (i in seq(5)){
	  out[[i]] <- rowSums(y[[i]])
	}
  }

  ## Return output
  return(invisible(out))
}
