## 'hist' S3 Method for class 'DALY'

hist.DALY <-
function(x, xval = "DALY", prob = .95, central = "mean", ...){
  ## Check 'xval'
  nameList <- c("DALY", "YLD", "YLL", "Cases", "Deaths")
  dput <- "c(\"DALY\", \"YLD\", \"YLL\", \"Cases\", \"Deaths\")"
  if (is.null(xval) || is.na(xval))
    stop(paste("'xval' should be any of", dput))
  if (!any(tolower(xval) == tolower(nameList)))
    stop(paste("'xval' should be any of", dput))

  xval <- nameList[which(tolower(xval) == tolower(nameList))]

  ## Check 'prob'
  if (is.null(prob) || is.na(prob) || !is.numeric(prob))
    stop("'prob' must be a numeric value between 0 and 1")
  if (prob < 0 | prob > 1)
    stop("'prob' must be a numeric value between 0 and 1")

  ## Check 'central'
  cntrList <- c("mean", "median")
  dput <- "c(\"mean\", \"median\")"
  if (is.null(central) || is.na(central))
    stop(paste("'central' should be any of", dput))
  if (!any(tolower(central) == tolower(cntrList)))
    stop(paste("'central' should be any of", dput))
	
  ## Window settings
  old.mar <- par("mar")
  on.exit(par(mar = old.mar))
  par(mar = c(4, 4, 5, 2) + .5)

  ## Obtain aggregated 'xval'
  y <- aggregate(x, by = "total")[xval][[1]]
  
  ## Plot historgram
  hist(y, col = "grey90", freq = FALSE, include.lowest = TRUE, 
       xlab = xval, ylab = "Density", main = NULL, cex.axis = .9,
       breaks = seq(min(y, na.rm = TRUE), max(y, na.rm = TRUE),
                    length = 25))
  box()

  ## Save margins
  usr <- par("usr")
  
  ## Obtain summary statistics of 'xval'
  q <- summarize(y, .prob = prob)
  
  ## Plot CI & mean/median
  at <- c(mean(c(usr[1], q[3])), mean(q[3:4]), mean(c(usr[2], q[4])))

  text(mean(usr[c(1, 2)]), 1.2 * usr[4], x$name, xpd = NA, cex = 1.5)
  lines(density(y, na.rm = TRUE), col = "red", lwd = 2)

  rect(usr[1], usr[4], usr[2], usr[4] * 1.05, col = "grey95", xpd = NA)
  rect(q[3], usr[4], q[4], usr[4] * 1.05, col = "red", xpd = NA)

  at.y <- 1.025 * usr[4]
  text(at[1], at.y, sprintf("%0.1f%%", 100 * (1-prob)/2), xpd = NA, cex = .7)
  text(at[2], at.y, sprintf("%0.1f%%", 100 * prob), xpd = NA, cex = .7)
  text(at[3], at.y, sprintf("%0.1f%%", 100 * (1-prob)/2), xpd = NA, cex = .7)

  points(q[3:4], rep(1.05 * usr[4], 2), pch = 25, bg = "black", xpd = NA)
  text(q[3:4], rep(1.08 * usr[4], 2), round(q[3:4]), xpd = NA, cex = .6)
  abline(v = q[3:4], lwd = 1)

  c <- ifelse(central == "mean", q[1], q[2])
  points(c, 1.05 * usr[4], pch = 25, cex = 1,
         col = "black", bg = "red", xpd = NA)
  text(c, 1.08 * usr[4], c, cex = .6, xpd = NA)
}
