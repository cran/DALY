print.daly.output <-
function(x, ...){
cat("\nDALY Calculator: ", x$name, "\n\n")
cat("\t Mean", rep(" ",nchar(round(mean(x$DALY)))+1), "Median", rep(" ",ifelse(nchar(round(median(x$DALY)))>3, nchar(round(median(x$DALY)))-1, 2)), "95% Credible Interval\n", sep="")
cat(" DALY:\t ", round(mean(x$DALY),0), rep(" ",5), round(median(x$DALY),0), rep(" ",5+floor(2/nchar(round(median(x$DALY))))),"[ ", round(quantile(x$DALY, .025),0), " - ", round(quantile(x$DALY, .975),0), " ]\n", sep="")
cat(" YLD:\t ", round(mean(x$YLD),0), rep(" ",5+nchar(round(mean(x$DALY)))-nchar(round(mean(x$YLD)))), round(median(x$YLD),0), rep(" ",5+nchar(round(median(x$DALY)))-nchar(round(median(x$YLD)))+floor(2/nchar(round(median(x$DALY))))),"[ ", round(quantile(x$YLD, .025),0), " - ", round(quantile(x$YLD, .975),0), " ]\n", sep="")
cat(" YLL:\t ", round(mean(x$YLL),0), rep(" ",5+nchar(round(mean(x$DALY)))-nchar(round(mean(x$YLL)))), round(median(x$YLL),0), rep(" ",5+nchar(round(median(x$DALY)))-nchar(round(median(x$YLL)))+floor(2/nchar(round(median(x$DALY))))),"[ ", round(quantile(x$YLL, .025),0), " - ", round(quantile(x$YLL, .975),0), " ]\n", sep="")
cat(" Deaths: ", round(mean(x$Deaths),0), rep(" ",5+nchar(round(mean(x$DALY)))-nchar(round(mean(x$Deaths)))), round(median(x$Deaths),0), rep(" ",5+nchar(round(median(x$DALY)))-nchar(round(median(x$Deaths)))+floor(2/nchar(round(median(x$DALY))))),"[ ", round(quantile(x$Deaths, .025),0), " - ", round(quantile(x$Deaths, .975),0), " ]\n\n", sep="")
cat(" DALY ~ ", round(100*mean(x$YLD)/mean(x$DALY),0), "% YLD + ", round(100*mean(x$YLL)/mean(x$DALY),0), "% YLL\n\n", sep="")
}

