#' @title calculate overlap between samples
#' @description function to calculate the overlap of area between two samples (normal or log-normal).
#' interpreted as the fraction of overlapping realizations given sufficient samples
#' @export
#' @importFrom stats shapiro.test
sampleOverlap = function(x, y, p.value.cutoff=0.05) {
	
	# check if normal.  return transformed data, if needed.
	get.distr = function(x, p.value.cutoff) {
		test.x=stats::shapiro.test(x)
		if(test.x$p.value < p.value.cutoff) {
			x = log(x)
			test.x=stats::shapiro.test(x)
			if(test.x$p.value < p.value.cutoff) {
				stop("With or without log-transformation, data is non-normal.  Halting.")
			} else {
				warning("With log-transformation, data is normal.")
			}
		}
		
		return(list(mean=mean(x), sd=sd(x)))
	}
	
	# get density function parameters
	xd = get.distr(x, p.value.cutoff)
	yd = get.distr(y, p.value.cutoff)
	
	return(distribution.overlap(xd$mean, yd$mean, xd$sd, yd$sd) )
}
