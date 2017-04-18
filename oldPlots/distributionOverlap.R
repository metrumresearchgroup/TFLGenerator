#' @title function to calculate the overlap of area between two normal distributions
#' @importFrom stats dnorm integrate
distributionOverlap = function(mu1, mu2, sd1, sd2) {
	
	# generate the minima of d1(x),d2(x) across x.
	min.f1f2 <- function(x, mu1, mu2, sd1, sd2) {
		# this could certainly be extended to more distributions
		f1 <- stats::dnorm(x, mean=mu1, sd=sd1)
		f2 <- stats::dnorm(x, mean=mu2, sd=sd2)
		# return min
		return( pmin(f1, f2) )
	}
	
	### this works in general
	lower.bound = min(mu1-sd1*10, mu2-sd2*10)	
	upper.bound = min(mu1+sd1*10, mu2+sd2*10)
	ret=stats::integrate(min.f1f2, lower.bound, upper.bound, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2, subdivisions=1000)
	
	return(ret$value)
}



