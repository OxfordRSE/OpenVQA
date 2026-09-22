######################################
# General functions
######################################

# Force strictly local scope for functions
strict <- function(f, pos=2) eval(substitute(f), as.environment(pos))

# Dummy function used to detect if already loaded. Do not delete!
functions.loaded <- function() {}

ei.params.global <- function( ei.name ) {
	##############################
	# Global version of project-specific function
	# in params.PROJECT_CODE.R
	# Returns parameters associated with
	# this indicator
	##############################
	if ( ei.name =='GC' ) {
		distn <- 'Bet'
		ei.name <- 'Ground Cover'
		q.method <- 'empirical'
		has.stratum <- T
		test.tail <- "both"
		bm.val <- NA
	} else if ( ei.name =='PCESS' ) {
		distn <- 'Bet'
		ei.name <- 'Percent Cover Exotic Species'
		q.method <- 'empirical'
		has.stratum <- T
		test.tail <- "upper"
		bm.val <- 0
	} else if ( ei.name =='PCGF' ) {
		distn <- 'Bet'
		ei.name <- 'Percent Cover by Growth Form'
		q.method <- 'empirical'
		has.stratum <- T
		test.tail <- "both"
		bm.val <- NA
	} else if ( ei.name =='PES' ) {
		distn <- 'Bet'
		ei.name <- 'Percent Exotic Species'
		q.method <- 'empirical'
		has.stratum <- F
		test.tail <- "upper"
		bm.val <- NA
	} else if ( ei.name =='SR' ) {
		distn <- 'NBin'
		ei.name <- 'Species Richness'
		q.method <- 'empirical'
		has.stratum <- F
		test.tail <- "both"
		bm.val <- NA
	} else if ( ei.name =='TD' ) {
		distn <- 'gamma'
		ei.name <- 'Taxonomic distance'
		q.method <- 'empirical'
		has.stratum <- F
		test.tail <- "both"
		bm.val <- NA
	} else if ( ei.name =='asc' ) {
		distn <- 'NBin'
		ei.name <- 'Abundance by Size Class'
		q.method <- 'empirical'
		has.stratum <- TRUE
		test.tail <- "both"
		bm.val <- NA
	} else if ( ei.name =='ba' ) {
		distn <- 'gamma'
		ei.name <- 'Basal Area'
		q.method <- 'empirical'
		has.stratum <- FALSE
		test.tail <- "both"
		bm.val <- NA
	} else if ( ei.name =='mccsc' ) {
		distn <- 'NBin'
		ei.name <- 'Mean Cone Count by Size Class'
		q.method <- 'empirical'
		has.stratum <- TRUE
		test.tail <- "lower"
		bm.val <- NA
	} else if ( ei.name =='mpck' ) {
		distn <- 'Bet'
		ei.name <- 'Mean % Canopy Kill'
		q.method <- 'fixed'
		has.stratum <- FALSE
		test.tail <- "upper"
		bm.val <- 0
	} else if ( ei.name =='pbi' ) {
		distn <- 'Bet'
		ei.name <- 'Percent Beetle-Infected Trees'
		q.method <- 'fixed'
		has.stratum <- FALSE
		test.tail <- "upper"
		bm.val <- 0
	} else if ( ei.name =='pbst' ) {
		distn <- 'Bet'
		ei.name <- 'Percent Bark-Stripped Trees'
		q.method <- 'fixed'
		has.stratum <- FALSE
		test.tail <- "upper"
		bm.val <- 0
	} else if ( ei.name =='pcnwbp' ) {
		distn <- 'Bet'
		ei.name <- 'Percent Cover Non-Whitebark Pine'
		q.method <- 'fixed'
		has.stratum <- TRUE
		test.tail <- "upper"
		bm.val <- 0
	} else if ( ei.name =='pinf' ) {
		distn <- 'Bet'
		ei.name <- 'Percent Infected Trees'
		q.method <- 'fixed'
		has.stratum <- FALSE
		test.tail <- "upper"
		bm.val <- 0
	} else if ( ei.name =='sa' ) {
		distn <- 'NBin'
		ei.name <- 'Seedling Abundance'
		q.method <- 'empirical'
		has.stratum <- FALSE
		test.tail <- "lower"
		bm.val <- NA
	} else if ( ei.name =='pcs' ) {
		distn <- 'Bet'
		ei.name <- 'Percent Cover by Stratum'
		q.method <- 'empirical'
		has.stratum <- TRUE
		test.tail <- "both"
		bm.val <- NA
	} else if ( ei.name =='vhud' ) {
		distn <- 'gamma'
		ei.name <- 'Vegetation Height Upper Decile'
		q.method <- 'empirical'
		has.stratum <- FALSE
		test.tail <- "both"
		bm.val <- NA
	} else if ( ei.name =='pcv' ) {
		distn <- 'Bet'
		ei.name <- 'Percent Cover Vegetation'
		q.method <- 'empirical'
		has.stratum <- FALSE
		test.tail <- "both"
		bm.val <- NA
	} else {
		stop("ERROR: unknown EI (function ei.params.global)" )
	}
	
	param.list <- list(distn, ei.name, q.method, has.stratum, test.tail, bm.val)
	names(param.list) <- c("distn", "ei.name", "q.method", "has.stratum", "test.tail", "bm.val")
	
	return(param.list)
	
}

###############################################
###############################################
# Distribution-fitting & quality coefficient functions
###############################################
###############################################

###############################################
# Quality coefficient calcuations for two-sample tests
###############################################

q.linear <- function( x, x1, x0 ) {
	#############################################
	# Converts x to q (quality) along domain ( x0 ... x1 ), where 
	# x0 is the value of x @ q=0, and x1 is the value of x @ q=1.
	# x must fall between x0 and x1
	#
	# This is a simple linear conversion; send to function
	# q.logistic to apply sigmoid scaling
	##############################################
	
	# Error checks
	if (x0 == x1 ) {
		stop("Error in function q.raw: x0 and x1 cannot be equal!")
	} else if ( x0 < x1 ) {
		if ( x < x0 || x > x1 ) stop("Error in function q.raw: x must be between x0 and x1!")
	} else {
		# x0 > x1
		if ( x > x0 || x < x1 ) stop("Error in function q.raw: x must be between x0 and x1!")
	}
	
	# Do the calculations
	diff <- abs( x - x1 )
	maxdiff <- abs( x1 - x0 )
	q <- 1- diff / maxdiff
	
	return(q)
}

q.logistic <- function( q.raw, k=10 ) {
	###################################################
	# Converts linear score q.raw on domain [0,1]  to transformed
	# logistic score on same domain.
	#
	# q.raw MUST be in domain [0,1]
	# k is steepness parameter for the logistic curve. Optimum 
	# setting depend on the domain. Default of k=10 works well
	# over domain [0,1], but feel free to tinker.
	###################################################
	
	# Error checks
	if ( q.raw >1 || q.raw < 0 ) stop("Error in function q.logistic: q.raw must be <=1 & >=0")

	L <- 1				# Upper limit of logistic growth curve
							# By definition=1 for quality coefficient
	x0 <- 0.5			# Midpoint of the curve; by definition=0.5 over domain [0,1]
	
	# The logistic equation
	q <- L / ( 1 + exp( -k * ( q.raw - x0 ) ) )		
	
	return(q)		# Return the quality coefficient
}

q.inverse.logit <- function(q, beta=3) {
	###################################################
	# Similar to q.lostistic but output is anchored on domain [0,1] (i.e., 
	# input-->output: 0-->0 and 1-->1)
	#
	# q MUST be in domain [0,1], beta must be >0
	# Source: 
	# https://stats.stackexchange.com/a/289477/30044
	###################################################
	
	if (is.null(beta)) beta <- 3

	# Error checks
	if ( beta<=0 ) stop("Error in function q.inverse.logit: beta must be >0")
	if ( q >1 || q < 0 ) stop("Error in function q.inverse.logit: q must be <=1 & >=0")

	q <- 1 / ( 1 + ( q / ( 1 - q ) ) ^(beta*-1)   )		
	
	return(q)		# Return the quality coefficient
}

q.scaled.beta <- function(q, s1=0.3, s2=1) {
	###################################################
	# Converts linear score q on domain [0,1]  to transformed score q.tr
	# on same domain using beta function
	#
	# q MUST be in domain [0,1]
	# Default values of s1 and s2 cause q.tr to drop steeply
	# from 1. Feel free to tinker
	###################################################
	
	# Error checks
	if ( q >1 || q < 0 ) stop("Error in function q.scaled.beta: q must be <=1 & >=0")

	q.tr <- qbeta(q, s1, s2)	
	
	return(q.tr)	
}

beta.wt <- function(x, sh1=0.2, sh2=2) {
	######################################
	# Transforms value on [0,1] to new values on [0,1]
	# equal to 1 at limits (0 or 1) and dropping rapidly to zero
	# in middle of range. Use for weighting means-based
	# quality, which should only be used when one or
	# both means are close to the edge of the distribution
	######################################

	if (x>=0.5 && x<=1) {
		x.tr <- x - ( 1 - x )
	} else if ( x<0.5 && x>=0 ) {
		x.tr <- (1-x)-(1-(1-x))
	} else {
		stop("input value outside domain [0,1]!")
	}
	half.wt <- qbeta(x.tr,sh1, sh2)
	
	return(half.wt)
}

u.beta <- function(x, sh1=0.2, sh2=1.25) {
	######################################
	# Transforms value on [0,1] to new values on [0,1]
	# which equals 1 at 0 or 1 and drops rapidly to zero
	# in middle of range. Use for weighting means-based
	# quality, which should only be used when one or
	# both means are close to the edge of the distribution
	######################################

	if (x>=0.5 && x<=1) {
		x.tr <- x - ( 1 - x )
	} else if ( x<0.5 && x>=0 ) {
		x.tr <- (1-x)-(1-(1-x))
	} else {
		stop("input value outside domain [0,1]!")
	}
	u.beta <- qbeta(x.tr,sh1, sh2)
	return(u.beta)
}

u.beta.buffered <- function(x, buffer=null, sh1=0.2, sh2=1.25) {
	######################################
	# Transforms input value x using u.beta function
	# (see above for details), but compressed on either
	# end such that x.tr reaches max value of one at
	# values of x < buffer or > 1-buffer
	######################################

	if (buffer < 0 || buffer>0.1) stop("Value of 'buffer' outside domain of [0,0.1]!")

	if (is.null(buffer)) {
		buffer <- 0.02
	} else if (buffer < 0 || buffer>0.5) {
		stop("Value of 'buffer' outside domain of [0,0.5]!")
	}
	lowlim <- buffer
	uplim <- 1 - lowlim
	
	if ( x >= 0 && x < lowlim ) {
		x.tr <- 0
	} else if ( x >= lowlim && x <= uplim ) {
		x.tr <- 0.5 + ( ( ( x - 0.5 ) / ( uplim - 0.5 ) ) * 0.5 )
		
		# Fix rare cases where transformation goes 
		# slightly out of bounds
		if ( x.tr < 0 ) {
			x.tr <- 0
		} else if (x.tr > 1 ) {
			x.tr <- 1
		}
	} else if ( x > uplim && x <=1 ) {
		x.tr <- 1
	} else {
		stop("input value outside domain [0,1]!")
	}
	
	u.beta.buffered <- u.beta(x.tr, sh1=sh1, sh2=sh2)
	return(u.beta.buffered)
}

qc.calc <- function(x, x.min, x.max, lcl, ucl, tail, k) {
	###################################################
	# Converts value x into a score between 0 and 1 using logistic function. 
	# Value x must fall between x.min and x.max.
	# If tail='lower', qc drops from 1 at x.max to (near) zero at x.min
	# If tail='upper', qc drops from 1 at x.min to (near) zero at x.max
	# k is the steepness function for the logistic curve. The optimum 
	# setting depend on the domain. Until I can figure out how to 
	# determine this algorithmically, this will have to be tinkered
	# with and passed as a parameter
	###################################################
	
	y <- NA			# The quality coefficient
	L <- 1				# Upper limit of logistic growth curve
							# By definition=1 for quality coefficient

	if ( tail=='upper' | ( tail=='both' & x > ucl ) ) {
		# Calculate score based on upper tail
		if ( x > x.max ) x == x.max		# Shouldn't be necessary; just in case
		x0 <- ( x.max - ucl ) / 2				# Midpoint of the curve
		x.diff <- x - ucl								# x distance from ucl
		y <- L / ( 1 + exp( -k * ( x.diff - x0 ) ) )		# The logistic equation
		y <- 1 - y										# So curve increases toward ucl
		
	} else if ( tail=='lower' | ( tail=='both' & x < lcl ) ) {
		# Calculate score based on lower tail
		x0 <- ( lcl - x.min ) / 2	# Midpoint of the curve
		
		y <- L / ( 1 + exp( -k * ( x - x0 ) ) )		# The logistic equation
	}
	
	return(y)		# Return the quality coefficient
}

qc.calc.simple<- function(x, x.min, x.max, k, transform=T) {
	###################################################
	# Converts value x into a score between 0 and 1 using logistic function. 
	# Value x must fall between x.min and x.max.
	# k is the steepness function for the logistic curve. The optimum 
	# setting depend on the domain. For x=(0:1), k=10 works well
	# Optional parameter transform=F does simple linear transform
	###################################################

	if (!is.na(x))	{
			
		if ( x > x.max | x < x.min ) {
			y <- NA		
		} else if (transform==F) {
			# Simple linear transformation
			# Convert scale y linearly with x
			 y <- ( x - x.min ) / ( x.max - x.min ) 
		} else if (x==x.min) {
			y <- 0
		} else if (x==x.max) {
			y <- 1
		} else  {
			# Logistic transformation
			
			# x >= x.min & x <= x.max
			L <- 1				# Upper limit of logistic growth curve
									# By definition=1 for quality coefficient
		
			# Calculate score based on lower tail
			x0 <- ( x.max - x.min ) / 2	# Midpoint of the curve
			
			y <- L / ( 1 + exp( -k * ( x - x0 )  )  )		# The logistic equation
		}
		
		return(y)		# Return the quality coefficient
		
	}
}

qc.calc.simple.notransform <- function(x, x.min, x.max, k) {
	###################################################
	# No conversion!
	###################################################
	
	if (!is.na(x))	{
			
		if ( x > x.max | x < x.min ) {
			y <- NA
		} else  {
			Y <- x
		}
		
		return(y)		# Return the quality coefficient
		}
}

qc.calc.simple.demo <- function(x.min, x.max, k) {
	###################################################
	# Useful for visualizing the effect of different ranges 
	# of x and the steepness function k 
	###################################################

	n.reps <- 1000
	x.vals <- seq(x.min, x.max, length.out= n.reps)
	y.vals <- rep(NA, n.reps)
	
	for (i in 1:length(x.vals) ) {
		y.vals[i] <- qc.calc.simple(x.vals[i], x.min=0, x.max=1, k=k)
	}
	main.txt <- paste( "Logistic transformation, x range = ", 
		x.min, " to ", x.max, ", k=", k, sep="")
		
	plot(y.vals ~ x.vals, 
		 type="l",
		xlab='Original value (x)', ylab="Transformed value (Quality)", 
		main= main.txt
	)
}	

###############################################
# Bootstrap overlap functions
###############################################

boot.overlap <- function(distn, v1, v2, xs=NULL, boot.reps=NULL, seed=NULL, set.seed=F, ...) {
	###################################################
	# Calculates overlap (ol) & bootstrapped 95% CLs between two
	# probability distributions, as fit to two input vectors
	#
	# Parameters:
	# 		1. distn				Distribution family ('NBin','gamma','Bet')
	# 		2. v1					Vector 1
	# 		3. v2					Vector 2
	#		4. xs					Vector of x values for generating predicted probabilities
	#									[Default=0 -> highest value of v1, v2, plus 10]
	#		5. boot.reps		Number of bootstap replicates [Default=1000]
	#		6. seed				Randomization seed for fixed results [Default=no seed]
	#		7. Standard optional arguments specific to fitdistr():
	#			(a) nbin: no additional arguments needed
	# 			(b) beta: shape1 & shape2 (starting parameters ), lowlim (lower limit)
	#			(c) gamma: shape & rate (starting parameters ), lowlim (lower limit)
	#
	# Returns list object:
	#		1. Observed ol
	#		2. Lower and upper 95% CLs for observed ol
	#		3. Saved x values (xs)
	#		4. Fitted pdf1
	#		5. Fitted pdf2
	#		6. Bootrapped prob density function values for vector1
	#		7. Bootrapped prob density function values for vector2
	#		8. Bootstrapped ol values
	#
	###################################################
	
	# Set default values for optional parameters & check for errors
	if (is.null(xs)) 	{
		xs <- seq(0,max(max(f), max(b)) + 10)
	}
	
	if (is.null(boot.reps)) 	{
		boot.reps <- 100
	}	else {
		if (! ( boot.reps ==round(boot.reps) ) ) {
			stop("ERROR: Parameter 'boot.reps' must be an integer!")
		}
	}
	
	if (set.seed==T) {
		if ( !(is.null(seed)) )	{
			if (! ( seed==round(seed) ) ) stop("ERROR: Parameter 'seed' must be an integer!")
			set.seed(seed)
		}		
	}	
	
	# Compose fitdistr optional arguments
	opt.args <- ""
	if ( distn=='Bet' ) {
		# Distribution fitting parameters
		opt.args <- paste0( ", lower = c(", lowlim, ", ",  lowlim, "), start=list(shape1=", shape1, ", shape2=", shape2, ")" )
		densfun <- "beta"
		
		# Predicted pdf parameters
		predfun <- "dbeta"
		predpar1 <- "shape1"
		predpar2 <- "shape2"
		
	} else if ( distn=='gamma' ) {
		# Distribution fitting parameters
		densfun <- "gamma"

		# Predicted pdf parameters
		predfun <- "dgamma"
		predpar1 <- "shape"
		predpar2 <- "rate"	

		# Optional arguments, including lower and upper bounds
		opt.args <- paste0(", ",
			"lower = c(", lowlim, ", ",  lowlim, "), ",
			"upper = c(", uplim, ", ",  uplim, "), ",
			"start=list(shape=", shape, ", ",
			"rate=", rate, ")"
		)
	} else if ( distn=='NBin' ) {
		# Distribution fitting parameters
		densfun <- "negative binomial"

		# Predicted pdf parameters
		predfun <- "dnbinom"
		predpar1 <- "size"
		predpar2 <- "mu"
		
		# Optional arguments, plus alternative lower bounds
		# if unbounded version fails
		opt.args <- ""
		opt.args.nbin.lb1 <- ", lower=c(1)"	# alt lower bound=1
		opt.args.nbin.lb0 <- ", lower=c(0.1)"	# alt lower bound~0
	} else {
		stop( paste0( "ERROR: distribution '", distn, "' not recognized"))
	}
	
	# Construct the final commands
	fit.v1 <- paste0("fitdistr(v1, densfun='", densfun, "'", opt.args, ")")
	fit.v2 <- paste0("fitdistr(v2, densfun='", densfun, "'", opt.args, ")")
	
	pdf <- paste0(predfun, "(xs, ", predpar1, "= mod$estimate[1], ", predpar2, "= mod$estimate[2])")

	fit.boot <- paste0("fitdistr(boot.samp, densfun='", densfun, "'", opt.args, ")")
	
	################################
	# Transformations
	################################
	
	if (distn=='gamma') {
		# Set zeros to very small value > 1 to avoid crash
		v1[ v1==0 ] <- 0.0000001
		v2[v2==0 ] <- 0.0000001
	}
	
	##############################
	# Fit actual pdfs and get pdfs
	# Note use of eval(parse...) to force 
	# evaluation string as a command
	##############################
	
	# Focal pdf
	mod <- tryCatch(	 eval(parse(text= fit.v1)),
					error = function(cond) { 	return("FAIL") },
					warning = function(cond) {return("FAIL") }
				)
	# Fix for special case of NBin only
	# Adds explicit lower bound to avoid negative value errors
	if ( mod[1]=='FAIL' && distn=='NBin' )	{
		
		if ( min(unique(b))>=1 ) {
			fit.v1 <- paste0("fitdistr(v1, densfun='", densfun, "'", opt.args.nbin.lb1, ")")
		} else {	
			fit.v1 <- paste0("fitdistr(v1, densfun='", densfun, "'", opt.args.nbin.lb0, ")")				}
		
		# Try again
		mod <- tryCatch( eval(parse(text= fit.v1)),
			error = function(cond) { return("FAIL") },
			warning = function(cond) { return("FAIL") }
		)
	}	

	if ( mod[1]=='FAIL' ) return( list( 'FAIL' ) )
	pdf1 <- eval(parse(text= pdf))
	pdf1[pdf1==Inf] <- NA
	
	# Get model parameters for alternative overlap method
	# if this is beta dist
	if ( distn=='Bet' ) {	
		mod1.shp1 <- mod$estimate[[1]]
		mod1.shp2 <- mod$estimate[[2]]
	}
	
	# Benchmark pdf
	mod <- tryCatch(	 eval(parse(text= fit.v2)),
					error = function(cond) { 	return("FAIL") },
					warning = function(cond) {return("FAIL") }
				)
	# Fix for special case of NBin only
	# Adds explicit lower bound to avoid negative value errors
	if ( mod[1]=='FAIL' && distn=='NBin' )	{
		
		if ( min(unique(b))>=1 ) {
			fit.v2 <- paste0("fitdistr(v2, densfun='", densfun, "'", opt.args.nbin.lb1, ")")
		} else {	
			fit.v2 <- paste0("fitdistr(2, densfun='", densfun, "'", opt.args.nbin.lb0, ")")			}
			
		# Try again
		mod <- tryCatch( eval(parse(text= fit.v2)),
			error = function(cond) { return("FAIL") },
			warning = function(cond) { return("FAIL") }
		)
	}	

	if ( mod[1]=='FAIL' ) return( list( 'FAIL' ) )
	pdf2 <- eval(parse(text= pdf))
	pdf2[pdf2==Inf] <- NA

	# Get model parameters for alternative overlap method
	# if this is beta dist
	if ( distn=='Bet' ) {	
		mod2.shp1 <- mod$estimate[[1]]
		mod2.shp2 <- mod$estimate[[2]]
	}

	# Calculate overlap between actual pdfs
	#ol <- sum( pmin( pdf1, pdf2 ) * mean( diff( xs ) ), na.rm=T )
	if ( distn=='Bet' ) {
		ol <- prop.ol.beta( xs=xs, 
					s1=mod1.shp1, r1= mod1.shp2, 
					s2= mod2.shp1, r2= mod2.shp2
				)
		ol <- ol[[1]]
	} else {
		ol <- sum( pmin( pdf1, pdf2 ) * mean( diff( xs ) ), na.rm=T )
	}
	
	# UNDER CONSTRUCTION!
	# NEED TO FIGURE OUT WAY TO DO BETA OVERLAP FOR
	# BOOTSTRAPS !!!!!!!!!!
	
	boot.pdf1 <- sapply(1: boot.reps, function(i) {
		boot.samp <- sample(v1, length(v1), replace =T )
		mod <- tryCatch(	 eval(parse(text= fit.boot)),
					error = function(cond) { 	return("FAIL") },
					warning = function(cond) {return("FAIL") }
				)
		# Fix for special case of NBin only
		# Adds explicit lower bound to avoid negative value errors
		if ( mod[1]=='FAIL' && distn=='NBin' ) {	
			
			if ( min(unique(v1) )>=1 ) {
				fit.boot <- paste0("fitdistr(boot.samp, densfun='", 
					densfun, "'", opt.args.nbin.lb1, ")")
			} else {	
				fit.boot <- paste0("fitdistr(boot.samp, densfun='", 
					densfun, "'", opt.args.nbin.lb0, ")")
			}
			
			# Try again
			mod <- tryCatch( eval(parse(text= fit.boot)),
				error = function(cond) { return("FAIL") },
				warning = function(cond) { return("FAIL") }
			)
		}
			
		if ( mod[1]=='FAIL'  ) {
			rep(NA,  length(xs))
		} else {
			eval( parse( text= pdf ) )
		}
	} )
	boot.pdf1[boot.pdf1 ==Inf] <- NA
	
	boot.pdf2 <- sapply(1: boot.reps, function(i) {
		boot.samp <- sample(v2, length(v2), replace =T )
		# mod <- suppressWarnings(eval(parse(text = fit.boot ) ) ) 
		# eval( parse( text= pdf ) )		
		mod <- tryCatch(	 eval(parse(text= fit.boot)),
					error = function(cond) { 	return("FAIL") },
					warning = function(cond) {return("FAIL") }
				)

		# Fix for special case of NBin only
		# Adds explicit lower bound to avoid negative value errors
		if ( mod[1]=='FAIL' && distn=='NBin' ) {	
			
			if ( min(unique(v2))>=1 ) {
				fit.boot <- paste0("fitdistr(boot.samp, densfun='", 
					densfun, "'", opt.args.nbin.lb1, ")")
			} else {	
				fit.boot <- paste0("fitdistr(boot.samp, densfun='", 
					densfun, "'", opt.args.nbin.lb0, ")")
			}
			
			# Try again
			mod <- tryCatch( eval(parse(text= fit.boot)),
				error = function(cond) { return("FAIL") },
				warning = function(cond) { return("FAIL") }
			)
		}	

		if ( mod[1]=='FAIL'  ) {
			rep(NA,  length(xs))
		} else {
			eval( parse( text= pdf ) )
		}		
	} )
	boot.pdf2[boot.pdf2 ==Inf] <- NA
	
	# Create vector of bootstrapped overlap values
	boot.ol <- rep(NA, boot.reps)
	for (i in 1: boot.reps) {
		boot.ol[i] <- sum( pmin( boot.pdf1[ ,i], boot.pdf2[ ,i] ) * mean( diff(xs) ), na.rm=T )
	}
	
	# sapply method. SLOWER!
	# boot.ol <- sapply(1:boot.reps, function(i) {
		# sum(pmin(boot.pdf1[ ,i], boot.pdf2[ ,i]))*mean(diff(xs))	
	# } )
	
	# Get 95% CLs of the overlaps of the bootstrapped distributions 
	# using empirical bootstrap method. Uses bootstrap deviations from
	# bootstrap mean, and adds/subtracts these from observed value
	boot.ol.mean <- mean(boot.ol, na.rm=T)
	boot.ol.dev <- boot.ol - boot.ol.mean			# Deviations from bootstrap mean
	ol.dev <- ol + boot.ol.dev
	ol.sd <- sd(ol.dev)												# Bootstrap SD
	ol.cls <- quantile(ol.dev, c(0.025, 0.975), na.rm=T)
	ol.cls
	ol.lcl <- ol.cls[[1]]
	ol.ucl <- ol.cls[[2]]
	
	# Assemble list object & return
	ol.list <- list(ol, ol.cls, ol.lcl, ol.ucl,
		xs, pdf1, pdf2, boot.pdf1, boot.pdf2, boot.ol, ol.sd)
	names(ol.list) <- c('ol.obs', 'ol.cls', 'ol.lcl', 'ol.ucl',
		'xs', 'pdf1', 'pdf2', 
		'boot.pdf1', 'boot.pdf2', 'boot.ol', 'ol.sd')
	return(ol.list)
	
}

q.overlap <- function(pdf.f, pdf.b, xs, b.mean, test.tail=NULL) {
	###################################################
	# Returns quality of overlap of focal and benchmark pdfs, with correction for 
	# one-tailed comparisons. If test.tail="both", q.overlap=overlap.
	# If test.tail="upper", q.overlap is 1 - area of pdf.f>pdf.b, over x>mean(b). 
	# If test.tail="lower", q.overlap is 1 - area of pdf.f> pdf.b, over x<b.mean
	#
	# NOT FOR BETA DISTRIBUTION
	#
	# Parameters:
	# 		pdf.f			focal pdf vector
	#		pdf.b		benchmark pdf vector
	#		xs				vector of x values of the pdfs
	#		b.mean	Mean bm value
	#		test.tail	Tails to be compared: "both", "upper", "lower"
	#
	# Returns: area of overlap  or f>b, as described above
	#
	# Requirements: pdf.f, pdf.b, and xs must be vectors of equal length,
	#		with identical indexes, such that pdf.f[i] represents the value of 
	#		pdf.f at xs[i], and same for pdf.b
	###################################################

	# get default value of test tail
	if (is.null(test.tail)) test.tail <- "both"
	
	# Calculate overlap between actual pdfs
	if (test.tail=="upper") {
		upper <- which( xs>b.mean ) 
		xs.up <- xs[ upper ]
		pdf.b.up <- pdf.b[ upper ]
		pdf.f.up <- pdf.f[ upper ]
		pdf.diff <- pdf.f.up - pdf.b.up
		pdf.diff[pdf.diff<0] <- 0
		area.diff <- sum( pdf.diff * mean( diff( xs.up ) ), na.rm=T )
		q.overlap <- 1- area.diff
	} else if (test.tail=="lower") {
		lower <- which( xs<b.mean ) 
		xs.low <- xs[ lower ]
		pdf.b.low <- pdf.b[ lower ]
		pdf.f.low <- pdf.f[ lower ]
		pdf.diff <- pdf.f.low - pdf.b.low
		pdf.diff[pdf.diff<0] <- 0
		area.diff <- sum( pdf.diff * mean( diff( xs.low ) ), na.rm=T )
		q.overlap <- 1- area.diff
	} else {
		# test.tail=="both"
		q.overlap <- sum( pmin( pdf.f, pdf.b ) * mean( diff( xs ) ), na.rm=T )
	}
	
	return(q.overlap)

}

boot.overlap.diff <- function(distn, test.tail=NULL, f, b, xs=NULL, 
	q.tr.logit.inverse=TRUE, logit.inverse.beta=2,
	boot.reps=NULL, seed=NULL, set.seed=F, ...) {
	###################################################
	# Calculates overlap (ol) & bootstrapped 95% CLs between two
	# probability distributions, as fit to two input vectors. Includes correction
	# for one-tailed comparisions.
	# 
	# NOT FOR BETA DISTRIBUTION
	#
	# Parameters:
	# 		1. distn				Distribution family ('NBin','gamma','Bet')
	# 		2. f					focal vector
	# 		3. b				benchmark vector
	#		4. xs					Vector of x values for generating predicted probabilities
	#									[Default=0 -> highest value of f, b, plus 10]
	#		5. boot.reps		Number of bootstap replicates [Default=1000]
	#		6. seed				Randomization seed for fixed results [Default=no seed]
	#		7. Standard optional arguments specific to fitdistr():
	#			(a) nbin: no additional arguments needed
	# 			(b) beta: shape1 & shape2 (starting parameters ), lowlim (lower limit)
	#			(c) gamma: shape & rate (starting parameters ), lowlim (lower limit)
	#
	# Returns list object:
	#		1. Observed ol
	#		2. Lower and upper 95% CLs for observed ol
	#		3. Saved x values (xs)
	#		4. pdf.f: Fitted focal pdf 
	#		5. pdf.b: Fitted benchmark pdf
	#		6. Bootrapped prob density function values for pdf.f
	#		7. Bootrapped prob density function values for pdf.b
	#		8. Bootstrapped ol values
	#
	#  Optional arguments (...):
	# 		lowlim
	# 		uplim
	# 		rate
	# 		shape
	###################################################
	
	# Set default values for optional parameters & check for errors
	# Set default values for optional parameters & check for errors
	if (is.null(test.tail)) test.tail <- "both"
	if (is.null(xs)) xs <- seq(0,1, length.out= 100)
	if (is.null(boot.reps)) 	boot.reps <- 1000
	if (is.null(perm.reps)) 	perm.reps <- 10000
	if ( (set.seed==T) && !(is.null(seed)) ) set.seed(seed)
	
	# Compose fitdistr optional arguments
	opt.args <- ""
	if ( distn=='Bet' ) {
		stop( paste0( "ERROR: wrong function for beta distribution!"))
	} else if ( distn=='gamma' ) {
		densfun <- "gamma"

		# Predicted pdf parameters
		predfun <- "dgamma"
		predpar1 <- "shape"
		predpar2 <- "rate"	

		# Upper and lower bounds, rate, shape
		opt.args <- paste0(", ",
			"lower = c(", lowlim, ", ",  lowlim, "), ",
			"upper = c(", uplim, ", ",  uplim, "), ",
			"start=list(shape=", shape, ", rate=", rate, ")" 
		)
		# Alternate args if fails: lower bounds, rate, shape, but no upper bounds
		opt.args.no.ub <- paste0(", ",
			"lower = c(", lowlim, ", ",  lowlim, "), ",
			"start=list(shape=", shape, ", rate=", rate, ")" 
		)
	} else if ( distn=='NBin' ) {
		densfun <- "negative binomial"

		# Predicted pdf parameters
		predfun <- "dnbinom"
		predpar1 <- "size"
		predpar2 <- "mu"
		
		opt.args <- ""
		
		# Alternative lower bounds
		# Use if unbounded version fails
		opt.args.nbin.lb1 <- ", lower=c(1)"	# alt lower bound=1
		opt.args.nbin.lb0 <- ", lower=c(0.1)"	# alt lower bound~0
		
	} else {
		stop( paste0( "ERROR: distribution '", distn, "' not recognized"))
	}
	
	# Construct the final commands
	fit.f <- paste0("fitdistr(f, densfun='", densfun, "'", opt.args, ")")
	fit.b <- paste0("fitdistr(b, densfun='", densfun, "'", opt.args, ")")
	
	pdf <- paste0(predfun, "(xs, ", predpar1, "= mod$estimate[1], ", predpar2, "= mod$estimate[2])")

	fit.boot <- paste0("fitdistr(boot.samp, densfun='", densfun, "'", opt.args, ")")
	
	# Get benchmark mean, used for one-tail overlap
	b.mean <- mean(b, na.rm=T)

	################################
	# Transformations
	################################
	
	if (distn=='gamma') {
		# Set zeros to very small value > 1 to avoid crash
		f[ f==0 ] <- 0.0000001
		b[b==0 ] <- 0.0000001
	}
	
	##############################
	# Fit actual pdfs and get pdfs
	# Note use of eval(parse...) to force 
	# evaluation string as a command
	##############################
	
	# Focal pdf
	mod <- tryCatch( eval(parse(text= fit.f)),
					error = function(cond) { return("FAIL") },
					warning = function(cond) {return("FAIL") }
				)
	if ( mod[1]=='FAIL' && distn=='NBin' )	{
		# Fix for special case of NBin only
		# Adds explicit lower bound to avoid negative value errors
		
		if ( min(unique(f))>=1 ) {
			fit.f <- paste0("fitdistr(f, densfun='", densfun, "'", opt.args.nbin.lb1, ")")
		} else {	
			fit.f <- paste0("fitdistr(f, densfun='", densfun, "'", opt.args.nbin.lb0, ")")
		}
		
		# Try again
		mod <- tryCatch( eval(parse(text= fit.f)),
			error = function(cond) { return("FAIL") },
			warning = function(cond) { return("FAIL") }
		)
	}	else if ( mod[1]=='FAIL' && distn=='gamma' )	{
		# Fix for gamma: remove upper bounds
		fit.f <- paste0("fitdistr(f, densfun='", densfun, "'", opt.args.no.ub, ")")
		
		cat("    [fitdistr(f) failed, repeating without upper bounds...")
		
		# Try again
		mod <- tryCatch( eval(parse(text= fit.f)),
			error = function(cond) { return("FAIL") },
			warning = function(cond) { return("FAIL") }
		)
		
		if ( mod[1]=='FAIL' ) {
		  # Uncomment to echo details
		  cat("failed]\n")
		  cat("Failed vectors:\n")
		  cat( "f:\n")
		  print(f)
		  cat( "b:\n")
		  print(b)
		  
		} else {
		  cat("success]\n")
		}
	}	

	if ( mod[1]=='FAIL' ) return( list( 'FAIL' ) )
	pdf.f <- eval(parse(text= pdf))
	pdf.f[pdf.f==Inf] <- NA
		
	# Benchmark pdf
	mod <- tryCatch( eval(parse(text= fit.b)),
					error = function(cond) { return("FAIL") },
					warning = function(cond) { return("FAIL") }
				)

	if ( mod[1]=='FAIL' && distn=='NBin' )	{
		
		if ( min(unique(b))>=1 ) {
			fit.b <- paste0("fitdistr(b, densfun='", densfun, "'", opt.args.nbin.lb1, ")")
		} else {	
			fit.b <- paste0("fitdistr(b, densfun='", densfun, "'", opt.args.nbin.lb0, ")")			
		}
			
		# Try again
		mod <- tryCatch( eval(parse(text= fit.b)),
			error = function(cond) { return("FAIL") },
			warning = function(cond) { return("FAIL") }
		)
	}	else if ( mod[1]=='FAIL' && distn=='gamma' )	{
		fit.b <- paste0("fitdistr(b, densfun='", densfun, "'", opt.args.no.ub, ")")
		
		cat("b failed (gamma): Using opt.args.no.ub\n")
		
		
		# Try again
		mod <- tryCatch( eval(parse(text= fit.b)),
			error = function(cond) { return("FAIL") },
			warning = function(cond) { return("FAIL") }
		)
	}	

	if ( mod[1]=='FAIL' ) return( list( 'FAIL' ) )
	pdf.b <- eval(parse(text= pdf))
	pdf.b[pdf.b==Inf] <- NA

	# Calculate overlap between actual pdfs	
	ol <- q.overlap(pdf.f=pdf.f, pdf.b=pdf.b, xs=xs, b.mean=b.mean, test.tail=test.tail)
	q <- ol
	
	# Transform indicator quality if required (see global params file
	q.raw <- q		# Save original value, even if don't change
	if ( q.tr.logit.inverse==TRUE ) {
		q <- q.inverse.logit(q, logit.inverse.beta)
	}
	
	boot.pdf.f <- sapply(1: boot.reps, function(i) {
		boot.samp <- sample(f, length(f), replace =T )
		mod <- tryCatch( eval(parse(text= fit.boot)),
					error = function(cond) { return("FAIL") },
					warning = function(cond) { return("FAIL") }
				)
		# Fix for special case of NBin only
		# Adds explicit lower bound to avoid negative value errors
		if ( mod[1]=='FAIL' && distn=='NBin' ) {	
			
			if ( min(unique(f) )>=1 ) {
				fit.boot <- paste0("fitdistr(boot.samp, densfun='", 
					densfun, "'", opt.args.nbin.lb1, ")")
			} else {	
				fit.boot <- paste0("fitdistr(boot.samp, densfun='", 
					densfun, "'", opt.args.nbin.lb0, ")")
			}
			
			# Try again
			mod <- tryCatch( eval(parse(text= fit.boot)),
				error = function(cond) { return("FAIL") },
				warning = function(cond) { return("FAIL") }
			)
		}
			
		if ( mod[1]=='FAIL'  ) {
			rep(NA,  length(xs))
		} else {
			eval( parse( text= pdf ) )
		}
	} )
	boot.pdf.f[boot.pdf.f ==Inf] <- NA
	
	boot.pdf.b <- sapply(1: boot.reps, function(i) {
		boot.samp <- sample(b, length(b), replace =T )
		mod <- tryCatch( eval(parse(text= fit.boot)),
					error = function(cond) { return("FAIL") },
					warning = function(cond) { return("FAIL") }
				)
		# Fix for special case of NBin only
		# Adds explicit lower bound to avoid negative value errors
		if ( mod[1]=='FAIL' && distn=='NBin' ) {	
			
			if ( min(unique(b))>=1 ) {
				fit.boot <- paste0("fitdistr(boot.samp, densfun='", 
					densfun, "'", opt.args.nbin.lb1, ")")
			} else {	
				fit.boot <- paste0("fitdistr(boot.samp, densfun='", 
					densfun, "'", opt.args.nbin.lb0, ")")
			}
			
			# Try again
			mod <- tryCatch( eval(parse(text= fit.boot)),
				error = function(cond) { return("FAIL") },
				warning = function(cond) { return("FAIL") }
			)
		}	
		if ( mod[1]=='FAIL'  ) {
			# Length of failed vector must match length of successful
			# vector, which is xs +1 (pdf mean plus the x values, see below)
			rep(NA,  length(xs) + 1)
		} else {
			# Get expected values (mean of x) for pdf
			if (distn=="NBin") {
				curr.b.mean <- mod$estimate[[2]]		
			} else if (distn=="gamma") {
				curr.b.mean <- mod$estimate[[1]] / mod$estimate[[2]]	
			} else {
				stop(paste0("ERROR in function boot.overlap.diff: Not supported for distribution ", distn))
			}
			curr.boot.pdf <- eval( parse( text= pdf ) )
			curr.boot.pdf[curr.boot.pdf==Inf]<-NA
			# append b.mean to front of bootstrap vector
			curr.boot.pdf <- as.vector( c(curr.b.mean, curr.boot.pdf)	)	
			curr.boot.pdf
		}		
	} )
	boot.pdf.b[boot.pdf.b==Inf] <- NA
	
	# Create vector of bootstrapped overlap values
	boot.ol <- rep(NA, boot.reps)
	for (i in 1: boot.reps) {
		curr.pdf.f <- boot.pdf.f[ , i]
		curr.b.mean <- boot.pdf.b[ , i][1]											# first element=mean
		curr.pdf.b <- boot.pdf.b[ , i ][ 2-length(boot.pdf.b[ , i]) ]	# remaining elements=the pdf
		boot.ol[i] <- q.overlap(pdf.f=curr.pdf.f, pdf.b=curr.pdf.b, xs=xs, b.mean=curr.b.mean, test.tail= test.tail)
	}
	boot.q <- boot.ol
	
	# Transform bootstrapped quality (overlap) if requested
	if ( q.tr.logit.inverse==TRUE ) {
		boot.q <- q.inverse.logit(boot.q, logit.inverse.beta)
	}
	
	# Get 95% CLs of the overlaps of the bootstrapped distributions 
	# using empirical bootstrap method. Uses bootstrap deviations from
	# bootstrap mean, and adds/subtracts these from observed value
	boot.q.mean <- mean(boot.q, na.rm=T)
	boot.q.dev <- boot.q - boot.q.mean			# Deviations from bootstrap mean
	q.dev <- q + boot.q.dev
	q.sd <- sd(q.dev)											# Bootstrap SD
	q.cls <- quantile(q.dev, c(0.025, 0.975), na.rm=T)
	q.cls
	q.lcl <- q.cls[[1]]
	q.ucl <- q.cls[[2]]
	
	# Assemble list object & return
	ol.list <- list(ol, q.raw, q, q.cls, q.lcl, q.ucl, q.sd,
		xs, pdf.f, pdf.b, boot.pdf.f, boot.pdf.b, boot.q)
	names(ol.list) <- c('ol', 'q.raw', 'q', 'q.cls', 'q.lcl', 'q.ucl', 'q.sd',
		'xs', 'pdf.f', 'pdf.b', 'boot.pdf.f', 'boot.pdf.b', 'boot.q')
	
	return(ol.list)
	
}

q.overlap.no.cls <- function(distn, test.tail=NULL, f, b, xs=NULL, ...) {
	###################################################
	# Calculates quality of focal vs. benchmark vector, based on the 
	# overlap (ol) between two probability distributions. Includes correction
	# for one-tailed comparisions. NOT YET READY FOR BETA DISTRIBUTION
	#
	# Parameters:
	# 		1. distn				Distribution family ('NBin','gamma','Bet')
	# 		2. f					focal vector
	# 		3. b				benchmark vector
	#		4. xs					Vector of x values for generating predicted probabilities
	#									[Default=0 -> highest value of f, b, plus 10]
	#		5. Standard optional arguments specific to fitdistr():
	#			(a) nbin: no additional arguments needed
	# 			(b) beta: shape1 & shape2 (starting parameters ), lowlim (lower limit)
	#			(c) gamma: shape & rate (starting parameters ), lowlim (lower limit)
	#
	# Returns list object:
	#		1. Observed ol
	#		2. Saved x values (xs)
	#		3. pdf.f: Fitted focal pdf 
	#		4. pdf.b: Fitted benchmark pdf
	###################################################
	
	maxTries <- 10; # Max time to try fit before reporting error

	# Set default values for optional parameters & check for errors
	# Set default values for optional parameters & check for errors
	if (is.null(test.tail)) test.tail <- "both"
	if (is.null(xs)) xs <- seq(0,1, length.out= 100)
	#if (is.null(perm.reps)) 	perm.reps <- 10000
	
	# Compose fitdistr optional arguments
	opt.args <- ""
	if ( distn=='Bet' ) {
		stop( paste0( "ERROR: wrong function for beta distribution!"))
	} else if ( distn=='gamma' ) {
		# Set default gamma parameters
		lowlim <- 0.0000001		
		shape <- 1
		rate <- 1
		
		# Distribution fitting parameters
		opt.args <- paste0( ", lower = c(", lowlim, ", ",  lowlim, "), start=list(shape=", shape, ", rate=", rate, ")" )
		# opt.args <- paste0( ", lower = c(", lowlim, ", ",  lowlim, "), upper = c(", uplim, ", ",  uplim, "),  start=list(shape=", shape, ", rate=", rate, ")" )
		densfun <- "gamma"
		
		# Predicted pdf parameters
		predfun <- "dgamma"
		predpar1 <- "shape"
		predpar2 <- "rate"	
	} else if ( distn=='NBin' ) {
		# Distribution fitting parameters
		densfun <- "negative binomial"

		# Predicted pdf parameters
		predfun <- "dnbinom"
		predpar1 <- "size"
		predpar2 <- "mu"
	} else {
		stop( paste0( "ERROR: distribution '", distn, "' not recognized"))
	}
	
	# Construct the final commands
	fit.f <- paste0("fitdistr(f, densfun='", densfun, "'", opt.args, ")")
	fit.b <- paste0("fitdistr(b, densfun='", densfun, "'", opt.args, ")")
	
	pdf <- paste0(predfun, "(xs, ", predpar1, "= mod$estimate[1], ", predpar2, "= mod$estimate[2])")
	
	# Get benchmark mean, used for one-tail overlap
	b.mean <- mean(b, na.rm=T)
	
	fail <- F	# Assume success to start

	#########################
	# Fit focal pdf
	#########################
	tries <- 0
	done <- F
	while (done==F) {
		tries <- tries + 1
		mod <- tryCatch(	 eval(parse(text= fit.f)),
						error = function(cond) { 	return("FAIL") },
						warning = function(cond) {return("FAIL") }
					)
		if ( mod[1]=='FAIL' ) {
			if ( tries >= maxTries )  {
				fail <- T
				break
			}
		} else {
			pdf.f <- eval(parse(text= pdf))
			pdf.f[pdf.f==Inf] <- NA
			done <- T
		}
	}

	#########################
	# Fit benchmark pdf
	#########################
	tries <- 0
	done <- F
	while (done==F && fail==F) {
		tries <- tries + 1
		mod <- tryCatch(	 eval(parse(text= fit.b)),
						error = function(cond) { 	return("FAIL") },
						warning = function(cond) {return("FAIL") }
					)
		if ( mod[1]=='FAIL' ) {
			if ( tries >= maxTries )  {
				fail <- T
				break
			}
		} else {
			pdf.b <- eval(parse(text= pdf))
			pdf.b[pdf.b ==Inf] <- NA
			done <- T
		}
	}

	if (fail ==T) {
		ol <- NA
		pdf.f <- NA
		pdf.b <- NA
	} else {
		# Calculate overlap between actual pdfs	
		ol <- q.overlap(pdf.f=pdf.f, pdf.b=pdf.b, xs=xs, 
			b.mean=b.mean, test.tail=test.tail)
	}
	
	# Assemble results list & return
	ol.results <- list(ol, pdf.f, pdf.b)
	names(ol.results) <- c('ol.obs', 'pdf.f', 'pdf.b')
	return(ol.results)
	
}


###############################################
# Bootstrap overlap graphs
###############################################
	
boot.ol.plot <- function(ol.list, title.text="", group.text="", f.text, bm.text, subsample=FALSE, plot.boot=TRUE ) {
	###################################################
	# Plots pair of fitted distributions plus 95% bootstrap CLs, optionally
	# adding bootstrap fits, and shaded observed overlap. Overlap plus
	# confidence limits are displayed in legend 
	#
	# Parameters:
	# 		1. ol.list	 (list object, results from bootstrap overlap function)
	#			(a) ol.obs				Observed overlap
	#			(b) ol.cls				Overlap conidence limits
	#			(c) xs						Vector of X values used for pdfs
	#			(d) pdf1					Fitted distribution 1
	#			(e) pdf2					Fitted distribution 2
	#			(c) boot.pdf1		Boostrapped pdf1
	#			(d) boot.pdf2		Boostrapped pdf2
	#			(e) ol.boot			Bootstrapped overlap of the two distributions
	#		2. title.text				Title above graph
	# 		3. group.text			Text identifying color of each group [default assumes 
	#											benchmark is first group: c("Benchmark","Focal")
	#		4. subsample			Subsample bootstrap pdfs for clarity? Ignored if
	#											plot.boot=FALSE
	# 		5. plot.boot				Plot bootstrapped pdfs (default=TRUE)
	###################################################
			
	# Set transparency for overlaps
	# Must be from 0 to 1
	alpha.val <- 0.25
		
	# Group colors
	v1.col <- rgb(0,0,1,alpha.val)
	v2.col <- rgb(1,0,0,alpha.val)
			
	# Set defaults
	n.boot <- length(ol.list$boot.ol)
	if ( subsample && n.boot > 100 ) {
			cols <- 100
	} else {
		cols <- n.boot
	}
	
	# Extract elements
	ol.obs	 <- ol.list$ol.obs
	ol.lcl <- ol.list$ol.cls[[1]]
	ol.ucl <- ol.list$ol.cls[[2]]
	xs <- ol.list$xs
	pdf1 <- ol.list$pdf1
	pdf2 <- ol.list$pdf2
	boot.pdf1 <- ol.list$boot.pdf1
	boot.pdf2 <- ol.list$boot.pdf2
	ol.boot <- ol.list$ol.boot
	
	# Set Inf=NA
	pdf1[pdf1 ==Inf] <- NA
	pdf2[pdf2 ==Inf] <- NA
	boot.pdf1[boot.pdf1==Inf] <- NA
	boot.pdf2[boot.pdf2==Inf] <- NA

	# Get limits
	x.max <- max(max(xs))
	y.max <- max(max(boot.pdf1, na.rm=T), max(boot.pdf2, na.rm=T))
	
	# pdf1
	par(bg="white", las=1, cex=1.2)
	
	# bootstrap pdfs
	plot(xs, boot.pdf1[, 1], type="l", 
		col=rgb(.6, .6, .6, .1), 
		ylim=c(0,y.max),
		xlab="x", ylab="Probability density")
	if ( plot.boot == TRUE ) {
		for(i in 2:cols) lines(xs, boot.pdf1[, i], col=rgb(.6, .6, .6, .1))
	}
	
	# Highlight confidence bands
	if ( plot.boot == TRUE ) {
		quants <- apply(boot.pdf1, 1, quantile, c(0.025, 0.5, 0.975), na.rm=TRUE )
		lines(xs, quants[1, ], col=rgb(0,0,1, alpha.val*2), lwd=1.5, lty=2)
		lines(xs, quants[3, ], col=rgb(0,0,1, alpha.val*2), lwd=1.5, lty=2)
		#lines(xs, quants[2, ], col=rgb(0,0,1, alpha.val*2), lwd=1.5, lty=2)
	}
	
	# add the observed distribution
	lines(xs, pdf1, col=rgb(0,0,1, alpha.val*2), lwd=2.5)
	
	# pdf2
	
	# bootstrap pdfs
	if ( plot.boot == TRUE ) {
		for(i in 2:cols) lines(xs, boot.pdf2[, i], col=rgb(.6, .6, .6, .1))
	}
	
	# Highlight confidence bands
	if ( plot.boot == TRUE ) {
		quants <- apply(boot.pdf2, 1, quantile, c(0.025, 0.5, 0.975), na.rm=TRUE )
		lines(xs, quants[1, ], col=rgb(1,0,0, alpha.val*2), lwd=1.5, lty=2)
		lines(xs, quants[3, ], col=rgb(1,0,0, alpha.val*2), lwd=1.5, lty=2)
		#lines(xs, quants[2, ], col=rgb(1,0,0, alpha.val*2), lwd=1.5, lty=2)
	}
	
	# add the observed distribution
	lines(xs, pdf2, col=rgb(1,0,0, alpha.val*2), lwd=2.5)
	
	# Shade in the mean overlap
	# polygon( x=c(xs,rev(xs)), y=c(rep(0,length(xs)),
		# rev( pmin( pdf1, pdf2 ) ) ),
		# col=rgb(1, 1, 0,0.2)
	# )
	polygon( x=c(xs,rev(xs)), y=c(rep(0,length(xs)),
		rev( pmin( pdf1, pdf2 ) ) ),
		col=rgb(1, 1, 0,0.5)
	)
	
	# Add overlap legend
	ol.txt <- specify_decimal(ol*100,1)
	ol.ucl.txt <- specify_decimal( ol.ucl * 100, 1)
	ol.lcl.txt <- specify_decimal(ol.lcl * 100, 1) 
	legendText <- paste("Overlap: ", ol.txt, "% (", ol.lcl.txt, "-", ol.ucl.txt, ")", sep="")
	x.pos <- x.max-0.55*x.max
	y.pos <- y.max+0.09*y.max
	legend(
		x= x.pos, y=y.pos,		# position
		legend = legendText, 
		title='',							# keep this to maintain position
		cex = 1.0,
		bty = "n"						# border
	) 
	
	# Add focal/benchmark color legend below overlap
	#inset.x <- 0.24*x.max
	inset.x <- 0.04*x.max
	inset.y <- 0.13*y.max
	if ( group.text=="" ) group.text <- c(bm.text, f.text)
	legend(
		x= x.pos+ inset.x, y=y.pos-inset.y,	
		legend = group.text,
		fill= c(rgb(0,0,1,alpha.val), 	rgb(1,0,0, alpha.val)),
		box.lty=0
	)
	
	# Add title above graph
	if (! (title.text=='')) {
		title(main=title.text
		)
	}

}

########################################
# Distribution overlap vector functions
#
# Accept: 
# Vector of x values and sets of shape and rate parameters 
# for two sets of distributions
# 
# Return: 
# Vector of y values outlining the domain overlap between 
# the two distributions over the domain of x
# 
# Based on:
# http://stats.stackexchange.com/questions/12209/percentage-of-overlapping-regions-of-two-normal-distributions    f1 <- dgamma(x, shape=s1, rate=r1 )
########################################

ol.gamma <- function(x, s1, r1, s2, r2) {
	###########################
	# Overlap vector - Gamma distribution
	###########################

    f1 <- dgamma(x, shape=s1, rate=r1 )
    f2 <- dgamma(x, shape=s2, rate=r2 )
    return(pmin(f1, f2))
}

ol.nbin <- function(x, s1, mu1, s2, mu2) {
	###########################
	# Overlap vector - Negative binomial
	# 
	# DOESN'T WORK - USE GAMMA APPROX.
	###########################

    f1 <- dnbinom(x, mu = mu1, size = s1 )
    f2 <- dnbinom(x, mu = mu2, size = s2 )
    return( pmin(f1, f2) )
}

ol.beta <- function(x, s1, r1, s2, r2) {
	###########################
	# Overlap vector - Beta distribution
	###########################

    f1 <- dbeta(x, shape1=s1, shape2=r1 )
    f2 <- dbeta(x, shape1=s2, shape2=r2 )
     return(pmin(f1, f2))
}

###############################################
# Proportional overlap functions
#
# Accept: 
# 		Vector of x values and sets of shape and rate 
# 		parameters for two sets of distributions
#
# Return: 
# 		List of (1) % overlap and (b) absolute error between 
# 		the distributions
#
# Require:
# 		Distribution vector function for the relevant distribution
# 
# Based on:
# http://stats.stackexchange.com/questions/12209/percentage-of-overlapping-regions-of-two-normal-distributions    f1 <- dgamma(x, shape=s1, rate=r1 )
###############################################

prop.ol.gamma <- function ( xs, s1, r1, s2, r2 ) {
	#####################################
	# Proportional overlap - Gamma distribution
	#
	# Requires: function ol.gamma
	#####################################

	# Calculate the area of overlap
	ol <- integrate(ol.gamma, 0, Inf, s1=s1, r1=r1, s2=s2, r2=r2)
	#ol <- integrate(ol.gamma, 0, Inf, s1=s1, s2=s2, r1=r1, r2=r2)
	ol.val <- ol$value
	ol.err <- ol$abs.error
	
	# Return the results as list
	result <- list(ol.val, ol.err)
	return(result)
}

prop.ol.nbin <- function ( xs, s1, mu1, s2, mu2 ) {
	#####################################
	# Proportional overlap - Negative binomial distribution
	#
	# DOESN'T WORK - USE GAMMA APPROX.
	# Requires: function ol.gamma
	#####################################

	# Calculate the area of overlap
	ol <- integrate(ol.nbin, 0, Inf, s1=s1, mu1=mu1, s2=s2, mu2=mu2)
	ol.val <- ol$value
	ol.err <- ol$abs.error
	
	# Return the results as list
	result <- list(ol.val, ol.err)
	return(result)
}

prop.ol.beta <- function ( xs, s1, r1, s2, r2 ) {
	#####################################
	# Proportional overlap - Beta distribution
	#
	# Requires: function ol.beta
	#####################################

	# Calculate the area of overlap
	ol <- integrate(ol.beta, 0, 1, s1=s1, r1=r1, s2=s2, r2=r2)
	ol.val <- ol$value
	ol.err <- ol$abs.error
	
	# Return the result
	result <- list(ol.val, ol.err)
	names(result) <- c("ol", "ol.err")
	return(result)
}

###############################################
# Overlap plotting functions
#
# Accept: 
# 		Vector of x values and sets of shape and rate parameters for two 
# 		sets of distributions
#
# Return: 
#		Nothing; prints the graph directly
#
# Require: 
#		Proportional overlap and overlap vector functions, relevant to
#		the distribution. E.g., ol.gamma() & p.ol.gamma().
# 
# Based on:
# http://stats.stackexchange.com/questions/12209/percentage-of-overlapping-regions-of-two-normal-distributions    f1 <- dgamma(x, shape=s1, rate=r1 )
###############################################

plot.ol.gamma <- function ( x.vals, s1, r1, s2, r2, title.main='', 
	title.x='', trt1.txt='', trt2.txt='' ) {
	#####################################
	# Plotr proportional overlap between two gamma
	# distributions & labels with percent overlap
	#
	# Requires: 
	# 	1. Function ol.gamma()
	# 2. Function p.ol.gamma()
	#
	# Note: can also be used for fitting continuous distribution
	# to negative binomial distributions
	#####################################
	
	# for formatting
	set_decimal <- function(x, k) format(round(x, k), nsmall=k)

	# Assign parameters to vectors or length 2
	shps <- c( s1, s2 )
	rates <- c( r1, r2 )
	
	# Generate the distributions to get axis limits
	y.max <- 0					# Starting values: highest peak value of y
	y.max.min <- 2		# Starting values: lowest peak value of y
	p.thresh <- 0.01		# Lowest probability for plotting right tail threshold
	x.max <- 0					# Starting values: highest value of x at threshold
	
	# Run models without plotting to get limits
	for(i in 1:2) {
		# The distribution
		hx <- dgamma(x.vals, shape= shps[i], rate=rates[i] )
		# Value of x where right tail p drops below right-tail probability threshold
		x.thresh <- qgamma(p.thresh, shape= shps[i], rate=rates[i], lower.tail=F)

		max.hx <- max( hx[ hx < Inf ] )
		if ( max.hx > y.max ) {		# Get maximum y value of the distributions
			y.max <- max.hx
		}
		if ( x.thresh > x.max ) {	# Save parameters of widest distribution
			x.max <- x.thresh
		}
	}
	
	# Set axis limits
	xlim.max <- x.max + ( x.max * 0.1 )
	ylim.max <- y.max + ( y.max * 0.5 )
	
	# Set title
	if (  title.main==''  ) {
		main.txt <- 'Overlap of gamma probability distributions'
	} else {
		main.txt <- title.main
	}
	
	# Set x axis label
	if ( title.x=='' ) {
		x.txt <- 'x values'
	} else {
		x.txt <- title.x
	}
	
	# Dummy probability density function for initializing graph
	initDist <- dgamma(x.vals, shape=1.25, scale=1/0.25) 
	
	# Set up empty plot with correct dimensions and labels
	plot(x.vals, initDist, type="l", yaxs="i", xaxs="i", 
		ylim=c( 0,  ylim.max ), xlim=c( 0,  xlim.max ), 
		xlab=x.txt, ylab="Density", 
		main= main.txt, 
		lwd=0
		)
	
	colors <- c("blue", "orange")
	
	s1.txt <- set_decimal(s1,3); s2.txt <- set_decimal(s2,3)
	r1.txt <- set_decimal(r1,3); r2.txt <- set_decimal(r2,3)
	
	if (trt1.txt=='' & trt2.txt=='') {
		labels <- c(
			paste("alpha=", s1.txt, ", beta=", r1.txt, sep=""),
			paste("alpha=", s2.txt, ", beta=", r2.txt, sep="")
			)
	} else {
		labels <- c(
			paste(trt1.txt, sep=""),
			paste(trt2.txt, sep="")
			)	
		}
	
	# plot the curves
	for( i in 1:2 ) {
			hx <- dgamma(x.vals, shape= shps[i], rate=rates[i] )
			lines(x.vals, hx, lwd=1, col=colors[i])
	}
	legend("topright", inset=.05, 
	labels, lwd=1, col=colors)

	#############################
	# Plot overlap
	#############################
		
	y.vals <- ol.gamma(x.vals, s1=s1, s2=s2, r1=r1, r2=r2)
	x.vals <- c(x.vals, x.vals[1])
	y.vals <- c(y.vals, y.vals[1])
	polygon(x.vals, y.vals, col="gray")
	
	# Calculate the area of overlap
	ol <- prop.ol.gamma( x.vals, s1=s1, r1=r1, s2=s2, r2=r2 )
	ol.val <- ol[[1]]			# overlap
	ol.err <- ol[[2]]			# overlap error
	
	ol.val.txt <- as.character(set_decimal(ol.val*100, 2))
	if (ol.val < 0.01) ol.val.txt <- '<0.01'
	ol.err.txt <- as.character(set_decimal(ol.err * 100, 3))
	if (ol.err < 0.001) ol.err.txt <- '<0.001'
	
	label.txt <- paste('Overlap: ', ol.val.txt, '% \nAbs. error ', ol.err.txt, '%', sep='') 
	
	# Add overlap text to graph
	label.text <- 
	text( labels= label.txt,
		x = 0.05 * xlim.max,
		y = ylim.max - (0.1 * ylim.max),  
		pos=4
		)
}

plot.ol.beta <- function ( x.vals, s1, r1, s2, r2, title.main='', title.x='', trt1.txt='', trt2.txt='' ) {
	#####################################
	# Plot proportional overlap between two beta
	# distributions & labels with percent overlap
	#
	# Requires: 
	# 	1. Function ol.beta()
	# 2. Function p.ol.beta()
	#####################################
	
	# for formatting
	set_decimal <- function(x, k) format(round(x, k), nsmall=k)

	# Assign parameters to vectors or length 2
	shps <- c( s1, s2 )
	shps2 <- c( r1, r2 )
	
	# Generate the distributions to get axis limits
	y.max <- 0					# Starting values: highest peak value of y
	y.max.min <- 2		# Starting values: lowest peak value of y
	p.thresh <- 0.01		# Lowest probability for plotting right tail threshold
	x.max <- 0					# Starting values: highest value of x at threshold
	
	# Run models without plotting to get limits
	for(i in 1:2) {
		# The distribution
		hx <- dbeta(x.vals, shape1= shps[i], shape2 = shps2[i] )
		# Value of x where right tail p drops below right-tail probability threshold
		x.thresh <- qbeta(p.thresh, shape1= shps[i], shape2 = shps2[i], lower.tail=F)

		max.hx <- max( hx[ hx < Inf ] )
		if ( max.hx > y.max ) {		# Get maximum y value of the distributions
			y.max <- max.hx
		}
		if ( x.thresh > x.max ) {	# Save parameters of widest distribution
			x.max <- x.thresh
		}
	}
	
	# Set axis limits
	#xlim.max <- x.max + ( x.max * 0.1 )
	ylim.max <- y.max + ( y.max * 0.5 )
	xlim.max <- 1		# Best for beta
	#ylim.max <- 10 # Use this setting to magnify lower half of graph
												# useful for ZIBet
	
	# Set title
	if (  title.main==''  ) {
		main.txt <- 'Overlap of gamma probability distributions'
	} else {
		main.txt <- title.main
	}
	
	# Set x axis label
	if ( title.x=='' ) {
		x.txt <- 'x values'
	} else {
		x.txt <- title.x
	}
	
	# Dummy probability density function for initializing graph
	initDist <- dbeta(x.vals, shape1=1.25, shape2=1) 
	
	# Set up empty plot with correct dimensions and labels
	plot(x.vals, initDist, type="l", yaxs="i", xaxs="i", 
		ylim=c( 0,  ylim.max ), xlim=c( 0,  xlim.max ), 
		xlab=x.txt, ylab="Density", 
		main= main.txt, 
		lwd=0
		)
	
	colors <- c("blue", "orange")
	
	s1.txt <- set_decimal(s1,3); s2.txt <- set_decimal(s2,3)
	r1.txt <- set_decimal(r1,3); r2.txt <- set_decimal(r2,3)
	
	if (trt1.txt=='' & trt2.txt=='') {
		labels <- c(
			paste("alpha=", s1.txt, ", beta=", r1.txt, sep=""),
			paste("alpha=", s2.txt, ", beta=", r2.txt, sep="")
			)
	} else {
		labels <- c(
			paste(trt1.txt, sep=""),
			paste(trt2.txt, sep="")
			)	
		}
	
	# plot the curves
	for( i in 1:2 ) {
			hx <- dbeta(x.vals, shape1= shps[i], shape2 = shps2[i] )
			lines(x.vals, hx, lwd=1, col=colors[i])
	}
	legend("topright", inset=0.05, 
	labels, lwd=1, col=colors)

	#############################
	# Plot overlap
	#############################
		
	y.vals <- ol.beta(x.vals, s1=s1, r1=r1, s2=s2, r2=r2)
	x.vals <- c(x.vals, 0, x.vals[1])
	y.vals <- c(y.vals, 0, y.vals[1])
	polygon(x.vals, y.vals, col="gray")
	
	# Calculate the area of overlap
	ol <- prop.ol.beta( x.vals, s1=s1, r1=r1, s2=s2, r2=r2 )
	ol.val <- ol[[1]]			# overlap
	ol.err <- ol[[2]]			# overlap error
	
	ol.val.txt <- as.character(set_decimal(ol.val*100, 2))
	if (ol.val < 0.01) ol.val.txt <- '<0.01'
	ol.err.txt <- as.character(set_decimal(ol.err * 100, 3))
	if (ol.err < 0.001) ol.err.txt <- '<0.001'
	
	#label.txt <- paste('Overlap: ', ol.val.txt, '% \nAbs. error ', ol.err.txt, '%', sep='') 
	label.txt <- paste('Overlap: ', ol.val.txt, '%', sep='') 

	# Add overlap text to graph
	label.text <- 
	text( labels= label.txt,
		x = 0.05 * xlim.max,
		y = ylim.max - (0.1 * ylim.max),  
		pos=4
		)
}

plot.ol.nbin <- function ( x.vals, s1, mu1, s2, mu2, title.main='', 
	title.x='', trt1.txt='', trt2.txt='' ) {
	##############################################
	# Calculates & plots estimated proportional overlap between two 
	# neg binomial distributions using gamma approximation 
	#
	# Requires the following functions:
	# 	1. Function ol.gamma()
	# 2. Function p.ol.gamma()
	# 3. Function ol.nbin()
	##############################################
	
	# for formatting
	set_decimal <- function(x, k) format(round(x, k), nsmall=k)

	# Assign parameters to vectors or length 2
	sizes <- c( s1, s1 )
	mus <- c( mu1, mu2 )
	
	# Generate the distributions to get axis limits
	y.max <- 0					# Starting values: highest peak value of y
	y.max.min <- 2		# Starting values: lowest peak value of y
	p.thresh <- 0.01		# Lowest probability for plotting right tail threshold
	x.max <- 0					# Starting values: highest value of x at threshold
	
	# Run models without plotting to get limits
	for(i in 1:2) {
		# The distribution
		hx <- dnbinom(x.vals, mu = mus[i], size = sizes[i] )
		# Value of x where right tail p drops below right-tail probability threshold
		x.thresh <- qnbinom(p.thresh, mu = mus[i], size = sizes[i], lower.tail=F)

		max.hx <- max( hx[ hx < Inf ] )
		if ( max.hx > y.max ) {		# Get maximum y value of the distributions
			y.max <- max.hx
		}
		if ( x.thresh > x.max ) {	# Save parameters of widest distribution
			x.max <- x.thresh
		}
	}
	
	# Set axis limits
	xlim.max <- x.max + ( x.max * 0.1 )
	ylim.max <- y.max + ( y.max * 0.5 )
	
	# Set title
	if (  title.main==''  ) {
		main.txt <- 'Overlap of negative binomial probability distributions'
	} else {
		main.txt <- title.main
	}
	
	# Set x axis label
	if ( title.x=='' ) {
		x.txt <- 'x values'
	} else {
		x.txt <- title.x
	}
	
	# Dummy probability density function for initializing graph
	initDist <- dnbinom(x.vals, mu = 4, size = 10 ) 
	
	# Set up empty plot with correct dimensions and labels
	plot(x.vals, initDist, type="l", yaxs="i", xaxs="i", 
		ylim=c( 0,  ylim.max ), xlim=c( 0,  xlim.max ), 
		xlab=x.txt, ylab="Density", 
		main= main.txt, 
		lwd=0
		)
	
	colors <- c("blue", "orange")
	
	s1.txt <- set_decimal(s1,3); s2.txt <- set_decimal(s2,3)
	mu1.txt <- set_decimal(mu1,3); mu2.txt <- set_decimal(mu2,3)
	
	if (trt1.txt=='' & trt2.txt=='') {
		labels <- c(
			paste("size =", s1.txt, ", mu =", mu1.txt, sep=""),
			paste("size =", s2.txt, ", mu =", mu2.txt, sep="")
			)
	} else {
		labels <- c(
			paste(trt1.txt, sep=""),
			paste(trt2.txt, sep="")
			)	
		}
	
	# plot the curves
	for( i in 1:2 ) {
			hx <- dnbinom(x.vals, mu = mus[i], size = sizes[i] )
			lines(x.vals, hx, lwd=1, col=colors[i])
	}
	legend("topright", inset=.05, 
	labels, lwd=1, col=colors)

	#############################
	# Plot overlap
	#############################
		
	# Plot zone of overlap using actual neg binom dist
	y.vals <- ol.nbin(x.vals, mu1 = mu1, mu2 = mu2, s1 = s1, s2 = s2)
	x.vals2 <- c(x.vals, 0, x.vals[1])
	y.vals2 <- c(y.vals, 0, y.vals[1])
	polygon(x.vals2, y.vals2, col="gray")
	
	# Calculate the area of overlap
	# Gamma is a good approximation
	# Note use of inverse of size parameters
	ol <- prop.ol.gamma( x.vals, s1=s1, r1=1/mu1, s2=s2, r2=1/mu2 )
	ol.val <- ol[[1]]			# overlap
	ol.err <- ol[[2]]			# overlap error
	
	ol.val.txt <- as.character(set_decimal(ol.val*100, 2))
	if (ol.val < 0.01) ol.val.txt <- '<0.01'
	ol.err.txt <- as.character(set_decimal(ol.err * 100, 3))
	if (ol.err < 0.001) ol.err.txt <- '<0.001'
	
	label.txt <- paste('Overlap: ', ol.val.txt, '% \nAbs. error ', ol.err.txt, '%', sep='') 
	
	# Add overlap text to graph
	label.text <- 
	text( labels= label.txt,
		x = 0.05 * xlim.max,
		y = ylim.max - (0.1 * ylim.max),  
		pos=4
		)
}

###############################################
# Distribution-fitting functions
###############################################

best.fit.beta <- function(df) {
	###################################################
	# Select best fitting one-way GLM regression model using Beta family of
	# distributions.
	# 
	# Appropriate for proportional data (continuous, bounded by zero and one).
	# 	Returns simplest model with significant fit. Order of testing:
	# 		Beta (Bet) --> Zero-inflated beta (ZIBet) --> One-inflated Beta (OIBet)
	# 		--> Zero- and one-inflated Beta (ZOIBet)
	# Accepts: data frame which *MUST* have the following columns:
	#		Trt: factor with two values (coding doesn't matter)
	#  	Rsp: Response. Must be integers >=0
	# Returns: List containing:
	#		error: FALSE if function fails, else TRUE
	#		fit.found: TRUE if at least one best model found, otherwise FALSE
	#		dist.best: Abbreviation for one of four models, as shown aboveb
	#		dist.best.text: Spelled out name of the best model
	#		mod.best: the best model (model results, itself a list)
	#		gf.mod.best: p-value of goodness-of-fit test for best-fitting model
	#	If no model passes, returns above values for model Bet
	# Requires: betareg package
	###################################################

	# Some error testing
	min.rsp <- min(df$Rsp)
	max.rsp <- max(df$Rsp)
	
	if (min.rsp < 0) {
		error <- "Error: negative values of response not allowed"
		print(error)
		return(error)
	} else if (max.rsp > 1) {
		error <- "Error: values of response >1 not allowed"
		print(error)
		return(error)	
	}

	########################
	# Transformations
	########################
	
	# Add extra columns to hold original and transformed 
	# response values
	df$Rsp.tr <- NA
	df$Rsp.orig <- df$Rsp
	
	if (min.rsp == 0 || max.rsp == 1) {
		# Apply standard beta transformation
		# if zeros or ones present
		n <- nrow(df)
		df$Rsp.tr <- (df$Rsp * (n - 1) + 0.5 ) / n
	} else {
		df$Rsp.tr <- df$Rsp
	}
	
	########################
	# Set defaults & initial values
	########################
	
	# Set standard distribution codes
	dist.code.bet <- "Bet"
	dist.code.zibet <- "ZIBet"
	dist.code.oibet <- "OIBet"
	dist.code.zoibet <- "ZOIBet"
	
	# Start by assuming all models don't fit
	mod.fits.bet <- F
	mod.fits.zibet <- F
	mod.fits.oibet <- F
	mod.fits.zoibet <- F

	########################
	# Basic Beta (Bet)
	########################

	# Use transformed value
	df$Rsp <- df$Rsp.tr
	
	# Basic model, equal dispersion, logit link
	mod <- betareg(Rsp ~ Trt, data = df)
	
	# Basic model, log-log link function instead of (default) logit
	mod2 <- betareg(Rsp ~ Trt, data = df, link ="loglog")

	# Use model 3 only if have >2 values in each response category
	trt1.vals <- length(unique(df$Rsp[df$Trt==0]))
	trt2.vals <- length(unique(df$Rsp[df$Trt==1]))
	if ( trt1.vals>2 & trt2.vals>2   ) {
		# Two parameter model, variable dispersion, logit link
		mod3 <- betareg(Rsp ~ Trt | Trt, data = df)	
	}
	
	#Two parameter model, variable dispersion, log-log link
	mod4 <- betareg(Rsp ~ Trt, data = df, link ="loglog")
	
	# Compare the models using AIC
	if ( trt1.vals>2 & trt2.vals>2   ) {
		mods.aic <- AIC(mod, mod2, mod3, mod4)
	} else {
		mods.aic <- AIC(mod, mod2, mod4)
	}
	mods.aic$model <- rownames(mods.aic)		# add model name as column
	# Select best model; if ties, will select simplest as a result of index
	best.mod <- mods.aic$model[mods.aic$AIC == min(mods.aic$AIC)][1]
	
	# Save best model and variable indicating if one or two parameters
	if (best.mod == 'mod') {
		mod.bet <- mod
		mod.bet.type <- 'fixed'
	} else if (best.mod == 'mod2') {
		mod.bet <- mod2
		mod.bet.type <- 'fixed'
	} else if (best.mod == 'mod3') {
		mod.bet <- mod3
		mod.bet.type <- 'variable'
	} else {
		# best.mod=='mod4'
		mod.bet <- mod4
		mod.bet.type <- 'variable'
	}
	
	# Run goodness of fit based on analysis of deviance
	# nb. function Anova from package car (NOT base "anova")
	gf.mod <- Anova(mod.bet, type="II")
	gf <- gf.mod[[3]]
	
	# Save model pseudo-R2
	r2 <- mod.bet$pseudo.r.squared
	
	# Update success variable if model fits
	if (gf>=0.05) {
		mod.fits.bet <- T
	}
			
	# Save results for this distribution/model
	mod.bet <- mod
	gf.bet <- gf


	# Reset to non-transformed value for running
	# inflated models
	df$Rsp <- df$Rsp.orig
		
	########################
	# Zero-inflated Beta (ZIBet)
	########################

	if (FALSE) {		# Deactivating for now
	#if ( min(df$Rsp)==0 ) { START Zero-inflated
		# Only do if zeros present

		# UNDER CONSTRUCTION! NOT READY

		# Run the model
		mod <- gamlss(Rsp ~ Trt, data=df, family=BEINF,
			sigma.formula=~1, 
			nu.formula=~x1 + x3 + x4, 
			tau.formula=~x5
		)

		mod.sum <- summary(mod)
		
		# Run goodness of fit measure is zero-inflation coefficient
		gf <- coef(mod.sum)[[2]][4]
		
		# Update success variable if model fits
		if (gf>=0.05) {
			mod.fits.zibet <- T
		}
						
		# Save results for this distribution/model
		mod.zibet <- mod
		gf.zibet <- gf
		
	} # END Zero-inflated

	########################
	# One-inflated Beta (OIBet)
	########################

	if (FALSE) {		# Deactivating for now
	#if ( max(df$Rsp)==1 ) { START One-inflated beta
		# Only do if ones present
		
		# UNDER CONSTRUCTION
		
	} # END One-inflated beta
	
	########################
	# Zero-One-inflated Beta (ZOIBet)
	########################

	if (FALSE) {		# Deactivating for now
	#if ( min(df$Rsp)==0 & max(df$Rsp)==1 ) { START One-inflated beta
		# Only do if zeros and ones present
		
		# UNDER CONSTRUCTION
		
	} # END One-inflated beta
	
	###########################
	# Select the best model
	###########################
	
	# Stop after first successful model
	fit.found <- T	# Assume at least one model fits
	
	if (mod.fits.bet) {
		mod.best <- mod.bet
		dist.best.text <- 'Beta'
		gf.mod.best <- gf.bet
		dist.best <- dist.code.bet
	} else if (mod.fits.zibet) {
		mod.best <- mod.zibet
		dist.best.text <- 'Zero-inflated Beta'
		gf.mod.best <- gf.zibet
		dist.best <- dist.code.zibet
	} else if (mod.fits.oibet) {
		mod.best <- mod.oibet
		dist.best.text <- 'One-Inflated Beta'
		gf.mod.best <- gf.oibet
		dist.best <- dist.code.oibet
	} else if (mod.fits.zoibet) {
		mod.best <- mod.zoibet
		dist.best.text <- 'Zero-one-inflated Beta'
		gf.mod.best <- gf.zoibet
		dist.best <- dist.code.zoibet
	} else {
		# Return non-inflated model if no fit
		fit.found <- F
		mod.best <- mod.bet
		dist.best.text <- 'Beta'
		gf.mod.best <- gf.bet
		dist.best <- dist.code.bet
	}
	
	best.fit.results <- list(fit.found, dist.best, dist.best.text, mod.best, gf.mod.best,r2)
	return(best.fit.results)
}

beta.glm <- function(f,b) {
	###################################################
	# Select best fitting one-way GLM regression model using Beta 
	# distribution.
	# 
	# Appropriate for proportional data (continuous, bounded by zero and one).
	# 	Returns simplest model with significant fit. Order of testing:
	# 		Beta (Bet) --> Zero-inflated beta (ZIBet) --> One-inflated Beta (OIBet)
	# 		--> Zero- and one-inflated Beta (ZOIBet)
	# Accepts: data frame which *MUST* have the following columns:
	#		Trt: factor with two values (coding doesn't matter)
	#  	Rsp: Response. Must be integers >=0
	# Returns: List containing:
	#		error: FALSE if function fails, else TRUE
	#		fit.found: TRUE if at least one best model found, otherwise FALSE
	#		dist.best: Abbreviation for one of four models, as shown aboveb
	#		dist.best.text: Spelled out name of the best model
	#		mod.best: the best model (model results, itself a list)
	#		gf.mod.best: p-value of goodness-of-fit test for best-fitting model
	#	If no model passes, returns above values for model Bet
	# Requires: betareg package
	###################################################

	# transform if necessary
	f <-beta.transform(f)
	b <- beta.transform(b)
	
	# Make data frame
	df.f <- as.data.frame(f)
	names(df.f) <- c("Rsp")
	df.f$Trt <- 1
	df.b <- as.data.frame(b)
	names(df.b) <- c("Rsp")
	df.b$Trt <- 0
	df <- rbind(df.f,df.b)

	########################
	# Basic Beta
	########################

	# Basic model, equal dispersion, logit link
	mod <- betareg(Rsp ~ Trt, data = df)
	
	# Basic model, log-log link function instead of (default) logit
	mod2 <- betareg(Rsp ~ Trt, data = df, link ="loglog")

	# Use model 3 only if have >2 values in each response category
	trt1.vals <- length(unique(df$Rsp[df$Trt==0]))
	trt2.vals <- length(unique(df$Rsp[df$Trt==1]))
	if ( trt1.vals>2 & trt2.vals>2   ) {
		# Two parameter model, variable dispersion, logit link
		mod3 <- betareg(Rsp ~ Trt | Trt, data = df)	
	}
	
	#Two parameter model, variable dispersion, log-log link
	mod4 <- betareg(Rsp ~ Trt, data = df, link ="loglog")
	
	# Compare the models using AIC
	if ( trt1.vals>2 & trt2.vals>2   ) {
		mods.aic <- AIC(mod, mod2, mod3, mod4)
	} else {
		mods.aic <- AIC(mod, mod2, mod4)
	}
	mods.aic$model <- rownames(mods.aic)		# add model name as column
	# Select best model; if ties, will select simplest as a result of index
	best.mod <- mods.aic$model[mods.aic$AIC == min(mods.aic$AIC)][1]
	
	# Save best model and variable indicating if one or two parameters
	if (best.mod == 'mod1') {
		mod.bet <- mod
		mod.bet.type <- 'fixed logit'
	} else if (best.mod == 'mod2') {
		mod.bet <- mod2
		mod.bet.type <- 'fixed loglog'
	} else if (best.mod == 'mod3') {
		mod.bet <- mod3
		mod.bet.type <- 'variable logit'
	} else {
		# best.mod=='mod4'
		mod.bet <- mod4
		mod.bet.type <- 'variable loglog'
	}
	
	# Run goodness of fit based on analysis of deviance
	# nb. function Anova from package car (NOT base "anova")
	gf.mod <- Anova(mod.bet, type="II")
	gf <- gf.mod[[3]]
	
	# Save model pseudo-R2
	r2 <- mod.bet$pseudo.r.squared
	
	# Update success variable if model fits
	mod.fits.bet<-F
	if (gf>=0.05) {
		mod.fits.bet <- T
	}
	
	mod.type<-mod.bet.type
	mod <- mod.bet
			
	###########################
	# Save the model results
	###########################
	
	results <- list(mod, best.mod, mod.type, mod.fits.bet , gf, r2)
	names(results) <- c("mod", "best.mod", "mod.type", "mod.fits.bet", "gf", "r2")
	return(results)
	
}

beta.tr <- function(vec) {
	###################################################
	# Performs standard beta transformation on vector	
	# 
	# Accepts vector, returns transformed vector.
	# if vec>=0 & vec<=1, then transformation is vec>0 & vec<1
	# If vec>0 & vec<1 (no values equal to 1 or 0) then returns original 
	# untransformed vector.
	# Exits with error if any values >1 or <0
	###################################################
	# Some preliminary tests
	if ( max(vec)>1 || min(vec)<0 ) {
		error <- "Error: Values >1 or <0 not allowed for beta distribution!"
		print(error)
		return(error)
	}
	
	if ( max(vec)==1 || min(vec)==0 ) {
		# Apply standard beta transformation
		# if zeros or ones present
		n <- length(vec)
		vec <- (vec * (n - 1) + 0.5 ) / n
	}
	
	return(vec)
}

beta.constrain <- function(vec) {
	###################################################
	# Constrains vector distributed on [0,1] to beta interval (0,1)
	# 
	# Accepts vector, returns transformed vector.
	# Sets zeros only to very small value >0 & sets one to value very 
	# slightly < 1. Values on interval (0,1) are unchanged.
	# Exits with error if any values >1 or <0
	###################################################
	# Some preliminary tests
	if ( max(vec)>1 || min(vec)<0 ) {
		error <- "Error: Values >1 or <0 not allowed for beta distribution!"
		print(error)
		return(error)
	}
	
	if ( max(vec)==1 || min(vec)==0) {
		vec <- replace(vec, vec ==0, 0.000001)
		vec <- replace(vec, vec ==1, 0.999999)
	}
	
	return(vec)
}

beta.tr.all <- function(vec, val) {
	###################################################
	# Performs standard beta transformation on vector (vec) plus
	# single fixed value (val) which is not part of vector, based on 
	# sample size of vector.
	# 
	# Purpose: transforms focal IE vector & benchmark reference
	# value, for EIs which used fixed scoring method (q.method='fixed')
	# 
	# Accepts: vector & fixed value. Returns transformed vector & value.
	# 	if vec>=0 & vec<=1 OR val>=0 & val<=1 then transforms both such
	# 		that vec>0 & vec<1 AND val>0 & val<1
	# 	If vec>0 & vec<1 AND val>0 & vale<1 (no values equal to 1 or 0) then
	# 		returns original untransformed vector & value
	#
	# Returns: List object containing transformed vector and value
	#
	# Exits with error if any values >1 or <0
	###################################################
	
	# Check for illegal values
	if ( max(vec)>1 || min(vec)<0 || max(val)>1 || min(val)<0) {
		error <- "Error: Values >1 or <0 not allowed for beta distribution!"
		print(error)
		return(error)
	}
	
	if ( max(vec)==1 || min(vec)==0 ) {
		# Apply standard beta transformation
		# if zeros or ones present
		n <- length(vec)
		vec <- (vec * (n - 1) + 0.5 ) / n
		val <- (val * (n - 1) + 0.5 ) / n
	}

	trans <- list(vec, val)
	return(trans)
}

uni.fit.beta <- function(vec, shape1=NULL, shape2=NULL, lowlim=NULL ) {
	###################################################
	# Fits a univariate beta distribution to a vector	
	#
	# Parameters:
	#		vec			The beta-distributed vector
	#		shape1	[Optional]. Starting value for first beta shape parameter
	#		shape2	[Optional]. Starting value for second beta shape parameter
	#		lowlim		[Optional]. Starting value for optimization lower limit
	#
	# Return list object consistion of model parameters and bootstrap 
	# confidence limits.
	#
	# Vector must fall in the range 0 <--> 1. Will transform exact
	# 0s and 1s, but values <0 or >1 will cause it to fail
	###################################################

	# Check required packages
	if ( !require(simpleboot) ) stop("ERROR: package 'simpleboot' not loaded")
	if ( !require(MASS) ) stop("ERROR: package 'MASS' not loaded")

	# Some preliminary tests
	if ( max(vec)>1 || min(vec)<0 ) {
		error <- "Error: Values >1 or <0 not allowed for beta distribution!"
		print(error)
		return(error)
	}
	
	if ( max(vec)==1 || min(vec)==0 ) {
		# Apply standard beta transformation
		# if zeros or ones present
		n <- length(vec)
		vec <- (vec * (n - 1) + 0.5 ) / n
	}
	
	# Set optional parameters if not supplied
	if (is.null( shape1 ) ) shape1<-0.1
	if (is.null( shape2 ) ) shape2 <-0.1
	if (is.null( lowlim ) ) lowlim <-0.0001
	
	# Fit the distribution
	mod<- fitdistr(vec, "beta", 
		lower = c(lowlim, lowlim), 
		start=list(shape1=0.1,shape2=0.1)
		)	
		
	# Get parameter estimates
	alpha <- mod$estimate[1]
	beta <- mod $estimate[2]
	mod.mean <- alpha / (alpha+beta)
		
	# Get bootstrap confidence limits
	# Using simpleboot package
	# Can't use CLs from fitdistr as they are asymptotic (symmetrical) confidence
	# limits; we need the actual asymmetrical CLs	
	x.boot = one.boot(vec, mean, R=10000)
	mod.boot <- boot.ci(x.boot, type="bca") 
	mod.boot.lcl <- mod.boot$bca[4]
	mod.boot.ucl <- mod.boot$bca[5]

	# Get the half-confidence intervals
	# Can use as measure of effect size
	mod.boot.lci <- as.numeric( mod.mean - mod.boot.lcl )
	mod.boot.uci <- as.numeric( mod.boot.ucl - mod.mean )
	
	uni.fit.beta.results <- list(mod, mod.mean, mod.boot.lcl, mod.boot.ucl, mod.boot.lci, mod.boot.uci)
	names(uni.fit.beta.results) <- c("mod", "mod.mean", "mod.boot.lcl", "mod.boot.ucl", "mod.boot.lci", "mod.boot.uci")
	return(uni.fit.beta.results)	
}

best.fit.pois <- function(df) {
	###################################################
	# Select best fitting one-way GLM regression model for Poisson family. 
	# Appropriate for count data.
	# 	Returns best model with significant fit. Order of testing:
	# Poisson (P) --> Negative Binomial (NBin) --> Zero-inflated Poisson (ZIP)
	# --> Zero-inflated Negative Binomial (ZINB)
	# Accepts: data frame which *MUST* have the following columns:
	#		Trt: factor with two values (coding doesn't matter)
	#  	Rsp: Response. Must be integers >=0
	# Returns: List containing:
	#			error: FALSE if function fails, else TRUE
	#			fit.found: TRUE if at least one best model found, otherwise FALSE
	#			dist.best: Abbreviation for one of four models, as shown aboveb
	#			dist.best.text: Spelled out name of the best model
	#			mod.best: the best model (model results, itself a list)
	#			gf.mod.best: p-value of goodness-of-fit test for best-fitting model
	#	If no model passes, returns above values for negative binomial model
	###################################################

	# Some error testing
	if (min(df$Rsp<0)) {
		error <- "Error: negative values of response not allowed"
		print(error)
		return(error)
	}

	# Set standard distribution codes
	dist.code.p <- "P"
	dist.code.nbin <- "NBin"
	dist.code.zip <- "ZIP"
	dist.code.zinb <- "ZINB"
	
	# Start by assuming all models don't fit
	mod.fits.p <- F
	mod.fits.nbin <- F
	mod.fits.zip <- F
	mod.fits.zinb <- F

	########################
	# Test Poisson (P)
	########################

	# Run the model
	mod <- glm(Rsp ~ Trt, data = df, family = poisson)

	# Run goodness of fit X2 test
	gf <- 1 - pchisq( summary(mod)$deviance, summary(mod)$df.residual )
	
	# Update success variable if model fits
	if (gf>=0.05) {
		mod.fits.p <- T
	}
			
	# Save results for this distribution/model
	mod.p <- mod
	gf.p <- gf

	########################
	# Test Negative Binomial (NBin)
	########################

	# Run the model
	mod <- glm.nb(Rsp ~ Trt, data = df)
	
	# Run goodness of fit X2 test
	gf <- 1 - pchisq( summary(mod)$deviance, summary(mod)$df.residual )
	
	# Update success variable if model fits
	if (gf>=0.05) {
		mod.fits.nbin <- T
	}

	# Save results for this distribution/model
	mod.nbin <- mod
	gf.nbin <- gf
	
	####################################
	# Zero-inflated models
	# Only test if response contains at least one zero!
	####################################

	if (FALSE) {	#Disabling for now
	#if ( min(df$Rsp)==0 ) {
		
		########################
		# Test Zero-inflated Poisson (ZIP)
		########################

		# Run the model
		mod <- zeroinfl(Rsp ~ Trt |1, data = df, dist="poisson")
		mod.sum <- summary(mod)
		
		# Run goodness of fit measure on zero-inflation coefficient
		gf <- coef(mod.sum)[[2]][4]
		
	# Update success variable if model fits
		if (gf>=0.05) {
			mod.fits.zip <- T
		}
						
		# # Additional test  in case both this and plain P model pass
		# # *** UNDER CONSTRUCTION ****
		# if (mod.fits && mod.fits.p) {
			# vuong(mod.p, mod)
			# # Don't yet know how to save coefficients
			
			# # Reset mod.fits.p to FAIL if vuong AIC test shows that
			# # ZIP model is superior to P

		# }
	
		# Save results for this distribution/model
		mod.zip <- mod
		gf.zip <- gf

		########################
		# Test Zero-inflated Negative Binomial (ZINB)
		########################

		# Run the model
		# Try first with logit function, if that doesn't work, try probit,
		# if still fail, count model as failed
		
		mod <- tryCatch(	 zeroinfl(Rsp ~ Trt |1, data = df, dist="negbin"),
						error = function(cond) { 	return("FAIL") },
						warning = function(cond) {return("FAIL") }
					)
		if ( mod[1]=='FAIL' ) {
			mod <- tryCatch(	 zeroinfl(Rsp ~ Trt |1, data = df, dist="negbin", link="probit"),
							error = function(cond) { 	return("FAIL") },
							warning = function(cond) {return("FAIL") }
						)	
		}
		if ( mod[1]!='FAIL' ) {
			mod.sum <- summary(mod)
			
			# Run goodness of fit measure on zero-inflation coefficient
			gf <- coef(mod.sum)[[2]][4]
			
		# Update success variable if model fits
			if (gf>=0.05) {
				mod.fits.zinb <- T
			}
			
			# Save results for this distribution/model
			mod.zinb <- mod
			gf.zinb <- gf
		}
		
	}	
	###########################
	# Select the best model
	###########################
	
	# Stop after first successful model
	fit.found <- T	# Assume at least one model fits
	
	if (mod.fits.p) {
		mod.best <- mod.p
		dist.best.text <- 'Poisson'
		gf.mod.best <- gf.p
		dist.best <- dist.code.p
	} else if (mod.fits.nbin) {
		mod.best <- mod.nbin
		dist.best.text <- 'Negative Binomial'
		gf.mod.best <- gf.nbin
		dist.best <- dist.code.nbin
	} else if (mod.fits.zip) {
		mod.best <- mod.zip
		dist.best.text <- 'Zero-Inflated Poisson'
		gf.mod.best <- gf.zip
		dist.best <- dist.code.zip
	} else if (mod.fits.zinb) {
		mod.best <- mod.zinb
		dist.best.text <- 'Zero-inflated Negative Binomial'
		gf.mod.best <- gf.zinb
		dist.best <- dist.code.zinb
	} else {
		# Return NBin model if none fit
		fit.found <- F
		mod.best <- mod.nbin
		dist.best.text <- 'Negative Binomial'
		gf.mod.best <- gf.nbin
		dist.best <- dist.code.nbin
	}
	
	# Save best model pseudo-R2, in this case McFadden's
	r2 <- 1 - ( mod.best$null.deviance / mod.best$null.deviance )

	best.fit.results <- list(fit.found, dist.best, dist.best.text, mod.best, gf.mod.best, r2)
	return(best.fit.results)
}

fit.gamma <- function (df) {
	###################################################
	# Fits one-way GLM regression model using gamma distn
	#
	# Appropriate for continuous variables 0, Inf
	# Accepts: data frame which *MUST* have the following columns:
	#		Trt: factor with two values (coding doesn't matter)
	#  	Rsp: Response. Must be integers >=0
	# Returns: List containing:
	#			error: FALSE if function fails, else TRUE
	#			fit.found: TRUE if at least one best model found, otherwise FALSE
	#			dist.best: Abbreviation for one of four models, as shown aboveb
	#			dist.best.text: Spelled out name of the best model
	#			mod.best: the best model (model results, itself a list)
	#			gf.mod.best: p-value of goodness-of-fit test for best-fitting model
	#	If no model passes, returns above values for negative binomial model
	###################################################

	# Some error testing
	if (min(df$Rsp<0)) {
		error <- "Error: negative values of response not allowed"
		print(error)
		return(error)
	}

	# Set standard distribution codes
	dist.code.gamma <- "gamma"
	
	# Start by assuming all models don't fit
	mod.fits.gamma <- F

	########################
	# Fit model 
	########################

	# Run the model
	#mod <- glm(Rsp ~ Trt, data=df, family = Gamma(link=identity) )
	mod <- glm(Rsp ~ Trt, data=df, family = Gamma(link="identity"))		# log-link
	
	# Run goodness of fit X2 test
	gf <- 1 - pchisq( summary(mod)$deviance, summary(mod)$df.residual )	

	# Update success variable if model fits
	if (gf>=0.05) mod.fits.gamma <- T
			
	# Save results for this distribution/model
	mod.gamma <- mod
	gf.gamma <- gf

	###########################
	# Select the best model
	#
	# Keeping this structure for now, although only
	# fit one model, so nothing to select
	###########################
	
	# Stop after first successful model
	fit.found <- T	# Assume at least one model fits
	
	if (mod.fits.gamma) {
		mod.best <- mod.gamma
		dist.best.text <- 'Gamma'
		gf.mod.best <- gf.gamma
		dist.best <- dist.code.gamma
	} else {
		# Return NBin model if none fit
		fit.found <- F
		mod.best <- mod.gamma
		dist.best.text <- 'Gamma'
		gf.mod.best <- gf.gamma
		dist.best <- dist.code.gamma
	}

	# Save best model pseudo-R2, in this case McFadden's
	r2 <- 1 - ( mod.best$null.deviance / mod.best$null.deviance )
	
	best.fit.results <- list(fit.found, dist.best, dist.best.text, mod.best, gf.mod.best, r2)
	return(best.fit.results)
}

###############################################
###############################################
# Graphing functions
###############################################
###############################################

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
	###################################################
	# Multiple plot function
	#
	# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
	# - cols:   Number of columns in layout
	# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
	#
	# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
	# then plot 1 will go in the upper left, 2 will go in the upper right, and
	# 3 will go all the way across the bottom.
	#
	# Source:
	# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
	###################################################

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

split.zoib <- function (v) {
	###################################################
	# Split zero-inflated beta vector
	#
	# Purpose: 
	# 	Splits a vector of proportions into separate vectors of zeros, 
	# 	ones and a pure beta-distributed remainder. Returns
	# 	three vectors: v0 (zeros only), v1 (ones only), and v.beta (the 
	# 	remainder distributed on interval (0,1) ), their lengths
	# 	and proportions of the original vector vector length.
	#
	# Returns: 
	#	List object containing the split vectors, their lengths and 
	#	probabilities (proportions of the original vector length)
	###################################################
	
	# First check that vector is distributed on [0,1]
	if ( any( v>1 ) || any( v<0) )  {
		stop("ERROR: Vector has values outside interval [0,1]!")
	}
	
	# Split the vector
	v0 <- v[ v==0 ]
	v1 <- v[ v==1 ]
	v.beta <- v[ v!=1 & v!=0 ]

	# Get sample sizes of ech vector
	n <- length(v)
	n0 <- length(v0)
	n1 <- length(v1)
	n.beta <- length(v.beta)
				
	# Get proportions of each fraction relative to original vector
	p0 <- n0 / n
	p1 <- n1 / n
	p.beta <- n.beta / n
	
	v.split <- list(
		v0, v1, v.beta,
		n0, n1, n.beta,
		p0, p1, p.beta
	)
	names(v.split) <- c(
		"v0", "v1", "v.beta",
		"n0", "n1", "n.beta",
		"p0", "p1", "p.beta"
	)

	return(v.split)
}

beta.transform <- function(vec) {
	###################################################
	# Performs beta transformation on vector distributed on [0.1), 
	# (0,1] or [0,1], or returns original vector if already distributed
	# on (0,1). Also checks for zero-length vector.
	###################################################
	
	# Check vector is distributed on [0,1]
	msg_err <- "ERROR: Vector has values outside interval [0,1]!"
	#if ( any( vec > 1 ) || any( vec < 0) )  stop(msg_err)
	
	# Check vector not zero-length
	msg_err <- "ERROR: Vector has zero length!"
	if ( length(vec)==0 )  stop(msg_err)
	
	if ( max(vec) == 1 || min(vec) == 0 ) {
		n <- length(vec)
		vec <- (vec * (n - 1) + 0.5 ) / n
	}
	
	return(vec)
}

qual.prop <- function(f, b) {
	###################################################
	# Calculates quality based on difference between means of two
	# vectors of proportions	
	#
	# Parameters: 
	#		f: 	focal vector
	#		b: benchmark vector
	# 
	# Returns: Quality (proportion) determined by 1 minus difference
	# 	between 	f and b divided by distance between b and limit 
	# (either 0 or 1) over interval bounded by b and limit and
	# containing f. Discount method is thus linear.
	###################################################

	b.max <- max(b)
	b.min <- min(b)
	f.max <- max(f)
	f.min <- min(f)

	# Check f and b in bounds
	if ( b.min<0 || b.max>1 || f.min<0 || f.max>1 ) {
		stop("ERROR: f and/or b not a proportion!")
	}

	f.mean <- mean(f, na.rm=T)
	b.mean <- mean(b, na.rm=T)
	
	if ( f.mean == b.mean ) {
		q <- 1
	} else {
		# linear discount method
		d.abs <- abs( f.mean - b.mean )
		
		# Max distance between b.mean and distn. boundary
		d.max <- max( b.mean, 1-b.mean )	
		q <- 1 - ( d.abs / d.max )
	}
	
	return(q)
}

qual.prop.fixed.b <- function(f, b.fixed, perm.reps=1000, test.diff=NULL, use.q.diff.raw=FALSE, discount.method="linear" ) {
	###################################################
	# Calculates quality based on difference between means of focal
	# vector and fixed bm value
	#
	# Parameters:
	#		f: 	focal vector
	#		b.fixed: fixed benchmark value
	#		discount.method: 	linear (default; 1:1) or
	#										scaled (q declines exponentially toward
	#										zero)
	#
	# Returns: Quality (proportion) determined by 1 minus difference
	# 	between 	f and b divided by distance between b and limit
	# (either 0 or 1) over interval bounded by b and limit and
	# containing f.
	###################################################

	if ( is.null( test.diff ) ) test.diff <- TRUE

	f.mean <- mean(f, na.rm=T)
	f.max <- max(f, na.rm=T)
	f.min <- min(f, na.rm=T)
	d.abs <- abs(f.mean-b.fixed)
	p<-NA

	# Check f and b in bounds
	if ( b.fixed <0 || b.fixed>1 || f.min<0 || f.max>1 ) {
		stop("ERROR: f and/or b.fixed not a proportion!")
	}

	# Get p value of test H0: f.mean==b.fixed
	if ( test.diff==TRUE ) {
		test <- uni.test.boot(v=f, val=b.fixed, reps= perm.reps )
		p <- test$p
		if (d.abs ==0) p <- 1		# Adjust for erroneous p when f.mean=b.fixed
	}

	if ( f.mean == b.fixed ) {
		q <- 1
	} else {
		# Max distance between b.mean and distn. boundary
		d.max <- max( b.fixed, 1-b.fixed 	)
		q <- 1 - ( d.abs / d.max )
	}

	# Save q before any adjustments
	q.raw <- q

	# Reset q to 1 if diff not significant and not forcing use of raw value
	if ( test.diff==TRUE && use.q.diff.raw==FALSE && p >= 0.05 ) q <- 1

	# Rescale q if requested
	if ( discount.method=="scaled" ) q <- q.scaled.beta(q)

	# Assemble results & return
	results <- list( q, q.raw, p, d.abs 	)
	names(results) <- c( "q", "q.raw", "p", "d.abs" )
	return(results)
}

qual.prop.fixed <- function(f, b) {
	###################################################
	# Calculates quality of a single focal value relative to a fixed bm
	# value, where both are proportions
	#
	# Parameters: two proportions (over interval [0,1])
	# Returns: Quality (proportion) determined by 1 minus difference
	# 	between 	f and b divided by distance between b and limit 
	# (either 0 or 1) over interval bounded by b and limit and
	# containing f.
	###################################################

	# First check f and b in bounds
	if ( b<0 || b>1 || f<0 || f>1 ) stop("ERROR: f and/or b not a proportion!")
	
	if ( f==b ) {
		q <- 1
	} else if ( f > b ) {
		q <- 1 - ( f-b ) / ( 1-b )
	} else {
		q <- f / b
	}
	
	return(q)
}

qual.prop.fixed.f <- function( f, b ) {
	###################################################
	# Calculates quality of a fixed focal value (f) relative to a beta-
	# distribution vector of benchmark values (b). f must be
	# a proportion over [0,1]	
	#
	# Quality is calculated based on bootstrapped 95% CLs. Q=q00%
	# when lcl >= f <= ucl. Outside the CLs, Q decreases linearly to 
	# 0 at lower limit (0) or upper limit (1).
	#
	# Parameters: two proportions (over interval [0,1])
	# 
	# Returns: Quality (proportion) determined by 1 minus difference
	# 	between 	f and b divided by distance between b and limit 
	# (either 0 or 1) over interval bounded by b and limit and
	# containing f.
	###################################################
	
	# First check f and b in bounds
	if ( min(b)<0 || max(b)>1 || f<0 || f>1 ) stop("ERROR: f and/or b not proportions!")
	
	# Get CLs of b dist
	dist.bm <- fit.beta(b, xs=xs, shape1= shape1, shape2= shape2, 
		lowlim= lowlim, boot.cls=T)
	ucl <- dist.bm$boot.ucl
	lcl <- dist.bm$boot.lcl
	
	if ( f >= lcl && f <= ucl ) {
		q <- 1
	} else if ( f > ucl ) {
		q <- 1 - ( f - ucl ) / ( 1 - ucl )
	} else {
		q <- f / lcl
	}
	
	return(q)	
}

qual.beta.zoib <- function(f, b, xs=NULL, shape1=0.1, shape2=0.1, lowlim=0.0001) {
	###################################################
	# Calculates quality of a beta-distributed indicator, with support for 
	# zero- and one-inflation.
	# 
	# Accepts vectors of focal (f) and benchmark (b) values. These
	# are split into separate vectors of zeros, ones and the remainder.
	# Quality of the zero fraction compares the f proportion of zeros relative 
	# to the (expected) b fraction of zeros. Quality of the one fraction is 
	# analogous. Quality of the beta fraction is the overlap of the fitted beta
	# pdfs. Final overal quality is the sum of the zero, one and beta qualities,
	# with each weighted by the proportion of beta values in that fraction.
	#
	# Parameters:
	# 		1. f					Vector of focal EI values
	# 		2. b				Vector of benchmark EI
	#		3. xs				Vector of x values for generating predicted probabilities
	#								over interval [0,1]. [Default supplied below]
	#		4. shape1		Beta shape parameter 1
	#		5. shape1		Beta shape parameter 1
	#		6. lowlim			Lower limit to prevent optimization going out of bounds
	#
	# Returns list object consisting of final quality, overlap, the fitted
	# 		pdfs, plus the split vectors and all intermediate values.
	###################################################

	# First check vectors are distributed on [0,1]
	msg_err <- "ERROR: Vector has values outside interval [0,1]!"
	if ( any( f > 1 ) || any( f < 0) )  stop(msg_err)
	# First check that vector is distributed on [0,1]
	if ( any( b > 1 ) || any( b < 0) )  stop(msg_err)
	
	# Set default values for optional parameters & check for errors
	if (is.null(xs)) 	xs <- seq(0,1, length.out= 100)	
	
	###################################
	# Get quality and weightings for zero- and 
	# one- fractions, if any 
	###################################
	
	# Split the focal vector
	split <- split.zoib(f)
	f0 <- split$v0
	f1 <- split$v1
	f.bet <- split$v.beta
	p.f0 <- split$p0
	p.f1 <- split$p1
	p.f.bet <- split$p.beta
	
	# Split the benchmark vector
	split <- split.zoib(b)
	b0 <- split$v0
	b1 <- split$v1
	b.bet <- split$v.beta
	p.b0 <- split$p0
	p.b1 <- split$p1
	p.b.bet <- split$p.beta
		
	####################################
	# Calculate raw quality for each focal vector
	#
	# Raw quality is proportional overlap with the
	# benchmark fractions
	####################################

	q.f0 <- min (p.b0, p.f0 )
	q.f1 <- min (p.b1, p.f1 )
	f.bet.w <- min ( p.b.bet, p.f.bet ) # i.e., beta weighting factor
		
	################################
	# Quality of beta fraction
	#
	# First handle special cases where one or
	# both of the vectors are zero length or
	# have < 2 values. Then apply overlap method.
	###################################
	
	# Sizes and values of beta vectors, for handling special cases
	n.f.bet <- length(f.bet)
	n.b.bet <- length(b.bet)
	vals.f.bet <- length(unique(f.bet))
	vals.b.bet <- length(unique(b.bet))

	# Start by assuming overlap method not used
	# Therefore following variables will be NA
	ol <- NA	
	pdf.f <- NA
	pdf.b <- NA
	
	if ( n.f.bet==0 || n.b.bet==0 ) {	# START Beta quality
		q.f.bet <- 0
	} else if ( vals.f.bet==1 ) {
		# Fixed focal value
		f.fixed <- as.numeric( unique(f.bet) )
		if ( vals.b.bet==1 ) {
			b.fixed <- as.numeric( unique(b.bet) )
			q.f.bet <- qual.prop.fixed(  f.fixed, b.fixed )
		} else {
			q.f.bet <- qual.prop.fixed.f( f.fixed, b.bet )
		}
	} else if ( vals.b.bet==1 )	 {
		# Fixed beta value
		b.fixed <- as.numeric( unique(b.bet) )
		q.f.bet <- qual.prop.fixed.b( f.bet, b.fixed )
	} else {

		#######################################
		# Use overlap method
		#######################################
	
		fit.ol <- overlap.beta(f.bet, b.bet, xs=xs, 
			shape1= shape1, shape2= shape2, lowlim= lowlim)
		ol <- fit.ol$ol
		pdf.f <- fit.ol$pdf1
		pdf.b <- fit.ol$pdf2
		q.f.bet <- ol
	
	}	# END Beta quality	
	
	# Calculate weighted beta fraction quality
	q.f.bet.w <- q.f.bet * f.bet.w
		
	# Calculate overall quality
	q <- q.f0 + q.f1 + q.f.bet.w

	# Assemble list of results & return
	results <- list( q, ol, 
		b0, p.b0, f0, p.f0, q.f0,
		b1, p.b1, f1, p.f1, q.f1,
		b.bet, p.b.bet, f.bet, p.f.bet, q.f.bet, f.bet.w , q.f.bet.w,
		pdf.f, pdf.b 
	)
	names(results) <- c( "q", "ol", 
		"b0", "p.b0", "f0", "p.f0", "q.f0",
		"b1", "p.b1", "f1", "p.f1", "q.f1",
		"b.bet", "p.b.bet", "f.bet", "p.f.bet", "q.f.bet", "f.bet.w", "q.f.bet.w",
		"pdf.f", "pdf.b" 
	)
	return(results)
}

boot.diff <- function( x, y, boot.reps=NULL ) {
	###################################################
	# Calculates bootstrap 95% CLs of difference between means of 
	# two groups. 
	#
	# Parameters: two vectors, x & y
	# Returns list object:
	#		x.mean	observed mean of x
	# 		y.mean		observed mean of y
	#		d.obs		observed mean difference
	#		d.lcl			lower 95% CL
	# 		d.ucl		upper 95% CL
	###################################################

	# Set boot.reps if not supplied
	if ( is.null(boot.reps) ) boot.reps <- 10000
	
	x.mean <-mean(x, na.rm=T)
	y.mean <- mean(y, na.rm=T)
	d.obs <- abs( x.mean - y.mean )

	# # Create vector of bootstrapped overlap values
	# boot.d <- rep(NA, boot.reps)
	# for (i in 1: boot.reps) {
		# boot.samp.x <- sample(x, length(x), replace =T )
		# boot.samp.y <- sample(y, length(y), replace =T )
		# boot.d[i] <- abs(mean(boot.samp.x)-mean(boot.samp.y)), na.rm=T)
	# }
	
	boot.d <- sapply(1: boot.reps, function(i) {
		boot.samp.x <- sample(x, length(x), replace =T )
		boot.samp.y <- sample(y, length(y), replace =T )
		d.samp <- abs( mean(boot.samp.x, na.rm=T) - mean(boot.samp.y, na.rm=T) )
	} )
	
	# Get 95% CLs of the bootstrapped differences 
	# Empirical bootstrap method
	boot.mean <- mean(boot.d, na.rm=T)
	boot.dev <- boot.d - boot.mean			# Deviations from bootstrap mean
	d.dev <- d.obs + boot.dev
	d.cls <- quantile(d.dev, c(0.025, 0.975), na.rm=T)	# bootstrap CLs
	d.sd <- sd(d.dev)									# Bootstrap SD
	
	results <- list(x.mean, y.mean, d.obs, d.sd, d.cls)
	names(results) <- c("x.mean", "y.mean", "d.obs", "d.sd", "d.cls")
	return( results )
}

boot.qual.diff <- function( f, b, dist, boot.reps=NULL ) {
	###################################################
	# Calculates bootstrap 95% CLs of quality based on difference 
	# between means of focal (f) and benchmark (b) vectors
	#
	# Parameters: 
	#		f					focal vectors
	#		b					benchmark vectors
	#		dist				distribution of the vectors c("Bet","NBin","gamma")
	#		boot.reps	[Optional; default=10000)
	#
	# Returns list object:
	#		f.mean		observed mean of f
	# 		b.mean	observed mean of b
	#		d.obs		observed mean difference
	# 		q.obs		observed mean quality
	#		q.cls			bootstrap 95% CLs
	#		p.diff		p-value for perm test of H0: f.mean=b.mean
	# 		boot.q		bootstrapped qualities
	#		boot.reps	Number of replications used
	###################################################

	# Set boot.reps if not supplied
	if ( is.null(boot.reps) ) boot.reps <- 10000
	
	f.mean <-mean(f, na.rm=T)
	b.mean <- mean(b, na.rm=T)
	d.obs <- abs( f.mean - b.mean )
	
	# Test if means different
	p.diff <- means.test.perm( f, b, iterations=boot.reps )
	
	# Calculate quality according to distribution
	if (dist=="Bet") {
		q.obs <- qual.prop.fixed(f.mean, b.mean) 
	} else {
		stop("UNDER CONSTRUCTION (function: boot.qual.diff)")
	}

	boot.q <- sapply(1: boot.reps, function(i) {
		boot.samp.f <- sample(f, length(f), replace =T )
		boot.samp.b <- sample(b, length(b), replace =T )
		boot.samp.f.mean <-mean(boot.samp.f )
		boot.samp.b.mean <- mean(boot.samp.b)
		
		if (dist=="Bet") {
			q.samp <- qual.prop.fixed(boot.samp.f.mean, boot.samp.b.mean) 
		} else {
			stop("UNDER CONSTRUCTION (function: boot.qual.diff)")
		}
	} )
	
	# Get 95% CLs of the bootstrapped differences 
	# Empirical bootstrap method
	boot.mean <- mean(boot.q, na.rm=T)
	boot.dev <- boot.q - boot.mean			# Deviations from bootstrap mean
	q.dev <- q.obs + boot.dev						# Bootstrap mean deviances
	q.cls <- quantile(q.dev, c(0.025, 0.975), na.rm=T)		# bootstrap CLs
	q.sd <- sd(q.dev)															# Bootstrap SD
	q.lcl <- q.cls[[1]]
	q.ucl <- q.cls[[2]]
	
	results <- list(
		f.mean, b.mean, d.obs, 
		q.obs, q.sd, q.lcl, q.ucl, 
		p.diff
	)
	names(results) <- c(
		"f.mean", "b.mean", "d.obs", 
		"q.obs", "q.sd", "q.lcl", "q.ucl", 
		"p.diff"
	)
	# results <- list(f.mean, b.mean, d.obs, q.obs, q.sd, q.lcl, q.ucl, p.diff, boot.q, boot.reps)
	# names(results) <- c("f.mean", "b.mean", "d.obs", "q.obs", "q.sd", "q.lcl", "q.ucl", "p.diff", "boot.q", "boot.reps")

	return( results )
}

jitter.beta <- function( v, tinybit=NULL ) {
	###################################################
	# Adds small amount of noise to single-valued vector distributed on 
	# interval [0,1]. Enables vector to be fit by beta distribution with 
	# mean identical or very close to original vaue 
	#
	# Vectors which are all 0s will have tiny increase in mean because 
	# can only add, not subtract. For analogous reason, vectors which
	# are all 1s will have small decrease in mean.
	#
	# Accepts:
	#		v			Single-valued vector v
	#		itsybit	Diplacement (+/-) to be added to vector
	#
	#	Returns:	The jittered vector
	###################################################
	
	if (is.null(tinybit)) tinybit <- 0.0001
	n.v <- length(v)
	v.unique <- unique(v)
	v.numvals <- length(v.unique)
	v.mean <- mean(v, na.rm=T)
	
	if ( v.numvals==1 && v.unique==0 )  {
		e1 <- max(0, v.mean + tinybit/2)
		e2 <- max(0, v.mean + tinybit)
	} else if  ( v.numvals==1 && v.unique==1 ) {
		e1 <- min(1, v.mean - tinybit/2)
		e2 <- min(1, v.mean - tinybit)
	} else {
		e1 <- max(0, v.mean - tinybit)
		e2 <- min(1, v.mean + tinybit)
	}

	v.jittered <- c(e1, e2, v[3:n.v])
	return(v.jittered)
}

fit.beta <- function(vec, xs=NULL, shape1=0.1, shape2=0.1, lowlim=0.0001, boot.cls=FALSE) {
	###################################################
	# Fits beta distribution to vector distributed on (0,1)
	#
	# Requires: function fitdistr from package MASS
	# 
	# Parameters:
	#		vec				The vector
	#		xs					The range of x values over which pdf predicted
	#		shape1		Starting shape parameter 1
	#		shape2		Starting shape parameter 2
	# 		lowlim			Lower limit for optimization
	#		boot.cls		[Optional] Return boostrap 95% CLs, if desired
	#
	# Returns: List object containing:
	# 		pdf			The fitted pdf
	#		f.shape1	First shape parameter of fitted pdf
	#		f.shape2	Second shape parameter of fitted pdf
	#		f.mean		Mean of the fitted distribution
	#		boot.lcl	Bootstrap lower 95% CL of the vector
	#		boot.lcl	Bootstrap upper 95% CL of the vector
	#
	# On fail returns message 'FAIL'
	#
	# Note: vector must be distributed on (0,1) and have >1 value
	#####################################################
	
	maxTries <- 10; # Max time to try fit before reporting error

	if (! require(MASS) ) stop("Required package MASS not installed!")

	# Transform if any values==1 or 0
	# Throws error if (a) outside interval [0,1], (b) vector is null,
	# or (c) vector has <2 values
	vec<- vec[!is.na(vec)]
	vec <- beta.transform( vec )
	
	# # Check for too few values
	# msg_err <- "Vector has only one value!"
	# if ( length( unique(vec) ) <= 1 ) 	stop(msg_err)
	
	# Set default values for optional parameters & check for errors
	if (is.null(xs)) 	xs <- seq(0,1, length.out= 100)	

	# Compose fitdistr optional arguments
	opt.args <- paste0( ", lower = c(", lowlim, ", ",  lowlim, "), start=list(shape1=", shape1, ", shape2=", shape2, ")" )
	
	# Construct the fitdistr command
	fit <- paste0("fitdistr(vec, densfun='beta'",  opt.args, ")")
		
	###########################################
	# Fit pdf
	#
	# Note:
	# (1) Use of eval(parse...) to force evaluation of string 
	# 		as a command
	# (2) Tries repeatedly, up to a maximum of maxTries, if
	# 		if optimization fails
	###########################################

	tries <- 0
	done <- F
	fail <- F
	while (done==F) {
		tries <- tries + 1
		mod <- tryCatch(	 eval(parse(text= fit)),
						error = function(cond) { 	return("FAIL") },
						warning = function(cond) {return("FAIL") }
					)
		if ( mod[1]=='FAIL' ) {
			if ( tries >= maxTries )  {
				fail <- T
				break
			}
		} else {
			pdf <- dbeta( xs, shape1= mod$estimate[1], shape2= mod$estimate[2])
			pdf[pdf ==Inf] <- NA
			
			# Save model parameters for overlap calculation
			f.shape1 <- mod[[1]][[1]]
			f.shape2 <- mod[[1]][[2]]
			done <- T
		}
	}
	
	if (fail ==T) {
		pdf <- NA
		f.shape1 <- NA
		f.shape2 <- NA
		f.mean <- NA
		boot.lcl <- NA
		boot.ucl <- NA
	} else {
		
		# Get model mean
		f.mean <- f.shape1 / ( f.shape1 + f.shape2 )
		
		# Set CLs to null in case not calculating
		boot.lcl <- NA
		boot.ucl <- NA
			
		if ( boot.cls==TRUE ) { 
			################################################
			# Optionally calculate bootstrap confience limits
			# Using simpleboot package
			# Can't use CLs from fitdistr as they are asymptotic 
			# (= symmetrical). We need the asymmetrical CLs	
			################################################
	
			# Check required package
			if ( !require(simpleboot) ) stop("ERROR: package 'simpleboot' not loaded")
	
			# Calculate CLs
			x.boot = one.boot(vec, mean, R=10000)
			mod.boot <- boot.ci(x.boot, type="bca") 
			boot.lcl <- mod.boot$bca[4]
			boot.ucl <- mod.boot$bca[5]
		}
	}
	
	result <- list( pdf, f.shape1, f.shape2, f.mean,  boot.lcl, boot.ucl, fail, tries )
	names(result) <- c( "pdf", "shape1", "shape2", "f.mean",  "boot.lcl", "boot.ucl", "fail", "tries" )
	
	return(result)
	
}

overlap.beta <- function(v1, v2, xs=NULL, shape1=0.1, shape2=0.1, lowlim=0.0001) {
	###################################################
	# Calculates overlap of two beta-distributed vectors
	# 
	# Details: Calculates overlap only if both vectors are successfully
	#		fit. If not fit, sets overlap=0, returns ol.fail=TRUE, and remaining
	# 		objects are returned as NA.
	#
	# Requires: fitdistr function from MASS package required by function
	#		fit.beta, called by this one.
	# 
	# Parameters:
	# 		1. v1				Vector 1
	# 		2. v2				Vector 2
	#		3. xs				Vector of x values for generating predicted probabilities
	#								over interval [0,1]. [Default supplied below]
	#		4. shape1		Beta shape parameter 1
	#		5. shape1		Beta shape parameter 1
	#		6. lowlim			Lower limit to prevent optimization going out of bounds
	#
	# Returns:
	#		List object containing:
	#		ol			Proportional overlap between the fitted distributions
	#		pdf1		Fitted distribution 1
	#		pdf2		Fitted distribution 2
	###################################################

	# Transform if any values==1 or 0
	# Throws error if (a) outside interval [0,1], (b) vector is null,
	# or (c) vector has <2 values
	# v1 <- beta.transform( v1 )
	# v2 <- beta.transform( v2 )
	
	# # Check for too few values
	# msg_err <- "ERROR: Vector has only one value!"
	# if ( length( unique(v1) )<2 || length( unique(v2) )<2 ) stop(msg_err)
	
	# Set default values for optional parameters & check for errors
	if (is.null(xs)) 	xs <- seq(0,1, length.out= 100)	

	###########################################
	# Fit v1 pdf & get shape parameters for overlap
	###########################################
	
	# Start by assuming everything works
	fit.fail <- FALSE
	ol.fail <- FALSE		
	
	# Initialize fitted pdfs and their parameters
	pdf1 <- NA
	v1.shp1 <- NA
	v1.shp2 <- NA

	pdf2 <- NA
	v2.shp1 <- NA
	v2.shp2 <- NA
			
	fit <- fit.beta(v1, xs=xs, shape1= shape1, shape2= shape2, lowlim= lowlim, boot.cls=F) 

	if ( fit$fail == TRUE ) {
		fit.fail <- TRUE
		ol.fail <- TRUE
	} else {
		pdf1 <- fit$pdf
		v1.shp1 <- fit$shape1
		v1.shp2 <- fit$shape2

		###################################
		# Fit v2 pdf
		###################################
	
		fit <- fit.beta(v2, xs=xs, shape1= shape1, shape2= shape2, lowlim= lowlim, boot.cls=F) 

		if ( fit$fail == T ) {
			fit.fail <- TRUE
			ol.fail <- TRUE
		} else {
			pdf2 <- fit$pdf
			v2.shp1 <- fit$shape1
			v2.shp2 <- fit$shape2

			###################################
			# Calculate overlap between pdfs using
			# exact calculus method
			# Wrapped in error catcher as well
			###################################
			#ols <- prop.ol.beta( xs=xs, s1=v1.shp1, r1=v1.shp2, s2=v2.shp1, r2=v2.shp2 )
			ols <- tryCatch(
				prop.ol.beta( xs=xs, s1=v1.shp1, r1=v1.shp2, s2=v2.shp1, r2=v2.shp2 ), 
				error = function(cond) { 	return("FAIL") },
				warning = function(cond) {return("FAIL") }
			)
			if (ols[1]=='FAIL') {
				ol.fail <- T
			} else {
				ol <- ols$ol
			}
		}
	}
		
	if ( ol.fail==T ) {
		ol <- 0
	}
	
	result <- list( ol, ol.fail, fit.fail, pdf1, pdf2, v1.shp1, v1.shp2, v2.shp1, v2.shp2 )
	names(result) <- c( "ol", "ol.fail", "fit.fail", 
		"pdf1", "pdf2", 
		"v1.shp1", "v1.shp2", 
		"v2.shp1", "v2.shp2"  
		)
	
	return(result)
}

qual.beta.orig <- function( f, b, xs=NULL, test.diff=FALSE, perm.reps=NULL, shape1=0.1, shape2=0.1, lowlim=0.0001, use.q.diff.raw=FALSE, discount.method=NULL  )  {
	###################################################
	# Calculates overall quality for beta-distributed focal indicator vector, 
	# relatve to benchmark vector, based on difference of means & 
	# weighted by distribution overlap
	#
	# Vectors f & b must be distributed on [0,1]. Runs permutation test
	# only if test.diff=T. Recommend test.diff=FALSE if running inside 
	# bootstrap to avoid excessive run times.
	#
	# Returns list object: f.mean, b.mean, diff, diff.p, q.diff, q.ol, q
	###################################################

	# Set default values for optional parameters & check for errors
	if (is.null(xs)) xs <- seq(0,1, length.out= 100)
	if (is.null(perm.reps)) 	perm.reps <- 10000
	if (is.null(discount.method)) 	discount.method <- "linear"
	
	f.mean <- mean(f, na.rm=T)
	f.w <- min(f.mean, 1 - f.mean)		# Focal weight
	b.mean <- mean(b, na.rm=T)
	b.w <- min( b.mean, 1 - b.mean) 	# Benchmark weight
	w <- f.w + b.w		# final weighting factor
	
	diff <- f.mean - b.mean
	
	q.diff.raw <- qual.prop(f, b)		# Difference-based quality

	# Set default values of adjusted quality and means test p-value
	q.diff.adj <- q.diff.raw
	diff.p <- NULL

	# Get p value from perm test & adjust q.diff if needed
	if (test.diff==T) {
		diff.p <- means.test.perm( x=f, y=b, iterations=perm.reps ) 
		if ( diff.p>=0.05 ) q.diff.adj <- 1		# set adj. q to 100% if no sig diff
	}
	
	# Set final value of q.diff
	if ( use.q.diff.raw==TRUE )	{
		q.diff <- q.diff.raw
	} else {
		q.diff <- q.diff.adj
	}
			
	# Get overlap if possible
	q.ol <- NA
	
	# Transform if any values==1 or 0
	# Throws error if (a) outside interval [0,1], (b) vector is null,
	# or (c) vector has <2 values
	f.tr <- beta.transform( f )
	b.tr <- beta.transform( b )
	len.u.f <- length( unique( f.tr ))
	len.u.b <- length( unique( b.tr ))
	
	# Add small amount of jitter on interval (0,1) 
	# to one or both vectors to allow fitting
	if ( len.u.f==1) f.tr <- jitter.beta(f)
	if ( len.u.b==1) b.tr <- jitter.beta(b)
		
	ol.result <- overlap.beta(v1=f.tr, v2=b.tr, xs=xs, 
						shape1= shape1, shape2= shape2, lowlim=lowlim)
	q.ol <- ol.result$ol
	pdf.f <- ol.result$pdf1
	pdf.b <- ol.result$pdf2
	fit.fail <- ol.result$fit.fail
	
	# Scale distance-based quality, if requested
	if ( discount.method == "scaled" ) {
		q.diff <- q.scaled.beta(q.diff)
	}
	
	# Overall quality
	q <- ( q.ol * w ) + ( q.diff * ( 1 - w ) )

	results <- list(
		f.mean, b.mean, diff, diff.p, q.diff.raw, q.diff, q.ol, q, w, pdf.f, pdf.b
	)
	names(results) <- c(
		"f.mean", "b.mean", "diff", "diff.p", "q.diff.raw", "q.diff", "q.ol", "q", "ol.w", "pdf.f", "pdf.b"
	)
	
	return(results)
}

qual.beta <- function( f, b, xs=NULL, test.diff=FALSE, perm.reps=NULL, shape1=0.1, shape2=0.1, lowlim=0.0001, use.q.diff.raw=FALSE, discount.method="linear", wt.method="u.beta.buffered", buffer=0.01  )  {
	###################################################
	# Calculates overall quality for beta-distributed focal indicator vector, 
	# relatve to benchmark vector, based on difference of means & 
	# weighted by distribution overlap
	#
	# Vectors f & b must be distributed on [0,1]. Runs permutation test
	# only if test.diff=T. Recommend test.diff=FALSE if running inside 
	# bootstrap to avoid excessive run times.
	#
	# Returns list object: f.mean, b.mean, diff, diff.p, q.diff, q.ol, q
	###################################################

	# Set default values for optional parameters & check for errors
	if (is.null(xs)) xs <- seq(0,1, length.out= 100)
	if (is.null(perm.reps)) perm.reps <- 10000
	
	f.mean <- mean(f, na.rm=T)
	b.mean <- mean(b, na.rm=T)
	diff.mean.abs <- abs(f.mean-b.mean)
	
	# Calcuate half-weights for each mean
	if ( wt.method == "linear" ) {
		# Linear decay (original method)
		# 100% overlap at 0.5, 100% means at 0 and 1, transitions
		# linearly between these points
		
		f.w <- min(f.mean, 1 - f.mean)		# Focal weight
		b.w <- min( b.mean, 1 - b.mean) 	# Benchmark weight
		w <- f.w + b.w	
	
	} else if ( wt.method == "u.beta" ) {
		#  Reflected (U-shaped) beta weighting function
		# 100% overlap for most of beta mid-range
		# 100% means-based at 0 and 1 only
		# Smooth but steep transition at extreme values only
		# determined by beta function ([0.5:1]) and
		# reflected beta function ([0.5:0])
		
		f.w <- beta.wt( f.mean ) / 2
		b.w <- beta.wt( b.mean ) / 2
	
	} else if ( wt.method == "u.beta.buffered" ) {
		# Edge-buffered reflected (U-shaped) beta weighting function
		# Similar to wt.method='u.beta', but beta U-function is pushed
		# inward, reaching 100% not at 0 and 1, but instead at lower limit
		# buffer and upper limit 1-buffer. Beyond buffer (values <= buffer 
		# and >=1-buffer) stays at 100%.
		
		f.w <- u.beta.buffered( f.mean, buffer=buffer, sh1=0.2, sh2=2 ) / 2
		b.w <- u.beta.buffered( b.mean, buffer=buffer, sh1=0.2, sh2=2 ) / 2		
	
	}	else {
		stop("wt.method not valid (func: qual.beta)")
	}
	
	# Step correction for extreme values
	# Sets means weight to 100% if at least one mean very close to 0 or 1
	# and the other mean within 1% of the more extreme mean
	if ( (f.mean<0.001 || f.mean>0.999 ) && diff.mean.abs<0.01) f.w <- 0.5		
	if ( (b.mean<0.001 || b.mean>0.999 ) && diff.mean.abs<0.01) b.w <- 0.5	
	
	# Combine half-weights into final weight
	w <- 1 - ( f.w + b.w	) 

	# Get means-based quality
	diff <- f.mean - b.mean	
	q.diff.raw <- qual.prop(f, b)		# Difference-based quality

	# Set default values of adjusted quality and means test p-value
	q.diff.adj <- q.diff.raw
	diff.p <- NULL

	# Get p value from perm test & reset q.diff to 1 if difference not sig.
	if (test.diff==T) {
		diff.p <- means.test.perm( x=f, y=b, iterations=perm.reps ) 
		if ( diff.p>=0.05 ) q.diff.adj <- 1		# set adj. q to 100% if no sig diff
	}
	
	# Set final value of q.diff
	if ( use.q.diff.raw==TRUE )	{
		q.diff <- q.diff.raw
	} else {
		q.diff <- q.diff.adj
	}
			
	# Get overlap if possible
	q.ol <- NA
	
	# Transform if any values==1 or 0
	# Throws error if (a) outside interval [0,1], (b) vector is null,
	# or (c) vector has <2 values
	f.tr <- beta.transform( f )
	b.tr <- beta.transform( b )
	len.u.f <- length( unique( f.tr ))
	len.u.b <- length( unique( b.tr ))
	
	# Add small amount of jitter on interval (0,1) 
	# to one or both vectors to allow fitting
	if ( len.u.f==1) f.tr <- jitter.beta(f)
	if ( len.u.b==1) b.tr <- jitter.beta(b)
		
	ol.result <- overlap.beta(v1=f.tr, v2=b.tr, xs=xs, 
						shape1= shape1, shape2= shape2, lowlim=lowlim)
	q.ol <- ol.result$ol
	pdf.f <- ol.result$pdf1
	pdf.b <- ol.result$pdf2
	fit.fail <- ol.result$fit.fail
	
	# Scale distance-based quality, if requested
	if ( discount.method == "scaled" ) {
		q.diff <- q.scaled.beta(q.diff)
	}
	
	# Overall quality
	q <- ( q.ol * w ) + ( q.diff * ( 1 - w ) )

	results <- list(
		f.mean, b.mean, diff, diff.p, q.diff.raw, q.diff, q.ol, q, w, pdf.f, pdf.b
	)
	names(results) <- c(
		"f.mean", "b.mean", "diff", "diff.p", "q.diff.raw", "q.diff", "q.ol", "q", "ol.w", "pdf.f", "pdf.b"
	)
	
	return(results)
}

qual.beta.ol <- function( f, b, xs=NULL, shape1=0.1, shape2=0.1, lowlim=0.0001  )  {
	###################################################
	# Calculates overall quality for beta-distributed focal indicator vector, 
	# relatve to benchmark vector, based distribution overlap only.
	#
	# Vectors f & b must be distributed on [0,1]. 
	#
	# Returns list: 
	#	f.mean		Focal mean
	# b.mean		Benchmark mean
	# diff			Abs. difference f.mean, b.mean
	# q.ol			Overlap-based quality
	# pdf.f			Fitted focal distribution
	# pdf.b			Fitted benchmark distribution
	###################################################

	# Variable for recording transformation method
	beta.tr.method <- ""

	# Set default values for optional parameters & check for errors
	if (is.null(xs)) xs <- seq(0,1, length.out= 100)
	
	f.mean <- mean(f, na.rm=T)
	b.mean <- mean(b, na.rm=T)
	diff <- f.mean - b.mean

	# Get overlap
	q.ol <- NA
	
	# Get count of unique values
	len.u.f <- length( unique( f ))
	len.u.b <- length( unique( b ))

	#####################################
	# Handle special cases and calculate overlap
	# 
	# Checks:
	# 	(a)	Values outside [0,1] (error)
	# 	(b)	b & f effectively identical
	# 	(c) 	Vector consists entirely of 0 or 1
	#	(d)	Some values 0 or 1
	# 	(e) 	Mean very low (<0.01) but not all zeros
	#	(f)		Single-value vectors on domain (0,1)
	#####################################

	f.tr <- f; b.tr <- b

	if ( any(f<0) || any(f>1) || any(b<0) || any(b>1) ) {

		# (a) Outside beta domain
		# Report error and stop
		f.bad<-""; b.bad<-""
		if ( any(f<0) || any(f>1) ) f.bad<-"f"
		if ( any(b<0) || any(b>1) ) b.bad<-"b"
		bad.vecs <- smartpaste(f.bad, b.bad, sep=", ")
		msg.err <- paste0("ERROR: One or more value of ", bad.vecs, " outside domain [0,1]")
		stop(msg.err)
	
	} else if ( ( len.u.f==1 && len.u.b==1 && unique(f)==unique(b) ) || ( all(length(f)==length(b)) && all(f==b)) ) {
		
		# (b) f & b are identical, or are effectively identical vectors of one or 
		# more identical values
		# Can't be fit so just set overlap (quality) to 100% and skip the rest
		q.ol <- 1
		f.tr <- f
		pdf.f <- NA
		pdf.b <- NA
		beta.tr.method <- "effectively identical"
	
	} else {		
		
		# Calculate overlap for the remainder, transforming as needed
		
		# Use these variable to stop further transformations as needed
		f.finished <- FALSE
		b.finished <- FALSE

		#########################################
		# (c) Transform all-zero and all-one vectors
		#
		# Notes:
		# 1. Converts to highly skewed true beta distribution with 
		#	expected value very close to 0 or 1, but on the domain
		#	(0,1) to allow fitting and calculation of overlap.
		#########################################

		# Skew, mean & stdev parameters 
		sh1_rskew <- 8; 	sh2_rskew <- 0.01		# Right skewed mean near zero
		sh1_lskew <- 0.01; sh2_lskew <- 8		# Left skewed mean near one
		sd_lep <- 0.01				# Highly leptokurtic
		sd_lep2 <- 0.05				# Less leptokurtic
		sd_lep3 <- 0.1				# Less leptokurtic
		mean_zero <- 0.0001	# Close to zero
		mean_one <- 0.9999		# Close to one
		s1r_01 <- 10
		s2r_01 <- 0.01
		s1l_01 <- 0.01
		s2l_01 <- 10
		
		# f all zeros or ones
		if ( all( unique(f)==0 ) ) {
			# f all zeros; convert to right skewed, mean near 0.01
			f.tr <- pbeta(xs, s1r_01, s2r_01)
			beta.tr.method <- "focal all zeros"
			f.finished<-TRUE
		} else if ( all( unique(f)==1 ) ) {
			# f all ones; convert to left skewed, mean near 0.99
			f.tr <- pbeta(xs, s1l_01, s2l_01)		
			# f.tr <- beta.constrain(f.tr)
			# # f.tr <- beta.normal( f.tr, mn=mean_one, 
				# # sdev=sd_lep, sh1=sh1_lskew, sh2=sh2_lskew
			# # )
			# f.tr <- noise.beta.norm(f.tr,length(f.tr),sd_lep)
			beta.tr.method <- "focal all ones"
			f.finished<-TRUE
		}

		# b all zeros or ones
		if ( all( unique(b)==0 ) ) {
			# b all zeros; convert to right skewed, mean near 0.01
			b.tr <-  pbeta(xs, s1r_01, s2r_01)
			beta.tr.method <- smartpaste(beta.tr.method, "bm all zeros", sep=",")
			b.finished<-TRUE
		} else if ( all( unique(b)==1 ) ) {
			# b all ones; convert to left skewed, mean near 0.99
			b.tr <- pbeta(xs, s1l_01, s2l_01)		
			# b.tr <- beta.constrain(b.tr)
			# b.tr <- beta.normal( b.tr, mn=mean_one, 
				# sh1=sh1_lskew, sh2=sh2_lskew, sdev=sd_lep
			# )
			beta.tr.method <- smartpaste(beta.tr.method, "bm all ones", sep=",")
			b.finished<-TRUE
		}
	
		#########################################
		# (d) Some zero or ones (but not all)
		#########################################

		if ( 
		( f.finished==FALSE && ( any(f.tr ==0) || any(f.tr ==1) ) )
		|| 	
		( b.finished=FALSE && ( any(b.tr==0) || any(b.tr ==1) ) )
		) {
			if ( any(f.tr ==0) || any(f.tr ==1) ) {
				# f.tr <- beta.transform( f.tr )	# traditional beta transform
				f.tr <- beta.constrain( f.tr )	# Just squeezes 0s and 1s inside (0,1)
				beta.tr.method <- smartpaste(beta.tr.method, "f some zeros/ones", sep=",")
			}
			if ( any(b.tr ==0) || any(b.tr ==1) ) {
				# b.tr <- beta.transform( b.tr )
				b.tr <- beta.constrain( b.tr )
				beta.tr.method <- smartpaste(beta.tr.method, "b some zeros/ones", sep=",")
			}
		}
		
		####################################################
		# (e) Mean very low (<0.01) but not all zeros
		# Convert to highly right-skewed distn as fitted distribution may be
		# highly leptokurtic and not overlapping smaller values closer to
		# zero
		####################################################

		f.tr.mean <- mean(f.tr, na.rm=TRUE)
		b.tr.mean <- mean(b.tr, na.rm=TRUE)

		if ( f.finished==FALSE && f.tr.mean <0.1  ) {
			s1<-0.5
			s2 <- f.tr.mean
			f.tr <- pbeta(xs,s1, s2)	
			beta.tr.method <- smartpaste(beta.tr.method, "f.mean<0.01", sep=",")
			f.finished<-TRUE
		}
		if ( b.finished==FALSE && b.tr.mean <0.1  ) {
			s1<-0.5
			s2 <- b.tr.mean
			b.tr <- pbeta(xs,s1, s2)	
			beta.tr.method <- smartpaste(beta.tr.method, "b.mean<0.01", sep=",")
			b.finished<-TRUE
		}
			
		#########################################
		# (f) Single valued vectors on (0,1)
		#
		# Method:
		# 1.	Mean <=0.1: right-skewed beta dist with mean
		#		equal to original mean
		# 2. 	Mean >=0.9: left-skewed beta dist with mean
		#		equal to original mean
		# 3. 	The rest: add small amount of noise to allow fitting
		#########################################

		# Shape and sdev params for leptokurtic distn on
		# domain (0.1,0.9)
		sh1_mid <- 0.2
		sh2_mid <- 0.1
		sd_mid <- 0.01

		# Recalculate count of unique values
		len.u.f <- length( unique( f.tr ))
		len.u.b <- length( unique( b.tr ))

		# Focal singled-valued on (0,1)
		if ( f.finished==FALSE && len.u.f==1 ) {
			f.tr.mean <- mean(f.tr, na.rm=TRUE)
			if ( f.tr.mean <=0.1 ) {
				s1<-0.5
				s2 <- f.tr.mean
				f.tr <- pbeta(xs,s1, s2)	
				beta.tr.method <- smartpaste(beta.tr.method, "f single value<=0.1", sep=",")
			} else if ( f.tr.mean>=0.9 ) {
				s2 <- f.tr.mean
				s1<-0.01 + ( (1-f.tr.mean)*0.1 )
				f.tr <- pbeta(xs,s1, s2)	
				beta.tr.method <- smartpaste(beta.tr.method, "f single value>=0.9", sep=",")
			} else {
				# On domain (0.1,0.9)
				f.tr <- beta.normal( f.tr, mn= f.tr.mean, 
					sh1=sh1_mid, sh2=sh2_mid, sdev= sd_mid
				)
				beta.tr.method <- smartpaste(beta.tr.method, "f single value on (0.1,0.9)", sep=",")
			}
		}
		
		# Bm singled-valued on (0,1)
		if ( b.finished==FALSE && len.u.b==1 ) {
			b.tr.mean <- mean( b.tr, na.rm=TRUE)
			if ( b.tr.mean <=0.1 ) {
				s1<-0.5
				s2 <- b.tr.mean
				b.tr <- pbeta(xs,s1, s2)	
				beta.tr.method <- smartpaste(beta.tr.method, "b single value<=0.1", sep=",")
			} else if ( b.tr.mean>=0.9 ) {
				s2 <- b.tr.mean
				s1<-0.01 + ( (1-b.tr.mean)*0.1 )
				b.tr <- pbeta(xs,s1, s2)	
				beta.tr.method <- smartpaste(beta.tr.method, "b single value>=0.9", sep=",")
			} else {
				# On domain (0.1,0.9)
				b.tr <- beta.normal( b.tr, mn= b.tr.mean, 
					sh1=sh1_mid, sh2=sh2_mid, sdev= sd_mid
				)
				beta.tr.method <- smartpaste(beta.tr.method, "b single value on (0.1,0.9)", sep=",")
			}
		}
		
		# just to be safe
		f.tr <- beta.constrain( f.tr )
		b.tr <- beta.constrain( b.tr )
		
		#########################################
		# Calculate overlap (finally!)
		#########################################
	
		# Get overlap results
		ol.result <- overlap.beta(v1=f.tr, v2=b.tr, xs=xs, 
							shape1= shape1, shape2= shape2, lowlim=lowlim)
		q.ol <- ol.result$ol
		pdf.f <- ol.result$pdf1
		pdf.b <-  ol.result$pdf2
		#fit.fail <- ol.result$fit.fail
	}
	
	results <- list(
		f.mean, b.mean, diff, q.ol, pdf.f, pdf.b, beta.tr.method, f.tr, b.tr
	)
	names(results) <- c(
		"f.mean", "b.mean", "diff", "q.ol", "pdf.f", "pdf.b", "beta.tr.method", "f.tr", "b.tr"
	)
	
	return(results)
}

boot.qual.beta <- function( f, b, xs=NULL, 
	boot.reps=NULL, perm.reps=NULL, seed=NULL, set.seed=F, 
	shape1=0.1, shape2=0.1, lowlim=0.0001, 
	use.q.diff.raw=FALSE, discount.method="linear", buffer=0.01,
	wt.method="u.beta.buffered" , test.diff.boot=FALSE,
	q.tr.logit.inverse=TRUE, logit.inverse.beta=2
	) {
	###################################################
	# Calculates mean and bootstrap 95% CLs of quality for beta-
	# distribution vectors on interval [0,1] using mixed overlap- and
	# means-based quality algorithm
	#
	# Calculates quality based on both (a) difference of means and (b) 
	# overlap, and applies correction that weights overlap for means near
	# middle of distribution, and distance for means near limits (0 and 1)
	#
	# Parameters: 
	#		f					focal vectors
	#		b					benchmark vectors
	#		dist				distribution of the vectors c("Bet","NBin","gamma")
	#		boot.reps	[Optional] Bootstrap replicates; default=1000
	#		perm.reps	[Optional] Permutation iterations; default=10000
	#		use.q.diff.raw [Optional]	If FALSE, q.diff=1 when p.diff>=0.05, otherwiwse
	#													q.diff = actual means-based quality in all cases
	#		discount.method 	[Optional] Discount algorithm for difference-based
	#										quality. Default="linear". See params file for details.
	#
	# Returns list object:
	#		f.mean		observed mean of f
	# 		b.mean	observed mean of b
	#		d.obs		observed mean difference
	# 		q.obs		observed mean quality
	#		q.cls			bootstrap 95% CLs
	#		p.diff		p-value for perm test of H0: f.mean=b.mean
	# 		boot.q		bootstrapped qualities
	#		boot.reps	Number of replications used
	# 		boot.mean	Boostrap mean quality
	#		ol.w			overlap-vs-means test weighting factor
	#		pdf.f, pdf.b	The fitted f & b pdfs
	# 		xs				X values used to fit pdfs
	###################################################

	# Set default values for optional parameters & check for errors
	if (is.null(xs)) xs <- seq(0,1, length.out= 100)
	if (is.null(boot.reps)) 	boot.reps <- 1000
	if (is.null(perm.reps)) 	perm.reps <- 10000
	if ( (set.seed==T) && !(is.null(seed)) ) set.seed(seed)
	
	# Get observed quality
	# Note that perm test always performed (test.diff=T) even if q.diff.raw not reset
	q.list <- qual.beta( f=f, b=b, xs=xs, perm.reps= perm.reps, 
		test.diff=TRUE, use.q.diff.raw=use.q.diff.raw, 
		discount.method=discount.method, wt.method= wt.method, buffer=buffer,
		shape1= shape1, shape2= shape2, lowlim= lowlim  
		)
	
	f.mean <- q.list $f.mean
	b.mean <- q.list $b.mean
	diff <- q.list$diff
	diff.p <- q.list $diff.p
	q.diff <- q.list $q.diff
	q.diff.raw <- q.list $q.diff.raw
	q.ol <- q.list $q.ol
	q <- q.list $q
	ol.w <- q.list $ol.w
	pdf.f <- q.list$pdf.f
	pdf.b <- q.list$pdf.b
	
	# Transform indicator quality if required (see global params file
	q.raw <- q		# Save original value, even if don't change
	if ( q.tr.logit.inverse==TRUE ) {
		q <- q.inverse.logit(q, logit.inverse.beta)
	}

	# Set remaining values to NA 
	q.sd <- NA															# Bootstrap SD
	q.lcl <- NA
	q.ucl <- NA
	boot.q <- NA
	boot.mean <- NA
	
	# Get bootstrap qualities
	# Note that set.seed must be false
	if ( ol.w > 0 ) {
				
		i <- 0		
		boot.q <- sapply(1: boot.reps, function(i) {
			f.samp <- sample(f, length(f), replace =T )
			b.samp <- sample(b, length(b), replace =T )
	
			# get quality for this iteration
			# Note that perm. test for q.diff can be turned off by test.diff.boot
			q.list.boot <- qual.beta( f=f.samp, b=b.samp, xs=xs, perm.reps= perm.reps, 
				test.diff= test.diff.boot, use.q.diff.raw=use.q.diff.raw, 
				discount.method=discount.method, wt.method=wt.method, buffer=buffer,
				shape1= shape1, shape2= shape2, lowlim= lowlim  
				)
			q.boot <- q.list.boot$q
			if ( q.tr.logit.inverse==TRUE ) q.boot <- q.inverse.logit(q.boot, logit.inverse.beta)		
			q.boot
		} )
		#boot.q <- boot.q[!is.na(boot.q)]
		
		# Get 95% CLs of the bootstrapped differences 
		# Empirical bootstrap method
		boot.mean <- mean(boot.q, na.rm=T)
		boot.dev <- boot.q - boot.mean		# Deviations from bootstrap mean
		q.dev <- q + boot.dev					# Bootstrap mean deviances
		q.cls <- quantile(q.dev, c(0.025, 0.975), na.rm=T)		# bootstrap CLs
		q.sd <- sd(q.dev)															# Bootstrap SD
		q.lcl <- q.cls[[1]]
		q.ucl <- q.cls[[2]]
	} 
			
	results <- list(
		f.mean, b.mean, diff, diff.p, q.diff, q.diff.raw,
		q.ol, q.raw, q, q.sd, q.lcl, q.ucl, 
		boot.q, boot.reps, boot.mean, ol.w,
		pdf.f, pdf.b, xs
	)
	names(results) <- c(
		"f.mean", "b.mean", "diff", "p.diff", "q.diff", "q.diff.raw",
		"q.ol", "q.raw", "q", "q.sd", "q.lcl", "q.ucl", 
		"boot.q", "boot.reps", "boot.mean", "ol.w",
		"pdf.f", "pdf.b", "xs"
	)

	return( results )
}

boot.overlap.beta <- function( f, b, xs=NULL, 
	boot.reps=NULL, seed=NULL, set.seed=F, 
	shape1=0.1, shape2=0.1, lowlim=0.0001, 
	discount.method="linear", test.diff.boot=FALSE,
	q.tr.logit.inverse=TRUE, logit.inverse.beta=2
	) {
	###################################################
	# Calculates mean and bootstrap 95% CLs of quality for beta-
	# distribution vectors on interval [0,1], using overlap method only
	#
	# Includes correction for species case where one or both indicators are
	# all 0s or all 1s.
	#
	# Parameters: 
	#		f					focal vectors
	#		b					benchmark vectors
	#		dist				distribution of the vectors c("Bet","NBin","gamma")
	#		boot.reps	[Optional] Bootstrap replicates; default=1000
	#		perm.reps	[Optional] Permutation iterations; default=10000
	#		use.q.diff.raw [Optional]	If FALSE, q.diff=1 when p.diff>=0.05, otherwiwse
	#													q.diff = actual means-based quality in all cases
	#		discount.method 	[Optional] Discount algorithm for difference-based
	#										quality. Default="linear". See params file for details.
	#		q.tr.logit.inverse	Transform final indicator quality using q.logit.inverse fn?
	#		logit.inverse.beta	Steepness parameter for fn q.logit.inverse.
	#
	# Returns list object:
	#		f.mean		observed mean of f
	# 		b.mean	observed mean of b
	#		d.obs		observed mean difference
	# 		q.obs		observed mean quality
	#		q.cls			bootstrap 95% CLs
	#		p.diff		p-value for perm test of H0: f.mean=b.mean
	# 		boot.q		bootstrapped qualities
	#		boot.reps	Number of replications used
	# 		boot.mean	Boostrap mean quality
	#		ol.w			overlap-vs-means test weighting factor
	#		pdf.f, pdf.b	The fitted f & b pdfs
	# 		xs				X values used to fit pdfs
	###################################################

	# Set default values for optional parameters & check for errors
	if (is.null(xs)) xs <- seq(0,1, length.out= 100)
	if (is.null(boot.reps)) 	boot.reps <- 1000
	if ( (set.seed==T) && !(is.null(seed)) ) set.seed(seed)
	
	# Get observed quality
	q.list <- qual.beta.ol( f=f, b=b, xs=xs, 
		shape1= shape1, shape2= shape2, lowlim= lowlim  
		)
	
	f.mean <- q.list $f.mean
	b.mean <- q.list $b.mean
	diff <- q.list$diff
	q.ol <- q.list $q.ol
	q <- q.ol
	pdf.f <- q.list$pdf.f
	pdf.b <- q.list$pdf.b
	beta.tr.method <- q.list$beta.tr.method
	f.tr <- q.list$f.tr
	b.tr <- q.list$b.tr
	
	# Transform indicator quality if required (see global params file
	q.raw <- q		# Save original value, even if don't change
	if ( q.tr.logit.inverse==TRUE ) {
		q <- q.inverse.logit(q, logit.inverse.beta)
	}
	
	# Set remaining values to NA 
	q.sd <- NA															# Bootstrap SD
	q.lcl <- NA
	q.ucl <- NA
	boot.q <- NA
	boot.mean <- NA
	
	# Get bootstrap qualities
	# Note that set.seed must be false				
	i <- 0		
	boot.q <- sapply(1: boot.reps, function(i) {
		f.samp <- sample(f, length(f), replace =T )
		b.samp <- sample(b, length(b), replace =T )

		# get quality for this iteration
		# Note that perm. test for q.diff can be turned off by test.diff.boot
		q.list.boot <- qual.beta.ol( f=f.samp, b=b.samp, xs=xs,  
			shape1= shape1, shape2= shape2, lowlim= lowlim  
			)
		q.boot <- q.list.boot$q
		if ( q.tr.logit.inverse==TRUE ) q.boot <- q.inverse.logit(q.boot, logit.inverse.beta)		
		q.boot
	} )
	
	# Get 95% CLs of the bootstrapped differences 
	# Empirical bootstrap method
	boot.mean <- mean(boot.q, na.rm=T)
	boot.dev <- boot.q - boot.mean		# Deviations from bootstrap mean
	q.dev <- q + boot.dev					# Bootstrap mean deviances
	q.cls <- quantile(q.dev, c(0.025, 0.975), na.rm=T)		# bootstrap CLs
	q.sd <- sd(q.dev)															# Bootstrap SD
	q.lcl <- q.cls[[1]]
	q.ucl <- q.cls[[2]]
			
	results <- list(
		f.mean, b.mean, diff, 
		q.ol, q.raw, q, q.sd, q.lcl, q.ucl, 
		boot.q, boot.reps, boot.mean, 
		pdf.f, pdf.b, xs, beta.tr.method, b.tr, f.tr
	)
	names(results) <- c(
		"f.mean", "b.mean", "diff", 
		"q.ol", "q.raw", "q", "q.sd", "q.lcl", "q.ucl", 
		"boot.q", "boot.reps", "boot.mean", 
		"pdf.f", "pdf.b", "xs", "beta.tr.method", "b.tr", "f.tr"
	)

	return( results )
}

boot.qual.beta.fixed <- function( f, b.fixed, 
	boot.reps=NULL, perm.reps=NULL, use.q.diff.raw=FALSE, 
	discount.method="linear", test.diff.boot=FALSE,
	q.tr.logit.inverse=TRUE, logit.inverse.beta=2
 	)  {
	###################################################
	# Calculates overall quality for beta-distributed focal indicator 
	# vector (f) relatve to fixed benchmark value
	#
	# Vector f must be distributed on [0,1]. Runs permutation test
	# only if test.diff=T. 
	#
	# Returns list object: f.mean, b.fixed, diff, diff.p, q.diff, q.ol, q
	###################################################

	# Set defaults for optional parameters
	if ( is.null( boot.reps ) ) 	boot.reps <- 1000
	if ( is.null( perm.reps ) ) 	perm.reps <- 1000
	
	f.mean <- mean(f, na.rm=T)
	f.w <- min(f.mean, 1 - f.mean)		# Focal weight
		
	# get difference-based quality
	q.results <- qual.prop.fixed.b(f=f, b.fixed=b.fixed, perm.reps=perm.reps, 
		test.diff=TRUE, use.q.diff.raw=use.q.diff.raw, 
		discount.method=discount.method	
	)	
	q <- q.results $q
	q.diff.raw <- q.results $q.raw
	p <- q.results $p
	diff <- q.results $d.abs
	
	# Transform indicator quality if required (see global params file
	q.raw <- q		# Save original value, even if don't change
	if ( q.tr.logit.inverse==TRUE ) {
		q <- q.inverse.logit(q, logit.inverse.beta)
	}

	# Get bootstrap quality
	boot.fixed.q <- sapply(1: boot.reps, function(i) {
		f.samp <- sample(f, length(f), replace =T )
		q.results.boot <- qual.prop.fixed.b(f=f.samp, b.fixed=b.fixed	, 
			perm.reps=perm.reps, test.diff=test.diff.boot, 
			use.q.diff.raw=use.q.diff.raw, discount.method=discount.method	
		)	
		q.boot <- q.results.boot $q
		if ( q.tr.logit.inverse==TRUE ) q.boot <- q.inverse.logit(q.boot, logit.inverse.beta)		
		q.boot
	} )
		
	# Get 95% CLs of the bootstrap quality 
	boot.mean <- mean(boot.fixed.q, na.rm=T)
	boot.dev <- boot.fixed.q - boot.mean		# Deviations from bootstrap mean
	q.dev <- q + boot.dev								# Bootstrap mean deviances
	q.cls <- quantile(q.dev, c(0.025, 0.975), na.rm=T)		# bootstrap CLs
	q.sd <- sd(q.dev)															# Bootstrap SD
	q.lcl <- q.cls[[1]]
	q.ucl <- q.cls[[2]]
	
	results <- list(
		f.mean, b.fixed, diff, p, q.diff.raw,
		q.raw, q, q.lcl, q.ucl,
		boot.fixed.q, boot.reps, boot.mean
	)
	names(results) <- c(
		"f.mean", "b.fixed", "diff", "p", "q.diff.raw", 
		"q.raw", "q", "q.lcl", "q.ucl",
		"boot.q", "boot.reps", "boot.mean"
	)
	return( results )
}

qual.beta.fixed <- function( f, b.fixed, boot.reps=NULL, test.diff=NULL )  {
	###################################################
	# Calculates overall quality for beta-distributed focal indicator 
	# vector (f) relative to fixed benchmark value
	#
	# Vector f must be distributed on [0,1]. Runs permutation test
	# only if test.diff=T. 
	#
	# Returns list object: f.mean, b.fixed, diff, diff.p, q.diff, q.ol, q
	###################################################

	# Set defaults for optional parameters
	if ( is.null( boot.reps ) ) 	boot.reps <- 1000
	if ( is.null( test.diff ) ) 	test.diff <- TRUE
	
	f.mean <- mean(f, na.rm=T)
	f.w <- min(f.mean, 1 - f.mean)		# Focal weight
	diff <- f.mean - b.fixed
	
	# Get p value of H0: f.mean==b.fixed
	if (test.diff==TRUE) {
		test <- uni.test.boot(v=f, val=b.fixed, reps=boot.reps ) 
		p.boot <- test$p
	} else {
		# Assume difference is significant, setting p to < 0.05
		# Only use this option to speed up bootstrap
		p.boot <- 0.05
	}
	
	if (diff==0) p.boot <- 1		# Adjust for fact that bootstrap returns
												# erroneous p-value when f.mean=b.fixed
	
	# get difference-based quality
	qual <- qual.prop.fixed.b(f=f, b.fixed=b.fixed, test.diff=test.diff)	
	q <- unlist(qual[1])
	
	# No adjustment applied, so q.diff.raw=q (adjusted)
	q.diff.raw <- q
		
	results <- list(
		f.mean, b.fixed, diff, p.boot, q, q.diff.raw
	)
	names(results) <- c(
		"f.mean", "b.fixed", "diff", "p.boot", "q", "q.diff.raw"
	)

	return( results )
	
}

boot.cls <- function(x) {
	###################################################
	# Calculates (asymmetric) bootstrap 95% confidence limits of a 
	# vector
	#
	# Returns list object:
	# 		lcl 	Lower 95% confidence limit
	#		ucl	Upper 95% confidence limit
	#
	# Requires:
	#		Package simpleboot
	###################################################
	
	# Check required packages
	if ( !require(simpleboot) ) stop("ERROR: package 'simpleboot' not loaded")
	
	vec.mean <- mean(x, na.rm=T)

	if (length(unique(x))==1) {
		boot.lcl <- vec.mean
		boot.ucl <- vec.mean
	} else {
	
		# Get bootstrap confidence limits
		# Using simpleboot package
		x.boot = one.boot(x, mean, R=10000)
		boot <- boot.ci(x.boot, type="bca") 
		boot.lcl <- boot $bca[4]
		boot.ucl <- boot $bca[5]
	}

	results <- list(boot.lcl, boot.ucl)	
	names(results) <- c("boot.lcl", "boot.ucl")
	return(results)
}

uni.test.boot <- function(v, val, reps=NULL ) {
	###################################################
	# Performs bootstrap univariate means test of H0: mean(v)=val
	#
	# Accepts: vector v and fixed value val, and optional number of
	#		bootstrap replicates.
	#
	# Returns list object:
	#		v.mean		Mean of v
	# 		p				Two-tailed p-value
	#		lcl				Bootstrap lower 95% CL of v
	# 		ucl			Bootstrap upper 95% CL of v
	###################################################

	# Set default value
	if ( is.null( reps ) ) reps <- 1000
	
	n <- length(v)
	v.mean <- mean(v, na.rm=T)
	dev <- abs(v.mean - val)
	ulim <- v.mean + dev
	llim <- v.mean - dev
	bstrap <- c()
	
	for (i in 1:reps){
		v.samp <- sample(v, n, replace=T)
		bstrap <- c(bstrap, mean(v.samp))
	}
	
	p.H0 <- ( sum(bstrap < llim ) + sum( bstrap > ulim ) ) / reps
	
	cls <- boot.cls(v)
	lcl <- cls$boot.lcl
	ucl <- cls$boot.ucl
	
	result <- list( v.mean, p.H0, lcl, ucl)
	names(result) <- c( "v.mean", "p", "lcl", "ucl")
	return(result)
}

means.test.boot <- function( x, y, boot.reps=NULL ) {
	###################################################
	# Performs non-parametric bootstrap test of null hypothesis that
	# the means of two groups are the same
	#
	# Parameters: two vectors, x & y
	# Returns: 		p-value of test
	# Source: 		https://tinyurl.com/klzcjy4
	###################################################

	# Set boot.reps if not supplied
	if ( is.null(boot.reps) ) boot.reps <- 10000

	# sample from H0 separately, no assumption about equal variance
	pooled <- c(x, y)
	xt <- x - mean(x) + mean(pooled)
	yt <- y - mean(y) + mean(pooled)
	
	boot.t <- c(1: boot.reps)
	for (i in 1: boot.reps){
	  sample.x <- sample(xt,replace=TRUE)
	  sample.y <- sample(yt,replace=TRUE)
	  boot.t[i] <- t.test(sample.x,sample.y)$statistic
	}
	p.h0 <-  (1 + sum(abs(boot.t) > abs(t.test(x,y)$statistic))) / (boot.reps +1)  
	
	return( p.h0	)
}

means.test.perm <- function( x, y, iterations=NULL, seed=NULL ) {
	###################################################
	# Performs non-parametric permutation test of null hypothesis that
	# the means of two groups are the same
	#
	# Parameters: two vectors, x & y, and optional # of iterations for test
	# Returns: 		p-value of test
	# Source: 		https://tinyurl.com/klzcjy4
	###################################################

	if (!is.null(seed)) set.seed(seed)

	# Set iterations if not supplied
	if ( is.null(iterations) ) iterations <- 1000

	nx <- length (x)
	ny <- length (y)
	
	pool <- c(x,y)
	obs.diff.p <- mean (x) - mean (y)
	iterations <- 10000
	sampl.dist.p <- NULL
	
	for (i in 1 : iterations) {
	        resample <- sample (c(1:length (pool)), length(pool))
	
	        x.perm = pool[resample][1 : nx]
	        # Is this next one right or should it be (ny + 1)?
	        y.perm = pool[resample][(nx +1) : length(pool)]
	        sampl.dist.p[i] = mean (x.perm) - mean (y.perm) 
	}
	p.permute <- (sum (abs (sampl.dist.p) >= obs.diff.p) + 1)/ (iterations+1)
	
	return(p.permute)
}

mm.power <- function( df.pwr, pwr.target=NULL ) {
	###################################################
	# Fits a Michaelis-Menten power curve to a data frame of point 
	# power estimate (pwr) vs sample size (n). Returns the fitted power 
	# curve, plus estimated minimum sample size (n.min) needed to 
	# achieve target power. Assumes conventional power of 0.80 if
	# target power not supplied.
	#
	# Parameters:
	# 		df.pwr			Data frame with columns n (sample size) and 
	#							pwr (power)
	#		pwr.target	The target power (default: 0.80)
	#
	# Returns:
	#		df.p.pred		Data frame of predicted power curve over range of n
	#		n.min			Interpolated n at power=pwr.target
	# Requires package 
	###################################################

	if ( !require(drc) ) stop("ERROR: package 'drc' not loaded")
	
	if (is.null(pwr.target)) pwr.target <- 0.80
	
	model.drm <- drm (pwr ~ n, data = df.pwr, fct = MM.2())
	mm <- data.frame(n = seq(0, max(df.pwr $n), length.out = 1000))
	mm$pwr.fit <- predict(model.drm, newdata = mm)
	
	# Get n for target power 
	# pwr.fit.diff <- abs( pwr.target - mm$pwr.fit )
	# idx <- which.min(pwr.fit.diff)
	# n.min <- mm$n[idx]
	n.min <- x.pred( x=n, y=mm$pwr.fit, y.fixed= pwr.target)	
	
	results <- list(mm, n.min)
	names(results) <- c("mm.df.pwr.fit", "n.min")
	return(results)
}

plot.mm.power <- function( df.pwr, pwr.target=NULL, title.main=NULL, x.title=NULL, y.title=NULL ) {
	###################################################
	# Plot predicted (pwr) vs sample size (n), with power curve fit
	# using Michaelis-Menten equation. Add horizontal line for conventional
	# power (pwr.target) and minimum required sample size (n.min) at
	# y=pwr.target. Assumes conventional power of 0.80 if
	# target power not supplied.
	#
	# Parameters:
	# 		df.pwr			Data frame with columns n (sample size) and 
	#							pwr (power)
	#		pwr.target	The target power (default: 0.80)
	#		title.main 	Main title (default: none)
	# 		x.title				X axis title (default: "Sample size")
	#		y.title				Y axis title (default: "Power")
	#
	# Returns: ggplot object, ready for printing and saving
	###################################################

	if ( !require(drc) ) stop("ERROR: package 'drc' not loaded")
	if ( !require(ggplot2) ) stop("ERROR: package 'ggplot2' not loaded")
	
	if (is.null(pwr.target)) pwr.target <- 0.80	
	if (is.null(title.main)) title.main <- ""
	if (is.null(x.title)) x.title <- "Sample size"
	if (is.null(y.title)) y.title <- "Power"
	
	# Fit the MM curve 
	fit <- mm.power(df.pwr, pwr.target)
	df.pwr.fit <- fit$mm.df.pwr.fit			# df of n, pwr and pwr.fit
	n.target <- fit$n.min							# min n, @ target power
	
	# ---------------------------------------------
	# Plot the power curve
	# --------------------------------------------
	
	p <- ggplot(df.pwr.fit, aes(x = n, y = pwr)) +
		theme_bw() +
		xlab(x.title) +
		ylab(y.title) +
		ggtitle(title.main) +
		geom_point(alpha = 0.5) +
		geom_line(data = df.pwr.fit, aes(x = n, y = pwr.fit), colour = "red")
	  
	 # Add horizontal line at target power or precision
	 p <- p + geom_hline(aes(yintercept= pwr.target), colour="#BB0000", linetype="dotted")
	 
	n.axis.max <- max(df.pwr $n)
	if  ( n.target <= n.axis.max ) {
		# Add vertical line for n @ target power or precision
		p <- p + geom_vline(aes(xintercept=n.target), colour="#BB0000", linetype="dotted")
	 }
	 
	#theme with white background
	p <- p + theme_bw() 
	
	#eliminates background, gridlines, and chart border
	p <- p +  theme(
	  plot.background = element_blank(),
	  panel.grid.major = element_blank(),
	  panel.grid.minor = element_blank()
	 ) 	
	 
	 # Set text sizes
	p <- p + theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
    p <- p + theme(plot.title = element_text(size=22))
         
	# return the graph
	return(p)

}

power.logistic <- function( df.pwr, pwr.target=NULL ) {
	###################################################
	# Fits a logistic curve to a data frame of point 
	# power estimate (pwr) vs sample size (n). Returns the fitted power 
	# curve, plus estimated minimum sample size (n.min) needed to 
	# achieve target power. Assumes conventional power of 0.80 if
	# target power not supplied.
	#
	# Parameters:
	# 		df.pwr			Data frame with columns n (sample size) and 
	#							pwr (power)
	#		pwr.target	The target power (default: 0.80)
	#
	# Returns list object:
	#		n					Vector of sample sizes	
	#		pwr				Vector of point power predictions
	#		pwr.fit			Vector of fitted logistic power values
	#		pwr.target	Target power
	#		n.min			Interpolated n at power=pwr.target
	###################################################
	
	maxTries <- 100; # Max times to try fit before reporting error

	if (is.null(pwr.target)) pwr.target <- 0.80	
	pwr <- df.pwr$pwr
	n <- df.pwr$n
	
	a.start <- 1			# The asymptote, 1 by definition for power
	b.start  <- 0.5		# Starting value for growth rate
	
	# Starting value for inflection point
	# Try value of n near mean of pwr 
	diffs <- abs(pwr-mean(pwr))
	min.diffs <- min(diffs)
	idx <-  which(diffs== min.diffs)
	c.start <- n[idx][1]
	c.start.orig <- c.start
	
	# Fit the model
	fail <- F
	tries <- 0
	done <- F
	while (done==F) {
		tries <- tries + 1
		mod <- tryCatch(
						nls(pwr ~ a / ( 1 + exp( -b * ( n - c ) ) ), 
						start = list( a=a.start, b=b.start, c=c.start) ),
						error = function(cond) { return("FAIL") },
						warning = function(cond) {return("FAIL") }
					)
		if ( mod[1]=='FAIL' ) {
			if ( tries >= maxTries )  {
				fail <- T
				break
			} else if (tries <= maxTries/2) {
				# Try decreasing c.start
				c.start <- c.start - 0.2*(c.start - min(n))
			} else {
				# Try increasing c.start
				c.start <- c.start + 0.2*(max(n) - c.start )
			}
		} else {
			done <- T
		}
	}

	if (fail==T) {
		results<-'fail'
	} else {
		# Get the coefficients
		params <- as.list(coef(mod))
		
		# Get the predicted values
		pwr.fit <- params$a / ( 1 + exp( -params$b * ( n - params$c ) ) )
		
		# Get n for target power 
		# pwr.pred.diff <- abs( pwr.target - pwr.fit )
		# idx <- which.min(pwr.pred.diff)
		# n.min <- n[idx]
		n.min <- x.pred( x=n, y=pwr.fit, y.fixed= pwr.target)
	
		# plot(pwr.fit~n,type="l")
		# points(pwr~n)
		# abline(h = pwr.target, col="red", lwd=1, lty=2)
		# abline(v = n.min, col="red", lwd=1, lty=2)

		results <- list(n, pwr, pwr.fit, pwr.target, n.min)
		names(results) <- c("n", "pwr", "pwr.fit", "pwr.target", "n.min")
	}
	
	return(results)
}

x.pred <- function( x, y, y.fixed ) {
	###################################################
	# For vectors of y fitted to x, returns predicted value of x at a fixed
	# value of y
	###################################################

	y.diff <- abs( y.fixed - y )
	idx <- which.min(y.diff)
	x.pred <- x[idx]
	return(x.pred)
}

plot.power.logistic <- function(n, pwr, pwr.fit, e.size=NULL, pwr.target=NULL, n.target=NULL, title.main=NULL, x.title=NULL, y.title=NULL, p.text.pos=NULL, n.text.pos=NULL, e.size.text.pos.x=NULL, e.size.text.pos.y=NULL ) {
	###################################################
	# Plots predicted (pwr) vs sample size (n) with fit curve &
	# adds lines for target power (pwr.target) and n @ target power (n.min).
	# Uses conventional power of 0.80 if target power not supplied.
	# Calculates n.min if not supplied.
	#
	# Parameters:
	# 		n						Vector of sample size values
	#		pwr					Vector of predicted power values
	#		pwr.fit				Vector of fitted power values (logistic fit)
	#		e.size				Effect size (optional, for label)
	#		pwr.target		Target power (default: 0.80)
	#		n.target			Sample size at target power (calculated if missing)
	#		title.main			Title for graph (default: none)
	# 		x.title				X axis title (default: "Sample size")
	#		y.title				Y axis title (default: "Power")
	#		p.text.pos		Optional specification of x coordinate of left edge of
	#								label for target power line
	#		n.text.pos		Optional specification of x coordinate of left edge of
	#								label for n.target line
	#		e.size.text.pos.x		Optional specification of x coordinate of left edge of
	#								label for effect size
	#		e.size.text.pos.y		Optional specification of y coordinate of label for
	#								effect size
	#
	# Returns: ggplot object, ready for printing and saving
	###################################################

	if ( !require(ggplot2) ) stop("ERROR: package 'ggplot2' not loaded")
	
	# Convert the vectors to a data frame
	df.pwr.fit <- as.data.frame(list(n, pwr, pwr.fit))
	names(df.pwr.fit) <- c("n", "pwr", "pwr.fit")
	n <- df.pwr.fit$n
	pwr <- df.pwr.fit$pwr

	# Set default/calculated values
	if (is.null(e.size)) e.size <- ""
	if (is.null(title.main)) title.main <- ""
	if (! ( title.main=="" ) ) {
		if (!e.size=="") title.main <- paste0(title.main, " (ES=", e.size, ")" )
	}
	if (is.null(x.title)) x.title <- "Sample size"
	if (is.null(y.title)) y.title <- "Power"
	if (is.null(pwr.target)) pwr.target <- 0.80	
	if (is.null(n.target)) n.target <- x.pred( x=n, y=pwr.fit, y.fixed= pwr.target)
	if (is.null(p.text.pos)) 	{
		if ( n.target > min(n) + 0.75 * ( (max(n)) - (min(n)) ) ) {
			# Place legend at left of graph
			p.text.pos <- min(n) + 6
		} else {
			# Place legend at right of graph
			p.text.pos <- max(n) - 6
		}
	}
	if (is.null(n.text.pos)) {
		if ( n.target > min(n) + 0.75 * ( (max(n)) - (min(n)) ) ) {
			# Place legend to left of line
			n.text.pos <- n.target - 7
		} else {
			# Place legend to right of line
			n.text.pos <- n.target + ( 0.25 * ( max(n) - n.target ) )
		}
	}
	# if (is.null(e.size.text.pos.x)) e.size.text.pos.x <- max(n) - 0.2*(max(n-min(n)))
	# if (is.null(e.size.text.pos.y)) e.size.text.pos.y <- min(pwr) + 0.3*(max(pwr-min(pwr)))
	
	# ---------------------------------------------
	# Plot the power curve
	# --------------------------------------------
	
	p <- ggplot(df.pwr.fit, aes(x = n, y = pwr)) +
		theme_bw() +
		xlab(x.title) +
		ylab(y.title) +
		ggtitle(title.main) +
		geom_point(alpha = 0.5) +
		geom_line(data = df.pwr.fit, aes(x = n, y = pwr.fit), colour = "red")
		
	# Set y axis limit
	p <- p + scale_y_continuous(limits = c(0, 1))
	  
	# Add horizontal line at target power or precision & label it
	p <- p + geom_hline(aes(yintercept= pwr.target), colour="#BB0000", linetype="dotted")
	p.text <- paste0("Power=", pwr.target)
	p <- p + geom_text(aes(p.text.pos, pwr.target, label=p.text, vjust=-1, fontface="plain", family="sans"), size=5 )
		 
	n.axis.max <- max(df.pwr.fit$n)
	if  ( n.target <= n.axis.max ) {
		# Add vertical line for n @ target power or precision, and label it
		p <- p + geom_vline(aes(xintercept=n.target), colour="#BB0000", linetype="dotted")
		n.text <- paste0("N.min=", n.target)
		#n.text.y.pos <- min(pwr)+0.03
		n.text.y.pos <- 0.06
		p <- p + geom_text(aes(n.text.pos, n.text.y.pos, label = n.text, vjust = 2, fontface="plain", family="sans"), size=5 )
	 }
	 
	# # Add effect size label
	# if (!e.size=="") {
		# e.size.text <- paste0("Effect size=", e.size)
		# p <- p + geom_text(aes(
				# e.size.text.pos.x, 
				# e.size.text.pos.y, 
				# label = e.size.text, 
				# vjust = 2
			# ), size=6 )
	# }

	#theme with white background
	p <- p + theme_bw() 
	
	#eliminates background, gridlines, and chart border
	p <- p +  theme(
	  plot.background = element_blank(),
	  panel.grid.major = element_blank(),
	  panel.grid.minor = element_blank()
	 ) 	
	 
	 # Set text sizes
	p <- p + theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
    p <- p + theme(plot.title = element_text(size=14))
         
	# return the graph
	return(p)
}

plot.power.logistic.multi.ei <- function(df.pwr, 
	title.main=NULL, title.main.center=NULL, x.title=NULL, y.title=NULL, 
	p.text.pos=NULL, p.text.pos.x.increment=NULL,
	n.text.pos=NULL, e.size.text.pos.x=NULL, e.size.text.pos.y=NULL, 
	n.min.txt.pos.x.increment=NULL, n.min.txt.pos.y.increment=NULL,
	jitter.val =NULL, grayscale=NULL, no.fill=NULL,
	color.codes.manual=NULL, reverse.legend=FALSE
	) {
	###################################################
	# Plots predicted (pwr) vs sample size (n) with fit curve &
	# adds lines for target power (pwr.target) and n @ target power (n.min).
	# Uses conventional power of 0.80 if target power not supplied.
	# Calculates n.min if not supplied.
	#
	# Parameters:
	#		df.pwr				df of power analysis simulation results, should already
	#								be subsetted to single vegetation + EI only
	# 	From df.pwr:
	#		landcover		Vegetation/land cover class 
	#		EI						Indicator
	#		stratum			Stratum (if applicable)
	#		n.b					Total benchmark samples in original data
	#		n.f					Total focal samples in original data
	# 		n						Simulated sample sizes (n.b.sim=n.f.sim)
	#		e.size				Effect size used for simulation
	#		e.size.adj			
	#		e.size.min		Lowest of the two margins of error: min(mean-lcl, ucl-mean)
	#		pwr					Post hoc (observed) power of current simulation
	#		n.min				Sample size at target power (pwr.target), from logistic fit
	#		fit				Predicted power values from logistic fit
	#		expts				Number of runs (experiments) in this simulation
	#		boot.reps		Number of bootstrap replications used to calculate 95% CLs
	#		pwr.target		Target power (default: 0.80)
	# 	Passed directly
	#		title.main			Title for graph (default: none)
	#		title.main.center	Optional. Center the title. Default=FALSE=left just.
	# 		x.title				X axis title (default: "Sample size")
	#		y.title				Y axis title (default: "Power")
	#		p.text.pos		Optional specification of x coordinate of left edge of
	#								label for target power line
	#		p.text.pos.x.increment		Optional specification of horizontal displacement 
	#								of power line label, relative to default position, in values of x.
	#								Default=0. More intuitive than p.text.pos. Can be negative.
	#		n.text.pos		Optional specification of x coordinate of left edge of
	#								label for n.target line
	#		e.size.text.pos.x		Optional specification of x coordinate of left edge of
	#								label for effect size
	#		e.size.text.pos.y		Optional specification of y coordinate of label for
	#								effect size
	#		n.min.txt.pos.x.increment	Optional specification of horizontal displacement 
	#								of n.min	label to right of vertical n.min line; default=4
	#		n.min.txt.pos.y.increment		Optional specification of vertical displacement of
	#								n.min label
	#		jitter.val			Option. Amount of jitter to add to x and y using function 
	#								jitter(). Integer. See base function definition.
	#		color.codes.manual	Optional. Vector of color codes to apply to liness
	#								and legend. 
	#		reverse.legend	Optional. If NULL or FALSE use default sort order, if
	#								TRUE reverse the sort order DOESN'T WORK!
	# Returns: ggplot object, ready for printing and saving
	###################################################

	if ( !require(ggplot2) ) stop("ERROR: package 'ggplot2' not loaded")
	
	# Extract key variables and vectors
	df.pwr$e.size <- as.factor(df.pwr$e.size)	# Convert effect size to a factor

	pwr.target <- unique(df.pwr$pwr.target)
	if (length(pwr.target)>1) stop("ERROR: >1 value of target power (plot.power.logistic.multi.ei)")
	
	all.n <- df.pwr$n									# Simulated sample sizes
	n.max <- max(all.n)
	n.min <- min(all.n)
	e.sizes <- unique(df.pwr$e.size)		# Effect sizes used
	num.es <- length(e.sizes)					# Number of effect sizes
	n.mins <- unique(df.pwr$n.min)		# Minimum sample sizes at each effect size
	num.n.mins <- length(n.mins)			# Should be same as num.es

	# Set default/calculated values
	if (is.null(title.main)) title.main <- "Power vs. Sample size"
	if (is.null(x.title)) x.title <- "Sample size (n)"
	if (is.null(y.title)) y.title <- "Power"
	if (is.null(pwr.target)) pwr.target <- 0.80	
	if (is.null(jitter.val)) jitter.val <- 0
	if (is.null(grayscale)) grayscale <- FALSE
	if (is.null(no.fill)) no.fill <- FALSE
	if (is.null( n.min.txt.pos.x.increment )) n.min.txt.pos.x.increment <- 4
	if (is.null( n.min.txt.pos.y.increment )) n.min.txt.pos.y.increment <- 0.05
	if (is.null( p.text.pos.x.increment )) p.text.pos.x.increment <- 0
	if (is.null(reverse.legend)) reverse.legend <- FALSE

	# Set horizontal placement of text label for conventional power line
	if (is.null(p.text.pos)) 	{
		if ( min(n.mins, na.rm=TRUE) > min(all.n, na.rm=TRUE) + 0.75 * ( (max(all.n, na.rm=TRUE)) - (min(all.n, na.rm=TRUE)) ) ) {
			# Place legend at left of graph
			p.text.pos <- min(all.n) + 6
		} else {
			# Place legend at right of graph
			p.text.pos <- max(all.n) - 6
		}
	} 
	
	p.text.pos <- p.text.pos + p.text.pos.x.increment
	
	# Axis limits
	x.axis.max <- n.max
	x.axis.min <- n.min
	y.axis.max <- 1
	y.axis.min <- 0
	
	# Generate n.min text and set positions 
	df.pwr$n.min.txt <- paste0("n.min=", as.character(df.pwr$n.min))
	df.pwr$n.min.txt.pos.x <- df.pwr$n.min + n.min.txt.pos.x.increment		# To right of line
	df.pwr$n.min.txt.pos.y <- ( 1 / df.pwr$n.min*2 ) + n.min.txt.pos.y.increment 	# Vertical displacement
	
	# Set vector of manually-defined colour codes
	# Make sure this has enough elements to support the largest possible number
	# of effect sizes
	#color.codes.all <-as.character(c("blue", "red", "green", "orange" ))
	#color.codes.all <-as.character(c("blue4", "royalblue1", "lightskyblue", "salmon" ))
	if ( is.null(color.codes.manual) ) {
		color.codes.all <-as.character(c("coral", "royalblue1", "seagreen", "red" ))
		color.codes.all <-as.character(c("red", "royalblue1", "seagreen", "salmon" ))
		color.codes <- color.codes.all[1:num.es]		# Select the first num.es colors
	} else {
		# Use user-supplied color code vector
		color.codes <- color.codes.manual[1:num.es]		# Select the first num.es colors
	}
	
	# Manually defined shape codes
	# These are all filled with outline
	shape.codes.all <- c(21,22,23,24)
	shape.codes <- shape.codes.all[1:num.es]		# Select the first num.es colors

	# Set legend title
	legend.title <- "Effect size"
	
	# Add requested jitter if applicable
	if (! (is.null(jitter.val) || jitter.val==0)  ) {
		df.pwr$n <- jitter( df.pwr$n, jitter.val )
		df.pwr$pwr <- jitter( df.pwr$pwr, jitter.val )
	}

	# ---------------------------------------------
	# Plot the power curve
	# --------------------------------------------
	
	if (grayscale==TRUE) {
		p <- ggplot(df.pwr, aes(x = n, y = pwr, shape=e.size) ) +
			theme_bw() +
			xlab(x.title) +
			ylab(y.title) +
			ggtitle(title.main) +
			geom_point(alpha = 0.95) 
	} else {
		p <- ggplot(df.pwr, aes(x = n, y = pwr, colour=e.size, shape=e.size) ) +
			theme_bw() +
			xlab(x.title) +
			ylab(y.title) +
			ggtitle(title.main) +
			geom_point(alpha = 0.95) 	
					
		# Apply manual colour scale and set legend title 
		p <- p + scale_colour_manual(values=setNames(color.codes, e.sizes) )  +
		labs(colour= legend.title, linetype= legend.title , shape= legend.title ) 			
	}
	
	# Set legend sort order
	# DOESN'T WORK!
	p <- p + guides(fill = guide_legend(reverse= reverse.legend))
	
	if (no.fill==TRUE) {
		p <- p + scale_shape(solid = FALSE)
	}
			
	# Apply manual shapes 
	#p <- p + scale_shape_manual(values=shape.codes) 			

	# Add the logistic fit lines
	p <- p +geom_line(aes(y = fit), show.legend=FALSE )
	
	# Modify the size of the legend symbols
	p <- p + guides(colour = guide_legend(override.aes = list(size = 3)))+
  theme(legend.key=element_rect(fill=NA))		
  
	# Set axis limits, removing gutter
	p <- p + scale_x_continuous(limits = c(x.axis.min,x.axis.max) )
	p <- p + scale_y_continuous(limits = c(y.axis.min,y.axis.max) )
	  
	# Add horizontal line at target power or precision & label it
	p <- p + geom_hline(aes(yintercept= pwr.target), colour="black", linetype="dotted")
	p.text <- paste0("Power=", pwr.target)
	#p <- p + geom_text(aes(p.text.pos, pwr.target, label=p.text, vjust=-1, fontface="plain", family="sans"), size=5, colour="black" , show.legend=FALSE )	
	p <- p + geom_text(aes(p.text.pos-5, pwr.target, label=p.text, vjust=-1), size=5, colour="black" , show.legend=FALSE )	

	# Add vertical lines for n.min @ target power, and label them
	if (grayscale==TRUE) {
		p <- p + geom_vline(data = df.pwr,aes(xintercept = n.min), linetype = "dashed", show.legend=FALSE)
	} else {
		p <- p + geom_vline(data = df.pwr,aes(xintercept = n.min,colour = e.size), linetype = "dashed", show.legend=FALSE)	
	}
	
	# Label the lines
	leg.pos.y <- df.pwr$e.size
	p <- p + geom_text(data = df.pwr, 
		aes(
			x= n.min.txt.pos.x, 
			y= n.min.txt.pos.y+0.2, 
			label=n.min.txt, 
			vjust=0, 
			fontface="plain", 
			family="sans"
		), 
		size=4, 
		colour="black" , 
		show.legend=FALSE 
		)	

	#theme with white background
	p <- p + theme_bw() 
	
	#eliminates background, gridlines, and chart border
	p <- p +  theme(
	  plot.background = element_blank(),
	  panel.grid.major = element_blank(),
	  panel.grid.minor = element_blank()
	 ) 	
	 
	 # Set text sizes
	p <- p + theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="plain"))
    p <- p + theme(plot.title = element_text(size=16, face="bold"))
    p <- p + theme(legend.text=element_text(size=12))
    p <- p + theme(legend.title=element_text(size=14))
    
    if (title.main.center==TRUE) {
    	p <- p + theme(plot.title = element_text(hjust = 0.5))
    }
         
	# return the graph
	return(p)
}

plot.power.logistic.multi.veg <- function(df.pwr, no.legend=FALSE, n.min.minmaxonly=NULL, n.min.min.fixed=NULL, n.min.max.fixed=NULL, n.min.omit=FALSE, x.max.fixed=NULL, title.main=NULL, x.title=NULL, y.title=NULL, p.text.pos=NULL, n.text.pos=NULL, veg.text.pos.x=NULL, veg.text.pos.y=NULL, jitter.val =NULL ) {
	###################################################
	# Plots predicted (pwr) vs sample size (n) with fit curve for multi 
	# strata (vegetation types) at once. Adds lines for target power 
	# (pwr.target) and minimum sample size @ target power (n.min).
	# Can be configured to show only min and max lines for n.min.
	# Uses conventional power of 0.80 if target power not supplied.
	#
	# Parameters:
	#		df.pwr				df of power analysis simulation results, should already
	#								be subsetted to single vegetation + EI only
	# 	From df.pwr:
	#		landcover		Vegetation/land cover class ("veg")
	#		EI						Indicator
	#		stratum			Stratum (if applicable)
	#		n.b					Total benchmark samples in original data
	#		n.f					Total focal samples in original data
	# 		n						Simulated sample sizes (n.b.sim=n.f.sim)
	#		e.size				Effect size used for simulation
	#		e.size.adj			
	#		e.size.min		Lowest of the two margins of error: min(mean-lcl, ucl-mean)
	#		pwr					Post hoc (observed) power of current simulation
	#		n.min				Sample size at target power (pwr.target), from logistic fit
	#		fit				Predicted power values from logistic fit
	#		expts				Number of runs (experiments) in this simulation
	#		boot.reps		Number of bootstrap replications used to calculate 95% CLs
	#		pwr.target		Target power (default: 0.80)
	# 	Passed directly
	#		no.legend		Omit legend (avoids clutter if many veg types)
	#		n.min.minmaxonly	Plot smallest and largest n.min lines only
	#		n.min.min.fixed		Fixed value of n.min.min. If supplied overrides actual value from data
	#		n.min.max.fixed		Fixed value of n.min.max. If supplied overrides actual value from data
	#		n.min.omit		Don't plot n.min line(s)
	#		title.main			Title for graph (default: none)
	# 		x.title				X axis title (default: "Sample size")
	#		y.title				Y axis title (default: "Power")
	#		p.text.pos		Optional specification of x coordinate of left edge of
	#								label for target power line
	#		n.text.pos		Optional specification of x coordinate of left edge of
	#								label for n.target line
	#		veg.text.pos.x		Optional specification of x coordinate of left edge of
	#								label for effect size
	#		veg.text.pos.y		Optional specification of y coordinate of label for
	#								effect size
	#		jitter.val			Option. Amount of jitter to add to x and y using function 
	#								jitter(). Integer. See base function definition.
	#		x.max.fixed	Fixed max value of x axis. If NULL then max value of x used
	#
	# Returns: ggplot object, ready for printing and saving
	###################################################

	if ( !require(ggplot2) ) stop("ERROR: package 'ggplot2' not loaded")
	
	# Extract key variables and vectors
	df.pwr$landcover <- as.factor(df.pwr$landcover)	# Convert main grouping class to factor

	pwr.target <- unique(df.pwr$pwr.target)
	if (length(pwr.target)>1) stop("ERROR: >1 value of target power (plot.power.logistic.multi.ei)")
	
	all.n <- df.pwr$n									# Simulated sample sizes
	n.max <- max(all.n)
	n.min <- min(all.n)
	veggies <- unique(df.pwr$landcover)		# Effect sizes used
	num.es <- length(veggies)					# Number of effect sizes
	n.mins <- unique(df.pwr$n.min)		# Minimum sample sizes at each effect size
	num.n.mins <- length(n.mins)			# Should be same as num.es
	
	# Get min and max values of n.min
	n.min.min <- min(df.pwr$n.min)
	if (is.null(n.min.min.fixed)) {
		n.min.min <- min(df.pwr$n.min)
	} else {
		n.min.min <- n.min.min.fixed
	}
	n.min.min.text <- paste0("n.min=",n.min.min)
	if (is.null(n.min.max.fixed)) {
		n.min.max <- max(df.pwr$n.min)
	} else {
		n.min.max <- n.min.max.fixed
	}
	n.min.max.text <- paste0("n.min=",n.min.max)

	# Set default/calculated values
	if (is.null(title.main)) title.main <- "Power vs. Sample size"
	if (is.null(x.title)) x.title <- "Sample size (n)"
	if (is.null(y.title)) y.title <- "Power"
	if (is.null(pwr.target)) pwr.target <- 0.80	
	if (is.null(no.legend)) nolegend=FALSE
	if (is.null(n.min.minmaxonly)) n.min.minmaxonly =FALSE
	
	# Set horizontal placement of text label for conventional power line
	if (is.null(p.text.pos)) 	{
		if ( min(n.mins, na.rm=TRUE) > min(all.n, na.rm=TRUE) + 0.75 * ( (max(all.n, na.rm=TRUE)) - (min(all.n, na.rm=TRUE)) ) ) {
			# Place legend at left of graph
			p.text.pos <- min(all.n) + 6
		} else {
			# Place legend at right of graph
			p.text.pos <- max(all.n) - 6
		}
	}
	
	# Axis limits
	if (is.null(x.max.fixed)) {
		x.axis.max <- n.max
	} else {
		x.axis.max <- x.max.fixed
	}
	x.axis.min <- n.min
	y.axis.max <- 1
	y.axis.min <- 0
	
	if (!is.null(x.max.fixed)) {
		x.axis.max <- x.max.fixed
		p.text.pos <- x.axis.max - (0.05*x.axis.max)
	}
	
	# Generate n.min text and set positions 
	df.pwr$n.min.txt <- paste0("n=", as.character(df.pwr$n.min))
	df.pwr$n.min.txt.pos <- df.pwr$n.min + 3		# To right of line
	df.pwr$n.min.txt.pos[df.pwr$n.min.txt.pos > x.axis.max-6 && !is.na(df.pwr$n.min.txt.pos)] <- df.pwr$n.min.txt.pos[df.pwr$n.min.txt.pos> x.axis.max-6 && !is.na(df.pwr$n.min.txt.pos)] - 6
	
	# Set vector of manually-defined colour codes
	# Make sure this has enough elements to support the largest possible number
	# of effect sizes
	color.codes.all <-as.character(c("blue", "red", "green", "orange" ))
	#color.codes.all <-as.character(c("blue4", "royalblue1", "lightskyblue", "salmon" ))
	#color.codes.all <-as.character(c("coral", "lightskyblue2", "seagreen", "red" ))
	color.codes <- color.codes.all[1:num.es]		# Select the first num.es colors
	
	# Manually defined shape codes
	# These are all filled with outline
	shape.codes.all <- c(21,22,23,24)
	shape.codes <- shape.codes.all[1:num.es]		# Select the first num.es colors

	# Set legend title
	legend.title <- "Vegetation"
	
	# Add requested jitter if applicable
	if (!is.null(jitter.val) || jitter.val==0) {
		df.pwr$n <- jitter( df.pwr$n, jitter.val )
		df.pwr$pwr <- jitter( df.pwr$pwr, jitter.val )
	}

	# ---------------------------------------------
	# Plot the power curve
	# --------------------------------------------
	
	p <- ggplot(df.pwr, aes(x = n, y = pwr, colour=landcover) ) +
		theme_bw() +
		xlab(x.title) +
		ylab(y.title) +
		ggtitle(title.main) +
		geom_point(alpha = 0.95) 
			
	# Add the logistic fit lines
	p <- p +geom_line(aes(y = fit), show.legend=FALSE )

	if (no.legend==TRUE) {
		# Turn off legend
		p <- p + theme(legend.position = "none")
	} else {
		# Modify the size of the legend symbols
		p <- p + guides(colour = guide_legend(override.aes = list(size = 3)))+
	  theme(legend.key=element_rect(fill=NA))		
  	}
  
	# Set axis limits, removing gutter
	p <- p + scale_x_continuous(limits = c(x.axis.min,x.axis.max) )
	p <- p + scale_y_continuous(limits = c(y.axis.min,y.axis.max) )
	  
	# Add horizontal line at target power or precision & label it
	p <- p + geom_hline(aes(yintercept= pwr.target), colour="black", linetype="dotted")
	p.text <- paste0("Power=", pwr.target)
	p <- p + geom_text(aes(p.text.pos-1, pwr.target, label=p.text, vjust=-1), size=5, colour="black" , show.legend=FALSE )	

	# Add vertical lines for n.min @ target power, and label them
	if (n.min.minmaxonly==TRUE) {
		p <- p + geom_vline(aes(xintercept= n.min.min), colour="black", linetype="dotted")
		p <- p + geom_vline(aes(xintercept= n.min.max), colour="black", linetype="dotted")
	} else {
		p <- p + geom_vline(data = df.pwr,aes(xintercept = n.min,colour = landcover), linetype = "dashed", show.legend=FALSE)
	}
	
	# Label the n.min lines
		if (n.min.minmaxonly==TRUE) {
			p <- p + geom_text(aes(n.min.min+1.5, 0,  label= n.min.min.text, vjust=-1, fontface="plain", family="sans"), size=5, colour="black" , show.legend=FALSE )		
			p <- p + geom_text(aes(n.min.max+1.5, 0.1, label= n.min.max.text, vjust=-1, fontface="plain", family="sans"), size=5, colour="black" , show.legend=FALSE )		
		} else {
			p <- p + geom_text(data = df.pwr, aes(n.min.txt.pos, 0, label=n.min.txt, vjust=-1, fontface="plain", family="sans"), size=5, colour="black" , show.legend=FALSE )	
		}

	#eliminates background, gridlines, and chart border
	p <- p +  theme(
	  plot.background = element_blank(),
	  panel.grid.major = element_blank(),
	  panel.grid.minor = element_blank()
	 ) 	
	 
	 # Set text sizes
	p <- p + theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
    p <- p + theme(plot.title = element_text(size=16))
    if (no.legend==FALSE) {
	    p <- p + theme(legend.text=element_text(size=12))
	    p <- p + theme(legend.title=element_text(size=14))
	  }
         
	# return the graph
	return(p)
}


###############################################
###############################################
# Miscellaneous functions
###############################################
###############################################

unix.friendly <- function(sometext) {
	#################################
	# Turn text into a unix-friendly file name
	#################################

	# Make unix-friendly version of stratum for filename
	str <- gsub(' ', '_', sometext)
	str <- gsub('/', '_', str)
	str <- gsub(',', '', str)
	str <- gsub('\\(', '', str)
	str <- gsub('\\)', '', str)
	str <- gsub('_-_', '-', str) # Remove redundant underscores+hyphens
	str <- gsub("]", "", str)		# Remove square brackets
	str <- gsub("\\[", "", str)	# Remove square brackets
	friendly <- tolower(str)
	
	return(friendly)
}

human.readable <- function(str) {
	##############################
	# Teck-specific
	# Converts database land cover & 
	# vegetation codes to something more
	# readable
	##############################
	
	str2 <- gsub("v\\[", "", str)
	str2 <- gsub("]\\+s\\[", ", ", str2)
	str2 <- gsub("]\\+d\\[", ", ", str2)
	str2 <- gsub("]\\+r\\[", ", ", str2)
	
	return(str2)
}

human.readable.landcover <- function(str) {
	##############################
	# Teck-specific
	# Converts database land cover to 
	# something more readable
	##############################
	
	str2 <- gsub("\\+d\\[mine]\\+r\\[r.pl]", " (reclaimed)", str)
	str2 <- gsub("\\+r\\[r.pl]", " (reclaimed)", str2)
	str2 <- gsub("\\+d\\[d]", " (disturbed)", str2)
	str2 <- gsub("\\+d\\[mine]", " (disturbed)", str2)
	str2 <- gsub("v\\[", "", str2)
	str2 <- gsub("]\\+s\\[", ", ", str2)
	str2 <- gsub("\\(disturbed) \\(reclaimed)", "(reclaimed)", str2)
	
	return(str2)
}

human.readable.bm.veg <- function(str) {
	##############################
	# Teck-specific
	# Converts database benchmark 
	# vegetation codes to something more
	# readable
	##############################
	
	str2 <- gsub("v\\[", "", str)
	str2 <- gsub("]\\+s\\[", ", ", str2)
	
	return(str2)
}

reclaimed.make.pretty <- function(str) {
	##############################
	# Teck-specific
	# Converts reclamation codes to something
	# more readable
	##############################
	
	str2 <- gsub(", mine, r.pl", " (reclaimed)", str)
	str2 <- gsub(", mine, r", " (reclaimed)", str2)
	
	return(str2)
}

opt.grid <- function(n) {
	#################################
	# Set optimum grid for multipanel figure
	# subject to constraint that rows<=cols
	# and cols<=5
	# returns c & r as vector of length 2
	#################################

	done<-F
	for (c in 1:5) {
		low <- c-1
		for (r in low:c) { 	if (n <= c*r) { done<-T; break	} }
		if (done==T) break
	}
	dims <- c(c,r)
	return (dims)
}

fixZeros <- function(val) {
	#################################
	# Function for adjusting zero values
	# Accepts value, if value=0, add small amount
  # For single values only. Keeping for 
  # backwards-compatibility.
  # For adjusting entire vectors and data
  # frames, set dfAdj0(), below.
	#################################

	tinyVal <- 0.000001
	newVal <- val
	if (val==0) {
		newVal <- val + tinyVal
	}
		return (newVal)
}

specify_decimal <- function(x, k) {
	# Set fixed number of decimals
	if ( is.na(x) || is.null(x) ) {
		x.formatted <- x
	} else {
		x.formatted <- format(round(x, k), nsmall=k)
	}
	return( x.formatted )
}

logittr <- function(x) {
	#################################
	# logit transform for proportions
	# Accepts vector x, returns transformed vector of 
	# same length
	#################################

	minVal <- min(x[x>0])
	xtr <- x
	
	for(i in 1:length(x)){
		prop <- x[i]
		if (prop ==0 ) {
				xtr[i]<-log(minVal*0.000001)
			} else if (prop>=1)  { 
				xtr[i]<-0
		} else {
			xtr[i]<-log(prop/(1-prop))
		}
	}
	return(xtr)
}

convert.magic <- function(obj, type){
	##############################
	# For bulk-converting df data types
	##############################
	FUN1 <- switch(type,
                 character = as.character,
                 numeric = as.numeric,
                 factor = as.factor)
  	out <- lapply(obj, FUN1)
 	 as.data.frame(out)
}

trim <- function(str){
	##############################
	# Trims leading and trailing whitespace
	##############################
	str_trimmed <- gsub("^\\s+|\\s+$", "", str)
	return(str_trimmed)
}

move.col <- function(col.mv, col.next, n.cols){
	######################################
	# Rearranges vector of indexes, moving index 
	# 'col.mv' before index 'col.next', where n.cols is 
	# the total number of indexes. Assume indexes
	# are 1:n.cols. Use for rearranging columns
	# of data frames
	######################################
	
	# Make the original vector
	cols <- 1:n.cols
	
	# Get tne new positions
	cols.a <- c(col.mv, col.next)
	cols.b <- cols[which(! cols %in% cols.a)]
	cols.first <- cols.b[cols.b<col.next]
	cols.last <- cols.b[cols.b>col.next]
	
	# Assemble & return the new vector
	cols.new <- c( cols.first, col.mv, col.next, cols.last)
	return(cols.new)
}

gmean.orig <- function(x, na.rm=TRUE, na.weighted=FALSE){
  ######################################
  # Calculates geometric mean
  #
  # Original VQA version; retired 2026-02-11
  #
  # Based on:
  # https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
  # But with two critical differences:
  # NA, if not removed, always return NA for whole vector
  # O, if present, always returns 0 for entire vector
  #
  # Parameters:
  #   na.rm: if TRUE and NAs present, returns NA
  #   na.weighted: remove NA from calculation, but weight by 
  #     total observations, including NAs (in denominator)
  # 
  # IMPORTANT! For VQA, keep defaults to:
  # na.rm=TRUE, na.weighted=FALSE
  ######################################
  
  # Return NA right away if na.rm=FALSE and NAs present
  if ( !na.rm && any(is.na(x)) ) {
    return(NA)
  }
  
  # Return NaN if any negative numbers
  if(any(x < 0, na.rm=na.rm)){
    return(NaN)
  }
  
  if(any(x==0, na.rm=TRUE)){
    return(0)
  }

  if(na.weighted ){
    #exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x)) # Not needed; zeros handled above
    exp(sum(log(x), na.rm=na.rm) / length(x))
  } else {
    exp(mean(log(x), na.rm=na.rm))
  }
}

replace.last <- function(old, new, str) {
	####################################
	# Replaces last occurrence of pattern old 
	# in target string str with pattern new
	# Sequence of parameters same as for R base
	# function gsub
	####################################
	
	exp.old <- paste0( old, "([^", old, "]*)$" )
	exp.new <- paste0( new, "\\1")
	str.new <- sub( exp.old, exp.new, str)

	return( str.new )
}

gmean <- function(x, na.rm=TRUE, na.weighted=FALSE, no.zero=TRUE){
  ######################################
  # Calculates geometric mean
  #
  # Feature:
  # NA, if not removed, always return NA for whole vector
  # If no.zero=FALSE: any(x==0) returns 0 for entire vector 
  # If no.zero=TRUE: any(x==0) returns non-zero mean, 
  #   heavily down-weighted by low values of x.
  #
  # Parameters:
  #   na.rm: if FALSE and NAs present, returns NA
  #   na.weighted: remove NA from calculation, but weight by 
  #     total observations, including NAs (in denominator)
  # 
  # IMPORTANT! For VQA:
  # Use default na.rm=TRUE, na.weighted=FALSE, no.zero=TRUE
  ######################################
  
  # Return NA right away if na.rm=FALSE and NAs present
  if ( !na.rm && any(is.na(x)) ) {
    return(NA)
  }
  
  # Return NaN if any negative numbers
  if(any(x < 0, na.rm=na.rm)){
    return(NaN)
  }
  
  # if(any(x==0, na.rm=TRUE)){
  #   return(0)
  # }
  
  if(na.weighted ){
    if(any(x==0, na.rm=TRUE)) {
      return(0)
    } else {
      exp(sum(log(x), na.rm=na.rm) / length(x))
    }
  } else {
    if (no.zero) {
      # Modified geometric mean
      # Adds, then subtracts, small offset to prevent
      # one or more x=0 dropping all to zero
      offset <- 0.0001
      exp(mean(log(x + offset), na.rm=na.rm)) - offset
    } else {
      # Normal geometric mean
      # Returns zero if any one x=0
      if(any(x==0, na.rm=TRUE)) {
        return(0)
      } else {
        exp(mean(log(x), na.rm=na.rm))
      }
    }
  }
}

# Generalized Mean (Power Mean)
# p = 1: Arithmetic Mean
# p -> 0: Geometric Mean
# p = -1: Harmonic Mean
# For VQA, default p=0.3 provides performance similar to
# geometric mean, but with less severe penalty for low
# values and without dropping to zero if one value is 
# zero. Hence the default value of p
generalized_mean <- function(x, p=0.3) {
  if (p == 0) return(gmean(x, no.zero=FALSE))
  (mean(x^p))^(1/p)
}

df.reorder <- function(df=NULL, col.move=NULL, col.before=NULL, move.first=FALSE, move.last=FALSE) {
  ####################################
  # Moves column col.move of data frame df to
  # position after col.before. 
  # if move.first==TRUE, ignores col.before and 
  # moves col.move to first column of df. 
  # If move.last=TRUE, ignores both col.before
  # and move.first and move to last position
  ####################################
  
  # Extract data frame name for use in error messages
  df.name <- deparse(substitute(df))
  
  if (is.null(df))  stop("ERROR: parameter 'df' not supplied (fnc: df.cols.reorder)")
  if (is.null(col.move))  stop("ERROR: parameter 'col.move' not specified (fnc: df.cols.reorder)")
  if( ! col.move %in% colnames(df) ) {
    stop( paste0("ERROR: column '", col.move, "' not in data frame '", df.name, "', (fnc: df.cols.reorder)") )
  }
  
  if (move.first==TRUE) {
    df <- df[ ,c( which(colnames(df)==col.move), which(colnames(df)!=col.move) )]
  } else if (move.last==TRUE) {
    df <- df[ ,c( which(colnames(df)!=col.move), which(colnames(df)==col.move) )]
  } else {
    # Check for missing parameters
    if (is.null(col.before))  stop("ERROR: parameter 'col.before' not specified (fnc: df.cols.reorder)")
    if ( ! col.before %in% colnames(df) ) {
      stop( paste0("ERROR: column '", col.before, "' not in data frame '", df.name, "', (fnc: df.cols.reorder)") )
    }
    
    # Get index of column to move
    col.move_idx <- which(colnames(df)== col.move)
    
    # Get indexes of columns before and columns after
    col.before_idx <- which(colnames(df)== col.before)
    col.after_idx <- col.before_idx + 1
    cols.before_idx <- setdiff( (1:col.before_idx), col.move_idx )
    cols.after_idx <- setdiff( ( col.after_idx:ncol(df) ), col.move_idx )
    
    # Reorder columns
    df <- df[ , c( cols.before_idx, col.move_idx, cols.after_idx ) ]
  }
  return(df)
}

mylogit <- function(x){
	# logit transform
	log(x/(1-x))
}

noise.beta.norm <- function(v,n,sd){
	###########################################
	# Accepts beta-distributed vector, logit transforms,
	# adds random noise, then back-transforms
	#
	# v:		The vector
	# n:		Desired sample size of output vector
	# 			doesn't have to be same a input vector
	# sd: 	SD of the normal dist used to introduce noise
	###########################################

	1/(1+exp(-rnorm(n,mean=mylogit(v),sd=sd)))
}

beta.normal <- function( v, n=NULL, sh1=0.6, sh2=0.1, mn=NULL, sdev=1 ){
	###########################################
	# Accepts vector on (0,1) and converts to new beta 
	# distributed vector with new shape and mean. 	
	#
	# Parameters:
	# v:			The vector
	# n:			Desired sample size of output vector. Same as
	# 				original vector if not supplied
	# sh1: 		Beta shape parameter 1
	# sh2: 		Beta shape parameter 2
	# mn: 		Expected value of new vector. Uses arithmetic
	#				mean of original vector if not supplied. 
	# sdev: 	SD of the normal dist used to introduce noise
	###########################################

	# Check for illegal values
	if ( any(v<=0) || any(v>=1) ) {
		msg.err <- paste0("ERROR: Values outside domain (0,1) (func. beta.normal)")
		stop(msg.err)
	}

	require(VGAM)	# For betanorm functions
	
	if (is.null(mn)) mn=mean(v, na.rm=TRUE)
	# v.no.na <- v[!is.na(v)]
	# if (is.null(n)) n=length(v.no.na)
	if (is.null(n)) n=length(v)

	# Output new betanormal vector
	v2 <- rbetanorm(n, shape1=sh1, shape2=sh2, mean=mn, sd = sdev)
		
	#v2<- 1 / ( 1+exp(v2) )
		
	return(v2)
}

smartpaste <- function(..., sep=" ") {
	###########################################
	# Pastes together elements using delimiter sep.	
	# Differs from base function paste() in that delimiter is
	# omitted for elements that are empty strings
	# Similar to paste() in that single whitespace is appended
	# to delimiter. To omit padding, see smartpaste0().
	###########################################

	# Pad original delimiter
	sep <- paste0(sep," ")
	
	# Temporary delimiter to protect against edge case
	# where sep is also found in items to be concatenated
	delim <- "ExtremelyXXXUnlikelyYYYDelimiter"
	#delim <- paste0(delim," ")	
	
	# Convert elements to be concatenated to delimited string
	# Unlisting & converting to character allows mixing of strings
	# with numeric and/or character vectors in input
	v <- list(...)
	v <- unlist(v, use.names=FALSE, recursive=TRUE)
	v<- as.character(v)
	bad <- paste(v, collapse=delim)
	
	# strip all internal orphan delimiters by pairs
	better<-bad
	while ( grepl( paste0(delim,delim), better, fixed=TRUE ) ) {
		better <- gsub(paste0(delim,delim), delim, better)
	}

	# Remove initial orphan delimiter if any
	evenbetter <- sub(paste0("^",delim),"", better)
	
	# Remove trailing orphan delimiters if any
	best <- sub(paste0(delim,"$"), "", evenbetter)
		
	best <- gsub(delim, sep, best)
	
	return(best)
}

smartpaste0 <- function(..., sep="") {
	###########################################
	# Pastes together elements of vector v using delimiter sep.
	# Differs from base function paste0() in that delimiter is
	# omitted for elements that are empty strings
	# Similar to paste0() in that no whitespace is appended
	# to delimiter. To add padding, see smartpaste().
	###########################################
	
	# Temporary delimiter to protect against edge case
	# where sep is also found in items to be concatenated
	delim <- "ExtremelyXXXUnlikelyYYYDelimiter"
	
	# Unlisting & converting to character allows mixing of strings
	# with numeric and/or character vectors in input
	v <- list(...)
	v <- unlist(v, use.names=FALSE, recursive=TRUE)
	v<- as.character(v)
	bad <- paste(v, collapse=delim)
	
	# strip all internal orphan delimiters by pairs
	better<-bad
	while ( grepl( paste0(delim,delim), better, fixed=TRUE ) ) {
		better <- gsub(paste0(delim,delim), delim, better)
	}

	# Remove initial orphan delimiter if any
	evenbetter <- sub(paste0("^",delim),"", better)
	
	# Remove trailing orphan delimiters if any
	best <- sub(paste0(delim,"$"), "", evenbetter)
		
	best <- gsub(delim, sep, best)
	
	return(best)
}

is.num <- function(x) {
	###############################################
	# Tests if x is a single number (integer or float)
	# Returns false if non-numeric or null or NA
	# Complex objects (vectors, dfs, lists, etc) also return false
	# Combine with exists() if object might not exist and don't
	# want to throw error
	###############################################
	
	if ( length(x)==1 && is.numeric(x) ) {
		return(TRUE)
	} else {
		return(FALSE)
	}
	
}

################################################
# General graphing functions
################################################

plot.boxplot <- function(df, x.col, y.col, 
	x.lab=NULL, y.lab=NULL, fill.var=NULL, title.main=NULL,
	fill_manual=NULL
) {
	#############################################
	# Plot boxplot from data frame using ggplot with 
	# geom_boxplot
	#############################################

	# Set defaults
	if (is.null(x.lab)) x.lab <- x.col
	if (is.null(y.lab)) y.lab <- y.col
	if (is.null(fill.var)) fill.var <- x.col
	if (is.null(title.main)) title.main <- ""
	
	p <- ggplot(
			data=df, 
			aes(
				x= eval(parse(text=x.col)), y= eval(parse(text=y.col)), 
				fill= eval(parse(text=fill.var))
			) 
		) + geom_boxplot() +
	    theme(
	    	legend.position="none",
	    	plot.title = element_text(size=18)
	    ) +
	    ggtitle(title.main) +
	    xlab(x.lab) +
	    ylab(y.lab)
	    
	# Set color if requested
	if ( ! is.null( fill_manual ) ) {
		p <- p + scale_fill_manual(values=c(fill_manual))
	}
	
	# Set axis label format
	p <- p + theme(
		axis.title.x = element_text(face="bold", 	size=12),
        axis.text.x  = element_text(size=14)
    )
	p <- p + theme(
		axis.title.y = element_text(face="bold", 	size=12),
        axis.text.y  = element_text(size=12)
    )
   
    return(p)
}

plot.violin <- function(df, x.col, y.col, 
	x.lab=NULL, y.lab=NULL, fill.var=NULL, title.main=NULL,
	points.binwidth=NULL, fill_manual=NULL

) {
	#############################################
	# Plot violin plot from data frame using ggplot with 
	# geom_violin
	#############################################

	# Set defaults
	if (is.null(x.lab)) x.lab <- x.col
	if (is.null(y.lab)) y.lab <- y.col
	if (is.null(fill.var)) fill.var <- x.col
	if (is.null(title.main)) title.main <- ""
	if (is.null(points.binwidth)) points.binwidth <- 0.05
	
	p <- ggplot(
			data=df, 
			aes(
				x= eval(parse(text=x.col)), y= eval(parse(text=y.col)), 
				fill= eval(parse(text=fill.var))
			) 
		) +
	    geom_violin() +
	    theme(
	    	legend.position="none",
	    	plot.title = element_text(size=18)
	    ) +
	    ggtitle(title.main) +
	    xlab(x.lab) +
	    ylab(y.lab)
	   
	# Data points, no jitter
	p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3, binwidth = points.binwidth)
	
	# Mean and median
	p <- p + stat_summary(fun=mean, geom="point", size=3)
	p <- p + stat_summary(fun=median, geom="point", shape=23, size=3)	
    
	# Set color if requested
	if ( ! is.null( fill_manual ) ) {
		p <- p + scale_fill_manual(values=c(fill_manual))
	}
   
	# Set axis label format
	p <- p + theme(
		axis.title.x = element_text(face="bold", 	size=12),
        axis.text.x  = element_text(size=14)
    )
	p <- p + theme(
		axis.title.y = element_text(face="bold", 	size=12),
        axis.text.y  = element_text(size=12)
    )
   
    return(p)
}


save.plot <- function(p, p.dir, p.name, no.print=NULL) {
	##########################################
	# Echos plot to terminal and saves
	##########################################

	if ( is.null( no.print ) ) print(p)
	graph.file <- paste(p.dir, '/', p.name, ".png", sep="")
	print(paste('Printing graph: ',  p.name, sep=''))
	dev.copy(png, graph.file)
	dev.off()
}

plot.hist.single <- function(df, x.col, text.x=NULL, text.y=NULL, title.main=NULL, fill.val=NULL, no.bins=NULL, xlim.max=NULL ) {
	if ( is.null( title.main ) ) title.main<-""
	if ( is.null( fill.val ) ) fill.val <-"black"
	if ( is.null( no.bins ) ) title.main<-12
	if ( is.null( text.y ) ) text.y <-'Count'
	if ( is.null( text.x ) ) text.x <- x
	
	p <- ggplot( df, aes(x=eval(parse(text=x.col))) ) + 
    geom_histogram(color="black", fill=fill.val, bins=no.bins) +
	labs( title= title.main ) +
	ylab( text.y ) + 
	xlab( text.x )
	
	# X axis limits
	if ( !is.null(xlim.max) ) {
		p <- p + xlim(-3, xlim.max)
	}
	
	# Y axis limits
	p <- p + scale_y_continuous(
		expand = c(0,0),
        limits=c( 0, max( ggplot_build( p )$data[[1]]$count )*1.1 )
    )
	
	# Format title
	p <- p + theme( plot.title = element_text(size = 12, face = "bold",  hjust = 0.5) ) 
	
	# Remove background & gridlines, add axis lines
	p <- p + theme(
		panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(), 
		panel.background = element_blank(), 
		axis.line = element_line(colour = "black")
	)
	
	return(p)
}

plot.hist.comb <- function(df1, df2, x.col, title.main=NULL, text.x=NULL, text.y=NULL, 
	no.bins=12, xlim.min=NULL, xlim.max=NULL, ylim.max=NULL, proportions=NULL,
	x1.rgb=NULL, x2.rgb=NULL, alpha.val=NULL, print.legend=NULL,
	leg.pos=NULL, x1.leg.text=NULL, x2.leg.text=NULL, leg.inset=NULL ) 
	{
		
	###############################################
	# Plot histogram for two sets of x values, with overlap
	###############################################
			
	# Set defaults
	par(bg="white")
	if ( is.null( title.main ) ) title.main <- ""	# No main title
	if ( is.null( text.x ) ) text.x <- "X"
	if ( is.null( xlim.min ) ) xlim.min <- 0
	if ( is.null( no.bins ) ) no.bins <- 12
	if ( is.null( proportions ) ) proportions <- FALSE
	
	# Validate parameter proportions and set default 
	# values of freq and ylab parameters accordingly 
	if ( proportions ==TRUE ) {
		freq.val <- FALSE
		if ( is.null( text.y ) ) text.y <- "Frequency"
	} else if ( proportions ==FALSE ) {
		freq.val <- TRUE
		if ( is.null( text.y ) ) text.y <- "Count"
	} else {
		stop( "ERROR: Invalid parameter 'proportions'. (Fn: plot.hist.comb)")
	}
	
	# Color parameter defaults
	if ( is.null( x1.rgb ) ) x1.rgb <- c(0,0,1)		# Blue
	if ( is.null( x2.rgb ) ) x2.rgb <- c(1,0,0)	# Red
	if ( is.null( alpha.val ) ) alpha.val <- 0.2 		# Transparency, must be <.9 to see overlap

	# Color legend defaults
	if ( is.null( print.legend ) ) print.legend <- TRUE
	if ( is.null( leg.pos ) ) leg.pos <- 'topright'
	if ( is.null( x1.leg.text ) ) x1.leg.text <- 'x1'
	if ( is.null( x2.leg.text ) ) x2.leg.text <- 'x2'
	if ( is.null( leg.inset ) ) leg.inset <- c( 0.1, 0 )

	# Get largest value of x across both dfs
	x.max <- max( df1[[x.col]], df2[[x.col]] )
	
	# If null, set xlim.max based on max value of x
	if ( is.null( xlim.max ) ) {
		# Determine mid-point of largest x-axis bin
		xlim.max <- x.max  + ( x.max * 0.1 )
	}

	# Generate vector of x values over which y densities 
	# will be calculated, based on x.max and # of bins
	x.vals <- seq(0, x.max + ( x.max * 0.1 ), length.out= no.bins)
		
	# Generate histogram objects but don't print yet
	# Needed for setting y.axis upper limits
	h1 <- hist(df1[[x.col]], breaks = x.vals, plot=FALSE)
	h2 <- hist(df2[[x.col]], breaks = x.vals, plot=FALSE)
	
	# Save submitted ylim.max parameter
	ylim.max.submitted <- ylim.max
	
	# Set y axis upper limit to bin with highest count if not supplied by user
	highestCount <- max(h1$counts, h2$counts)
	ylim.max <- highestCount + (highestCount*0.4)
	
	# Rescale to proportions if requested & reset ylim.max accordingly
	if ( proportions ==TRUE ) {			
		# Get density distributions
		h1$density <- h1$counts / sum( h1$counts )
		h2$density <- h2$counts / sum( h2$counts )
		
		# y upper limit is bin with highest density 
		highestDensity <- max(h1$density, h2$density)
		ylim.max  <- min (1, highestDensity + 0.1)		
	}
	
	# Reset ylim.max to user-submitted parameter if applicable
	if ( !is.null( ylim.max.submitted ) ) ylim.max <- ylim.max.submitted
	
	# Plot the histograms
	plot(	h1,
	 	main=title.main,
	 	xlab=text.x, 
	 	ylab=text.y,
	 	col=rgb( x1.rgb[1], x1.rgb[2], x1.rgb[3], alpha.val ), 
	 	xlim=c( xlim.min, xlim.max ),
	 	ylim=c( 0, ylim.max ),
		freq=freq.val,
		add=FALSE
	)
	plot(	h2,
	 	main=title.main,
	 	xlab=text.x, 
	 	ylab=text.y,
	 	col=rgb( x2.rgb[1], x2.rgb[2], x2.rgb[3], alpha.val ), 
	 	xlim=c( xlim.min, xlim.max ),
	 	ylim=c( 0, ylim.max ),
		freq=freq.val,
		add=TRUE
	)
	
	# Print color legend if requested
	if ( print.legend==TRUE ) {
		legend('topright', 
			legend = c( x1.leg.text, x2.leg.text ),
			inset= leg.inset,
			fill= c( 
				rgb(x1.rgb[1], x1.rgb[2], x1.rgb[3], alpha.val), 
				rgb(x2.rgb[1], x2.rgb[2], x2.rgb[3], alpha.val)
			),
			box.lty=0
		)

	}
}

############################################
# Geospatial functions
############################################

is.valid.lat <- function(lat) {
	is.valid <- FALSE
	
	if ( suppressWarnings( ! is.na( as.numeric( as.character( lat ) ) ) ) ) {
		lat <- as.numeric( lat )
		
		if ( lat>=-90 & lat<=90 ) {	
			is.valid <- TRUE
		}
	}
	
	return( is.valid )
}

is.valid.lon <- function(lon) {
	is.valid <- FALSE
	
	if ( suppressWarnings( ! is.na( as.numeric( as.character( lon ) ) ) ) ) {
		lon <- as.numeric( lon )
		
		if ( lon>=-180 & lon <=180 ) {	
			is.valid <- TRUE
		}
	}
	
	return( is.valid )
}

gcd <- function(lat1, lon1, lat2, lon2) {
	# Great circle distance between two pairs of coordinates
	# Accepts: coordinate in decimal degrees
	# Returns: distance in m
	
	if ( is.valid.lat(lat1) && is.valid.lon(lon1) &&  is.valid.lat(lat2) && is.valid.lon(lon2) ) {
		R <- 6371000		# Radius of earth in meters
		
		# Convert degrees to radians
		lat1 <- lat1 * pi / 180
		lon1 <- lon1 * pi / 180
		lat2 <- lat2 * pi / 180
		lon2 <- lon2 * pi / 180
		
		dlon = lon2 - lon1
		dlat = lat2 - lat1
		a = sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
		c = 2 * asin( min( 1, sqrt(a) ) )
		d = R * c
		
		return(d)
	} else {
		return(NA)
	}
}

############################################
# Misc functions
############################################

stop_quietly <- function(exit.msg=NULL) {
	# Workaround for idiotic inability of R to stop without throwing error
	# Modified from: https://stackoverflow.com/a/42945293/2757825
	opt <- options(show.error.messages = FALSE)
	on.exit(options(opt))
	
	if ( is.null(exit.msg) ) {
		stop()
	} else {
		cat(exit.msg)
		stop()		
	}
}

quiet <- function(code){
	# Workaround for R's stupid lack of simple solution for
	# silently suppressing command output
	# Modified from: https://stackoverflow.com/a/49945753/2757825
	
	# Set correct null redirect for current operating system
	if ( .Platform$OS.type=="windows" ) {
		nr <- "NUL"
	} else {
		# unix (also works on mac os)
		nr <- "/dev/null"
	}
  
  sink(nr)
  tmp = code
  sink()
  return(tmp)
}

lm_eqn <- function(df){
	# Return lm regression object
	# Columns of df must be name x & y
    m <- lm(y ~ x, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
         list(a = format(unname(coef(m)[1]), digits = 2),
              b = format(unname(coef(m)[2]), digits = 2),
             r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
}

capFirst <- function(s) {
  # Capitalize first letter
  # s can be a df column
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

lowerFirst <- function(s) {
    paste(tolower(substring(s, 1, 1)), substring(s, 2), sep = "")
}

pasteNA <- function(...,sep=", ") {
	# Convert NA to empty string when paste-ing
	# Source: https://stackoverflow.com/a/15673180/2757825
	L <- list(...)
	L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
	ret <- gsub( paste0( "(^",sep,"|",sep,"$)" ), "", 
		gsub( paste0( sep,sep ),sep,
		do.call( paste,c( L, list( sep=sep ) ) )
		) )
	is.na(ret) <- ret==""
	ret
}

factors.to.chr <- function( df ) {
	# Convert all factors in data frame to character
	# Source: https://stackoverflow.com/a/2853231/2757825
	i <- sapply(df, is.factor)
	df[i] <- lapply(df[i], as.character)
	return(df)	
}

smart.concat <- function( v1, v2, delim=", ", empty.to.na=FALSE ) {
	# Concatenate two vectors, strings, or mix of strings and vectors
	# with delimiter, treating NA as "" and omitting delimiter
	# when one or both vectors are NULL, NA or ""
	# Source:
	# https://stackoverflow.com/a/49201394/2757825
	
	v3 <- apply(cbind(v1, v2), 1, function(x) paste(x[!is.na(x)], collapse = delim ))	
	if ( empty.to.na==TRUE )  v3[v3 %in% ""] <- NA
	return(v3)
}

pk.uniq <- function( df, pk.col ) {
	# Return TRUE if pk.col is unique throughout table, 
	# otherwise return FALSE
	if ( length(df[ , pk.col]) == length( unique( df[ , pk.col] ) ) ) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

ls.df <- function() {
	# List data frames only
	ls(envir=.GlobalEnv)[sapply(ls(envir=.GlobalEnv),function(t) is.data.frame(get(t)))]
}

#####################################################
# Spreadsheet functions
#
# For reading, writing and manipulating spreadsheet
# objects when using package openxlsx2
#####################################################

xlcolconv <- function(col){
  ################################################
  # Convert Excel letter column names to numbers,
  # and vice versa
  # Source: https://stackoverflow.com/a/52214227/2757825
  ################################################

  # test: 1 = A, 26 = Z, 27 = AA, 703 = AAA
  if (is.character(col)) {
    # codes from https://stackoverflow.com/a/34537691/2292993
    s = col
    # Uppercase
    s_upper <- toupper(s)
    # Convert string to a vector of single letters
    s_split <- unlist(strsplit(s_upper, split=""))
    # Convert each letter to the corresponding number
    s_number <- sapply(s_split, function(x) {which(LETTERS == x)})
    # Derive the numeric value associated with each letter
    numbers <- 26^((length(s_number)-1):0)
    # Calculate the column number
    column_number <- sum(s_number * numbers)
    return(column_number)
  } else {
    n = col
    letters = ''
    while (n > 0) {
      r = (n - 1) %% 26  # remainder
      letters = paste0(intToUtf8(r + utf8ToInt('A')), letters) # ascii
      n = (n - 1) %/% 26 # quotient
    }
    return(letters)
  }
}

xl.col.wj <- function( curr.wb, curr.ws, curr.df, 
  increment=2, row.start=1, row.end=500 ) {
  ##############################################################
  # Adaptively set column width and justification of Excel 
  # workbook sheet
  #
  # Set width to widest string (data or header) plus increment.
  # Set justification left for test, right for numbers.
  # Apply styling from row.start to row.end
  # Required objects: 
  #   wb: existing Excel workbook
  #   ws: existing worksheet in wb
  #   df: data frame
  # Dependencies: 
  #   Package "openxlsx2" 
  ##############################################################
  
  for ( i in 1:ncol(curr.df)) {
    # Get Excel column name for this index
    curr.col.xl <- xlcolconv(i)
    
    # Set col width to widest string (including header) plus increment
    max.chars.data <- max( nchar( curr.df[,i] ), na.rm=TRUE )
    chars.header <- nchar( colnames(curr.df)[i] )
    curr.col.max.chars <-  max( max.chars.data, chars.header, na.rm=TRUE )
    curr.col.wid <- curr.col.max.chars + increment
    
    # Apply column width setting
    curr.wb$set_col_widths( sheet=curr.ws, cols=c(i), widths=c(curr.col.wid) )
    
    # Set justification: left for text, right for numeric data
    if ( suppressWarnings( any( is.na(as.numeric(curr.df[,i])) ) ) ) {
      # Character; left justify
      cmd <- paste0("curr.wb$add_cell_style( sheet='", curr.ws, "', dims='", curr.col.xl, row.start, ":", curr.col.xl, row.end, "', horizontal='left' )")
    } else {
      # A number; right justify
      cmd <- paste0("curr.wb$add_cell_style( sheet='", curr.ws, "', dims='", curr.col.xl, row.start, ":", curr.col.xl, row.end, "', horizontal='right' )")
    }
    eval(parse( text=cmd ))
  }
}

gmean.fix0 <- function(vec, tinyamt=0.000001) {
  ################################################
  # Corrects exact zeros in a vector by adding
  # a very small number. Use to prepare a vector
  # before calculating geometric mean.
  # Geometric mean fails if any value=0
  ################################################
  
  if ( !is.vector(vec, mode="numeric") ) {
    stop("Function vec_fix_exact_zeros() failed: object is not a numeric vector!\n")
  }

  for ( i in length(vec) ) {
    x <- vec[i]
    if ( x==0 ) vec[i] <- tinyamt
  }
  
  return(vec)
} 

df.err.save <- function( df.err, err.file.name, err.file.path, msg.err=NULL, msg.action=NULL ) {
  ####################################################
  # Save df of error information to CSV file and echo
  # latest error detected (msg.err) and action taken 
  # (msg.action) in response to error. 
  # Messages suppressed by setting message text to NULL
  ####################################################
  if ( !is.null( msg.err) ) cat( msg.err, "\n" )
  if ( !is.null( msg.action) ) cat( msg.action, "\n" )
  err.file <- paste0(err.file.path, err.file.name)
  write.csv( df.err, file=err.file, row.names=FALSE)
}

sorensen <- function(spp1, spp2) {
  # Calculate Sorensen overlap index between
  # two sets of species
  A <- length(spp1)
  B <- length(spp2)
  shared <- intersect(spp1, spp2)
  C <- length(shared)
  sorensen.index <- (2 * C) / (A + B)
  return(sorensen.index)
}

input_file_list <- function(
  df.landcover.name=DF.LANDCOVER, df.plotmetadata.name=DF.PLOTMETADATA, 
  df.species.name=DF.SPECIES, ei.vec=EI.vec
  ) {
  ###########################################################
  # Prepare vector of required input files for this assessment
  #
  # * Prepare vector of standard VQA input data frames to be
  # generated for this project/assessment.
  # * Input options are general parameters set in params.R, and
  # should always be present
  # * Use df.input.list to test if required input files exist.
  ############################################################
  
  # These dfs are always required
  df.input.list.a <- c(df.landcover.name, df.plotmetadata.name, df.species.name)
  
  # The remainder vary by project & assessment
  for (i in 1:length(ei.vec)) {
    curr.ei <- ei.vec[i]
    df.input <- ei.params(curr.ei)$df.input
    if ( i==1 ) {
      df.input.list.b <- df.input
    } else {
      df.input.list.b <- c(df.input.list.b, df.input)
    }
  }
  df.input.list.b <- unique(df.input.list.b)
  df.input.list <- c(df.input.list.a, df.input.list.b)
  
  return(df.input.list)
}

input_file_list_all <- function(
  df.landcover.name=DF.LANDCOVER, df.plotmetadata.name=DF.PLOTMETADATA, 
  df.species.name=DF.SPECIES, ei.vec=EI.vec
  ) {
  ###########################################################
  # Prepare list of vectors or required input files for this assessment
  #
  # * Similar to input_file_list(), but prepares a list of 
  #   three vectors:
  #   [1] df.input.list.a: essential input files (always required)
  #   [2] df.input.list.b: assessment-specific input files
  #   [3] df.input.list: df.input.list.a & df.input.list.b combined
  ############################################################
  
  # These dfs are always required
  df.input.list.a <- c(df.landcover.name, df.plotmetadata.name, df.species.name)
  
  # The remainder vary by project & assessment
  for (i in 1:length(ei.vec)) {
    curr.ei <- ei.vec[i]
    df.input <- ei.params(curr.ei)$df.input
    if ( i==1 ) {
      df.input.list.b <- df.input
    } else {
      df.input.list.b <- c(df.input.list.b, df.input)
    }
  }
  df.input.list.b <- unique(df.input.list.b)
  df.input.list <- c(df.input.list.a, df.input.list.b)
  
  # Compile the final list
  df.input.list.comb <- list(
    df.input.list.a = df.input.list.a, 
    df.input.list.b = df.input.list.b, 
    df.input.list = df.input.list
    )
  
  return(df.input.list.comb)
}

transform_column <- function(column, mapping_vector) {
  ####################################################
  # Transform 0, 1 or more elements in a column by 
  # matching to a mapping vector of the form:
  # mapping_vector <- c(
  #   "XXX" <- "YYY",
  #   "apples" <- "oranges"
  # )
  # Column elements that do not match are not changed
  ####################################################

  # Find column elements which exist in mapping_vector
  matches <- column %in% names(mapping_vector)
  
  # Replace only the matching elements
  column[matches] <- mapping_vector[column[matches]]
  
  return(column)
}
