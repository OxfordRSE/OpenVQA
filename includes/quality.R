########################################################
# Calculate quality based on distance and overlap
#
# The following parameters are inherited from calling script:
#		curr.EI
#		EI.name
#		distn
#		RESULTSDIR (from params file)
#		resultsFocal Name of focal results summary file for all EIs
#		test.tail		Tail to be compared to benchmark; same for entire EI
#		distn			Code for distribution
#		q.method		Benchmark quality scoring method:
#		bm.val			Benchmark value (fixed method only)
########################################################

cat("**************************\n")
cat ("Calculating quality\n")
cat("**************************\n")

if ( exists("qual") ) rm(qual)

############################################
# For development only
# --> Comment out for production
############################################

# # Reload parameters and functions (development only)
# source("params.R", local=TRUE)
# source(paste(INCLUDESDIR, "functions.R", sep=''), local=TRUE)

# # Fixed values for testing without loop
# j<- 1   	#curr.lc
# k<-1		#curr.stratum

############################################
# END For development only
###########################################

############################################
# Parameters 
############################################

has.data <- TRUE

# Names of bootstrap & pdf files
boot.q.filename <- paste0(curr.EI, '_boot.q', '.csv')	# quality bootstrap runs
pdf.f.filename <- paste0(curr.EI, '_pdf.f', '.csv')		# focal pdfs
pdf.b.filename <- paste0(curr.EI, '_pdf.b', '.csv')		# bm pdfs
pdf.xs.filename <- paste0(curr.EI, '_pdf.xs', '.csv')	# x values for both pdfs

# Summary file of parameters for the current EI
# Used by later scripts that summarize VQA results
file.ei.summary <- paste0( RESULTSDIR, FILENAME.EI.SUMMARY ) 

############################################
# Start 
############################################

graphics.off()

#################################################
# Load raw data for this EI & prepared focal results df
#################################################

# Load raw data and order by stratum
rawFileName <- paste(curr.EI, "_raw.csv", sep="")
rawFile <- paste(RESULTSDIR, rawFileName, sep="")
dat.EI <- read.csv(rawFile, header=T, stringsAsFactors=FALSE)
dat.EI <- with(dat.EI, dat.EI[order(dat.EI$landCover, dat.EI$vegClass,	
	dat.EI$focalOrBenchmark, dat.EI$plotCode, dat.EI$stratum),])

# Load prepared focal results data frame
# Add columns for overlap results
f.results.file <- paste(RESULTSDIR, resultsFocal, sep="")
f.results.raw <- read.csv(f.results.file, header=T, stringsAsFactors=FALSE)
f.results <- f.results.raw

#################################################
# Set pdf x values for this EI
#################################################

max.ei <- max(dat.EI$EI)

if (distn=="Bet") {
	xs <- seq(0,1, length.out= 100)	
	xs[1]<-0.0001
	xs[100]<-0.9999
} else if ( distn=='NBin') {
	xs <- seq(0, max.ei + 10)
} else if (distn=='gamma') {
	xs <- seq(0, max.ei, length.out=1000)
}

#################################################
# Prepare data frames to hold bootstrap quality runs, focal pdfs &
# benchmark pdfs. Data columns are added later to pdf dfs, after
# value of xs has been set
#################################################

# Bootstrap quality 
boot.q.df <- data.frame(
	landCover =character(),
	stratum=character(),
	boot.reps=integer(),
	q.obs=double()
	)
boot.cols <- data.frame(matrix(NA, nrow = 0, ncol = boot.reps))
boot.cols <- convert.magic(boot.cols, "numeric")
boot.q.df <- cbind(boot.q.df, boot.cols)

# Number of data columns for all pdf dfs
pdf.cols <- data.frame(matrix(NA, nrow = 0, ncol = length(xs)))
pdf.cols <- convert.magic(pdf.cols, "numeric")

# Focal pdfs (observed)
pdf.f.df <- data.frame(
	landCover =character(),
	stratum=character(),
	f.mean= double()
	)
pdf.f.df <- cbind(pdf.f.df, pdf.cols)

# Bm pdfs
# Will repeate for landcover and strata sharing the 
# same bm vegetation, but having the same # of 
# rows as pdf.f makes plotting easier
pdf.b.df <- data.frame(
	landCover =character(),
	stratum=character(),
	b.mean= double()
	)
pdf.b.df <- cbind(pdf.b.df, pdf.cols)

# Throw the xs into a data frame as well
pdf.xs.df <- data.frame(pdf.cols)
pdf.xs.df[1,]<-xs

##########################################################
# Loop through vegetation and strata (if applicable), fitting the requested 
# distribution and saving parameters. Plot and save the figures
##########################################################

# get land cover classes
all.lc <- unique(dat.EI$landCover[ dat.EI $focalOrBenchmark=='f' ])
n.lc <- length(all.lc)

for (j in 1:n.lc) {		# START landCover loop
#for (j in 1:1) {				# Alternative for testing with loop
	curr.lc <- all.lc[j]
	
	cat('\n')
	#cat('************************************************************\n')
	cat( paste0('Current land cover: ', curr.lc, '\n' ))

	# Get focal plots for this land cover
	EI.lc <- subset(dat.EI, dat.EI$focalOrBenchmark=='f' & dat.EI$landCover == curr.lc )
	
	# Get bm plots for current benchmark vegetation
	curr.veg <- unique(EI.lc$vegClass)
	EI.lc.b <- subset(dat.EI, dat.EI$focalOrBenchmark=='b' & dat.EI$vegClass==curr.veg )
	
	# get strata
	strata <- unique(EI.lc $stratum)
	n.strata <- length(strata)
	
	for (k in 1:n.strata) {		# START strata loop
	#for (k in 1:1) {					# Alternative for testing with loop
	
		# Get subset for this stratum and vegetation type, benchmark only
		curr.stratum <- strata[k]
		EI.strat.f <- subset(EI.lc, EI.lc$stratum==curr.stratum)
		EI.strat.b <- subset(EI.lc.b, EI.lc.b$stratum==curr.stratum)
		cat('------------------------------------------------------------\n')
		if ( ! curr.stratum=='nostrata' ) cat(paste0('    Stratum: ', curr.stratum, '\n'))		
		cat("    Indicator: ", curr.EI, "\n", sep="")

		# These should already be either f or b, but just in case...
		f <- as.vector(EI.strat.f$EI[ EI.strat.f$focalOrBenchmark=='f' ])
		b <- as.vector(EI.strat.b$EI[ EI.strat.b$focalOrBenchmark=='b' ])
		
		# Get focal and bm vectors for current vegetation and EI
		fb.pooled <- c( f, b )

		f.hat <- mean(f, na.rm=T)
		b.hat <- mean(b, na.rm=T)
		fb.diff <- f.hat - b.hat

		# Check if data for this stratum
		has.data <- T
		if ( length(f) == 0 || length(b) == 0 ) {
			has.data <- F
			
			if ( length(f) == 0 && length(b) == 0 ) {
				fb.missing <- "focal & benchmark data missing"
			} else if (  length(f) == 0 ) {
				fb.missing <- "focal data missing"
			} else if ( length(b)==0 ) {
				fb.missing <- "benchmark data missing"
			}
			
			msg.stratum <- ""
			if  ( !curr.stratum=="nostrata") msg.stratum <- paste0("; ", curr.stratum)
			err.msg <- paste0("ERROR: ", fb.missing, " (", curr.lc, msg.stratum, ")")
			
			stop ( err.msg )
		}
		
		#####################################
		# Set distribution fitting starting parameters
		#####################################
		
		if (distn=="Bet") {
			lowlim <- 0.0000001		
			shape1 <- 0.1
			shape2 <- 0.1
			#xs <- seq(0,1, length.out= 100)	
		} else if ( distn=='NBin') {
			#xs <- xs_nbin <- seq(0,max(max(f), max(b)) + 10)
		} else if (distn=='gamma') {
			lowlim <- 0.0000001		
			uplim <- max(fb.pooled) + ( 0.1 * max(fb.pooled) )
			shape <- 1
			rate <- 1
			#xs <- seq(0, max( max(f),max(b) ), length.out=52)
		}

		#####################################
		# Calculate 	quality
		#####################################
			
		# Reset model results variables to prevent
		# from persisting across iterations
		p.diff <- NA
		q.diff <- NA
		q.diff.method <- NA
		q.wt.method <- NA
		q.diff.raw <- NA
		ol <- NA
		ol.lcl <- NA
		ol.ucl <- NA
		ol.w <- NA
		q <- NA
		q.raw <- NA
		q.obs <- NA
		q.lcl <- NA
		q.ucl <- NA
		boot.q.curr <- NA
		pdf.f.curr <- NA
		pdf.b.curr <- NA
		f.tr <- ""	
		b.tr <- ""		
		
		if (distn=="Bet") {	# START distn=="Bet"
			if (q.method=='fixed') {
				# Adjust for fixed bm
				b.hat <- bm.val
				fb.diff <- f.hat - bm.val

				# use fixed benchmark value method
				qual <- boot.qual.beta.fixed(f=f, b.fixed=bm.val, boot.reps=boot.reps, 
					perm.reps=perm.reps, use.q.diff.raw=use.q.diff.raw, 
					discount.method=discount.method, test.diff.boot=test.diff.boot,
					q.tr.logit.inverse= q.tr.logit.inverse, logit.inverse.beta= logit.inverse.beta   
				)

				# Extract the results
				q.diff.raw <- qual$q.diff.raw		# raw means-based quality
				q.diff <- qual$q							  # p-adjusted means-based quality
				p.diff <- qual$p							  # means test p-value
				ol <- NA
				ol.lcl <- NA
				ol.ucl <- NA
				q <- qual$q					# quality same as diff-based quality
				q.raw <- qual$q.raw					
				q.lcl <- qual$q.lcl
				q.ucl <- qual$q.ucl
				ol.w <- 0						# No overlap component
				boot.q.curr <- qual$boot.q
				pdf.f.curr <- NA			# Not calculated for fixed bm
				pdf.b.curr <- NA			# Not applicable to fixed bm
			} else if (beta.algorithm=='mixed') { 
				# Empirical benchmarking using mixed means/overlap-based quality
			  # This should be the default empirical beta method
				qual <- boot.qual.beta( f=f, b=b, xs=xs, 
					boot.reps= boot.reps, seed= seed, set.seed= set.seed, 
					shape1= shape1, shape2= shape2,	lowlim= lowlim, 
					use.q.diff.raw=use.q.diff.raw, discount.method=discount.method, 
					wt.method=wt.method, buffer=buffer,
					test.diff.boot=test.diff.boot ,
					q.tr.logit.inverse= q.tr.logit.inverse, logit.inverse.beta= logit.inverse.beta  
				)		
	
				# Extract the results
				q.diff.raw <- qual$q.diff.raw		# raw means-based quality
				q.diff <- qual$q.diff					  # adjusted means-based quality
				q.diff.method <- discount.method	# Means-based quality discount method
				q.wt.method <- wt.method		    # Means vs overlap weighting method
				p.diff <- qual$p.diff					  # means test p-value
				ol <- qual$q.ol
				ol.lcl <- NA
				ol.ucl <- NA
				q <- qual$q
				q.raw <- qual$q.raw					
				q.lcl <- qual$q.lcl
				q.ucl <- qual$q.ucl
				ol.w <- qual$ol.w		# Relative weighting factor, overlap vs. distance quality
													# 1=all overlap, 0=all distance.
				boot.q.curr <- qual$boot.q
				pdf.f.curr <- qual$pdf.f
				pdf.b.curr <- qual$pdf.b				
			} else if (beta.algorithm=='overlap') { 
				# Empirical benchmarking, overlap-based quality only
			  # No longer recommended due to poor fitting for means close to 0 or 1
				qual <- boot.overlap.beta( f=f, b=b, xs=xs, 
					boot.reps=boot.reps, seed=seed, set.seed=set.seed, 
					shape1=shape1, shape2=shape2, lowlim=lowlim, 
					discount.method=discount.method, test.diff.boot=test.diff.boot,
					q.tr.logit.inverse= q.tr.logit.inverse, logit.inverse.beta= logit.inverse.beta  
				)		
	
				# Extract the results
				q.diff.raw <- NA			# means-based value, not applicable
				q.diff <- NA					# means-based value, not applicable
				q.diff.method <- NA		# means-based parameters not applicable
				p.diff <- NA					# means-based value, not applicable
				ol <- qual$q.ol
				ol.lcl <- qual$q.lcl
				ol.ucl <- qual$q.ucl
				q <- ol
				q.raw <- qual$q.raw					
				q.lcl <- ol.lcl
				q.ucl <- ol.ucl
				ol.w <- NA					  # means-based value, not applicable
				boot.q.curr <- qual$boot.q
				pdf.f.curr <- qual$pdf.f
				pdf.b.curr <- qual$pdf.b		
				f.tr <- qual$f.tr		
				b.tr <- qual$b.tr		
			} else {
				stop("ERROR: parameter 'beta.algorithm' not valid! Check global params file")
			}		# END distn=="Bet"
		} else if ( distn=='NBin' || distn=='gamma') {
			p.diff <- means.test.perm( f, b, iterations=perm.reps, seed=seed ) 
			
			if ( all(f==0) && all(b==0) ) {
			  # Special handling of case where both vectors are all zeros, and therefore identical
			  q.diff <- NA					# diff-based quality not used for neg. binom. distn
			  q.diff.raw <- NA			# diff-based quality not used for neg. binom. distn
			  ol <- 1
			  q <- 1					# overlap used directly as quality
			  q.raw <- 1
			  ol.lcl <- 1
			  ol.ucl <- 1
			  q.lcl <- 1
			  q.ucl <- 1
			  ol.w <- 1						# Relative weighting factor, all overlap
			  boot.q.curr <- 1
			  pdf.f.curr <- NA    # Not calculated for all-zero vectors
			  pdf.b.curr <- NA    # Not calculated for all-zero vectors
			} else {
  			qual <- boot.overlap.diff(distn, test.tail= test.tail, f=f, b=b, xs=xs, 
  				boot.reps=boot.reps, seed=seed, set.seed=set.seed,
  				q.tr.logit.inverse= q.tr.logit.inverse, logit.inverse.beta= logit.inverse.beta  
  				)
			
  			# Extract the results
  			q.diff <- NA					# diff-based quality not used for neg. binom. distn
  			q.diff.raw <- NA			# diff-based quality not used for neg. binom. distn
  			ol <- qual$ol
  			q <- qual$q					# overlap used directly as quality
  			q.raw <- qual$q.raw					
  			ol.lcl <- qual$q.lcl
  			ol.ucl <- qual$q.ucl
  			q.lcl <- qual$q.lcl
  			q.ucl <- qual$q.ucl
  			ol.w <- 1						# Relative weighting factor, 1=all overlap
  			boot.q.curr <- qual$boot.q		# Again, overlap used directly
  			pdf.f.curr <- qual$pdf.f
  			pdf.b.curr <- qual$pdf.b
			}
			
		} else {
			stop("ERROR: unknown distribution: '", distn, "'")
		}
		
		if ( exists("qual") ) {
		  if ( qual[[1]]=='FAIL' ) {
		    err.msg <- paste0("ERROR: Quality calculation failed! EI=", curr.EI, 
		      ", curr.lc=", curr.lc, ", curr.stratum=", curr.stratum, "\n")
		    stop(err.msg)
		  }
		}
		
		#######################################
		# Append bootstrap and pdf results to the applicable
		# data frames. Nothing appended if vector undefined.
		#######################################

		# Bootstrap quality
		if ( ! ( length(boot.q.curr)==1 && is.na(boot.q.curr[1]) ) ) {
		  
		  if ( length(boot.q.curr)==1 ) {
		    # Special handling for ( all(f==0) && all(b==0) )
		    if ( boot.q.curr==1 ) {
		      cols
		      curr.boot.q <-data.frame(curr.lc, curr.stratum, boot.reps, q, t(rep(1,boot.reps)) )
		    }
		  } else {
		    curr.boot.q <-data.frame(curr.lc, curr.stratum, boot.reps, q, t(boot.q.curr))
		  }
		  
		  names(curr.boot.q)[names(curr.boot.q) == 'curr.lc'] <- 'landCover'
		  names(curr.boot.q)[names(curr.boot.q) == 'curr.stratum'] <- 'stratum'		
		  boot.q.df <- rbind(boot.q.df, curr.boot.q)
		}

		# Focal pdf
		if ( ! ( length(pdf.f.curr)==1 && is.na(pdf.f.curr[1]) ) ) {
		  curr.pdf.f <-data.frame(curr.lc, curr.stratum, f.hat, t(pdf.f.curr))
		  pdf.f.df <- rbind(pdf.f.df, curr.pdf.f)
		}
		
		# Benchmark pdf
		if ( ! ( length(pdf.b.curr)==1 && is.na(pdf.b.curr[1]) ) ) {
	    curr.pdf.b <-data.frame(curr.lc, curr.stratum, b.hat, t(pdf.b.curr))
		  pdf.b.df <- rbind(pdf.b.df, curr.pdf.b)
		} 
		
		######################################
		#Display the current results
		######################################
		
		# Formatting for runtime message
		f.hat.txt <- specify_decimal(f.hat, 3)
		b.hat.txt <- specify_decimal(b.hat, 3)
		fb.diff.txt <- specify_decimal(fb.diff, 4)
		p.diff.txt <- specify_decimal(p.diff, 3)
		q.diff.txt  <- specify_decimal(q.diff, 3)
		q.diff.raw.txt  <- specify_decimal(q.diff.raw, 3)
		ol.txt  <- specify_decimal(ol, 3)
		ol.lcl.txt  <- specify_decimal(ol.lcl, 3)
		ol.ucl.txt  <- specify_decimal(ol.ucl, 3)
		q.raw.txt  <- specify_decimal(q.raw, 3)
		q.txt  <- specify_decimal(q, 3)
		q.lcl.txt  <- specify_decimal(q.lcl, 3)
		q.ucl.txt  <- specify_decimal(q.ucl, 3)
		if (is.na(ol.w)) {
			ol.w.txt <- ""
		} else {
			ol.w.txt <- specify_decimal(ol.w, 2)
			ol.w.txt <- paste0(", ol.w=", ol.w.txt)
		}
	
		cat(paste0("    ", "f.mean=", f.hat.txt, ", b.mean=", b.hat.txt, ", f-b=", fb.diff.txt, 
			", p=", p.diff.txt, '\n' ))
		cat(paste0("    ", 
			"q.diff.raw=", q.diff.raw.txt, 
			", q.diff=", q.diff.txt, 
			", q.diff.method='", q.diff.method, "'",
			", q.wt.method='", q.wt.method, "'",
			"\n"
			))
		cat(paste0("    q.ol=", ol.txt, ol.w.txt, '\n' ))
		cat(paste0("    q.raw=", q.raw.txt, '\n'  ))
		cat(paste0("    q=", q.txt, ", q.lcl=", q.lcl.txt, ", q.ucl=", q.ucl.txt, '\n' ))

		# # Print original and transformed vectors, for troubleshooting
		# print(paste0("f: "))
		# print(paste(f, collapse=" " ))
		# print(paste0("b: "))
		# print(paste(b, collapse=" " ))
		# print(paste0("f.tr: "))
		# print(paste(f.tr, collapse=" " ))
		# print(paste0("b.tr: "))
		# print(paste(b.tr, collapse=" " ))

		######################################
		# Save results for current vegetation & stratum
		######################################

		f.results$p.diff[f.results$landCover == curr.lc & f.results$stratum==curr.stratum]<- p.diff
		f.results$q.diff[f.results$landCover ==curr.lc & f.results$stratum==curr.stratum] <- q.diff
		f.results$q.diff.raw[f.results$landCover ==curr.lc & f.results$stratum==curr.stratum] <- q.diff.raw
		f.results$q.diff.method[f.results$landCover ==curr.lc & f.results$stratum==curr.stratum] <- q.diff.method
		f.results$q.wt.method[f.results$landCover ==curr.lc & f.results$stratum==curr.stratum] <- q.wt.method
		f.results$ol[f.results$landCover ==curr.lc & f.results$stratum==curr.stratum] <- ol
		f.results$ol.lcl[f.results$landCover ==curr.lc & f.results$stratum==curr.stratum] <-ol.lcl
		f.results$ol.ucl[f.results $landCover ==curr.lc & f.results$stratum==curr.stratum] <-ol.ucl
		f.results$ol.w[f.results$landCover ==curr.lc & f.results$stratum==curr.stratum] <- ol.w
		f.results$q.raw[f.results$landCover ==curr.lc & f.results$stratum==curr.stratum] <-q.raw
		f.results$q[f.results$landCover ==curr.lc & f.results$stratum==curr.stratum] <-q
		f.results$q.lcl[f.results$landCover ==curr.lc & f.results$stratum==curr.stratum] <-q.lcl
		f.results$q.ucl[f.results $landCover ==curr.lc & f.results$stratum==curr.stratum] <-q.ucl
		
	} 		# END strata loop
	
	cat('------------------------------------------------------------\n')
}	# END landCover loop

####################################################
# Save indicator parameters to file
####################################################

cat("\nSaving results to files:\n")

cat("  Indicator parameters...")

# Create file if it doesn't already exist
if ( file.exists( file.ei.summary )==FALSE ) {
	df.ei.summary <- data.frame(
		ei=character(),
		ei.name=character(),
		distn=character(),
		q.method=character(),
		test.tail=character(),
		bm.val=numeric()
	)
	write.csv(df.ei.summary, file=file.ei.summary, row.names=FALSE)
}

# Open existing file 
df.ei.summary <- read.csv(file.ei.summary, header=T, stringsAsFactors=FALSE)
df.ei.summary$bm.val <- as.numeric(df.ei.summary$bm.val)
curr.ei.summary <- c(
		curr.EI,
		EI.name,
		distn,
		q.method,
		test.tail,
		bm.val
)

# Append new or replace existing row for this EI
if ( curr.EI %in% df.ei.summary$ei ) {
	# Replace row
	df.ei.summary[ df.ei.summary$ei==curr.EI, ] <- curr.ei.summary
} else {
	# Append row
	df.ei.summary <- rbind(df.ei.summary, curr.ei.summary)
}

# Reset column names and data types because R is a POS
colnames(df.ei.summary) <- c('ei', 'ei.name', 'distn', 'q.method', 'test.tail', 'bm.val')

# Save file
write.csv(df.ei.summary, file=file.ei.summary, row.names=FALSE)

cat("done\r\n")

####################################################
# Save results files
####################################################

cat("  Quality calculations...")

# Reorder columns
f.results <- df.reorder(df= f.results, col.move='q.diff.raw', col.before='diff')
f.results <- df.reorder(df= f.results, col.move='q.raw', col.before='ol.w')

# Save revised data frame to focal results file
names(f.results)[names(f.results) == 'vegClass'] <- 'bm.vegetation'
names(f.results)[names(f.results) == 'targetVegClass'] <- 'bm.vegetation'
names(f.results)[names(f.results) == 'landCoverClass'] <- 'landcover'
names(f.results)[names(f.results) == 'landCover'] <- 'landcover'
f.results <- df.reorder(df= f.results, col.move='bm.vegetation', col.before='EI')

if (curr.EI=="asc") {
	# Re-sort using stratum order column "ord"
	f.results <- f.results[ order( f.results$bm.vegetation, f.results$landcover, 
	f.results$ord, f.results$stratum ), ]
}

write.csv(f.results, file= f.results.file, row.names=FALSE)

# Rename first two columns of boostrap quality df and save
names(boot.q.df)[names(boot.q.df) == 'curr.lc'] <- 'landcover'
names(boot.q.df)[names(boot.q.df) == 'curr.stratum'] <- 'stratum'
boot.q.file <-paste(RESULTSDIR, boot.q.filename, sep="")
write.csv(boot.q.df, file= boot.q.file, row.names=FALSE)

# Rename first two columns of focal pdf df and save
names(pdf.f.df)[names(pdf.f.df) == 'curr.lc'] <- 'landcover'
names(pdf.f.df)[names(pdf.f.df) == 'curr.stratum'] <- 'stratum'
pdf.f.file <-paste(RESULTSDIR, pdf.f.filename, sep="")
write.csv(pdf.f.df, file= pdf.f.file, row.names=FALSE)

# Rename first two columns of benchmark pdf df and save
names(pdf.b.df)[names(pdf.b.df) == 'curr.lc'] <- 'landcover'
names(pdf.b.df)[names(pdf.b.df) == 'curr.stratum'] <- 'stratum'
pdf.b.file <-paste(RESULTSDIR, pdf.b.filename, sep="")
write.csv(pdf.b.df, file= pdf.b.file, row.names=FALSE)

# The pdf x values
pdf.xs.file <-paste(RESULTSDIR, pdf.xs.filename, sep="")
write.csv(pdf.xs.df, file= pdf.xs.file, row.names=FALSE)

cat("done\r\n\n")
