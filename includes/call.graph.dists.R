#############################################
# Print all required distribution graphs for current indicator
#
# Call includes script graph.dists.R
#
# Required objects and parameters:
# 		curr.EI
# 		EI.name
# 		distn
# 		plot.hist
# 		q.method
# 		allow.group.strata
# 		plot.pdf (if q.method="empirical")
#############################################

cat("***************************************\n")
cat("Generating indicator distribution figures\n")
cat("***************************************\n\n")

if (plot.hist == TRUE) {
  ##########################################
  # Set initial plotting options
  ##########################################
  group.strata <- FALSE

  if (q.method == "fixed") {
    plot.fit <- FALSE
  } else {
    plot.fit <- plot.pdf
  }

  ##########################################
  # Toggle through all 2 x 2 x 2 x 2 = 16
  # potential combinations of plotting
  # options.
  #
  # Notes:
  # (1) Some combinations may not be allowed
  # depending on parameter values set in
  # params files.
  # (2) Each set of figures produced by each
  # combination of options is store in its
  # own subdirectory under main figure
  # directory "figs/"
  ##########################################

  for (opt0 in 1:2) {
    #############################
    # opt0=1: group.strata=FALSE
    # opt0=2: group.strata=TRUE
    #############################
    if (opt0 == 2) {
      if (allow.group.strata == TRUE) {
        group.strata <- TRUE
      } else {
        break # Skip second pass if grouping not allowed
      }
    }
    rescale <- FALSE

    for (opt1 in 1:2) {
      #############################
      # opt1=1: rescale=FALSE
      # opt1=2: rescale=TRUE
      #############################
      if (opt1 == 2) rescale <- TRUE
      plot.bm <- FALSE

      for (opt2 in 1:2) {
        #############################
        # opt2=1: plot.bm=FALSE
        # opt2=2: plot.bm=TRUE
        #############################
        if (opt2 == 2) plot.bm <- TRUE
        plot.focal <- FALSE

        for (opt3 in 1:2) {
          #############################
          # opt3=1: plot.focal=FALSE
          # opt3=2: plot.focal=TRUE
          #############################

          if (opt3 == 2) plot.focal <- TRUE

          # fit model for all combinations except the following
          if (!(plot.bm == FALSE & plot.focal == FALSE)) {
            # Produce the figures representing the
            # current combination of plotting options
            source("includes/graph.dists.R")
            # source('includes/graph.dists.test.R')
          }
        }
      }
    }
  }
}
