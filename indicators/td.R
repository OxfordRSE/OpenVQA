# Taxonomic Distance (TD): callable quality and figure workflow.
# Scientific preparation and ordination notes accompany the functions in
# analysis/taxonomic-distance.R.
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com

# Calculate TD, retaining per-land-cover ordination, outlier, backup, and figure behavior.
# The run seed and configured NMDS seed remain distinct; results are local to each call.
run_taxonomic_distance <- function(inputs, config, output_paths, seed = config$seed) {
  state <- taxonomic_indicator_state(inputs, config, output_paths, seed)
  state$run_seed <- seed
  state$F.TD.OUTLIERS <- "TD_outliers.csv"
  evalq(
    {
      # Ecological indicator (EI) code
      # CRITICAL! Determines which EI parameters are loaded
      # and behavior of many included functions and files
      curr.EI <- "TD"


      ##############################
      # Input file for this indicator
      ##############################

      # Get attributes for this indicator (set in params file)
      ei.pars <- ei.params(curr.EI)
      source.file <- ei.pars$source.file
      ei.name <- ei.pars$ei.name # Full name of this EI, for display/graphics
      EI.name <- ei.name # Name used by shared quality and plotting stages
      source.file <- ei.pars$source.file # Species occurrence input filename
      prepare.raw <- ei.pars$prepare.raw # Prepare raw data from scratch?
      ei.data.type <- ei.pars$ei.data.type # Data type for this EI (cover|ind)
      distn <- ei.pars$distn # Distribution of the EI
      q.method <- ei.pars$q.method # "fixed" (fixed bm) | "empirical" (overlap)
      bm.val <- ei.pars$bm.val # Fixed bm value. Applies only if q.method="fixed"
      test.tail <- ei.pars$test.tail # Test tail for this EI
      convert.percent <- ei.pars$convert.percent # Convert % cover to proportion
      remove.zero.cover.plots <- ei.pars$remove.zero.cover.plots
      has.strata <- ei.pars$has.stratum # True if this EI has strata, otherwise F
      has.stratum <- has.strata # Hack for backward compatibility
      if (has.strata == F) curr.stratum <- ""
      td.multiplier <- ei.pars$td.multiplier # Applies scalar multiplier to improve distn fit
      td.multiplier.omit <- ei.pars$td.multiplier.omit
      scale.abund <- ei.pars$scale.abund
      logit <- ei.pars$logit

      #################################
      # Transformation parameters
      #
      # Set automatically based on value
      # of ei.data.type. Generally should
      # not be set manually.
      # Used by abundance transformation.
      #################################

      # TRUE if data are proportions or percent, otherwise false
      # In theory no longer needed, replaced by convert.percent
      if (ei.data.type == "cover") {
        proportions <- TRUE
      } else {
        proportions <- FALSE
      }

      # Set to TRUE to set any proportions > 1 to 1
      # Used by abundance transformation.
      # Truncation at one is a last resort; generally
      # better to scale values over [0:1] by setting
      # scale.abund<-TRUE (set in ei.params; see above)
      # truncate.at.one has no effect if scale.abund==TRUE.
      if (proportions == TRUE) {
        truncate.at.one <- TRUE
      } else {
        truncate.at.one <- FALSE
      }

      ##############################
      # Indicator-specific graphing parameters
      ##############################

      # Group strata in single figure?
      # Set to TRUE to make extra set of figures grouping
      # figures for different strata of same vegetation in
      # single figure. MUST be FALSE if no strata for this
      # indicator
      allow.group.strata <- FALSE

      # Minimum value for y axis
      # Leave as empty string ("") to allow to vary automatically
      y.axis.fixed.min <- 0

      ##############################
      # Names and locations of output files
      ##############################
      rawFileName <- paste(curr.EI, "_raw.csv", sep = "") # Complete raw data with transformations
      resultsFocal <- paste(curr.EI, "_focal.csv", sep = "")
      resultsBm <- paste(curr.EI, "_benchmark.csv", sep = "")

      cat("\n##################################################\n")
      cat("Calculating Quality for indicator TD (Taxonomic Distance)\n")
      cat("##################################################\n\n")

      # Set name and path+name of backup raw data file
      # and path+name of raw data file (name set above at start)
      # Defining here as need both inside and outside of prepare.raw block
      rawFileName.bak <- sub(".csv", ".bak.csv", rawFileName)
      rawFile.bak <- paste0(RESULTSDIR, rawFileName.bak)
      rawFile <- paste0(RESULTSDIR, rawFileName)

      if (PLOT.FIGS.ONLY == FALSE) {
        cat("************************\n")
        cat("Preparing data\n")
        cat("************************\n")

        if (prepare.raw == TRUE || force.prepare.raw == TRUE) { # START prepare.raw

          TdPlot <- prepare_taxonomic_distance(inputs, config, output_paths, seed = run_seed)

          # Write file of taxonomic distances as raw data file
          write.csv(TdPlot, file = rawFile, row.names = FALSE)
        } # END prepare.raw

        cat("**************************************\n")
        cat("Preparing raw indicator values file\n")
        cat("**************************************\n")

        #######################################################
        # Calculate and save benchmark values
        #######################################################

        cat("Summarizing benchmark values...")

        # Re-import prepared raw data
        inputFile <- paste(RESULTSDIR, rawFileName, sep = "")
        # cat("InputFile=",inputFile)
        TdPlot <- read.csv(inputFile, header = T)
        TdPlot.bm <- subset(TdPlot, TdPlot$focalOrBenchmark == "b")

        # First, drop land cover class & remove duplicated bm plots
        TdPlot.bm <- subset(TdPlot.bm, select = -c(landCover))
        TdPlot.bm <- unique(TdPlot.bm)

        # Make data frame of moments for transformed species cover
        # aggregated by bm vegetation (vegclass) + stratum
        tdFinal.bm <- with(TdPlot.bm, aggregate(EI.tr, list(vegClass = vegClass),
          FUN = function(x) {
            c(
              n = length(x),
              mean = mean(x),
              sd = sd(x),
              med = median(x),
              max = max(x),
              min = min(x)
            )
          }
        ))
        tdFinal.bm <- cbind(tdFinal.bm[-ncol(tdFinal.bm)], tdFinal.bm[[ncol(tdFinal.bm)]])
        colnames(tdFinal.bm) <- c("vegClass", "n", "EI.mean", "EI.sd", "EI.med", "EI.max", "EI.min")

        # Add dummy stratum column
        tdFinal.bm$stratum <- "nostrata"

        # Add EI & rearrange the columns
        tdFinal.bm$EI <- curr.EI # add indicator
        tdFinal.bm <- tdFinal.bm[, c(
          "EI", "vegClass", "stratum", "n", "EI.mean",
          "EI.sd", "EI.med", "EI.max", "EI.min"
        )]

        # Make final, human-readable veg classes
        tdFinal.bm$vegClass <- human.readable(tdFinal.bm$vegClass)

        # Inspect the columns of tdFinal.bm
        sorted <- tdFinal.bm[order(tdFinal.bm$vegClass), ]
        head(sorted)

        # save benchmark results file
        resultsFile <- paste(RESULTSDIR, resultsBm, sep = "")
        write.csv(tdFinal.bm, file = resultsFile, row.names = FALSE)

        cat("done\n")

        ####################################################
        # Prepare (focal) results file, and populate basic summary stats
        ####################################################

        prepare_indicator_results(environment())

        ####################################################
        # Calculate quality
        ####################################################

        calculate_indicator_quality(environment())
      } else {
        cat("************************\n")
        cat("Generating figures\n")
        cat("************************\n")
      } # END PLOT.FIGS.ONLY

      ####################################################
      # Plot overlap
      ####################################################

      if (plot.overlap == TRUE) plot_indicator_overlap(environment())

      ####################################################
      # Graph the fitted distributions for each vegetation type
      # Include model statistic and overlap scores on figures if requested
      ####################################################

      plot_indicator_distributions(environment())

      cat("\n##################################################\n")
      cat("Operation completed\n")
      cat("##################################################\n\n")
    },
    envir = state
  )
  if (config$plot_figures_only) {
    return(invisible(NULL))
  }
  invisible(list(
    raw = read.csv(file.path(output_paths$results, "TD_raw.csv")),
    focal = read.csv(file.path(output_paths$results, "TD_focal.csv")),
    benchmark = read.csv(file.path(output_paths$results, "TD_benchmark.csv"))
  ))
}
