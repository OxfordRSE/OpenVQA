#######################################################
# Quality Hectares Ecological Indicator (EI) Calculation
# EI = Taxonomic Distance (TD)
# Method = NMDS
#
# By: Brad Boyle
# Email: bboyle@email.arizona.edu / ojalaquellueva@gmail.com
#
# Includes automated detection and removal of TD outliers.
#
# Purpose:
# Calculates "Taxonomic Distance" ECM score
# for one or more focal sites, relative to sample of benchmark (bm) sites
#
# Important note:
# Handles more than one land cover class (landCover) per class of benchmark
# vegetation (vegClass). To work
# as intended, bm plots must be replicated for each land cover
# class and the value of vegClass for each bm plot set equal to the
# value of landCover for the land cover class. Replication and re-
# classification of the bm plots should be handled by the import script.
#
# Details:
# Taxonomic distance is the standardized Euclidean distance from a
# given plot to the multivariate centroid in NMDS space. Centroid
# calculated for benchmark plots only. Distances for focal sites
# calculated individually, separately for from benchmark sites.
# ECM is based on the focal site TD score, relative to the distribution
# of TD scores for bm sites.
#
# Requires tab-delimited file of raw EI values:
# 		plotCode: 							unique alphanumeric code for plot
# 		focalOrBenchmark: 		'b' for benchmark, 'f' for focal site
# 		vegClass: 						short code for benchmark vegetation class
# 		landCover: 						short code for land cover class
# 		species: 							species name, excluding morphospecies
# 		cover: 								Maximum percent cover of the species across all
# 													stratum
#######################################################


# Transform occurrence abundance exactly as required by TD before ordination.
# EI and EI.tr are both consumed downstream; preserve their distinct transformations.
transform_taxonomic_abundance <- function(dat, config) {
  settings <- resolve_params(config$indicator_defaults, config$indicator_settings$TD)
  data_type <- if (is.null(settings$data_type)) config$data_type else settings$data_type
  proportions <- data_type == "cover"
  convert.percent <- settings$convert_percent
  scale.abund <- settings$scale_abundance
  logit <- settings$logit
  truncate.at.one <- proportions
  no.trans <- 0

  if (proportions == TRUE) {
    # Transformations relevant to proportions

    if (convert.percent == TRUE) {
      no.trans <- no.trans + 1
      if (no.trans == 1) cat(":\n")
      # convert percents to proportions, replacing raw values
      cat("- WARNING: converting percent to proportions...")
      dat$EI <- dat$EI / 100
      cat("done\n")
    }

    dat$EI.tr <- dat$EI
    x <- dat$EI.tr

    if (scale.abund == TRUE) {
      no.trans <- no.trans + 1
      if (no.trans == 1) {
        cat("- WARNING: scaling cover values over 0:1 (scale.abund==TRUE)...")
      }
      # scale cover values so they range between 0-1
      # this may be necessary if cover sums to >100%)
      maxCurr <- max(x)
      minCurr <- min(x)
      maxNew <- 1
      minNew <- 0
      x.scaled <- (((maxNew - minNew) * (x - minCurr)) / (maxCurr - minCurr)) + minNew
      x <- x.scaled
      dat$EI.tr <- x
      x <- dat$EI.tr # Reset vector x to the scaled values
      cat("done\n")
    } else if (truncate.at.one == TRUE) {
      no.trans <- no.trans + 1
      if (no.trans == 1) cat(":\n")
      cat("- WARNING: truncating cover values at 1 (#1)...")
      # Set any values > 1 to 1
      dat$EI.tr[dat$EI.tr > 1] <- 1
      dat$EI[dat$EI > 1] <- 1
      cat("done\n")
    }

    if (logit == TRUE) {
      no.trans <- no.trans + 1
      if (no.trans == 1) cat(":\n")
      cat("- WARNING: applying logit transformation...")
      # logit transform proportions
      minVal <- min(x[x > 0])

      for (i in 1:length(x)) {
        propCover <- x[i]
        if (propCover == 0) {
          dat[i, "EI.tr"] <- log(minVal * 0.000001)
        } else if (propCover >= 1) {
          dat[i, "EI.tr"] <- 0
        } else {
          dat[i, "EI.tr"] <- log(propCover / (1 - propCover))
        }
      }
      cat("done\n")
    }

    # Truncate above 1
    # Shouldn't be necessary if scale.abund==TRHE
    if (truncate.at.one == TRUE) {
      no.trans <- no.trans + 1
      if (no.trans == 1) cat(":\n")
      cat("- WARNING: truncating cover values at 1 (#2)...")
      dat$EI[dat$EI > 1] <- 1
      cat("done\n")
    }
  } # End proportions

  if (no.trans == 0) cat("...done\n")
  dat
}

# Preserve picante's text conversion and species/site ordering with a call-local file.
# Use the same serialization as the occurrence table; always remove the temporary file.
taxonomic_distance_matrix <- function(occurrences, scale_abundance) {
  matrix_file <- tempfile("td-species-", fileext = ".txt")
  on.exit(unlink(matrix_file), add = TRUE)
  write.table(occurrences,
    file = matrix_file, quote = FALSE, sep = "\t",
    col.names = FALSE, row.names = FALSE
  )
  matrix <- picante::readsample(matrix_file)
  if (scale_abundance) {
    # Turn cover into relative abundance by dividing each site's values by its total.
    matrix <- vegan::decostand(matrix, method = "total")
  }
  matrix
}

# Run-level seed presence controls resetting, but NMDS uses its own configured seed.
# The recorded nmds$seed_enabled flag does not control the existing fitting path.
fit_taxonomic_ordination <- function(matrix, config, seed = config$seed) {
  fit <- vegan::metaMDS
  if (!is.null(seed)) base::set.seed(config$nmds$seed)
  fit(matrix,
    distance = "bray", try = 20, trymax = config$nmds$trymax,
    k = config$nmds$dimensions, maxit = config$nmds$max_iterations,
    autotransform = FALSE, trace = config$nmds$verbose
  )
}

# Distances use the arithmetic centroid of benchmark plots in two or three NMDS dimensions.
# Keep the two-dimensional z column as an empty string in the exported scores.
taxonomic_ordination_distances <- function(ordination, plots, dimensions) {
  spCov.mds <- ordination
  nmds.k <- dimensions
  stress <- ordination$stress
  converged <- ordination$converged
  # Get site scores
  sampleScores <- data.frame(spCov.mds$points)

  # Restructure the rest of the df, converting row labels (plot
  # names) to column, and merge the two matrices
  for (j in 1:nrow(sampleScores)) {
    rn <- rownames(sampleScores)
    if (nmds.k == 2) sampleScores[j, 3] <- "" # Add dummy z col if 2D
    sampleScores[j, 4] <- nmds.k # Number of dimensions
    sampleScores[j, 5] <- rn[j] # Plot code
    sampleScores[j, 6] <- stress # Final stress
    sampleScores[j, 7] <- converged # Converged?
  }
  colnames(sampleScores) <- c("x", "y", "z", "dim", "plotCode", "stress", "converged")
  TdPlotTemp <- merge(plots, sampleScores, by = "plotCode")

  # Calculate centroids and taxonomic distance
  if (nmds.k == 2) {
    # Calculate 2D centroid of benchmark plots
    # using the mean coordinates algorithm
    bmTd <- subset(TdPlotTemp,
      TdPlotTemp$focalOrBenchmark == "b",
      select = c("x", "y")
    )
    centroid <- c(mean(bmTd$x), mean(bmTd$y))

    # Calculate TD (Taxonomic Distance) as 2D Eculidean
    # distance from each plot to centroid
    cen.x <- centroid[1]
    cen.y <- centroid[2]
    TdPlotTemp$td <- sqrt(
      (TdPlotTemp$x - cen.x)^2 + (TdPlotTemp$y - cen.y)^2
    )
  } else if (nmds.k == 3) {
    # Calculate 3D centroid of benchmark plots
    # using the mean coordinates algorithm
    bmTd <- subset(TdPlotTemp,
      TdPlotTemp$focalOrBenchmark == "b",
      select = c("x", "y", "z")
    )
    centroid <- c(mean(bmTd$x), mean(bmTd$y), mean(bmTd$z))

    # Calculate TD (Taxonomic Distance) as 3D Euclidean
    # distance from each plot to centroid
    cen.x <- centroid[1]
    cen.y <- centroid[2]
    cen.z <- centroid[3]
    TdPlotTemp$td <- sqrt(
      (TdPlotTemp$x - cen.x)^2 + (TdPlotTemp$y - cen.y)^2 +
        (TdPlotTemp$z - cen.z)^2
    )
  } else {
    stop(paste0("ERROR: invalid value (", nmds.k, ") of NMDS parameter k"))
  }

  TdPlotTemp
}

# Flag distances separately for focal and benchmark samples in each focal land cover.
# An absolute threshold and a mean +/- SD threshold both inspect the same observations.
flag_taxonomic_outliers <- function(TdPlot, config, land_cover_classes) {
  landCoverClasses <- land_cover_classes
  tot.landCoverClasses <- length(landCoverClasses)
  TD.OUTLIER.UPPER.THRESHOLD <- config$td_outlier_upper_threshold
  TD.OUTLIER.STDEVS <- config$td_outlier_stdevs
  cat("Checking for new NMDS outliers...")

  # Need to extract bm vegetation again as these have changed
  landCoverVegClasses <- unique(TdPlot[
    TdPlot$focalOrBenchmark == "f",
    c("landCover", "vegClass")
  ])
  TdPlot$outlier <- FALSE

  for (i in 1:tot.landCoverClasses) { # BEGIN outlier landCover loop
    # Get current landCover and bm vegetation codes
    curr.landCover <- landCoverClasses[i]
    curr.vegClass <- landCoverVegClasses[
      landCoverVegClasses$landCover == curr.landCover, c("vegClass")
    ]

    # Extract matrix of plots for this landCover
    # Filter focal and bm separately, then combine
    nmds.scores.f <- TdPlot[
      TdPlot$landCover == curr.landCover & TdPlot$focalOrBenchmark == "f",
    ]
    nmds.scores.b <- TdPlot[
      TdPlot$vegClass == curr.vegClass & TdPlot$focalOrBenchmark == "b",
    ]
    nmds.scores <- rbind(nmds.scores.f, nmds.scores.b)
    dims <- as.integer(unique(nmds.scores$dim))
    fb.vals <- c("f", "b")

    for (fb in fb.vals) { # BEGIN for ( fb in fb.vals )
      curr.nmds.scores <- nmds.scores[nmds.scores$focalOrBenchmark == fb, ]

      # Test #1: TD > fixed maximum (=TD.OUTLIER.UPPER.THRESHOLD)
      # Flag extreme values; Test #2 still uses those values in its mean and SD.
      curr.nmds.scores$outlier[
        curr.nmds.scores$EI.tr > TD.OUTLIER.UPPER.THRESHOLD
      ] <- TRUE

      # Test #2: TD outside TD.OUTLIER.STDEVS standard deviations
      # of mean TD.
      mean <- mean(curr.nmds.scores$EI.tr)
      sds <- sd(curr.nmds.scores$EI.tr) * TD.OUTLIER.STDEVS
      upper <- mean + sds
      lower <- mean - sds

      # Flag the outliers
      curr.nmds.scores$outlier[
        curr.nmds.scores$EI.tr > upper | curr.nmds.scores$EI.tr < lower
      ] <- TRUE

      # # Old method: flags NMDS scores outside TD.OUTLIER.STDEVS std deviations
      # mean <- mean( curr.nmds.scores$x ); sds <- sd( curr.nmds.scores$x )*TD.OUTLIER.STDEVS
      # x.upper <- mean+ sds
      # x.lower <- mean-sds
      #
      # mean <- mean( curr.nmds.scores$y ); sds <- sd( curr.nmds.scores$y )*TD.OUTLIER.STDEVS
      # y.upper <- mean+sds
      # y.lower <- mean-sds
      #
      # if (dims==3 ) {
      #   mean <- mean( curr.nmds.scores$z ); sds <- sd( curr.nmds.scores$z )*TD.OUTLIER.STDEVS
      #   z.upper <- mean+sds; z.lower <- mean-sds
      #
      #   curr.nmds.scores$outlier[
      #     curr.nmds.scores$x > x.upper | curr.nmds.scores$x < x.lower |
      #     curr.nmds.scores$y > y.upper | curr.nmds.scores$y < y.lower |
      #     curr.nmds.scores$z > z.upper | curr.nmds.scores$z < z.lower
      #   ] <- TRUE
      # } else if (dims==2) {
      #   curr.nmds.scores$outlier[
      #     curr.nmds.scores$x > x.upper | curr.nmds.scores$x < x.lower |
      #     curr.nmds.scores$y > y.upper | curr.nmds.scores$y < y.lower
      #   ] <- TRUE
      # }

      # Update outlier column in TdPlot for this set of plots
      curr.plots <- curr.nmds.scores$plotCode
      TdPlot$outlier[TdPlot$plotCode %in% curr.plots] <- curr.nmds.scores$outlier
    } # END for ( fb in fb.vals )
  } # END outlier landCover loop

  TdPlot
}

# Prepare ordination distances with the existing filtering, scaling, backup, outlier,
# and missing-focal-plot rules. Each call has its own ordination and temporary tables.
prepare_taxonomic_distance <- function(inputs, config, output_paths, seed = config$seed) {
  state <- taxonomic_indicator_state(inputs, config, output_paths, seed)
  parameters <- state$ei.params("TD")
  required <- c(parameters$source.file, "plotMetadata.csv")
  if (any(vapply(inputs[required], is.null, logical(1)))) {
    stop("Taxonomic distance requires: ", paste(required, collapse = ", "), call. = FALSE)
  }
  state$source.file <- parameters$source.file
  state$ei.data.type <- parameters$ei.data.type
  state$remove.zero.cover.plots <- parameters$remove.zero.cover.plots
  state$scale.abund <- parameters$scale.abund
  state$td.multiplier <- parameters$td.multiplier
  state$td.multiplier.omit <- parameters$td.multiplier.omit
  state$run_seed <- seed
  state$F.TD.OUTLIERS <- "TD_outliers.csv"
  state$rawFile.bak <- file.path(output_paths$results, "TD_raw.bak.csv")
  evalq(
    {
      #######################################################
      # Load data
      #######################################################

      cat("Loading input files:\n")

      # Import table containing cover values of all species in
      # each plot by stratum (growth form)
      # Table must also include landCover, vegClass (=bm vegetation)
      # and focalOrBenchmark (f|b) of each plot
      cat("- Loading species occurrence data from file '", source.file, "'...", sep = "")
      dat.raw <- inputs[[source.file]]
      # detachAllData()
      # attach(dat)
      dat <- dat.raw
      cat("done\n")

      # Load plot metadata
      # Used after NMDS for adding no-data plots and outliers to results files
      cat("- Loading plot metadata from file '", "plotMetadata.csv", "'...", sep = "")
      plotMetadata <- inputs[["plotMetadata.csv"]]
      cat("done\n")

      #######################################################
      # Prepare data
      #######################################################

      # For individuals/stem data only, convert to
      # counts of individuals per species per plot.
      # Require individual identifier column
      # Groups on individual id
      if (ei.data.type == "ind") {
        cat("Calculating individuals per species per plot...")
        dat$species.ns <- dat$species
        dat$species.ns <- gsub(" ", "_", dat$species.ns)
        dat$plot.species <- paste0(dat$plotCode, "_", dat$species.ns)
        plot.ind <- unique(dat[, c("plotCode", "plot.species", "species", "ind_id")])
        plot.spp.abund <- aggregate(
          ind_id ~ plot.species,
          data = plot.ind,
          FUN = length
        )
        colnames(plot.spp.abund) <- c("plot.species", "abund")
        plot.spp <- unique(dat[, c(
          "plotCode", "focalOrBenchmark", "landCover",
          "vegClass", "plot.species", "species"
        )])
        plot.spp.abund <- merge(plot.spp.abund, plot.spp, by = "plot.species", all.x = TRUE)
        plot.spp.abund <- plot.spp.abund[, c(
          "plotCode", "focalOrBenchmark",
          "landCover", "vegClass", "species", "abund"
        )]
        dat <- plot.spp.abund
        cat("done\n")
      }

      # **************************
      # WARNING: Need to add in plots with NO individuals
      # Perhaps add single individual of a non-existent species name?
      # **************************

      # Remove rows with missing data or zero cover
      # Appplies to cover data only
      if (ei.data.type == "cover") {
        cat("Removing rows with zero or missing cover...")
        n.before <- nrow(dat)

        # Save backups for troubleshooting
        dat.cover.na <- dat[is.na(dat$cover), ]
        dat.cover.lt.zero <- dat[dat$cover <= 0, ]

        dat <- dat[!is.na(dat$cover), ]
        dat <- dat[dat$cover > 0, ]
        n.after <- nrow(dat)

        n.diff <- n.before - n.after
        if (n.diff > 0) {
          cat(
            "WARNING: ", n.diff, " of ", n.before,
            " rows deleted due to missing or zero cover!\n"
          )
        }
        cat("done\n")
      }

      # Exclude vegetation with no data
      # (resolved vegetation_without_data exclusions)
      dat <- dat[
        !dat$landCover %in% veg.nodata &
          !dat$vegClass %in% veg.nodata,
      ]

      # Exclude land cover classes with no benchmark vegetation
      dat.f <- dat[dat$focalOrBenchmark == "f", ]
      dat.b <- dat[dat$focalOrBenchmark == "b", ]
      bm.veg.f <- unique(dat.f$vegClass)
      dat.b <- dat.b[dat.b$vegClass %in% bm.veg.f, ]
      dat <- rbind(dat.f, dat.b)

      if (remove.zero.cover.plots == TRUE && ei.data.type == "cover") {
        # Get sum of cover for each plot
        # Options 'na.rm = TRUE, na.action=na.pass' cause
        # NAs to be treated as zeros
        dat$plot <- as.character(dat$plotCode) # convert factor to character
        plot.tot.cover <- aggregate(
          cover ~ plot,
          data = dat,
          FUN = sum,
          na.rm = TRUE,
          na.action = na.pass
        )
        plots.no.cover <- plot.tot.cover[plot.tot.cover$cover == 0, c("plot")]
        n.plots.no.cover <- length(plots.no.cover)
        n.plots.before <- length(unique(dat$plot))
        dat <- dat[!dat$plot %in% plots.no.cover, ]
        n.plots.after <- length(unique(dat$plot))
        plots.deleted <- n.plots.before - n.plots.after
        perc.deleted <- specify_decimal(plots.deleted / n.plots.before * 100, 1)

        if (plots.deleted > 0) {
          msg <- paste0(
            "WARNING: ", plots.deleted, " plots (", perc.deleted,
            "%) removed due to all-zero cover!\n"
          )
          cat(msg)

          if (plots.deleted == n.plots.before) {
            msg <- "ERROR: No plots with no-zero cover; aborting...\n\n"
            stop(msg)
          }
        }
      }

      #######################################################
      # Remove outlier plots if applicable
      # This applies when a prior preparation saved
      # TD_outliers.csv in the results directory.
      #######################################################

      f.outliers <- paste0(RESULTSDIR, F.TD.OUTLIERS)

      if (file.exists(f.outliers) && REMOVE.TD.OUTLIERS == TRUE) {
        cat("\nChecking for previously-saved NMDS outliers:\n")
        df.outliers <- read.csv(f.outliers, header = TRUE)
        outlier.plots <- unique(df.outliers$plotCode)
        n.outliers <- length(outlier.plots)
        cat(paste0("- Found ", n.outliers, " outlier plots in file '", F.TD.OUTLIERS, "'\n"))

        # Delete the offending plots
        cat(paste0("- Deleting ", n.outliers, " NMDS outliers..."))
        n.plots.before <- length(unique(dat$plotCode))
        dat <- dat[!dat$plotCode %in% outlier.plots, ]
        cat("done\n")

        n.plots.after <- length(unique(dat$plotCode))
        cat("- Plots before: ", n.plots.before, ", plots after: ", n.plots.after, "\n")
      }

      #######################################################
      # Validations and transformations
      #######################################################

      # Rename cover to "abund", if applicable
      names(dat)[names(dat) == "cover"] <- "abund"

      # Keep only fields needed
      dat <- dat[, c(
        "plotCode", "focalOrBenchmark", "landCover",
        "vegClass", "species", "abund"
      )]

      # Rename percent cover field to EI and add EI-transformed column
      colnames(dat) <- c(
        "plotCode", "focalOrBenchmark", "landCover",
        "vegClass", "species", "EI"
      )
      names(dat)[names(dat) == "abund"] <- "EI"
      dat$EI.tr <- NA

      # Initialize both representations; transformation and quality stages consume them.
      dat$EI.tr <- dat$EI

      # Transform abundance while retaining raw and transformed EI values.
      dat <- transform_taxonomic_abundance(dat, config)

      # Remove species with effectively zero cover after transformation
      dat <- dat[dat$EI.tr > 0.000000001, ]

      #######################################################
      # Calculate taxonomic distances by land cover class
      #######################################################

      cat("\n*************************\n")
      cat("Performing NMDS\n")
      cat("*************************\n\n")

      # Start data frame to hold raw NMDS results

      # Remove any NA land cover or bm veg classes
      dat <- dat[!is.na(dat$landCover) & !is.na(dat$vegClass), ]

      # Set up loop to extract all plots from a given vegetation type
      landCoverClasses <- unique(dat[
        dat$focalOrBenchmark == "f",
        c("landCover")
      ]) # Focal land cover classes only
      tot.landCoverClasses <- length(landCoverClasses)
      landCoverVegClasses <- unique(dat[
        dat$focalOrBenchmark == "f",
        c("landCover", "vegClass")
      ]) # bm veg for each land cover
      dat.f <- dat[dat$focalOrBenchmark == "f", ] # df of just focal plots
      dat.b <- dat[dat$focalOrBenchmark == "b", ] # df of just bm plots
      tot.converged <- 0
      # Number of classes whose returned convergence value is at least 10.
      tot.converged.high <- 0

      for (i in 1:tot.landCoverClasses) { # BEGIN NMDS landCover loop
        # Get each unique landCover x stratum combination in bm table
        curr.landCover <- landCoverClasses[i]
        curr.vegClass <- landCoverVegClasses[
          landCoverVegClasses$landCover == curr.landCover, c("vegClass")
        ]
        cat("landCover='", as.character(curr.landCover), "'\n", sep = "")
        cat("bmVeg='", as.character(curr.vegClass), "'\n", sep = "")

        # Extract matrix of plots for this landCover, fully identified species only
        # Filter focal and bm separately, then combine
        spCov.f <- subset(dat.f,
          !is.nan(species) & !(species == "<NA>") &
            landCover == curr.landCover,
          select = c(plotCode, EI.tr, species)
        )
        spCov.b <- subset(dat.b,
          !is.nan(species) & !(species == "<NA>") &
            vegClass == curr.vegClass,
          select = c(plotCode, EI.tr, species)
        )
        spCov <- rbind(spCov.f, spCov.b)

        # Make another matrix of plots (by omitting species and abundance)
        plots.f <- subset(dat.f,
          landCover == curr.landCover,
          select = c(plotCode, focalOrBenchmark, landCover, vegClass)
        )
        plots.b <- subset(dat.b,
          vegClass == curr.vegClass,
          select = c(plotCode, focalOrBenchmark, landCover, vegClass)
        )
        plots <- rbind(plots.f, plots.b)
        plots <- plots[!duplicated(plots[, c(
          "plotCode", "focalOrBenchmark",
          "landCover", "vegClass"
        )]), ]

        n.f <- nrow(plots[plots$focalOrBenchmark == "f", ])
        n.b <- nrow(plots[plots$focalOrBenchmark == "b", ])
        n.tot <- n.f + n.b

        #############################
        # NMDS
        #
        # NMDS settings come from the resolved configuration.
        #############################

        # The parameter adapter returns the same resolved NMDS settings for each
        # focal land cover; fitting below consumes those settings directly.
        nmds.pars <- nmds.params(curr.landCover)

        # Set general parameters
        nmds.verbose <- nmds.pars$nmds.verbose
        nmds.set.seed <- nmds.pars$nmds.set.seed

        # Set metaMDS parameters
        nmds.seed <- nmds.pars$nmds.seed
        nmds.trymax <- nmds.pars$nmds.trymax
        nmds.k <- nmds.pars$nmds.k
        nmds.maxit <- nmds.pars$nmds.maxit

        # Echo nmds parameters
        cat("  Sample sizes:\n")
        cat("    n.f=", n.f, ", n.b=", n.b, ", n.tot=", n.tot, "\n", sep = "")
        cat("  Parameters:\n")
        cat("    seed=", nmds.seed, "\n", sep = "")
        cat("    k=", nmds.k, "\n", sep = "")
        cat("    trymax=", nmds.trymax, "\n", sep = "")
        cat("    maxit=", nmds.maxit, "\n", sep = "")

        spCov <- taxonomic_distance_matrix(spCov, scale.abund)
        spCov.mds <- fit_taxonomic_ordination(spCov, config, seed = run_seed)

        # Echo actual iterations and tries
        converged <- spCov.mds$converged
        stress <- spCov.mds$stress

        cat("  Results:\n")
        cat("    iterations=", spCov.mds$iters, "\n", sep = "")
        cat("    tries=", spCov.mds$tries, "\n", sep = "")
        cat("    stress=", stress, "\n", sep = "")
        cat("    converged=", converged, "\n", sep = "")

        if (converged == TRUE || converged > 0) tot.converged <- tot.converged + 1
        if (converged >= 10) tot.converged.high <- tot.converged.high + 1

        TdPlotTemp <- taxonomic_ordination_distances(spCov.mds, plots, nmds.k)

        if (TD.BM.PLOTS.DEDUPLICATE == FALSE) {
          #######################################################
          # Add a unique suffix to vegClass for current NMDS
          # run.
          #
          # Ensures that focal and benchmark plots are
          # grouped together when calculating quality. Bm plots
          # will be replicated for each landcover class linked
          # to that benchmark, but this OK because NMDS results
          # for the bm plots are different each time they are
          # ordinated with a different vegetation.
          #
          # Workaround until v7 of vegan is released and we are
          # able to add the focal ordination to a single set of
          # pre-existing benchmark scores from a separate NMDS run
          #
          # If TD.BM.PLOTS.DEDUPLICATE==TRUE, be sure to de-
          # duplicate bm plots. See second use of this parameter
          # below (after NMDS landCover loop)
          #######################################################

          # create a left-padded integer of exactly three digits
          suffix.num <- as.character(i)
          l.pad.n <- 3 - nchar(suffix.num)
          l.pad <- ""
          if (l.pad.n > 0) l.pad <- paste(rep("0", l.pad.n), collapse = "")

          # Create the suffix and append to vegClass for both focal and benchmark plots
          bm.suffix <- paste0("_", l.pad, suffix.num)
          TdPlotTemp$vegClass <- paste0(TdPlotTemp$vegClass, bm.suffix)
        }

        if (i == 1) {
          # Begin the final matrix
          TdPlot <- TdPlotTemp
        } else {
          # Append the next set of rows
          TdPlot <- rbind(TdPlot, TdPlotTemp)
        }
      } # END NMDS landCover loop

      # Echo summary of convergence results for all veg classes
      perc.conv <- specify_decimal(tot.converged / tot.landCoverClasses * 100, 1)
      cat("*********************************\n")
      cat("Convergence results:\n")
      cat("  ", tot.converged, " veg classes out of ", tot.landCoverClasses,
        " converged (", perc.conv, "%)\n",
        sep = ""
      )
      if (tot.converged.high >= 10) {
        cat("  ", tot.converged.high,
          " classes took >10 iterations to converge and may be unstable--recommend checking.\n",
          sep = ""
        )
      }
      cat("*********************************\n\n")

      #######################################################
      # Add pseudo-stratum column
      #######################################################

      # Get column names
      cols <- colnames(TdPlot)

      # Reorder columns, keeping stratum column if present,
      # and keeping the transformed EI column
      if (!any("stratum" %in% cols)) {
        has.stratum <- FALSE
        TdPlot$stratum <- "nostrata"
      }

      if (TD.BM.PLOTS.DEDUPLICATE == TRUE) {
        # Remove duplicated bm plots
        # Workaround until vegan 7.0 released
        TdPlot <- TdPlot[!duplicated(TdPlot[, c("plotCode", "focalOrBenchmark", "landCover")]), ]
      }

      names(TdPlot)[names(TdPlot) == "td"] <- "EI"
      TdPlot$EI.tr <- TdPlot$EI
      TdPlot <- TdPlot[, c(
        "vegClass", "landCover", "stratum", "focalOrBenchmark",
        "plotCode", "x", "y", "z", "dim", "stress", "converged", "EI", "EI.tr"
      )]

      # Inspect the contents
      sorted <- TdPlot[order(
        TdPlot$vegClass, TdPlot$landCover,
        TdPlot$focalOrBenchmark, TdPlot$plotCode
      ), ]
      # head(sorted)

      #######################################################
      # Rescale TD values
      # Objective is to avoid bad pdf fits when one or both vectors (f & b) have
      # means at or below one
      #######################################################

      cat("Rescaling all TD to >1...")

      td.scaled.mult <- function(x, y) {
        # Stretch transformation
        # Increases scale of TD to ensure mean >>1
        # Resolves distortions caused by 0<TD<1
        # (i.e., removes or reduces fractional values of TD)

        # Multiply x by y, assuming x is float >= 0
        for (i in length(x)) {
          if (!(is.null(x[i]) || is.na(x[i]))) {
            x[i] <- x[i] * y
          }
        }

        return(x)
      }

      # Transform both EI and EI.tr because distinction between these two
      # is mixed up in later functions (need to fix)
      # No change if td.multiplier==1
      # TdPlot$EI.tr[ ! TdPlot$landCover %in% td.multiplier.omit ] <-
      # 	td.scaled.mult(TdPlot$EI.tr[ ! TdPlot$landCover %in% td.multiplier.omit ], td.multiplier)
      # TdPlot$EI[ ! TdPlot$landCover %in% td.multiplier.omit ] <-
      # 	td.scaled.mult(TdPlot$EI[ ! TdPlot$landCover %in% td.multiplier.omit ], td.multiplier)

      # Above not working; do as loop instead
      # Inefficient but it works
      if (!td.multiplier == 1) {
        for (i in 1:nrow(TdPlot)) {
          EI.tr.i <- TdPlot$EI.tr[i]
          EI.i <- TdPlot$EI[i]
          lc.i <- TdPlot$landCover[i]

          if (!lc.i %in% td.multiplier.omit) {
            EI.tr.i_scaled <- td.scaled.mult(EI.tr.i, td.multiplier)
            EI.i_scaled <- td.scaled.mult(EI.i, td.multiplier)
          }
          TdPlot$EI.tr[i] <- EI.tr.i_scaled
          TdPlot$EI[i] <- EI.i_scaled
        }
      }

      cat("done\n")

      #######################################################
      # Flag NMDS outliers and save to file
      # This file can be used to exclude outlier plots
      # by repeating the raw data import after setting
      # REMOVE.TD.OUTLIERS==TRUE
      #######################################################

      TdPlot <- flag_taxonomic_outliers(TdPlot, config, landCoverClasses)

      if (!exists("F.TD.OUTLIERS")) stop("Parameter F.TD.OUTLIERS not defined!")
      outlier.file <- paste0(RESULTSDIR, F.TD.OUTLIERS)

      if (!file.exists(outlier.file)) {
        # Save a raw-data backup whenever no saved outlier list exists.
        # Backup can be used to plot NMDS outliers.
        write.csv(TdPlot, file = rawFile.bak, row.names = FALSE)
      }

      # count outliers
      n.outliers <- length(TdPlot$plotCode[TdPlot$outlier == TRUE])

      if (n.outliers == 0) {
        cat("no outliers found\n")
      } else {
        cat(n.outliers, " new outliers found!\n", sep = "")

        cat("Saving NMDS outliers:\n")
        td.outliers <- TdPlot[TdPlot$outlier == TRUE, ]
        td.outliers <- unique(td.outliers[, c(
          "plotCode", "landCover", "vegClass",
          "focalOrBenchmark"
        )])
        # TdPlot <- TdPlot[ TdPlot$outlier==FALSE, ]

        if (!file.exists(outlier.file)) {
          # Save the outliers to new file
          cat("- Saving new file '", F.TD.OUTLIERS, "'...", sep = "")
        } else {
          if (TD.OUTLIERS.APPEND == TRUE) {
            cat("- Appending to existing file '", F.TD.OUTLIERS, "'...", sep = "")
            td.outliers.orig <- read.csv(outlier.file, header = TRUE)
            td.outliers <- rbind(td.outliers.orig, td.outliers)
            td.outliers <- unique(td.outliers)
          } else {
            # Warn and replace
            cat("- Replacing existing file '", F.TD.OUTLIERS, "'...", sep = "")
          }
        }
        write.csv(td.outliers, file = outlier.file, row.names = FALSE)
        cat("done\n")
      }


      cat("Appending outliers and focal plots with no data")
      # Add column "no_data_plot"
      TdPlot$no_data_plot <- FALSE

      if (INCLUDE.PLOTS.NODATA) {
        cat(":\n")
        cat("- Checking for missing focal plots...")
        currPlots <- unique(TdPlot$plotCode)
        allPlots <- unique(plotMetadata$plotCode[plotMetadata$focalOrBenchmark == "f"])
        missingPlots <- setdiff(allPlots, currPlots)
        n_missing <- length(missingPlots)

        if (n_missing == 0) {
          cat("no missing plots...done\n")
        } else {
          cat(n_missing, " plots found\n", sep = "")
          cat("- Adding missing plots and metadata...")
          TdPlot_missing <- TdPlot[FALSE, ]
          TdPlot_missing[1:n_missing, ] <- NA
          TdPlot_missing$plotCode <- missingPlots
          TdPlot_missing <- TdPlot_missing |>
            dplyr::left_join(plotMetadata, by = "plotCode", suffix = c("", ".new")) |>
            dplyr::mutate(dplyr::across(
              .cols = dplyr::ends_with(".new"),
              .fns = ~ dplyr::coalesce(.x, get(sub(".new", "", dplyr::cur_column()))),
              .names = "{sub('.new', '', .col)}"
            )) |>
            dplyr::select(-dplyr::ends_with(".new"))
          TdPlot_missing$stratum <- "nostrata"
          cat("done\n")

          cat("- Flagging outlier plots...")
          if (file.exists(outlier.file)) {
            # Reload outlier plot file
            td.outliers <- read.csv(outlier.file, header = TRUE)
            outlierPlots <- td.outliers[, c("plotCode"), drop = FALSE]
            outlierPlots$is_outlier <- TRUE
            TdPlot_missing <- merge(TdPlot_missing, outlierPlots, by = "plotCode", all.x = TRUE)
            TdPlot_missing$outlier <- TdPlot_missing$is_outlier
            TdPlot_missing <- TdPlot_missing[, !names(TdPlot_missing) == "is_outlier"]
            cat("done\n")
          } else {
            cat("none present...done\n")
          }

          cat("- Flagging no-data plots...")
          # TdPlot_missing$no_data_plot[ is.na(TdPlot_missing$outlier) ] <- TRUE
          # TdPlot_missing$no_data_plot[ is.na(TdPlot_missing$no_data_plot) ] <- FALSE
          TdPlot_missing$no_data_plot <- TRUE
          cat("done\n")

          cat("- Assigning vegetation-specific no-data indicator values...")
          df.vegClasses <- unique(plotMetadata[, c("vegClass"), drop = FALSE])
          df.vegClasses$EI <- as.numeric(NA)
          vegClasses <- unique(df.vegClasses$vegClass)
          TdPlot_bm <- TdPlot[TdPlot$focalOrBenchmark == "b", ]
          TdPlot_f <- TdPlot[TdPlot$focalOrBenchmark == "f", ]

          # EI
          EI.means <- aggregate(
            EI ~ vegClass,
            data = TdPlot_bm,
            FUN = base::mean
          )
          colnames(EI.means) <- c("vegClass", "EI.mean")
          EI.sds.bm <- aggregate(
            EI ~ vegClass,
            data = TdPlot_bm,
            FUN = sd
          )
          colnames(EI.sds.bm) <- c("vegClass", "EI.sd")
          EI.sds.f <- aggregate(
            EI ~ vegClass,
            data = TdPlot_f,
            FUN = sd
          )
          colnames(EI.sds.f) <- c("vegClass", "EI.sd")
          EI.sds <- pmax(EI.sds.bm, EI.sds.f)
          colnames(EI.sds) <- c("vegClass", "EI.sd")
          EI.sds$EI.sd <- as.numeric(EI.sds$EI.sd)
          EI.vals <- merge(EI.means, EI.sds, by = "vegClass")
          EI.vals$EI <- EI.vals$EI.mean + (EI.vals$EI.sd * NODATA.SD)
          EI.vals <- EI.vals[, c("vegClass", "EI")]

          TdPlot_missing <- TdPlot_missing[, !names(TdPlot_missing) == "EI"]
          TdPlot_missing <- merge(TdPlot_missing, EI.vals, by = "vegClass", all.x = TRUE)
          TdPlot_missing <- df.reorder(TdPlot_missing, col.move = "EI", col.before = "converged")

          # EI.tr
          EI.means <- aggregate(
            EI.tr ~ vegClass,
            data = TdPlot_bm,
            FUN = base::mean
          )
          colnames(EI.means) <- c("vegClass", "EI.mean")
          EI.sds.bm <- aggregate(
            EI.tr ~ vegClass,
            data = TdPlot_bm,
            FUN = sd
          )
          colnames(EI.sds.bm) <- c("vegClass", "EI.sd")
          EI.sds.f <- aggregate(
            EI.tr ~ vegClass,
            data = TdPlot_f,
            FUN = sd
          )
          colnames(EI.sds.f) <- c("vegClass", "EI.sd")
          EI.sds <- pmax(EI.sds.bm, EI.sds.f)
          colnames(EI.sds) <- c("vegClass", "EI.sd")
          EI.sds$EI.sd <- as.numeric(EI.sds$EI.sd)
          EI.vals <- merge(EI.means, EI.sds, by = "vegClass")
          EI.vals$EI.tr <- EI.vals$EI.mean + (EI.vals$EI.sd * NODATA.SD)
          EI.vals <- EI.vals[, c("vegClass", "EI.tr")]

          TdPlot_missing <- TdPlot_missing[, !names(TdPlot_missing) == "EI.tr"]
          TdPlot_missing <- merge(TdPlot_missing, EI.vals, by = "vegClass", all.x = TRUE)
          TdPlot_missing <- df.reorder(TdPlot_missing, col.move = "EI.tr", col.before = "EI")
          cat("done\n")

          cat("- Appending no-data plots to main results data frame...")
          TdPlot <- rbind(TdPlot, TdPlot_missing)
          cat("done\n")
        }
      } else {
        cat("...skipping (INCLUDE.PLOTS.NODATA==FALSE)\n")
      }

      TdPlot
    },
    envir = state
  )
}
