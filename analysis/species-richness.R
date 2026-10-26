species_richness_data <- function(inputs, config) {
  settings <- resolve_params(config$indicator_defaults, config$indicator_settings$SR)
  data_type <- if (is.null(settings$data_type)) config$data_type else settings$data_type
  source_file <- if (data_type == "ind") "speciesStems.csv" else "speciesCover.csv"
  required <- c("plotMetadata.csv", "species.csv", source_file)
  if (any(vapply(inputs[required], is.null, logical(1)))) {
    stop("Species richness requires plot metadata, species attributes, and occurrences.",
      call. = FALSE
    )
  }
  plots <- inputs[["plotMetadata.csv"]]
  species <- inputs[["species.csv"]]
  occurrences <- inputs[[source_file]]
  names(species)[names(species) == "isExotic"] <- "is_exotic"
  occurrences <- merge(occurrences, species[, c("species", "is_exotic")],
    by = "species",
    all.x = TRUE
  )
  if (config$missing_exotic_status_assume_native) {
    occurrences$is_exotic[is.na(occurrences$is_exotic)] <- 0
  } else {
    stop("ERROR: value of 'is_exotic' missing for one or more species. Please correct.\n")
  }
  if (data_type == "cover") occurrences <- occurrences[occurrences$cover > 0, ]
  occurrences <- occurrences[!occurrences$is_exotic == 1, ]
  occurrences <- occurrences[, c("plotCode", "species")]
  plots <- merge(
    plots[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")],
    occurrences,
    by = "plotCode", all.x = TRUE
  )
  plots <- unique(plots[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")])
  nodata <- unlist(config$vegetation_without_data, use.names = FALSE)
  if (!length(nodata)) nodata <- ""
  plots <- plots[!plots$landCover %in% nodata & !plots$vegClass %in% nodata, ]
  plot_species <- unique(occurrences[, c("plotCode", "species")])
  richness <- as.data.frame(table(plot_species$plotCode))
  colnames(richness) <- c("plotCode", "speciesRichness")
  data <- merge(plots, richness, by = "plotCode", all.x = TRUE)
  data$speciesRichness[is.na(data$speciesRichness)] <- 0
  data$speciesRichness <- as.integer(data$speciesRichness)
  if (length(unique(data$plotCode)) != nrow(unique(data[, c("focalOrBenchmark", "plotCode")]))) {
    stop("Plot codes not unique!")
  }
  data$stratum <- "nostrata"
  colnames(data) <- c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "EI", "stratum")
  data <- data[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass", "stratum", "EI")]
  if (config$zero_to_one) data$EI[data$EI == 0] <- 1
  data$EI.tr <- data$EI
  data
}

run_species_richness <- local({
  helpers <- new.env(parent = environment())
  sys.source(file.path("includes", "functions.R"), envir = helpers)

  function(inputs, config, output_paths) {
    # shortcut: shared stages need local parameter names; remove bindings when stages accept config.
    state <- list2env(c(legacy_common_params(config), legacy_project_params(config)),
      parent = helpers
    )
    for (name in ls(helpers)) {
      helper <- helpers[[name]]
      environment(helper) <- state
      state[[name]] <- helper
    }
    state$fitdistr <- MASS::fitdistr
    state$RESULTSDIR <- paste0(output_paths$results, "/")
    state$FIGDIR <- paste0(output_paths$figures, "/")
    state$FIGDIR_REL <- paste0(output_paths$figures, "/")
    state$FILENAME.EI.SUMMARY <- "ei.summary.csv"
    state$set.seed <- !is.null(config$seed)
    # Permutation tests also receive the historical seed when bootstrap seeding is disabled.
    state$seed <- if (is.null(config$seed)) 123 else config$seed
    state$curr.EI <- "SR"
    parameters <- state$ei.params("SR")
    state$ei.name <- state$EI.name <- parameters$ei.name
    state$distn <- parameters$distn
    state$q.method <- parameters$q.method
    state$bm.val <- parameters$bm.val
    state$test.tail <- parameters$test.tail
    state$has.strata <- state$has.stratum <- parameters$has.stratum
    if (!parameters$has.stratum) state$curr.stratum <- ""
    state$allow.group.strata <- FALSE
    state$y.axis.fixed.min <- 0
    state$lc.in.title <- TRUE
    state$leg.x.extra <- state$leg.x.extra.g <- -0.2
    state$leg.y.extra <- state$leg.y.extra.g <- NA
    state$rawFileName <- "SR_raw.csv"
    state$resultsFocal <- "SR_focal.csv"
    state$resultsBm <- "SR_benchmark.csv"
    state$proportions <- FALSE

    cat("Calculating Quality for indicator SR (Species Richness)\n")
    if (!config$plot_figures_only) {
      if (parameters$prepare.raw || config$force_prepare_raw) {
        raw <- species_richness_data(inputs, config)
        write.csv(raw, file.path(output_paths$results, "SR_raw.csv"), row.names = FALSE)
      }
      evalq(
        {
          dat <- read.csv(paste0(RESULTSDIR, rawFileName), header = TRUE)
          EI.bm <- subset(dat, dat$focalOrBenchmark == "b")
          EI.bm <- with(EI.bm, aggregate(EI.tr, list(vegClass = vegClass),
            FUN = function(x) {
              c(
                n = length(x), mean = mean(x), med = median(x), sd = sd(x), max = max(x),
                min = min(x)
              )
            }
          ))
          EI.bm <- cbind(EI.bm[-ncol(EI.bm)], EI.bm[[ncol(EI.bm)]])
          colnames(EI.bm) <- c("vegClass", "n", "EI.mean", "EI.med", "EI.sd", "EI.max", "EI.min")
          EI.bm$stratum <- "nostrata"
          EI.bm$EI <- curr.EI
          EI.bm <- EI.bm[, c(
            "EI", "vegClass", "stratum", "n", "EI.mean", "EI.sd", "EI.med",
            "EI.max", "EI.min"
          )]
          EI.bm$vegClass <- human.readable(EI.bm$vegClass)
          write.csv(EI.bm, paste0(RESULTSDIR, resultsBm), row.names = FALSE)
          prepare_indicator_results(environment())
          calculate_indicator_quality(environment())
        },
        envir = state
      )
    }
    if (config$plot_overlap) state$plot_indicator_overlap(state)
    state$plot_indicator_distributions(state)
    invisible(list(
      raw = file.path(output_paths$results, "SR_raw.csv"),
      focal = file.path(output_paths$results, "SR_focal.csv"),
      benchmark = file.path(output_paths$results, "SR_benchmark.csv")
    ))
  }
})
