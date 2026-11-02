supported_indicator_config <- function() {
  resolve_params(load_default_params(), list(
    project = "supported-test", data_type = "mixed", bootstrap_reps = 5,
    permutation_reps = 19, active_indicators = list("PCS", "CH", "BAES"),
    aggregate_indicators = list("PCS"), plot_hist = FALSE, plot_overlap = FALSE
  ))
}

# Use eight benchmark plots from the standardised demo, with a synthetic focal copy.
# The demo has no heights: height_m is synthetic test data, not an inferred measurement.
# Mark one recorded species exotic to exercise BAES without changing the demo lookup.
supported_indicator_inputs <- function() {
  directory <- file.path(.params_root, "data", "vqa-demo2-min", "project_current", "inputs")
  stems <- read.csv(file.path(directory, "speciesStems.csv"))
  codes <- head(unique(stems$plotCode[stems$vegClass == "W17"]), 8)
  stems <- stems[stems$plotCode %in% codes, ]
  stems$height_m <- stems$dbh_cm / 2
  focal <- stems
  focal$plotCode <- paste0("f-", focal$plotCode)
  focal$focalOrBenchmark <- "f"
  focal$landCover <- "focal"
  multiplier <- 0.6 + 0.04 * match(stems$plotCode, codes)
  focal$height_m <- focal$height_m * multiplier
  focal$ba_m2 <- focal$ba_m2 * multiplier
  stems <- rbind(stems, focal)
  species <- read.csv(file.path(directory, "species.csv"))
  species$is_exotic[species$species == stems$species[1]] <- 1
  plots <- unique(stems[, c("plotCode", "focalOrBenchmark", "landCover", "vegClass")])
  cover <- read.csv(file.path(directory, "coverByGrowthForm.csv"))
  cover <- cover[cover$plotCode %in% codes, ]
  focal_cover <- cover
  focal_cover$plotCode <- paste0("f-", focal_cover$plotCode)
  focal_cover$focalOrBenchmark <- "f"
  focal_cover$landCover <- "focal"
  focal_cover$cover <- focal_cover$cover * 0.65
  cover <- rbind(cover, focal_cover)
  strata <- unique(cover$stratum)
  inputs <- list(
    speciesStems.csv = stems, species.csv = species, plotMetadata.csv = plots,
    coverByStratum.csv = cover,
    ei.stratum.include.csv = data.frame(
      EI = "PCS", stratum = strata, f.group = "structure", include = TRUE
    ),
    ei.stratum.veg.include.csv = data.frame(
      EI = character(), stratum = character(), bm.vegetation = character(), include = logical()
    )
  )
  # Use CSV precision, as when inputs are loaded from an assessment.
  directory <- tempfile("supported-inputs-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  lapply(inputs, function(table) {
    file <- file.path(directory, "input.csv")
    write.csv(table, file, row.names = FALSE)
    read.csv(file)
  })
}
