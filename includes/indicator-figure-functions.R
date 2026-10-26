plot_indicator_overlap <- function(state) {
  evalq(
    {
      fig.file.type <- "pdf"

      fig.font.fam <- "Helvetica"

      fig.width.cm <- 9
      res <- 500 # DPI (ignored for pdf)


      graphics.off()


      no.bins <- 12 # Number of bins to use for histogram, adjust as needed
      trt1.txt <- "Benchmark"
      trt2.txt <- "Focal"

      do.graph <- FALSE # this option is turned on if other tests succeed
      has.data <- TRUE

      boot.ol.filename <- paste0(curr.EI, "_boot.ol", ".csv")


      cat("\n")
      cat("***************************************\n")
      cat("Generating overlap figures\n")
      cat("***************************************\n")

      curr.EI <- as.character(curr.EI) # Just in case this is factor in calling script

      cat("")
      cat(paste0(curr.EI, " - ", EI.name, " (distribution=", distn, ")", "\n"))


      rawFileName <- paste(curr.EI, "_raw.csv", sep = "")
      rawFile <- paste(RESULTSDIR, rawFileName, sep = "")
      dat.EI <- read.csv(rawFile, header = T, stringsAsFactors = FALSE)
      dat.EI <- with(dat.EI, dat.EI[order(
        dat.EI$landCover, dat.EI$vegClass,
        dat.EI$focalOrBenchmark, dat.EI$plotCode, dat.EI$stratum
      ), ])

      f.results.file <- paste(RESULTSDIR, resultsFocal, sep = "")
      f.results <- read.csv(f.results.file, header = T, stringsAsFactors = FALSE)


      max.ei <- max(dat.EI$EI)

      if (distn == "Bet") {
        xs <- seq(0, 1, length.out = 100)
      } else if (distn == "NBin") {
        xs <- xs_nbin <- seq(0, max.ei + 10)
      } else if (distn == "gamma") {
        xs <- seq(0, max.ei, length.out = 1000)
      }


      figs.fitted.dir.name <- "overlap"

      dir.create(file.path(FIGDIR, figs.fitted.dir.name), showWarnings = F)
      figs.fitted.dir <- paste(FIGDIR, figs.fitted.dir.name, sep = "")

      boot.ol.df <- data.frame(
        vegClass = character(),
        stratum = character(),
        boot.reps = integer(),
        ol.obs = double()
      )
      boot.cols <- data.frame(matrix(NA, nrow = 0, ncol = boot.reps))
      boot.cols <- convert.magic(boot.cols, "numeric")
      boot.ol.df <- cbind(boot.ol.df, boot.cols)


      allveg <- unique(dat.EI$vegClass)
      n.veg <- length(allveg)

      for (j in 1:n.veg) { # START veg loop
        curr.veg <- allveg[j]

        veg.fname <- gsub(" ", "_", curr.veg)
        veg.fname <- gsub("/", "_", veg.fname)
        veg.fname <- gsub(",", "", veg.fname)

        cat("\n")
        cat("====================================\n")
        cat(paste0("Current vegetation: ", veg.fname, "\n"))

        EI.veg <- subset(dat.EI, dat.EI$vegClass == curr.veg)

        strata <- unique(EI.veg$stratum)
        n.strata <- length(strata)

        fig.cols <- 1 # Dummy values
        fig.rows <- 1

        for (k in 1:n.strata) { # START strata loop

          text.main <- curr.veg
          text.x <- EI.name
          text.y <- "Count of Plots"

          curr.stratum <- strata[k]
          EI.strat <- subset(EI.veg, EI.veg$stratum == curr.stratum)
          cat("------------------------------------------------------------\n")
          cat(paste0("Current stratum: ", curr.stratum, "\n"))

          stratum.fname <- gsub(" ", "_", curr.stratum)
          stratum.fname <- gsub("/", "_", stratum.fname)
          stratum.fname <- gsub(",", "", stratum.fname)
          stratum.fname <- tolower(stratum.fname)
          if (!has.stratum) stratum.fname == ""

          has.data <- T
          if (nrow(EI.strat) == 0) {
            has.data <- F
            print(paste("WARNING: NO DATA (", curr.veg, "; ", curr.stratum, ")", sep = ""))
          }

          ei.numvals <- length(unique(EI.strat$EI))
          cat(paste0("  Unique values this stratum: ", ei.numvals, "\n"))

          text.main <- curr.veg

          if (curr.EI == "PCGF") {
            text.x <- paste("Percent Cover ", curr.stratum, sep = "")
          } else if (curr.EI == "asc" || curr.EI == "ASC") {
            text.x <- paste("Abundance ", curr.stratum, sep = "")
          } else if (curr.EI == "GC" | curr.EI == "PCESS") {
            text.x <- paste("Percent Cover ", curr.stratum, sep = "")
          } else if (curr.EI == "SR") {
            text.x <- "Species Richness"
          } else if (curr.EI == "PES") {
            text.x <- "Proportion Exotic Species"
          } else if (curr.EI == "TD") {
            text.x <- "Taxonomic Distance from Benchmark Mean (Centroid)"
          }

          n.f <- nrow(EI.strat[EI.strat$focalOrBenchmark == "f" & !is.na(EI.strat$EI), ])
          n.bm <- nrow(EI.strat[EI.strat$focalOrBenchmark == "b" & !is.na(EI.strat$EI), ])

          f.text <- "Focal"
          bm.text <- "Benchmark"

          if (show.n == T) {
            f.text <- paste0("Focal (n=", n.f, ")")
            bm.text <- paste0("Benchmark (n=", n.bm, ")")
          }

          EIs <- as.vector(EI.strat$EI)

          under.construction <- FALSE


          cat(paste0("  Distribution=", distn, "\n"))

          if (has.data == T & distn == "Bet") {
            if (min(EI.strat$EI) == 0 || max(EI.strat$EI) == 1) {
              n <- nrow(EI.strat)
              EI.strat$EI <- (EI.strat$EI * (n - 1) + 0.5) / n
            }

            b <- EI.strat[EI.strat$focalOrBenchmark == "b", c("EI")]
            f <- EI.strat[EI.strat$focalOrBenchmark == "f", c("EI")]

            lowlim <- 0.0000001
            uplim <- 1 - lowlim

            shape1 <- 0.1
            shape2 <- 0.1

            xs <- seq(0, 1, length.out = 100)

            overlap <- boot.overlap(distn, b, f,
              xs = xs,
              boot.reps = boot.reps, seed = seed, set.seed = set.seed,
              shape1 = shape1, shape2 = shape2, lowlim = lowlim
            )
          } else if (has.data == T & distn == "NBin") {
            b <- EI.strat[EI.strat$focalOrBenchmark == "b", c("EI")]
            f <- EI.strat[EI.strat$focalOrBenchmark == "f", c("EI")]

            xs <- seq(0, max(max(f), max(b)) + 10) # range of x
            overlap <- boot.overlap(distn, b, f,
              xs = xs, boot.reps = boot.reps,
              seed = seed, set.seed = set.seed
            )
          } else if (has.data == T & ei.numvals > 1 & distn == "norm") {
            under.construction <- TRUE
          } else if (has.data == T & ei.numvals > 1 & distn == "gamma") {
            if (min(EI.strat$EI) < 0) {
              stop("ERROR: Values < 0 not allowed for gamma distribution!")
            }

            b <- EI.strat[EI.strat$focalOrBenchmark == "b", c("EI")]
            f <- EI.strat[EI.strat$focalOrBenchmark == "f", c("EI")]

            lowlim <- 0.0000001
            uplim <- max(EI.strat$EI) + (0.1 * max(EI.strat$EI))

            shape <- 1
            rate <- 1

            xs <- seq(0, max.ei, length.out = 1000)

            overlap <- boot.overlap(
              distn,
              v1 = b, v2 = f, xs = xs, boot.reps = boot.reps,
              seed = seed, set.seed = set.seed, shape = shape,
              rate = rate, lowlim = lowlim, uplim = uplim
            )
          } # END Fit distribution


          if (!overlap[[1]] == "FAIL") {
            ol <- overlap$ol.obs
            ol.sd <- overlap$ol.sd
            ol.lcl <- overlap$ol.cls[1]
            ol.ucl <- overlap$ol.cls[2]
            boot.ol <- overlap$boot.ol
            do.graph <- TRUE

            curr.boot.ol <- data.frame(curr.veg, curr.stratum, boot.reps, ol, t(boot.ol))
            boot.ol.df <- rbind(boot.ol.df, curr.boot.ol)
          } else {
            ol <- NA
            ol.sd <- NA
            ol.lcl <- NA
            ol.ucl <- NA
            boot.ol <- rep(NA, length(xs))
            do.graph <- FALSE
          }


          title.text <- text.main

          if (curr.stratum == "nostrata") {
            graphFileName <- paste(curr.EI, "overlap", veg.fname, sep = "_")
          } else {
            graphFileName <- paste(curr.EI, "overlap", veg.fname, stratum.fname, sep = "_")
          }

          if (overlap[1] == "FAIL") {
            cat("  WARNING: Cannot generate overlap figure for '", graphFileName, "'\n", sep = "")
          } else {
            ol.disp <- specify_decimal(ol, 3)
            ol.lcl.disp <- specify_decimal(ol.lcl, 3)
            ol.ucl.disp <- specify_decimal(ol.ucl, 3)
            cat(paste0("  Overlap=", ol.disp, " (", ol.lcl.disp, "-", ol.ucl.disp, ")\n"))

            if (do.graph) {
              if (nchar(text.x) <= 30) {
                title.text <- paste(text.x, curr.veg, sep = " - ")
              } else {
                title.text <- paste(curr.EI, curr.veg, sep = " - ")
              }

              graphFile <- paste(figs.fitted.dir, "/", graphFileName, ".", fig.file.type, sep = "")
              cat(paste0("  Printing graph: ", graphFileName, "\n"))

              if (!interactive()) {
                if (fig.file.type == "png") {
                  png(graphFile)
                } else if (fig.file.type == "pdf") {
                  pdf(graphFile,
                    onefile = TRUE, bg = "white", family = fig.font.fam,
                    width = fig.width.cm / 2.5
                  )
                } else {
                  err.msg <- paste0(
                    "ERROR: fig.file.type '", fig.file.type,
                    "' not supported! (see file graph.dists.R)"
                  )
                  stop(err.msg)
                }
              } # END interactive

              graph <- boot.ol.plot(overlap,
                title.text = title.text,
                f.text = f.text,
                bm.text = bm.text,
                subsample = boot.graph.subsample,
                plot.boot = plot.boot.pdfs
              )

              if (interactive()) {
                if (fig.file.type == "png") {
                  dev.copy(png, graphFile)
                } else if (fig.file.type == "pdf") {
                  dev.copy(pdf, graphFile)
                }
              } # END interactive

              invisible(dev.off())
            } else if (under.construction == TRUE) {
              print("  This distribution under construction!")
            } else if (has.data == FALSE) {
              print("  No data for this EI+stratum combination")
            }
          }
        } # End strata loop
      } # end veg loop


      boot.ol.file <- paste(RESULTSDIR, boot.ol.filename, sep = "")
      write.csv(boot.ol.df, file = boot.ol.file, row.names = FALSE)

      cat("\n")
    },
    envir = state
  )
  invisible(state)
}

plot_indicator_distributions <- function(state) {
  evalq(
    {
      cat("***************************************\n")
      cat("Generating indicator distribution figures\n")
      cat("***************************************\n\n")

      if (plot.hist == TRUE) {
        group.strata <- FALSE

        if (q.method == "fixed") {
          plot.fit <- FALSE
        } else {
          plot.fit <- plot.pdf
        }


        for (opt0 in 1:2) {
          if (opt0 == 2) {
            if (allow.group.strata == TRUE) {
              group.strata <- TRUE
            } else {
              break # Skip second pass if grouping not allowed
            }
          }
          rescale <- FALSE

          for (opt1 in 1:2) {
            if (opt1 == 2) rescale <- TRUE
            plot.bm <- FALSE

            for (opt2 in 1:2) {
              if (opt2 == 2) plot.bm <- TRUE
              plot.focal <- FALSE

              for (opt3 in 1:2) {
                if (opt3 == 2) plot.focal <- TRUE

                if (!(plot.bm == FALSE & plot.focal == FALSE)) {
                  plot_indicator_distribution(environment())
                }
              }
            }
          }
        }
      }
    },
    envir = state
  )
  invisible(state)
}

plot_indicator_distribution <- function(state) {
  evalq(
    {
      do.debug <- FALSE


      if (exists("FIG.TYPE")) {
        fig.file.type <- FIG.TYPE
      } else {
        fig.file.type <- "png"
      }

      fig.font.fam <- "Helvetica"

      pdf.wid.def <- 7
      pdf.ht.def <- 7

      pdf.wid.grouped <- pdf.wid.def + 2
      pdf.ht.grouped <- pdf.ht.def + 2
      pdf.wid.grouped.onerow <- pdf.wid.def + 2
      pdf.ht.grouped.onerow <- pdf.ht.def / 1.2

      pdf.wid.cm <- 9
      pdf.wid.single <- pdf.wid.cm / 1.4

      png.dpi.def <- 72 # DPI (ignored for pdf)
      png.wid.def <- 480
      png.ht.def <- 480

      png.dpi <- png.dpi.def
      png.wid <- png.wid.def
      png.ht <- png.ht.def


      curr.EI <- as.character(curr.EI) # Just in case this is factor in calling script


      text.x <- EI.name # Histogram x axis text
      text.y <- "Number of Plots" # Histogram y axis text
      alpha.val <- 0.25 # Transparency of overlaps, from 0 to 1

      no.bins <- 12 # Number of bins to use for histogram, adjust as needed

      plot.figs <- T
      if (plot.bm == F && plot.focal == F) plot.figs <- F

      plot.mod.results <- T

      plot.fit.bak <- plot.fit
      if (q.method == "fixed") plot.fit <- F

      leg.x.extra <- 0.45
      leg.y.extra <- NA
      leg.x.extra <- -0.1
      leg.y.extra <- 0

      leg.x.extra.g <- -0.6
      leg.y.extra.g <- 0
      leg.x.extra.g <- -0.32
      leg.y.extra.g <- -0.05

      box.bg.color <- "white"
      box.bg.color <- "transparent"


      rawFileName <- paste(curr.EI, "_raw.csv", sep = "") # raw data
      f.results.filename <- paste0(curr.EI, "_focal.csv") # focal pdfs
      pdf.f.filename <- paste0(curr.EI, "_pdf.f.csv") # focal pdfs
      pdf.b.filename <- paste0(curr.EI, "_pdf.b.csv") # bm pdfs
      pdf.xs.filename <- paste0(curr.EI, "_pdf.xs.csv") # x values for both pdfs

      rawFile <- paste(RESULTSDIR, rawFileName, sep = "")
      dat.ei <- read.csv(rawFile, header = T, stringsAsFactors = FALSE)
      names(dat.ei)[names(dat.ei) == "landCover"] <- "landcover"
      names(dat.ei)[names(dat.ei) == "vegClass"] <- "bm.vegetation"
      dat.ei <- with(dat.ei, dat.ei[order(
        dat.ei$landcover, dat.ei$bm.vegetation,
        dat.ei$focalOrBenchmark, dat.ei$plotCode, dat.ei$stratum
      ), ])

      f.results.file <- paste(RESULTSDIR, f.results.filename, sep = "")
      f.results <- read.csv(f.results.file, header = T)

      pdf.f.file <- paste(RESULTSDIR, pdf.f.filename, sep = "")
      pdf.f.df <- read.csv(pdf.f.file, header = T)

      pdf.b.file <- paste(RESULTSDIR, pdf.b.filename, sep = "")
      pdf.b.df <- read.csv(pdf.b.file, header = T)

      pdf.xs.file <- paste(RESULTSDIR, pdf.xs.filename, sep = "")
      pdf.xs.df <- read.csv(pdf.xs.file, header = T)

      if (distn == "Bet") {
        pdf.f.df[is.na(pdf.f.df)] <- Inf
        pdf.b.df[is.na(pdf.b.df)] <- Inf
      }


      figs.fitted.dir.name <- "dists_fitted"

      if (plot.bm == T && plot.focal == F) {
        figs.fitted.dir.name <- paste(figs.fitted.dir.name, "_bm", sep = "")
      } else if (plot.bm == F && plot.focal == T) {
        figs.fitted.dir.name <- paste(figs.fitted.dir.name, "_focal", sep = "")
      }

      if (group.strata == T) {
        figs.fitted.dir.name <- paste(figs.fitted.dir.name, "_grouped", sep = "")
      }

      if (rescale == T) {
        figs.fitted.dir.name <- paste(figs.fitted.dir.name, "_rescaled", sep = "")
      }

      if (plot.fit == F && no.fit.keep.separate == T) {
        figs.fitted.dir.name <- paste(figs.fitted.dir.name, "_nofit", sep = "")
      }

      make.plots <- TRUE

      if (figs.fitted.dir.name == "dists_fitted") {
        if (plot.dists_fitted == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_bm") {
        if (plot.dists_fitted_bm == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_bm_grouped") {
        if (plot.dists_fitted_bm_grouped == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_bm_grouped_nofit") {
        if (plot.dists_fitted_bm_grouped_nofit == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_bm_grouped_rescaled") {
        if (plot.dists_fitted_bm_grouped_rescaled == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_bm_grouped_rescaled_nofit") {
        if (plot.dists_fitted_bm_grouped_rescaled_nofit == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_bm_nofit") {
        if (plot.dists_fitted_bm_nofit == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_bm_rescaled") {
        if (plot.dists_fitted_bm_rescaled == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_bm_rescaled_nofit") {
        if (plot.dists_fitted_bm_rescaled_nofit == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_focal") {
        if (plot.dists_fitted_focal == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_focal_grouped") {
        if (plot.dists_fitted_focal_grouped == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_focal_grouped_nofit") {
        if (plot.dists_fitted_focal_grouped_nofit == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_focal_grouped_rescaled") {
        if (plot.dists_fitted_focal_grouped_rescaled == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_focal_grouped_rescaled_nofit") {
        if (plot.dists_fitted_focal_grouped_rescaled_nofit == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_focal_nofit") {
        if (plot.dists_fitted_focal_nofit == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_focal_rescaled") {
        if (plot.dists_fitted_focal_rescaled == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_focal_rescaled_nofit") {
        if (plot.dists_fitted_focal_rescaled_nofit == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_grouped") {
        if (plot.dists_fitted_grouped == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_grouped_nofit") {
        if (plot.dists_fitted_grouped_nofit == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_grouped_rescaled") {
        if (plot.dists_fitted_grouped_rescaled == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_grouped_rescaled_nofit") {
        if (plot.dists_fitted_grouped_rescaled_nofit == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_nofit") {
        if (plot.dists_fitted_nofit == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_rescaled") {
        if (plot.dists_fitted_rescaled == FALSE) make.plots <- FALSE
      } else if (figs.fitted.dir.name == "dists_fitted_rescaled_nofit") {
        if (plot.dists_fitted_rescaled_nofit == FALSE) make.plots <- FALSE
      }

      cat("Saving figures to '", FIGDIR_REL, figs.fitted.dir.name, "/'", sep = "")

      if (make.plots == TRUE) {
        cat(":\n") # Close the directory name header and move to new line

        dir.create(file.path(FIGDIR, figs.fitted.dir.name), showWarnings = F)
        figs.fitted.dir <- paste(FIGDIR, figs.fitted.dir.name, sep = "")


        all.lc <- unique(dat.ei$landcover[dat.ei$focalOrBenchmark == "f"])
        n.lc <- length(all.lc)

        for (j in 1:n.lc) { # START land cover loop
          curr.lc <- all.lc[j]

          lc.fname <- unix.friendly(curr.lc)

          strata <- unique(dat.ei$stratum[dat.ei$landcover == curr.lc])
          n.strata <- length(strata)

          if (n.strata == 1) group.strata <- FALSE


          fig.cols <- 1 # Dummy values
          fig.rows <- 1

          par(mar = c(4, 4, 2, 2))

          if (plot.figs == TRUE) {
            if (n.strata > 1 && group.strata == TRUE) {
              dims <- opt.grid(n.strata) # Optimum grid dimensions given # of strata
              fig.cols <- dims[1]
              fig.rows <- dims[2]
              par(
                bg = "white",
                mfrow = c(fig.rows, fig.cols),
                oma = c(4, 4, 4, 3)
              )
            } else {
              par(bg = "white")
            }
          }


          if (group.strata == TRUE) { # BEGIN group.strata==TRUE
            graphFileName <- paste(curr.EI, "dists_fitted", lc.fname, sep = "_")
            graphFileNameFull <- paste0(graphFileName, ".", fig.file.type)
            graphFile <- paste(figs.fitted.dir, "/", graphFileNameFull, sep = "")
            cat(paste0("  ", graphFileNameFull, "\n"))

            if (interactive() == FALSE) {
              if (fig.file.type == "png") {
                png(graphFile, res = png.dpi, height = png.ht, width = png.ht)
              } else if (fig.file.type == "pdf") {
                pdf(graphFile,
                  onefile = TRUE, bg = "white", family = fig.font.fam,
                  width = pdf.wid.grouped
                )
              } else {
                err.msg <- paste0(
                  "ERROR: fig.file.type '", fig.file.type,
                  "' not supported! (see file graph.dists.R)"
                )
                stop(err.msg)
              }
            }

            n.k.skipped <- 0 # Number of strata skipped (due failed pdf fits, etc)

            for (k in 1:n.strata) {
              curr.stratum <- strata[k]
              curr.results <- f.results[f.results$landcover == curr.lc &
                f.results$stratum == curr.stratum, ]

              curr.dat.f <- dat.ei[dat.ei$landcover == curr.lc &
                dat.ei$stratum == curr.stratum & dat.ei$focalOrBenchmark == "f", ]
              curr.veg <- unique(curr.dat.f$bm.vegetation)
              curr.dat.b <- dat.ei[dat.ei$bm.vegetation == curr.veg &
                dat.ei$stratum == curr.stratum & dat.ei$focalOrBenchmark == "b", ]

              pdf.f <- pdf.f.df[pdf.f.df$landcover == curr.lc & pdf.f.df$stratum == curr.stratum, ]
              pdf.f <- pdf.f[, -which(names(pdf.f) %in% c("landcover", "stratum", "f.hat"))]
              pdf.f <- as.vector(unlist(pdf.f))

              pdf.b <- pdf.b.df[pdf.b.df$landcover == curr.lc & pdf.b.df$stratum == curr.stratum, ]
              pdf.b <- pdf.b[, -which(names(pdf.b) %in% c("landcover", "stratum", "b.hat"))]
              pdf.b <- as.vector(unlist(pdf.b))

              if (q.method == "empirical" && (length(pdf.f) == 0 || length(pdf.b) == 0)) {
                n.k.skipped <- n.k.skipped + 1
              }
              n.strata.actual <- n.strata - n.k.skipped
            }
          } # END group.strata==TRUE

          n.k.skipped <- 0 # Number of strata skipped (due failed pdf fits, etc)

          for (k in 1:n.strata) { # START strata loop

            curr.stratum <- strata[k]
            stratum.fname <- unix.friendly(curr.stratum) # Unix-friendly version for filename


            curr.results <- f.results[f.results$landcover == curr.lc &
              f.results$stratum == curr.stratum, ]

            curr.dat.f <- dat.ei[dat.ei$landcover == curr.lc & dat.ei$stratum == curr.stratum &
              dat.ei$focalOrBenchmark == "f", ]
            curr.veg <- unique(curr.dat.f$bm.vegetation)
            curr.dat.b <- dat.ei[dat.ei$bm.vegetation == curr.veg &
              dat.ei$stratum == curr.stratum & dat.ei$focalOrBenchmark == "b", ]

            f <- curr.dat.f$EI
            f <- f[!is.na(f)]
            b <- curr.dat.b$EI
            b <- b[!is.na(b)]

            ei.all <- c(f, b)

            pdf.f <- pdf.f.df[pdf.f.df$landcover == curr.lc & pdf.f.df$stratum == curr.stratum, ]
            pdf.f <- pdf.f[, -which(names(pdf.f) %in% c("landcover", "stratum", "f.hat"))]
            pdf.f <- as.vector(unlist(pdf.f))

            pdf.b <- pdf.b.df[pdf.b.df$landcover == curr.lc & pdf.b.df$stratum == curr.stratum, ]
            pdf.b <- pdf.b[, -which(names(pdf.b) %in% c("landcover", "stratum", "b.hat"))]
            pdf.b <- as.vector(unlist(pdf.b))

            xs <- as.vector(unlist(pdf.xs.df))

            if (q.method == "empirical" && (length(pdf.f) == 0 || length(pdf.b) == 0)) {
              n.k.skipped <- n.k.skipped + 1
              next
            }


            n.f <- length(f)
            n.b <- length(b)

            min.ei <- min(ei.all)
            max.ei <- max(ei.all)

            if (show.n == T) {
              f.text <- paste0("Focal (n=", n.f, ")")
              b.text <- paste0("Benchmark (n=", n.b, ")")
            } else {
              f.text <- "Focal"
              b.text <- "Benchmark"
            }

            b.fill <- c(rgb(0, 0, 1, alpha.val))
            f.fill <- c(rgb(1, 0, 0, alpha.val))
            bf.fill <- c(rgb(0, 0, 1, alpha.val), rgb(1, 0, 0, alpha.val))
            border.val <- "black"

            if (LEGEND.NO.FB == FALSE) {
              f.text <- ""
              b.text <- ""
              b.fill <- NULL
              f.fill <- NULL
              bf.fill <- NULL
              border.val <- "white"
            }

            text.main <- curr.lc

            if (group.strata == FALSE) {
              text.main <- curr.lc

              if (!curr.stratum == "nostrata" && !curr.stratum == "") {
                if (!exists("lc.in.title")) {
                  ti.stratum.only <- FALSE
                } else {
                  if (lc.in.title == FALSE) {
                    ti.stratum.only <- TRUE
                  } else {
                    ti.stratum.only <- FALSE
                  }
                }

                if (ti.stratum.only == TRUE) {
                  text.main <- curr.stratum
                } else {
                  text.main <- paste0(curr.lc, ":  ", curr.stratum)

                  ti.length <- nchar(text.main)
                  if (ti.length > 45) text.main <- paste0(curr.lc, ":\n", curr.stratum)
                }
              }
            } else {
              text.main <- curr.stratum
            }

            if (curr.EI %in% c("PCGF", "GC", "PCS")) {
              if (group.strata == T) {
                text.x <- paste("Percent Cover", sep = "")
              } else {
                text.x <- paste("Percent Cover ", curr.stratum, sep = "")
              }
            } else if (curr.EI == "PCESS") {
              if (group.strata == T) {
                text.x <- paste("Percent Cover Exotic Species", sep = "")
              } else {
                text.x <- paste("Percent Cover Exotic Species (", curr.stratum, ")", sep = "")
              }
            } else if (curr.EI == "asc" || curr.EI == "ASC") {
              text.x <- paste0("Number of individuals")
            }

            if (distn == "Bet") {
              xlim.min <- 0
              xlim.max <- 1

              ysize <- sum(ei.all) * 1.1
              ylim.min <- 0
              ylim.max <- ysize

              obs.x <- seq(0, 1, length.out = no.bins)
            } else if (distn == "NBin") {
              xlim.min <- 0
              xlim.max <- max.ei + (max.ei * 0.1)

              ysize <- sum(ei.all) * 1.1
              ylim.min <- 0
              ylim.max <- ysize

              obs.x <- seq(0, max.ei + (max.ei * 0.1), length.out = no.bins)

              if (exists("x.ticks")) rm(x.ticks) # Clear previous values if any
              if (max.ei < no.bins) x.ticks <- seq(0, max.ei + 1)
            } else if (distn == "gamma") {
              xlim.min <- 0
              xlim.max <- max.ei + (max.ei * 0.1)

              ysize <- sum(ei.all) * 1.1 # was ysize<-sum(curr.dat $EI)*1.1
              ylim.min <- 0
              ylim.max <- NA

              obs.x <- seq(0, xlim.max, length.out = no.bins)
            }

            h.b <- hist(b, breaks = obs.x, plot = FALSE)
            h.f <- hist(f, breaks = obs.x, plot = FALSE)

            highestCount <- max(h.b$counts, h.f$counts)
            if (plot.focal == F) {
              highestCount <- max(h.b$counts) # Use bm values only
            } else if (plot.bm == F) {
              highestCount <- max(h.f$counts) # Use focal values only
            }
            ylim.max <- highestCount + (highestCount * 0.4)

            freq.val <- T
            if (rescale == T) {
              h.b$density <- h.b$counts / sum(h.b$counts)
              h.f$density <- h.f$counts / sum(h.f$counts)
              freq.val <- F
              text.y <- "Proportion of plots"

              highestDensity <- max(h.b$density, h.f$density)
              if (plot.focal == F) {
                highestDensity <- max(h.b$density) # Use bm values only
              } else if (plot.bm == F) {
                highestDensity <- max(h.f$density) # Use focal values only
              }
              ylim.max <- min(1, highestDensity + 0.1)

              if (length(pdf.b) > 0) {
                pdf.b <- pdf.b / max(pdf.b[pdf.b < Inf & !is.na(
                  pdf.b
                )]) * max(h.b$density) * 0.90
              }
              if (length(pdf.f) > 0) {
                pdf.f <- pdf.f / max(pdf.f[pdf.f < Inf & !is.na(
                  pdf.f
                )]) * max(h.f$density) * 0.90
              }
            } else {
              if (length(pdf.b) > 0) {
                pdf.b <- pdf.b / max(pdf.b[pdf.b < Inf & !is.na(
                  pdf.b
                )]) * max(h.b$counts) * 0.90
              }
              if (length(pdf.f) > 0) {
                pdf.f <- pdf.f / max(pdf.f[pdf.f < Inf & !is.na(
                  pdf.f
                )]) * max(h.f$counts) * 0.90
              }
            }

            if (has.stratum == T && group.strata == T && n.strata > 1) {
              del.y <- FALSE
              del.x <- FALSE
              k.actual <- k - n.k.skipped


              if (n.strata.actual == 4) {
                del.x <- FALSE
                if (k.actual %in% c(1, 2)) del.x <- TRUE
              } else if (n.strata.actual == 3) {
                del.x <- FALSE
                if (k.actual == 1) del.x <- TRUE
              } else if (n.strata.actual == 2) {
                del.x <- FALSE
              } # Add more here as needed

              if (del.y == TRUE) text.y <- ""
              if (del.x == TRUE) text.x <- ""
            }

            ull.text <- ""

            if (plot.focal == T && plot.mod.results == T) { # START Upper-left legend text

              perc.diff <- NA
              perc.diff.text <- NA
              p.diff <- curr.results$p.diff
              if (is.null(p.diff)) p.diff <- NA # Set to NA instead to allow boolean tests
              f.mean <- curr.results$f.mean
              b.mean <- curr.results$b.mean
              if (!is.na(p.diff)) {
                perc.diff <- f.mean / b.mean
                perc.diff.text <- as.character(round(perc.diff, digits = 2))
              }
              ol <- curr.results$ol
              ol.lcl <- curr.results$ol.lcl
              ol.ucl <- curr.results$ol.ucl
              q <- curr.results$q
              q.lcl <- curr.results$q.lcl
              q.ucl <- curr.results$q.ucl

              if (f.mean > b.mean) {
                f.diff <- "greater"
              } else if (f.mean < b.mean) {
                f.diff <- "less"
              } else {
                f.diff <- "same"
              }

              result.text <- ""
              if (!is.na(p.diff) && !is.na(perc.diff)) { # START result.text

                if (p.diff < 0.001) {
                  if (f.diff == "greater") {
                    f.to.b.text <- "Focal > Benchmark"
                  } else if (f.diff == "less") {
                    f.to.b.text <- "Focal < Benchmark"
                  }
                  result.text <- paste(f.to.b.text, " (p<0.001)", sep = "")
                } else {
                  p.val.mod.text <- as.character(specify_decimal(p.diff, 3))
                  if (p.diff >= 0.05) {
                    result.text <- paste("Focal = Benchmark (p>0.05)", sep = "")
                  } else {
                    if (f.diff == "greater") {
                      f.to.b.text <- "Focal > Benchmark"
                    } else if (f.diff == "less") {
                      f.to.b.text <- "Focal < Benchmark"
                    }
                    result.text <- paste(f.to.b.text, " (p=",
                      p.val.mod.text, ")",
                      sep = ""
                    )
                  }
                }
              } # END result.text

              if (is.na(q)) {
                q.text <- ""
              } else {
                if (Q.LEGEND.PERCENT == TRUE) {
                  q.fixed <- as.character(specify_decimal(q * 100, 1))
                  q.text <- paste0("Q: ", q.fixed, "%")

                  if (!(q.lcl == 1 && q.ucl == 1) && !is.na(q.lcl) && !is.na(q.ucl)) {
                    q.lcl.fixed <- as.character(specify_decimal(q.lcl * 100, 1))
                    q.ucl.fixed <- as.character(specify_decimal(q.ucl * 100, 1))
                    q.text <- paste0(q.text, " [", q.lcl.fixed, ", ", q.ucl.fixed, "%]")
                  }
                } else {
                  q.fixed <- as.character(specify_decimal(q, 2))
                  q.text <- paste0("Q: ", q.fixed)

                  if (!(q.lcl == 1 && q.ucl == 1) && !is.na(q.lcl) && !is.na(q.ucl)) {
                    if (q.lcl < 0.01) {
                      q.lcl.fixed <- as.character(specify_decimal(q.lcl, 3))
                    } else {
                      q.lcl.fixed <- as.character(specify_decimal(q.lcl, 2))
                    }

                    if (q.ucl < 0.01) {
                      q.ucl.fixed <- as.character(specify_decimal(q.ucl, 3))
                    } else {
                      q.ucl.fixed <- as.character(specify_decimal(q.ucl, 2))
                    }

                    q.text <- paste0(q.text, " [", q.lcl.fixed, ", ", q.ucl.fixed, "]")
                  }
                }
              }

              if (is.na(ol) || show.overlap == F) {
                ol.text <- ""
              } else {
                ol.fixed <- as.character(specify_decimal(ol * 100, 0))

                if (test.tail == "upper") {
                  ol.text <- paste0("Overlap (f<b, upper tail only):", ol.fixed, "%")
                } else if (test.tail == "lower") {
                  ol.text <- paste0("Overlap (f<b, lower tail only): ", ol.fixed, "%")
                } else {
                  ol.text <- paste0("Overlap: ", ol.fixed, "%")
                }

                if (!(ol.lcl == 1 && ol.ucl == 1) && !is.na(ol.lcl) && !is.na(ol.ucl)) {
                  ol.lcl.fixed <- as.character(specify_decimal(ol.lcl * 100, 1))
                  ol.ucl.fixed <- as.character(specify_decimal(ol.ucl * 100, 1))
                  ol.text <- paste0(ol.text, " [", ol.lcl.fixed, ", ", ol.ucl.fixed, "%]")
                }
              }

              if (q.method == "fixed") {
                if (beta.algorithm == "overlap") {
                  ull.text <- paste0(q.text)
                } else {
                  ull.text <- paste0(result.text, "\n", q.text)
                }
              } else if (beta.algorithm == "overlap") {
                ull.text <- q.text
              } else {
                if (ol.text == "") {
                  ull.text <- paste0(result.text, "\n", q.text)
                } else {
                  ull.text <- paste0(result.text, "\n", ol.text, "\n", q.text)
                }
              }

              if (quality.legend.only == TRUE) ull.text <- q.text
            } # END  Upper-left legend text


            if (group.strata == FALSE) {
              if (curr.stratum == "nostrata") {
                graphFileName <- paste(curr.EI, "dists_fitted", lc.fname, sep = "_")
              } else {
                graphFileName <- paste(curr.EI, "dists_fitted", lc.fname, stratum.fname, sep = "_")
              }

              graphFileNameFull <- paste0(graphFileName, ".", fig.file.type)
              graphFile <- paste(figs.fitted.dir, "/", graphFileNameFull, sep = "")
              cat(paste0("  ", graphFileNameFull, "\r\n"))

              if (interactive() == FALSE) {
                if (do.debug == TRUE) {
                  cat(
                    "  DEBUG: Printing single plot from Rscript command line...\n"
                  )
                } # Debugging

                if (fig.file.type == "png") {
                  png(graphFile, res = png.dpi, height = png.ht, width = png.ht)
                } else if (fig.file.type == "pdf") {
                  pdf(graphFile,
                    onefile = TRUE, bg = "white", family = fig.font.fam,
                    width = pdf.wid.single
                  )
                } else {
                  err.msg <- paste0(
                    "ERROR: fig.file.type '", fig.file.type,
                    "' not supported! (see file graph.dists.R)"
                  )
                  stop(err.msg)
                }
              }
            }


            if (plot.bm == T) {
              if (exists("x.ticks")) {
                plot(h.b,
                  main = text.main,
                  font.main = 1, # Plain text title; default is bold for titles
                  xlab = text.x,
                  ylab = text.y,
                  col = rgb(0, 0, 1, alpha.val),
                  ylim = c(ylim.min, ylim.max),
                  freq = freq.val,
                  xaxt = "n"
                )
                axis(1,
                  at = x.ticks,
                  labels = x.ticks,
                  las = 0
                )
              } else {
                plot(h.b,
                  main = text.main,
                  font.main = 1, # Plain text title; default is bold for titles
                  xlab = text.x,
                  ylab = text.y,
                  col = rgb(0, 0, 1, alpha.val),
                  xlim = c(xlim.min, xlim.max),
                  ylim = c(ylim.min, ylim.max),
                  freq = freq.val
                )
              }
            }

            if (plot.focal == T) {
              add.val <- T
              if (plot.bm == F) add.val <- F

              if (exists("x.ticks")) {
                plot(h.f,
                  main = text.main,
                  font.main = 1, # Plain text title; default is bold for titles
                  xlab = text.x,
                  ylab = text.y,
                  col = rgb(1, 0, 0, alpha.val),
                  ylim = c(ylim.min, ylim.max),
                  freq = freq.val,
                  xaxt = "n",
                  add = add.val
                )
                axis(1,
                  at = x.ticks,
                  labels = x.ticks,
                  las = 0
                )
              } else {
                plot(h.f,
                  main = text.main,
                  font.main = 1, # Plain text title; default is bold for titles
                  xlab = text.x,
                  ylab = text.y,
                  col = rgb(1, 0, 0, alpha.val),
                  xlim = c(xlim.min, xlim.max),
                  ylim = c(ylim.min, ylim.max),
                  freq = freq.val,
                  add = add.val
                )
              }
            }

            if (q.method == "fixed" && !is.na(bm.val)) {
              abline(v = bm.val, col = "black", lwd = 3, lty = 2)
            }

            if (plot.fit == T) { # START Add fit lines

              if (plot.bm == T) {
                curve1 <- lines(xs, pdf.b,
                  lwd = 2,
                  col = rgb(0, 0, 1, alpha.val)
                )
              }

              if (plot.focal == T) {
                curve2 <- lines(xs, pdf.f,
                  lwd = 2,
                  col = rgb(1, 0, 0, alpha.val)
                )
              }
            } # END The pdf fit lines

            if (plot.legends == T) {
              inset.default <- c(0.1, 0) # Default
              inset.f.default <- c(0.17, 0.07) # Default, f only
              inset.qual.comb <- c(0.55, 0) # Quality legend, combined with f/b
              inset.beta <- c(0.58, 0.07) # Beta indicator legend
              inset.group <- c(0, 0) # Group (multi-panel) figures

              if (group.strata == TRUE) {
                inset.default <- c(0.4, -0.05) # Default
                inset.f.default <- c(0.4, -0.05) # Default, f only
                inset.qual.comb <- c(0.4, -0.05) # Quality legend, combined with f/b
                inset.beta <- c(0.45, 0.1) # Beta indicator legend
                inset.group <- c(0.4, -0.05) # Group (multi-panel) figures
              }

              if (group.strata == FALSE) {
                if (exists("leg.x.extra") && is.num(leg.x.extra)) {
                  inset.default[1] <- inset.default[1] + leg.x.extra
                  inset.f.default[1] <- inset.f.default[1] + leg.x.extra
                  inset.qual.comb[1] <- inset.qual.comb[1] + leg.x.extra
                  inset.beta[1] <- inset.beta[1] + leg.x.extra
                }
                if (exists("leg.y.extra") && is.num(leg.y.extra)) {
                  inset.default[2] <- inset.default[2] + leg.y.extra
                  inset.f.default[2] <- inset.f.default[2] + leg.x.extra
                  inset.qual.comb[2] <- inset.qual.comb[2] + leg.y.extra
                  inset.beta[2] <- inset.beta[2] + leg.y.extra
                }
              } else {
                if (group.strata == T && exists("leg.x.extra.g") && is.num(leg.x.extra.g)) {
                  inset.default[1] <- inset.default[1] + leg.x.extra.g
                  inset.f.default[1] <- inset.f.default[1] + leg.x.extra.g
                  inset.qual.comb[1] <- inset.qual.comb[1] + leg.x.extra.g
                  inset.beta[1] <- inset.beta[1] + leg.x.extra.g
                }
                if (group.strata == T && exists("leg.y.extra.g") && is.num(leg.y.extra.g)) {
                  inset.default[2] <- inset.default[2] + leg.y.extra.g
                  inset.f.default[2] <- inset.f.default[2] + leg.y.extra.g
                  inset.qual.comb[2] <- inset.qual.comb[2] + leg.y.extra.g
                  inset.beta[2] <- inset.beta[2] + leg.y.extra.g
                }
              }


              if (
                (group.strata == F) ||
                  (group.strata == T && k == fig.cols) ||
                  (group.strata == T && group.strata.print.all.n == TRUE)
              ) {
                if (plot.focal == T && plot.bm == T) {
                  if (beta.algorithm == "overlap" || legend.combined == TRUE) {
                    legend("topleft",
                      legend = c(b.text, f.text),
                      inset = inset.beta,
                      fill = bf.fill,
                      border = border.val,
                      box.lty = 0
                    )
                  } else {
                    legend("topright",
                      legend = c(b.text, f.text),
                      inset = inset.default,
                      fill = bf.fill,
                      border = border.val,
                      box.lty = 0
                    )
                  }
                } else if (plot.focal == T && plot.bm == F) {
                  legend("topright",
                    inset = inset.f.default,
                    legend = c(f.text),
                    fill = f, fill,
                    border = border.val,
                    box.lty = 0
                  )
                } else { # plot.focal==F && plot.bm==T
                  legend("topright",
                    inset = inset.default,
                    legend = c(b.text),
                    fill = b.fill,
                    border = border.val,
                    box.lty = 0
                  )
                } # END Set f, b or f/b combined
              } # END  Print f/b colour legend (+optional n)

              if (plot.focal == T && plot.mod.results == T) {
                if (legend.combined == TRUE) {
                  legend("topleft",
                    inset = inset.qual.comb,
                    box.lty = 0, # Comment out to add box for troubleshooting
                    x.intersp = -0.5, # Move text horizontally within legend box
                    bg = box.bg.color, # Box background
                    xjust = 0, # Right-justify text in legend box
                    legend = ull.text
                  )
                } else {
                  legend("topleft",
                    legend = ull.text,
                    inset = inset.default,
                    box.lty = 0 # Comment out to add box for troubleshooting
                  )
                }
              } # END Print quality legend
            } # END Print figure legends

            if (group.strata == FALSE) {
              if (interactive() == TRUE) {
                if (do.debug == TRUE) {
                  cat(
                    "  DEBUG: Printing single plot from R Studio...\n"
                  )
                } # Debugging

                if (fig.file.type == "png") {
                  dev.copy(png, graphFile)
                } else if (fig.file.type == "pdf") {
                  dev.copy(pdf, graphFile)
                }
              } # END interactive


              invisible(dev.off())
            }
          } # End strata loop

          if (group.strata == TRUE) {
            ei.name.alt <- gsub("Percent Cover", "Cover", ei.name, ignore.case = TRUE)
            ti.group <- paste0(ei.name.alt, " — ", curr.lc)

            if (interactive() == TRUE) {
              if (do.debug == TRUE) {
                cat(
                  "  DEBUG: Printing multipanel plot from R Studio...\n"
                )
              } # Debugging

              mtext(ti.group, side = 3, line = GROUP.MTEXT.LINE, cex = 1, font = 2, outer = TRUE)

              if (fig.rows == 1) {
                if (fig.file.type == "png") {
                  dev.copy(png, graphFile, width = 800, height = 500)
                } else if (fig.file.type == "pdf") {
                  dev.copy(pdf, graphFile,
                    width = pdf.wid.grouped.onerow,
                    height = pdf.ht.grouped.onerow
                  )
                }
              } else {
                if (fig.file.type == "png") {
                  dev.copy(png, graphFile)
                } else if (fig.file.type == "pdf") {
                  dev.copy(pdf, graphFile, width = pdf.wid.grouped, height = pdf.ht.grouped)
                }
              }
            } else { # interactive()==FALSE

              if (fig.file.type == "png") {
                png(graphFile, res = png.dpi, height = png.ht, width = png.ht)
              } else if (fig.file.type == "pdf") {
                pdf(graphFile,
                  onefile = TRUE, bg = "white", family = fig.font.fam,
                  width = pdf.wid.grouped
                )
              } else {
                err.msg <- paste0(
                  "ERROR: fig.file.type '", fig.file.type,
                  "' not supported! (see file graph.dists.R)"
                )
                stop(err.msg)
              }
            } # END if interactive()

            invisible(dev.off())
          }
        } # end land cover loop

        if (plot.figs == F) Print("WARNING: No figures printed (plot.figs==FALSE)")

        plot.fit <- plot.fit.bak
      } else {
        cat("...skipping\n")
      }
    },
    envir = state
  )
  invisible(state)
}
