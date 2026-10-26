prepare_indicator_results <- function(state) {
  evalq(
    {
      cat("Summarizing focal values...")


      curr.EI <- as.character(curr.EI)


      rawFileName <- paste(curr.EI, "_raw.csv", sep = "")
      rawFile <- paste(RESULTSDIR, rawFileName, sep = "")
      dat.EI.raw <- read.csv(rawFile, header = T, stringsAsFactors = FALSE)
      dat.EI <- dat.EI.raw


      cols <- colnames(dat.EI)

      EI.col.name <- curr.EI

      if (any("stratum" %in% cols)) {
        has.stratum <- TRUE
      } else {
        has.stratum <- FALSE
        dat.EI$stratum <- "nostrata"
      }

      dat.EI <- dat.EI[, c(
        "landCover", "vegClass", "focalOrBenchmark", "plotCode", "stratum",
        "EI.tr"
      )]
      colnames(dat.EI) <- c(
        "landCover", "vegClass", "focalOrBenchmark", "plotCode",
        "stratum", "EI"
      )

      dat.EI <- dat.EI[with(dat.EI, order(dat.EI[, 1], dat.EI[, 2])), ]

      cols <- colnames(dat.EI)


      EI.b.all <- dat.EI[dat.EI$focalOrBenchmark == "b", ]
      EI.b.all <- EI.b.all[!is.na(EI.b.all$EI), ] # Remove NAs

      EI.b <- with(EI.b.all, aggregate(EI, list(vegClass = vegClass, stratum = stratum),
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
      EI.b <- cbind(EI.b[-ncol(EI.b)], EI.b[[ncol(EI.b)]])
      colnames(EI.b) <- c("vegClass", "stratum", "b.n", "b.mean", "b.sd", "b.med", "b.max", "b.min")

      EI.b.plots <- unique(EI.b.all[, c("plotCode", "vegClass", "stratum")])
      EI.b.n <- aggregate(
        plotCode ~ vegClass + stratum,
        data = EI.b.plots,
        FUN = length
      )
      colnames(EI.b.n) <- c("vegClass", "stratum", "b.n.new")

      EI.b.n$vegClass.stratum <- paste0(EI.b.n$vegClass, "|", EI.b.n$stratum)
      EI.b.n <- EI.b.n[, c("vegClass.stratum", "b.n.new")]
      EI.b$vegClass.stratum <- paste0(EI.b$vegClass, "|", EI.b$stratum)

      EI.b <- merge(EI.b, EI.b.n, by = "vegClass.stratum")
      names(EI.b)[names(
        EI.b
      ) == "b.n"] <- "b.n.bad" # Save temporarily in case compare before delete
      names(EI.b)[names(EI.b) == "b.n.new"] <- "b.n"
      EI.b <- EI.b[, !names(EI.b) %in% c("vegClass.stratum", "b.n.bad")]
      EI.b <- df.reorder(EI.b, col.move = "b.n", col.before = "stratum")


      EI.f.all <- dat.EI[dat.EI$focalOrBenchmark == "f", ]
      EI.f.all <- EI.f.all[!is.na(EI.f.all$EI), ] # Remove NAs

      EI.f <- with(EI.f.all, aggregate(EI, list(
        landCover = landCover, vegClass = vegClass,
        stratum = stratum
      ),
      FUN = function(x) {
        c(
          n = length(x), # This may be incorrect for indicators with multiple strata
          mean = mean(x),
          sd = sd(x),
          med = median(x),
          max = max(x),
          min = min(x)
        )
      }
      ))
      EI.f <- cbind(EI.f[-ncol(EI.f)], EI.f[[ncol(EI.f)]])
      colnames(EI.f) <- c(
        "landCover", "vegClass", "stratum", "f.n", "f.mean", "f.sd", "f.med",
        "f.max", "f.min"
      )

      EI.f.plots <- unique(EI.f.all[, c("plotCode", "landCover", "stratum")])
      EI.f.n <- aggregate(
        plotCode ~ landCover + stratum,
        data = EI.f.plots,
        FUN = length
      )
      colnames(EI.f.n) <- c("landCover", "stratum", "f.n.new")

      EI.f.n$landCover.stratum <- paste0(EI.f.n$landCover, "|", EI.f.n$stratum)
      EI.f.n <- EI.f.n[, c("landCover.stratum", "f.n.new")]
      EI.f$landCover.stratum <- paste0(EI.f$landCover, "|", EI.f$stratum)

      EI.f <- merge(EI.f, EI.f.n, by = "landCover.stratum")
      names(EI.f)[names(
        EI.f
      ) == "f.n"] <- "f.n.bad" # Save temporarily in case compare before delete
      names(EI.f)[names(EI.f) == "f.n.new"] <- "f.n"
      EI.f <- EI.f[, !names(EI.f) %in% c("landCover.stratum", "f.n.bad")]
      EI.f <- df.reorder(EI.f, col.move = "f.n", col.before = "stratum")

      EI.f$EI <- curr.EI

      EI.f$test.tail <- test.tail # Tail to be compared to benchmark; same for entire EI
      EI.f$dist <- distn # Code for distribution
      EI.f$q.method <- q.method # Benchmark quality scoring method:
      EI.f$bm.val <- bm.val # Benchmark fixed value. NA if q.method="empirical"

      EI.f <- EI.f[, c(
        "EI", "landCover", "vegClass", "stratum", "dist", "test.tail",
        "q.method", "bm.val", "f.n", "f.mean", "f.sd", "f.med", "f.max", "f.min"
      )]


      EI.f.temp <- EI.f
      EI.f <- merge(EI.f.temp, EI.b, by.x = c("vegClass", "stratum"), by.y = c(
        "vegClass",
        "stratum"
      ), all.x = TRUE)

      col_idx <- grep("landCover", names(EI.f))
      EI.f <- EI.f[, c(col_idx, (1:ncol(EI.f))[-col_idx])]
      col_idx <- grep("EI", names(EI.f))
      EI.f <- EI.f[, c(col_idx, (1:ncol(EI.f))[-col_idx])]

      if (curr.EI == "asc") {
        stratum.ord <- unique(dat.EI.raw[, c("stratum", "ord")])
        EI.f <- merge(EI.f, stratum.ord, by = "stratum", all.x = TRUE)
        EI.f <- df.reorder(df = EI.f, col.move = "stratum", col.before = "vegClass")
        EI.f <- df.reorder(df = EI.f, col.move = "ord", col.before = "stratum")
        EI.f <- EI.f[order(EI.f$landCover, EI.f$vegClass, EI.f$ord, EI.f$stratum), ]
      }


      EI.f$diff <- EI.f$f.mean - EI.f$b.mean # Difference, f.mean - b.mean
      EI.f$p.diff <- NA # P-value of difference, from permutation test
      EI.f$q.diff <- NA # Distance-based quality
      EI.f$ol <- NA # Overlap of fitted f and b distributions
      EI.f$ol.lcl <- NA # Lower 95% CL of overlap
      EI.f$ol.ucl <- NA # Upper 95% CL of overlap
      EI.f$ol.w <- NA # Weight of ol relative to diff in calculating overall quality
      EI.f$q <- NA # Overall quality (based on combination of d and o)
      EI.f$q.lcl <- NA # Lower 95% CL of quality
      EI.f$q.ucl <- NA # Upper 95% CL of quality

      f.results.file <- paste(RESULTSDIR, resultsFocal, sep = "")
      write.csv(EI.f, file = f.results.file, row.names = FALSE)

      cat("done\n")
    },
    envir = state
  )
  invisible(state)
}

calculate_indicator_quality <- function(state) {
  evalq(
    {
      cat("**************************\n")
      cat("Calculating quality\n")
      cat("**************************\n")

      if (exists("qual")) rm(qual)


      has.data <- TRUE

      boot.q.filename <- paste0(curr.EI, "_boot.q", ".csv") # quality bootstrap runs
      pdf.f.filename <- paste0(curr.EI, "_pdf.f", ".csv") # focal pdfs
      pdf.b.filename <- paste0(curr.EI, "_pdf.b", ".csv") # bm pdfs
      pdf.xs.filename <- paste0(curr.EI, "_pdf.xs", ".csv") # x values for both pdfs

      file.ei.summary <- paste0(RESULTSDIR, FILENAME.EI.SUMMARY)


      graphics.off()


      rawFileName <- paste(curr.EI, "_raw.csv", sep = "")
      rawFile <- paste(RESULTSDIR, rawFileName, sep = "")
      dat.EI <- read.csv(rawFile, header = T, stringsAsFactors = FALSE)
      dat.EI <- with(dat.EI, dat.EI[order(
        dat.EI$landCover, dat.EI$vegClass,
        dat.EI$focalOrBenchmark, dat.EI$plotCode, dat.EI$stratum
      ), ])

      f.results.file <- paste(RESULTSDIR, resultsFocal, sep = "")
      f.results.raw <- read.csv(f.results.file, header = T, stringsAsFactors = FALSE)
      f.results <- f.results.raw


      max.ei <- max(dat.EI$EI)

      if (distn == "Bet") {
        xs <- seq(0, 1, length.out = 100)
        xs[1] <- 0.0001
        xs[100] <- 0.9999
      } else if (distn == "NBin") {
        xs <- seq(0, max.ei + 10)
      } else if (distn == "gamma") {
        xs <- seq(0, max.ei, length.out = 1000)
      }


      boot.q.df <- data.frame(
        landCover = character(),
        stratum = character(),
        boot.reps = integer(),
        q.obs = double()
      )
      boot.cols <- data.frame(matrix(NA, nrow = 0, ncol = boot.reps))
      boot.cols <- convert.magic(boot.cols, "numeric")
      boot.q.df <- cbind(boot.q.df, boot.cols)

      pdf.cols <- data.frame(matrix(NA, nrow = 0, ncol = length(xs)))
      pdf.cols <- convert.magic(pdf.cols, "numeric")

      pdf.f.df <- data.frame(
        landCover = character(),
        stratum = character(),
        f.mean = double()
      )
      pdf.f.df <- cbind(pdf.f.df, pdf.cols)

      pdf.b.df <- data.frame(
        landCover = character(),
        stratum = character(),
        b.mean = double()
      )
      pdf.b.df <- cbind(pdf.b.df, pdf.cols)

      pdf.xs.df <- data.frame(pdf.cols)
      pdf.xs.df[1, ] <- xs


      all.lc <- unique(dat.EI$landCover[dat.EI$focalOrBenchmark == "f"])
      n.lc <- length(all.lc)

      for (j in 1:n.lc) { # START landCover loop
        curr.lc <- all.lc[j]

        cat("\n")
        cat(paste0("Current land cover: ", curr.lc, "\n"))

        EI.lc <- subset(dat.EI, dat.EI$focalOrBenchmark == "f" & dat.EI$landCover == curr.lc)

        curr.veg <- unique(EI.lc$vegClass)
        EI.lc.b <- subset(dat.EI, dat.EI$focalOrBenchmark == "b" & dat.EI$vegClass == curr.veg)

        strata <- unique(EI.lc$stratum)
        n.strata <- length(strata)

        for (k in 1:n.strata) { # START strata loop

          curr.stratum <- strata[k]
          EI.strat.f <- subset(EI.lc, EI.lc$stratum == curr.stratum)
          EI.strat.b <- subset(EI.lc.b, EI.lc.b$stratum == curr.stratum)
          cat("------------------------------------------------------------\n")
          if (!curr.stratum == "nostrata") cat(paste0("    Stratum: ", curr.stratum, "\n"))
          cat("    Indicator: ", curr.EI, "\n", sep = "")

          f <- as.vector(EI.strat.f$EI[EI.strat.f$focalOrBenchmark == "f"])
          b <- as.vector(EI.strat.b$EI[EI.strat.b$focalOrBenchmark == "b"])

          fb.pooled <- c(f, b)

          f.hat <- mean(f, na.rm = T)
          b.hat <- mean(b, na.rm = T)
          fb.diff <- f.hat - b.hat

          has.data <- T
          if (length(f) == 0 || length(b) == 0) {
            has.data <- F

            if (length(f) == 0 && length(b) == 0) {
              fb.missing <- "focal & benchmark data missing"
            } else if (length(f) == 0) {
              fb.missing <- "focal data missing"
            } else if (length(b) == 0) {
              fb.missing <- "benchmark data missing"
            }

            msg.stratum <- ""
            if (!curr.stratum == "nostrata") msg.stratum <- paste0("; ", curr.stratum)
            err.msg <- paste0("ERROR: ", fb.missing, " (", curr.lc, msg.stratum, ")")

            stop(err.msg)
          }


          if (distn == "Bet") {
            lowlim <- 0.0000001
            shape1 <- 0.1
            shape2 <- 0.1
          } else if (distn == "NBin") {} else if (distn == "gamma") {
            lowlim <- 0.0000001
            uplim <- max(fb.pooled) + (0.1 * max(fb.pooled))
            shape <- 1
            rate <- 1
          }


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

          if (distn == "Bet") { # START distn=="Bet"
            if (q.method == "fixed") {
              b.hat <- bm.val
              fb.diff <- f.hat - bm.val

              qual <- boot.qual.beta.fixed(
                f = f, b.fixed = bm.val, boot.reps = boot.reps,
                perm.reps = perm.reps, use.q.diff.raw = use.q.diff.raw,
                discount.method = discount.method, test.diff.boot = test.diff.boot,
                q.tr.logit.inverse = q.tr.logit.inverse, logit.inverse.beta = logit.inverse.beta
              )

              q.diff.raw <- qual$q.diff.raw # raw means-based quality
              q.diff <- qual$q # p-adjusted means-based quality
              p.diff <- qual$p # means test p-value
              ol <- NA
              ol.lcl <- NA
              ol.ucl <- NA
              q <- qual$q # quality same as diff-based quality
              q.raw <- qual$q.raw
              q.lcl <- qual$q.lcl
              q.ucl <- qual$q.ucl
              ol.w <- 0 # No overlap component
              boot.q.curr <- qual$boot.q
              pdf.f.curr <- NA # Not calculated for fixed bm
              pdf.b.curr <- NA # Not applicable to fixed bm
            } else if (beta.algorithm == "mixed") {
              qual <- boot.qual.beta(
                f = f, b = b, xs = xs,
                boot.reps = boot.reps, seed = seed, set.seed = set.seed,
                shape1 = shape1, shape2 = shape2, lowlim = lowlim,
                use.q.diff.raw = use.q.diff.raw, discount.method = discount.method,
                wt.method = wt.method, buffer = buffer,
                test.diff.boot = test.diff.boot,
                q.tr.logit.inverse = q.tr.logit.inverse, logit.inverse.beta = logit.inverse.beta
              )

              q.diff.raw <- qual$q.diff.raw # raw means-based quality
              q.diff <- qual$q.diff # adjusted means-based quality
              q.diff.method <- discount.method # Means-based quality discount method
              q.wt.method <- wt.method # Means vs overlap weighting method
              p.diff <- qual$p.diff # means test p-value
              ol <- qual$q.ol
              ol.lcl <- NA
              ol.ucl <- NA
              q <- qual$q
              q.raw <- qual$q.raw
              q.lcl <- qual$q.lcl
              q.ucl <- qual$q.ucl
              ol.w <- qual$ol.w # Relative weighting factor, overlap vs. distance quality
              boot.q.curr <- qual$boot.q
              pdf.f.curr <- qual$pdf.f
              pdf.b.curr <- qual$pdf.b
            } else if (beta.algorithm == "overlap") {
              qual <- boot.overlap.beta(
                f = f, b = b, xs = xs,
                boot.reps = boot.reps, seed = seed, set.seed = set.seed,
                shape1 = shape1, shape2 = shape2, lowlim = lowlim,
                discount.method = discount.method, test.diff.boot = test.diff.boot,
                q.tr.logit.inverse = q.tr.logit.inverse, logit.inverse.beta = logit.inverse.beta
              )

              q.diff.raw <- NA # means-based value, not applicable
              q.diff <- NA # means-based value, not applicable
              q.diff.method <- NA # means-based parameters not applicable
              p.diff <- NA # means-based value, not applicable
              ol <- qual$q.ol
              ol.lcl <- qual$q.lcl
              ol.ucl <- qual$q.ucl
              q <- ol
              q.raw <- qual$q.raw
              q.lcl <- ol.lcl
              q.ucl <- ol.ucl
              ol.w <- NA # means-based value, not applicable
              boot.q.curr <- qual$boot.q
              pdf.f.curr <- qual$pdf.f
              pdf.b.curr <- qual$pdf.b
              f.tr <- qual$f.tr
              b.tr <- qual$b.tr
            } else {
              stop("ERROR: parameter 'beta.algorithm' not valid! Check global params file")
            } # END distn=="Bet"
          } else if (distn == "NBin" || distn == "gamma") {
            p.diff <- means.test.perm(f, b, iterations = perm.reps, seed = seed)

            if (all(f == 0) && all(b == 0)) {
              q.diff <- NA # diff-based quality not used for neg. binom. distn
              q.diff.raw <- NA # diff-based quality not used for neg. binom. distn
              ol <- 1
              q <- 1 # overlap used directly as quality
              q.raw <- 1
              ol.lcl <- 1
              ol.ucl <- 1
              q.lcl <- 1
              q.ucl <- 1
              ol.w <- 1 # Relative weighting factor, all overlap
              boot.q.curr <- 1
              pdf.f.curr <- NA # Not calculated for all-zero vectors
              pdf.b.curr <- NA # Not calculated for all-zero vectors
            } else {
              qual <- boot.overlap.diff(distn,
                test.tail = test.tail, f = f, b = b, xs = xs,
                boot.reps = boot.reps, seed = seed, set.seed = set.seed,
                q.tr.logit.inverse = q.tr.logit.inverse, logit.inverse.beta = logit.inverse.beta
              )

              q.diff <- NA # diff-based quality not used for neg. binom. distn
              q.diff.raw <- NA # diff-based quality not used for neg. binom. distn
              ol <- qual$ol
              q <- qual$q # overlap used directly as quality
              q.raw <- qual$q.raw
              ol.lcl <- qual$q.lcl
              ol.ucl <- qual$q.ucl
              q.lcl <- qual$q.lcl
              q.ucl <- qual$q.ucl
              ol.w <- 1 # Relative weighting factor, 1=all overlap
              boot.q.curr <- qual$boot.q # Again, overlap used directly
              pdf.f.curr <- qual$pdf.f
              pdf.b.curr <- qual$pdf.b
            }
          } else {
            stop("ERROR: unknown distribution: '", distn, "'")
          }

          if (exists("qual")) {
            if (qual[[1]] == "FAIL") {
              err.msg <- paste0(
                "ERROR: Quality calculation failed! EI=", curr.EI,
                ", curr.lc=", curr.lc, ", curr.stratum=", curr.stratum, "\n"
              )
              stop(err.msg)
            }
          }


          if (!(length(boot.q.curr) == 1 && is.na(boot.q.curr[1]))) {
            if (length(boot.q.curr) == 1) {
              if (boot.q.curr == 1) {
                cols
                curr.boot.q <- data.frame(curr.lc, curr.stratum, boot.reps, q, t(rep(1, boot.reps)))
              }
            } else {
              curr.boot.q <- data.frame(curr.lc, curr.stratum, boot.reps, q, t(boot.q.curr))
            }

            names(curr.boot.q)[names(curr.boot.q) == "curr.lc"] <- "landCover"
            names(curr.boot.q)[names(curr.boot.q) == "curr.stratum"] <- "stratum"
            boot.q.df <- rbind(boot.q.df, curr.boot.q)
          }

          if (!(length(pdf.f.curr) == 1 && is.na(pdf.f.curr[1]))) {
            curr.pdf.f <- data.frame(curr.lc, curr.stratum, f.hat, t(pdf.f.curr))
            pdf.f.df <- rbind(pdf.f.df, curr.pdf.f)
          }

          if (!(length(pdf.b.curr) == 1 && is.na(pdf.b.curr[1]))) {
            curr.pdf.b <- data.frame(curr.lc, curr.stratum, b.hat, t(pdf.b.curr))
            pdf.b.df <- rbind(pdf.b.df, curr.pdf.b)
          }


          f.hat.txt <- specify_decimal(f.hat, 3)
          b.hat.txt <- specify_decimal(b.hat, 3)
          fb.diff.txt <- specify_decimal(fb.diff, 4)
          p.diff.txt <- specify_decimal(p.diff, 3)
          q.diff.txt <- specify_decimal(q.diff, 3)
          q.diff.raw.txt <- specify_decimal(q.diff.raw, 3)
          ol.txt <- specify_decimal(ol, 3)
          ol.lcl.txt <- specify_decimal(ol.lcl, 3)
          ol.ucl.txt <- specify_decimal(ol.ucl, 3)
          q.raw.txt <- specify_decimal(q.raw, 3)
          q.txt <- specify_decimal(q, 3)
          q.lcl.txt <- specify_decimal(q.lcl, 3)
          q.ucl.txt <- specify_decimal(q.ucl, 3)
          if (is.na(ol.w)) {
            ol.w.txt <- ""
          } else {
            ol.w.txt <- specify_decimal(ol.w, 2)
            ol.w.txt <- paste0(", ol.w=", ol.w.txt)
          }

          cat(paste0(
            "    ", "f.mean=", f.hat.txt, ", b.mean=", b.hat.txt, ", f-b=", fb.diff.txt,
            ", p=", p.diff.txt, "\n"
          ))
          cat(paste0(
            "    ",
            "q.diff.raw=", q.diff.raw.txt,
            ", q.diff=", q.diff.txt,
            ", q.diff.method='", q.diff.method, "'",
            ", q.wt.method='", q.wt.method, "'",
            "\n"
          ))
          cat(paste0("    q.ol=", ol.txt, ol.w.txt, "\n"))
          cat(paste0("    q.raw=", q.raw.txt, "\n"))
          cat(paste0("    q=", q.txt, ", q.lcl=", q.lcl.txt, ", q.ucl=", q.ucl.txt, "\n"))


          f.results$p.diff[f.results$landCover == curr.lc &
            f.results$stratum == curr.stratum] <- p.diff
          f.results$q.diff[f.results$landCover == curr.lc &
            f.results$stratum == curr.stratum] <- q.diff
          f.results$q.diff.raw[f.results$landCover == curr.lc &
            f.results$stratum == curr.stratum] <- q.diff.raw
          f.results$q.diff.method[f.results$landCover == curr.lc &
            f.results$stratum == curr.stratum] <- q.diff.method
          f.results$q.wt.method[f.results$landCover == curr.lc &
            f.results$stratum == curr.stratum] <- q.wt.method
          f.results$ol[f.results$landCover == curr.lc & f.results$stratum == curr.stratum] <- ol
          f.results$ol.lcl[f.results$landCover == curr.lc &
            f.results$stratum == curr.stratum] <- ol.lcl
          f.results$ol.ucl[f.results$landCover == curr.lc &
            f.results$stratum == curr.stratum] <- ol.ucl
          f.results$ol.w[f.results$landCover == curr.lc & f.results$stratum == curr.stratum] <- ol.w
          f.results$q.raw[f.results$landCover == curr.lc &
            f.results$stratum == curr.stratum] <- q.raw
          f.results$q[f.results$landCover == curr.lc & f.results$stratum == curr.stratum] <- q
          f.results$q.lcl[f.results$landCover == curr.lc &
            f.results$stratum == curr.stratum] <- q.lcl
          f.results$q.ucl[f.results$landCover == curr.lc &
            f.results$stratum == curr.stratum] <- q.ucl
        } # END strata loop

        cat("------------------------------------------------------------\n")
      } # END landCover loop


      cat("\nSaving results to files:\n")

      cat("  Indicator parameters...")

      if (file.exists(file.ei.summary) == FALSE) {
        df.ei.summary <- data.frame(
          ei = character(),
          ei.name = character(),
          distn = character(),
          q.method = character(),
          test.tail = character(),
          bm.val = numeric()
        )
        write.csv(df.ei.summary, file = file.ei.summary, row.names = FALSE)
      }

      df.ei.summary <- read.csv(file.ei.summary, header = T, stringsAsFactors = FALSE)
      df.ei.summary$bm.val <- as.numeric(df.ei.summary$bm.val)
      curr.ei.summary <- c(
        curr.EI,
        EI.name,
        distn,
        q.method,
        test.tail,
        bm.val
      )

      if (curr.EI %in% df.ei.summary$ei) {
        df.ei.summary[df.ei.summary$ei == curr.EI, ] <- curr.ei.summary
      } else {
        df.ei.summary <- rbind(df.ei.summary, curr.ei.summary)
      }

      colnames(df.ei.summary) <- c("ei", "ei.name", "distn", "q.method", "test.tail", "bm.val")

      write.csv(df.ei.summary, file = file.ei.summary, row.names = FALSE)

      cat("done\r\n")


      cat("  Quality calculations...")

      f.results <- df.reorder(df = f.results, col.move = "q.diff.raw", col.before = "diff")
      f.results <- df.reorder(df = f.results, col.move = "q.raw", col.before = "ol.w")

      names(f.results)[names(f.results) == "vegClass"] <- "bm.vegetation"
      names(f.results)[names(f.results) == "targetVegClass"] <- "bm.vegetation"
      names(f.results)[names(f.results) == "landCoverClass"] <- "landcover"
      names(f.results)[names(f.results) == "landCover"] <- "landcover"
      f.results <- df.reorder(df = f.results, col.move = "bm.vegetation", col.before = "EI")

      if (curr.EI == "asc") {
        f.results <- f.results[order(
          f.results$bm.vegetation, f.results$landcover,
          f.results$ord, f.results$stratum
        ), ]
      }

      write.csv(f.results, file = f.results.file, row.names = FALSE)

      names(boot.q.df)[names(boot.q.df) == "curr.lc"] <- "landcover"
      names(boot.q.df)[names(boot.q.df) == "curr.stratum"] <- "stratum"
      boot.q.file <- paste(RESULTSDIR, boot.q.filename, sep = "")
      write.csv(boot.q.df, file = boot.q.file, row.names = FALSE)

      names(pdf.f.df)[names(pdf.f.df) == "curr.lc"] <- "landcover"
      names(pdf.f.df)[names(pdf.f.df) == "curr.stratum"] <- "stratum"
      pdf.f.file <- paste(RESULTSDIR, pdf.f.filename, sep = "")
      write.csv(pdf.f.df, file = pdf.f.file, row.names = FALSE)

      names(pdf.b.df)[names(pdf.b.df) == "curr.lc"] <- "landcover"
      names(pdf.b.df)[names(pdf.b.df) == "curr.stratum"] <- "stratum"
      pdf.b.file <- paste(RESULTSDIR, pdf.b.filename, sep = "")
      write.csv(pdf.b.df, file = pdf.b.file, row.names = FALSE)

      pdf.xs.file <- paste(RESULTSDIR, pdf.xs.filename, sep = "")
      write.csv(pdf.xs.df, file = pdf.xs.file, row.names = FALSE)

      cat("done\r\n\n")
    },
    envir = state
  )
  invisible(state)
}
