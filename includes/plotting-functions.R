qc.calc.simple.demo <- function(x.min, x.max, k) {
  ###################################################
  # Useful for visualizing the effect of different ranges
  # of x and the steepness function k
  ###################################################

  n.reps <- 1000
  x.vals <- seq(x.min, x.max, length.out = n.reps)
  y.vals <- rep(NA, n.reps)

  for (i in 1:length(x.vals)) {
    y.vals[i] <- qc.calc.simple(x.vals[i], x.min = 0, x.max = 1, k = k)
  }
  main.txt <- paste("Logistic transformation, x range = ",
    x.min, " to ", x.max, ", k=", k,
    sep = ""
  )

  plot(y.vals ~ x.vals,
    type = "l",
    xlab = "Original value (x)", ylab = "Transformed value (Quality)",
    main = main.txt
  )
}

boot.ol.plot <- function(
  ol.list, title.text = "", group.text = "", f.text, bm.text,
  subsample = FALSE, plot.boot = TRUE
) {
  ###################################################
  # Plots pair of fitted distributions plus 95% bootstrap CLs, optionally
  # adding bootstrap fits, and shaded observed overlap. Overlap plus
  # confidence limits are displayed in legend
  #
  # Parameters:
  # 		1. ol.list	 (list object, results from bootstrap overlap function)
  # 			(a) ol.obs				Observed overlap
  # 			(b) ol.cls				Overlap conidence limits
  # 			(c) xs						Vector of X values used for pdfs
  # 			(d) pdf1					Fitted distribution 1
  # 			(e) pdf2					Fitted distribution 2
  # 			(c) boot.pdf1		Boostrapped pdf1
  # 			(d) boot.pdf2		Boostrapped pdf2
  # 			(e) ol.boot			Bootstrapped overlap of the two distributions
  # 		2. title.text				Title above graph
  # 		3. group.text			Text identifying color of each group [default assumes
  # 											benchmark is first group: c("Benchmark","Focal")
  # 		4. subsample			Subsample bootstrap pdfs for clarity? Ignored if
  # 											plot.boot=FALSE
  # 		5. plot.boot				Plot bootstrapped pdfs (default=TRUE)
  ###################################################

  # Set transparency for overlaps
  # Must be from 0 to 1
  alpha.val <- 0.25

  # Group colors
  v1.col <- rgb(0, 0, 1, alpha.val)
  v2.col <- rgb(1, 0, 0, alpha.val)

  # Set defaults
  n.boot <- length(ol.list$boot.ol)
  if (subsample && n.boot > 100) {
    cols <- 100
  } else {
    cols <- n.boot
  }

  # Extract elements
  ol.obs <- ol.list$ol.obs
  ol.lcl <- ol.list$ol.cls[[1]]
  ol.ucl <- ol.list$ol.cls[[2]]
  xs <- ol.list$xs
  pdf1 <- ol.list$pdf1
  pdf2 <- ol.list$pdf2
  boot.pdf1 <- ol.list$boot.pdf1
  boot.pdf2 <- ol.list$boot.pdf2
  ol.boot <- ol.list$ol.boot

  # Set Inf=NA
  pdf1[pdf1 == Inf] <- NA
  pdf2[pdf2 == Inf] <- NA
  boot.pdf1[boot.pdf1 == Inf] <- NA
  boot.pdf2[boot.pdf2 == Inf] <- NA

  # Get limits
  x.max <- max(max(xs))
  y.max <- max(max(boot.pdf1, na.rm = T), max(boot.pdf2, na.rm = T))

  # pdf1
  par(bg = "white", las = 1, cex = 1.2)

  # bootstrap pdfs
  plot(xs, boot.pdf1[, 1],
    type = "l",
    col = rgb(.6, .6, .6, .1),
    ylim = c(0, y.max),
    xlab = "x", ylab = "Probability density"
  )
  if (plot.boot == TRUE) {
    for (i in 2:cols) lines(xs, boot.pdf1[, i], col = rgb(.6, .6, .6, .1))
  }

  # Highlight confidence bands
  if (plot.boot == TRUE) {
    quants <- apply(boot.pdf1, 1, quantile, c(0.025, 0.5, 0.975), na.rm = TRUE)
    lines(xs, quants[1, ], col = rgb(0, 0, 1, alpha.val * 2), lwd = 1.5, lty = 2)
    lines(xs, quants[3, ], col = rgb(0, 0, 1, alpha.val * 2), lwd = 1.5, lty = 2)
    # lines(xs, quants[2, ], col=rgb(0,0,1, alpha.val*2), lwd=1.5, lty=2)
  }

  # add the observed distribution
  lines(xs, pdf1, col = rgb(0, 0, 1, alpha.val * 2), lwd = 2.5)

  # pdf2

  # bootstrap pdfs
  if (plot.boot == TRUE) {
    for (i in 2:cols) lines(xs, boot.pdf2[, i], col = rgb(.6, .6, .6, .1))
  }

  # Highlight confidence bands
  if (plot.boot == TRUE) {
    quants <- apply(boot.pdf2, 1, quantile, c(0.025, 0.5, 0.975), na.rm = TRUE)
    lines(xs, quants[1, ], col = rgb(1, 0, 0, alpha.val * 2), lwd = 1.5, lty = 2)
    lines(xs, quants[3, ], col = rgb(1, 0, 0, alpha.val * 2), lwd = 1.5, lty = 2)
    # lines(xs, quants[2, ], col=rgb(1,0,0, alpha.val*2), lwd=1.5, lty=2)
  }

  # add the observed distribution
  lines(xs, pdf2, col = rgb(1, 0, 0, alpha.val * 2), lwd = 2.5)

  # Shade in the mean overlap
  # polygon( x=c(xs,rev(xs)), y=c(rep(0,length(xs)),
  # rev( pmin( pdf1, pdf2 ) ) ),
  # col=rgb(1, 1, 0,0.2)
  # )
  polygon(
    x = c(xs, rev(xs)), y = c(
      rep(0, length(xs)),
      rev(pmin(pdf1, pdf2))
    ),
    col = rgb(1, 1, 0, 0.5)
  )

  # Add overlap legend
  ol.txt <- specify_decimal(ol * 100, 1)
  ol.ucl.txt <- specify_decimal(ol.ucl * 100, 1)
  ol.lcl.txt <- specify_decimal(ol.lcl * 100, 1)
  legendText <- paste("Overlap: ", ol.txt, "% (", ol.lcl.txt, "-", ol.ucl.txt, ")", sep = "")
  x.pos <- x.max - 0.55 * x.max
  y.pos <- y.max + 0.09 * y.max
  legend(
    x = x.pos, y = y.pos, # position
    legend = legendText,
    title = "", # keep this to maintain position
    cex = 1.0,
    bty = "n" # border
  )

  # Add focal/benchmark color legend below overlap
  # inset.x <- 0.24*x.max
  inset.x <- 0.04 * x.max
  inset.y <- 0.13 * y.max
  if (group.text == "") group.text <- c(bm.text, f.text)
  legend(
    x = x.pos + inset.x, y = y.pos - inset.y,
    legend = group.text,
    fill = c(rgb(0, 0, 1, alpha.val), rgb(1, 0, 0, alpha.val)),
    box.lty = 0
  )

  # Add title above graph
  if (!(title.text == "")) {
    title(main = title.text)
  }
}

plot.ol.gamma <- function(x.vals, s1, r1, s2, r2, title.main = "",
                          title.x = "", trt1.txt = "", trt2.txt = "") {
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
  set_decimal <- function(x, k) format(round(x, k), nsmall = k)

  # Assign parameters to vectors or length 2
  shps <- c(s1, s2)
  rates <- c(r1, r2)

  # Generate the distributions to get axis limits
  y.max <- 0 # Starting values: highest peak value of y
  y.max.min <- 2 # Starting values: lowest peak value of y
  p.thresh <- 0.01 # Lowest probability for plotting right tail threshold
  x.max <- 0 # Starting values: highest value of x at threshold

  # Run models without plotting to get limits
  for (i in 1:2) {
    # The distribution
    hx <- dgamma(x.vals, shape = shps[i], rate = rates[i])
    # Value of x where right tail p drops below right-tail probability threshold
    x.thresh <- qgamma(p.thresh, shape = shps[i], rate = rates[i], lower.tail = F)

    max.hx <- max(hx[hx < Inf])
    if (max.hx > y.max) { # Get maximum y value of the distributions
      y.max <- max.hx
    }
    if (x.thresh > x.max) { # Save parameters of widest distribution
      x.max <- x.thresh
    }
  }

  # Set axis limits
  xlim.max <- x.max + (x.max * 0.1)
  ylim.max <- y.max + (y.max * 0.5)

  # Set title
  if (title.main == "") {
    main.txt <- "Overlap of gamma probability distributions"
  } else {
    main.txt <- title.main
  }

  # Set x axis label
  if (title.x == "") {
    x.txt <- "x values"
  } else {
    x.txt <- title.x
  }

  # Dummy probability density function for initializing graph
  initDist <- dgamma(x.vals, shape = 1.25, scale = 1 / 0.25)

  # Set up empty plot with correct dimensions and labels
  plot(x.vals, initDist,
    type = "l", yaxs = "i", xaxs = "i",
    ylim = c(0, ylim.max), xlim = c(0, xlim.max),
    xlab = x.txt, ylab = "Density",
    main = main.txt,
    lwd = 0
  )

  colors <- c("blue", "orange")

  s1.txt <- set_decimal(s1, 3)
  s2.txt <- set_decimal(s2, 3)
  r1.txt <- set_decimal(r1, 3)
  r2.txt <- set_decimal(r2, 3)

  if (trt1.txt == "" & trt2.txt == "") {
    labels <- c(
      paste("alpha=", s1.txt, ", beta=", r1.txt, sep = ""),
      paste("alpha=", s2.txt, ", beta=", r2.txt, sep = "")
    )
  } else {
    labels <- c(
      paste(trt1.txt, sep = ""),
      paste(trt2.txt, sep = "")
    )
  }

  # plot the curves
  for (i in 1:2) {
    hx <- dgamma(x.vals, shape = shps[i], rate = rates[i])
    lines(x.vals, hx, lwd = 1, col = colors[i])
  }
  legend("topright",
    inset = .05,
    labels, lwd = 1, col = colors
  )

  #############################
  # Plot overlap
  #############################

  y.vals <- ol.gamma(x.vals, s1 = s1, s2 = s2, r1 = r1, r2 = r2)
  x.vals <- c(x.vals, x.vals[1])
  y.vals <- c(y.vals, y.vals[1])
  polygon(x.vals, y.vals, col = "gray")

  # Calculate the area of overlap
  ol <- prop.ol.gamma(x.vals, s1 = s1, r1 = r1, s2 = s2, r2 = r2)
  ol.val <- ol[[1]] # overlap
  ol.err <- ol[[2]] # overlap error

  ol.val.txt <- as.character(set_decimal(ol.val * 100, 2))
  if (ol.val < 0.01) ol.val.txt <- "<0.01"
  ol.err.txt <- as.character(set_decimal(ol.err * 100, 3))
  if (ol.err < 0.001) ol.err.txt <- "<0.001"

  label.txt <- paste("Overlap: ", ol.val.txt, "% \nAbs. error ", ol.err.txt, "%", sep = "")

  # Add overlap text to graph
  label.text <-
    text(
      labels = label.txt,
      x = 0.05 * xlim.max,
      y = ylim.max - (0.1 * ylim.max),
      pos = 4
    )
}

plot.ol.beta <- function(
  x.vals, s1, r1, s2, r2, title.main = "", title.x = "", trt1.txt = "",
  trt2.txt = ""
) {
  #####################################
  # Plot proportional overlap between two beta
  # distributions & labels with percent overlap
  #
  # Requires:
  # 	1. Function ol.beta()
  # 2. Function p.ol.beta()
  #####################################

  # for formatting
  set_decimal <- function(x, k) format(round(x, k), nsmall = k)

  # Assign parameters to vectors or length 2
  shps <- c(s1, s2)
  shps2 <- c(r1, r2)

  # Generate the distributions to get axis limits
  y.max <- 0 # Starting values: highest peak value of y
  y.max.min <- 2 # Starting values: lowest peak value of y
  p.thresh <- 0.01 # Lowest probability for plotting right tail threshold
  x.max <- 0 # Starting values: highest value of x at threshold

  # Run models without plotting to get limits
  for (i in 1:2) {
    # The distribution
    hx <- dbeta(x.vals, shape1 = shps[i], shape2 = shps2[i])
    # Value of x where right tail p drops below right-tail probability threshold
    x.thresh <- qbeta(p.thresh, shape1 = shps[i], shape2 = shps2[i], lower.tail = F)

    max.hx <- max(hx[hx < Inf])
    if (max.hx > y.max) { # Get maximum y value of the distributions
      y.max <- max.hx
    }
    if (x.thresh > x.max) { # Save parameters of widest distribution
      x.max <- x.thresh
    }
  }

  # Set axis limits
  # xlim.max <- x.max + ( x.max * 0.1 )
  ylim.max <- y.max + (y.max * 0.5)
  xlim.max <- 1 # Best for beta
  # ylim.max <- 10 # Use this setting to magnify lower half of graph
  # useful for ZIBet

  # Set title
  if (title.main == "") {
    main.txt <- "Overlap of gamma probability distributions"
  } else {
    main.txt <- title.main
  }

  # Set x axis label
  if (title.x == "") {
    x.txt <- "x values"
  } else {
    x.txt <- title.x
  }

  # Dummy probability density function for initializing graph
  initDist <- dbeta(x.vals, shape1 = 1.25, shape2 = 1)

  # Set up empty plot with correct dimensions and labels
  plot(x.vals, initDist,
    type = "l", yaxs = "i", xaxs = "i",
    ylim = c(0, ylim.max), xlim = c(0, xlim.max),
    xlab = x.txt, ylab = "Density",
    main = main.txt,
    lwd = 0
  )

  colors <- c("blue", "orange")

  s1.txt <- set_decimal(s1, 3)
  s2.txt <- set_decimal(s2, 3)
  r1.txt <- set_decimal(r1, 3)
  r2.txt <- set_decimal(r2, 3)

  if (trt1.txt == "" & trt2.txt == "") {
    labels <- c(
      paste("alpha=", s1.txt, ", beta=", r1.txt, sep = ""),
      paste("alpha=", s2.txt, ", beta=", r2.txt, sep = "")
    )
  } else {
    labels <- c(
      paste(trt1.txt, sep = ""),
      paste(trt2.txt, sep = "")
    )
  }

  # plot the curves
  for (i in 1:2) {
    hx <- dbeta(x.vals, shape1 = shps[i], shape2 = shps2[i])
    lines(x.vals, hx, lwd = 1, col = colors[i])
  }
  legend("topright",
    inset = 0.05,
    labels, lwd = 1, col = colors
  )

  #############################
  # Plot overlap
  #############################

  y.vals <- ol.beta(x.vals, s1 = s1, r1 = r1, s2 = s2, r2 = r2)
  x.vals <- c(x.vals, 0, x.vals[1])
  y.vals <- c(y.vals, 0, y.vals[1])
  polygon(x.vals, y.vals, col = "gray")

  # Calculate the area of overlap
  ol <- prop.ol.beta(x.vals, s1 = s1, r1 = r1, s2 = s2, r2 = r2)
  ol.val <- ol[[1]] # overlap
  ol.err <- ol[[2]] # overlap error

  ol.val.txt <- as.character(set_decimal(ol.val * 100, 2))
  if (ol.val < 0.01) ol.val.txt <- "<0.01"
  ol.err.txt <- as.character(set_decimal(ol.err * 100, 3))
  if (ol.err < 0.001) ol.err.txt <- "<0.001"

  # label.txt <- paste('Overlap: ', ol.val.txt, '% \nAbs. error ', ol.err.txt, '%', sep='')
  label.txt <- paste("Overlap: ", ol.val.txt, "%", sep = "")

  # Add overlap text to graph
  label.text <-
    text(
      labels = label.txt,
      x = 0.05 * xlim.max,
      y = ylim.max - (0.1 * ylim.max),
      pos = 4
    )
}

plot.ol.nbin <- function(x.vals, s1, mu1, s2, mu2, title.main = "",
                         title.x = "", trt1.txt = "", trt2.txt = "") {
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
  set_decimal <- function(x, k) format(round(x, k), nsmall = k)

  # Assign parameters to vectors or length 2
  sizes <- c(s1, s1)
  mus <- c(mu1, mu2)

  # Generate the distributions to get axis limits
  y.max <- 0 # Starting values: highest peak value of y
  y.max.min <- 2 # Starting values: lowest peak value of y
  p.thresh <- 0.01 # Lowest probability for plotting right tail threshold
  x.max <- 0 # Starting values: highest value of x at threshold

  # Run models without plotting to get limits
  for (i in 1:2) {
    # The distribution
    hx <- dnbinom(x.vals, mu = mus[i], size = sizes[i])
    # Value of x where right tail p drops below right-tail probability threshold
    x.thresh <- qnbinom(p.thresh, mu = mus[i], size = sizes[i], lower.tail = F)

    max.hx <- max(hx[hx < Inf])
    if (max.hx > y.max) { # Get maximum y value of the distributions
      y.max <- max.hx
    }
    if (x.thresh > x.max) { # Save parameters of widest distribution
      x.max <- x.thresh
    }
  }

  # Set axis limits
  xlim.max <- x.max + (x.max * 0.1)
  ylim.max <- y.max + (y.max * 0.5)

  # Set title
  if (title.main == "") {
    main.txt <- "Overlap of negative binomial probability distributions"
  } else {
    main.txt <- title.main
  }

  # Set x axis label
  if (title.x == "") {
    x.txt <- "x values"
  } else {
    x.txt <- title.x
  }

  # Dummy probability density function for initializing graph
  initDist <- dnbinom(x.vals, mu = 4, size = 10)

  # Set up empty plot with correct dimensions and labels
  plot(x.vals, initDist,
    type = "l", yaxs = "i", xaxs = "i",
    ylim = c(0, ylim.max), xlim = c(0, xlim.max),
    xlab = x.txt, ylab = "Density",
    main = main.txt,
    lwd = 0
  )

  colors <- c("blue", "orange")

  s1.txt <- set_decimal(s1, 3)
  s2.txt <- set_decimal(s2, 3)
  mu1.txt <- set_decimal(mu1, 3)
  mu2.txt <- set_decimal(mu2, 3)

  if (trt1.txt == "" & trt2.txt == "") {
    labels <- c(
      paste("size =", s1.txt, ", mu =", mu1.txt, sep = ""),
      paste("size =", s2.txt, ", mu =", mu2.txt, sep = "")
    )
  } else {
    labels <- c(
      paste(trt1.txt, sep = ""),
      paste(trt2.txt, sep = "")
    )
  }

  # plot the curves
  for (i in 1:2) {
    hx <- dnbinom(x.vals, mu = mus[i], size = sizes[i])
    lines(x.vals, hx, lwd = 1, col = colors[i])
  }
  legend("topright",
    inset = .05,
    labels, lwd = 1, col = colors
  )

  #############################
  # Plot overlap
  #############################

  # Plot zone of overlap using actual neg binom dist
  y.vals <- ol.nbin(x.vals, mu1 = mu1, mu2 = mu2, s1 = s1, s2 = s2)
  x.vals2 <- c(x.vals, 0, x.vals[1])
  y.vals2 <- c(y.vals, 0, y.vals[1])
  polygon(x.vals2, y.vals2, col = "gray")

  # Calculate the area of overlap
  # Gamma is a good approximation
  # Note use of inverse of size parameters
  ol <- prop.ol.gamma(x.vals, s1 = s1, r1 = 1 / mu1, s2 = s2, r2 = 1 / mu2)
  ol.val <- ol[[1]] # overlap
  ol.err <- ol[[2]] # overlap error

  ol.val.txt <- as.character(set_decimal(ol.val * 100, 2))
  if (ol.val < 0.01) ol.val.txt <- "<0.01"
  ol.err.txt <- as.character(set_decimal(ol.err * 100, 3))
  if (ol.err < 0.001) ol.err.txt <- "<0.001"

  label.txt <- paste("Overlap: ", ol.val.txt, "% \nAbs. error ", ol.err.txt, "%", sep = "")

  # Add overlap text to graph
  label.text <-
    text(
      labels = label.txt,
      x = 0.05 * xlim.max,
      y = ylim.max - (0.1 * ylim.max),
      pos = 4
    )
}

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
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

  numPlots <- length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
      ncol = cols, nrow = ceiling(numPlots / cols)
    )
  }

  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(
        layout.pos.row = matchidx$row,
        layout.pos.col = matchidx$col
      ))
    }
  }
}

mm.power <- function(df.pwr, pwr.target = NULL) {
  ###################################################
  # Fits a Michaelis-Menten power curve to a data frame of point
  # power estimate (pwr) vs sample size (n). Returns the fitted power
  # curve, plus estimated minimum sample size (n.min) needed to
  # achieve target power. Assumes conventional power of 0.80 if
  # target power not supplied.
  #
  # Parameters:
  # 		df.pwr			Data frame with columns n (sample size) and
  # 							pwr (power)
  # 		pwr.target	The target power (default: 0.80)
  #
  # Returns:
  # 		df.p.pred		Data frame of predicted power curve over range of n
  # 		n.min			Interpolated n at power=pwr.target
  # Requires package
  ###################################################

  if (!require(drc)) stop("ERROR: package 'drc' not loaded")

  if (is.null(pwr.target)) pwr.target <- 0.80

  model.drm <- drm(pwr ~ n, data = df.pwr, fct = MM.2())
  mm <- data.frame(n = seq(0, max(df.pwr$n), length.out = 1000))
  mm$pwr.fit <- predict(model.drm, newdata = mm)

  # Get n for target power
  # pwr.fit.diff <- abs( pwr.target - mm$pwr.fit )
  # idx <- which.min(pwr.fit.diff)
  # n.min <- mm$n[idx]
  n.min <- x.pred(x = n, y = mm$pwr.fit, y.fixed = pwr.target)

  results <- list(mm, n.min)
  names(results) <- c("mm.df.pwr.fit", "n.min")
  return(results)
}

plot.mm.power <- function(
  df.pwr, pwr.target = NULL, title.main = NULL, x.title = NULL,
  y.title = NULL
) {
  ###################################################
  # Plot predicted (pwr) vs sample size (n), with power curve fit
  # using Michaelis-Menten equation. Add horizontal line for conventional
  # power (pwr.target) and minimum required sample size (n.min) at
  # y=pwr.target. Assumes conventional power of 0.80 if
  # target power not supplied.
  #
  # Parameters:
  # 		df.pwr			Data frame with columns n (sample size) and
  # 							pwr (power)
  # 		pwr.target	The target power (default: 0.80)
  # 		title.main 	Main title (default: none)
  # 		x.title				X axis title (default: "Sample size")
  # 		y.title				Y axis title (default: "Power")
  #
  # Returns: ggplot object, ready for printing and saving
  ###################################################

  if (!require(drc)) stop("ERROR: package 'drc' not loaded")
  if (!require(ggplot2)) stop("ERROR: package 'ggplot2' not loaded")

  if (is.null(pwr.target)) pwr.target <- 0.80
  if (is.null(title.main)) title.main <- ""
  if (is.null(x.title)) x.title <- "Sample size"
  if (is.null(y.title)) y.title <- "Power"

  # Fit the MM curve
  fit <- mm.power(df.pwr, pwr.target)
  df.pwr.fit <- fit$mm.df.pwr.fit # df of n, pwr and pwr.fit
  n.target <- fit$n.min # min n, @ target power

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
  p <- p + geom_hline(aes(yintercept = pwr.target), colour = "#BB0000", linetype = "dotted")

  n.axis.max <- max(df.pwr$n)
  if (n.target <= n.axis.max) {
    # Add vertical line for n @ target power or precision
    p <- p + geom_vline(aes(xintercept = n.target), colour = "#BB0000", linetype = "dotted")
  }

  # theme with white background
  p <- p + theme_bw()

  # eliminates background, gridlines, and chart border
  p <- p + theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

  # Set text sizes
  p <- p + theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )
  p <- p + theme(plot.title = element_text(size = 22))

  # return the graph
  return(p)
}

power.logistic <- function(df.pwr, pwr.target = NULL) {
  ###################################################
  # Fits a logistic curve to a data frame of point
  # power estimate (pwr) vs sample size (n). Returns the fitted power
  # curve, plus estimated minimum sample size (n.min) needed to
  # achieve target power. Assumes conventional power of 0.80 if
  # target power not supplied.
  #
  # Parameters:
  # 		df.pwr			Data frame with columns n (sample size) and
  # 							pwr (power)
  # 		pwr.target	The target power (default: 0.80)
  #
  # Returns list object:
  # 		n					Vector of sample sizes
  # 		pwr				Vector of point power predictions
  # 		pwr.fit			Vector of fitted logistic power values
  # 		pwr.target	Target power
  # 		n.min			Interpolated n at power=pwr.target
  ###################################################

  maxTries <- 100 # Max times to try fit before reporting error

  if (is.null(pwr.target)) pwr.target <- 0.80
  pwr <- df.pwr$pwr
  n <- df.pwr$n

  a.start <- 1 # The asymptote, 1 by definition for power
  b.start <- 0.5 # Starting value for growth rate

  # Starting value for inflection point
  # Try value of n near mean of pwr
  diffs <- abs(pwr - mean(pwr))
  min.diffs <- min(diffs)
  idx <- which(diffs == min.diffs)
  c.start <- n[idx][1]
  c.start.orig <- c.start

  # Fit the model
  fail <- F
  tries <- 0
  done <- F
  while (done == F) {
    tries <- tries + 1
    mod <- tryCatch(
      nls(pwr ~ a / (1 + exp(-b * (n - c))),
        start = list(a = a.start, b = b.start, c = c.start)
      ),
      error = function(cond) {
        return("FAIL")
      },
      warning = function(cond) {
        return("FAIL")
      }
    )
    if (mod[1] == "FAIL") {
      if (tries >= maxTries) {
        fail <- T
        break
      } else if (tries <= maxTries / 2) {
        # Try decreasing c.start
        c.start <- c.start - 0.2 * (c.start - min(n))
      } else {
        # Try increasing c.start
        c.start <- c.start + 0.2 * (max(n) - c.start)
      }
    } else {
      done <- T
    }
  }

  if (fail == T) {
    results <- "fail"
  } else {
    # Get the coefficients
    params <- as.list(coef(mod))

    # Get the predicted values
    pwr.fit <- params$a / (1 + exp(-params$b * (n - params$c)))

    # Get n for target power
    # pwr.pred.diff <- abs( pwr.target - pwr.fit )
    # idx <- which.min(pwr.pred.diff)
    # n.min <- n[idx]
    n.min <- x.pred(x = n, y = pwr.fit, y.fixed = pwr.target)

    # plot(pwr.fit~n,type="l")
    # points(pwr~n)
    # abline(h = pwr.target, col="red", lwd=1, lty=2)
    # abline(v = n.min, col="red", lwd=1, lty=2)

    results <- list(n, pwr, pwr.fit, pwr.target, n.min)
    names(results) <- c("n", "pwr", "pwr.fit", "pwr.target", "n.min")
  }

  return(results)
}

x.pred <- function(x, y, y.fixed) {
  ###################################################
  # For vectors of y fitted to x, returns predicted value of x at a fixed
  # value of y
  ###################################################

  y.diff <- abs(y.fixed - y)
  idx <- which.min(y.diff)
  x.pred <- x[idx]
  return(x.pred)
}

plot.power.logistic <- function(
  n, pwr, pwr.fit, e.size = NULL, pwr.target = NULL,
  n.target = NULL, title.main = NULL, x.title = NULL, y.title = NULL, p.text.pos = NULL,
  n.text.pos = NULL, e.size.text.pos.x = NULL, e.size.text.pos.y = NULL
) {
  ###################################################
  # Plots predicted (pwr) vs sample size (n) with fit curve &
  # adds lines for target power (pwr.target) and n @ target power (n.min).
  # Uses conventional power of 0.80 if target power not supplied.
  # Calculates n.min if not supplied.
  #
  # Parameters:
  # 		n						Vector of sample size values
  # 		pwr					Vector of predicted power values
  # 		pwr.fit				Vector of fitted power values (logistic fit)
  # 		e.size				Effect size (optional, for label)
  # 		pwr.target		Target power (default: 0.80)
  # 		n.target			Sample size at target power (calculated if missing)
  # 		title.main			Title for graph (default: none)
  # 		x.title				X axis title (default: "Sample size")
  # 		y.title				Y axis title (default: "Power")
  # 		p.text.pos		Optional specification of x coordinate of left edge of
  # 								label for target power line
  # 		n.text.pos		Optional specification of x coordinate of left edge of
  # 								label for n.target line
  # 		e.size.text.pos.x		Optional specification of x coordinate of left edge of
  # 								label for effect size
  # 		e.size.text.pos.y		Optional specification of y coordinate of label for
  # 								effect size
  #
  # Returns: ggplot object, ready for printing and saving
  ###################################################

  if (!require(ggplot2)) stop("ERROR: package 'ggplot2' not loaded")

  # Convert the vectors to a data frame
  df.pwr.fit <- as.data.frame(list(n, pwr, pwr.fit))
  names(df.pwr.fit) <- c("n", "pwr", "pwr.fit")
  n <- df.pwr.fit$n
  pwr <- df.pwr.fit$pwr

  # Set default/calculated values
  if (is.null(e.size)) e.size <- ""
  if (is.null(title.main)) title.main <- ""
  if (!(title.main == "")) {
    if (!e.size == "") title.main <- paste0(title.main, " (ES=", e.size, ")")
  }
  if (is.null(x.title)) x.title <- "Sample size"
  if (is.null(y.title)) y.title <- "Power"
  if (is.null(pwr.target)) pwr.target <- 0.80
  if (is.null(n.target)) n.target <- x.pred(x = n, y = pwr.fit, y.fixed = pwr.target)
  if (is.null(p.text.pos)) {
    if (n.target > min(n) + 0.75 * ((max(n)) - (min(n)))) {
      # Place legend at left of graph
      p.text.pos <- min(n) + 6
    } else {
      # Place legend at right of graph
      p.text.pos <- max(n) - 6
    }
  }
  if (is.null(n.text.pos)) {
    if (n.target > min(n) + 0.75 * ((max(n)) - (min(n)))) {
      # Place legend to left of line
      n.text.pos <- n.target - 7
    } else {
      # Place legend to right of line
      n.text.pos <- n.target + (0.25 * (max(n) - n.target))
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
  p <- p + geom_hline(aes(yintercept = pwr.target), colour = "#BB0000", linetype = "dotted")
  p.text <- paste0("Power=", pwr.target)
  p <- p + geom_text(aes(p.text.pos, pwr.target,
    label = p.text, vjust = -1,
    fontface = "plain", family = "sans"
  ), size = 5)

  n.axis.max <- max(df.pwr.fit$n)
  if (n.target <= n.axis.max) {
    # Add vertical line for n @ target power or precision, and label it
    p <- p + geom_vline(aes(xintercept = n.target), colour = "#BB0000", linetype = "dotted")
    n.text <- paste0("N.min=", n.target)
    # n.text.y.pos <- min(pwr)+0.03
    n.text.y.pos <- 0.06
    p <- p + geom_text(aes(n.text.pos, n.text.y.pos,
      label = n.text, vjust = 2,
      fontface = "plain", family = "sans"
    ), size = 5)
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

  # theme with white background
  p <- p + theme_bw()

  # eliminates background, gridlines, and chart border
  p <- p + theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

  # Set text sizes
  p <- p + theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )
  p <- p + theme(plot.title = element_text(size = 14))

  # return the graph
  return(p)
}

plot.power.logistic.multi.ei <- function(df.pwr,
                                         title.main = NULL, title.main.center = NULL,
                                         x.title = NULL, y.title = NULL,
                                         p.text.pos = NULL, p.text.pos.x.increment = NULL,
                                         n.text.pos = NULL, e.size.text.pos.x = NULL,
                                         e.size.text.pos.y = NULL,
                                         n.min.txt.pos.x.increment = NULL,
                                         n.min.txt.pos.y.increment = NULL,
                                         jitter.val = NULL, grayscale = NULL, no.fill = NULL,
                                         color.codes.manual = NULL, reverse.legend = FALSE) {
  ###################################################
  # Plots predicted (pwr) vs sample size (n) with fit curve &
  # adds lines for target power (pwr.target) and n @ target power (n.min).
  # Uses conventional power of 0.80 if target power not supplied.
  # Calculates n.min if not supplied.
  #
  # Parameters:
  # 		df.pwr				df of power analysis simulation results, should already
  # 								be subsetted to single vegetation + EI only
  # 	From df.pwr:
  # 		landcover		Vegetation/land cover class
  # 		EI						Indicator
  # 		stratum			Stratum (if applicable)
  # 		n.b					Total benchmark samples in original data
  # 		n.f					Total focal samples in original data
  # 		n						Simulated sample sizes (n.b.sim=n.f.sim)
  # 		e.size				Effect size used for simulation
  # 		e.size.adj
  # 		e.size.min		Lowest of the two margins of error: min(mean-lcl, ucl-mean)
  # 		pwr					Post hoc (observed) power of current simulation
  # 		n.min				Sample size at target power (pwr.target), from logistic fit
  # 		fit				Predicted power values from logistic fit
  # 		expts				Number of runs (experiments) in this simulation
  # 		boot.reps		Number of bootstrap replications used to calculate 95% CLs
  # 		pwr.target		Target power (default: 0.80)
  # 	Passed directly
  # 		title.main			Title for graph (default: none)
  # 		title.main.center	Optional. Center the title. Default=FALSE=left just.
  # 		x.title				X axis title (default: "Sample size")
  # 		y.title				Y axis title (default: "Power")
  # 		p.text.pos		Optional specification of x coordinate of left edge of
  # 								label for target power line
  # 		p.text.pos.x.increment		Optional specification of horizontal displacement
  # 								of power line label, relative to default position, in values of x.
  # 								Default=0. More intuitive than p.text.pos. Can be negative.
  # 		n.text.pos		Optional specification of x coordinate of left edge of
  # 								label for n.target line
  # 		e.size.text.pos.x		Optional specification of x coordinate of left edge of
  # 								label for effect size
  # 		e.size.text.pos.y		Optional specification of y coordinate of label for
  # 								effect size
  # 		n.min.txt.pos.x.increment	Optional specification of horizontal displacement
  # 								of n.min	label to right of vertical n.min line; default=4
  # 		n.min.txt.pos.y.increment		Optional specification of vertical displacement of
  # 								n.min label
  # 		jitter.val			Option. Amount of jitter to add to x and y using function
  # 								jitter(). Integer. See base function definition.
  # 		color.codes.manual	Optional. Vector of color codes to apply to liness
  # 								and legend.
  # 		reverse.legend	Optional. If NULL or FALSE use default sort order, if
  # 								TRUE reverse the sort order DOESN'T WORK!
  # Returns: ggplot object, ready for printing and saving
  ###################################################

  if (!require(ggplot2)) stop("ERROR: package 'ggplot2' not loaded")

  # Extract key variables and vectors
  df.pwr$e.size <- as.factor(df.pwr$e.size) # Convert effect size to a factor

  pwr.target <- unique(df.pwr$pwr.target)
  if (length(pwr.target) > 1) stop("ERROR: >1 value of target power (plot.power.logistic.multi.ei)")

  all.n <- df.pwr$n # Simulated sample sizes
  n.max <- max(all.n)
  n.min <- min(all.n)
  e.sizes <- unique(df.pwr$e.size) # Effect sizes used
  num.es <- length(e.sizes) # Number of effect sizes
  n.mins <- unique(df.pwr$n.min) # Minimum sample sizes at each effect size
  num.n.mins <- length(n.mins) # Should be same as num.es

  # Set default/calculated values
  if (is.null(title.main)) title.main <- "Power vs. Sample size"
  if (is.null(x.title)) x.title <- "Sample size (n)"
  if (is.null(y.title)) y.title <- "Power"
  if (is.null(pwr.target)) pwr.target <- 0.80
  if (is.null(jitter.val)) jitter.val <- 0
  if (is.null(grayscale)) grayscale <- FALSE
  if (is.null(no.fill)) no.fill <- FALSE
  if (is.null(n.min.txt.pos.x.increment)) n.min.txt.pos.x.increment <- 4
  if (is.null(n.min.txt.pos.y.increment)) n.min.txt.pos.y.increment <- 0.05
  if (is.null(p.text.pos.x.increment)) p.text.pos.x.increment <- 0
  if (is.null(reverse.legend)) reverse.legend <- FALSE

  # Set horizontal placement of text label for conventional power line
  if (is.null(p.text.pos)) {
    if (min(n.mins, na.rm = TRUE) > min(all.n, na.rm = TRUE) + 0.75 * ((max(all.n,
      na.rm = TRUE
    )) - (min(all.n, na.rm = TRUE)))) {
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
  df.pwr$n.min.txt.pos.x <- df.pwr$n.min + n.min.txt.pos.x.increment # To right of line
  df.pwr$n.min.txt.pos.y <- (1 / df.pwr$n.min * 2) +
    n.min.txt.pos.y.increment # Vertical displacement

  # Set vector of manually-defined colour codes
  # Make sure this has enough elements to support the largest possible number
  # of effect sizes
  # color.codes.all <-as.character(c("blue", "red", "green", "orange" ))
  # color.codes.all <-as.character(c("blue4", "royalblue1", "lightskyblue", "salmon" ))
  if (is.null(color.codes.manual)) {
    color.codes.all <- as.character(c("coral", "royalblue1", "seagreen", "red"))
    color.codes.all <- as.character(c("red", "royalblue1", "seagreen", "salmon"))
    color.codes <- color.codes.all[1:num.es] # Select the first num.es colors
  } else {
    # Use user-supplied color code vector
    color.codes <- color.codes.manual[1:num.es] # Select the first num.es colors
  }

  # Manually defined shape codes
  # These are all filled with outline
  shape.codes.all <- c(21, 22, 23, 24)
  shape.codes <- shape.codes.all[1:num.es] # Select the first num.es colors

  # Set legend title
  legend.title <- "Effect size"

  # Add requested jitter if applicable
  if (!(is.null(jitter.val) || jitter.val == 0)) {
    df.pwr$n <- jitter(df.pwr$n, jitter.val)
    df.pwr$pwr <- jitter(df.pwr$pwr, jitter.val)
  }

  # ---------------------------------------------
  # Plot the power curve
  # --------------------------------------------

  if (grayscale == TRUE) {
    p <- ggplot(df.pwr, aes(x = n, y = pwr, shape = e.size)) +
      theme_bw() +
      xlab(x.title) +
      ylab(y.title) +
      ggtitle(title.main) +
      geom_point(alpha = 0.95)
  } else {
    p <- ggplot(df.pwr, aes(x = n, y = pwr, colour = e.size, shape = e.size)) +
      theme_bw() +
      xlab(x.title) +
      ylab(y.title) +
      ggtitle(title.main) +
      geom_point(alpha = 0.95)

    # Apply manual colour scale and set legend title
    p <- p + scale_colour_manual(values = setNames(color.codes, e.sizes)) +
      labs(colour = legend.title, linetype = legend.title, shape = legend.title)
  }

  # Set legend sort order
  # DOESN'T WORK!
  p <- p + guides(fill = guide_legend(reverse = reverse.legend))

  if (no.fill == TRUE) {
    p <- p + scale_shape(solid = FALSE)
  }

  # Apply manual shapes
  # p <- p + scale_shape_manual(values=shape.codes)

  # Add the logistic fit lines
  p <- p + geom_line(aes(y = fit), show.legend = FALSE)

  # Modify the size of the legend symbols
  p <- p + guides(colour = guide_legend(override.aes = list(size = 3))) +
    theme(legend.key = element_rect(fill = NA))

  # Set axis limits, removing gutter
  p <- p + scale_x_continuous(limits = c(x.axis.min, x.axis.max))
  p <- p + scale_y_continuous(limits = c(y.axis.min, y.axis.max))

  # Add horizontal line at target power or precision & label it
  p <- p + geom_hline(aes(yintercept = pwr.target), colour = "black", linetype = "dotted")
  p.text <- paste0("Power=", pwr.target)
  # p <- p + geom_text(aes(p.text.pos, pwr.target, label=p.text, vjust=-1, fontface="plain",
  # family="sans"), size=5, colour="black" , show.legend=FALSE )
  p <- p + geom_text(aes(p.text.pos - 5, pwr.target, label = p.text, vjust = -1),
    size = 5,
    colour = "black", show.legend = FALSE
  )

  # Add vertical lines for n.min @ target power, and label them
  if (grayscale == TRUE) {
    p <- p + geom_vline(
      data = df.pwr, aes(xintercept = n.min), linetype = "dashed",
      show.legend = FALSE
    )
  } else {
    p <- p + geom_vline(
      data = df.pwr, aes(xintercept = n.min, colour = e.size),
      linetype = "dashed", show.legend = FALSE
    )
  }

  # Label the lines
  leg.pos.y <- df.pwr$e.size
  p <- p + geom_text(
    data = df.pwr,
    aes(
      x = n.min.txt.pos.x,
      y = n.min.txt.pos.y + 0.2,
      label = n.min.txt,
      vjust = 0,
      fontface = "plain",
      family = "sans"
    ),
    size = 4,
    colour = "black",
    show.legend = FALSE
  )

  # theme with white background
  p <- p + theme_bw()

  # eliminates background, gridlines, and chart border
  p <- p + theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

  # Set text sizes
  p <- p + theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "plain")
  )
  p <- p + theme(plot.title = element_text(size = 16, face = "bold"))
  p <- p + theme(legend.text = element_text(size = 12))
  p <- p + theme(legend.title = element_text(size = 14))

  if (title.main.center == TRUE) {
    p <- p + theme(plot.title = element_text(hjust = 0.5))
  }

  # return the graph
  return(p)
}

plot.power.logistic.multi.veg <- function(
  df.pwr, no.legend = FALSE, n.min.minmaxonly = NULL,
  n.min.min.fixed = NULL, n.min.max.fixed = NULL, n.min.omit = FALSE, x.max.fixed = NULL,
  title.main = NULL, x.title = NULL, y.title = NULL, p.text.pos = NULL, n.text.pos = NULL,
  veg.text.pos.x = NULL, veg.text.pos.y = NULL, jitter.val = NULL
) {
  ###################################################
  # Plots predicted (pwr) vs sample size (n) with fit curve for multi
  # strata (vegetation types) at once. Adds lines for target power
  # (pwr.target) and minimum sample size @ target power (n.min).
  # Can be configured to show only min and max lines for n.min.
  # Uses conventional power of 0.80 if target power not supplied.
  #
  # Parameters:
  # 		df.pwr				df of power analysis simulation results, should already
  # 								be subsetted to single vegetation + EI only
  # 	From df.pwr:
  # 		landcover		Vegetation/land cover class ("veg")
  # 		EI						Indicator
  # 		stratum			Stratum (if applicable)
  # 		n.b					Total benchmark samples in original data
  # 		n.f					Total focal samples in original data
  # 		n						Simulated sample sizes (n.b.sim=n.f.sim)
  # 		e.size				Effect size used for simulation
  # 		e.size.adj
  # 		e.size.min		Lowest of the two margins of error: min(mean-lcl, ucl-mean)
  # 		pwr					Post hoc (observed) power of current simulation
  # 		n.min				Sample size at target power (pwr.target), from logistic fit
  # 		fit				Predicted power values from logistic fit
  # 		expts				Number of runs (experiments) in this simulation
  # 		boot.reps		Number of bootstrap replications used to calculate 95% CLs
  # 		pwr.target		Target power (default: 0.80)
  # 	Passed directly
  # 		no.legend		Omit legend (avoids clutter if many veg types)
  # 		n.min.minmaxonly	Plot smallest and largest n.min lines only
  # 		n.min.min.fixed		Fixed value of n.min.min. If supplied overrides actual value from data
  # 		n.min.max.fixed		Fixed value of n.min.max. If supplied overrides actual value from data
  # 		n.min.omit		Don't plot n.min line(s)
  # 		title.main			Title for graph (default: none)
  # 		x.title				X axis title (default: "Sample size")
  # 		y.title				Y axis title (default: "Power")
  # 		p.text.pos		Optional specification of x coordinate of left edge of
  # 								label for target power line
  # 		n.text.pos		Optional specification of x coordinate of left edge of
  # 								label for n.target line
  # 		veg.text.pos.x		Optional specification of x coordinate of left edge of
  # 								label for effect size
  # 		veg.text.pos.y		Optional specification of y coordinate of label for
  # 								effect size
  # 		jitter.val			Option. Amount of jitter to add to x and y using function
  # 								jitter(). Integer. See base function definition.
  # 		x.max.fixed	Fixed max value of x axis. If NULL then max value of x used
  #
  # Returns: ggplot object, ready for printing and saving
  ###################################################

  if (!require(ggplot2)) stop("ERROR: package 'ggplot2' not loaded")

  # Extract key variables and vectors
  df.pwr$landcover <- as.factor(df.pwr$landcover) # Convert main grouping class to factor

  pwr.target <- unique(df.pwr$pwr.target)
  if (length(pwr.target) > 1) stop("ERROR: >1 value of target power (plot.power.logistic.multi.ei)")

  all.n <- df.pwr$n # Simulated sample sizes
  n.max <- max(all.n)
  n.min <- min(all.n)
  veggies <- unique(df.pwr$landcover) # Effect sizes used
  num.es <- length(veggies) # Number of effect sizes
  n.mins <- unique(df.pwr$n.min) # Minimum sample sizes at each effect size
  num.n.mins <- length(n.mins) # Should be same as num.es

  # Get min and max values of n.min
  n.min.min <- min(df.pwr$n.min)
  if (is.null(n.min.min.fixed)) {
    n.min.min <- min(df.pwr$n.min)
  } else {
    n.min.min <- n.min.min.fixed
  }
  n.min.min.text <- paste0("n.min=", n.min.min)
  if (is.null(n.min.max.fixed)) {
    n.min.max <- max(df.pwr$n.min)
  } else {
    n.min.max <- n.min.max.fixed
  }
  n.min.max.text <- paste0("n.min=", n.min.max)

  # Set default/calculated values
  if (is.null(title.main)) title.main <- "Power vs. Sample size"
  if (is.null(x.title)) x.title <- "Sample size (n)"
  if (is.null(y.title)) y.title <- "Power"
  if (is.null(pwr.target)) pwr.target <- 0.80
  if (is.null(no.legend)) nolegend <- FALSE
  if (is.null(n.min.minmaxonly)) n.min.minmaxonly <- FALSE

  # Set horizontal placement of text label for conventional power line
  if (is.null(p.text.pos)) {
    if (min(n.mins, na.rm = TRUE) > min(all.n, na.rm = TRUE) + 0.75 * ((max(all.n,
      na.rm = TRUE
    )) - (min(all.n, na.rm = TRUE)))) {
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
    p.text.pos <- x.axis.max - (0.05 * x.axis.max)
  }

  # Generate n.min text and set positions
  df.pwr$n.min.txt <- paste0("n=", as.character(df.pwr$n.min))
  df.pwr$n.min.txt.pos <- df.pwr$n.min + 3 # To right of line
  df.pwr$n.min.txt.pos[df.pwr$n.min.txt.pos > x.axis.max - 6 && !is.na(
    df.pwr$n.min.txt.pos
  )] <- df.pwr$n.min.txt.pos[df.pwr$n.min.txt.pos > x.axis.max - 6 &&
    !is.na(df.pwr$n.min.txt.pos)] - 6

  # Set vector of manually-defined colour codes
  # Make sure this has enough elements to support the largest possible number
  # of effect sizes
  color.codes.all <- as.character(c("blue", "red", "green", "orange"))
  # color.codes.all <-as.character(c("blue4", "royalblue1", "lightskyblue", "salmon" ))
  # color.codes.all <-as.character(c("coral", "lightskyblue2", "seagreen", "red" ))
  color.codes <- color.codes.all[1:num.es] # Select the first num.es colors

  # Manually defined shape codes
  # These are all filled with outline
  shape.codes.all <- c(21, 22, 23, 24)
  shape.codes <- shape.codes.all[1:num.es] # Select the first num.es colors

  # Set legend title
  legend.title <- "Vegetation"

  # Add requested jitter if applicable
  if (!is.null(jitter.val) || jitter.val == 0) {
    df.pwr$n <- jitter(df.pwr$n, jitter.val)
    df.pwr$pwr <- jitter(df.pwr$pwr, jitter.val)
  }

  # ---------------------------------------------
  # Plot the power curve
  # --------------------------------------------

  p <- ggplot(df.pwr, aes(x = n, y = pwr, colour = landcover)) +
    theme_bw() +
    xlab(x.title) +
    ylab(y.title) +
    ggtitle(title.main) +
    geom_point(alpha = 0.95)

  # Add the logistic fit lines
  p <- p + geom_line(aes(y = fit), show.legend = FALSE)

  if (no.legend == TRUE) {
    # Turn off legend
    p <- p + theme(legend.position = "none")
  } else {
    # Modify the size of the legend symbols
    p <- p + guides(colour = guide_legend(override.aes = list(size = 3))) +
      theme(legend.key = element_rect(fill = NA))
  }

  # Set axis limits, removing gutter
  p <- p + scale_x_continuous(limits = c(x.axis.min, x.axis.max))
  p <- p + scale_y_continuous(limits = c(y.axis.min, y.axis.max))

  # Add horizontal line at target power or precision & label it
  p <- p + geom_hline(aes(yintercept = pwr.target), colour = "black", linetype = "dotted")
  p.text <- paste0("Power=", pwr.target)
  p <- p + geom_text(aes(p.text.pos - 1, pwr.target, label = p.text, vjust = -1),
    size = 5,
    colour = "black", show.legend = FALSE
  )

  # Add vertical lines for n.min @ target power, and label them
  if (n.min.minmaxonly == TRUE) {
    p <- p + geom_vline(aes(xintercept = n.min.min), colour = "black", linetype = "dotted")
    p <- p + geom_vline(aes(xintercept = n.min.max), colour = "black", linetype = "dotted")
  } else {
    p <- p + geom_vline(
      data = df.pwr, aes(xintercept = n.min, colour = landcover),
      linetype = "dashed", show.legend = FALSE
    )
  }

  # Label the n.min lines
  if (n.min.minmaxonly == TRUE) {
    p <- p + geom_text(aes(n.min.min + 1.5, 0,
      label = n.min.min.text, vjust = -1,
      fontface = "plain", family = "sans"
    ), size = 5, colour = "black", show.legend = FALSE)
    p <- p + geom_text(aes(n.min.max + 1.5, 0.1,
      label = n.min.max.text, vjust = -1,
      fontface = "plain", family = "sans"
    ), size = 5, colour = "black", show.legend = FALSE)
  } else {
    p <- p + geom_text(data = df.pwr, aes(n.min.txt.pos, 0,
      label = n.min.txt, vjust = -1,
      fontface = "plain", family = "sans"
    ), size = 5, colour = "black", show.legend = FALSE)
  }

  # eliminates background, gridlines, and chart border
  p <- p + theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

  # Set text sizes
  p <- p + theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )
  p <- p + theme(plot.title = element_text(size = 16))
  if (no.legend == FALSE) {
    p <- p + theme(legend.text = element_text(size = 12))
    p <- p + theme(legend.title = element_text(size = 14))
  }

  # return the graph
  return(p)
}

opt.grid <- function(n) {
  #################################
  # Set optimum grid for multipanel figure
  # subject to constraint that rows<=cols
  # and cols<=5
  # returns c & r as vector of length 2
  #################################

  done <- F
  for (c in 1:5) {
    low <- c - 1
    for (r in low:c) {
      if (n <= c * r) {
        done <- T
        break
      }
    }
    if (done == T) break
  }
  dims <- c(c, r)
  return(dims)
}

plot.boxplot <- function(df, x.col, y.col,
                         x.lab = NULL, y.lab = NULL, fill.var = NULL, title.main = NULL,
                         fill_manual = NULL) {
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
    data = df,
    aes(
      x = eval(parse(text = x.col)), y = eval(parse(text = y.col)),
      fill = eval(parse(text = fill.var))
    )
  ) +
    geom_boxplot() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 18)
    ) +
    ggtitle(title.main) +
    xlab(x.lab) +
    ylab(y.lab)

  # Set color if requested
  if (!is.null(fill_manual)) {
    p <- p + scale_fill_manual(values = c(fill_manual))
  }

  # Set axis label format
  p <- p + theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 14)
  )
  p <- p + theme(
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 12)
  )

  return(p)
}

plot.violin <- function(df, x.col, y.col,
                        x.lab = NULL, y.lab = NULL, fill.var = NULL, title.main = NULL,
                        points.binwidth = NULL, fill_manual = NULL) {
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
    data = df,
    aes(
      x = eval(parse(text = x.col)), y = eval(parse(text = y.col)),
      fill = eval(parse(text = fill.var))
    )
  ) +
    geom_violin() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 18)
    ) +
    ggtitle(title.main) +
    xlab(x.lab) +
    ylab(y.lab)

  # Data points, no jitter
  p <- p + geom_dotplot(
    binaxis = "y", stackdir = "center", dotsize = 0.3,
    binwidth = points.binwidth
  )

  # Mean and median
  p <- p + stat_summary(fun = mean, geom = "point", size = 3)
  p <- p + stat_summary(fun = median, geom = "point", shape = 23, size = 3)

  # Set color if requested
  if (!is.null(fill_manual)) {
    p <- p + scale_fill_manual(values = c(fill_manual))
  }

  # Set axis label format
  p <- p + theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 14)
  )
  p <- p + theme(
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 12)
  )

  return(p)
}

save.plot <- function(p, p.dir, p.name, no.print = NULL) {
  ##########################################
  # Echos plot to terminal and saves
  ##########################################

  if (is.null(no.print)) print(p)
  graph.file <- paste(p.dir, "/", p.name, ".png", sep = "")
  print(paste("Printing graph: ", p.name, sep = ""))
  dev.copy(png, graph.file)
  dev.off()
}

plot.hist.single <- function(
  df, x.col, text.x = NULL, text.y = NULL, title.main = NULL,
  fill.val = NULL, no.bins = NULL, xlim.max = NULL
) {
  if (is.null(title.main)) title.main <- ""
  if (is.null(fill.val)) fill.val <- "black"
  if (is.null(no.bins)) title.main <- 12
  if (is.null(text.y)) text.y <- "Count"
  if (is.null(text.x)) text.x <- x

  p <- ggplot(df, aes(x = eval(parse(text = x.col)))) +
    geom_histogram(color = "black", fill = fill.val, bins = no.bins) +
    labs(title = title.main) +
    ylab(text.y) +
    xlab(text.x)

  # X axis limits
  if (!is.null(xlim.max)) {
    p <- p + xlim(-3, xlim.max)
  }

  # Y axis limits
  p <- p + scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, max(ggplot_build(p)$data[[1]]$count) * 1.1)
  )

  # Format title
  p <- p + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

  # Remove background & gridlines, add axis lines
  p <- p + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )

  return(p)
}

plot.hist.comb <- function(df1, df2, x.col, title.main = NULL, text.x = NULL, text.y = NULL,
                           no.bins = 12, xlim.min = NULL, xlim.max = NULL, ylim.max = NULL,
                           proportions = NULL,
                           x1.rgb = NULL, x2.rgb = NULL, alpha.val = NULL, print.legend = NULL,
                           leg.pos = NULL, x1.leg.text = NULL, x2.leg.text = NULL,
                           leg.inset = NULL) {
  ###############################################
  # Plot histogram for two sets of x values, with overlap
  ###############################################

  # Set defaults
  par(bg = "white")
  if (is.null(title.main)) title.main <- "" # No main title
  if (is.null(text.x)) text.x <- "X"
  if (is.null(xlim.min)) xlim.min <- 0
  if (is.null(no.bins)) no.bins <- 12
  if (is.null(proportions)) proportions <- FALSE

  # Validate parameter proportions and set default
  # values of freq and ylab parameters accordingly
  if (proportions == TRUE) {
    freq.val <- FALSE
    if (is.null(text.y)) text.y <- "Frequency"
  } else if (proportions == FALSE) {
    freq.val <- TRUE
    if (is.null(text.y)) text.y <- "Count"
  } else {
    stop("ERROR: Invalid parameter 'proportions'. (Fn: plot.hist.comb)")
  }

  # Color parameter defaults
  if (is.null(x1.rgb)) x1.rgb <- c(0, 0, 1) # Blue
  if (is.null(x2.rgb)) x2.rgb <- c(1, 0, 0) # Red
  if (is.null(alpha.val)) alpha.val <- 0.2 # Transparency, must be <.9 to see overlap

  # Color legend defaults
  if (is.null(print.legend)) print.legend <- TRUE
  if (is.null(leg.pos)) leg.pos <- "topright"
  if (is.null(x1.leg.text)) x1.leg.text <- "x1"
  if (is.null(x2.leg.text)) x2.leg.text <- "x2"
  if (is.null(leg.inset)) leg.inset <- c(0.1, 0)

  # Get largest value of x across both dfs
  x.max <- max(df1[[x.col]], df2[[x.col]])

  # If null, set xlim.max based on max value of x
  if (is.null(xlim.max)) {
    # Determine mid-point of largest x-axis bin
    xlim.max <- x.max + (x.max * 0.1)
  }

  # Generate vector of x values over which y densities
  # will be calculated, based on x.max and # of bins
  x.vals <- seq(0, x.max + (x.max * 0.1), length.out = no.bins)

  # Generate histogram objects but don't print yet
  # Needed for setting y.axis upper limits
  h1 <- hist(df1[[x.col]], breaks = x.vals, plot = FALSE)
  h2 <- hist(df2[[x.col]], breaks = x.vals, plot = FALSE)

  # Save submitted ylim.max parameter
  ylim.max.submitted <- ylim.max

  # Set y axis upper limit to bin with highest count if not supplied by user
  highestCount <- max(h1$counts, h2$counts)
  ylim.max <- highestCount + (highestCount * 0.4)

  # Rescale to proportions if requested & reset ylim.max accordingly
  if (proportions == TRUE) {
    # Get density distributions
    h1$density <- h1$counts / sum(h1$counts)
    h2$density <- h2$counts / sum(h2$counts)

    # y upper limit is bin with highest density
    highestDensity <- max(h1$density, h2$density)
    ylim.max <- min(1, highestDensity + 0.1)
  }

  # Reset ylim.max to user-submitted parameter if applicable
  if (!is.null(ylim.max.submitted)) ylim.max <- ylim.max.submitted

  # Plot the histograms
  plot(h1,
    main = title.main,
    xlab = text.x,
    ylab = text.y,
    col = rgb(x1.rgb[1], x1.rgb[2], x1.rgb[3], alpha.val),
    xlim = c(xlim.min, xlim.max),
    ylim = c(0, ylim.max),
    freq = freq.val,
    add = FALSE
  )
  plot(h2,
    main = title.main,
    xlab = text.x,
    ylab = text.y,
    col = rgb(x2.rgb[1], x2.rgb[2], x2.rgb[3], alpha.val),
    xlim = c(xlim.min, xlim.max),
    ylim = c(0, ylim.max),
    freq = freq.val,
    add = TRUE
  )

  # Print color legend if requested
  if (print.legend == TRUE) {
    legend("topright",
      legend = c(x1.leg.text, x2.leg.text),
      inset = leg.inset,
      fill = c(
        rgb(x1.rgb[1], x1.rgb[2], x1.rgb[3], alpha.val),
        rgb(x2.rgb[1], x2.rgb[2], x2.rgb[3], alpha.val)
      ),
      box.lty = 0
    )
  }
}

lm_eqn <- function(df) {
  # Return lm regression object
  # Columns of df must be name x & y
  m <- lm(y ~ x, df)
  eq <- substitute(
    italic(y) == a + b %.% italic(x) * "," ~ ~ italic(r)^2 ~ "=" ~ r2,
    list(
      a = format(unname(coef(m)[1]), digits = 2),
      b = format(unname(coef(m)[2]), digits = 2),
      r2 = format(summary(m)$r.squared, digits = 3)
    )
  )
  as.character(as.expression(eq))
}
