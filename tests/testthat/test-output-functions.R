output_functions_environment <- function() {
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "includes", "functions.R"), envir = env)
  env
}

testthat::test_that("labels and filename formatting preserve existing substitutions", {
  env <- output_functions_environment()
  testthat::expect_equal(env$unix.friendly("Wet / Forest (A), [B]"), "wet___forest_a_b")
  testthat::expect_equal(env$human.readable("v[forest]+s[canopy]"), "forest, canopy]")
  testthat::expect_equal(
    env$human.readable.landcover("v[forest]+d[mine]+r[r.pl]"), "forest] (reclaimed)"
  )
  testthat::expect_equal(env$human.readable.bm.veg("v[forest]+s[canopy]"), "forest, canopy]")
  testthat::expect_equal(env$reclaimed.make.pretty("forest, mine, r.pl"), "forest (reclaimed)")
  testthat::expect_equal(env$specify_decimal(1.2, 3), "1.200")
  testthat::expect_true(is.na(env$specify_decimal(NA_real_, 3)))
  testthat::expect_equal(env$trim(c("  forest\t", "meadow")), c("forest", "meadow"))
  testthat::expect_equal(env$replace.last("a", "x", "banana"), "bananx")
  testthat::expect_equal(env$smartpaste("", "A", "", "B", sep = ","), "A, B")
  testthat::expect_equal(env$smartpaste0("", "A", "", "B", sep = ","), "A,B")
  testthat::expect_equal(env$capFirst(c("forest", "meadow")), c("Forest", "Meadow"))
  testthat::expect_equal(env$lowerFirst(c("Forest", "Meadow")), c("forest", "meadow"))
  testthat::expect_equal(env$pasteNA(c(NA, "A"), c(NA, "B")), c(NA_character_, "A, B"))
  testthat::expect_equal(env$smart.concat(c(NA, "A"), c("B", NA)), c("B", "A"))
  testthat::expect_true(is.na(env$smart.concat(NA_character_, NA_character_, empty.to.na = TRUE)))
})

testthat::test_that("data utilities retain column order, types, and validation", {
  env <- output_functions_environment()
  df <- data.frame(a = 1:2, b = 3:4, c = 5:6, d = 7:8)
  testthat::expect_identical(names(env$df.reorder(df, "d", move.first = TRUE)), c(
    "d", "a",
    "b", "c"
  ))
  testthat::expect_identical(names(env$df.reorder(df, "a", move.last = TRUE)), c(
    "b", "c",
    "d", "a"
  ))
  testthat::expect_identical(names(env$df.reorder(df, "d", "a")), c("a", "d", "b", "c"))
  testthat::expect_error(env$df.reorder(df, "missing"), "not in data frame")
  testthat::expect_error(env$df.reorder(df, "a"), "col.before.*not specified")
  testthat::expect_equal(env$move.col(4, 2, 4), c(1, 4, 2, 3))
  factors <- data.frame(label = factor(c("A", "B")), number = 1:2)
  testthat::expect_identical(env$factors.to.chr(factors), data.frame(
    label = c("A", "B"),
    number = 1:2
  ))
  testthat::expect_identical(env$convert.magic(df, "character")$a, c("1", "2"))
  testthat::expect_true(env$pk.uniq(df, "a"))
  testthat::expect_false(env$pk.uniq(data.frame(a = c(1, 1)), "a"))
  testthat::expect_equal(env$transform_column(c("A", "B", NA), c(A = "Forest")), c(
    "Forest",
    "B", NA
  ))
  testthat::expect_true(env$is.num(1))
  testthat::expect_false(env$is.num(c(1, 2)))
  testthat::expect_false(env$is.num("1"))
})

testthat::test_that("summary means retain zero and missing-value behaviour", {
  env <- output_functions_environment()
  testthat::expect_equal(env$gmean(c(1, 4), no.zero = FALSE), 2)
  testthat::expect_equal(env$gmean.orig(c(1, 4)), 2)
  testthat::expect_equal(env$gmean.orig(c(0, 4)), 0)
  testthat::expect_equal(env$gmean(c(0, 4), no.zero = FALSE), 0)
  testthat::expect_gt(env$gmean(c(0, 4)), 0)
  testthat::expect_equal(env$gmean(c(0, 4), na.weighted = TRUE), 0)
  testthat::expect_true(is.na(env$gmean(c(1, NA), na.rm = FALSE)))
  testthat::expect_true(is.nan(env$gmean(c(-1, 4))))
  testthat::expect_equal(env$generalized_mean(c(1, 4), p = 0), 2)
  testthat::expect_equal(env$generalized_mean(c(1, 4), p = 1), 2.5)
  testthat::expect_equal(env$fixZeros(0), 0.000001)
  testthat::expect_equal(env$fixZeros(0.5), 0.5)
  testthat::expect_equal(env$gmean.fix0(c(1, 0)), c(1, 0.000001))
  testthat::expect_error(env$gmean.fix0("0"), "not a numeric vector")
  testthat::expect_equal(env$logittr(c(0.25, 0.5, 1)), c(log(1 / 3), 0, 0))
  testthat::expect_equal(env$sorensen(c("A", "B"), c("B", "C")), 0.5)
})

testthat::test_that("input helpers resolve indicator settings in the evaluation environment", {
  env <- output_functions_environment()
  env$ei.params <- function(ei) list(df.input = c(SR = "cover", TS = "cover", BA = "stems")[[ei]])
  env$DF.LANDCOVER <- "landCover"
  env$DF.PLOTMETADATA <- "plotMetadata"
  env$DF.SPECIES <- "species"
  env$EI.vec <- c("SR", "TS", "BA")
  expected <- c("landCover", "plotMetadata", "species", "cover", "stems")
  testthat::expect_identical(env$input_file_list(), expected)
  testthat::expect_identical(env$input_file_list_all(), list(
    df.input.list.a = expected[1:3], df.input.list.b = expected[4:5], df.input.list = expected
  ))
  testthat::expect_identical(env$ei.params.global("SR")$distn, "NBin")
  testthat::expect_identical(env$ei.params.global("mpck")$bm.val, 0)
  testthat::expect_error(env$ei.params.global("missing"), "unknown EI")
})

testthat::test_that("coordinate helpers retain bounds and great-circle distances", {
  env <- output_functions_environment()
  for (lat in c(-90, 0, 90, "45")) testthat::expect_true(env$is.valid.lat(lat))
  for (lat in c(-91, 91, NA, "invalid")) testthat::expect_false(env$is.valid.lat(lat))
  for (lon in c(-180, 0, 180, "45")) testthat::expect_true(env$is.valid.lon(lon))
  for (lon in c(-181, 181, NA, "invalid")) testthat::expect_false(env$is.valid.lon(lon))
  testthat::expect_equal(env$gcd(0, 0, 0, 0), 0)
  testthat::expect_equal(env$gcd(0, 0, 0, 90), 6371000 * pi / 2)
  testthat::expect_true(is.na(env$gcd(91, 0, 0, 0)))
})

testthat::test_that("spreadsheet helpers preserve column references and formatting decisions", {
  env <- output_functions_environment()
  for (i in c(1, 26, 27, 52, 703)) {
    letter <- c("A", "Z", "AA", "AZ", "AAA")[[match(i, c(1, 26, 27, 52, 703))]]
    testthat::expect_identical(env$xlcolconv(i), letter)
    testthat::expect_equal(env$xlcolconv(tolower(letter)), i)
  }
  widths <- list()
  styles <- list()
  wb <- list(
    set_col_widths = function(...) widths[[length(widths) + 1L]] <<- list(...),
    add_cell_style = function(...) styles[[length(styles) + 1L]] <<- list(...)
  )
  df <- data.frame(Label = c("Forest", "Meadow"), Quality = c(0.5, NA_real_), Count = 2:3)
  env$xl.col.wj(wb, "Summary", df, row.end = 3)
  testthat::expect_equal(vapply(widths, function(x) x$widths, numeric(1)), c(8, 9, 7))
  testthat::expect_identical(vapply(styles, function(x) x$dims, character(1)), c(
    "A1:A3",
    "B1:B3", "C1:C3"
  ))
  # A missing numeric value currently selects left alignment; retain that decision.
  testthat::expect_identical(vapply(styles, function(x) x$horizontal, character(1)), c(
    "left", "left", "right"
  ))
  actual <- openxlsx2::wb_workbook()$add_worksheet("Summary")
  actual$add_data(sheet = "Summary", x = df)
  env$xl.col.wj(actual, "Summary", df, row.end = 3)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path))
  actual$save(path)
  testthat::expect_identical(readxl::excel_sheets(path), "Summary")
  testthat::expect_equal(as.data.frame(readxl::read_excel(path)), df)
})

testthat::test_that("plot constructors retain labels, data mapping, and layout decisions", {
  suppressPackageStartupMessages(library(ggplot2))
  env <- output_functions_environment()
  df <- data.frame(group = rep(c("A", "B"), each = 4), value = c(1:4, 2:5))
  plot <- env$plot.boxplot(df, "group", "value", fill_manual = c(A = "red", B = "blue"))
  testthat::expect_identical(plot$labels$x, "group")
  testthat::expect_identical(plot$labels$y, "value")
  built <- ggplot2::ggplot_build(plot)
  testthat::expect_equal(nrow(built$data[[1]]), 2)
  testthat::expect_identical(built$data[[1]]$fill, c("red", "blue"))
  violin <- env$plot.violin(df, "group", "value", points.binwidth = 0.5)
  testthat::expect_length(violin$layers, 4)
  histogram <- env$plot.hist.single(df, "value", text.x = "Cover", no.bins = 4, xlim.max = 6)
  testthat::expect_identical(histogram$labels$x, "Cover")
  testthat::expect_equal(sum(ggplot2::ggplot_build(histogram)$data[[1]]$count), nrow(df))
  testthat::expect_equal(env$opt.grid(1), c(1, 1))
  testthat::expect_equal(env$opt.grid(6), c(3, 2))
  testthat::expect_equal(env$x.pred(c(10, 20, 30), c(0.3, 0.8, 0.9), 0.8), 20)
  testthat::expect_match(env$lm_eqn(data.frame(x = 1:4, y = c(2, 4, 5, 9))), "italic")
  testthat::expect_error(
    env$plot.power.logistic.multi.ei(data.frame(e.size = 1, pwr.target = c(0.8, 0.9))),
    ">1 value of target power"
  )
})

testthat::test_that("base plotting and plot saving keep device and filename behaviour", {
  env <- output_functions_environment()
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  device <- grDevices::dev.cur()
  on.exit({
    if (device %in% grDevices::dev.list()) grDevices::dev.off(device)
    unlink(path)
  })
  env$qc.calc.simple.demo(0, 1, 10)
  testthat::expect_identical(grDevices::dev.cur(), device)
  testthat::expect_error(
    env$plot.hist.comb(data.frame(x = 1:3), data.frame(x = 1:3), "x", proportions = "bad"),
    "Invalid parameter"
  )
  saved <- NULL
  closed <- FALSE
  env$dev.copy <- function(device, filename) saved <<- filename
  env$dev.off <- function() closed <<- TRUE
  testthat::expect_output(env$save.plot(NULL, "figures", "quality", no.print = TRUE), "quality")
  testthat::expect_identical(saved, "figures/quality.png")
  testthat::expect_true(closed)
})

testthat::test_that("CSV error reporting and quiet evaluation retain caller state", {
  env <- output_functions_environment()
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  df <- data.frame(id = 1:2, issue = c("missing", "invalid"))
  testthat::expect_output(
    env$df.err.save(df, basename(path), paste0(dirname(path), "/"), "Missing", "Skipped"),
    "Missing.*Skipped"
  )
  testthat::expect_equal(read.csv(path), df)
  depth <- sink.number()
  testthat::expect_identical(env$quiet({
    cat("hidden")
    42L
  }), 42L)
  testthat::expect_identical(sink.number(), depth)
  old_option <- getOption("show.error.messages")
  testthat::expect_output(testthat::expect_error(env$stop_quietly("Stopped")), "Stopped")
  testthat::expect_identical(getOption("show.error.messages"), old_option)
})
