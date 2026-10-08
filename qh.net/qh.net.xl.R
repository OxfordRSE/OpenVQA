##################################################################
# Convert QH.net results to Excel workbook
#
# Sourced by qh.net.R
# Note: currently only converts main QH.net results file, not
# potential or trade-up net QHs
##################################################################

##########################################
# Check required dependencies loaded.
# Assumes params file loaded.
##########################################

require(openxlsx2)
if (!exists("functions.loaded", mode = "function")) source(paste0(SRCDIR, "includes/functions.R"))

##########################################
# Set local parameters
##########################################

# Output file
if (INCLUDE.OFFSET) {
  file.qh.net.xslx <- paste0(qh.net.resultsdir, qh.net.off.file.basename, ".xlsx")
} else {
  file.qh.net.xslx <- paste0(qh.net.resultsdir, qh.net.file.basename, ".xlsx")
}

# Field-specific numeric precision (decimal places)
dp.area <- 1
dp.qh <- 1
dp.qh.net <- 1

##########################################
# Begin
##########################################

cat("\n******************************************\n")
cat("Exporting QH.net results to Excel\n\n")
cat("Importing and restructuring CSV summary files:\n")

##########################################
# Overall quality results
##########################################

if (INCLUDE.OFFSET) {
  input.filename <- paste0(qh.net.off.file.basename, ".csv")
} else {
  input.filename <- paste0(qh.net.file.basename, ".csv")
}
input.file <- paste0(qh.net.resultsdir, input.filename)

cat("- ", basename(input.filename), "...", sep = "")
qh.net <- read.csv(input.file, header = T)
qh.net.bak <- qh.net

# Convert to character and set precision of area and quality hectares fields
for (i in 1:nrow(qh.net)) {
  qh.net$area_ha0[i] <- specify_decimal(as.numeric(qh.net$area_ha0[i]), dp.area)
  qh.net$qh0[i] <- specify_decimal(as.numeric(qh.net$qh0[i]), dp.qh)
  qh.net$qh.lcl0[i] <- specify_decimal(as.numeric(qh.net$qh.lcl0[i]), dp.qh)
  qh.net$qh.ucl0[i] <- specify_decimal(as.numeric(qh.net$qh.ucl0[i]), dp.qh)
  qh.net$area_ha1[i] <- specify_decimal(as.numeric(qh.net$area_ha1[i]), dp.area)
  qh.net$qh1[i] <- specify_decimal(as.numeric(qh.net$qh1[i]), dp.qh)
  qh.net$qh.lcl1[i] <- specify_decimal(as.numeric(qh.net$qh.lcl1[i]), dp.qh)
  qh.net$qh.ucl1[i] <- specify_decimal(as.numeric(qh.net$qh.ucl1[i]), dp.qh)
  qh.net$qh.net[i] <- specify_decimal(as.numeric(qh.net$qh.net[i]), dp.qh.net)
  qh.net$qh.net.lcl[i] <- specify_decimal(as.numeric(qh.net$qh.net.lcl[i]), dp.qh.net)
  qh.net$qh.net.ucl[i] <- specify_decimal(as.numeric(qh.net$qh.net.ucl[i]), dp.qh.net)
}

if (INCLUDE.OFFSET) {
  for (i in 1:nrow(qh.net)) {
    qh.net$qh.off[i] <- specify_decimal(as.numeric(qh.net$qh.off[i]), dp.qh)
    qh.net$qh.off.lcl[i] <- specify_decimal(as.numeric(qh.net$qh.off.lcl[i]), dp.qh)
    qh.net$qh.off.ucl[i] <- specify_decimal(as.numeric(qh.net$qh.off.ucl[i]), dp.qh)
    qh.net$qh.net.final[i] <- specify_decimal(as.numeric(qh.net$qh.net.final[i]), dp.qh.net)
    qh.net$qh.net.final.lcl[i] <- specify_decimal(as.numeric(qh.net$qh.net.final.lcl[i]), dp.qh.net)
    qh.net$qh.net.final.ucl[i] <- specify_decimal(as.numeric(qh.net$qh.net.final.ucl[i]), dp.qh.net)
  }
}

cat("done\n")

##########################################
# Metadata
##########################################

cat("Preparing metadata...")
latest.run <- file.info(input.file)$mtime
latest.run <- format(latest.run, "%Y-%m-%d %H:%M:%S %Z")

df.meta <- data.frame(
  header = character(0),
  value = character(0),
  stringsAsFactors = FALSE
)
if (INCLUDE.OFFSET) {
  df.meta <- rbind(
    df.meta,
    data.frame(
      head = c("Project:", "Baseline assessment:", "Current assessment:", "Offset assessment:", "Analysis date:"),
      value = c(PROJ, ASSESS.BASELINE, ASSESS, ASSESS.OFFSET, latest.run),
      stringsAsFactors = FALSE
    )
  )
} else {
  df.meta <- rbind(
    df.meta,
    data.frame(
      head = c("Project:", "Baseline assessment:", "Current assessment:", "Analysis date:"),
      value = c(PROJ, ASSESS.BASELINE, ASSESS, latest.run),
      stringsAsFactors = FALSE
    )
  )
}
cat("done\n")

#####################################################
# Make data frames more Excel-friendly
#####################################################

cat("Making data frames Excel-friendly:\n")

cat("- Renaming data frames...")
# Prefix dfs to distinguish them from sheet objects
df.qh.net <- qh.net
cat("done\n")

cat("- Combining quality hectares and confidences limis into single column...")
df.qh.net$qh0.full <- paste0(df.qh.net$qh0, " [", df.qh.net$qh.lcl0, ", ", df.qh.net$qh.ucl0, "]")
df.qh.net$qh1.full <- paste0(df.qh.net$qh1, " [", df.qh.net$qh.lcl1, ", ", df.qh.net$qh.ucl1, "]")
df.qh.net$qh.net.full <- paste0(
  df.qh.net$qh.net, " [", df.qh.net$qh.net.lcl, ", ", df.qh.net$qh.net.ucl, "]"
)
if (INCLUDE.OFFSET) {
  df.qh.net$qh.off.full <- paste0(
    df.qh.net$qh.off, " [", df.qh.net$qh.off.lcl, ", ", df.qh.net$qh.off.ucl, "]"
  )
  df.qh.net$qh.net.final.full <- paste0(
    df.qh.net$qh.net.final, " [", df.qh.net$qh.net.final.lcl, ", ", df.qh.net$qh.net.final.ucl, "]"
  )
}
cat("done\n")

cat("- Converting '0.0 [0.0, 0.0]' to '0'...")
df.qh.net[df.qh.net == "0.0 [0.0, 0.0]"] <- "0"
cat("done\n")

cat("- Dropping superfluous columns...")
drop.cols <- c(
  "qh0", "qh.lcl0", "qh.ucl0", "qh1", "qh.lcl1", "qh.ucl1", "qh.net", "qh.net.lcl", "qh.net.ucl"
)
if (INCLUDE.OFFSET) {
  drop.cols <- c(
    drop.cols,
    c("qh.off", "qh.off.lcl", "qh.off.ucl", "qh.net.final", "qh.net.final.lcl", "qh.net.final.ucl")
  )
}
df.qh.net <- df.qh.net[, !(names(df.qh.net) %in% drop.cols)]
cat("done\n")

cat("- Reordering columns...")
df.qh.net <- df.reorder(df.qh.net, col.move = "area_ha1", col.before = "qh0.full")
cat("done\n")

cat("- Renaming headers...")
names(df.qh.net)[names(df.qh.net) == "bm.veg"] <- "Benchmark Vegetation"
names(df.qh.net)[names(df.qh.net) == "area_ha1"] <- "Current\narea (ha)"
names(df.qh.net)[names(df.qh.net) == "area_ha0"] <- "Baseline\narea (ha)"
# names(df.qh.net)[names(df.qh.net) == 'qh0.full'] <- 'Baseline QH\n[Lower, Upper 95% CLs]'
# names(df.qh.net)[names(df.qh.net) == 'qh1.full'] <- 'Current QH\n[Lower, Upper 95% CLs]'
# names(df.qh.net)[names(df.qh.net) == 'qh.net.full'] <- 'Net QH\n[Lower, Upper 95% CLs]'

# Alias the following column names for reuse below
col.qh0.full <- "Baseline QH"
col.qh1.full <- "Current QH"
col.qh.net.full <- "Net QH\n(no offsets)"
names(df.qh.net)[names(df.qh.net) == "qh0.full"] <- col.qh0.full
names(df.qh.net)[names(df.qh.net) == "qh1.full"] <- col.qh1.full
names(df.qh.net)[names(df.qh.net) == "qh.net.full"] <- col.qh.net.full

if (INCLUDE.OFFSET) {
  col.qh.off.full <- "Additional\noffset QH"
  col.qh.net.final.full <- "Net QH\n(with offsets)"
  names(df.qh.net)[names(df.qh.net) == "qh.off.full"] <- col.qh.off.full
  names(df.qh.net)[names(df.qh.net) == "qh.net.final.full"] <- col.qh.net.final.full
}
cat("done\n")

#####################################################
# Convert to sheets of single Excel workbook & save
#####################################################

cat("Preparing Excel file:\n", sep = "")

cat("- Creating workbook...")
wb <- wb_workbook()
cat("done\n")

####################
# Add sheet meta
####################

ws <- "Meta"
cat("- Preparing sheet '", ws, "':\n", sep = "")

cat("-- Adding sheet...")
wb <- wb_add_worksheet(wb, sheet = ws)
wb <- wb_add_data(wb, ws, df.meta, col_names = FALSE)
cat("done\n")

cat("-- Setting fonts...")
# Set first column bold and second column plain
wb$add_font(ws, "A1:A100", name = "Arial", bold = TRUE)
wb$add_font(ws, "B1:B100", name = "Arial")
cat("done\n")

cat("-- Setting column width and justification...")
wb$set_col_widths(sheet = "meta", cols = c(1), widths = max(nchar(df.meta$head)) + 2)
wb$set_col_widths(sheet = "meta", cols = c(2), widths = max(nchar(df.meta$value)) + 2)
cat("done\n")

#################################
# Add sheet Net Quality Hectares
#################################

ws <- "Net Quality Hectares"
cat("- Preparing sheet '", ws, "':\n", sep = "")

cat("-- Adding sheet...")
wb <- wb_add_worksheet(wb, sheet = ws)
wb <- wb_add_data(wb, ws, df.qh.net)
cat("done\n")

cat("-- Setting fonts...")
wb$add_font(ws, "A1:Z500", name = "Arial") # Whole sheet
wb$add_font(ws, "A1:Z1", name = "Arial", bold = TRUE) # Header
cat("done\n")

# Set col width to widest string, justify text left, numbers right
cat("-- Setting column width and justification...")
xl.col.wj(curr.wb = wb, curr.ws = ws, curr.df = df.qh.net)

# Special handling for formatted column qh0.full
col.idx <- which(names(df.qh.net) == col.qh0.full)
col.idx.xl <- xlcolconv(col.idx)
cmd <- paste0("wb$add_cell_style(sheet=ws, dims='", col.idx.xl, "1:", col.idx.xl, "500', horizontal='right' )")
eval(parse(text = cmd))

# Special handling for formatted column qh1.full
col.idx <- which(names(df.qh.net) == col.qh1.full)
col.idx.xl <- xlcolconv(col.idx)
cmd <- paste0("wb$add_cell_style( sheet=ws, dims='", col.idx.xl, "1:", col.idx.xl, "500', horizontal='right' )")
eval(parse(text = cmd))

# Special handling for formatted column qh.net.full
col.idx <- which(names(df.qh.net) == col.qh.net.full)
col.idx.xl <- xlcolconv(col.idx)
cmd <- paste0("wb$add_cell_style( sheet=ws, dims='", col.idx.xl, "1:", col.idx.xl, "500', horizontal='right' )")
eval(parse(text = cmd))

if (INCLUDE.OFFSET) {
  # Special handling for formatted column qh1.full
  col.idx <- which(names(df.qh.net) == col.qh.off.full)
  col.idx.xl <- xlcolconv(col.idx)
  cmd <- paste0("wb$add_cell_style( sheet=ws, dims='", col.idx.xl, "1:", col.idx.xl, "500', horizontal='right' )")
  eval(parse(text = cmd))

  # Special handling for formatted column qh.net.full
  col.idx <- which(names(df.qh.net) == col.qh.net.final.full)
  col.idx.xl <- xlcolconv(col.idx)
  cmd <- paste0("wb$add_cell_style( sheet=ws, dims='", col.idx.xl, "1:", col.idx.xl, "500', horizontal='right' )")
  eval(parse(text = cmd))
}

# Enable first row (headers) to wrap at embedded line breaks
wb$add_cell_style(dims = "A1:Z1", wrap_text = "1")

cat("done\n")

##########################
# Save the final workbook
##########################

qh.net.file.name <- paste0(qh.net.file.basename, ".xlsx")
cat("Saving completed Excel file '", qh.net.file.name, "' to results directory...", sep = "")
wb$save(file.qh.net.xslx)
cat("done\n")
