#####################################################
# Calculate sorensen index for all focal species pool
# vs benchmark species pool, using speciesCover
# as input data frame
#####################################################

spp.all <- unique(speciesCover$species)
vegClasses <- unique(speciesCover$vegClass)
vegClasses <- sort(vegClasses)

sorensen <- function(spp1, spp2) {
  A <- length(f.spp)
  B <- length(b.spp)
  shared <- intersect(spp1, spp2)
  C <- length(shared)
  sorensen.index <- (2 * C) / (A + B)
  return(sorensen.index)
}

cat("Focal vs benchmark species overlap by vegclass for '", PROJ, " ", ASSESS, "':\n", sep = "")
cat("vegClass: SorensenIndex\n")

for (vegClass in vegClasses) {
  f.spp <- f.spp <- unique(speciesCover[speciesCover$focalOrBenchmark == "f" & speciesCover$vegClass == vegClass, c("species")])
  b.spp <- unique(speciesCover[speciesCover$focalOrBenchmark == "b" & speciesCover$vegClass == vegClass, c("species")])
  I <- sorensen(f.spp, b.spp)
  cat(" ", vegClass, ": ", I, "\n", sep = "")
}

cat("\n")
cat("Comparing to different benchmarks:\n")
cat("ASSESS=", ASSESS, "\n", sep = "")
f.vegClass <- "5340"
f.spp <- f.spp <- unique(speciesCover[speciesCover$focalOrBenchmark == "f" & speciesCover$vegClass == f.vegClass, c("species")])
b.vegClass <- "5340"
b.spp <- unique(speciesCover[speciesCover$focalOrBenchmark == "b" & speciesCover$vegClass == b.vegClass, c("species")])
I <- sorensen(f.spp, b.spp)
cat("Focal vegClass='", f.vegClass, "', benchmark vegClass='", b.vegClass, "', I=", I, "\n", sep = "")

f.vegClass <- "5340"
f.spp <- f.spp <- unique(speciesCover[speciesCover$focalOrBenchmark == "f" & speciesCover$vegClass == f.vegClass, c("species")])
b.vegClass <- "5350"
b.spp <- unique(speciesCover[speciesCover$focalOrBenchmark == "b" & speciesCover$vegClass == b.vegClass, c("species")])
I <- sorensen(f.spp, b.spp)
cat("Focal vegClass='", f.vegClass, "', benchmark vegClass='", b.vegClass, "', I=", I, "\n", sep = "")
