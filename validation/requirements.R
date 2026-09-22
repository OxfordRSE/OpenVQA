# Shared validation requirements.
#
# This is the single source of truth for indicator input dependencies. The
# dependency table follows schema/README.md. Schema selection remains in
# validation/schema.R, while cross-file checks remain in validation/crossfile.R.

validation_requirements <- function() {
  list(
    core = list(
      required = c("landCover.csv", "plotMetadata.csv"),
      optional = c("vegetation.csv")
    ),
    indicators = list(
      SR = list(
        required = c("species.csv"),
        one_of = list(c("speciesCover.csv", "speciesStems.csv"))
      ),
      TD = list(
        required = character(),
        one_of = list(c("speciesCover.csv", "speciesStems.csv"))
      ),
      TS = list(
        required = c("species.csv"),
        one_of = list(c("speciesCover.csv", "speciesStems.csv"))
      ),
      PCGF = list(
        required = c("coverByGrowthForm.csv")
      ),
      PCS = list(
        required = c("coverByStratum.csv")
      ),
      GC = list(
        required = c("groundCover.csv")
      ),
      PCES = list(
        required = c("exoticCoverByStratum.csv")
      ),
      PCESS = list(
        required = c("exoticCoverByStratum.csv")
      ),
      BA = list(
        required = c("speciesStems.csv", "species.csv")
      ),
      BAES = list(
        required = c("speciesStems.csv", "species.csv")
      ),
      ASC = list(
        required = c("speciesStems.csv", "species.csv")
      ),
      CH = list(
        required = c("species.csv"),
        one_of = list(c("speciesCover.csv", "speciesStems.csv"))
      )
    )
  )
}
