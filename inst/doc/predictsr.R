## ----setup--------------------------------------------------------------------
predicts <- predictsr::GetPredictsData(extract = c(2016, 2022))
str(predicts)

## -----------------------------------------------------------------------------
taxa <- predicts[
  !duplicated(predicts[, c("Source_ID", "Study_name", "Taxon_name_entered")]),
]
species_counts <- length(
  unique(taxa[taxa$Rank %in% c("Species", "Infraspecies"), "Taxon"])
) +  nrow(taxa[!taxa$Rank %in% c("Species", "Infraspecies"), ])

print(glue::glue(
  "This database has {length(unique(predicts$SS))} studies across ",
  "{length(unique(predicts$SSBS))} sites, in ",
  "{length(unique(predicts$Country))} countries, and with ",
  "{species_counts} species."
))

## -----------------------------------------------------------------------------
print(glue::glue(
  "Earliest sample collection (midpoint): {min(predicts$Sample_midpoint)}, ",
  "latest sample collection (midpoint): {max(predicts$Sample_midpoint)}"
))

## -----------------------------------------------------------------------------
summaries <- predictsr::GetSitelevelSummaries(extract = c(2016, 2022))
str(summaries)

## -----------------------------------------------------------------------------
print(names(predicts)[!(names(predicts) %in% names(summaries))])

## -----------------------------------------------------------------------------
summaries_rep <- predicts |>
  dplyr::mutate(
    Higher_taxa = paste(sort(unique(Higher_taxon)), collapse = ","),
    N_samples = length(Measurement),
    Rescaled_sampling_effort = mean(Rescaled_sampling_effort),
    .by = SSBS
  ) |>
  dplyr::select(
    dplyr::all_of(names(summaries))
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(SSBS)

summaries_copy <- summaries |>
  subset(SSBS %in% summaries_rep$SSBS) |>
  dplyr::arrange(SSBS)

all.equal(summaries_copy, summaries_rep)

## -----------------------------------------------------------------------------
descriptions <- predictsr::GetColumnDescriptions()
str(descriptions)

## ----echo = FALSE, results = 'asis'-------------------------------------------
descriptions_sub <-  subset(
  descriptions,
  Column %in% c(
    "Source_ID", "SS", "Block", "SSBS", "Diversity_metric_type", "Measurement"
  )
)

for (i in seq_along(descriptions_sub$Column)) {
  cat(
    paste0(
      "* `", descriptions_sub$Column[i], "` (`", descriptions_sub$Type[i], "`): "
    )
  )
  notes <- descriptions_sub$Notes[i] |>
    (\(s) gsub("\\*", "  -", s))() |>
    (\(s) gsub("\n\n", "  \n", s))() |>
    (\(s) gsub("\n", "  \n", s))()

  if (notes == " ") {
    notes <- "As title."
  }

  cat(notes)
  cat("  \n")
}

