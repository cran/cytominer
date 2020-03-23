## ---- echo=FALSE, warning=FALSE, message=FALSE---------------------------
library(magrittr)
futile.logger::flog.threshold(futile.logger::WARN)

## ------------------------------------------------------------------------
fixture <-
  system.file("extdata", "fixture_intensities_shapes.sqlite",
              package = "cytominer")

db <- DBI::dbConnect(RSQLite::SQLite(), fixture)
  

## ------------------------------------------------------------------------
ext_metadata <-
  readr::read_csv(system.file("extdata", "metadata.csv",
                              package = "cytominer")) %>%
  dplyr::rename(g_well = Well)

ext_metadata <- dplyr::copy_to(db, ext_metadata)


## ------------------------------------------------------------------------
intensities <-
  dplyr::tbl(src = db, "view_intensities") %>%
  dplyr::compute()


## ------------------------------------------------------------------------
measurements <-
  intensities %>%
  dplyr::filter(g_well %in% c("A01", "A02", "A10", "A11"))

## ------------------------------------------------------------------------
measurements %>%
  dplyr::tally() %>%
  knitr::kable()

## ------------------------------------------------------------------------
qualities <- c("q_debris")

groupings <-
  c("g_plate",
    "g_well",
    "g_image",
    "g_pattern",
    "g_channel")

variables <-
  colnames(measurements) %>%
  stringr::str_subset("^m_")

measurements %<>%
  dplyr::select(dplyr::one_of(c(groupings, qualities, variables)))


## ------------------------------------------------------------------------
debris_removed <-
  measurements %>% dplyr::filter(q_debris == 0)

## ------------------------------------------------------------------------
na_rows_removed <-
  cytominer::drop_na_rows(
    population = debris_removed,
    variables = variables
  ) %>%
  dplyr::compute()

## ------------------------------------------------------------------------
normalized <-
  cytominer::normalize(
    population = na_rows_removed %>% 
      dplyr::collect(),
    variables = variables,
    strata =  c("g_plate", "g_pattern", "g_channel"),
    sample =
      na_rows_removed %>%
      dplyr::inner_join(
        ext_metadata %>% 
          dplyr::filter(Type == "ctrl") %>% 
          dplyr::select(g_well) 
      ) %>% dplyr::collect()
  )

normalized %<>% dplyr::collect()

## ------------------------------------------------------------------------
na_frequency <-
  cytominer::count_na_rows(
    population = normalized,
    variables = variables)

na_frequency %>%
  tidyr::gather(feature, na_count) %>%
  knitr::kable()


## ------------------------------------------------------------------------

cleaned <-
  cytominer::variable_select(
    population = normalized,
    variables = variables,
    operation = "drop_na_columns"
)

## ------------------------------------------------------------------------
transformed <-
  cytominer::transform(
    population = cleaned,
    variables = variables
  )

## ------------------------------------------------------------------------
aggregated <-
  cytominer::aggregate(
    population = transformed,
    variables = variables,
    strata = groupings
  ) %>%
  dplyr::collect()

variables <-
  colnames(aggregated) %>%
  stringr::str_subset("^m_")


## ------------------------------------------------------------------------
selected <-
  cytominer::variable_select(
    population = transformed,
    variables = variables,
    sample = aggregated,
    operation = "correlation_threshold"
  ) %>%
  dplyr::collect()

## ------------------------------------------------------------------------
selected %>%
  dplyr::glimpse()

## ------------------------------------------------------------------------
  DBI::dbDisconnect(db)

