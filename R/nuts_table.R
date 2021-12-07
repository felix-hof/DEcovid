#' Get the NUTS table used to map German national statistical units to NUTS regions
#'
#' @template cache_dir
#'
#' @return A \code{tibble} mapping German national administrative to the NUTS classification.
#' @export
#'
#' @examples
#' nts <- nuts_table()
#'
nuts_table <- function(cache_dir = NULL){

  # set parameters for cacheing
  filename <- "nuts_table.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 60, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    dat <- get_nuts_table_from_source(cache_dir = cache_dir, filename = filename)
  }

  return(dat)
}


#' Get the nuts table from source
#'
#' @param filename The filename under which the table should be stored.
#' @template  cache_dir
#'
#' @return A \code{tibble} mapping German national administrative to the NUTS classification.
#'
#' @importFrom httr GET content
#' @importFrom magrittr %$% %>%
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate select add_row filter arrange rename left_join
get_nuts_table_from_source <- function(filename, cache_dir){

  lk2nuts <- "https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_Landkreisdaten/FeatureServer/0/query?where=1%3D1&outFields=NUTS,RS,county&returnGeometry=false&f=json" %>%
    httr::GET() %>%
    httr::content("parsed") %$%
    features %>%
    unlist(recursive = FALSE) %>%
    purrr::map_dfr(function(x) {c(adm_unit = x$RS, lvl3 = x$NUTS, LK = x$county)} ) %>%
    dplyr::mutate(lvl3 = dplyr::if_else(grepl("Berlin", LK), "DE300", lvl3),
    ) %>%
    dplyr::select(LK, adm_unit, lvl3) %>%
    dplyr::add_row(LK = "SK Berlin", adm_unit = "11000", lvl3 = "DE300")

  # check that there are no NAs
  stopifnot(!any(is.na(lk2nuts$adm_unit)),
            !any(is.na(lk2nuts$lvl3)),
            !any(is.na(lk2nuts$LK)))

  # create tables for nuts levels
  nuts <- "https://gisco-services.ec.europa.eu/distribution/v2/nuts/csv/NUTS_AT_2021.csv" %>%
    httr::GET() %>%
    httr::content(as = "parsed", encoding = "UTF-8",
                  col_types = "-cc----", progress = FALSE, trim_ws = TRUE) %>%
    dplyr::filter(grepl("^DE", NUTS_ID)) %>%
    {
      nuts_lvl3 <- .[] %>%
        dplyr::filter(nchar(NUTS_ID) == 5) %>%
        dplyr::arrange(NUTS_ID) %>%
        dplyr::mutate(lvl2 = substr(NUTS_ID, 1, 4)) %>%
        dplyr::rename(lvl3_name = NAME_LATN, lvl3 = NUTS_ID)
      nuts_lvl2 <- .[] %>%
        dplyr::filter(nchar(NUTS_ID) == 4) %>%
        dplyr::rename(lvl2 = NUTS_ID, lvl2_name = NAME_LATN) %>%
        dplyr::mutate(lvl1 = substr(lvl2, 1, 3))
      nuts_lvl1 <- .[] %>%
        dplyr::filter(nchar(NUTS_ID) == 3) %>%
        dplyr::rename(lvl1 = NUTS_ID, lvl1_name = NAME_LATN) %>%
        dplyr::mutate(lvl0 = substr(lvl1, 1, 2))
      nuts_lvl0 <- .[] %>%
        dplyr::filter(nchar(NUTS_ID) == 2) %>%
        dplyr::rename(lvl0 = NUTS_ID, lvl0_name = NAME_LATN)
      nuts_lvl3 %>%
        dplyr::left_join(., nuts_lvl2, by = "lvl2") %>%
        dplyr::left_join(., nuts_lvl1, by = "lvl1") %>%
        dplyr::left_join(., nuts_lvl0, by = "lvl0") %>%
        dplyr::arrange(lvl3) %>%
        # Remove DEG0N as German Authorities apparently do not work with it
        dplyr::filter(lvl3 != "DEG0N")
    }

  # Check for non-used regions
  if(length(setdiff(lk2nuts$lvl3, nuts$lvl3)) != 0){
    stop("NUTS 3 regions in German case data detected that do not appear in Eurostat data:\n",
         paste0(setdiff(lk2nuts$lvl3, nuts$lvl3), collapse = "\n"))
  }
  if(length(setdiff(nuts$lvl3, lk2nuts$lvl3)) != 0){
    stop("NUTS 3 regions in Eurostat data detected that do not appear in German case data:\n",
         paste0(setdiff(nuts$lvl3, lk2nuts$lvl3), collapse = "\n"))
  }

  # join German administrative unit IDs and NUTS table
  lk_info <- lk2nuts %>%
    dplyr::left_join(., nuts, by = "lvl3")

  saveRDS(lk_info, file = file.path(cache_dir, filename))
  return(lk_info)
}
