#' Get urbanicity index by NUTS-3 level
#'
#' @template cache_dir
#'
#' @return A \code{tibble} with columns \code{lvl3} and \code{value}. Column \code{lvl3} contains NUTS 3 districts,
#' whereas column \code{value} contains the log-population of the largest city within the respective district.
#' @export
#'
#' @examples
#' urbanicity <- get_urbanicity()
get_urbanicity <- function(cache_dir = NULL){

  # set parameters for cacheing
  filename <- "urbanicity.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 60, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    dat <- get_urbanicity_from_source(cache_dir, filename)
  }

  return(dat)
}

#' Get urbanicity index from Destatis
#'
#' @template cache_dir
#' @param  filename The name of the file where population data is stored.
#'
#' @return A \code{tibble} with columns \code{lvl3} and \code{value}. Column \code{lvl3} contains NUTS 3 districts,
#' whereas column \code{value} contains the log-population of the largest city within the respective district.
#'
#' @importFrom utils download.file
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter mutate left_join select group_by summarise
#'
get_urbanicity_from_source <- function(cache_dir, filename){

  # get data
  nuts_table <- nuts_table(cache_dir)
  # Download excel file
  "https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/GVAuszugJ/31122019_Auszug_GV.xlsx?__blob=publicationFile" %>%
    utils::download.file(destfile = make_path(cache_dir, "31122019_Auszug_GV.xlsx"), quiet = TRUE)

  dat <- readxl::read_xlsx(make_path(cache_dir, "31122019_Auszug_GV.xlsx"),
                           sheet = "Onlineprodukt_Gemeinden_311219",
                           trim_ws = TRUE,
                           range = "A7:J20000",
                           col_names = c("Satzart", "Textkennzeichen", "Land",
                                         "RB", "Kreis", "VB", "Gem", "Gemeindename",
                                         "Flaeche", "Bevoelkerung"),
                           col_types = c(rep("text", 8), rep("numeric", 2))) %>%
    dplyr::filter(!is.na(Bevoelkerung)) %>%
    dplyr::mutate(adm_unit = paste0(Land, RB, Kreis),
                  adm_unit = ifelse(Gemeindename == "Eisenach, Stadt", "16063", adm_unit)) %>%
    dplyr::left_join(y = nuts_table, by = "adm_unit") %>%
    dplyr::select(Gemeindename, Bevoelkerung, lvl3) %>%
    dplyr::group_by(lvl3) %>%
    dplyr::summarise(City = Gemeindename[which.max(Bevoelkerung)],
                     population = Bevoelkerung[which.max(Bevoelkerung)],
                     .groups = "drop") %>%
    dplyr::filter(population != 0) %>%
    dplyr::mutate(value = log(population)) %>%
    dplyr::select(lvl3, value) %>%
    dplyr::arrange(lvl3)

  # save this
  saveRDS(dat, file = make_path(cache_dir, filename))

  return(dat)
}
