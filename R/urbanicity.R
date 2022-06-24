#' Get urbanicity index from \insertCite{DEcovid:Urbanicity}{DEcovid}.
#'
#' @template time_res
#' @template spat_res
#' @template age_res
#' @template cache_dir
#' @template enforce_cache
#'
#' @return A \code{tibble} with columns \code{region} and \code{value}. Column \code{region} contains NUTS districts,
#' whereas column \code{value} contains the log-population of the largest city within the respective district. By setting arguments
#' \code{time_res}, \code{spat_res}, and \code{age_res}, the \code{tibble} is expanded to the specified dimensions.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join
#' @importFrom tidyr expand_grid
#'
#' @export
#' 
#' @references 
#' \insertRef{DEcovid:Urbanicity}{DEcovid}
#'
#' @examples
#' urbanicity <- get_urbanicity()
get_urbanicity <- function(time_res = NULL,
                           spat_res = NULL,
                           age_res = NULL,
                           cache_dir = NULL,
                           enforce_cache = FALSE){

  # Check inputs
  join <- check_res_args(time_res = time_res,
                         spat_res = spat_res,
                         age_res = age_res)
  check_enforce_cache(enforce_cache = enforce_cache)

  # set parameters for cacheing
  filename <- "urbanicity.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 60, units = "days")

  # get pre-processed data from file or from source
  if(enforce_cache){
    if(!file.exists(make_path(cache_dir, filename))){
      stop("There is no cached version of the requested data in 'cache_dir' directory.")
    } else {
      dat <- readRDS(make_path(cache_dir, filename))
    }
  } else {
    if(from_cache){
      dat <- readRDS(make_path(cache_dir, filename))
    } else {
      dat <- get_urbanicity_from_source(cache_dir, filename)
    }
  }

  # aggregate if desired
  if(join){
    dims <- get_case_info(spat_res = 3, time_res = "daily", cache_dir = cache_dir)
    region <- dims$region
    age <- dims$age
    date <- dims$date
    dat <- tidyr::expand_grid(age = age, date = date, region = region) %>%
      dplyr::left_join(y = dat, by = "region") %>%
      summarise_data(time_res = time_res, spat_res = spat_res, age_res = age_res,
                     time_f = time_f_urbanicity, spat_f = spat_f_urbanicity, age_f = age_f_urbanicity)
  }

  return(dat)
}

#' Get urbanicity index from Destatis
#'
#' @template cache_dir
#' @param  filename The name of the file where population data is stored.
#'
#' @return A \code{tibble} with columns \code{region} and \code{value}. Column \code{region} contains NUTS 3 districts,
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
    dplyr::arrange(lvl3) %>%
    dplyr::rename(region = lvl3)

  # save this
  saveRDS(dat, file = make_path(cache_dir, filename))

  return(dat)
}
