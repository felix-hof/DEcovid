# Plot a fitted \code{hhh4} model
#
# @param x A fitted \code{hhh4} model.
# @param units Either \code{"all"}, \code{"aggregated"}, or a \code{character} vector indicating which units should be plotted.
# @param meanHHH See \code{\link[surveillance]{plotHHH4_fitted}}.
#
# @return A ggplot object of the fitted model.
#
# @importFrom surveillance meanHHH epoch observed
# @importFrom ISOweek ISOweek2date
# @importFrom dplyr as_tibble filter case_when summarise group_by
# @importFrom purrr map_dfr
# @importFrom tidyr pivot_longer
# @importFrom ggplot2 ggplot aes geom_area geom_point facet_wrap labs scale_x_date
# @importFrom stats terms
#
# @export
#
# plot_fitted <- function(x, units = "all", meanHHH = NULL){
#
#   # check input
#   cond <- all(units %in% c("aggregated", "all", colnames(observed(x$stsObj))))
#   if(!cond){
#     stop("Invalid 'units' argument. Must be either 'aggregated', 'all' or at least one of colnames(observed(x$stsObj)).\n")
#   }
#   if(any(units %in% c("all", "aggregated")) & length(units) != 1L){
#     stop("Invalid 'units' argument. Must be either 'aggregated', 'all' or at least one of colnames(observed(x$stsObj)).\n")
#   }
#
#   # # Add decompose variable
#   # if(decompose) decompose_variable <- colnames(x$stsObj) else decompose_variable <- NULL
#
#   # Get correct meanHHH
#   if (is.null(meanHHH)) {
#     # if(!decompose){
#     meanHHH <- surveillance:::meanHHH(x$coefficients, stats::terms(x))
#     # } else {
#     #   meanHHH <- hhh4addon:::decompose.hhh4lag(x)
#     # }
#   }
#
#   # get the units
#   units_orig <- units
#   if(units == "all"){
#     units <- colnames(x$stsObj)
#   } else {
#     if(units == "aggregated") { units <- "Overall" }
#   }
#
#
#   # get the time of observations
#   t_all <- surveillance::epoch(x$stsObj)
#   t_mod <- surveillance::epoch(x$stsObj)[x$control$subset]
#   if(x$stsObj@freq == 52L){
#     start <- as.Date(ISOweek::ISOweek2date(paste0(x$stsObj@start[1], "-W", x$stsObj@start[2], "-4")))
#     date_seq <- start + (t_all - 1L) * 7
#     t_all <- date_seq
#     t_mod <- date_seq[t_mod]
#   } else if(x$stsObj@freq == 365L){
#     start <- as.Date(paste0(x$stsObj@start[1], "-", x$stsObj@start[2]), format = "%Y-%j")
#     date_seq <- start + (t_all - 1L)
#     t_mod <- date_seq[t_mod]
#     t_all <- date_seq
#   }
#
#   # compute model components
#   model_components <- vapply(list("end", "ar", "ne"),
#                              function(z) x$control[[z]]$inModel,
#                              logical(1L))
#   model_components <- c("endemic", "epi.own", "epi.neighbours")[model_components]
#
#   # construct data frame from meanHHH
#   dat <- purrr::map_dfr(seq_along(meanHHH), function(y){
#     dat <- dplyr::as_tibble(meanHHH[[y]])
#     dat$t <- t_mod
#     dat$component <- names(meanHHH)[y]
#     dat <- tidyr::pivot_longer(dat, cols = !c("t", "component"), names_to = "unit")
#   })
#
#   # Filter the components we are interested in
#   dat <- dplyr::filter(dat, component %in% model_components)
#   dat$component <- dplyr::case_when(dat$component == "epi.neighbours" ~ "spatiotemporal",
#                                     dat$component == "epi.own" ~ "autoregressive",
#                                     TRUE ~ dat$component)
#
#   # Filter units desired or aggregate data
#   if(units_orig[1] != "all"){
#     if(units_orig[1] == "aggregated"){
#       dat$unit <- "Overall"
#       dat <- dplyr::summarise(dplyr::group_by(dat, across(c(-value))), value = sum(value), .groups = "drop")
#     } else {
#       dat <- dplyr::filter(dat, unit %in% units)
#     }
#   }
#
#   # get observations
#   obs <- dplyr::as_tibble(surveillance::observed(x$stsObj))
#   obs$t <- t_all
#   obs <- tidyr::pivot_longer(obs, cols = !t, names_to = "unit")
#   if(units_orig[1] == "aggregated"){
#     obs$unit <- units
#     obs <- dplyr::summarise(dplyr::group_by(obs, t, unit), value = sum(value), .groups = "drop")
#   }
#   if(!(units_orig[1] %in% c("all", "aggregated"))){
#     obs <- dplyr::filter(obs, unit %in% units)
#   }
#
#   # convert component to a factor
#   dat$component <- factor(dat$component, levels = c("spatiotemporal", "autoregressive", "endemic"))
#
#   p <- ggplot2::ggplot(dat, ggplot2::aes(x = t, y = value)) +
#     ggplot2::geom_area(ggplot2::aes(fill = component)) +
#     ggplot2::geom_point(data = obs, ggplot2::aes(x = t, y = value),
#                         size = 0.5, alpha = 0.5) +
#     ggplot2::facet_wrap(~unit, scales = "free_y") +
#     #ggplot2::theme_minimal() +
#     #ggplot2::scale_fill_manual(values = viridisLite::viridis(n = 6, direction = -1,
#     #                                                         option = "B")[c(2, 5)]) +
#     ggplot2::labs(x = "", y = "No. infected", fill = "Component")
#
#   if(inherits(dat$t, "Date")){
#     p <- p + ggplot2::scale_x_date(date_breaks = "2 months", date_labels = "%b\n(%y)")
#   }
#
#   return(p)
# }
