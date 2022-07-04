# Make a parameter table from an \code{hhh4} model
# 
# @description This function takes a fitted \code{hhh4}-model and extracts its estimates and standard errors as well as the parameter
# names. The parameter names are returned in a latex compatible format. 
#
# @param model \code{hhh4} model object
# @param par_names A \code{list}. The elements of the list must be named \code{character} vectors mapping the internal parameter names to 
# names that should actually be displayed in the table. The format requires that the replacement names must be constructed such that the 
# parameter name that should be displayed in the table should be the name of the vector element containing the internal name. Intercepts are automatically
# handled and should not be renamed by the user. Also, the output table respects the order of the list elements such that the parameters in each
# list element will be placed below each other in the output table.
# @param idx2Exp Must be one of \code{TRUE} or \code{FALSE}. Indicates whether covariates should be given on the exp scale.
# @param reparamPsi Must be one of \code{TRUE} or \code{FALSE}. Indicates whether overdispersion parameters should be reparametrised.
# @param amplitudeShift Must be one of \code{TRUE} or \code{FALSE}. Indicates whether seasonality coefficients should be reparametrised.
# @param show_intercept Must be one of \code{TRUE} or \code{FALSE}. Indicates whether intercepts should be shown in the output table.
#
# @return A \code{tibble} containing parameter estimates and standard errors. Can be passed to \code{\link[kableExtra]{kable}}
# @importFrom dplyr case_when mutate tibble left_join select %>%
# @importFrom hhh4addon poisson_lag
#
# @export
#
# parameter_table <- function(model, par_names = NULL, idx2Exp = FALSE, reparamPsi = TRUE, amplitudeShift = TRUE,
#                             show_intercept = TRUE){
#   
#   stopifnot(length(idx2Exp) == 1L && length(reparamPsi) == 1L && length(amplitudeShift) == 1L,
#             is.logical(idx2Exp) && is.logical(reparamPsi) && is.logical(amplitudeShift))
#   
#   # get parameters
#   if(idx2Exp){
#     pars <- summary(model, amplitudeShift = amplitudeShift, idx2Exp = TRUE, reparamPsi = reparamPsi)[["fixef"]]
#   } else {
#     pars <- summary(model, amplitudeShift = amplitudeShift, reparamPsi = reparamPsi)[["fixef"]]
#   }
#   
#   # make regular expressions and get lag parameter
#   regex_comp <- paste0(ifelse(idx2Exp, "^exp\\(", "^"), c("end\\.", "ar\\.", "ne\\."))
#   regex_comp <- c(regex_comp, 
#                   paste0(ifelse(reparamPsi, "^", "^-log\\("), "overdisp\\.?"),
#                   "^neweights\\.")
#   if(inherits(model, "hhh4_lag")){
#     if(identical(model$control$funct_lag, hhh4addon::poisson_lag)){
#       lag_par <- dplyr::tibble(par = "lag.kappa", est = model$par_lag, sderr = model$se_par_lag, comp = "lag", trans = "log")
#     } else {
#       stop("Unrecognised lag function.")
#     }
#   } else {
#     lag_par <- NULL
#   }
#   
#   # construct table
#   model_components <- c("endemic", "autoregressive", "spatiotemporal")
#   dat <- dplyr::tibble(par = rownames(pars),
#                        est = pars[, "Estimate"],
#                        sderr = pars[, "Std. Error"])
#   dat <- dplyr::mutate(dat, comp = dplyr::case_when(grepl(regex_comp[1], par) ~ "endemic",
#                                                     grepl(regex_comp[2], par) ~ "autoregressive",
#                                                     grepl(regex_comp[3], par) ~ "spatiotemporal",
#                                                     grepl(regex_comp[4], par) ~ "overdispersion",
#                                                     grepl(regex_comp[5], par) ~ "power law"))
#   dat <- dplyr::mutate(dat, 
#                        trans = dplyr::case_when(comp %in% model_components & idx2Exp ~ "exp",
#                                                 comp == "overdispersion" & reparamPsi ~ "",
#                                                 comp == "overdispersion" & !reparamPsi ~ "-log",
#                                                 TRUE ~ ""))
#   # add lag parameter
#   if(!is.null(lag_par)) dat <- dplyr::add_row(dat, lag_par)
#   
#   # get parameter names
#   dat$name <- gsub("^.+?\\.", "", dat$par)
#   dat$name <- gsub("^1\\.", "", dat$name)
#   dat$name[grepl("^A\\(.+\\)$", dat$name)] <- "Amplitude"
#   dat$name[grepl("^s\\(.+\\)$", dat$name)] <- "Phase"
#   if(any(grepl("^(sin|cos)\\(.+\\)$", dat$name))) stop("amplitudeShift = FALSE is not implemented.")
#   dat$is_in_par_list <- dat$name %in% do.call(`c`, par_names)
#   dat_split <- lapply(seq_along(par_names), function(x){
#     subset <- dplyr::filter(dat, name %in% par_names[[x]])
#     subset <- dplyr::left_join(x = subset, y = dplyr::tibble(new_name = names(par_names[[x]]), name = par_names[[x]]), by = "name")
#     subset$category <- names(par_names)[x]
#     subset
#   })
#   not_in_par_list <- dat[!dat$is_in_par_list, ]
#   not_in_par_list$new_name <- not_in_par_list$name
#   not_in_par_list$category <- "other"
#   dat <- do.call(rbind, append(dat_split, list(not_in_par_list)))
#   dat$name <- dat$new_name
#   dat <- dplyr::select(dat, -new_name)
#   dat$name[dat$name == "1"] <- "Intercept"
#   n_overdisp <- sum(dat$comp == "overdispersion")
#   dat$is_intercept <- grepl("^(end|ar|ne)\\.1\\.?.*$", dat$par)
#   dat$category[dat$is_intercept] <- "Intercept"
#   if(!show_intercept) dat <- dat[!dat$is_intercept, ]
#   
#   # construct latex
#   dat <- dplyr::mutate(dat, latex = paste0(
#     "$",
#     ifelse(trans != "", paste0("\\text{", trans, "} \\, "), ""),
#     dplyr::case_when(comp %in% model_components & !is_intercept ~ "\\beta",
#                      comp %in% model_components & is_intercept ~ "\\alpha",
#                      comp == "overdispersion" ~ "\\psi",
#                      comp == "lag" ~ paste0("\\", name),
#                      comp == "power law" ~ name),
#     #ifelse(comp != "lag", "_{", ""),
#     dplyr::case_when(comp == model_components[1] ~ "^{(\\nu)}",
#                      comp == model_components[2] ~ "^{(\\lambda)}",
#                      comp == model_components[3] ~ "^{(\\phi)}",
#                      comp == "overdispersion" & name != "overdisp" ~ name,
#                      TRUE ~ ""),
#     dplyr::case_when(#comp == "endemic" & grepl("^\\d", name) ~ paste0(" ", name),
#       #comp == "endemic" & !grepl("^\\d", name) ~ paste0("_{\\text{", name, "}}"),
#       comp == model_components[1] & name != "Intercept" ~ paste0("_{\\text{", name, "}}"),
#       #comp == "autoregressive" & grepl("^\\d", name) ~ paste0(" ", name),
#       #comp == "autoregressive" & !grepl("^\\d", name) ~ paste0("_{\\text{", name, "}}"),
#       comp == model_components[2] & name != "Intercept" ~ paste0("_{\\text{", name, "}}"),
#       #comp == "spatiotemporal" & grepl("^\\d", name) ~ paste0(" ", name),
#       #comp == "spatiotemporal" & !grepl("^\\d", name) ~ paste0("_{\\text{", name, "}}"),
#       comp == model_components[3] & name != "Intercept" ~ paste0("_{\\text{", name, "}}"),
#       #comp == "overdispersion" ~ "}",
#       TRUE ~ ""),
#     #ifelse(comp %in% model_components, "}", ""),
#     "$"
#   ))
#   
#   # split table and align everything
#   ## loop over components and categories
#   dat$comp[!(dat$comp %in% model_components)] <- "other"
#   categories <- unique(dat$category)
#   # Move intercept to top
#   categories <- c(categories[categories == "Intercept"], categories[categories != "Intercept"])
#   # Get components
#   components <- unique(dat$comp)
#   components <- c(model_components, "other")[c(model_components, "other") %in% components]
#   # set some kind of order for the parameters in other such that it can be appended
#   # to par_names list
#   other_intercept <- unique(with(dat, name[category == "Intercept"]))
#   names(other_intercept) <- other_intercept
#   other_levels <- unique(with(dat, name[category == "other" & !is_intercept]))
#   # other_levels <- unique(with(dplyr::arrange(dat, dplyr::desc(is_intercept)), name[category == "other"]))
#   if(all(c("Amplitude", "Phase") %in% other_levels)){
#     other_levels <- c("Amplitude", "Phase", other_levels[!(other_levels %in% c("Amplitude", "Phase"))])
#   }
#   names(other_levels) <- other_levels
#   par_names <- append(par_names, list("Intercept" = other_intercept), after = 0L)
#   par_names <- append(par_names, list("other" = other_levels))
#   
#   # Complete the table
#   tab <- lapply(components, function(x){
#     # get parameters of every component
#     comp_subs <- dat[dat$comp == x, ]
#     # Complete groups
#     out <- lapply(categories, function(y){
#       cat_subs <- comp_subs[comp_subs$category == y, ]
#       all_levels <- dplyr::tibble(name = names(par_names[[y]]))
#       out <- dplyr::left_join(all_levels, cat_subs, by = "name")
#       out <- out[, c("latex", "est", "sderr")]
#       colnames(out) <- c("Coefficient", "Estimate", "Std. Error")
#       out
#     })
#     do.call(`rbind`, out)
#   })
#   names(tab) <- components
#   
#   # cbind columns
#   tab <- do.call(`cbind`, tab)
#   colnames(tab) <- gsub("^.*?\\.", "", colnames(tab))
#   # return
#   return(tab)
# }