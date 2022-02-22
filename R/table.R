#' Make a table from an \code{hhh4} model object
#'
#' @param model \code{hhh4} model object
#' @param par_names A named \code{character} vector which is used to map variable names to the actual names for the table.
#' @param idx2Exp Must be one of \code{TRUE} or \code{FALSE}. Indicates whether covariates should be given on the exp scale.
#' @param reparamPsi Must be one of \code{TRUE} or \code{FALSE}. Indicates whether overdispersion parameters should be reparametrised.
#' @param amplitudeShift Must be one of \code{TRUE} or \code{FALSE}. Indicates whether seasonality coefficients should be reparametrised.
#'
#' @return A \code{tibble} containing parameter estimates and standard errors. Can be passed to \code{\link[kableExtra]{kable}}
#' @importFrom dplyr case_when mutate tibble left_join select arrange bind_rows full_join %>% slice
#'
#' @export
#'
parameter_table <- function(model, par_names, idx2Exp = FALSE, reparamPsi = TRUE, amplitudeShift = TRUE){

  # get parameters
  if(idx2Exp){
    pars <- summary(model, amplitudeShift = amplitudeShift, idx2Exp = TRUE, reparamPsi = reparamPsi)[["fixef"]]
  } else {
    pars <- summary(model, amplitudeShift = amplitudeShift, reparamPsi = reparamPsi)[["fixef"]]
  }

  # make regular expressions and get lag parameter
  regex_comp <- paste0(ifelse(idx2Exp, "^exp\\(", "^"), c("end\\.", "ar\\.", "ne\\."))
  regex_comp <- c(regex_comp, paste0(ifelse(reparamPsi, "^", "^-log\\("), "overdisp\\.*"))

  # construct table
  dat <- dplyr::tibble(par = rownames(pars),
                       est = pars[, "Estimate"],
                       sderr = pars[, "Std. Error"])
  dat <- dplyr::mutate(dat, comp = dplyr::case_when(grepl(regex_comp[1], par) ~ "endemic",
                                                    grepl(regex_comp[2], par) ~ "autoregressive",
                                                    grepl(regex_comp[3], par) ~ "spatiotemporal",
                                                    grepl(regex_comp[4], par) ~ "overdispersion"))
  dat <- dplyr::mutate(dat,
                       trans = dplyr::case_when(comp %in% c("endemic", "autoregressive", "spatiotemporal") & idx2Exp ~ "exp",
                                                comp == "overdispersion" & reparamPsi ~ "",
                                                comp == "overdispersion" & !reparamPsi ~ "-log",
                                                TRUE ~ ""))
  # get parameter names
  dat$name <- gsub("^.+\\.(.+)$", "\\1", dat$par)
  dat$name[grepl("^A\\(.+\\)$", dat$name)] <- "amplitude"
  dat$name[grepl("^s\\(.+\\)$", dat$name)] <- "phase"
  if(any(grepl("^(sin|cos)\\(.+\\)$", dat$name))) stop("amplitudeShift = FALSE is not implemented.")
  dat <- dplyr::left_join(x = dat, y = dplyr::tibble(new_name = names(par_names),
                                                     name = par_names), by = "name")
  dat <- dplyr::mutate(dat, name = ifelse(is.na(new_name), name, new_name))
  dat <- dplyr::select(dat, -new_name)
  dat$name[dat$name == "1"] <- "intercept"
  n_overdisp <- sum(dat$comp == "overdispersion")

  # construct latex
  dat <- dplyr::mutate(dat, latex = paste0(
    "$",
    ifelse(trans != "", paste0("\\text{", trans, "} \\, "), ""),
    dplyr::case_when(comp %in% c("endemic", "autoregressive", "spatiotemporal") ~ "\\beta",
                     comp == "overdispersion" ~ "\\psi",
                     comp == "lag" ~ paste0("\\", name)),
    ifelse(!(comp %in% c("lag", "overdispersion")) | (comp == "overdispersion" & n_overdisp > 1L), "_{", ""),
    dplyr::case_when(comp == "endemic" ~ "\\nu",
                     comp == "autoregressive" ~ "\\lambda",
                     comp == "spatiotemporal" ~ "\\phi",
                     comp == "overdispersion"  & n_overdisp != 1L ~ name,
                     TRUE ~ ""),
    dplyr::case_when(#comp == "endemic" & grepl("^\\d", name) ~ paste0(" ", name),
      #comp == "endemic" & !grepl("^\\d", name) ~ paste0("_{\\text{", name, "}}"),
      comp == "endemic" ~ paste0("_{\\text{", name, "}}"),
      #comp == "autoregressive" & grepl("^\\d", name) ~ paste0(" ", name),
      #comp == "autoregressive" & !grepl("^\\d", name) ~ paste0("_{\\text{", name, "}}"),
      comp == "autoregressive" ~ paste0("_{\\text{", name, "}}"),
      #comp == "spatiotemporal" & grepl("^\\d", name) ~ paste0(" ", name),
      #comp == "spatiotemporal" & !grepl("^\\d", name) ~ paste0("_{\\text{", name, "}}"),
      comp == "spatiotemporal" ~ paste0("_{\\text{", name, "}}"),
      comp == "overdispersion"  & n_overdisp != 1L ~ "}",
      TRUE ~ ""),
    ifelse(comp %in% c("endemic", "autoregressive", "spatiotemporal"), "}", ""),
    "$"
  ))

  # split table and align everything
  in_model <- c(vapply(list("end", "ar", "ne"), function(x) model$control[[x]]$inModel, logical(1L)), TRUE)
  dat <- dplyr::select(dat, -par, -trans)
  dat <- dplyr::arrange(dat, name)
  if(any(dat$name == "amplitude")){
    amp_end_row <- dat %>% dplyr::slice(which(name == "amplitude" & comp == "endemic")) %>% nrow
    if(amp_end_row != 0L){
      idx <- which(dat$name == "amplitude" & dat$comp == "endemic")
      amp_row <- dat %>% dplyr::slice(idx)
      shf_idx <- which(dat$name == "phase" & dat$comp == "endemic")
      dat <- dplyr::bind_rows(dat[seq_len(shf_idx)[-c(idx, shf_idx)], ], amp_row, dat[shf_idx:nrow(dat), ])
    }
    amp_epi_row <- dat %>% dplyr::slice(which(name == "amplitude" & comp == "epidemic")) %>% nrow
    if(amp_epi_row != 0L){
      idx <- which(dat$name == "amplitude" & dat$comp == "epidemic")
      amp_row <- dat %>% dplyr::slice(idx)
      shf_idx <- which(dat$name == "phase" & dat$comp == "epidemic")
      dat <- dplyr::bind_rows(dat[seq_len(shf_idx)[-c(idx, shf_idx)], ], amp_row, dat[shf_idx:nrow(dat), ])
    }
    amp_ar_row <- dat %>% dplyr::slice(which(name == "amplitude" & comp == "autoregressive")) %>% nrow
    if(amp_ar_row != 0L){
      idx <- which(dat$name == "amplitude" & dat$comp == "autoregressive")
      amp_row <- dat %>% dplyr::slice(idx)
      shf_idx <- which(dat$name == "phase" & dat$comp == "autoregressive")
      dat <- dplyr::bind_rows(dat[seq_len(shf_idx)[-c(idx, shf_idx)], ], amp_row, dat[shf_idx:nrow(dat), ])
    }
  }
  dat <- dplyr::select(dat, latex, est, sderr, name, comp)
  end <- dat[dat$comp == "endemic", colnames(dat) != "comp"]
  colnames(end)[grepl("^(est|sderr|latex)", colnames(end))] <- paste0(colnames(end)[grepl("^(est|sderr|latex)", colnames(end))], ".end")
  ar <- dat[dat$comp == "autoregressive", colnames(dat) != "comp"]
  colnames(ar)[grepl("^(est|sderr|latex)", colnames(ar))] <- paste0(colnames(ar)[grepl("^(est|sderr|latex)", colnames(ar))], ".ar")
  ne <- dat[dat$comp == "spatiotemporal", colnames(dat) != "comp"]
  colnames(ne)[grepl("^(est|sderr|latex)", colnames(ne))] <- paste0(colnames(ne)[grepl("^(est|sderr|latex)", colnames(ne))], ".ne")
  rest <- dat[!(dat$comp %in% c("endemic", "autoregressive", "spatiotemporal")), colnames(dat) != "comp"]
  colnames(rest)[grepl("^(est|sderr|latex)", colnames(rest))] <- paste0(colnames(rest)[grepl("^(est|sderr|latex)", colnames(rest))], ".rest")
  out <- dplyr::full_join(dplyr::full_join(dplyr::full_join(end, ar, "name"), ne, "name"), rest, "name")
  out <- dplyr::select(out, seq_along(out)[vapply(out, function(x){!all(is.na(x))}, logical(1L))])
  out <- dplyr::select(out, -name)
  colnames(out) <- rep(c("Coefficient", "Estimate", "Std. Error"), times = ncol(out)/2)

  # return
  return(out)
}
