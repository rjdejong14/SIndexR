#' @title
#'    Get site index based on site class.
#' @description
#'    Get site index based on site class.
#' @param sp_index Integer/Numeric, Species index.
#' @param sitecl character, Site class, must be one of \code{G}, \code{M}, \code{P} and \code{L}.
#' @param fiz character, Forest inventory zone: (A,B,C)=coast, (D,E,F,G,H,I,J,K,L)=interior.
#' @return \code{output} contains site index;
#'         \code{error} contains error information, i.e.,
#'    0, or an error code under the following conditions:
#'
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_SPEC     source species index is not valid, or no conversion
#'    SI_ERR_CLASS    if site class is unknown
#'    SI_ERR_FIZ      if FIZ code is unknown
#' @import data.table data.table
#' @rdname SIndexR_SCToSI
SIndexR_SCToSI <- function(sp_index,
                           sitecl,
                           fiz){
  sp_index <- wholeToInteger(sp_index, "sp_index")
  inputdata <- data.table::data.table(sp_index, sitecl, fiz)
  rm(sp_index, sitecl, fiz)
  sp_index_list <- as.list(inputdata$sp_index)
  sitecl_list <- as.list(inputdata$sitecl)
  fiz_list <- as.list(inputdata$fiz)
  inputdata_list <- Map(list, sp_index_list, sitecl_list, fiz_list)

  site <- unlist(lapply(inputdata_list, function(s) class_to_index(sp_index = s[[1]],
                                                                   sitecl = s[[2]],
                                                                   fiz = s[[3]])))

  error <- site
  error[error > 0] <- 0
  return(list(output = site,
              error = error))
}
