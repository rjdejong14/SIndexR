#' @title
#' Returns a code telling where a species generally exists.
#' @description
#' Returns a code telling where a species generally exists.
#' @param sp_index Integer/Numeric, Specifies species index.
#' @return
#' Integer code.
#'    May return an error code under the following conditions:
#'
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_SPEC     input parameter is not a valid species index
#'
#' @note
#'    Code bits are set as follows:
#'         1: BC coast
#'        10: BC interior
#'       100: common species in BC (0 means uncommon)
#' @rdname SIndexR_SpecUse
SIndexR_SpecUse <- function(sp_index){
  if(!(class(sp_index) %in% c("numeric", "integer"))){
    stop("sp_index must be either numeric or integer.")
  } else if (class(sp_index) == "numeric") {
    if(round(sp_index) != sp_index){
    stop("sp_index must be either numeric or integer.")
    }
  }
  return(unlist(lapply(sp_index, function(s) Sindex_SpecUse(s))))
}
