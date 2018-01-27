#' @title
#' Returns default GI curve index for a species.
#' @description
#' Returns default GI curve index for a species.
#' @param sp_index Integer/Numeric, Specifies species index.
#' @return
#'    Integer curve index, for use in other Sindex functions.
#'    May return an error code under the following conditions:
#'
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_SPEC     input parameter is not a valid species index
#'    SI_ERR_NO_ANS   no GI equations defined for this species.
#' @rdname SIndexR_DefGICurve
SIndexR_DefGICurve <- function(sp_index){
  sp_index <- wholeToInteger(sp_index, "sp_index")
  return(unlist(lapply(sp_index, function(s) Sindex_DefGICurve(s))))
}
