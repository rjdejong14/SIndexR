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
  if(!(class(sp_index) %in% c("numeric", "integer"))){
    stop("sp_index must be either numeric or integer.")
  } else if (class(sp_index) == "numeric") {
    if(identical(sp_index, round(sp_index))){
      sp_index <- as.integer(sp_index)
    } else {
      stop("sp_index must be either numeric or integer.")
    }
  }
  return(unlist(lapply(sp_index, function(s) Sindex_DefGICurve(s))))
}
