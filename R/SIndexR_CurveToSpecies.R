#' @title
#' Convert curve index to species index
#' @description
#' Returns species index for a given curve index.
#' @param cu_index Integer/Numeric, Specifies cuive index.
#' @return
#'    Integer species index, for use in other Sindex functions.
#'    May return an error code under the following conditions:
#'
#'    return value    condition
#'
#'    ------------    ---------
#'
#'    SI_ERR_CURVE    input curve is not a valid curve index for any species.
#' @rdname SIndexR_CurveToSpecies
SIndexR_CurveToSpecies <- function(cu_index){
  if(!(class(cu_index) %in% c("numeric", "integer"))){
    stop("cu_index must be either numeric or integer.")
  } else if (class(cu_index) == "numeric") {
    if(!identical(round(cu_index), cu_index)){
      stop("cu_index must be either numeric or integer.")
    } else {
      cu_index <- as.integer(cu_index)
    }
  }
  return(unlist(lapply(cu_index, function(s) Sindex_CurveToSpecies(s))))
}
