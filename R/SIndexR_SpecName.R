#' @title
#' Returns string containing species name.
#' @description
#' Returns string containing species name.
#' @param sp_index Integer/Numeric, Specifies species index
#' @return
#'    Pointer to string containing species name.
#'    If input parameter is not a valid species index, the return is the
#'    null pointer.
#'
#' @note
#'    Species name string examples: "Coastal Douglas-fir", "Sitka Spruce".
#' @rdname SIndexR_SpecName
SIndexR_SpecName <- function(sp_index){
  if(!(class(sp_index) %in% c("numeric", "integer"))){
    stop("sp_index must be either numeric or integer.")
  } else if (class(sp_index) == "numeric") {
    if(!identical(round(sp_index), sp_index)){
      stop("sp_index must be either numeric or integer.")
    } else {
      sp_index <- as.integer(sp_index)
    }
  }
  return(unlist(lapply(sp_index, function(s) Sindex_SpecName(s))))
}
