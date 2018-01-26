#' @title
#' First species defined in Sindex
#' @description
#' Returns a species index for the first species defined in Sindex
#' @return
#' Integer species index, for use in other Sindex functions.
#' @note
#' No assumption should be made about the ordering of the species.
#' @export
#' @rdname SIndexR_FirstSpecies
SIndexR_FirstSpecies <- function(){
  return(Sindex_FirstSpecies())
}
