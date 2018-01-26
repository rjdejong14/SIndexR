#' @title 
#'    Get site index based on site class.
#' @description 
#'    Get site index based on site class.
#' @param sp_index Integer, Species index.
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
#' @export
#' @docType methods
#' @rdname Sindex_SCToSI
setGeneric("Sindex_SCToSI",
           function(sp_index,
                    sitecl,
                    fiz){standardGeneric("Sindex_SCToSI")})

#' @rdname Sindex_SCToSI
setMethod("Sindex_SCToSI",
          signature = c(sp_index = "integer",
                        sitecl = "character",
                        fiz = "character"),
          definition = function(sp_index,
                                sitecl,
                                fiz){
            
            site <- class_to_index (sp_index, sitecl, fiz)
            error <- site
            error[error > 0] <- 0
            return(list(output = site,
                        error = error))          
          })

#' @export
#' @rdname Sindex_SCToSI
setMethod("Sindex_SCToSI",
          signature = c(sp_index = "numeric",
                        sitecl = "character",
                        fiz = "character"),
          definition = function(sp_index,
                                sitecl,
                                fiz){
            return(Sindex_SCToSI(sp_index = as.integer(sp_index),
                                 sitecl, fiz))          
          })