#' @title 
#'     Calculates the number of years a stand takes to grow from seed to
#'     breast height.
#' @description 
#'     Calculates the number of years a stand takes to grow from seed to
#'     breast height, in steps ending in 0.5 (i.e. 0.5, 1.5. 2.5, etc.)
#' @param curve Integer, The particular site index curve to project the height and age along.
#' @param siteIndex Numeric, The site index value of the stand.
#' @return \code{output} contains calculated year to breast height
#'         \code{error} contains error information, i.e.,
#'         0, or an error code under the following conditions:
#'    
#'         return value    condition
#'         ------------    ---------
#'         SI_ERR_CURVE    input curve is not a valid curve index.
#'         SI_ERR_GI_TOT   if GI curve
#'         SI_ERR_LT13     if site index <= 1.3
#' @docType methods
#' @rdname Sindex_Y2BH05          
setGeneric("Sindex_Y2BH05",
           function(curve,
                    siteIndex){standardGeneric("Sindex_Y2BH05")})
#' @rdname Sindex_Y2BH05
setMethod("Sindex_Y2BH05",
          signature = c(curve = "integer",
                        siteIndex = "numeric"),
          definition = function(curve,
                                siteIndex){
            
            y2bh <- si_y2bh05(curve, siteIndex)
            error <- y2bh
            error[error > 0] <- 0
            return(list(output = y2bh,
                        error = error))          
          })

#' @export
#' @rdname Sindex_Y2BH05
setMethod("Sindex_Y2BH05",
          signature = c(curve = "numeric",
                        siteIndex = "numeric"),
          definition = function(curve,
                                siteIndex){
            return(Sindex_Y2BH05(curve = as.integer(curve), siteIndex))          
          })
