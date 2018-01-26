#' @title 
#' Calculates site index based on height, age and site index curve.
#' @description Converts a Height and Age to a Site Index for a particular Site Index
#'              Curve.
#' @param curve Integer, Specifies site index curve. 
#'                       The particular site index curve to project the height and age along.
#' @param age numeric, Tree age.The age of the trees indicated by the curve selection.  The
#'                     interpretation of this age is modified by the 'ageType' parameter.
#' @param ageType Integer, Defines age type. Must be one of:
#'                \code{SI_AT_TOTAL}, the age is the total age of the stand in years since
#'                planting; \code{SI_AT_BREAST}, the age indicates the number of years since the stand
#'                reached breast height.
#' @param height numeric, The height of the species in metres.
#' @param estType Integer, Defines estimate type. Must be one of:
#'      v             \code{SI_EST_DIRECT}, compute the site index based on direct equations
#'                    if available.  If the equations are not available,
#'                    then automatically fall to the \code{SI_EST_ITERATE}
#'                    method; \code{SI_EST_ITERATE}, compute the site index based on an iterative
#'                    method which converges on the true site index.
#' @return \code{output} contains computed site index.
#'         \code{error} contains error values.
#'         
#'      If an error condition occurs, the site index is set to the
#'        same as the returned site index value.
#'
#'  Return Value
#'  ------------
#'    0, or an error code under the following conditions:
#'    
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_CURVE    input curve is not a valid curve index.
#'    SI_ERR_GI_MIN   if bhage < 0.5
#'    SI_ERR_GI_MAX   if bhage > GI range
#'    SI_ERR_NO_ANS   if computed SI > 999
#'    SI_ERR_GI_TOT   if total age and GI curve
#' @docType methods
#' @export
#' @rdname Sindex_HtAgeToSI
#'  
setGeneric("Sindex_HtAgeToSI",
           function(curve,
             age,
             ageType,
             height,
             estType){standardGeneric("Sindex_HtAgeToSI")})

#' @rdname Sindex_HtAgeToSI
setMethod("Sindex_HtAgeToSI",
          signature = c(curve = "integer",
                        age = "numeric",
                        ageType = "integer",
                        height = "numeric",
                        estType = "integer"),
          definition = function(curve,
                                age,
                                ageType,
                                height,
                                estType){
            site <- height_to_index (curve, age, ageType, height, estType)
            error <- site
            error[error >= 0] <- 0
            return(list(output = site,
                   error = error))
          })

#' @export
#' @rdname Sindex_HtAgeToSI
setMethod("Sindex_HtAgeToSI",
          signature = c(curve = "numeric",
                        age = "numeric",
                        ageType = "numeric",
                        height = "numeric",
                        estType = "numeric"),
          definition = function(curve,
                                age,
                                ageType,
                                height,
                                estType){
            return(Sindex_HtAgeToSI(curve = as.integer(curve), 
                                    age = age,
                                    ageType = as.integer(ageType),
                                    height = height,
                                    estType = as.integer(estType)))
          })



