#' Add users to custom audience
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
createAudienceUser <- function(token, data, schema = "MADID_SHA256"){
  
  if (missing(token)) {
    stop("Missing token", call. = FALSE)
  }
  if (data) {
    stop("Missing data", call. = FALSE)
  }
  
}