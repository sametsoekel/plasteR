#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param data Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' outline(Hitters,plot = TRUE)


outline <- function(data){

  df <- data.frame(data)

  number_obs <- length(df[,1])

  missing_obs_ratio <- apply(df, 1, function(x) sum(is.na(x)) / length(x))

  names(missing_obs_ratio) <- NULL

  missing_var_ratio <- apply(df, 2, function(x) sum(is.na(x)) / length(x))

  names(missing_var_ratio) <- NULL




}







