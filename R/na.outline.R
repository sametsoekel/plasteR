#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param data Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' na.outline(Hitters,plot = TRUE)


na.outline <- function(data, plot = FALSE){

  df <- data.frame(data)

  number_obs <- length(df[,1])

  missing_obs_ratio <- 100*apply(df, 1, function(x) sum(is.na(x)) / length(x))

  names(missing_obs_ratio) <- NULL

  missing_var_ratio <- 100*apply(df, 2, function(x) sum(is.na(x)) / length(x))

  names(missing_var_ratio) <- NULL

  mor <- missing_obs_ratio

  paint <- function(x){

    if(x==0){
      return("chartreuse4")
    }
    else if(x>0 && x<=5){
      return("chartreuse")
    }
    else if(x>5 && x<=10){
      return("darkgoldenrod2")
    }
    else if(x>10 && x<=15){
      return("coral1")
    }
    else if(x>15 && x<=20){
      return("coral3")
    }
    else{
      return("darkred")
    }
  }

  color_vector <- sapply(missing_var_ratio,paint)









}



k <- na.outline(Hitters)


cat("plasteR Missing Value Outcome Report\n
    ",z,"\nss")

k$




