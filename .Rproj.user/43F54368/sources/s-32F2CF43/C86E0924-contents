#' Get observations under the limit
#'
#' This function deletes observations above the missing percentage and returns new data.
#' @param data Dataset, matrix or dataframe.
#' @param limit The lower limit of the desired missing rate (percentage).
#' @keywords missing, under limit
#' @export
#' @examples
#' Hitters %>% underlimit(20)

underlimit <- function(data,limit){

  df <- data.frame(data)

  missing_obs_ratio <- 100*apply(df, 1, function(x) sum(is.na(x)) / length(x))

  mor_index <- which(missing_obs_ratio>limit)

  names(mor_index) <- NULL

  newdata <- df[mor_index,]

  if(length(newdata[,1])==0){
    return(df)
  }
  else{
    return(newdata)
  }

}


