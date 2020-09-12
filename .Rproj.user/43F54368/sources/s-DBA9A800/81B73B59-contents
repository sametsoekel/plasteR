#' Get observations without multivariate outliers
#'
#' This function deletes multivariate outlier observations which are detected by robust principal component analysis and returns new data.
#' @param data Dataset, matrix or dataframe.
#' @param keepoutlier if TRUE, keeps outlier observations in a separate dataframe and returns a list including both.
#' @keywords outlier
#' @export
#' @examples
#' Hitters %>% withnomvoutlier()

withnomvoutlier <- function(data,keepoutlier=FALSE){

  df <- data.frame(data)

  df <- na.omit(df)

  ###### detection multivariate outlier indexes #######


  index <- plasteR::outlier.outline(data)

  df[index$mv_outliers,]

}


