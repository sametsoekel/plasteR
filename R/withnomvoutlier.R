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

  df <- data.frame(Hitters)

  df <- na.omit(df)


  ###### detection multivariate outlier indexes #######


  results_mvo <- mvoutlier::aq.plot(purrr::keep(df,is.numeric))

  mvindex <- results_mvo[["outliers"]]

  return(df[mvindex,])

}



