#' Outlier Outline
#'
#' This function allows you a report about outliers of your data.
#' @param data Dataset, matrix or dataframe.
#' @param plot_show Logical argument for plotting outliers.
#' @keywords outlier
#' @export
#' @examples
#' outlier.outline(Hitters,plot_show = TRUE)



outlier.outline <- function(data){

  options(warn=-1) ## warn silici

  df <- data.frame(data)

  plotter <- function(x){

    #boxplot(x,horizontal = T,axes =F)
    plot(density(x),
         main = "",
         xlab = "",
         ylab = "")



  }

  par(mfrow=c(3,3),new=F)
  apply(df,2,plotter)




}



