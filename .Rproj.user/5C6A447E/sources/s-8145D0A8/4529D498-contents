#' Outlier Outline
#'
#' This function allows you a report about structural information, mainly outliers, and normal distribution checks of your data.
#' @param data Dataset, matrix or dataframe.
#' @param plot_show Logical argument for plotting.
#' @param type Logical argument for plot type for each numeric variable. boxplot ("box"), density ("den").
#' @param get_skew if TRUE prints each skewness coefficient for each variable on the plot
#' @param get_normality if TRUE prints results of Shapiro-Wilk normality test for each variable on the plot
#' @param return_clean_data if TRUE the object of outlier.outline function includes a dataframe without multivarite outliers (via robust principal component analysis)
#' @keywords outlier, boxplot, density
#' @export
#' @examples
#' outlier.outline(Hitters,plot_show = TRUE,type="box",get_skew = TRUE,get_normality = FALSE,
#' return_clean_data = TRUE)



outlier.outline <- function(data,plot_show = TRUE,type="den",get_skew=TRUE,get_normality=TRUE,
                            return_clean_data=TRUE){


  ####### margin arbiter func.-start ########

  margin_arbiter <-function(x){

    if(sqrt(x) == round(sqrt(x))){

      xlim <- sqrt(x)
      ylim <- sqrt(x)

      return(list("xlim"=xlim,"ylim"=ylim))
    }
    else{

      xlim <- sqrt(x)
      xlim <- floor(xlim)
      ylim <- xlim + 1

      if(xlim*ylim<x){
        return(list("xlim"=xlim+1,"ylim"=ylim))
      }
      else{
        return(list("xlim"=xlim,"ylim"=ylim))
      }
    }
  }

  ####### margin arbiter func.-end ########


#########################################################################################################


  ####### some statistical calculations via MVN and mvoutlier ######

  options(warn=-1) ## warn hider

  df <- data.frame(data)

  dfn <- keep(df,is.numeric)

  dfn_nona <- na.omit(dfn)

  results_mvo<-aq.plot(keep(dfn_nona,is.numeric))

  if(nrow(dfn)>=5000 || nrow(dfn)<3) {
    stop("Sample size should be between 3 and 5000")}

  results <- mvn(dfn)

  var_names <- variable.names(dfn,showOutliers=TRUE)

  skewness <- results$Descriptives$Skew

  normality_test <- results$univariateNormality$Normality

  normality_p_value <- results$univariateNormality$`p value`

  mv_outliers <- results_mvo$outliers

  ####### some statistical calculations via MVN and mvoutlier #######


  #########################################################################################################


  ######## finding optimal plot margins #########

  margin <- margin_arbiter(length(dfn))

  xlim <- margin$xlim

  ylim <- margin$ylim

  ######## finding optimal plot margins #########


  #########################################################################################################


  ######## below plots  ########

  if(plot_show==TRUE){



    plotter <- function(x,var){

      den <- density(x[[var]],na.rm = T)

      ####### finding the p value and skewness indexes ########

      skw <- skewness[which(var_names==var)]

      ntest <- normality_test[which(var_names==var)]

      #####
      lenx <- max(x[[var]],na.rm = T)-min(x[[var]],na.rm = T)

      lim1 <- min(x[[var]],na.rm = T)-lenx
      lim2 <- max(x[[var]],na.rm = T)+lenx

      ####### finding the p value and skewness indexes ########





      ################# checking given logical arguments ######################

      if(get_skew==T){
        skw_txt <- c("Skewness Coef.:",toString(skw))
      }
      else{
        skw_txt <- c("","")
      }

      if(get_normality==T){
        nrm_txt <- c("Normality Test :",toString(ntest))
      }
      else{
        nrm_txt <- c("","")
      }



      text_vector <- c(skw_txt,nrm_txt)

      color_vector <- c("dodgerblue","dodgerblue",
                        "darkblue","darkblue")

      ################# checking given logical arguments ######################




      if (!anyNA(x[[var]])){
        plot(den,
             main = c(var),
             xlab = "",
             ylab = "",
             xlim=c(lim1,lim2),
             col.main=c("black"))
        legend("topright",text_vector,
          bty = "n",cex = .9,text.col = color_vector)
      }
      else{


        plot(den,
             main = c(var," (including NA's)"),
             xlab = "",
             ylab = "",
             xlim=c(lim1,lim2),
             col.main="red")
        legend("topright",text_vector,
          bty = "n",cex = .9,text.col = color_vector)

      }

    }

    par(mfrow=c(xlim,ylim),mar=c(2.3,2.3,2.3,1))

    for(vn in var_names) plotter(dfn, vn)

  }


  ######## top plots ########

  return(
    list(
      "normality_p_values"=normality_p_value,
      "without_mv_outliers"=ifelse(return_clean_data==T,dfn_nona[!mv_outliers,],"NULL")
    )
  )


}

