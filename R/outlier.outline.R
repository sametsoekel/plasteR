#' Outlier Outline
#'
#' This function allows you a report about structural information, mainly outliers, and normal distribution checks of your data.
#' @param data Dataset, matrix or dataframe.
#' @param plot_show Logical argument for plotting.
#' @param type Logical argument for plot type for each numeric variable. boxplot ("box"), density ("den").
#' @param get_skew if TRUE prints each skewness coefficient for each variable on the plot
#' @param get_normality if TRUE prints results of Shapiro-Wilk normality test for each variable on the plot
#' @param return_clean_data if TRUE the object of outlier.outline function includes a dataframe without multivariate outliers (via robust principal component analysis)
#' @param normality_as Default "log", return type of normality information. p value ("pval"), logical ("log"), no info ("NULL")
#' @keywords outlier, boxplot, density
#' @export
#' @examples
#' outlier.outline(Hitters,plot_show = TRUE,type="box",get_skew = TRUE,get_normality = FALSE,
#' return_clean_data = TRUE)



outlier.outline <- function(data,plot_show = TRUE,type="den",get_skew=TRUE,get_normality=TRUE,
                            normality_as="log",return_clean_data=TRUE){


  ####### plot par reseters func. #######

  opar <- par()

  ####### plot par reseters func. #######


  ####### plot margin arbiter func.-start ########

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


  #### a part of report output ####
  cat("plasteR Outlier Outline\n\n\n")
  #### a part of report output ####

  options(warn=-1) ## warn hider

  df <- data.frame(data)

  dfn <- purrr::keep(df,is.numeric)

  dfn_nona <- na.omit(dfn)

  results_mvo<-mvoutlier::aq.plot(purrr::keep(dfn_nona,is.numeric))
  cat("\n\n\n")
  if(nrow(dfn)>=5000 || nrow(dfn)<3) {
    stop("Sample size should be between 3 and 5000")}

  results <- MVN::mvn(dfn)

  var_names <- variable.names(dfn,showOutliers=TRUE)

  skewness <- results$Descriptives$Skew

  normality_test <- results$univariateNormality$Normality

  normality_p_value <- as.numeric(results$univariateNormality$`p value`)

  mv_outliers <- results_mvo[["outliers"]]

  ####### some statistical calculations via MVN and mvoutlier #######


  #########################################################################################################


  ######## finding optimal plot margins #########

  margin <- margin_arbiter(length(dfn))

  xlim <- margin$xlim

  ylim <- margin$ylim

  ######## finding optimal plot margins #########


  #########################################################################################################


  ######## below plots  ########


  if(type=="den"){
    if(plot_show==TRUE){



      plotter_den <- function(x,var){

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

      for(vn in var_names) plotter_den(dfn, vn)

      par(opar)

    }
  }


  else if(type=="box"){
    if(plot_show==TRUE){
      par(opar)
      boxplot(df,horizontal = T)
    }
  }



  ####### normality result vector ########

  normality_vector<-MVN::mvn(purrr::keep(df,is.numeric))
  if(normality_as=="log"){
    nvector <- normality_vector$univariateNormality$Normality
  }
  else if(normality_as=="pval"){
    nvector <- normality_vector$univariateNormality$`p value`
  }
  else if(normality_as=="NULL"){
    nvector <- NULL
  }
  else{
    stop("argument 'normality_as' should be 'log','pval' or 'NULL'")
  }

  names(nvector) <- normality_vector$univariateNormality$Variable

  ####### normality result vector ########


  ######## top plots ########


  ######### keeping univariate outliers #########

  univariatedetect <- function(x){
    if(is.numeric(x)){
      k<-boxplot.stats(x)
      outliers<-k$out
      z<-which(x%in% outliers)

      return(z)
    }
    else{
      return(NA)
    }

  }

  univariateoutliers<-lapply(df, univariatedetect)

  ######### keeping univariate outliers #########





  ########## making clean data ##########

  if(return_clean_data==T){
    clean_data <- dfn_nona[!mv_outliers,]
  }
  else{
    clean_data <- data.frame()
  }

  ########## making clean data ##########


  return(
    list(
      "univariate_outliers"=univariateoutliers,
      "mv_outliers"=clean_data,
      "normality_test"=nvector
    )
  )


}
