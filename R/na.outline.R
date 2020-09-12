#' NA Outline
#'
#' This function allows you a report about missing values of your data.
#' @param data Dataset, matrix or dataframe.
#' @param plot_show Logical argument for plotting missing variables.
#' @param limit The lower limit of the desired missing rate (percentage).
#' @param show_over_limit Logical argument to print the caught index numbers of satisfied rate.
#' @keywords missing
#' @export
#' @examples
#' na.outline(Hitters,plot_show = TRUE)





na.outline <- function(data, plot_show = FALSE,limit = 20,show_over_limit = FALSE){

  options(warn=-1)

  ######## Little's missing completely at random test - start #########

  littles <- function (x) {
    if (!(is.matrix(x) | is.data.frame(x)))
      stop("Data should be a matrix or dataframe")
    if (is.data.frame(x))
      x <- data.matrix(x)
    n.var <- ncol(x)
    n <- nrow(x)
    var.names <- colnames(x)
    r <- 1 * is.na(x)
    nmis <- as.integer(apply(r, 2, sum))
    mdp <- (r %*% (2^((1:n.var - 1)))) + 1
    x.mp <- data.frame(cbind(x, mdp))
    colnames(x.mp) <- c(var.names, "MisPat")
    n.mis.pat <- length(unique(x.mp$MisPat))
    p <- n.mis.pat - 1
    gmean <- mvnmle::mlest(x)$muhat
    gcov <- mvnmle::mlest(x)$sigmahat
    colnames(gcov) <- rownames(gcov) <- colnames(x)
    x.mp$MisPat2 <- rep(NA, n)
    for (i in 1:n.mis.pat) {
      x.mp$MisPat2[x.mp$MisPat == sort(unique(x.mp$MisPat),
                                       partial = (i))[i]] <- i
    }
    x.mp$MisPat <- x.mp$MisPat2
    x.mp <- x.mp[, -which(names(x.mp) %in% "MisPat2")]
    datasets <- list()
    for (i in 1:n.mis.pat) {
      datasets[[paste("DataSet", i, sep = "")]] <- x.mp[which(x.mp$MisPat ==
                                                                i), 1:n.var]
    }
    kj <- 0
    for (i in 1:n.mis.pat) {
      no.na <- as.matrix(1 * !is.na(colSums(datasets[[i]])))
      kj <- kj + colSums(no.na)
    }
    df <- kj - n.var
    d2 <- 0
    for (i in 1:n.mis.pat) {
      mean <- (colMeans(datasets[[i]]) - gmean)
      mean <- mean[!is.na(mean)]
      keep <- 1 * !is.na(colSums(datasets[[i]]))
      keep <- keep[which(keep[1:n.var] != 0)]
      cov <- gcov
      cov <- cov[which(rownames(cov) %in% names(keep)), which(colnames(cov) %in%
                                                                names(keep))]
      d2 <- as.numeric(d2 + (sum(x.mp$MisPat == i) * (t(mean) %*%
                                                        solve(cov) %*% mean)))
    }
    p.value <- 1 - pchisq(d2, df)

    return(p.value)

  }

  #getAnywhere("mlest")
  ######## Little's missing completely at random test - end #########


  df <- data.frame(data)


  ######### calculations of data ###########

  missing_obs_ratio <- 100*apply(df, 1, function(x) sum(is.na(x)) / length(x))

  names(missing_obs_ratio) <- NULL

  missing_var_ratio <- 100*apply(df, 2, function(x) sum(is.na(x)) / length(x))

  missing_var_ratio <- round(missing_var_ratio,digits=2)

  ######### calculations of data ###########




  ######### barplot color arbiter function #########

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

  ######### barplot color arbiter function #########






  ########## a part of report output ##########

  cat("\nplasteR NA Outline\n\n\n~Little's missing completely at random test\n\np value -> ",
      littles(df),"\n\n~Missing Variable Ratios (Percentages)\n\n")

  print(missing_var_ratio)

  ########## a part of report output ##########




  ####

  mor_index <- which(missing_obs_ratio>limit)
  names(mor_index) <- NULL

  ####





  if(show_over_limit == T){



    cat("\n\n~The Most Lacking Observations (> %",limit,")\n\n")

    ifelse(length(mor_index)==0,print("None !"),print(mor_index))

    cat("\n\n\n")


  }




  if(plot_show == TRUE){



    color_vector <- sapply(missing_var_ratio,paint)

    plt <- barplot(missing_var_ratio, col=color_vector, xaxt="n",ylab = "Missing Percentage",ylim = c(0,50),main = "plasteR NA Outline")

    text(plt, par("usr")[3], labels = colnames(df), srt = 40, adj = c(1.1,1.1), xpd = TRUE, cex=0.7)



  }


}







