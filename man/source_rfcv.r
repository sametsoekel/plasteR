structure(list(name = "LittleMCAR", objs = list("package:BaylorEdPsych" = function (x) 
{
    if (!require(mvnmle)) 
        stop("You must have mvnmle installed to use LittleMCAR")
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
    gmean <- mlest(x)$muhat
    gcov <- mlest(x)$sigmahat
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
    cat("this could take a while")
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
    amount.missing <- matrix(nmis, 1, length(nmis))
    percent.missing <- amount.missing/n
    amount.missing <- rbind(amount.missing, percent.missing)
    colnames(amount.missing) <- var.names
    rownames(amount.missing) <- c("Number Missing", "Percent Missing")
    list(chi.square = d2, df = df, p.value = p.value, missing.patterns = n.mis.pat, 
        amount.missing = amount.missing, data = datasets)
}, function (x) 
{
    if (!require(mvnmle)) 
        stop("You must have mvnmle installed to use LittleMCAR")
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
    gmean <- mlest(x)$muhat
    gcov <- mlest(x)$sigmahat
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
    cat("this could take a while")
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
    amount.missing <- matrix(nmis, 1, length(nmis))
    percent.missing <- amount.missing/n
    amount.missing <- rbind(amount.missing, percent.missing)
    colnames(amount.missing) <- var.names
    rownames(amount.missing) <- c("Number Missing", "Percent Missing")
    list(chi.square = d2, df = df, p.value = p.value, missing.patterns = n.mis.pat, 
        amount.missing = amount.missing, data = datasets)
}), where = c("package:BaylorEdPsych", "namespace:BaylorEdPsych"
), visible = c(TRUE, FALSE), dups = c(FALSE, TRUE)), class = "getAnywhere")
