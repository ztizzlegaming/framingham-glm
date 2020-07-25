na.convert.mean = function (frame) 
{
    vars <- names(frame)
    if (!is.null(resp <- attr(attr(frame, "terms"), "response"))) {
        vars <- vars[-resp]
        x <- frame[[resp]]
        pos <- is.na(x)
        if (any(pos)) {
            frame <- frame[!pos, , drop = FALSE]
            warning(paste(sum(pos), "observations omitted due to missing values in the response"))
        }
    }
    for (j in vars) {  #j is variable names
        x <- frame[[j]]
        pos <- is.na(x)
        if (any(pos)) {
            if (length(levels(x))) {   # factors
                xx <- as.character(x)
                xx[pos] <- "NA"
                x <- factor(xx, exclude = NULL)
            }
            else if (is.matrix(x)) {   # matrices
                ats <- attributes(x)
                x.na <- 1*pos
#               x[pos] <- 0
                w <- !pos
                n <- nrow(x)
                TT <- array(1, c(1, n))
                xbar <- (TT %*% x)/(TT %*% w)
                xbar <- t(TT) %*% xbar
                x[pos] <- xbar[pos]
                attributes(x) <- ats
                attributes(x.na) <- ats
                dimnames(x.na)[[2]]=paste(dimnames(x)[[2]],".na",sep='')
                frame[[paste(j,".na",sep='')]] <- x.na 
            } else {   # ordinary numerical vector
                ats <- attributes(x)
                x[pos] <- mean(x[!pos])
#               x[pos] <- 0
                x.na <- 1*pos
                frame[[paste(j,".na",sep='')]] <- x.na 
                attributes(x) <- ats
            }
            frame[[j]] <- x
        }
    }
    frame
}
