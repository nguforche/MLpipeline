#' @title Optimal threshold
#
#' @description
#' Compute the the optimal classification threshold
#'
#' @param prob Predicted probabilities by a classifier
#' @param obs ground truth (correct) 0-1 labels vector
#' @param opt.method  optima classifcation threshold method see package \code{PresenceAbsence}. Default 
#'  is the minRoc distance: i.e the threshold value at the minimum distance between the ROC curve and the 
#' to left hand corner (0,1)
#' @return threhold 
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' prob <- logreg$fitted.values
#' opt.thresh(prob = pob, obs = mtcars$vs) 
#' @export 
opt.thresh <- function(prob, obs, opt.methods = 9){
  thresh = 0.5 
  if(length(unique(obs)) > 1){
    obs <- as.numeric(as.factor(obs))-1 
    SIMDATA = cbind.data.frame(plotID = 1:length(obs), Observed = obs, Predicted = prob)
    thresh <- optimal.thresholds(SIMDATA, threshold = 101, which.model = 1, opt.methods = opt.methods)
    thresh <- ifelse(length(thresh["Predicted"]) >= 1 &  !is.na(as.numeric(thresh["Predicted"])), as.numeric(thresh["Predicted"]), 0.5)
  }
 
  return(thresh)
}

#' @title Cost/Benefit Optimal threshold
#
#' @description
#' Compute cost/benefit optimal classification thresholh. See \code{evThreshold}
#
#' @param prob Predicted probabilities by a classifier
#' @param obs ground truth (correct) 0-1 labels vector
#' @export 
cost.thresh <- function(prob, obs, cost = matrix(c(0, 0, 0, 0), nrow = 2)){

  if(length(unique(obs)) > 1){
    obs <- as.numeric(as.factor(obs))-1 
    evThreshold(response = obs, pprob = prob, crMatrix = cost)  
  } else list(best.threshold = 0.5)
}


#' @title Confusion Matrix
#'
#' @description
#' Compute confusion matrix to evaluate the accuracy of a classification.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @return a table of Confusion Matrix
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' ConfusionMatrix(y_pred = pred, y_true = mtcars$vs)
#' @export

ConfusionMatrix <- function(y_pred, y_true) {
  Confusion_Mat <- table(y_true, y_pred)
  return(Confusion_Mat)
}


#' @title Confusion Matrix (Data Frame Format)
#'
#' @description
#' Compute data frame format confusion matrix for internal usage.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @return a data.frame of Confusion Matrix
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' ConfusionDF(y_pred = pred, y_true = mtcars$vs)
#' @keywords internal
#' @export

ConfusionDF <- function(y_pred, y_true) {
  Confusion_DF <- transform(as.data.frame(ConfusionMatrix(y_pred, y_true)),
                            y_true = as.character(y_true),
                            y_pred = as.character(y_pred),
                            Freq = as.integer(Freq))
  return(Confusion_DF)
}

#' @title Recall
#
#' @description
#' Compute the recall score.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result
#' @return Recall
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' Recall(y_pred = pred, y_true = mtcars$vs, positive = "0")
#' Recall(y_pred = pred, y_true = mtcars$vs, positive = "1")
#' @export
#' @export
Recall <- function(y_true, y_pred, positive = NULL) {
  Confusion_DF <- ConfusionDF(y_pred, y_true)
  if (is.null(positive) == TRUE) positive <- as.character(Confusion_DF[1,1])
  TP <- as.integer(subset(Confusion_DF, y_true==positive & y_pred==positive)["Freq"])
  FN <- as.integer(sum(subset(Confusion_DF, y_true==positive & y_pred!=positive)["Freq"]))
  Recall <- TP/(TP+FN)
  return(Recall)
}




#' @title Precision
#'
#' @description
#' Compute the precision score.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result
#' @return Precision
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' Precision(y_pred = pred, y_true = mtcars$vs, positive = "0")
#' Precision(y_pred = pred, y_true = mtcars$vs, positive = "1")
#' @export
Precision <- function(y_true, y_pred, positive = NULL) {
  Confusion_DF <- ConfusionDF(y_pred, y_true)
  if (is.null(positive) == TRUE) positive <- as.character(Confusion_DF[1,1])
  TP <- as.integer(subset(Confusion_DF, y_true==positive & y_pred==positive)["Freq"])
  FP <- as.integer(sum(subset(Confusion_DF, y_true!=positive & y_pred==positive)["Freq"]))
  Precision <- TP/(TP+FP)
  return(Precision)
}


#' @title F1 Score
#'
#' @description
#' Compute the F1 Score.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result
#' @return F1 Score
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' F1_Score(y_pred = pred, y_true = mtcars$vs, positive = "0")
#' F1_Score(y_pred = pred, y_true = mtcars$vs, positive = "1")
#' @export

F1_Score <- function(y_true, y_pred, positive = NULL) {
  Confusion_DF <- ConfusionDF(y_pred, y_true)
  if (is.null(positive) == TRUE) positive <- as.character(Confusion_DF[1,1])
  Precision <- Precision(y_true, y_pred, positive)
  Recall <- Recall(y_true, y_pred, positive)
  F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
  return(F1_Score)
}

#' @title Performance Measures
#
#' @description
#' Compute several performance metrics.
#
#' @param pred prediction scores from classifier
#' @param obs ground truth  (correct) 0-1 labels vector
#' @return data frame with performance metrics 
#' @export
Performance.measures <- function(pred, obs, threshold=NULL){
#  obs <- as.numeric(factor(obs))-1 
  ## get best cut-off 
  if(is.null(threshold))
    threshold <- 0.5
  ### get these performance measures
  nme = c("PCC",  "AUC",  "sensitivity", "specificity")
  xx = na.omit(cbind.data.frame(plotID = 1:length(pred), Observed = obs, Predicted = pred))
  accuracy <- presence.absence.accuracy(xx, threshold = threshold, st.dev = TRUE)[, nme] 
  pred.prev <- predicted.prevalence(DATA=xx, threshold = threshold)[, c("Obs.Prevalence", "Predicted")]
  nme <- c("Pos Pred Value", "Neg Pred Value")#, "Balanced Accuracy")
  accuracy$G.mean <- sqrt(as.numeric(accuracy$sensitivity)*as.numeric(accuracy$specificity))
  accuracy$BER <- 1 - 0.5*(as.numeric(accuracy$sensitivity) + as.numeric(accuracy$specificity))  
  prevalence = as.numeric(pred.prev$Obs.Prevalence)
  
  obs <- factor(ifelse(obs == 1, "Yes", "No"), levels = c("Yes", "No")) 
  pred <- factor(ifelse(pred >= threshold, "Yes", "No"), levels = c("Yes", "No"))
  F1 <- F1_Score(y_true=obs, y_pred=pred, positive="Yes")
  accuracy$F1.score = F1
 
  cmx <- confusionMatrix(data=pred, reference=obs,  prevalence = prevalence)$byClass[nme]
  res <- cbind.data.frame(accuracy, t(cmx),  threshold = threshold)
  return(res)
}

#' @title normalize data
#
#' @description
#' Normalize matrix or data frame to the min-max range of the variables.
#
#' @param x  matrix or data frame 
#' @return nomalized matrix/data frame with min and max of each variable as attributes 
#' @export
# normalize data to 
normalize <- function(x) { 
  x <- as.matrix(x)
  minAttr=apply(x, 2, min, na.rm=TRUE)
  maxAttr=apply(x, 2, max, na.rm=TRUE)
  x <- sweep(x, 2, minAttr, FUN="-") 
  x=sweep(x, 2,  maxAttr-minAttr, "/") 
  attr(x, 'min') = minAttr
  attr(x, 'max') = maxAttr
  return (x)
} 
#' @title denomalize data 
#
#' @description 
#' tranform normalized data back to original scale  
#
#' @param  normalized  the normalized data frame/matrix 
#' @param  min,max the min and max of each variable in the data. This can be otained by retrieving the attribute values of 
#' the normalized data, i.e. output of \code{normalize}
#' @export
# denormalize back to original scale
denormalize <- function (normalized, min, max) {
  if(dim(normalized)[2]!= length(min)) stop("length of min or max must equal number of columns of data ")
  nme <- colnames(normalized)
  if( !all(nme%in%names(min)) ) stop("colnames of data do not match names of min or max")
  sapply(nme, function(x)   normalized[, x] * (max[x]-min[x]) + min[x] )
}


requireNamespaceStop <- function (package)
{
    if (!requireNamespace(package, quietly = TRUE))
        stop(paste("package", package, "is required"), call. = FALSE)
       
}

AUC <- function(x, y){
	  x1 = x[y==1]; n1 = length(x1); 
	  x2 = x[y==0]; n2 = length(x2);
	  r = rank(c(x1,x2))  
       (sum(r[1:n1]) - n1*(n1+1)/2) / (n1*n2) 
}

sortImp <- function (object, top) 
{
    
    best <- "max"
    featureRank <- switch(best, max = rank(-apply(object, 1, max, na.rm = TRUE)), 
                                min = rank(apply(object, 1, min, na.rm = TRUE)), 
                                maxabs = rank(-apply(abs(object), 1, max, na.rm = TRUE)))

    tiedRanks <- as.numeric(names(table(featureRank)[table(featureRank) > 1]))
    if (length(tiedRanks) > 0) {
        for (i in seq(along = tiedRanks)) {
            tmp <- featureRank[featureRank == tiedRanks[i]]
            featureRank[featureRank == tiedRanks[i]] <- tmp + 
                runif(length(tmp), min = 0.001, max = 0.999)
        }
    }
    featureOrder <- order(featureRank)
    out <- object[featureOrder, , drop = FALSE]
    out <- out[1:top, , drop = FALSE]
    out
}

#' @title Plot variable importance 
#
#' @description 
#' plot variable importance for random forest 
# 
#' @param x one column data frame with variable importance. Row names corresponds to the variables 
#' @param to plot the top variables 
#' @export
VimPlot <- function (x, top = min(20, length(x$importance)), ...) 
{
    varSubset <- sortImp(x, top)
    plotObj <- stack(varSubset)
    if (dim(varSubset)[2] == 1) {
        plotObj <- varSubset
        names(plotObj) <- "values"
        plotObj$ind <- "Overall"
    }
    else plotObj <- stack(varSubset)
    plotObj$Var <- rep(rownames(varSubset), dim(varSubset)[2])
    plotObj$Var <- factor(plotObj$Var, levels = rev(rownames(varSubset)))
    if (dim(varSubset)[2] < 3) {
        if (dim(varSubset)[2] > 1) 
            plotObj <- plotObj[plotObj$ind == levels(plotObj$ind)[1], ]
#            out <- dotplot(Var ~ values, data = plotObj, as.table = TRUE,  xlab = "Importance", cex = 1.5, cex.labels = 1.5, pch=21, ...)
            par(mar=c(5,5.5,4,2) + 0.1)  
            out <- dotchart2(data=plotObj$values, labels=plotObj$Var, horizontal=TRUE, pch=19,col = "blue", 
			xlab="Importance", ylab="Variables", lty=1, lines=TRUE, dotsize = 1.2,
			cex = 1.2, cex.labels = 1.1, sort.=FALSE, add=FALSE, xaxis=TRUE, width.factor= 1,
			lcolor='gray', leavepar=FALSE, ...)
	    par()			    
#            out <- dotchart(plotObj$values, labels=plotObj$Var,  xlab = "Importance", ylab="Variables", lty=1, ...)
    }
    else {
        out <- dotplot(Var ~ values, data = plotObj, groups = plotObj$ind, 
            auto.key = list(columns = min(3, length(levels(plotObj$ind)))), 
            as.table = TRUE, xlab = "Importance", ...)
    }
    out
}

#' @title sanitaized perfomance results  
#' @description 
#' This funtion takes a data frame with performance results say from bootstrap of CV with some grouping variable 
#' and compute mean, and confidence intervals (by groups)  
#' @export
# get summary results with confidence intervals 
getSummary <- function(tab,  groups = NULL,  alpha = 0.05){


if(is.null(groups)) {
tab[, "groups"] <- 1
groups = "groups"
}
 
mn = ddply(tab, .variables = groups, numcolwise(mean), na.rm = TRUE)
ci  <-   ddply(tab, .variables = groups, numcolwise(quantile), 
probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)

 
mn[, -1] <- format(round(mn[, -1], 2), nsmall = 2) 
ci[, -1] <- format(round(ci[, -1], 2), nsmall = 2) 

#### split by model, identify each status and merge 
df <- do.call(rbind.data.frame, lapply(unique(mn[, groups]), function(mod){
xx <- mn[mn[,groups] == mod, ]
yy <- ci[ci[,groups] == mod, ]
tb <- cbind.data.frame(groups = mod, 
t(as.character(paste0(paste0(paste0(paste0(paste0(xx[1, -1], " (" ),  yy[1, -1]), ","), yy[2, -1]), ")"))))
}))
names(df) <- names(mn)
df
}


#' @title get Imbalanced Ratioa 
#' @description 
#' Compute the class imbalanced raitio of an outcome  
#' @param y a binary outcome variable 
#' @export
getIMR <- function(y){
if(length(unique(y)) == 2){
    minCl <- names(which.min(table(y)))
  (sum(y==minCl)/sum(y!=minCl))*100
} else 0     
}


#' @title Hmisc summary data 
#' @description 
#' This function converts the Hmisc summary (reverse) object to a dataframe 
#' @param x summary.formula object (reverse). see 
#' @param digits number of descimal digits. See \code{summary.formula} for the other parameters 
#' @export
##### convert Hmisc summary.formula (reverse) to a matrix/data.frame
HmscData <- function (x, digits, prn = any(n != N), pctdig = 0, what = c("%", 
    "proportion"), npct = c("numerator", "both", "denominator", 
    "none"), exclude1 = TRUE, vnames = c("labels", "names"), 
    prUnits = TRUE, sep = "/", abbreviate.dimnames = FALSE, prefix.width = max(nchar(lab)), 
    min.colwidth, formatArgs = NULL, round = NULL, prtest = c("P", 
        "stat", "df", "name"), prmsd = FALSE, long = FALSE, pdig = 3, 
    eps = 0.001, ...) 
{
    npct <- match.arg(npct)
    vnames <- match.arg(vnames)
    what <- match.arg(what)
    if (is.logical(prtest) && !prtest) 
        prtest <- "none"
    stats <- x$stats
    nv <- length(stats)
    cstats <- lab <- character(0)
    nn <- integer(0)
    type <- x$type
    n <- x$n
    N <- x$N
    nams <- names(stats)
    labels <- x$labels
    Units <- x$units
    test <- x$testresults
    if (!length(test)) 
        prtest <- "none"
    nw <- if (lg <- length(x$group.freq)) 
        lg
    else 1
    gnames <- names(x$group.freq)
    if (!missing(digits)) {
        oldopt <- options(digits = digits)
        on.exit(options(oldopt))
    }
    cstats <- NULL
    for (i in 1:nv) {
        nn <- c(nn, n[i])
        xx <- stats[[i]]
        
        vv <- do.call(rbind, lapply(1:nrow(xx), function(ii) {
	tb <- as.numeric(xx[ii, ]) 
	ix<- which(tb <= 11)
	if(length(ix) > 0)
	tb[unique(c(ix,3))] <- NA
	tb 
	}))
        rownames(vv) <- rownames(xx)
        colnames(vv) <- colnames(xx) 
        nam <- if (vnames == "names") 
            nams[i]
        else labels[i]
        if (prUnits && nchar(Units[i])) 
            nam <- paste0(nam, " [", gsub("*", " ", Units[i], 
                fixed = TRUE), "]")
        tr <- if (length(test) && all(prtest != "none")) 
            test[[nams[i]]]
        else NULL
        if (type[i] == 1 || type[i] == 3) {
            cs <- formatCats(vv, nam, tr, type[i], if (length(x$group.freq)) 
                x$group.freq
            else x$n[i], what, npct, pctdig, exclude1, long, 
                prtest, pdig = pdig, eps = eps)
            nn <- c(nn, rep(NA, nrow(cs) - 1))
        }
        else cs <- formatCons(vv, nam, tr, x$group.freq, 
            prmsd, sep, formatArgs, round, prtest, pdig = pdig, 
            eps = eps)
        cstats <- rbind(cstats, cs)
    }
    lab <- dimnames(cstats)[[1]]
    gl <- names(x$group.freq)
    gl <- if (length(gl)) 
        paste0(gl, " \n(N=", x$group.freq, ")")
    else ""
    if (length(test) && !all(prtest == "none")) 
        gl <- c(gl, if (length(prtest) == 1 && prtest != "stat") if (prtest == 
            "P") "P-value" else prtest else "  Test\nStatistic")
    nc <- nchar(cstats)
    spaces <- substring("                                                        ", 
        1, (max(nc) - nc + 1)/2)
    dc <- dim(cstats)
    cstats <- paste0(spaces, cstats)
    dim(cstats) <- dc
    if (prn) {
        cnn <- format(nn)
        cnn[is.na(nn)] <- ""
        cstats <- cbind(cnn, cstats)
        gl <- c("N", gl)
    }
    cstats <- rbind(gl, cstats)
    dimnames(cstats) <- list(c("", lab), NULL)
#    cat("\n\nDescriptive Statistics", if (length(x$group.label)) 
#    paste(" by", x$group.label)
#    else paste0("  (N=", x$N, ")"), "\n\n", sep = "")
    if (missing(min.colwidth)) 
        min.colwidth <- max(min(nchar(gl)), min(nc[nc > 0]))
#    print.char.matrix(cstats, col.names = FALSE, col.txt.align = "left", ...)
    ss <- cstats
xx <- gsub("\\n", "", ss[1,])  
ss <- ss[-1, ]
colnames(ss) <- xx
n <- ncol(ss)  
p <- sapply(ss[,n], function(xx) tail(unlist(strsplit(xx,  " ")),1))
ss[, n] <- p 
ss <- data.frame(variables = rownames(ss), ss, check.names = FALSE)
rownames(ss) <- NULL 
ss
}

#
reduce.factor <- function(x, old.levels, new.level =NULL) {
  if(is.null(old.levels)) stop("must specify levels to replace")
  if(is.null(new.level)) {new.level <-"Other"}  
  levels(x)[levels(x)%in%old.levels] <- new.level
  return(x)
}
       
Filter.Vars = function(dat, rhs.vars, thresh=0.00016){

  for(x in rhs.vars){
    lunique = length(unique(dat[, x]))  
    
    if(lunique==1)  
      dat[, x] <- NULL 
    else if(lunique == 2){
      levs <- sort(unique(dat[, x]))
      m1 = mean(dat[,x] ==levs[1], na.rm = TRUE)
      m0 = mean(dat[,x] ==levs[2], na.rm = TRUE)
      if(m1 <= thresh | m0 <= thresh) dat[, x] <- NULL 
    } else if(lunique > 2 & class(dat[,x]) == "factor") { ### reduce number of levels 
      
#       xx<- factor(dat[,x])
       tb <- prop.table(table(dat[,x]))
       nme <- names(tb)[which(tb < thresh)]
       dat[, x] <- reduce.factor(x= dat[, x], old.levels = nme)       
    }
  }
  return(dat)
  }

stError <- function(x, ..) sd(x, ...)/length(x) 

#' @title Reduce Factor levels 
#' @description 
#' Reduce the levels in a factor variable. Can be use to collapse rare factor levels to a single level  
#' @param x  factor variable 
#' @param  old.levels levels to replace 
#' @param new.levels new level to replace the old levels 
#' @export
reduce.factor <- function(x, old.levels, new.level =NULL) {
  if(is.null(old.levels)) stop("must specify levels to replace")
  if(is.null(new.level)) {new.level <-"Other"; warning("new level not specified, so using 'Other' ")}  
  levels(x)[levels(x)%in%old.levels] <- new.level
  return(x)
}


#' @title Filter variables in a data frame  
#' @description 
#' Remove constant variable, or rare factor variables below a threshold. Reduce factor levels   
#' @param dat  data frame 
#' @param  rhs.vars variables in data frame to filter  
#' @param threh  threshold 
#' @export

Filter.Vars = function(dat, rhs.vars, thresh=0.00016){
  
  for(x in rhs.vars){
    lunique = length(unique(dat[, x]))  
    
    if(lunique==1)  
      dat[, x] <- NULL 
    else if(lunique == 2){
      levs <- sort(unique(dat[, x]))
      m1 = mean(dat[,x] ==levs[1], na.rm = TRUE)
      m0 = mean(dat[,x] ==levs[2], na.rm = TRUE)
      if(m1 <= thresh | m0 <= thresh) dat[, x] <- NULL 
    } else if(lunique > 2 & class(dat[,x]) == "factor") { ### reduce number of levels 
      tb <- prop.table(table(dat[,x]))
      nme <- names(tb)[which(tb < thresh)]
      dat[, x] <- reduce.factor(x= dat[, x], old.levels = nme)       
    } 
  }
  return(dat)
}


