# these are the functions for 2_Model_Structure_for_Baseline_and_Dynamic_Model

# libraries
library(survival)

# keep results
init_preds_BASE_DYN <- function(){
  
  results <- tibble(preds = double(),
                    y_true_cat = character(),
                    y_true_time = double(),
                    train_set = character(),
                    test_set = character(),
                    model = character(),
                    model_type = character(),
                    horizon = character(),
                    imputation = character(),
                    LM = double(),
                    functioneelDossierNr = double(),
                    CAT_catheter_episode = double())
  
  return(results) 
}

init_results_BASE_DYN <- function(){
  
  results <- tibble(train_set = character(),
                    test_set = character(),
                    metric = character(),
                    value = numeric(),
                    model = character(),
                    model_type = character(),
                    horizon = character(),
                    imputation = character(),
                    LM = double())
  
  return(results)
}

init_coefs <- function(){
  
  results <- tibble(variable = character(),
                    value = numeric(),
                    train_set = character(),
                    model = character(),
                    model_type = character(),
                    horizon = character(),
                    imputation = character(),
                    LM = double())

  
  return(results)
}

init_results_sumstats <- function(){
  
  summary_results <- tibble(median_AUC = character(),
                            mean_AUC = character(),
                            median_Calibration_Slope = character(),
                            mean_Calibration_Slope = character(),
                            median_Calibration_Intercept = character(),
                            mean_Calibration_Intercept = character(),
                            median_OE_ratio = character(),
                            mean_OE_ratio = character(),
                            median_ECE = character(),
                            mean_ECE = character(),
                            median_ECI = character(),
                            mean_ECI = character(),
                            median_BS = character(),
                            mean_BS = character(),
                            median_Scaled_BS = character(),
                            mean_Scaled_BS = character(),
                            model = character(),
                            imputation = character())
  
  return(summary_results)
}

init_results_plot <- function(){
  
  summary_plot <- tibble(metric = character(),
                         mean = numeric(),
                         lower.ci = numeric(),
                         upper.ci = numeric(),
                         median = numeric(),
                         min = numeric(),
                         max = numeric(), 
                         Q1 = numeric(),
                         Q3 = numeric(),
                         model = character(),
                         imputation = character())
  
  return(summary_plot)
}

# for computing performances in terms of various calibration and discrimination measures
# using only y_test and pred_test for calculation, treating all observations/predictions as binary

#' @param dd test dataset
#' @param fm The formula that will be called by the model, of the form \code{y_test ~ pred_test}
#'  
#' @describeIn Area under the ROC curve
#' @importFrom pROC roc auc
#' @export
c_statistic <- function(obs, pred){
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package \"pROC\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  m <- pROC:::roc(obs, pred, direction="<", quiet = TRUE)
  pROC:::auc(m)
}

#' @describeIn calibration_slope Estimate calibration slope
#' @export
calibration_slope <- function(obs, pred){
  dat <- data.frame(e = pred, o = obs)
  dat$e[dat$e == 0] = 0.0000000001
  dat$e[dat$e == 1] = 0.9999999999
  fm <- as.formula(o~e)
  fm <- update(fm, . ~ qlogis(.))
  m <- glm(fm, dat, family = binomial, na.action = na.omit)
  coef(m)[2]
}

#' @describeIn calibration_intercept Estimate calibration-in-the-large
#' @export
calibration_large <- function(obs, pred){
  dat <- data.frame(e = pred, o = obs)
  dat$e[dat$e == 0] = 0.0000000001
  dat$e[dat$e == 1] = 0.9999999999
  fm <- as.formula(o~e)
  fm <- update(fm, . ~ offset(qlogis(.)))
  m <- glm(fm, dat, family = binomial, na.action = na.omit)
  coef(m)
}

#' @describeIn O/E ratio Estimate ratio of observed to expected number of events
#' @export
oe_ratio <- function(obs, pred){
  sum(obs, na.rm = TRUE) / sum(pred, na.rm = TRUE)
}

#' @describeIn ECE Estimate expected calibration error
#' @export
ECE <- function (obs, pred, n_bins = 10){
  idx <- order(pred)
  pred_actual <- (cbind(pred[idx], obs[idx]))
  N <- nrow(pred_actual)
  rest <- N%%n_bins
  S <- 0
  W <- c()
  B <- min(N, n_bins)
  groups <- list()
  for (i in 1:B) {
    if (i <= rest) {
      group_pred <- (pred_actual[(((i - 1) * ceiling(N/n_bins) + 
                                     1):(i * ceiling(N/n_bins))), 1])
      group_actual <- (pred_actual[(((i - 1) * ceiling(N/n_bins) + 
                                       1):(i * ceiling(N/n_bins))), 2])
    }
    else {
      group_pred <- (pred_actual[((rest + (i - 1) * floor(N/n_bins) + 
                                     1):(rest + i * floor(N/n_bins))), 1])
      group_actual <- (pred_actual[((rest + (i - 1) * 
                                       floor(N/n_bins) + 1):(rest + i * floor(N/n_bins))), 
                                   2])
    }
    n_ <- length(group_pred)
    expected <- mean(group_pred, na.rm=TRUE)
    observed <- mean(group_actual, na.rm=TRUE)
    S[i] <- abs(observed - expected)
    W[i] <- n_/N
    groups[[i]] <- group_pred
  }
  mean_prediction <- lapply(groups, mean, na.rm=TRUE)
  min_group <- lapply(groups, min, na.rm=TRUE)
  max_group <- lapply(groups, max, na.rm=TRUE)
  res <- t(S) %*% W
  return(as.numeric(res))
}

#' @describeIn ECI Estimate estimated calibration index (average squared difference)
#' @export
ECI <- function(obs, pred){
  # Remove NA values from obs and pred
  valid_idx <- complete.cases(obs, pred)
  obs <- obs[valid_idx]
  pred <- pred[valid_idx]
  
  idx <- order(pred)
  obs <- obs[idx]
  pred <- pred[idx]
  argzLoess = alist(degree = 2)
  # pred <- jitter(pred, factor=0.2)
  argzLoess$formula = obs ~ pred
  Sm <- do.call("loess", argzLoess)
  Sm <- data.frame(Sm$x, Sm$fitted)
  cal.smooth <- approx(Sm, xout = pred, ties = "ordered")$y
  coom <- cbind(pred, cal.smooth)
  mean((pred - cal.smooth) ^ 2) * 100
}
 



#' @describeIn brier score Estimate Brier Score
#' @export
brier_score <- function(obs, pred) {
  mean((obs - pred)^2, na.rm = TRUE)
}

#' @describeIn scaled brier score Estimate Scaled Brier Score
#' @export
scaled_brier_score <- function(obs, pred) {
  1 - (brier_score(obs, pred) / (mean(obs, na.rm = TRUE) * (1 - mean(obs, na.rm = TRUE))))
}



# apply the same sequence of thresholds to all samples and take the mean of the sensitivity and specificity per threshold to get the "mean ROC curve"

mean_roc <- function(data, cutoffs = seq(from = 0, to = 1, by = 0.01)) {
  map_df(cutoffs, function(cp) {
    out <- cutpointr(data = data, x = preds, class = y_test,
                     subgroup = test_set, method = oc_manual, cutpoint = cp,
                     pos_class = 1, direction = ">=")
    data.frame(cutoff = cp, 
               sensitivity = mean(out$sensitivity),
               specificity = mean(out$specificity))
  })
}


# for making the 10 deciles of calibration plot

lift_table <- function(table, bin_number = 10) {
  table %>%
    dplyr::mutate(yhat_bin = ggplot2::cut_number(yhat, bin_number)) %>%
    dplyr::group_by(yhat_bin) %>%
    dplyr::summarise(mean_y = mean(y), mean_yhat = mean(yhat)) 
}

# for adding rcs smooth calibration curves with confidence bands
# this comes from https://github.com/BavoDC/CalibrationCurves/blob/master/R/rcspline.plot.noprint.R

#' Internal function
#'
#' Adjusted version of the \code{\link[Hmisc]{rcspline.plot}} function where only the output is returned and no plot is made
#'
#'
#' @param x a numeric predictor
#' @param y a numeric response. For binary logistic regression, \code{y} should be either 0 or 1.
#' @param model \code{"logistic"} or \code{"cox"}. For \code{"cox"}, uses the \code{coxph.fit} function with \code{method="efron"} argument set.
#' @param xrange range for evaluating \code{x}, default is \eqn{f} and \eqn{1 - f} quantiles of \code{x},
#' where \eqn{f = \frac{10}{\max{(n, 200)}}}{f = 10/max(\code{n}, 200)} and \eqn{n} the number of observations
#' @param event event/censoring indicator if \code{model="cox"}. If \code{event} is present, \code{model} is assumed to be \code{"cox"}
#' @param nk number of knots
#' @param knots knot locations, default based on quantiles of \code{x} (by \code{\link[Hmisc]{rcspline.eval}})
#' @param show \code{"xbeta"} or \code{"prob"} - what is plotted on \verb{y}-axis
#' @param adj optional matrix of adjustment variables
#' @param xlab \verb{x}-axis label, default is the \dQuote{label} attribute of \code{x}
#' @param ylab \verb{y}-axis label, default is the \dQuote{label} attribute of \code{y}
#' @param ylim \verb{y}-axis limits for logit or log hazard
#' @param plim \verb{y}-axis limits for probability scale
#' @param plotcl plot confidence limits
#' @param showknots show knot locations with arrows
#' @param add add this plot to an already existing plot
#' @param plot logical to indicate whether a plot has to be made. \code{FALSE} suppresses the plot.
#' @param subset subset of observations to process, e.g. \code{sex == "male"}
#' @param lty line type for plotting estimated spline function
#' @param noprint suppress printing regression coefficients and standard errors
#' @param m for \code{model="logistic"}, plot grouped estimates with triangles. Each group contains \code{m} ordered observations on \code{x}.
#' @param smooth plot nonparametric estimate if \code{model="logistic"} and \code{adj} is not specified
#' @param bass smoothing parameter (see \code{supsmu})
#' @param main main title, default is \code{"Estimated Spline Transformation"}
#' @param statloc location of summary statistics. Default positioning by clicking left mouse button where upper left corner of statistics should appear.
#'  Alternative is \code{"ll"} to place below the graph on the lower left, or the actual \code{x} and \code{y} coordinates. Use \code{"none"} to suppress statistics.
#'
#' @return list with components (\samp{knots}, \samp{x}, \samp{xbeta}, \samp{lower}, \samp{upper}) which are respectively the knot locations, design matrix,
#' linear predictor, and lower and upper confidence limits
#' @seealso   \code{\link[rms]{lrm}}, \code{\link[rms]{cph}}, \code{\link[Hmisc]{rcspline.eval}}, \code{\link[graphics]{plot}}, \code{\link[stats]{supsmu}},
#' \code{\link[survival:survival-internal]{coxph.fit}}, \code{\link[rms]{lrm.fit}}
rcspline_plot <- function(x, y, model=c("logistic","cox","ols"), xrange,
                           event, nk=5, knots=NULL, show=c("xbeta", "prob"),
                           adj=NULL, xlab, ylab, ylim, plim=c(0,1),
                           plotcl=TRUE, showknots=TRUE, add=FALSE, plot = TRUE, subset,
                           lty=1, noprint=FALSE, m, smooth=FALSE, bass=1,
                           main="auto", statloc)
{
  model <- match.arg(model)
  show  <- match.arg(show)
  if(plot) {
    oldpar = par(no.readonly = TRUE)
    on.exit(par(oldpar))
  }
  
  if(! missing(event))
    model<-"cox"
  
  if(model == "cox" & missing(event))
    stop('event must be given for model="cox"')
  
  if(show == "prob" & ! missing(adj))
    stop('show="prob" cannot be used with adj')
  
  if(show == "prob" & model != "logistic")
    stop('show="prob" can only be used with model="logistic"')
  
  if(length(x) != length(y))
    stop('x and y must have the same length')
  
  if(! missing(event) && length(event) != length(y))
    stop('y and event must have the same length')
  
  if(! missing(adj)) {
    if(! is.matrix(adj)) adj <- as.matrix(adj)
    if(dim(adj)[1] != length(x))
      stop('x and adj must have the same length')
  }
  
  if(missing(xlab))
    xlab <- Hmisc::label(x)
  
  if(missing(ylab))
    ylab <- Hmisc::label(y)
  
  isna <- is.na(x) | is.na(y)
  if(! missing(event))
    isna <- isna | is.na(event)
  
  nadj <- 0
  if(! missing(adj)) {
    nadj <- ncol(adj)
    isna <- isna | apply(is.na(adj), 1, sum) > 0
  }
  
  if(! missing(subset))
    isna <- isna | (! subset)
  
  x <- x[! isna]
  y <- y[! isna]
  if(! missing(event))
    event <- event[! isna]
  
  if(! missing(adj))
    adj <- adj[! isna, ]
  
  n <- length(x)
  if(n<6)
    stop('fewer than 6 non-missing observations')
  
  if(missing(xrange)) {
    frac<-10./max(n, 200)
    xrange<-quantile(x, c(frac, 1.-frac))
  }
  
  if(missing(knots))
    xx <- Hmisc::rcspline.eval(x, nk=nk)
  else xx <- Hmisc::rcspline.eval(x, knots)
  
  knots <- attr(xx, "knots")
  nk <- length(knots)
  
  df1 <- nk-2
  if(model == "logistic") {
    b <- rms::lrm.fit(cbind(x, xx, adj),  y)
    beta <- b$coef
    cov <- b$var
    model.lr <- b$stats["Model L.R."]
    offset <- 1 	#to skip over intercept parameter
    ylabl <-
      if(show == "prob")
        "Probability"
    else "log Odds"
    
    sampled <- paste("Logistic Regression Model,  n=", n," d=", sum(y), sep="")
  }
  
  if(model == "cox") {
    if(! existsFunction('coxph.fit'))
      coxph.fit <- getFromNamespace('coxph.fit', 'survival')
    ##11mar04
    
    ## added coxph.control around iter.max, eps  11mar04
    lllin <- coxph.fit(cbind(x, adj), cbind(y, event), strata=NULL,
                       offset=NULL, init=NULL,
                       control=coxph.control(iter.max=10, eps=.0001),
                       method="efron", rownames=NULL)$loglik[2]
    b <- coxph.fit(cbind(x, xx, adj), cbind(y, event), strata=NULL,
                   offset=NULL, init=NULL,
                   control=coxph.control(iter.max=10, eps=.0001),
                   method="efron", rownames=NULL)
    beta <- b$coef
    if(! noprint) {
      print(beta);
      print(b$loglik)
    }
    
    beta <- b$coef
    cov <- b$var
    model.lr<-2*(b$loglik[2]-b$loglik[1])
    offset <- 0
    ylabl <- "log Relative Hazard"
    sampled <- paste("Cox Regression Model, n=",n," events=",sum(event),
                     sep="")
  }
  
  if(model == "logistic"|model == "cox") {
    model.df <- nk - 1 + nadj
    model.aic <- model.lr-2.*model.df
    v <- solve(cov[(1 + offset) : (nk + offset - 1), (1 + offset) : (nk + offset - 1)])
    assoc.chi <- beta[(1 + offset) : (nk + offset - 1)] %*% v %*%
      beta[(1 + offset) : (nk + offset - 1)]
    assoc.df <- nk - 1   #attr(v,"rank")
    assoc.p <- 1.-pchisq(assoc.chi, nk - 1)
    v <- solve(cov[(2 + offset) : (nk + offset - 1), (2 + offset) : (nk + offset - 1)])
    linear.chi <- beta[(2 + offset) : (nk + offset - 1)] %*% v %*%
      beta[(2 + offset) : (nk + offset - 1)]
    linear.df <- nk - 2   #attr(v,"rank")
    linear.p <- 1. - pchisq(linear.chi, linear.df)
    if(nadj > 0) {
      ntot <- offset + nk - 1 + nadj
      v <- solve(cov[(nk + offset) : ntot, (nk + offset) : ntot])
      adj.chi <- beta[(nk + offset) : ntot] %*% v %*%
        beta[(nk + offset) : ntot]
      adj.df <- ncol(v)   #attr(v,"rank")
      adj.p <- 1. - pchisq(adj.chi, adj.df)
    } else {
      adj.chi <- 0
      adj.p <- 0
    }
  }
  
  ## Evaluate xbeta for expanded x at desired range
  xe <- seq(xrange[1], xrange[2], length=200)
  if(model == "cox")
    xx <- Hmisc::rcspline.eval(xe, knots, inclx=TRUE)
  else
    xx<- cbind(rep(1, length(xe)), Hmisc::rcspline.eval(xe, knots, inclx=TRUE))
  
  xbeta <- xx %*% beta[1 : (nk - 1 + offset)]
  var <- drop(((xx %*% cov[1 : (nk - 1 + offset), 1 : (nk - 1 + offset)])*xx) %*%
                rep(1, ncol(xx)))
  lower <- xbeta - 1.96*sqrt(var)
  upper <- xbeta + 1.96*sqrt(var)
  if(show == "prob") {
    xbeta <- 1./(1. + exp(-xbeta))
    lower <- 1./(1. + exp(-lower))
    upper <- 1./(1. + exp(-upper))
  }
  
  xlim <- range(pretty(xe))
  if(missing(ylim))
    ylim <- range(pretty(c(xbeta, if(plotcl) lower, if(plotcl) upper)))
  
  if(main == "auto") {
    if(show == "xbeta")
      main <- "Estimated Spline Transformation"
    else main <- "Spline Estimate of Prob{Y=1}"
  }
  
  if(! interactive() & missing(statloc))
    statloc<-"ll"
  
  if(plot) {
    if(! add) {
      oldmar<-par("mar")
      if(! missing(statloc) && statloc[1] == "ll")
        oldmar[1]<- 11
      
      plot(xe, xbeta, type="n", main=main, xlab=xlab, ylab=ylabl,
           xlim=xlim, ylim=ylim)
      lines(xe, xbeta, lty=lty)
      ltext<-function(z, line, label, cex=.8, adj=0)
      {
        zz<-z
        zz$y<-z$y-(line - 1)*1.2*cex*par("csi")*(par("usr")[4]-par("usr")[3])/
          (par("fin")[2])   #was 1.85
        text(zz, label, cex=cex, adj=adj)
      }
      
      sl<-0
      if(missing(statloc)) {
        message("Click left mouse button at upper left corner for statistics\n")
        z<-locator(1)
        statloc<-"l"
      } else if(statloc[1] != "none") {
        if(statloc[1] == "ll") {
          z<-list(x=par("usr")[1], y=par("usr")[3])
          sl<-3
        } else z<-list(x=statloc[1], y=statloc[2])
      }
      
      if(statloc[1] != "none" & (model == "logistic" | model == "cox"))	{
        rnd <- function(x, r=2) as.single(round(x, r))
        
        ltext(z, 1 + sl, sampled)
        ltext(z, 2 + sl, "    Statistic        X2  df")
        chistats<-format(as.single(round(c(model.lr, model.aic,
                                           assoc.chi, linear.chi, adj.chi), 2)))
        pvals<-format(as.single(round(c(assoc.p, linear.p, adj.p), 4)))
        ltext(z, 3 + sl, paste("Model        L.R. ", chistats[1], model.df,
                               " AIC=", chistats[2]))
        ltext(z, 4 + sl, paste("Association  Wald ", chistats[3], assoc.df,
                               " p= ", pvals[1]))
        ltext(z, 5 + sl, paste("Linearity    Wald ", chistats[4], linear.df,
                               " p= ", pvals[2]))
        if(nadj > 0)ltext(z, 6 + sl, paste("Adjustment   Wald " , chistats[5],
                                           adj.df, " p= ", pvals[3]))}
    } else lines(xe, xbeta, lty=lty)
    
    if(plotcl) {
      #prn(cbind(xe, lower, upper))
      lines(xe, lower, lty=2)
      lines(xe, upper, lty=2)
    }
    
    if(showknots) {
      bot.arrow <- par("usr")[3]
      top.arrow <- bot.arrow + .05 * (par("usr")[4]-par("usr")[3])
      for(i in 1 : nk)
        arrows(knots[i], top.arrow, knots[i], bot.arrow, length=.1)
    }
    
    if(model == "logistic" & nadj == 0) {
      if(smooth) {
        z<-supsmu(x, y, bass=bass)
        if(show == "xbeta") z$y <- logb(z$y/(1.-z$y))
        points(z, cex=.4)
      }
      
      if(! missing(m)) {
        z<-groupn(x, y, m=m)
        if(show == "xbeta") z$y <- logb(z$y/(1.-z$y))
        
        points(z, pch=2, mkh=.05)}
    }
  }
  
  invisible(list(
    knots = knots,
    x = xe,
    xbeta = xbeta,
    lower = lower,
    upper = upper
  ))
}

## these are functions used to calculate weights IPCW ##
create.wData.omega <-
  function(Tstart, Tstop, status, id, stratum, failcode, cens){
    ## Function to create framework for dataset in which individuals are reweighted after they
    ## experience a competing event, the omega-weights in Geskus (2015)
    event.times <- sort(unique(Tstop[status==failcode]))
    n <- length(Tstop)
    Nw <- rep(1,n)
    sel.compet <- (status!=cens)&(status!=failcode)&!is.na(status)
    ## Number of rows in dataset with weights
    Nw[sel.compet] <-  apply(outer(Tstop[sel.compet],event.times,"<"), 1, sum)+1
    data.weight <- data.frame(id=rep(id,Nw), Tstart=NA, Tstop=NA, status=rep(status,Nw),
                              strata=rep(stratum,sum(Nw)))
    data.weight$Tstart <- unlist(lapply(1:n, FUN=function(x,tms,N) {
      if (N[x]==1) {
        Tstart[x]
      } else {
        if (N[x]==2) {
          c(Tstart[x],Tstop[x])
        } else {
          c(Tstart[x], Tstop[x], rev(rev(tms)[2:(N[x]-1)]))
        }
      }
    }, tms=event.times, N=Nw))
    data.weight$Tstop <- unlist(lapply(1:n, FUN=function(x,tms,N) {
      if (N[x]==1) {
        Tstop[x]
      } else {
        c(Tstop[x], tail(tms,N[x]-1))
      }
    }, tms=event.times, N=Nw))
    data.weight
  }


crr_custom <-
  function(Tstop, status, data, trans, cens, Tstart, id, strata, 
           keep, shorten=TRUE, rm.na=TRUE, origin=0,
           prec.factor=1000, ...) {
    
    nn <- length(Tstop)
    # calc.trunc <- any(Tstart[!is.na(Tstart)] != 0)
    
    sel <- !is.na(Tstart) & !is.na(Tstop)
    
    strata.val <- rep(1,nn)
    
    strata.num <- as.numeric(factor(strata.val))
    
    num.id <- 1:nn
    
    
    ## Eliminate records with missings in status if rm.na=TRUE
    if(rm.na) sel <- sel & !is.na(status)
    Tstart <- Tstart[sel]
    Tstop <- Tstop[sel]
    status <- status[sel]
    strata.val <- strata.val[sel]
    strata.num <- strata.num[sel]
    id <- id[sel]
    num.id <- num.id[sel]
    n <- length(Tstop)
    
    # keep.name <- names(keep)
    # nkeep <- length(keep)
    # nkeep <- 1
    nkeep <- ncol(keep)
    ##        keep.name <- names(as.data.frame(keep))
    keep.name <- names(keep)
    
    Tstart <- Tstart - origin
    Tstop <- Tstop - origin
    
    
    ## Start calculations
    prec <- .Machine$double.eps*prec.factor
    
    ## Calculate product-limit time-to-censoring distribution, "event" not included in case of ties
    surv.cens <- survival::survfit(Surv(Tstart,Tstop+ifelse(status==cens,prec,0),status==cens)~strata.num)
    
    ## Calculate time to entry (left truncation) distribution at t-, use 2*prec in order to exclude censorings at same time
    # if(calc.trunc) surv.trunc <- survival::survfit(Surv(-Tstop,-(Tstart+2*prec),rep(1,n))~strata.num)
    ## trunc.dist <- summary(surv.trunc)
    ## trunc.dist$time <- rev(-trunc.dist$time)-prec
    ## trunc.dist$surv <- c(rev(trunc.dist$surv)[-1],1)
    
    ## Create weighted data set for each event type as specified in trans
    data.out <- vector("list",length(trans))
    i.list <- 1
    strat <- sort(unique(strata.num),na.last=TRUE)
    len.strat <- length(strat)
    ## Start weight calculation per event type
    for(failcode in trans) {
      if(len.strat==1){ # no strata
        data.weight <- create.wData.omega(Tstart, Tstop, status, num.id, 1, failcode, cens)
        tmp.time <- data.weight$Tstop
        data.weight$weight.cens[order(tmp.time)] <- summary(surv.cens, times=tmp.time-prec)$surv
        # if(calc.trunc) data.weight$weight.trunc[order(-tmp.time)] <- summary(surv.trunc, times=-tmp.time)$surv
      } else {
        data.weight <- vector("list",len.strat)
        if(is.na(strat[len.strat])) {
          tmp.sel <- is.na(strata.num)
          data.weight[[len.strat]] <- data.frame(id=num.id[tmp.sel], Tstart=Tstart[tmp.sel], Tstop=Tstop[tmp.sel], status=status[tmp.sel], strata=NA,  weight.cens=NA)
          # if(calc.trunc) data.weight[[len.strat]]$weight.trunc <- NA
        }
        for(tmp.strat in 1:(len.strat-is.na(strat[len.strat]))){
          tmp.sel <- !is.na(strata.num) & strata.num==tmp.strat
          data.weight[[tmp.strat]] <- create.wData.omega(Tstart[tmp.sel], Tstop[tmp.sel], status[tmp.sel], num.id[tmp.sel], tmp.strat, failcode, cens)
          tmp.time <- data.weight[[tmp.strat]]$Tstop
          data.weight[[tmp.strat]]$weight.cens[order(tmp.time)] <- summary(surv.cens[tmp.strat], times=tmp.time-prec)$surv
          # if(calc.trunc) data.weight[[tmp.strat]]$weight.trunc[order(-tmp.time)] <- summary(surv.trunc[tmp.strat], times=-tmp.time)$surv
        }
        data.weight <- do.call("rbind", data.weight)
      }
      ## Calculate omega-censoring weights
      data.weight <- data.weight[order(data.weight$id,data.weight$Tstop), ]
      data.weight$weight.cens <- unlist(tapply(data.weight$weight.cens, data.weight$id, FUN=function(x) if(length(x)==1&!is.na(x[1])) 1 else x/x[1]))
      
      ## Calculate omega-truncation weights
      ## check this!!! ##
      # if(calc.trunc) {
      #   data.weight$weight.trunc <- unlist(tapply(data.weight$weight.trunc, data.weight$id, FUN=function(x) if(length(x)==1&!is.na(x[1])) 1 else x/x[1]))
      # }
      
      tbl <- table(data.weight$id)
      
      ## Add covariates
      if(!missing(keep)) {
        ## Extract covariate name from function
        if (is.null(keep.name)) {
          m <- match.call(expand.dots = FALSE)
          m <- m[match("keep", names(m))]
          if(!is.null(m)) {
            keep.name <- as.character(m[1])
            keep.name.split <- strsplit(keep.name, '')[[1]]
            tag <- which(keep.name.split == '$')
            if(length(tag) != 0) {
              keep.name <- substring(keep.name, tag[length(tag)]+1)
            } else {
              tag <- which(keep.name.split == '"')
              if(length(tag) != 0) {
                keep.name <- substring(keep.name, tag[1]+1, tag[2]-1)
              }
            }
          }
        }
        
        ## Add covariates to the resultset
        if (nkeep > 0) {
          if (nkeep == 1) {
            keep <- keep[sel]
            ddcovs <- rep(keep, tbl)
            ddcovs <- as.data.frame(ddcovs)
            names(ddcovs) <- as.character(keep.name)
          } else {
            keep <- keep[sel, ]
            ddcovs <- lapply(1:nkeep, function(i) rep(as.matrix(keep[, i]), tbl))
            ddcovs <- as.data.frame(ddcovs)
            names(ddcovs) <- keep.name
          }
          data.weight <- cbind(data.weight, ddcovs)
        }
      }
      
      ## Shorten data set by combining rows with event types without censoring or truncation time in between
      ## check this !!! ##
      if (shorten) {
        # if(calc.trunc) {
        #   keep.rows <- with(data.weight, c(diff(id)!=0 | diff(weight.cens)!=0 | diff(weight.trunc)!=0, TRUE))
        # } else {
        keep.rows <- with(data.weight, c(diff(id)!=0 | diff(weight.cens)!=0, TRUE))
        # }
        ## First record always included as separate row in order to allow for CSH analysis
        keep.rows[unlist(mapply(seq,1,tbl))==1] <- TRUE
        keep.start <- data.weight$Tstart[unlist(tapply(keep.rows, data.weight$id, FUN=function(x) if(length(x)==1) x else c(TRUE,x[-length(x)])))]
        data.weight <- data.weight[keep.rows,]
        data.weight$Tstart <- keep.start
      }
      
      ## Recalculate tbl after shorten
      tbl <- table(data.weight$id)
      ## Add count
      data.weight$count <- unlist(mapply(seq,1,tbl))
      data.weight$failcode <- failcode
      ## Return to original id
      data.weight$id <- rep(id,tbl)
      
      data.out[[i.list]] <- data.weight
      i.list <- i.list+1
    }
    
    out <- do.call("rbind", data.out)
    
    return(out)
  }



# riskRegression::predictCox does not know how to handle frailty, manual calculation of risk for CSC is required
# this is adapted from https://github.com/survival-lumc/ValidationCompRisks/blob/main/sharing_CSC_model.R

predictLM_CSC <- function(model_info, # List object (see above)
                          predictors_list,
                          newdata, # Data.frame for which to make predictions
                          horizon, # Prediction time horizon (numeric)
                          primary_event) { # Primary event (numeric)
  
  # n_causes <- unique(vapply(model_info, length, FUN.VALUE = integer(1L)))
  n_causes <- length(model_info)
  # -- Absolute risk prediction
  
  causes_ind <- seq_len(n_causes)
  
  # Calculate linear predictors for all causes in new dataset
  # linpreds <- lapply(causes_ind, function(cause) {
  #   mod_matrix <- predict(fit_model$models[[cause]], newdata, type="lp", reference = "zero")
  # })
  
  linear_predictors <- vector("list", length(model_info$model_terms))
  
  for (i in seq_along(model_info$model_terms)) {
    terms <- model_info$model_terms[[i]]
    
    # Create design matrix based on list_of_predictors for each model
    des_matr <- as.data.frame(model.matrix(as.formula(paste0(" ~ ", paste0(predictors_list, collapse = " + "))), newdata))
    des_matr$`(Intercept)` <- NULL
    
    # Calculate linear predictors and store them
    linear_predictors[[i]] <- as.vector(as.matrix(des_matr) %*% cbind(model_info$coefficients[[i]]))
  }
  
  
  
  # Compute absolute risks for each individual
  preds <- vapply(seq_len(nrow(newdata)), function(id) {
    
    # Calculate individual-specific cause-specific hazards
    time_points <- model_info$baseline_hazards[[primary_event]][["time"]]
    hazards <- vapply(causes_ind, function(cause) {
      cumhaz <- model_info$baseline_hazards[[cause]][["hazard"]] * exp(linear_predictors[[cause]][[id]])
      diff(c(0, cumhaz))
    }, FUN.VALUE = numeric(length(time_points)))
    
    hazards <- cbind(hazards, time_points)
    lmsi <- newdata$LM[id]
    hazards<- hazards[hazards[,"time_points"] <= lmsi+horizon & hazards[,"time_points"] >= lmsi,]
    
    if (length(hazards) == 0) {
      return(0)
    }
    
    
    if (is.null(dim(hazards))) {
           hazards <- matrix(hazards, nrow = 1)
    }
    
    # Calculate event-free survival
    # this is using product integral estimator to calculate overall survival
    # however in occasional cases, the surv can be extreme or weird in our test data
    # surv <- cumprod(1 - rowSums(hazards[, 1:n_causes, drop = FALSE]))
    # thus, we change to use exponential approximation
    hazard_sum <- rowSums(hazards[,1:n_causes])
    cumulative_hazard <- cumsum(hazard_sum)
    surv <- exp(-cumulative_hazard)

    # Calculate cumulative incidence
    cuminc <- cumsum(hazards[, primary_event] * c(1, surv[-length(surv)]))
    cuminc_horizon <- tail(cuminc, n=1)
    
    return(cuminc_horizon)
  }, FUN.VALUE = numeric(1L))
}