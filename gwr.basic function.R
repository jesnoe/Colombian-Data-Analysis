function (formula, data, regression.points, bw, kernel = "bisquare", 
          adaptive = FALSE, p = 2, theta = 0, longlat = F, dMat, F123.test = F, 
          cv = F, W.vect = NULL, parallel.method = FALSE, parallel.arg = NULL) 
{
  timings <- list()
  timings[["start"]] <- Sys.time()
  this.call <- match.call()
  p4s <- as.character(NA)
  if (missing(regression.points)) {
    rp.given <- FALSE
    regression.points <- data
    rp.locat <- coordinates(data)
    hatmatrix <- T
  }
  else {
    rp.given <- TRUE
    hatmatrix <- F
    if (is(regression.points, "Spatial")) {
      rp.locat <- coordinates(regression.points)
    }
    else if (is.numeric(regression.points) && dim(regression.points)[2] == 
             2) 
      rp.locat <- regression.points
    else {
      warning("Output loactions are not packed in a Spatial object,and it has to be a two-column numeric vector")
      rp.locat <- dp.locat
    }
  }
  griddedObj <- F
  if (is(regression.points, "Spatial")) {
    if (is(regression.points, "SpatialPolygonsDataFrame")) 
      polygons <- polygons(regression.points)
    else griddedObj <- gridded(regression.points)
  }
  if (is(data, "Spatial")) {
    p4s <- proj4string(data)
    dp.locat <- coordinates(data)
    data <- as(data, "data.frame")
  }
  else {
    stop("Given regression data must be Spatial*DataFrame")
  }
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- model.extract(mf, "response")
  x <- model.matrix(mt, mf)
  var.n <- ncol(x)
  rp.n <- nrow(rp.locat)
  dp.n <- nrow(data)
  betas <- matrix(0, nrow = rp.n, ncol = var.n)
  if (hatmatrix) {
    betas.SE <- matrix(0, nrow = rp.n, ncol = var.n)
    betas.TV <- matrix(0, nrow = rp.n, ncol = var.n)
  }
  idx1 <- match("(Intercept)", colnames(x))
  if (!is.na(idx1)) 
    colnames(x)[idx1] <- "Intercept"
  colnames(betas) <- colnames(x)
  lms <- lm(formula, data = data)
  lms$x <- x
  lms$y <- y
  gTSS <- c(cov.wt(matrix(y, ncol = 1), wt = rep(as.numeric(1), 
                                                 dp.n), method = "ML")$cov * dp.n)
  DM.given <- F
  if (missing(dMat)) {
    DM.given <- F
    DM1.given <- F
    if (dp.n + rp.n <= 5000) {
      dMat <- gw.dist(dp.locat = dp.locat, rp.locat = rp.locat, 
                      p = p, theta = theta, longlat = longlat)
      DM.given <- T
    }
    else {
      dMat <- matrix(0, 1, 1)
    }
  }
  else {
    DM.given <- T
    DM1.given <- T
    dim.dMat <- dim(dMat)
    if (dim.dMat[1] != dp.n || dim.dMat[2] != rp.n) 
      stop("Dimensions of dMat are not correct")
  }
  timings[["calibration"]] <- Sys.time()
  s_hat <- c(0, 0)
  q.diag <- matrix(0, 1, dp.n)
  if (parallel.method == F) {
    reg.result <- gw_reg_all(x, y, dp.locat, rp.given, rp.locat, 
                             DM.given, dMat, hatmatrix, p, theta, longlat, bw, 
                             kernel, adaptive)
    betas = betas + reg.result$betas
    if (hatmatrix) {
      betas.SE = reg.result$betas.SE
      s_hat = reg.result$s_hat
      q.diag = reg.result$q.diag
    }
  }
  else if (parallel.method == "cuda") {
    if (missing(parallel.arg)) {
      groupl <- 16
    }
    else {
      groupl <- ifelse(is(parallel.arg, "numeric"), parallel.arg, 
                       16)
    }
    reg.result <- gw_reg_all_cuda(x, y, dp.locat, rp.given, 
                                  rp.locat, DM.given, dMat, hatmatrix, p, theta, longlat, 
                                  bw, kernel, adaptive, groupl)
    if (is(reg.result, "logical") && reg.result == FALSE) {
      stop("Some CUDA errors occured.")
    }
    else {
      betas = betas + reg.result$betas
      if (hatmatrix) {
        betas.SE = reg.result$betas.SE
        s_hat = reg.result$s_hat
        q.diag = reg.result$q.diag
      }
    }
  }
  else if (parallel.method == "omp") {
    if (missing(parallel.arg)) {
      threads <- 0
    }
    else {
      threads <- ifelse(is(parallel.arg, "numeric"), parallel.arg, 
                        0)
    }
    reg.result <- gw_reg_all_omp(x, y, dp.locat, rp.given, 
                                 rp.locat, DM.given, dMat, hatmatrix, p, theta, longlat, 
                                 bw, kernel, adaptive, threads)
    betas = betas + reg.result$betas
    if (hatmatrix) {
      betas.SE = reg.result$betas.SE
      s_hat = reg.result$s_hat
      q.diag = reg.result$q.diag
    }
  }
  else if (parallel.method == "cluster") {
    if (missing(parallel.arg)) {
      parallel.arg.n <- max(detectCores() - 4, 2)
      parallel.arg <- makeCluster(parallel.arg.n)
    }
    else parallel.arg.n <- length(parallel.arg)
    clusterCall(parallel.arg, function() {
      library(GWmodel)
    })
    parallel.arg.results <- clusterApplyLB(parallel.arg, 
                                           1:parallel.arg.n, function(group.i, parallel.arg.n, 
                                                                      x, y, dp.locat, rp.given, rp.locat, DM.given, 
                                                                      dMat, hatmatrix, p, theta, longlat, bw, kernel, 
                                                                      adaptive) {
                                             reg.result <- gw_reg_all(x, y, dp.locat, rp.given, 
                                                                      rp.locat, DM.given, dMat, hatmatrix, p, theta, 
                                                                      longlat, bw, kernel, adaptive, parallel.arg.n, 
                                                                      group.i)
                                             return(reg.result)
                                           }, parallel.arg.n, x, y, dp.locat, rp.given, rp.locat, 
                                           DM.given, dMat, hatmatrix, p, theta, longlat, bw, 
                                           kernel, adaptive)
    for (i in 1:parallel.arg.n) {
      reg.result <- parallel.arg.results[[i]]
      betas = betas + reg.result$betas
      if (hatmatrix) {
        betas.SE = betas.SE + reg.result$betas.SE
        s_hat = s_hat + reg.result$s_hat
        q.diag = q.diag + reg.result$q.diag
      }
    }
    if (missing(parallel.arg)) {
      stopCluster(parallel.arg)
    }
  }
  else {
    for (i in 1:rp.n) {
      if (DM.given) 
        dist.vi <- dMat[, i]
      else {
        if (rp.given) 
          dist.vi <- gw.dist(dp.locat, rp.locat, focus = i, 
                             p, theta, longlat)
        else dist.vi <- gw.dist(dp.locat = dp.locat, 
                                focus = i, p = p, theta = theta, longlat = longlat)
      }
      W.i <- gw.weight(dist.vi, bw, kernel, adaptive)
      if (!is.null(W.vect)) 
        W.i <- W.i * W.vect
      gwsi <- gw_reg(x, y, W.i, hatmatrix, i)
      betas[i, ] <- gwsi[[1]]
      if (hatmatrix) {
        si <- gwsi[[2]]
        Ci <- gwsi[[3]]
        betas.SE[i, ] <- rowSums(Ci * Ci)
        s_hat[1] = s_hat[1] + si[i]
        s_hat[2] = s_hat[2] + sum(si %*% t(si))
        onei <- numeric(rp.n)
        onei[i] = 1
        p_i = onei - si
        q.diag = q.diag + p_i * p_i
      }
    }
  }
  timings[["diagnostic"]] <- Sys.time()
  GW.diagnostic <- NA
  Ftests <- list()
  if (hatmatrix) {
    diags <- gwr_diag1(y, x, betas, as.vector(s_hat))
    tr.S <- s_hat[1]
    tr.StS <- s_hat[2]
    RSS.gw <- diags[5]
    yhat <- gw_fitted(x, betas)
    residual <- y - yhat
    CV <- numeric(dp.n)
    local.R2 <- numeric(dp.n)
    if (cv) 
      CV <- gwr.cv.contrib(bw, x, y, kernel, adaptive, 
                           dp.locat, p, theta, longlat, dMat)
    sigma.hat1 <- RSS.gw/(dp.n - 2 * tr.S + tr.StS)
    Stud_residual <- residual/sqrt(sigma.hat1 * as.vector(q.diag))
    betas.SE <- sqrt(sigma.hat1 * betas.SE)
    betas.TV <- betas/betas.SE
    dybar2 <- (y - mean(y))^2
    dyhat2 <- (y - yhat)^2
    if (DM.given) {
      W <- gw.weight(dMat, bw, kernel, adaptive)
      TSSw <- W %*% dybar2
      RSSw <- W %*% dyhat2
      local.R2 <- (TSSw - RSSw)/TSSw
    }
    else {
      dybar2 <- t(dybar2)
      dyhat2 <- t(dyhat2)
      local.R2 <- gw_local_r2(dp.locat, dybar2, dyhat2, 
                              DM.given, dMat, p, theta, longlat, bw, kernel, 
                              adaptive)
    }
    AIC <- diags[1]
    AICc <- diags[2]
    edf <- diags[3]
    enp <- diags[4]
    gw.R2 <- diags[6]
    gwR2.adj <- diags[7]
    BIC <- diags[8]
    GW.diagnostic <- list(RSS.gw = RSS.gw, AIC = AIC, AICc = AICc, 
                          enp = enp, edf = edf, gw.R2 = gw.R2, gwR2.adj = gwR2.adj, 
                          BIC = BIC)
    Ftests <- list()
    if (F123.test) {
      F.test.parameters <- list(dp.n = dp.n, var.n = var.n, 
                                dMat = dMat, dp.locat = dp.locat, x = x, bw = bw, 
                                adaptive = adaptive, kernel = kernel, betas = betas, 
                                RSS.lm = sum(lms$residuals^2), DF.lm = lms$df.residual, 
                                RSS.gw = RSS.gw, tr.S = tr.S, tr.StS = tr.StS, 
                                q.diag = q.diag, W.vect = W.vect, p = p, theta = theta, 
                                longlat = longlat)
      Ftests <- F1234.test(F.test.parameters)
    }
  }
  timings[["encapsulate"]] <- Sys.time()
  GW.arguments <- list(formula = formula, rp.given = rp.given, 
                       hatmatrix = hatmatrix, bw = bw, kernel = kernel, adaptive = adaptive, 
                       p = p, theta = theta, longlat = longlat, DM.given = DM1.given, 
                       F123.test = F123.test)
  if (hatmatrix) {
    if (is.null(W.vect)) {
      gwres.df <- data.frame(betas, y, yhat, residual, 
                             CV, Stud_residual, betas.SE, betas.TV, local.R2)
      colnames(gwres.df) <- c(c(c(colnames(betas), c("y", 
                                                     "yhat", "residual", "CV_Score", "Stud_residual")), 
                                paste(colnames(betas), "SE", sep = "_")), paste(colnames(betas), 
                                                                                "TV", sep = "_"), "Local_R2")
    }
    else {
      gwres.df <- data.frame(betas, y, yhat, residual, 
                             CV, Stud_residual, betas.SE, betas.TV, W.vect, 
                             local.R2)
      colnames(gwres.df) <- c(c(c(colnames(betas), c("y", 
                                                     "yhat", "residual", "CV_Score", "Stud_residual")), 
                                paste(colnames(betas), "SE", sep = "_")), paste(colnames(betas), 
                                                                                "TV", sep = "_"), "E_weigts", "Local_R2")
    }
  }
  else {
    if (is.null(W.vect)) 
      gwres.df <- data.frame(betas)
    else {
      gwres.df <- data.frame(betas, W.vect)
      colnames(gwres.df) <- c(colnames(betas), "E_weigts")
    }
  }
  rownames(rp.locat) <- rownames(gwres.df)
  if (is(regression.points, "SpatialPolygonsDataFrame")) {
    polygons <- polygons(regression.points)
    rownames(gwres.df) <- sapply(slot(polygons, "polygons"), 
                                 function(i) slot(i, "ID"))
    SDF <- SpatialPolygonsDataFrame(Sr = polygons, data = gwres.df, 
                                    match.ID = F)
  }
  else {
    SDF <- SpatialPointsDataFrame(coords = rp.locat, data = gwres.df, 
                                  proj4string = CRS(p4s), match.ID = F)
    if (griddedObj) 
      gridded(SDF) <- T
  }
  timings[["stop"]] <- Sys.time()
  res <- list(GW.arguments = GW.arguments, GW.diagnostic = GW.diagnostic, 
              lm = lms, SDF = SDF, timings = timings, this.call = this.call, 
              Ftests = Ftests)
  class(res) <- "gwrm"
  invisible(res)
}