# data=collinearity_data_sp; bw=bw_i; kernel = "boxcar"; adaptive = FALSE; p = 2; theta = 0; longlat = F
gwr.collin.diagno_i <- function (formula, data, bw, kernel = "bisquare", adaptive = FALSE, local_i,
          p = 2, theta = 0, longlat = F, dMat) {
  timings <- list()
  timings[["start"]] <- Sys.time()
  this.call <- match.call()
  p4s <- as.character(NA)
  polygons <- NULL
  griddedObj <- F
  if (is(data, "Spatial")) {
    p4s <- proj4string(data)
    if (is(data, "SpatialPolygonsDataFrame")) {
      polygons <- polygons(data)
      dp.locat <- coordinates(data)
    }
    else {
      dp.locat <- coordinates(data)
      griddedObj <- gridded(data)
    }
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
  idx1 <- match("(Intercept)", colnames(x))
  var.n <- ncol(x)
  if (!is.na(idx1)) {
    colnames(x)[idx1] <- "Intercept"
    x1 <- x[, -1]
  }
  else {
    x1 <- x
  }
  var.nms <- colnames(x)
  if (var.n <= 1) 
    stop("The number of independent variables must be larger than one")
  dp.n <- nrow(data)
  if (missing(dMat)) {
    DM.given <- F
    DM1.given <- F
    if (dp.n <= 5000) {
      dMat <- gw.dist(dp.locat = dp.locat, p = p, theta = theta, 
                      longlat = longlat)
      DM.given <- T
    }
  }
  else {
    DM.given <- T
    DM1.given <- T
    dim.dMat <- dim(dMat)
    if (dim.dMat[1] != dp.n || dim.dMat[2] != dp.n) 
      stop("Dimensions of dMat are not correct")
  }
  corr.nms <- c()
  corr.mat <- matrix(numeric((var.n - 1) * var.n * dp.n/2), 
                     nrow = dp.n)
  vifs.mat <- matrix(numeric((var.n - 1) * dp.n), nrow = dp.n)
  vdp.idx <- matrix(numeric(var.n * dp.n), nrow = dp.n)
  vdp.pi <- array(0, dim = c(dp.n, var.n, var.n))
  if (DM.given) 
    dist.vi <- dMat[, local_i]
  else {
    dist.vi <- gw.dist(dp.locat = dp.locat, focus = local_i, 
                       p = p, theta = theta, longlat = longlat)
  }
  W.i <- matrix(gw.weight(dist.vi, bw, kernel, adaptive), 
                nrow = 1)
  sum.w <- sum(W.i)
  Wi <- W.i/sum.w
  tag <- 0
  for (j in 1:(var.n - 1)) for (k in (j + 1):var.n) {
    tag <- tag + 1
    corr.mat[local_i, tag] <- cov.wt(cbind(x[, j], x[, k]), 
                               wt = Wi[1, ], cor = TRUE)$cor[1, 2]
  }
  corr.mati <- cov.wt(x1, wt = Wi[1, ], cor = TRUE)$cor
  vifs.mat[local_i, ] <- diag(solve(corr.mati))
  xw <- as.matrix(sweep(x, 1, Wi, "*"))
  svd.x <- svd(sweep(xw, 2, sqrt(colSums(xw^2)), "/"))
  vdp.idx[local_i, ] <- svd.x$d[1]/svd.x$d
  Phi = svd.x$v %*% diag(1/svd.x$d)
  Phi <- t(Phi^2)
  pi.ij <- prop.table(Phi, 2)
  vdp.pi[local_i, , ] <- pi.ij
  for (i in 1:(var.n - 1)) {
    for (j in (i + 1):var.n) {
      corr.v1v2 <- paste("Corr", paste(var.nms[i], var.nms[j], 
                                       sep = "."), sep = "_")
      corr.nms <- c(corr.nms, corr.v1v2)
    }
  }
  nm1 <- corr.nms
  nm2 <- paste(var.nms[-1], "VIF", sep = "_")
  local_CN <- vdp.idx[, var.n]
  VDP <- vdp.pi[, var.n, ]
  nm3 <- paste(var.nms, "VDP", sep = "_")
  res.df <- data.frame(vifs.mat, local_CN, VDP, corr.mat)
  colnames(res.df) <- c(nm2, "local_CN", nm3, nm1)
  if (!is.null(polygons)) {
    rownames(res.df) <- sapply(slot(polygons, "polygons"), 
                               function(i) slot(i, "ID"))
    SDF <- SpatialPolygonsDataFrame(Sr = polygons, data = res.df, 
                                    match.ID = F)
  }
  else {
    SDF <- SpatialPointsDataFrame(coords = dp.locat, data = res.df, 
                                  proj4string = CRS(p4s), match.ID = F)
    if (griddedObj) 
      gridded(SDF) <- griddedObj
  }
  res <- list()
  res$corr.mat <- corr.mat
  res$VIF <- vifs.mat
  res$local_CN <- local_CN
  res$VDP <- VDP
  res$SDF <- SDF
  timings[["stop"]] <- Sys.time()
  res
}
