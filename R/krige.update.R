krige.update <-
  function (krige.mat, mcmc.samples = 100, accepted.beta, accepted.nugget,
    accepted.decay, accepted.psill, y, X, east, north, powered.exp = 2,
    spatial.share = 0.5, range.share = 0.5, beta.var = 10, range.tol = 0.05,
    b.tune = 1, nugget.tune = 10, psill.tune = 1, message = NULL, save = NULL,
    time = NULL, checks = TRUE)
{
  if (checks == TRUE) {
    if (any(is.na(y)))
      stop("missing values in 'y' ")
    if (TRUE %in% apply(X, 2, function(x) any(is.na(x))))
      stop("missing values in 'X' ")
    if (any(is.na(east)))
      stop("missing values in 'east' ")
    if (any(is.na(north)))
      stop("missing values in 'north' ")
    if (powered.exp <= 0 | powered.exp > 2)
      stop("powered.exp must be greater than 0 and less than or equal to 2.")
    if (spatial.share <= 0 | spatial.share >= 1)
      stop("spatial.share must be between 0 and 1.")
    if (range.share <= 0)
      stop("range.share must be greater than 0.")
    if (range.tol <= 0 | range.tol >= 1)
      stop("p.range.tol must be between 0 and 1.")
    if (beta.var <= 0)
      stop("beta.var must be greater than 0.")
    if (b.tune <= 0)
      stop("b.tune must be greater than 0.")
    if (nugget.tune <= 0)
      stop("nugget.tune must be greater than 0.")
    if (psill.tune <= 0)
      stop("psill.tune must be greater than 0.")
  }
  simp.mvrnorm <- function(n = 1, mu, Sigma) {
    p <- length(mu)
    eS <- eigen(Sigma, symmetric = TRUE)
    ev <- eS$values
    X <- matrix(rnorm(p * n), n)
    X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)),
                                        p) %*% t(X)
    if (n == 1)
      drop(X)
    else t(X)
  }
  dist.mat <- k_distmat(cbind(east, north))
  max.distance <- max(dist.mat)
  min.distance <- min(dist.mat[dist.mat > 0])
  init.ols <- lm(y ~ X - 1)
  b.cand.var <- b.tune * vcov(init.ols)
  err.var <- var(resid(init.ols))
  beta.A <- round(100 * range.share/((-log(range.tol))^(1/powered.exp)))
  beta.B <- round(100 * (1 - (range.share/((-log(range.tol))^(1/powered.exp)))))
  full.mcmc <- mcmc.samples + nrow(krige.mat)
  mcmc.mat <- matrix(NA, nrow = full.mcmc, ncol = 3 + ncol(X))
  if (is.null(dimnames(X)[[2]])) {
    mynames <- rep(".", dim(X)[2])
    for (k in 1:dim(X)[2]) mynames[k] <- paste("V", k, sep = "")
    dimnames(X)[[2]] <- mynames
  }
  dimnames(mcmc.mat)[[2]] <- c("tau2", "phi", "sigma2", dimnames(X)[[2]])
  n.vars <- ncol(X)

  mcmc.mat[1:nrow(krige.mat),] <- krige.mat
  start <- nrow(krige.mat)
  local.Sigma <- NULL
  time0 <- Sys.time()
  for (i in start:(nrow(mcmc.mat)-1)) {
    ii <- i - start
    if (!is.null(save))
      if (ii > 0){
      if (ii%%save == 0) {
        save(mcmc.mat, file = "mcmc-mat.Rdata")
        cat(ii, " iterations saved", ".\n", sep = "")
      }}
    if (!is.null(time)) {
      if (ii %in% time) {
        time1 <- Sys.time()
        time.diff <- time1 - time0
        est.time <- (time.diff * (mcmc.samples - ii))/ii
        if (as.numeric(time.diff, units = "mins") > 120) {
          t.unit1 <- "hours"
        }
        else {
          t.unit1 <- "mins"
        }
        if (as.numeric(est.time, units = "mins") > 120) {
          t.unit2 <- "hours"
        }
        else {
          t.unit2 <- "mins"
        }
        cat(ii, " iterations ", "complete in ", round(as.numeric(time.diff, units = t.unit1), 2),
            " ", t.unit1, "; ", "the rest will take roughly ",
            round(as.numeric(est.time, units = t.unit2),
                  2), " ", t.unit2, ".\n", sep = "")
      }
    }
    if (is.null(message))
      message <- round(mcmc.samples, digits=-2)/10
    if (ii > 0){
    if (ii%%message == 0) {
      cat("Iteration ", ii, ": Nugget=", mcmc.mat[i, 1],
          ". Decay=", mcmc.mat[i, 2], ". Partial sill=",
          mcmc.mat[i, 3], ".\n", sep = "")
    }
    if (ii%%message == 0) {
      beta.rate.temp <- round(100 * accepted.beta/(i-1))
      tau2.rate.temp <- round(100 * accepted.nugget/(i-1))
      phi.rate.temp <- round(100 * accepted.decay/(i-1))
      sigma2.rate.temp <- round(100 * accepted.psill/(i-1))
      cat("Iteration ", ii, ": Acceptance percentages: Coefficients=",
          beta.rate.temp, "%. Nugget=", tau2.rate.temp,
          "%. Decay=", phi.rate.temp, "%. Partial sill=",
          sigma2.rate.temp, "%", ".\n", sep = "")
    }}
    old.beta <- mcmc.mat[i, 4:ncol(mcmc.mat)]
    old.var <- var(y - X %*% old.beta)
    beta <- simp.mvrnorm(n = 1, mu = old.beta, Sigma = b.cand.var)
    local.share <- mcmc.mat[i, 3]/(mcmc.mat[i, 1] + mcmc.mat[i, 3])
    local.tau2.shape <- 1 + nugget.tune/(1 - local.share)
    local.sigma2.shape <- 1 + psill.tune/local.share
    tau2 <- 1/rgamma(1, shape = local.tau2.shape, rate = nugget.tune *
                       old.var)
    sigma2 <- 1/rgamma(1, shape = local.sigma2.shape, rate = psill.tune *
                         old.var)
    phi <- 1/(max.distance * rbeta(1, shape1 = beta.A, shape2 = beta.B))
    if (is.null(local.Sigma))
    local.Sigma <- ifelse(dist.mat > 0, mcmc.mat[i, 3] *
                              exp(-abs(mcmc.mat[i, 2] * dist.mat)^powered.exp),
                            mcmc.mat[i, 1] + mcmc.mat[i, 3])
    current <- krige.posterior(mcmc.mat[i, 1], mcmc.mat[i,2], mcmc.mat[i, 3], old.beta, y, X, east, north,
                               semivar.exp = powered.exp, p.spatial.share = spatial.share,
                               p.range.share = range.share, p.range.tol = range.tol,
                               p.beta.var = beta.var, tot.var = err.var, local.Sigma = local.Sigma,
                               distance = dist.mat, max.distance = max.distance,
                               checks = checks)
    candidate.beta <- krige.posterior(mcmc.mat[i, 1], mcmc.mat[i,
                                                               2], mcmc.mat[i, 3], beta, y, X, east, north, semivar.exp = powered.exp,
                                      p.spatial.share = spatial.share, p.range.share = range.share,
                                      p.range.tol = range.tol, p.beta.var = beta.var, tot.var = err.var,
                                      local.Sigma = local.Sigma, distance = dist.mat, max.distance = max.distance,
                                      checks = checks)
    candidate.nugget <- krige.posterior(tau2, mcmc.mat[i,
                                                       2], mcmc.mat[i, 3], old.beta, y, X, east, north,
                                        semivar.exp = powered.exp, p.spatial.share = spatial.share,
                                        p.range.share = range.share, p.range.tol = range.tol,
                                        p.beta.var = beta.var, tot.var = err.var, local.Sigma = NULL,
                                        distance = dist.mat, max.distance = max.distance,
                                        checks = checks)
    candidate.decay <- krige.posterior(mcmc.mat[i, 1], phi,
                                       mcmc.mat[i, 3], old.beta, y, X, east, north, semivar.exp = powered.exp,
                                       p.spatial.share = spatial.share, p.range.share = range.share,
                                       p.range.tol = range.tol, p.beta.var = beta.var, tot.var = err.var,
                                       local.Sigma = NULL, distance = dist.mat, max.distance = max.distance,
                                       checks = checks)
    candidate.psill <- krige.posterior(mcmc.mat[i, 1], mcmc.mat[i, 2], sigma2, old.beta, y, X, east, north, semivar.exp = powered.exp,
                                       p.spatial.share = spatial.share, p.range.share = range.share,
                                       p.range.tol = range.tol, p.beta.var = beta.var, tot.var = err.var,
                                       local.Sigma = NULL, distance = dist.mat, max.distance = max.distance,
                                       checks = checks)
    a.beta <- exp(candidate.beta - current)
    a.nugget <- exp(candidate.nugget - current)
    a.decay <- exp(candidate.decay - current)
    a.psill <- exp(candidate.psill - current)
    if (a.beta > runif(1)) {
      accepted.beta <- accepted.beta + 1
      mcmc.mat[(i + 1), 4:ncol(mcmc.mat)] <- beta
    }
    else mcmc.mat[(i + 1), 4:ncol(mcmc.mat)] <- mcmc.mat[i, 4:ncol(mcmc.mat)]
    if (a.nugget > runif(1)) {
      accepted.nugget <- accepted.nugget + 1
      mcmc.mat[(i + 1), 1] <- tau2
      local.Sigma <- NULL
    }
    else mcmc.mat[(i + 1), 1] <- mcmc.mat[i, 1]
    if (a.decay > runif(1)) {
      accepted.decay <- accepted.decay + 1
      mcmc.mat[(i + 1), 2] <- phi
      local.Sigma <- NULL
    }
    else mcmc.mat[(i + 1), 2] <- mcmc.mat[i, 2]
    if (a.psill > runif(1)) {
      accepted.psill <- accepted.psill + 1
      mcmc.mat[(i + 1), 3] <- sigma2
      local.Sigma <- NULL
    }
    else mcmc.mat[(i + 1), 3] <- mcmc.mat[i, 3]
  }
  beta.rate <- round(100 * accepted.beta/(nrow(mcmc.mat) - 1))
  tau2.rate <- round(100 * accepted.nugget/(nrow(mcmc.mat) - 1))
  phi.rate <- round(100 * accepted.decay/(nrow(mcmc.mat) - 1))
  sigma2.rate <- round(100 * accepted.psill/(nrow(mcmc.mat) - 1))
  print(paste("Acceptance percentages: Coefficients=", beta.rate,
              "%. Nugget=", tau2.rate, "%. Decay=", phi.rate, "%. Partial sill=",
              sigma2.rate, "%.", sep = ""))
  if (beta.rate < 20)
    print("Coefficient acceptance rate is low, which can indicate slow mixing. You may need to run many iterations or consider adjusting the tuning parameter *b.tune* to increase acceptance.")
  if (beta.rate > 70)
    print("Coefficient acceptance rate is high. If coefficients are nonconvergent, consider adjusting the tuning parameter *b.tune* to reduce acceptance.")
  if (tau2.rate < 20)
    print("Nugget acceptance rate is low, which can indicate slow mixing. You may need to run many iterations or consider adjusting the tuning parameter *nugget.tune* to increase acceptance.")
  if (tau2.rate > 70)
    print("Nugget acceptance rate is high. If tau2 is nonconvergent, consider adjusting the tuning parameter *nugget.tune* to reduce acceptance.")
  if (phi.rate < 20)
    print("Decay term acceptance rate is low, which can indicate slow mixing. You may need to run many iterations or consider adjusting the tolerance parameter *range.tol* to increase acceptance.")
  if (phi.rate > 70)
    print("Decay term acceptance rate is high. If phi is nonconvergent, consider adjusting the tolerance parameter *range.tol* to reduce acceptance.")
  if (sigma2.rate < 20)
    print("Partial sill acceptance rate is low, which can indicate slow mixing. You may need to run many iterations or consider adjusting the tuning parameter *psill.tune* to increase acceptance.")
  if (sigma2.rate > 70)
    print("Partial sill acceptance rate is high. If sigma2 is nonconvergent, consider adjusting the tuning parameter *psill.tune* to reduce acceptance.")
  return(mcmc.mat)
  }
