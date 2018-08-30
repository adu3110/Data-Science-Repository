#' @export

MigrationMatrix <- function (M)
{
  if (!is.matrix(M))
    stop("M is not a matrix")
  if (dim(M)[1] < 2 || dim(M)[2] < 2 || dim(M)[1] != dim(M)[2])
    stop("incorrect dimension of migration matrix")
  if (length(M[M < 0]) != 0)
    stop("negative value in input data")
  if (length(M[M > 1]) != 0)
    stop("probabilities larger than 1 in input data")
  y <- numeric(dim(M)[1])
  for (i in 1:dim(M)[1]) {
    y[i] <- sum(M[i, ])
  }
  y <- round(y, digits = 12)
  if (length(y[y != 1]) != 0)
    stop("the sum of each row is not equal 1")
}


#' @export
CreditSpreads <- function (migration_matrix, lgd)
{
  MigrationMatrix(migration_matrix)
  if (lgd < 0 || lgd > 1)
    stop("loss given default not between 0 and 1")
  pd <- migration_matrix[1:(dim(migration_matrix)[1] - 1), dim(migration_matrix)[2]]
  cs <- -(log(1 - lgd * pd))
  return(cs)
}

#' @export
CreditVal <- function (M, lgd, ead, r, rating)
{
  MigrationMatrix(M)
  if (r < 0 || r > 1)
    stop("interest rate not between 0 and 1")
  if (length(ead) != length(rating))
    stop("vectors ead and rating should have the same length")
  cs <- CreditSpreads(M, lgd)
  y.constVal <- ead * exp(-(r + cs[rating]))
  y.constPV <- sum(y.constVal)
  list(constVal = y.constVal, constPV = y.constPV)
}


#' @export
MigrationQuantiles <- function (M)
{
  MigrationMatrix(M)
  mpRev <- M[1:(dim(M)[1] - 1), dim(M)[2]:1]
  cumMPRev <- t(apply(mpRev, 1, cumsum))
  q <- qnorm(cumMPRev)
  q <- cbind(-Inf, q)
  return(q)
}

#' @export
CreditRnorm <- function (N, n)
{
  if (N <= 0 || length(N) != 1)
    stop("N should be greater 0 and its length should be 1")
  if ((N <= 1 && n <= 1) || N > n)
    stop("N and n have no valid input data")
  X <- matrix(rnorm(N * n/2), N, n/2)
  Y <- cbind(X, -X)
  return(Y)
}


#' @export
CreditRnormCor <- function (N, n, rho)
{
  if (!is.matrix(rho))
    stop("rho is not a matrix")
  y <- eigen(rho)$values
  if (length(y[y < 0]) != 0)
    stop("rho should be positiv definit or positiv semidefinit")
  A <- t(chol(rho))
  Y <- CreditRnorm(N, n)
  Y <- A %*% Y
  return(Y)
}


#' @export
StateSpace <- function (M, lgd, ead, N, r)
{
  MigrationMatrix(M)
  if (r < 0 || r > 1)
    stop("interest rate not between 0 and 1")
  if (length(ead) != N)
    stop("the length of vector ead and N should be equal")
  cs <- CreditSpreads(M, lgd)
  cs2 <- matrix(rep(cs, N), N, dim(M)[1] - 1, byrow = T)
  ead2 <- matrix(rep(ead, dim(M)[1] - 1), N, dim(M)[1] - 1,
                 byrow = F)
  V <- ead2 * exp(-(r + cs2))
  V <- cbind(V, ead * (1 - lgd))
  return(V)
}


#' @export
Valuation <- function (M, lgd, ead, N, n, r, rho, rating)
{
  MigrationMatrix(M)
  if (length(ead) != length(rating))
    stop("vectors ead and rating should have the same length")
  if (!is.matrix(rho))
    stop("rho is not a matrix")
  y <- eigen(rho)$values
  if (length(y[y < 0]) != 0)
    stop("rho should be positiv definit or positiv semidefinit")
  V <- StateSpace(M, lgd, ead, N, r)
  simV <- matrix(0, N, n)
  q <- MigrationQuantiles(M)
  Y <- CreditRnormCor(N, n, rho)
  for (i in 1:N) {
    l <- q[rating[i], ]
    simClasses <- findInterval(Y[i, ], l)
    simClasses <- (simClasses - (dim(M)[1] + 1)) * (-1)
    simV[i, ] <- V[i, simClasses]
  }
  return(simV)
}


#' @export
Portfolio <- function (M, lgd, ead, N, n, r, rho, rating)
{
  MigrationMatrix(M)
  simV <- Valuation(M, lgd, ead, N, n, r, rho, rating)
  simPV <- colSums(simV)
  return(simPV)
}


#' @export
Gain <- function (M, lgd, ead, N, n, r, rho, rating)
{
  MigrationMatrix(M)
  constPV <- CreditVal(M, lgd, ead, r, rating)$constPV
  simPV <- Portfolio(M, lgd, ead, N, n, r, rho, rating)
  simGV <- simPV - constPV
  return(simGV)
}


#' @export
CreditValatRisk <- function (M, lgd, ead, N, n, r, rho, alpha, rating)
{
  MigrationMatrix(M)
  if (alpha < 0 || alpha > 1)
    stop("confidence level alpha not between 0 and 1")
  simGV <- Gain(M, lgd, ead, N, n, r, rho, rating)
  CVaR <- -quantile(simGV, 1 - alpha)
  return(CVaR)
}


#' @export
CreditHist <- function (M, lgd, ead, N, n, r, rho, rating, col = "steelblue4",
                        main = "Profit / Loss Distribution", xlab = "profit / loss",
                        ylab = "frequency")
{
  MigrationMatrix(M)
  simGV <- Gain(M, lgd, ead, N, n, r, rho, rating)
  b <- seq(min(simGV), max(simGV), length = (max(simGV) -
                                               min(simGV))/(2 * n))
  hist(simGV, main = main, xlab = xlab, ylab = ylab, breaks = b,
       col = col)
}


#' @export
BlackScholesMerton <- function (t, asset_price, risk_free_rate,
                                sigma, strike, maturity, type = c("call", "put"))
{
  d1 <- (log(asset_price/strike) + (risk_free_rate + sigma^2/2) * (maturity - t))/(sigma * sqrt(maturity -
                                                               t))
  d2 <- d1 - sigma * sqrt(maturity - t)
  type <- match.arg(type)
  switch(type, call = {
    asset_price * pnorm(d1) - strike * exp(-risk_free_rate * (maturity - t)) * pnorm(d2)
  }, put = {
    asset_price * (pnorm(d1) - 1) + strike * exp(-risk_free_rate * (maturity - t)) * (1 - pnorm(d2))
  }, stop("Wrong type"))
}


#' @export
MertonSpread <- function(leverage,tau,sigmaV)
{
  dt1 <- (-log(leverage) + 0.5*tau*sigmaV^2)/(sigmaV*sqrt(tau))
  dt2 <-	dt1-sigmaV*sqrt(tau)
  -log(pnorm(dt2)+pnorm(-dt1)/leverage)/tau
}


#' @export
VASICEKnegloglike<-function(param,data,times)
{ n.obs<-length(data)
dum<-sort.list(times)
data<-data[dum]
times<-times[dum]
delta<-diff(times,1)
mv<-data[1:(n.obs-1)]*exp(-param[2]*delta)+param[1]*(1-exp(-param[2]*delta))
variance<-param[3]^2*(1-exp(-2*param[2]*delta))/(2*param[2])
VASICEKnegloglike<--sum(log(dnorm(data[2:n.obs],mv,sqrt(variance))))/n.obs

}


#' @export
VASICEKyield<-function(r,tau,Pparam,riskpremium=0)
{ b<-Pparam[1]+riskpremium
a<-Pparam[2]
sig<-Pparam[3]
Btau<-(1-exp(-a*tau))/a
Atau<-((Btau-tau)*(a^2*b-0.5*sig^2)/a^2 - sig^2*Btau^2/(4*a))
VASICEKyield<-r*Btau/tau-Atau/tau
}
