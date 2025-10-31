

pass = function(){
}

split_code <- function(region, countries){
  del_index = c()
  names = c()
  for (i in c(1:length(region))){
    if (nchar(region[i]) < 1){
      del_index = c(del_index, i)
    } else if (nchar(region[i]) > 2){
      del_index = c(del_index, i)
      names = c(names, strsplit(region[i],',')[[1]])
    } else if (!(region[i] %in% countries)){
      del_index = c(del_index, i)
    } else {
      pass()
    }
  }
  if (length(del_index) > 0){
    return(c(region[-del_index], names))
  }
  return(region)
}

exponential.smooth <- function(x, lambda){
  if(length(lambda) > 1)
    stop("lambda must be a single number")
  if(lambda > 1 || lambda <= 0)
    stop("lambda must be between zero and one")
  xlam <- x * lambda
  xlam[1] <- x[1]
  stats::filter(xlam, filter = 1 - lambda, method = "rec")
}

### Helper functions

# myslice <- function(xx, K, start, end){
#   if (K==2){
#     return(xx[start:end,,,drop=FALSE])
#   } else if (K==3){
#     return(xx[start:end,,,,drop=FALSE])
#   } else {
#     stop("not support tensor mode K > 3")
#   }
# }

# mat projection
matAR.PROJ <- function(xx, dim, r, t){
  xx.mat <- matrix(xx,t,dim[1]*dim[2])
  kroneck <- t(xx.mat[2:t,]) %*% xx.mat[1:(t-1),] %*% solve(t(xx.mat[1:(t-1),]) %*% xx.mat[1:(t-1),])
  return(projection(kroneck, r, dim[1],dim[2],dim[1],dim[2]))
}

# Tensor Times List
tl <- function(x, list_mat, k = NULL){
  if (is.null(k)){
    tensor(tensor(tensor(x, list_mat[[1]], 2, 2), list_mat[[2]], 2, 2), list_mat[[3]], 2, 2)
  } else if (k == 1){
    tensor(tensor(x, list_mat[[1]], 3, 2), list_mat[[2]], 3, 2)
  } else if (k == 2){
    aperm(tensor(tensor(x, list_mat[[1]], 2, 2), list_mat[[2]], 3, 2),c(1,3,2,4))
  } else if (k == 3){
    aperm(tensor(tensor(x, list_mat[[1]], 2, 2), list_mat[[2]], 2, 2),c(1,3,4,2))
  } else {
    stop("not support tensor mode K > 3")
  }
  
}

# standard error extraction
covtosd <- function(cov, dim, R){
  K <- length(dim)
  P <- length(R)
  sd = list()
  for (p in c(1:P)){
    if (is.na(R[p])) stop("p != length(R)")
    if (R[p] == 0) next
    sd[[p]] <- lapply(1:R[p], function(j) {lapply(1:K, function(i) {list()})})
  }
  for (i in c(1:P)){
    for (j in c(1:R[i])){
      for (k in c(1:K)){
        left <- sum(dim^2)*sum(R[0:(i-1)]) + sum(dim^2)*(j-1) + sum((dim^2)[1:(k-1)])+1
        right <- sum(dim^2)*sum(R[0:(i-1)]) + sum(dim^2)*(j-1) + sum((dim^2)[1:k])
        sd[[i]][[j]][[k]] <- array(sqrt(diag(cov)[left:right]), c(dim[k], dim[k]))
      }
    }
  }
  return(sd)
}

# Permutation matrix em
em <- function(m,n,i,j){
  ## m,n,i,j set \eqn{m \times n} zero matrix with \eqn{A_{ij} = 1}
  ## return: Permutation matrix em such that \eqn{A_{ij} = 1} and other entries equals 0.
  mat <- matrix(0,m,n)
  mat[i,j] <- 1
  return(mat)
}

# Permutation matrix pm
pm <- function(m,n){
  ## m: an array of dimensions of matrices \eqn{A_1,A_2,\cdots,A_k}
  ## n: length of time
  ## return: Permutation matrix pm
  mat <- matrix(0,m*n,m*n)
  for (i in c(1:n)){
    for (j in c(1:m)){
      mat <- mat + kronecker(em(n,m,i,j),t(em(n,m,i,j)))
    }
  }
  return(mat)
}

# rearrangement operator for tensor
trearrange <- function(A,dim){
  m1 = dim[1]; m2 = dim[2]; m3 = dim[3]
  n1 = m1; n2 = m2; n3 = m3
  m <- nrow(A)
  n <- ncol(A)
  if(n!=n1*n2*n3 | m!=m1*m2*m3){
    stop("wrong dimention with your input Phi for rearrangement")
  }
  ans <- divide(A,m1,n1)
  dim <- c(m1*n1,m2*n2,m3*n3)
  t <- array(0, dim)
  for (i in c(1:m1)){
    for (j in c(1:n1)){
      t[(j-1)*m1+i,,] <- mrearrange(ans[[i]][[j]],m2,m3,n2,n3)
    }
  }
  return(t)
}


divide <- function(A,m,n){
  # the inner function of "trearrange"
  c <- dim(A)[1]/m
  l <- dim(A)[2]/n
  tmp <- lapply(1:m, function(i){
    lapply(1:n, function(j){
      A[((i-1)*c+1):(i*c),((j-1)*l+1):(j*l)]
    })
  })
  return(tmp)
}


mrearrange <- function(A,m1,m2,n1,n2){
  # the inner function of "projection"
  # A: m1m2*n1n2
  # B: m1*n1
  # C: m2*n2
  # A \approx B \otimes C
  # return RA
  m <- nrow(A)
  n <- ncol(A)
  if(n!=n1*n2 | m!=m1*m2){
    stop("error m")
  }
  ans <- matrix(NA, m1*n1, m2*n2)
  for(i in 1:m1){
    for(j in 1:n1){
      ans[(j-1)*m1+i,] <- t(as.vector(A[(i-1)*m2+1:m2,(j-1)*n2+1:n2]))
    }
  }
  return(ans)
}

projection <- function(M,r,m1,m2,n1,n2){
  # the inner function of MAR1.projection
  # M: m1m2*n1n2
  # B: m1*n1
  # C: m2*n2
  # M \approx B \otimes C
  # return B and C
  RA <- mrearrange(M,m1,m2,n1,n2)
  RA.svd <- svd(RA,nu=r,nv=r)
  A <- list()
  for (i in c(1:r)){
    A[[i]] <- list(matrix(RA.svd$v[,i] * RA.svd$d[i], m2, n2), matrix(RA.svd$u[,i], m1, n1))
  }
  for (j in c(1:r)){
    A[[j]] <- rev(A[[j]])
    a <- c()
    for (i in c(1:2)){
      m <- A[[j]][[i]]
      if (i != 2){
        a[i] <- svd(m,nu=0,nv=0)$d[1]
        A[[j]][[i]] <- m/a[i]
      } else {
        A[[j]][[i]] <- m * prod(a)
      }
    }
  }
  return(A)
}

ten.proj <- function(tt, dim, R){
  ## inner func of "TenAR.proj"
  cpd <- rTensor::cp(rTensor::as.tensor(tt), num_components = R, max_iter = 100, tol = 1e-06)
  lam <- cpd$lambdas
  A.proj <- list()
  for (j in c(1:R)){
    u1 <- cpd$U[[1]][,j]
    u2 <- cpd$U[[2]][,j]
    u3 <- cpd$U[[3]][,j]
    f1 <- sqrt(sum(cpd$U[[1]][,j]^2))
    f2 <- sqrt(sum(cpd$U[[2]][,j]^2))
    f3 <- sqrt(sum(cpd$U[[3]][,j]^2))
    a1 <- u1/f1
    a2 <- u2/f2
    a3 <- u3*f1*f2*lam[j]
    A.proj[[j]] <- list(matrix(a1,dim[1],dim[1]),
                        matrix(a2,dim[2],dim[2]),
                        matrix(a3,dim[3],dim[3]))
  }
  return(fro.order(A.proj))
}


fro.rescale <- function(A){
  r <- length(A)
  k <- length(A[[1]])
  for (j in c(1:r)){
    a <- c()
    for (i in c(1:k)){
      m <- A[[j]][[i]]
      if (i < k ){
        a[i] <- norm(m,"f")
        A[[j]][[i]] <- m/a[i]
      } else if (i == k){
        A[[j]][[i]] <- m * prod(a)
      } else {
        print("WRONG dimension")
      }
    }
  }
  return(A)
}

svd.rescale <- function(A){
  r <- length(A)
  k <- length(A[[1]])
  for (j in c(1:r)){
    a <- c()
    for (i in c(1:k)){
      m <- A[[j]][[i]]
      if (i < k ){
        a[i] <- svd(m,nu=0,nv=0)$d[1]
        A[[j]][[i]] <- m/a[i]
      } else if (i == k){
        A[[j]][[i]] <- m * prod(a)
      } else {
        print("WRONG dimension")
      }
    }
  }
  return(A)
}

eigen.rescale <- function(A){
  r <- length(A)
  k <- length(A[[1]])
  for (j in c(1:r)){
    a <- c()
    for (i in c(1:k)){
      m <- A[[j]][[i]]
      if (i < k ){
        a[i] <- eigen(m)$values[1]
        A[[j]][[i]] <- m/a[i]
      } else if (i == k){
        A[[j]][[i]] <- m * prod(a)
      } else {
        print("WRONG dimension")
      }
    }
  }
  return(A)
}

fro.order <- function(A){
  R <- length(A)
  K <- length(A[[1]])
  if (R == 1){return(A)}
  A.norm <- c()
  for (j in c(1:R)){
    A.norm[j] <- Reduce("*",lapply(c(1:K), function(k) { norm(A[[j]][[k]], 'f')}))
  }
  order.norm <- order(A.norm, decreasing=TRUE)
  A.temp <- A
  for (j in c(1:R)){
    A[[j]] <- A.temp[[order.norm[j]]]
  }
  return(A)
}

ten.dis.A <- function(A, B, R, K){
  P = length(R)
  dis <- 0
  for (p in c(1:P)){
    for (r in c(1:R[p])){
      for (k in c(1:K)){
        dis <- dis + min(sum((A[[p]][[r]][[k]] - B[[p]][[r]][[k]])^2), sum((A[[p]][[r]][[k]] + B[[p]][[r]][[k]])^2))
      }
    }
  }
  return(sqrt(dis))
}

ten.dis.phi <- function(phi.A, phi.B){
  P <- length(phi.A)
  dis <- 0
  for (i in c(1:P)){
    dis <- dis + sqrt(sum((phi.A[[i]] - phi.B[[i]])^2))
  }
  return(dis)
}


ten.res <- function(xx,A,P,R,K,t){
  L1 = 0
  for (l in c(1:P)){
    if (R[l] == 0) next
    L1 <- L1 + Reduce("+",lapply(c(1:R[l]), function(n) {rTensor::ttl(abind::asub(xx, (1+P-l):(t-l), 1, drop=FALSE), A[[l]][[n]], (c(1:K) + 1))}))
  }
  res <- abind::asub(xx, (1+P):(t), 1, drop=FALSE) - L1
  return(res)
}


M.eigen <- function(A, R, P, dim){
  phi <- list()
  PP = P
  for (i in c(1:P)){
    if (sum(R[i:length(R)]) == 0){
      PP = i-1
      break
    }
    if (R[i] == 0){
      phi[[i]] = pracma::zeros(prod(dim))
    } else {
      phi[[i]] <- Reduce("+", lapply(1:R[i], function(j) {rTensor::kronecker_list(rev(A[[i]][[j]]))}))
    }
    if (i == 1){M <- phi[[1]]} else {M <- cbind(M, phi[[i]])}
  }
  K <- dim(phi[[1]])[[1]]
  
  M <- rbind(M, cbind(diag(K*(PP-1)), array(0,c(K*(PP-1),K))))
  return(max(Mod(eigen(M, only.values = TRUE)$values)))
}

specRadius <- function(M){
  return(max(Mod(eigen(M, only.values = TRUE)$values)))
}

likelihood <- function(xx, A, Sigma){
  if (!(mode(xx) == "S4")) {xx <- as.tensor(xx)}
  r <- length(A[[1]])
  dd <- dim(xx)
  t <- dd[1]
  dim <- dd[-1]
  k <- length(dd[-1])
  i = 1
  res <- ten.res(xx,A,P=1,R=r,K=k,t=t)@data
  Sigma.inv <- lapply(1:k, function (i) {solve(Sigma[[i]])})
  ll <- tl(res, Sigma.inv)
  l1 <- sum(diag(tensor(ll, res, c(1:4)[-(i+1)],c(1:4)[-(i+1)])))
  l2 <- 0
  for (i in c(1:k)){
    l2 = l2 - prod(dim[-i]) * (t-1) * (log(det(Sigma[[i]])))
  }
  return((l2 - l1)/2)
}


initializer <- function(xx, k1=1, k2=1){
  
  dim = dim(xx)[-1]
  
  return(list(A1=0.5*diag(dim[1]),A2=0.5*diag(dim[2])))
  
  
  PROJ = MAR1.PROJ(xx)
  if (specRadius(PROJ$A1)*specRadius(PROJ$A2) < 1){
    return(list(A1=PROJ$A1,A2=PROJ$A2))
  }
  MAR = MAR1.LS(xx)
  if (specRadius(MAR$A1)*specRadius(MAR$A2) < 1){
    return(list(A1=MAR$A1,A2=MAR$A2))
  }
  RRMAR = MAR1.RR(xx, k1, k2)
  if (specRadius(MAR1.RR$A1)*specRadius(MAR1.RR$A2) < 1){
    return(list(A1=MAR1.RR$A1,A2=MAR1.RR$A2))
  }
  stop('causality condition of initializer fails.')
}



# initializer <- function(xx, k1=1, k2=1){
#   dim = dim(xx)[-1]
#   p = dim(xx)[2]
#   q = dim(xx)[3]
# 
#   PROJ = MAR1.PROJ(xx)
#   if (specRadius(PROJ$A1)*specRadius(PROJ$A2) < 1){
#     A1 = PROJ$A1; A2 = PROJ$A2
#     eps1 = matrix(rnorm(p^2, sd=sqrt(sum(A1^2))/(p^2)), ncol=p)
#     eps2 = matrix(rnorm(q^2, sd=sqrt(sum(A2^2))/(q^2)), ncol=q)
#     return(list(A1=PROJ$A1+eps1,A2=PROJ$A2+eps2))
#   }
#   MAR = MAR1.LS(xx)
#   if (specRadius(MAR$A1)*specRadius(MAR$A2) < 1){
#     A1 = MAR$A1; A2 = MAR$A2
#     eps1 = matrix(rnorm(p^2, sd=sqrt(sum(A1^2))/(p^2)), ncol=p)
#     eps2 = matrix(rnorm(q^2, sd=sqrt(sum(A2^2))/(q^2)), ncol=q)
#     return(list(A1=MAR$A1+eps1,A2=MAR$A2+eps2))
#   }
#   RRMAR = MAR1.RR(xx, k1, k2)
#   if (specRadius(MAR1.RR$A1)*specRadius(MAR1.RR$A2) < 1){
#     A1 = RRMAR$A1; A2 = RRMAR$A2
#     eps1 = matrix(rnorm(p^2, sd=sqrt(sum(A1^2))/(p^2)), ncol=p)
#     eps2 = matrix(rnorm(q^2, sd=sqrt(sum(A2^2))/(q^2)), ncol=q)
#     return(list(A1=MAR1.RR$A1+eps1,A2=MAR1.RR$A2+eps2))
#   }
#   stop('causality condition of initializer fails.')
# }

initializer.sig <- function(xx){
  dim = dim(xx)[-1]
  t = dim(xx)[1]
  res = tenAR.VAR(xx, P=1)$res
  SIGMA = res %*% t(res) / (t-1)
  sig = projection(SIGMA, 1, dim[1],dim[2],dim[1],dim[2])[[1]]
  if (sig[[1]][1,1] < 0){sig[[1]] = - sig[[1]]}
  if (sig[[2]][1,1] < 0){sig[[2]] = - sig[[2]]}
  return(list(Sigl.init=sig[[1]], Sigr.init=sig[[2]]))
}


likelihood.lse <- function(fres, s, d, t){
  l1 <- fres/2/s^2
  l2 <- -(t - 1)*d*log(2*pi*s^2)/2
  return(l2 - l1)
}


IC <- function(xx,res,r,t,dim){
  N <- prod(dim)
  ic <- log(sum((res)^2)/(N*t))/2 + sum(r)*log(t)/t
  return(ic)
}

###Functions of Autoregressive Models

#' Generate TenAR(p) tensor time series
#'
#' Simulate from the TenAR(p) model.
#'@name tenAR.sim
#'@rdname tenAR.sim
#'@aliases tenAR.sim
#'@export
#'@import tensor rTensor
#'@importFrom MASS ginv
#'@importFrom stats rnorm
#'@importFrom pracma randortho
#'@importFrom Matrix rankMatrix
#'@param t length of output series, a strictly positive integer.
#'@param dim dimension of the tensor at each time.
#'@param R Kronecker rank for each lag.
#'@param P autoregressive order.
#'@param rho spectral radius of coefficient matrix \eqn{\Phi}.
#'@param cov covariance matrix of the error term: diagonal ("iid"), separable ("mle"), random ("svd").
#'@param A coefficient matrices. If not provided, they are randomly generated according to given \code{dim}, \code{R}, \code{P} and \code{rho}.
#' It is a multi-layer list, the first layer for the lag \eqn{1 \le i \le P}, the second the term \eqn{1 \le r \le R}, and the third the mode \eqn{1 \le k \le K}.
#' See "Details" of \code{\link{tenAR.est}}.
#'@param Sig only if \code{cov=mle}, a list of initial values of \eqn{\Sigma_1,\ldots,\Sigma_K}. The default are identity matrices.
#'@return A tensor-valued time series generated by the TenAR(p) model.
#'@seealso \code{\link{tenFM.sim}}
#'@examples
#' set.seed(123)
#' dim <- c(3,3,3)
#' xx <- tenAR.sim(t=500, dim, R=2, P=1, rho=0.5, cov='iid')
tenAR.sim <- function(t, dim, R, P, rho, cov, A=NULL, Sig=NULL){
  if (missing(A) || is.null(A)){A <- tenAR.A(dim, R, P, rho)}
  K <- length(A[[1]][[1]])
  dim <- c()
  for (i in c(1:K)){
    dim[i] <- nrow(A[[1]][[1]][[i]])
  }
  x <- array(0, c(t+500,prod(dim)))
  for (l in c(1:P)){
    x[l,] <- rnorm(prod(dim))
  }
  
  if (cov == "mle"){
    if (missing(Sig) || is.null(Sig)){
      Sig.true <- lapply(1:K, function(i){
        Q <- pracma::randortho(dim[i])
        D <- abs(diag(rnorm(dim[i])))
        Q %*% D %*% t(Q)
      })
      Sig.true <- fro.rescale(list(Sig.true))[[1]]
      Sig.true.sqrt <- lapply(1:K, function(i){
        pracma::sqrtm(Sig.true[[i]])$B
      })
    } else {
      Sig.true = Sig
      Sig.true.sqrt <- lapply(1:K, function(i){
        pracma::sqrtm(Sig.true[[i]])$B
      })
    }
  }
  
  if (cov == "svd"){
    Q <- pracma::randortho(prod(dim))
    D <- sqrt(abs(diag(rnorm(prod(dim)))))
    E <- Q %*% D
  }
  
  for (i in c((P+1):(500 + t))){ # burning number = 500
    if (cov == "iid"){
      e <- rnorm(prod(dim), mean=0, sd=1)
    } else if (cov == "mle"){
      e <- kronecker_list(rev(Sig.true.sqrt)) %*% rnorm(prod(dim))
    } else if (cov == "svd"){
      e <- E %*% rnorm(prod(dim))
    } else {
      stop("Please specify cov")
    }
    temp = 0
    for (l in c(1:P)){
      if (R[l] == 0) next
      phi <-  Reduce("+", lapply(1:R[l], function(j) {rTensor::kronecker_list(rev(A[[l]][[j]]))}))
      temp = temp + phi %*% x[i-l, ]
    }
    x[i,] <-  temp + e
  }
  return(array(x[501:(500+t),], c(t, dim)))
}


#' Estimation for Autoregressive Model of Tensor-Valued Time Series
#'
#' Estimation function for tensor autoregressive models. Methods include
#' projection (PROJ), Least Squares (LSE), maximum likelihood estimation (MLE)
#' and vector autoregressive model (VAR), as determined by the value of \code{method}.
#'@details
#' Tensor autoregressive model (of autoregressive order one) has the form:
#' \deqn{X_t = \sum_{r=1}^R X_{t-1} \times_{1}  A_1^{(r)} \times_{2}  \cdots \times_{K} A_K^{(r)} + E_t,}
#' where \eqn{A_k^{(r)}} are \eqn{d_k \times d_k} coefficient matrices, \eqn{k=1,\cdots,K}, and \eqn{E_t} is a tensor white noise. \eqn{R} is the Kronecker rank.
#' The model of autoregressive order \eqn{P} takes the form
#' \deqn{X_t = \sum_{i=1}^{P} \sum_{r=1}^{R_i} X_{t-i} \times_{1} A_{1}^{(ir)} \times_{2}  \cdots \times_{K} A_{K}^{(ir)} + E_t.}
#' For the "MLE" method, we also assume,
#'  \deqn{\mathrm{Cov}(\mathrm{vec}(E_t))= \Sigma_K \otimes \Sigma_{K-1} \otimes \cdots \otimes \Sigma_1,}
#'@name tenAR.est
#'@rdname tenAR.est
#'@aliases tenAR.est
#'@usage tenAR.est(xx,R=1,P=1,method="LSE",init.A=NULL,init.sig=NULL,niter=150,tol=1e-6)
#'@export
#'@import tensor rTensor
#'@importFrom methods new
#'@param xx \eqn{T \times d_1 \times \cdots \times d_K} tensor-valued time series, \eqn{T} is the length of the series.
#'@param method character string, specifying the type of the estimation method to be used. \describe{
#'  \item{\code{"PROJ",}}{Projection method.}
#'  \item{\code{"LSE",}}{Least squares.}
#'  \item{\code{"MLE",}}{MLE under a separable cov(vec(\eqn{E_t})).}
#'  \item{\code{"VAR",}}{VAR(\eqn{P}) model for the \eqn{\mathrm{vec}(E_t)}.}
#'}
#'@param R Kronecker rank for each lag, a vector for \eqn{P>1}, a positive integer, it assumes same number of terms in each lag.
#'@param P Autoregressive order, a positive integer.
#'@param init.A initial values of coefficient matrices \eqn{A_k^{(ir)}} in estimation algorithms, which is a multi-layer list such that
#' the first layer for the lag \eqn{1 \le i \le P}, the second the term \eqn{1 \le r \le R}, and the third the mode \eqn{1 \le k \le K}.
#' See "Details". By default, we use PROJ estimators as initial values.
#'@param init.sig only if \code{method=MLE}, a list of initial values of \eqn{\Sigma_1,\ldots,\Sigma_K}. The default are identity matrices.
#'@param niter maximum number of iterations if error stays above \code{tol}.
#'@param tol error tolerance in terms of the Frobenius norm.
#'@return return a list containing the following:\describe{
#'\item{\code{A}}{a list of estimated coefficient matrices \eqn{A_k^{(ir)}}. It is a multi-layer list,
#' the first layer for the lag \eqn{1 \le i \le P}, the second the term \eqn{1 \le r \le R}, and the third the mode \eqn{1 \le k \le K}. See "Details".}
#'\item{\code{SIGMA}}{only if \code{method=MLE}, a list of estimated \eqn{\Sigma_1,\ldots,\Sigma_K}.}
#'\item{\code{res}}{residuals}
#'\item{\code{Sig}}{sample covariance matrix of the residuals vec(\eqn{\hat E_t}).}
#'\item{\code{cov}}{grand covariance matrix of all estimated entries of \eqn{A_k^{(ir)}}}
#'\item{\code{sd}}{standard errors of the coefficient matrices \eqn{A_k^{(ir)}}, returned as a list aligned with \code{A}.}
#'\item{\code{niter}}{number of iterations.}
#'\item{\code{BIC}}{value of extended Bayesian information criterion.}
#'}
#'@references
#'Rong Chen, Han Xiao, and Dan Yang. "Autoregressive models for matrix-valued time series". Journal of Econometrics, 2020.
#'
#'Zebang Li, Han Xiao. "Multi-linear tensor autoregressive models". arxiv preprint arxiv:2110.00928 (2021).
#'
#'@examples
#' set.seed(333)
#'
#' # case 1: tensor-valued time series
#'
#' dim <- c(2,2,2)
#' xx <- tenAR.sim(t=100, dim, R=2, P=1, rho=0.5, cov='iid')
#' est <- tenAR.est(xx, R=2, P=1, method="LSE") # two-term tenAR(1) model
#' A <- est$A # A is a multi-layer list
#'
#' length(A) == 1 # TRUE, since the order P = 1
#' length(A[[1]]) == 2 # TRUE, since the number of terms R = 2
#' length(A[[1]][[1]]) == 3 # TRUE, since the mode K = 3
#'
#' # est <- tenAR.est(xx, R=c(1,2), P=2, method="LSE") # tenAR(2) model with R1=1, R2=2
#'
#' # case 2: matrix-valued time series
#'
#' dim <- c(2,2)
#' xx <- tenAR.sim(t=100, dim, R=2, P=1, rho=0.5, cov='iid')
#' est <- tenAR.est(xx, R=2, P=1, method="LSE") # two-term MAR(1) model 
#' A <- est$A # A is a multi-layer list
#'
#' length(A) == 1 # TRUE, since the order P = 1
#' length(A[[1]]) == 2 # TRUE, since the number of terms R = 2
#' length(A[[1]][[1]]) == 2 # TRUE, since the mode K = 2
tenAR.est <- function(xx, R=1, P=1, method="LSE", init.A=NULL, init.sig=NULL, niter=150, tol=1e-6){
  if (identical("PROJ", method)) {
    tenAR.PROJ(xx, R, P)
  } else if (identical("LSE", method)) {
    tenAR.LS(xx, R, P, init.A, niter, tol, print.true=FALSE)
  } else if (identical("MLE", method)) {
    tenAR.MLE(xx, R, P, init.A, init.sig, niter, tol, print.true=FALSE)
  } else if (identical("VAR", method)) {
    tenAR.VAR(xx, P)
  } else {
    stop("Please specify the type you want to use. See manuals or run ?tenAR for details.")
  }
}


#' Estimation for Reduced Rank MAR(1) Model
#'
#' Estimation of the reduced rank MAR(1) model, using least squares (RRLSE) or MLE (RRMLE), as determined by the value of \code{method}.
#'@details
#' The reduced rank MAR(1) model takes the form:
#' \deqn{X_t =  A_1 X_{t-1} A_2^{^\top} + E_t,}
#' where \eqn{A_i} are \eqn{d_i \times d_i} coefficient matrices of ranks \eqn{\mathrm{rank}(A_i) = k_i \le d_i}, \eqn{i=1,2}. For the MLE method we also assume
#'  \deqn{\mathrm{Cov}(\mathrm{vec}(E_t))=\Sigma_2 \otimes \Sigma_1}
#'
#'@name matAR.RR.est
#'@rdname matAR.RR.est
#'@aliases matAR.RR.est
#'@usage matAR.RR.est(xx, method, A1.init=NULL, A2.init=NULL,Sig1.init=NULL,Sig2.init=NULL,
#'k1=NULL, k2=NULL, niter=200,tol=1e-4)
#'@export
#'@import tensor rTensor expm
#'@importFrom MASS ginv
#'@param xx \eqn{T \times d_1 \times d_2} matrix-valued time series, \eqn{T} is the length of the series.
#'@param method character string, specifying the method of the estimation to be used. \describe{
#'  \item{\code{"RRLSE",}}{Least squares.}
#'  \item{\code{"RRMLE",}}{MLE under a separable cov(vec(\eqn{E_t})).}
#'}
#'@param A1.init initial value of \eqn{A_1}. The default is the identity matrix.
#'@param A2.init  initial value of \eqn{A_2}. The default is the identity matrix.
#'@param Sig1.init only if \code{method=RRMLE}, initial value of \eqn{\Sigma_1}. The default is the identity matrix.
#'@param Sig2.init only if \code{method=RRMLE}, initial value of \eqn{\Sigma_2}. The default is the identity matrix.
#'@param k1  rank of \eqn{A_1}, a positive integer.
#'@param k2  rank of \eqn{A_2}, a positive integer.
#'@param niter maximum number of iterations if error stays above \code{tol}.
#'@param tol tolerance in terms of the Frobenius norm.
#'@param niter maximum number of iterations if error stays above \code{tol}.
#'@param tol relative Frobenius norm error tolerance.
#'@return return a list containing the following:\describe{
#'\item{\code{A1}}{estimator of \eqn{A_1}, a \eqn{d_1} by \eqn{d_1} matrix.}
#'\item{\code{A2}}{estimator of \eqn{A_2}, a \eqn{d_2} by \eqn{d_2} matrix.}
#'\item{\code{loading}}{a list of estimated \eqn{U_i}, \eqn{V_i}, 
#' where we write \eqn{A_i=U_iD_iV_i} as the singular value decomposition (SVD) of \eqn{A_i}, \eqn{i = 1,2}.}
#'\item{\code{Sig1}}{only if \code{method=MLE}, when \eqn{\mathrm{Cov}(\mathrm{vec}(E_t))=\Sigma_2 \otimes \Sigma_1}.}
#'\item{\code{Sig2}}{only if \code{method=MLE}, when \eqn{\mathrm{Cov}(\mathrm{vec}(E_t))=\Sigma_2 \otimes \Sigma_1}.}
#'\item{\code{res}}{residuals.}
#'\item{\code{Sig}}{sample covariance matrix of the residuals vec(\eqn{\hat E_t}).}
#'\item{\code{cov}}{a list containing \describe{
#'  \item{\code{Sigma}}{asymptotic covariance matrix of (vec( \eqn{\hat A_1}),vec(\eqn{\hat A_2^{\top}})).}
#'  \item{\code{Theta1.u}, \code{Theta1.v}}{asymptotic covariance matrix of vec(\eqn{\hat U_1}), vec(\eqn{\hat V_1}).}
#'  \item{\code{Theta2.u}, \code{Theta2.v}}{asymptotic covariance matrix of vec(\eqn{\hat U_2}), vec(\eqn{\hat V_2}).}
#'}}
#'\item{\code{sd.A1}}{element-wise standard errors of \eqn{\hat A_1}, aligned with \code{A1}.}
#'\item{\code{sd.A2}}{element-wise standard errors of \eqn{\hat A_2}, aligned with \code{A2}.}
#'\item{\code{niter}}{number of iterations.}
#'\item{\code{BIC}}{value of the extended Bayesian information criterion.}
#'}
#'@references
#'Reduced Rank Autoregressive Models for Matrix Time Series, by Han Xiao, Yuefeng Han, Rong Chen and Chengcheng Liu.
#'@examples
#' set.seed(333)
#' dim <- c(3,3)
#' xx <- tenAR.sim(t=500, dim, R=2, P=1, rho=0.5, cov='iid')
#' est <- matAR.RR.est(xx, method="RRLSE", k1=1, k2=1)
matAR.RR.est <- function(xx, method, A1.init=NULL, A2.init=NULL, Sig1.init=NULL, Sig2.init=NULL,k1=NULL, k2=NULL, niter=200,tol=1e-4){
  if (identical("PROJ", method)) {
    MAR1.PROJ(xx) # just keep it there
  } else if (identical("LSE", method)) {
    MAR1.LS(xx, niter=niter, tol=tol) # just keep it there
  } else if (identical("MLE", method)) {
    MAR1.MLE(xx, Sigl.init=Sig1.init,Sigr.init=Sig2.init, niter=niter, tol=tol) # just keep it there
  } else if (identical("VAR", method)) {
    tenAR.VAR(xx, P=1)
  } else if (identical("RRLSE", method)) {
    MAR1.RR(xx=xx, k1=k1, k2=k2, A1.init=A1.init, A2.init=A2.init, niter=niter, tol=tol)
  } else if (identical("RRMLE", method)) {
    MAR1.CC(xx=xx, k1=k1, k2=k2, A1.init=A1.init, A2.init=A2.init, Sigl.init=Sig1.init, Sigr.init=Sig2.init, niter=niter, tol=tol)
  } else {
    stop("Please specify the method in MAR.")
  }
}


#' Asymptotic Covariance Matrix of One-Term Reduced rank MAR(1) Model
#'
#' Asymptotic covariance matrix of the reduced rank MAR(1) model. If \code{Sigma1} and \code{Sigma2} is provided in input,
#' we assume a separable covariance matrix, Cov(vec(\eqn{E_t})) = \eqn{\Sigma_2 \otimes \Sigma_1}.
#'@name matAR.RR.se
#'@rdname matAR.RR.se
#'@aliases matAR.RR.se
#'@usage matAR.RR.se(A1,A2,k1,k2,method,Sigma.e=NULL,Sigma1=NULL,Sigma2=NULL,RU1=diag(k1),
#'RV1=diag(k1),RU2=diag(k2),RV2=diag(k2),mpower=100)
#'@import tensor rTensor expm
#'@importFrom MASS ginv
#'@export
#'@param A1 left coefficient matrix.
#'@param A2 right coefficient matrix.
#'@param k1 rank of \eqn{A_1}.
#'@param k2 rank of \eqn{A_2}.
#'@param method character string, specifying the method of the estimation to be used. \describe{
#'  \item{\code{"RRLSE",}}{Least squares.}
#'  \item{\code{"RRMLE",}}{MLE under a separable cov(vec(\eqn{E_t})).}
#'}
#'@param Sigma.e only if \code{method} = "RRLSE". Cov(vec(\eqn{E_t})) = Sigma.e: covariance matrix of dimension \eqn{(d_1 d_2) \times (d_1 d_2)}
#'@param Sigma1,Sigma2 only if \code{method} = "RRMLE". Cov(vec(\eqn{E_t})) = \eqn{\Sigma_2 \otimes \Sigma_1}. \eqn{\Sigma_i} is \eqn{d_i \times d_i}, \eqn{i=1,2}.
#'@param RU1,RV1,RU2,RV2 orthogonal rotations of \eqn{U_1,V_1,U_2,V_2}, e.g., new_U1=U1 \code{RU1}.
#'@param mpower truncate the VMA(\eqn{\infty}) representation of vec(\eqn{X_t}) at \code{mpower} for the purpose of calculating the autocovariances. The default is 100.
#'@return a list containing the following:\describe{
#'\item{\code{Sigma}}{asymptotic covariance matrix of (vec(\eqn{\hat A_1}),vec(\eqn{\hat A_2^T})).}
#'\item{\code{Theta1.u}}{asymptotic covariance matrix of vec(\eqn{\hat U_1}).}
#'\item{\code{Theta1.v}}{asymptotic covariance matrix of vec(\eqn{\hat V_1}).}
#'\item{\code{Theta2.u}}{asymptotic covariance matrix of vec(\eqn{\hat U_2}).}
#'\item{\code{Theta2.v}}{asymptotic covariance matrix of vec(\eqn{\hat V_2}).}
#'}
#'@references
#'Han Xiao, Yuefeng Han, Rong Chen and Chengcheng Liu, Reduced Rank Autoregressive Models for Matrix Time Series.
matAR.RR.se <- function(A1,A2,k1,k2,method,Sigma.e=NULL,Sigma1=NULL,Sigma2=NULL,RU1=diag(k1),RV1=diag(k1),RU2=diag(k2),RV2=diag(k2),mpower=100){
  if (method == "RRLSE"){
    return(MAR1.RRLS.SE(A1,A2,k1,k2,Sigma.e,RU1=diag(k1),RV1=diag(k1),RU2=diag(k2),RV2=diag(k2),mpower=100))
  } else if (method == "RRMLE") {
    return(MAR1.RRCC.SE(A1,A2,k1,k2,Sigma1,Sigma2,RU1=diag(k1),RV1=diag(k1),RU2=diag(k2),RV2=diag(k2),mpower=100))
  } else {
    stop("please specify method.")
  }
}


MAR1.RRLS.SE <- function(A1,A2,k1,k2,Sigma.e,RU1=diag(k1),RV1=diag(k1),RU2=diag(k2),RV2=diag(k2),mpower=100){
  # iterative least square
  # X_t = A1 X_{t-1} A2^T + E_t
  # RU1,RV1,RU2,RV2: rotation of U1,V1,U2,V2, e.g., new_U1=U1 RU1
  # Cov(vec(E_t)) = Sigma.e : of dimension d1d2\times d1d2
  # truncate vec(X_t) to mpower term, i.e. VMA(mpower)
  # return Sigma.LS: asymptotic covariance matrix of (vec(\hat A1),vec(\hat A2^T))
  #  Theta1.LS.u, Theta1.LS.v: asymptotic covariance matrix of vec(\hat U1), vec(\hat V1)
  #  Theta2.LS.u, Theta2.LS.v: asymptotic covariance matrix of vec(\hat U2), vec(\hat V2)
  d1 <- dim(A1)[1]
  d2 <- dim(A2)[1]
  Sigma.ee <- array(Sigma.e,c(d1,d2,d1,d2))
  B <- kronecker(A2,A1)
  #Sigma.x.vec, Sigma.xx: covariance matrix of vec(X_t)
  Sigma.x.vec <- Sigma.e
  for(i in 1:mpower){
    Sigma.x.vec <- Sigma.x.vec+(B%^%i)%*%Sigma.e%*%t(B%^%i)
  }
  Sigma.xx <- array(Sigma.x.vec,c(d1,d2,d1,d2))
  Gamma1 <- tensor(Sigma.xx,t(A1)%*%A1,c(1,3),c(1,2))
  Gamma2 <- tensor(Sigma.xx,t(A2)%*%A2,c(2,4),c(1,2))
  Gamma3 <- kronecker(diag(1,d1),A1)%*%matrix(aperm(Sigma.xx,c(3,1,4,2)),d1^2,d2^2)%*% kronecker(t(A2),diag(1,d2))
  H <- matrix(NA,d1^2+d2^2,d1^2+d2^2)
  H[1:d1^2,1:d1^2] <- as.vector(A1)%*%t(as.vector(A1)) + kronecker(Gamma2,diag(1,d1))
  H[1:d1^2,(d1^2+1):(d1^2+d2^2)] <- Gamma3
  H[(d1^2+1):(d1^2+d2^2),1:d1^2] <- t(Gamma3)
  H[(d1^2+1):(d1^2+d2^2),(d1^2+1):(d1^2+d2^2)] <- kronecker(diag(1,d2),Gamma1)
  H.inv <- solve(H)
  D1 <- Gamma2%*%t(A1)%*% ginv(A1%*%Gamma2%*%t(A1)) %*%A1
  D2 <- Gamma1%*%t(A2)%*% ginv(A2%*%Gamma1%*%t(A2)) %*%A2
  P1 <- A1%*%ginv(t(A1)%*%A1)%*%t(A1)
  P2 <- A2%*%ginv(t(A2)%*%A2)%*%t(A2)
  Sigma.Q <- matrix(NA,d1^2+d2^2,d1^2+d2^2)
  M1 <- kronecker(t(A2),P1)%*%Sigma.e%*%kronecker(A2,P1)
  M2 <- kronecker(t(A2),diag(d1)-P1)%*%Sigma.e%*%kronecker(A2,P1)
  M3 <- kronecker(t(A2),P1)%*%Sigma.e%*%kronecker(A2,diag(d1)-P1)
  M4 <- kronecker(t(A2),diag(d1)-P1)%*%Sigma.e%*%kronecker(A2,diag(d1)-P1)
  Sigma.Q1 <- Sigma.Q2 <- Sigma.Q3 <- Sigma.Q4 <- matrix(0,d1^2,d1^2)
  for(i in 1:d1){
    for(j in 1:d1){
      for(k in 1:d1){
        for(l in 1:d1){
          Sigma.Q1[(i-1)*d1+j,(k-1)*d1+l] <- sum(Sigma.xx[i,,k,] * M1[(1:d2-1)*d1+j,(1:d2-1)*d1+l])
          Sigma.Q2[(i-1)*d1+j,(k-1)*d1+l] <- sum(Sigma.xx[i,,k,] * M2[(1:d2-1)*d1+j,(1:d2-1)*d1+l])
          Sigma.Q3[(i-1)*d1+j,(k-1)*d1+l] <- sum(Sigma.xx[i,,k,] * M3[(1:d2-1)*d1+j,(1:d2-1)*d1+l])
          Sigma.Q4[(i-1)*d1+j,(k-1)*d1+l] <- sum(Sigma.xx[i,,k,] * M4[(1:d2-1)*d1+j,(1:d2-1)*d1+l])
        }
      }
    }
  }
  Sigma.Q[1:d1^2,1:d1^2] <- Sigma.Q1+kronecker(D1,diag(d1))%*%Sigma.Q2+Sigma.Q3%*%kronecker(t(D1),diag(d1))+
    kronecker(D1,diag(d1))%*%Sigma.Q4%*%kronecker(t(D1),diag(d1))
  M1 <- kronecker(P2,t(A1))%*%Sigma.e%*%kronecker(P2,A1)
  M2 <- kronecker(P2,t(A1))%*%Sigma.e%*%kronecker(diag(d2)-P2,A1)
  M3 <- kronecker(diag(d2)-P2,t(A1))%*%Sigma.e%*%kronecker(P2,A1)
  M4 <- kronecker(diag(d2)-P2,t(A1))%*%Sigma.e%*%kronecker(diag(d2)-P2,A1)
  Sigma.Q1 <- Sigma.Q2 <- Sigma.Q3 <- Sigma.Q4 <- matrix(0,d2^2,d2^2)
  for(i in 1:d2){
    for(j in 1:d2){
      for(k in 1:d2){
        for(l in 1:d2){
          Sigma.Q1[(i-1)*d2+j,(k-1)*d2+l] <- sum(Sigma.xx[,j,,l] * M1[(i-1)*d1+(1:d1),(k-1)*d1+(1:d1)])
          Sigma.Q2[(i-1)*d2+j,(k-1)*d2+l] <- sum(Sigma.xx[,j,,l] * M2[(i-1)*d1+(1:d1),(k-1)*d1+(1:d1)])
          Sigma.Q3[(i-1)*d2+j,(k-1)*d2+l] <- sum(Sigma.xx[,j,,l] * M3[(i-1)*d1+(1:d1),(k-1)*d1+(1:d1)])
          Sigma.Q4[(i-1)*d2+j,(k-1)*d2+l] <- sum(Sigma.xx[,j,,l] * M4[(i-1)*d1+(1:d1),(k-1)*d1+(1:d1)])
        }
      }
    }
  }
  Sigma.Q[(d1^2+1):(d1^2+d2^2),(d1^2+1):(d1^2+d2^2)] <- Sigma.Q1+Sigma.Q2%*%kronecker(diag(d2),t(D2))+
    kronecker(diag(d2),D2)%*%Sigma.Q3+kronecker(diag(d2),D2)%*%Sigma.Q4%*%kronecker(diag(d2),t(D2))
  M1 <- kronecker(t(A2),P1)%*%Sigma.e%*%kronecker(P2,A1)
  M2 <- kronecker(t(A2),P1)%*%Sigma.e%*%kronecker(diag(d2)-P2,A1)
  M3 <- kronecker(t(A2),diag(d1)-P1)%*%Sigma.e%*%kronecker(P2,A1)
  M4 <- kronecker(t(A2),diag(d1)-P1)%*%Sigma.e%*%kronecker(diag(d2)-P2,A1)
  Sigma.Q1 <- Sigma.Q2 <- Sigma.Q3 <- Sigma.Q4 <- matrix(0,d1^2,d2^2)
  for(i in 1:d1){
    for(j in 1:d1){
      for(k in 1:d2){
        for(l in 1:d2){
          Sigma.Q1[(i-1)*d1+j,(k-1)*d2+l] <- sum(Sigma.xx[i,,,l] * M1[(1:d2-1)*d1+j,(k-1)*d1+(1:d1)])
          Sigma.Q2[(i-1)*d1+j,(k-1)*d2+l] <- sum(Sigma.xx[i,,,l] * M2[(1:d2-1)*d1+j,(k-1)*d1+(1:d1)])
          Sigma.Q3[(i-1)*d1+j,(k-1)*d2+l] <- sum(Sigma.xx[i,,,l] * M3[(1:d2-1)*d1+j,(k-1)*d1+(1:d1)])
          Sigma.Q4[(i-1)*d1+j,(k-1)*d2+l] <- sum(Sigma.xx[i,,,l] * M4[(1:d2-1)*d1+j,(k-1)*d1+(1:d1)])
        }
      }
    }
  }
  Sigma.Q[1:d1^2,(d1^2+1):(d1^2+d2^2)] <- Sigma.Q1+Sigma.Q2%*%kronecker(diag(d2),t(D2))+
    kronecker(D1,diag(d1))%*%Sigma.Q3+kronecker(D1,diag(d1))%*%Sigma.Q4%*%kronecker(diag(d2),t(D2))
  Sigma.Q[(d1^2+1):(d1^2+d2^2),1:d1^2] <- t(Sigma.Q[1:d1^2,(d1^2+1):(d1^2+d2^2)])
  Sigma.LS <- H.inv%*%Sigma.Q%*%H.inv
  Sigma.LS11 <- Sigma.LS[1:d1^2,1:d1^2]
  Sigma.LS11.t <- array(Sigma.LS11,c(d1,d1,d1,d1))
  Sigma.LS11.t <- aperm(Sigma.LS11.t,c(2,1,4,3))
  Sigma.LS11.t <- matrix(Sigma.LS11.t, d1^2, d1^2)
  Sigma.LS22.t <- Sigma.LS[(d1^2+1):(d1^2+d2^2),(d1^2+1):(d1^2+d2^2)]
  Sigma.LS22 <- array(Sigma.LS22.t,c(d2,d2,d2,d2))
  Sigma.LS22 <- aperm(Sigma.LS22,c(2,1,4,3))
  Sigma.LS22 <- matrix(Sigma.LS22, d2^2, d2^2)
  svd.A1 <- svd(A1)
  #k1 <- length(svd.A1$d[svd.A1$d>1e-10])
  D1 <- diag(c(svd.A1$d[1:k1],1))[1:k1,1:k1]
  U1 <- svd.A1$u[,1:k1]
  V1 <- svd.A1$v[,1:k1]
  if(k1<d1){
    U1c <- svd.A1$u[,(k1+1):d1]
    V1c <- svd.A1$v[,(k1+1):d1]
  }else if(k1==d1){
    U1c <- 0
    V1c <- 0
  }
  #U1c <- svd.A1$u[,(k1+1):d1]  # original version : didn't consider if k1=d1
  #V1c <- svd.A1$v[,(k1+1):d1]
  e1 <- diag(d1)
  J1 <- matrix(0,d1^2,d1^2)
  for(i in 1:d1){
    J1[,((i-1)*d1+1):(i*d1)] <- kronecker(diag(d1),e1[,i])
  }
  C1 <- kronecker(A1,diag(d1)) + kronecker(diag(d1),A1)%*%J1
  Sigma.LS.1u <- C1%*%Sigma.LS11%*%t(C1)
  C1.t <- kronecker(t(A1),diag(d1)) + kronecker(diag(d1),t(A1))%*%J1
  Sigma.LS.1v <- C1.t%*%Sigma.LS11.t%*%t(C1.t)
  e1 <- diag(k1)
  L1 <- matrix(0,k1^2,k1)
  for(i in 1:k1){
    L1[,i] <- kronecker(e1[,i],e1[,i])
  }
  
  if(k1<d1){
    R1u <- cbind(kronecker(diag(k1),U1), kronecker(diag(k1),U1c)) %*% rbind(solve(kronecker(D1^2,diag(k1)) -
                                                                                    kronecker(diag(k1),D1^2)+L1%*%t(L1)) %*% (diag(k1^2)-L1%*%t(L1)) %*% kronecker(t(U1),t(U1)),
                                                                            kronecker(diag(1/c(svd.A1$d[1:k1],1))[1:k1,1:k1]^2%*%t(U1), t(U1c))  )
    R1v <- cbind(kronecker(diag(k1),V1), kronecker(diag(k1),V1c)) %*% rbind(solve(kronecker(D1^2,diag(k1)) -
                                                                                    kronecker(diag(k1),D1^2)+L1%*%t(L1)) %*% (diag(k1^2)-L1%*%t(L1)) %*% kronecker(t(V1),t(V1)),
                                                                            kronecker(diag(1/c(svd.A1$d[1:k1],1))[1:k1,1:k1]^2%*%t(V1), t(V1c))  )
  }else if(k1==d1){
    R1u <- kronecker(diag(k1),U1) %*% solve(kronecker(D1^2,diag(k1)) -
                                              kronecker(diag(k1),D1^2)+L1%*%t(L1)) %*% (diag(k1^2)-L1%*%t(L1)) %*% kronecker(t(U1),t(U1))
    R1v <- kronecker(diag(k1),V1) %*% solve(kronecker(D1^2,diag(k1)) -
                                              kronecker(diag(k1),D1^2)+L1%*%t(L1)) %*% (diag(k1^2)-L1%*%t(L1)) %*% kronecker(t(V1),t(V1))
    
  }
  
  # R1u <- cbind(kronecker(diag(k1),U1), kronecker(diag(k1),U1c)) %*% rbind(solve(kronecker(D1^2,diag(k1)) -
  #                                                                                 kronecker(diag(k1),D1^2)+L1%*%t(L1)) %*% (diag(k1^2)-L1%*%t(L1)) %*% kronecker(t(U1),t(U1)),
  #                                                                         kronecker(diag(1/c(svd.A1$d[1:k1],1))[1:k1,1:k1]^2%*%t(U1), t(U1c))  )
  # R1v <- cbind(kronecker(diag(k1),V1), kronecker(diag(k1),V1c)) %*% rbind(solve(kronecker(D1^2,diag(k1)) -
  #                                                                                 kronecker(diag(k1),D1^2)+L1%*%t(L1)) %*% (diag(k1^2)-L1%*%t(L1)) %*% kronecker(t(V1),t(V1)),
  #                                                                         kronecker(diag(1/c(svd.A1$d[1:k1],1))[1:k1,1:k1]^2%*%t(V1), t(V1c))  )
  Theta1.LS.u <- R1u%*% Sigma.LS.1u %*%t(R1u)
  Theta1.LS.v <- R1v%*% Sigma.LS.1v %*%t(R1v)
  Theta1.LS.u <- kronecker(t(RU1),diag(d1))%*%Theta1.LS.u
  Theta1.LS.v <- kronecker(t(RV1),diag(d1))%*%Theta1.LS.v
  svd.A2 <- svd(A2)
  #k2 <- length(svd.A2$d[svd.A2$d>1e-10])
  D2 <- diag(c(svd.A2$d[1:k2],1))[1:k2,1:k2]
  U2 <- svd.A2$u[,1:k2]
  V2 <- svd.A2$v[,1:k2]
  if(k2<d2){
    U2c <- svd.A2$u[,(k2+1):d2]
    V2c <- svd.A2$v[,(k2+1):d2]
  }else if(k2==d2){
    U2c <- 0
    V2c <- 0
  }
  #U2c <- svd.A2$u[,(k2+1):d2]
  #V2c <- svd.A2$v[,(k2+1):d2]
  e2 <- diag(d2)
  J2 <- matrix(0,d2^2,d2^2)
  for(i in 1:d2){
    J2[,((i-1)*d2+1):(i*d2)] <- kronecker(diag(d2),e2[,i])
  }
  C2 <- kronecker(A2,diag(d2)) + kronecker(diag(d2),A2)%*%J2
  Sigma.LS.2u <- C2%*%Sigma.LS22%*%t(C2)
  C2.t <- kronecker(t(A2),diag(d2)) + kronecker(diag(d2),t(A2))%*%J2
  Sigma.LS.2v <- C2.t%*%Sigma.LS22.t%*%t(C2.t)
  e2 <- diag(k2)
  L2 <- matrix(0,k2^2,k2)
  for(i in 1:k2){
    L2[,i] <- kronecker(e2[,i],e2[,i])
  }
  
  
  if(k2<d2){
    R2u <- cbind(kronecker(diag(k2),U2), kronecker(diag(k2),U2c)) %*% rbind(solve(kronecker(D2^2,diag(k2)) -
                                                                                    kronecker(diag(k2),D2^2)+L2%*%t(L2)) %*% (diag(k2^2)-L2%*%t(L2)) %*% kronecker(t(U2),t(U2)),
                                                                            kronecker(diag(1/c(svd.A2$d[1:k2],1))[1:k2,1:k2]^2%*%t(U2), t(U2c))  )
    R2v <- cbind(kronecker(diag(k2),V2), kronecker(diag(k2),V2c)) %*% rbind(solve(kronecker(D2^2,diag(k2)) -
                                                                                    kronecker(diag(k2),D2^2)+L2%*%t(L2)) %*% (diag(k2^2)-L2%*%t(L2)) %*% kronecker(t(V2),t(V2)),
                                                                            kronecker(diag(1/c(svd.A2$d[1:k2],1))[1:k2,1:k2]^2%*%t(V2), t(V2c))  )
  }else if(k2==d2){
    R2u <- kronecker(diag(k2),U2) %*% solve(kronecker(D2^2,diag(k2)) -
                                              kronecker(diag(k2),D2^2)+L2%*%t(L2)) %*% (diag(k2^2)-L2%*%t(L2)) %*% kronecker(t(U2),t(U2))
    R2v <- kronecker(diag(k2),V2) %*% solve(kronecker(D2^2,diag(k2)) -
                                              kronecker(diag(k2),D2^2)+L2%*%t(L2)) %*% (diag(k2^2)-L2%*%t(L2)) %*% kronecker(t(V2),t(V2))
    
  }
  
  
  # R2u <- cbind(kronecker(diag(k2),U2), kronecker(diag(k2),U2c)) %*% rbind(solve(kronecker(D2^2,diag(k2)) -
  #                                                                                 kronecker(diag(k2),D2^2)+L2%*%t(L2)) %*% (diag(k2^2)-L2%*%t(L2)) %*% kronecker(t(U2),t(U2)),
  #                                                                         kronecker(diag(1/c(svd.A2$d[1:k2],1))[1:k2,1:k2]^2%*%t(U2), t(U2c))  )
  # R2v <- cbind(kronecker(diag(k2),V2), kronecker(diag(k2),V2c)) %*% rbind(solve(kronecker(D2^2,diag(k2)) -
  #                                                                                 kronecker(diag(k2),D2^2)+L2%*%t(L2)) %*% (diag(k2^2)-L2%*%t(L2)) %*% kronecker(t(V2),t(V2)),
  #                                                                         kronecker(diag(1/c(svd.A2$d[1:k2],1))[1:k2,1:k2]^2%*%t(V2), t(V2c))  )
  Theta2.LS.u <- R2u%*% Sigma.LS.2u %*%t(R2u)
  Theta2.LS.v <- R2v%*% Sigma.LS.2v %*%t(R2v)
  Theta2.LS.u <- kronecker(t(RU2),diag(d2))%*%Theta2.LS.u
  Theta2.LS.v <- kronecker(t(RV2),diag(d2))%*%Theta2.LS.v
  return(list("Sigma"=Sigma.LS,"Theta1.u"=Theta1.LS.u,"Theta1.v"=Theta1.LS.v,
              "Theta2.u"=Theta2.LS.u,"Theta2.v"=Theta2.LS.v))
}


MAR1.RRCC.SE <- function(A1,A2,k1,k2,Sigma1,Sigma2,RU1=diag(k1),RV1=diag(k1),RU2=diag(k2),RV2=diag(k2),mpower=100){
  # canonical correlation analysis
  # X_t = A1 X_{t-1} A2^T + E_t
  # RU1,RV1,RU2,RV2: rotation of U1,V1,U2,V2, e.g., new_U1=U1 RU1
  # Cov(vec(E_t)) = Sigma.e=Sigma2 \otimes \Sigma1 : of dimension d1d2\times d1d2
  # truncate vec(X_t) to mpower term, i.e. VMA(mpower)
  # return Sigma.CC: asymptotic covariance matrix of (vec(\hat A1),vec(\hat A2^T))
  #  Theta1.CC.u, Theta1.CC.v: asymptotic covariance matrix of vec(\hat U1), vec(\hat V1)
  #  Theta2.CC.u, Theta2.CC.v: asymptotic covariance matrix of vec(\hat U2), vec(\hat V2)
  d1 <- dim(A1)[1]
  d2 <- dim(A2)[1]
  Sigma1.inv=solve(Sigma1)
  Sigma2.inv=solve(Sigma2)
  Sigma.e=kronecker(Sigma2,Sigma1)
  Sigma.e.inv=kronecker(Sigma2.inv,Sigma1.inv)
  Sigma.ee <- array(Sigma.e,c(d1,d2,d1,d2))
  B <- kronecker(A2,A1)
  #Sigma.x.vec, Sigma.xx: covariance matrix of vec(X_t)
  Sigma.x.vec <- Sigma.e
  for(i in 1:mpower){
    Sigma.x.vec <- Sigma.x.vec+(B%^%i)%*%Sigma.e%*%t(B%^%i)
  }
  Sigma.xx <- array(Sigma.x.vec,c(d1,d2,d1,d2))
  Gamma1 <- tensor(Sigma.xx,t(A1)%*%Sigma1.inv%*%A1,c(1,3),c(1,2))
  Gamma2 <- tensor(Sigma.xx,t(A2)%*%Sigma2.inv%*%A2,c(2,4),c(1,2))
  Gamma3 <- kronecker(diag(1,d1),Sigma1.inv%*%A1)%*%matrix(aperm(Sigma.xx,c(3,1,4,2)),d1^2,d2^2)%*%
    kronecker(t(A2)%*%Sigma2.inv,diag(1,d2))
  H <- matrix(NA,d1^2+d2^2,d1^2+d2^2)
  H[1:d1^2,1:d1^2] <- as.vector(A1)%*%t(as.vector(A1)) + kronecker(Gamma2,Sigma1.inv)
  H[1:d1^2,(d1^2+1):(d1^2+d2^2)] <- Gamma3
  H[(d1^2+1):(d1^2+d2^2),1:d1^2] <- t(Gamma3)
  H[(d1^2+1):(d1^2+d2^2),(d1^2+1):(d1^2+d2^2)] <- kronecker(Sigma2.inv,Gamma1)
  H.inv <- solve(H)
  D1 <- Gamma2%*%t(A1)%*% ginv(A1%*%Gamma2%*%t(A1)) %*%A1
  D2 <- Gamma1%*%t(A2)%*% ginv(A2%*%Gamma1%*%t(A2)) %*%A2
  P1 <- Sigma1.inv%*%A1%*%ginv(t(A1)%*%Sigma1.inv%*%A1)%*%t(A1)
  P2 <- Sigma2.inv%*%A2%*%ginv(t(A2)%*%Sigma2.inv%*%A2)%*%t(A2)
  Sigma.Q <- matrix(NA,d1^2+d2^2,d1^2+d2^2)
  M1 <- kronecker(t(A2),P1)%*%Sigma.e.inv%*%kronecker(A2,P1)
  M2 <- kronecker(t(A2),diag(d1)-P1)%*%Sigma.e.inv%*%kronecker(A2,P1)
  M3 <- kronecker(t(A2),P1)%*%Sigma.e.inv%*%kronecker(A2,diag(d1)-P1)
  M4 <- kronecker(t(A2),diag(d1)-P1)%*%Sigma.e.inv%*%kronecker(A2,diag(d1)-P1)
  Sigma.Q1 <- Sigma.Q2 <- Sigma.Q3 <- Sigma.Q4 <- matrix(0,d1^2,d1^2)
  for(i in 1:d1){
    for(j in 1:d1){
      for(k in 1:d1){
        for(l in 1:d1){
          Sigma.Q1[(i-1)*d1+j,(k-1)*d1+l] <- sum(Sigma.xx[i,,k,] * M1[(1:d2-1)*d1+j,(1:d2-1)*d1+l])
          Sigma.Q2[(i-1)*d1+j,(k-1)*d1+l] <- sum(Sigma.xx[i,,k,] * M2[(1:d2-1)*d1+j,(1:d2-1)*d1+l])
          Sigma.Q3[(i-1)*d1+j,(k-1)*d1+l] <- sum(Sigma.xx[i,,k,] * M3[(1:d2-1)*d1+j,(1:d2-1)*d1+l])
          Sigma.Q4[(i-1)*d1+j,(k-1)*d1+l] <- sum(Sigma.xx[i,,k,] * M4[(1:d2-1)*d1+j,(1:d2-1)*d1+l])
        }
      }
    }
  }
  Sigma.Q[1:d1^2,1:d1^2] <- Sigma.Q1+kronecker(D1,diag(d1))%*%Sigma.Q2+Sigma.Q3%*%kronecker(t(D1),diag(d1))+
    kronecker(D1,diag(d1))%*%Sigma.Q4%*%kronecker(t(D1),diag(d1))
  M1 <- kronecker(P2,t(A1))%*%Sigma.e.inv%*%kronecker(P2,A1)
  M2 <- kronecker(P2,t(A1))%*%Sigma.e.inv%*%kronecker(diag(d2)-P2,A1)
  M3 <- kronecker(diag(d2)-P2,t(A1))%*%Sigma.e.inv%*%kronecker(P2,A1)
  M4 <- kronecker(diag(d2)-P2,t(A1))%*%Sigma.e.inv%*%kronecker(diag(d2)-P2,A1)
  Sigma.Q1 <- Sigma.Q2 <- Sigma.Q3 <- Sigma.Q4 <- matrix(0,d2^2,d2^2)
  for(i in 1:d2){
    for(j in 1:d2){
      for(k in 1:d2){
        for(l in 1:d2){
          Sigma.Q1[(i-1)*d2+j,(k-1)*d2+l] <- sum(Sigma.xx[,j,,l] * M1[(i-1)*d1+(1:d1),(k-1)*d1+(1:d1)])
          Sigma.Q2[(i-1)*d2+j,(k-1)*d2+l] <- sum(Sigma.xx[,j,,l] * M2[(i-1)*d1+(1:d1),(k-1)*d1+(1:d1)])
          Sigma.Q3[(i-1)*d2+j,(k-1)*d2+l] <- sum(Sigma.xx[,j,,l] * M3[(i-1)*d1+(1:d1),(k-1)*d1+(1:d1)])
          Sigma.Q4[(i-1)*d2+j,(k-1)*d2+l] <- sum(Sigma.xx[,j,,l] * M4[(i-1)*d1+(1:d1),(k-1)*d1+(1:d1)])
        }
      }
    }
  }
  Sigma.Q[(d1^2+1):(d1^2+d2^2),(d1^2+1):(d1^2+d2^2)] <- Sigma.Q1+Sigma.Q2%*%kronecker(diag(d2),t(D2))+
    kronecker(diag(d2),D2)%*%Sigma.Q3+kronecker(diag(d2),D2)%*%Sigma.Q4%*%kronecker(diag(d2),t(D2))
  M1 <- kronecker(t(A2),P1)%*%Sigma.e.inv%*%kronecker(P2,A1)
  M2 <- kronecker(t(A2),P1)%*%Sigma.e.inv%*%kronecker(diag(d2)-P2,A1)
  M3 <- kronecker(t(A2),diag(d1)-P1)%*%Sigma.e.inv%*%kronecker(P2,A1)
  M4 <- kronecker(t(A2),diag(d1)-P1)%*%Sigma.e.inv%*%kronecker(diag(d2)-P2,A1)
  Sigma.Q1 <- Sigma.Q2 <- Sigma.Q3 <- Sigma.Q4 <- matrix(0,d1^2,d2^2)
  for(i in 1:d1){
    for(j in 1:d1){
      for(k in 1:d2){
        for(l in 1:d2){
          Sigma.Q1[(i-1)*d1+j,(k-1)*d2+l] <- sum(Sigma.xx[i,,,l] * M1[(1:d2-1)*d1+j,(k-1)*d1+(1:d1)])
          Sigma.Q2[(i-1)*d1+j,(k-1)*d2+l] <- sum(Sigma.xx[i,,,l] * M2[(1:d2-1)*d1+j,(k-1)*d1+(1:d1)])
          Sigma.Q3[(i-1)*d1+j,(k-1)*d2+l] <- sum(Sigma.xx[i,,,l] * M3[(1:d2-1)*d1+j,(k-1)*d1+(1:d1)])
          Sigma.Q4[(i-1)*d1+j,(k-1)*d2+l] <- sum(Sigma.xx[i,,,l] * M4[(1:d2-1)*d1+j,(k-1)*d1+(1:d1)])
        }
      }
    }
  }
  Sigma.Q[1:d1^2,(d1^2+1):(d1^2+d2^2)] <- Sigma.Q1+Sigma.Q2%*%kronecker(diag(d2),t(D2))+
    kronecker(D1,diag(d1))%*%Sigma.Q3+kronecker(D1,diag(d1))%*%Sigma.Q4%*%kronecker(diag(d2),t(D2))
  Sigma.Q[(d1^2+1):(d1^2+d2^2),1:d1^2] <- t(Sigma.Q[1:d1^2,(d1^2+1):(d1^2+d2^2)])
  Sigma.CC <- H.inv%*%Sigma.Q%*%H.inv
  Sigma.CC11 <- Sigma.CC[1:d1^2,1:d1^2]
  Sigma.CC11.t <- array(Sigma.CC11,c(d1,d1,d1,d1))
  Sigma.CC11.t <- aperm(Sigma.CC11.t,c(2,1,4,3))
  Sigma.CC11.t <- matrix(Sigma.CC11.t, d1^2, d1^2)
  Sigma.CC22.t <- Sigma.CC[(d1^2+1):(d1^2+d2^2),(d1^2+1):(d1^2+d2^2)]
  Sigma.CC22 <- array(Sigma.CC22.t,c(d2,d2,d2,d2))
  Sigma.CC22 <- aperm(Sigma.CC22,c(2,1,4,3))
  Sigma.CC22 <- matrix(Sigma.CC22, d2^2, d2^2)
  svd.A1 <- svd(A1)
  #k1 <- length(svd.A1$d[svd.A1$d>1e-10])
  D1 <- diag(c(svd.A1$d[1:k1],1))[1:k1,1:k1]
  U1 <- svd.A1$u[,1:k1]
  V1 <- svd.A1$v[,1:k1]
  if(k1<d1){
    U1c <- svd.A1$u[,(k1+1):d1]
    V1c <- svd.A1$v[,(k1+1):d1]
  }else if(k1==d1){
    U1c <- 0
    V1c <- 0
  }
  e1 <- diag(d1)
  J1 <- matrix(0,d1^2,d1^2)
  for(i in 1:d1){
    J1[,((i-1)*d1+1):(i*d1)] <- kronecker(diag(d1),e1[,i])
  }
  C1 <- kronecker(A1,diag(d1)) + kronecker(diag(d1),A1)%*%J1
  Sigma.CC.1u <- C1%*%Sigma.CC11%*%t(C1)
  C1.t <- kronecker(t(A1),diag(d1)) + kronecker(diag(d1),t(A1))%*%J1
  Sigma.CC.1v <- C1.t%*%Sigma.CC11.t%*%t(C1.t)
  e1 <- diag(k1)
  L1 <- matrix(0,k1^2,k1)
  for(i in 1:k1){
    L1[,i] <- kronecker(e1[,i],e1[,i])
  }
  
  if(k1<d1){
    R1u <- cbind(kronecker(diag(k1),U1), kronecker(diag(k1),U1c)) %*% rbind(solve(kronecker(D1^2,diag(k1)) -
                                                                                    kronecker(diag(k1),D1^2)+L1%*%t(L1)) %*% (diag(k1^2)-L1%*%t(L1)) %*% kronecker(t(U1),t(U1)),
                                                                            kronecker(diag(1/c(svd.A1$d[1:k1],1))[1:k1,1:k1]^2%*%t(U1), t(U1c))  )
    R1v <- cbind(kronecker(diag(k1),V1), kronecker(diag(k1),V1c)) %*% rbind(solve(kronecker(D1^2,diag(k1)) -
                                                                                    kronecker(diag(k1),D1^2)+L1%*%t(L1)) %*% (diag(k1^2)-L1%*%t(L1)) %*% kronecker(t(V1),t(V1)),
                                                                            kronecker(diag(1/c(svd.A1$d[1:k1],1))[1:k1,1:k1]^2%*%t(V1), t(V1c))  )
  }else if(k1==d1){
    R1u <- kronecker(diag(k1),U1) %*% solve(kronecker(D1^2,diag(k1)) -
                                              kronecker(diag(k1),D1^2)+L1%*%t(L1)) %*% (diag(k1^2)-L1%*%t(L1)) %*% kronecker(t(U1),t(U1))
    R1v <- kronecker(diag(k1),V1) %*% solve(kronecker(D1^2,diag(k1)) -
                                              kronecker(diag(k1),D1^2)+L1%*%t(L1)) %*% (diag(k1^2)-L1%*%t(L1)) %*% kronecker(t(V1),t(V1))
    
  }
  
  
  Theta1.CC.u <- R1u%*% Sigma.CC.1u %*%t(R1u)
  Theta1.CC.v <- R1v%*% Sigma.CC.1v %*%t(R1v)
  Theta1.CC.u <- kronecker(t(RU1),diag(d1))%*%Theta1.CC.u
  Theta1.CC.v <- kronecker(t(RV1),diag(d1))%*%Theta1.CC.v
  svd.A2 <- svd(A2)
  #k2 <- length(svd.A2$d[svd.A2$d>1e-10])
  D2 <- diag(c(svd.A2$d[1:k2],1))[1:k2,1:k2]
  U2 <- svd.A2$u[,1:k2]
  V2 <- svd.A2$v[,1:k2]
  if(k1<d1){
    U2c <- svd.A2$u[,(k2+1):d2]
    V2c <- svd.A2$v[,(k2+1):d2]
  }else if(k2==d2){
    U2c <- 0
    V2c <- 0
  }
  e2 <- diag(d2)
  J2 <- matrix(0,d2^2,d2^2)
  for(i in 1:d2){
    J2[,((i-1)*d2+1):(i*d2)] <- kronecker(diag(d2),e2[,i])
  }
  C2 <- kronecker(A2,diag(d2)) + kronecker(diag(d2),A2)%*%J2
  Sigma.CC.2u <- C2%*%Sigma.CC22%*%t(C2)
  C2.t <- kronecker(t(A2),diag(d2)) + kronecker(diag(d2),t(A2))%*%J2
  Sigma.CC.2v <- C2.t%*%Sigma.CC22.t%*%t(C2.t)
  e2 <- diag(k2)
  L2 <- matrix(0,k2^2,k2)
  for(i in 1:k2){
    L2[,i] <- kronecker(e2[,i],e2[,i])
  }
  if(k2<d2){
    R2u <- cbind(kronecker(diag(k2),U2), kronecker(diag(k2),U2c)) %*% rbind(solve(kronecker(D2^2,diag(k2)) -
                                                                                    kronecker(diag(k2),D2^2)+L2%*%t(L2)) %*% (diag(k2^2)-L2%*%t(L2)) %*% kronecker(t(U2),t(U2)),
                                                                            kronecker(diag(1/c(svd.A2$d[1:k2],1))[1:k2,1:k2]^2%*%t(U2), t(U2c))  )
    R2v <- cbind(kronecker(diag(k2),V2), kronecker(diag(k2),V2c)) %*% rbind(solve(kronecker(D2^2,diag(k2)) -
                                                                                    kronecker(diag(k2),D2^2)+L2%*%t(L2)) %*% (diag(k2^2)-L2%*%t(L2)) %*% kronecker(t(V2),t(V2)),
                                                                            kronecker(diag(1/c(svd.A2$d[1:k2],1))[1:k2,1:k2]^2%*%t(V2), t(V2c))  )
  }else if(k2==d2){
    R2u <- kronecker(diag(k2),U2) %*% solve(kronecker(D2^2,diag(k2)) -
                                              kronecker(diag(k2),D2^2)+L2%*%t(L2)) %*% (diag(k2^2)-L2%*%t(L2)) %*% kronecker(t(U2),t(U2))
    R2v <- kronecker(diag(k2),V2) %*% solve(kronecker(D2^2,diag(k2)) -
                                              kronecker(diag(k2),D2^2)+L2%*%t(L2)) %*% (diag(k2^2)-L2%*%t(L2)) %*% kronecker(t(V2),t(V2))
  }
  Theta2.CC.u <- R2u%*% Sigma.CC.2u %*%t(R2u)
  Theta2.CC.v <- R2v%*% Sigma.CC.2v %*%t(R2v)
  Theta2.CC.u <- kronecker(t(RU2),diag(d2))%*%Theta2.CC.u
  Theta2.CC.v <- kronecker(t(RV2),diag(d2))%*%Theta2.CC.v
  return(list("Sigma"=Sigma.CC,"Theta1.u"=Theta1.CC.u,"Theta1.v"=Theta1.CC.v,
              "Theta2.u"=Theta2.CC.u,"Theta2.v"=Theta2.CC.v))
}


MAR1.PROJ <- function(xx){
  # xx: T * p * q
  # X_t = LL X_{t-1} RR + E_t
  # Sig = cov(vec(E_t))
  # one-step projection estimation
  # Return LL, RR, and estimate of Sig
  dd <- dim(xx)
  T <- dd[1]
  p <- dd[2]
  q <- dd[3]
  xx.mat <- matrix(xx,T,p*q)
  kroneck <- t(xx.mat[2:T,]) %*% xx.mat[1:(T-1),] %*% solve(t(xx.mat[1:(T-1),]) %*% xx.mat[1:(T-1),])
  ans.projection <- projection(kroneck,r=1,p,q,p,q)
  a <- svd(ans.projection[[1]][[1]],nu=0,nv=0)$d[1]
  LL <- ans.projection[[1]][[1]] / a
  RR <- t(ans.projection[[1]][[2]]) * a
  res = xx[2:T,,,drop=FALSE] - aperm(tensor(tensor(xx[1:(T-1),,,drop=FALSE],RR,3,1),LL,2,2),c(1,3,2))
  Sig <- matrix(tensor(res,res,1,1),p*q)/(T-1)
  # sd <- MAR.SE(xx, t(RR), LL, Sig)
  return(list(A1=LL,A2=RR,Sig=Sig))
}


MAR2.PROJ <- function(xx, R=1, P=1){
  # xx: T * p * q
  # X_t = LL X_{t-1} RR + E_t
  # Sig = cov(vec(E_t))
  # one-step projection estimation
  # Return LL, RR, and estimate of Sig
  dd <- dim(xx)
  T <- dd[1]
  d1 <- dd[2]
  d2 <- dd[3]
  # xx.mat <- matrix(xx,T,d1*d2)
  # kroneck <- t(xx.mat[2:T,]) %*% xx.mat[1:(T-1),] %*% solve(t(xx.mat[1:(T-1),]) %*% xx.mat[1:(T-1),])
  A = list()
  kroneck <- tenAR.VAR(xx, P)$coef
  for (i in c(1:P)){
    ans.projection <- projection(kroneck[[i]],R[i],d2,d1,d2,d1)
    for (j in c(1:R[i])){
      a = svd(ans.projection[[j]][[1]],nu=0,nv=0)$d[1]
      ans.projection[[j]][[1]] <- ans.projection[[j]][[1]] / a
      ans.projection[[j]][[2]] <- t(ans.projection[[j]][[2]]) * a
    }
    A[[i]] <- ans.projection
  }
  return(list(A=A))
}


MAR1.LS <- function(xx,niter=50,tol=1e-6,print.true = FALSE){
  # xx: T * p * q
  # X_t = LL X_{t-1} RR + E_t
  # Sig = cov(vec(E_t))
  # LS criterion
  # iterative algorithm between LL <--> RR
  # Return LL, RR, and estimate of Sig
  dd=dim(xx)
  T <- dd[1]
  p <- dd[2]
  q <- dd[3]
  LL.old <- diag(p)
  RR.old <- diag(q)
  dis <- 1
  iiter <- 1
  
  while(iiter <= niter & dis >= tol){
    # estimate RR0
    temp <- tensor(xx[1:(T-1),,,drop=FALSE],LL.old,2,2)  # (T-1) * q * p
    AA <- tensor(temp,temp,c(1,3),c(1,3))
    BB <- tensor(temp,xx[2:T,,,drop=FALSE],c(1,3),c(1,2))
    RR <- solve(AA,BB)
    # estimate LL0
    temp <- tensor(xx[1:(T-1),,,drop=FALSE],RR,3,1)  # (T-1) * p * q
    AA <- tensor(temp,temp,c(1,3),c(1,3))
    BB <- t(tensor(temp,xx[2:T,,,drop=FALSE],c(1,3),c(1,3)))
    LL <- t(solve(t(AA),t(BB)))
    a <- svd(LL,nu=0,nv=0)$d[1]
    LL <- LL / a
    RR <- RR * a
    # update for the next iteration
    dis <- sqrt(sum((kronecker(t(RR),LL)-kronecker(t(RR.old),LL.old))^2))
    LL.old <- LL
    RR.old <- RR
    iiter <- iiter + 1
    if(print.true==TRUE){
      print(LL)
      print(RR)
    }
  }
  # res=xx[2:T,,,drop=FALSE] - aperm(tensor(tensor(xx[1:(T-1),,,drop=FALSE],RR,3,1),LL,2,2),c(1,3,2))
  # Sig <- matrix(tensor(res,res,1,1),p*q)/(T-1)
  # sd <- MAR.SE(xx, t(RR), LL, Sig)
  return(list(A1=LL,A2=t(RR)))
}


MAR1.MLE <- function(xx,LL.init=NULL,Sigl.init=NULL,Sigr.init=NULL,niter=50,tol=1e-6,print.true = FALSE){
  # xx: T * p * q
  # X_t = LL X_{t-1} RR + E_t
  # Sig = cov(vec(E_t)) = Sigr otimes Sigl
  # optimization criterion is likelihood
  # iterative algorithm between LL <--> RR <--> Sig_r <--> Sig_l
  # Return LL, RR, Sigl, Sigr
  dd=dim(xx)
  T <- dd[1]
  p <- dd[2]
  q <- dd[3]
  if(is.null(LL.init)){
    LL.old <- diag(p)
  } else{
    LL.old <- LL.init
  }
  RR.old <- diag(q)
  if(is.null(Sigl.init)){
    Sigl.old <- diag(p)
  } else{
    Sigl.old <- Sigl.init
  }
  if(is.null(Sigr.init)){
    Sigr.old <- diag(q)
  } else{
    Sigr.old <- Sigr.init
  }
  dis <- 1
  iiter <- 1
  
  while(iiter <= niter & dis >= tol){
    Sigl.inv.old <- ginv(Sigl.old)
    Sigr.inv.old <- ginv(Sigr.old)
    # estimate RR0
    temp1 <- tensor(xx[1:(T-1),,,drop=FALSE],LL.old,2,2)  # (T-1) * q * p
    temp2 <- tensor(temp1,Sigl.inv.old,3,2)  # (T-1) * q * p
    AA <- tensor(temp1,temp2,c(1,3),c(1,3))
    BB <- tensor(temp2,xx[2:T,,,drop=FALSE],c(1,3),c(1,2))
    RR <- solve(AA,BB)
    # estimate LL0
    temp1 <- tensor(xx[1:(T-1),,,drop=FALSE],RR,3,1)  # (T-1) * p * q
    temp2 <- tensor(temp1,Sigr.inv.old,3,1)  # (T-1) * p * q
    AA <- tensor(temp1,temp2,c(1,3),c(1,3))
    BB <- t(tensor(temp2,xx[2:T,,,drop=FALSE],c(1,3),c(1,3)))
    LL <- t(solve(t(AA),t(BB)))
    res=xx[2:T,,,drop=FALSE] - aperm(tensor(tensor(xx[1:(T-1),,,drop=FALSE],RR,3,1),LL,2,2),c(1,3,2))
    temp <- tensor(res,Sigl.inv.old,2,1) # (T-1) * q * p
    Sigr <- tensor(temp,res,c(1,3),c(1,2))/T/p
    temp <- tensor(res,ginv(Sigr),3,1) # (T-1) * p * q
    Sigl <- tensor(temp,res,c(1,3),c(1,3))/T/q
    a <- svd(LL,nu=0,nv=0)$d[1]
    LL <- LL / a
    RR <- RR * a
    a <- eigen(Sigl)$values[1]
    Sigl <- Sigl / a
    Sigr <- Sigr * a
    
    # update for the next iteration
    dis1 <- sqrt(sum((kronecker(t(RR),LL)-kronecker(t(RR.old),LL.old))^2))
    dis2 <- sqrt(sum((kronecker(Sigr,Sigl)-kronecker(Sigr.old,Sigl.old))^2))
    dis <- max(dis1,dis2)
    LL.old <- LL
    RR.old <- RR
    Sigr.old <- Sigr
    Sigl.old <- Sigl
    iiter <- iiter + 1
    if(print.true==TRUE){
      print(LL)
      print(RR)
    }
  }
  # Sig <- kronecker(Sigr,Sigl)
  # sd <- MAR.SE(xx, t(RR), LL, Sig)
  return(list(A1=LL,A2=t(RR),res=res,Sig1=Sigl,Sigr=Sigr))
}


MAR.SE <- function(xx, B, A, Sigma){
  dd <- dim(xx)
  T <- dd[1]
  p <- dd[2]
  q <- dd[3]
  BX <- tensor(xx,B,3,2) # Tpq
  AX <- tensor(xx,A,2,2) # Tqp
  BXI <- apply(BX,1,function(x){kronecker(t(x),diag(p))})
  AXI <- apply(AX,1,function(x){kronecker(diag(q),t(x))})
  BXI.array <- array(BXI,c(p*q,p^2,T)) #pq*p^2*T
  AXI.array <- array(AXI,c(p*q,q^2,T)) #pq*q^2*T
  # W transpose
  Wt <- abind(BXI.array,AXI.array,along=2) #pq*(p^2+q^2)*T
  EWWt <- tensor(Wt,Wt,c(1,3),c(1,3))/T #(p^2+q^2)*(p^2+q^2)
  alpha <- as.vector(A)
  beta <- as.vector(t(B))
  gamma <- c(alpha,rep(0,q^2))
  H <- EWWt + gamma %*% t(gamma) #(p^2+q^2)*(p^2+q^2)
  WSigma <- tensor(Wt,Sigma,1,1) #(p^2+q^2)*T*pq
  EWSigmaWt <- tensor(WSigma,Wt,c(3,2),c(1,3))/T
  Hinv <- solve(H)
  Xi <- Hinv %*% EWSigmaWt %*% Hinv
  Xi <- Xi/T
}


tenAR.VAR <- function(xx, P){
  dd=dim(xx)
  t <- dd[1]
  n <- prod(dd[-1])
  if (prod(dd[-1]) > t){stop("sample size T too small")}
  yy=apply(xx, MARGIN=1, as.vector)
  
  x = 0
  for (l in c(1:P)){
    if (l == 1){x = yy[,(P):(t-1)]} else {x = rbind(x, yy[,(P+1-l):(t-l)])}
  }
  y = yy[,(P+1):t]
  coef = t(solve(x %*% t(x)) %*% x %*% t(y))
  res= y - coef %*% x
  phi = list()
  for (l in c(1:P)){
    phi[[l]] = coef[,(n*(l-1)+1):(n*l)]
  }
  return(list(coef=phi, res=res))
}


tenAR.A <- function(dim,R,P,rho){
  K <- length(dim)
  A <- lapply(1:P, function(p) {lapply(1:max(1,R[p]), function(j) {lapply(1:K, function(i) {diag(dim[i])})})})
  for (p in c(1:P)){
    if (R[p] == 0) next
    for (j in c(1:R[p])){
      for (i in c(1:K)){
        A[[p]][[j]][[i]] <- matrix(rnorm(dim[i]^2), c(dim[i],dim[i]))
      }
    }
  }
  eigen = M.eigen(A, R, P, dim)
  for (l in c(1:P)){
    if (R[l] == 0) next
    for (j in c(1:R[l])){
      A[[l]][[j]][[K]] <- rho * A[[l]][[j]][[K]]/ eigen
    }
    A[[l]] <- fro.order(fro.rescale(A[[l]]))
  }
  eigen = M.eigen(A, R, P, dim)
  return(A)
}


tenAR.PROJ <- function(xx,R,P){
  dim <- dim(xx)[-1]
  if (length(dim) == 2){
    A = MAR2.PROJ(xx,R,P)
    return(list(A=A))
  }
  if (length(dim) != 3){
    stop("temporarily 'tenAR.PROJ' only support K=2,3.")
  }
  mm <- tenAR.VAR(xx, P)$coef
  A = list()
  for (p in c(1:P)){
    if (is.na(R[p])) stop("p != length(R)")
    if (R[p] == 0) next
    tt <- aperm(trearrange(mm[[p]], rev(dim)))
    A[[p]] <- fro.order(fro.rescale(ten.proj(tt, dim, R[p])))
  }
  # A1 = A
  # phi1 = kronecker(kronecker(A1[[1]][[1]][[3]], A1[[1]][[1]][[2]]), A1[[1]][[1]][[1]]) + kronecker(kronecker(A1[[1]][[2]][[3]], A1[[1]][[2]][[2]]), A1[[1]][[2]][[1]])
  # sqrt(sum((mm[[1]] - phi1)**2))
  return(list(A=A))
}


tenAR.LS <- function(xx, R, P, init.A=NULL, niter=150, tol=1e-5, print.true=FALSE){
  if (!(mode(xx) == "S4")) {xx <- as.tensor(xx)}
  if (length(P) == 2){P = P[1]; rolling=TRUE} else {rolling = FALSE} # len(P) > 1 implies rolling in func 'tenAR.predict'
  dim <- dim(xx)[-1]
  K <- length(dim)
  t <- dim(xx)[1]
  if (length(R) == 1 & P > 1){
    R = rep(R, P)
  }
  # if (is.null(init.A)) {
  #   A.old = list()
  #   for (p in c(1:P)){
  #     if (is.na(R[p])) stop("p != length(R)")
  #     if (R[p] == 0) next
  #     A.old[[p]] <- lapply(1:R[p], function(j) {lapply(1:K, function(i) {0.5*diag(dim[i])})})
  #   }
  # } else {A.old <- init.A}
  if (K==2|K>3){
    if (is.null(init.A)) {
      A.old = list()
      for (p in c(1:P)){
        if (is.na(R[p])) stop("p != length(R)")
        if (R[p] == 0) next
        A.old[[p]] <- lapply(1:R[p], function(j) {lapply(1:K, function(i) {0.5*diag(dim[i])})})
      }
    } else {A.old <- init.A}
  } else if (K==3) {
    if (is.null(init.A)) {A.old <- tenAR.PROJ(xx@data,R,P)$A} else {A.old <- init.A}
  } else {stop("dimension K of time series must at least be 2")}
  A.new <- A.old
  Tol <- tol*sqrt(sum(dim^2))*sum(R)
  dis <- 1
  iiter <- 1
  AX = A.new
  while(iiter <= niter & dis >= tol){
    for (p in c(1:P)){
      if (R[p] == 0) next
      for (r in c(1:R[p])){
        for (k in c(K:1)){ # update last matrix first
          AX[[p]][[r]][[k]] = rTensor::ttl(xx, A.new[[p]][[r]][-k], c(2:(K+1))[-k])
          temp = abind::asub(AX[[p]][[r]][[k]], (1+P-p):(t-p), 1, drop=FALSE)
          L1 <- 0
          for (l in c(1:P)){
            if (R[l] == 0) next
            if (l == p){if (R[l] > 1){L1 <- L1 + Reduce("+",lapply(c(1:R[l])[-r], function(n) {rTensor::ttl(abind::asub(xx, (1+P-l):(t-l), 1, drop=FALSE), A.new[[l]][[n]], (c(1:K) + 1))}))}
            } else {L1 <- L1 + Reduce("+",lapply(c(1:R[l]), function(n) {rTensor::ttl(abind::asub(xx, (1+P-l):(t-l), 1, drop=FALSE), A.new[[l]][[n]], (c(1:K) + 1))}))}
          }
          temp2 <- abind::asub(xx, (1+P):t, 1, drop=FALSE) - L1
          RR <- tensor(temp@data,temp@data,c(1:(K+1))[-(k+1)],c(1:(K+1))[-(k+1)])
          LL <- tensor(temp2@data,temp@data,c(1:(K+1))[-(k+1)],c(1:(K+1))[-(k+1)])
          A.new[[p]][[r]][[k]] <- LL %*% ginv(RR)
        }
      }
    }
    dis = ten.dis.A(A.new, A.old, R, K)
    for (p in c(1:P)){
      if (R[p] == 0) next
      A.new[[p]] <- svd.rescale(A.new[[p]])
    }
    A.old <- A.new
    iiter <- iiter + 1
    if (print.true == TRUE){
      print(dis)
      print(paste('iiter num=',iiter))
    }
  }
  for (p in c(1:P)){
    if (R[p] == 0) next
    A.new[[p]] <- fro.order(fro.rescale(A.new[[p]]))
  }
  if (rolling){ # early return for rolling forecast, no need calculate other parameters
    return(list(A=A.new))
  }
  res <- ten.res(xx,A.new,P,R,K,t)@data
  # Sig <- matrix(tensor(res,res,1,1),prod(dim))/(t-1)
  # cov = tenAR.SE.LSE(dim, R, P, K, t, AX, A.new, Sig)
  # sd <- covtosd(cov, dim, R)
  # bic <- IC(xx, res, R, t, dim)
  return(list(A=A.new,res=res))
}


tenAR.MLE <- function(xx, R, P, init.A=NULL, init.sig=NULL, niter=150, tol=1e-5, print.true = FALSE){
  if (!(mode(xx) == "S4")) {xx <- as.tensor(xx)}
  if (length(P) == 2){P = P[1]; rolling=TRUE} else {rolling = FALSE} # len(P) > 1 implies rolling in func 'tenAR.predict'
  dim <- dim(xx)[-1]
  K <- length(dim)
  t <- dim(xx)[1]
  if (length(R) == 1 & P > 1){
    R = rep(R, P)
  }
  # if (is.null(init.A)) {
  #   A.old = list()
  #   for (p in c(1:P)){
  #     if (is.na(R[p])) stop("p != length(R)")
  #     if (R[p] == 0) next
  #     A.old[[p]] <- lapply(1:R[p], function(j) {lapply(1:K, function(i) {0.5*diag(dim[i])})})
  #   }
  # } else {A.old <- init.A}
  if (K==2|K>3){
    if (is.null(init.A)) {
      A.old = list()
      for (p in c(1:P)){
        if (is.na(R[p])) stop("p != length(R)")
        if (R[p] == 0) next
        A.old[[p]] <- lapply(1:R[p], function(j) {lapply(1:K, function(i) {0.5*diag(dim[i])})})
      }
    } else {A.old <- init.A}
  } else if (K==3) {
    if (is.null(init.A)) {A.old <- tenAR.PROJ(xx@data,R,P)$A} else {A.old <- init.A}
  } else {stop("dimension K of time series must at least be 2")}
  if (is.null(init.sig)) {Sig.old <- lapply(1:K, function(i) {diag(dim[i])})} else {Sig.old <- init.sig}
  Sig.new <- Sig.old
  Sig.new.inv <- lapply(1:K, function (k) {solve(Sig.new[[k]])})
  A.new <- A.old
  Tol <- tol*sqrt(sum(dim^2))*sum(R)
  dis <- 1
  iiter <- 1
  AX = A.new
  while(iiter <= niter & dis >= Tol){
    for (p in c(1:P)){
      for (r in c(1:R[p])){
        for (k in c(K:1)){
          res.old <- ten.res(xx,A.new,P,R,K,t)
          rs <- rTensor::ttl(res.old, Sig.new.inv[-k], c(2:(K+1))[-k])
          Sig.new[[k]] <- tensor(res.old@data, rs@data, c(1:(K+1))[-(k+1)],c(1:(K+1))[-(k+1)])/(t-1)/prod(dim[-k])
          Sig.new.inv <- lapply(1:K, function (k) {ginv(Sig.new[[k]])})
        }
        for (k in c(K:1)){
          sphi <-  lapply(1:K, function (k) {Sig.new.inv[[k]] %*% (A.new[[p]][[r]][[k]])})
          AX[[p]][[r]][[k]] = rTensor::ttl(xx, A.new[[p]][[r]][-k], c(2:(K+1))[-k])
          temp = abind::asub(AX[[p]][[r]][[k]], (1+P-p):(t-p), 1, drop=FALSE)
          temp1 <- abind::asub(rTensor::ttl(xx, sphi[-k], c(2:(K+1))[-k]), (1+P-p):(t-p), 1, drop=FALSE)
          L1 <- 0
          for (l in c(1:P)){
            if (R[l] == 0) next
            if (l == p){if (R[l] > 1){L1 <- L1 + Reduce("+",lapply(c(1:R[l])[-r], function(n) {rTensor::ttl(abind::asub(xx, (1+P-l):(t-l), 1, drop=FALSE), A.new[[l]][[n]], (c(1:K) + 1))}))}
            } else {L1 <- L1 + Reduce("+",lapply(c(1:R[l]), function(n) {rTensor::ttl(abind::asub(xx, (1+P-l):(t-l), 1, drop=FALSE), A.new[[l]][[n]], (c(1:K) + 1))}))}
          }
          temp2 <- abind::asub(xx, (1+P):t, 1, drop=FALSE) - L1
          RR <- tensor(temp@data,temp1@data,c(1:(K+1))[-(k+1)],c(1:(K+1))[-(k+1)])
          LL <- tensor(temp2@data,temp1@data,c(1:(K+1))[-(k+1)],c(1:(K+1))[-(k+1)])
          A.new[[p]][[r]][[k]] <- LL %*% ginv(RR)
        }
      }
      A.new[[p]] <- svd.rescale(A.new[[p]])
      Sig.new <- eigen.rescale(list(Sig.new))[[1]]
    }
    dis = ten.dis.A(A.new, A.old, R, K)
    Sig.old <- Sig.new
    A.old <- A.new
    iiter <- iiter + 1
    if (print.true == TRUE){
      print(dis)
      print(paste('iiter num=',iiter))
    }
  }
  for (p in c(1:P)){
    if (R[p] == 0) next
    A.new[[p]] <- fro.order(fro.rescale(A.new[[p]]))
  }
  if (rolling){ # early return for rolling forecast, no need calculate other parameters
    return(list(A=A.new))
  }
  # res <- ten.res(xx,A.new,P,R,K,t)@data
  # Sig <- matrix(tensor(res,res,1,1),prod(dim))/(t-1)
  # cov = tenAR.SE.MLE(dim, R, P, K, t, AX, A.new, Sig)
  # sd <- covtosd(cov, dim, R)
  # bic <- IC(xx, res, R, t, dim)
  return(list(A=A.new,sig=Sig.new))
}


MAR1.RR <- function(xx, k1, k2, A1.init=NULL, A2.init=NULL, niter=200, tol=1e-4,  print.true=FALSE){
  # xx: T * p * q
  # X_t = LL X_{t-1} RR' + E_t ### NOTE! This function is written with RR'.
  # Sig = cov(vec(E_t)) = Sigr \otimes Sigl
  # optimization criterion is likelihood
  # iterative algorithm between LL <--> Sigma_l <--> RR <--> Sig_r
  # Return LL, RR, Sigl, Sigr
  dd=dim(xx)
  T <- dd[1]
  p <- dd[2]
  q <- dd[3]
  if(is.null(A1.init) | is.null(A2.init)){
    init = initializer(xx, k1, k2)
  }
  if(is.null(A1.init)){
    LL.old <- init$A1
  } else{
    LL.old <- A1.init
  }
  if(is.null(A2.init)){
    RR.old <- init$A2
  } else{
    RR.old <- A2.init
  }
  Tol=tol*sqrt(p^2+q^2)
  dis <- 1
  iiter <- 1
  
  while(iiter <= niter & dis >= Tol){
    ## Save old
    LL.oold=LL.old
    RR.oold=RR.old
    # estimate LL0
    temp1 <- tensor(xx[1:(T-1),,,drop=FALSE],RR.old,3,2)  # (T-1) * p * q
    AA <- tensor(temp1,temp1,c(1,3),c(1,3))
    BB <- tensor(xx[2:T,,,drop=FALSE],temp1,c(1,3),c(1,3))
    LL <- BB%*%ginv(AA)
    U <- svd(LL%*%t(BB))$u[,1:k1]
    LL <- U%*%t(U)%*%LL
    # update for next iteration
    a=svd(LL,nu=0,nv=0)$d[1]
    LL=LL/a
    dis3=sum((LL-LL.old)^2)
    LL.old <- LL
    
    # estimate RR0
    temp1 <- tensor(xx[1:(T-1),,,drop=FALSE],LL.old,2,2)  # (T-1) * q * p
    AA <- tensor(temp1,temp1,c(1,3),c(1,3))
    BB <- tensor(xx[2:T,,,drop=FALSE],temp1,c(1,2),c(1,3))
    RR <- BB%*%ginv(AA)
    U <- svd(RR%*%t(BB))$u[,1:k2]
    RR <- U%*%t(U)%*%RR
    # update for next iteration
    dis3=dis3+sum((RR-RR.old)^2)
    RR.old <- RR
    
    # update for the next iteration
    dis1 <- sqrt(sum((kronecker(t(RR),LL)-kronecker(t(RR.oold),LL.oold))^2))
    dis3 = sqrt(dis3)
    dis <- dis3
    iiter <- iiter + 1
    
    #print(max(abs(eigen(LL)$values)))
    #print(max(abs(eigen(RR)$values)))
    
    if(print.true==TRUE){
      print(dis)
      print(paste('iiter num=',iiter))
    }
  }
  a <- sqrt(sum(LL^2))
  LL <- LL / a
  RR <- RR * a
  # res=xx[2:T,,,drop=FALSE] - aperm(tensor(tensor(xx[1:(T-1),,,drop=FALSE],RR,3,2),LL,2,2),c(1,3,2))
  # Sig <- matrix(tensor(res,res,1,1),p*q)/(T-1)
  # bic <- T*p*q*log(sum(res^2/(T*p*q))) ##+log(T*p*q)*(bic.penalty(p,k1)+bic.penalty(q,k2))
  # cov <- matAR.RR.se(LL,RR,k1,k2,method="RRLSE",Sigma.e=Sig,RU1=diag(k1),RV1=diag(k1),RU2=diag(k2),RV2=diag(k2),mpower=100)
  # sd.A1 <- sqrt(array(diag(cov$Sigma)[1:p^2], c(p,p))/T)
  # sd.A2 <- sqrt(array(diag(cov$Sigma)[(p^2+1):(p^2+q^2)], c(q,q))/T)
  # #sd <- list(sqrt(array(diag(cov$Sigma)[1:p^2], c(p,p))/T), sqrt(array(diag(cov$Sigma)[(p^2+1):(p^2+q^2)], c(q,q))/T))
  # loading = list(U1=svd(LL)$u,V1=svd(LL)$v,U2=svd(RR)$u,V2=svd(RR)$v)
  return(list(A1=LL,A2=RR))
}


MAR1.CC <- function(xx,k1,k2,A1.init=NULL,A2.init=NULL,Sigl.init=NULL,Sigr.init=NULL,niter=200,tol=1e-4,print.true = FALSE){
  # xx: T * p * q
  # X_t = LL X_{t-1} RR' + E_t ### NOTE! This function is written with RR'.
  # Sig = cov(vec(E_t)) = Sigr \otimes Sigl
  # optimization criterion is likelihood
  # iterative algorithm between LL <--> Sigma_l <--> RR <--> Sig_r
  # Return LL, RR, Sigl, Sigr
  dd=dim(xx)
  T <- dd[1]
  p <- dd[2]
  q <- dd[3]
  # init = MAR1.LS(xx, k1, k2)
  if(is.null(A1.init) | is.null(A2.init)){
    init = initializer(xx, k1, k2)
  }
  if(is.null(A1.init)){
    LL.old <- init$A1
  } else{
    LL.old <- A1.init
  }
  if(is.null(A2.init)){
    RR.old <- init$A2
  } else{
    RR.old <- A2.init
  }
  # init.sig <- initializer.sig(xx)
  if(is.null(Sigl.init)){
    Sigl.old <- diag(p)
  } else{
    Sigl.old <- Sigl.init
  }
  if(is.null(Sigr.init)){
    Sigr.old <- diag(q)
  } else{
    Sigr.old <- Sigr.init
  }
  Tol=tol*sqrt(p^2+q^2)
  dis <- 1
  iiter <- 1
  while(iiter <= niter & dis >= Tol){
    
    ## Save old
    LL.oold=LL.old
    RR.oold=RR.old
    Sigl.oold=Sigl.old
    Sigr.oold=Sigr.old
    
    # estimate LL0 and Sigl
    Sigr.inv.old <- ginv(Sigr.old)
    temp1 <- tensor(xx[1:(T-1),,,drop=FALSE],RR.old,3,2)  # (T-1) * p * q
    temp2 <- tensor(temp1,Sigr.inv.old,3,1)  # (T-1) * p * q
    AA <- tensor(temp1,temp2,c(1,3),c(1,3))
    BB <- tensor(xx[2:T,,,drop=FALSE],temp2,c(1,3),c(1,3))
    LL <- BB%*%ginv(AA)
    res <- xx[2:T,,,drop=FALSE] - aperm(tensor(tensor(xx[1:(T-1),,,drop=FALSE],RR.old,3,2),LL,2,2),c(1,3,2)) # (T-1) * p * q
    temp <- tensor(res,Sigr.inv.old,3,1) # (T-1) * p * q
    Sigl <- tensor(temp,res,c(1,3),c(1,3))/T/q
    Sigl.spec <- eigen(Sigl)
    Sigl.root <- Sigl.spec$vectors%*%diag(sqrt(Sigl.spec$values))%*%t(Sigl.spec$vectors)
    Sigl.root.inv <- Sigl.spec$vectors%*%diag(1/sqrt(Sigl.spec$values))%*%t(Sigl.spec$vectors)
    U <- svd(Sigl.root.inv%*%LL%*%t(BB)%*%Sigl.root.inv)$u[,1:k1]
    LL <- Sigl.root%*%U%*%t(U)%*%Sigl.root.inv%*%LL
    res <- xx[2:T,,,drop=FALSE] - aperm(tensor(tensor(xx[1:(T-1),,,drop=FALSE],RR.old,3,2),LL,2,2),c(1,3,2)) # (T-1) * p * q
    temp <- tensor(res,Sigr.inv.old,3,1) # (T-1) * p * q
    Sigl <- tensor(temp,res,c(1,3),c(1,3))/T/q
    # update for next iteration
    a=svd(LL,nu=0,nv=0)$d[1]
    LL=LL/a
    dis3=sum((LL-LL.old)^2)
    LL.old <- LL
    Sigl.old <- Sigl
    
    
    # estimate RR0 and Sigr
    Sigl.inv.old <- ginv(Sigl.old)
    temp1 <- tensor(xx[1:(T-1),,,drop=FALSE],LL.old,2,2)  # (T-1) * q * p
    temp2 <- tensor(temp1,Sigl.inv.old,3,1)  # (T-1) * q * p
    AA <- tensor(temp1,temp2,c(1,3),c(1,3))
    BB <- tensor(xx[2:T,,,drop=FALSE],temp2,c(1,2),c(1,3))
    RR <- BB%*%ginv(AA)
    res <- xx[2:T,,,drop=FALSE] - aperm(tensor(tensor(xx[1:(T-1),,,drop=FALSE],RR,3,2),LL.old,2,2),c(1,3,2)) # (T-1) * p * q
    temp <- tensor(res,Sigl.inv.old,2,1) # (T-1) * q * p
    Sigr <- tensor(temp,res,c(1,3),c(1,2))/T/p
    Sigr.spec <- eigen(Sigr)
    Sigr.root <- Sigr.spec$vectors%*%diag(sqrt(Sigr.spec$values))%*%t(Sigr.spec$vectors)
    Sigr.root.inv <- Sigr.spec$vectors%*%diag(1/sqrt(Sigr.spec$values))%*%t(Sigr.spec$vectors)
    U <- svd(Sigr.root.inv%*%RR%*%t(BB)%*%Sigr.root.inv)$u[,1:k2]
    RR <- Sigr.root%*%U%*%t(U)%*%Sigr.root.inv%*%RR
    res <- xx[2:T,,,drop=FALSE] - aperm(tensor(tensor(xx[1:(T-1),,,drop=FALSE],RR,3,2),LL.old,2,2),c(1,3,2)) # (T-1) * p * q
    temp <- tensor(res,Sigl.inv.old,2,1) # (T-1) * q * p
    Sigr <- tensor(temp,res,c(1,3),c(1,2))/T/p
    # update for next iteration
    dis3=dis3+sum((RR-RR.old)^2)
    RR.old <- RR
    Sigr.old <- Sigr
    
    
    a <- eigen(Sigl)$values[1]
    Sigl <- Sigl / a
    Sigr <- Sigr * a
    ### cat(eigen(Sigl)$values[1]," ",eigen(Sigr)$values[1], " ")
    
    # update for the next iteration
    dis1 <- sqrt(sum((kronecker(t(RR),LL)-kronecker(t(RR.oold),LL.oold))^2))
    ### cat(dis1," ")
    dis2 <- sqrt(sum((kronecker(Sigr,Sigl)-kronecker(Sigr.oold,Sigl.oold))^2))
    ### cat(dis2," ",dis3,"\n")
    dis3 = sqrt(dis3)
    ### dis <- max(dis1,dis2)
    dis <- dis3
    Sigr.old <- Sigr
    Sigl.old <- Sigl
    iiter <- iiter + 1
    if(print.true==TRUE){
      print(LL)
      print(RR)
    }
  }
  a <- sqrt(sum(LL^2))
  LL <- LL / a
  RR <- RR * a
  # Sig <- matrix(tensor(res,res,1,1),p*q)/(T-1)
  # #Sig <- kronecker(Sigr,Sigl)
  # #cov <- matAR.RR.se(LL,RR,k1,k2,method="RRMLE",Sigma1=Sigl,Sigma2=Sigr,RU1=diag(k1),RV1=diag(k1),RU2=diag(k2),RV2=diag(k2),mpower=100)
  # cov = MAR1.RRCC.SE(LL,RR,k1,k2,Sigl,Sigr,RU1=diag(k1),RV1=diag(k1),RU2=diag(k2),RV2=diag(k2),mpower=100)
  # sd.A1 = sqrt(array(diag(cov$Sigma)[1:p^2], c(p,p))/T)
  # sd.A2 = sqrt(array(diag(cov$Sigma)[(p^2+1):(p^2+q^2)], c(q,q))/T)
  # #sd <- list(sqrt(array(diag(cov$Sigma)[1:p^2], c(p,p))/T), sqrt(array(diag(cov$Sigma)[(p^2+1):(p^2+q^2)], c(q,q))/T))
  # loading = list(U1=svd(LL)$u,V1=svd(LL)$v,U2=svd(RR)$u,V2=svd(RR)$v)
  return(list(A1=LL,A2=RR))
}


tenAR.SE.LSE <- function(dim, r, p, K, t, AX, A, Sigma){
  # dim = dim; r=R; p=P; K=K; t=t; AX = AX; A=A.new; Sigma=Sig;
  pdim = prod(dim) # d1d2d3
  fdim = dim**2
  ndim <- sum(fdim) # d1^2+d2^2+d^3
  Gamma <- matrix(0,sum(r)*ndim,sum(r)*ndim)
  n = 0
  for (i in c(1:p)){
    for (j in c(1:r[i])){
      for (k in c(1:(K-1))){
        r1 <- matrix(0, sum(r)*ndim, 1)
        a = as.vector(A[[i]][[j]][[k]])
        r1[( n*ndim + sum(fdim[0:(k-1)]) + 1): (n*ndim + sum(fdim[0:k])),] = a
        Gamma = Gamma + r1 %*% t(r1)
        if (K==2){
          for (l in c(1:r[i])){
            if (l != j){
              r1 <- matrix(0, sum(r)*ndim, 1)
              a = as.vector(A[[i]][[l]][[k]])
              r1[( n*ndim + sum(fdim[0:(k-1)]) + 1): (n*ndim + sum(fdim[0:k])),] = a
              Gamma = Gamma + r1 %*% t(r1)
            }
          }
        }
      }
      n = n + 1
    }
  }
  WT = c(); Q = list(); perm = list(); size = list()
  for (i in c(1:p)){
    for (j in c(1:r[i])){
      for (k in c(1:K)){
        if (length(Q) < K){
          perm[[k]] = c(k+1, (1:K)[-k] + 1, 1)
          size[[k]] = c(dim[k], prod(dim[-k]), t)
          s = if (is.na(prod(dim[(k+1):K]))) 1 else prod(dim[(k+1):K])
          Q[[k]] = kronecker(diag(s), pm(dim[k], prod(dim[0:(k-1)])))
        }
        # AXX = AX[[i]][[j]][[k]] # AXX = rTensor::ttl(xx, A.new[[i]][[j]][-k], c(2:(K+1))[-k])
        AXX = abind::asub(AX[[i]][[j]][[k]], (1+p-i):(t-i), 1, drop=FALSE)
        AXfold = array(aperm(AXX@data, perm[[k]]), size[[k]])
        AXI = apply(AXfold,3,function(x){kronecker(x, diag(dim[k])) %*% Q[[k]]})
        AXI.array <- array(AXI,c(dim[k]^2,pdim,t))
        WT <- abind(WT, AXI.array,along=1)
      }
    }
  }
  WT = aperm(WT, c(3,1,2))
  WSigma <- tensor(WT,Sigma,3,1) #t*(d1^2+d2^2+d^3)*(d1d2d3)
  EWSigmaWt <- tensor(WSigma,WT,c(3,1),c(3,1))/t
  H <- tensor(WT,WT,c(3,1),c(3,1))/t + Gamma #r(d1^2+d2^2+d^2)*r(d1^2+d2^2+d^3)
  Hinv <- solve(H)
  Xi <- Hinv %*% EWSigmaWt %*% Hinv
  return(Xi/t)
}


tenAR.SE.MLE <- function(dim, r, p, K, t, AX, A, Sigma){
  # dim = dim; r=R; p=P; K=K; t=t; AX = AX; A=A.new; Sigma=Sig;
  pdim = prod(dim) # d1d2d3
  fdim = dim**2
  ndim <- sum(fdim) # d1^2+d2^2+d^3
  Gamma <- matrix(0,sum(r)*ndim,sum(r)*ndim)
  r1 <- matrix(0, sum(r)*ndim, 1)
  n = 0
  for (i in c(1:p)){
    for (j in c(1:r[i])){
      for (k in c(1:(K-1))){
        r1 <- matrix(0, sum(r)*ndim, 1)
        a = as.vector(A[[i]][[j]][[k]])
        r1[( n*ndim + sum(fdim[0:(k-1)]) + 1): (n*ndim + sum(fdim[0:k])),] = a
        Gamma = Gamma + r1 %*% t(r1)
        if (K==2){
          for (l in c(1:r[i])){
            if (l != j){
              r1 <- matrix(0, sum(r)*ndim, 1)
              a = as.vector(A[[i]][[l]][[k]])
              r1[( n*ndim + sum(fdim[0:(k-1)]) + 1): (n*ndim + sum(fdim[0:k])),] = a
              Gamma = Gamma + r1 %*% t(r1)
            }
          }
        }
      }
      n = n + 1
    }
  }
  WT = c(); Q = list(); perm = list(); size = list()
  for (i in c(1:p)){
    for (j in c(1:r[i])){
      for (k in c(1:K)){
        if (length(Q) < K){
          perm[[k]] = c(k+1, (1:K)[-k] + 1, 1)
          size[[k]] = c(dim[k], prod(dim[-k]), t)
          s = if (is.na(prod(dim[(k+1):K]))) 1 else prod(dim[(k+1):K])
          Q[[k]] = kronecker(diag(s), pm(dim[k], prod(dim[0:(k-1)])))
        }
        AXX = AX[[i]][[j]][[k]]
        AXfold = array(aperm(AXX@data, perm[[k]]), size[[k]])
        AXI = apply(AXfold,3,function(x){kronecker(x, diag(dim[k])) %*% Q[[k]]})
        AXI.array <- array(AXI,c(dim[k]^2,pdim,t))
        WT <- abind(WT, AXI.array,along=1)
      }
    }
  }
  WT = aperm(WT, c(3,1,2))
  WSigma <- tensor(WT,solve(Sigma),3,1) 
  EWSigmaWt <- tensor(WSigma,WT,c(3,1),c(3,1))/t
  H <- EWSigmaWt + Gamma
  Hinv <- solve(H)
  Xi <- Hinv %*% EWSigmaWt %*% Hinv
  return(Xi/t)
}


tenAR.bic <- function(xx, rmax=5){
  if (mode(xx) != "S4") {xx <- rTensor::as.tensor(xx)}
  dim <- xx@modes[-1]
  t <- xx@modes[[1]]
  ans <- c()
  for (r in c(1:rmax)){
    est <- tenAR.LS(xx, R=r, P=1)
    res <- est$res
    ans[r] <- IC(xx,res,r,t, dim)
  }
  which.min(ans)
}

#' Plot Matrix-Valued Time Series
#'
#' Plot matrix-valued time series, can be also used to plot a given slice of a tensor-valued time series.
#'@name mplot
#'@rdname mplot
#'@aliases mplot
#'@export
#'@importFrom graphics par
#'@param xx  \eqn{T \times d_1 \times d_2} matrix-valued time series. Note that the number of mode is 3, where the first mode is time.
#'@return a figure.
#'@examples
#' dim <- c(3,3,3)
#' xx <- tenAR.sim(t=50, dim, R=2, P=1, rho=0.5, cov='iid')
#' mplot(xx[1:30,,,1])
mplot <- function(xx){
  if (mode(xx) == "S4"){xx = xx@data}
  dim = dim(xx)
  time = array(c(1:dim[1]),dim[1])
  opar <- par(mfrow=c(dim[2],dim[3]),mai=0.05*c(1,1,1,1),oma=c(2,2,0,0))
  on.exit(par(opar))
  for(i in 1:dim[2]){
    for(j in 1:dim[3]){
      if(i!=dim[2] & j!=1){
        plot(time,xx[,i,j],type='l',xaxt='n',yaxt='n',ylim=range(xx[,i,]))
      }
      if(i!=dim[2] & j==1){
        plot(time,xx[,i,j],type='l',xaxt='n',ylim=range(xx[,i,]))
      }
      if(i==dim[2] & j!=1){
        plot(time,xx[,i,j],type='l',yaxt='n',ylim=range(xx[,i,]))
      }
      if(i==dim[2] & j==1){
        plot(time,xx[,i,j],type='l',ylim=range(xx[,i,]))
      }
    }
  }
  
}


#' Plot ACF of Matrix-Valued Time Series
#'
#' Plot ACF of matrix-valued time series, can be also used to plot ACF of a given slice of a tensor-valued time series.
#'@name mplot.acf
#'@rdname mplot.acf
#'@aliases mplot.acf
#'@export
#'@importFrom graphics par
#'@importFrom graphics plot
#'@importFrom stats acf
#'@param xx  \eqn{T \times d_1 \times d_2} matrix-valued time series. Note that the number of mode is 3, where the first mode is time.
#'@return a figure.
#'@examples
#' dim <- c(3,3,3)
#' xx <- tenAR.sim(t=50, dim, R=2, P=1, rho=0.5, cov='iid')
#' mplot.acf(xx[1:30,,,1])
mplot.acf <- function(xx){
  if (mode(xx) == "S4"){xx = xx@data}
  dim = dim(xx)
  opar <- par(mfrow=c(dim[2],dim[3]),mai=0.05*c(1,1,1,1),oma=c(2,2,0,0))
  on.exit(par(opar))
  for(i in 1:dim[2]){
    for(j in 1:dim[3]){
      if(i!=dim[2] & j!=1){
        acf(xx[,i,j])
      }
      if(i!=dim[2] & j==1){
        acf(xx[,i,j])
      }
      if(i==dim[2] & j!=1){
        acf(xx[,i,j])
      }
      if(i==dim[2] & j==1){
        acf(xx[,i,j])
      }
    }
  }
}

#' Predictions for Tensor Autoregressive Models
#'
#' Prediction based on the tensor autoregressive model or reduced rank MAR(1) model. If \code{rolling = TRUE}, returns the rolling forecasts.
#'@name tenAR.predict
#'@rdname tenAR.predict
#'@aliases tenAR.predict
#'@export
#'@importFrom abind abind
#'@param object a model object returned by \code{tenAR.est()}.
#'@param n.ahead prediction horizon.
#'@param xx \eqn{T^{\prime} \times d_1 \times \cdots \times d_K} new tensor time series to be used for prediction. Must have at least \code{n.ahead} length.
#'@param rolling TRUE or FALSE, rolling forecast, is FALSE by default.
#'@param n0 only if \code{rolling = TRUE}, the starting point of rolling forecast.
#'@return
#'a tensor time series of length \code{n.ahead} if \code{rolling = FALSE};
#'
#'a tensor time series of length \eqn{T^{\prime} - n_0 - n.ahead + 1} if \code{rolling = TRUE}.
#'@seealso 'predict.ar' or 'predict.arima'
#'@examples
#' set.seed(333)
#' dim <- c(2,2,2)
#' t = 20
#' xx <- tenAR.sim(t, dim, R=2, P=1, rho=0.5, cov='iid')
#' est <- tenAR.est(xx, R=1, P=1, method="LSE")
#' pred <- tenAR.predict(est, n.ahead = 1, xx = xx)
#' # rolling forcast
#' n0 = t - min(50,t/2)
#' pred.rolling <- tenAR.predict(est, n.ahead = 5, xx = xx, rolling=TRUE, n0)
#'
#' # prediction for reduced rank MAR(1) model
#' dim <- c(2,2)
#' t = 20
#' xx <- tenAR.sim(t, dim, R=1, P=1, rho=0.5, cov='iid')
#' est <- matAR.RR.est(xx, method="RRLSE", k1=1, k2=1)
#' pred <- tenAR.predict(est, n.ahead = 1)
#' # rolling forcast
#' n0 = t - min(50,t/2)
#' pred.rolling <- tenAR.predict(est, n.ahead = 5, rolling=TRUE, n0=n0)
tenAR.predict <- function(object, n.ahead=1, xx=NULL, rolling=FALSE, n0=NULL){
  if (is.null(object$SIGMA)){method = "LSE"} else {method = "MLE"}
  if (!is.null(object$A1)){
    A <- list(list(list(object$A1, object$A2)))
    if (is.null(object$Sig1)){
      method = "RRLSE"
    } else {
      method = "RRMLE"
    }
  } else if (!is.null(object$coef)){
    A <- list(list(object$coef))
    method = "VAR"
  } else {
    A <- object$A
  }
  if (is.null(xx)){xx = object$data}
  print(mode(xx))
  if (mode(xx) != "S4") {xx <- rTensor::as.tensor(xx)}
  if (rolling == TRUE){
    return(predict.rolling(A, xx, n.ahead, method, n0))
  }
  P <- length(A)
  R <- sapply(c(1:P), function(l){length(A[[l]])})
  K <- xx@num_modes - 1
  dim <- xx@modes
  ttt <- (dim[1]+1):(dim[1]+n.ahead)
  for(tt in ttt){
    L1 = 0
    for (l in c(1:P)){
      if (R[l] == 0) next
      L1 <- L1 + Reduce("+",lapply(c(1:R[l]), function(n) {rTensor::ttl(abind::asub(xx, tt-l, 1, drop=FALSE), A[[l]][[n]], (c(1:K) + 1))}))
    }
    xx <- as.tensor(abind(xx@data, L1@data, along=1))
  }
  return(abind::asub(xx@data, ttt, 1, drop=FALSE))
}


predict.rolling <- function(A, xx, n.ahead, method, n0){
  if ((method == "RRLSE") || (method == "RRMLE")){
    k1 <- rankMatrix(A[[1]][[1]][[1]])
    k2 <- rankMatrix(A[[1]][[1]][[2]])
  }
  P <- length(A)
  R <- sapply(c(1:P), function(l){length(A[[l]])})
  K <- xx@num_modes - 1
  dim <- xx@modes
  t <- dim[1]
  if(is.null(n0)){n0 = t - min(50,t/2)}
  ttt <- (n0):(t - n.ahead)
  for(tt in ttt){
    tti <- tt - ttt[1] + 1
    print(paste("rolling forcast t =", tti))
    if (method == "RRLSE"){
      model <- MAR1.RR(abind::asub(xx@data, 1:tt, 1, drop=FALSE), k1, k2)
      A <- list(list(list(model$A1, model$A2)))
    } else if (method == "RRMLE"){
      model <- MAR1.CC(abind::asub(xx@data, 1:tt, 1, drop=FALSE), k1, k2)
      A <- list(list(list(model$A1, model$A2)))
    } else {
      model = tenAR.est(abind::asub(xx@data, 1:tt, 1, drop=FALSE), R, c(P,0), method)
      A <- model$A
    }
    L1 = 0
    for (l in c(1:P)){
      if (R[l] == 0) next
      L1 <- L1 + Reduce("+",lapply(c(1:R[l]), function(n) {rTensor::ttl(abind::asub(xx, tt-l+1, 1, drop=FALSE), A[[l]][[n]], (c(1:K) + 1))}))
    }
    if (tti == 1){xx.pred = L1@data} else {xx.pred = abind(xx.pred, L1@data, along=1)}
  }
  return(xx.pred)
}



.getpos <- function(mode, rank){
  pos = 0
  for (k in c(1:length(mode))){
    if (k > 1){mode[k] = mode[k] - 1}
    pos = pos + rank[k]*mode[k]
  }
  return(pos)
}

.getrank <- function(dim){
  rank = array(1, length(dim))
  for (k in c(1:length(dim))){
    if (k > 1){ for (q in c(1:(k-1))){rank[k] = rank[k]*(rev(dim)[q])}}
  }
  return(rank)
}

.remove.mean <- function(xx){
  dim <- xx@modes
  m <- apply(xx@data, c(2:xx@num_modes), mean)
  mm <- aperm(array(m, c(dim[-1],dim[1])), c(xx@num_modes,c(1:(xx@num_modes-1))))
  return(xx - mm)
}


sig.proj <- function(xx){
  est = tenAR.MLE(xx,R=1,P=1)
  return(list(sig1=est$sig[[1]],sig2=est$sig[[2]]))
}




exponential.smooth <- function(x, lambda){
  if(length(lambda) > 1)
    stop("lambda must be a single number")
  if(lambda > 1 || lambda <= 0)
    stop("lambda must be between zero and one")
  xlam <- x * lambda
  xlam[1] <- x[1]
  stats::filter(xlam, filter = 1 - lambda, method = "rec")
}



evaluate_RMSE <- function(pred, data, type='overall', truth=FALSE){
  # This functions takes the prediction and data, and type as input, then calculates RMSE according to the type.
  # pred: A tensor which order is c(rolling.periods, country.numbers, event.numbers, step)
  # data: Original data which order is  c(time, country.number, event.number)
  # type: "overall": calculate the overall RMSE and return a single value.
  #       "step": calculate the RMSE for each step and return a 1*4 array.
  #       "country.step": calculate the RMSE for each country and step. A 260*4 array will be returned.
  #       "event.step": calculate the RMSE for each event and step. A 20*4 array will be returned.
  #       "detail": calculate the RMSE for each (country, event) and step. A 260*20*4 tensor will be returned.
  # Please note if you want to improve the speed of this evaluation function. Please generate truth in advance and set truth = TRUE.
  
  if(!(type %in% c('overall', 'step', 'country.step', 'event.step', 'detail'))){
    stop("Type should be selected from c('overall', 'step', 'country.step', 'event.step', 'detail')")
  }  
  pred.horizon = dim(data)[1] -100
  country.number = dim(data)[2]
  event.number = dim(data)[3]
  n_ahead = 4
  if(truth){
    truth = data
  }else{
    truth = array(0,c(pred.horizon-3,country.number,event.number,n_ahead))
    for(t0 in 1:4){
      truth[,,,t0] = data[(100+t0):(100+t0+pred.horizon-4),,]
    }    
  }
  
  mse_col = apply((pred - truth)^2, c(2,3,4), mean)
  if(type == 'overall'){
    rmse = mean(sqrt(apply(mse_col, c(1,2), mean)))
  }else if(type == 'step'){
    rmse = array(apply(sqrt(mse_col), 3, mean), c(1,4))
    colnames(rmse) = c('Step1','Step2','Step3','Step4')  
  }else if(type == 'country.step'){
    rmse = array(apply(sqrt(mse_col), c(1,3), mean), c(country.number, n_ahead))
    dimnames(rmse) = list(dimnames(data)[[2]], c('Step1','Step2','Step3','Step4'))
  }else if(type == 'event.step'){
    rmse = array(apply(sqrt(mse_col), c(2,3), mean), c(event.number, n_ahead))
    dimnames(rmse) = list(dimnames(data)[[3]], c('Step1','Step2','Step3','Step4'))
  }else{
    rmse = sqrt(mse_col)
    dimnames(rmse) = list(dimnames(data)[[2]], dimnames(data)[[3]], c('Step1','Step2','Step3','Step4'))
  }
  return(rmse)
}


evaluate_MASE <- function(pred, data, type='overall', truth=FALSE){
  # This function takes the prediction, data and type as input, then calculates MASE in different ways according to the type.
  # pred: A tensor which order is c(rolling.periods, country.numbers, event.numbers, step).
  # data: Original data which order is c(time, country.number, event.number).
  # type: "overall": calculate the overall MASE and return a single value.
  #       "step": calculate the MASE for each step and return a 1*4 array.
  #       "country.step": calculate the MASE for each country and step. A 260*4 array will be returned.
  #       "event.step": calculate the MASE for each event and step. A 20*4 array will be returned.
  #       "detail": calculate the MASE for each (country, event) and step. A 260*20*4 tensor will be returned.
  # Please note if you want to improve the speed of this evaluation function. Please generate truth in advance and set truth = TRUE.
  
  
  if(!(type %in% c('overall', 'step', 'country.step', 'event.step', 'detail'))){
    stop("Type should be selected from c('overall', 'step', 'country.step', 'event.step', 'detail')")
  }  
  pred.horizon = dim(data)[1] -100
  country.number = dim(data)[2]
  event.number = dim(data)[3]
  n_ahead = 4
  if(truth){
    truth = data
  }else{
    truth = array(0,c(pred.horizon-3,country.number,event.number,n_ahead))
    for(t0 in 1:4){
      truth[,,,t0] = data[(100+t0):(100+t0+pred.horizon-4),,]
    }    
  }
  
  
  naive_error = apply(abs(apply(data, c(2,3), diff)), c(2,3), mean)
  pred_error = apply(abs(pred-truth), c(2,3,4), mean)
  # Generate MASE according to type.
  if(type == 'overall'){
    mase = sum(apply(pred_error, c(1,2), mean))/sum(naive_error)
  }else if(type == 'step'){
    mase = array(apply(pred_error, 3, sum)/sum(naive_error), c(1,n_ahead))
    dimnames(mase) = list('RMSE',c('Step1','Step2','Step3','Step4'))
  }else if(type == 'country.step'){
    mase = array(0, c(country.number, n_ahead))
    dimnames(mase) = list(dimnames(data)[[2]], c('Step1','Step2','Step3','Step4'))
    for(t0 in 1:n_ahead){
      mase[, t0] = apply(pred_error[,,t0], 1, sum)/apply(naive_error, 1, sum)
    }
  }else if(type == 'event.step'){
    mase = array(0, c(event.number, n_ahead))
    dimnames(mase) = list(dimnames(data)[[3]], c('Step1','Step2','Step3','Step4'))
    for(t0 in 1:n_ahead){
      mase[, t0] = apply(pred_error[,,t0], 2, sum)/apply(naive_error, 2, sum)
    }
  }else{
    mase = array(0, c(country.number, event.number, n_ahead))
    dimnames(mase) = list(dimnames(data)[[2]], dimnames(data)[[3]], c('Step1','Step2','Step3','Step4'))
    for(t0 in 1:n_ahead){
      mase[,,t0] = pred_error[,,t0]/naive_error
    }
  }
  
  return(mase)
}


ten.dis.A <- function(A, B, R, K){
  P = length(R)
  dis <- 0
  for (p in c(1:P)){
    if (R[p] == 0) next
    for (r in c(1:R[p])){
      for (k in c(1:K)){
        dis <- dis + min(sum((A[[p]][[r]][[k]] - B[[p]][[r]][[k]])^2), sum((A[[p]][[r]][[k]] + B[[p]][[r]][[k]])^2))
      }
    }
  }
  return(sqrt(dis))
}

tenAR.LSP <- function(xx, PP=NULL, init.A=NULL, niter=150, tol=1e-5, print.true=FALSE){
  
  if (!(mode(xx) == "S4")) {xx <- as.tensor(xx)}
  dim <- dim(xx)[-1]
  K <- length(dim)
  t <- dim(xx)[1]
  R = rep(0, PP)
  R[PP] = 1
  Tol <- tol*sqrt(sum(dim^2))*sum(R)
  dis <- 1
  iiter <- 1
  A.old = list()
  A.old[[PP]] <- lapply(1:R[PP], function(j) {lapply(1:K, function(i) {0.5*diag(dim[i])})})
  A.new <- A.old
  AX = A.new
  
  while(iiter <= niter & dis >= tol){
    for (k in c(K:1)){
      AX[[PP]][[1]][[k]] = rTensor::ttl(xx, A.new[[PP]][[1]][-k], c(2:(K+1))[-k])
      temp = abind::asub(AX[[PP]][[1]][[k]], 1:(t-PP), 1, drop=FALSE)
      temp2 <- abind::asub(xx, (1+PP):t, 1, drop=FALSE) - 0
      RR <- tensor(temp@data,temp@data,c(1:(K+1))[-(k+1)],c(1:(K+1))[-(k+1)])
      LL <- tensor(temp2@data,temp@data,c(1:(K+1))[-(k+1)],c(1:(K+1))[-(k+1)])
      A.new[[PP]][[1]][[k]] <- LL %*% ginv(RR)
    }
    dis = ten.dis.A(A.new, A.old, R, K)
    A.new[[PP]] <- svd.rescale(A.new[[PP]])
    A.old <- A.new
    iiter <- iiter + 1
  }
  
  A.new[[PP]] <- fro.order(fro.rescale(A.new[[PP]]))
  return(list(A=A.new))
  
}


tenAR.MLEP <- function(xx, PP=NULL, niter=150, tol=1e-5){
  
  if (!(mode(xx) == "S4")) {xx <- as.tensor(xx)}
  dim <- dim(xx)[-1]
  K <- length(dim)
  t <- dim(xx)[1]
  R = rep(0, PP)
  R[PP] = 1
  Tol <- tol*sqrt(sum(dim^2))*sum(R)
  dis <- 1
  iiter <- 1
  A.old = list()
  A.old[[PP]] <- lapply(1:R[PP], function(j) {lapply(1:K, function(i) {0.5*diag(dim[i])})})
  A.new <- A.old
  AX = A.new
  
  Sig.old <- lapply(1:K, function(i) {diag(dim[i])})
  Sig.new <- Sig.old
  Sig.new.inv <- lapply(1:K, function (k) {solve(Sig.new[[k]])})
  
  while(iiter <= niter & dis >= Tol){
    
    for (k in c(K:1)){
      sphi <-  lapply(1:K, function (k) {Sig.new.inv[[k]] %*% A.new[[PP]][[1]][[k]]})
      AX[[PP]][[1]][[k]] = rTensor::ttl(xx, A.new[[PP]][[1]][-k], c(2:(K+1))[-k])
      temp = abind::asub(AX[[PP]][[1]][[k]], 1:(t-PP), 1, drop=FALSE)
      temp1 <- abind::asub(rTensor::ttl(xx, sphi[-k], c(2:(K+1))[-k]), 1:(t-PP), 1, drop=FALSE)
      temp2 <- abind::asub(xx, (1+PP):t, 1, drop=FALSE)
      RR <- tensor(temp@data,temp1@data,c(1:(K+1))[-(k+1)],c(1:(K+1))[-(k+1)])
      LL <- tensor(temp2@data,temp1@data,c(1:(K+1))[-(k+1)],c(1:(K+1))[-(k+1)])
      A.new[[PP]][[1]][[k]] <- LL %*% ginv(RR)
    }
    
    for (k in c(K:1)){
      res.old <- abind::asub(xx, (1+PP):(t), 1, drop=FALSE) - rTensor::ttl(abind::asub(xx, 1:(t-PP), 1, drop=FALSE), A.new[[PP]][[1]], (c(1:K) + 1))
      rs <- rTensor::ttl(res.old, Sig.new.inv[-k], c(2:(K+1))[-k])
      Sig.new[[k]] <- tensor(res.old@data, rs@data, c(1:(K+1))[-(k+1)],c(1:(K+1))[-(k+1)])/(t-1)/prod(dim[-k])
      Sig.new.inv <- lapply(1:K, function (k) {ginv(Sig.new[[k]])})
    }
    
    A.new[[PP]] <- svd.rescale(A.new[[PP]])
    Sig.new <- eigen.rescale(list(Sig.new))[[1]]
    dis = ten.dis.A(A.new, A.old, R, K)
    Sig.old <- Sig.new
    A.old <- A.new
    iiter <- iiter + 1
  }
  
  A.new[[PP]] <- fro.order(fro.rescale(A.new[[PP]]))
  
  
  return(list(A=A.new))
}

#' Estimation for Tucker structure Factor Models of Tensor-Valued Time Series
#'
#' Estimation function for Tucker structure factor models of tensor-valued time series.
#' Two unfolding methods of the auto-covariance tensor, Time series Outer-Product Unfolding Procedure (TOPUP), Time series Inner-Product Unfolding Procedure (TIPUP),
#' are included, as determined by the value of \code{method}.
#'@details
#' Tensor factor model with Tucker structure has the following form,
#' \deqn{X_t = F_t \times_{1} A_1 \times_{2} \cdots \times_{K} A_k + E_t,}
#' where \eqn{A_k} is the deterministic loading matrix of size \eqn{d_k \times r_k} and \eqn{r_k \ll d_k},
#' the core tensor \eqn{F_t} itself is a latent tensor factor process of dimension \eqn{r_1 \times \cdots \times r_K},
#' and the idiosyncratic noise tensor \eqn{E_t} is uncorrelated (white) across time. Two estimation approaches, named TOPUP and TIPUP, are studied.
#' Time series Outer-Product Unfolding Procedure (TOPUP) are based on
#' \deqn{ {\rm{TOPUP}}_{k}(X_{1:T}) = \left(\sum_{t=h+1}^T \frac{{\rm{mat}}_{k}( X_{t-h}) \otimes {\rm{mat}}_k(X_t)} {T-h}, \ h=1,...,h_0 \right),}
#' where \eqn{h_0} is a predetermined positive integer, \eqn{\otimes} is tensor product. Note that
#' \eqn{ {\rm{TOPUP}}_k(\cdot)} is a function mapping a tensor time series to an order-5 tensor.
#' Time series Inner-Product Unfolding Procedure (TIPUP) replaces the tensor product in TOPUP with the inner product:
#' \deqn{ {\rm{TIPUP}}_k(X_{1:T})={\rm{mat}}_1\left(\sum_{t=h+1}^T \frac{{\rm{mat}}_k(X_{t-h}) {\rm{mat}}_k^\top(X_t)} {T-h}, \ h=1,...,h_0 \right).}
#'@name tenFM.est
#'@rdname tenFM.est
#'@aliases tenFM.est
#'@usage tenFM.est(x,r,h0=1,method='TIPUP',iter=TRUE,tol=1e-4,maxiter=100)
#'@export
#'@importFrom stats varimax
#'@param x \eqn{T \times d_1 \times \cdots \times d_K} tensor-valued time series.
#'@param r input rank of factor tensor.
#'@param h0 the number of lags used in auto-covariance tensor.
#'@param method character string, specifying the type of the estimation method to be used. \describe{
#'  \item{\code{"TIPUP",}}{TIPUP method.}
#'  \item{\code{"TOPUP",}}{TOPUP method.}
#'}
#'@param iter boolean, specifying using an iterative approach or an non-iterative approach.
#'@param tol tolerance in terms of the Frobenius norm.
#'@param maxiter maximum number of iterations if error stays above \code{tol}.
#'@return returns a list containing the following:\describe{
#'\item{\code{Ft}}{estimated factor processes of dimension \eqn{T \times r_1 \times r_2 \times \cdots \times r_k}.}
#'\item{\code{Ft.all}}{Summation of factor processes over time, of dimension \eqn{r_1,r_2,\cdots,r_k}.}
#'\item{\code{Q}}{a list of estimated factor loading matrices \eqn{Q_1,Q_2,\cdots,Q_K}. }
#'\item{\code{x.hat}}{fitted signal tensor, of dimension \eqn{T \times d_1 \times d_2 \times \cdots \times d_k}.}
#'\item{\code{niter}}{number of iterations.}
#'\item{\code{fnorm.resid}}{Frobenius norm of residuals, divide the Frobenius norm of the original tensor.}
#'}
#'@references
#'Chen, Rong, Dan Yang, and Cun-Hui Zhang. "Factor models for high-dimensional tensor time series." Journal of the American Statistical Association (2021): 1-59.
#'
#'Han, Yuefeng, Rong Chen, Dan Yang, and Cun-Hui Zhang. "Tensor factor model estimation by iterative projection." arXiv preprint arXiv:2006.02611 (2020).
#'
#'@examples
#'set.seed(333)
#'dims <- c(16,18,20) # dimensions of tensor time series
#'r <- c(3,3,3)  # dimensions of factor series
#'Ft <- tenAR.sim(t=100, dim=r, R=1, P=1, rho=0.9, cov='iid')
#'lambda <- sqrt(prod(dims))
#'x <- tenFM.sim(Ft,dims=dims,lambda=lambda,A=NULL,cov='iid') # generate t*dims tensor time series
#'result <- tenFM.est(x,r,h0=1,iter=TRUE,method='TIPUP')  # Estimation
#'Ft <- result$Ft
tenFM.est=function(x,r,h0=1,method='TIPUP',iter=TRUE,tol=1e-4,maxiter=100){
  x <- aperm(x,c(2:length(dim(x)),1))
  dd <- dim(x)
  d <- length(dd) # d >= 2
  d.seq <- 1:(d-1)
  n <- dd[d]
  
  x.tnsr <- as.tensor(x)
  tnsr.norm <- fnorm(x.tnsr)
  if(method=="TIPUP"){
    ans.init <- tipup.init.tensor(x,r,h0,norm.true=TRUE)
  }else if(method=="TOPUP"){
    ans.init <- topup.init.tensor(x,r,h0,norm.true=TRUE)
  }else{
    stop('Wrong method !')
  }
  ddd=dd[-d]
  iiter <- 1
  dis <- 1
  fnorm.resid <- rep(0,maxiter+2)
  
  x.hat <- ttl(x.tnsr,lapply(ans.init$Q,t),d.seq)
  x.hat <- ttl(x.hat,ans.init$Q,d.seq)
  
  fnorm.resid[1] <- fnorm(x.tnsr-x.hat)/tnsr.norm
  ans.Q <- ans.init$Q
  Ft <- ttl(x.tnsr,lapply(ans.Q,t),d.seq)
  Ft.all <- apply(Ft@data,c(1:(d-1)),sum)
  fnorm.resid[iiter+1] <- fnorm(x.tnsr-x.hat)/tnsr.norm
  
  if(iter==TRUE){
    while((dis > tol) & (iiter < maxiter)){
      for(i in 1:(d-1)){
        x.new <- aperm(ttl(x.tnsr,lapply(ans.Q[-i],t),ms=d.seq[-i])@data,c(i,d.seq[-i],d))
        if(method=="TIPUP"){
          ans.iter <- tipup.init.tensor(x.new,c(r[i],r[-i]),h0,oneside.true=TRUE,norm.true=FALSE)
          ans.Q[[i]] <- ans.iter$Q[[1]]
        }else if(method=="TOPUP"){
          ans.iter <- topup.init.tensor(x.new,c(r[i],r[-i]),h0,oneside.true=TRUE,norm.true=FALSE)
          ans.Q[[i]] <- ans.iter$Q[[1]]
        }else{
          stop('Wrong method !')
        }
        ddd=dd[-d]
        
        x.hat <- ttl(x.tnsr,lapply(ans.Q,t),d.seq)
        x.hat <- ttl(x.hat,ans.Q,d.seq)
        Ft <- ttl(x.tnsr,lapply(ans.Q,t),d.seq)
        Ft.all <- apply(Ft@data,c(1:(d-1)),sum)
        
        fnorm.resid[iiter+1] <- fnorm(x.tnsr-x.hat)/tnsr.norm
        dis <- abs(fnorm.resid[iiter+1] - fnorm.resid[iiter])
        if(iiter==1){
          Qfirst <- ans.Q
          x.hat.first <- x.hat@data
        }
        iiter <- iiter + 1
      }
    }
  }else{
    iiter <- iiter + 1
  }
  fnorm.resid <- fnorm.resid[fnorm.resid != 0]
  fnorm.resid <- fnorm.resid^2
  x0 <- matrix(x,prod(dd[-d]))
  x0 <- t(scale(t(x0),scale=FALSE) )
  x0 <- array(x0,dd)
  return(list("Ft"=aperm(Ft@data,c(d,1:(d-1))),"Ft.all"=Ft.all,"Q"=ans.Q,"x.hat"=aperm(x.hat@data,c(d,1:(d-1))),"niter"=iiter,"fnorm.resid"=fnorm.resid[iiter]))
}

#' Rank Determination for Tensor Factor Models with Tucker Structure
#'
#' Function for rank determination of tensor factor models with Tucker Structure.
#' Two unfolding methods of the auto-covariance tensor, Time series Outer-Product Unfolding Procedure (TOPUP), Time series Inner-Product Unfolding Procedure (TIPUP),
#' are included, as determined by the value of \code{method}.
#' Different penalty functions for the information criterion (IC) and the eigen ratio criterion (ER) can be used,
#' which should be specified by the value of \code{rank} and \code{penalty}. The information criterion resembles BIC in the vector factor model literature.
#' And the eigen ratio criterion is similar to the eigenvalue ratio based methods in the vector factor model literature.
#'@details
#' Let \eqn{W} be a \eqn{p\times p} symmetric and non-negative definite matrix and \eqn{\widehat{W}} be its sample version, \eqn{{\hat\lambda}_j} be the eigenvalues of \eqn{\widehat{W}}
#' such that \eqn{{\hat\lambda}_1\geq {\hat\lambda}_2 \geq \cdots \hat{\lambda}_p}.
#' The rank determination methods using the information criterion ("IC") and the eigen ratio criterion ("ER") are defined as follows:
#' \deqn{IC(\widehat{W}) = \mathrm{argmin}_{0\leq m \leq m^{*}} \left\{ \sum_{j=m+1}^{p} {\hat\lambda}_j + mg(\widehat{W}) \right\},}
#' \deqn{ER(\widehat{W}) = \mathrm{argmin}_{0\leq m \leq m^{*}} \left\{ \frac{{\hat\lambda}_{m+1}+h(\widehat{W})}{ {\hat\lambda}_m +h(\widehat{W})} \right\},}
#' where \eqn{m^{*}} is a predefined upper bound, \eqn{g} and \eqn{h} are some appropriate positive penalty functions. We have provided 5 choices for \eqn{g} and \eqn{h};
#' see more details in the argument "\code{penalty}".
#' For non-iterative TOPUP and TIPUP methods, \eqn{\widehat{W}} is
#' \eqn{ {\rm mat}_1({\rm{TOPUP}}_{k}(X_{1:T})) {\rm mat}_1({\rm{TOPUP}}_{k}(X_{1:T}))^\top } or
#' \eqn{ ({\rm{TIPUP}}_{k}(X_{1:T})) ({\rm{TIPUP}}_{k}(X_{1:T}))^\top }, for each tensor mode \eqn{k}, \eqn{1\leq k \leq K},
#' where \eqn{{\rm{TOPUP}}_{k}(X_{1:T})} and \eqn{{\rm{TIPUP}}_{k}(X_{1:T})} are defined in the Details section of the function \code{\link{tenFM.est}}.
#' For iterative TOPUP and TIPUP methods, we refer to the literature in the References section for more information.
#'@name tenFM.rank
#'@rdname tenFM.rank
#'@aliases tenFM.rank
#'@usage tenFM.rank(x,r,h0=1,rank='IC',method='TIPUP',inputr=FALSE,iter=TRUE,penalty=1,
#'delta1=0,tol=1e-4,maxiter=100)
#'@export
#'@param x \eqn{T \times d_1 \times \cdots \times d_K} tensor-valued time series.
#'@param r initial guess of the rank of factor tensor.
#'@param h0 the number of lags used in auto-covariance tensor.
#'@param rank character string, specifying the type of the rank determination method to be used. \describe{
#'  \item{\code{"IC",}}{information criterion.}
#'  \item{\code{"ER",}}{eigen ratio criterion.}
#'}
#'@param method character string, specifying the type of the factor estimation method to be used. \describe{
#'  \item{\code{"TIPUP",}}{TIPUP method.}
#'  \item{\code{"TOPUP",}}{TOPUP method.}
#'}
#'@param inputr boolean, if TRUE, always use initial guess rank r in each iteration; if FLASE, the rank will be updated in each iteration.
#'@param iter boolean, specifying using an iterative approach or a non-iterative approach.
#'@param penalty takes value in {1,2,3,4,5}, decides which penalty function to use for each tesnor mode \eqn{k}. Here \eqn{\nu} is a tuning parameter defined in the argument "\code{delta1}", and \eqn{d=\prod_{i=1}^{K} d_k }.
#' When \code{rank}= '\code{IC}':\cr
#' if \code{penalty}=1, \eqn{g_1= \frac{h_0 d^{2-2\nu}}{T}\log(\frac{dT}{d+T})};\cr
#' if \code{penalty}=2, \eqn{g_2= h_0 d^{2-2\nu}(\frac{1}{T}+\frac{1}{d})\log(\frac{dT}{d+T})};\cr
#' if \code{penalty}=3, \eqn{g_3= \frac{h_0 d^{2-2\nu}}{T} \log(\min{(d,T)})};\cr
#' if \code{penalty}=4, \eqn{g_4= h_0 d^{2-2\nu}(\frac{1}{T}+\frac{1}{d})\log(\min{(d,T)})};\cr
#' if \code{penalty}=5, \eqn{g_5= h_0 d^{2-2\nu}(\frac{1}{T}+\frac{1}{d})\log(\min{(d_k,T)})}.\cr
#' When \code{rank}= '\code{ER}':\cr
#' if \code{penalty}=1, \eqn{h_1= c_0 h_0};\cr
#' if \code{penalty}=2, \eqn{h_2= \frac{h_0 d^2}{T^2}};\cr
#' if \code{penalty}=3, \eqn{h_3= \frac{h_0 d^2}{T^2 d_k^2}};\cr
#' if \code{penalty}=4, \eqn{h_4= \frac{h_0 d^2}{T^2 d_k^2} + \frac{h_0 d_k^2}{T^2}};\cr
#' if \code{penalty}=5, \eqn{h_5= \frac{h_0 d^2}{T^2 d_k^2} + \frac{h_0 dd_k^2}{T^2}}.\cr
#'@param delta1 weakest factor strength, a tuning parameter used for IC method only
#'@param tol tolerance in terms of the Frobenius norm.
#'@param maxiter maximum number of iterations if error stays above \code{tol}.
#'@return return a list containing the following:\describe{
#'\item{\code{path}}{a \eqn{K \times (\rm{niter}+1)} matrix of the estimated Tucker rank of the factor process as a path of the maximum number of iteration (\eqn{\rm{niter}}) used. The first row is the estimated rank under non-iterative approach, the \eqn{i+1}-th row is the estimated rank \eqn{\hat r_1, \hat r_2, \cdots, \hat r_K} at \eqn{(i)}-th iteration.}
#'\item{\code{factor.num}}{final solution of the estimated Tucker rank of the factor process \eqn{\hat r_1, \hat r_2, \cdots, \hat r_K}.}
#'}
#'@references
#'Han, Yuefeng, Cun-Hui Zhang, and Rong Chen. "Rank Determination in Tensor Factor Model." Available at SSRN 3730305 (2020).
#'@examples
#'set.seed(333)
#'dims <- c(16,18,20) # dimensions of tensor time series
#'r <- c(3,3,3)  # dimensions of factor series
#'Ft <- tenAR.sim(t=100, dim=r, R=1, P=1, rho=0.9, cov='iid')
#'lambda <- sqrt(prod(dims))
#'x <- tenFM.sim(Ft,dims=dims,lambda=lambda,A=NULL,cov='iid') # generate t*dims tensor time series
#'rank <- tenFM.rank(x,r=c(4,4,4),h0=1,rank='IC',iter=TRUE,method='TIPUP')  # Estimate the rank
tenFM.rank = function(x,r=NULL,h0=1,rank='IC',method='TIPUP',inputr=FALSE,iter=TRUE,penalty=1,delta1=0,tol=1e-4,maxiter=100){
  x <- aperm(x,c(2:length(dim(x)),1))
  dd <- dim(x)
  d <- length(dd) # d >= 2
  d.seq <- 1:(d-1)
  n <- dd[d]
  x.tnsr <- as.tensor(x)
  tnsr.norm <- fnorm(x.tnsr)
  factor.num <- array(NA, c(d-1,5,maxiter+1))
  
  if(is.null(r)){
    r = rep(1,d-1)
  }
  
  if(method=="TIPUP"){
    ans.init <- tipup.init.tensor(x,r,h0,norm.true=TRUE)
  }else if(method=="TOPUP"){
    ans.init <- topup.init.tensor(x,r,h0,norm.true=TRUE)
  }
  else{
    stop('Wrong method !')
  }
  ddd=dd[-d]
  for(i in 1:(d-1)){
    if(rank=='BIC'|rank=='IC'){
      sigmas.hat=1
      factor.num[i,,1]=tensor.bic(ans.init$lambda[[i]]/sigmas.hat,h0,ddd[i],ddd[-i],n,delta1)
    }else if(rank=='ER'){
      factor.num[i,,1]=tensor.ratio(ans.init$lambda[[i]],ddd[i],ddd[-i],n)
    }else{
      stop('Wrong rank method !')
    }
  }
  r <- factor.num[,penalty,1]
  iiter <- 1
  dis <- 1
  fnorm.resid <- rep(0,maxiter+1)
  x.hat <- ttl(x.tnsr,lapply(ans.init$Q,t),d.seq)
  x.hat <- ttl(x.hat,ans.init$Q,d.seq)
  
  fnorm.resid[1] <- fnorm(x.tnsr-x.hat)/tnsr.norm
  ans.Q <- ans.init$Q
  
  if(iter==TRUE){
    for(i in 1:d){r[i] = min(dd[i],r[i]+1)}
    while((dis > tol) & (iiter < maxiter)){
      for(i in 1:(d-1)){
        x.new <- aperm(ttl(x.tnsr,lapply(ans.Q[-i],t),ms=d.seq[-i])@data,c(i,d.seq[-i],d))
        if(method=="TIPUP"){
          ans.iter <- tipup.init.tensor(x.new,c(r[i],r[-i]),h0,oneside.true=TRUE,norm.true=FALSE)
          ans.Q[[i]] <- ans.iter$Q[[1]]
        }else if(method=="TOPUP"){
          ans.iter <- topup.init.tensor(x.new,c(r[i],r[-i]),h0,oneside.true=TRUE,norm.true=FALSE)
          ans.Q[[i]] <- ans.iter$Q[[1]]
        }else{
          stop('Wrong estimation method input !')
        }
        ddd=dd[-d]
        
        if(rank=='BIC'|rank=='IC'){
          factor.num[i,,1+iiter]=tensor.bic(ans.iter$lambda[[1]]/sigmas.hat,h0,ddd[i],ddd[-i],n,delta1)
        }else if(rank=='ER'){
          factor.num[i,,1+iiter]=tensor.ratio(ans.iter$lambda[[1]],ddd[i],ddd[-i],n)
        }else{
          stop('Wrong rank method !')
        }
        if(inputr==FALSE){
          r[i]=factor.num[i,penalty,1+iiter]
        }
        
      }
      x.hat <- ttl(x.tnsr,lapply(ans.Q,t),d.seq)
      x.hat <- ttl(x.hat,ans.Q,d.seq)
      
      fnorm.resid[iiter+1] <- fnorm(x.tnsr-x.hat)/tnsr.norm
      dis <- abs(fnorm.resid[iiter+1] - fnorm.resid[iiter])
      if(iiter==1){
        Qfirst <- ans.Q
        x.hat.first <- x.hat@data
      }
      iiter <- iiter + 1
    }
  }else{
    iiter <- iiter + 1
  }
  
  # factor.num[,,maxiter]=factor.num[,,iiter]
  factor.num[,,maxiter]=factor.num[,,iiter-1]
  fnorm.resid <- fnorm.resid[fnorm.resid != 0]
  
  # label the factor number path
  path = t(factor.num[,penalty,1:(iiter)])
  path.rowname = c()
  for(ii in 1:iiter){path.rowname <- c(path.rowname,paste('iteration ',ii-1,sep=''))}
  path.colname = c()
  for(ii in 1:(d-1)){path.colname <- c(path.colname,paste('mode ',ii,sep=''))}
  rownames(path)=path.rowname
  colnames(path)=path.colname
  
  # return(list("path"=t(factor.num[,penalty,1:(iiter)]),"factor.num"=factor.num[,penalty,maxiter]))
  return(list("path"=path,"factor.num"=factor.num[,penalty,maxiter]))
}

#' Generate Tensor Time series using given Factor Process and Factor Loading Matrices
#'
#' Simulate tensor time series \eqn{X_t} using a given factor process \eqn{F_t}. The factor process \eqn{F_t} can be generated by the function \code{\link{tenAR.sim}}.
#'@details
#' Simulate from the model :
#' \deqn{X_t = \lambda F_t \times_{1} A_1 \times_{2} \cdots \times_{K} A_k + E_t,}
#' where \eqn{A_k} is the deterministic loading matrix of size \eqn{d_k \times r_k} and \eqn{r_k \ll d_k},
#' the core tensor \eqn{F_t} itself is a latent tensor factor process of dimension \eqn{r_1 \times \cdots \times r_K},
#' \eqn{\lambda} is an additional signal strength parameter,
#' and the idiosyncratic noise tensor \eqn{E_t} is uncorrelated (white) across time. In this function, by default \eqn{A_k} are orthogonal matrices.
#'@name tenFM.sim
#'@rdname tenFM.sim
#'@aliases tenFM.sim
#'@usage tenFM.sim(Ft,dims=NULL,lambda=1,A=NULL,cov='iid',rho=0.2)
#'@export
#'@importFrom stats rnorm
#'@param Ft input of the factor process, of dimension \eqn{T \times r_1 \times r_2 \times \cdots \times r_k}. It can be TenAR(p) tensor time series generated by the function \link{tenAR.sim}.
#'@param dims dimensions of the output tensor at each time,  \eqn{d_1\times d_2\cdots\times d_K}.
#'@param A a list of the factor loading matrices \eqn{A_1, A_2, \cdots, A_K}. The default is random orthogonal matrices \eqn{A_k} of dimension \eqn{d_k \times r_k}.
#'@param lambda signal strength parameter of the tensor factor models, see Details section for more information.
#'@param cov covariance matrix of the error tensor: identity ("iid"), separable Kronecker structure ("separable"), random ("random").
#'@param rho a parameter only for "separable" covariance matrix of the error tensor. It is the off-diagonal element of the error matrices, with the diagonal being 1.
#'@return A tensor-valued time series of dimension \eqn{T\times d_1\times d_2\cdots\times d_K}.
#'@seealso \code{\link{tenAR.sim}}
#'@examples
#'set.seed(333)
#'dims <- c(16,18,20) # dimensions of tensor time series
#'r <- c(3,3,3)  # dimensions of factor series
#'Ft <- tenAR.sim(t=100, dim=r, R=1, P=1, rho=0.9, cov='iid')
#'lambda <- sqrt(prod(dims))
#'# generate t*dims tensor time series with iid error covaraince structure
#'x <- tenFM.sim(Ft,dims=dims,lambda=lambda,A=NULL,cov='iid')
#'# generate t*dims tensor time series with separable error covaraince structure
#'x <- tenFM.sim(Ft,dims=dims,lambda=lambda,A=NULL,cov='separable',rho=0.2)
tenFM.sim <- function(Ft,dims=NULL,lambda=1,A=NULL,cov='iid',rho=0.2){
  r <- dim(Ft)[-1] #dimensions of the factor process
  t <- dim(Ft)[1] #length of output series
  dd <- length(dims)
  if(length(r)!=dd & is.null(A)){
    stop("Incorrect length K of input dims or A, for order K tensor time series.")
  }
  if(is.null(A)){
    X <- aperm(Ft,c(2:(dd+1),1))
    for(i in 1:dd){
      Ai = matrix(rnorm(dims[i]*r[i]),dims[i],r[i])
      Q = qr.Q(qr(Ai))
      X <- tensor(X,Q,1,2)
    }
  } else{
    X <- aperm(Ft,c(2:(dd+1),1))
    for(i in 1:dd){
      Ai = A[[i]]
      Q = qr.Q(qr(Ai))
      X <- tensor(X,Q,1,2)
    }
  }
  if (cov == "iid"){
    E <- array(rnorm(prod(dims)*t),c(t,dims))
  } else if (cov == "separable"){
    E <- array(rnorm(prod(dims)*t),c(dims,t)) # d_1 * ... * d_K * t
    for(i in 1:dd){
      cov.matrix <- matrix(rho,dims[i],dims[i]) + diag(rep(1-rho,dims[i]))
      x.eig <- eigen(cov.matrix)
      values <- x.eig$values
      values.sqrt <- rep(0,nrow(cov.matrix))
      values.sqrt[values > 0] <- sqrt(values[values > 0])
      cov.half <- x.eig$vectors %*% diag(values.sqrt) %*% t(x.eig$vectors)
      E <- tensor(E,cov.half,1,1) # d_{i+1} * ... * d_K * t * d_1 *... *d_i
    }
  } else if (cov == "random"){
    E <- matrix(rnorm(prod(dims)*t),t,prod(dims)) %*% qr.Q(qr(matrix(rnorm(prod(dims)^2),prod(dims))))
    E <- array(E,c(t,dims))
  } else {
    stop("Please specify cov")
  }
  X <- lambda * X + E
  #return(as.tensor(X))
  return(X)
}

tensor.bic<-function(reigen,h0=1,p1,p2,n,delta1=0){
  delta=2*delta1
  if(length(p2)>1){
    p2=prod(p2)
  }
  factor.p1=numeric(5)
  p=p1*p2
  m1=ceiling(p1/3)
  
  lambda.p1<-reigen[p1:1]
  cumlambda.p1<-cumsum(lambda.p1)
  cumlambda.p1<-cumlambda.p1[(p1-1):1]
  
  #threshold
  ic=cumlambda.p1[1:m1]/p^2*p^delta+(1:m1)*h0*(1/n)*log(p*n/(p+n))
  factor.p1[1]<-which.min(ic)
  ic=cumlambda.p1[1:m1]/p^2*p^delta+(1:m1)*h0*(1/n)*log(min(p,n))
  factor.p1[2]<-which.min(ic)
  ic=cumlambda.p1[1:m1]/p^2*p^delta+(1:m1)*h0*(1/n+1/p)*log(p*n/(p+n))
  factor.p1[3]<-which.min(ic)
  ic=cumlambda.p1[1:m1]/p^2*p^delta+(1:m1)*h0*(1/n+1/p)*log(min(p,n))
  factor.p1[4]<-which.min(ic)
  ic=cumlambda.p1[1:m1]/p^2*p^delta+(1:m1)*h0*(1/n+1/p)*log(min(p1,n))
  factor.p1[5]<-which.min(ic)
  
  factor.p1
}

tensor.ratio<-function(reigen,p1,p2,n){
  if(length(p2)>1){
    p2=prod(p2)
  }
  factor.p1=numeric(5)
  p=p1*p2
  m1=ceiling(p1/3)
  
  lambda.p1<-reigen[p1:1]
  cumlambda.p1<-cumsum(lambda.p1)
  cumlambda.p1<-cumlambda.p1[(p1-1):1]
  
  #ratio
  ratio<-lambda.p1[(p1-1):(p1-m1)]/lambda.p1[p1:(p1-m1+1)]
  factor.p1[1]<-which.min(ratio)
  ratio<-(lambda.p1[(p1-1):(p1-m1)] +p^2/n^2)/(lambda.p1[p1:(p1-m1+1)] +p^2/n^2)  #p^2*1/n
  factor.p1[2]<-which.min(ratio)
  ratio<-(lambda.p1[(p1-1):(p1-m1)] +p^2*(1/n^2/p1^2))/(lambda.p1[p1:(p1-m1+1)] +p^2*(1/n^2/p1^2))  #p^2*(1/n+1/p)
  factor.p1[3]<-which.min(ratio)
  ratio<-(lambda.p1[(p1-1):(p1-m1)] +p^2*(1/n^2/p1^2+1/n^2/p2^2))/(lambda.p1[p1:(p1-m1+1)] +p^2*(1/n^2/p1^2+1/n^2/p2^2)) #p^2*(1/n+1/p1)
  factor.p1[4]<-which.min(ratio)
  ratio<-(lambda.p1[(p1-1):(p1-m1)] +p^2*(1/n^2/p1+1/n^2/p2))/(lambda.p1[p1:(p1-m1+1)] +p^2*(1/n^2/p1+1/n^2/p2))  #p^2*(1/n)*log(p*n/(p+n))
  factor.p1[5]<-which.min(ratio)
  
  factor.p1
}

tipup.init.tensor <- function(x,r,h0=1,oneside.true=FALSE,norm.true=FALSE){
  # x: tensor of any dimension
  # TIPUP initialization (one step)
  # x: d1 * d2 * d3 * ... * d_d * n
  # if oneside.true==TRUE, then only compute the one sided column space,
  # not the other sides, this option is useful for the iterative method
  dd <- dim(x)
  d <- length(dd) # d >= 2
  n <- dd[d]
  dd.prod <- prod(dd) / n
  x.matrix <- matrix(x,ncol=n)
  if(oneside.true==TRUE){
    k=1
  } else{
    k=d-1
  }
  ans.M <- ans.Q <- ans.lambda <- NULL
  for(i in 1:k){
    M.temp <- matrix(0,dd[i],dd[i])
    for(h in 1:h0){
      x.left <- array(x.matrix[,1:(n-h)],c(dd[-d],n-h))
      x.right <- array(x.matrix[,(h+1):n],c(dd[-d],n-h))
      Omega <- tensor(x.left,x.right,c(1:d)[-i],c(1:d)[-i])/(n-h)
      M.temp <- M.temp + Omega %*% t(Omega)
    }
    #M.temp <- M.temp / dd.prod * dd[i]
    ans.eig <- eigen(M.temp)
    ans.M <- c(ans.M,list(M.temp))
    ans.Q <- c(ans.Q,list(ans.eig$vectors[,1:r[i],drop=FALSE]))
    ans.lambda <- c(ans.lambda,list(ans.eig$values))
  }
  norm.percent <- NULL
  x.hat <- NULL
  if(norm.true==TRUE){
    x.tnsr <- as.tensor(x)
    #x.hat <- get.hat(x.tnsr,ans.Q,1:k)
    x.hat <- ttl(x.tnsr,lapply(ans.Q,t),1:k)
    x.hat <- ttl(x.hat,ans.Q,1:k)
    norm.percent <- fnorm(x.tnsr-x.hat)/fnorm(x.tnsr)
    x.hat <- x.hat@data
  }
  list("M"=ans.M,"Q"=ans.Q,"lambda"=ans.lambda,"norm.percent"=norm.percent,"x.hat"=x.hat)
}

topup.init.tensor <- function(x,r,h0=1,oneside.true=FALSE,norm.true=FALSE){
  # x: tensor of any dimension
  # TOPUP initialization (one step)
  # x: d1 * d2 * d3 * ... * d_d * n
  # if oneside.true==TRUE, then only compute the one sided column space,
  # not the other sides, this option is useful for the iterative method
  
  dd <- dim(x)
  d <- length(dd) # d >= 2
  n <- dd[d]
  dd.prod <- prod(dd) / n
  x.matrix <- matrix(x,ncol=n)
  if(oneside.true==TRUE){
    k=1
  } else{
    k=d-1
  }
  ans.M <- ans.Q <- ans.lambda <- NULL
  # allocate 0 for all k matrices with different sizes
  for(i in 1:k){
    M.temp <- matrix(0,dd[i],dd[i])
    ans.M <- c(ans.M,list(M.temp))
  }
  for(h in 1:h0){
    x.left <- array(x.matrix[,1:(n-h)],c(dd[-d],n-h))
    x.right <- array(x.matrix[,(h+1):n],c(dd[-d],n-h))
    Omega <- tensor(x.left,x.right,d,d)/(n-h)
    for(i in 1:k){
      ans.M[[i]] <- ans.M[[i]] + tensor(Omega,Omega,c(1:(2*(d-1)))[-i],c(1:(2*(d-1)))[-i])
    }
  }
  for(i in 1:k){
    ans.eig <- eigen(ans.M[[i]])
    ans.Q <- c(ans.Q,list(ans.eig$vectors[,1:r[i],drop=FALSE]))
    ans.lambda <- c(ans.lambda,list(ans.eig$values))
  }
  norm.percent <- NULL
  x.hat <- NULL
  if(norm.true==TRUE){
    x.tnsr <- as.tensor(x)
    #x.hat <- get.hat(x.tnsr,ans.Q,1:(d-1))
    x.hat <- ttl(x.tnsr,lapply(ans.Q,t),1:(d-1))
    x.hat <- ttl(x.hat,ans.Q,1:(d-1))
    
    norm.percent <- fnorm(x.tnsr-x.hat)/fnorm(x.tnsr)
    x.hat <- x.hat@data
  }
  list("M"=ans.M,"Q"=ans.Q,"lambda"=ans.lambda,"norm.percent"=norm.percent,"x.hat"=x.hat)
}

