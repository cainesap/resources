# CREATED BY NATALYA PYA, DEPARTMENT OF STATISTICS, UNIVERSITY OF BATH
# VERSION 7: November 19, 2014
# Changed by Martijn Wieling, mgcv::: rwMatrix instead of rwMatrix

## routines for very large dataset autoregressive t additive modelling...


# no need for special functions of Sl.fit and fast.REML.fit, as bam.fit.art uses the same criterion fREML...
## however, there ARE CHANGES IN bam.art.fit() ...


## scaled t family with residual autocorrelation for large datasets...
## corrected residuals() in art() to take into account 'AR.start' variable...


art <- function(theta = NULL, rho=0, link = "identity") { 
## Extended family object for scaled t distribution with AR1
## to be used with bam(); the family components assume that the data are transformed: response var-s are independent t  
  linktemp <- substitute(link)
  if (!is.character(linktemp)) linktemp <- deparse(linktemp)
  if (linktemp %in% c("identity", "log", "inverse")) stats <- make.link(linktemp)
  else if (is.character(link)) {
    stats <- make.link(link)
    linktemp <- link
  } else {
    if (inherits(link, "link-glm")) {
       stats <- link
            if (!is.null(stats$name))
                linktemp <- stats$name
        }
        else stop(linktemp, " link not available for scaled t distribution; available links are \"identity\", \"log\",  and \"inverse\"")
    }
    Theta <-  NULL;n.theta <- 2
    if (!is.null(theta)&&sum(theta==0)==0) {
      if (abs(theta[1]<2)) stop("scaled t df must be >2")
      if (sum(theta<0)) { 
        iniTheta <- c(log(abs(theta[1])-2),log(abs(theta[2]))) ## initial theta supplied
      } else { ## fixed theta supplied
        iniTheta <- Theta <- c(log(theta[1]-2),log(theta[2])) 
        n.theta <- 0 ## no thetas to estimate
      }
    } else iniTheta <- c(-2,-1) ## inital log theta value
    env <- new.env(parent = .GlobalEnv)
    assign(".Theta", iniTheta, envir = env)
    getTheta <- function(trans=FALSE) { 
    ## trans transforms to the original scale...
      th <- get(".Theta")
      if (trans) { th <- exp(th); th[1] <- th[1] + 2  }
      th
    }
    putTheta <- function(theta) assign(".Theta", theta,envir=environment(sys.function()))
    
    Rho <- 0
    if (!is.null(rho)&& length(rho)==1) {
       if (rho <0) Rho <- 0
       else     Rho <- rho
    } else Rho <- 0  
    if (Rho!=0 && (linktemp!="identity")) stop(linktemp, " link not available for scaled t with residual autocorrelation; the only available link is \"identity\"")
    assign(".Rho", Rho, envir = env)
    getRho <- function() { 
      rho <- get(".Rho")
      rho
    }
       
    
    variance <- function() { 
        rho <- get(".Rho")
        th <- get(".Theta")
        nu <- exp(th[1])+2; sig <- exp(th[2])
        sig^2*nu/(nu-2)/(1-rho^2)
    }

   validmu <- function(mu) all(is.finite(mu))

   dev.resids <- function(y, mu, wt,theta=NULL) {  
      if (is.null(theta)) theta <- get(".Theta")
      rho <- get(".Rho")
      nu <- exp(theta[1])+2; sig <- exp(theta[2])
      wt * (nu + 1)*log(1+(1-rho^2)*(y-mu)^2/sig^2/nu)
    }
    
    Dd <- function(y, mu, theta, wt, level=0) {  ## actually not all of the derivatives required
    ## derivatives of the deviance...
     # ltheta <- theta
      rho <- get(".Rho")
      nu <- exp(theta[1])+2; sig2 <- exp(theta[2])^2
      rho1 <- 1-rho^2
      nu1 <- nu + 1;  ymu <- y - mu; nu2 <- nu - 2; 
      nusig2 <- nu*sig2
      a <- nusig2 + rho1*ymu^2
      oo <- list()
      ## get the quantities needed for IRLS. 
      ## Dmu2eta2 is deriv of D w.r.t mu twice and eta twice,
      ## Dmu is deriv w.r.t. mu once, etc...
      f0 <- ymu*rho1
      f2 <- f0*ymu/a  ## rho1*ymu^2/a
      oo$Dmu <- -2 * wt * nu1*f0/a
      oo$Dmu2 <- 2*wt *nu1*rho1*(1-2*f2)/a 
      term <- 2*nu1*rho1^.5/sig2/(nu+3)
      n <- length(y) 
      oo$EDmu2 <- rep(term,n)
      if (level>0) { ## quantities needed for first derivatives
        f1 <- f0*ymu/nu  ## rho1*ymu^2/nu
        f3 <- nu1*sig2/a; f4 <- nusig2/a
        f5 <- nu2*rho1/a
        nu21 <- nu2/nu1
        oo$Dmu2th <- oo$Dmuth <- oo$Dth <- matrix(0,n,2)
        oo$Dth[,1] <- wt * nu2 * (log(a) - log(nusig2) - nu1*f2/nu) 
        oo$Dth[,2] <- -2 * wt * nu1*f2   
        oo$Dmuth[,1] <-  wt *nu21*oo$Dmu*(1 -f3)
        oo$Dmuth[,2] <- -2* wt* oo$Dmu*f4
        oo$Dmu3 <- wt * 4*nu1*f0*rho1*(3-4*f2)/a^2
        oo$Dmu2th[,1] <-  wt * (2*f5*(1-f3) - oo$Dmu*(2*oo$Dmuth[,1]-oo$Dmu*nu21)/nu1)
        oo$Dmu2th[,2] <- -2*wt* (2*f3*nu*rho1/a +oo$Dmu*oo$Dmuth[,2]/nu1)
      } 
      if (level>1) { ## whole lot
        oo$Dmu4 <- -12*wt *nu1*rho1^2 *(1-8*f2*(1-f2))/a^2
        n2d <- 3 # number of the 2nd order derivatives
        oo$Dmu3th <- matrix(0,n,2)
        oo$Dmu2th2 <- oo$Dmuth2 <- oo$Dth2 <- matrix(0,n,n2d)
        oo$Dmu3th[,1] <- wt*4*f5*f0*(3 - 4*f2 + 6*f3*(-1 + 2*f2))/a
        oo$Dmu3th[,2] <- 48*wt* f0*f4*rho1*nu1*(-1 +2*f2)/a^2
        oo$Dth2[,1] <- wt *(oo$Dth[,1] + nu2^2*(-2*f1*(1-f3) + nu1*f1^2/a)/a) ## deriv of D w.r.t. theta1 theta1 
        oo$Dth2[,2] <- 2*wt*nu2*f2*(sig2 -f0*ymu)/a  ## deriv of D wrt theta1 theta2
        oo$Dth2[,3] <- - wt *oo$Dth[,2]* (2 +oo$Dth[,2]/nu1)  ## deriv of D wrt theta2 theta2

        oo$Dmuth2[,1] <- wt*(nu21*oo$Dmu + 2*f0*sig2*nu2*(3*(nu-1) - 2*f3*nu2)/a^2)

        term <- 4*f5*sig2* (2*nu +1 -2*f4*nu1)/a
        oo$Dmuth2[,2] <- wt*term*ymu

        oo$Dmuth2[,3] <- -8*wt* f0*f3*nu*(2*f4- 1)/a

        oo$Dmu2th2[,1] <- wt*(2*f5*(1-sig2*(3*nu-3-2*f3*nu2)/a ) 
                  - 2*(oo$Dmuth[,1]^2 +oo$Dmu*oo$Dmuth2[,1])/nu1 +
                  nu21*oo$Dmu*(4*oo$Dmuth[,1]+oo$Dmu)/nu1 - 2*oo$Dmu^2*nu21^2/nu1)
       
        oo$Dmu2th2[,2] <- wt*(-term 
                  -2*(oo$Dmuth[,2]*oo$Dmuth[,1]+oo$Dmu*oo$Dmuth2[,2])/nu1 +
                  2*nu21*oo$Dmu*oo$Dmuth[,2]/nu1)
        oo$Dmu2th2[,3] <- -2*wt * (-4*f4*nu1*rho1*(2*f4-1)/a + (oo$Dmu*oo$Dmuth2[,3] + oo$Dmuth[,2]^2)/nu1)
      }
      oo
    }  ## end of Dd

    aic <- function(y, mu, theta=NULL, wt,dev) {
        if (is.null(theta)) theta <- get(".Theta")
        rho <- get(".Rho")
        nu <- exp(theta[1])+2; sig <- exp(theta[2])
        term <- -lgamma((nu+1)/2)+ lgamma(nu/2) + log(sig*(pi*nu)^.5) +
           (nu+1)*log(1 + (1-rho^2)*(y-mu)^2/sig^2/nu)/2-log(1-rho^2)/2  ## `-'log likelihood for each observation
        2 * sum(term * wt)
    }
    
    ls <- function(y,w,n,theta,scale) {
       ## the log saturated likelihood function.
     #  Theta <- exp(theta);  nu <- Theta[1]; sig <- Theta[2]
       rho <- get(".Rho")
       nu <- exp(theta[1])+2; sig <- exp(theta[2]); nu2 <- nu-2;
       nu2nu <- nu2/nu; nu12 <- (nu+1)/2
       term <- lgamma(nu12) - lgamma(nu/2) - log(sig*(pi*nu)^.5) + log(1-rho^2)/2 
       ls <- sum(term*w) 
       ## first derivative wrt theta...
       lsth <- rep(0,2) 
       lsth2 <- matrix(0,2,2)  ## rep(0, 3)
       term <- nu2 * digamma(nu12)/2- nu2 * digamma(nu/2)/2 - 0.5*nu2nu
       lsth[1] <- sum(w*term)
       lsth[2] <- sum(-1*w)
       
       ## second deriv...      
       term <-  nu2^2 * trigamma(nu12)/4 + nu2 * digamma(nu12)/2 -
           nu2^2 * trigamma(nu/2)/4 - nu2 * digamma(nu/2)/2 + 0.5*(nu2nu)^2 - 0.5*nu2nu
       lsth2[1,1] <- sum(term*w)
       lsth2[1,2] <- lsth2[2,1] <- lsth2[2,2] <- 0
       list(ls=ls,## saturated log likelihood
            lsth1=lsth, ## first derivative vector wrt theta
            lsth2=lsth2) ## Hessian wrt theta
    }

    preinitialize <- expression({
      ## initialize theta from raw observations..
       if (G$family$n.theta>0) {
         Theta <- c(-1, log(0.2*var(G$y)^.5))
         G$family$putTheta(Theta)
       } ## otherwise fixed theta supplied
    })

    initialize <- expression({
        if (any(is.na(y))) stop("NA values not allowed for the scaled t family")
        n <- rep(1, nobs)
        mustart <- y + (y == 0)*.1 
    })

    postproc <- expression({
      object$family$family <- 
      paste("Scaled t(",paste(round(object$family$getTheta(TRUE),3),collapse=","),"), rho=",
           paste(object$family$getRho()),sep="")  
    })
  
    rd <- function(mu,wt,scale) {
     ## simulate data given fitted latent variable in mu 
      theta <- get(".Theta")
      rho <- get(".Rho")
      nu <- exp(theta[1])+2; sig <- exp(theta[2])
      n <- length(mu)
      stats::rt(n=n,df=nu)*sig/(1-rho^2)^.5 + mu
    }
    
    residuals <- function(object,type=c("deviance","working","response","pearson")) {
      if (type == "working") { 
        res <- object$residuals 
      } else if (type == "response") {
        res <- object$y - object$fitted.values
      } else if (type == "deviance") { 
          y <- object$y
          mu <- object$fitted.values
          wts <- object$prior.weights
          rho <- object$family$getRho()
          if (rho!=0){
            ld <- 1/sqrt(1-rho^2) ## leading diagonal of root inverse correlation
            sd <- -rho*ld         ## sub diagonal
            n <- length(y)
            row <- c(1,rep(1:n,rep(2,n))[-c(1,2*n)])
            weight <- c(1,rep(c(sd,ld),n-1))
            stop <- c(1,1:(n-1)*2+1) 
            if (!is.null(object$AR.start)) { ## need to correct the start of new AR sections...
                 ii <- which(object$AR.start==TRUE)
                 if (length(ii)>0) {
                    if (ii[1]==1) ii <- ii[-1] ## first observation does not need any correction
                    weight[ii*2-2] <- 0 ## zero sub diagonal
                    weight[ii*2-1] <- 1 ## set leading diagonal to 1
                 }
              }
            ## apply transform...
            mu <- mgcv::: rwMatrix(stop,row,weight,mu)  
            y <- mgcv::: rwMatrix(stop,row,weight,y)  
          } 
          res <- object$family$dev.resids(y,mu,wts)
          s <- sign(y-mu)
          res <- as.numeric(sqrt(res) * s)   
      } else if (type == "pearson") {
           mu <- object$fitted.values
           res <- as.numeric((object$y - mu)/object$family$variance()^.5)
       }
      res
    } ## residuals
    environment(dev.resids) <-  environment(aic) <- environment(getTheta)  <- environment(rd)<-  environment(variance) <- environment(putTheta) <- environment(Dd) <- environment(ls) <-  environment(getRho) <-  env 

    structure(list(family ="scaled t", link = linktemp, linkfun = stats$linkfun,
        linkinv = stats$linkinv, dev.resids = dev.resids,Dd=Dd,postproc=postproc,  variance=variance,
        aic = aic, mu.eta = stats$mu.eta, initialize = initialize,ls=ls, preinitialize=preinitialize,
        validmu = validmu, valideta = stats$valideta,n.theta=n.theta, residuals=residuals,
        ini.theta = iniTheta,putTheta=putTheta,getTheta=getTheta, rd=rd, getRho=getRho),
        class = c("extended.family","family"))
} ## art




efam.ar.qr.up <- function(arg) {  ## this function still needs to be checked...
## routine for parallel computation of the QR factorization with AR residuals of 
## a large egam model matrix, suitable for calling with parLapply.
  if (arg$rho!=0) { ## AR1 error model
     ld <- 1/sqrt(1 - arg$rho^2) ## leading diagonal of root inverse correlation
     sd <- -arg$rho * ld         ## sub diagonal
  } 
  yX.last <- NULL
  wt <- rep(0,0) 
  dev <- 0    
  for (b in 1:arg$n.block) {
    ind <- arg$start[b]:arg$stop[b]
    if (arg$rho!=0) { ## have to find AR1 transform...
       N <- arg$stop[b]-arg$start[b]+1
       ## note first row implied by this transform
       ## is always dropped, unless really at beginning of data.
       row <- c(1,rep(1:N,rep(2,N))[-c(1,2*N)])
       weight <- c(1,rep(c(sd,ld),N-1))
       end <- c(1,1:(N-1)*2+1)
       if (!is.null(arg$mf$"(AR.start)")) { ## need to correct the start of new AR sections...
           ii <- which(arg$mf$"(AR.start)"[ind]==TRUE)
           if (length(ii)>0) {
             if (ii[1]==1) ii <- ii[-1] ## first observation does not need any correction
             weight[ii*2-2] <- 0 ## zero sub diagonal
             weight[ii*2-1] <- 1 ## set leading diagonal to 1
           }
       }
    } 
    X <- predict(arg$G,newdata=arg$mf[ind,],type="lpmatrix",newdata.guaranteed=TRUE,block.size=length(ind))
    rownames(X) <- NULL
    y <- arg$G$y[ind] ## arg$G$model[[arg$response]] 
    if (arg$rho!=0) {
       ## Apply transform...
       if (arg$last&&arg$end[b]==arg$nobs) yX.last <- 
           c(y[nrow(X)],X[nrow(X),]) ## store final row, in case of update
       if (arg$first&&b==1) {
          X <- mgcv::: rwMatrix(end,row,weight,X)
          y <- mgcv::: rwMatrix(end,row,weight,y)
       } else {
          X <- mgcv::: rwMatrix(end,row,weight,X)[-1,]
          y <- mgcv::: rwMatrix(end,row,weight,y)[-1]
       } 
    } ## dealt with AR1  
    if (is.null(arg$coef)) eta1 <- arg$eta[ind] else eta1 <- drop(X%*%arg$coef) + arg$offset[ind]
    mu <- arg$linkinv(eta1) 
    weights <- arg$G$w[ind]
    mu.eta.val <- arg$mu.eta(eta1)
    dd <- mgcv::: dDeta(y,mu,weights,theta,family,0) ## derivatives of deviance w.r.t. eta
    good <- is.finite(dd$Deta.Deta2)
           ##if (control$trace&sum(!good)>0) cat("\n",sum(!good)," not good\n")  ## this is from gam.fit4 control
    w <- dd$EDeta2 * .5;
    if (sum(!good)) {
         good1 <- is.finite(w)&good ## make sure w finite too
         w[!is.finite(w)] <- 0      ## clear infinite w
         w[!good1&w==0] <- max(w)*.Machine$double.eps^.5 ## reset zero value weights for problem elements
         dd$Deta.Deta2[!good] <- .5*dd$Deta[!good]/w[!good] ## reset problem elements to finite
         good <- is.finite(dd$Deta.Deta2) ## check in case Deta not finite, for example
    }
    z <- (eta1-offset[ind])[good]  - .5 * dd$Deta[good] / w  ## - dd$Deta.Deta2[good]
    dev <- dev + sum(arg$dev.resids(y,mu,weights,theta))
    wt <- c(wt,w)
    w <- sqrt(w)
    if (b == 1) qrx <- qr.update(w*X[good,],w*z,use.chol=arg$use.chol) 
    else qrx <- qr.update(w*X[good,],w*z,qrx$R,qrx$f,qrx$y.norm2,use.chol=arg$use.chol)
    rm(X);if(arg$gc.level>1) gc() ## X can be large: remove and reclaim
  }
  qrx$dev <- dev;qrx$wt <- wt
  qrx$yX.last <- yX.last
  if (arg$gc.level>1) { rm(arg,ind,mu,y,weights,mu.eta.val,good,z,w,wt,w);gc()}
  qrx
} ## end of efam.ar.qr.up





bam.art.fit <- function(G,mf,chunk.size,gp,scale,rho=0,gamma,method,coef=NULL,etastart = NULL,
                mustart = NULL, offset = rep(0, nobs), control = gam.control(), intercept = TRUE, 
                cl=NULL,gc.level=0,use.chol=FALSE,nobs.extra=0,samfrac=1) 
## function that does big additive model fit in art case
{  y <- mf[[gp$response]]
   weights <- G$w
   conv <- FALSE
   nobs <- nrow(mf)
   nvars <- ncol(G$X)
   offset <- G$offset
   family <- G$family
   G$family <- gaussian() ## needed if REML/ML used
   variance <- family$variance
   dev.resids <- family$dev.resids
   aic <- family$aic
   linkinv <- family$linkinv
   mu.eta <- family$mu.eta
   if (!is.function(variance) || !is.function(linkinv))
        stop("'family' argument seems not to be a valid family object")
   valideta <- family$valideta
   if (is.null(valideta))
        valideta <- function(eta) TRUE
   validmu <- family$validmu
   if (is.null(validmu))
        validmu <- function(mu) TRUE

   ## call the families initialization code...
   if (is.null(mustart)) {
        eval(family$initialize)
   }
   else {
        mukeep <- mustart
        eval(family$initialize)
        mustart <- mukeep
   }
 
   coefold <- NULL
   eta <- if (!is.null(etastart))  etastart
          else family$linkfun(mustart)

      
   mu <- linkinv(eta)
   if (!(validmu(mu) && valideta(eta)))
       stop("cannot find valid starting values: please specify some")
   n <- nrow(mf)
  
 #  rho <- family$getRho()
   if (rho!=0) {
        ld <- 1/sqrt(1-rho^2) ## leading diagonal of root inverse correlation
        sd <- -rho*ld         ## sub diagonal
        row <- c(1,rep(1:n,rep(2,n))[-c(1,2*n)])
        weight <- c(1,rep(c(sd,ld),n-1))
        end <- c(1,1:(n-1)*2+1) 
        if (!is.null(mf$"(AR.start)")) { ## need to correct the start of new AR sections...
                 ii <- which(mf$"(AR.start)"==TRUE)
                 if (length(ii)>0) {
                    if (ii[1]==1) ii <- ii[-1] ## first observation does not need any correction
                    weight[ii*2-2] <- 0 ## zero sub diagonal
                    weight[ii*2-1] <- 1 ## set leading diagonal to 1
                 }
        }
        ## apply transform...
        mu <- mgcv::: rwMatrix(end,row,weight,mu)  
        y <- mgcv::: rwMatrix(end,row,weight,y)    
      }   
 
   ## get theta from family object....
   theta <- family$getTheta() ## fixed value  
   n.theta <- family$n.theta 
   ## if theta is supplied, then scale parameter is fixed,
   if (n.theta==0){
           nu <- exp(theta[1])+2; sig <- exp(theta[2])
           scale <- 1 
         #  scale <- sig*sqrt(nu/(nu-2)/(1-rho^2))
   } else stop("'theta' must be specified in 'art' family")

   dev <- sum(dev.resids(y, mu, weights,theta))*2 ## just to avoid converging at iter 1
   boundary <- conv <- FALSE
   
   G$coefficients <- rep(0,ncol(G$X))
   class(G) <- "gam"  

   ## need to reset response and weights to post initialization values
   ## in particular to deal with binomial properly...
   G$y <- mf[[gp$response]]
   G$w <- weights

   ## set up cluster for parallel computation...

   if (!is.null(cl)&&inherits(cl,"cluster")) {
      require(parallel)
      n.threads <- length(cl)
   } else n.threads <- 1

   if (n.threads>1) { ## set up thread argument lists
       ## number of obs per thread
       nt <- rep(ceiling(n/n.threads),n.threads)
       nt[n.threads] <- n - sum(nt[-n.threads])
       arg <- list()
       n1 <- 0
       for (i in 1:n.threads) { 
         n0 <- n1+1;n1 <- n1+nt[i]
         if (i>1&&rho!=0) { ## need to start from end of last block if rho!=0
           n0 <- n0-1;nt[i] <- nt[i]+1 
         }   
         ind <- n0:n1 ## this thread's data block from mf
         n.block <- nt[i]%/%chunk.size ## number of full sized blocks
         stub <- nt[i]%%chunk.size ## size of end block
         if (n.block>0) { 
           ## each block is of size 
           start <- (0:(n.block-1))*chunk.size+1
           stop <- start + chunk.size - 1  ## (1:n.block)*chunk.size
           if (stub>0) {
             start[n.block+1] <- stop[n.block]+1
             stop[n.block+1] <- nt[i]
             n.block <- n.block+1
           } 
           if (rho!=0) { ## then blocks must overlap
             ns <- length(start)
             if (ns>1) start[2:ns] <- start[2:ns]-1 
           }
         } else {
           n.block <- 1
           start <- 1
           stop <- nt[i]
         }
         arg[[i]] <- list(nobs= nt[i],start=start,stop=stop,n.block=n.block,
                         rho=rho,linkinv=linkinv,dev.resids=dev.resids,response=gp$response,
                         mf = mf[ind,],gc.level=gc.level,mu.eta=mu.eta,variance=variance,
                         eta = eta[ind],offset = offset[ind],G = G,
                         first=FALSE,last=FALSE,use.chol=use.chol)
         if (i==1) arg[[1]]$first <- TRUE
         if (i==n.threads) arg[[i]]$last <- TRUE 
         arg[[i]]$G$w <- G$w[ind];arg[[i]]$G$model <- NULL
         arg[[i]]$G$y <- G$y[ind]
       }
     } else { ## single thread, requires single indices 
       ## construct indices for splitting up model matrix construction... 
       n.block <- nobs%/%chunk.size ## number of full sized blocks
       stub <- nobs%%chunk.size ## size of end block
       if (stub>0) n.block <- n.block + 1
       start <- 0:(n.block-1)*chunk.size    ## block starts
       stop <- start + chunk.size;           ## block ends
       stop[n.block] <- n
       if (rho==0) start <- start + 1  ## otherwise most blocks go to 1 before block start
       start[1] <- 1  
      # if (n.block>0) {
      #  start <- (0:(n.block-1))*chunk.size+1
      #  stop <- (1:n.block)*chunk.size
      #  if (stub>0) {
      #    start[n.block+1] <- stop[n.block]+1
      #    stop[n.block+1] <- nobs
      #    n.block <- n.block+1
      #  } 
      # } else {
      #   n.block <- 1
      #   start <- 1
      #   stop <- nobs
      # }
      # if (rho==0) start <- start + 1  ## otherwise most blocks go to 1 before block start      
     } ## single thread indices complete
 
    conv <- FALSE
    ##-------------
    if (method=="fREML") Sl <- mgcv::: Sl.setup(G) ## setup block diagonal penalty object
    else stop(method, " not available for scaled t with residual autocorrelation; currently the only available method is \"fREML\"")
   
    for (iter in 1L:control$maxit) { ## main fitting loop......
       ## accumulate the QR decomposition of the weighted model matrix
       wt <- rep(0,0) 
       devold <- dev
       dev <- 0
       if (n.threads == 1) { ## use original serial update code     
         for (b in 1:n.block) {
           ind <- start[b]:stop[b]
           if (rho!=0) {
              N <- stop[b]-start[b]+1
              row <- c(1,rep(1:N,rep(2,N))[-c(1,2*N)])
              weight <- c(1,rep(c(sd,ld),N-1))
              end <- c(1,1:(N-1)*2+1) 
              if (!is.null(mf$"(AR.start)")) { ## need to correct the start of new AR sections...
                 ii <- which(mf$"(AR.start)"[ind]==TRUE)
                 if (length(ii)>0) {
                    if (ii[1]==1) ii <- ii[-1] ## first observation does not need any correction
                    weight[ii*2-2] <- 0 ## zero sub diagonal
                    weight[ii*2-1] <- 1 ## set leading diagonal to 1
                 }
              }
           } 
           X <- mgcv::: predict.gam(G,newdata=mf[ind,],type="lpmatrix",newdata.guaranteed=TRUE,block.size=length(ind))
           rownames(X) <- NULL
           y <- G$y[ind] ## G$model[[gp$response]] ## - G$offset[ind]
           if (rho!=0) {
              ## Apply transform...
              if (stop[b]==n) yX.last <- c(y[nrow(X)],X[nrow(X),]) ## store final row, in case of update
              if (b==1) {
                X <- mgcv::: rwMatrix(end,row,weight,X)
                y <- mgcv::: rwMatrix(end,row,weight,y)
              } else {
                 X <- mgcv::: rwMatrix(end,row,weight,X)[-1,]
                 y <- mgcv::: rwMatrix(end,row,weight,y)[-1]
                 ind <- ind[-1]
               } 
           }     
           if (is.null(coef)) eta1 <- eta[ind] else eta1 <- drop(X%*%coef) + offset[ind]
           mu <- linkinv(eta1) 
           weights <- G$w[ind]
           dd <- mgcv::: dDeta(y,mu,weights,theta,family,0) ## derivatives of deviance w.r.t. eta
           good <- is.finite(dd$Deta.Deta2)
           ##if (control$trace&sum(!good)>0) cat("\n",sum(!good)," not good\n")  ## this is from gam.fit4 control
           w <- dd$EDeta2 *.5;
           if (sum(!good)) {
             good1 <- is.finite(w)&good ## make sure w finite too
             w[!is.finite(w)] <- 0      ## clear infinite w
             w[!good1&w==0] <- max(w)*.Machine$double.eps^.5 ## reset zero value weights for problem elements
             dd$Deta.Deta2[!good] <- .5*dd$Deta[!good]/w[!good] ## reset problem elements to finite
             good <- is.finite(dd$Deta.Deta2) ## check in case Deta not finite, for example
           }
           z <- (eta1-offset[ind])[good]  - .5 * dd$Deta[good] / w  ## - dd$Deta.Deta2[good]
      
           dev <- dev + sum(dev.resids(y,mu,weights,theta))
           wt <- c(wt,w)
           w <- sqrt(w)
           if (b == 1) qrx <- mgcv::: qr.update(w*X[good,],w*z,use.chol=use.chol) 
           else qrx <- mgcv::: qr.update(w*X[good,],w*z,qrx$R,qrx$f,qrx$y.norm2,use.chol=use.chol)
           rm(X);if(gc.level>1) gc() ## X can be large: remove and reclaim
        } ## end of single thread block loop 
        if (use.chol) { ## post proc to get R and f...
          y.norm2 <- qrx$y.norm2 
          qrx <- chol2qr(qrx$R,qrx$f)
          qrx$y.norm2 <- y.norm2
        }
      } else { ## use parallel accumulation 
        
        for (i in 1:length(arg)) arg[[i]]$coef <- coef
        res <- parLapply(cl,arg,efam.ar.qr.up) 
        ## Single thread de-bugging...
        # res <- list()
        # for (i in 1:length(arg)) {
        #   res[[i]] <- ar.qr.up(arg[[i]])
        # }

        ## now consolidate the results from the parallel threads...
        R <- res[[1]]$R;f <- res[[1]]$f;dev <- res[[1]]$dev
        y.norm2 <- res[[1]]$y.norm2
        for (i in 2:n.threads) {
          if (use.chol) {
             R <- R + res[[i]]$R; f <- f + res[[i]]$f
          } else {
             R <- rbind(R,res[[i]]$R); f <- c(f,res[[i]]$f)
          }
          y.norm2 <- y.norm2 + res[[i]]$y.norm2
        } 
        if (use.chol) {
          qrx <- chol2qr(R,f)
          qrx$y.norm2 <- y.norm2
        } else { ## proper QR        
          qrx <- qr(R,tol=0,LAPACK=TRUE) 
          f <- qr.qty(qrx,f)[1:ncol(R)]
          rp <- qrx$pivot;rp[rp] <- 1:ncol(R) # reverse pivot
          qrx <- list(R=qr.R(qrx)[,rp],f=f,y.norm2=y.norm2)
        }
        yX.last <- res[[n.threads]]$yX.last
      } ## end parallel accumulation
      G$n <- n
      G$y <- mf[[gp$response]]   
    


      ## if the routine has been called with only a random sample of the data, then 
      ## R, f and ||y||^2 can be corrected to estimate the full versions...
 
      qrx$R <- qrx$R/sqrt(samfrac)
      qrx$f <- qrx$f/sqrt(samfrac)
      qrx$y.norm2 <- qrx$y.norm2/samfrac
      
      rss.extra <- qrx$y.norm2 - sum(qrx$f^2)
      
      if (control$trace)
         gettextf("Deviance = %s Iterations - %d", dev, iter, domain = "R-mgcv")

      if (!is.finite(dev)) stop("Non-finite deviance")

      ## preparation for working model fit is ready, but need to test for convergence first
      if (iter>2 && abs(dev - devold)/(0.1 + abs(dev)) < control$epsilon) {
          conv <- TRUE
          coef <- start
          break
      }

      if (method=="GCV.Cp") {   ## NOT DONE....
       ##------------------------------       
       #  fit <- magic(qrx$f,qrx$R,G$sp,G$S,G$off,L=G$L,lsp0=G$lsp0,rank=G$rank,
       #               H=G$H,C=matrix(0,0,ncol(qrx$R)),     ##C=G$C,
       #               gamma=gamma,scale=scale,gcv=(scale<=0),
       #               extra.rss=rss.extra,n.score=nobs+nobs.extra)
 
       #  post <- magic.post.proc(qrx$R,fit,qrx$f*0+1) 
      } else if (method=="fREML") { ## use fast REML code   
        ## block diagonal penalty object, Sl, set up before loop
        um <- mgcv::: Sl.Xprep(Sl,qrx$R)
        lambda.0 <- mgcv::: initial.sp(qrx$R,G$S,G$off)
        lsp0 <- log(lambda.0) ## initial s.p.
        ## carry forward scale estimate if possible...
        ## -------------------------------
        ## theta is supplied, but scale parameter is estimated...
        
        if (scale>0) log.phi <- log(scale) else {
          if (iter>1) log.phi <- log(object$scale) else {  ## initial phi guess
             if (is.null(coef)||qrx$y.norm2==0) log.phi <- log(var(as.numeric(G$y))*.05) else
               log.phi <- log(qrx$y.norm2/(nobs+nobs.extra))
             # nu <- exp(theta[1])+2; 
             # log.phi <- -log(nu+1) -log(1-rho^2)/2 +2*theta[2] +log(nu+3)
          }
        }
        fit <- mgcv::: fast.REML.fit(um$Sl,um$X,qrx$f,rho=lsp0,L=G$L,rho.0=G$lsp0,
                             log.phi=log.phi,phi.fixed=scale>0,rss.extra=rss.extra,
                             nobs =nobs+nobs.extra,Mp=um$Mp)
        res <- mgcv::: Sl.postproc(Sl,fit,um$undrop,qrx$R,cov=FALSE)
        object <- list(coefficients=res$beta,db.drho=fit$d1b,
                       gcv.ubre=fit$reml,mgcv.conv=list(iter=fit$iter,
                       message=fit$conv),rank=ncol(um$X),
                       Ve=NULL,scale.estimated = scale<=0,outer.info=fit$outer.info,
                        optimizer=c("perf","newton"))
 
        if (scale<=0) { ## get sp's and scale estimate
          nsp <- length(fit$rho)
        #  theta <- object$theta <- fit$rho[(nsp-1):nsp]
         # nu <- exp(object$theta[1])+2; sig <- exp(object$theta[2])
         # object$sig2 <- object$scale <- sig^2*(nu+3)/(nu+1)/(1-rho^2)^.5
          object$sig2 <- object$scale <- exp(fit$rho[nsp])
         # object$sp <- exp(fit$rho[-c(nsp-1,nsp)]) 
         # nsp <- length(fit$rho.full)
         # object$full.sp <- exp(fit$rho.full[-c(nsp-1,nsp)])
          object$sp <- exp(fit$rho[-nsp]) 
          nsp <- length(fit$rho.full)
          object$full.sp <- exp(fit$rho.full[-nsp])
        } else { ## get sp's
          object$sig2 <- object$scale <- scale  
       #   object$theta <- theta
          object$sp <- exp(fit$rho)
          object$full.sp <- exp(fit$rho.full)
        }
        class(object)<-c("gam")               
      } else { ## method is one of "ML", "P-REML" etc...
        y <- G$y; w <- G$w; n <- G$n;offset <- G$offset
        G$y <- qrx$f
        G$w <- G$y*0+1
        G$X <- qrx$R
        G$n <- length(G$y)
        G$offset <- G$y*0
        G$dev.extra <- rss.extra
        G$pearson.extra <- rss.extra
        G$n.true <- nobs+nobs.extra
        object <- mgcv::: gam(G=G,method=method,gamma=gamma,scale=scale)
        y -> G$y; w -> G$w; n -> G$n;offset -> G$offset
      }
     
      if (method=="GCV.Cp") { 
        object <- list()
        object$coefficients <- fit$b
        object$edf <- post$edf 
        object$edf1 <- post$edf1
        ##object$F <- post$F
        object$full.sp <- fit$sp.full
        object$gcv.ubre <- fit$score
        object$hat <- post$hat
        object$mgcv.conv <- fit$gcv.info 
        object$optimizer="magic"
        object$rank <- fit$gcv.info$rank
        object$Ve <- post$Ve
        object$Vp <- post$Vb
        object$sig2 <- object$scale <- fit$scale
        object$sp <- fit$sp
        names(object$sp) <- names(G$sp)
        class(object)<-c("gam")
      }

      coef <- object$coefficients
        
      if (any(!is.finite(coef))) {
          conv <- FALSE
          warning(gettextf("non-finite coefficients at iteration %d",
                  iter))
          break
      }
    } ## end fitting iteration

    if (method=="fREML") { ## do expensive cov matrix cal only at end
      res <- mgcv::: Sl.postproc(Sl,fit,um$undrop,qrx$R,cov=TRUE,scale=scale)
      object$edf <- res$edf
      object$edf1 <- res$edf1
      object$edf2 <- res$edf2
      ##object$F <- res$F
      object$hat <- res$hat
      object$Vp <- res$Vp
      object$Ve <- res$Ve
      object$Vc <- res$Vc
      object$trans.th <- c(exp(theta[1])+2,exp(theta[2]))
      if (rho!=0) { ## correct RE/ML score for AR1 transform
       df <- if (is.null(mf$"(AR.start)")) 1 else sum(mf$"(AR.start)")
       object$gcv.ubre <- object$gcv.ubre + (nobs-df)*log(1-rho^2)/2
       object$AR.start <- mf$"(AR.start)"
      }
    }      

  if (!conv)
      warning("algorithm did not converge")
   
  object$AR1.rho <- rho
   if (rho!=0) { ## need to store last model matrix row, to allow update 
     object$yX.last <- yX.last
   }
  family$putTheta(theta)
  object$R <- qrx$R    
  object$iter <- iter 
  object$wt <- wt
  object$y <- G$y
  rm(G);if (gc.level>0) gc()
  object
} # end of bam.art.fit



## art family has been added below....

bam <- function(formula,family=gaussian(),data=list(),weights=NULL,subset=NULL,na.action=na.omit,
                offset=NULL,method="fREML",control=list(),scale=0,gamma=1,knots=NULL,
                sp=NULL,min.sp=NULL,paraPen=NULL,chunk.size=10000,rho=0,AR.start=NULL,sparse=FALSE,cluster=NULL,
                gc.level=1,use.chol=FALSE,samfrac=1,...)

## Routine to fit an additive model to a large dataset. The model is stated in the formula, 
## which is then interpreted to figure out which bits relate to smooth terms and which to 
## parametric terms.
## This is a modification of `gam' designed to build the QR decompostion of the model matrix 
## up in chunks, to keep memory costs down.
## If n.threads!=1 uses parallel QR build on n.thread threads. If n.threads==0
{ control <- do.call("gam.control",control)
  if (is.character(family))
            family <- eval(parse(text = family))
  if (is.function(family))
            family <- family()
  if (is.null(family$family))
            stop("family not recognized")
  ##family = gaussian() ## no choise here
  if (family$family=="gaussian"&&family$link=="identity") am <- TRUE else am <- FALSE
  ## --------------
  ## NEW....
  if (family$family=="scaled t"&&family$link=="identity") artm <- TRUE else artm <- FALSE
  ## end NEW
  ## --------------
  if (scale==0) { if (family$family%in%c("poisson","binomial")) scale <- 1 else scale <- -1} 
  if (!method%in%c("fREML","GCV.Cp","REML",
                    "ML","P-REML","P-ML")) stop("un-supported smoothness selection method")
  if (method=="fREML"&&!is.null(min.sp)) {
    min.sp <- NULL
    warning("min.sp not supported with fast REML computation, and ignored.")
  }
  if (sparse&&method=="fREML") {
    method <- "REML"
    warning("sparse=TRUE not supported with fast REML, reset to REML.")
  }
  gp <- interpret.gam(formula) # interpret the formula 
  cl <- match.call() # call needed in gam object for update to work
  mf <- match.call(expand.dots=FALSE)
  mf$formula <- gp$fake.formula 
  mf$method <-  mf$family<-mf$control<-mf$scale<-mf$knots<-mf$sp<-mf$min.sp <- mf$gc.level <-
  mf$gamma <- mf$paraPen<- mf$chunk.size <- mf$rho <- mf$sparse <- mf$cluster <-
  mf$use.chol <- mf$samfrac <- mf$...<-NULL
  mf$drop.unused.levels<-TRUE
  mf[[1]]<-as.name("model.frame")
  pmf <- mf
 
  pmf$formula <- gp$pf
  pmf <- eval(pmf, parent.frame()) # pmf contains all data for parametric part
  pterms <- attr(pmf,"terms") ## pmf only used for this
  rm(pmf);

  if (gc.level>0) gc()

  mf <- eval(mf, parent.frame()) # the model frame now contains all the data 
  if (nrow(mf)<2) stop("Not enough (non-NA) data to do anything meaningful")
  terms <- attr(mf,"terms")
  if (gc.level>0) gc()  
  ##------------------------
  ## NEW...
  if (rho==0&&!is.null(family$getRho)) rho <- family$getRho()
  ##  end NEW
  ##-------------------------
  if (rho!=0&&!is.null(mf$"(AR.start)")) if (!is.logical(mf$"(AR.start)")) stop("AR.start must be logical")


  ## summarize the *raw* input variables
  ## note can't use get_all_vars here -- buggy with matrices
  vars <- all.vars(gp$fake.formula[-2]) ## drop response here
  inp <- parse(text = paste("list(", paste(vars, collapse = ","),")"))

  ## allow a bit of extra flexibility in what `data' is allowed to be (as model.frame actually does)
  if (!is.list(data)&&!is.data.frame(data)) data <- as.data.frame(data) 

  dl <- eval(inp, data, parent.frame())
  if (!control$keepData) { rm(data);gc()} ## save space
  names(dl) <- vars ## list of all variables needed
  var.summary <- mgcv::: variable.summary(gp$pf,dl,nrow(mf)) ## summarize the input data
  rm(dl); if (gc.level>0) gc() ## save space    

  ## need mini.mf for basis setup, then accumulate full X, y, w and offset
  mf0 <- mgcv::: mini.mf(mf,chunk.size)
    
  if (sparse) sparse.cons <- 2 else sparse.cons <- -1

  G <- mgcv::: gam.setup(gp,pterms=pterms,
                 data=mf0,knots=knots,sp=sp,min.sp=min.sp,
                 H=NULL,absorb.cons=TRUE,sparse.cons=sparse.cons,select=FALSE,
                 idLinksBases=TRUE,scale.penalty=control$scalePenalty,
                 paraPen=paraPen)

  ## no advantage to "fREML" with no free smooths...
  if (((!is.null(G$L)&&ncol(G$L) < 1)||(length(G$sp)==0))&&method=="fREML") method <- "REML"

  G$var.summary <- var.summary
  G$family <- family
  G$terms<-terms;##G$pterms<-pterms
  pvars <- all.vars(delete.response(terms))
  G$pred.formula <- if (length(pvars)>0) reformulate(pvars) else NULL

  n <- nrow(mf)
  
  if (is.null(mf$"(weights)")) G$w<-rep(1,n)
  else G$w<-mf$"(weights)"    
  
  G$offset <- model.offset(mf)  
  if (is.null(G$offset)) G$offset <- rep(0,n)

  if (ncol(G$X)>nrow(mf)) stop("Model has more coefficients than data")      ##+nrow(G$C)) stop("Model has more coefficients than data")
 
  G$cl<-cl;
  G$am <- am
  ##------------
  ## NEW.....
  G$artm <- artm
  ## end NEW
  ##------------
  G$min.edf<-G$nsdf #-dim(G$C)[1]
  if (G$m) for (i in 1:G$m) G$min.edf<-G$min.edf+G$smooth[[i]]$null.space.dim

  G$formula<-formula
  environment(G$formula)<-environment(formula)
  
  G$conv.tol<-control$mgcv.tol      # tolerence for mgcv
  G$max.half<-control$mgcv.half     # max step halving in bfgs optimization


  ## now build up proper model matrix, and deal with y, w, and offset...

  if (control$trace) cat("Setup complete. Calling fit\n")
  
  colnamesX <- colnames(G$X)  

  if (sparse) { ## Form a sparse model matrix...
    if (sum(G$X==0)/prod(dim(G$X))<.5) warning("model matrix too dense for any possible benefit from sparse")
    if (nrow(mf)<=chunk.size) G$X <- as(G$X,"dgCMatrix") else 
      G$X <- sparse.model.matrix(G,mf,chunk.size)
    if (rho!=0) warning("AR1 parameter rho unused with sparse fitting")
    object <- bgam.fit2(G, mf, chunk.size, gp ,scale ,gamma,method=method,
                       control = control,...)
  } else if (am) {
    if (nrow(mf)>chunk.size) G$X <- matrix(0,0,ncol(G$X)); if (gc.level>1) gc() 
    object <- mgcv::: bam.fit(G,mf,chunk.size,gp,scale,gamma,method,rho=rho,cl=cluster,
                      gc.level=gc.level,use.chol=use.chol)
   ##---------------
   ## NEW ....
  } else if (artm) {  
    G$X  <- matrix(0,0,ncol(G$X)); if (gc.level>1) gc()
    coef <- NULL
    if (samfrac<1 && samfrac>0) { ## sub-sample first to get close to right answer...
      ind <- sample(1:nrow(mf),ceiling(nrow(mf)*samfrac))
      if (length(ind)<2*ncol(G$X)) warning("samfrac too small - ignored") else {
        Gw <- G$w;Goffset <- G$offset
        G$w <- G$w[ind];G$offset <- G$offset[ind]
        control1 <- control
        control1$epsilon <- 1e-2
        object <- bam.art.fit(G, mf[ind,], chunk.size, gp ,scale ,gamma,method=method,rho=rho,nobs.extra=0,
                       control = control1,cl=cluster,gc.level=gc.level,use.chol=use.chol,samfrac=1,...)
        G$w <- Gw;G$offset <- Goffset
        coef <- object$coefficients
      }
    }
    ## fit full dataset
    object <- bam.art.fit(G, mf, chunk.size, gp ,scale ,gamma,method=method,rho=rho,coef=coef,
                       control = control,cl=cluster,gc.level=gc.level,use.chol=use.chol,...)
  ## end NEW
  ##-----------------
  } else {
    G$X  <- matrix(0,0,ncol(G$X)); if (gc.level>1) gc()
    if (rho!=0) warning("AR1 parameter rho unused with generalized model")
    coef <- NULL
    if (samfrac<1 && samfrac>0) { ## sub-sample first to get close to right answer...
      ind <- sample(1:nrow(mf),ceiling(nrow(mf)*samfrac))
      if (length(ind)<2*ncol(G$X)) warning("samfrac too small - ignored") else {
        Gw <- G$w;Goffset <- G$offset
        G$w <- G$w[ind];G$offset <- G$offset[ind]
        control1 <- control
        control1$epsilon <- 1e-2
        object <- bgam.fit(G, mf[ind,], chunk.size, gp ,scale ,gamma,method=method,nobs.extra=0,
                       control = control1,cl=cluster,gc.level=gc.level,use.chol=use.chol,samfrac=1,...)
        G$w <- Gw;G$offset <- Goffset
        coef <- object$coefficients
      }
    }
    ## fit full dataset
    object <- bgam.fit(G, mf, chunk.size, gp ,scale ,gamma,method=method,coef=coef,
                       control = control,cl=cluster,gc.level=gc.level,use.chol=use.chol,...)
  }

  if (gc.level>0) gc()

  if (control$trace) cat("Fit complete. Finishing gam object.\n")

  if (scale < 0) { object$scale.estimated <- TRUE;object$scale <- object$scale.est} else {
    object$scale.estimated <- FALSE; object$scale <- scale
  }

  object$assign <- G$assign # applies only to pterms  
  object$boundary <- FALSE  # always FALSE for this case
  object$call<-G$cl # needed for update() to work 
  object$cmX <- G$cmX ## column means of model matrix --- useful for CIs
 
  object$contrasts <- G$contrasts
  object$control <- control
  object$converged <- TRUE ## no iteration
  object$data <- NA ## not saving it in this case
  object$df.null <- nrow(mf)
  object$df.residual <- object$df.null - sum(object$edf) 
 
  object$family <- family
  object$formula<-G$formula 
 
  #object$linear.predictors <- NA
  if (method=="GCV.Cp") {
    if (scale<=0) object$method <- "GCV" else object$method <- "UBRE"
  } else {
    object$method <- method
  }
  object$min.edf<-G$min.edf
  object$model <- mf;rm(mf);if (gc.level>0) gc()
  object$na.action <- attr(object$model,"na.action") # how to deal with NA's
  object$nsdf <- G$nsdf
  if (G$nsdf>0) names(object$coefficients)[1:G$nsdf] <- colnamesX[1:G$nsdf]
  object$offset <- G$offset
  object$prior.weights <- G$w
  object$pterms <- G$pterms
  object$pred.formula <- G$pred.formula
  object$smooth <- G$smooth

  object$terms <- G$terms
  object$var.summary <- G$var.summary 
  if (is.null(object$wt)) object$weights <- object$prior.weights else
  object$weights <- object$wt
  object$xlevels <- G$xlevels
  #object$y <- object$model[[gp$response]]
  object$NA.action <- na.action ## version to use in bam.update
  names(object$sp) <- names(G$sp)
  if (!is.null(object$full.sp)) names(object$full.sp) <- names(G$lsp0)

  names(object$coefficients) <- G$term.names
  names(object$edf) <- G$term.names

  rm(G);if (gc.level>0) gc()
  ##---------------------
  ## NEW ....
  ## extended family may need to manipulate fit object...
  if (!is.null(family$postproc)) eval(family$postproc)
  ## end NEW
  ##----------------------

  ## note that predict.gam assumes that it must be ok not to split the 
  ## model frame, if no new data supplied, so need to supply explicitly
  class(object) <- c("bam","gam","glm","lm")
  object$linear.predictors <- as.numeric(mgcv::: predict.bam(object,newdata=object$model,block.size=chunk.size,cluster=cluster))
  object$fitted.values <- family$linkinv(object$linear.predictors)
  
  object$residuals <- sqrt(family$dev.resids(object$y,object$fitted.values,object$prior.weights)) * 
                      sign(object$y-object$fitted.values)
  object$deviance <- sum(object$residuals^2)
  object$aic <- family$aic(object$y,1,object$fitted.values,object$prior.weights,object$deviance) +
                2*sum(object$edf)
  object$null.deviance <- sum(family$dev.resids(object$y,mean(object$y),object$prior.weights))
  if (!is.null(object$full.sp)) {
    if (length(object$full.sp)==length(object$sp)&&
        all.equal(object$sp,object$full.sp)==TRUE) object$full.sp <- NULL
  }
  object
} ## end of bam



######################################################
## scat with AR1 error process to be used with gam()...

scat <- function (theta = NULL, rho=0, link = "identity", AR.start=NULL) { 
## Extended family object for scaled t distribution with AR1
## currently fixed `rho' has to be supplied: C tranform is at preinitialization step only, not updated 
  linktemp <- substitute(link)
  if (!is.character(linktemp)) linktemp <- deparse(linktemp)
  if (linktemp %in% c("identity", "log", "inverse")) stats <- make.link(linktemp)
  else if (is.character(link)) {
    stats <- make.link(link)
    linktemp <- link
  } else {
    if (inherits(link, "link-glm")) {
       stats <- link
            if (!is.null(stats$name))
                linktemp <- stats$name
        }
        else stop(linktemp, " link not available for scaled t distribution; available links are \"identity\", \"log\",  and \"inverse\"")
    }
    
   Theta <-  NULL;n.theta <- 2
    if (!is.null(theta)&&sum(theta==0)==0) {
      if (abs(theta[1]<=2)) stop("scaled t df must be >2")
      if (sum(theta<0)) { 
        iniTheta <- c(log(abs(theta[1])-2),log(abs(theta[2]))) ## initial theta supplied
      } else { ## fixed theta supplied
        iniTheta <- Theta <- c(log(theta[1]-2),log(theta[2])) 
        n.theta <- 0 ## no thetas to estimate
      }
    } else iniTheta <- c(-2,-1) ## inital log theta value

    env <- new.env(parent = .GlobalEnv)
    assign(".Theta", iniTheta, envir = env)
    getTheta <- function(trans=FALSE) { 
    ## trans transforms to the original scale...
      th <- get(".Theta")
      if (trans) { th <- exp(th); th[1] <- th[1] + 2  }
      th
    }
    putTheta <- function(theta) assign(".Theta", theta,envir=environment(sys.function()))

    Rho <- 0
    if (!is.null(rho)&& length(rho)==1) {
       if (rho <0) Rho <- 0
       else     Rho <- rho
    } else Rho <- 0  
    if (Rho!=0 && (linktemp!="identity")) stop(linktemp, " link not available for scaled t with residual autocorrelation; the only available link is \"identity\"")
    assign(".Rho", Rho, envir = env)
    getRho <- function() { 
      rho <- get(".Rho")
      rho
    }
    
    if (Rho!=0&&!is.null(AR.start)) if (!is.logical(AR.start)) stop("AR.start must be logical")
   
    variance <- function() { 
        rho <- get(".Rho")
        th <- get(".Theta")
        nu <- exp(th[1])+2; sig <- exp(th[2])
        sig^2*nu/(nu-2)/(1-rho^2)
    }

   validmu <- function(mu) all(is.finite(mu))

   dev.resids <- function(y, mu, wt,theta=NULL) {
      if (is.null(theta)) theta <- get(".Theta")
      rho <- get(".Rho")
      nu <- exp(theta[1])+2; sig <- exp(theta[2])
      wt * (nu + 1)*log(1+(1-rho^2)*(y-mu)^2/sig^2/nu)
    }
    
    Dd <- function(y, mu, theta, wt, level=0) {
    ## derivatives of the deviance...
     # ltheta <- theta
      rho <- get(".Rho")
      nu <- exp(theta[1])+2; sig2 <- exp(theta[2])^2
      rho1 <- 1-rho^2
      nu1 <- nu + 1;  ymu <- y - mu; nu2 <- nu - 2; 
      nusig2 <- nu*sig2
      a <- nusig2 + rho1*ymu^2
      oo <- list()
      ## get the quantities needed for IRLS. 
      ## Dmu2eta2 is deriv of D w.r.t mu twice and eta twice,
      ## Dmu is deriv w.r.t. mu once, etc...
      f0 <- ymu*rho1
      f2 <- f0*ymu/a  ## rho1*ymu^2/a
      oo$Dmu <- -2 * wt * nu1*f0/a
      oo$Dmu2 <- 2*wt *nu1*rho1*(1-2*f2)/a 
      term <- 2*nu1*rho1^.5/sig2/(nu+3)
      n <- length(y) 
      oo$EDmu2 <- rep(term,n)
      if (level>0) { ## quantities needed for first derivatives
        f1 <- f0*ymu/nu  ## rho1*ymu^2/nu
        f3 <- nu1*sig2/a; f4 <- nusig2/a
        f5 <- nu2*rho1/a
        nu21 <- nu2/nu1
        oo$Dmu2th <- oo$Dmuth <- oo$Dth <- matrix(0,n,2)
        oo$Dth[,1] <- wt * nu2 * (log(a) - log(nusig2) - nu1*f2/nu) 
        oo$Dth[,2] <- -2 * wt * nu1*f2   
        oo$Dmuth[,1] <-  wt *nu21*oo$Dmu*(1 -f3)
        oo$Dmuth[,2] <- -2* wt* oo$Dmu*f4
        oo$Dmu3 <- wt * 4*nu1*f0*rho1*(3-4*f2)/a^2
        oo$Dmu2th[,1] <-  wt * (2*f5*(1-f3) - oo$Dmu*(2*oo$Dmuth[,1]-oo$Dmu*nu21)/nu1)
        oo$Dmu2th[,2] <- -2*wt* (2*f3*nu*rho1/a +oo$Dmu*oo$Dmuth[,2]/nu1)
      } 
      if (level>1) { ## whole lot
        oo$Dmu4 <- -12*wt *nu1*rho1^2 *(1-8*f2*(1-f2))/a^2
        n2d <- 3 # number of the 2nd order derivatives
        oo$Dmu3th <- matrix(0,n,2)
        oo$Dmu2th2 <- oo$Dmuth2 <- oo$Dth2 <- matrix(0,n,n2d)
        oo$Dmu3th[,1] <- wt*4*f5*f0*(3 - 4*f2 + 6*f3*(-1 + 2*f2))/a
        oo$Dmu3th[,2] <- 48*wt* f0*f4*rho1*nu1*(-1 +2*f2)/a^2
        oo$Dth2[,1] <- wt *(oo$Dth[,1] + nu2^2*(-2*f1*(1-f3) + nu1*f1^2/a)/a) ## deriv of D w.r.t. theta1 theta1 
        oo$Dth2[,2] <- 2*wt*nu2*f2*(sig2 -f0*ymu)/a  ## deriv of D wrt theta1 theta2
        oo$Dth2[,3] <- - wt *oo$Dth[,2]* (2 +oo$Dth[,2]/nu1)  ## deriv of D wrt theta2 theta2

        oo$Dmuth2[,1] <- wt*(nu21*oo$Dmu + 2*f0*sig2*nu2*(3*(nu-1) - 2*f3*nu2)/a^2)

        term <- 4*f5*sig2* (2*nu +1 -2*f4*nu1)/a
        oo$Dmuth2[,2] <- wt*term*ymu

        oo$Dmuth2[,3] <- -8*wt* f0*f3*nu*(2*f4- 1)/a

        oo$Dmu2th2[,1] <- wt*(2*f5*(1-sig2*(3*nu-3-2*f3*nu2)/a ) 
                  - 2*(oo$Dmuth[,1]^2 +oo$Dmu*oo$Dmuth2[,1])/nu1 +
                  nu21*oo$Dmu*(4*oo$Dmuth[,1]+oo$Dmu)/nu1 - 2*oo$Dmu^2*nu21^2/nu1)
       
        oo$Dmu2th2[,2] <- wt*(-term 
                  -2*(oo$Dmuth[,2]*oo$Dmuth[,1]+oo$Dmu*oo$Dmuth2[,2])/nu1 +
                  2*nu21*oo$Dmu*oo$Dmuth[,2]/nu1)
        oo$Dmu2th2[,3] <- -2*wt * (-4*f4*nu1*rho1*(2*f4-1)/a + (oo$Dmu*oo$Dmuth2[,3] + oo$Dmuth[,2]^2)/nu1)
      }
      oo
    }  ## end of Dd

    aic <- function(y, mu, theta=NULL, wt, dev) {
        if (is.null(theta)) theta <- get(".Theta")
        rho <- get(".Rho")
        nu <- exp(theta[1])+2; sig <- exp(theta[2])
        term <- -lgamma((nu+1)/2)+ lgamma(nu/2) + log(sig*(pi*nu)^.5) +
           (nu+1)*log(1 + (1-rho^2)*(y-mu)^2/sig^2/nu)/2-log(1-rho^2)/2  ## `-'log likelihood for each observation
        2 * sum(term * wt)
    }
    
    ls <- function(y,w,n,theta,scale) {
       ## the log saturated likelihood function.
     #  Theta <- exp(theta);  nu <- Theta[1]; sig <- Theta[2]
       rho <- get(".Rho")
       nu <- exp(theta[1])+2; sig <- exp(theta[2]); nu2 <- nu-2;
       nu2nu <- nu2/nu; nu12 <- (nu+1)/2
       term <- lgamma(nu12) - lgamma(nu/2) - log(sig*(pi*nu)^.5) + log(1-rho^2)/2 
       ls <- sum(term*w) 
       ## first derivative wrt theta...
       lsth <- rep(0,2) 
       lsth2 <- matrix(0,2,2)  ## rep(0, 3)
       term <- nu2 * digamma(nu12)/2- nu2 * digamma(nu/2)/2 - 0.5*nu2nu
       lsth[1] <- sum(w*term)
       lsth[2] <- sum(-1*w)
       
       ## second deriv...      
       term <-  nu2^2 * trigamma(nu12)/4 + nu2 * digamma(nu12)/2 -
           nu2^2 * trigamma(nu/2)/4 - nu2 * digamma(nu/2)/2 + 0.5*(nu2nu)^2 - 0.5*nu2nu
       lsth2[1,1] <- sum(term*w)
       lsth2[1,2] <- lsth2[2,1] <- lsth2[2,2] <- 0
       list(ls=ls,## saturated log likelihood
            lsth1=lsth, ## first derivative vector wrt theta
            lsth2=lsth2) ## Hessian wrt theta
    }

    preinitialize <- expression({
      ## initialize theta from raw observations..
       if (G$family$n.theta>0) {
         Theta <- c(-1, log(0.2*var(G$y)^.5))
         G$family$putTheta(Theta)
       } ## otherwise fixed theta supplied
      ## C transformation of y and X...
      n <- length(G$y)
      rho<- G$family$getRho() # get(".Rho")
      if (rho!=0) {
        G$family$y.origin <- G$y
        G$family$X.origin <- G$X
        ld <- 1/sqrt(1-rho^2) ## leading diagonal of root inverse correlation
        sd <- -rho*ld         ## sub diagonal
        row <- c(1,rep(1:n,rep(2,n))[-c(1,2*n)])
        weight <- c(1,rep(c(sd,ld),n-1))
        stop <- c(1,1:(n-1)*2+1) 
        if (!is.null(G$family$AR.start)) { ## need to correct the start of new AR sections...
             ii <- which(G$family$AR.start==TRUE)
             if (length(ii)>0) {
               if (ii[1]==1) ii <- ii[-1] ## first observation does not need any correction
               weight[ii*2-2] <- 0 ## zero sub diagonal
               weight[ii*2-1] <- 1 ## set leading diagonal to 1
             }
        }
        ## apply transform...
        G$X <- mgcv::: rwMatrix(stop,row,weight,G$X)  
        G$y <- mgcv::: rwMatrix(stop,row,weight,G$y)   
      }   
    })

    initialize <- expression({
        if (any(is.na(y))) stop("NA values not allowed for the scaled t family")
        n <- rep(1, nobs)
        mustart <- y + (y == 0)*.1 
    })

    postproc <- expression({
      rho <- object$family$getRho()
      object$family$family <- 
      paste("Scaled t(",paste(round(object$family$getTheta(TRUE),3),collapse=","),"), rho=",
           paste(round(rho,3)),sep="")      
      if (rho!=0) { ## get y, lin predictors and fitted values on original scale
        object$y <- G$family$y <- G$family$y.origin 
        object$linear.predictors <- G$offset + (G$family$X.origin%*%object$coefficients)
        object$fitted.values <- object$family$linkinv(object$linear.predictors)
      }      
    #  object$gcv.ubre <- object$gcv.ubre +(length(G$y)-G$Mp)*log(1- object$family$getRho()^2)/2 ## correct REML score for C transform
      if (rho!=0) { ## correct RE/ML score for AR1 transform 
       df <- if (is.null(object$family$AR.start)) 1 else sum(object$family$AR.start)
       object$gcv.ubre <- object$gcv.ubre + (length(G$y)-df)*log(1- rho^2)/2
      }
    })
  
    rd <- function(mu,wt,scale) {
     ## simulate data given fitted latent variable in mu 
      theta <- get(".Theta")
      rho <- get(".Rho")
      nu <- exp(theta[1])+2; sig <- exp(theta[2])
      n <- length(mu)
      stats::: rt(n=n,df=nu)*sig/(1-rho^2)^.5 + mu
    }

    residuals <- function(object,type=c("deviance","working","response","pearson")) {
      if (type == "working") { 
        res <- object$residuals 
      } else if (type == "response") {
        res <- object$y - object$fitted.values
      } else if (type == "deviance") { 
          y <- object$y
          mu <- object$fitted.values
          wts <- object$prior.weights
          rho <- object$family$getRho()
          if (rho!=0){
            ld <- 1/sqrt(1-rho^2) ## leading diagonal of root inverse correlation
            sd <- -rho*ld         ## sub diagonal
            n <- length(y)
            row <- c(1,rep(1:n,rep(2,n))[-c(1,2*n)])
            weight <- c(1,rep(c(sd,ld),n-1))
            stop <- c(1,1:(n-1)*2+1) 
            if (!is.null(object$family$AR.start)) { ## need to correct the start of new AR sections...
               ii <- which(object$family$AR.start==TRUE)
               if (length(ii)>0) {
                 if (ii[1]==1) ii <- ii[-1] ## first observation does not need any correction
                 weight[ii*2-2] <- 0 ## zero sub diagonal
                 weight[ii*2-1] <- 1 ## set leading diagonal to 1
               }
            }
            ## apply transform...
            mu <- mgcv::: rwMatrix(stop,row,weight,mu)  
            y <- mgcv::: rwMatrix(stop,row,weight,y)  
          } 
          res <- object$family$dev.resids(y,mu,wts)
        #  if (rho!=0){
        #    theta <- object$family$getTheta(TRUE)
        #    nu <- theta[1]; sig <- theta[2]
        #    e <- (y-mu)/sig
        #    t <- e[2:length(y)] - rho*e[1:(length(y)-1)]
        #    t <- c(e[1],t)
        #    res <- wts * (nu + 1)*log(1+t^2/nu)
        #    s <- sign(t)
        #  } else res <- object$family$dev.resids(y,mu,wts)
          s <- sign(y-mu)
          res <- as.numeric(sqrt(res) * s)   
      } else if (type == "pearson") {
           mu <- object$fitted.values
           res <- as.numeric((object$y - mu)/object$family$variance()^.5)
       }
      res
     } ## residuals

    environment(dev.resids) <-  environment(aic) <- environment(getTheta)  <- environment(rd)<-  environment(variance) <- environment(putTheta) <- environment(Dd) <- environment(ls) <-  environment(getRho) <-  env  

    structure(list(family ="scaled t", link = linktemp, linkfun = stats$linkfun,
        linkinv = stats$linkinv, dev.resids = dev.resids,Dd=Dd,postproc=postproc,  variance=variance,
        aic = aic, mu.eta = stats$mu.eta, initialize = initialize,ls=ls, preinitialize=preinitialize,
        validmu = validmu, valideta = stats$valideta,n.theta=n.theta, residuals=residuals,  
        AR.start=AR.start,
        ini.theta = iniTheta,putTheta=putTheta,getTheta=getTheta, rd=rd, getRho=getRho),
        class = c("extended.family","family"))
} ## scat

