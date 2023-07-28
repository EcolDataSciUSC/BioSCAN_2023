library('abind')
library('rjags')
library('R2jags')
library('runjags')
library('R2WinBUGS')
library('parallel')

## expit and logit functions
expit <- function(x) 1/(1+exp(-x))
logit <- function(x) log(x/(1-x))

## standard error function
se <- function(x) sqrt(var(x)/(length(x)-1))

## nice little pdf function
pdf.f <- function(f, file, ...) {
  cat(sprintf('Writing %s\n', file))
  pdf(file, ...)
  on.exit(dev.off())
  f()
}

## load and return loaded object
load.local <- function(file) {
 v <- load(file)
 stopifnot(length(v) == 1)
 get(v)
}

id <- function(x) unique(sort(x))

## make a colour transparent
make.transparent <- function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha = floor(255*alpha)  
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  apply(newColor, 2, .makeTransparent, alpha=alpha)
}

## make a summary for a run.jags model
make.summary <- function(jags.out) {
  ## create sims.matrix
  sims.mat <- do.call(rbind, jags.out$mcmc)
  vars <- colnames(sims.mat)
  mean <- colMeans(sims.mat)
  quantiles <- apply(sims.mat, 2, quantile, p=c(0.025,0.975))
  cbind(mean, t(quantiles))
}

## get mean and bci for parameter from jags output
get.mean.bci <- function(out, param) {
  sims.arr <-
    aperm(sapply(out$mcmc, I, simplify='array'), c(1,3,2))
  c(mean=mean(sims.arr[,,param]),
    quantile(sims.arr[,,param], p=c(0.025,0.975)))
}

## plot chains
plot.chains <- function(rr, par) {
  layout(matrix(1:2, 2, 1, byrow=TRUE))
  ## par(oma=c(3,3,0,0), mar=c(0.1, 0.1, 0.1, 0.1),
  ##     mgp=c(2,0.2,0), tcl=0, cex.axis=0.7, cex.main=0.7)
  ## chains <- cbind(as.vector(rr$mcmc[[1]][,par]),
  ##                 as.vector(rr$mcmc[[2]][,par]),
  ##                 as.vector(rr$mcmc[[3]][,par]))
  sims.arr <- aperm(sapply(rr$mcmc, I, simplify='array'), c(1,3,2))
  chains <- sims.arr[,,par]
  plot(chains[,1],col='green4', type='l',ylim=range(chains))
  lines(chains[,2],col='dodgerblue')
  lines(chains[,3],col='red')
  hist(chains, col='red', main='', xlab=par)
  abline(v=0, col='blue', lwd=2)
  cat(sprintf('Mean=%2.3f\n', mean(chains)))
  ## print BCI
  bci <- quantile(chains, p=c(0.025,0.975))
  cat(sprintf('BCI=[%2.3f,%2.3f]\n', bci[1], bci[2]))
  ## print HPD
  hpd <- HPDinterval(mcmc(as.vector(chains)))
  cat(sprintf('HPD BCI=[%2.3f,%2.3f]\n', hpd[1], hpd[2]))
  cat(sprintf('\n\n'))
}
