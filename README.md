---
title: "Notes on R"
author: "André Veríssimo"
output: github_document
---



## Benchmarking parallel methods with `cor`

### Functions to build dataset


```r
library(futile.logger)

genData <- function(reps = 100, sample.size = 1000) {
  genDataAux <- function(template = runif(1000), correlation = 1) {
    xdata <- array(0, length(template))
    correlated.ixs <- template < correlation
    xdata[correlated.ixs] <- template[correlated.ixs] + .0005 * runif(length(template[correlated.ixs]))
    xdata[!correlated.ixs] <- template[!correlated.ixs] + .05 * rnorm(length(template[!correlated.ixs]))
    return(xdata)
  }
  
  template <- runif(sample.size)
  dat <- sapply(seq(reps), function(e) {genDataAux(template , correlation = abs(.3 + runif(1))) })
  #
  flog.info('Size of generated data:', dim(dat), capture = T)
  return(list(dat = dat, template = template)) 
}

my.fun <- function(dat.ix) {
  cor(dat$template, dat$dat[,1], method = 'pearson')
}
```

### Setting up common benchmark parameters


```r
library(microbenchmark)

ntimes <- 10
#
dat <- genData(reps = 1, sample.size = 1000)
```

```
## INFO [2016-12-13 18:48:37] Size of generated data:
## 
## [1] 1000    1
```

```r
#
reps <- 100000;

flog.info('')
```

```
## INFO [2016-12-13 18:48:37]
```

```r
flog.info('')
```

```
## INFO [2016-12-13 18:48:37]
```

```r
flog.info('Repeating benchmarks %d times to find statistics', ntimes)
```

```
## INFO [2016-12-13 18:48:37] Repeating benchmarks 10 times to find statistics
```

```r
flog.info('  -> %d samples per variables', length(dat$template))
```

```
## INFO [2016-12-13 18:48:37]   -> 1000 samples per variables
```

```r
flog.info('  -> %d correlations being calculated', reps)
```

```
## INFO [2016-12-13 18:48:37]   -> 100000 correlations being calculated
```

```r
flog.info('')
```

```
## INFO [2016-12-13 18:48:37]
```

### Benchmark Xapply functions


```r
microbenchmark(
    lapply(1:reps, my.fun)
  , sapply(1:reps, my.fun)
  , vapply(1:reps, my.fun, array(double(0), 1))
  , times = ntimes)
```

```
## Unit: seconds
##                                         expr      min       lq     mean
##                       lapply(1:reps, my.fun) 13.33089 13.46899 13.58955
##                       sapply(1:reps, my.fun) 13.49540 13.55955 13.68354
##  vapply(1:reps, my.fun, array(double(0), 1)) 13.24117 13.39339 13.46551
##    median       uq      max neval cld
##  13.61063 13.71154 13.85029    10  ab
##  13.67085 13.80471 13.92711    10   b
##  13.46388 13.52053 13.68896    10  a
```

### Benchmark different combinations of mcapply


```r
library(parallel)
microbenchmark(
    mclapply(1:reps, my.fun, mc.cores = 16)
  , mclapply(1:reps, my.fun, mc.cores = 15)
  , mclapply(1:reps, my.fun, mc.cores = 14)
  , times = ntimes)
```

```
## Unit: seconds
##                                     expr      min       lq     mean
##  mclapply(1:reps, my.fun, mc.cores = 16) 1.610561 1.642636 1.680131
##  mclapply(1:reps, my.fun, mc.cores = 15) 1.674383 1.711419 1.752971
##  mclapply(1:reps, my.fun, mc.cores = 14) 1.776585 1.782533 1.801348
##    median       uq      max neval cld
##  1.645246 1.666160 1.968459    10  a 
##  1.725278 1.744501 2.015944    10  ab
##  1.793219 1.807371 1.849380    10   b
```

### Benchamark different combinations of foreach


```r
library(parallel)
library(foreach)
library(doMC) # parallel computing
registerDoMC(cores = 16) 
flog.info('16 cores')
```

```
## INFO [2016-12-13 18:56:19] 16 cores
```

```r
microbenchmark(
    foreach(el = 1:reps) %dopar% { my.fun(el) }
  , foreach(el = 1:reps, .inorder = F ) %dopar% { my.fun(el) }
  , foreach(el = 1:reps, .combine = 'c') %dopar% { my.fun(el) }
  , foreach(el = 1:reps, .combine = 'c', .inorder = F ) %dopar% { my.fun(el) }
  , foreach(el = 1:reps, .combine = 'cbind') %dopar% { my.fun(el) }
  , foreach(el = 1:reps, .combine = 'cbind', .inorder = F ) %dopar% { my.fun(el) }
  , times = ntimes)
```

```
## Unit: seconds
##                                                                                            expr
##                                                 foreach(el = 1:reps) %dopar% {     my.fun(el) }
##                                   foreach(el = 1:reps, .inorder = F) %dopar% {     my.fun(el) }
##                                 foreach(el = 1:reps, .combine = "c") %dopar% {     my.fun(el) }
##                   foreach(el = 1:reps, .combine = "c", .inorder = F) %dopar% {     my.fun(el) }
##                             foreach(el = 1:reps, .combine = "cbind") %dopar% {     my.fun(el) }
##  foreach(el = 1:reps, .combine = "cbind", .inorder = F) %dopar%      {         my.fun(el)     }
##       min       lq     mean   median       uq      max neval   cld
##  42.29074 42.40442 42.71077 42.64085 43.07830 43.18791    10   c  
##  42.99840 43.13113 43.24925 43.19802 43.37627 43.63018    10    d 
##  41.45168 41.48543 41.79080 41.69424 42.05392 42.29476    10 a    
##  42.19233 42.27469 42.36435 42.29764 42.34854 42.77995    10  b   
##  43.15101 43.20782 43.29498 43.27505 43.32283 43.63896    10    d 
##  43.30991 43.72202 43.75812 43.80028 43.85897 43.94144    10     e
```


### Benchmark combinations of BiocParallel  


```r
library(BiocParallel)
#
microbenchmark(
    bplapply(1:reps, my.fun, BPPARAM = MulticoreParam(16))
  , bplapply(1:reps, my.fun, BPPARAM = MulticoreParam(15))
  , bplapply(1:reps, my.fun, BPPARAM = MulticoreParam(14))
  , times = ntimes)
```

```
## Unit: seconds
##                                                    expr      min       lq
##  bplapply(1:reps, my.fun, BPPARAM = MulticoreParam(16)) 4.018822 4.042074
##  bplapply(1:reps, my.fun, BPPARAM = MulticoreParam(15)) 4.005688 4.051643
##  bplapply(1:reps, my.fun, BPPARAM = MulticoreParam(14)) 4.027397 4.051482
##      mean   median       uq      max neval cld
##  4.070524 4.073427 4.104559 4.110163    10   a
##  4.092317 4.083183 4.102033 4.287266    10   a
##  4.071622 4.063018 4.095685 4.127835    10   a
```

