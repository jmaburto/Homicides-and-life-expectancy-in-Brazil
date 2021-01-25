
LifeExpectancy <- compiler::cmpfun(function(mx,Sex = "f", Age = seq(0,90,5)){
  AgeInt <- age2int(Age = Age,  OAvalue = 5)
  radix <- 1
  IMR  <- NA
  OAnew <- max(Age)
  axmethod <- "pas"
  nMx <- mx
  
  nAx <- lt_id_morq_a(nMx = nMx,Age = Age,axmethod = axmethod,
                   AgeInt = AgeInt, Sex = Sex,region = 'w',mod = TRUE)
  nqx <- (AgeInt*nMx)/(1 + (AgeInt - nAx)*nMx)
    
  lx <- lt_id_q_l(nqx, radix = radix)
  ndx <- lt_id_l_d(lx)
  nLx <- lt_id_lda_L(lx = lx, ndx = ndx, nax = nAx, AgeInt = AgeInt)
  Tx <- lt_id_L_T(nLx)
  ex <- Tx/lx
  ex[1]
  })
  

e0frommxc <- function(mxcvec,sex){
  dim(mxcvec) <- c(19,length(mxcvec)/19)
  mx          <- rowSums(mxcvec)
  LifeExpectancy(mx,sex)
}


COD_Decomp_fun <- function(.SD ){
  cause_labels <- unique(.SD$cause)
  age.group    <- sort(unique(.SD$age.group))
  years        <- sort(unique(.SD$year))
  sex          <- .SD$sex2[1]
  
  
  decomp <- mclapply(years[-1],function(x,m = m, sex = sex, cause_labels = cause_labels, age.group = age.group){
    mx2  <- m[m$year == x,]$mx
    mx1  <- m[m$year == x-1,]$mx
    hor  <- horiuchi(func = e0frommxc,pars1 = mx1,pars2 = mx2,N = 50,sex = ifelse(sex == 'female','f','m'))
    dim(hor)  <- c(19,10)
    colnames(hor) <- cause_labels
    rownames(hor) <- age.group
    hor <- melt(hor)
    names(hor) <- c('age.group','cause','contribution')
    hor$year <- as.numeric(x)
    hor
  }, m = .SD, sex = sex,cause_labels=cause_labels,age.group = age.group,mc.cores = 1)
  
  decomp.results <- data.table(do.call(rbind,decomp))
  
  decomp.results
  
}
