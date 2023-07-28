
get.summ <- function(pars, res.summary) {
  summ <- round(cbind(
    res.summary$summary$statistics[pars,'Mean',drop=FALSE],
    res.summary$summary$quantiles[pars,c('2.5%', '97.5%'),drop=FALSE],
    Rhat=res.summary$psrf$psrf[pars,1]
  ), digits=3)
  colnames(summ)[1] <- 'mean'
  summ
}

## mean effect function

get.y.val.main <- function(sims.mat, tt, pp,  vv, ii) {
  chains <- expit(sims.mat[,'mu.psi.0'] +
                    sims.mat[,'mu.psi.GDD3']    * tt +
                    sims.mat[,'mu.psi.RH']   * pp +
                    sims.mat[,'mu.psi.value']   * vv +
                    sims.mat[,'mu.psi.imperv']   * ii
  )
  data.table(data.frame(mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
}

get.y.val.main.all <- function(sims.mat, ev, my.data){
  
  if(ev == "GDD3"){
    t <- seq(from=min(my.data[[1]]$GDD_Prev_3Month),  
              to=  max(my.data[[1]]$GDD_Prev_3Month),
              length.out=1000)
    p <- mean(my.data[[1]]$RH)
    v <- mean(my.data[[1]]$landvalue)
    i <- mean(my.data[[1]]$imperv)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(t), function(x) get.y.val.main(sims.mat, tt = t[x], pp = p, vv = v, ii=i)))
    
    
  }else if(ev == 'RH'){
    
    t <- mean(my.data[[1]]$GDD_Prev_3Month)
    p <- seq(from=min(my.data[[1]]$RH),  
             to=  max(my.data[[1]]$RH),
             length.out=1000)
    v <- mean(my.data[[1]]$landvalue)
    i <- mean(my.data[[1]]$imperv)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(p), function(x) get.y.val.main(sims.mat, tt = t, pp = p[x],  vv = v, ii=i)))
    
  }else if(ev == 'landvalue'){
    
    t <- mean(my.data[[1]]$GDD_Prev_3Month)
    p <- mean(my.data[[1]]$RH)
    v <- seq(from=min(my.data[[1]]$landvalue, na.rm=TRUE),  
              to=  max(my.data[[1]]$landvalue, na.rm=TRUE),
              length.out=1000)
    i <- mean(my.data[[1]]$imperv)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(v), function(x) get.y.val.main(sims.mat, tt = t, pp = p,  vv = v[x], ii=i)))
    
  }else if(ev == 'imperv'){
    
    t <- mean(my.data[[1]]$GDD_Prev_3Month)
    p <- mean(my.data[[1]]$RH)
    v <- mean(my.data[[1]]$landvalue)
    i <- seq(from=min(my.data[[1]]$imperv, na.rm=TRUE),  
             to=  max(my.data[[1]]$imperv, na.rm=TRUE),
             length.out=1000)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(i), function(x) get.y.val.main(sims.mat, tt = t, pp = p,  vv = v, ii=i[x])))
    
  }
  
  return(bind_cols(one_sp_ev_occ, tt = t, pp = p, vv = v, ii=i))
}

### species specific function 

get.y.val <- function(sims.mat, ss, tt, pp, vv, ii) {
  
  chains <- expit(sims.mat[,'mu.psi.0'] +
                    sims.mat[,sprintf('psi.sp[%d]', ss)]       +
                    sims.mat[,sprintf('psi.GDD3[%s]', ss)]    * tt +
                    sims.mat[,sprintf('psi.RH[%s]', ss)]    * pp  +
                    sims.mat[,sprintf('psi.value[%s]', ss)]     * vv +
                    sims.mat[,sprintf('psi.imperv[%s]', ss)]     * ii
  )
  data.table(data.frame(ss, tt, pp, vv, ii,  mean=mean(chains), t(quantile(chains, probs=c(0.025,0.975)))))
}




## calculating occupancy for all species for a certain environment ###

get.y.val.all <- function(sims.mat, ev, ss, my.data){
  
  if(ev == "GDD3"){
    
    t <- seq(from=min(my.data[[1]]$GDD_Prev_3Month),  
              to=  max(my.data[[1]]$GDD_Prev_3Month),
              length.out=1000)
    p <- mean(my.data[[1]]$RH)
    v <- mean(my.data[[1]]$landvalue)
    i <- mean(my.data[[1]]$imperv)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(t), function(x) get.y.val(sims.mat, ss = ss, tt = t[x],pp = p, vv = v, ii=i)))
    
    
  }else if(ev == 'RH'){
    
    t <- mean(my.data[[1]]$GDD_Prev_3Month)
    p <- seq(from=min(my.data[[1]]$RH),  
             to=  max(my.data[[1]]$RH),
             length.out=1000)
    v <- mean(my.data[[1]]$landvalue)
    i <- mean(my.data[[1]]$imperv)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(p), function(x) get.y.val(sims.mat, ss = ss, tt = t, pp = p[x],  vv = v, ii=i)))
    
  }else if(ev == 'landvalue'){
    
    t <- mean(my.data[[1]]$GDD_Prev_3Month)
    p <- mean(my.data[[1]]$RH)
    v <- seq(from=min(my.data[[1]]$landvalue),  
              to=  max(my.data[[1]]$landvalue),
              length.out=1000)
    i <- mean(my.data[[1]]$imperv)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(v), function(x) get.y.val(sims.mat, ss = ss, tt = t, pp = p, vv = v[x], ii=i)))
    
    
  }else if(ev == 'imperv'){
    
    t <- mean(my.data[[1]]$GDD_Prev_3Month)
    p <- mean(my.data[[1]]$RH)
    v <- mean(my.data[[1]]$landvalue)
    i <- seq(from=min(my.data[[1]]$imperv),  
             to=  max(my.data[[1]]$imperv),
             length.out=1000)
    
    one_sp_ev_occ <- rbindlist(lapply(1:length(i), function(x) get.y.val(sims.mat, ss = ss, tt = t, pp = p, vv = v, ii=i[x])))
    
  }
  
  return(one_sp_ev_occ)
}

