model.jags <- function() {
  
  ### priors
  
    #Detection
  
  #overall intercept of detection across species
  mu.p.0   ~ dnorm(0,0.01)
  
  ## random effect of species on detection
  sigma.p.sp   ~ dunif(0,10)
  tau.p.sp    <- 1/(sigma.p.sp*sigma.p.sp)
  # for(sp in 1:nsp) {
  #   p.sp[sp]   ~ dnorm(0, tau.p.sp)
  # }
  
  
  ## fixed effect of # collection days on detection (doesn't need to be species-specific)
  # p.colldays ~ dnorm(0,0.01)
  
  ## fixed effect of wind on detection (doesn't need to be species-specific because doing it by taxonomic group; so only use this effect for flying insects, not arachnids)
  # p.wind ~ dnorm(0,0.01)
  
  # month as quadatric
  mu.p.month.quad ~ dnorm(0,0.01);T(,0)
  
  
  
  
  
  ##Occupancy
  
  #overall intercept of occupancy across species
  mu.psi.0 ~ dnorm(0,0.01)
  
  #Quadratic term for temp
  #mu.psi.temp.quad ~ dnorm(0,0.01);T(,0)
  
  ## random effect of species on occupancy
  sigma.psi.sp   ~ dunif(0,10)
  tau.psi.sp    <- 1/(sigma.psi.sp*sigma.psi.sp)
  for(sp in 1:nsp) {
    psi.sp[sp]   ~ dnorm(0, tau.psi.sp)
  }
  
  ## Species-specific slopes to distance to natural areas
  mu.psi.distance.natural ~ dnorm(0,0.01)
  sigma.psi.distance.natural ~ dunif(0,10)
  tau.psi.distance.natural <- 1/(sigma.psi.distance.natural*sigma.psi.distance.natural)
  
  for(sp in 1:nsp){
    psi.distance.natural[sp] ~ dnorm(mu.psi.distance.natural, tau.psi.distance.natural)
  }
  
  ## Species-specific slopes to elevation
  
  mu.psi.elevation ~ dnorm(0,0.01)
  sigma.psi.elevation ~ dunif(0,10)
  tau.psi.elevation <- 1/(sigma.psi.elevation*sigma.psi.elevation)
  
  for(sp in 1:nsp){
    psi.elevation[sp] ~ dnorm(mu.psi.elevation, tau.psi.elevation)
  }
  
  
  ##### Actual Model
  
  for(ind in 1:nind) {
    
        
          ##occupancy
          
          logit(psi[ind]) <- #has to be in same order as array
            mu.psi.0 +
            psi.sp[sp[ind]] +
            psi.elevation[sp[ind]]*elevation[site[ind]] +
            psi.distance.natural[sp[ind]]*distance_natural[site[ind]]
    
        
        ## detection
        logit(p[ind]) <- #has to be in same order as array
          mu.p.0 +
          mu.p.month.quad*month_det[site[ind], visit[ind]]^2
          # p.colldays*colldays[site[ind], visit[ind]] +
          # p.wind*wind[site[ind], visit[ind]] +
          # p.sp[sp[ind]]
      }
    
  
  ## latent state and likelihood    

        
  
  for(ind in 1:nind) {
    
    Z[ind] ~ dbern(psi[ind]) #has to be in same order as array
    
    mu.p[ind] <- Z[ind] * p[ind]
    
    X[ind] ~ dbern(mu.p[ind])
  }
  
}
get.params <- function()
  c('mu.p.0',
    'mu.p.month.quad',
    'mu.psi.0',
    'mu.psi.distance.natural',
    'mu.psi.elevation',
    'psi.sp',
    'psi.distance.natural',
    'psi.elevation')

