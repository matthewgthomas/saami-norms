##
## social relations model using amen package
## - https://cran.r-project.org/web/packages/amen/vignettes/amen.pdf
##
source("init if necessary.r")

library(amen)

flog.info("Fitting social relations models")


#################################################################
## fit Saami SRM
##

##
## create adjacency matrices
##
# get adjacency matrices for gifts, cooperation, kinship and herding group co-membership
adj.gifts = get.adjacency(g.gifts, sparse = F)
adj.kin   = get.adjacency(g.kin,   sparse = F, attr="r")
adj.siida = get.adjacency(g.siida, sparse = F)
adj.coop  = get.adjacency(g.netmap.o, sparse = F)

# (self-loops must be set to NA)
diag(adj.gifts) = NA
diag(adj.kin)   = NA
diag(adj.siida) = NA
diag(adj.coop)  = NA

# dyadic covariates: relatedness, named in cooperative network, herding group co-membership
X_dyad = array(dim=c(nrow(adj.kin), ncol(adj.kin), 3))
X_dyad[,,1] = adj.kin
X_dyad[,,2] = adj.siida
X_dyad[,,3] = adj.gifts
dimnames(X_dyad)[[3]] = c("kin", "siida", "gifts")

# dyadic covariates, including siida x relatedness interaction
X_dyad_int = array(dim=c(nrow(adj.kin), ncol(adj.kin), 4))
X_dyad_int[,,1] = adj.kin
X_dyad_int[,,2] = adj.siida
X_dyad_int[,,3] = adj.gifts
X_dyad_int[,,4] = adj.kin * adj.siida
dimnames(X_dyad_int)[[3]] = c("kin", "siida", "gifts", "kin x siida")

# dyadic covariates, including siida x relatedness x gift interaction
X_dyad_all = array(dim=c(nrow(adj.kin), ncol(adj.kin), 7))
X_dyad_all[,,1] = adj.kin
X_dyad_all[,,2] = adj.siida
X_dyad_all[,,3] = adj.gifts
X_dyad_all[,,4] = adj.kin * adj.siida
X_dyad_all[,,5] = adj.kin * adj.gifts
X_dyad_all[,,6] = adj.siida * adj.gifts
X_dyad_all[,,7] = adj.kin * adj.siida * adj.gifts
dimnames(X_dyad_all)[[3]] = c("kin", "siida", "gifts", "kin x siida", "kin x gifts", "siida x gifts", "kin x siida x gifts")

# individual covariates: interviewed/not?
X_ind = as.matrix(subset(herders, select=c(Interviewed.)))
dimnames(X_ind)[[2]] = c("interviewed")

##
## fit models
##
flog.info("... fitting intercept-only models")

# fit intercept-only models
fit_null      = ame(adj.coop, model="bin", plot=F, print=F)
fit_null_ind  = ame(adj.coop, Xr=X_ind, model="bin", plot=F, print=F)                    # intercept-only, but controlling for whether/not ego was interviewed

flog.info("... fitting model: r + siida + gifts")

# fit with dyadic and individual covariates
fit_srrm2 = ame(adj.coop,       # response adjacency matrix
                Xd=X_dyad,      # array of dyadic covariates
                Xr=X_ind,       # node row covariates (missing values are replaced with zeros)
                model="bin",
                plot=F, print=F)

flog.info("... fitting model: r x siida + gifts")

# fit with r x siida interaction
fit_srrm3 = ame(adj.coop,       # response adjacency matrix
                Xd=X_dyad_int,  # array of dyadic covariates, including r x siida interaction
                Xr=X_ind,       # node row covariates (missing values are replaced with zeros)
                model="bin",
                plot=F, print=F)

flog.info("... fitting model: r x siida x gifts")

# fit with all interactions
fit_srrm4 = ame(adj.coop,       # response adjacency matrix
                Xd=X_dyad_all,  # array of dyadic covariates, including r x siida interaction
                Xr=X_ind,       # node row covariates (missing values are replaced with zeros)
                model="bin",
                plot=F, print=F)


####################################################################
## Link functions for each model
## - they'll be used in model comparison and analysis
##
# inverse probit link function
## found out how to calculate this by fitting a nonsense probit model: 
##   myprobit <- glm(Interviewed. ~ 1, family=binomial(link="probit"), data=herders)
## then looking at the code in `family(myprobit)$linkinv`
probit = function(eta) 
{
  thresh <- -qnorm(.Machine$double.eps)  # `.Machine$double.eps` == the smallest positive floating-point number x such that 1 + x != 1
  eta <- pmin(pmax(eta, -thresh), thresh)
  pnorm(eta)
}

# functions for posterior predictions
# - m is the SRM
# - r is relatedness (0-0.5)
# - siida is group co-membership (0, 1)
# - coop is whether/not alter named ego in their cooperative network
# - re is the random effects

p.link_null = function(m, r=0, siida=0, gift=0, re=0) {
  post = m$BETA
  
  eta = post[,1] +         # intercept
    post[,2] +             # people we interviewed
    re                     # random effects
  
  return( probit(eta) )
}

p.link_srrm2 = function(m, r, siida, gift, re=0) {
  post = m$BETA
  
  eta = post[,1] +         # intercept
    post[,2] +             # people we interviewed
    post[,3]*r +           # relatedness
    post[,4]*siida +       # members of same siida?
    post[,5]*gift +        # ego gave alter a gift?
    re                     # random effects
  
  return( probit(eta) )
}

p.link_srrm3 = function(m, r, siida, gift, re=0) {
  post = m$BETA
  
  eta = post[,1] +          # intercept
    post[,2] +              # people we interviewed
    post[,3]*r +            # relatedness
    post[,4]*siida +        # members of same siida?
    post[,5]*gift +         # ego gave alter a gift?
    post[,6] * r * siida +  # r x siida interaction
    re                      # random effects
  
  return( probit(eta) )
}

p.link_srrm4 = function(m, r, siida, gift, re=0) {
  post = m$BETA
  
  eta = post[,1] +                 # intercept
    post[,2] +                     # people we interviewed
    post[,3]*r +                   # relatedness
    post[,4]*siida +               # members of same siida?
    post[,5]*gift +                # ego gave alter a gift?
    post[,6] * r * siida +         # r x siida interaction
    post[,7] * r * gift +          # r x gift interaction
    post[,8] * siida * gift +      # siida x gift interaction
    post[,9] * r * siida * gift +  # r x siida x gift interaction
    re                             # random effects
  
  return( probit(eta) )
}


#######################################################################
## Save models etc.
##
save(X_dyad, X_dyad_int, X_dyad_all, adj.coop, adj.kin, adj.siida, adj.gifts,
     fit_null, fit_null_ind, fit_srrm2, fit_srrm3, fit_srrm4,
     p.link_null, p.link_srrm2, p.link_srrm3, p.link_srrm4, probit,
     file=file.path(models.dir, "social relations models.rdata"))

flog.info("Finished fitting social relations models")
