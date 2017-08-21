##
## Fit Bayesian GLMs
##
# devtools::install_github("standev/loo")  # install dev version of loo to calculate model weights

source("init if necessary.r")

flog.info("Started social network analysis - degree and gifts")

library(rstanarm)
library(gridExtra)
library(bayesplot)
library(loo)

options(mc.cores = parallel::detectCores())


#########################################################################################################
## Prepare data
##
d.gift = na.omit( subset(herders, NamedInNetmap==1, select=c(NumGifts, CoopGiven.OtherReport)) )


#########################################################################################################
## Plot degree distributions
##
# these functions are the same as rethinking::simplehist() but without requiring the user to install the rethinking package...
plot_self = function() plot(table(subset(herders, HerderID %in% netmappers_list)$CoopGiven.SelfReport), 
                            ylab="Frequency", xlab="Self-reported cooperation given", main=("(a)"))

plot_other = function() plot(table(subset(herders, HerderID %in% netmappers_list)$CoopGiven.OtherReport), 
                             ylab="", xlab="Cooperation given (reported by others)", main=("(b)"))


png(file.path(plots.dir, "degree distribution - cooperation network.png"), height=10, width=20, units="cm", res=300)
  par(mfrow=c(1,2))
  plot_self()
  plot_other()
dev.off()

pdf(file.path(plots.dir, "degree distribution - cooperation network.pdf"), height=5, width=10)
  par(mfrow=c(1,2))
  plot_self()
  plot_other()
dev.off()

par(mfrow=c(1,1))  # reset 


#########################################################################################################
## Model specifications
## - univariate model specifications for each outcome
## - intercept-only and negative binomial (if Poisson) models will also be fitted from these univariate specs
## - all models will use the same priors
##
# cooperation given by ego, reported by others (out-degree in `g.netmap.i`)
f.gift.coop = as.formula("NumGifts ~ CoopGiven.OtherReport")


#########################################################################################################
## Functions:
## 1. fit models
## 2. analyse/plot model predictions
##

#' Fit Bayesian social analysis Poisson models
#'
#' @param d The data
#' @param f The formula
#' @param type The link function (default: Poisson)
#' @param fit.nb Fit a negative binomial model (default: true)?
#'
#' @return stanreg object of the best-fitting model
#'
fit_sna_models = function(d, f, type=poisson, fit.nb=T) {
  # fit models
  m = list()
  m[[1]] = stan_glm(update(f, . ~ 1), data=d, family=type,
                    prior=normal(0,2), prior_intercept=normal(0,5), 
                    chains=1, iter=10000, warmup=2000, cores=2)
  
  m[[2]] = stan_glm(f, data=d, family=type,
                    prior=normal(0,2), prior_intercept=normal(0,5), 
                    chains=1, iter=10000, warmup=2000, cores=2)
  
  if (fit.nb)
    m[[3]] = update(m[[2]], family = neg_binomial_2)
  
  # model comparison
  log_liks = list()
  for (i in 1:length(m))
    log_liks[[i]] = log_lik(m[[i]])
  
  m.w = model_weights(log_liks)

  return(list(
    best_model = m[[ which.max(m.w) ]],
    weights = m.w
  ))
}


#' Plot predictions from SNA models
#'
#' @param m stanreg model
#' @param d the data
#' @param param string, name of variable used for predictions
#' @param param_labels string, what to label `param` on the parameter estimates plot
#' @param response string, name of the response variable (probably 'NumGifts')
#' @param x_label string, what to label the x axis
#' @param y_label string, what to label the y axis
#'
#' @return list containing two ggplots: the parameter estimates and the prediction plot
#'
plot_sna_model = function(m, d, param="", param_labels="", response="NumGifts",
                          x_label="", y_label="No. gifts received") {
  
  # construct a new univariate dataframe for generating predictions
  newdata = data.frame(seq(from = min(d[[param]]), to = max(d[[param]]), length.out=10))
  names(newdata) = param
  
  post = posterior_predict(m, newdata = newdata)
  pred = posterior_linpred(m, newdata = newdata, transform=T)
  
  quants = apply(post, 2, quantile, probs = c(0.025, 0.5, 0.975))  # quantiles over mcmc samples
  quants2 = apply(pred, 2, quantile, probs = c(0.025, 0.5, 0.975))  # quantiles over mcmc samples
  row.names(quants) = c("sim.lwr", "sim.med", "sim.upr")
  row.names(quants2) = c("lwr", response, "upr")
  
  newdata = cbind(newdata, t(quants), t(quants2))
  
  # get dataframe of posterior samples, rename to prettier labels, and exponentiate
  draws = as.data.frame(m)
  names(draws) = c("Intercept", param_labels)
  #draws = exp(draws)
  
  # plot prediction intervals
  # this will go in the top-left corner of the predictions plot
  options(bayesplot.base_size = 20, bayesplot.base_family = "sans")
  color_scheme_set("pink")
  pi = mcmc_intervals(draws, prob=0.8, prob_outer=0.97) #+ labs(subtitle = "Interval estimates")
  
  # colour for plotting
  pinkish = rgb(224, 151, 163, maxColorValue = 255)

  p1 = ggplot(d, aes_string(x=param, y=response)) +
    geom_jitter(alpha=0.2) +
    geom_line(data=newdata, colour=pinkish, size=1.5) +
    geom_ribbon(data=newdata, aes(ymin=lwr, ymax=upr), alpha=0.2, fill=pinkish) +
    geom_ribbon(data=newdata, aes(ymin=sim.lwr, ymax=sim.upr), alpha=0.2, fill=pinkish) +
    
    xlab(x_label) +
    ylab(y_label) +
    common_theme
  
  return(list(
    pred_intervals = pi,
    predictions = p1
  ))
}

flog.info("Fitting models...")


#########################################################################################################
## do people named more often in cooperative networks receive more gifts?
##
m.gift.coop = fit_sna_models(d.gift, f.gift.coop)

plt.gift.coop = plot_sna_model(m.gift.coop$best_model, d.gift, param=all.vars(f.gift.coop)[2], 
                               param_labels="Times named", x_label="Cooperation (reported by others)")

flog.info("... finished fitting models")
flog.info("Saving model outputs")


#########################################################################################################
## Plot predictions for gift size
##
ggsave(file.path(plots.dir, "SNA - degree and gifts.png"), plot=plt.gift.coop$predictions, height=10, width=10, units="cm")
ggsave(file.path(plots.dir, "SNA - degree and gifts.pdf"), plot=plt.gift.coop$predictions, height=10, width=10, units="cm")


#########################################################################################################
## Save models and plots
##
save(d.gift, m.gift.coop, plt.gift.coop,
     file=file.path(models.dir, "social network analysis - degree and gifts.rdata"))

flog.info("Finished social network analysis - degree and gifts")
