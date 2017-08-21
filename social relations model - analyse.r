##
## Analyse social relations models
##
source("init if necessary.r")

library(ggplot2)
library(bayesplot)

# import social relations models
load(file.path(models.dir, "social relations models.RData"))
load(file.path(models.dir, "social relations models - comparison.RData"))

flog.info("Analysing social relations models")

best_srrm = fit_srrm2  # the best model, based on goodness of fit and performance metrics


################################################################################
## Coefficients for the best-fitting model
##
log_odds = best_srrm$BETA
log_odds_sum = t( apply(log_odds, 2, function(x) quantile(x, c(0.5, 0.025, 0.975))) )

write.csv(log_odds_sum, file=file.path(results.dir, "social relations model - coefficients table.csv"), row.names=T)


####################################################################
## Posterior predictions for best model
##
n.samples = 50
r_seq = seq(0, 0.5, length.out = n.samples)
d.pred = expand.grid(r=r_seq, siida=0:1, gift=0:1)

# calculate model predictions
pred.raw = sapply(1:nrow(d.pred), function(i) p.link_srrm2(best_srrm, d.pred$r[i], d.pred$siida[i], d.pred$gift[i]))
pred.p = apply(pred.raw, 2, mean)
pred.p.PI = apply(pred.raw, 2, quantile, c(0.025, 0.975))  # apply(pred.raw, 2, PI)

d.pred$pred.p = pred.p
d.pred$lwr = pred.p.PI[1,]
d.pred$upr = pred.p.PI[2,]

##
## plot predictions, split by siida co-membership and whether or not ego was named by alter in their cooperation network
##
d.pred$gift = factor(d.pred$gift)
levels(d.pred$gift) = c("No gift given", "Gift given to alter")

plt.srm = ggplot(d.pred, aes(x=r, y=pred.p)) +
  geom_line(aes(colour=factor(siida)), size=1.5) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=factor(siida)), alpha=0.3) +
  
  facet_grid(~gift) +  #, labeller="label_both") +
  
  scale_colour_manual(values = saami_blue_green) +
  scale_fill_manual(values = saami_blue_green) +
  
  scale_x_log10(breaks=c(0.0001, 0.015, 0.031, 0.063, 0.125, 0.25, 0.5)) +  # plot relatedness on log scale with breaks at familiar points
  #c(0.0001, 0.0039, 0.0078, 0.015, 0.031, 0.063, 0.125, 0.25, 0.5)
  
  xlab("Coefficient of relatedness") +
  ylab("Probability of naming alter in cooperation network") +
  ylim(c(0,1)) +
  common_theme

plt.srm
ggsave(filename=file.path(plots.dir, "social relations model predictions.png"), plot=plt.srm, width=300, height=150, units="mm")
ggsave(filename=file.path(plots.dir, "social relations model predictions.pdf"), plot=plt.srm, width=300, height=150, units="mm")

# save a version for A0 poster
# plt.srm + poster_theme
# ggsave(filename=file.path(plots.dir, "social relations model predictions - poster.png"), width=475, height=250, units="mm")


#######################################################################
## Calculate and plot variance partition coefficients and reciprocity correlations
##
vc = best_srrm$VC

sigma2_g = vc[,1]  # within-row (giver) variance
sigma2_r = vc[,3]  # within-column (receiver) variance
sigma_gr = vc[,2]  # giver-receiver covariance

rho_gr = sigma_gr / sqrt(sigma2_g * sigma2_r)  # giver-receiver correlation

rho_dd = vc[,4]    # within-dyad correlation
sigma2_d = vc[,5]  # between-dyad variance

##
## reciprocity
##
# plot prediction intervals of correlations directly from the posteriors
rho = data.frame(Generalised = rho_gr, Dyadic = rho_dd)

# plot prediction intervals for reciprocity correlations
# this will go in the top-left corner of the predictions plot
options(bayesplot.base_size = 12, bayesplot.base_family = "sans")
color_scheme_set("yellow")

plt.srm.recip = mcmc_intervals(rho, prob=0.8, prob_outer=0.95) + labs(x = "Reciprocity correlation") + common_theme

##
## partitioning variance
##
vpc_g = (sigma2_g / (sigma2_g + sigma2_r + sigma2_d))  # giver VPC
vpc_r = (sigma2_r / (sigma2_g + sigma2_r + sigma2_d))  # receiver VPC
vpc_d = (sigma2_d / (sigma2_g + sigma2_r + sigma2_d))  # relationship VPC

# make a violin plot of the VPCs
vpc = data.frame(vpc_g, vpc_r, vpc_d) %>% 
  gather(VPC, value, vpc_g:vpc_d)
vpc$VPC = factor(vpc$VPC, levels=c("vpc_d", "vpc_r", "vpc_g"))
levels(vpc$VPC) = c("Dyadic VPC", "Receiver VPC", "Giver VPC")

plt.srm.vpc = ggplot(vpc, aes(x=VPC, y=value)) +
  geom_violin(fill=saami_yellow, colour=saami_yellow_dark, alpha=0.75, draw_quantiles = c(0.025, 0.5, 0.975)) +
  xlab("") +
  ylab("Variance partition coefficient") +
  coord_flip() +
  common_theme

##
## make figure for paper containing reciprocity correlations + VPCs
##
plt_a = plt.srm.recip + ggtitle("a")
plt_b = plt.srm.vpc   + ggtitle("b")

png(file.path(plots.dir, "social relations model - reciprocity and vpc.png"), height=12.5, width=25, units="cm", res=300)
gridExtra::grid.arrange(plt_a, plt_b, ncol=2)
dev.off()

pdf(file.path(plots.dir, "social relations model - reciprocity and vpc.pdf"), height=5, width=10)
gridExtra::grid.arrange(plt_a, plt_b, ncol=2)
dev.off()

##
## save plots for poster
##
# plt.srm.recip + poster_theme
# ggsave(filename=file.path(plots.dir, "social relations model - poster - reciprocity correlations.png"), plot=recip, width=175, height=125, units="mm")
# plt.srm.vpc + poster_theme
# ggsave(filename=file.path(plots.dir, "social relations model - poster - vpc.png"), width=175, height=125, units="mm")  # for poster

flog.info("Finished analysing social relations models")
