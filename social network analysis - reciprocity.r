##
## Reciprocity in the three net-maps and gift networks
##
source("init if necessary.r")
library(dplyr)
library(igraph)
library(ggplot2)

flog.info("Started social network analysis - reciprocity")

net_names = c("Advice", "Help", "Sharing", "Gifts")


####################################################
## Observed reciprocity
##
obs.recip = data.frame(Network = net_names,
                       reciprocity = c(reciprocity(g.advice), 
                                       reciprocity(g.help), 
                                       reciprocity(g.sharing), 
                                       reciprocity(g.gifts)),
                       assortment  = c(assortativity.nominal(g.advice, types=V(g.advice)$SiidaID, directed=T),
                                       assortativity.nominal(g.help, types=V(g.help)$SiidaID, directed=T),
                                       assortativity.nominal(g.sharing, types=V(g.sharing)$SiidaID, directed=T),
                                       assortativity.nominal(g.gifts, types=V(g.gifts)$SiidaID, directed=T)))


####################################################
## simulate random behaviour in networks
##
neutral.network.sim = function(n.egos, n.alters, n.ties=3, n.sims=1000, n.groups = 15) {
  require(igraph)
  
  ## debug ###
  # n.egos = 53; n.alters = 120; n.ties=3; n.sims=1000
  ###
  
  blank.matrix = matrix(0, n.alters - n.egos, n.alters)  # blank matrix to tag onto end (since iGraph needs symmetrical matrix to generate graphs)
  # set up dataframe for results
  recip = data.frame(i=integer(0), reciprocity=numeric(0), reciprocity.er=numeric(0), assortment=numeric(0))
  
  # assign people to siidas randomly
  siida_members = sample(1:n.groups, n.alters, replace=T)
  
  for (i in 1:n.sims) {
    gifts.rnd = matrix(0, n.egos, n.alters)
    
    # each row (giver) can give at least one, up to `n.ties` gifts/helps
    for (j in 1:n.egos) {
      #recipients = sample(1:n.alters, n.ties, replace=F)  # choose `n.ties` number of recipients
      recipients = sample(1:n.alters, round(runif(1, min=1, max=n.ties), 0), replace=F)  # choose between 1 and `n.ties` number of recipients
      gifts.rnd[j, recipients] = 1  # give them gifts
    }
    # make symmetrical matrix
    gifts.rnd = rbind(gifts.rnd, blank.matrix)
    # generate the graph from adj matrix
    g.tmp = graph.adjacency(gifts.rnd, mode="directed", diag=F)
    
    # for assortativity, get nodes and in-degrees
    in.deg = degree(g.tmp, mode="in")
    
    # also make an erdos-renyi random graph and calculate its reciprocity
    g.rnd = sample_gnm(n=n.alters, m=n.ties, directed=T)
    
    # save results
    recip = rbind(recip, c(i, reciprocity(g.tmp), reciprocity(g.rnd), assortativity.nominal(g.tmp, types=siida_members, directed=T)))
  }
  
  names(recip) = c("i", "reciprocity", "reciprocity.rnd", "assortment")
  return(recip)
}


##################################################
## Simulate networks for each net-map and gifts
##
## gifts
##
n.gift.givers = length(unique(gifts$Ego))  # no. people who played gift game
n.recipients = nrow(herders)  # no. potential recipients
n.gifts = gifts %>% group_by(Ego) %>% summarise(n.gifts = n())
n.gifts = max(n.gifts$n.gifts)
n.gifts = ifelse(n.gifts == 0, 1, n.gifts)  # make sure they give at least one gift

# sim
sim.gifts = neutral.network.sim(n.gift.givers, n.recipients, n.gifts)
sim.gifts$Network = "Gifts"

##
## advice
##
n.ego   = length(unique(net.maps.advice$Ego))
n.alter = length(unique(net.maps.advice$Alter))
n.ties  = net.maps.advice %>% group_by(Ego) %>% summarise(n.ties =length(Ego)) %>% summarise(max(n.ties)) %>% unlist(.)
n.ties  = ifelse(n.ties == 0, 1, n.ties)  # make sure they have at least one tie
# sim
sim.advice = neutral.network.sim(n.ego, n.alter, n.ties)
sim.advice$Network = "Advice"

##
## help
##
n.ego   = length(unique(net.maps.help$Ego))
n.alter = length(unique(net.maps.help$Alter))
n.ties  = net.maps.help %>% group_by(Ego) %>% summarise(n.ties =length(Ego)) %>% summarise(max(n.ties)) %>% unlist(.)
n.ties  = ifelse(n.ties == 0, 1, n.ties)  # make sure they have at least one tie
# sim
sim.help = neutral.network.sim(n.ego, n.alter, n.ties)
sim.help$Network = "Help"

##
## sharing
##
n.ego   = length(unique(net.maps.sharing$Ego))
n.alter = length(unique(net.maps.sharing$Alter))
n.ties  = net.maps.sharing %>% group_by(Ego) %>% summarise(n.ties =length(Ego)) %>% summarise(max(n.ties)) %>% unlist(.)
n.ties  = ifelse(n.ties == 0, 1, n.ties)  # make sure they have at least one tie
# sim
sim.sharing = neutral.network.sim(n.ego, n.alter, n.ties)
sim.sharing$Network = "Sharing"

# bosh them all together
sim.nets = dplyr::bind_rows(sim.advice, sim.help, sim.sharing, sim.gifts)

sim.nets$Network = factor(sim.nets$Network, levels=net_names)


############################################################
## Plot reciprocities
##
plt.recip = ggplot(sim.nets, aes(x=factor(Network), y=reciprocity)) + 
  #facet_wrap(~Measure, scales="free", ncol=2) +
  geom_boxplot() + 
  geom_point(data=obs.recip, aes(x=factor(Network), y=reciprocity), fill=saami_lightred, colour=saami_red, shape=23, size=3) + 
  
  geom_blank(data=obs.recip) +
  #ylim(c(0, round(max(obs.recip$Value), 1))) +  # extend y-axis so observed reciprocities will appear
  ylab("Simulated/observed reciprocity") +
  xlab("Network") +
  ggtitle("a") +
  common_theme +
  theme(plot.title = element_text(face="bold"))

##
## ... and assortativities
##
plt.assort = ggplot(sim.nets, aes(x=factor(Network), y=assortment)) + 
  #facet_wrap(~Measure, scales="free", ncol=2) +
  geom_boxplot() + 
  geom_point(data=obs.recip, aes(x=factor(Network), y=assortment), fill=saami_lightred, colour=saami_red, shape=23, size=3) + 
  
  geom_blank(data=obs.recip) +
  #ylim(c(0, round(max(obs.recip$Value), 1))) +  # extend y-axis so observed reciprocities will appear
  ylab("") +  # don't need y label because displaying it next to reciprocity figure, which has one
  xlab("Network") +
  ggtitle("b") +
  common_theme +
  theme(plot.title = element_text(face="bold"))

png(filename=file.path(plots.dir, "network stats - reciprocity and assortment.png"), height=10, width=20, units="cm", res=300)
  gridExtra::grid.arrange(plt.recip, plt.assort, ncol=2)
dev.off()

pdf(file.path(plots.dir, "network stats - reciprocity and assortment.pdf"), height=5, width=10)
  gridExtra::grid.arrange(plt.recip, plt.assort, ncol=2)
dev.off()

flog.info("Finished society network analysis - reciprocity")
