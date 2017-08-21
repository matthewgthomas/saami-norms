##
## Initialisation variables, functions, etc.
##
library(tidyverse)
# library(readxl)
# library(data.table)
library(igraph)
library(futile.logger)

flog.info("Initialising...")

##
## directories for loading/saving stuff
##
data.dir    = "./data"
plots.dir   = "./plots"
results.dir = "./results"
models.dir  = "./models"

# create the directories if they don't exist
if (!dir.exists(plots.dir))
  dir.create(plots.dir)
if (!dir.exists(results.dir))
  dir.create(results.dir)
if (!dir.exists(models.dir))
  dir.create(models.dir)


############################################################################
## Import all data
##
flog.info("Importing data files...")

herders      = read.csv(file.path(data.dir, "herders.csv"))
herders.wide = read.csv(file.path(data.dir, "herders-dyadic-wide.csv"))
gifts        = read.csv(file.path(data.dir, "gifts.csv"))
net.maps     = read.csv(file.path(data.dir, "net-maps.csv"))

##
## some useful lists
##
interviewee_list = subset(herders, Interviewed.==1)$HerderID  # people we interviewed (list everyone, including unlicensed herders but *not* people in district 30C)
netmappers_list  = unique(net.maps$Interviewee)               # people who drew net-maps


############################################################################
## Create social networks
##
flog.info("Creating social networks...")

##
## First, a bit of data wrangling
##
# make dataframe containing outbound connections only
net.maps.o = filter(net.maps, Alter!=Interviewee) %>% dplyr::select(Ego, Alter, LinkType)
## in this case, in-degree will be a measure of help received, as reported by others
## and out-degree will be the amount of self-reported help given to others
#... inbound connections only
net.maps.i = filter(net.maps, Alter==Interviewee) %>% dplyr::select(Ego, Alter, LinkType)
## in this case, in-degree will be a self-reported measure of help received
## and out-degree will be the amount of help given, as reported by others

# keep only one entry for each Ego-Alter pair, rather than separate entries for each help type
# (this would have the same effect as using igraph::simplify())
coop.net   = unique(subset(net.maps,   select=c(Ego, Alter)))
coop.net.o = unique(subset(net.maps.o, select=c(Ego, Alter)))
coop.net.i = unique(subset(net.maps.i, select=c(Ego, Alter)))


##
## create networks
##
# siida membership
siida.mem = subset( herders.wide, SameSiida==1, select=c(Ego, Alter) )
g.siida = graph.data.frame(siida.mem, vertices=herders, directed=T)
rm(siida.mem)

# relatives
kin = subset( herders.wide, r>0, select=c(Ego, Alter, r) )  # this overwrites the previous `kin` df that was created from the net maps (and, thus, was incomplete)
kin$Weight = kin$r
g.kin = graph.data.frame(kin, vertices=herders, directed=T)

# gifts (the `gifts` dataframe was created directly from the netmap, towards the beginning of this file)
gifts$Weight = gifts$GiftSize
g.gifts = graph.data.frame(gifts, vertices=herders, directed=T)

# net-maps with interviewees' outbound connections only
g.netmap.o = graph.data.frame(coop.net.o, vertices=herders, directed=T)

# net-maps with interviewees' inbound connections only
g.netmap.i = graph.data.frame(coop.net.i, vertices=herders, directed=T)

# net-map with everything
g.all = graph.data.frame(coop.net, vertices = herders, directed = T)

##
## make graphs of each type of net-map (self-reported outbound connections only)
##
net.maps.advice  = subset(net.maps.o, LinkType=="Advice", select=c(Ego, Alter))
net.maps.help    = subset(net.maps.o, LinkType=="Help",   select=c(Ego, Alter))
net.maps.sharing = subset(net.maps.o, LinkType=="Items",  select=c(Ego, Alter))

g.advice  = simplify( graph.data.frame( net.maps.advice,  vertices=herders, directed=T) )
g.help    = simplify( graph.data.frame( net.maps.help,    vertices=herders, directed=T) )
g.sharing = simplify( graph.data.frame( net.maps.sharing, vertices=herders, directed=T) )

# don't need these anymore
rm(coop.net.i, coop.net.o)
rm(net.maps.i, net.maps.o)


#######################################################################################
## Calculate centrality measures (in/out degrees)
##
flog.info("Calculating network statistics...")

## there are four measures of degree in the cooperative network:
##   
## - cooperation given from ego to alter, as reported by ego (`herders$CoopGiven.SelfReport` which is out-degree in `g.netmap.o`)
## - cooperation given from ego to alter, as reported by alter (`herders$CoopGiven.OtherReport` which is out-degree in `g.netmap.i`)
## - cooperation received by ego from alter, as reported by ego (`herders$CoopReceived.SelfReport` which is in-degree in `g.netmap.i`)
## - cooperation received by ego from alter, as reported by alter (`herders$CoopReceived.OthersReport` which is in-degree in `g.netmap.o`)
##
# cooperation from outbound links (in-degree = cooperation received, reported by others; out-degree = cooperation given, reported by self)
deg.in  = degree(g.netmap.o, mode="in")
herders$CoopReceived.OthersReport = deg.in[ as.character(herders$HerderID) ]
deg.out = degree(g.netmap.o, mode="out")
herders$CoopGiven.SelfReport = deg.out[ as.character(herders$HerderID) ]

# cooperation from inbound links (in-degree = cooperation received, reported by self; out-degree = cooperation given, reported by others)
deg.in  = degree(g.netmap.i, mode="in")
herders$CoopReceived.SelfReport = deg.in[ as.character(herders$HerderID) ]
deg.out = degree(g.netmap.i, mode="out")
herders$CoopGiven.OtherReport = deg.out[ as.character(herders$HerderID) ]

# cooperation from complete network
deg.in  = degree(g.all, mode="in")
herders$coop.deg.in = deg.in[ as.character(herders$HerderID) ]
deg.out = degree(g.all, mode="out")
herders$coop.deg.out = deg.out[ as.character(herders$HerderID) ]

# no. gifts received
deg.in  = degree(g.gifts, mode="in")
herders$NumGifts = deg.in[ as.character(herders$HerderID) ]
herders$gifts.bin = ifelse(herders$NumGifts > 0, 1, 0)  # also create a binary variable for whether/not they received a gift

# no. gifts given
deg.out = degree(g.gifts, mode="out")
herders$NumGiftsGiven = deg.out[ as.character(herders$HerderID) ]

rm(deg.in, deg.out)

flog.info("Finished initialising")
