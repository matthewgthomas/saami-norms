##
## Initialisation variables, functions, etc.
##
library(tidyverse)
library(readxl)
library(data.table)
library(igraph)
library(futile.logger)

flog.info("Preparing data...")

##
## directories for loading/saving stuff
##
data.dir     = "../../data/2016"
data.out.dir = "./data"
plots.dir    = "./plots"
results.dir  = "./results"
models.dir   = "./models"

# create the directories if they don't exist
if (!dir.exists(data.out.dir))  # data out
  dir.create(data.out.dir)
if (!dir.exists(plots.dir))     # plots
  dir.create(plots.dir)
if (!dir.exists(results.dir))   # results
  dir.create(results.dir)
if (!dir.exists(models.dir))    # models
  dir.create(models.dir)

##
## data processing parameters
##
license.owners.only = T  # whether or not to output only license owners (siida shares)
keep.30C = F             # whether or not to remove people in district 30C

# if this variable has been defined outside of this file and set to True, then the people in `people.to.remove` (line 55) will *not* be removed;
# otherwise, set the variable to false and remove unwanted herders
if (!exists("keep.all.herders")) { keep.30C=F }


############################################################################
## Import all data
##
flog.info("Importing data files...")

herders    = read_excel(file.path(data.dir, "Herders_anon.xlsx"), sheet = 1)
herd.sizes = read_excel(file.path(data.dir, "Herders_anon.xlsx"), sheet = "Reindeer - from NRK")
survey     = read_excel(file.path(data.dir, "Surveys.xlsx"), sheet = "Survey")  # interview data
net.maps   = read_excel(file.path(data.dir, "Net-maps.xlsx"), sheet = "Net-maps - individuals")
sibs       = read_excel(file.path(data.dir, "Surveys.xlsx"), sheet = "Siblings")
kids       = read_excel(file.path(data.dir, "Surveys.xlsx"), sheet = "Children")

# remove person/people from district 30C (no net-maps or games data for them)
if (!keep.30C)
{
  people.to.remove = subset(herders, Distrikt=="30C")$HerderID
  herders  = subset(herders,  !HerderID %in% people.to.remove)
  survey   = subset(survey,   !HerderID %in% people.to.remove)
  net.maps = subset(net.maps, !Ego %in% people.to.remove | !Alter %in% people.to.remove)  # this shouldn't cause any changes
  sibs     = subset(sibs,     !SibID %in% people.to.remove)
  
  rm(people.to.remove)
}


############################################################################
## Sort out variables
##
flog.info("Formatting data...")

##
## main herders table
##
setnames(herders, c("License?", "Interviewed?"), c("SiidaShareYN", "Interviewed."))

# drop columns to preserve anonymity
herders$Etternavn = NULL
herders$Kode = NULL

##
## some useful lists
##
siida.share.ids = subset(herders, SiidaShareYN==1)$HerderID  # IDs for herders with siida shares in the district
interviewee_list = survey$HerderID                           # people we interviewed (list everyone, including unlicensed herders but *not* people in district 30C)
netmappers_list = unique(net.maps$Interviewee)               # people who drew net-maps

##
## should we keep siida shares only?
##
if (license.owners.only)
{
  herders = subset(herders, SiidaShareYN==1)
  survey = subset(survey, HerderID %in% siida.share.ids)
  net.maps = subset(net.maps, Ego %in% siida.share.ids & Alter %in% siida.share.ids)
}

##
## interview data
##
# copy each participant's siida ID into survey table
survey = survey %>% left_join(
  select(herders, HerderID, SiidaID), by="HerderID"
)

# calculate age at the time of fieldwork
# survey$Age = 2016 - survey$BirthYear

# convert amount of subsistence from herd into ordinal scale
scale_subsistence = c("None", "Almost none", "Less than half", "About half", "More than half", "Almost all", "All")
survey$SubsistenceSpring = ordered(survey$SubsistenceSpring, levels=scale_subsistence)
survey$SubsistenceSummer = ordered(survey$SubsistenceSummer, levels=scale_subsistence)
survey$SubsistenceAutumn = ordered(survey$SubsistenceAutumn, levels=scale_subsistence)
survey$SubsistenceWinter = ordered(survey$SubsistenceWinter, levels=scale_subsistence)

# add up subsistence scores (first convert to numbers)
survey$SubsistenceTotal = as.integer(survey$SubsistenceSpring) + as.integer(survey$SubsistenceSummer) + 
  as.integer(survey$SubsistenceAutumn) + as.integer(survey$SubsistenceWinter)

# copy total subsistence score into herders table
herders = herders %>% 
  left_join( select(survey, HerderID, SubsistenceTotal), by="HerderID" )

##
## net-maps
##
net.maps = filter(net.maps, Type != "Influence")  # remove influence because it wasn't a useful measure in the end

# move gifts and kin into separate dataframes
gifts = net.maps %>% 
  filter(Type == "Gift") %>% 
  dplyr::select(Ego, Alter, Value) %>% 
  mutate(Value = as.numeric(as.character(Value))) %>%   # convert the value of the gifts into numeric
  rename(GiftSize = Value)

kin = net.maps %>% 
  filter(Type == "Kin") %>% 
  dplyr::select(Ego, Alter, r) %>% 
  na.omit(.) %>%  # de-dupe and remove NAs
  distinct()
# NB: `kin` based on the net-maps doesn't include all kin we know about - they'll be added when we start accounting for sibs and kids
#     and this dataframe will get overwritten by a more complete edge list when we construct the social networks

# keep only advice, help and shared items in `net.maps`
net.maps = filter(net.maps, Type %in% c("Advice", "Help", "Items"))

# make dataframe containing outbound connections only
net.maps.o = filter(net.maps, Alter!=Interviewee) %>% dplyr::select(Ego, Alter, Type)
## in this case, in-degree will be a measure of help received, as reported by others
## and out-degree will be the amount of self-reported help given to others
#... inbound connections only
net.maps.i = filter(net.maps, Alter==Interviewee) %>% dplyr::select(Ego, Alter, Type)
## in this case, in-degree will be a self-reported measure of help received
## and out-degree will be the amount of help given, as reported by others

# sort out factors to remove missing levels
net.maps$Type   = factor(net.maps$Type)
net.maps.o$Type = factor(net.maps.o$Type)
net.maps.i$Type = factor(net.maps.i$Type)

##
## clean up
##
rm(scale_subsistence)


############################################################################
## Get herd size data
##
# centre herd size in 2012 and rename variable
herd.sizes$num.reindeer.z = scale(herd.sizes$`Rein 2012`, scale = T)
setnames(herd.sizes, "Rein 2012", "num.reindeer")

herders = merge(herders,
                subset(herd.sizes, select=c(HerderID, num.reindeer, num.reindeer.z)),
                by="HerderID", all.x=T)


############################################################################
## Calculate complete set of siblings for each person (if they have them)
##
# assign ID numbers to sibs (continuing on from participant IDs)
## first, swap 'HerderID' and 'SibID' column names
names(sibs)[names(sibs)=="HerderID"] = "SibID2"
names(sibs)[names(sibs)=="SibID"] = "HerderID"
names(sibs)[names(sibs)=="SibID2"] = "SibID"

sibs = subset(sibs, SibName != "(deceased)" | is.na(SibName))  # remove anyone deceased (but explicitly keep people we don't know names for)

## assign a new ID number to each sib based on their 'tmpName' (a unique string basd on sex, birth year and no. kids)
## (this is the best we can do to not generate duplicate IDs where sibs without ID numbers are related to more than one interviewee -- e.g. in the case of herders 23 and 24)
largest_id = max(herders$HerderID) + 1
tmpNames = unique( subset(sibs, nchar(tmpName) > 5)$tmpName )  # make sure these names have more five characters (most information)
sib.ids = data.frame(tmpName = tmpNames, tmpID = seq(from=largest_id, to=(largest_id + length(tmpNames) - 1)))

## assign a new ID number to each sib based on their tmpNames...
sibs = merge(sibs, sib.ids, by="tmpName", all.x=T)
#... if we didn't assign an ID based on 'tmpName', give them a new, sequential ID
largest_id = max(sib.ids$tmpID) + 1
sibs$tmpID[ is.na(sibs$tmpID) ] = seq(from=largest_id, to=(largest_id + length(sibs$tmpID[ is.na(sibs$tmpID) ]) - 1))
##... and only use the generated ID if we haven't already linked the sib to someone in our original list of herders
sibs$HerderID = ifelse(is.na(sibs$HerderID), sibs$tmpID, sibs$HerderID)

sibs = as.data.table( subset(sibs, select=c(SibID, HerderID, Sex, BirthYear, NumSons, NumDaughters)) )

# each sibling 'HerderID' related to the person we interviewed 'SibID' is also sib with the others related to the interviewee
# get all combinations of sib relationships
# code adapted from: http://stackoverflow.com/a/30312324
sibs2 = left_join(
  dplyr::select(sibs, SibID, HerderID1 = HerderID),
  dplyr::select(sibs, SibID, HerderID2 = HerderID),
  by = "SibID"
) %>%
  filter(HerderID1 != HerderID2) %>%
  dplyr::select(SibID=HerderID1, HerderID=HerderID2)
sibs2 = as.data.table(sibs2)
setkey(sibs2, SibID, HerderID)
sibs2 = unique(sibs2)

# merge in covariates for the alter in the sib relationship (in this case, 'HerderID')
sibs.s = unique( subset(sibs, HerderID %in% sibs2$HerderID, select=-c(SibID)) )  # covariates to merge
setkey(sibs.s, HerderID)
setkey(sibs2, HerderID)
sibs2 = sibs.s[sibs2]

# de-dupe
setkey(sibs2, SibID, HerderID)
sibs2 = unique(sibs2)
setcolorder(sibs2, c("SibID", "HerderID", "Sex", "BirthYear", "NumSons", "NumDaughters"))  # make sure columns in right order before binding

# now need to add the people we interviewed (currently: sibs$SibID) as the alter in the sib relationship
## first, swap ego/alter columns
sibs3 = copy(sibs)
sibs3 = sibs3 %>% dplyr::select(SibID=HerderID, HerderID=SibID)
## get covars from survey table
sibs.s = as.data.table( subset(survey, select=c("HerderID", "Sex", "BirthYear", "NumSons", "NumDaughters")) )
## merge
setkey(sibs3, HerderID)
setkey(sibs.s, HerderID)
sibs3 = sibs.s[sibs3]
setcolorder(sibs3, c("SibID", "HerderID", "Sex", "BirthYear", "NumSons", "NumDaughters"))  # make sure columns in right order before binding

# merge all sib combinations
sibs = rbindlist(list(sibs, sibs2, sibs3))  # append newly expanded list of sibs to main list of sibs

# de-dupe
setkey(sibs, SibID, HerderID)
sibs = unique(sibs)

rm(sibs.s, sibs2, sibs3, sib.ids, largest_id, tmpNames)


############################################################################
## Assign new ID numbers to kids if they don't already have them
##
# assign ID numbers to kids (continuing on from participant IDs)
# set participant as parent for their kids and assign ID numbers to kids
names(kids)[names(kids)=="HerderID"] = "ParentID"
names(kids)[names(kids)=="ChildID"]  = "HerderID"
largest_id = max(sibs$HerderID) + 1
kids$tmpID = seq(from=largest_id, to=(largest_id + nrow(kids) - 1))
##... but only use new ID if we haven't already linked the sib to someone in our original list of herders
kids$HerderID = ifelse(is.na(kids$HerderID), kids$tmpID, kids$HerderID)


############################################################################
## Make relatedness matrix (as edge list)
##
flog.info("Making relatedness matrix...")

sibs_sub = subset(sibs, select=c("HerderID", "SibID"))
kids_sub = subset(kids, select=c("HerderID", "ParentID"))

names(sibs_sub) = c("Ego", "Alter")
names(kids_sub) = c("Ego", "Alter")

herders.r = rbind(sibs_sub, kids_sub)
herders.r$r = 0.5  # relatedness of sibs and kids

# add kin from netmaps
herders.r = rbind(herders.r, kin)

# convert to a data table and de-dupe
herders.r = as.data.table(herders.r)
setkey(herders.r, Ego, Alter)
herders.r = na.omit(herders.r)  # remove incomplete
herders.r = unique(herders.r)   # remove duplicates
herders.r = subset(herders.r, Ego!=Alter)  # (just in case)

##
## some dyads only have one entry - but each ego-alter pair should also appear as alter-ego - should be symmetric - so add missing dyads
##
# first, assign a dyad ID to each entry
herders.r[, DyadID := ifelse(Ego < Alter, paste(Ego, Alter, sep=""), paste(Alter, Ego, sep=""))]
herders.r$DyadID = as.integer(herders.r$DyadID)  # paste() makes it character; convert to number

# count no. times each dyad appears
herders.r.sum = herders.r %>% group_by(DyadID) %>% summarise(n_appearances=length(DyadID))
single.dyads = herders.r.sum$DyadID[ herders.r.sum$n_appearances<2 ]  # list of ego-alter pairs without corresponding alter-ego (i.e. appears only once)
herders.r.new = herders.r[DyadID %in% single.dyads]                   # get entries that only appear once
setnames(herders.r.new, c("Ego", "Alter"), c("Alter", "Ego"))         # swap ego and alter column names
setcolorder(herders.r.new, names(herders.r))                          # swap column order
herders.r = rbind(herders.r, herders.r.new)                           # append new dyads onto end of relatedness table

#### DEBUG - are any dyads repeated? ##
#herders.r.sum = ddply(herders.r, .(DyadID), summarise, n_appearances=length(DyadID))
#herders.r.sum[ herders.r.sum$n_appearances>2, ]  # do any dyads appear more than twice (too many entries)
####

# remove DyadID - no longer needed
herders.r$DyadID = NULL

# clean up
rm(sibs_sub, kids_sub, largest_id, herders.r.sum, herders.r.new, single.dyads)


############################################################################
## Load dyadic data (or create if file doesn't exist)
##
flog.info("Creating dyadic data...")
source("create dyadic data.r")


############################################################################
## Create social networks
##
flog.info("Creating social networks...")

##
## First, a bit of data wrangling
##
# rename some columns for compatibility with Gephi
setnames(net.maps,   "Type", "LinkType")
setnames(net.maps.o, "Type", "LinkType")
setnames(net.maps.i, "Type", "LinkType")

# make dummy variable in `herders` identifying people who were named in the net-maps
# (these are the subset we should be doing SNA with)
herders$NamedInNetmap = ifelse( herders$HerderID %in% unique(c(net.maps$Ego, net.maps$Alter)), 1, 0 )

# keep only one entry for each Ego-Alter pair, rather than separate entries for each help type
# (this would have the same effect as using igraph::simplify())
coop.net   = unique(subset(net.maps,   select=c(Ego, Alter)))
coop.net.o = unique(subset(net.maps.o, select=c(Ego, Alter)))
coop.net.i = unique(subset(net.maps.i, select=c(Ego, Alter)))

# if we don't know someone's siida membership, assign them to something arbitrary
# this has no effect if we're only working with siida shares, since we know every license owner's siida
herders$SiidaID[ is.na(herders$SiidaID) ] = max(herders$SiidaID, na.rm=T) + 1 

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

##
## net-maps with interviewees' outbound connections only
##
# cooperation network
g.netmap.o = graph.data.frame(coop.net.o, vertices=herders, directed=T)

# make a cooperation network containing only people we interviewed and people they named
# g.netmap.o.sub = graph.data.frame(coop.net.o, 
#                                   vertices = subset(herders, NamedInNetmap==1),
#                                   directed = T)

##
## net-maps with interviewees' inbound connections only
##
# cooperation network
g.netmap.i = graph.data.frame(coop.net.i, vertices=herders, directed=T)

# make a cooperation network containing only people we interviewed and people they named
# g.netmap.i.sub = graph.data.frame(coop.net.i, 
#                                   vertices = subset(herders, NamedInNetmap==1),
#                                   directed = T)

##
## net-maps with everything
##
g.all = graph.data.frame(coop.net, vertices = herders, directed = T)
# g.all.sub = graph.data.frame(coop.net, vertices = subset(herders, NamedInNetmap==1), directed = T)

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


#######################################################################################
## tidy up and save data
##
# keep only subset of herder variables used in these analyses
herders = herders %>% 
  select(HerderID, SiidaID, Interviewed., SiidaShareYN, NamedInNetmap, num.reindeer, SubsistenceTotal,
         NumGifts, CoopGiven.OtherReport, CoopGiven.SelfReport, coop.deg.out)

net.maps = net.maps %>% 
  select(Interviewee:LinkType)

# clean up environment
rm(license.owners.only, keep.30C)
rm(sibs, kids, herd.sizes, herders.r, kin)
rm(survey)

# save processed data
write_csv(herders,      file.path(data.out.dir, "herders.csv"))
write_csv(herders.wide, file.path(data.out.dir, "herders-dyadic-wide.csv"))

write_csv(gifts,    file.path(data.out.dir, "gifts.csv"))
write_csv(net.maps, file.path(data.out.dir, "net-maps.csv"))

flog.info("Finished preparing data")
