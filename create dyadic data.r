##
## Create dyadic data frame for all dyads of license owners ('siida shares')
## - for each pair, we'll know how they're related, whether one gave the other a gift (and how much), and whether they belong to the same siida
##
library(data.table)

covars = c("SiidaID")


#########################################################################
## Data wrangling
##
# convert dataframes to data tables and set keys
herders    = as.data.table(herders)
herders.r  = as.data.table(herders.r)
gifts      = as.data.table(gifts)

setkey(herders,    HerderID)
setkey(herders.r,  Ego, Alter)
setkey(gifts,      Ego, Alter)

# keep only siida shares and chop out unnecessary covars
herders2 = subset(herders, SiidaShareYN==1, select=c("HerderID", covars))


#########################################################################
## Make main dyadic data frame in wide format
##
herders.wide = data.table(expand.grid(Ego=herders2$HerderID, Alter=herders2$HerderID))
setkeyv(herders.wide, c("Ego", "Alter"))
herders.wide = herders.wide[Ego!=Alter]  # remove dyads where ego==alter


#########################################################################
## Set dyad IDs for each ego/alter pair
##
# set dyad ID to be the smallest of ego/alter followed by the largest
herders.wide[, DyadID := ifelse(Ego < Alter, paste(Ego, Alter, sep=""), paste(Alter, Ego, sep=""))]
herders.wide$DyadID = as.integer(herders.wide$DyadID)  # paste() makes it character; convert to number


#########################################################################
## Merge in gifts
##
herders.wide = gifts[herders.wide]
setkeyv(herders.wide, c("Ego", "Alter"))  # need to reset keys after merge

# remove NAs
herders.wide[is.na(GiftSize), GiftSize := 0]

# create binary response variable for gifts
herders.wide[, GiftGiven := 0]
herders.wide[GiftSize > 0, GiftGiven := 1]

# figure out whether ego also received a gift from ego
gift.received = subset(herders.wide, GiftGiven==1, select=c(Ego, Alter, GiftGiven))  # keep only instances where alters received gifts from egos
setnames(gift.received, c("Alter", "Ego", "GiftReceived"))                           # alters become egos (because we want to know who received gifts from whom)
setkey(gift.received, Ego, Alter)
herders.wide = gift.received[herders.wide]            # merge gifts received into main table
herders.wide[is.na(GiftReceived), GiftReceived := 0]  # remove NAs


#########################################################################
## Merge in relatedness
##
# merge coefficients of relatedness
herders.wide = herders.r[herders.wide]
# set NAs to zero
herders.wide[is.na(r), r := 0]


#########################################################################
## Merge in covariates for ego and alter
##
setkey(herders.wide, Ego)
herders.wide = herders2[herders.wide]
# repeat for alter
setkey(herders.wide, Alter)
herders.wide = herders2[herders.wide]

# sort out column names and keys
setnames(herders.wide, "HerderID", "Alter")
setnames(herders.wide, "i.HerderID", "Ego")

setnames(herders.wide, covars, paste("Alter", covars, sep="."))
setnames(herders.wide, paste("i", covars, sep="."), paste("Ego", covars, sep="."))

setkey(herders.wide, Ego, Alter)

# calculate within-dyad covariates
## same siida?
herders.wide[, SameSiida := 0]
herders.wide[Ego.SiidaID==Alter.SiidaID, SameSiida := 1]  # belong to same siida?


#########################################################################
## Sort out column order and save dyadic data file
##
# reorder the columns into something more sensible
setcolorder(herders.wide, c("DyadID", "Ego", paste("Ego", covars, sep="."),
                            "Alter", paste("Alter", covars, sep="."),
                            "r", "SameSiida", "GiftSize", "GiftGiven", "GiftReceived"
))
setkey(herders.wide, DyadID)

rm(covars, herders2, gift.received)
