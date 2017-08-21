#' ---
#' title: Social relations model results
#' output: word_document
#' ---

#+ echo=F
# This code should be run after running `social network analysis - reciprocity.r` and `social relations model - fit.r`
flog.info("Spinning results text")

#+ echo=F
siida_links = coop.net %>% 
  left_join(herders %>% select(HerderID, SiidaID), by=c("Ego" = "HerderID")) %>%    # get siida IDs for ego
  rename(Ego.SiidaID = SiidaID) %>% 
  left_join(herders %>% select(HerderID, SiidaID), by=c("Alter" = "HerderID")) %>%  # get siida IDs for alter
  rename(Alter.SiidaID = SiidaID) %>% 
  mutate(SameSiida = as.integer(Ego.SiidaID == Alter.SiidaID)) %>%                  # who belongs to the same siida?
  summarise(n_links = sum(SameSiida) / n())                                         # what proportion of links are within-siida?

winnings = gifts %>% 
  group_by(Alter) %>% 
  summarise(Winnings = sum(GiftSize))

#' We interviewed `r length(interviewee_list)` herders from a single district in Finnmark, `r nrow(subset(herders, SiidaShareYN==1 & Interviewed.==1))` of whom were license owners. `r length(unique(gifts$Ego))` of the `r nrow(subset(herders, SiidaShareYN==1 & Interviewed.==1))` license owners played the gift game, giving `r nrow(gifts)` gifts to `r length(unique(gifts$Alter))` people (see Figure S1 for distribution of gifts).
#' 
#' 27 license owners drew cooperative networks during their interviews. A number of participants mentioned groups or categories of people (e.g. "elders in the winter siida" or "members of neighbouring siidas") in their networks, which were subsequently dropped from the quantitative analysis of individuals presented here. After removing all entities in the networks that were not license owners, we were left with `r length(unique(net.maps$Interviewee))` networks naming `r length(unique(c(net.maps$Ego, net.maps$Alter)))` license owners, `r round(siida_links$n_links * 100, 1)`% of whom belonging to the same siida as the interviewee.
#' 
#' The median amount given was `r round(median(gifts$GiftSize), 2)` litres of petrol. The median amount received in total was `r round(median(winnings$Winnings), 2)` litres; the largest amount received by any one herder was `r round(max(winnings$Winnings), 2)` litres.
#' 
#' # Social network analysis

#+ echo=F
##
## reciprocity among people we interviewed
##
# keep only egos and alters we interviewed, and where ego was the interviewee
net.maps.interviewed = net.maps %>% 
  filter(Ego == Interviewee & Ego %in% interviewee_list & Alter %in% interviewee_list) %>% 
  filter(LinkType %in% c("Advice", "Help", "Items")) %>% 
  dplyr::select(Ego, Alter, LinkType)

net_mapped_ids = unique(c(net.maps.interviewed$Ego, net.maps.interviewed$Alter))

interviewees = herders %>% 
  filter(HerderID %in% net_mapped_ids) %>% 
  dplyr::select(HerderID, SiidaID)

# make the network
g.interviewed = simplify( graph.data.frame(net.maps.interviewed, vertices=interviewees, directed=T) )

#' Taking the subset of cooperation networks containing only egos and alters we interviewed, `r round(reciprocity(g.interviewed) * 100, 1)`% of links were reciprocated

#+ echo=F
obs.recip_range = obs.recip %>% filter(Network != "Gifts") %>% summarise(min=min(reciprocity), max=max(reciprocity)) %>% round(., 3)
obs.recip_assort = obs.recip %>% summarise(min=min(assortment), max=max(assortment)) %>% round(., 3)
obs.recip_gifts = round( as.numeric( obs.recip %>% filter(Network == "Gifts") %>% select(reciprocity) ), 3)

#' Reciprocity---the proportion of two-way ties---was slightly higher in the three self-reported cooperation networks (range: `r obs.recip_range$min` - `r obs.recip_range$max`) than reciprocity in the gift network (`r obs.recip_gifts`; Figure 2a). There was strong assortment with members of the same siida in all four networks (range: `r obs.recip_assort$min` - `r obs.recip_assort$max`; Figure ...).
#' 
#' # Social relations model

#+ echo=F
or = exp(log_odds)
or_sum = t( apply(or, 2, function(x) quantile(x, c(0.5, 0.025, 0.975))) )

#' Members of the same herding group were more likely to be named in cooperation networks (OR = `r round( or_sum[4,1], 3 )`; 95% credible interval [`r round( or_sum[4,2], 3 )`, `r round( or_sum[4,3], 3 )`]
#' 
#' Over half of the variance in ties in the cooperation network was explained by particular relationships (`r round( median(vpc_d) * 100, 2)` &plusmn; `r round( sd(vpc_d) * 100, 2)`%), whereas givers explained `r round( median(vpc_g) * 100, 2)`% (&plusmn; `r round( sd(vpc_g) * 100, 2)`%) and receivers explained `r round( median(vpc_r) * 100, 2)`% (&plusmn; `r round( sd(vpc_r) * 100, 2)`%) of the variance.

#+ echo=F
flog.info("Finished spinning results text")
