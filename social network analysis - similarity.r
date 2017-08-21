##
## Calculate Jaccard similarities between the advice, help, sharing and gift networks
##
source("init if necessary.r")

library(igraph)
library(proxy)

flog.info("Started social network analysis - similarities")

# make adjacency matrices from graphs of each type of net-map
adj.advice  = get.adjacency( graph.data.frame( net.maps.advice,  vertices=herders, directed=T), sparse=F )
adj.help    = get.adjacency( graph.data.frame( net.maps.help,    vertices=herders, directed=T), sparse=F )
adj.sharing = get.adjacency( graph.data.frame( net.maps.sharing, vertices=herders, directed=T), sparse=F )
adj.gifts   = get.adjacency( graph.data.frame( gifts,            vertices=herders, directed=T), sparse=F )

# shorthand function to calculate Jaccard similarity between adjacency matrices a and b
jaccard = function(a, b) proxy::simil(list(as.vector(a), as.vector(b)), method="jaccard")

jacs = matrix(NA, nrow=4, ncol=4, dimnames=list(c("Advice", "Help", "Sharing", "Gifts"), c("Advice", "Help", "Sharing", "Gifts")))

# calculate similarities
jacs["Advice", "Help"]    = jaccard(adj.advice, adj.help)
jacs["Advice", "Sharing"] = jaccard(adj.advice, adj.sharing)
jacs["Advice", "Gifts"]   = jaccard(adj.advice, adj.gifts)

jacs["Help", "Sharing"] = jaccard(adj.help, adj.sharing)
jacs["Help", "Gifts"]   = jaccard(adj.help, adj.gifts)

jacs["Sharing", "Gifts"] = jaccard(adj.sharing, adj.gifts)

# save table
write.csv(round(jacs, 3), file.path(results.dir, "Network similarities.csv"), row.names=T)

flog.info("Finished society network analysis - similarities")
