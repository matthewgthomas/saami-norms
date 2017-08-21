source("init if necessary.r")

flog.info("Plotting distribution of gifts")

ggplot(gifts, aes(x=factor(round(GiftSize, 2)))) +
  geom_bar() +
  
  xlab("Gift size (litres of petrol)") +
  ylab("Count") +
  common_theme

for (ext in c("png", "pdf"))
  ggsave(file.path(plots.dir, paste0("distribution of gifts.", ext)), height=100, width=150, units="mm")
