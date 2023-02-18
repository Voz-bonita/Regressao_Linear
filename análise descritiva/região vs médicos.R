source("main.r")
ggplot(train_df) +
  aes(x = `Região geográfica`, y = `Médicos`) +
  geom_boxplot(fill = c("black"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
#  ylim(NA, 1000) +
  labs(x = "Região geográfica", y = "Médicos") +
  theme_bw()
#ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")