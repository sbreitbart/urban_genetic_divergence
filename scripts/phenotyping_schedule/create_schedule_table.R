# Import data
sched <- read.csv(here::here("./Figures_Tables/Phenotyping_schedule/Schedule_for_R.csv"))%>%
  dplyr::rename("Trait" = 1) %>%
  dplyr::select(1:3) 

# Load libraries
# source("libraries.R")


# to find number of plants alive at end of growing season each year:
survival %>%
  dplyr::filter(Dead == 0) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(alive = n())


library(ggtext)
# Create table showing when traits were recorded
p <- ggplot(sched,
       aes(x = as.factor(Year),
           y = forcats::fct_rev(Trait)))  +
  facet_grid(vars(Category),
             scales = "free_y",
             switch = "y")+
  geom_tile(aes(fill = Category),
            color = "black",
            height = 1,
            width = 1) +
  labs(x="Year Recorded\n\n",
       y="",
       subtitle = "Plants with ramets alive at end of growing season:           865            812            692           551     ",
       caption = expression(
         paste(italic("*Aphis asclepiadis, Aphis nerii, Danaus plexippus, Euchaetes egle, Labidomera clivicollis, Liriomyza asclepiadis, \nLygaeus kalmii, Myzocallis asclepiadis, Rhyssomatus lineaticollis, Tetraopes tetropthalmus\n\n**D. plexippus, L. clivicollis, L. asclepiadis")))) +
  theme_pubr() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=14),
        axis.text = element_text(size=12),
        axis.text.y = element_markdown(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        strip.text.y = element_text(
          size = 12,
          face = "bold"),
        plot.caption=element_text(size=10),
        plot.caption.position = "plot",
        plot.subtitle = element_text(color = "gray35",
                                     hjust = 1,
                                     size = 11,
                                     face = "italic")) +
  scale_fill_brewer(palette = "Greens")
p

# save it
png(here::here("./Figures_Tables/Phenotyping_schedule/Phenotyping_schedule.png"),
    width = 18, height = 19,
    units = "cm",
    res = 500)
print(p)
dev.off()
ggsave(here::here("./Figures_Tables/Phenotyping_schedule/Phenotyping_schedule.pdf"),
                  width = 7, height = 7)
       