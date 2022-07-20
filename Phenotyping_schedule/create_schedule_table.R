# Import data
sched <- read.csv(here::here("./Phenotyping_schedule/Schedule_for_R.csv"))%>%
  dplyr::rename("Trait" = 1) %>%
  dplyr::select(1:3) 

# Load libraries
# source("libraries.R")

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
labs(x="Year Recorded", y="") +
  theme_pubclean() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=16),
        axis.text = element_text(size=14),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        strip.text.y = element_text(
          size = 14,
          face = "bold")) +
  scale_fill_brewer(palette = "Greens")

# save it
png(here::here("./Phenotyping_schedule/Phenotyping_schedule.png"))
print(p)
dev.off()
ggsave(here::here("./Phenotyping_schedule/Phenotyping_schedule.pdf"),
                  width = 4, height = 4)
ggsave(here::here("./Phenotyping_schedule/Phenotyping_schedule_large.pdf"),
                  width = 6, height = 6)
       