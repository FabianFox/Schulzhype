# Polling data wrangling

# Load/install packages
# ---------------------------------------------------------------------------- #
if(!require("pacman")) install.packages("pacman")
p_load(tidyverse, lubridate)

# Load data created in "scrape_raw_wahlrecht.R"
# ---------------------------------------------------------------------------- #
polls.df <- readRDS("./data/polls.rds") %>%
  as_tibble()

# Data wrangling
# ---------------------------------------------------------------------------- #
# Transform "Zeitpunkt" into a datetime (time)
election.ts <- polls.df %>%
  mutate(time = ymd(Zeitpunkt)) %>%
  select(-Zeitpunkt)

election.grp.ts <- election.ts %>%
  mutate(week = round_date(time, unit = "week")) %>%
  group_by(week, party) %>%
  add_count() %>%
  mutate(mean_vote_week = mean(vote))

# Exploratory plot on the way
ggplot(election.grp.ts) +
  geom_line(aes(x = week, y = mean_vote_week, color = party), size = .75) +
  scale_color_manual(values = c("#009EE0", "#000000", "#FFED00", "#64A12D",
                                "#BE3075", "#EB001F"),
                     guide = guide_legend(title = NULL)) +
  labs(x = "", y = "", 
       title = "Sonntagsfrage: Wenn am nächsten Sonntag Bundestagswahl wäre...",
       Quelle = "Wahlrecht.de (Forsa)") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 10),
        axis.ticks = element_line(size = .5))
