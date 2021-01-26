# Setup
library(tidyverse)
library(ggpubr)
library(cowplot)
library(extrafont)
loadfonts(device = "win")

# Load nested dataset (see create_nested_dataset.Rmd) ----------------------------------------------------------------------------
data <- readRDS(file = "nested_data.rds")
usd_citation <- "University of Essex, Institute for Social and Economic Research, NatCen Social Research, Kantar Public (2020)\nUnderstanding Society: Waves 1-10, 2009-2019."

# Orders / parameters / functions ------------------------------------------------------------------------------------------------
make_nice_names <- function(x) {x %>% str_replace_all("_", " ") %>% str_to_sentence() }
primary_modes   <- c("cycle", "public_transport", "car", "walk") %>% make_nice_names()

# Process churn data -------------------------------------------------------------------------------------------------------------
data %>%
  unnest(data) %>%
  filter(!is.na(year),
         !is.na(renewed),
         !commute_status_simp %in% c("missing", "not_in_employment", "other"),
         !commute_status_simp_next %in% c("missing", "not_in_employment")) %>%
  group_by(year_next, commute_status_simp, renewed) %>%
  tally() %>%
  ungroup() %>%
  group_by(year_next, commute_status_simp) %>%
  mutate(churn = (n / sum(n)) * 100,
         commute_status_simp = commute_status_simp %>% make_nice_names(),
         label = case_when(year_next == 2019 ~ commute_status_simp,
                           TRUE ~ as.character(NA))) %>%
  filter(renewed == FALSE,
         year_next < 2019,
         commute_status_simp %in% primary_modes) %>%
  select(-renewed) -> churn

churn %>%
  group_by(commute_status_simp) %>%
  summarise(mean = mean(churn)) -> churn_av

# Process next modes data -------------------------------------------------------------------------------------------------
data %>%
  unnest(data) %>%
  filter(!is.na(year),
         !is.na(renewed),
         !commute_status_simp      %in% c("missing", "not_in_employment", "other"),
         !commute_status_simp_next %in% c("missing", "not_in_employment")) %>%
  group_by(year_next, commute_status_simp, commute_status_simp_next, renewed) %>%
  tally() %>%
  filter(renewed == FALSE,
         commute_status_simp != "work_from_home") %>%
  group_by(year_next, commute_status_simp) %>%
  mutate(prop = (n / sum(n)) * 100) %>%
  group_by(commute_status_simp, commute_status_simp_next) %>%
  summarise(mean = mean(prop)) %>%
  mutate(multiplier               = 100 / sum(mean),
         hundred_mean             = mean * multiplier,
         commute_status_simp_next = commute_status_simp_next %>% make_nice_names(),
         commute_status_simp      = commute_status_simp  %>% make_nice_names(),
         facet_title = paste0("Former ", commute_status_simp %>% str_to_lower(), " users")) %>%
  ungroup() -> next_modes

# Set styles ------------------------------------------------------------------------------------------------------------
l       <- "grey98"
d       <- "grey15"
b       <- 20
s       <- 13
t       <- 10
width   <- 195
font    <- "Segoe UI"
palette <- setNames(RColorBrewer::brewer.pal(7, "Dark2"),
                    c(unique(next_modes$commute_status_simp_next), "Other"))

# Copy --------------------------------------------------------------------------------------------------------------------
title_main <- "Churn among users of different commuting modes"

intro_para <- paste(
              paste("Longitudinal data from the UK-wide Understanding Society survey suggests that low",
                    "carbon commuting modes have a user retention problem, when compared with car commuting.", sep = " ") %>% str_wrap(width = width),
              paste("Commuting decisions were derived from the annual responses given by members of the survey panel.",
                    "A churn metric was then applied. This finds the proportion of commuters in one",
                    "year who switched to a different mode the following year. Panel members who left the labour market,",
                    "or did not respond to the next questionnaire, were excluded.", sep = " ") %>% str_wrap(width = width),
              paste("For example, of 434 cycle commuters in 2017 who were still in work the following year, 311 continued cycling.",
                    "123 switched to another mode. This equates to 28% cycling \"churn\" in 2018.", sep = " ") %>% str_wrap(width = width),
                     sep = "\n\n")

follow_up_text <- paste("A note on variation between years. Trends in the churn rate for may be related to the changing",
                        "characteristics of the survey panel over time, rather than changes in people's experience",
                        "of different modes. For example, the average age of the panel was 42.5 in 2010 and 44.2 2018.",
                        "More work on this will be forthcoming.", sep = " ") %>% str_wrap(width = width)

credits <- paste(
           paste("Data source:", usd_citation, sep = " ") %>% str_wrap(width = width),
           paste("Author: Christopher C Brown (@chrisb_go).",
                 "Reproducible code at github.com/ccb2n19/usd_commuting_exploration.", sep = " ") %>% str_wrap(width = width), sep = "\n")

# Set theme --------------------------------------------------------------------------------------------------------------
cb_theme <- theme_minimal(base_family = font) +
            theme(plot.title      = element_text(hjust = 0, face = "bold", size = b+2, colour = d),
                  plot.subtitle   = element_text(hjust = 0, face = "plain", size = s, colour = d, margin = margin(0, 0, 1.5, 0, unit = "cm")),
                  plot.caption    = element_text(hjust = 0, face = "plain", size = s, colour = d, margin = margin(1, 0, 0, 0, unit = "cm")),
                  plot.margin     = margin(1.5, 1.5, 1.5, 1.5, unit = "cm"),
                  plot.background = element_rect(fill = l, colour = "transparent"),
                  strip.text      = element_text(hjust = 0, face = "bold", size = b,   colour = d),
                  axis.text       = element_text(size = t, colour = d),
                  axis.title      = element_text(size = s, colour = d))

# Create churn plot ------------------------------------------------------------------------------------------------------
ggplot(data = churn, aes(x = year_next, y = churn, colour = commute_status_simp)) +
  geom_point(alpha = 0.8) +
  scale_x_continuous(breaks       = seq(churn$year_next %>% min(), churn$year_next %>% max(), 2),
                     minor_breaks = FALSE,
                     limits       = c(churn$year_next %>% min(), churn$year_next %>% max() + 3)) +
  lims(y = c(0, 45)) +
  scale_colour_manual(values = palette, guide = FALSE) +
## Draw line at mean ----------------
  geom_segment(data = churn_av, 
               x    = 2010, 
               xend = 2020, 
               aes(y      = mean, 
                   yend   = mean, 
                   colour = commute_status_simp)) +
## Draw mean labels -----------------
  geom_label(data = churn_av, aes(label = mean %>% signif(3) %>% paste0("%"), y = mean), x = 2020, size = 5) +
  facet_wrap(~commute_status_simp,
             nrow = 1) +
  labs(x        = "",
       y        = "Churn rate (%)",
       title    = title_main,
       subtitle = intro_para,
       caption  = paste(follow_up_text, credits, sep = "\n\n")) +
  cb_theme -> churn_plot

ggsave("churn_plot.png",
       churn_plot,
       width  = 1024 / 55,
       height = 512 / 55)

# Create next steps plot --------------------------------------------------------------------------------------------------
ggplot(data = next_modes %>%
         mutate(commute_status_simp_next = fct_inorder(commute_status_simp_next)), aes(x = commute_status_simp, y = hundred_mean)) +
  geom_col(aes(fill = commute_status_simp_next), show.legend = TRUE) +
  scale_fill_manual(values = palette,
                    guide  = guide_legend(reverse = TRUE)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  facet_wrap(~facet_title,
             scales = "free_y",
             ncol   = 1) +
  labs(title = "Which commuting modes do people shift to?",
       subtitle = "Next modes chosen by people who switched. Average across 2009 to 2018 cohorts.",
       x     = "",
       y     = "% of people who switch",
       caption = credits,
       fill  = "") +
  cb_theme +
  theme(axis.text.y = element_blank()) -> next_step_plot

ggsave("next_step_plot.png",
       next_step_plot,
       width  = 1024 / 55,
       height = 512 / 55)
