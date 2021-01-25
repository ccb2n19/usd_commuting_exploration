# Load nested dataset (see create_nested_dataset.Rmd) ----------------------------------------------------------------------------
data <- readRDS(file = "nested_data.rds")
usd_citation <- "University of Essex, Institute for Social and Economic Research, NatCen Social Research, Kantar Public (2020)\nUnderstanding Society: Waves 1-10, 2009-2019."

# Process data -------------------------------------------------------------------------------------------------------------------
churn <- data %>%
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
         commute_status_simp = commute_status_simp %>% str_replace_all("_", " ") %>% str_to_sentence(),
         label = case_when(year_next == 2019 ~ commute_status_simp,
                           TRUE ~ as.character(NA))) %>%
  filter(renewed == FALSE,
         year_next < 2019) %>%
  select(-renewed)

# Get means -----------------------------------------------------------------------------------------------------------------------
churn_av <- churn %>%
  group_by(commute_status_simp) %>%
  summarise(mean = mean(churn))

# Set colour palette --------------------------------------------------------------------------------------------------------------
palette <- RColorBrewer::brewer.pal(5, "Dark2")
l <- "grey98"
d <- "grey15"
b <- 20
s <- 13
t <- 10
width <- 170

# Title and paragraph -------------------------------------------------------------------------------------------------------------
title <- "User churn for different commuting modes"
paragraphs <- paste(
  paste("Longitudinal data from the UK-wide Understanding Society survey suggests that low",
        "carbon commuting modes have a user retention problem, when compared with car commuting.",sep = " ") %>% str_wrap(width = width),
  paste("Commuting decisions were derived from the annual responses given by members of the survey panel.",
        "A churn metric was then applied. This finds the proportion of commuters in one",
        "year who switched to a different mode the following year. Panel members who left the labour market,",
        "or did not respond to the next questionnaire, were excluded.",            sep = " ") %>% str_wrap(width = width),
  paste("For example, of 434 cycle commuters in 2017 who were still in work the following year, 311 continued cycling.",
        "123 switched to another mode. This equates to 28% cycling \"churn\" in 2018.",           sep = " ") %>% str_wrap(width = width),
  sep = "\n\n")
caption <- paste(
  paste("A note on variation between years. Trends in the churn rate for may be related to the changing",
        "characteristics of the survey panel over time, rather than changes in people's experience",
        "of different modes. For example, the average age of the panel was 42.5 in 2010 and 44.2 2018.",
        "More work on this will be forthcoming.",                                                    sep = " ") %>% str_wrap(width = width),
  paste("Data source:", usd_citation, "\n",           sep = " ") %>% str_wrap(width = width),
  "Author: Christopher C Brown (@chrisb_key)",
  sep = "\n\n")

# Plot -----------------------------------------------------------------------------------------------------------------------------
ggplot(data = churn, aes(x = year_next, y = churn, colour = commute_status_simp)) +
  geom_point(alpha = 0.8) +
  scale_x_continuous(breaks = seq(churn$year_next %>% min(), churn$year_next %>% max(), 2),
                     minor_breaks = FALSE,
                     limits = c(churn$year_next %>% min(), churn$year_next %>% max() + 3)) +
  lims(y = c(0, 45)) +
  scale_colour_manual(values = palette, guide = FALSE) +
  geom_segment(data = churn_av, x = 2010, xend = 2020, aes(y = mean, yend = mean, colour = commute_status_simp)) +
  geom_label(data = churn_av, aes(label = mean %>% signif(3) %>% paste0("%"), y = mean), x = 2020, size = 5) +
  facet_wrap(~commute_status_simp,
             nrow = 1) +
  labs(x        = "",
       y        = "Churn rate (%)",
       title    = title,
       subtitle = paragraphs,
       caption = caption) +
  theme_minimal(base_family = "Segoe UI") +
  theme(strip.text      = element_text(hjust = 0, face = "bold", size = b, colour = d),
        plot.title      = element_text(hjust = 0, face = "bold", size = b, colour = d, margin = margin(0,0.8,0.5,0.8, unit = "cm")),
        plot.subtitle   = element_text(hjust = 0, size = s, colour = d,                margin = margin(0, 0, 0.5, 0,    unit = "cm")),
        plot.caption    = element_text(hjust = 0, size = s, colour = d,                margin = margin(0.8, 0, 0, 0,    unit = "cm")),
        plot.background = element_rect(fill = l, colour = "transparent"),
        plot.margin     =                                                               margin(1, 1, 1, 1, unit = "cm"),
        axis.text = element_text(size = t, colour = d),
        axis.title = element_text(size = t, colour = d)) -> churn_plot

ggsave("churn_plot.png",
       churn_plot,
       width = 1200 / 30,
       height = 627 / 30,
       units = "cm")