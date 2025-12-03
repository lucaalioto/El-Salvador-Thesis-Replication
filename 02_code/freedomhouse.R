library(tidyverse)
library(readxl)
library(patchwork)
library(dplyr)
library(ggplot2)
library(tidyr)

freedom = read_xlsx("All_data_FIW_2013-2024.xlsx", sheet = "FIW13-25")


colnames(freedom) <- as.character(freedom[1, ])
freedom <- freedom[-1, ]


salvador <- freedom %>%
  filter(`Country/Territory` == "El Salvador") %>%
  mutate(
    Edition = as.integer(Edition),   # removes decimal point
    Total = as.numeric(Total)
  )

ggplot(salvador, aes(x = Edition, y = Total)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  # Vertical line for Bukele's 2019 election
  geom_vline(xintercept = 2019, linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = 2019, y = max(salvador$Total, na.rm = TRUE),
           label = "Bukele elected", vjust = -0.5, angle = 90, size = 3.5) +
  
  labs(
    title = "El Salvador Freedom House Total Score Over Time",
    x = "Year",
    y = "Total Score"
  ) +
  scale_x_continuous(breaks = unique(salvador$Edition)) +
  scale_y_continuous(limits = c(40, 80)) +
  theme_classic()

ggsave("el_salvador_freedom_score.png", width = 8, height = 5)

# Parts of Sum (A–G)

salvador_parts <- freedom %>%
  filter(`Country/Territory` == "El Salvador") %>%
  mutate(Edition = as.integer(Edition)) %>%
  select(Edition, A, B, C, D, E, F, G) %>%
  mutate(across(A:G, as.numeric)) %>%
  pivot_longer(
    cols = A:G,
    names_to = "part",
    values_to = "score"
  ) %>%
  mutate(
    part_label = dplyr::recode(
      part,
      "A" = "A - Electoral Process",
      "B" = "B - Political Pluralism and Participation",
      "C" = "C - Functioning of Government",
      "D" = "D - Freedom of Expression and Belief",
      "E" = "E - Associational and Organizational Rights",
      "F" = "F - Rule of Law",
      "G" = "G - Personal Autonomy and Individual Rights"
    )
  )

ggplot(salvador_parts, aes(x = Edition, y = score, color = part_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2019, linetype = "dashed", linewidth = 0.8) +
  annotate(
    "text",
    x = 2019,
    y = 15,  # stay within 0–16
    label = "Bukele elected",
    vjust = -0.5,
    angle = 90,
    size = 3.5
  ) +
  scale_y_continuous(limits = c(0, 16)) +
  scale_x_continuous(breaks = unique(salvador_parts$Edition)) +
  labs(
    title = "El Salvador – Freedom House Subscores by Category (A–G)",
    x = "Year (Edition)",
    y = "Score",
    color = "Category"
  ) +
  theme_classic() +
  theme(legend.position = "right")


ggsave("el_salvador_freedom_subscores_AG.png", width = 10, height = 6)


decline_table <- freedom %>%
  filter(`Country/Territory` == "El Salvador",
         Edition %in% c(2018, 2025)) %>%
  mutate(Edition = as.integer(Edition)) %>%
  select(Edition, A, B, C, D, E, F, G) %>%
  mutate(across(A:G, ~ as.numeric(.))) %>%   # force numeric
  pivot_longer(cols = A:G, names_to = "category", values_to = "score") %>%
  pivot_wider(names_from = Edition, values_from = score, names_prefix = "yr_") %>%
  mutate(
    yr_2018 = as.numeric(yr_2018),
    yr_2025 = as.numeric(yr_2025),
    decline_raw = yr_2018 - yr_2025,
    pct_decline = (decline_raw / yr_2018) * 100
  ) %>%
  arrange(desc(pct_decline))

library(gt)

library(gt)

decline_table %>%
  mutate(
    category = recode(category,
                      "A" = "A – Electoral Process",
                      "B" = "B – Political Pluralism & Participation",
                      "C" = "C – Functioning of Government",
                      "D" = "D – Freedom of Expression & Belief",
                      "E" = "E – Associational & Organizational Rights",
                      "F" = "F – Rule of Law",
                      "G" = "G – Personal Autonomy & Individual Rights"
    ),
    pct_decline = round(pct_decline, 1)
  ) %>%
  gt() %>%
  tab_header(
    title = "Percentage Decline in Freedom House Scores, 2018 → 2025"
  ) %>%
  cols_label(
    category = "Category",
    yr_2018 = "Score (2018)",
    yr_2025 = "Score (2025)",
    pct_decline = "% Decline"
  )



# A - Political Rights

# Filter for El Salvador and keep A1–A3
salvador_A <- freedom %>%
  filter(`Country/Territory` == "El Salvador") %>%
  mutate(Edition = as.integer(Edition)) %>%
  select(Edition, A1, A2, A3) %>%
  mutate(across(A1:A3, as.numeric)) %>%
  pivot_longer(
    cols = A1:A3,
    names_to = "indicator",
    values_to = "score"
  )

plota = ggplot(salvador_A, aes(x = Edition, y = score, color = indicator)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 4)) +
  scale_x_continuous(breaks = unique(salvador_A$Edition)) +
  labs(
    title = "El Salvador – Electoral Process (A1–A3) Over Time",
    x = "Year (Edition)",
    y = "Score (0–4 per item)",
    color = "Indicator"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")


# B: Political Pluralism and Participation

salvador_B <- freedom %>%
  filter(`Country/Territory` == "El Salvador") %>%
  mutate(Edition = as.integer(Edition)) %>%
  select(Edition, B1, B2, B3, B4) %>%    # <- B items here
  mutate(across(B1:B4, as.numeric)) %>%
  pivot_longer(
    cols = B1:B4,
    names_to = "indicator",
    values_to = "score"
  )

plotb = ggplot(salvador_B, aes(x = Edition, y = score, color = indicator)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 4)) +
  scale_x_continuous(breaks = unique(salvador_B$Edition)) +
  labs(
    title = "El Salvador – Political Pluralism & Participation (B1–B4) Over Time",
    x = "Year (Edition)",
    y = "Score (0–4 per item)",
    color = "Indicator"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

plota / plotb

ggsave("el_salvador_freedom_subscores.png", width = 10, height = 8)
