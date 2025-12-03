library(tidyverse)
library(ggcorrplot)

# read data
df = read_csv("output/clean-merged.csv")


# theme
theme_fancy <- function() {
  theme_minimal(base_family = "Asap Condensed") +
    theme(panel.grid.minor = element_blank())
}


# calculate total
shares = df %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(nuestro_tiempo:jesus_segovia))) %>% 
  mutate(n_share = n/total, 
         fmln_share = fmln/total, 
         arena_share = arena/total)


# demographics
shares %>% 
  select(densidad, percent_urbano, tmi, agua_potable, electricidad, 
         sin_servicio_sanitario, con_piso_de_tierra) %>% 
  pivot_longer(everything(), names_to = "var", values_to = "value") %>% 
  ggplot(aes(x = value, group = var)) +
  geom_histogram(color = "white", fill = "#CD3333", alpha = .8) + 
  facet_wrap(vars(var), scales = "free") + 
  theme_bw()


# corrplot
shares %>% 
  select(densidad, percent_urbano, tmi, agua_potable, electricidad, 
         sin_servicio_sanitario, con_piso_de_tierra) %>% 
  drop_na() %>% 
  cor() %>% 
  ggcorrplot(hc.order = TRUE)


# party shares
shares %>% 
  select(contains("share")) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, group = name)) +
  geom_histogram(color = "white", fill = "#CD3333", alpha = .8) + 
  facet_wrap(vars(name), scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(labels = scales::percent)


# ridge plot
shares %>% 
  select(contains("share")) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(y = name, x = value, fill = name)) + 
  theme_fancy() + 
  theme(legend.position = "top", 
        axis.text.y = element_blank(), 
        text = element_text(family = "Asap Condensed"),
        plot.title = element_text(face = "bold",
                                  size = rel(1.5)), 
        legend.text = element_text(size = rel(1.25))) + 
  scale_x_continuous(labels = scales::percent) + 
  stat_density_ridges(quantile_lines = TRUE, size = 1,
                      quantiles = 2, scale = 3, color = "white") + 
  scale_fill_manual(values = c("#0288b7", "#a90010", "#35274A")) + 
  labs(title = "El Salvador's Legislative Elections", 
       subtitle = "Municipal vote share of top three parties.", 
       x = "Percent of the vote won", y = NULL)

# relationships
shares %>% 
  select(n_share, densidad, percent_urbano, tmi, 
         agua_potable, electricidad, 
         sin_servicio_sanitario, con_piso_de_tierra) %>% 
  drop_na() %>% 
  cor() %>% 
  ggcorrplot(hc.order = TRUE)


ggplot(shares, aes(x = electricidad, y = n_share)) + geom_point() + 
  geom_smooth(method = "lm") + 
  theme_fancy() + 
  scale_y_continuous(labels = scales::percent)

ggplot(shares, aes(x = tmi, y = n_share)) + geom_point() + 
  geom_smooth(method = "lm") + 
  theme_fancy() + 
  scale_y_continuous(labels = scales::percent)

shares %>% 
  select(fmln_share, densidad, percent_urbano, tmi, 
         agua_potable, electricidad, 
         sin_servicio_sanitario, con_piso_de_tierra) %>% 
  drop_na() %>% 
  cor() %>% 
  ggcorrplot(hc.order = TRUE)


ggplot(shares, aes(x = electricidad, y = fmln_share)) + geom_point() + 
  geom_smooth() + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)

ggplot(shares, aes(x = tmi, y = fmln_share)) + geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)


# difference in shares
shares %>% 
  mutate(diff = n_share - (fmln_share + arena_share)) %>% 
  ggplot(aes(x = diff)) + 
  geom_histogram(color = "white", fill = "#35274A", alpha = .8) + theme_fancy() + 
  labs(x = "Nuevas Ideas votes - (FLMN + ARENA)", 
       title = "Nuevas Ideas vs. The Rest") + 
  scale_x_continuous(labels = scales::percent) + 
  theme(text = element_text(family = "Asap Condensed"),
        plot.title = element_text(face = "bold",
                                  size = rel(1.5)))



# difference
shares %>% 
  mutate(diff = n_share - (fmln_share + arena_share)) %>% 
  ggplot(aes(x = tmi, y = diff)) + geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)


shares %>% 
  mutate(diff = n_share - (fmln_share + arena_share)) %>% 
  select(diff, densidad, percent_urbano, tmi, 
         agua_potable, electricidad, 
         sin_servicio_sanitario, con_piso_de_tierra) %>% 
  drop_na() %>% 
  cor() %>% 
  ggcorrplot(hc.order = TRUE)
