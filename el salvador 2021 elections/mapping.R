library(sf)
library(stringi)
library(tidyverse)
library(hrbrthemes)
library(paletteer)

library(tmap)
library(rmapshaper)
library(tigris)

df = read_sf("el salvador 2021 elections/gadm-shapefiles/slv_admbnda_adm2_gadm_20210204.shp") 
results = read_rds("finalproject/01_data/clean-merged.rds") # election data from 2021
violence = read_csv("finalproject/01_data/ACLED Data_2025-11-20.csv")


# calculate percentage vote share
results = results %>% 
  # replace NA with 0 for now
  mutate(across(everything(), replace_na, 0)) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(c(n:leonardo_bonilla, coalicion_n_gana:jesus_segovia)), 
                     na.rm = TRUE)) %>% 
  mutate(n_share = (n+coalicion_n_gana)/total*100, 
         arena_share = (arena + coalicion_arena + coalicion_arena_pcn)/total*100, 
         fmln_share = fmln/total*100)


map = df %>% 
  select(name = ADM2_ES, geometry) %>% 
  mutate(name = tolower(stri_trans_general(name, "Latin-ASCII"))) %>% 
  filter(!str_detect(name, "lago|embalse"))

# clean up unmatched names
map$name[!map$name %in% results$municipio]

# san jose las flores = las flores
# nueva san salvador = santa tecla
# opico = san juan opico
# san jose = San José La Fuente
# san antonio = san antonio del monte
# delgado = ciudad delgado
# nahuilingo
# mercedes umaña
map$name[map$name == "san jose las flores"] = "las flores"
map$name[map$name == "nueva san salvador"] = "santa tecla"
map$name[map$name == "opico"] = "san juan opico"
map$name[map$name == "san jose"] = "san jose las fuentes"
map$name[map$name == "san antonio"] = "san antonio del monte"
map$name[map$name == "delgado"] = "ciudad delgado"
map$name[map$name == "nahulingo"] = "nahuilingo"
map$name[map$name == "mercedes umana"] = "mercedes umaña"


# merge in
merged = left_join(map, results, by = c("name" = "municipio"))

# lets filter up to 2021 election 2/18/2021. Current format 2018-01-02
violence = violence %>%
  filter(event_type == "Violence against civilians" | 
           !is.na(civilian_targeting))  %>%
  mutate(date = as.Date(event_date)) %>%
  filter(date <= as.Date("2021-02-18"))


# make violence sf with merged datum
violence.sf <- st_as_sf(violence, coords = c("longitude", "latitude"),
                        crs = st_crs(merged))

# Counting the number of ADUs pertract
violence_agg <- aggregate(violence.sf["event_id_cnty"], 
                          merged, 
                          FUN = "length") %>%
  mutate(event_id_cnty = replace_na(event_id_cnty,0))

#Putting info with Sac Tract Data 
merged_violence <- merged %>%
  mutate(violent_event = violence_agg$event_id_cnty)

sum(merged_violence$violent_event) # seems to be some issues with agg

# drop geo / save as rds
st_drop_geometry(merged_violence) %>% 
  write_rds("finalproject/01_data/clean-merged-with-votes.rds")

# plot
ggplot(merged, aes(fill = n_share)) + geom_sf() + 
  #scale_fill_viridis_c(option = "magma", direction = -1) +
  #scale_fill_gradient(low = "white", high = "#52854C") + 
  scale_fill_paletteer_c("grDevices::Oranges", direction = -1) + 
  theme_ipsum(grid = F) + 
  labs(title = "Legislative Performance of Bukele's Nuevas Ideas", 
       subtitle = "February 2021 parliamentary elections.",
       caption = "Source: https://app-div2021-eastus-web-02.azurewebsites.net/asamblea-legislativa",
       fill = "Percent of votes cast:") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) + 
  #geom_sf_label(data = filter(merged, big == "yes"), aes(label = name)) + 
  NULL

ggplot(merged, aes(fill = arena_share)) + geom_sf() + 
  scale_fill_paletteer_c("grDevices::Oranges", direction = -1) + 
  #scale_fill_gradient(low = "white", high = "blue") + 
  hrbrthemes::theme_ipsum(grid = F) + 
  labs(title = "Legislative Performance of Bukele's Nuevas Ideas", 
       subtitle = "February 2021 parliamentary elections.",
       caption = "Source: https://app-div2021-eastus-web-02.azurewebsites.net/asamblea-legislativa",
       fill = "Percent of votes cast:") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) + 
  #geom_sf_label(data = filter(merged, big == "yes"), aes(label = name)) + 
  NULL


## side by side plots
merged %>% 
  select(n_share, arena_share, fmln_share, geometry) %>% 
  pivot_longer(cols = c(n_share:fmln_share), 
               names_to = "party", values_to = "share") %>% 
  ggplot(aes(fill = share)) + geom_sf(aes(geometry = geometry)) + 
  facet_wrap(vars(party)) +
  scale_fill_paletteer_c("grDevices::Oranges", direction = -1) + 
  #scale_fill_gradient(low = "white", high = "blue") + 
  theme_ipsum(grid = F) + 
  labs(title = "Legislative Performance of Bukele's Nuevas Ideas", 
       subtitle = "February 2021 parliamentary elections.",
       caption = "Source: https://app-div2021-eastus-web-02.azurewebsites.net/asamblea-legislativa",
       fill = "Percent of votes cast:") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) + 
  #geom_sf_label(data = filter(merged, big == "yes"), aes(label = name)) + 
  NULL


tmap_mode("view")

merged_violence %>%
  tm_shape(unit = "mi") +
  tm_polygons(
    col = "violent_event",
    style = "quantile",
    palette = "Reds",
    legend.show = TRUE,
    legend.title = ""
  ) +
  tm_shape(violence.sf) +  
  tm_dots()



# with leaflet
library(leaflet)

# colors
bins <- c(0, 10, 20, 50, 100, Inf)
pal <- colorBin("YlOrRd", domain = merged$n_share, bins = bins)
labels <- sprintf("<strong>%s</strong><br/>%g percent of votes",
                  merged$name, merged$n_share) %>% lapply(htmltools::HTML)


m = leaflet(merged) %>% 
  addPolygons(
    fillColor = ~pal(n_share),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) 
m
