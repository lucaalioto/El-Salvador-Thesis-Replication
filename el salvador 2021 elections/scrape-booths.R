# libraries
library(tidyverse)
library(rvest)
library(janitor)
library(stringi)
library(furrr)

# make a plan
plan(multisession)

# # setup urls
base = "https://app-div2021-eastus-web-02.azurewebsites.net/asamblea-legislativa/municipal/"
depts = 1:14 # 14 departments in el salvador
munis = 1:40 # varies, but is about 40 max
wave = 1463 # most recent wave of results

# grid of urls
urls = expand_grid(base, depts, munis, wave) %>% 
  mutate(links = paste0(base, depts, "/", munis, "/", wave))


# scrape all the URLs
results = 
  urls %>% 
  mutate(htmls = future_map(links, possibly(read_html, otherwise = NA)))


# get muni names and department names
clean_results = results %>% 
  filter(htmls != "NA") %>% 
  mutate(departamento = htmls %>% map_chr(~html_text(html_node(., ".active+ .breadcrumb-item a")))) %>% 
  mutate(municipio = htmls %>% map_chr(~html_text(html_node(., ".active~ .active a"))))


# extract tables
booth_results = clean_results %>% 
  select(departamento, municipio, htmls) %>% 
  mutate(tables = htmls %>% map(html_table)) %>% 
  unnest(cols = tables) %>% 
  unnest()


for(ii in 1:nrow(clean_results))
{
  possibly(html_table(clean_results$htmls[[ii]]), otherwise = NULL)
  print(ii)
}


# clean up data
muni_clean = muni_results %>% 
  clean_names() %>% 
  mutate(municipio = str_remove(municipio, "\\[PREF\\] "), 
         municipio = tolower(stri_trans_general(municipio, "Latin-ASCII")), 
         departamento = tolower(stri_trans_general(departamento, "Latin-ASCII"))) %>% 
  select(-cod_munic)


# weirdly, vote tallies have fractional votes? round them 
muni_clean = muni_clean %>% 
  mutate(across(fmln:jesus_segovia, round, 0))