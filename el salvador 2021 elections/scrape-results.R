# libraries
library(tidyverse)
library(rvest)
library(janitor)
library(labelled)
library(stringi)



# # setup urls
base = "https://app-div2021-eastus-web-02.azurewebsites.net/asamblea-legislativa/departamental/"
depts = 1:14 # 14 departments in el salvador
wave = 1463 # most recent wave of results
links = paste0(base, depts, "/", wave)


# pull the htmls
results = crossing(links) %>% 
  mutate(results = links %>% map(read_html))

# save results for posterity
write_rds(results, "results/html-results.rds")

# get department names
dept_results = results %>% 
  mutate(departamento = results %>% map(~html_text(html_node(.,".active a")))) %>% 
  unnest(departamento)


# extract tables
muni_results = dept_results %>% 
  select(departamento, results) %>% 
  mutate(tables = results %>% map(html_table)) %>% 
  select(departamento, tables) %>% 
  unnest(cols = tables) %>% 
  unnest()


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


# variable dictionary
dict = tribble(~name, ~label, 
        "departamento", "Department", 
        "municipio", "Municipality", 
        "nuestro_tiempo", "Nuestro Tiempo (political party)", 
        "fmln", "FMLN (political party)", 
        "n", "Nuevas Ideas (political party)", 
        "pdc", "PDC (political party; christian dems)", 
        "vamos", "Vamos (political party)", 
        "pcn", "PCN (political party)", 
        "gana", "Gana (political party)", 
        "cd", "CD (political party)", 
        "coalicion_arena", "A coalition involving ARENA?", 
        "leonardo_bonilla", "No idea; one guy?", 
        "impugnados", "Number of contested ballots", 
        "nulos", "Number of null ballots", 
        "abstenciones", "Number of abstentions", 
        "papeletas_faltantes", "Number of failed ballots", 
        "sobrantes", "Number of remaining ballots", 
        "inutilizadas", "Number of unused ballots", 
        "coalicion_n_gana", "A coalition between NI and Gana?", 
        "arena", "ARENA (political party)", 
        "coalicion_arena_pcn", "A coalition involving ARENA and PCN?", 
        "coalicion_pcn", "A coalition involving PCN?", 
        "jesus_segovia", "No idea; one guy?", 
        )

var_label(muni_clean) = dict$label

# save output
write_rds(muni_clean, "output/clean-votes.rds")
write_csv(muni_clean, "output/clean-votes.csv")



# let's look at the results
muni_clean %>% 
  select(n, fmln, pdc) %>% 
  summarise(across(everything(),~ sum(., na.rm = TRUE)))

