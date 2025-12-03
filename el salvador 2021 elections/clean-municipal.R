# libraries
library(tidyverse)
library(janitor)
library(labelled)
library(here)
library(readxl)
library(stringr)

# read data
# from here: http://www.digestyc.gob.sv/index.php/temas/des/poblacion-y-estadisticas-demograficas/censo-de-poblacion-y-vivienda/poblacion-censos.html
df = read_excel(here("municipal-data", "07_Indicadores_Sociodemograficos.xlsx"), 
                sheet = "Municipios") %>% 
  clean_names() %>% 
  drop_na(municipio)

# separate out department names and get rid of number
df = df %>% 
  mutate(departamento = ifelse(str_detect(municipio, "[0-9]"), municipio, NA)) %>% 
  select(departamento, municipio, everything()) %>% 
  fill(departamento, .direction = "down") %>% 
  mutate(departamento = str_remove(departamento, ".*-")) %>% 
  filter(!str_detect(municipio, "[0-9]"))


# clean up names to remove all accents
df = df %>% 
  mutate(departamento = tolower(stri_trans_general(departamento, "Latin-ASCII")), 
         municipio = tolower(stri_trans_general(municipio, "Latin-ASCII")))                  

# spit it out
write_csv(df, here("output", "clean-municipal.csv"))
