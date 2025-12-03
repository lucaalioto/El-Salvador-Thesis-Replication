# libraries
library(tidyverse)
library(janitor)
library(labelled)
library(here)


# read results
muni = read_csv(here("output", "clean-municipal.csv"))
votes = read_csv(here("output", "clean-votes.csv"))


# create merger key
votes$key = paste0(votes$departamento, "-", votes$municipio) %>% str_remove_all(" ")
muni$key = paste0(muni$departamento, "-", muni$municipio) %>% str_remove_all(" ")

# which don't match?
votes$key[!votes$key %in% muni$key]


# clean up
votes$key[votes$key == "sansalvador-ciudaddelgado"] = "sansalvador-delgado"
votes$key[votes$key == "sanmiguel-sanantoniodelmosco"] = "sanmiguel-sanantonio"
votes$key[votes$key == "sonsonate-nahuilingo"] = "sonsonate-nahulingo"
votes$key[votes$key == "launion-sanjoselasfuentes"] = "launion-sanjose"
votes$key[votes$key == "chalatenango-sanjosecancasque"] = "chalatenango-cancasque"

# which don't match?
votes$key[!votes$key %in% muni$key]


# merge
votes = votes %>% 
  left_join(select(muni, -departamento, -municipio), by = "key") %>% 
  select(-key)


# add variable descriptions
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
               "densidad", "Populationd density (ppl per sq km?)",
               "percent_urbano", "% urban population", 
               "masculinidad", "Male index (# of men per 100 women", 
               "relacion_dependencia", "Dependency ratio (non-working age / working age)",
               "x60_anos_y_mas", "Percent over 60", 
               "tgf", "Fertility rate (avg. number of kids per woman)", 
               "tmi", "Infant mortality rate (number of deaths per 1,000 live births)", 
               "tasa_analfabetismo", "Illiteracy rate", 
               "asistencia_escolar", "School assistance rates (ages?)", 
               "agua_potable", "% access potable water", 
               "electricidad", "% access electricity", 
               "sin_servicio_sanitario", "% without sanitation", 
               "con_piso_de_tierra", "% with dirt floor"
)


var_label(votes) = dict$label

# save output
write_rds(votes, "output/clean-merged.rds")



# clean up more for class
votes = votes %>% 
  select(-abstenciones, -papeletas_faltantes, -sobrantes, -inutilizadas) %>% 
  # convert missing votes to 0
  mutate(across(nuestro_tiempo:jesus_segovia, replace_na, 0)) %>% 
  mutate(nuestro_tiempo = round(nuestro_tiempo))


# merge votes into fewer parties
votes = votes %>% 
  mutate(n = n + coalicion_n_gana, 
         arena = arena + coalicion_arena + coalicion_arena_pcn) %>% 
  select(-coalicion_n_gana, -coalicion_arena, -coalicion_arena_pcn)




# spit out
write_csv(votes, "output/clean-merged.csv")



# get dictionary
make_dictionary = function(data, vars)
{
  # subset to variables you want
  subset = select(data, one_of(vars))
  
  # extract labels
  labels = subset %>% map_chr(~attributes(.)$label)
  var_dict = tibble(original = names(subset),
                    labels = labels)
  return(var_dict)
}

# get library
var_dict =
  make_dictionary(votes, vars = names(votes))


