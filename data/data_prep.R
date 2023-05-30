# https://github.com/Tychobra/shiny_crud

library(dplyr)
library(tidyr)
library(tibble)
library(xml2)

# Make DwC Events table
DwC_events <- read_xml("https://rs.gbif.org/core/dwc_event_2022-02-02.xml") %>% 
  xml_children() %>% 
  purrr::map_df(function(x) {
    list(name = xml_attr(x, "name"),
         description = xml_attr(x, "description"))
  }) %>% 
  mutate(row_id = character())
  
