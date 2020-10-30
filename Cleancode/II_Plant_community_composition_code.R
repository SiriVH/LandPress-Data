#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤#
########### PLANT COMMUNITY #####################
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤#

library(tidyverse)
library(readxl)


######## IMPORT DATA #########

plant.com.raw <- read_xlsx('Data/comdata.xlsx', sheet = 'Sheet1') # PLANT COMMUNITY DATA

species.corr <- read_xlsx('Data/comdata.xlsx', sheet = 'Sheet2') # CORRECT NAMES



######## CLEAN DATA #########

community <- plant.com.raw %>% 
  left_join(species.corr, by= 'species') %>% 
  mutate(species = coalesce(correct_name, species)) %>%  #jumps to second if first in NA, keep first if it is not NA
  select(-correct_name) %>% 
  mutate(plot = if_else(site == 'BUO' & year == '2016', #BUO had wrong plot numbers in 2016. Fix that
                        true = recode(plot,   
                                      '10.1.' = '11.1.',
                                      '10.2.' = '10.1.',
                                      '10.3.' = '12.1.',
                                      '11.1.' = '10.3.',
                                      '11.2.' = '10.2.',
                                      '11.3.' = '11.2.',
                                      '12.1.' = '12.2.',
                                      '12.2.' = '11.3.',
                                      '12.3.' = '12.3.'
                        ), false = plot)) %>% 
  mutate(plot = str_replace(plot, pattern = "$" , replacement =  "."),       # This fix the plot number issue on TOR 
         plot = str_replace(plot, pattern = "\\.\\.", replacement = ".")) %>% 
  mutate(new.species = coalesce(ifelse(site == 'BUO' & (species == 'Agrostis_canina' | species == 'Agrostis_capillaris' | species == 'Agrostis_sp'), 'Agrostis_capillaris',
                                ifelse(site == 'BUO' & (species == 'Betula_nana' | species == 'Betula_sp'), 'Betula_pubescens',
                                ifelse(site == 'BUO' & species == 'Vicia_sp', 'Vicia_cracca', 
                                ifelse(site == 'BUO' & (species == 'Polygala_serpyllifolia' | species == 'Polygala_sp'), 'Polygala_vulgaris', 
                                ifelse(site == 'BUO' & (species == 'Galium_saxatile' | species == 'Galium_sp'), 'Galium_boreale', 
                                ifelse(site == 'LYG' & plot == '7.1.' & species == 'Carex_nigra', 'Carex_echinata', 
                                ifelse(site == 'LYG' & plot == '1.1.' & species == 'Festuca_sp' & year == '2017', 'Festuca_vivipara',
                                ifelse(site == 'LYG' & (plot == '1.2.' | plot == '3.1.') & species == 'Festuca_rubra' & year == '2019', 'Festuca_vivipara',
                                ifelse(site == 'LYG'  & species == 'Luzula_sylvatica' & year == '2019', 'Luzula_pilosa',
                                ifelse(site == 'LYG' & plot == '2.2.' & species == 'Hylocomium_splendens' & year == '2018', 'Hypnum_sp',
                                ifelse(site == 'LYG' & plot == '2.3.' & species == 'Luzula_pilosa' & year == '2018', 'Luzula_multiflora',
                                ifelse(site == 'LYG' & plot == '3.2.' & species == 'Carex_sp' & year == '2019', 'Carex_pilulifera',
                                ifelse(site == 'LYG' & (plot == '4.3.' | plot == '6.2.' | plot == '6.3.') & species == 'Carex_sp' & year == '2016', 'Carex_pilulifera',
                                ifelse(site == 'LYG' & plot == '3.2.' & species == 'Lotus_corniculatus' & year == '2018', 'Galium_saxatile',
                                ifelse(site == 'LYG' & plot == '3.3.' & species == 'Luzula_multiflora_cf' & year == '2019', 'Luzula_multiflora',
                                ifelse(site == 'LYG' & plot == '5.1.' & species == 'Carex_binervis' & year == '2018','Eriophorum_angustifolium',  
                                ifelse(site == 'LYG' & plot == '5.2.' & species == 'Carex_sp' & year == '2017','Eriophorum_angustifolium',
                                ifelse(site == 'LYG' & plot == '7.3.' & species == 'Eriophorum_vaginatum' & (year == '2017' | year == '2016'),'Eriophorum_angustifolium',  
                                ifelse(site == 'LYG' & plot == '9.2.' & species == 'Eriophorum_vaginatum' & year == '2019','Eriophorum_angustifolium',
                                ifelse(site == 'LYG' & plot == '5.2.' & species == 'Carex_flava' & year == '2016','Carex_pilulifera',
                                ifelse(site == 'LYG' & (plot == '5.2.' | plot == '5.3.') & species == 'Carex_nigra' & year == '2016', 'Carex_panicea',
                                ifelse(site == 'LYG' & (plot == '5.3.' | plot == '6.1.') & species == 'Carex_echinata' & year == '2016', 'Carex_pilulifera',
                                ifelse(site == 'LYG' & (plot == '5.2.' | plot == '5.3.') & species == 'Juncus_sp' & year == '2016', 'Juncus_squarrosus',
                                ifelse(site == 'LYG' & plot == '7.1.' & (species == 'Carex_binervis' | species == 'Carex_pilulifera') & year == '2017', 'Carex_echinata',
                                ifelse(site == 'NOV' & (plot == '20.9.' &  species == 'Rhytidiadelphus_loreus' & year == '2018'), 'Rhytidiadelphus_squarrosus',
                                species ))))))))))))))))))))))))))) %>% 
  mutate (species = new.species)  %>% 
  group_by(site, 
           plot, 
           year, 
           month,
           day,
           species, 
           group, 
           zone, 
           life_cycle, 
           red_list, 
           leaf, 
           freq1,
           freq2,
           freq3,
           freq4,
           freq5,
           freq6,
           freq7,
           freq8,
           freq9,
           freq10,
           freq11,
           freq12,
           freq13,
           freq14,
           freq15,
           freq16) %>% 
  summarise(cover = sum(as.numeric(cover, na.rm = TRUE)))



######## EXPORT DATA #########

writexl::write_xlsx(community, path = 'Cleandata/II_Plant_community_composition.xlsx')

