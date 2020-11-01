#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤#
########### BIOMASS #####################
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤#

library(tidyverse)
library(readxl)


######## IMPORT AND CLEAN DATA #########

biomass <- read_xlsx('Data/VII_Biomass_aboveground.xlsx') %>%
  filter(comment != 'topplag', #this came from part of the plot that should not be harvested
         !(str_detect(group, 'Bryophyte project'))) %>% #does not belong in this dataset
  mutate(plot = ifelse(bag == '1121', '15.1.',
                ifelse(plot == '14.3. (?)', '14.3.',
                ifelse(plot == '6.1.(?)', '6.1.', plot))),
         year = ifelse(site == 'LYG' & plot == '8.3.' & year == '2018' & dry_mass == '426.79', '2019', 
                       ifelse(site == 'BUO' & year == '2018', '2019', year)),
         year = as.factor(year),
         main_group = fct_recode(group, 'Dead' = 'Dead_Calluna', 
                                   'Dead' = 'Dead_graminoids',
                                   'Dead' = 'Dead_stuck',
                                   'Dead' = 'Dead_Eriophorum',
                                   'Ericales' = 'Calluna',
                                   'Ericales' = 'Damaged_Calluna',
                                   'Ericales' = 'mixed Calluna',
                                   'Ericales' = 'Mix',
                                   'Forbs' = 'Forb',
                                   #'Litter' = 'litter',
                                   'Litter' = 'mixed Litter', 
                                   #'Graminoids' = 'Graminoids_green',
                                   'Wood' = 'Tree'),
         life_group = fct_recode(group, 'dead' = 'Dead_Calluna', 
                                 'dead' = 'Dead_graminoids', 
                                 'dead' = 'Dead_stuck', 
                                 'new' = 'Graminoids',
                                 'new' = 'Forb', 
                                 'standing' = 'Damaged_Calluna', 
                                 'standing' = 'Calluna', 
                                 'standing' = 'Ericales', 
                                 'standing' = 'Fern', 
                                 'dead' = 'Dead_Eriophorum', 
                                 'Litter' = 'mixed Litter', 
                                 'Wood' = 'Tree'))


######## EXPORT DATA #########

writexl::write_xlsx(biomass, path = 'Cleandata/VII_Plant_Biomass_Aboveground.xlsx')

