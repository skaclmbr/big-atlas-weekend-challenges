# 20220606
# Big Atlas Weekend Challenge Winners

# eBird provides a snapshot of atlas data at the start of the event, including
# a list of current atlasers. Grouped checklists should not be merged.

# Projects involved in this event include Maine, New York, MD-DC, 
# North Carolina, Ontario, Newfoundland.

## 2023 challenges
### Individual:
# Nocturnal coverage
## Complete nocturnal checklists/observer (any block)
### Nocturnal: eBird definition; only start time is used

# Newly coded or species that have had their category upgraded per 
# priority block
## Number of newly coded/upgraded species compared to pre-event per 
## priority block per user.

# Checklists with breeding codes from incomplete priority blocks
## Complete checklists with codes per incomplete block per person

### Inter-project
## Nocturnal coverage
### Complete nocturnal checklists/observer (checklists/atlasers)

## New/upgraded species
### Total number of new/upgraded species (codes/atlasers)

## Coded checklists from incomplete blocks
### Complete coded checklists from incomplete blocks/atlaser

here::i_am("scripts/big-atlas-weekend/big-atlas-weekend-challenges.R")

library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(auk)
library(sf)
library(purrr)

# year of event
year <- 2023

moveup <- "../../"

# project_codes
ny <- "EBIRD_ATL_NY"
md <- "EBIRD_ATL_MD_DC"
nc <- "EBIRD_ATL_NC"
pr <- "EBIRD_CB"

# Data Import -----------------------------------------------------------------
# there are multiple sets of data; 1) the ebd for the duration of each project;
# 2) the ebd format from the last ebd date to the event, delivered early by 
# ian; 3) the block effort summary for each region, delivered early by ian; 
# 4) the event observations data from ian; 5) the event checklist data from 
# ian.

# read in ebd data for each region
cols <- c("group_identifier",
          "sampling_event_identifier",
          "scientific_name",
          "observation_count",
          "atlas_block",
          "project_code",
          "state_code",
          "common_name",
          "breeding_category",
          "observer_id")

### MARYLAND
ebdmd <- auk_ebd(here(moveup, "data", "ebird", "1_raw",
                      "ebd_US-MD_202001_202306_relMay-2023.txt")) %>%
  auk_project("EBIRD_ATL_MD_DC") %>%
  auk_filter(here(moveup, "data", "big atlas weekend", "2023",
                  "prebaw_ebdmd.txt"),
             keep = cols) %>%
  read_ebd(rollup = TRUE, unique = FALSE)

# ebdmd_spp <- ebdmd %>%
#   group_by(project_code,
#            state_code,
#            observer_id,
#            atlas_block) %>%
#   filter(breeding_category %in% c("C2", "C3", "C4")) %>%
#   distinct(common_name) %>%
#   ungroup()
# 
# ebdmd_blocks <- ebdmd %>%
#   group_by(project_code,
#            state_code,
#            observer_id) %>%
#   distinct(atlas_block) %>%
#   ungroup()
# 
# rm(ebdmd)
# 
# ### DISTRICT
ebddc <- auk_ebd(here(moveup, "data", "ebird", "1_raw",
                      "ebd_US-DC_202001_202306_relMay-2023.txt")) %>%
  auk_project("EBIRD_ATL_MD_DC") %>%
  auk_filter(here(moveup, "data", "big atlas weekend", "2023",
                  "prebaw_ebddc.txt"),
             keep = cols) %>%
  read_ebd(rollup = TRUE, unique = FALSE)
# 
# ebddc_spp <- ebddc %>%
#   group_by(project_code,
#            state_code,
#            observer_id,
#            atlas_block) %>%
#   filter(breeding_category %in% c("C2", "C3", "C4")) %>%
#   distinct(common_name) %>%
#   ungroup()
# 
# ebddc_blocks <- ebddc %>%
#   group_by(project_code,
#            state_code,
#            observer_id) %>%
#   distinct(atlas_block) %>%
#   ungroup()
# 
# rm(ebddc)
# 
# ### PUERTO RICO
ebdpr <- auk_ebd(here(moveup, "data", "big atlas weekend", "2023",
                      "ebd_PR_202101_202306_relMay-2023.txt")) %>%
  auk_filter(here(moveup, "data", "big atlas weekend", "2023",
                  "prebaw_ebdpr.txt"),
             keep = c(cols, "latitude", "longitude")) %>%
  read_ebd(rollup = TRUE, unique = FALSE)

ebd_junpr <- read.csv(here(moveup, "data", "big atlas weekend", "2023",
                           "Big Atlas Weekend 2023 PR data 20230626.csv"))

ebd_junpr <- ebd_junpr %>%
  select(sampling_event_identifier = sub_id,
         common_name = primary_com_name,
         project_code = proj_id,
         breeding_category = category_code,
         latitude,
         longitude,
         datetime = to_char,
         observer_id = user_id,
         duration_hrs,
         all_obs_reported,
         atlas_block = block,
         state_code = state,
         is_nocturnal = nocturnal)

ebdpr <- bind_rows(ebdpr, ebd_junpr)

pr_blocks <- read_sf(here(moveup, "data", "big atlas weekend", "2023",
                          "GAP project hexagons.kml")) %>%
  rename(atlas_block = Name)

ebdpr$atlas_block <- NULL

hexes <- st_intersects(st_as_sf(ebdpr,
                                coords = c("longitude", "latitude"),
                                crs = 4326), pr_blocks) 

ind <- which(lapply(hexes, length) != 1)

hexes <- lapply(hexes, function(x) ifelse(is_empty(x), NA, x))

hexes <- unlist(hexes)

ebdpr$hex_block <- st_drop_geometry(pr_blocks[hexes, "atlas_block"])

ebdpr <- ebdpr %>%
  unnest(hex_block)

# ebdme_spp <- ebdme %>%
#   group_by(project_code,
#            state_code,
#            observer_id,
#            atlas_block) %>%
#   filter(breeding_category %in% c("C2", "C3", "C4")) %>%
#   distinct(common_name) %>%
#   ungroup()
# 
# ebdme_blocks <- ebdme %>%
#   group_by(project_code,
#            state_code,
#            observer_id) %>%
#   distinct(atlas_block) %>%
#   ungroup()
# 
# rm(ebdme)
# 
# ### NORTH CAROLINA
ebdnc <- auk_ebd(here(moveup, "data", "big atlas weekend", "2023",
                      "ebd_US-NC_202101_202306_relMay-2023.txt")) %>%
  auk_project("EBIRD_ATL_NC") %>%
  auk_filter(here(moveup, "data", "big atlas weekend", "2023",
                  "prebaw_ebdnc.txt"),
             keep = cols) %>%
  read_ebd(rollup = TRUE, unique = FALSE)

# ebdnc_spp <- ebdnc %>%
#   group_by(project_code,
#            state_code,
#            observer_id,
#            atlas_block) %>%
#   filter(breeding_category %in% c("C2", "C3", "C4")) %>%
#   distinct(common_name) %>%
#   ungroup()
# 
# ebdnc_blocks <- ebdnc %>%
#   group_by(project_code,
#            state_code,
#            observer_id) %>%
#   distinct(atlas_block) %>%
#   ungroup()
# 
# rm(ebdnc)
# 
# ### NEW YORK
ebdny <- auk_ebd(here(moveup, "data", "big atlas weekend", "2023",
                      "ebd_US-NY_202001_202306_relMay-2023.txt")) %>%
  auk_project("EBIRD_ATL_NY") %>%
  auk_filter(here(moveup, "data", "big atlas weekend", "2023",
                  "prebaw_ebdny.txt"),
             keep = cols) %>%
  read_ebd(rollup = TRUE, unique = FALSE)

# ebdny_spp <- ebdny %>%
#   group_by(project_code,
#            state_code,
#            observer_id,
#            atlas_block) %>%
#   filter(breeding_category %in% c("C2", "C3", "C4")) %>%
#   distinct(common_name) %>%
#   ungroup()
# 
# ebdny_blocks <- ebdny %>%
#   group_by(project_code,
#            state_code,
#            observer_id) %>%
#   distinct(atlas_block) %>%
#   ungroup()
# 
# rm(ebdny)
# 
# ## combine regional datasets
ebd <- bind_rows(ebddc, ebdmd, ebdnc, ebdny, ebdpr)

rm(ebddc, ebdmd, ebdnc, ebdny, ebdpr)

ebd_jun <- read.csv(here(moveup, "data", "big atlas weekend", "2023",
                         "Big Atlas Weekend 2023 all data 20230626.csv"))

# ebd_junpr <- read.csv(here(moveup, "data", "big atlas weekend", "2023",
#                            "Big Atlas Weekend 2023 PR data 20230626.csv"))
# 
# ebd_jun <- bind_rows(ebd_jun, ebd_junpr)
# 
# rm(ebd_junpr)

ebd_jun <- ebd_jun %>%
  select(sampling_event_identifier = sub_id,
         common_name = primary_com_name,
         project_code = proj_id,
         breeding_category = category_code,
         latitude,
         longitude,
         datetime = to_char,
         observer_id = user_id,
         duration_hrs,
         all_obs_reported,
         atlas_block = block,
         state_code = state,
         is_nocturnal = nocturnal)

ebd <- full_join(ebd, ebd_jun)

rm(ebd_jun)

write.csv(ebd, here(moveup, "data", "big atlas weekend", "2023",
                    "prebaw_2023.csv"), row.names = FALSE)

ebd <- read.csv(here(moveup, "data", "big atlas weekend", "2023",
                     "prebaw_2023.csv"))

details <- file.info(list.files(here(moveup, "data", "big atlas weekend", 
                                     "2023"), 
                                pattern = "tsv+", 
                                full.names = TRUE))

# sort by most recently created and keep the most recent five, since eBird
# only sends five downloads each month.
details <- head(details[order(details$ctime, decreasing = TRUE),], n = 8)

# get friendly names from the files
spcl <- row.names(details) %>%
  str_extract("(?<=2023/)(.+)(?=-2023)") %>%
  str_to_lower() %>%
  str_replace_all(c(" " = "_", "-" = "_"))

# read in the data
for(i in 1:nrow(details)) {
  assign(spcl[i],
         read.delim(row.names(details)[i], quote = ""))
}

ls(pattern = "effort|zero")

# effort <- bind_rows(maine_bba_effort,
#                     maryland_dc_bba_effort,
#                     new_york_bba_effort,
#                     north_carolina_bba_effort)

zero_species <- bind_rows(maryland_dc_bba_zero_species_checklists,
                          new_york_bba_zero_species_checklists,
                          north_carolina_bba_zero_species_checklists)

rm(new_york_bba_zero_species_checklists,
   maryland_dc_bba_zero_species_checklists,
   north_carolina_bba_zero_species_checklists)

# effort <- effort %>%
#   # remove the commas in the numbers so it can be classed as numeric
#   mutate(across(contains("hours"), ~ str_remove_all(., ",")),
#          across(contains("hours"), as.numeric)) 
# 
# # remove the start year from the end of the project ids.
# effort$proj_period_id <- str_remove(effort$proj_period_id, "(_[0-9]+)")

zero_species <- zero_species %>%
  mutate(nocturnal = ifelse(nocturnal == "t", TRUE, FALSE),
         all_species_reported = ifelse(all_species_reported == 1, TRUE, FALSE),
         observation_date = mdy_hms(observation_date)) %>%
  rename(datetime = observation_date,
         all_obs_reported = all_species_reported,
         is_nocturnal = nocturnal) %>%
  select(-c(num_observers,
            protocol_code,
            effort_distance_km))

rm(details, spcl)

## read in event data

## this only contains coded species!
# baw <- read.csv(here("data", "ebird", "1_raw",
#                      "2022 Big Atlas Weekend observations 20220627.csv")) %>%
#   rename(datetime = to_char) %>%
#   mutate(datetime = as_datetime(datetime),
#          nocturnal = ifelse(nocturnal == 1, TRUE, FALSE),
#          all_obs_reported = ifelse(all_obs_reported == 1, TRUE, FALSE),
#          user_id = str_replace_all(user_id, "USER", "obsr"))

ebd <- mutate(ebd, datetime = as_datetime(datetime))

ebd <- full_join(ebd, zero_species)

baw <- ebd %>%
  mutate(is_nocturnal = ifelse(is_nocturnal == 1, TRUE, FALSE),
         all_obs_reported = ifelse(all_obs_reported == 1, TRUE, FALSE)) %>%
  filter(datetime >= as_datetime("2023-06-23 18:00:00") &
           datetime <= as_datetime("2023-06-25 23:59:59"))


prebaw <- ebd %>%
  mutate(is_nocturnal = ifelse(is_nocturnal == 1, TRUE, FALSE),
         all_obs_reported = ifelse(all_obs_reported == 1, TRUE, FALSE)) %>%
  filter(datetime < as_datetime("2023-06-23 18:00:00"))

  

# prebawpr <- prebaw %>%
#   filter(grepl("PR-", state_code))
# 
# pr_blocks <- read_sf(here(moveup, "data", "big atlas weekend", "2023",
#                           "GAP project hexagons.kml")) %>%
#   rename(atlas_block = Name)
# 
# hexes <- st_intersects(st_as_sf(prebawpr,
#                                 coords = c("longitude", "latitude"),
#                                 crs = 4326), pr_blocks) 
# 
# ind <- which(lapply(hexes, length) != 1)
# 
# hexes <- lapply(hexes, function(x) ifelse(is_empty(x), NA, x))
# 
# hexes <- unlist(hexes)
# 
# prebawpr$hex_block <- st_drop_geometry(pr_blocks[hexes, "atlas_block"])
# 
# prebawpr <- prebawpr %>%
#   select(-atlas_block) %>%
#   unnest(hex_block)
# 
# prebaw <- prebaw %>%
#   filter(!grepl("PR-", state_code))
# 
# prebaw <- full_join(prebaw, prebawpr)

############################################################################

prebaw_spp <- prebaw %>%
  filter(breeding_category %in% c("C2", "C3", "C4") &
           (state_code %in% c("US-DC", "US-MD", "US-NC", "US-NY") | 
              grepl("PR-", state_code))) %>%
  mutate(breeding_category = factor(breeding_category, ordered = TRUE,
                                    levels = c("C2", "C3", "C4"))) %>%
  group_by(atlas_block,
           common_name) %>%
  filter(breeding_category %in% max(breeding_category)) %>%
  ungroup() %>%
  distinct(project_code, state_code, atlas_block, 
           common_name, breeding_category)

baw_spp <- baw %>%
  filter(breeding_category %in% c("C2", "C3", "C4") &
           (state_code %in% c("US-DC", "US-MD", "US-NC", "US-NY") | 
              grepl("PR-", state_code))) %>%
  mutate(breeding_category = factor(breeding_category, ordered = TRUE,
                                    levels = c("C2", "C3", "C4"))) %>%
  group_by(atlas_block,
           common_name) %>%
  filter(breeding_category %in% max(breeding_category)) %>%
  ungroup() %>%
  distinct(project_code, state_code, atlas_block, 
           common_name, breeding_category)

# prebaw_spp <- bind_rows(ebdmd_spp, ebddc_spp, ebdme_spp, ebdnc_spp, ebdny_spp)
# 
# write.csv(prebaw_spp, here("data", "ebird", "3_filtered",
#                            "prebaw_unique-block-spp.csv"),
#           row.names = FALSE)
# 
# rm(ebdmd_spp, ebddc_spp, ebdme_spp, ebdnc_spp, ebdny_spp)
# 
# prebaw_blocks <- bind_rows(ebdmd_blocks, ebddc_blocks, ebdme_blocks,
#                            ebdnc_blocks, ebdny_blocks)
# 
# write.csv(prebaw_blocks, here("data", "ebird", "3_filtered",
#                               "prebaw_unique-atlaser-blocks.csv"),
#           row.names = FALSE)
# 
# rm(ebdmd_blocks, ebddc_blocks, ebdme_blocks,
#    ebdnc_blocks, ebdny_blocks)

# prebaw_spp <- read.csv(here("data", "ebird", "3_filtered", 
#                             "prebaw_unique-block-spp.csv"))
# 
# prebaw_blocks <- read.csv(here("data", "ebird", "3_filtered",
#                                "prebaw_unique-atlaser-blocks.csv"))
#   
# ## read in early ebd download from ian for each region
# early_ebd <- read.csv(here("data", "ebird", "1_raw",
#                            "1-24 June 2022 BAW state atlas obs  20220627.csv"))

baw[, "observer_id"] <- str_replace(baw[, "observer_id"], "USER", "obsr")

# early_ebd_spp <- early_ebd %>%
#   filter(category_code %in% c("C2", "C3", "C4")) %>%
#   select(common_name = primary_com_name,
#          state_code = state,
#          atlas_block = block, 
#          observer_id = user_id,
#          project_code = proj_id) %>%
#   group_by(project_code,
#            state_code,
#            observer_id,
#            atlas_block) %>%
#   distinct(common_name) %>%
#   ungroup()
# 
# prebaw_spp <- rbind(prebaw_spp, early_ebd_spp)
# 
# early_ebd_blocks <- early_ebd %>%
#   select(state_code = state,
#          atlas_block = block,
#          observer_id = user_id,
#          project_code = proj_id) %>%
#   group_by(project_code,
#            state_code,
#            observer_id) %>%
#   distinct(atlas_block) %>%
#   ungroup()
# 
# prebaw_blocks <- rbind(prebaw_blocks, early_ebd_blocks)
# 
# rm(early_ebd, early_ebd_spp, early_ebd_blocks)

## read in other pertinent datasets
### priority blocks from each region
# priority_blocks <- read.csv(here("data", "ebird", "2_standardized",
#                                  paste0("priority_blocks.csv")))

## complete blocks
comp_md <- read.csv(here(moveup, "data", "bba3", "complete_blocks.csv"))
comp_ny <- read.csv(here(moveup, "data", "big atlas weekend", "2023",
                         "NY Block Status.csv"))
comp_nc <- read.csv(here(moveup, "data", "big atlas weekend", "2023", 
                         "NC_blocks.csv"))

comp_md <- comp_md %>%
  select(atlas_block) %>%
  mutate(state_code = "US-MD")

comp_ny <- comp_ny %>%
  filter(eBird_status == "complete") %>%
  select(atlas_block) %>%
  mutate(state_code = "US-NY")

comp_nc <- comp_nc %>%
  filter(ID_EBD_NAME %in% c("Boone SE",
                            "Concord SE",
                            "Green Level SE",
                            "Southeast Durham SE",
                            "Cary SE",
                            "Bayleaf SE",
                            "Wake Forest SE",
                            "Raleigh East SE",
                            "Rolesville SE",
                            "Knightdale SE",
                            "Bunn West SE",
                            "Zebulon SE",
                            "Rivermont SE",
                            "Pollocksville SE",
                            "Upper Broad Creek SE",
                            "Arapahoe SE",
                            "Oriental CW",
                            "Vandemere SE")) %>%
  select(atlas_block = ID_BLOCK_CODE) %>%
  mutate(state_code = "US-NC")

complete <- bind_rows(comp_md, comp_ny, comp_nc)

rm(comp_md, comp_nc, comp_ny)
  

# priority_blocks <- priority_blocks %>%
#   mutate(priority = ifelse(priority == "yes", TRUE, FALSE),
#          state = case_when(
#            state == "Maryland-DC" ~ "US-MD",
#            state == "Maine" ~ "US-ME",
#            state == "North Carolina" ~ "US-NC",
#            state == "New York" ~ "US-NY"
#          ),
#          # add in DC as a state
#          state = case_when(
#            atlas_block %in% c("38077G1CE",
#                               "38077G1NE",
#                               "38076G8NW",
#                               "38076H8CW",
#                               "38076H8SW",
#                               "38077H1CE",
#                               "38077H1CW",
#                               "38077H1NE",
#                               "38077H1SE",
#                               "38077H1SW") ~ "US-DC",
#            TRUE ~ state
#          )) 



## read in pre-event effort data
## make sure zero-species are included, since a lot of nocturnal checklists
## have zero species.

# details <- file.info(list.files(here(moveup, "data", "big atlas weekend", 
#                                      "2023"), 
#                                 pattern = "tsv+", 
#                                 full.names = TRUE))
# 
# # sort by most recently created and keep the most recent five, since eBird
# # only sends five downloads each month.
# details <- head(details[order(details$ctime, decreasing = TRUE),], n = 8)
# 
# # get friendly names from the files
# spcl <- row.names(details) %>%
#   str_extract("(?<=2023/)(.+)(?=-2023)") %>%
#   str_to_lower() %>%
#   str_replace_all(c(" " = "_", "-" = "_"))
# 
# # read in the data
# for(i in 1:nrow(details)) {
#   assign(spcl[i],
#          read.delim(row.names(details)[i], quote = ""))
# }
# 
# ls(pattern = "effort|zero")
# 
# # effort <- bind_rows(maine_bba_effort,
# #                     maryland_dc_bba_effort,
# #                     new_york_bba_effort,
# #                     north_carolina_bba_effort)
# 
# zero_species <- bind_rows(maryland_dc_bba_zero_species_checklists,
#                           new_york_bba_zero_species_checklists,
#                           north_carolina_bba_zero_species_checklists)
# 
# rm(maine_bba_effort, maine_bba_zero_species_checklists,
#    new_york_bba_effort, new_york_bba_zero_species_checklists,
#    maryland_dc_bba_effort, maryland_dc_bba_zero_species_checklists,
#    north_carolina_bba_effort, north_carolina_bba_zero_species_checklists)
# 
# # effort <- effort %>%
# #   # remove the commas in the numbers so it can be classed as numeric
# #   mutate(across(contains("hours"), ~ str_remove_all(., ",")),
# #          across(contains("hours"), as.numeric)) 
# # 
# # # remove the start year from the end of the project ids.
# # effort$proj_period_id <- str_remove(effort$proj_period_id, "(_[0-9]+)")
# 
# zero_species <- zero_species %>%
#   mutate(nocturnal = ifelse(nocturnal == "t", TRUE, FALSE),
#          all_species_reported = ifelse(all_species_reported == 1, TRUE, FALSE),
#          observation_date = mdy_hms(observation_date)) %>%
#   rename(datetime = observation_date,
#          all_obs_reported = all_species_reported,
#          is_nocturnal = nocturnal) %>%
#   select(-c(num_observers,
#             protocol_code,
#             effort_distance_km))
# 
# rm(details, spcl)

## read in event data

## this only contains coded species!
# baw <- read.csv(here("data", "ebird", "1_raw",
#                      "2022 Big Atlas Weekend observations 20220627.csv")) %>%
#   rename(datetime = to_char) %>%
#   mutate(datetime = as_datetime(datetime),
#          nocturnal = ifelse(nocturnal == 1, TRUE, FALSE),
#          all_obs_reported = ifelse(all_obs_reported == 1, TRUE, FALSE),
#          user_id = str_replace_all(user_id, "USER", "obsr"))

# prebaw <- mutate(prebaw, datetime = as_datetime(datetime))
# 
# prebaw <- full_join(prebaw, zero_species)
# 
# baw <- prebaw %>%
#   mutate(datetime = as_datetime(datetime),
#          is_nocturnal = ifelse(is_nocturnal == 1, TRUE, FALSE),
#          all_obs_reported = ifelse(all_obs_reported == 1, TRUE, FALSE)) %>%
#   filter(datetime >= as_datetime("2023-06-23 18:00:00") &
#            datetime <= as_datetime("2023-06-25 23:59:59"))

# event checklists from eBird
# baw_checklists <- read.csv(here("data", "ebird", "1_raw",
#                                 "2022 Big Atlas Weekend checklists 20220627.csv")) %>%
#   rename(datetime = to_char) %>%
#   mutate(datetime = as_datetime(datetime),
#          nocturnal = ifelse(nocturnal == 1, TRUE, FALSE),
#          all_obs_reported = ifelse(all_obs_reported == 1, TRUE, FALSE),
#          user_id = str_replace_all(user_id, "USER", "obsr"))

# join the zero-species and block effort data to the event dataset
# baw_checklists <- bind_rows(baw_checklists, zero_species)
# 
# baw_checklists <- full_join(baw_checklists, effort, 
#                             by = c("block" = "atlas_block", 
#                                    "proj_id" = "proj_period_id"))

rm(effort, zero_species)

### priority/rare species from each region
# priority_birds <- read.csv(here("data", "ebird", "1_raw", 
#                                 "priority_species.csv"))
# 
# ny_complete_blocks <- c("41073A7NW", "41073B5CE", "41073B7CE", "41073B8CE", 
#                         "41073D8NW", "41073E7NW", "41073F7NW", "41073F8CE", 
#                         "41073G6CE", "41073G6NW", "41073G7CE", "41073G8NW", 
#                         "41073H8NW", "41074C4NW", "41074D1CE", "41074E2CE", 
#                         "41074E4NW", "41074G1NW", "41074G3NW", "41074H1CE", 
#                         "42073A5CE", "42073A8NW", "42073F8SW", "42073H3CE", 
#                         "42074D8NW", "42075C8CE", "42075D1CE", "42075D1NW", 
#                         "42075E2CE", "42075F1NW", "42075F2CE", "42076C5NW", 
#                         "42076D4NW", "42076F3CE", "42078E8NW", "42078F4CE", 
#                         "42078F5CE", "42078F5NW", "42078F6NW", "42078F7NW", 
#                         "42078G5NW", "42078G6CE", "42078G7NW", "42078H8CE", 
#                         "42079C2CE", "42079C2NW", "42079C3NW", "42079E1NW", 
#                         "43076A5CE", "43077A5CE", "43077A5NW", "43077A5SE", 
#                         "43077B4NW", "43077B5CE", "43077C8NW", "43078A4CE", 
#                         "44074D3CE", "44075A5NW", "44075A6CE", "44075A6NW")
# 
# md_complete_blocks <- c("39079F3NW", "39079F2NE", "39079E3NE", "39079E3CE", 
#                         "39079E2CW", "39079E4SW", "39079E4SE", "39079D3SW", 
#                         "39079C4NW", "39077F3CW", "39077F3CE", "39077E4NW",
#                         "39077E7SW", "39077D6CW", "39077D5CW", "39077D5SW", 
#                         "39077C5NW", "39077D4NE", "39077D4CE", "39077D4CW", 
#                         "39077D2CW", "39077D3SE", "39077D3SW", "39077C5NE",
#                         "39077C5CE", "39077C4CW", "39077C4CE", "39077C4SE", 
#                         "39077C4SW", "39077C4NE", "39077B3NW", "39077C3SE", 
#                         "39077C2CE", "39077C1CE", "39077C1SE", "39076D7CW",
#                         "39076D7SW", "39076D7SE", "39076C7NW", "39076C7CW", 
#                         "39076C8CE", "39076D5NW", "39076E5SW", "39076F7CE", 
#                         "39076E6SE", "39076E7SW", "39076D6NW", "39076C5NW",
#                         "39076C5NE", "39076C5CW", "39076B7NW", "39076B7CW", 
#                         "39077C2CE", "39076B8SE", "39076A8NW", "39077A1CE", 
#                         "39077A2NE", "39077B1SE", "39077A3CW", "39077A4CE",
#                         "38076H8CW", "38076H8SW", "38077H1CW", "38077H1NE", 
#                         "38076G6NW", "39076A7SE", "38076C6SE", "38076C4SE", 
#                         "39076A7CE", "39076A7NE", "39076A6NE", "39076A6NW",
#                         "39076B6CE", "39076B6SE", "39076B4SW", "39076E2NE", 
#                         "39076E2SE", "39076D3SW", "39076F4CW", "39076E1SW", 
#                         "39076E4CW", "39076F5SE", "39076D2CW", "39076E5CE",
#                         "39075E7CW", "39075F7NW", "39075E8CE", "39075E8CW", 
#                         "39075E8NE", "38076H2SE", "38076H2SW", "38075H8CW")

# Data Prep -------------------------------------------------------------------

# Convert internal breeding codes to public codes
int_codes <- read.csv(here(moveup, "data", "ebird", "0_metadata", 
                           "ebird_internal_codes.csv"))

baw <- left_join(baw, int_codes, by = c("breeding_code" = "internal")) %>%
  rename(public_breeding_code = public,
         internal_breeding_code = breeding_code)

rm(int_codes)

# edit the block codes to match partial blocks in ebird, which start with "o"
priority_blocks$atlas_block <- gsub("o", "", priority_blocks$atlas_block)
prebaw_blocks$atlas_block <- gsub("o", "", prebaw_blocks$atlas_block)
prebaw_spp$atlas_block <- gsub("o", "", prebaw_spp$atlas_block)
baw$block <- gsub("o", "", baw$block)
baw_checklists$block <- gsub("o", "", baw_checklists$block)

# Add all blocks into pre-event dataset so that 0-hour blocks are included
## prebaw includes some non-atlas blocks; remove those
# prebaw_blocks <- prebaw_blocks %>%
#   filter(atlas_block %in% priority_blocks$atlas_block)
# 
# prebaw_spp <- prebaw_spp %>%
#   filter(atlas_block %in% priority_blocks$atlas_block)
# 
# prebaw_blocks <- full_join(prebaw_blocks, priority_blocks, 
#                            by = c("atlas_block", "state_code" = "state")) %>%
#   distinct()
# 
# prebaw_spp <- full_join(prebaw_spp, priority_blocks, 
#                         by = c("atlas_block", "state_code" = "state")) %>%
#   distinct()
# 
# baw <- full_join(baw, priority_blocks,
#                  by = c("block" = "atlas_block", "state")) %>%
#   distinct()
# 
# baw_checklists <- full_join(baw_checklists, priority_blocks,
#                             by = c("block" = "atlas_block", "state")) %>%
#   distinct()

# baw <- baw %>%
#   filter(datetime > as_datetime("2022-06-24 17:59:59") &
#            datetime < as_datetime("2022-06-27 0:00:00")) %>%
#   filter(state %in% c("US-ME", "US-NY", "US-MD", "US-DC", "US-NC")) 
# 
# baw_checklists <- baw_checklists %>%
#   filter(datetime > as_datetime("2022-06-24 17:59:59") &
#            datetime < as_datetime("2022-06-27 0:00:00")) %>%
#   filter(state %in% c("US-ME", "US-NY", "US-MD", "US-DC", "US-NC"))

# Data Checks -----------------------------------------------------------------
# check min and max dates, check that the users and sub_ids overlap in the baw
# vs baw_checklist files, etc

## baw
dim(baw)

unique(max(baw$datetime, na.rm = TRUE))
unique(min(baw$datetime, na.rm = TRUE))

table(as_date(baw$datetime))

lapply(baw[, c("project_code", 
               "breeding_category", 
               "is_nocturnal", 
               "all_obs_reported",
               "state_code")], unique)

lapply(baw[, c("sampling_event_identifier",
               "observer_id",
               "common_name")], n_distinct)

sum(!baw$sampling_event_identifier %in% baw_checklists$sampling_event_identifier)
sum(!baw$observer_id %in% baw_checklists$observer_id)

# baw_checklists
dim(baw_checklists)

sum(!baw_checklists$sub_id %in% baw$sub_id)
sum(!baw_checklists$user_id %in% baw$user_id)

unique(max(baw_checklists$datetime, na.rm = TRUE))
unique(min(baw_checklists$datetime, na.rm = TRUE))

unique(baw_checklists$proj_id)

lapply(baw_checklists[, c("sub_id",
                          "user_id")], n_distinct)

baw %>%
  filter(!duplicated(sampling_event_identifier) & is_nocturnal == TRUE) %>%
  group_by(project_code) %>%
  summarize(nocturnal = sum(duration_hrs, na.rm = TRUE),
            noc_checklists = n_distinct(sub_id))

## prebaw
dim(prebaw_blocks)

n_distinct(prebaw_blocks$atlas_block)
prebaw_blocks[which(duplicated(prebaw_blocks$atlasblock)), ]

all.equal(baw_checklists$nocturnal_hours + baw_checklists$diurnal_hours, 
          baw_checklists$total_hours)

# check for NAs
lapply(baw_checklists[, c("nocturnal_hours",
               "diurnal_hours",
               "total_hours")], function(x) sum(is.na(x)))

# remove obs submitted through the wrong atlas portal
baw_checklists <- baw_checklists[-which(is.na(baw_checklists$total_hours)), ]

# State Challenges ------------------------------------------------------------

exclude_obs <- c("obsr207928",
                 "obsr228000",
                 "obsr1739101",
                 "obsr1621424",
                 "obsr1698730",
                 "obsr436000",
                 "obsr1058918")

# o Nocturnal Hours -----------------------------------------------------------
baw <- baw %>% 
  mutate(project_code = ifelse(project_code == "EBIRD", 
                               "EBIRD_CB", project_code))

## Complete nocturnal checklists from any block
baw_noc <- baw %>%
  filter(!duplicated(sampling_event_identifier) &
           all_obs_reported == TRUE &
           is_nocturnal == TRUE) %>%
  group_by(observer_id, project_code) %>%
  mutate(n_checklists = n_distinct(sampling_event_identifier)) %>%
  ungroup()

# pr lists
baw_noc_pr <- baw %>%
  filter(!duplicated(sampling_event_identifier) &
           all_obs_reported == TRUE &
           is_nocturnal == TRUE &
           project_code %in% c("EBIRD", "EBIRD_CB")) %>%
  slice_sample(n = 2) 

print(baw_noc_pr$sampling_event_identifier)

# WINNER: ANTONIO & ALONSO ARCE
# BACKUP WINNER: Jose A. Salguero

# baw_noc <- baw_checklists[!duplicated(baw_checklists$sub_id), ] %>%
#   filter(nocturnal == TRUE & 
#            proj_id %in% c(me, ny, md, nc) &
#            all_obs_reported == TRUE) %>%
#   group_by(user_id, proj_id) %>%
#   mutate(n_checklists = n_distinct(sub_id)) %>%
#   ungroup()

winners <- baw_noc %>%
  group_by(project_code) %>%
  slice_sample(n = 2) %>%
  select(project_code, observer_id) %>%
  ungroup() %>%
  mutate(contest = "Nocturnal Checklists")

top_ten <- baw_noc %>%
  group_by(project_code, observer_id) %>%
  summarize(challenge_value = n_distinct(sampling_event_identifier)) %>%
  slice_max(challenge_value, n = 10) %>%
  mutate(contest = "Nocturnal Checklists") %>%
  ungroup()

# o New Coded Species ---------------------------------------------------------
## Find species with breeding codes from priority blocks that have not been
## coded in that block yet.
# 
# prebaw_species <- prebaw_spp %>%
#   group_by(project_code, atlas_block) %>%
#   distinct(common_name) %>%
#   ungroup()

prebaw_spp <- prebaw_spp %>%
  rename(breeding_category_first = breeding_category)

baw_upgrade <- baw %>%
  mutate(breeding_category = factor(breeding_category, ordered = TRUE,
                                    levels = c("C2", "C3", "C4"))) %>%
  filter(breeding_category %in% c("C2", "C3", "C4")) %>%
  group_by(atlas_block, observer_id) %>%
  distinct(common_name, breeding_category, .keep_all = TRUE) %>%
  full_join(prebaw_spp,
            by = c("common_name",
                   "atlas_block",
                   "project_code",
                   "state_code")) %>%
  ungroup() %>%
  mutate(new_spp = case_when(
    is.na(breeding_category_first) ~ "new",
    breeding_category > breeding_category_first ~ "upgrade",
    TRUE ~ "exists"
  )) %>%
  filter(new_spp %in% c("new", "upgrade"))

# pr winners
baw_newspecies_pr <- baw_newspecies %>%
  filter(project_code %in% c("EBIRD", "EBIRD_CB")) %>%
  slice_sample(n = 2)

print(baw_newspecies_pr$sampling_event_identifier)

# WINNER: ANTONIO & ALONSO ARCE
# BACKUP WINNER: Jose A. Salguero

winners <- baw_newspecies %>%
  group_by(project_code) %>%
  slice_sample(n = 2) %>%
  ungroup() %>%
  select(project_code, observer_id) %>%
  mutate(contest = "New Coded Species") %>%
  bind_rows(winners)

top_ten <- baw_newspecies %>%
  group_by(project_code, observer_id) %>%
  summarize(challenge_value = n_distinct(common_name)) %>%
  group_by(project_code) %>%
  slice_max(challenge_value, n = 10) %>%
  mutate(contest = "New Coded Species") %>%
  ungroup() %>%
  bind_rows(top_ten)

# o Priority Coded Species ----------------------------------------------------
## Find priority/rare species in blocks that have not been coded in 
# that block yet.

# need to have species and project column in priority species dataframe
# prebaw_priority_species <- prebaw_spp %>%
#   semi_join(priority_birds,
#             by = c("common_name", "project_code" = "proj_id")) %>%
#   group_by(project_code, atlas_block) %>%
#   distinct(common_name, .keep_all = TRUE) %>%
#   ungroup()
# 
# baw_priority_species <- baw %>%
#   filter(category_code %in% c("C2", "C3", "C4") &
#            !user_id %in% exclude_obs) %>%
#   group_by(proj_id, block, user_id) %>%
#   distinct(primary_com_name, .keep_all = TRUE) %>% 
#   semi_join(priority_birds,
#             by = c("primary_com_name" = "common_name",
#                    "proj_id")) %>%
#   anti_join(prebaw_priority_species,
#             by = c("primary_com_name" = "common_name",
#                    "block" = "atlas_block",
#                    "proj_id" = "project_code")) %>%
#   group_by(proj_id, user_id) %>%
#   mutate(n_priority_species = n_distinct(primary_com_name)) %>%
#   ungroup()
# 
# winners <- baw_priority_species %>%
#   group_by(proj_id) %>%
#   slice_sample(n = 2) %>%
#   ungroup() %>%
#   select(proj_id, user_id) %>%
#   mutate(contest = "New Priority Species") %>%
#   bind_rows(winners)
# 
# top_ten <- baw_priority_species %>%
#   group_by(proj_id) %>%
#   mutate(challenge_value = n_priority_species) %>%
#   distinct(user_id, .keep_all = TRUE) %>%
#   slice_max(challenge_value, n = 10) %>%
#   mutate(contest = "New Priority Species") %>%
#   ungroup() %>%
#   select(proj_id, user_id, challenge_value, contest) %>%
#   bind_rows(top_ten)

# Effort in incomplete priority blocks
## Complete checklists per incomplete block per person
# o Checklists in <20 hr Blocks -----------------------------------------------
## Complete checklists from blocks with <20 hrs of diurnal effort

# need what each project is considering "complete"; default is 20 hrs
baw_checklists <- baw %>%
  filter(!atlas_block %in% complete[, "atlas_block"] &
           all_obs_reported == TRUE &
           breeding_category %in% c("C2", "C3", "C4")) %>%
  distinct(sampling_event_identifier, .keep_all = TRUE) %>%
  group_by(project_code, observer_id) %>% 
  mutate(n_checklists = n_distinct(sampling_event_identifier)) %>%
  ungroup()

# pr winners
baw_checklists_pr <- baw_checklists %>%
  filter(project_code %in% c("EBIRD", "EBIRD_CB")) %>%
  slice_sample(n = 2)

print(baw_checklists_pr$sampling_event_identifier)

# WINNER: ANTONIO & ALONSO ARCE
# BACKUP WINNER: Gabriel Lugo-Ortiz

winners <- baw_checklists %>%
  group_by(project_code) %>%
  slice_sample(n = 2) %>%
  ungroup() %>%
  select(project_code, observer_id) %>%
  mutate(contest = "Checklists") %>%
  bind_rows(winners)

top_ten <- baw_checklists %>%
  group_by(project_code, observer_id) %>%
  summarize(challenge_value = n_checklists) %>%
  group_by(project_code) %>%
  slice_max(challenge_value, n = 10) %>%
  mutate(contest = "Checklists") %>%
  bind_rows(top_ten)

# o New-to-user Priority Blocks ----------------------------------------------- 
## Visit in new-to-user priority block documented by a complete atlasing 
## checklist that is >= 5 min.

# baw_new_blocks <- baw_checklists %>%
#   group_by(proj_id) %>%
#   # add number of atlasers for calculating state winner
#   mutate(n_atlasers = n_distinct(user_id)) %>%
#   ungroup() %>%
#   anti_join(prebaw_blocks,
#             by = c("proj_id" = "project_code",
#                    "user_id" = "observer_id",
#                    "block" = "atlas_block")) %>%
#   filter(all_obs_reported == TRUE &
#            duration_hrs >= 5/60) # 5 minutes in hours (0.083 hrs = 5 min)
# 
# winners <- baw_new_blocks %>%
#   group_by(proj_id) %>%
#   slice_sample(n = 2) %>%
#   ungroup() %>%
#   select(proj_id, user_id) %>%
#   mutate(contest = "New-to-user Blocks") %>%
#   bind_rows(winners)
# 
# top_ten <- baw_new_blocks %>%
#   group_by(proj_id, user_id) %>%
#   summarize(challenge_value = n_distinct(block)) %>%
#   group_by(proj_id) %>%
#   slice_max(challenge_value, n = 10) %>%
#   mutate(contest = "New-to-user Blocks") %>%
#   bind_rows(top_ten)

# o Atlas Checklists ----------------------------------------------------------
## Complete checklist with at least one coded species 
# baw_atlas <- baw[!duplicated(baw$sub_id), ] %>%
#   filter(category_code %in% c("C2", "C3", "C4") & all_obs_reported == TRUE)
# 
# winners <- baw_atlas %>%
#   group_by(proj_id) %>%
#   slice_sample(n = 2) %>%
#   ungroup() %>%
#   select(proj_id, user_id) %>%
#   mutate(contest = "Atlas Checklists") %>%
#   bind_rows(winners)
# 
# top_ten <- baw_atlas %>%
#   group_by(proj_id, user_id) %>%
#   summarize(challenge_value = length(sub_id)) %>%
#   slice_max(challenge_value, n = 10) %>%
#   mutate(contest = "Atlas Checklists") %>%
#   bind_rows(top_ten)

write.csv(winners, here("data", "ebird", "3_filtered", "BAW_2022_winners.csv"),
          row.names = FALSE)

write.csv(top_ten, here("data", "ebird", "3_filtered", 
                        "BAW_2022_top10-atlaser-challenges.csv"),
          row.names = FALSE)

# Inter-state Challenges ------------------------------------------------------
# --Nocturnal -----------------------------------------------------------------
# for each region, find checklists per atlaser
interstate_atlasers <- baw %>%
  group_by(project_code) %>%
  summarize(n_atlasers = n_distinct(observer_id))

interstate_atlasers[4,2] <- 5

interstate_noc <- baw %>%
  group_by(project_code) %>%
  left_join(interstate_atlasers) %>%
  filter(!duplicated(sampling_event_identifier) &
         all_obs_reported == TRUE &
         is_nocturnal == TRUE) %>%
  mutate(n_checklists = n_distinct(sampling_event_identifier),
         challenge_value = n_checklists/n_atlasers,
         challenge = "Nocturnal checklists") %>%
  ungroup() %>%
  distinct(project_code, n_atlasers, challenge_value, challenge, n_checklists) 

# --Checklists ------------
interstate_checklist <- baw %>%
  group_by(project_code) %>%
  left_join(interstate_atlasers) %>%
  filter(!duplicated(sampling_event_identifier) &
           all_obs_reported == TRUE &
           breeding_category %in% c("C2", "C3", "C4") &
           !atlas_block %in% complete[, "atlas_block"]) %>%
  mutate(n_checklists = n_distinct(sampling_event_identifier),
         challenge_value = n_checklists/n_atlasers,
         challenge = "Coded checklists") %>%
  ungroup() %>%
  distinct(project_code, n_atlasers, challenge_value, challenge, n_checklists) 

# --New species -------------

interstate_spp <- baw_upgrade %>%
  mutate(project_code = ifelse(project_code == "EBIRD", 
                               "EBIRD_CB", project_code)) %>%
  left_join(interstate_atlasers) %>%
  group_by(project_code) %>%
  mutate(n_spp = n(),
         challenge_value = n_spp/n_atlasers,
         challenge = "New/upgraded species") %>%
  ungroup() %>%
  distinct(project_code, n_atlasers, challenge_value, challenge, n_spp) 

interstate_spp[4, "n_spp"] <- 219

interstate_spp <- interstate_spp %>%
  group_by(project_code) %>%
  mutate(challenge_value = n_spp/n_atlasers) %>%
  ungroup() %>%
  distinct(project_code, n_atlasers, challenge_value, challenge, n_spp) 

interstate <- bind_rows(interstate_checklist,
                        interstate_noc,
                        interstate_spp)



# o Nocturnal Hours -----------------------------------------------------------
# state_winner <- baw_checklists[!duplicated(baw_checklists$sub_id), ] %>%
#   filter(nocturnal == TRUE & all_obs_reported == TRUE) %>%
#   group_by(proj_id) %>%
#   summarize(challenge_value = n_distinct(sub_id) / 
#               n_distinct(user_id)) %>%
#   mutate(contest = "Nocturnal Checklists") %>%
#   ungroup()
# 
# # o Breeding Codes ------------------------------------------------------------
# ## Total number of breeding codes submitted (codes/atlasers)
# 
# state_winner <- baw %>% 
#   filter(category_code %in% c("C2", "C3", "C4")) %>%
#   group_by(proj_id) %>%
#   summarize(challenge_value = length(category_code) / n_distinct(user_id)) %>%
#   ungroup() %>%
#   mutate(contest = "Breeding Codes") %>%
#   bind_rows(state_winner)
# 
# # o New-to-user Blocks --------------------------------------------------------
# ### Number of priority blocks visited that the atlaser hadn't visited prior
# ### to the event. (% atlasers who go to a new-to-them block)
# 
# state_winner <- baw_new_blocks %>%
#   group_by(proj_id) %>%
#   summarize(challenge_value = n_distinct(user_id)/unique(n_atlasers)) %>%
#   ungroup() %>%
#   mutate(contest = "New-to-user Blocks") %>%
#   bind_rows(state_winner)

# add canada
can_winner <- data.frame(project_code = rep(c("ON", "NF"), 3), 
                         challenge_value = c(14.02778, 6.833333,
                                             0.238095, 0.833333, 
                                             2.761905, 1.916667),
                         challenge = rep(c("New/upgraded species",
                                         "Nocturnal Checklists", 
                                         "Coded checklists"), each = 2),
                         n_atlasers = rep(c(252, 12), 3))

state_winner <- bind_rows(interstate, can_winner)

# o Find winner ---------------------------------------------------------------
state_winner <- state_winner %>%
  group_by(challenge) %>%
  mutate(contest_rank = rank(challenge_value, ties.method = "max")) %>%
  ungroup() %>%
  arrange(challenge)

trophy <- state_winner %>%
  group_by(project_code) %>%
  summarize(BAW_rank = sum(contest_rank)) %>%
  ungroup() %>%
  arrange(desc(BAW_rank))

# if there are ties...
if(any(duplicated(trophy$BAW_rank))) {
  tie_breaker <- state_winner %>%
    left_join(trophy) %>%
    arrange(contest, -rank) %>%
    group_by(contest) %>%
    mutate(tie_breaker = BAW_rank + challenge_value) %>%
    group_by(proj_id) %>%
    summarize(tie_breaker = sum(tie_breaker))
} else print(paste("No ties!", "The winner is", 
                   trophy[which(trophy$BAW_rank == max(trophy$BAW_rank)), 
                          "proj_id"]))


# Create summary --------------------------------------------------------------
baw_summary <- baw %>%
  filter(category_code %in% c("C2", "C3", "C4")) %>%
  group_by(proj_id) %>%
  summarize(n_codes = length(internal_breeding_code),
            n_c4 = sum(category_code == "C4"),
            n_c3 = sum(category_code == "C3"),
            n_c2 = sum(category_code == "C2"),
            n_species = n_distinct(primary_com_name),
            n_blocks = n_distinct(block)) %>%
  ungroup()

baw_checklists_summary <- baw_checklists %>%
  group_by(proj_id) %>%
  summarize(n_checklists = n_distinct(sub_id),
            n_nocturnal_checklists = sum(nocturnal),
            n_atlasers = n_distinct(user_id),
            n_hrs = sum(duration_hrs, na.rm = TRUE),
            n_nocturnal_hrs = sum(duration_hrs[nocturnal == TRUE], 
                                  na.rm = TRUE)) %>%
  ungroup()

baw_summary <- left_join(baw_summary, baw_checklists_summary,
                         by = "proj_id")

write.csv(baw_summary, here("data", "ebird", "3_filtered", 
                            "BAW_2022_summary.csv"), row.names = FALSE)

blanktheme <- theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "transparent",
                                                    colour = NA), 
                    plot.background = element_rect(fill = "transparent",
                                                   colour = NA),
                    axis.line = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.text.y = element_blank(), 
                    axis.ticks = element_blank(), 
                    axis.title.x = element_blank(), 
                    axis.title.y = element_blank(),
                    legend.background = element_rect(fill = "transparent",
                                                     colour = NA))

## rank new coded species for the block based on number of blocks
baw_newspecies_sum <- baw_newspecies %>%
  group_by(proj_id, primary_com_name) %>%
  summarize(n_blocks = n_distinct(block)) %>%
  ungroup() %>%
  arrange(proj_id, -n_blocks)

# add ontario/nf
canada <- read.csv(here("data", "ebird", "1_raw", 
                        "newBE_sp.csv"))

canada <- canada %>%
  select(proj_id = StateProvince,
         n_blocks = n_squares,
         primary_com_name = common_name)

baw_newspecies_sum <- rbind(baw_newspecies_sum, canada)

top_new_spp <- ggplot(baw_newspecies_sum %>%
         group_by(proj_id) %>%
         slice_max(n_blocks, n = 3, with_ties = FALSE) %>%
         ungroup()) +
  geom_col(aes(x = reorder(primary_com_name, -n_blocks, sum), 
               y = n_blocks, fill = proj_id),
           position = "dodge") +
  geom_text(aes(x = primary_com_name, y = 1,#(n_blocks/2)+1.5, 
                 label = primary_com_name), angle = 90, size = 5, hjust = 0, 
            check_overlap = TRUE) +
  ggtitle("Top 3 species added to new blocks/squares for each Atlas in BAW 2022") +
  ylab("Number of new blocks/squares") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent",
                                         color = NA)) +
  scale_fill_discrete(name = NULL,
                      labels = c("Maryland-DC",
                                 "Maine", 
                                 "North Carolina",
                                 "New York",
                                 "Newfoundland",
                                 "Ontario"))

ggsave(here("output", "big atlas weekend", "top_new_spp.png"),
       top_new_spp, 
       bg = "transparent")  

# find species that are unique to each region
unique_spp <- baw %>%
  filter(category_code %in% c("C2", "C3", "C4")) %>%
  group_by(proj_id) %>%
  distinct(primary_com_name) %>%
  group_by(primary_com_name) %>%
  mutate(projects_coded = length(primary_com_name)) %>%
  filter(projects_coded == 1)

# find number of nests in md
baw %>%
  filter(public_breeding_code %in% c("ON", "NE", "NY", "NB") &
           proj_id == md) %>%
  summarize(n_nests = length(public_breeding_code),
            n_spp = n_distinct(primary_com_name))

# find number of young birds in md
baw %>%
  filter(public_breeding_code %in% c("FL", "FY", "NY") &
           proj_id == md) %>%
  summarize(n_chicks = length(public_breeding_code),
            n_spp = n_distinct(primary_com_name))

# find owls and nightjars for md and compare it to other regions
baw %>% 
  filter(category_code %in% c("C2", "C3", "C4")) %>% 
  group_by(proj_id) %>% 
  summarize(easo = sum(primary_com_name == "Eastern Screech-Owl"), 
            bado = sum(primary_com_name == "Barred Owl"), 
            ghow = sum(primary_com_name == "Great Horned Owl"), 
            bano = sum(primary_com_name == "Barn Owl"), 
            ewpw = sum(primary_com_name == "Eastern Whip-poor-will"), 
            cwiw = sum(primary_com_name == "Chuck-will's-widow")) 

# plot when checklists were submitted
ggplot(baw_checklists %>%
         mutate(start = floor_date(datetime, unit = "hour")) %>%
         group_by(start, proj_id) %>%
         summarize(n_checklists = length(start))) + 
  geom_line(aes(x = start, y = n_checklists, color = proj_id))

library(sf)

countymap <- st_read(dsn = here("data", "mapping", "md_county_maps"),
                     layer = "md_county_maps")

find_county <- function(df, map_dsn, map_layer, map_sf = NULL, 
                        map_col = "NAME", crs = 4326,
                        lon = "longitude", lat = "latitude") {
  if(any(class(map_sf) %in% c("sf", "sfc", "sfg"))) {
    census_county <- sf::st_transform(map_sf, crs = crs)
  } else {
    census_county <- sf::st_read(map_dsn, map_layer) %>%
      sf::st_transform(crs = crs)
  }
  points <- sf::st_as_sf(df, coords = c(lon, lat), crs = crs) 
  
  counties_points_in <- sf::st_intersects(points, census_county)
  
  county <- NA
  
  for(i in seq_along(df[, 1])) {
    county[i] <- data.frame(census_county)[counties_points_in[[i]], map_col]
  }
  return(county)
} 

county <- find_county(df = baw_checklists[which(baw_checklists$proj_id == md), 
                                          c("latitude", "longitude")],
                      map_dsn = here("data", "mapping", 
                                     "census_county_boundaries"),
                      map_layer = "census_county_boundaries_mddc")

ggplot(baw_checklists %>%
         filter(proj_id == md)) +
  geom_sf(data = county_map, )

baw_checklists[which(baw_checklists$proj_id == md), "county"] <- county 

baw_md <- filter(baw_checklists, proj_id == md)

countymap <- left_join(countymap, baw_checklists, by  = "county")

countymap <- countymap %>%
  group_by(county) %>%
  mutate(n_checklists = n_distinct(sub_id))

ggplot(countymap) +
  geom_sf(aes(fill = n_checklists)) + 
  blanktheme

# Find winner's names ---------------------------------------------------------
atlasers <- read.csv(here("data", "bba3", "atlasers_mar.csv"))

winner_names <- winners %>%
  mutate(user_id = str_replace(user_id, "obsr", "USER")) %>%
  filter(proj_id == md) %>%
  left_join(atlasers, by = "user_id")

write.csv(winner_names, here("data", "ebird", "3_filtered", 
                             "MD-DC_BAW_2022_winners.csv"),
          row.names = FALSE)

top_ten_names <- top_ten %>%
  mutate(user_id = str_replace(user_id, "obsr", "USER")) %>%
  filter(proj_id == md) %>%
  left_join(atlasers, by = "user_id")

write.csv(top_ten_names, here("data", "ebird", "3_filtered", 
                             "MD-DC_BAW_2022_top_ten.csv"),
          row.names = FALSE)

top_ten_names <- top_ten_names %>%
  mutate(name = paste(first_name, last_name)) 

mva_summary <- baw %>%
  filter(category_code %in% c("C2", "C3", "C4")) %>%
  group_by(user_id, proj_id) %>%
  summarize(n_codes = length(internal_breeding_code),
            n_c4 = sum(category_code == "C4"),
            n_c3 = sum(category_code == "C3"),
            n_c2 = sum(category_code == "C2"),
            n_species = n_distinct(primary_com_name),
            n_blocks = n_distinct(block)) %>%
  ungroup()

mva_checklists_summary <- baw_checklists %>%
  group_by(user_id, proj_id) %>%
  summarize(n_checklists = n_distinct(sub_id),
            n_nocturnal_checklists = sum(nocturnal),
            n_projects = n_distinct(proj_id),
            n_hrs = sum(duration_hrs, na.rm = TRUE),
            n_nocturnal_hrs = sum(duration_hrs[nocturnal == TRUE], 
                                  na.rm = TRUE)) %>%
  ungroup()


mva_summary <- full_join(mva_summary, mva_checklists_summary,
                         by = c("user_id", "proj_id"))
mva_summary
write.csv(mva_summary, here("data", "ebird", "3_filtered",
                            "BAW_2022_mva_summary.csv"),
          row.names = FALSE)

mva_summary_names <- mva_summary %>%
  mutate(user_id = str_replace(user_id, "obsr", "USER")) %>%
  filter(proj_id == md) %>%
  left_join(atlasers, by = "user_id") %>%
  mutate(name = paste(first_name, last_name)) 

write.csv(mva_summary_names, here("data", "ebird", "3_filtered", 
                              "MD-DC_BAW_2022_mva.csv"),
          row.names = FALSE)

# summaries with canada
can_summary <- read.csv(here("data", "ebird", "1_raw", 
                             "BAW_2022_summary_Canada.csv"))

codesp <- ggplot(codes) +
  geom_col(aes(x = proj_id, y = n_codes, fill = codes), position = "dodge") +
  ggtitle("Number of codes in each region during the Big Atlas Weekend 2022") +
  ylab("Number of breeding codes") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent",
                                         color = NA)) +
  scale_fill_discrete(name = NULL)

ggsave(here("output", "big atlas weekend", "n_codes.png"), codesp, 
       bg = "transparent")

sppp <- ggplot(can_summary) + 
  geom_col(aes(x = proj_id, y = n_species, fill = n_checklists)) +
  ggtitle("Coded species and checklists in each region during Big Atlas Weekend 2022") +
  ylab("Number of coded species") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA),
        axis.title.x = element_blank(),
        legend.background = element_rect(fill = "transparent",
                                         color = NA)) +
  scale_fill_continuous(name = "Number of\nchecklists")

ggsave(here("output", "big atlas weekend", "n_spp.png"), sppp, 
       bg = "transparent")

effort <- data.frame(proj_id = rep(can_summary$proj_id, 2), 
                     n_atlasers = rep(can_summary$n_atlasers, 2),
                     Effort = c(can_summary$n_hrs, 
                                can_summary$n_nocturnal_hrs),
                     type = rep(c("Daytime", "Nocturnal"), each = 6))

ggplot(effort) +
  geom_col(aes(x = proj_id, y = Effort, fill = type))
