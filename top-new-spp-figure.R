# you'll just have to pull in your own data; the structure of this data looks like:

#> baw_newspecies

# A tibble: 3,767 x 4
# primary_com_name       proj_id         user_id     block    
# <chr>                  <chr>           <chr>       <chr>    
#   1 Barred Owl             EBIRD_ATL_MD_DC obsr470319  38076F6CE
# 2 Eastern Whip-poor-will EBIRD_ATL_ME    obsr224295  45067A8CW
# 3 American Woodcock      EBIRD_ATL_ME    obsr333476  45068G5NW
# 4 Eastern Whip-poor-will EBIRD_ATL_ME    obsr317171  43070G5CW
# 5 Northern Flicker       EBIRD_ATL_NY    obsr1368117 43073E6NW
# 6 Hairy Woodpecker       EBIRD_ATL_NY    obsr1368117 43073E6NW
# 7 Alder Flycatcher       EBIRD_ATL_NY    obsr1368117 43073E6NW
# 8 Barred Owl             EBIRD_ATL_NY    obsr1368117 43073E6NW
# 9 Song Sparrow           EBIRD_ATL_NY    obsr1368117 43073E6NW
# 10 American Goldfinch     EBIRD_ATL_NY    obsr1368117 43073E6NW
# ... with 3,757 more rows

# so each row is a new species for that block and that user.

library(ggplot2)

baw_upgrade <- baw %>%
  mutate(breeding_category = factor(breeding_category, ordered = TRUE,
                                    levels = c("C2", "C3", "C4"))) %>%
  filter(breeding_category %in% c("C2", "C3", "C4")) %>%
  group_by(atlas_block) %>%
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
  filter(new_spp %in% c("new", "upgrade")) %>%
  distinct(common_name, atlas_block, .keep_all = TRUE) %>%
  select(common_name, breeding_category, state_code, atlas_block,
         observer_id, sampling_event_identifier, project_code,
         breeding_category_first, new_spp) 

## rank new coded species for the block based on number of blocks
baw_new_sum <- baw_upgrade %>%
  filter(new_spp == "new") %>%
  group_by(project_code, common_name) %>%
  summarize(n_blocks = n_distinct(atlas_block)) %>%
  ungroup() %>%
  arrange(project_code, -n_blocks)

baw_new_canada <- data.frame(
  project_code = c(rep("Ontario", 3),
                   rep("Newfoundland", 3)),
  common_name = c("Northern Flicker",
                  "Eastern Kingbird",
                  "Common Yellowthroat",
                  "Yellow Warbler",
                  "Ring-necked Duck",
                  "Canada Jay"),
  n_blocks = c(13, 13, 13, 2, 1, 1)
)

baw_new_sum <- bind_rows(baw_new_sum, baw_new_canada)

baw_upgrade_sum <- baw_upgrade %>%
  filter(new_spp == "upgrade") %>%
  group_by(project_code, common_name) %>%
  summarize(n_blocks = n_distinct(atlas_block)) %>%
  ungroup() %>%
  arrange(project_code, -n_blocks)

baw_upgrade_canada <- data.frame(
  project_code = c(rep("Ontario", 3),
                   rep("Newfoundland", 3)),
  common_name = c("American Redstart", 
                  "Cedar Waxwing",
                  "Nashville Warbler",
                  "Spotted Sandpiper",
                  "Yellow-rumped Warbler",
                  "Common Yellowthroat"),
  n_blocks = c(19, 18, 18, 4, 3, 3)
)

baw_upgrade_sum <- bind_rows(baw_upgrade_sum, baw_upgrade_canada)

top_new_spp <- ggplot(baw_new_sum %>%
                        group_by(project_code) %>%
                        slice_max(n_blocks, n = 3) %>%
                        ungroup()) +
  geom_col(aes(x = reorder(common_name, -n_blocks, sum), 
               y = n_blocks, fill = project_code),
           position = "dodge") +
  geom_text(aes(x = common_name, y = ifelse(n_blocks < 10, 6, n_blocks/2), 
                label = common_name), angle = 90, check_overlap = TRUE) +
  ggtitle("Top 3 species added to new blocks for each Atlas in BAW 2023") +
  ylab("Number of new blocks") +
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
                                 "North Carolina",
                                 "New York",
                                 "Puerto Rico",
                                 "Newfoundland",
                                 "Ontario"))


ggsave(here(moveup, "output", "big atlas weekend", "top_new_spp_2023.png"),
       top_new_spp, 
       bg = "transparent")  

top_upgrade_spp <- ggplot(baw_upgrade_sum %>%
                        group_by(project_code) %>%
                        slice_max(n_blocks, n = 3) %>%
                        ungroup()) +
  geom_col(aes(x = reorder(common_name, -n_blocks, sum), 
               y = n_blocks, fill = project_code),
           position = "dodge") +
  geom_text(aes(x = common_name, y = ifelse(n_blocks < 10, 6, n_blocks/2), 
                label = common_name), angle = 90, check_overlap = TRUE) +
  ggtitle("Top 3 upgraded species in blocks for each Atlas in BAW 2023") +
  ylab("Number of new blocks") +
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
                                 "North Carolina",
                                 "New York",
                                 "Puerto Rico",
                                 "Newfoundland",
                                 "Ontario"))

ggsave(here(moveup, "output", "big atlas weekend", "top_upgraded_spp_2023.png"),
       top_upgrade_spp, 
       bg = "transparent")  

# coded species and checklists in each region during BAW 2023
baw_summary <- baw %>% 
  filter(breeding_category %in% c("C2", "C3", "C4")) %>%
  group_by(project_code) %>%
  summarize(n_checklists = n_distinct(sampling_event_identifier),
            n_species = n_distinct(common_name)) %>%
  ungroup() %>% 
  mutate(state = case_when(
    project_code == "EBIRD_ATL_MD_DC" ~ "Maryland-DC",
    project_code == "EBIRD_ATL_NY" ~ "New York",
    project_code == "EBIRD_ATL_NC" ~ "North Carolina",
    project_code %in% c("EBIRD", "EBIRD_CB") ~ "Puerto Rico"
  ))

baw_summary_canada <- data.frame(
  state = c("Ontario", "Newfoundland"),
  project_code = NA,
  n_checklists = c(1412, 44),
  n_species = c(201, 61)
  )

baw_summary <- bind_rows(baw_summary, baw_summary_canada)

coded_checklists_plot <- ggplot(baw_summary) +
  geom_col(aes(x = state, y = n_species, fill = n_checklists)) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA),
        legend.background = element_rect(fill = "transparent",
                                         color = NA)) +
  xlab(NULL) +
  ylab("Number of coded species") +
  scale_fill_continuous(name = "Number of\nchecklists")

ggsave(here(moveup, "output", "big atlas weekend", "coded_checklists_2023.png"),
       coded_checklists_plot, 
       bg = "transparent")  

baw_code_summary <- baw %>%
  filter(breeding_category %in% c("C2", "C3", "C4")) %>%
  group_by(project_code, breeding_category) %>%
  summarize(n_codes = n()) %>%
  ungroup() %>% 
  mutate(state = case_when(
    project_code == "EBIRD_ATL_MD_DC" ~ "Maryland-DC",
    project_code == "EBIRD_ATL_NY" ~ "New York",
    project_code == "EBIRD_ATL_NC" ~ "North Carolina",
    project_code %in% c("EBIRD", "EBIRD_CB") ~ "Puerto Rico"
  ))

baw_code_summary_canada <- data.frame(
  state = c(rep("Ontario", 3), rep("Newfoundland", 3)),
  project_code = NA,
  breeding_category = rep(c("C2", "C3", "C4"), 2),
  n_codes = c(13580, 2810, 1255, 357, 28, 25)
)

baw_code_summary <- bind_rows(baw_code_summary, baw_code_summary_canada)

code_plot <- ggplot(baw_code_summary) +
  geom_col(aes(x = state,
               y = n_codes,
               fill = breeding_category), position = "dodge") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent",
                                         color = NA)) +
  scale_fill_discrete(name = NULL, 
                      labels = c("Possible", "Probable", "Confirmed")) +
  ylab("Number of breeding codes") + 
  xlab(NULL)

ggsave(here(moveup, "output", "big atlas weekend", "codes_2023.png"),
       code_plot, 
       bg = "transparent")  

baw_noc_summary <- baw %>% 
  filter(is_nocturnal == TRUE & all_obs_reported == TRUE) %>%
  group_by(project_code) %>%
  summarize(n_checklists = n_distinct(sampling_event_identifier)) %>%
  mutate(state = case_when(
    project_code == "EBIRD_ATL_MD_DC" ~ "Maryland-DC",
    project_code == "EBIRD_ATL_NY" ~ "New York",
    project_code == "EBIRD_ATL_NC" ~ "North Carolina",
    project_code %in% c("EBIRD", "EBIRD_CB") ~ "Puerto Rico"
  ))

baw_noc_summary_canada <- data.frame(
  state = c("Ontario", "Newfoundland"),
  n_checklists = c(60, 10)
)

baw_noc_summary <- bind_rows(baw_noc_summary, baw_noc_summary_canada)

noc_plot <- ggplot(baw_noc_summary) +
  geom_col(aes(x = state, y = n_checklists)) + 
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA)) +
  xlab(NULL) +
  ylab("Number of nocturnal checklists")

ggsave(here(moveup, "output", "big atlas weekend", 
            "nocturnal_checklists_2023.png"),
       noc_plot, 
       bg = "transparent")  

baw2 <- data.frame(n_nocturnal_hrs = 37, 
                   n_spp = 161, 
                   n_codes = 11442, 
                   n_checklists = 1233, 
                   n_atlasers = 213, 
                   n_blocks = 363, 
                   n_hrs = 668,
                   event = "baw2")

baw3 <- baw %>%
  filter(project_code %in% c("EBIRD_ATL_MD_DC")) %>%
  mutate(n_nocturnal_hrs = 7.13) %>%
  filter(breeding_category %in% c("C2", "C3", "C4")) %>%
  mutate(n_spp = n_distinct(common_name),
         n_codes = sum(breeding_category %in% c("C2", "C3", "C4"))) %>%
  filter(!duplicated(sampling_event_identifier) & all_obs_reported == TRUE) %>%
  mutate(n_checklists = n_distinct(sampling_event_identifier),
         n_atlasers = n_distinct(observer_id),
         n_blocks = n_distinct(atlas_block),
         n_hrs = sum(duration_hrs, na.rm = TRUE),
         event = "baw3") %>%
  select(matches("^n_"), event) %>%
  distinct() #%>%
  bind_rows(baw2)

rm(baw2)
