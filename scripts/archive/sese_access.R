library(readxl)
sese.base = read_excel("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\SESE Fuels Gradient\\r code\\study design\\SESE_burnedAreas_wOwners_county_14Jan2026.xls", sheet = "Fires_All2.shp")

sese = sese.base %>%
  filter(County != "Monterey") %>%
  select(-FID,-Shap_Ar,-Shp_Lng) %>%
  arrange(Brn_Svr) %>%
  mutate(access = case_when(AccssTy == "Easement" ~ "easement", 
                            is.na(AccssTy) ~ "likely easement", .default = AccssTy),
         severity = case_when(Brn_Svr == 1 ~ "und_chg",
                              Brn_Svr == 2 ~ "low",
                              Brn_Svr == 3 ~ "moderate",
                              Brn_Svr == 4 ~ "high",
                              .default = "oopsy"),
         cty_grp = case_when(County %in% c("Santa Cruz","Santa Clara","San Mateo") ~ "southern counties",
                             County %in% c("Napa","Sonoma") ~ "Sonoma/Napa",
                             County  %in% c("Curry County","Humboldt") ~ "Humboldt/Curry",
                             .default = County)
         ) %>%
  group_by(Yr_Brnd, Owner, Agency, severity, access, State, cty_grp) %>%
  summarize(area_ha = sum(Brnd_HA)) %>%
  pivot_wider(id_cols = c(Yr_Brnd, Owner, Agency, access, State, cty_grp), names_from = "severity", values_from = "area_ha") %>%
  relocate(high, .after = moderate) %>%
  relocate(und_chg, .before = low)
unique(sese$access)
head(sese)

sese.open = sese %>%
  mutate(gen.access = case_when(access == "Open Access" ~ "open",
                                     .default = "misc")) %>%
  group_by(Yr_Brnd, gen.access, cty_grp) %>%
  summarize(across(und_chg:high, sum, na.rm = TRUE)) %>%
  group_by(Yr_Brnd) %>%
  mutate(
    und_chng_pct = und_chg / sum(und_chg, na.rm = TRUE) * 100,
    low_pct = low / sum(low, na.rm = TRUE) * 100,
    mod_pct = moderate / sum(moderate, na.rm = TRUE) * 100,
    high_pct = high / sum(high, na.rm = TRUE) * 100,
    # Severity_1_total = sum(Severity_1, na.rm = TRUE),
    # Severity_2_total = sum(Severity_2, na.rm = TRUE),
    # Severity_3_total = sum(Severity_3, na.rm = TRUE),
    # Severity_4_total = sum(Severity_4, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(across(is.numeric, replace_na, 0))
sese.open  

sese.access.list = sese.base %>%
  mutate(access = case_when(AccssTy == "Easement" ~ "easement", 
                            is.na(AccssTy) ~ "likely easement", .default = AccssTy),
         severity = case_when(Brn_Svr == 1 ~ "und_chg",
                              Brn_Svr == 2 ~ "low",
                              Brn_Svr == 3 ~ "moderate",
                              Brn_Svr == 4 ~ "high",
                              .default = "oopsy"
         )) %>%
  group_by(Yr_Brnd, Owner, Agency, severity, access, State, County) %>%
  summarise(area_ha = sum(Brnd_HA))
sese.access.list



##############
###refiend list
library(readxl)
sese.access = sese %>%
  ungroup() %>%
  select(Owner, Agency, access)
sese.access

sese.ref = read_excel("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\SESE Fuels Gradient\\r code\\study design\\owner_severity_table2.xlsx", sheet = "Sheet1") %>%
  mutate(access = case_when(AccessType == "Easement" ~ "easement", 
                            is.na(AccessType) ~ "likely easement", .default = AccessType),
         severity = case_when(Burn_Severity == 1 ~ "und_chg",
                              Burn_Severity == 2 ~ "low",
                              Burn_Severity == 3 ~ "moderate",
                              Burn_Severity == 4 ~ "high",
                              .default = "oopsy"),
         cty_grp = case_when(County %in% c("Santa Cruz","Santa Clara","San Mateo") ~ "southern counties",
                             County %in% c("Napa","Sonoma") ~ "Sonoma/Napa",
                             County  %in% c("Curry County","Humboldt") ~ "Humboldt/Curry",
                             .default = County)) %>%
  group_by(Year_Burned, Owner, severity, access, cty_grp) %>%
  summarize(area_ha = round(sum(Hectares_Burned),1)) %>%
  pivot_wider(id_cols = c(Year_Burned, Owner, access, cty_grp), names_from = "severity", values_from = "area_ha", values_fill = 0) %>%
  relocate(cty_grp, .after = Year_Burned) %>%
  relocate(high, .after = moderate) %>%
  relocate(und_chg, .before = low)
head(sese.ref)
write.csv(sese.ref, "C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\SESE Fuels Gradient\\r code\\study design\\owner_severity_table_rotate.csv")
