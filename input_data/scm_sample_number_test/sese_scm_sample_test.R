library(dplyr)
library(readxl)

# scm.key = read_excel("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\SESE Fuels Gradient\\r code\\scm_sample_number_test\\SCMLTFMN-masterdata-2025-shivelab.xlsx", sheet = "plots_master") %>%
#   filter(czu_dnbr != "UNBURNED")
# names(scm.key)
# 
scm.cbi.rdnbr = read_excel("C:\\Users\\kshive\\Documents\\UCB\\GIS\\Projects\\SESE_fuels\\tables\\scm_woodward_plot-locations_cbi_rdnbr.xls", sheet = "scm_woodward_plot-locations.shp") %>%
  select(plot_name, cbi4, rdnbr) %>%
  rename(plot_id = plot_name)
scm.cbi.rdnbr
nrow(scm.cbi.rdnbr)

scm = read_excel("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\SESE Fuels Gradient\\r code\\scm_sample_number_test\\SCMLTFMN-masterdata-2025-shivelab.xlsx", sheet = "fuels_calcs") %>%
  inner_join(scm.cbi.rdnbr)
nrow(scm)
head(scm)
table(scm$cbi4)

scm.low = scm%>%
  filter(cbi4 == 2)
scm.low

# Number of repetitions
num_repeats <- 30

# Subsample size
sample_size <- 15

# Use replicate to repeat the sampling and mean calculation
means <- replicate(n = num_repeats,
                   expr = {
                     subsample <- sample(scm.low$mgha_1h_mean, size = sample_size, replace = TRUE) # Set replace = TRUE or FALSE as needed
                     mean(subsample)
                     # sd(subsample)
                   })
means
hist(means)

sd <- replicate(n = num_repeats,
                   expr = {
                     subsample <- sample(scm.low$mgha_1h_mean, size = sample_size, replace = TRUE) # Set replace = TRUE or FALSE as needed
                     # mean(subsample)
                     sd(subsample)
                   })
sd
hist(sd)





library(infer)

# Create a sample data frame
df <- tibble(values = c(10, 15, 20, 25, 30, 35, 40, 45, 50))

# Perform repeated sampling
repeated_samples <- scm.low %>%
  rep_sample_n(size = 3, reps = 1000) %>%
  group_by(replicate) %>%
  summarise(sample_mean = mean(mgha_1h_mean), sample_sd = sd(mgha_1h_mean))
# View results
head(repeated_samples)
mean(repeated_samples$sample_sd)
