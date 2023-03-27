library(ggplot2)
library(tidyverse)

# Working directory should be set using Session -> Set Working Directory. Not hard coded. 
# Better practices suggest your file structure look like this:
# .
# └── Project name/
#   ├── data/
#   │   ├── external
#   │   ├── fake
#   │   ├── interim
#   │   ├── processed
#   │   └── raw
#   ├── docs
#   ├── models
#   └── reports/
#       ├── images
#       └── graphs

# This sets a global theme for all my plots so they are not the default
# assault against design. 
theme_set(theme_bw() +
            theme(
              plot.background = element_blank()
              ,panel.grid.major = element_blank()
              ,panel.grid.minor = element_blank()
              ,panel.background = element_blank()
              ,panel.border = element_blank()
              ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
              ,axis.ticks = element_blank()
            ))
# This reads in the csv. NOTE you may need to use different na values. 
grasshoppers_raw <- read_csv("data/raw/sev106_grasshopper_counts.csv",
                      na=c('NA','.','#VALUE!','-999','na'))
grasshoppers_raw
colnames(grasshoppers_raw)

# Here I am interested in looking at the ecosystem and year level.
# What are the total counts of each species in an ecosystem by year.
# NOTE this data is in the long format. 
grasshoppers_by_ecosystem <- grasshoppers_raw %>% 
  group_by(YEAR, SITE, season, ecosystem, SPECIES) %>% 
  tally()
grasshoppers_by_ecosystem

# This is a quick plot to look at how the species counts change over time.
# This could also be a heatmap however the data is sparse. 
grasshoppers_by_ecosystem %>% 
  ggplot(., aes(x=SPECIES, y=ecosystem)) + 
  geom_point(aes(size=n)) + facet_wrap(~YEAR)

# This sets up a standard OTU table site is by row and species are the columns. There
# is covariate data attached as well. YEAR, season, ecosystem. You will need to drop
# those columns for some community analysis. 
grasshopper_otutable <- grasshoppers_by_ecosystem %>% 
  pivot_wider(names_from = SPECIES, values_from = n) %>%
  replace(is.na(.), 0)
grasshopper_otutable

# This writes out the otu table as a csv. 
write.table(grasshopper_otutable, "data/interim/sevlter_grasshopper_otutable.csv",
            sep=",", row.names = FALSE, quote = FALSE, na = ".")

# This will create a unique sampled ID for each row.
grasshopper_otutable_unique_id <- grasshopper_otutable %>% 
  unite(YEAR,SITE,season,ecosystem)
grasshopper_otutable_unique_id
colnames(grasshopper_otutable_unique_id)

# Some community analysis software will only take a single column that is not species counts.
# Here we are writing this out so we can read it in later. 
write.table(grasshopper_otutable_unique_id, "data/interim/sevlter_grasshopper_otutable_uniqueid.csv",
            sep=",", row.names = FALSE, quote = FALSE, na = ".")
