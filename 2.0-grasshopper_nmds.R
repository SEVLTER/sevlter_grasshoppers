library(ggplot2)
library(tidyverse)
library(vegan)

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
grasshoppers_otu <- read_csv("data/interim/sevlter_grasshopper_otutable_uniqueid.csv",
                             na=c('NA','.','#VALUE!','-999','na'))
grasshoppers_otu
colnames(grasshoppers_otu)

grasshopper_otu_covar <- read_csv("data/interim/sevlter_grasshopper_otutable.csv",
                                  na=c('NA','.','#VALUE!','-999','na'))
grasshopper_otu_covar

grasshopper_covar <- grasshopper_otu_covar %>% 
  select(YEAR, SITE, season, ecosystem)
grasshopper_covar

# Here we make the community composition data frame and double check it. 
com_table = grasshoppers_otu[,2:ncol(grasshoppers_otu)]
com_table

# Convert the abundance data frame into a matrix for distance measures. 
com_table_matrix <- as.matrix(com_table)
class(com_table_matrix)

# This runs the NMDS using the vegan package. 
nmds_com = metaMDS(com_table_matrix, distance = "bray")
nmds_com

plot(nmds_com)

# Takes the NMDS scores and adds on the environmental parameters.
# There is a way to do this in tidyverse. Need to look that up. 
site.scrs <- as.data.frame(scores(nmds_com, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, Site = grasshopper_covar$SITE) #add grouping variable "Management" to dataframe
site.scrs <- cbind(site.scrs, Year = grasshopper_covar$YEAR) #add grouping variable of cluster grouping to dataframe
site.scrs <- cbind(site.scrs, Season = grasshopper_covar$season)
site.scrs <- cbind(site.scrs, Ecosystem = grasshopper_covar$ecosystem)
colnames(site.scrs)
site.scrs

# Now we can dump the site.scrs into ggplot for nicer plots.
site.scrs %>% ggplot(., aes(x=NMDS1, y=NMDS2))+ 
  geom_point(aes(colour = factor(Site))) + 
  coord_fixed()+
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), axis.text = element_text(size = 10))

site.scrs %>% ggplot(., aes(x=NMDS1, y=NMDS2))+ 
  geom_point(aes(colour = factor(Site), shape=Season)) + 
  coord_fixed()+
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), axis.text = element_text(size = 10))

site.scrs %>% ggplot(., aes(x=NMDS1, y=NMDS2))+ 
  geom_point(aes(colour = factor(Ecosystem), shape=Season)) + 
  coord_fixed()+
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), axis.text = element_text(size = 10))


