library(dplyr)
library(googleVis)

pets <- read.csv("/Users/sjones/Google Drive/tidyTuesday/Sep11/cats_vs_dogs.csv", header = TRUE)

all_animals <- pets %>%
  select(state, dog_population, cat_population) %>%
  mutate(diff_total = dog_population - cat_population)

total_chart <- gvisGeoChart(all_animals, "state", "diff_total",
                                options=list(region="US", 
                                             displayMode="regions", 
                                             resolution="provinces",
                                             colorAxis="{colors:[\'green', \'white', \'purple']}",
                                             width=600, height=400))
print(total_chart, file="/Users/sjones/Google Drive/tidyTuesday/Sep11/total_animal_difference.html")

by_household <- pets %>%
  select(state, avg_dogs_per_household, avg_cats_per_household) %>%
  mutate(diff_hh = avg_dogs_per_household - avg_cats_per_household)

hh_chart <- gvisGeoChart(by_household, "state", "diff_hh",
                            options=list(region="US", 
                                         displayMode="regions", 
                                         resolution="provinces",
                                         colorAxis="{colors:[\'green', \'white', \'purple']}",
                                         width=600, height=400))

print(hh_chart, file="/Users/sjones/Google Drive/tidyTuesday/Sep11/avg_by_household_difference.html")
