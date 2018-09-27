library(tidyverse)
library(countrycode)
library(readr)
library(sf)
library(spData)

setwd("/Users/sjones/Google Drive/tidyTuesday/Sep25")

threat <- read_csv("./table_2.csv")
source <- read_csv("./table_4.csv")
t3 <- read_csv("./table_3.csv")

threat <- threat %>% mutate(tcostbil = invasion_cost/1000000000)
source <- source %>% mutate(scostbil = invasion_cost/1000000000)
threat$country[threat$country == "United"] <- "United Kingdom"
source$country[source$country == "United"] <- "United Kingdom"
threat$country[threat$country == "Czech"] <- "Czech Republic"
source$country[source$country == "Czech"] <- "Czech Republic"
threat$country[threat$country == "Bosnia"] <- "Bosnia and Herzegovina"
source$country[source$country == "Bosnia"] <- "Bosnia and Herzegovina"

threat$continent <- factor(countrycode(sourcevar = threat$country,
                             origin = "country.name",
                             destination = "continent"))

#Need to put in Czech Republic by hand
threat$continent[threat$country == "Czech"] <- "Europe"

cost_df <- left_join(threat, source, by = "country")
#cost_df <- merge(cost_df, t3[, c("country", "gdp_mean")], by = "country")

eur_df <- cost_df %>% filter(continent == "Europe") %>% select(country, tcostbil, scostbil)
#Calculate source - threat & rename country column
eur_df <- eur_df %>% mutate(diffbil = scostbil - tcostbil) %>%
  rename(name_long = country)

eur_sf <- filter(world, continent == "Europe")
eur_mg <- left_join(eur_sf, eur_df, by = "name_long")

dfworldmap = map_data("world")
df <- data.frame(countries = eur_sf$name_long)
dfworldmap2 <- merge(dfworldmap, df, by.x = "region", by.y = "countries")
dfworldmap2 <- filter(dfworldmap, region %in% unique(df$countries)) %>% rename(region = name_long)
eur_mg <- left_join(dfworldmap2, eur_df, by = "name_long")

ggplot(eur_mg) + geom_polygon(aes(long, lat, group=group, fill=diffbil)) + labs(title="Difference Between Source and Threat Costs", fill="Billions of \ndollars")

ggsave(filename = "eur_map.png")