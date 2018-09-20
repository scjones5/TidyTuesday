library(dplyr)
library(ggplot2)
library(broom)

flights <- read.csv("/Users/sjones/Google Drive/tidyTuesday/Sep18/us-airports.csv", header = TRUE)

levels(flights$airport_name)[levels(flights$airport_name)=="Dallas/Fort Worth International"] <- "Dallas-Fort Worth International"
df_names <- levels(flights$hub_type)
future_years <- c(2018,2019,2020,2021,2022)
for(c in 1:length(df_names)) {
  hub <- flights %>% filter(hub_type == df_names[c]) 
  # Find the top four by passenger in each hub type
  hi_hub <- hub %>% 
    select(year, passengers, airport_name) %>%
    group_by(year) %>%
    arrange(desc(passengers)) %>%
    top_n(4, passengers) 
  ports <- hi_hub$airport_name[hi_hub$year == 2017]
  ports <- droplevels(ports)
  hub2 <- data.frame(airport_name = rep(ports, times = 5), year = c(2018,2019,2020,2021,2022),
                     passengers = 0)
  hub1 <- hub %>%
    filter(as.character(airport_name) %in% ports) %>%
    group_by(airport_name) %>%
    nest() %>%
    inner_join(hub2 %>% select(-passengers) %>% group_by(airport_name) %>% nest(),
               by = c("airport_name")) %>%
    mutate(model = data.x %>% map(~lm(passengers ~ year, data=.)),
           value = map2(model, data.y, predict)) 
  
  assign(paste0(df_names[c], "_mod"), hub1)
  hubOut <- hub1 %>%
    select(-data.x, -model) %>%
    unnest() %>%
    bind_rows(hub)
  
  assign(df_names[c], hubOut)
}

Lslope <- c()
Lints <- c()
for(p in Large_mod$airport_name) {
  Lints <- c(Lints, Large_mod$model[Large_mod$airport_name == p][[1]]$coefficients[1])
  Lslope <- c(Lslope, Large_mod$model[Large_mod$airport_name == p][[1]]$coefficients[2])
}

Large_mod <- Large_mod %>%
  mutate(slope = Lslope) %>%
  mutate(intercept = Lints)

new_Large <- merge(Large, Large_mod, by = "airport_name")
new_Large %>% 
  filter(airport_name %in% Large_mod$airport_name) %>%
  select(airport_name, year, value.x, passengers, slope, intercept) %>%
  ggplot(aes(year)) +
    geom_point(aes(y = value.x)) +
    geom_point(aes(y = passengers)) +
    geom_abline(aes(intercept = intercept, slope = slope)) + 
    facet_wrap(~ airport_name) + 
    scale_x_continuous(breaks = unique(new_Large$year)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ylab("Passengers") + 
    labs(title="Large Airports")
ggsave(filename = "/Users/sjones/Google Drive/tidyTuesday/Sep18/large_flights.png")

Mslope <- c()
Mints <- c()
for(p in Medium_mod$airport_name) {
  Mints <- c(Mints, Medium_mod$model[Medium_mod$airport_name == p][[1]]$coefficients[1])
  Mslope <- c(Mslope, Medium_mod$model[Medium_mod$airport_name == p][[1]]$coefficients[2])
}

Medium_mod <- Medium_mod %>%
  mutate(slope = Mslope) %>%
  mutate(intercept = Mints)

new_Medium <- merge(Medium, Medium_mod, by = "airport_name")
new_Medium %>% 
  filter(airport_name %in% Medium_mod$airport_name) %>%
  select(airport_name, year, value.x, passengers, slope, intercept) %>%
  ggplot(aes(year)) +
  geom_point(aes(y = value.x)) +
  geom_point(aes(y = passengers)) +
  geom_abline(aes(intercept = intercept, slope = slope)) + 
  facet_wrap(~ airport_name) + 
  scale_x_continuous(breaks = unique(new_Medium$year)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("Passengers") + 
  labs(title="Medium Airports")
ggsave(filename = "/Users/sjones/Google Drive/tidyTuesday/Sep18/med_flights.png")


Sslope <- c()
Sints <- c()
for(p in Small_mod$airport_name) {
  Sints <- c(Sints, Small_mod$model[Small_mod$airport_name == p][[1]]$coefficients[1])
  Sslope <- c(Sslope, Small_mod$model[Small_mod$airport_name == p][[1]]$coefficients[2])
}

Small_mod <- Small_mod %>%
  mutate(slope = Sslope) %>%
  mutate(intercept = Sints)

new_Small <- merge(Small, Small_mod, by = "airport_name")
new_Small %>% 
  filter(airport_name %in% Small_mod$airport_name) %>%
  select(airport_name, year, value.x, passengers, slope, intercept) %>%
  ggplot(aes(year)) +
  geom_point(aes(y = value.x)) +
  geom_point(aes(y = passengers)) +
  geom_abline(aes(intercept = intercept, slope = slope)) + 
  facet_wrap(~ airport_name) + 
  scale_x_continuous(breaks = unique(new_Small$year)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("Passengers") + 
  labs(title="Small Airports")
ggsave(filename = "/Users/sjones/Google Drive/tidyTuesday/Sep18/small_flights.png")
