install.packages(c("tidyverse", "janitor", "forecast", "DT"))
library(tidyverse)
library(janitor)
library(forecast)
library(DT)
library(rvest)

# 1_IMPORT ---------------------------------------------------------------------

content <- rvest::read_html("https://www.worldometers.info/world-population/population-by-country/")
df <- rvest::html_table(content) %>% pluck(1)

# 2_CLEAN ----------------------------------------------------------------------

df %>% 
  clean_names() %>%
  select(2:ncol(.)) %>% 
  rename(country = 1,
         pop_2020 = 2,
         yearly_change_per = 3,
         density_km2 = 5,
         area_km2 = 6,
         urban_pop_per = 10) %>% 
  mutate_all(parse_guess) %>% 
  mutate_all(.tbl = ., ~str_replace_all(.,pattern = "N.A.", replacement = "0")) %>% 
  mutate_all(~str_remove_all(., pattern = " %")) %>% 
  mutate_all(~str_replace_na(.,replacement = "0")) %>% 
  mutate_all(parse_guess) -> df

# 3_PREPARE --------------------------------------------------------------------

df %>% 
  arrange(desc(pop_2020)) %>% 
  select(1,2,9,11) %>% 
  mutate(country = str_replace(country, pattern = "United States", replacement = "USA")) %>% 
  mutate(country = str_replace(country, pattern = "United Kingdom", replacement = "UK")) %>%
  mutate(country = as_factor(country),
         country = fct_inorder(country)) %>%
  head(20) %>%
  
  ggplot(aes(x = country,
             y = pop_2020,
             fill = country)) +
  
  geom_bar(stat = "identity", color = "black", show.legend = F) +
  geom_text(aes(label = scales::number(x = pop_2020, scale = .000000001, suffix = " B", accuracy = .01),
                y = pop_2020 + 30000000),
            size = 4) +
  
  scale_y_continuous(labels = scales::number) +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "World Popuplation", subtitle = "(Top 20 Countries)",x = "", y = "") +
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        plot.title = element_text(size = 20, hjust = .5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "mono", hjust = .5))
  
  


















