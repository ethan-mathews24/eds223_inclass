# Install Packages -------------------------------------------------------

install.packages("here")
install.packages("tidyverse")
install.packages("sf")
install.packages("geodata")
install.packages("kableExtra")

packages <- c(
  "here",
  "janitor",
  "tidyverse",
  "sf",
  "terra",
  "tmap",
  "spData",
  "geodata",
  "kableExtra",
  "viridisLite"
)
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

install.packages(
  "spDataLarge",
  repos = c("https://geocompx.r-universe.dev", "https://cloud.r-project.org")
)


# Read in Packages -------------------------------------------------------

library(tidyverse)
library(here)
library(kableExtra)
library(janitor)
library(sf)
library(terra)
library(spData)
library(geodata)
library(spDataLarge)


# Read in Data -----------------------------------------------------------

gdw_df <- read_csv(here("Week0", "data", "gdw.csv")) |> 
  clean_names() # coverting the variable names to lower snake case

# Chr ( text ) are country, river, dam name
# Dbl ( numeric values ) are dam height and capacity
# Lgl ( logical varialbes ) TRUE/FALSE


# Data Exploration -------------------------------------------------------


head(gdw_df, n = 10) |> 
  kable()

tail(gdw_df, n = 10) |> 
  kable()

dim(gdw_df)
nrow(gdw_df)
ncol(gdw_df)

names(gdw_df)



# Indexing ---------------------------------------------------------------

country_df <- gdw_df[, "country"]

country_vec <- gdw_df[['country']]

gdw_df |> 
  group_by(dam_type) |> 
  summarise(count = n()) |> 
  ungroup()

sub_dam <- gdw_df |> 
  filter(dam_type == "Dam")

gdw_df <- gdw_df |> 
  arrange(year_dam)

# to do it in descending, do arrange(desc(data))


# Data Visualization -----------------------------------------------------


gdw_df |>
  group_by(country) |>
  summarize(mean_dam_hgt_m = mean(dam_hgt_m, na.rm = TRUE)) |>
  ungroup() |> 
  ggplot(aes(x = country, y = mean_dam_hgt_m)) +
    geom_bar(stat = "identity") +
    labs(x = "Country",
         y = "Average height of dam/barrier in meters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



ggplot(data = gdw_df,
      aes(x = cap_mcm, y = dam_hgt_m)) +
  geom_point() +
  labs(x = "Storage capacity of reservoir in million cubic meters",
       y = "Height of dam/barrier in meters") +
  theme_minimal()

