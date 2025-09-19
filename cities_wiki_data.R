library(rvest)
library(dplyr)
library(ggplot2)
library(stringr)

#getting the data from a wikipedia page
url <- "https://en.wikipedia.org/wiki/List_of_cities_proper_by_population_density"
webpage <- read_html(url)

cities_table <- webpage |>
  html_element("table.wikitable")
#city name
cities <- cities_table |>
  html_elements("tr") |>
  html_elements("td:nth-child(1)") |>
  html_text(trim = TRUE)
#population
pop <- cities_table |>
  html_elements("tr") |>
  html_elements("td:nth-child(2)") |>
  html_text(trim = TRUE)
#area in km2 
area_km2 <- cities_table |>
  html_elements("tr") |>
  html_elements("td:nth-child(3)") |>
  html_text(trim = TRUE)
#countries name without notes from the wikipedia
countries <- cities_table |>
  html_elements("tr") |>
  html_elements("td:nth-child(7)") |>
  html_text(trim = TRUE) |>
  str_remove_all("note 1") |>
  str_trim()
  
#creating table with required columns
cities_data <- tibble(
  city = cities,
  population = pop,
  area = area_km2,
  country = countries
)

#cleaning and structuring the data
cities_data <- cities_data |>
  filter(city != "")

cities_data <- cities_data |>
  mutate(
    population = as.numeric(str_remove_all(population, ",")),
    area = as.numeric(str_remove_all(area, ",")),
    country = str_remove_all(country, " \\[.*?\\]$") |> str_trim()
  )
#removing empty data
cities_data <- na.omit(cities_data)
#printing table head and saving the data as a csv file
print(head(cities_data))
write.csv(cities_data, "data/cities_data.csv", row.names = FALSE)
#analysis and plotting
cities_data <- cities_data |>
  mutate(density = population / area)

top_5_by_pop <- cities_data |>
  arrange(desc(population)) |>
  head(5)

ggplot(top_5_by_pop, aes(x = reorder(city, - population / 1000000), y = (population / 1000000), fill = country)) + geom_bar(stat = "identity") + labs(
    title = "top 5 cities by population", 
    x = "city", 
    y = "population (in millions)", 
    fill = "country") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/top_5_by_pop.png")

ggplot(cities_data, aes(x = area, y = (population / 1000000), color = country)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Population and Area Comparison",
    x = "Area (km²)",
    y = "Population (in millions)"
  ) +
  theme_classic()

ggsave("figures/pop_area.png")

  