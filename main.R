require(here)
require(tidyverse)

library(ggmap)
library(gspartial)

library(geosphere)

library(ggridges)

library(igraph)
library(ggraph)

theme_set(theme_bw())

migration <- read_csv(here("data", "migration_export.csv")) |>
  # mutate(
  #  form = list(c(depature_longitude, depature_latitude)),
  #  to = list(c(arrival_longitude, arrival_latitude))
  # ) |>
  mutate(
    reason = ifelse(reason == "NULL", NA, reason) |> as.factor(),
    reason2 = ifelse(reason2 == "NULL", NA, reason2) |> as.factor(),
    depature_city = ifelse(depature_city == "NULL", NA, depature_city),
    arrival_city = ifelse(arrival_city == "NULL", NA, arrival_city),
    year = lubridate::year(start_date)
  ) |>
  rowwise() |>
  mutate(
    travel_distance = distHaversine(
      c_across(c(depature_longitude, depature_latitude)),
      c_across(c(arrival_longitude, arrival_latitude))
    ),
  ) |>
  ungroup() |>
  arrange(start_date) |>
  mutate(
    stay_duration = start_date - lag(start_date),
    .by = c(first_name, family_name)
  )

migration |>
  ggplot(aes(x = travel_distance, y = after_stat(ndensity))) +
  stat_bin(aes(fill = reason)) +
  scale_x_continuous("Travel Distance", labels = scales::label_number(scale_cut = scales::cut_si("m"))) +
  facet_grid(rows = vars(reason)) +
  guides(fill = "none")
ggsave(here("travel_distance.png"))

migration |>
  # filter(reason == "Forced" | reason2 == "Forced") |>
  ggplot(aes(x = stay_duration / 365, y = after_stat(ndensity))) +
  stat_bin(aes(fill = reason)) +
  facet_grid(rows = vars(reason)) +
  scale_x_continuous("Stay Duration [y]") +
  guides(fill = "none")
ggsave(here("stay_duration.png"))

migration |>
  ggplot(aes(x = start_date, color = reason, y = travel_distance)) +
  geom_point(aes()) +
  geom_smooth() +
  scale_y_continuous(limits = c(0, NA))
ggsave(here("lm-distance.png"))

migration |>
  ggplot(aes(x = start_date, color = reason)) +
  geom_point(aes(y = stay_duration / 365)) +
  scale_y_continuous("Stay [y]")
ggsave(here("start-stay_duration.png"))

personal_migration <- migration |>
  summarize(
    count = n(),
    # starting_point = depature_city,
    path = paste0(coalesce(arrival_city, "?"), collapse = "-"),
    travel_distance = sum(travel_distance),
    .by = c(first_name, family_name)
  ) |>
  arrange(desc(count))

personal_migration |>
  ggplot(aes(x = travel_distance)) +
  stat_bin()

personal_migration |>
  ggplot(aes(x = travel_distance, y = count)) +
  geom_point() +
  geom_smooth() +
  coord_flip() +
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_si("m")))
ggsave(here("lm-distance-per-count-2"))

personal_migration |>
  ggplot(aes(y = travel_distance, x = count)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("m")))
ggsave(here("lm-distance-per-count"))

relationship <- read_csv(
  here("data", "relationship_export.csv")
) |>
  mutate(
    relationshiptype = ifelse(Relationshiptype == "NULL", NA, Relationshiptype) |> as.factor(),
    relationshiptype_precise = ifelse(relationshiptype_precise == "NULL", NA, relationshiptype_precise) |> as.factor(),
    start_date = ifelse(start_date == "NULL", NA, start_date),
    end_date = ifelse(end_date == "NULL", NA, end_date),
    .keep = "unused"
  )

data <- relationship |>
  left_join(
    rename_with(migration, ~ paste("active", ., sep = ".")),
    by = join_by(active_firstname == active.first_name, active_familyname == active.family_name),
    relationship = "many-to-many"
  ) |>
  left_join(
    rename_with(migration, ~ paste("passive", ., sep = ".")),
    by = join_by(passive_firstname == passive.first_name, passive_familyname == passive.family_name),
    relationship = "many-to-many"
  )

migration |>
  mutate(
    reason = forcats::fct_recode(
      reason,
      Bildung = "Education",
      Familie = "Family",
      Zwang = "Forced",
      Arbeit = "Labour",
      Sonstiges = "Other"
    )
  ) |>
  ggplot(aes(x = year, fill = reason)) +
  stat_bin(binwidth = 10, color = "black") +
  labs("Theatermigration im 19. Jahrhundert", x = "Jahr", y = "Anzahl Migrationen", fill = "Ursache")
ggsave("start_date.png")

unique(data$relationshiptype)

data |>
  filter(relationshiptype == "family relation") |>
  View()

data |>
  filter(abs(active.start_date - passive.start_date) < 365 & active.depature_city == passive.depature_city & active.arrival_city == passive.arrival_city)


migration |>
  filter(!is.na(depature_city)) |>
  filter(n() >= 3, .by = c(depature_city)) |>
  ggplot(aes(x = start_date, y = depature_city, fill = ..x..)) +
  geom_point() +
  geom_density_ridges_gradient() +
  guides(fill = "none")
ggsave("migration-from.png")

migration |>
  filter(!is.na(arrival_city)) |>
  filter(n() >= 3, .by = c(arrival_city)) |>
  ggplot(aes(x = start_date, y = arrival_city, fill = ..x..)) +
  geom_point() +
  geom_density_ridges_gradient() +
  guides(fill = "none")
ggsave("migration-to.png")

g <- migration |>
  filter(!(is.na(depature_city) | is.na(arrival_city))) |>
  group_by(depature_city, arrival_city) |>
  summarize(weight = n(), .groups = "keep") |>
  filter(weight > 1) |>
  graph_from_data_frame(directed = T)

V(g)$color <- rainbow(length(unique(V(g)$name)))[rank(degree(g))]
V(g)$size <- 0
# E(g)$width <- E(g)$weight * 2
E(g)$curved <- 0.2
E(g)$color_tail <- rainbow(max(E(g)$weight))[E(g)$weight]
E(g)$color_head <- heat.colors(max(E(g)$weight))[E(g)$weight]


plot(
  g,
  # layout = layout_on_grid(g),
  layout = layout_nicely(g),
  # vertex.size = 10,
  # vertex.label.cex = 1.2,
  edge.arrow.size = .5,
  main = "Graph Visualization of City Connections"
)

data |>
  mutate(
    start_date = as.Date(start_date),
    end_date = as.Date(end_date)
  ) |>
  filter(!is.na(start_date) | !is.na(end_date)) |>
  ggplot(aes(x = start_date, xend = end_date, y = active_familyname)) +
  geom_segment(aes(yend = active_familyname), color = "blue") +
  labs(title = "Timeline of Theatre Migrants", x = "Time", y = "Individual") +
  scale_x_continuous() +
  theme_minimal()

kmeans_result <- migration |>
  select(travel_distance, reason, stay_duration) |>
  filter(!is.na(travel_distance) & !is.na(stay_duration) & !is.na(reason)) |>
  mutate(stay_duration = as.double(stay_duration / 365), reason = as.integer(reason)) |>
  kmeans(centers = 5)

migration |>
  filter(!is.na(travel_distance) & !is.na(stay_duration)) |>
  mutate(cluster = factor(kmeans_result$cluster)) |>
  ggplot(aes(x = travel_distance, y = stay_duration, color = cluster)) +
  geom_point(aes(shape = reason)) +
  labs() +
  theme_minimal() +
  guides(color = "none")
ggsave("kmeans-stay-travel.png")

kmeans_result <- migration |>
  select(travel_distance, reason, stay_duration, start_date) |>
  filter(!is.na(travel_distance) & !is.na(stay_duration) & !is.na(reason)) |>
  mutate(
    stay_duration = as.double(stay_duration / 365),
    start_date = as.double.POSIXlt(start_date),
    reason = as.integer(reason)
  ) |>
  kmeans(centers = 5)

migration |>
  filter(!is.na(travel_distance) & !is.na(stay_duration)) |>
  mutate(cluster = factor(kmeans_result$cluster)) |>
  ggplot(aes(x = start_date, y = travel_distance, size = is.double(stay_duration), color = cluster)) +
  geom_point(aes(shape = reason)) +
  labs() +
  theme_minimal() +
  guides(color = "none")
ggsave("kmeans-start-travel.png")


migration |>
  filter(!is.na(start_date)) |>
  count(year) |>
  ggplot(aes(x = year, y = n)) +
  geom_line()



data |>
  mutate(
    from = paste(active_firstname, active_familyname),
    to = paste(passive_firstname, passive_familyname),
    relationship = relationshiptype
  ) |>
  select(from, to, relationship) |>
  filter(!is.na(relationship)) |>
  graph_from_data_frame(directed = TRUE) |>
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = as.numeric(relationship)), show.legend = FALSE) +
  # geom_node_point(color = "blue", size = 5) +
  # geom_node_text(aes(label = name), vjust = 1.5, hjust = 1.5) +
  theme_void() +
  labs(title = "Network of Theatre Migrants")


ggraph(graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = relationship), show.legend = FALSE) +
  geom_node_point(color = "blue", size = 5) +
  # geom_node_text(aes(label = name), vjust = 1.5, hjust = 1.5) +
  theme_void() +
  labs(title = "Network of Theatre Migrants")
ggsave("network.png")


register_stadiamaps("6776a65b-0360-43c0-8c4f-06d661e69baf", write = FALSE)

ggmap(
  # get_map(
  #  source = "stadia",
  get_stadiamap(
    maptype = "stamen_toner_lite",
    # bbox = c(left = min(migration$depature_longitude), right = max(migration$depature_longitude), bottom = min(migration$depature_latitude), top = max(migration$depature_latitude)),
    bbox = c(left = -14, right = 46, bottom = 32, top = 62),
    # location = c(lon = mean(migration$depature_longitude), lat = mean(migration$depature_latitude)),
    zoom = 4
  )
) +
  geom_point(data = migration, aes(x = depature_longitude, y = depature_latitude), color = "green") +
  geom_point(data = migration, aes(x = arrival_longitude, y = arrival_latitude), color = "red") +
  geom_segment(data = migration, aes(x = depature_longitude, y = depature_latitude, xend = arrival_longitude, yend = arrival_latitude, color = year)) +
  scale_color_viridis() +
  facet_wrap(~cluster) +
  guides(color = "none") +
	labs(x = NULL, y = NULL)
ggsave("map-eu.png")

migration <- migration |>
  mutate(cluster = if_else(year < 1850, "1800-1850", if_else(year < 1900, "1850-1900", "1900-1950")))

ggmap(
  # get_map(
  #  source = "stadia",
  get_stadiamap(
    maptype = "stamen_toner_lite",
    # bbox = c(left = min(migration$depature_longitude), right = max(migration$depature_longitude), bottom = min(migration$depature_latitude), top = max(migration$depature_latitude)),
    bbox = c(left = -170, right = 170, bottom = -60, top = 80),
    # location = c(lon = mean(migration$depature_longitude), lat = mean(migration$depature_latitude)),
    zoom = 2
  )
) +
  geom_point(data = migration, aes(x = depature_longitude, y = depature_latitude), color = "green") +
  geom_point(data = migration, aes(x = arrival_longitude, y = arrival_latitude), color = "red") +
  geom_segment(data = migration, aes(x = depature_longitude, y = depature_latitude, xend = arrival_longitude, yend = arrival_latitude, color = start_date)) +
  scale_color_viridis() +
  facet_grid(rows = vars(cluster)) +
	guides(color = "none")
ggsave("map.png")
# 	geom_segment(aes(x = min(depature_longitude), y = min(depature_latitude),
#                   xend = max(depature_longitude), yend = max(depature_latitude)),
#               arrow = arrow(type = "closed", length = unit(0.2, "inches")),
#               color = "blue", size = 1)


write_csv(data, here("joined.csv"))
