## ----setup, include = FALSE, eval = TRUE--------------------------------------
knitr::opts_chunk$set(echo = TRUE, include = TRUE, eval = TRUE,
                      out.width = "100%", fig.width = 7.2, fig.height = 7.2,
                      dpi = 200)

## ----message = FALSE----------------------------------------------------------
# Load libraries
library(rphylopic)
library(palmerpenguins)
# Get penguin data
data(penguins)

## ----eval = FALSE-------------------------------------------------------------
#  # Pick a silhouette for Pygoscelis (here we pick #2)
#  penguin <- pick_phylopic("Pygoscelis", n = 3, view = 3)

## ----echo = FALSE-------------------------------------------------------------
penguin <- get_phylopic("86334821-42ec-4da1-bb9d-53f3d6941c77")

## -----------------------------------------------------------------------------
# It's a little slanted, so let's rotate it a little bit
penguin_rot <- rotate_phylopic(img = penguin, angle = 15)

## -----------------------------------------------------------------------------
# Subset the data to remove rows with missing sex values
penguins_subset <- subset(penguins, !is.na(sex))
# Split the data by species
species_split <- split(penguins_subset, penguins_subset$species)

## -----------------------------------------------------------------------------
# Set up the plot area
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

# Loop over the species and create a plot for each one
for (i in seq_along(species_split)) {
  species_data <- species_split[[i]]
  plot(x = species_data$bill_length_mm, y = species_data$flipper_length_mm,
       xlab = "Bill length (mm)", ylab = "Flipper length (mm)",
       main = names(species_split)[i],
       xlim = range(penguins_subset$bill_length_mm, na.rm = TRUE),
       ylim = range(penguins_subset$flipper_length_mm, na.rm = TRUE))
}

## -----------------------------------------------------------------------------
# Set up the plot area
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

# Loop over the species and create a plot for each one
for (i in seq_along(species_split)) {
  species_data <- species_split[[i]]
  plot(x = species_data$bill_length_mm, y = species_data$flipper_length_mm,
       xlab = "Bill length (mm)", ylab = "Flipper length (mm)",
       main = names(species_split)[i],
       xlim = range(penguins_subset$bill_length_mm, na.rm = TRUE),
       ylim = range(penguins_subset$flipper_length_mm, na.rm = TRUE))
  if (i == 1) add_phylopic_base(img = penguin_rot, x = 59, y = 215, ysize = 30)
}

## -----------------------------------------------------------------------------
# Set up the plot area
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

# Loop over the species and create a plot for each one
for (i in seq_along(species_split)) {
  species_data <- species_split[[i]]
  plot(NA, xlab = "Bill length (mm)", ylab = "Flipper length (mm)",
       main = names(species_split)[i],
       xlim = range(penguins_subset$bill_length_mm, na.rm = TRUE),
       ylim = range(penguins_subset$flipper_length_mm, na.rm = TRUE))
  add_phylopic_base(img = penguin_rot,
                    x = species_data$bill_length_mm,
                    y = species_data$flipper_length_mm)
}

## -----------------------------------------------------------------------------
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

for (i in seq_along(species_split)) {
  species_data <- species_split[[i]]
  plot(NA, xlab = "Bill length (mm)", ylab = "Flipper length (mm)",
       main = names(species_split)[i],
       xlim = range(penguins_subset$bill_length_mm, na.rm = TRUE),
       ylim = range(penguins_subset$flipper_length_mm, na.rm = TRUE))
  add_phylopic_base(img = penguin_rot,
                    x = species_data$bill_length_mm,
                    y = species_data$flipper_length_mm,
                    ysize = species_data$body_mass_g /
                      max(penguins_subset$body_mass_g, na.rm = TRUE) * 8)
}

## -----------------------------------------------------------------------------
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

for (i in seq_along(species_split)) {
  species_data <- species_split[[i]]
  plot(NA, xlab = "Bill length (mm)", ylab = "Flipper length (mm)",
       main = names(species_split)[i],
       xlim = range(penguins_subset$bill_length_mm, na.rm = TRUE),
       ylim = range(penguins_subset$flipper_length_mm, na.rm = TRUE))
  add_phylopic_base(img = penguin_rot,
                    x = species_data$bill_length_mm,
                    y = species_data$flipper_length_mm,
                    ysize = species_data$body_mass_g /
                      max(penguins_subset$body_mass_g, na.rm = TRUE) * 8,
                    color = ifelse(species_data$sex == "male", "blue", "orange"))
}

# Add a legend to the last plot
legend("bottomright", legend = c("Female", "Male"), pch = 20,
       col = c("orange", "blue"), bty = "n")

## ----message = FALSE, warning = FALSE-----------------------------------------
# Load libraries
library(rphylopic)
library(maps)
library(palaeoverse)
# Get occurrence data
data(tetrapods)

## -----------------------------------------------------------------------------
# Subset to desired group
tetrapods <- subset(tetrapods, genus == "Mesosaurus")

## ----fig.height = 5, message = FALSE------------------------------------------
# Plot map
map("world", col = "lightgrey", fill = TRUE)
# Plot points
points(x = tetrapods$lng, y = tetrapods$lat, cex = 2, pch = 16,
       col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75))

## ----fig.height = 5, warning = FALSE------------------------------------------
map("world", col = "lightgrey", fill = TRUE)
add_phylopic_base(name = "Mesosaurus", x = tetrapods$lng, y = tetrapods$lat,
                  ysize = 8, color = "blue", alpha = 0.75)

## ----message = FALSE----------------------------------------------------------
# Load libraries
library(rphylopic)
library(ggplot2)
library(phytools)
# Get vertebrate phylogeny and data
data(vertebrate.tree)

## -----------------------------------------------------------------------------
# Make a data.frame for the PhyloPic names
vertebrate_data <- data.frame(species = vertebrate.tree$tip.label, uuid = NA)
# Try to get PhyloPic UUIDs for the species names
vertebrate_data$uuid <- sapply(vertebrate.tree$tip.label,
                               function(x) {
                                 tryCatch(get_uuid(x), error = function(e) NA)
                               })
vertebrate_data

## -----------------------------------------------------------------------------
vertebrate_data$uuid[vertebrate_data$species == "Myotis_lucifugus"] <-
  get_uuid("Vespertilioninae")

## ----eval = FALSE-------------------------------------------------------------
#  # Pick a different boar image; we'll pick #2
#  boar_svg <- pick_phylopic("Sus scrofa", view = 5)
#  # Extract the UUID
#  vertebrate_data$uuid[vertebrate_data$species == "Sus_scrofa"] <-
#    attr(boar_svg, "uuid")

## ----echo = FALSE-------------------------------------------------------------
vertebrate_data$uuid[vertebrate_data$species == "Sus_scrofa"] <-
  "87047da1-b40e-4b31-8492-4db262f129f5"

## -----------------------------------------------------------------------------
vertebrate_data$svg <- lapply(vertebrate_data$uuid, get_phylopic)

## ----message = FALSE----------------------------------------------------------
library(ape)
# Plot the tree
plot(vertebrate.tree)

## -----------------------------------------------------------------------------
library(palaeoverse)
# Plot the tree with a geological timescale on the bottom
plot(vertebrate.tree)
axis_geo_phylo(intervals = "periods")

## -----------------------------------------------------------------------------
plot(vertebrate.tree, show.tip.label = FALSE)
axis_geo_phylo(intervals = "periods")
add_phylopic_base(img = vertebrate_data$svg,
                  x = max(nodeHeights(vertebrate.tree)), y = 1:11, ysize = 0.5)

## -----------------------------------------------------------------------------
vertebrate_data$svg[[1]] <- rotate_phylopic(img = vertebrate_data$svg[[1]])
vertebrate_data$svg[[8]] <- rotate_phylopic(img = vertebrate_data$svg[[8]])

## -----------------------------------------------------------------------------
plot(vertebrate.tree, show.tip.label = FALSE)
axis_geo_phylo(intervals = "periods")
add_phylopic_base(img = vertebrate_data$svg,
                  x = max(nodeHeights(vertebrate.tree)), y = 1:11, ysize = 0.5)

