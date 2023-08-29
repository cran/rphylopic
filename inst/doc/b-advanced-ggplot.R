## ----setup, include = FALSE, eval = TRUE--------------------------------------
knitr::opts_chunk$set(echo = TRUE, include = TRUE, eval = TRUE,
                      out.width = "100%", fig.width = 7.2, fig.height = 7.2,
                      dpi = 200)

## ----message = FALSE----------------------------------------------------------
# Load libraries
library(rphylopic)
library(ggplot2)
library(palmerpenguins)
# Get penguin data and clean it
data(penguins)
penguins_subset <- subset(penguins, !is.na(sex))

## ---- eval = FALSE------------------------------------------------------------
#  # Pick a silhouette for Pygoscelis (here we pick #2)
#  penguin <- pick_phylopic("Pygoscelis", n = 3, view = 3)

## ---- echo = FALSE------------------------------------------------------------
penguin <- get_phylopic("86334821-42ec-4da1-bb9d-53f3d6941c77")

## -----------------------------------------------------------------------------
# It's a little slanted, so let's rotate it a little bit
penguin_rot <- rotate_phylopic(img = penguin, angle = 15)

## -----------------------------------------------------------------------------
ggplot(penguins_subset) +
  geom_point(aes(x = bill_length_mm, y = flipper_length_mm)) +
  labs(x = "Bill length (mm)", y = "Flipper length (mm)") +
  facet_wrap(~species, ncol = 1) +
  theme_bw(base_size = 15)

## -----------------------------------------------------------------------------
silhouette_df <- data.frame(x = 59, y = 215, species = "Adelie")
ggplot(penguins_subset) +
  geom_point(aes(x = bill_length_mm, y = flipper_length_mm)) +
  geom_phylopic(data = silhouette_df, aes(x = x, y = y), size = 30,
                img = penguin_rot) +
  labs(x = "Bill length (mm)", y = "Flipper length (mm)") +
  facet_wrap(~species, ncol = 1) +
  theme_bw(base_size = 15)

## -----------------------------------------------------------------------------
ggplot(penguins_subset) +
  geom_phylopic(img = penguin_rot,
                aes(x = bill_length_mm, y = flipper_length_mm)) +
  labs(x = "Bill length (mm)", y = "Flipper length (mm)") +
  facet_wrap(~species, ncol = 1) +
  theme_bw(base_size = 15)

## -----------------------------------------------------------------------------
ggplot(penguins_subset) +
  geom_phylopic(img = penguin_rot,
                aes(x = bill_length_mm, y = flipper_length_mm,
                    size = body_mass_g)) +
  labs(x = "Bill length (mm)", y = "Flipper length (mm)") +
  facet_wrap(~species, ncol = 1) +
  theme_bw(base_size = 15)

## -----------------------------------------------------------------------------
ggplot(penguins_subset) +
  geom_phylopic(img = penguin_rot,
                aes(x = bill_length_mm, y = flipper_length_mm,
                    size = body_mass_g, color = sex),
                show.legend = TRUE) +
  labs(x = "Bill length (mm)", y = "Flipper length (mm)") +
  scale_size_continuous(guide = "none") +
  scale_color_manual("Sex", values = c("orange", "blue"),
                     labels = c("Female", "Male")) +
  facet_wrap(~species, ncol = 1) +
  theme_bw(base_size = 15) +
  theme(legend.position = c(0.9, 0.9))

## -----------------------------------------------------------------------------
ggplot(penguins_subset) +
  geom_phylopic(img = penguin_rot,
                aes(x = bill_length_mm, y = flipper_length_mm,
                    size = body_mass_g, color = sex),
                show.legend = TRUE,
                key_glyph = phylopic_key_glyph(img = penguin_rot)) +
  labs(x = "Bill length (mm)", y = "Flipper length (mm)") +
  scale_size_continuous(guide = "none") +
  scale_color_manual("Sex", values = c("orange", "blue"),
                     labels = c("Female", "Male")) +
  facet_wrap(~species, ncol = 1) +
  theme_bw(base_size = 15) +
  theme(legend.position = c(0.9, 0.9))

## ----message = FALSE----------------------------------------------------------
# Load libraries
library(rphylopic)
library(ggplot2)
library(palaeoverse)
# Get occurrence data
data(tetrapods)

## -----------------------------------------------------------------------------
# Subset to desired group
tetrapods <- subset(tetrapods, genus == "Mesosaurus")

## ----fig.height = 3.5---------------------------------------------------------
# Get map data
world <- map_data(map = "world")
# Make map
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "darkgrey", linewidth = 0.1) +
  geom_point(data = tetrapods, aes(x = lng, y = lat),
             size = 4, alpha = 0.75, color = "blue") +
  theme_void() +
  coord_sf()

## ----fig.height = 3.5, warning = FALSE----------------------------------------
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "darkgrey", linewidth = 0.1) +
  geom_phylopic(data = tetrapods, aes(x = lng, y = lat, name = genus),
                size = 4, alpha = 0.75, color = "blue") +
  theme_void() +
  coord_sf()

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
library(ggtree)
# Plot the tree
ggtree(vertebrate.tree, size = 1, layout = "circular")

## -----------------------------------------------------------------------------
library(deeptime)
# Plot the tree with a geological timescale in the background
revts(ggtree(vertebrate.tree, size = 1)) +
  scale_x_continuous(breaks = seq(-500, 0, 100),
                     labels = seq(500, 0, -100),
                     limits = c(-500, 0)) +
  coord_geo_polar(dat = "periods") +
  theme(line = element_line(linewidth = 1),
        axis.text.r = element_text(size = 5, hjust = -0.5, vjust = -1.5))

## ----message = FALSE, warning = FALSE-----------------------------------------
revts(ggtree(vertebrate.tree, size = 1)) %<+% vertebrate_data +
  geom_phylopic(aes(img = svg), size = 25) +
  scale_x_continuous(breaks = seq(-500, 0, 100),
                     labels = seq(500, 0, -100),
                     limits = c(-500, 0)) +
  coord_geo_polar(dat = "periods") +
  theme(line = element_line(linewidth = 1),
        axis.text.r = element_text(size = 5, hjust = -0.5, vjust = -1.5))

## -----------------------------------------------------------------------------
vertebrate_data$svg[[1]] <- rotate_phylopic(img = vertebrate_data$svg[[1]])
vertebrate_data$svg[[8]] <- rotate_phylopic(img = vertebrate_data$svg[[8]])

## ----message = FALSE, warning = FALSE-----------------------------------------
revts(ggtree(vertebrate.tree, size = 1)) %<+% vertebrate_data +
  geom_phylopic(aes(img = svg), size = 25) +
  scale_x_continuous(breaks = seq(-500, 0, 100),
                     labels = seq(500, 0, -100),
                     limits = c(-500, 0)) +
  coord_geo_polar(dat = "periods") +
  theme(line = element_line(linewidth = 1),
        axis.text.r = element_text(size = 5, hjust = -0.5, vjust = -1.5))

