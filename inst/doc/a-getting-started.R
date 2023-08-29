## ----setup, include = FALSE, eval = TRUE--------------------------------------
knitr::opts_chunk$set(echo = TRUE, include = TRUE, eval = TRUE,
                      out.width = "100%", fig.width = 7.2, fig.height = 5,
                      dpi = 200)

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("rphylopic")

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("remotes")
#  remotes::install_github("palaeoverse-community/rphylopic")

## ----message = FALSE----------------------------------------------------------
library(rphylopic)

## -----------------------------------------------------------------------------
citation("rphylopic")

## -----------------------------------------------------------------------------
# Load rphylopic
library(rphylopic)
# Get a single image UUID for a species
uuid <- get_uuid(name = "Canis lupus")
# Get the image for that UUID
img <- get_phylopic(uuid = uuid)
# But multiple silhouettes can exist per species...
uuid <- get_uuid(name = "Canis lupus", n = 5)

## ---- eval = FALSE------------------------------------------------------------
#  # How do I pick?!
#  # It's difficult without seeing the image itself, let's use:
#  img <- pick_phylopic(name = "Canis lupus", n = 4, view = 4)

## ----warning = FALSE----------------------------------------------------------
# OK, now we've got the image we want... let's add it to a plot!
plot(x = 1, y = 1, type = "n", ann = FALSE)
add_phylopic_base(img = img, x = 1.25, y = 1.25, ysize = 0.25)

# But can't we just add an image straight away using the UUID? Sure!
uuid <- get_uuid(name = "Canis lupus", n = 1)
add_phylopic_base(uuid = uuid, x = 1, y = 1, ysize = 0.25)

# What about just using the first image linked to the name? Definitely!
add_phylopic_base(name = "Canis lupus", x = 0.75, y = 0.75, ysize = 0.25)

## ----warning = FALSE----------------------------------------------------------
library(ggplot2)
p <- ggplot() +
  coord_cartesian(xlim = c(0.6, 1.4), ylim = c(0.6, 1.4)) +
  add_phylopic(img = img, x = 1.25, y = 1.25, ysize = 0.25)

# But can't we just add an image straight away using the UUID? Sure!
uuid <- get_uuid(name = "Canis lupus", n = 1)
p <- p + add_phylopic(uuid = uuid, x = 1, y = 1, ysize = 0.25)

# What about just using the first image linked to the name? Definitely!
p + add_phylopic(name = "Canis lupus", x = 0.75, y = 0.75, ysize = 0.25)

## -----------------------------------------------------------------------------
# Flip silhouette horizontally
img_flip <- flip_phylopic(img = img, horizontal = TRUE, vertical = FALSE)

## -----------------------------------------------------------------------------
# Rotate silhouette by 45 degrees
img_rot <- rotate_phylopic(img = img, angle = 45)

## -----------------------------------------------------------------------------
# Change color to blue and transparency to 50%
img_col <- recolor_phylopic(img = img, alpha = 0.5,
                            color = "blue", fill = "blue")

## -----------------------------------------------------------------------------
ggplot() +
  coord_cartesian(xlim = c(0.6, 1.4), ylim = c(0.6, 1.4)) +
  add_phylopic(img = img_flip, x = 1.25, y = 1.25, ysize = 0.25) +
  add_phylopic(img = img_rot, x = 1, y = 1, ysize = 0.25) +
  add_phylopic(img = img_col, x = 0.75, y = 0.75, ysize = 0.25,
               color = "original")

## -----------------------------------------------------------------------------
# Get valid uuid
uuid <- get_uuid(name = "Nycticebus")
# Get attribution data for uuid
get_attribution(uuid = uuid)

## -----------------------------------------------------------------------------
# Get valid uuid
uuid <- get_uuid(name = "Nycticebus")
# Get attribution data for uuid
get_attribution(uuid = uuid, text = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  # How do I save an image?
#  # Get image
#  img <- pick_phylopic(name = "Phascolarctos cinereus", n = 1)
#  # Save image
#  save_phylopic(img = img)

