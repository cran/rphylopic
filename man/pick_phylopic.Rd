% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pick_phylopic.R
\name{pick_phylopic}
\alias{pick_phylopic}
\title{Pick a PhyloPic image from available options}
\usage{
pick_phylopic(
  name = NULL,
  n = 5,
  uuid = NULL,
  view = 1,
  filter = NULL,
  auto = NULL
)
}
\arguments{
\item{name}{\code{character}. A taxonomic name. Different taxonomic levels
are supported (e.g. species, genus, family).}

\item{n}{\code{numeric}. How many uuids should be viewed? Depending on the
requested \code{name}, multiple silhouettes may exist. If \code{n} exceeds the number
of available images, all available uuids will be returned. Defaults to 5.
Only relevant if \code{name} supplied.}

\item{uuid}{\code{character}. A vector (or list) of valid PhyloPic
silhouette uuids, such as that returned by \code{\link[=get_uuid]{get_uuid()}} or
\code{\link[=resolve_phylopic]{resolve_phylopic()}}.}

\item{view}{\code{numeric}. Number of silhouettes that should be plotted at
the same time. Defaults to 1.}

\item{filter}{\code{character}. Filter uuid(s) by usage license. Use "by"
to limit results to image uuids which do not require attribution, "nc"
for image uuids which allow commercial usage, and "sa" for image uuids
without a ShareAlike clause. The user can also combine these filters. Only
relevant if \code{name} supplied.}

\item{auto}{\code{numeric}. This argument allows the user to automate input
into the menu choice. If the input value is \code{1}, the first returned image
will be selected. If the input value is \code{2}, requested images will be
automatically cycled through with the final image returned. If the input
value is \code{3}, a list of attribution information for each image is
returned (this functionality is principally intended for testing). If
\code{NULL} (default), the user must interactively respond to the called menu.}
}
\value{
A \link[grImport2:Picture-class]{Picture} object is returned. The uuid of
the selected image is saved as the "uuid" attribute of the returned object
and is also printed to console.
}
\description{
This function provides a visually interactive way to pick an image and valid
uuid for an input taxonomic name. As multiple silhouettes can exist for each
organism in PhyloPic, this function is useful for choosing the right
image/uuid for the user.
}
\details{
This function allows the user to visually select the desired image
from a pool of silhouettes available for the input \code{name}.

Note that while the \code{view} argument can be any positive integer,
weaker/older computers may have issues displaying very large numbers of
images at the same time (i.e. \code{view} > 9). If no images are displayed in
your plotting environment, try decreasing the value of \code{view}.
}
\examples{
\dontrun{
# Defaults pane layout
img <- pick_phylopic(name = "Canis lupus", n = 5)
# 3 x 3 pane layout
img <- pick_phylopic(name = "Scleractinia", n = 9, view = 9)
}
}
