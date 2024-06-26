% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_phylopic.R
\name{save_phylopic}
\alias{save_phylopic}
\title{Save a PhyloPic image}
\usage{
save_phylopic(img = NULL, path = NULL, bg = "transparent", ...)
}
\arguments{
\item{img}{\code{picture}. A \link[grImport2:Picture-class]{Picture} object.
For PhyloPic silhouettes, this is generated by
\code{\link[rphylopic]{get_phylopic}} and
\code{\link[rphylopic]{pick_phylopic}}.}

\item{path}{\code{character}. The output file path. The name of the file
and file extension should be included (e.g. "./phylopic.png"). If this
argument is \code{NULL} (default), the file is automatically saved in the
current working directory as a png file.}

\item{bg}{\code{character}. The desired background color. By default this
argument is set to "transparent". Note: .jpeg files are always exported
with a background colour.}

\item{...}{Additional arguments available for the called grDevices
function. The called function is determined by the file extension used in
\code{path} (e.g. \code{\link[grDevices]{png}}).}
}
\value{
An image is saved. Nothing is returned to the R interface.
}
\description{
\code{save_phylopic} is a convenient function for saving a PhyloPic silhouette
fetched via \code{\link[rphylopic]{get_phylopic}}. Available formats for
saving are those available in the \link{grDevices} package (i.e. pdf, png, svg,
tiff, jpeg, and bmp).
}
\details{
This function can be used to save a PhyloPic image obtained via
the \code{\link[rphylopic]{get_phylopic}} or any object of class
\code{Picture} from the grImport2 package. Any file format available in the
\link{grDevices} package can be used (i.e. pdf, png, svg, tiff, jpeg, and
bmp).
}
\examples{
\dontrun{
# Get image data
img <- get_phylopic("27356f15-3cf8-47e8-ab41-71c6260b2724")
# Save file
save_phylopic(img = img, path = "./phylopic.png",
              width = 500, height = 500)
}
}
