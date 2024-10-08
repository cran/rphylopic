% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_phylopic.R
\name{geom_phylopic}
\alias{geom_phylopic}
\title{Geom for adding PhyloPic silhouettes to a plot}
\usage{
geom_phylopic(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = TRUE,
  remove_background = TRUE,
  verbose = FALSE,
  filter = NULL
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this layer.
When using a \verb{geom_*()} function to construct a layer, the \code{stat}
argument can be used the override the default coupling between geoms and
stats. The \code{stat} argument accepts the following:
\itemize{
\item A \code{Stat} ggproto subclass, for example \code{StatCount}.
\item A string naming the stat. To give the stat as a string, strip the
function name of the \code{stat_} prefix. For example, to use \code{stat_count()},
give the stat as \code{"count"}.
\item For more information and other ways to specify the stat, see the
\link[ggplot2:layer_stats]{layer stat} documentation.
}}

\item{position}{A position adjustment to use on the data for this layer. This
can be used in various ways, including to prevent overplotting and
improving the display. The \code{position} argument accepts the following:
\itemize{
\item The result of calling a position function, such as \code{position_jitter()}.
This method allows for passing extra arguments to the position.
\item A string naming the position adjustment. To give the position as a
string, strip the function name of the \code{position_} prefix. For example,
to use \code{position_jitter()}, give the position as \code{"jitter"}.
\item For more information and other ways to specify the position, see the
\link[ggplot2:layer_positions]{layer position} documentation.
}}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}'s \code{params} argument. These
arguments broadly fall into one of 4 categories below. Notably, further
arguments to the \code{position} argument, or aesthetics that are required
can \emph{not} be passed through \code{...}. Unknown arguments that are not part
of the 4 categories below are ignored.
\itemize{
\item Static aesthetics that are not mapped to a scale, but are at a fixed
value and apply to the layer as a whole. For example, \code{colour = "red"}
or \code{linewidth = 3}. The geom's documentation has an \strong{Aesthetics}
section that lists the available options. The 'required' aesthetics
cannot be passed on to the \code{params}. Please note that while passing
unmapped aesthetics as vectors is technically possible, the order and
required length is not guaranteed to be parallel to the input data.
\item When constructing a layer using
a \verb{stat_*()} function, the \code{...} argument can be used to pass on
parameters to the \code{geom} part of the layer. An example of this is
\code{stat_density(geom = "area", outline.type = "both")}. The geom's
documentation lists which parameters it can accept.
\item Inversely, when constructing a layer using a
\verb{geom_*()} function, the \code{...} argument can be used to pass on parameters
to the \code{stat} part of the layer. An example of this is
\code{geom_area(stat = "density", adjust = 0.5)}. The stat's documentation
lists which parameters it can accept.
\item The \code{key_glyph} argument of \code{\link[ggplot2:layer]{layer()}} may also be passed on through
\code{...}. This can be one of the functions described as
\link[ggplot2:draw_key]{key glyphs}, to change the display of the layer in the legend.
}}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{FALSE}, the default, never includes, \code{NA} includes if any aesthetics are
mapped, and \code{TRUE} always includes. It can also be a named logical vector
to finely select the aesthetics to display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{remove_background}{\code{logical}. Should any white background be
removed from the silhouette(s)? See \code{\link[=recolor_phylopic]{recolor_phylopic()}} for details.}

\item{verbose}{\code{logical}. Should the attribution information for the
used silhouette(s) be printed to the console (see \code{\link[=get_attribution]{get_attribution()}})?}

\item{filter}{\code{character}. Filter by usage license if using the \code{name}
aesthetic. Use "by" to limit results to images which do not require
attribution, "nc" for images which allows commercial usage, and "sa" for
images without a ShareAlike clause. The user can also combine these filters
as a vector.}
}
\description{
This geom acts like \code{\link[ggplot2:geom_point]{ggplot2::geom_point()}}, except that the specified
silhouettes are used as points. Silhouettes can be specified by their \code{name},
\code{uuid}, or image objects (\code{img}).
}
\details{
One (and only one) of the \code{img}, \code{name}, or \code{uuid} aesthetics must
be specified. The \code{img} aesthetic can be
\link[grImport2:Picture-class]{Picture} objects or png array objects, e.g.,
from using \code{\link[=get_phylopic]{get_phylopic()}}. Use the \code{x} and \code{y} aesthetics to place the
silhouettes at specified positions on the plot. The \code{height} or \code{width}
aesthetic specifies the height or width, respectively, of the silhouettes
in the units of the y axis (only one is allowed). The aspect ratio of the
silhouettes will always be maintained. The \code{hjust} and \code{vjust} aesthetics
can be used to manage the justification of the silhouettes with respect to
the \code{x} and \code{y} coordinates.

The \code{color} (default: NA), \code{fill} (default: "black"), and \code{alpha} (
default: 1) aesthetics can be used to change the outline color, fill color,
and transparency (outline and fill) of the silhouettes, respectively. If
\code{color} is specified and \code{fill} is NA \code{color} will be used as the fill
color (for backwards compatibility). If "original" is specified for the
\code{color} aesthetic, the original color of the silhouette outline will be
used (usually the same as "transparent"). If "original" is specified for
the \code{fill} aesthetic, the original color of the silhouette body will be
used (usually the same as "black"). To remove the fill or outline, you can
set \code{fill} or \code{color} to "transparent", respectively.

The \code{horizontal} and \code{vertical} aesthetics can be used to flip the
silhouettes. The \code{angle} aesthetic can be used to rotate the silhouettes.
When specifying a horizontal and/or vertical flip \strong{and} a rotation, the
flip(s) will always occur first. If you would like to customize this
behavior, you can flip and/or rotate the image within your own workflow
using \code{\link[=flip_phylopic]{flip_phylopic()}} and \code{\link[=rotate_phylopic]{rotate_phylopic()}}.

Note that png array objects can only be rotated by multiples of 90 degrees.
Also, outline colors do not currently work for png array objects.
}
\section{Aesthetics}{
 geom_phylopic understands the following aesthetics:
\itemize{
\item \strong{x} (required)
\item \strong{y} (required)
\item \strong{img} \emph{or} \strong{uuid} \emph{or} \strong{name} (one, and only one, required)
\item height \emph{or} width (optional, maximum of only one allowed)
\item ysize \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Deprecated in favor of height or
width
\item color \emph{or} colour
\item fill
\item alpha
\item horizontal
\item vertical
\item angle
\item hjust
\item vjust

Learn more about setting these aesthetics in \code{\link[=add_phylopic]{add_phylopic()}}.
}
}

\examples{
\dontrun{
library(ggplot2)
df <- data.frame(x = c(2, 4), y = c(10, 20),
                 name = c("Felis silvestris catus", "Odobenus rosmarus"))
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = name),
                fill = "purple", height = 10) +
  facet_wrap(~name) +
  coord_cartesian(xlim = c(1,6), ylim = c(5, 30))
}
}
