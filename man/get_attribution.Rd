% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_attribution.R
\name{get_attribution}
\alias{get_attribution}
\title{Get PhyloPic attribution data}
\usage{
get_attribution(uuid = NULL, img = NULL, text = FALSE, permalink = FALSE)
}
\arguments{
\item{uuid}{\code{character}. A vector of valid uuid(s) for PhyloPic
silhouette(s) such as that returned by \code{\link[=get_uuid]{get_uuid()}} or \code{\link[=pick_phylopic]{pick_phylopic()}}.}

\item{img}{A \link[grImport2:Picture-class]{Picture} or png array object from
\code{\link[=get_phylopic]{get_phylopic()}}. A list of these objects can also be supplied. If \code{img}
is supplied, \code{uuid} is ignored. Defaults to NULL.}

\item{text}{\code{logical}. Should attribution information be returned as
a text paragraph? Defaults to \code{FALSE}.}

\item{permalink}{\code{logical}. Should a permalink be created for this
collection of \code{uuid}(s)? Defaults to \code{FALSE}.}
}
\value{
A \code{list} of PhyloPic attribution data for an image \code{uuid} or
a text output of relevant attribution information.
}
\description{
This function provides a convenient way to obtain attribution data
for PhyloPic images via an image uuid returned by \code{\link[=get_uuid]{get_uuid()}}.
}
\details{
This function returns image \code{uuid} specific attribution data,
including: contributor name, contributor uuid, contributor contact,
image uuid, license, and license abbreviation. If \code{text} is set to
\code{TRUE}, a text paragraph with the contributor name, year of contribution,
and license type is printed and image attribution data is returned
invisibly (i.e. using \code{\link[=invisible]{invisible()}}. If \code{permalink} is set to \code{TRUE}, a
permanent link (hosted by \href{https://www.phylopic.org}{PhyloPic}) will be
generated. This link can be used to view and share details about the
image silhouettes, including contributors and licenses.
}
\examples{
\dontrun{
# Get valid uuid
uuid <- get_uuid(name = "Acropora cervicornis")
# Get attribution data for uuid
attri <- get_attribution(uuid = uuid)

# Get list of valid uuids
uuids <- get_uuid(name = "Scleractinia", n = 5)
# Get attribution data for uuids
get_attribution(uuid = uuids, text = TRUE)
# Get attribution data for uuids and create permalink
get_attribution(uuid = uuids, text = TRUE, permalink = TRUE)
}
}
