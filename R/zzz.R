#' Fixed phylogeny blank theme for ggphylo
#' @export
theme_phylo_blank2 <- function() {
  element_blank <- ggplot2::element_blank
  ggplot2::theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_text(colour = NA),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )
}

# Unnest a nested list
unnest <- function(x) {
  if (is.null(names(x))) {
    list(unname(unlist(x)))
  }
  else {
    c(list(all = unname(unlist(x))), do.call(c, lapply(x, unnest)))
  }
}

# Replaces null with "none"
replacenull <- function(x) {
  x$canonicalName[sapply(x$canonicalName, function(x) is.null(x))] <- "none"
  x
}

# Convert citation null to number 1
citationtonumber <- function(x) {
  make1 <- function(x){
    if (x$canonicalName$citationStart == "none") {
      x$canonicalName$citationStart <- 1
    }
    x
  }
  make1(x)
}

pc <- function(l) Filter(Negate(is.null), l)

as_null <- function(x) if (length(x) == 0) NULL else x

phy_GET <- function(path, args, ...) {
  tmp <- phy_GET2(path, args, ...)
  jsonlite::fromJSON(tmp, FALSE)
}

phy_GET2 <- function(path, args, ...) {
  cli <- crul::HttpClient$new(url = pbase(), opts = list(...))
  tt <- cli$get(path = path, query = as_null(pc(args)))
  tt$raise_for_status()
  tt$parse("UTF-8")
}

ibase <- function() "http://phylopic.org/api/a/image/"
pbase <- function() "http://phylopic.org"
