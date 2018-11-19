#' Input an image and add to an existing plot made with base graphics
#'
#' @export
#' @param img A png object, e.g, from using [image_data()]
#' @param x x value of the silhouette center. Ignored if y and ysize are 
#' not specified.
#' @param y y value of the silhouette center. Ignored if x and ysize are 
#' not specified.
#' @param ysize Height of the silhouette. The width is determined by the 
#' aspect ratio of the original image. Ignored if x and y are not specified.
#' @param alpha A value between 0 and 1, specifying the opacity of the 
#' silhouette.
#' @param color Color to plot the silhouette in.
#' @details Use parameters `x`, `y`, and `ysize` to place the silhouette 
#' at a specified position on the plot. If all three of these parameters 
#' are unspecified, then the silhouette will be plotted to the full height 
#' and width of the plot.
#' @examples \dontrun{
#' # get a silhouette
#' cat <- image_data("23cd6aa4-9587-4a2e-8e26-de42885004c9", size = 128)[[1]]
#' 
#' # single image
#' plot(1, 1, type="n", main="A cat herd")
#' add_phylopic_base(cat, 0.5, 0.5, 0.2)
#' 
#' # lots of images
#' posx <- runif(50, 0, 1)
#' posy <- runif(50, 0, 1)
#' size <- runif(50, 0.01, 0.2)
#' plot(posx, posy, type="n", main="A cat herd")
#' for (i in 1:50) {
#'   add_phylopic_base(cat, posx[i], posy[i], size[i])
#' }
#' 
#' # plot a stand alone image of the silhouette
#' plot_phylopic_base(cat)
#' }
add_phylopic_base <- function(img, x = NULL, y = NULL, ysize = NULL, 
                              alpha = 0.2, color = NULL) {
  # color and alpha the animal
  img <- recolor_phylopic(img, alpha, color)
  
  #number of x-y pixels for the logo (aspect ratio)
  dims <- dim(img)[1:2]
  AR <- dims[1] / dims[2]
  graphics::par(usr = c(0, 1, 0, 1))
  graphics::rasterImage(img, x - (ysize/2), 
    y - (AR*ysize/2), x + (ysize/2), y + (AR*ysize/2), 
    interpolate = TRUE)
}

#' @export
#' @rdname add_phylopic_base
plot_phylopic_base <- function(img, x = NULL, y = NULL, ysize = NULL, 
                              alpha = 0.2, color = NULL) {

  graphics::plot.new()
  add_phylopic_base(img, x, y, ysize, alpha, color)
}
