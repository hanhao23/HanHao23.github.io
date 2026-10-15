# Draw a Christmas Tree in R

# Set up the plot area
plot(0, 0, type="n", xlab="", ylab="", xlim=c(-5, 5), ylim=c(0, 8), asp=1)

# Function to draw a tree layer
draw_tree_layer <- function(center, bottom, height, color) {
  x <- c(center - height, center, center + height)
  y <- c(bottom, bottom + height, bottom)
  polygon(x, y, col=color, border=color)
}

# Tree colors
tree_color <- "green4"

# Draw the tree layers
draw_tree_layer(0, 0, 3, tree_color)
draw_tree_layer(0, 2.5, 2.5, tree_color)
draw_tree_layer(0, 4.5, 2, tree_color)
draw_tree_layer(0, 6, 1.5, tree_color)

# Draw the trunk
rect(-0.5, 0, 0.5, -1, col="brown4")

# Add some ornaments
set.seed(123) # For reproducible ornament placement
points(runif(20, -2, 2), runif(20, 0, 6.5), col="red", pch=19)
points(runif(20, -2, 2), runif(20, 0, 6.5), col="blue", pch=19)
points(runif(20, -2, 2), runif(20, 0, 6.5), col="purple", pch=19)
points(runif(20, -2, 2), runif(20, 0, 6.5), col="orange", pch=19)

# Add a star on top
points(0, 7.5, col="gold", pch=8, cex=3, lwd = 5)
