# Load libraries 
library(png)
library(grid)
library(gridExtra)

reef <- readPNG(file.choose())
dim(reef) #will tell what kind of array it is (resolution x #channels)

# Plot the full color image 
grid.raster(reef)

# Plot the image in grayscale (channel3)
grid.raster(reef[,,3])

# Triplicate the image for independent RGB channels
reef.R = reef
reef.G = reef
reef.B = reef

# zero out other channels for each image
reef.R[,,2:3] = 0
reef.G[,,1]=0
reef.G[,,3]=0
reef.B[,,1:2]=0

# Plot the 3 color channels -- good to inspect if a particular channel contains more (or less) information for objects of interest
img1 = rasterGrob(reef.R)
img2 = rasterGrob(reef.G)
img3 = rasterGrob(reef.B)
grid.arrange(img1, img2, img3, nrow=1)

# reshape image into a data frame
df = data.frame(
  red = matrix(reef[,,1], ncol=1),
  green = matrix(reef[,,2], ncol=1),
  blue = matrix(reef[,,3], ncol=1)
)

# compute the k-means clustering
K = kmeans(df,3) # the number after df is the # of clusters--experiment with low (2-3) and high #'s.
df$label = K$cluster

# calculate 'mean' value for each pixel given it's k-cluster

# clustered color image
colors = data.frame(
  label = 1:nrow(K$centers), 
  R = K$centers[,"red"],
  G = K$centers[,"green"],
  B = K$centers[,"blue"]
)

# merge color codes on to df
# IMPORTANT: we must maintain the original order of the df after the merge!
df$order = 1:nrow(df)
df = merge(df, colors)
df = df[order(df$order),]
df$order = NULL

# get mean color channel values for each row of the df.
R = matrix(df$R, nrow=dim(reef)[1])
G = matrix(df$G, nrow=dim(reef)[1])
B = matrix(df$B, nrow=dim(reef)[1])

# reconstitute the segmented image in the same shape as the input image
reef.segmented = array(dim=dim(reef))
reef.segmented[,,1] = R
reef.segmented[,,2] = G
reef.segmented[,,3] = B

# save the result
png("reef.segmented.png")
grid.raster(reef.segmented)
dev.off()

# PCA on the reef data and add the uv coordinates to the dataframe
PCA = prcomp(df[,c("red","green","blue")], center=TRUE, scale=TRUE)
df$u = PCA$x[,1]
df$v = PCA$x[,2]

library(ggplot2)
#Plot uv (PCA axes) values of the k-means clusters
ggplot(df, aes(x=u, y=v, col=rgb(R,G,B))) + 
  geom_point(size=2) + scale_color_identity()
ggsave("reef.segmented.quantplot.png", plot=last_plot())
