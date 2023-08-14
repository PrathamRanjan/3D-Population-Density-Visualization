install.packages(c("raster","tidyverse","stars","rayshader","MetBrewer","colorspace","rgl","png"))
install.packages("rgl")
install.packages("usethis")

usethis::create_github_token()

remotes::install_github("tylermorganwall/rayshader")
remotes::install_github("tylermorganwall/rayrender", force=TRUE)

library(sf)
library(raster)
library(tidyverse)
library(rayshader)
library(MetBrewer)
library(colorspace)
library(rgl)
library(png)
library(stars)


# load kontur data

data <- st_read("Desktop/rayshader_SG/kontur_population_SG_20220630.gpkg")


# load map 

st <- getData("GADM", country ="SGP",level=1)

# filter for data, check with map, do interesction on data to limit kontur to Singapore
singapore <- st |>
  st_as_sf() |>
  st_transform(crs = st_crs(data))


singapore |>
  ggplot()+
  geom_sf()

st_singapore <- st_intersection(data, singapore)


# define aspect ratio based on bounding box 

bb <-st_bbox(st_singapore)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

singapore |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

# handle conditions of width or height being the longer side

if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
}

# convert to raster so we can then convert to matrix

size <- 5000

singapore_rast <- st_rasterize(st_singapore, 
                               nx = floor(size * w_ratio),
                               ny = floor(size * h_ratio))

mat <- matrix(singapore_rast$population, 
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))
# create color palette

c1 <- met.brewer("OKeeffe2")
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 5)(256)
swatchplot(texture)

# plot in 3D

mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          zscale = 100,
          solid = FALSE,
          shadowdepth = 0)

render_camera(theta = -10, phi = 50, zoom = .8)

outfile<- "Desktop/rayshader_SG/images/test_plot.png"

render_highquality(
  filename = outfile,
  interactive = FALSE,
  lightdirection = 45,
  lightaltitude = c(20, 80),
  lightcolor = c(c1[2], "white"),
  lightintensity = c(600, 100),
  samples = 220,
  width = 2500,
  height = 2500
)
