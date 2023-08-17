#Distance Matrix dengan library SF
library(sf)
shp_irak01 <- st_read(dsn = "D:/My Drive/deden-RMarkdown & Github/GWPCA/data shp/514 kabupaten.shp")
shp_centroid <- st_point_on_surface(x = shp_irak01)
mtx_distance <- st_distance(shp_centroid, shp_centroid)
mtx_distance
library(xlsx)
write.xlsx(mtx_distance, file = "myworkbook.xlsx",
           sheetName = "distance", append = FALSE)

#Distance Matrix dengan library geosphere
library(geosphere)
mtx_distance_harvesine <- distm(x = st_coordinates(shp_centroid), y = st_coordinates(shp_centroid), fun = geosphere::distHaversine)
write.xlsx(mtx_distance_harvesine, file = "myworkbook2.xlsx",
           sheetName = "distance", append = FALSE)
