library("OpenStreetMap")
library("osmar")
library("rJava")
src <- osmsource_api(url = "https://api.openstreetmap.org/api/0.6/")
get_osm(node(18961430), source = src)
get_osm(way(3810479), source = src)
get_osm(way(3810479), source = src, full = TRUE)
bb <- center_bbox(174.76778, -36.85056, 700, 700)
ua <- get_osm(bb, source = src)
summary(ua$nodes)
bs_ids <- find(ua, node(tags(v %agrep% "busstop")))
ts_ids <- find(ua, node(tags(v == "traffic_signals")))
hw_ids <- find(ua, way(tags(k == "highway")))
hw_ids <- find_down(ua, way(hw_ids))
str(hw_ids)
ts <- subset(ua, node_ids = ts_ids)
bs <- subset(ua, node_ids = bs_ids)
hw <- subset(ua, ids = hw_ids)
plot(ua)
plot_ways(hw, add = TRUE, col = "green")
plot_nodes(ts, add = TRUE, col = "red")
plot_nodes(bs, add = TRUE, col = "blue")

LAT1 =  30 ; LAT2 = 50
LON1 = -10 ; LON2 = 10

map <- openmap(c(LAT2,LON1), c(LAT1,LON2), zoom = NULL,
               type = c("osm", "stamen-toner", "stamen-terrain","stamen-watercolor", "esri","esri-topo")[6],
               mergeTiles = TRUE)

