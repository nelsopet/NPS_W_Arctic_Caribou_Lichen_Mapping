require(sf)
list.files()
ARCN_FIA_plots<-read_sf('ARCN_FIA_LICHVOL_WGS84_20200413.shp')
plot(ARCN_FIA_plots)