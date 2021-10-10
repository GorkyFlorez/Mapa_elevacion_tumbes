library(raster)
library(sf)
library(ggspatial)
library(sp)
library(ggplot2)
alt = getData("alt", country = "Peru", path = tempdir())
slope = terrain(alt, opt = "slope") 
aspect = terrain(alt, opt = "aspect")
hill = hillShade(slope, aspect, angle = 40, direction = 270)
Peru           <- getData('GADM', country='Peru', level=1) %>%st_as_sf() 
Tumbes         <- subset(Peru , NAME_1  == "Tumbes")
dem.p          <-  rasterToPoints(alt)
df             <-  data.frame(dem.p)
colnames(df) = c("lon", "lat", "alt")
SurAmerica     <- st_read ("SHP/SurAmerica.shp")  
SurAmeric      <- st_transform(SurAmerica,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Peru_Per       = st_as_sfc(st_bbox(Peru))
Tumbes_Per     = st_as_sfc(st_bbox(Tumbes))
ANP            <- st_read ("SHP/ANPNacionalDefinitivas.shp") 


Per=ggplot()+
  geom_sf(data = SurAmeric, fill=NA, color="black")+
  geom_sf(data = Peru_Per, fill=NA, color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))
Peru.grob  <- ggplotGrob(Per)


Tum = ggplot()+
  geom_sf(data = SurAmeric, fill=NA, color="gray")+
  geom_sf(data = ANP, fill="darkseagreen4", color="darkseagreen4")+
  geom_sf(data = Tumbes, fill=NA, color="black")+
  coord_sf(xlim = c(-81.0443,-80.12676), ylim = c(-4.23151,-3.402917),expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))
Tum.grob  <- ggplotGrob(Tum)

cortes <- c(200, 500,1000,2000,3000,4000,5000, 6247)
Map= ggplot()+
  geom_raster(data = df , aes(lon,lat, fill = alt) )+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = "Greys"), 
                       na.value = 'white',name="Elevacion\n(m.s.n.m)",breaks = cortes ,
                       labels = c("[menor a - 270] ","[270 - 499]", "[500 - 999]", "[1000 - 1999]", "[2000 - 2999]",
                                  "[3000 - 3999]", "[4000 - 4999]", "[5000 -6247]"))+
  geom_sf(data = SurAmeric, fill=NA, color="gray")+
  geom_sf(data = ANP, fill="darkseagreen4", color="darkseagreen4")+
  geom_sf(data = Peru, fill=NA, color="gray")+
  geom_sf(data = Tumbes_Per, fill=NA, color="black")+
  theme_bw()+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(color = "gray",linetype = "dashed", size = 0.5),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=9,family="serif"),
        axis.text.x  = element_text(face="bold", color="black", size=9,family="serif"),
        panel.border = element_rect(size = 1.5),
        legend.background = element_rect(fill = "gray80",colour = 'black', size = 0.5, linetype='solid'),
        legend.text =element_text(size=9, family="serif"),
        legend.position = c(0.1,0.35),
        legend.key.size = unit(0.2, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.5,"cm"), #ancho de cuadrados de referencia 
        legend.title = element_text(size=10, family="serif", face = "bold"))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  coord_sf(xlim = c(-82,-69), ylim = c(-19,-1),expand = FALSE)+
  scale_x_continuous(breaks = seq(-80,-72, by = 6),name=expression(paste("Longitude (",degree,")")))+
  scale_y_continuous(breaks = seq(-14,-2, by = 4),name=expression(paste("Latitude (",degree,")")))+
  annotation_custom(Peru.grob, xmin = -82, xmax = -79, ymin =-19, ymax=-15)+
  annotation_custom(Tum.grob , xmin = -74, xmax = -69, ymin =-5, ymax=-1.2)+
  annotate(geom = "text", x = -80, y = -10.5, label = "OCEANO \nPACIFICO", fontface = "italic", color = "Blue", size = 4, face = "bold")+
  annotate(geom = "text", x = -78, y = -2, label = "Ecuador", fontface = "italic", color = "Black", size = 4, face = "bold")+
  annotate(geom = "text", x = -70, y = -7, label = "Brasil", fontface = "italic", color = "Black", size = 4, face = "bold")+
  guides(fill = guide_legend(title.position = "top", 
                             title.theme = element_text( size = 11, colour = "black"),
                             barheight = .5, barwidth = .95,
                             title.hjust = 0.5, raster = FALSE))+
  geom_segment(aes(x=-80.12676, xend=-73.8, y=-4.23151, yend=-5),  linetype = "solid", color = "black", size = 0.6) +
  geom_segment(aes(x=-80.12676, xend=-73.8, y=-3.402917, yend=-1.2), linetype = "solid", color = "black", size = 0.6) 

ggsave(plot = Map ,"MAPAS/Mapa elevacion tumbes.png", 
       units = "cm", width = 21,height = 29, dpi = 1000) 


