library(rgdal)
library(sf)
library(rgeos)
library(sp)
library(raster)
library(glue)
library(dplyr)

#setup:
Sys.setlocale(locale = "us")
#----testing Chinese----
x <- "中文" # if get random thing here, need to change text encoding to utf-8
Encoding(x) <- "UTF-8"
Encoding(x)


#----1. LOAD POLYGON FILE-----
# read the shp file:
cnt <- (readOGR(dsn = "./v6_1911_cnty_pgn_utf/v6_1911_cnty_pgn_utf.shp",
                stringsAsFactors=FALSE))

#1-1 ENCODING AND CORRECT TYPE:
col_names = colnames(cnt@data)

#*****change character encoding to UTF-8******:
for (col in colnames(cnt@data)){
  if (class(cnt@data[[col]]) == "character"){
    Encoding(cnt@data[[col]]) <- "UTF-8"
    cnt@data[[col]]<- iconv(cnt@data[[col]], from = "UTF-8", to = "Latin1")
    Encoding(cnt@data[[col]]) <- "UTF-8"
  }
}

#factor:

for (col in (c("LEV_RANK", "BEG_RULE", "END_RULE", "DYN_PY", "BEG_YR","END_YR",
               "DYN_CH","LEV1_PY","LEV1_CH","LEV2_PY","LEV2_CH","OBJ_TYPE","COMPILER","GEOCOMP","TYPE_PY", "TYPE_CH"))){
  cnt@data[[col]] <- as.factor(cnt@data[[col]])
}

#change some number to correct number type:
for (col in c("SYS_ID", "OID_", "NOTE_ID", "SYS_ID_1","X_COORD","Y_COORD")){
  print(col)
  cnt@data[[col]] <- as.numeric(cnt@data[[col]])
}

#check col name and type again:
sapply(cnt@data, class)

#rm unwanted variables:
rm(col)

#----1-2. POLYGON DATA EXPLORE----
#need to use Traditional Chinese
cnt_name = levels(as.factor(cnt$NAME_FT))

#ex1: check whether county is in the namelist
"天津縣" %in% cnt_name # return TRUE

#ex2: check which county has level3?
cnt$NAME_CH[which(cnt$LEV_RANK == "3")]

#ex3: get the area of each county:
gArea(cnt[cnt$NAME_CH == "懷柔縣",])

#----2 LOAD CGED-Q FILE----
# header = FALSE to encode colname as well
jsl <- read.csv("./CGED-Q/CGED-Q+Public+Release+1900-1912+9+May+2019.csv",header = TRUE, stringsAsFactors = TRUE, encoding = "UTF-8")
#get col name:
jsl_cp <- read.csv("./CGED-Q/CGED-Q+Public+Release+1900-1912+9+May+2019.csv",header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8", nrows = 1)
#jsl_col_names <- as.character(jsl_cp[1, ])
colnames(jsl) <- jsl_cp[1, ] # the first row will be the header
colnames(jsl)[1] <- "阳历年份" #fix some strange bug
rm(jsl_cp)

#change jsl to correct datatype:
#factor_additional:

for (col in c("阳历年份", "季节号", "插补号", "卷号", "册号")){
  jsl[[col]] <- as.factor(jsl[[col]])
}

#character_additional:
jsl[["姓"]] <- as.character(jsl[["姓"]])
jsl[["名"]] <- as.character(jsl[["名"]])
jsl[["字号"]] <- as.character(jsl[["字号"]])

#FINISH

#MATCH THE NAME:
#modify the cnt to show only 2 characters:
cnt$NAME_FT <- substr(cnt$NAME_FT,1,2)

cnt_name = levels(as.factor(cnt$NAME_FT))

#Start matching the county name

#currently only select one year, 1911
jsl_sel <- jsl[(jsl[["阳历年份"]]== 1911) & ! is.na(jsl[["阳历年份"]]),]

#clean the data
#1. remove cnt which is NA
jsl_sel <- jsl_sel[jsl_sel["籍贯县"] != "",]
#2. remove cnt which has unrecognized name:
jsl_sel <- jsl_sel[!(jsl_sel[["籍贯县"]] %in% (grep("？", levels(as.factor(as.character(jsl_sel[["籍贯县"]]))), value = TRUE))),]

#3. change typo, ex: "甯" to "寧"
jsl_sel[["籍贯县"]] <- gsub("甯","寧",jsl_sel[["籍贯县"]])
jsl_sel[["籍贯县"]] <- gsub("寗","寧",jsl_sel[["籍贯县"]])
jsl_sel[["籍贯县"]] <- gsub("寕","寧",jsl_sel[["籍贯县"]])

jsl_sel[["籍贯县"]] <- gsub(" ","",jsl_sel[["籍贯县"]])

#4. keep only the first 2 characters
jsl_sel[["籍贯县"]] <- substr(jsl_sel[["籍贯县"]], 1, 2)

#5. check which county is not in cnt_name
jsl_name_unshow_in_cnt <- jsl_sel[!(jsl_sel[["籍贯县"]] %in% cnt$NAME_FT),][["籍贯县"]]
jsl_name_unshow_in_cnt <- levels(as.factor(jsl_name_unshow_in_cnt))

#check corresponding name manually, If you'd like to do so.
for (nm in jsl_name_unshow_in_cnt[1:10]){
  print(nm)
  print(grep(substr(nm,1,1), cnt_name_rm, value = TRUE))
}

# Here we only keep the counties which appeared both in cnt and jsl
jsl_sel <- jsl_sel[!(jsl_sel[["籍贯县"]] %in% jsl_name_unshow_in_cnt),]

#UP TO NOW, the selected JSL matches with cnt

#--END OF PREPARATION--

#----3. ATTRIBUTE JOINS----
# need to summarize what you want to show from jsl_sel, and then join into cnt polygon 
people_ag <-as.data.frame(table(unlist(jsl_sel[["籍贯县"]])))
colnames(people_ag) <- c("籍贯县","1911_FREQUENCY")
# Show the first two rows of the aggregated data
head(people_ag, 2)

head(cnt$NAME_FT) # dataset to add to (results not shown)
head(people_ag$"籍贯县") # the variables to join

#the `left_join` function from the **dplyr** package but the `merge` function
#could equally be used. 
head(left_join(cnt@data, people_ag, by = c('NAME_FT' = '籍贯县'))) # test it works
#We use left_join because we want the length of the data frame to remain unchanged
cnt@data <- left_join(cnt@data, people_ag, by = c('NAME_FT' = '籍贯县'))

#----4. PLOT USING BASIC PLOT FUNCTION----
#WHOLE PICTURE
plot(cnt)
#draft try, testing on cnt shp data

#4-1: plot the first 5 counties in the list:
sel <- head(cnt, n = 5)
plot(sel)
#highlight the first county to blue:
sel <- head(cnt, n = 1)
plot(sel, add = T, col = "lightblue")

#4-2: print selected name county:
sel <- cnt[cnt$NAME_FT == "懷柔",]
plot(sel, col = "red") 


#4-3:1 plot subplaces around one place:
sel <- cnt[cnt$NAME_FT == "懷柔",]
cent_cnt <- gCentroid(cnt[sel,]) 
points(cent_lnd, cex = 3)

#4-3:2 set 10 km buffer, get a buffer from point cent_lnd
cnt_buffer <- gBuffer(spgeom = cent_cnt, width = 50000) 

#4-3:3 subsetting selects any intersecting zones - light blue
cnt_central <- cnt[cnt_buffer,] # the selection is too big!
# test the selection for the previous method - uncomment below
plot(cnt_central, col = "lightblue")
plot(sel, add = T, col = "lightyellow") # highlight the selected area

#4-4: plot acoording to Area condition
cnt_area <- gArea(cnt,byid = TRUE) #calculate every counties area
sel <- cnt[cnt_area > 1e+10,]
plot(sel)

#----5. PLOT USING DIFFERENT PACKAGES-----

#----5-1: tmap package----
library(tmap)
library(tmaptools)
library(OpenStreetMap)

#5-1-1:qtm: Quick thematic map plot
qtm(cnt, fill = "1911_FREQUENCY") # plot the basic map
#5-1-2
#more examples for basic usage of tmap:
#inverted the color scheme, deeper color means less frequency:
# The sequential palettes names are Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
# The diverging palettes are BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral And also cool_warm. 
qtm(shp = cnt, fill = "1911_FREQUENCY", fill.palette = "YlGn")
#plot two pictures side by side 
qtm(shp = cnt, fill = c("LEV_RANK", "1911_FREQUENCY"), fill.palette = "YlGn", ncol = 2)

#5-1-3: Using seperate functions to realize the same visualization:
#it's equal to qtm(cnt, fill = "1911_FREQUENCY")
tm_img <- tm_shape(cnt) +
  tm_fill("1911_FREQUENCY", thres.poly = 0) +
  tm_borders(col = "grey80", lwd = 0.5)
#check the color cheatsheet for the color you want
tmap_save(tm = tm_img, filename = "1911_FREQUENCY_6.png")

#5-1-4:If you want basemap:

#To create a basemap with tmap, you can use the `read_osm` function, 
#from the [**tmaptools** package](https://github.com/mtennekes/tmaptools) as follows. 
#Note that you must first transform the data into wgs
cnt_wgs = spTransform(cnt, CRS("+init=epsg:4326"))
osm_tiles = read_osm(bbox(cnt_wgs))# read basemap from OpenStreetMap

#method1, static map: 
tmap_mode("plot")
tm_shape(osm_tiles) +
  tm_raster() +
  tm_shape(cnt_wgs) +
  tm_fill("1911_FREQUENCY", fill.title = "1911 Frequency", scale = 1, alpha = 0.6)

#method2:
#By changing map mode, tmap_mode("view"), this will make the maps appear on a zoomable webmap powered by leaflet. 
tmap_mode("view")
tm_basemap(bbox(cnt_wgs),server = "OpenStreetMap.Mapnik") +
  tm_shape(cnt_wgs) +
  tm_fill("rank", style = "cont" ,n = 10, scale = 1, alpha = 0.6)

#----5-2: ggplot package----
library(ggplot2)
#5-2-1:plot points using ggplot:
p <- ggplot(cnt@data, aes(X_COORD,Y_COORD))
#The real power of ggplot2 lies in its ability to add layers to a plot. In this case we can add text to the plot.
#***ggplot2 cannot directly plot spatial object
p + geom_point(aes(colour = -`1911_FREQUENCY`))

#5-2-2: plot polygon using ggplot:
# whether you want to only plot the selected part: sel = cnt[cnt$`1911_FREQUENCY` < 200 & !(is.na(cnt$`1911_FREQUENCY`)),]
cnt_f <- broom::tidy(cnt_wgs)#(after this step, cnt_f only contains polygon informaiton, it doesnt have other data information, so need to use left join from dplyr to join back the data information

head(cnt_f, n = 2) # peak at the fortified data
cnt$id <- row.names(cnt) # allocate an id variable to the sp data
head(cnt@data, n = 2) # final check before join (requires shared variable name)
cnt_f <- left_join(cnt_f, cnt@data) # join the data

#create the ggplot (as we mentioned before, ggplot cannot directly plot spatial object.)
#long, lat are new col generated by tidy function
#Now we can use ggplot to plot polygon information
#**group = id is important
map <- ggplot(cnt_f, aes(long, lat, group = id, fill =`1911_FREQUENCY`)) +
  geom_polygon() + coord_equal() +
  labs(x = "X_COORD", y = "Y_COORD") +
  ggtitle("1911_frequency")

#plot the map
map + scale_fill_gradient(low = "white", high = "red")

#use ggmap for basemap:
library(ggmap)  # you may have to use install.packages to install it first
cnt_wgs = spTransform(cnt, CRS("+init=epsg:4326")) # need to transfer to wgs
b <- bbox(cnt_wgs)
# download map data for the lnd data and plot, there are three map source: google, openstreetmap,stamenmap.
#Since OpenStreetMap doesn't support get_map anymore, if you don't have google API, you can choose get_stamenmap
#as well as choosing maptype:
map <- ggmap(get_stamenmap(b, zoom = 7, maptype = "toner-lite"))  

#plot the map:
map + 
  geom_polygon(data = cnt_f, aes(x = long, y = lat, group = id, fill = `1911_FREQUENCY`), alpha = 0.6) +
  scale_fill_gradientn(colors = c("lightyellow", "red", "red2","red4","darkred"))

ggsave("../../Time_series/1911_FREQUENCY_7.png", device = "png", scale = 5)

#5-3: leaflet package----
library(leaflet)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = cnt_wgs)


#----6. PLOT DIFFERENT YEARS' FREQUENCY----
#since we've just plot for 1911, it will be very easy to change to other years.
#the base map we're going to use
map <- ggmap(get_stamenmap(b, zoom = 7, maptype = "toner-lite"))  

for (year in seq(1903,1911, by = 2)){
  jsl_sel <- jsl[(jsl[["阳历年份"]]== year) & ! is.na(jsl[["阳历年份"]]),]

  jsl_sel <- jsl_sel[jsl_sel["籍贯县"] != "",]

  jsl_sel <- jsl_sel[!(jsl_sel[["籍贯县"]] %in% (grep("？", levels(as.factor(as.character(jsl_sel[["籍贯县"]]))), value = TRUE))),]
  
  jsl_sel[["籍贯县"]] <- gsub("甯","寧",jsl_sel[["籍贯县"]])
  jsl_sel[["籍贯县"]] <- gsub("寗","寧",jsl_sel[["籍贯县"]])
  jsl_sel[["籍贯县"]] <- gsub("寕","寧",jsl_sel[["籍贯县"]])
  jsl_sel[["籍贯县"]] <- gsub(" ","",jsl_sel[["籍贯县"]])
  jsl_sel[["籍贯县"]] <- substr(jsl_sel[["籍贯县"]], 1, 2)
  
  jsl_name_unshow_in_cnt <- jsl_sel[!(jsl_sel[["籍贯县"]] %in% cnt$NAME_FT),][["籍贯县"]]
  jsl_name_unshow_in_cnt <- levels(as.factor(jsl_name_unshow_in_cnt))
  
  # Here we only keep the correspondence counties
  jsl_sel <- jsl_sel[!(jsl_sel[["籍贯县"]] %in% jsl_name_unshow_in_cnt),]

  # need to summarize what you want to show from jsl_sel, and then join into cnt polygon 
  people_ag <-as.data.frame(table(unlist(jsl_sel[["籍贯县"]])))
  colnames(people_ag) <- c("籍贯县","FREQUENCY")
  
  head(cnt$NAME_FT) # dataset to add to (results not shown)
  head(people_ag$"籍贯县") # the variables to join
  
  cnt_new <- cnt
  head(left_join(cnt@data, people_ag, by = c('NAME_FT' = '籍贯县'))) # test it works
  cnt_new@data <- left_join(cnt@data, people_ag, by = c('NAME_FT' = '籍贯县'))
  
#start plot
  cnt_wgs = spTransform(cnt_new, CRS("+init=epsg:4326"))
  cnt_f <- broom::tidy(cnt_wgs)#(after this step, cnt_f only contains polygon informaiton, it doesnt have other data information, so need to use left join from dplyr to join back the data information)
  
  head(cnt_f, n = 2) # peak at the fortified data
  cnt_new$id <- row.names(cnt_new) # allocate an id variable to the sp data
  head(cnt@data, n = 2) # final check before join (requires shared variable name)
  cnt_f <- left_join(cnt_f, cnt_new@data) # join the data
  
  map + 
    geom_polygon(data = cnt_f, aes(x = long, y = lat, group = id, fill = `FREQUENCY`), alpha = 0.6) +
    scale_fill_gradientn(colors = c("lightyellow", "red", "red2","red4","darkred"))
  
  ggsave(glue("../../Time_series/{year}_FREQUENCY.png"), device = "png", scale = 5)
  rm(cnt_new)
}

