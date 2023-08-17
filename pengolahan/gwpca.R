#dataFolder <- "D:\\filenya\\indo_and_variabel.shp"
#datanya <- read.csv(paste0(dataFolder, "all variabel.csv"), header = T)
View(datanya)

datanya <- read_csv("E:/agregasi/New folder/fix bos.csv")
#peta <- readOGR("D:/filenya/indo_and_variabelnya.shp","514 kabupaten")
datazscore <- read_csv("E:/agregasi/dari spss/zscore.csv")
names(datazscore)

peta <- readOGR("E:/SIG/fix shp.shp","fix shp")
peta$idkab

peta@data

shp_datanya <- peta@data
idkab <- peta$idkab

gas <- merge(peta, datanya, by="idkab")
gas
gas$idkab
names(gas)

koordinat <- coordinates(gas)
force(koordinat)

coordnya <- data.frame(idkab,koordinat)
force(coordnya)
gaskor <- merge(gas, coordnya, by="idkab")
gaskor$idkab

View(gaskor)
names(gaskor)

gas1 <- gaskor[, c(43:57)]
names(gas1)
gaspca <- scale(as.matrix(gas1@data[, 1:15]))
pcanya <- princomp(gaspca, cor = FALSE)
(pcanya$sdev^2 / sum(pcanya$sdev^2)) * 100

gas3 <- pcanya$loadings
gas3  
View(gas3)

#data_shp <- readOGR("E:/SIG/514 kabupaten.shp","514 kabupaten")
#dat_shp <- data_shp@data
#Id_kab <- dat_shp$idkab

#y <- coordinates(data_shp)

gaskor$X1
 
coordstes <- as.matrix(cbind(gaskor$X1.y, gaskor$X2.y))
coordstes
state.gaskor <- SpatialPointsDataFrame(coordstes, as.data.frame(gaspca))
state.gaskor@data
dim(state.gaskor)

#d1s <- SpatialPointsDataFrame(Coords1,as.data.frame(Data.1.scaled))
#pca.gw <- gwpca(d1s,vars=colnames(d1s@data),bw=1000000,k=10)
install.packages("GWmodel", dependencies = T)
install.packages("gstat",dependencies=T) 

library(GWmodel)
library(gstat)
sessionInfo()
remove.packages("GWmodel")
bw.gw.pca <- bw.gwpca(state.gaskor, vars = colnames(state.gaskor@data),
                      k = 5, 
                      robust = FALSE, 
                      adaptive = TRUE)
View(bw.gw.pca)
bw.gw.pca


#gw.pca<- gwpca(state.gaskor, vars = colnames(state.gaskor@data), bw=1000000, k = 5, robust = FALSE, adaptive = TRUE)

gw.pca<- gwpca(state.gaskor, vars = colnames(state.gaskor@data), bw=bw.gw.pca, k = 5, robust = FALSE, adaptive = TRUE)
state.gaskor@data
gw.pca$var
View(ptv)
ptv <- gw.pca$SDF
ptv
loading_gwpca <- gw.pca$loadings
loading_gwpca
gw.pca
gw.pca$GW.arguments

gw.pca$gwpca.scores
gw.pca$local.PV
cobabro <- gw.pca$loadings[, , 2] 
cobabro
View(cobabro)
sdfnya <- gw.pca$SDF
View(sdfnya)

install.packages("writexl")
library("writexl")
write.xlsx(gw.pca$loadings,"D:\\bae\\gw.pca.loading.xlsx")
write.xlsx(gw.pca$var,"D:\\bae\\gw.pca.var.xlsx")
write.xlsx(gw.pca$local.PV,"D:\\bae\\gw.pca.local.pv.xlsx")

install.packages("xlsx")
library("xlsx")

prop.var <- function(gwpca.obj, n.components) {return((rowSums(gwpca.obj$var[, 1:n.components]) /rowSums(gwpca.obj$var)) * 100)}
var.gwpca <- prop.var(gw.pca, 3)
gas1$var.gwpca <- var.gwpca  
var.gwpca
polys<- list("sp.lines", as(peta, "SpatialLines"), col="grey", lwd=.8,lty=1)
col.palette<-colorRampPalette(c("blue",  "sky blue", "green","yellow", "red"),space="rgb",interpolate = "linear")  
mypalette.4 <- brewer.pal(8, "YlGnBu")
spplot(gas1, "var.gwpca", key.space = "right",  cuts = 7, sp.layout =list(polys),col="transparent",main = "Percent Total Variation for Local components 1 to 3")
loadings.pc1 <- gw.pca$loadings[, , 1]

win.item = max.col(abs(loadings.pc1))
gas1$win.item <- win.item
mypalette.4 <- c("lightpink", "blue", "grey", "purple",  "green")
spplot(gas1, "win.item", key.space = "right", at = c(1, 2, 3, 4, 5), main = "Winning variable: highest \n abs. loading on local Comp.1", sp.layout =list(polys))
win.item

write.xlsx(win.item,"D:\\bae\\win.item.xlsx")
write.xlsx(win.item2,"D:\\bae\\win.item2.xlsx")

loadings.pc2 <- gw.pca$loadings[, , 2]
win.item2 <- max.col(abs(loadings.pc2))
win.item2

loadings.pc3 <- gw.pca$loadings[, , 3]
win.item3 <- max.col(abs(loadings.pc3))
win.item3
write.xlsx(win.item3,"D:\\bae\\win.item3.xlsx")

loadings.pc4 <- gw.pca$loadings[, , 4]
win.item4 <- max.col(abs(loadings.pc4))
win.item4
write.xlsx(win.item4,"D:\\bae\\win.item4.xlsx")

loadings.pc5 <- gw.pca$loadings[, , 5]
win.item5 <- max.col(abs(loadings.pc5))
win.item5
write.xlsx(win.item5,"D:\\bae\\win.item5.xlsx")
