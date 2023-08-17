data_loading <- read_csv("D:/bae/local loading.csv")

popnya_baru <- read_excel("E:/agregasi/pop.xlsx",col_names = FALSE)
popnya[,1] <- data.frame(popnya[,1])
force(pop_yak)
pop_yak <- data.frame(pop_yak)
pop_yak <- read_excel("E:/agregasi/pop yak.xlsx",col_types = c("numeric"))

poppulasi <- cbind(pop_yak[,1])
populasi2 <- c(populasi$`pop yak`)
populasi2
data_shp <- readOGR("E:/SIG/514 kabupaten.shp","514 kabupaten")
peta <- readOGR("E:/SIG/indo_and_var.shp","indo_and_var")
dat_shp <- data_shp@data
Id_kab <- dat_shp$idkab

y <- coordinates(data_shp)
coords_kab <- data.frame(Id_kab,y)

data_gabungan[,1] <- as.factor(data_gabungan[,1])
gabung_jarak_data <- inner_join(coords_kab,data_gabungan,by="Id_kab")
gabung_jarak_data <- inner_join(data_gabungan,ambil,by="Id_kab")
coords_akhir <- cbind(gabung_jarak_data[,ncol(gabung_jarak_data)-1],gabung_jarak_data[,ncol(gabung_jarak_data)])
beda <- cbind(gabung_jarak_data[,ncol(gabung_jarak_data)],gabung_jarak_data[,ncol(gabung_jarak_data)])
urut <- arrange(coords_kab, (Id_kab))
datanya <- urut %>% select(-c(1))
baru <- cbind(datanya$X1,datanya$X2)
coord_fix <- spDists(baru, baru, longlat = TRUE)

# initiate parameternn


param_fgwc <- c(ncluster=5,m=3,distance='euclidean',order=3,
                alpha=0.5,a=1,b=1,max.iter=1000,error=1e-6,randomN=10)

## tune the PSO parameter

pso_param <- c(npar=15,
               vmax=0.8, pso.same=10, c1=0.7, c2=0.6, w.inert='chaotic',
               wmax=0.8,wmin=0.3,map=0.3)

nyobaloading <- fgwc(data_loading, populasi2, coord_fix, algorithm = "classic", param_fgwc, opt_param)
