library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)


data<-read.csv("C:/Users/TUT/OneDrive/SEMINARIO_III/SCRIPTS/ce_filter.csv")
#datos<-read_csv("C:/Users/TUT/OneDrive/SEMINARIO_III/SCRIPTS/datos.csv")#data original  


datos_orig<-data



library(factoextra)
df_scale<-scale(data)#estandarización de los datos

fviz_nbclust(df_scale, kmeans, method = "wss")#plot para ver el número de clusters


library(cluster)
gap_stat<-clusGap(df_scale, #segundo criterio para determinar el número de clusters
                  FUN=hcut,
                  nstart=2000,
                  K.max = 10,
                  B=5)


gap_stat#resumen del estadístico gap
fviz_gap_stat(gap_stat)#visualización del gréfico

km <- kmeans(data, centers = 3, nstart = 1000)#cluster final con k=3 según los dos criterios anteriores


fviz_cluster(km,data)+theme(text = element_text(size = 20))#muestra cómo quedaron los cluster




datos_orig<-cbind(datos_orig, clus_kmeans=km$cluster)#se van a almacenar los grupos de los tres algoritmos

df_kmeans<-cbind(data, cluster=km$cluster)#data solo con clusters segun kmneans
df_kmeans%>%
  ggplot(aes(x=X, y=Y, color=as.factor(cluster)))+#muestra la distribución espacial de los cluster
  geom_point()


resumen<-df_kmeans%>%group_by(cluster)%>%
  summarise(minimo=min(CE),
            q1=quantile(CE,0.25),
            media=mean(CE),
            mediana=median(CE),
            q3=quantile(CE,0.75),
            maximo=max(CE),
            stand.desv=sd(CE))


df_kmeans%>%
  ggplot(aes(x=as.factor(cluster), y=CE))+
  geom_boxplot(width=0.3)+
  xlab("Grupo")+
  ylab("CE (ds/m)")+
  theme(text = element_text(size=18))

#calculo de indices
#indice de silueta

sil_kmeans<-silhouette(km$cluster,dist(df_scale))#calcula indice de silueta

fviz_silhouette(sil_kmeans)#visualiza indice de silueta

sil_in_kmeans<-mean(sil_kmeans[,3])

wcss_in_kmeans<-km$tot.withinss#suma de cuadrados

#indice Davies-Bouldin
library(clusterSim)
db_in_kmeans<-index.DB(df_scale, km$cluster)$DB


#indices de calidadd varios
library(fpc)
dunn_in_kmeans<-cluster.stats(dist(df_scale), km$cluster)$dunn
dunn2_in_kmeans<-cluster.stats(dist(df_scale), km$cluster)$dunn2
ch_in_kmeans<-cluster.stats(dist(df_scale), km$cluster)$ch


kmean_index<-data.frame(algritmo=c("Kmeans"), 
                        index=c("Silhoette", "WCSS","DB",
                                "Dunn", "Dunn2", "CH"),
                        value=c(sil_in_kmeans,
                                wcss_in_kmeans,
                                db_in_kmeans,
                                dunn_in_kmeans,
                                dunn2_in_kmeans,
                                ch_in_kmeans))

kmean_index$value<-format(kmean_index$value, scientific = FALSE)



write.csv(data_final,"C:/Users/TUT/OneDrive/SEMINARIO_III/SCRIPTS/CE_kmean2s.csv", row.names = F)

data_final%>%
  ggplot(aes(x=as.factor(cluster), y=CE))+
  geom_boxplot(width=0.3)+
  xlab("Grupo")+
  ylab("CE (ds/m)")+
  theme(text = element_text(size=18))

resumen<-data_final%>%group_by(cluster)%>%
  summarise(minimo=min(CE),
            q1=quantile(CE,0.25),
            media=mean(CE),
            mediana=median(CE),
            q3=quantile(CE,0.75),
            maximo=max(CE),
            stand.desv=sd(CE))


write.csv(resumen, "C:/Users/TUT/OneDrive/SEMINARIO_III/SCRIPTS/sumaryKmeans2.csv", row.names = F)




#clasificacion cmeans
library(geocmeans)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(viridis)
library(spdep)
library(tmap)


Data<-data

for (Col in names(Data)){#se estandarizan los valores
  Data[[Col]] <- scale(Data[[Col]])
}


R2s <- sapply(2:10,function(k){ 
  Clust <- kmeans(Data,centers=k,iter.max = 150)
  R2 <- Clust$betweenss / Clust$totss
  return(R2)
})

R2s
Df <- data.frame(K=2:10,
                 R2 = R2s)
ggplot(Df)+
  geom_line(aes(x=K,y=R2s))+
  geom_point(aes(x=K,y=R2s),color="red")+
  xlab("Number of groups")+
  ylab("R2 of classification")+
  theme(text = element_text(size = 12))


#proceso para definor numero de grupos, aparentemente es 4
optimizacion<-NULL

for (i in 2:10) {
  Cmean<-CMeans(Data,i,1.5,500,standardize = FALSE, seed = 456, tol = 0.00001, verbose = FALSE)
  ver<-cbind(grupo=i, as.data.frame(table(calcqualityIndexes(Data, Cmean$Belongings, m = 1.5))))
  optimizacion<-rbind(optimizacion,ver)
  
}


library(tidyr)

optimizacion<-optimizacion[,-8]%>%
  gather(key = "Índice", value = "valor", 2:7)

optimizacion%>%
  filter(Índice%in%c("Silhouette.index", "XieBeni.index"))%>%
  ggplot(aes(x=grupo, y=as.numeric(valor),color=Índice))+
  geom_point(size=4)+
  geom_line(size=1)+
  labs(x="K", y="Valor")+
  theme(text = element_text(size = 20))+
  geom_vline(xintercept = 4, linetype="dashed")


Cmean<-CMeans(Data,4,1.5,500,standardize = FALSE, seed = 456, tol = 0.00001, verbose = FALSE)# se elige 4 grupos

summary(Cmean)



#indices de calidad 
#silhoete
siluete_index_cmean<-calcqualityIndexes(Data, Cmean$Belongings, m = 1.5)$Silhouette.index

#Indice Davies-Bouldin
DB_index_cmean<-calcDaviesBouldin(Cmean$Data, Cmean$Belongings, Cmean$Centers)



#indices de calidad varios

cluster_assignments <- Cmean$Groups

clus_assig<-as.numeric((gsub("V","",cluster_assignments)))



cluster.stats(dist(Data), clus_assig)
dunn_in_cmeans<-cluster.stats(dist(Data), clus_assig)$dunn
dunn2_in_cmeans<-cluster.stats(dist(Data), clus_assig)$dunn2
ch_in_cmeans<-cluster.stats(dist(Data), clus_assig)$ch




# wcss
cluster_assignments <- Cmean$Groups
cluster_assignments<-gsub("V","",cluster_assignments)
cluster_centers <- Cmean$Centers

wcss <- 0
for (i in 1:4) {
  cluster_points <- Data[clus_assig == 1, ]
  wcss <- wcss + sum(rowSums((cluster_points - cluster_centers[1, ])^2))
}

wcss_index_cmeans<-wcss


cmean_index<-data.frame(algritmo=c("Cmeans"), 
                        index=c("Silhoette", "WCSS","DB",
                                "Dunn", "Dunn2", "CH"),
                        value=c(siluete_index_cmean,
                                wcss_index_cmeans,
                                DB_index_cmean,
                                dunn_in_cmeans,
                                dunn2_in_cmeans,
                                ch_in_cmeans))







data_fin2<-data
data_fin2$Cluster <-Cmean$Groups

library(stringr)
data_fin2$Cluster<-gsub("V","",data_fin2$Cluster)
datos_orig<-cbind(datos_orig, clus_cmean=data_fin2$Cluster)
write.csv(data_fin2,"C:/Users/TUT/OneDrive/SEMINARIO_III/SCRIPTS/CE_cmeans2.csv", row.names = F)



data_fin2%>%ggplot(aes(x=as.character(Cluster), y=CE))+
  geom_boxplot(width=0.3)+
  xlab("Grupo")+
  ylab("CE (ds/m)")+
  theme(text = element_text(size=15))

data_fin2%>%ggplot(aes(x=X,y=Y, color=as.character(Cluster)))+
  geom_point()



ver<-data_fin2%>%group_by(Cluster)%>%#resument 5numeros de la clasificacion
  summarise(minimo=min(CE),
            media=mean(CE),
            q1=quantile(CE,0.25),
            mediana=median(CE),
            maximo=max(CE),
            q3=quantile(CE,0.75),
            stand.desv=sd(CE))



write.csv(ver,"C:/Users/TUT/OneDrive/SEMINARIO_III/SCRIPTS/resumen_cmeans2.csv", row.names = F)





#para revisar

calcqualityIndexes(Data, Cmean$Belongings, m = 1.5)


beta_values <- selectParameters("GFCM",data = Data, k = 4, m = 1.5,
                                beta = seq(0,1,0.05), spconsist = FALSE,
                                tol = 0.00001, seed = 456)



params<-beta_values[c("beta","Silhouette.index","XieBeni.index","Explained.inertia")]
colnames(params)<- c("beta", "silhouette index",
                                "Xie and Beni index", "explained inertia")


library(ggplot2)

library(tidyr)
params<-gather(params,key = "index",value = "valor", 2:4)

params%>%ggplot(aes(x=beta, y=valor, color=index))+
  geom_line()+
  geom_point()


GCmean <- GCMeans(Data,k = 5,m = 1.5, beta = 0.25,500,standardize = FALSE, seed=456,
                  tol = 0.00001, verbose = FALSE)
r1 <- calcqualityIndexes(Data,GCmean$Belongings,m=1.5)
r2 <- calcqualityIndexes(Data,Cmean$Belongings,m=1.5)
df <- cbind(unlist(r1), unlist(r2))

knitr::kable(df,
             digits = 3,col.names = c("GFCM", "FCM"))


library(sf)
library(sp)
Data2<-st_as_sf(data,
         coords = c("X","Y"),
         crs=2955)

library(spdep)

coordinates(data) <- ~ X + Y


knea <- knearneigh(coordinates(data), k=5)
neib <- knn2nb(knea)



Neighbours <- poly2nb(Data2,queen = TRUE)
WMat <- nb2listw(neib,style="W",zero.policy = TRUE)
WMat

DFindices_SFCM <- selectParameters(algo = "SFCM", data = Data,
                                   k = 5, m = 1.5, alpha = seq(0,2,0.05),
                                   nblistw = WMat, standardize = FALSE,
                                   tol = 0.0001, verbose = FALSE, seed = 456)




ggplot(DFindices_SFCM)+
  geom_smooth(aes(x=alpha,y=spConsistency), color = "black")+
  geom_point(aes(x=alpha,y=spConsistency), color = "red")


ggplot(DFindices_SFCM)+
  geom_smooth(aes(x=alpha,y=Explained.inertia), color = "black")+
  geom_point(aes(x=alpha,y=Explained.inertia), color = "red")


ggplot(DFindices_SFCM)+
  geom_smooth(aes(x=alpha,y=Silhouette.index), color = "black")+
  geom_point(aes(x=alpha,y=Silhouette.index), color = "red")


ggplot(DFindices_SFCM)+
  geom_smooth(aes(x=alpha,y=XieBeni.index), color = "black")+
  geom_point(aes(x=alpha,y=XieBeni.index), color = "red")






SFCM <- SFCMeans(Data, WMat, k = 4, m = 1.5, alpha = 0,
                 tol = 0.0001, standardize = FALSE,
                 verbose = FALSE, seed = 456)



future::plan(future::multisession(workers=2))


DFindices_SFGCM <- selectParameters.mc(algo = "SGFCM", data = Data,
                                       k = 5, m = 1.5, alpha = seq(0,2,0.05),
                                       beta = seq(0,0.85,0.05),
                                       nblistw = WMat, standardize = FALSE, chunk_size = 50,
                                       tol = 0.0001, verbose = FALSE, seed = 456)




ggplot(DFindices_SFGCM) + 
  geom_raster(aes(x = alpha, y = beta, fill = Silhouette.index),  size = 5) + 
  scale_fill_viridis(direction = -1) +
  coord_fixed(ratio=1)




ggplot(DFindices_SFGCM) + 
  geom_raster(aes(x = alpha, y = beta, fill = XieBeni.index),  size = 5) + 
  scale_fill_viridis(direction = -1) +
  coord_fixed(ratio=1)


ggplot(DFindices_SFGCM) + 
  geom_raster(aes(x = alpha, y = beta, fill = spConsistency),  size = 5) + 
  scale_fill_viridis(direction = -1) +
  coord_fixed(ratio=1)


SGFCM <- SGFCMeans(Data,WMat,k = 5,m=1.5, alpha=0.95, beta = 0.65,
                   tol=0.0001, standardize = FALSE, verbose = FALSE, seed = 456)


scale_fill_vi

r1 <- calcqualityIndexes(Data, SFCM$Belongings,m = 1.5)
r2 <- calcqualityIndexes(Data, SGFCM$Belongings,m = 1.5)

diagSFCM <- spatialDiag(SFCM$Belongings, nblistw = WMat,
                        undecided = 0.45,nrep = 500)
diagSGFCM <- spatialDiag(SGFCM$Belongings, nblistw = WMat,
                         undecided = 0.45,nrep = 500)

df <- cbind(
  c(unlist(r1),diagSFCM$SpConsist),
  c(unlist(r2),diagSGFCM$SpConsist)
)
row.names(df)[length(row.names(df))] <- "sp.consistency"

knitr::kable(df,digits = 3,col.names = c("SFCM","SGFCM"))


####Clasificacion DBSCAN


library(dbscan)
datos<-data

k <- 0.0020 # Porcentaje elegido. 
min_pts <- round(nrow(datos) * k) # Cálculo de min_pts. 
if (min_pts <= 1) { # Elimino la opción de un min_pts < 1 y limito su valor máximo.   
  min_pts <- 2 } else if (min_pts >= 20) {
    min_pts <- 20
  }

min_pts

library(scales)
datos_norm <- data.frame(lapply(datos,rescale))

library(dplyr)

auto_eps <- function(xdf, min_pts) { # Se introduce un dataset de dos columnas normalizadas.
  A <- dbscan::kNNdist(xdf, k = min_pts)
  Ar <- A[order(A)]
  y <- Ar
  x <- c(0:(length(Ar)-1))
  # Normalizo antes de calcular la curvatura.
  xr <- rescale(x)
  yr <- rescale(y)
  curvatura <- array() # Aquí se guardarán las pendientes.
  # Cálculo de pendiente.
  i = 1
  while (i <= length(x)) {
    curvatura[i] <- (yr[i+1]-yr[i])/(xr[i+1]-xr[i])
    i <- i + 1
  }
  # Elijo el primer valor que supere la pendiente m
  m <- 10   
  primer <- first(which(curvatura >= m))
  # Devuelve el valor eps óptimo
  return(y[primer])
}




epsilon <- auto_eps(datos_norm, min_pts)


A <- dbscan::kNNdist(datos_norm, k = min_pts)
Ar <- A[order(A)]
plot(Ar)
abline(h=0.12, col = "red", lty=7)



dbscan::kNNdistplot(datos_norm,k=20)
abline(h=0.12, col = "red", lty=7)

res<-dbscan(datos_norm, 0.12,20)



datos$cluster = res$cluster # Añado la columna clúster a mis datos. 
datos_orig<-cbind(datos_orig, clus_dbscan=res$cluster)
datos_limpios <- dplyr::filter(datos, cluster != 0) # Guardo datos limpios.

outliers <- dplyr::filter(datos, cluster == 0) # Guardo outliers.
write.csv(datos_limpios,"C:/Users/TUT/OneDrive/SEMINARIO_III/SCRIPTS/CE_dbscan.csv", row.names = F)


library(ggplot2)
datos_limpios%>%ggplot(aes(x=as.character(cluster), y=CE))+
  geom_boxplot(width=0.3)+
  xlab("Grupo")+
  ylab("CE (ds/m)")+
  theme(text = element_text(size=15))

library(cluster)
#indices dbscan

#silhoete
sil_dbscan<-silhouette(res$cluster,dist(datos_norm))
summary(sil)
fviz_silhouette(sil)

sil_index_dbscan<-mean(sil[,3])

#Indice Davies-Bouldin
library(clusterSim)

db_index_dbscan<-index.DB(datos_norm, res$cluster)$DB

#indeces varios
cluster.stats(dist(datos_norm), res$cluster)

dunn_in_dbscan<-cluster.stats(dist(datos_norm), res$cluster)$dunn
dunn2_in_cmeans<-cluster.stats(dist(datos_norm), res$cluster)$dunn2
ch_in_cmeans<-cluster.stats(dist(datos_norm), res$cluster)$ch







dbscan_summary<-datos_limpios%>%group_by(cluster)%>%#resument 5numeros de la clasificacion
  summarise(minimo=min(CE),
            media=mean(CE),
            q1=quantile(CE,0.25),
            mediana=median(CE),
            maximo=max(CE),
            q3=quantile(CE,0.75),
            stand.desv=sd(CE))
write.csv(dbscan_summary,"C:/Users/TUT/OneDrive/SEMINARIO_III/SCRIPTS/summary_dbscan.csv", row.names = F)


dbscan_index<-data.frame(algritmo=c("DBSCAN"), 
                         index=c("Silhoette","DB",
                                 "Dunn", "Dunn2", "CH"),
                         value=c(sil_index_dbscan,
                                 db_index_dbscan,
                                 dunn_in_dbscan,
                                 dunn2_in_cmeans,
                                 ch_in_cmeans))



indices<-rbind(cmean_index, kmean_index, dbscan_index)

write.csv(indices,"C:/Users/TUT/OneDrive/SEMINARIO_III/SCRIPTS/summary_indices.csv", row.names = F)
indices<-read.csv("C:/Users/TUT/OneDrive/SEMINARIO_III/SCRIPTS/summary_indices.csv")

indices$value<-as.numeric(format(indices$value, scientific = FALSE))
class(indices$value)

ind2<-NULL

for (i in 1:nrow(indices)) {
  indices2<-indices[i,]
  if (indices2$value<0.5) {
    indices2$value2<-round(indices2$value,4)
  } else if(indices2$value>=0.5 & indices2$value<=1){
    indices2$value2<-round(indices2$value,3)
  }else{
    indices2$value2<-round(indices2$value,0)
  }
  ind2<-rbind(ind2, indices2)
}

ind2$value2<-format(ind2$value2, scientific = F)

library(ggplot2)
library(dplyr)
ind2$value2<-sub('\\.0*$', '', ind2$value2)
ind2$index<-factor(ind2$index, levels = c("Silhoette", "WCSS","DB","CH","Dunn","Dunn2"))

ind2$index<-as.character(ind2$index)
ind2["index"][ind2["index"] == "Dunn2"] <- "DUNN"
ind2$index<-factor(ind2$index, levels = c("Silhoette", "WCSS","DB","CH","Dunn","DUNN"))


ind2%>%filter(!index%in%c("CH","Dunn"))%>%
  ggplot(aes(x=algritmo, y=value2, fill=algritmo))+
  geom_bar(stat = "identity", width=0.3)+
  facet_wrap(.~index, scales = "free")+
  theme(legend.position = "none",
        text = element_text(size=24))+
  labs(y="Valor del índice", x="Algoritmo")


ind2
library(fmsb)
library(ggplot2)
library(ggpubr)
#ggdotchart(ind2, x="algritmo", y="value2",
           group = "index", color = "index", palette = "jco",
           add = "segment", position = position_dodge(0.1),
           facet.by = "algritmo",
           rotate = TRUE, legend="none")+

