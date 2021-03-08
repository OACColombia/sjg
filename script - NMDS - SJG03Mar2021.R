#Código para artículo SJG - sometido 27Jun2020 a Revista Ornitología Colombiana
#Archivo de resometimiento
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("vegan", "ggplot2","ggrepel", "cowplot", "Rmisc", "reshape2","iNEXT",
              "readxl","viridis","dendextend","colorspace","ggdendro","factoextra",
              "NbClust","ape","readxl","devtools", "tanagR")
ipak(packages)#ojo, para usar tanagR debe install_github("cdanielcadena/tanagR")

#Composición, dieta, estrato y masa
dfun<-read.csv("funcSJG.csv")
dfun$Masa.Promedio<-as.character(dfun$Masa.Promedio)
mdfun<-melt(as.data.frame(dfun,id=c("Especie","Masa.Promedio","Categoria.de.Dieta")))
names(mdfun)<-c("Especie","Categoria.de.Dieta","Masa.Promedio","Estrato","Valor")
mdfun$Masa.Promedio<-as.numeric(mdfun$Masa.Promedio)
mdfun<-subset(mdfun,Valor!=0)

#FigA - boxplot Masa vs dietas
  mdfun$Categoria.de.Dieta <- factor(mdfun$Categoria.de.Dieta,levels=c('Plantas/Semillas',
                                                                       'Frutas/Néctar',
                                                                       'Invertebrados',
                                                                       'Omnívoro',
                                                                       'Carnívoro/Carroñero'))

Fa<-ggplot(mdfun,aes(x=Categoria.de.Dieta, y=Masa.Promedio))+
  geom_jitter(width=0.25, alpha=0.5)+
  geom_violin(alpha=0.5)+
  geom_boxplot(width=0.25)+
  scale_y_log10()+
  labs(x="",y="Masa promedio\n (Log10)")+
  theme_bw()+ 
  theme(legend.position = "none",
        axis.line.y=element_blank(),
        axis.text.x=element_blank(),#ojo, inhabilitar si quiere confirmar "label" de cada grupo
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())

Fb<-ggplot(mdfun,aes(x=Estrato, y=Masa.Promedio))+
  geom_jitter(width=0.25, alpha=0.5)+
  geom_violin(alpha=0.5)+
  geom_boxplot(width=0.25)+
  coord_flip()+
  scale_y_log10()+
  labs(x="",y="Masa promedio (Log10)")+
  theme_bw()+ 
  theme(legend.position = "none",
        axis.line.y=element_blank(),
        axis.text.y=element_blank(),#ojo, inhabilitar si quiere confirmar "label" de cada grupo
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())

#Ahora el heatmap
names(tanagr_palettes)
SppCount<-as.data.frame(table(mdfun$Categoria.de.Dieta, mdfun$Estrato))
SppCount<-within(SppCount,Var2 <- ifelse(Var2=="Dentro.de.agua","Dentro de agua",
                                  ifelse(Var2=="Sobre.el.agua","Sobre el agua",
                                  ifelse(Var2=="Piso","Piso",
                                  ifelse(Var2=="Sotobosque","Sotobosque",
                                  ifelse(Var2=="Medio","Medio",
                                  ifelse(Var2=="Dosel","Dosel",
                                  ifelse(Var2=="Aéreo","Aéreo",NA))))))))
SppCount$Var2 <- factor(SppCount$Var2,levels=c('Dentro de agua',
                                               'Sobre el agua',
                                               'Piso',
                                               'Sotobosque',
                                               'Medio',
                                               'Dosel',
                                               'Aéreo'))

Fc<-ggplot(SppCount, aes(x=Var1, y=Var2, fill=Freq)) + 
  geom_tile(color="white", size=0.1)+
  scale_fill_tanagr(palette_name = "tangara_chilensis", guide = "none", discrete  = FALSE)+
  geom_text(aes(label=round(Freq,1)),color="white", size=3.5)+
  theme_bw()+
	labs(x="Categorías de dieta",y="Estrato de forrajeo")+
  theme(legend.position = "none",
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())

#Combinar figuras
ggdraw()+
  draw_plot(Fa, x = 0.05, y = 0.7, width = 0.675, height = 0.3)+
  draw_plot(Fb, x = 0.7, y = 0, width = 0.3, height = 0.725)+
  draw_plot(Fc, x = 0, y = 0, width = 0.725, height = 0.725)

#Guardar esta figura en buena resolución .tiff a 300 dpi 
ggsave("Fig5new.tiff", units="in", width=8, height=7, dpi=300, compression = 'lzw')



##NMDS y Agrupamiento jerárquico 1 - 13 localidades al oriente de Colombia para Afinidades de SJG####
##Llamar datos
data1<-read.csv("loc_comparacion.csv", sep = ",") # ";" si es en lacompu de OAC #Llamar la matriz
df1<-data1[,c(2:876)]#subset de solo especies
data1$Sitio #Nombre de localidades

#Cluster analysis
Labels = data1$Sitio
dis1 <- vegdist(df1,method = "jaccard")#Uso Jaccard por que son datos de incidencia
clus1 <- hclust(dis1, "average")

#Dendrograma con GGPLOT
dend1 <- as.dendrogram(clus1)
# Cambiamos el nombre de los "labels":
labels(dend1) <- paste(as.character(data1[,1])[order.dendrogram(dend1)],
                           "", 
                           sep = "")
#extraemos datos para ggdendrogram
ddata_1 <- dendro_data(dend1)

p1 <- ggplot(segment(ddata_1)) +
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))

#y adicionamos la influencia como grupo
labs <- label(ddata_1)
labs$Agrupamiento <- as.factor(c(1,2,3,3,4,5,5,5,5,5,6,6,6))
labs
#y adicionamos al ggplot
F1B<-p1+ 
  geom_text(data=label(ddata_1),
               aes(label=label, x=x, y=0, colour=labs$Agrupamiento), vjust=-0.2,hjust=1, size=3.5)+
  geom_hline(yintercept=0.45, linetype = "dashed",color="grey", size=.5)+
  coord_flip()+
  scale_y_reverse()+
  scale_colour_manual(values=c("#440154FF", "#404788FF", "#287D8EFF","#29AF7FFF","#95D880FF","#FDE725FF"))+
  labs(x="",y="Disimilitud de Jaccard")+
  theme_bw()+ 
  theme(legend.position = "none",
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())

# Elbow method
fviz_nbclust(df1, kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2)+
  labs(subtitle = "Elbow method")
#Esta función grafica la variación total entrecluster o 
  #suma total de cuadrados entre cluster
# Para dibijar dendrograma con rectangulos (k es el numero de clusters)
plot(clus1, labels=Labels, cex = 1)
rect.hclust(clus1, k = 6, border = 1:6)


#NMDS
gua.mds1<-metaMDS(df1,distance = "jaccard")
gua.mds1 #stress 0.061 con 2 dimensiones; acorde
plot(gua.mds1, type="t", display = "sites")
#Extraer datos para usar GGPLOT
data.scores1<-as.data.frame(scores(gua.mds1))
#combinar con datos originales
data.scores1<-cbind(data.scores1,data1[c(1)])
names(data.scores1)<-c("NMDS1","NMDS2","Sitio")
#asignar el cluster
data.scores1<-within(data.scores1,Agrupamiento <- ifelse(Sitio==" San José del Guaviare","6",
                                  ifelse(Sitio=="iv) Matavén","6",
                                  ifelse(Sitio=="vii) Inírida","6",
                                  ifelse(Sitio=="xii) Leguízamo","5",
                                  ifelse(Sitio=="viii) Medio Putumayo - Algodón","5",
                                  ifelse(Sitio=="v) Vaupés","5",
                                  ifelse(Sitio=="xi) Bajo Putumayo - Cotuhé","5",
                                  ifelse(Sitio=="ii) CIEM","5",
                                  ifelse(Sitio=="i) Chiribiquete","4",
                                  ifelse(Sitio=="ix) Río Pauto","3",
                                  ifelse(Sitio=="iii) Caño Limón","3",
                                  ifelse(Sitio=="x) Río Tame","2",
                                  ifelse(Sitio=="vi) San Martín","1",NA))))))))))))))
head(data.scores1)
#poner riqueza de especies para tamaño
data.scores1<-within(data.scores1,Riqueza <- ifelse(Sitio==" San José del Guaviare","471",
                                  ifelse(Sitio=="iv) Matavén","376",
                                  ifelse(Sitio=="vii) Inírida","463",
                                  ifelse(Sitio=="xii) Leguízamo","531",
                                  ifelse(Sitio=="viii) Medio Putumayo - Algodón","486",
                                  ifelse(Sitio=="v) Vaupés","566",
                                  ifelse(Sitio=="xi) Bajo Putumayo - Cotuhé","346",
                                  ifelse(Sitio=="ii) CIEM","441",
                                  ifelse(Sitio=="i) Chiribiquete","384",
                                  ifelse(Sitio=="ix) Río Pauto","281",
                                  ifelse(Sitio=="iii) Caño Limón","250",
                                  ifelse(Sitio=="x) Río Tame","215",
                                  ifelse(Sitio=="vi) San Martín","91",NA))))))))))))))
head(data.scores1)
data.scores1$Riqueza<-as.numeric(data.scores1$Riqueza)
# Usando GGPLOT ###
# Graficando bonito el NMDS
F1A<-ggplot(data=data.scores1, aes(x=NMDS1,y=NMDS2,label=Sitio)) +
  geom_hline(yintercept = 0,linetype = "solid",color="grey", size=.5)+
  geom_vline(xintercept = 0,linetype = "solid",color="grey", size=.5) +
  geom_point(stroke=0.5, aes(color=Agrupamiento, size=Riqueza))+
  geom_text_repel(size=3.5, force=20, max.iter = 3e3, aes(label=data.scores1$Sitio))+ 
  labs(x="Eje 1 NMDS",y="Eje 2 NMDS")+
  scale_size_continuous(range = c(1,6))+
  scale_color_manual(values=c("#440154FF", "#404788FF", "#287D8EFF","#29AF7FFF","#95D880FF","#FDE725FF"))+
  theme_bw()+
  theme(panel.background=element_rect(fill="white"),
        panel.grid=element_blank(),
        legend.position = "none")+
  annotate("text", x=-0.3, y=-1, label="(Estrés 2D: 0.062)", size=3.5)

#Combinar figuras
ggdraw()+
  draw_plot(F1A, x = 0, y = 0.5, width = 0.93, height = 0.5)+
  draw_plot(F1B, x = 0.041, y = 0, width = 0.89, height = 0.5)+
  draw_plot_label(label = c("B", "C"), size = 10,
                  x = c(0, 0), y = c(1,0.51))
#Guardar esta figura en buena resolución .tiff a 300 dpi 
ggsave("Fig6new.tiff", units="in", width=5.5, height=7, dpi=300, compression = 'lzw')





##Info suplementaria - NMDS y Agrupamiento jerárquico 2 - 7 localidades con solo los especímenes####
##Llamar datos
data2<-read.csv("toNMDSb.csv", sep = ",") # ";" si es en lacompu de OAC #Llamar la matriz
df2<-data2[,c(2:78)]#subset de solo especies
data2$X #Nombre de localidades

#Cluster analysis
dis2 <- vegdist(df2,method = "jaccard")#Uso Jaccard por que son datos de incidencia
clus2 <- hclust(dis2, "average")
data2<-within(data2,Influencia <- ifelse(X=="11-LagunaNegra","Amazónico",
                                  ifelse(X=="13-PlayaGüío","Amazónico",
                                  ifelse(X=="14-PozosNaturales","Guayanés",
                                  ifelse(X=="15-PuentesNaturales","Guayanés",
                                  ifelse(X=="2-Buenavista","Amazónico",
                                  ifelse(X=="6-CiudaddePiedra-Cuevas","Guayanés",
                                  ifelse(X=="7-DiamantedelasAguas","Guayanés",NA))))))))
data2$Especimenes<-c(15,72,1,5,23,5,7)

data2<-within(data2,X <- ifelse(X=="11-LagunaNegra","11-Laguna Negra",
                                    ifelse(X=="13-PlayaGüío","13-Playa Güío",
                                    ifelse(X=="14-PozosNaturales","14-Pozos Naturales",
                                    ifelse(X=="15-PuentesNaturales","15-Puentes Naturales",
                                    ifelse(X=="7-DiamantedelasAguas","7-Diamante de las Aguas",
                                    ifelse(X=="6-CiudaddePiedra-Cuevas","6-Ciudad de Piedra-Cuevas",
                                    ifelse(X=="2-Buenavista","2-Buenavista",NA))))))))


#Dendrograma con GGPLOT
dend2 <- as.dendrogram(clus2)
# Cambiamos el nombre de los "labels":
labels(dend2) <- paste(as.character(data2[,1])[order.dendrogram(dend2)],
                        sep = "")
                            
#extraemos datos para ggdendrogram
ddata_2 <- dendro_data(dend2)

p2 <- ggplot(segment(ddata_2)) +
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))

#y adicionamos la influencia como grupo
labs <- label(ddata_2)
labs$Influencia <- c(rep("Guayanés", 3), rep("Amazónico", 1), rep("Guayanés", 1),rep("Amazónico", 2))
labs
#y adicionamos al ggplot
F2B<-p2+ 
  geom_text(data=label(ddata_2),
               aes(label=label, x=x, y=0, colour=labs$Influencia), vjust=-0.2,hjust=1, size=4)+
  coord_flip()+
  scale_y_reverse()+
  scale_colour_manual(values=c("#E495A5", "#86B875", "#7DB0DD"))+
  labs(x="",y="Disimilitud de Jaccard")+
  theme_bw()+ 
  theme(legend.position = "none",
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())

# Elbow method
fviz_nbclust(df2, kmeans, method = "wss") +
  geom_vline(xintercept = 1, linetype = 2)+
  labs(subtitle = "Elbow method")
#Esta función grafica la variación total entrecluster o 
  #suma total de cuadrados entre cluster

#NMDS
gua.mds2<-metaMDS(df2,distance = "jaccard")
gua.mds2 #stress muy cerca de 0 (4.738772e-05) con 2 dimensiones; acorde
plot(gua.mds2, type="t", display = "sites")
#Extraer datos para usar GGPLOT
data.scores2<-as.data.frame(scores(gua.mds2))
#combinar con datos originales
data.scores2<-cbind(data.scores2,data2[c(1,79,80)])
names(data.scores2)<-c("NMDS1","NMDS2","Sitio","Influencia","Colectas")
#no hubo agrupamientos
head(data.scores2)
# Usando GGPLOT ###
# Graficando bonito el NMDS
F2A<-ggplot(data=data.scores2, aes(x=NMDS1,y=NMDS2,label=Sitio)) +
  geom_hline(yintercept = 0,linetype = "solid",color="grey", size=.5)+
  geom_vline(xintercept = 0,linetype = "solid",color="grey", size=.5) +
  geom_text_repel(size=3.5, force=20, max.iter = 3e3)+ 
  geom_point(stroke=0.5, aes(color=Influencia, size=factor(Colectas)))+
  labs(x="Eje 1 NMDS",y="Eje 2 NMDS")+
  scale_color_manual(values=c("#E495A5", "#86B875"))+
  theme_bw()+
  theme(panel.background=element_rect(fill="white"),
        panel.grid=element_blank(),
        legend.position = "none")+
  annotate("text", x=0.3, y=0.5, label="(Estrés 2D: 4.74e-05)", size=4)

leg2<-ggplot(data=data.scores2, aes(x=NMDS1,y=NMDS2,label=Sitio)) +
  geom_hline(yintercept = 0,linetype = "solid",color="grey", size=.5)+
  geom_vline(xintercept = 0,linetype = "solid",color="grey", size=.5) +
  geom_label_repel(size=3, force=20, max.iter = 3e3, box.padding = unit(0.35, "lines"))+ 
  geom_point(stroke=0.5, aes(color=Influencia, size=factor(Colectas)))+
  labs(x="Eje 1 NMDS",y="Eje 2 NMDS")+
  scale_color_manual(values=c("#E495A5", "#86B875"))+
  theme_bw()+
  theme(panel.background=element_rect(fill="white"),
        panel.grid=element_blank())+
  annotate("text", x=0.3, y=0.5, label="(Estrés 2D: 4.74e-05)", size=4)

g_legend2<- function(a.plot){
  tmp<- ggplot_gtable(ggplot_build(a.plot))
  leg2<-which(sapply(tmp$grobs,function(x)x$name)=="guide-box")
  legend<-tmp$grobs[[leg2]]
  return(legend)}
leg2<-g_legend2(leg2)

#Combinar figuras
ggdraw()+
  draw_plot(F2A, x = 0, y = 0.55, width = 0.83, height = 0.44)+
  draw_plot(F2B, x = 0.041, y = 0, width = 0.79, height = 0.5)+
  draw_plot(leg2,x=0.87, y=0.1, width=0.1,height = 0.8)+
  draw_plot_label(label = c("A", "B"), size = 10,
                  x = c(0, 0), y = c(1,0.51))

#Guardar esta figura en buena resolución .tiff a 300 dpi 
ggsave("FigS2.tiff", units="in", width=8, height=10, dpi=300, compression = 'lzw')

#### Frecuencia de afinidad ####
#Llamar datos

Afinidades = read_excel("Afinidades especies colectadas-Dec52019.xlsx")
summary(Afinidades)

Afinidades$Afinidad <- factor(Afinidades$Afinidad,levels=c('AA','AD','DOA','AO',
                                                           'AE','LL','AM','AD-SA',
                                                           'LL-AO','LL-AE','WSV',
                                                           'AM-AE','AO-AM','LL-AD',
                                                           'SA'))

ggplot(data=Afinidades,aes(Afinidad))+
  geom_bar()+
  theme_bw()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        strip.background = element_rect(colour=NA, fill=NA),
        strip.text = element_text(size = 14))+
  labs(x="\nAfinidad", y="Número de taxones\n (especies y subespecies)\n")+
geom_text(stat='count', aes(label=..count..), vjust=-1)


##Otras aproximaciones viejas####
##Cluster de lugares con especimenes
toNMDS2<-read.csv("toNMDSb.csv", sep = ",") # ";" si es en lacompu de OAC #Llamar la matriz
names(toNMDS2)#visualizar nombres de especies y hasta donde van
df2<-toNMDS2[,c(2:78)]
dis2 <- vegdist(df2,method = "jaccard")#Uso Jaccard por que son datos de incidencia
clus2 <- hclust(dis2, "average")

toNMDS2<-within(toNMDS2,Influencia <- ifelse(Localidad.EpocaPrecipitacion=="11-LagunaNegra-PBaja","Amazónico",
                                            ifelse(Localidad.EpocaPrecipitacion=="13-PlayaGüío-PAlta","Amazónico",
                                            ifelse(Localidad.EpocaPrecipitacion=="13-PlayaGüío-PBaja","Amazónico",
                                            ifelse(Localidad.EpocaPrecipitacion=="14-PozosNaturales-PBaja","Guyanés",
                                            ifelse(Localidad.EpocaPrecipitacion=="15-PuentesNaturales-PBaja","Guyanés",
                                            ifelse(Localidad.EpocaPrecipitacion=="2-Buenavista-PAlta","Amazónico",
                                            ifelse(Localidad.EpocaPrecipitacion=="2-Buenavista-PBaja","Amazónico",
                                            ifelse(Localidad.EpocaPrecipitacion=="6-CiudaddePiedra-Cuevas-PBaja","Guyanés",
                                            ifelse(Localidad.EpocaPrecipitacion=="7-DiamantedelasAguas-PBaja","Guyanés",NA))))))))))
toNMDS2[79]=lapply(toNMDS2[79],as.factor)#poner como factor la influencia
influencia2 <- levels(toNMDS2[,79])

dend2 <- as.dendrogram(clus2)
# order it the closest we can to the order of the observations:
dend2 <- rotate(dend2, 1:9)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend2) <-
   rainbow_hcl(3)[as.numeric(toNMDS2[,79])[order.dendrogram(dend2)]]

# We shall add the flower type to the labels:
labels(dend2) <- paste(as.character(toNMDS2[,1])[order.dendrogram(dend2)],
                           "(",labels(dend2),")", 
                           sep = "")
# We hang the dendrogram a bit:
dend2 <- hang.dendrogram(dend2,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend2 <- set(dend2, "labels_cex", 1)
# And plot:
par(mar = c(4,4,4,8))
plot(dend2,
     main = "Agrupamiento jerárquico localidades con colectas", 
     horiz =  TRUE,  nodePar = list(cex = .007),xlab="Disimilaridad de Jaccard")
legend("bottomright", legend = influencia2, fill = rainbow_hcl(3))

#Jaccard que no les gustó
jacSJG<-vegdist(df,method = "jaccard", diag = T)#corro la distancia Jaccard en la matriz
str(jacSJG)
jacSJG<-as.matrix(jacSJG)
melted_jacSJG <- melt(jacSJG)
head(melted_jacSJG)
#Cambio el nombre identificado en vegdist por los sitios:
melted_jacSJG<-within(melted_jacSJG,Var1 <- ifelse(Var1=="1","2-Buenavista",
                                            ifelse(Var1=="2","3-CascadaLasDelicias",
                                            ifelse(Var1=="3","6-CiudaddePiedra-Cuevas",
                                            ifelse(Var1=="4","7-DiamantedelasAguas",
                                            ifelse(Var1=="5","9-EmbalseLaMaría",
                                            ifelse(Var1=="6","11-LagunaNegra",
                                            ifelse(Var1=="7","13-PlayaGüío",
                                            ifelse(Var1=="8","14-PozosNaturales",
                                            ifelse(Var1=="9","15-PuentesNaturales",
                                            ifelse(Var1=="10","17-SanJosé(city)",
                                            ifelse(Var1=="11","18-Tranquilandia/PuertaOrión",NA))))))))))))

melted_jacSJG<-within(melted_jacSJG,Var2 <- ifelse(Var2=="1","2-Buenavista",
                                            ifelse(Var2=="2","3-CascadaLasDelicias",
                                            ifelse(Var2=="3","6-CiudaddePiedra-Cuevas",
                                            ifelse(Var2=="4","7-DiamantedelasAguas",
                                            ifelse(Var2=="5","9-EmbalseLaMaría",
                                            ifelse(Var2=="6","11-LagunaNegra",
                                            ifelse(Var2=="7","13-PlayaGüío",
                                            ifelse(Var2=="8","14-PozosNaturales",
                                            ifelse(Var2=="9","15-PuentesNaturales",
                                            ifelse(Var2=="10","17-SanJosé(city)",
                                            ifelse(Var2=="11","18-Tranquilandia/PuertaOrión",NA))))))))))))
head(melted_jacSJG)
colnames(melted_jacSJG)<-c("Sitio1","Sitio2","Jaccard")
melted_jacSJG2<-na.omit(melted_jacSJG[order(melted_jacSJG$Jaccard, decreasing = TRUE),])
melted_jacSJG2

#organizo el orden de los sitios
#Y establezco el orden que quiero (Geo-Bio-Antro)
melted_jacSJG2$Sitio1 <- factor(melted_jacSJG2$Sitio1,levels=c('6-CiudaddePiedra-Cuevas',
                                                               '3-CascadaLasDelicias','14-PozosNaturales',
                                                               '11-LagunaNegra','7-DiamantedelasAguas', '2-Buenavista','13-PlayaGüío',
                                                               '15-PuentesNaturales','17-SanJosé(city)','9-EmbalseLaMaría',
                                                             '18-Tranquilandia/PuertaOrión'))
melted_jacSJG2$Sitio2 <- factor(melted_jacSJG2$Sitio2,levels=c('6-CiudaddePiedra-Cuevas',
                                                               '3-CascadaLasDelicias','14-PozosNaturales',
                                                               '11-LagunaNegra','7-DiamantedelasAguas', '2-Buenavista','13-PlayaGüío',
                                                               '15-PuentesNaturales','17-SanJosé(city)','9-EmbalseLaMaría',
                                                             '18-Tranquilandia/PuertaOrión'))

#y grafico el "heatmap" de disimilitud (entre más cerca a 1, más diferente o disimil)
JacA<-ggplot(data = melted_jacSJG2, aes(x=Sitio1, y=Sitio2, fill=Jaccard)) + 
  geom_tile(color="white", size=0.1)+
  scale_fill_viridis(name="Jaccard")+
  geom_text(aes(label=round(Jaccard,1)))+
  theme_bw()+
	labs(x="Sitio",y="Sitio")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


plot(clus,hang = -1, labels=Labels, cex = 1)
plot(clus, labels=Labels, cex = 1)

#Guardar esta figura en buena resolución .tiff a 300 dpi
ggsave("Jaccard.tiff", units="in", width=9, height=7, dpi=300, compression = 'lzw')#Ganó estos


#opcion dos
gua.mds<-metaMDS(df,distance = "jaccard")
gua.mds #stress 0.085 con 2 dimensiones; acorde
plot(gua.mds, type="t", display = "sites")


###### Graficar NMDS bonito - mover datos #####
#Usar ggplot2 para verlo bonito
#sacar las covariables
names(toNMDS)
Locality<-toNMDS$Locality#guarda el nombre de cada sitio
NumVisit<-toNMDS$Nviajes
Influencia<-toNMDS$Influencia
#using ggplot2 for the NMDS plot
data.scores<-as.data.frame(scores(gua.mds))
data.scores$site<-toNMDS$X

head(data.scores)
names(data.scores)

###### Usando GGPLOT ###

# Graficando bonito el NMDS
a=ggplot(data=data.scores, aes(x=NMDS1,y=NMDS2,label=site)) +
  geom_hline(yintercept = 0,linetype = "solid",color="grey", size=.5)+
  geom_vline(xintercept = 0,linetype = "solid",color="grey", size=.5) +
  geom_point(stroke=0.4, size=6)+
  geom_label_repel(size=4, force=1, max.iter = 3e3, box.padding = unit(0.35, "lines"))+ 
  labs(x="NMDS 1",y="NMDS 2")+
  theme_bw()+
  theme(axis.text.x = element_text(size=14),axis.text.y=element_text(size=14),axis.title=element_text(size=16),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(legend.position="none")+
  annotate("text", x=-0.6, y=0.6, label="Estrés 2D: 0.085", size=5)
a#para ver la figura
# Save in PNG 900 x 500
##### Cluster Analysis

#Cluster analysis
Labels = c("2-Buenavista","3-CascadaLasDelicias","6-CiudaddePiedra-Cuevas",
         "7-DiamantedelasAguas","9-EmbalseLaMaría","11-LagunaNegra",
           "13-PlayaGüío","14-PozosNaturales","15-PuentesNaturales",
           "17-SanJosé(city)","18-Tranquilandia/PuertaOrión")
dis <- vegdist(df)
clus <- hclust(dis, "average")
plot(clus, labels=Labels)


#Debo decidir cuantos cluster k-means agrupan, para esto necesito pkgs object
#pkgs <- c("factoextra",  "NbClust")
#install.packages(pkgs)
head(df)

library(factoextra)
library(NbClust)
library("ape")
# Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")
#Esta función grafica la variación total entrecluster o 
  #suma total de cuadrados entre cluster

# Para dibijar dendrograma con rectangulos (k es el numero de clusters)
plot(clus,hang = -1, labels=Labels, cex = 1)
rect.hclust(clus, k = 4, border = 1:4)
# PNG 900 x 420

#Indicator species
library(labdsv)
grp <- cutree(clus, 4)
const(df, grp)
importance(df, grp)
mod <- indval(df, as.numeric(grp))
mod
class(mod)
names(mod)
summary(mod)
summary(mod,type="long",p=0.05,show=p,too.many = 100)
ind_sp_occ=mod$relfrq
write.csv(ind_sp_occ, "ind_sp_occ.csv")
ind_sp_abu=mod$relabu
write.csv(ind_sp_abu, "ind_sp_abu.csv")
ind_sp_val=mod$indval
write.csv(ind_sp_val, "ind_sp_val.csv")
ind_sp1=mod$maxcls
ind_sp1
write.csv(ind_sp1, "ind_sp1.csv")
ind_sp_prob=mod$pval
write.csv(ind_sp_prob, "ind_sp_prob.csv")
