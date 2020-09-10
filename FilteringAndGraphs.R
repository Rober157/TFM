library(data.table)
library(Rcpp)
library(bit64)
library(dplyr)
#detectar lang
library(pacman)
pacman::p_load("cld2")
pacman::p_load("cld3")
#texto
library(tm)
library(readr)
library(stringi)
#ciudades
library(maps)

#####################################################################
############## Ciudades #############################################

data(world.cities)
world.cities = world.cities[world.cities$pop >= 100000,]
paises_hisp = c("Argentina", "Bolivia", "Chile", "Colombia", "Costa Rica","Cuba",
                "Ecuador","El Salvador", "Guatemala", "Honduras",
                'Mexico', 'Nicaragua', 'Panam?', 'Paraguay', 'Peru', 'Puerto Rico',
                "Dominican Republic", 'Uruguay', 'Venezuela', 'Spain','Guinea')
ciudades_hisp = c()
for(i in paises_hisp){
  ciudades = world.cities[world.cities$country.etc == i,1]
  ciudades_hisp = c(ciudades_hisp,ciudades)
}
ciudades_hisp = c(ciudades_hisp,paises_hisp,'Espa?a','Rep?blica Dominicana')

ciudades_hisp = tolower(ciudades_hisp)
ciudades_hisp = unique(unlist(strsplit(ciudades_hisp, split = ' ')))
ciudades_hisp = sort(ciudades_hisp)
stops =  stopwords(kind = "es")
ciudades_hisp = setdiff(ciudades_hisp,stops)
ciudades_hisp = setdiff(ciudades_hisp,c('berlin','italia','francia','co','angeles','nueva','san'
                                        ,'santa','nueva','toronto','island','milan','canada',
                                        'cairo'))

#####################################################################
############## Funciones #############################################

#### SIN NA #########################################################
process_sin_na = function(nombre_archivo){
  data = data.table::fread(nombre_archivo)
  colnames(data) = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12",
                     "V13","V14","V15","V16","V17","V18")
  data[,V2:=NULL]
  data[,V3:=NULL]
  data[,V17:=NULL]
  data[,V18:=NULL]
  data = data[V6 == FALSE,]
  #Idioma
  data = data[(V14 %in% c("eu","ca","es-MX","gl","es","es-ES")) | V16 == TRUE , ]
  
  #### Fechas
  dtparts = t(as.data.frame(strsplit(as.character(data[,V12]),' ')))
  row.names(dtparts) = NULL
  dtparts = dtparts[,-2]
  dtparts = as.Date(dtparts)
  fecha_inicio = as.Date("2019-09-01")
  days_twitter = as.numeric(fecha_inicio-dtparts)
  data[,V12:=NULL]
  data = cbind(data,days_twitter)
  
  ### Frecuencias
  data = data %>% mutate(frec_twits = V10/days_twitter)
  data = data %>% mutate(frec_favs = V11/days_twitter)
  data$V10 = NULL
  data$V11 = NULL
  data = data.table(data)
  data
}



##################################################################################
####################### FILTRADO CON NAs #########################################

process_con_na = function(nombre_archivo){
  data = data.table::fread(nombre_archivo)
  colnames(data) = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12",
                     "V13","V14","V15","V16","V17","V18")
  data[,V2:=NULL]
  data[,V3:=NULL]
  data[,V17:=NULL]
  data[,V18:=NULL]
  
  ### Cuentas Privadas
  data = data[V6 == FALSE,]
  
  #### Fechas
  dtparts = t(as.data.frame(strsplit(as.character(data[,V12]),' ')))
  row.names(dtparts) = NULL
  dtparts = dtparts[,-2]
  dtparts = as.Date(dtparts)
  fecha_inicio = as.Date("2019-09-01")
  days_twitter = as.numeric(fecha_inicio-dtparts)
  data[,V12:=NULL]
  data = cbind(data,days_twitter)
  
  ### Frecuencias
  data = data %>% mutate(frec_twits = V10/days_twitter)
  data = data %>% mutate(frec_favs = V11/days_twitter)
  data$favourites_count = NULL
  data$statuses_count = NULL
  data = data.table(data)
  
  ##### Lenguaje descripcion
  lang1 = cld2::detect_language(text = data[,V5], plain_text = FALSE)
  lang2 = cld3::detect_language(text = data[,V5])
  data = cbind(data,lang1,lang2)
  
  data = data %>% filter((lang1 == "es" | lang2 == "es") | (is.na(lang1) | is.na(lang2)| lang2 == 'ja')
                         |(lang1 == 'gl' | lang2 == 'gl')|(lang1 == 'ca' | lang2 == 'ca')
                         |(lang1 == 'eu') | (lang2 == 'eu')|  V16 == 0 | V13 == TRUE)
  
  data = data.table(data)
  
  ##### Filtrado por pa?s
  data1 = data[lang1 == 'es' | lang2 == 'es' | lang1 == 'gl' | lang2 == 'gl' | 
                 lang1 == 'ca' | lang2 == 'ca'| lang1 == 'eu' | lang2 == 'eu'| V16 == 0 | V13 == TRUE]
  
  data2 = setdiff(data,data1)
  
  texto = data2$V4
  texto = parse_character(texto, locale=locale(encoding="UTF-8"))
  texto = stringi::stri_trans_general(texto, "latin-ascii")
  texto = gsub("[^a-zA-Z]+", " ", texto)
  texto = tolower(texto)
  
  esp = c()
  for(i in 1:length(texto)){
    A = strsplit(texto[i],split = ' ')
    chars = length(A[[1]])
    for(j in 1:chars){
      if(A[[1]][j] %in% ciudades_hisp){
        esp = c(esp,TRUE)
        break}
      if(j == chars){
        esp = c(esp,FALSE)
      }
    }
    
  }
  data2 = data2[esp,]
  
  data = rbind(data1,data2)
  data}

#######################################################################



#########################################################
#################### Pseudoscience ######################

data = data.table::fread('naukas.nodes.csv')
data = data[community_id =='magufos',]
data = data[protected == FALSE,]

write.csv(data,'magufos.csv',row.names = FALSE,col.names = FALSE)
data = data.table::fread('magufos.csv')
data_pro = process_con_na('magufos.csv')

data_pro = data_pro[,V10 := NULL]
data_pro = data_pro[,V11 := NULL]
data_pro = data_pro[,lang1 := NULL]
data_pro = data_pro[,lang2 := NULL]


# write.csv(data_pro,'magufosPro.csv',row.names = FALSE,col.names = FALSE)


###########################################
############### Naukas ###################

data = data.table::fread('naukas.nodes.csv')
data = data[community_id =='Naukas',]
data = data[protected == FALSE,]
data = data[-1,] #eliminamos a i?aki

data = data[(lang == '')|(is.na(lang)),]



# write.csv(data,'naukasNA.csv',row.names = FALSE,col.names = FALSE)
# write.csv(data,'naukas.csv',row.names = FALSE,col.names = FALSE)

data_pro = process_sin_na('naukas.csv')
data_pro2 = process_con_na('naukasNA.csv')

# write.csv(data_pro,'naukasSinNApro.csv',row.names = FALSE,col.names = FALSE)
# write.csv(data_pro2,'naukasNApro.csv',row.names = FALSE,col.names = FALSE)

data_pro = data.table::fread('naukasSinNApro.csv')
data_pro = data_pro[V7 >= 100 | V16 == 0,]

data_pro2 = data_pro2[,V10 := NULL]
data_pro2 = data_pro2[,V11 := NULL]
data_pro2 = data_pro2[,lang1 := NULL]
data_pro2 = data_pro2[,lang2 := NULL]

data_final = rbind(data_pro,data_pro2)

# write.csv(data_final,'naukasFinal.csv',row.names = FALSE,col.names = FALSE)

###############################################################
################ Filtrados #####################################
data1 = data.table::fread('naukasFinal.csv')
data2 = data.table::fread('magufosPro.csv')
quantile(data1[,frec_twits])
quantile(data2[,frec_twits])
data22 = data2[ (V7 >= 100)  | V16 == 0 | V13 == TRUE,]
hist(log(data1[,frec_twits]))
hist(log(data2[,frec_twits]))

data_11 = data1[ (frec_twits >= 13 & V7 >= 100) | V16 == 0 | V13 == TRUE,]
data_22 = data2[ (V7 >= 100 & frec_twits >= 0.5)  | V16 == 0 | V13 == TRUE,]

data_f = rbind(data_11,data_22)

write.csv(data_f,'nodes_final.csv',row.names = FALSE,col.names = FALSE)
nodes = data.table::fread('nodes_final.csv')

nodes[V1 == 463259699,]

##################################################################
###################### Components #################################
edges = data.table::fread('edges_final.csv')
unicos = unique(c(edges[,from], edges[,to]))

magufos = data.table::fread('magufosPro.csv')
magufos = magufos[ (V7 >= 100 & frec_twits >= 0.5)  | V16 == 0 | V13 == TRUE,]
nrow(magufos[V16 ==2,])/nrow(magufos)
nrow(magufos)
sum(magufos[,V1] %in% unicos) #cuantos perd?

##################################################################
##################### Modularity #################################
modus = data.table::fread('modus.csv')
modus = modus[,c(1,4)]

magufos = data.table::fread('magufosPro.csv')
sum(magufos[,V1] %in% modus[,Id]) #compruebo

magufos_in = modus[,Id] %in% magufos[,V1]
table(modus[magufos_in,modularity_class])

percentages = c()
for(i in 0:19){
  percentage = (table(modus[magufos_in,modularity_class])[i]/table(modus[,modularity_class])[i])*100
  percentages = c(percentages,percentage)
}
percentages

########################################################
##################### Degreee #########################
library(igraph)

ids = modus[modularity_class == 18,Id]
edges = data.table::fread('edges_final.csv')
indices_from = edges[,from] %in% ids
indices_to = edges[,to] %in% ids
comb = indices_from & indices_to
data_edges = edges[comb,]
nrow(data_edges)


network = graph.data.frame(data_edges) 
nw.union.vertices = sort.int(igraph::degree(network),decreasing=TRUE,index.return=TRUE)
nw.union.vertices$x[1:10]

########################################### 
################ Heat map ################

Chile = c(0,0.2,6.9,8.7,7.4,7.6,2.5,8.0,7.3)
EEUU = c(0.2,0,0.6,2.0,0.7,0.7,0.8,0.8,0.6)
Scientifics_Influencers = c(6.9,0.6,0,50.4,	80.4,	39.1,	2.9,8.3,10.8)
Brands = c(8.7,2.0,50.4,0, 59.4,	40.7,	5.03,	12.8,	13.2)
Left_wing = c(7.4,0.7,80.4,59.4,0,63.2,	8.3,	9.8,	12.2)
Right_wing = c(7.6, 0.7, 39.1, 40.7, 63.2, 0,4.30,	18.4,	12.6)
Argentina = c(2.5,0.8,2.9,5.0,8.3,4.3,0,6.1,3.3)
Venezuela = c(8.0,0.8,8.3,12.8,9.8,18.4,6.1,0,11.1)
Colombia = c(7.3,0.6,10.8,13.2,12.2,12.6,3.3,11.1,0)


connections = data.frame(Chile,EEUU,Scientifics_Influencers,Brands,Left_wing,Right_wing,Argentina,
                         Venezuela,Colombia)
m = as.matrix(connections)
m = t(m)
m
colnames(m) = c('Chile','EEUU','Influen','Brands','Left','Right','Argen','Venez','Colom')
heatmap(m,scale ="row",symm = F,Rowv=NA, Colv=NA, main="Connection Density between communities")

library(gplots)
par(oma=c(3, 5, 0, 10))
heatmap.2(m,scale ="row",symm = F,Rowv=F, Colv=F,trace = 'none',
          col = rev(heat.colors(999)), density.info=c("none"))



##############################################
################## Density ##################
library(ggplot2)
Edge_Density = c(8.5,5.4,26.6,51.2,122.7,45.5,8.0,10.4,43.1,20.2)
Cluster = c('Chile','EEUU','Scientifics_Influencers','Brands','Left_wing','Right_wing','Argentina',
            'Venezuela','Colombia','Pseudoscience')
df = data.frame(Edge_Density,Cluster)

p<-ggplot(data=df, aes(x=reorder(Cluster, -Edge_Density), y=Edge_Density,fill=Cluster)) +
  geom_bar(stat="identity")+ggtitle('Edge Density per community')+ylab(expression('Edge Density ? 10'^'-5'))+xlab('Cluster')
p + coord_flip()

##############################################
############# Community percentage ###########

Percentage = c(20.64,13.65,13.62,12.69,11.31,10.11,8.5,5.89,3.44)
Cluster = c('Venezuela','Argentina','Chile','Scienctifics and Comedians','EEUU','Spanish Left-Wing','Spanish Right-Wing',
            'Brands','Colombia')
p<-ggplot(data=df, aes(x=reorder(Cluster, -Percentage), y=Percentage,fill=Cluster)) +
  geom_bar(stat="identity")+ggtitle('Network community composition')+ylab(expression('Percentage of the total Network'))+xlab('Cluster')
p + coord_flip()

####################################################
############ Pseudoscience percentages ##############

Percentage = c(5.3,7.4,0.4,0.2,0.2,0.7,2.4,2.9,1.8)
Cluster = c('Chile','EEUU','Scientifics_Influencers','Brands','Left_wing','Right_wing','Argentina',
            'Venezuela','Colombia')

p<-ggplot(data=df, aes(x=reorder(Cluster, -Percentage), y=Percentage,fill=Cluster)) +
  geom_bar(stat="identity")+ggtitle('Percentage of Pseudoscience in each community')+ylab(expression('Percentage of each community'))+xlab('Community')
p + coord_flip()+ geom_text(aes(label=Percentage), position=position_dodge(width=0.9), hjust=-0.1)

###############################################
############### Forcades and @aberron ########

# forcades 974438815
# aberron 10274252

edges = data.table::fread('edges_final.csv')

forcades = edges[from == 974438815,to]
length(forcades)
aberron = edges[from == 10274252,to]
length(aberron)
ids = c(forcades,aberron)
sum(table(ids) >= 2)
sum(table(ids) >= 2)/length(unique(ids))*100
length(unique(ids))
library(scales)
df <- data.frame(
  group = c("Following both", "Following one of them"),
  value = c(2.4,97.6)
)

bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

pie + scale_fill_brewer("Following") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5)

ids_2  = intersect(as.character(forcades),as.character(aberron))
nodes = data.table::fread('nodes_final.csv')

library(bit64)

ids_2 = as.integer64(ids_2)


data_int = nodes[V1 %in% ids_2,]

#######################################
######### Degree dist #################

library(igraph)
library(ggplot2)
edges = data.table::fread('edges_final.csv')
network = graph.data.frame(edges) #Creamos la red
df = data.frame(log(degree(network)))
colnames(df) = 'LogDegree'
hist(log(degree(network)))
ggplot(df, aes(x=LogDegree)) + geom_histogram(binwidth=0.5,color="darkblue", fill="lightblue")+ggtitle('Log degree histogram of the whole network')

edges_2 = data.table::fread('comunidad_magufos.csv')
network_2 = graph.data.frame(edges_2) #Creamos la red
df_2 = data.frame(log(degree(network_2)))
colnames(df_2) = 'LogDegree'
ggplot(df_2, aes(x=LogDegree)) + geom_histogram(binwidth=0.5,color="firebrick", fill="indianred1")+
  ggtitle('Log degree histogram of the Pseudoscience network')+coord_cartesian(xlim = c(0, 6))

hist(log(degree(network_2)))


####################################
######### heat map 2 ###############

ratios = read.csv('ratios.csv')
ratios = ratios[-11,]
ratios = ratios[-6,]
ratios = ratios[,-11]
ratios = ratios[,-6]


rownames(ratios) = c('Chile','EEUU','Scientifics_Influencers','Brands','Left_wing','Right_wing','Argentina','Venezuela','Colombia')
colnames(ratios) = c('Chile','EEUU','Influen','Brands','Left','Right','Argen','Venez','Colom')

ratios = ratios*10^6
ratios = as.matrix(ratios)
heatmap(ratios,scale ="none",symm = F,Rowv=NA, Colv=NA, main="Connection Density between communities")

