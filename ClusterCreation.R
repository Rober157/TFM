library(data.table)
library(Rcpp)
library(bit64)
library(dplyr)

########################################
####### Clusters individuales ##########
########################################

modus = data.table::fread('modus.csv') #modularity obtained by gephi
modus = modus[,c(1,4)]

# Cargamos edges

edges = data.table::fread('edges_final.csv')

# Clusters principales del 0 al 9 (menos el 5)

for(i in 0:9){
  if(i == 5){
    next
  }
  ids = modus[modularity_class == i,Id]
  indices_from = edges[,from] %in% ids
  indices_to = edges[,to] %in% ids
  comb = indices_from & indices_to
  new_edges = edges[comb,]
  name = paste0("Cluster",i,".csv")
  write.csv(new_edges, name,row.names = FALSE,col.names = FALSE)
}


###################################################
############## Cluster Pseudoscience ##############
###################################################

data_magufos = data.table::fread('magufosPro.csv')
data_magufos = data_magufos[ (V7 >= 100 & frec_twits >= 0.5)  | V16 == 0 | V13 == TRUE,]

#Cogemos TODOS los edges

edges = data.table::fread('naukas.edges.csv')
edges = edges[ , c("to", "from")]
colnames(edges) = c('from','to') #swap the names (flow information)


ids = data_magufos[,V1]
indices_from = edges[,from] %in% ids
indices_to = edges[,to] %in% ids
comb = indices_from & indices_to
new_edges = edges[comb,]
#write.csv(new_edges, 'comunidad_magufos.csv',row.names = FALSE,col.names = FALSE)

###############################################
############ De Naukas a Pseudoscience ########
###############################################

edges = data.table::fread('edges_final.csv')

data_magufos = data.table::fread('magufosPro.csv')
data_magufos = data_magufos[ (V7 >= 100 & frec_twits >= 0.5)  | V16 == 0 | V13 == TRUE,]

data_naukas = data.table::fread('naukasFinal.csv')
data_naukas = data_naukas[ (frec_twits >= 13 & V7 >= 500) | V16 == 0 | V13 == TRUE,]

ids_mag = data_magufos[,V1]
ids_nau = data_naukas[,V1]
indices_from = edges[,from] %in% ids_nau
indices_to = edges[,to] %in% ids_mag
comb = indices_from & indices_to
new_edges = edges[comb,]
nrow(new_edges)

head(sort(table(new_edges[,from]),decreasing=T))

degrees = sort(table(new_edges[,from]),decreasing=T)[1:6]
df = data.frame(degrees)
colnames(df) = c('ID','Links')

library(ggplot2)
ggplot(df, aes(x=ID,y=Links)) + geom_bar(stat="identity",fill="steelblue")+
  ggtitle('Links from Naukas to Pseudoscience - Top IDs')+ geom_text(aes(label=Links), position=position_dodge(width=1), vjust=-0.3)




data_naukas[V1== 138212728,]

###############################################
############ De Pseudoscience a Naukas ########
###############################################

edges = data.table::fread('edges_final.csv')

data_magufos = data.table::fread('magufosPro.csv')
data_magufos = data_magufos[ (V7 >= 100 & frec_twits >= 0.5)  | V16 == 0 | V13 == TRUE,]

data_naukas = data.table::fread('naukasFinal.csv')
data_naukas = data_naukas[ (frec_twits >= 13 & V7 >= 500) | V16 == 0 | V13 == TRUE,]

ids_mag = data_magufos[,V1]
ids_nau = data_naukas[,V1]
indices_from = edges[,from] %in% ids_mag
indices_to = edges[,to] %in% ids_nau
comb = indices_from & indices_to
new_edges = edges[comb,]
nrow(new_edges)

head(sort(table(new_edges[,from]),decreasing=T))

degrees = sort(table(new_edges[,from]),decreasing=T)[1:6]
df = data.frame(degrees)
colnames(df) = c('ID','Links')

library(ggplot2)
ggplot(df, aes(x=ID,y=Links)) + geom_bar(stat="identity",fill="tomato2")+
  ggtitle('Links from Pseudoscience to Naukas - Top IDs')+ geom_text(aes(label=Links), position=position_dodge(width=1), vjust=-0.3)


data_magufos[V1== 974438815 ,]
data_magufos[V1== 613469003 ,]
data_magufos[V1== 382156736 ,]
data_magufos[V1== 117568038 ,]
data_magufos[V1== 199408128 ,]
data_magufos[V1== 19190645  ,]

#################################################
############ Conexion entre clusters ############
#################################################

modus = data.table::fread('modus.csv') #modularity obtained by gephi
modus = modus[,c(1,4)]

edges = data.table::fread('edges_final.csv')


for(i in 0:9){
  for(j in 0:9){
    if((i == 5) | (j == 5) | (j <= i)){
      next
    }
    ids_1 = modus[modularity_class == i,Id]
    ids_2 = modus[modularity_class == j,Id]
    ids = unique(c(ids_1,ids_2))
    indices_from = edges[,from] %in% ids
    indices_to = edges[,to] %in% ids
    comb = indices_from & indices_to
    new_edges = edges[comb,]
    name = paste0("Cluster",i,j,".csv")
    write.csv(new_edges, name,row.names = FALSE,col.names = FALSE)
  }}


##############################################

modus = data.table::fread('modus.csv') #modularity obtained by gephi
modus = modus[,c(1,4)]

edges = data.table::fread('edges_final.csv')

for(i in 0:9){
  for(j in 0:9){
    if((i == 5) | (j == 5) | (j <= i)){
      next
    }
    ids_1 = modus[modularity_class == i,Id]
    ids_2 = modus[modularity_class == j,Id]
    indices_from = edges[,from] %in% ids_1
    indices_to = edges[,to] %in% ids_2
    comb = indices_from & indices_to
    indices_from_2 = edges[,from] %in% ids_2
    indices_to_2 = edges[,to] %in% ids_1
    comb_2 = indices_from & indices_to
    comb_tot = comb | comb_2
    new_edges = edges[comb_tot,]
    name = paste0("Cluster",i,j,".csv")
    write.csv(new_edges, name,row.names = FALSE,col.names = FALSE)
  }}


