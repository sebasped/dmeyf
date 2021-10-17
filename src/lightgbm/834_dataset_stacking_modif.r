#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#4 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

# setwd("~/buckets/b1/crudoB/" )
setwd("~/dataScience/maestriaDC/2021/DM_en_EyF_2021cuat2/")  #Establezco el Working Directory


version  <- "v001"  #cambiar cada vez, asi se tiene versionado del dataset

dataset  <- fread( "./datasets/dataset_epic_simple_v001.csv.gz" )
dataset  <- copy(  dataset[  , c("numero_de_cliente","foto_mes","clase_ternaria"),  with=FALSE] )
# dataset  <- copy(  dataset ) #usa TODAS las columnas: Ídem quitar la línea 18. Los modelitos van a 
#estar arriba en el importance
gc()


#leo TODOS los archivos que estan en la carpeta  modelitos
#y hago el join con  dataset  <numero_de_cliente, foto_mes, clase_ternaria>

archivos  <- list.files( pattern="modelitos.csv.gz", path="./modelitos/" )
for( archivo  in archivos )
{
  darchivo  <- fread( paste0("./modelitos/", archivo ) )
  dataset  <- merge( dataset, darchivo, by=c("numero_de_cliente","foto_mes") )
}

gc()

fwrite( dataset,
        file=paste0( "./datasets/dataset_stacking_", version, ".csv.gz"),
        sep="," )

