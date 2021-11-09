#Necesita para correr en Google Cloud
#128 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU

#clase_binaria2   1={BAJA+2,BAJA+1}    0={CONTINUA}
#Entrena en a union de VEINTE  meses de [201901, 202009] - { 202006 }  haciendo subsampling al 10% de los continua
#Testea en  { 202011 }
#estima automaticamente la cantidad de registros a enviar al medio de la meseta (en lugar de la prob de corte)

#Optimizacion Bayesiana de hiperparametros de  lightgbm
#funciona automaticamente con EXPERIMENTOS
#va generando incrementalmente salidas para kaggle

# WARNING  usted debe cambiar este script si lo corre en su propio Linux

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


#para poder usarlo en la PC y en la nube sin tener que cambiar la ruta
#cambiar aqui las rutas en su maquina
switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "M:\\" },   #Windows
         Darwin  = { directory.root  <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root  <-  "~/buckets/b1/" } #Google Cloud
         # Linux   = { directory.root  <-  "~/buckets/b1/" } #local
       )
#defino la carpeta donde trabajo
setwd( directory.root )


GLOBAL_iteracion <- 36 #la que corresponda

kexperimento  <- 5002   #el que corresponda
kscript         <- "962_epic"

kimp          <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, "_" )
kmodelitos    <- paste0("./modelitos/E", kexperimento, "_modelitos.csv.gz" )
kkaggle       <- paste0("./kaggle/E",kexperimento, "/E",  kexperimento, "_", kscript, "_" )

karch_dataset    <- "./datasets/dataset_epic_v952.csv.gz"
kapply_mes       <- c(202101)  #El mes donde debo aplicar el modelo
ktrain_meses_malos  <- c( 202006 )  #meses que quiero excluir del entrenamiento
kgen_mes_hasta    <- 202011   #La generacion final para Kaggle, sin undersampling
kgen_mes_desde    <- 201901


#cargo el dataset que tiene los 36 meses
dataset  <- fread(karch_dataset)

#cargo los datos donde voy a aplicar el modelo
dapply  <- copy( dataset[  foto_mes %in% kapply_mes ] )

#creo la clase_binaria2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

#donde entreno
dataset[    foto_mes>= kgen_mes_desde  &
            foto_mes<= kgen_mes_hasta & 
            !( foto_mes %in% ktrain_meses_malos ),
          generacion_final:= 1L ]  


tb_modelitos  <- dataset[  ,  c("numero_de_cliente","foto_mes"), with=FALSE ]
fwrite( tb_modelitos, file= kmodelitos, sep= "," )


x <- list( "learning_rate"= 0.0289933062436432,
           "feature_fraction"= 0.914142998647527,
           "min_data_in_leaf"= 367,
           "num_leaves"= 455,
           "num_iterations"= 461,
           "ratio_corte"= 0.0465659156440689
           )

semilla <- 999983
param_basicos  <- list( objective= "binary",
                        metric= "custom",
                        first_metric_only= TRUE,
                        boost_from_average= TRUE,
                        feature_pre_filter= FALSE,
                        verbosity= -100,
                        seed= semilla,
                        max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                        min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                        lambda_l1= 0.0,         #por ahora, lo dejo fijo
                        lambda_l2= 0.0,         #por ahora, lo dejo fijo
                        max_bin= 31,            #por ahora, lo dejo fijo
                        # num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                        force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
                        )

param_completo  <- c( param_basicos, x )


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), 
                           c("clase_ternaria","clase01", "generacion_final", "entrenamiento", "validacion", "test", "fold", campos_malos) )


FullModelo  <- function( hparam )
{
  #entreno sin undersampling
  dgeneracion  <- lgb.Dataset( data=    data.matrix(  dataset[ generacion_final==1 , campos_buenos, with=FALSE]),
                               label=   dataset[ generacion_final==1, clase01]
                               )
  
  modelo_final  <- lightgbm( data= dgeneracion,
                             param= hparam,
                             verbose= -100
                             )
  
  rm( dgeneracion )  #borro y libero memoria
  gc()
  
  #calculo la importancia de variables
  tb_importancia  <- lgb.importance( model= modelo_final )
  fwrite( tb_importancia, 
          file= paste0( kimp, "imp_", sprintf("%03d", GLOBAL_iteracion), ".txt"),
          sep="\t" )
  
  #Aplico sobre todo el dataset
  prediccion  <- predict( modelo_final, data.matrix( dataset[  , campos_buenos, with=FALSE]) )
  dataset[ , prob := prediccion ]
  tb_modelitos[ dataset, 
                on=c("numero_de_cliente","foto_mes"),  
                paste0( "E", kexperimento,"_", GLOBAL_iteracion ) := i.prob  ]
  
  #Fin primera pasada modelitos
  
  
  prediccion  <- predict( modelo_final, data.matrix( dapply[  , campos_buenos, with=FALSE]) )
  
  predsort  <- sort(prediccion, decreasing=TRUE)
  pos_corte  <- as.integer(hparam$ratio_corte*nrow(dapply))
  prob_corte <- predsort[ pos_corte ]
  Predicted  <- as.integer( prediccion > prob_corte )
  
  entrega  <- as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente, 
                                   "Predicted"= Predicted)  )
  
  #genero el archivo para Kaggle
  fwrite( entrega, 
          file= paste0(kkaggle, sprintf("%03d", GLOBAL_iteracion), ".csv" ),
          sep= "," )
  
  
  
  # base  <- round( pos_corte / 500 ) * 500   - 3000
  # evaluados  <- c( seq(from=base, to=pmax(base+6000,15000), by=500 ) , pos_corte )  
  # evaluados  <- sort( evaluados )
  # 
  # for(  pos  in  evaluados )
  # {
  #   prob_corte  <-  predsort[ pos ]
  #   Predicted  <- as.integer( prediccion > prob_corte )
  #   
  #   entrega  <- as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente, 
  #                                    "Predicted"= Predicted)  )
  #   
  #   #genero el archivo para Kaggle
  #   fwrite( entrega, 
  #           file= paste0(kkagglemeseta, sprintf("%03d", GLOBAL_iteracion), 
  #                        "_",  sprintf( "%05d", pos) ,".csv" ),
  #           sep= "," )
  # }
  
  rm( entrega, Predicted )
}



FullModelo(param_completo)




#apagado de la maquina virtual, pero NO se borra
system( "sleep 10  &&  sudo shutdown -h now", wait=FALSE)

#suicidio,  elimina la maquina virtual directamente
#system( "sleep 10  && 
#        export NAME=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/name -H 'Metadata-Flavor: Google') &&
#        export ZONE=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google') &&
#        gcloud --quiet compute instances delete $NAME --zone=$ZONE",
#        wait=FALSE )


quit( save="no" )


