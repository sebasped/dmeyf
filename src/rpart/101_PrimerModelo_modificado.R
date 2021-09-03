#Arbol elemental con libreria  rpart
require("data.table")
require("rpart")

# rm( list=ls() )

#Aqui se debe poner la carpeta de la computadora local
setwd("~/dataScience/maestriaDC/2021/DM_en_EyF_2021cuat2/")  #Establezco el Working Directory

#cargo los datos de 202009 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("./datasetsOri/paquete_premium_202009.csv")

c = rep(1,157)
c[124] = 0.2
c[146] = 0.2

# positiveWeight = 1.0 / (sum(dtrain$clase_ternaria=='BAJA+2') / nrow(dtrain))
# negativeWeight = 1.0 / (sum(dtrain$clase_ternaria!='BAJA+2') / nrow(dtrain))
# positiveWeight = 2
# negativeWeight = 1.0 / (sum(dtrain$clase_ternaria!='BAJA+2') / nrow(dtrain))

# modelWeights <- ifelse(dtrain$clase_ternaria=='BAJA+2', positiveWeight, negativeWeight)
# modelWeights <- ifelse(dtrain$clase_ternaria=='BAJA+2', 1, 5 )

# dtreeModel <- rpart(predFormula, training, weights = modelWeights)

#genero el modelo
modelo  <- rpart("clase_ternaria ~ .",
                 data = dtrain,
                 xval=0,
                 cp=        -2, 
                 minsplit=  10,
                 minbucket=  2,
                 maxdepth=   10)
                 # cost = c,#)
                 # weights = modelWeights)
                 # method = "class",
                 # parms = list(split = 'information'))
                  # parms = list(prior = c(0.4995,0.4995,0.001)))
# parms=list(loss=matrix(c(0,1,1,20,0,40,1,1,0),
              # byrow=TRUE,
              # nrow=3)))


#aplico al modelo  a los datos de 202011

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")

prediccion  <- predict( modelo, dapply , type = "prob") #aplico el modelo

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
# dapply[ , prob_baja1 := prediccion[, "BAJA+1"] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025) ]

# dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025 & prob_baja2 > prob_baja1 ) ]


entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, file="./kaggle/K241_-2_10_2_10.csv", sep="," )
