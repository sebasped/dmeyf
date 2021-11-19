#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

# directory.root  <-  "~/buckets/b1/"
directory.root  <-  "~/dataScience/maestriaDC/2021/DM_en_EyF_2021cuat2/"
setwd( directory.root )

dataset <- fread('./datasetsOri/paquete_premium.csv.gz', nrows = 0.5*10^6)


# dataset[ foto_mes==201801,  mttarjeta_visa_debitos_automaticos_nueva := dataset[ foto_mes ==201801,  mttarjeta_visa_debitos_automaticos*10]] 

# dataset[ foto_mes==201801,  mttarjeta_visa_debitos_automaticos]


# dataset[ foto_mes==201801,  mttarjeta_visa_debitos_automaticos := dataset[ foto_mes==201801,  apply(.SD, 1,mean), .SDcols=c('mttarjeta_visa_debitos_automaticos', 'mttarjeta_visa_debitos_automaticos_nueva')]]


dataset[ foto_mes==201801, anterior:= dataset[ foto_mes ==201801,  mttarjeta_visa_debitos_automaticos*10]]
dataset[ foto_mes==201801, siguiente:= dataset[ foto_mes ==201801,  mttarjeta_visa_debitos_automaticos*30]]
dataset[ foto_mes==201801, mttarjeta_visa_debitos_automaticos:=rowMeans(.SD), .SDcols=c('anterior','siguiente')]

dataset[ foto_mes==201906, anterior:= dataset[ foto_mes ==201905,  mttarjeta_visa_debitos_automaticos]]
dataset[ foto_mes==201906, siguiente:= dataset[ foto_mes ==201907,  mttarjeta_visa_debitos_automaticos]]
dataset[ foto_mes==201906, mttarjeta_visa_debitos_automaticos:=rowMeans(.SD), .SDcols=c('anterior','siguiente')]
dataset[ ,':='(anterior = NULL, siguiente = NULL)]

