#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require('creditmodel')


#Aqui comienza el programa
setwd("~/dataScience/maestriaDC/2021/DM_en_EyF_2021cuat2/")  #Establezco el Working Directory

datasetA  <- fread( "./datasetsOri/paquete_premium_202009.csv" )
datasetB  <- fread( "./datasetsOri/paquete_premium_202011.csv" )


campos_buenos  <- setdiff(  colnames( datasetA),  c("numero_de_cliente","foto_mes","clase_ternaria" ) )


# psi <- get_psi(dat = as.data.frame(datasetA), dat_test = as.data.frame(datasetB), x = campos_buenos[1])#,occur_time = "apply_date", bins_no = TRUE)


# fwrite( psi, file="./work/psi.csv", sep="," )

# campos_buenos = campos_buenos[1:3]

# psi_all <- get_psi_all(dat = as.data.frame(datasetA), dat_test = as.data.frame(datasetB), x_list = list(campos_buenos))#, parallel=TRUE)#,occur_time = "apply_date", bins_no = TRUE)


# fwrite( psi_all, file="./work/psi_dataDrift.csv", sep="," )


# 
# pdf("./work/densidades_01.pdf")
for( campo in  campos_buenos )
{
  
  psi <- get_psi(dat = as.data.frame(datasetA), dat_test = as.data.frame(datasetB), x = campo)#,occur_time = "apply_date", bins_no = TRUE)
  print(psi)
  fwrite( psi, file="./work/psi_dataDrift.csv", sep=",", append = TRUE )
  if (psi[1,8]>=0.8) {
    psi_reducido <- psi[1,c(1,8)]
    fwrite( psi_reducido, file="./work/psi_dataDrift_resumido.csv", sep=",", append = TRUE )
    
  }
  
# PSI Rules for evaluating the stability of a predictor 
  #Less than 0.02: Very stable 
  #0.02 to 0.1: Stable 
  #0.1 to 0.2: Unstable 
  #0.2 to 0.5: Change 
  #more than 0.5: Great change 
  
  
  
  
  
#   cat( campo, "  " )
#   distA  <- quantile(  datasetA[ , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
#   distB  <- quantile(  datasetB[ , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
#   
#   a1  <- pmin( distA[[1]], distB[[1]] )
#   a2  <- pmax( distA[[2]], distB[[2]] )
#   
#   densidadA  <- density( datasetA[ , get(campo) ] , kernel="gaussian", na.rm=TRUE)
#   densidadB  <- density( datasetB[ , get(campo) ] , kernel="gaussian", na.rm=TRUE)
#   
#   plot(densidadA, 
#        col="blue",
#        xlim= c( a1, a2),
#        main= paste0("Densidades    ",  campo), )
#   
#   lines(densidadB, col="red", lty=2)
#   
#   legend(  "topright",  
#            legend=c("A", "B"),
#            col=c("blue", "red"), lty=c(1,2))
#   
}
# dev.off()
