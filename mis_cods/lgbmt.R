
rm( list=ls() )
gc()

library("lightgbm")
library("data.table")


switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "C:/Users/macar/Documents/Educacion/Maestria_DM/2021_2C/DMEF" },   #Windows
         Linux   = { directory.root  <-  "~/buckets/b1/" } #Google Cloud
)
setwd( directory.root)

semillas <- c(887113, 894689, 895553, 896723, 900001)
campos_malos  <- c( "numero_de_cliente", "foto_mes", "ccajas_transacciones", "Master_mpagominimo" )

carpeta_datasetsOri <-  "./datasetsOri/"
septiembre <- "paquete_premium_202009.csv"


ds <- fread(paste0(carpeta_datasetsOri, septiembre,collapse = ""), showProgress = FALSE)

clase_binaria <- ifelse(ds$clase_ternaria == "BAJA+2", 1, 0)
ds$clase_ternaria <- NULL

ds_train  <- lgb.Dataset( data=  data.matrix(ds), label= clase_binaria )

ganancia_lgb <- function(probs, datos){
        return( list( "name"= "ganancia", 
                      "value"=  sum( (probs > 0.025  )* ifelse( getinfo(datos, "label")== 1, 48750, -1250 ) ) / 0.2,
                      "higher_better"= TRUE ) )
}

set.seed(17)
m1 <- lgb.cv( data= ds_train,
              eval= ganancia_lgb,
              stratified= TRUE,
              nfold= 5,
              param= list( objective= "binary",
                           max_bin= 15,
                           min_data_in_leaf= 4000,
                           learning_rate= 0.05 
              )
)

