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
)
#defino la carpeta donde trabajo
setwd( directory.root )



kexperimento  <- NA   #NA si se corre la primera vez, un valor concreto si es para continuar procesando

kscript         <- "test_pca"

karch_dataset    <- "./datasets/dataset_epic_v952.csv.gz"

get_experimento  <- function()
{
        if( !file.exists( "./maestro.yaml" ) )  cat( file="./maestro.yaml", "experimento: 1000" )
        
        exp  <- read_yaml( "./maestro.yaml" )
        experimento_actual  <- exp$experimento
        
        exp$experimento  <- as.integer(exp$experimento + 1)
        Sys.chmod( "./maestro.yaml", mode = "0644", use_umask = TRUE)
        write_yaml( exp, "./maestro.yaml" )
        Sys.chmod( "./maestro.yaml", mode = "0444", use_umask = TRUE) #dejo el archivo readonly
        
        return( experimento_actual )
}


dataset  <- fread(karch_dataset)
#dataset2<-dataset
#dataset2[-c("numero_de_cliente","foto_mes")]<-scale(dataset[-c("numero_de_cliente","foto_mes")])
datasets_mini <- read_csv("C:/Users/macar/Downloads/datasets_mini.csv")
exc<-c("numero_de_cliente","foto_mes")
library(dplyr)
a<-dplyr::select_if(select(datasets_mini, -one_of(exc)), is.numeric)
#a<-a[ , which(apply(a, 2, var) != 0)]
dataset.pcr<- prcomp(a, center = TRUE,scale. = TRUE)
View(names(a))

library(missMDA)
imputePCA(a,method="EM",ncp=1)

library(irlba)
j<-irlba(as.matrix(a), maxit = 100)#,  center = TRUE, scale. = FALSE)

library(sparsepca)
spca(a)

c("cliente_edad" ,"cliente_antiguedad"  , 
  "cproductos","tpaquete" ,  
  "tpaquete"  ,  "tpaquete" ,  
  "tcuentas"  ,"ccuenta_corriente"  ,  
  "mcuenta_corriente_adicional", "mcuenta_corriente", 
  "ccaja_ahorro"  ,  "mcaja_ahorro" ,  
  "mcaja_ahorro_adicional","mcaja_ahorro_dolares" ,
  "mdescubierto_preacordado", "ctarjeta_debito",
  "ctarjeta_visa"  , "ctarjeta_master",
  "cprestamos_prendarios", "mprestamos_prendarios",
  "cprestamos_hipotecarios",  "cplazo_fijo" ,
  "mplazo_fijo_dolares","mplazo_fijo_pesos" ,
  "cinversion" , "minversion_pesos" ,
  "minversion_dolares" ,  "cinversion"  ,  
  "minversion"  ,"cseguro_vida"  , 
  "cseguro_auto", "cseguro_vivienda"  ,
  "cseguro_accidentes_personales",  "ccaja_seguridad" ,  
  "cpayroll_trx" ,"mpayroll", 
  "ccuenta_debitos_automaticos", "mcuenta_debitos_automaticos",
  "ctarjeta_master_debitos_automaticos",  "mttarjeta_master_debitos_automaticos",
  "cpagodeservicios"  , "mpagodeservicios"  ,
  "cpagomiscuentas"  ,  "mpagomiscuentas",
  "ccomisiones_mantenimiento","mcomisiones_mantenimiento"  ,
  "cforex", "cforex_buy"  ,
  "mforex_buy" ,  "cforex_sell" ,
  "mforex_sell"  ,"ctransferencias_recibidas"  ,
  "mtransferencias_recibidas"  , "ctransferencias_emitidas",
  "mtransferencias_emitidas"  ,  "mes")
