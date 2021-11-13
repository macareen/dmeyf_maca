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

dataset.pcr<- prcomp(dataset[-c("numero_de_cliente","foto_mes")], center = TRUE,scale. = TRUE)
