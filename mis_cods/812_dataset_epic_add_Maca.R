#source("~/buckets/b1/crudoB/R/812_dataset_epic.r")
#Necesita para correr en Google Cloud
#256 GB de memoria RAM
#300 GB de espacio en el disco local
#8 vCPU


#limpio la memoria
rm( list=ls() )
gc()

soyMaca<-T #cambiar directorio depende de quién esté trabajando

switch ( Sys.info()[['sysname']],
         Windows = { if(soyMaca){
                                directory.root  <-  "C:/Users/macar/Documents/Educacion/Maestria_DM/2021_2C/DMEF"} 
                     else{directory.root  <- "~/Desktop/DM/1er_A?o/DMEyF"
                       
                     }},   #Windows
         Linux   = { directory.root  <-  "~/buckets/b1/crudoB" } #Google Cloud
)
setwd(directory.root)
#setwd("~/Desktop/DM/1er_A?o/DMEyF")

require("data.table")
require("Rcpp")
require("rlist")
require("yaml")
require("dplyr")
require("lightgbm")


palancas  <- list()  #variable con las palancas para activar/desactivar

palancas$version  <- "v001"   #Muy importante, ir cambiando la version

palancas$variablesdrift  <- c("ccajas_transacciones", "internet")   #aqui van las columnas que se quieren eliminar

palancas$corregir <-  TRUE    # TRUE o FALSE

palancas$nuevasvars <-  TRUE  #si quiero hacer Feature Engineering manual

palancas$dummiesNA  <-  FALSE #Idea de Santiago Dellachiesa de UAustral

palancas$lag1   <- FALSE    #lag de orden 1
palancas$delta1 <- FALSE    # campo -  lag de orden 1 
palancas$lag2   <- FALSE
palancas$delta2 <- FALSE
palancas$lag3   <- FALSE
palancas$delta3 <- FALSE
palancas$lag4   <- FALSE
palancas$delta4 <- FALSE
palancas$lag5   <- FALSE
palancas$delta5 <- FALSE
palancas$lag6   <- FALSE
palancas$delta6 <- FALSE

palancas$promedio3  <- FALSE  #promedio  de los ultimos 3 meses
palancas$promedio6  <- FALSE

palancas$minimo3  <- FALSE  #minimo de los ultimos 3 meses
palancas$minimo6  <- FALSE

palancas$maximo3  <- FALSE  #maximo de los ultimos 3 meses
palancas$maximo6  <- FALSE

palancas$tendencia6  <- FALSE    #Great power comes with great responsability

palancas$canaritosimportancia  <- FALSE  #si me quedo solo con lo mas importante de canaritosimportancia


#escribo para saber cuales fueron los parametros
write_yaml(  palancas,  paste0( "./work/palanca_",  palancas$version  ,".yaml" ) )

#------------------------------------------------------------------------------

ReportarCampos  <- function( dataset )
{
  cat( "La cantidad de campos es ", ncol(dataset) , "\n" )
}
#------------------------------------------------------------------------------
#Agrega al dataset una variable que va de 1 a 12, el mes, para que el modelo aprenda estacionalidad

AgregarMes  <- function( dataset )
{
  dataset[  , mes := foto_mes %% 100 ]
  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Elimina las variables que uno supone hace Data Drifting

DriftEliminar  <- function( dataset, variables )
{
  dataset[  , c(variables) := NULL ]
  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#A las variables que tienen nulos, les agrega una nueva variable el dummy de is es nulo o no {0, 1}

DummiesNA  <- function( dataset )
{

  nulos  <- colSums( is.na(dataset[foto_mes==202101]) )  #cuento la cantidad de nulos por columna
  colsconNA  <- names( which(  nulos > 0 ) )

  dataset[ , paste0( colsconNA, "_isNA") :=  lapply( .SD,  is.na ),
             .SDcols= colsconNA]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Corrige poniendo a NA las variables que en ese mes estan dañadas

Corregir  <- function( dataset )
{
  #acomodo los errores del dataset

  dataset[ foto_mes==201801,  internet   := NA ]
  dataset[ foto_mes==201801,  thomebanking   := NA ]
  dataset[ foto_mes==201801,  chomebanking_transacciones   := NA ]
  dataset[ foto_mes==201801,  tcallcenter   := NA ]
  dataset[ foto_mes==201801,  ccallcenter_transacciones   := NA ]
  dataset[ foto_mes==201801,  cprestamos_personales   := NA ]
  dataset[ foto_mes==201801,  mprestamos_personales   := NA ]
  dataset[ foto_mes==201801,  mprestamos_hipotecarios  := NA ]
  dataset[ foto_mes==201801,  ccajas_transacciones   := NA ]
  dataset[ foto_mes==201801,  ccajas_consultas   := NA ]
  dataset[ foto_mes==201801,  ccajas_depositos   := NA ]
  dataset[ foto_mes==201801,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==201801,  ccajas_otras   := NA ]

  dataset[ foto_mes==201806,  tcallcenter   :=  NA ]
  dataset[ foto_mes==201806,  ccallcenter_transacciones   :=  NA ]

  dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  :=  NA ]
  dataset[ foto_mes==201904,  mttarjeta_visa_debitos_automaticos := NA ]
  dataset[ foto_mes==201904,  Visa_mfinanciacion_limite := NA ]

  dataset[ foto_mes==201905,  mrentabilidad     := NA ]
  dataset[ foto_mes==201905,  mrentabilidad_annual     := NA ]
  dataset[ foto_mes==201905,  mcomisiones      := NA ]
  dataset[ foto_mes==201905,  mpasivos_margen  := NA ]
  dataset[ foto_mes==201905,  mactivos_margen  := NA ]
  dataset[ foto_mes==201905,  ctarjeta_visa_debitos_automaticos  := NA ]
  dataset[ foto_mes==201905,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201905,  mcomisiones_otras := NA ]

  dataset[ foto_mes==201910,  mpasivos_margen   := NA ]
  dataset[ foto_mes==201910,  mactivos_margen   := NA ]
  dataset[ foto_mes==201910,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones       := NA ]
  dataset[ foto_mes==201910,  mrentabilidad     := NA ]
  dataset[ foto_mes==201910,  mrentabilidad_annual        := NA ]
  dataset[ foto_mes==201910,  chomebanking_transacciones  := NA ]
  dataset[ foto_mes==201910,  ctarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  ctarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  mtarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  mtarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  ccajeros_propios_descuentos := NA ]
  dataset[ foto_mes==201910,  mcajeros_propios_descuentos := NA ]

  dataset[ foto_mes==202001,  cliente_vip   := NA ]

  dataset[ foto_mes==202006,  active_quarter   := NA ]
  dataset[ foto_mes==202006,  internet   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad_annual   := NA ]
  dataset[ foto_mes==202006,  mcomisiones   := NA ]
  dataset[ foto_mes==202006,  mactivos_margen   := NA ]
  dataset[ foto_mes==202006,  mpasivos_margen   := NA ]
  dataset[ foto_mes==202006,  mcuentas_saldo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_debito_transacciones   := NA ]
  dataset[ foto_mes==202006,  mautoservicio   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_visa_transacciones   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_visa_consumo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_master_transacciones   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_master_consumo   := NA ]
  dataset[ foto_mes==202006,  ccomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  mcomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  cextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  mextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  tcallcenter   := NA ]
  dataset[ foto_mes==202006,  ccallcenter_transacciones   := NA ]
  dataset[ foto_mes==202006,  thomebanking   := NA ]
  dataset[ foto_mes==202006,  chomebanking_transacciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_transacciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_consultas   := NA ]
  dataset[ foto_mes==202006,  ccajas_depositos   := NA ]
  dataset[ foto_mes==202006,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_otras   := NA ]
  dataset[ foto_mes==202006,  catm_trx   := NA ]
  dataset[ foto_mes==202006,  matm   := NA ]
  dataset[ foto_mes==202006,  catm_trx_other   := NA ]
  dataset[ foto_mes==202006,  matm_other   := NA ]
  dataset[ foto_mes==202006,  ctrx_quarter   := NA ]
  dataset[ foto_mes==202006,  tmobile_app   := NA ]
  dataset[ foto_mes==202006,  cmobile_app_trx   := NA ]


  dataset[ foto_mes==202010,  internet  := NA ]
  dataset[ foto_mes==202011,  internet  := NA ]
  dataset[ foto_mes==202012,  internet  := NA ]
  dataset[ foto_mes==202101,  internet  := NA ]

  dataset[ foto_mes==202009,  tmobile_app  := NA ]
  dataset[ foto_mes==202010,  tmobile_app  := NA ]
  dataset[ foto_mes==202011,  tmobile_app  := NA ]
  dataset[ foto_mes==202012,  tmobile_app  := NA ]
  dataset[ foto_mes==202101,  tmobile_app  := NA ]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio

AgregarVariables  <- function( dataset )
{
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_status02       := Master_status +  Visa_status ]
  dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]

  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]


  #combino MasterCard y Visa
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  #dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  #dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

  #Aqui debe usted agregar sus propias nuevas variables
  
  cr_transacciones <- c("ctarjeta_debito_transacciones","ctarjeta_visa_transacciones","ctarjeta_master_transacciones"  ,"ccuenta_debitos_automaticos","cpagodeservicios","cpagomiscuentas","ccajeros_propios_descuentos","ctarjeta_visa_descuentos","ccomisiones_mantenimiento","ccomisiones_otras","cforex","ctransferencias_recibidas","ctransferencias_emitidas","cextraccion_autoservicio","ccheques_depositados","ccheques_emitidos","ccheques_depositados_rechazados","ccheques_emitidos_rechazados","ccallcenter_transacciones","chomebanking_transacciones","ccajas_transacciones","ccajas_consultas","ccajas_depositos","ccajas_extracciones","ccajas_otras","catm_trx","catm_trx_other","ctrx_quarter")
  dataset[ , cr_tx_total := rowSums(.SD), .SDcols = cr_transacciones ]
  
  dataset[ , cr_consumo_mes:= (mautoservicio  + mtarjeta_visa_consumo +  mtarjeta_master_consumo + mcuenta_debitos_automaticos +mttarjeta_visa_debitos_automaticos + mttarjeta_master_debitos_automaticos + mpagodeservicios + mpagomiscuentas + mextraccion_autoservicio +Master_cadelantosefectivo+Visa_madelantopesos)/( mpayroll + mtransferencias_recibidas - mtransferencias_emitidas +mcheques_depositados - mcheques_emitidos)]
  
  cr_ingresos <- c("mpayroll","mpayroll2","mcajeros_propios_descuentos","mtarjeta_master_descuentos","mtransferencias_recibidas","mcheques_depositados","mcheques_depositados_rechazados")
  dataset[, cr_ing_total := rowSums(.SD), .SDcols = cr_ingresos]
  
  cr_egresos <- c("mautoservicio","mtarjeta_visa_consumo","mtarjeta_master_consumo","mcuenta_debitos_automaticos","mttarjeta_visa_debitos_automaticos","mttarjeta_master_debitos_automaticos","mpagodeservicios","mpagomiscuentas","mcomisiones_mantenimiento","mcomisiones_otras","mtransferencias_emitidas","mextraccion_autoservicio","mcheques_emitidos")
  dataset[, cr_eg_total := rowSums(.SD), .SDcols = cr_egresos]
  
  cr_gastos<- c("mautoservicio","mtarjeta_visa_consumo","mtarjeta_master_consumo","mcuenta_debitos_automaticos","mttarjeta_visa_debitos_automaticos","mttarjeta_master_debitos_automaticos","mpagodeservicios","mpagomiscuentas","mcomisiones_mantenimiento","mcomisiones_otras")
  dataset[, cr_gastos := rowSums(.SD), .SDcols = cr_gastos]
  
  cr_consumo_tarjeta <- c("mautoservicio","mtarjeta_visa_consumo","mtarjeta_master_consumo")
  dataset[, cr_consumo_tarjeta := rowSums(.SD), .SDcols = cr_consumo_tarjeta]
  
  #dataset[, cr_constarj_gastos := dataset$consumo_tarjeta/dataset$gastos]
  
  cr_pasivos<- c("mprestamos_personales","mprestamos_prendarios","mprestamos_hipotecarios","mcheques_emitidos_rechazados")
  dataset[, cr_pasivos := rowSums(.SD), .SDcols = cr_pasivos]
  
  cr_act_totales <- c("mcuentas_saldo","mplazo_fijo_dolares","mplazo_fijo_pesos","minversion1_pesos","minversion1_dolares","minversion2","mdescubierto_preacordado")
  dataset[, cr_activos := rowSums(.SD), .SDcols = cr_act_totales]
  
  cr_act_usd <- c("mcaja_ahorro_dolares","mplazo_fijo_dolares","minversion1_dolares")
  dataset[, cr_activos_usd := rowSums(.SD), .SDcols = cr_act_usd]
  
  cr_lim_tarj <- c("Master_mlimitecompra","Visa_mlimitecompra")
  dataset[, cr_lim_tarj := rowSums(.SD), .SDcols = cr_lim_tarj]
  
  dataset[ , cr_rt_Visa_mlimitecompra:= Visa_msaldototal/Visa_mlimitecompra ]
  dataset[ , cr_rt_Visa_msaldo:= Visa_msaldototal - Visa_mconsumototal ]
  dataset[ , cr_rt_Visa_mlimitecompra2:= (Visa_mpagado - Visa_msaldototal)/Visa_mlimitecompra  ]
  dataset[ , cr_rt_Visa_pago_min:= Visa_mpagominimo/ Visa_mlimitecompra  ]
  
  dataset[ , cr_rt_Master_mlimitecompra:= Master_msaldototal /Master_mlimitecompra  ]
  dataset[ , cr_rt_Master_msaldo:= Master_msaldototal - Master_mconsumototal ]
  dataset[ , cr_rt_Master_mlimitecompra2:= (Master_mpagado - Master_msaldototal)/Master_mlimitecompra  ]
  dataset[ , cr_rt_Master_pago_min:= Master_mpagominimo/ Master_mlimitecompra ]
  
  cr_cant_prod <- c("tcuentas","ccuenta_corriente","ccaja_ahorro","ctarjeta_debito","ctarjeta_visa","ctarjeta_master","cprestamos_personales","cprestamos_prendarios","cprestamos_hipotecarios","cplazo_fijo","cinversion1","cinversion2","cseguro_vida","cseguro_auto","cseguro_vivienda","cseguro_accidentes_personales","ccaja_seguridad","ctarjeta_visa_debitos_automaticos","ctarjeta_master_debitos_automaticos","tcallcenter","thomebanking")
  dataset[, cr_cant_prod := rowSums(.SD), .SDcols = cr_cant_prod]
  
  dataset[ , cr_cant_transacciones:= ctarjeta_debito_transacciones + ctarjeta_visa_transacciones + ctarjeta_master_transacciones +ccuenta_debitos_automaticos  + ctarjeta_visa_debitos_automaticos +ctarjeta_master_debitos_automaticos + cpagodeservicios + cpagomiscuentas + Master_cconsumos  ]
  
  dataset[ , cr_cant_transf:= ctransferencias_recibidas - ctransferencias_emitidas + ccheques_depositados - ccheques_emitidos  ]
  
  ##Variables "Geographics"
  #Sueldo/edad
  dataset[ , cr_ing_edad:= (dataset$mpayroll+dataset$mpayroll2)/dataset$cliente_edad ]
  
  #Cantidad de productos/ antiguedad
  dataset[ , cr_prod_ant:= cr_cant_prod/dataset$cliente_antiguedad ]
  
  #mdescubierto_preacordado/mpayroll
  dataset[ , cr_desc_pay:= mdescubierto_preacordado/dataset$mpayroll ]
  
  #mcuentas_saldo/mpayroll
  dataset[ , cr_totsaldo_payroll:= mcuentas_saldo/dataset$mpayroll ]
  
  #satisfaccion del  cliente
  dataset[ , cr_satisfaccion:= ccallcenter_transacciones + ccajas_transacciones + ccajas_consultas  ]
  
  #Nuevos ratios
  
  dataset[ , cr_ing_egr:= cr_ing_total /cr_eg_total ]
  dataset[ , cr_egr_act:= cr_eg_total /cr_activos ]
  dataset[ , cr_egr_pay:= cr_eg_total / (mpayroll+mpayroll2) ]
  dataset[ , cr_vis_egr:= Visa_mconsumototal / cr_eg_total ]
  dataset[ , cr_ah_pay:= cr_activos / cr_pasivos ]
  
  #combino MasterCard y Visa
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
  
  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  
  #combino nuevas recien creadas 
  
  dataset[ , cr_eg_total_ctrx_quarter:= cr_eg_total / ctrx_quarter ]
  dataset[ , cr_eg_total_cr_cant_prod:= cr_eg_total / cr_cant_prod ]
  dataset[ , ctrx_quarter_cr_consumo_tarjeta:= ctrx_quarter / cr_consumo_tarjeta ]
  dataset[ , cr_consumo_tarjeta_cr_cant_prod:= cr_consumo_tarjeta / cr_cant_prod ]
  dataset[ , cr_eg_total_cr_consumo_tarjeta:= cr_eg_total / cr_consumo_tarjeta ]
  dataset[ , ctrx_quarter_cr_ing_total:= ctrx_quarter / cr_ing_total ]
  dataset[ , cr_consumo_tarjeta_cr_ing_total:= cr_consumo_tarjeta / cr_ing_total ]
  dataset[ , cr_ing_total_cr_gastos:= cr_ing_total / cr_gastos]
  dataset[ , ctrx_quarter_cr_gastos:= ctrx_quarter / cr_gastos ]
  dataset[ , cr_ing_total_ctarjeta_visa_transacciones:= ctarjeta_visa_transacciones/cr_ing_total  ]
  
  
  dataset[ , cr_eg_total_ctrx_quarter:= cr_eg_total * ctrx_quarter ]
  dataset[ , cr_eg_total_cr_cant_prod:= cr_eg_total * cr_cant_prod ]
  dataset[ , ctrx_quarter_cr_consumo_tarjeta:= ctrx_quarter * cr_consumo_tarjeta ]
  dataset[ , cr_consumo_tarjeta_cr_cant_prod:= cr_consumo_tarjeta * cr_cant_prod ]
  dataset[ , cr_eg_total_cr_consumo_tarjeta:= cr_eg_total * cr_consumo_tarjeta ]
  dataset[ , ctrx_quarter_cr_ing_total:= ctrx_quarter * cr_ing_total ]
  dataset[ , cr_consumo_tarjeta_cr_ing_total:= cr_consumo_tarjeta * cr_ing_total ]
  dataset[ , cr_ing_total_cr_gastos:= cr_ing_total * cr_gastos]
  dataset[ , ctrx_quarter_cr_gastos:= ctrx_quarter * cr_gastos ]
  dataset[ , cr_ing_total_ctarjeta_visa_transacciones:= cr_ing_total * ctarjeta_visa_transacciones ]
  
  #------- variables Maca
  #sumas que no aportan al árbol pero sí a la lógica
  dataset[ , consumos          := rowSums( cbind( mautoservicio, mtarjeta_master_consumo ,mtarjeta_visa_consumo) , na.rm=TRUE ) ]
  dataset[ , deuda          := rowSums( cbind( mprestamos_personales,  mprestamos_prendarios,mprestamos_hipotecarios) , na.rm=TRUE ) ]
  dataset[ , inversiones        := rowSums( cbind( mplazo_fijo_dolares,  mplazo_fijo_pesos,minversion1_pesos,minversion1_dolares,minversion2) , na.rm=TRUE ) ]
  dataset[ , seguros       := rowSums( cbind( cseguro_vida,  cseguro_auto,cseguro_vivienda,cseguro_accidentes_personales) , na.rm=TRUE ) ]
  
  dataset[ , debitos          := rowSums( cbind(mcuenta_debitos_automaticos,  mttarjeta_visa_debitos_automaticos,mttarjeta_master_debitos_automaticos,mpagodeservicios,mpagomiscuentas) , na.rm=TRUE ) ]
  dataset[ , descuentos          := rowSums( cbind(mcajeros_propios_descuentos, mtarjeta_visa_descuentos, mtarjeta_master_descuentos) , na.rm=TRUE ) ]
  dataset[ , comisiones        := rowSums( cbind(mcomisiones_mantenimiento, mcomisiones_otras) , na.rm=TRUE ) ]
  dataset[ , movimientos       := rowSums( cbind(mforex_buy,mforex_sell, mtransferencias_recibidas, mtransferencias_emitidas,mextraccion_autoservicio,mcheques_depositados, mcheques_emitidos) , na.rm=TRUE ) ]
  
  dataset[ , comunicacion          := rowSums( cbind( ccallcenter_transacciones, ccajas_consultas) , na.rm=TRUE ) ]
  dataset[ , cantidad_transacciones          := rowSums( cbind(chomebanking_transacciones,ccallcenter_transacciones,ccajas_transacciones,ccajas_depositos,ccajas_extracciones, ccajas_otras,catm_trx,cmobile_app_trx) , na.rm=TRUE ) ]
  dataset[ , atm        := rowSums( cbind( matm,matm_other) , na.rm=TRUE ) ]
  
  #Hacer categóricas a las variables
  dataset[ , cut_edad         := cut(cliente_edad,4) ]
  dataset[ , cut_antiguedad         := cut(cliente_antiguedad,3) ]
  dataset[ , cut_rent         := cut(mrentabilidad,2) ]
  dataset[ , cut_rent_an         := cut(mrentabilidad_annual,2) ]
  dataset[ , cut_cc         := cut(mcuenta_corriente,3) ]
  dataset[ , cut_ca         := cut(mcaja_ahorro,3) ]
  dataset[ , cut_consumo_t         := cut(mv_mconsumototal,3) ]
  
  dataset[ , cut_consumos         := cut(consumos,4) ]
  dataset[ , cut_deuda         := cut(deuda,3) ]
  dataset[ , cut_inversiones         := cut(inversiones,3) ]
  dataset[ , cut_seguros         := cut(seguros,2) ]
  dataset[ , cut_debitos         := cut(debitos,3) ]
  dataset[ , cut_comisiones         := cut(comisiones,3) ]
  dataset[ , cut_movimientos         := cut(movimientos,5) ]
  dataset[ , cut_atm         := cut(atm,5) ]
  
  #---- agregado automático de features: pesado para la memoria!!
  nums <- as.data.frame(dplyr::select_if(dataset, is.numeric))
  memory.limit(size=20000)
  datasetx<-as.data.frame(dataset)
  var = names(nums[,c(6:113)])
  
  var2=var
  for(i in var){ 
    for(j in var2){
      if(i!=j){
        var_1<- datasetx[,i] * datasetx[,j] 
        datasetx<-cbind(datasetx,var_1)
        colnames(datasetx)[which(names(datasetx) == "var_1")]<-paste(i,j,sep="x")
        if(datasetx[,j]!=0 & !is.na(datasetx[,j])){
          var_2<-datasetx[,i] / datasetx[,j] 
          datasetx<-cbind(datasetx,var_2)
          colnames(datasetx)[which(names(datasetx) == "var_2")]<-paste(i,j,sep="_div_")
        }}}
    var_3 <- log(datasetx[,i])   
    datasetx<-cbind(datasetx,var_3)
    colnames(datasetx)[which(names(datasetx) == "var_3")]<-paste("log",i)
    var2=var2[-1]
    if (length(var2==0)){print("Corriendo...")}
    
  } 
  
  gc()
  
  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <- 0
  }

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#esta funcion supone que dataset esta ordenado por   <numero_de_cliente, foto_mes>
#calcula el lag y el delta lag

Lags  <- function( dataset, cols, nlag, deltas )
{

  sufijo  <- paste0( "_lag", nlag )

  dataset[ , paste0( cols, sufijo) := shift(.SD, nlag, NA, "lag"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  #agrego los deltas de los lags, con un "for" nada elegante
  if( deltas )
  {
    sufijodelta  <- paste0( "_delta", nlag )

    for( vcol in cols )
    {
     dataset[,  paste0(vcol, sufijodelta) := get( vcol)  - get(paste0( vcol, sufijo))]
    }
  }

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#calcula el promedio de los ultimos  nhistoria meses

Promedios  <- function( dataset, cols, nhistoria )
{

  sufijo  <- paste0( "_avg", nhistoria )
  
  dataset[ , paste0( cols, sufijo) := frollmean(x=.SD, n=nhistoria, na.rm=TRUE, algo="fast", align="right"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#calcula el minimo de los ultimos  nhistoria meses

Minimos  <- function( dataset, cols, nhistoria )
{

  sufijo  <- paste0( "_min", nhistoria )

  dataset[ , paste0( cols, sufijo) := frollapply(x=.SD, FUN="min", n=nhistoria, align="right"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#calcula el maximo de los ultimos  nhistoria meses

Maximos  <- function( dataset, cols, nhistoria )
{

  sufijo  <- paste0( "_max", nhistoria )

  dataset[ , paste0( cols, sufijo) := frollapply(x=.SD, FUN="max", n=nhistoria, na.rm=TRUE, align="right"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------

#se calculan para los 6 meses previos el minimo, maximo y tendencia calculada con cuadrados minimos
#la formual de calculo de la tendencia puede verse en https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
#para la maxíma velocidad esta funcion esta escrita en lenguaje C, y no en la porqueria de R o Python

Rcpp::cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  // [[Rcpp::plugins(openmp)]]
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( n );


  //#if defined(_OPENMP)
  //#pragma omp parallel for
  //#endif
  for(int i = 0; i < n; i++)
  {
    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) ) 
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      { 
        xsum  += x[h] ;
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
    }
    else
    {
      out[ i ]  =  NA_REAL ; 
    }
  }

  return  out;
}')

#------------------------------------------------------------------------------
#calcula la tendencia de las variables cols de los ultimos 6 meses
#la tendencia es la pendiente de la recta que ajusta por cuadrados minimos

Tendencia  <- function( dataset, cols )
{
  #Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion  <- 6

  last  <- nrow( dataset )

  #creo el vector_desde que indica cada ventana
  #de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids   <- dataset$numero_de_cliente

  vector_desde  <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
  vector_desde[ 1:ventana_regresion ]  <-  1

  for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
  for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }

  for(  campo  in   cols )
  {
    nueva_col     <- fhistC( dataset[ , get(campo) ], vector_desde ) 

    dataset[ , paste0( campo, "_tend") := nueva_col[ (0*last +1):(1*last) ]  ]
  }

}
#------------------------------------------------------------------------------
VPOS_CORTE  <- c()

fganancia_lgbm_meseta  <- function(probs, datos) ### CONSULTAR
{
  vlabels  <- getinfo(datos, "label")
  vpesos   <- getinfo(datos, "weight")

  #solo sumo 48750 si vpesos > 1, hackeo 
  tbl  <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1 & vpesos > 1, 48750, -1250 ) ) )

  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  setorder( tbl, -gan_acum )   #voy por la meseta

  gan  <- mean( tbl[ 1:500,  gan_acum] )  #meseta de tamaño 500

  pos_meseta  <- tbl[ 1:500,  median(posicion)]
  VPOS_CORTE  <<- c( VPOS_CORTE, pos_meseta )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#Elimina del dataset las variables que estan por debajo de la capa geologica de canaritos

CanaritosImportancia  <- function( dataset )
{

  gc()
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

  for( i  in 1:(ncol(dataset)/5))  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]

  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01" ) )

  azar  <- runif( nrow(dataset) )
  entrenamiento  <-  dataset[ , foto_mes>= 202001 &  foto_mes<= 202010 &  foto_mes!=202006 & ( clase01==1 | azar < 0.10 ) ]

  dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ entrenamiento==TRUE, campos_buenos, with=FALSE]),## filtro sobre columnas sacando 26 columnas (campos malos, mas clase01, clase ternaria, fold, entrenamiento, fotomes)
                          label=   dataset[ entrenamiento==TRUE, clase01],
                          weight=  dataset[ entrenamiento==TRUE, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] )

  dvalid  <- lgb.Dataset( data=    data.matrix(  dataset[ foto_mes==202011, campos_buenos, with=FALSE]),
                          label=   dataset[ foto_mes==202011, clase01],
                          weight=  dataset[ foto_mes==202011, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] )


  param <- list( objective= "binary",
                 metric= "custom",
                 first_metric_only= TRUE,
                 boost_from_average= TRUE,
                 feature_pre_filter= FALSE,
                 verbosity= -100,
                 seed= 700001,
                 max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                 min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                 lambda_l1= 0.0,         #por ahora, lo dejo fijo
                 lambda_l2= 0.0,         #por ahora, lo dejo fijo
                 max_bin= 31,            #por ahora, lo dejo fijo
                 num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                 force_row_wise= TRUE,    #para que los alumnos no se atemoricen con tantos warning
                 learning_rate= 0.02, 
                 feature_fraction= 0.50,
                 min_data_in_leaf= 4000,
                 num_leaves= 600,
                 early_stopping_rounds= 200 )

  modelo  <- lgb.train( data= dtrain,
                        valids= list( valid= dvalid ),
                        eval= fganancia_lgbm_meseta,
                        param= param,
                        verbose= -100 )

  tb_importancia  <- lgb.importance( model= modelo )
  tb_importancia[  , pos := .I ] # agrega columna con numero de fila (ordena importancia)
  
  fwrite( tb_importancia, file="./work/impo.txt", sep="\t" )
  
  umbral  <- tb_importancia[ Feature %like% "canarito", median(pos) - sd(pos) ]
  col_inutiles  <- tb_importancia[ pos >= umbral | Feature %like% "canarito",  Feature ]

  for( col in col_inutiles )
  {
    dataset[  ,  paste0(col) := NULL ]
  }

  rm( dtrain, dvalid )
  gc()

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------

correr_todo  <- function( palancas )
{
  #cargo el dataset ORIGINAL
  dataset1  <- fread( "./datasetsOri/paquete_premium_202009.csv")
  dataset2  <- fread( "./datasetsOri/paquete_premium_202011.csv")

  dataset   <- rbind( dataset1, dataset2 )
  rm( dataset1, dataset2 )
  gc()

  setorder(  dataset, numero_de_cliente, foto_mes )  #ordeno el dataset

  AgregarMes( dataset )  #agrego el mes del año

  if( length(palancas$variablesdrift) > 0 )   DriftEliminar( dataset, palancas$variablesdrift )

  if( palancas$dummiesNA )  DummiesNA( dataset )  #esta linea debe ir ANTES de Corregir  !!

  if( palancas$corregir )  Corregir( dataset )  #esta linea debe ir DESPUES de  DummiesNA

  if( palancas$nuevasvars )  AgregarVariables( dataset )

  cols_analiticas  <- setdiff( colnames(dataset),  c("numero_de_cliente","foto_mes","mes","clase_ternaria") )

  if( palancas$lag1 )   Lags( dataset, cols_analiticas, 1, palancas$delta1 )
  if( palancas$lag2 )   Lags( dataset, cols_analiticas, 2, palancas$delta2 )
  if( palancas$lag3 )   Lags( dataset, cols_analiticas, 3, palancas$delta3 )
  if( palancas$lag4 )   Lags( dataset, cols_analiticas, 4, palancas$delta4 )
  if( palancas$lag5 )   Lags( dataset, cols_analiticas, 5, palancas$delta5 )
  if( palancas$lag6 )   Lags( dataset, cols_analiticas, 6, palancas$delta6 )

  if( palancas$promedio3 )  Promedios( dataset, cols_analiticas, 3 )
  if( palancas$promedio6 )  Promedios( dataset, cols_analiticas, 6 )

  if( palancas$minimo3 )  Minimos( dataset, cols_analiticas, 3 )
  if( palancas$minimo6 )  Minimos( dataset, cols_analiticas, 6 )

  if( palancas$maximo3 )  Maximos( dataset, cols_analiticas, 3 )
  if( palancas$maximo6 )  Maximos( dataset, cols_analiticas, 6 )

  if( palancas$tendencia6 )  Tendencia( dataset, cols_analiticas)


  if( palancas$canaritosimportancia )  CanaritosImportancia( dataset )



  #dejo la clase como ultimo campo
  nuevo_orden  <- c( setdiff( colnames( dataset ) , "clase_ternaria" ) , "clase_ternaria" )
  setcolorder( dataset, nuevo_orden )

  #Grabo el dataset
  fwrite( dataset,
          paste0( "./datasets/dataset_epic_simple_", palancas$version, ".csv.gz" ),
          logical01 = TRUE,
          sep= "," )

}
#------------------------------------------------------------------------------

#Aqui empieza el programa


correr_todo( palancas )


#quit( save="no" )


