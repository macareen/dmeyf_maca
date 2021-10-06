#Feature Engineering
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "C:/Users/macar/Documents/Educacion/Maestria_DM/2021_2C/DMEF" },   #Windows
         Linux   = { directory.root  <-  "~/buckets/b1/crudoB" } #Google Cloud
)
setwd(directory.root)

#Establezco el Working Directory
#setwd( "~/buckets/b1/crudoB" )


EnriquecerDataset <- function( dataset , arch_destino )
{
        columnas_originales <-  copy(colnames( dataset ))
        
        
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
        dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
        
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
        dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]
        
        #------- variables mias
        
        dataset[ , cut_edad         := cut(cliente_edad,4) ]
        dataset[ , cut_antiguedad         := cut(cliente_antiguedad,3) ]
        dataset[ , cut_rent         := cut(mrentabilidad,2) ]
        dataset[ , cut_rent_an         := cut(mrentabilidad_annual,2) ]
        dataset[ , cut_cc         := cut(mcuenta_corriente,3) ]
        dataset[ , cut_ca         := cut(mcaja_ahorro,3) ]
        dataset[ , cut_consumo_t         := cut(mv_mconsumototal,3) ]
        
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
        
        dataset[ , cut_consumos         := cut(consumos,4) ]
        dataset[ , cut_deuda         := cut(deuda,3) ]
        dataset[ , cut_inversiones         := cut(inversiones,3) ]
        dataset[ , cut_seguros         := cut(seguros,2) ]
        dataset[ , cut_debitos         := cut(debitos,3) ]
        dataset[ , cut_comisiones         := cut(comisiones,3) ]
        dataset[ , cut_movimientos         := cut(movimientos,5) ]
        dataset[ , cut_atm         := cut(atm,5) ]
        #-------
        
        
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
        
        #FIN de la seccion donde se deben hacer cambios con variables nuevas
        
        columnas_extendidas <-  copy( setdiff(  colnames(dataset), columnas_originales ) )
        
        #grabo con nombre extendido
        fwrite( dataset,
                file=arch_destino,
                sep= "," )
}
#------------------------------------------------------------------------------

dir.create( "./datasets/" )


#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset1  <- fread("./datasetsOri/paquete_premium_202009.csv")
dataset2  <- fread("./datasetsOri/paquete_premium_202011.csv")

#----------------------- 06bp
# ds<-dataset1
# clase_binaria <- ifelse(ds$clase_ternaria == "CONTINUA", 0, 1)
# ds$clase_ternaria <- NULL
# 
# library(xgboost)
# 
# dtrain <- xgb.DMatrix(data=data.matrix(ds), label=  clase_binaria, missing=NA)
# param_fe <- list(max_depth=2, eta=1, silent=1, objective='binary:logistic')
# nrounds = 5
# 
# bst = xgb.train(params = param_fe, data = dtrain, nrounds = nrounds)
# 
# new.features.train <- xgb.create.features(model = bst, data.matrix(ds))
# 
# dataset1<-as.data.table(data.matrix(new.features.train))
# dataset1<-cbind(dataset1,clase_binaria)
# 
# 
# dataset2<-cbind(dataset2,clase_binaria=NA)
#------------------------





EnriquecerDataset( dataset1, "./datasets/paquete_premium_202009_fe.csv" )
EnriquecerDataset( dataset2, "./datasets/paquete_premium_202011_fe.csv" )

quit( save="no")
