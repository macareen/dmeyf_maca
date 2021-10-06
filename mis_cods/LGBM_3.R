rm( list=ls() )  #remove all objects
gc()             #garbage collection


require("lightgbm")
require("data.table")
require("rlist")
require("yaml")



#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "C:/Users/macar/Documents/Educacion/Maestria_DM/2021_2C/DMEF" },   #Windows
         Linux   = { directory.root  <-  "~/buckets/b1/" } #Google Cloud
)
#defino la carpeta donde trabajo
setwd( directory.root )



kexperimento  <- 211005   #NA si se corre la primera vez, un valor concreto si es para continuar procesando
kscript           <- "lgbm"
karch_generacion  <- "./datasets/paquete_premium_202009_stk.csv"

karch_generacion_simple  <- "./datasets/paquete_premium_202009_fe.csv"
karch_aplicacion  <- "./datasets/paquete_premium_202011_fe.csv"

kBO_iter    <-  5  #cantidad de iteraciones de la Optimizacion Bayesiana

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
        makeNumericParam("learning_rate",    lower= 0.01 , upper=    0.1),
        makeNumericParam("feature_fraction", lower= 0.2  , upper=    1.0),
        makeIntegerParam("min_data_in_leaf", lower= 5    , upper= 8000),
        makeIntegerParam("num_leaves",       lower=16L   , upper= 1024L)
)

campos_malos  <- c("clase_ternaria", "clase_binaria", "ccajas_transacciones", "Master_mpagominimo", "internet","ccajeros_propios_descuentos" ,         "mcajeros_propios_descuentos" , "ctarjeta_visa_descuentos",             "mtarjeta_visa_descuentos","ctarjeta_master_descuentos",           "mtarjeta_master_descuentos" )  #aqui se deben cargar todos los campos culpables del Data Drifting

semillas <- c(887113, 894689, 895553, 896723, 900001)
ksemilla_azar  <- semillas[3]
#sample(semillas, 1)
#------------------------------------------------------------------------------
#Funcion que lleva el registro de los experimentos

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
#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
        archivo  <- arch
        if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )
        
        if( !file.exists( archivo ) )  #Escribo los titulos
        {
                linea  <- paste0( "fecha\t", 
                                  paste( list.names(reg), collapse="\t" ), "\n" )
                
                cat( linea, file=archivo )
        }
        
        linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                          gsub( ", ", "\t", toString( reg ) ),  "\n" )
        
        cat( linea, file=archivo, append=TRUE )  #grabo al archivo
        
        if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------

VPROBS_CORTE  <- c()

fganancia_logistic_lightgbm   <- function(probs, datos) 
{
        vlabels  <- getinfo(datos, "label")
        vpesos   <- getinfo(datos, "weight")
        
        #solo sumo 48750 si vpesos > 1, hackeo 
        tbl  <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1 & vpesos > 1, 48750, -1250 ) ) )
        
        setorder( tbl, -prob )
        tbl[ , gan_acum :=  cumsum( gan ) ]
        gan  <- max( tbl$gan_acum )
        
        VPROBS_CORTE  <<- c(VPROBS_CORTE,  tbl[ which.max( tbl$gan_acum ) , prob ] )
        
        return( list( "name"= "ganancia", 
                      "value"=  gan,
                      "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
        GLOBAL_iteracion  <<- GLOBAL_iteracion + 1
        
        gc()
        
        kfolds  <- 5   # cantidad de folds para cross validation
        
        param_basicos  <- list( objective= "binary",
                                metric= "custom",
                                first_metric_only= TRUE,
                                boost_from_average= TRUE,
                                feature_pre_filter= FALSE,
                                verbosity= -100,
                                seed= semillas[3],
                                max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                                min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                                lambda_l1= 0.0,         #por ahora, lo dejo fijo
                                lambda_l2= 0.0,         #por ahora, lo dejo fijo
                                max_bin= 31,            #por ahora, lo dejo fijo
                                num_iterations= 9999,    #un numero muy grande, lo limita early_stopping_rounds
                                force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
        )
        
        #el parametro discolo, que depende de otro
        param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )
        
        param_completo  <- c( param_basicos, param_variable, x )
        
        VPROBS_CORTE  <<- c()
        set.seed( semillas[3] )
        modelocv  <- lgb.cv( data= dtrain,
                             eval= fganancia_logistic_lightgbm,
                             stratified= TRUE, #sobre el cross validation
                             nfold= kfolds,    #folds del cross validation
                             param= param_completo,
                             verbose= -100
        )
        
        #--------------
        #ds_train_mm  <- lgb.Dataset( data= data.matrix(dataset), label= clase_binaria )
       # mm <- lgb.train(dtrain, params = param_completo, verbose = -1)
        #--------------
        #-----------------------------
        
        
        
        params_gbdt <- list( objective= "binary", max_bin= 15, min_data_in_leaf= 4000, learning_rate= 0.05 )
        params_rf <- list(objective = "binary",  boosting_type = "rf", bagging_freq = 1, bagging_fraction = 0.66, feature_fraction = 0.4)
        params_goss <- list(objective = "binary", learning_rate = 0.05, top_rate = 0.5, other_rate = 0.1, feature_fraction_bynode = 0.2, boosting_type = "goss")
        
        # Los folds son para siempre! 
        folds <- splitTools::create_folds(clase_binaria, k = 5, seed = 17)
        
        print("llegó hasta antes de m1-m3")
        ds_train  <- lgb.Dataset( data=  data.matrix(ds), label= clase_binaria )
        m1 <- lgb.train(ds_train, params = params_gbdt, verbose = -1)
        m2 <- lgb.train(ds_train, params = params_rf, verbose = -1)
        m3 <- lgb.train(ds_train, params = params_goss, verbose = -1)
        
        print("llegó hasta antes de m1_scores")
        m1_scores <- predict(m1,data.matrix(dapply))
        m2_scores <- predict(m2,data.matrix(dapply))
        m3_scores <- predict(m3,data.matrix(dapply))
        
        print("llegó a cambiar dapply")
        dapply <- cbind(dapply,m1_scores, m2_scores,m3_scores)
        
        

        #---------------------------
        
        ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]
        
        ganancia_normalizada  <-  ganancia* kfolds  
        attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra
        
        param_completo$num_iterations  <- modelocv$best_iter  #asigno el mejor num_iterations
        param_completo["early_stopping_rounds"]  <- NULL
        param_completo["prob_corte"]  <- mean( VPROBS_CORTE )
        
        
        #si tengo una ganancia superadora, genero el archivo para Kaggle
        if(  ganancia > GLOBAL_ganancia_max )
        {
                GLOBAL_ganancia_max  <<- ganancia  #asigno la nueva maxima ganancia a una variable GLOBAL, por eso el <<-
                
                set.seed(ksemilla_azar)
                
                modelo  <- lightgbm( data= dtrain,
                                     param= param_completo,
                                     verbose= -100
                )
                
                
                #calculo la importancia de variables
                tb_importancia  <- lgb.importance( model= modelo )
                fwrite( tb_importancia, 
                        file= paste0(kimp, "imp_", GLOBAL_iteracion, ".txt"),
                        sep="\t" )
                
                 prediccion  <- predict( modelo, data.matrix( dapply[  , campos_buenos, with=FALSE]) )
                
                Predicted  <- as.integer( prediccion > param_completo$prob_corte )
                
                entrega  <- as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente, 
                                                 "Predicted"= Predicted)  )
                
                #genero el archivo para Kaggle
                fwrite( entrega, 
                        file= paste0(kkaggle, GLOBAL_iteracion, ".csv" ),
                        sep= "," )
        }
        
        #logueo 
        xx  <- param_completo
        xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
        loguear( xx,  arch= klog )
        
        return( ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
kbayesiana  <- paste0("./work/E",  kexperimento, "_", kscript, ".RDATA" )
klog        <- paste0("./work/E",  kexperimento, "_", kscript, ".txt" )
kimp        <- paste0("./work/E",  kexperimento, "_", kscript, "_" )
kkaggle     <- paste0("./kaggle/E",kexperimento, "_", kscript, "_" )


GLOBAL_ganancia_max  <-  -Inf
GLOBAL_iteracion  <- 0

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) ){
        tabla_log  <- fread( klog)
        GLOBAL_iteracion  <- nrow( tabla_log ) -1
        GLOBAL_ganancia_max  <- tabla_log[ , max(ganancia) ]
}


#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread(karch_generacion)
ds  <- fread(karch_generacion_simple)

clase_binaria<-dataset$clase_binaria


#creo la clase_binaria2   1={ BAJA+2}  0={CONTINUA,BAJA+1}
dataset[ , clase_binaria:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]
ds[ , clase_binaria:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]



#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase_binaria","clase_binaria", campos_malos) )

#dejo los datos en el formato que necesita LightGBM
#uso el weight como un truco ESPANTOSO para saber la clase real
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase_binaria,
                        weight=  dataset[ , ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] )


#cargo los datos donde voy a aplicar el modelo
dapply  <- fread(karch_aplicacion, stringsAsFactors= TRUE) #leo los datos donde voy a aplicar el modelo
dapply$clase_ternaria<-NA
dapply$clase_binaria<-NA

#Aqui comienza la configuracion de la Bayesian Optimization


funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
        fn=       funcion_optimizar, #la funcion que voy a maximizar
        minimize= FALSE,   #estoy Maximizando la ganancia
        noisy=    TRUE,
        par.set=  hs,     #definido al comienzo del programa
        has.simple.signature = FALSE   #paso los parametros en una lista
)

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if(!file.exists(kbayesiana)) {
        run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
        run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}



#apagado de la maquina virtual, pero NO se borra
system( "sleep 10  &&  sudo shutdown -h now", wait=FALSE)

#suicidio,  elimina la maquina virtual directamente
#system( "sleep 10  && 
#        export NAME=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/name -H 'Metadata-Flavor: Google') &&
#        export ZONE=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google') &&
#        gcloud --quiet compute instances delete $NAME --zone=$ZONE",
#        wait=FALSE )


quit( save="no" )


