rm( list=ls() )  
gc()             
require("data.table")
require("rlist")
require("yaml")
require("lightgbm")
require("DiceKriging")
require("mlrMBO")

switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "C:/Users/macar/Documents/Educacion/Maestria_DM/2021_2C/DMEF" },   #Windows
         Linux   = { directory.root  <-  "~/buckets/b1/" } #Google Cloud
)
setwd( directory.root)

semillas <- c(887113, 894689, 895553, 896723, 900001)
campos_malos  <- c( "numero_de_cliente", "foto_mes", "ccajas_transacciones", "Master_mpagominimo" )

carpeta_datasetsOri <-  "./datasetsOri/"
septiembre <- "paquete_premium_202009.csv"

ds <- fread(paste0(carpeta_datasetsOri, septiembre,collapse = ""), header=TRUE, showProgress = FALSE)

#ds[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", "evento", "noevento")]
ds[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", "1", "0")]
ds[, c("clase_ternaria") := NULL]


#---------xgb
library(xgboost)

ds2<-ds
clases <- as.numeric(ds2$clase_binaria) #- 1
ds2$clase_binaria <- NULL

dtrain   <- xgb.DMatrix( data = data.matrix(ds2),  label = clases, missing=NA )
set.seed(semillas[1])
t0 <- Sys.time()

modelo1 <- xgb.cv( 
        data = dtrain,  
        missing = NA,
        stratified = TRUE,       
        nround= 20,
        nfold = 5,
        watchlist = list(metric='auc'),
        early_stopping_rounds = 50,
        eval_metric= "auc",
        maximize =TRUE,
        subsample = 1, 
        colsample_bytree = 1, 
        eta = 0.3,
        min_child_weight = 1, 
        max_depth = 6,
        alpha = 0, 
        lambda = 0, 
        objective="binary:logistic",
        ####
        tree_method = "hist",
        grow_policy="lossguide",
        ####
        verbose = 2
)
t1 <- Sys.time()

print(paste0("El tiempo que tardó en ajustar XGB es:", as.numeric(  t1 - t0, units = "secs"), collapse = " "))

modelo1$best_iteration



#--------------------- test xgb
modelo_xgb_1 = xgb.train( 
        data = dtrain,
        nround= 20, # poner la mejor ronda
        objective="binary:logistic",
        verbose = 2
)
noviembre <- "paquete_premium_202011.csv"

ds_nov <- fread(paste0(carpeta_datasetsOri, noviembre,collapse = ""), header=TRUE, showProgress = FALSE)
ds_nov$clase_ternaria <- NULL


pred_nov <- predict(modelo_xgb_1, data.matrix(ds_nov),  type = "prob")

length(unique(pred_nov))
entrega  <- as.data.table( list( "numero_de_cliente"= ds_nov[, numero_de_cliente],
                                 "Predicted"= as.numeric(pred_nov > 0.025) ) ) 
#--------- optimizacion bayesiana sobre árboles

obj_fun <- function(x) { 
        experimento_rpart(ds, semillas, md= x$maxdepth, ms= x$minsplit)$mean_auc
}

obj.fun = makeSingleObjectiveFunction(
        name = "2 parametros",
        minimize = FALSE,
        fn = obj_fun,
        par.set = makeParamSet(
                makeIntegerParam("maxdepth",  lower = 1L, upper = 25L),
                makeIntegerParam("minsplit",  lower=2L , upper=  200L)
        ),
        has.simple.signature = FALSE
)
ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = 17L)
ctrl = setMBOControlInfill(
        ctrl,
        crit = makeMBOInfillCritEI(),
        opt = "focussearch"
)

#lrn = makeMBOLearner(ctrl, obj.fun)
design <- generateDesign(8L, getParamSet(obj.fun), fun = lhs::maximinLHS)

surr.km <-
        makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run  <-  mbo(obj.fun, design = design, learner = surr.km, control = ctrl)


saveRDS(run, "cache/03_HO_md_ms_OB.RDS")

#------------ lgbm

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