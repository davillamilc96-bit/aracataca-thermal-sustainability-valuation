# ================================================================
# ARACATACA | RANDOM FOREST - ANALISIS FINAL REPRODUCIBLE
# Requiere en la misma carpeta:
#   BASE_VIVIENDAS_RF.csv
#   GRUPOS_MODELOS_RF.csv
# EJECUCION: RStudio -> Source (no requiere ejecutar otros scripts)
# ================================================================

# AJUSTES: TRUE reproduce el procedimiento de seleccion utilizado para los
# resultados finales: 130 combinaciones x 5 folds y reevaluacion de las
# cinco mejores en 5-fold x 5 repeticiones. FALSE evalua toda la rejilla
# en 5-fold x 5 repeticiones y puede seleccionar parametros diferentes.
RAPIDO <- TRUE
SEMILLA <- 1234L
N_REPETICIONES <- 5L
CARPETA <- getwd()  # En RStudio: Session > Set Working Directory > To Source File Location

# Paquetes minimos. Se instalan si faltan.
paquetes <- c('ranger','rsample','openxlsx','ggplot2')
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly=TRUE)]
if (length(faltan)) install.packages(faltan, repos='https://cloud.r-project.org')
suppressPackageStartupMessages({
  library(ranger); library(rsample); library(openxlsx); library(ggplot2)
})

archivo_base <- file.path(CARPETA, 'BASE_VIVIENDAS_RF.csv')
archivo_grupos <- file.path(CARPETA, 'GRUPOS_MODELOS_RF.csv')
if (!file.exists(archivo_base) || !file.exists(archivo_grupos)) {
  if (interactive()) {
    message('Selecciona BASE_VIVIENDAS_RF.csv en la ventana que aparece.')
    elegido <- file.choose()
    CARPETA <- dirname(elegido)
    archivo_base <- file.path(CARPETA, 'BASE_VIVIENDAS_RF.csv')
    archivo_grupos <- file.path(CARPETA, 'GRUPOS_MODELOS_RF.csv')
  }
  if (!file.exists(archivo_base) || !file.exists(archivo_grupos))
    stop('Ambos CSV deben estar en la misma carpeta. Carpeta actual: ',CARPETA)
}

salida <- file.path(CARPETA, 'RESULTADOS_RANDOM_FOREST_FINAL')
dir.create(salida, showWarnings=FALSE, recursive=TRUE)
base <- read.csv(archivo_base, stringsAsFactors=FALSE, check.names=FALSE,
                 na.strings=c('', 'NA'), fileEncoding='UTF-8')
grupos_csv <- read.csv(archivo_grupos, stringsAsFactors=FALSE, fileEncoding='UTF-8')
stopifnot(all(c('Grupo','Variable') %in% names(grupos_csv)))
grupos <- split(grupos_csv$Variable, grupos_csv$Grupo)
stopifnot(all(c('construccion','terreno','servicios') %in% names(grupos)))

# Recuperar tipos del script Modelos_Lineales.R: nominales y ordinales.
nominales <- c('cubierta','color_cubi','sombras','muros','fachada',
               'ventilaci','emplazamie','material_v','orientacio','orientac_1')
for (v in intersect(nominales, names(base))) base[[v]] <- as.factor(base[[v]])
for (v in intersect(c('estrato_soc','tipificaci'), names(base))) {
  base[[v]] <- ordered(base[[v]], levels=sort(unique(base[[v]][!is.na(base[[v]])])))
}
if ('numero_pre' %in% names(base)) base$numero_pre <- as.character(base$numero_pre)

especificaciones <- list(
  M1=list(dep='log_valor_metr', bruto='valor_metr',
          x=unique(c(grupos$terreno,grupos$servicios))),
  M2=list(dep='log_v_m2_cons', bruto='v_m2_cons',
          x=unique(c(grupos$construccion,grupos$servicios))),
  M3=list(dep='log_int_terr', bruto='int_terr',
          x=unique(c(grupos$terreno,grupos$construccion,grupos$servicios)))
)

# Preparacion de predictores: log para X numericas estrictamente positivas.
# La variable dependiente ya esta en logaritmos naturales: NO aplicar log de nuevo.
preparar <- function(spec) {
  requeridas <- unique(c(spec$dep, spec$bruto, spec$x,
                          'factor_exp_nc_norm','factor_exp_nc'))
  if (!all(c(spec$dep,spec$bruto,spec$x) %in% names(base))) {
    stop('Faltan variables: ',paste(setdiff(c(spec$dep,spec$bruto,spec$x),names(base)),collapse=', '))
  }
  presentes <- intersect(requeridas, names(base))
  d <- base[, presentes, drop=FALSE]
  ids <- which(stats::complete.cases(d))
  d <- d[ids,,drop=FALSE]
  # Agrupacion de niveles raros, equivalente a fct_lump_min(min=3)
  for (v in intersect(c('cubierta','color_cubi','muros','fachada','ventilaci'),names(d))) {
    if (is.factor(d[[v]])) {
      f <- as.character(d[[v]]); tab <- table(f)
      f[f %in% names(tab)[tab < 3]] <- 'Other'
      d[[v]] <- factor(f)
    }
  }
  peso <- if ('factor_exp_nc_norm' %in% names(d)) 'factor_exp_nc_norm' else
          if ('factor_exp_nc' %in% names(d)) 'factor_exp_nc' else NA_character_
  w <- if (is.na(peso)) rep(1,nrow(d)) else as.numeric(d[[peso]])
  w[!is.finite(w) | w<=0] <- NA_real_
  w <- w/mean(w,na.rm=TRUE)
  d$w <- w
  feats <- spec$x
  for (v in spec$x) {
    if (is.numeric(d[[v]]) && all(d[[v]] > 0,na.rm=TRUE)) {
      nuevo <- paste0('log_',v)
      d[[nuevo]] <- log(d[[v]])
      feats[feats==v] <- nuevo
    } else if (is.character(d[[v]])) {
      d[[v]] <- factor(d[[v]])
    }
  }
  d$y <- as.numeric(d[[spec$dep]])
  # Comprobar que las variables log corresponden a las originales
  delta <- max(abs(d$y-log(as.numeric(d[[spec$bruto]]))),na.rm=TRUE)
  if (!is.finite(delta) || delta>1e-5) {
    stop('No se confirma equivalencia log(Y) para ',spec$dep,
         '. Diferencia maxima=',delta)
  }
  ok <- complete.cases(d[,c('w','y',feats),drop=FALSE]) &
        is.finite(d$y) & d$w>0
  d <- d[ok,,drop=FALSE]
  d$numero_fila_base <- ids[ok]
  # Retener unicamente columnas necesarias para ranger
  list(data=d, feats=feats, peso=peso, delta=delta)
}

# Correlacion al cuadrado, como yardstick::rsq_vec.
# R2_predictivo = 1-SSE/SST se informa tambien, por transparencia.
metricas <- function(y,p) {
  ok <- is.finite(y)&is.finite(p); y <- y[ok];p <- p[ok]
  if(length(y)<3) return(data.frame(R2_cor=NA_real_,R2_predictivo=NA_real_,RMSE=NA_real_,MAE=NA_real_))
  r <- suppressWarnings(cor(y,p))
  data.frame(R2_cor=if(is.finite(r)) r^2 else NA_real_,
             R2_predictivo=if(sum((y-mean(y))^2)>0) 1-sum((y-p)^2)/sum((y-mean(y))^2) else NA_real_,
             RMSE=sqrt(mean((y-p)^2)),MAE=mean(abs(y-p)))
}

ajustar <- function(entrenamiento, evaluacion, feats, nt, md, importancia='none') {
  f <- stats::reformulate(feats,response='y')
  mod <- ranger::ranger(formula=f,
    data=entrenamiento[,c('y',feats),drop=FALSE],
    case.weights=entrenamiento$w,
    num.trees=as.integer(nt), max.depth=as.integer(md),
    mtry=max(1L,floor(sqrt(length(feats)))), min.node.size=5L,
    importance=importancia, seed=SEMILLA, oob.error=TRUE)
  pred <- if(is.null(evaluacion)) NULL else predict(mod,data=evaluacion[,feats,drop=FALSE])$predictions
  list(mod=mod,pred=pred)
}

# Semilla y folds reproducibles.
set.seed(SEMILLA)
resumenes <- list(); grids <- list(); importancias <- list(); cv_detalles <- list()
predicciones <- list(); diagnosticos <- list()

for (nm in names(especificaciones)) {
  cat('\n================ ',nm,' ================\n')
  spec <- especificaciones[[nm]]
  prep <- preparar(spec); d <- prep$data; feats <- prep$feats
  cat('N=',nrow(d),' | X=',length(feats),' | delta log=',prep$delta,'\n')
  stopifnot(nrow(d)>50, length(feats)>0)
  diagnosticos[[nm]] <- data.frame(Modelo=nm,N=nrow(d),K=length(feats),
      Peso=prep$peso,Max_diferencia_log=prep$delta,
      Variables=paste(feats,collapse='; '))
  folds25 <- rsample::vfold_cv(d,v=5,repeats=N_REPETICIONES)
  # Cada particion es 80/20 (aprox.); la busqueda RAPIDA usa solo 5 folds
  folds_tuning <- if(RAPIDO) folds25$splits[1:5] else folds25$splits

  cv_combo <- function(y_col,nt,md,folds) {
    scores <- lapply(seq_along(folds),function(i) {
      tr <- rsample::analysis(folds[[i]]);te <- rsample::assessment(folds[[i]])
      tr$y <- tr[[y_col]];te$y <- te[[y_col]]
      pred <- ajustar(tr,te,feats,nt,md)$pred
      cbind(Fold=i,metricas(te$y,pred))
    })
    do.call(rbind,scores)
  }
  # 1. Seleccion de hiperparametros
  cat('Buscando hiperparametros (',if(RAPIDO) 'modo rapido' else 'modo exhaustivo',')...\n')
  grid <- expand.grid(Trees=seq(100L,1000L,100L),Depth=3L:15L)
  grid$R2_tuning <- NA_real_
  for(i in seq_len(nrow(grid))) {
    scores <- cv_combo('y',grid$Trees[i],grid$Depth[i],folds_tuning)
    grid$R2_tuning[i] <- mean(scores$R2_cor,na.rm=TRUE)
    if(i %% 20==0) cat('  ',i,'/',nrow(grid),' combinaciones\n')
  }
  grid <- grid[order(-grid$R2_tuning),,drop=FALSE]
  # En modo rapido: reevaluar los 5 mejores con los 25 folds
  if(RAPIDO) {
    top <- head(grid,5)
    top$R2_25fold <- vapply(seq_len(nrow(top)),function(i) {
      s <- cv_combo('y',top$Trees[i],top$Depth[i],folds25$splits)
      mean(s$R2_cor,na.rm=TRUE)
    },numeric(1))
    best <- top[which.max(top$R2_25fold),,drop=FALSE]
    grid$R2_25fold <- NA_real_
    for(i in seq_len(nrow(top))) {
      j <- which(grid$Trees==top$Trees[i]&grid$Depth==top$Depth[i])
      grid$R2_25fold[j] <- top$R2_25fold[i]
    }
  } else best <- grid[1,,drop=FALSE]
  nt <- as.integer(best$Trees);md <- as.integer(best$Depth)
  cat('Optimo final: trees=',nt,' depth=',md,'\n')

  # 2. Modelo final y evaluacion CV (estimacion interna, no anidada).
  fit_nuevo <- ajustar(d,NULL,feats,nt,md,'permutation')$mod
  nuevo_oob <- metricas(d$y,fit_nuevo$predictions)
  nuevo_cv <- cv_combo('y',nt,md,folds25$splits)
  cv_detalles[[nm]] <- data.frame(Modelo=nm,nuevo_cv)
  resumenes[[nm]] <- data.frame(
    Modelo=nm,N=nrow(d),Features=length(feats),
    Trees=nt,MaxDepth=md,
    OOB_R2_cor=nuevo_oob$R2_cor,
    OOB_R2_predictivo=nuevo_oob$R2_predictivo,
    OOB_RMSE=nuevo_oob$RMSE,OOB_MAE=nuevo_oob$MAE,
    CV_R2_cor=mean(nuevo_cv$R2_cor,na.rm=TRUE),
    CV_R2_predictivo=mean(nuevo_cv$R2_predictivo,na.rm=TRUE),
    CV_RMSE=mean(nuevo_cv$RMSE,na.rm=TRUE),
    CV_MAE=mean(nuevo_cv$MAE,na.rm=TRUE))
  importancias[[nm]] <- data.frame(
    Modelo=nm,Variable=names(fit_nuevo$variable.importance),
    Importancia=as.numeric(fit_nuevo$variable.importance))
  predicciones[[nm]] <- data.frame(
    Modelo=nm,Fila_base=d$numero_fila_base,
    Y_log_observada=d$y,OOB_predicha_log=fit_nuevo$predictions,
    Residuo_OOB_log=d$y-fit_nuevo$predictions)
  grids[[nm]] <- cbind(Modelo=nm,grid)
  cat('Final | OOB R2_cor=',round(nuevo_oob$R2_cor,4),
      ' | CV R2_cor=',round(mean(nuevo_cv$R2_cor,na.rm=TRUE),4),'\n')
}

resumen <- do.call(rbind,resumenes)
cv_total <- do.call(rbind,cv_detalles)
grid_total <- do.call(rbind,grids)
imp_total <- do.call(rbind,importancias)
pred_total <- do.call(rbind,predicciones)
diag_total <- do.call(rbind,diagnosticos)

# 3. Exportar tablas finales (sin resultados historicos).
notas <- data.frame(Nota=c(
  'Variable dependiente: logaritmo natural del valor original; no se aplica doble logaritmo.',
  'R2_cor = correlacion observados-predichos al cuadrado.',
  'R2_predictivo = 1-SSE/SST, puede ser negativo.',
  'CV: 5 folds x 5 repeticiones; el promedio de R2_predictivo es promedio de R2 por fold.',
  'CV se utiliza tanto para seleccionar hiperparametros como para evaluacion interna; no es validacion externa ni CV anidada.',
  'RAPIDO=TRUE: rejilla 130 combinaciones x 5 folds; top 5 x 25 folds.',
  'RAPIDO=FALSE: rejilla 130 combinaciones x 25 folds.',
  'RMSE y MAE se expresan en unidades de logaritmo natural de la variable dependiente.',
  'Importancia por permutacion: no indica signo ni causalidad.',
  'M3 incluye patio y fachada segun la especificacion RF.',
  'case.weights afecta el muestreo de ranger; metricas de evaluacion no ponderadas.'
))

ruta_excel <- file.path(salida,'RESULTADOS_RANDOM_FOREST_FINAL.xlsx')
openxlsx::write.xlsx(list(
  Resumen=resumen,CV_25_Folds=cv_total,Tuning=grid_total,
  Importancias=imp_total,Predicciones_OOB=pred_total,
  Diagnosticos=diag_total,Notas=notas
),ruta_excel,overwrite=TRUE)
write.csv(resumen,file.path(salida,'RESUMEN_MODELOS.csv'),row.names=FALSE)
write.csv(cv_total,file.path(salida,'VALIDACION_CRUZADA.csv'),row.names=FALSE)
write.csv(grid_total,file.path(salida,'BUSQUEDA_HIPERPARAMETROS.csv'),row.names=FALSE)
write.csv(imp_total,file.path(salida,'IMPORTANCIAS.csv'),row.names=FALSE)
write.csv(pred_total,file.path(salida,'PREDICCIONES_OOB.csv'),row.names=FALSE)
write.csv(diag_total,file.path(salida,'DIAGNOSTICOS.csv'),row.names=FALSE)

# 4. Graficas de importancia de variables
for(nm in names(especificaciones)) {
  z <- subset(imp_total,Modelo==nm)
  z <- head(z[order(-z$Importancia),],15)
  z$Variable <- factor(z$Variable,levels=rev(z$Variable))
  p <- ggplot(z,aes(x=Variable,y=Importancia))+geom_col()+coord_flip()+
    labs(title=paste(nm,'| Random Forest: importancia por permutacion'),
         x=NULL,y='Importancia OOB')+theme_minimal()
  ggsave(file.path(salida,paste0(nm,'_IMPORTANCIA_RF.png')),
         plot=p,width=8,height=6,dpi=180)
}
cat('\n===========================================\n')
cat('PROCESO COMPLETADO\n')
print(resumen[,c('Modelo','N','Trees','MaxDepth','OOB_R2_cor','CV_R2_cor')])
cat('Archivo principal: ',ruta_excel,'\n')
cat('Carpeta resultados: ',salida,'\n')
cat('Resultados finales listos para reproducibilidad.\n')

# 5. Graficas de observado vs predicho OOB, por modelo.
for (nm in names(especificaciones)) {
  z <- subset(pred_total, Modelo==nm)
  p <- ggplot(z,aes(x=Y_log_observada,y=OOB_predicha_log)) +
    geom_point(alpha=0.65,size=1.7) +
    geom_abline(intercept=0,slope=1,linetype='dashed') +
    labs(title=paste(nm,'| Observado vs predicho OOB'),
         x='Valor observado (log natural)',
         y='Prediccion OOB (log natural)') +
    theme_minimal()
  ggsave(file.path(salida,paste0(nm,'_OBSERVADO_VS_PREDICHO_OOB.png')),
         plot=p,width=7,height=6,dpi=180)
}
