# ============================================================
# 00_unificar_ajuste_base.R
#   - Prepara base "viviendas_completas" desde cero
#   - Calcula variables de valor (cons/terr), integra y log-transforma D
#   - Une distancias y genera logs
#   - Limpia nombres (snake_case, sin tildes) una sola vez
#   - Exporta: SHP + XLSX + CSV
# ============================================================

# ---- Paquetes
pkgs <- c("sf","dplyr","readr","stringr","janitor","stringi","openxlsx")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly=TRUE)]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- Rutas (usa las que enviaste)
ruta_valor_shp <- "C:/Users/ASUS/Documents/Daniel/IGAC investigacion y Prospectiva/Viviendas resilentes/Levantamiento de informacion/Spatial_Join/Valor_zhg.shp"
ruta_out_predios_val <- "C:/Users/ASUS/Documents/Daniel/IGAC investigacion y Prospectiva/Viviendas resilentes/Levantamiento de informacion/Spatial_Join/predios_valorizados.shp"

ruta_viv_shp <- "C:/Users/ASUS/Documents/Daniel/IGAC investigacion y Prospectiva/Viviendas resilentes/Levantamiento de informacion/Spatial_Join/Viviendas_variables.shp"

ruta_dist_cp_shp <- "C:/Users/ASUS/Documents/Daniel/IGAC investigacion y Prospectiva/Viviendas resilentes/Levantamiento de informacion/Dist_CS_Colegios_Plaza/PrediosDistColegiosPlaza.shp"
ruta_dist_salud_shp <- "C:/Users/ASUS/Documents/Daniel/IGAC investigacion y Prospectiva/Viviendas resilentes/Levantamiento de informacion/Dist_CS_Colegios_Plaza/PrediosDistCSalud.shp"

# Carpeta de salida final
out_dir <- "C:/Users/ASUS/Documents/Daniel/IGAC investigacion y Prospectiva/Viviendas resilentes/Levantamiento de informacion/Spatial_Join"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ============================================================
# 1) CARGUE BASE DE DATOS PARA CÁLCULO DE VALORES DE CONSTRUCCIÓN
# ============================================================
predios_sf <- sf::st_read(ruta_valor_shp, quiet = TRUE)

# Conversión y cálculos
predios_sf <- predios_sf %>%
  mutate(
    VALOR_METR = as.numeric(VALOR_METR),
    AREA_TERRE = as.numeric(AREA_TERRE),
    AV_COMERCI = as.numeric(AV_COMERCI),
    VALOR_CONS = as.numeric(VALOR_CONS),
    V_M2_CONS  = as.numeric(V_M2_CONS),
    AREA_CONST = as.numeric(AREA_CONST),
    AVALUO     = as.numeric(AVALUO),
    
    # Cálculos base
    VALOR_TERR = VALOR_METR * AREA_TERRE,
    AV_COMERCI = AVALUO/0.6,
    VALOR_CONS = AV_COMERCI - VALOR_TERR
  ) %>%
  # Reglas con AREA_CONST == 0
  mutate(
    VALOR_TERR = if_else(AREA_CONST == 0, AV_COMERCI, VALOR_TERR),
    VALOR_CONS = if_else(AREA_CONST == 0, 0, VALOR_CONS),
    V_M2_CONS  = if_else(AREA_CONST == 0, 0, VALOR_CONS / AREA_CONST)
  ) %>%
  # Corrección si VALOR_CONS < 0
  mutate(
    VALOR_TERR = if_else(VALOR_CONS < 0, 0, VALOR_TERR),
    V_M2_CONS  = if_else(VALOR_CONS < 0, 0, V_M2_CONS),
    VALOR_CONS = if_else(VALOR_CONS < 0, 0, VALOR_CONS)
  ) %>%
  # Índices integrales
  mutate(
    INTEG_CONS = if_else(AREA_CONST == 0, 0, AV_COMERCI / AREA_CONST),
    INT_TERR   = if_else(AREA_TERRE == 0, 0, AV_COMERCI / AREA_TERRE)
  )

# Guardar shapefile intermedio (solo campos útiles)
predios_valores <- predios_sf %>%
  dplyr::select(
    TARGET_FID, CODIGO, NUMERO_PRE, DESTINO_EC,
    AREA_TERRE, AREA_CONST, AVALUO, VALOR_METR, AV_COMERCI,
    VALOR_TERR, VALOR_CONS, V_M2_CONS, INTEG_CONS, INT_TERR, geometry
  )

sf::st_write(predios_valores, ruta_out_predios_val, delete_layer = TRUE, quiet = TRUE)

# ============================================================
# 2) CARGAR BASE CON VARIABLES PARA MODELO Y UNIR
# ============================================================
viviendas_sf <- sf::st_read(ruta_viv_shp, quiet = TRUE)

predios_valores <- predios_valores %>%
  mutate(NUMERO_PRE = as.character(NUMERO_PRE))
viviendas_sf <- viviendas_sf %>%
  mutate(NUMERO_PRE = as.character(NUMERO_PRE))

viviendas_completas <- viviendas_sf %>%
  inner_join(predios_valores %>% st_drop_geometry(), by = "NUMERO_PRE") %>%
  filter(NUMERO_PRE != "470530101000000350013000000000") %>%
  distinct(NUMERO_PRE, .keep_all = TRUE)

# ============================================================
# 3) LIMPIEZA DE NOMBRES (minúsculas, snake_case, sin tildes)
#    *Se hace UNA sola vez aquí*
# ============================================================
viviendas_completas <- viviendas_completas %>%
  janitor::clean_names()

# Quitar tildes en nombres
names(viviendas_completas) <- stringi::stri_trans_general(names(viviendas_completas), "Latin-ASCII")

# ============================================================
# 4) Asegurar clave numero_pre (por si viene *_x/_y)
# ============================================================
posibles_claves <- intersect(
  c("numero_pre","numero_pre.x","numero_pre.y","NUMERO_PRE","numero__pre"),
  names(viviendas_completas)
)
if (length(posibles_claves) == 0) stop("No encuentro la clave numero_pre en viviendas_completas.")

viviendas_completas <- viviendas_completas %>%
  mutate(
    numero_pre = dplyr::coalesce(!!!rlang::syms(posibles_claves)),
    numero_pre = as.character(numero_pre)
  ) %>%
  select(-any_of(setdiff(posibles_claves, "numero_pre")))

# ============================================================
# 5) LEER SHP DE DISTANCIAS, LIMPIAR Y UNIR
#    (agregar d_sp_cs_mn, d_sp_plaza, d_sp_cl_mn)
# ============================================================
dist_salud_df <- sf::st_read(ruta_dist_salud_shp, quiet = TRUE) %>%
  janitor::clean_names() %>%
  mutate(numero_pre = as.character(numero_pre)) %>%
  dplyr::select(numero_pre, d_sp_cs_mn) %>%
  sf::st_drop_geometry() %>%
  distinct(numero_pre, .keep_all = TRUE)

dist_cp_df <- sf::st_read(ruta_dist_cp_shp, quiet = TRUE) %>%
  janitor::clean_names() %>%
  mutate(numero_pre = as.character(numero_pre)) %>%
  dplyr::select(numero_pre, d_sp_plaza, d_sp_cl_mn) %>%
  sf::st_drop_geometry() %>%
  distinct(numero_pre, .keep_all = TRUE)

stopifnot(all(c("numero_pre","d_sp_cs_mn") %in% names(dist_salud_df)))
stopifnot(all(c("numero_pre","d_sp_plaza","d_sp_cl_mn") %in% names(dist_cp_df)))

viviendas_completas <- viviendas_completas %>%
  left_join(dist_salud_df, by = "numero_pre") %>%
  left_join(dist_cp_df,    by = "numero_pre") %>%
  # renombres prácticos (si te sirven)
  rename(
    dist_salud_min_m   = d_sp_cs_mn,
    dist_plaza_m       = d_sp_plaza,
    dist_colegio_min_m = d_sp_cl_mn
  ) %>%
  # imputación simple por mediana y logs auxiliares (log1p para distancias)
  mutate(
    dist_salud_min_m   = ifelse(is.na(dist_salud_min_m),   median(dist_salud_min_m,   na.rm = TRUE), dist_salud_min_m),
    dist_plaza_m       = ifelse(is.na(dist_plaza_m),       median(dist_plaza_m,       na.rm = TRUE), dist_plaza_m),
    dist_colegio_min_m = ifelse(is.na(dist_colegio_min_m), median(dist_colegio_min_m, na.rm = TRUE), dist_colegio_min_m),
    log_dist_salud_min   = log1p(dist_salud_min_m),
    log_dist_plaza       = log1p(dist_plaza_m),
    log_dist_colegio_min = log1p(dist_colegio_min_m)
  )

# ============================================================
# 5.2 UNIR VARIABLE: INDICE_VIE (ÍNDICE DE VIENTOS)
# ============================================================

suppressPackageStartupMessages({ library(sf); library(dplyr); library(janitor); library(stringi) })

ruta_indice_vie <- "C:/Users/ASUS/Documents/Daniel/IGAC investigacion y Prospectiva/Viviendas resilentes/Levantamiento de informacion/Shp_IndiceViento/Shp_IndiceViento/Shp_IndiceViento.shp"

# 1) Leer y limpiar nombres
indice_vie_sf <- sf::st_read(ruta_indice_vie, quiet = TRUE) %>%
  janitor::clean_names()
# quitar acentos en nombres por consistencia con tu pipeline
names(indice_vie_sf) <- stringi::stri_trans_general(names(indice_vie_sf), "Latin-ASCII")

# 2) Asegurar clave numero_pre (acepta NUMERO_PRE / numero__pre, etc.)
candidatas <- intersect(c("numero_pre","NUMERO_PRE","numero__pre"), names(indice_vie_sf))
if (length(candidatas) == 0) stop("[indice_vie] No encuentro la clave numero_pre en el shapefile.")
if (!"numero_pre" %in% names(indice_vie_sf)) {
  indice_vie_sf <- indice_vie_sf %>% mutate(numero_pre = as.character(.data[[candidatas[1]]]))
} else {
  indice_vie_sf <- indice_vie_sf %>% mutate(numero_pre = as.character(numero_pre))
}

# 3) Quitar geometría ANTES de tocar datos y limpiar NaN SOLO en numéricas
indice_vie_df <- indice_vie_sf %>%
  sf::st_drop_geometry() %>%
  distinct(numero_pre, .keep_all = TRUE)

# reemplazar NaN en columnas numéricas
num_cols <- names(indice_vie_df)[sapply(indice_vie_df, is.numeric)]
if (length(num_cols)) {
  for (cc in num_cols) {
    v <- indice_vie_df[[cc]]
    v[is.nan(v)] <- NA_real_
    indice_vie_df[[cc]] <- v
  }
}

# 4) Detectar columna del índice (elige la primera que exista)
col_indice <- intersect(c("indice_vie","ind_vie","indiceviento","valor_vie"), names(indice_vie_df))
if (length(col_indice) == 0) {
  stop("[indice_vie] No encontré una columna del índice (busqué: indice_vie, ind_vie, indiceviento, valor_vie).")
}

# 5) Unir a viviendas_completas (asegurar clave en minúsculas)
if (!"numero_pre" %in% names(viviendas_completas)) {
  if ("NUMERO_PRE" %in% names(viviendas_completas)) {
    viviendas_completas <- viviendas_completas %>% mutate(numero_pre = as.character(NUMERO_PRE))
  } else {
    stop("[indice_vie] 'viviendas_completas' no tiene numero_pre ni NUMERO_PRE.")
  }
} else {
  viviendas_completas <- viviendas_completas %>% mutate(numero_pre = as.character(numero_pre))
}

viviendas_completas <- viviendas_completas %>%
  left_join(
    indice_vie_df %>%
      select(numero_pre, !!rlang::sym(col_indice[1])) %>%
      rename(indice_vie = !!rlang::sym(col_indice[1])),
    by = "numero_pre"
  )

# ============================================================
# 6) CREAR LOGS DE LAS D (solo si > 0)
#     D = v_m2_cons, valor_metr, integ_cons, int_terr
# ============================================================
safe_log <- function(x) ifelse(is.na(x) | x <= 0, NA_real_, log(x))

viviendas_completas <- viviendas_completas %>%
  mutate(
    log_v_m2_cons = safe_log(v_m2_cons),
    log_valor_metr = safe_log(valor_metr),
    log_integ_cons = safe_log(integ_cons),
    log_int_terr   = safe_log(int_terr),
    log_indice_vie = safe_log(indice_vie)
  )


# ============================================================
# 7) SESGO DE SELECCIÓN usando UNIVERSO Viviendas_Aracataca
#     (reproduce tu pipeline original y añade reportes)
# ============================================================

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(janitor); library(stringi); library(broom); library(tidyr)
})

# --- 7.1 Universo: Viviendas_Aracataca (≈2919)
ruta_universo <- "C:/Users/ASUS/Documents/Daniel/IGAC investigacion y Prospectiva/Viviendas resilentes/Levantamiento de informacion/Viviendas_Aracataca_V1/Viviendas_Aracataca_V1/ViviendasAracatacaV1.shp"

universo_sf <- sf::st_read(ruta_universo, quiet = FALSE)
# Limpieza básica de nombres (no cambia tus nombres de Z; solo para joins)
universo_sf <- universo_sf %>% mutate(NUMERO_PRE = as.character(NUMERO_PRE))
cat("\n[Universo] Dimensiones universo_sf: ", paste(dim(universo_sf), collapse=" x "), "\n", sep="")
cat("[Universo] CRS: ", sf::st_crs(universo_sf)$input, "\n", sep="")

# --- 7.2 Levantados: 198 predios con variables detalladas
lev_sf <- sf::st_read(
  "C:/Users/ASUS/Documents/Daniel/IGAC investigacion y Prospectiva/Viviendas resilentes/Levantamiento de informacion/Spatial_Join/Viviendas_variables.shp",
  quiet = TRUE
) %>% janitor::clean_names()
names(lev_sf) <- stringi::stri_trans_general(names(lev_sf), "Latin-ASCII")
stopifnot("numero_pre" %in% names(lev_sf))
lev_ids <- unique(as.character(lev_sf$numero_pre))

# --- 7.3 Marcar D en el UNIVERSO (no en la tabla de levantados)
universo_sf <- universo_sf %>%
  mutate(D = if_else(NUMERO_PRE %in% lev_ids, 1L, 0L))

cat("\n[Selección] Conteo D en universo_sf:\n")
print(table(universo_sf$D, useNA = "ifany"))
cat("Total D==1 (esperado ~198): ", sum(universo_sf$D==1, na.rm=TRUE), "\n", sep="")

# --- 7.4 Matriz Z (exactamente tus columnas del pipeline original)
#     Usamos mayúsculas como en tu código para que el summary luzca igual.
cols_needed <- c("NUMERO_PRE","D","AREA_TERRE","HABITACION","BANOS_1","PUNTAJE_1","AREA_CONST")
missing <- setdiff(cols_needed, names(universo_sf))
if (length(missing)) stop("Faltan columnas en el universo: ", paste(missing, collapse=", "))

z_df <- universo_sf %>%
  st_drop_geometry() %>%
  dplyr::select(all_of(cols_needed)) %>%
  mutate(
    D          = as.integer(D),
    AREA_TERRE = suppressWarnings(as.numeric(AREA_TERRE)),
    HABITACION = suppressWarnings(as.numeric(HABITACION)),
    BANOS_1    = suppressWarnings(as.numeric(BANOS_1)),
    PUNTAJE_1  = suppressWarnings(as.numeric(PUNTAJE_1)),
    AREA_CONST = suppressWarnings(as.numeric(AREA_CONST))
  ) %>%
  filter(!is.na(D)) %>%
  tidyr::drop_na(AREA_TERRE, HABITACION, BANOS_1, PUNTAJE_1, AREA_CONST)

cat("\n[z_df] Observaciones tras limpieza: ", nrow(z_df), "\n", sep="")
cat("[z_df] Primeras filas:\n"); print(utils::head(z_df, 3))

# --- 7.5 M-Logit (universo general, como en tu código)
m_logit <- glm(
  D ~ AREA_TERRE + HABITACION + BANOS_1 + PUNTAJE_1,
  data = z_df, family = binomial(link = "logit")
)

cat("\n[LOGIT-universo] Coeficientes:\n")
print(summary(m_logit)$coefficients)
summary(m_logit)

mcfadden_R2 <- 1 - (m_logit$deviance / m_logit$null.deviance)
lr_test     <- anova(m_logit, test = "Chisq")

cat("\n[LOGIT-universo] McFadden R²: ", round(mcfadden_R2, 6), "\n", sep="")
cat("[LOGIT-universo] LR test (ANOVA Chisq):\n"); print(lr_test)

# OR table (universo)
est <- coef(m_logit); se <- sqrt(diag(vcov(m_logit)))
OR_table <- data.frame(
  term    = names(est),
  OR      = exp(est),
  CI95_lo = exp(est - 1.96*se),
  CI95_hi = exp(est + 1.96*se),
  p_value = summary(m_logit)$coefficients[,4],
  row.names = NULL
)
cat("\n[LOGIT-universo] Odds Ratios (IC95%):\n")
print(OR_table %>% dplyr::mutate(
  OR      = round(OR, 4),
  CI95_lo = round(CI95_lo, 4),
  CI95_hi = round(CI95_hi, 4),
  p_value = signif(p_value, 4)
))

# Probabilidades y soporte común (universo)
z_df$p_hat <- predict(m_logit, type = "response")
cat("\n[LOGIT-universo] Rangos p^ por D:\n"); print(tapply(z_df$p_hat, z_df$D, range, na.rm = TRUE))
cat("\n[LOGIT-universo] Resumen p^ en D==1:\n"); print(summary(subset(z_df, D==1)$p_hat))
cat("\n[LOGIT-universo] Resumen p^ en D==0:\n"); print(summary(subset(z_df, D==0)$p_hat))

# --- 7.6 Dominio CONSTRUIDOS (>0) como en tu código
universo_built <- universo_sf %>%
  mutate(AREA_CONST = suppressWarnings(as.numeric(AREA_CONST))) %>%
  filter(!is.na(AREA_CONST) & AREA_CONST > 0)

z_df_built <- universo_built %>%
  st_drop_geometry() %>%
  dplyr::select(all_of(cols_needed)) %>%
  mutate(
    D          = as.integer(D),
    AREA_TERRE = suppressWarnings(as.numeric(AREA_TERRE)),
    HABITACION = suppressWarnings(as.numeric(HABITACION)),
    BANOS_1    = suppressWarnings(as.numeric(BANOS_1)),
    PUNTAJE_1  = suppressWarnings(as.numeric(PUNTAJE_1)),
    AREA_CONST = suppressWarnings(as.numeric(AREA_CONST))
  ) %>%
  tidyr::drop_na(D, AREA_TERRE, HABITACION, BANOS_1, PUNTAJE_1, AREA_CONST)

m_logit_built <- glm(
  D ~ AREA_TERRE + HABITACION + BANOS_1 + PUNTAJE_1 + AREA_CONST,
  data = z_df_built, family = binomial(link = "logit")
)

cat("\n[LOGIT-built] Coeficientes:\n"); print(summary(m_logit_built)$coefficients)
mcfadden_R2_built <- 1 - (m_logit_built$deviance / m_logit_built$null.deviance)
lr_test_built     <- anova(m_logit_built, test = "Chisq")
cat("\n[LOGIT-built] McFadden R²: ", round(mcfadden_R2_built, 6), "\n", sep="")
cat("[LOGIT-built] LR test (ANOVA Chisq):\n"); print(lr_test_built)

est_b <- coef(m_logit_built); se_b <- sqrt(diag(vcov(m_logit_built)))
OR_table_built <- data.frame(
  term    = names(est_b),
  OR      = exp(est_b),
  CI95_lo = exp(est_b - 1.96*se_b),
  CI95_hi = exp(est_b + 1.96*se_b),
  p_value = summary(m_logit_built)$coefficients[,4],
  row.names = NULL
)
cat("\n[LOGIT-built] Odds Ratios (IC95%):\n")
print(OR_table_built %>% dplyr::mutate(
  OR      = round(OR, 4),
  CI95_lo = round(CI95_lo, 4),
  CI95_hi = round(CI95_hi, 4),
  p_value = signif(p_value, 4)
))

z_df_built$p_hat <- predict(m_logit_built, type = "response")
cat("\n[LOGIT-built] Rangos p^ por D:\n"); print(tapply(z_df_built$p_hat, z_df_built$D, range, na.rm = TRUE))
cat("\n[LOGIT-built] Resumen p^ en D==1:\n"); print(summary(subset(z_df_built, D==1)$p_hat))
cat("\n[LOGIT-built] Resumen p^ en D==0:\n"); print(summary(subset(z_df_built, D==0)$p_hat))

# --- 7.7 FACTOR DE EXPANSIÓN (IPW) usando m_logit_built (como tú)
#      Clipping, winsorización p99 en D==1 y normalización media=1
p_min <- 0.01; p_max <- 0.99
z_df_built$p_hat_clip <- pmin(p_max, pmax(p_min, z_df_built$p_hat))
z_df_built$w_ipw      <- 1 / z_df_built$p_hat_clip

p99 <- stats::quantile(z_df_built$w_ipw[z_df_built$D==1], 0.99, na.rm = TRUE)
z_df_built$w_ipw[z_df_built$D==1 & z_df_built$w_ipw > p99] <- as.numeric(p99)

mean_w <- mean(z_df_built$w_ipw[z_df_built$D==1], na.rm = TRUE)
z_df_built$w_ipw_norm <- ifelse(z_df_built$D==1, z_df_built$w_ipw/mean_w, NA_real_)

cat("\n[IPW] Resumen pesos (D==1):\n")
print(z_df_built %>% dplyr::filter(D==1) %>%
        dplyr::summarise(
          n       = dplyr::n(),
          uniq    = length(unique(w_ipw)),
          min     = min(w_ipw, na.rm=TRUE),
          p50     = stats::median(w_ipw, na.rm=TRUE),
          p90     = stats::quantile(w_ipw, 0.90, na.rm=TRUE),
          p99_cap = stats::quantile(w_ipw, 0.99, na.rm=TRUE),
          max     = max(w_ipw, na.rm=TRUE),
          mean    = mean(w_ipw, na.rm=TRUE)
        ))

# --- 7.8 Anexar factores a:
#       a) universo_sf (para auditoría)
universo_sf <- universo_sf %>%
  left_join(z_df_built[, c("NUMERO_PRE","p_hat","p_hat_clip","w_ipw","w_ipw_norm")], by="NUMERO_PRE") %>%
  mutate(
    factor_exp_nc      = ifelse(D==1, w_ipw, NA_real_),
    factor_exp_nc_norm = ifelse(D==1, w_ipw_norm, NA_real_)
  )

# ---- Copiar p_hat / IPW a viviendas_completas (opcional y robusto) ----
if (exists("viviendas_completas")) {
  # 1) Asegurar clave numero_pre en viviendas_completas
  if (!"numero_pre" %in% names(viviendas_completas)) {
    if ("NUMERO_PRE" %in% names(viviendas_completas)) {
      viviendas_completas <- viviendas_completas %>%
        mutate(numero_pre = as.character(NUMERO_PRE))
    } else {
      stop("[Join factores] 'viviendas_completas' no tiene 'numero_pre' ni 'NUMERO_PRE'.")
    }
  } else {
    viviendas_completas <- viviendas_completas %>%
      mutate(numero_pre = as.character(numero_pre))
  }
  
  # 2) Preparar df de factores sin duplicados (clave limpia)
  fact_df <- z_df_built %>%
    dplyr::select(numero_pre = NUMERO_PRE, p_hat, p_hat_clip, w_ipw, w_ipw_norm) %>%
    dplyr::mutate(numero_pre = as.character(numero_pre)) %>%
    dplyr::distinct(numero_pre, .keep_all = TRUE)
  
  # 3) Hacer el join (no pisa geometry)
  viviendas_completas <- viviendas_completas %>%
    left_join(fact_df, by = "numero_pre")
  
  # 4) Crear columnas de factores si no existen y rellenar con coalesce
  if (!"factor_exp_nc" %in% names(viviendas_completas)) {
    viviendas_completas$factor_exp_nc <- NA_real_
  }
  if (!"factor_exp_nc_norm" %in% names(viviendas_completas)) {
    viviendas_completas$factor_exp_nc_norm <- NA_real_
  }
  
  viviendas_completas <- viviendas_completas %>%
    mutate(
      factor_exp_nc      = dplyr::coalesce(factor_exp_nc, w_ipw),
      factor_exp_nc_norm = dplyr::coalesce(factor_exp_nc_norm, w_ipw_norm)
    )
  
  cat("\n[Join] Factores anexados a 'viviendas_completas' por numero_pre.\n")
  cat("[Join] Conteo no-NA factor_exp_nc:", sum(!is.na(viviendas_completas$factor_exp_nc)), "\n")
}

# ============================================================
# LIMPIEZA FINAL: conservar solo variables necesarias
# ============================================================

suppressPackageStartupMessages({ library(dplyr); library(sf) })

# --- 1) Asegurar clave numero_pre en minúsculas
if (!"numero_pre" %in% names(viviendas_completas)) {
  if ("NUMERO_PRE" %in% names(viviendas_completas)) {
    viviendas_completas <- viviendas_completas %>% mutate(numero_pre = as.character(NUMERO_PRE))
  } else {
    stop("[clean] 'viviendas_completas' no tiene numero_pre ni NUMERO_PRE.")
  }
} else {
  viviendas_completas <- viviendas_completas %>% mutate(numero_pre = as.character(numero_pre))
}

# --- 2) Log seguro para indice_vie
safe_log <- function(x) ifelse(is.na(x) | x <= 0, NA_real_, log(x))
if ("indice_vie" %in% names(viviendas_completas)) {
  viviendas_completas <- viviendas_completas %>%
    mutate(log_indice_vie = safe_log(indice_vie))
}

# --- 3) Normalizar nombres alternativos si faltan los *_x
`%notin%` <- function(x, y) !(x %in% y)

if ("area_terre_x" %notin% names(viviendas_completas) && "area_terre" %in% names(viviendas_completas)) {
  viviendas_completas <- viviendas_completas %>% rename(area_terre_x = area_terre)
}
if ("area_const_x" %notin% names(viviendas_completas) && "area_const" %in% names(viviendas_completas)) {
  viviendas_completas <- viviendas_completas %>% rename(area_const_x = area_const)
}

# helper: operador %notin%
`%notin%` <- function(x, y) !(x %in% y)

# --- 4) Vector de columnas requeridas
vars_requeridas <- c(
  # clave
  "numero_pre",
  # controles y entorno
  "area_terre_x","area_const_x","ndvi","cubierta","color_cubi","sombras","muros","fachada","ventilaci",
  "altura","altura_m","emplazamie","antigueda","patio","arboles","material_v","ancho_via","energia",
  "estrato_soc","acueducto","alcantaril","gas_natural","recoleccio","acesso_inte","habitaci_3",
  "orientacio","orientac_1","tipificaci",
  # distancias (y logs)
  "log_dist_salud_min","log_dist_plaza","log_dist_colegio_min",
  "dist_salud_min_m","dist_plaza_m","dist_colegio_min_m",
  # D's y logs
  "avaluo","av_comerci","valor_terr","valor_cons","v_m2_cons","valor_metr","integ_cons","int_terr",
  "log_v_m2_cons","log_valor_metr","log_integ_cons","log_int_terr",
  # índice viento
  "indice_vie","log_indice_vie",
  # selección / pesos
  "p_hat","p_hat_clip","w_ipw","w_ipw_norm","factor_exp_nc","factor_exp_nc_norm",
  # geometría
  "geometry"
)

# --- 5) Selección robusta (conserva geometry)
n0 <- ncol(viviendas_completas)
presentes <- intersect(vars_requeridas, names(viviendas_completas))
faltantes <- setdiff(vars_requeridas, presentes)

viviendas_completas <- viviendas_completas %>%
  dplyr::select(dplyr::any_of(vars_requeridas))

# ============================================================
# EXPORTAR: Excel + Shapefile (+ opcional GeoPackage)
# ============================================================

suppressPackageStartupMessages({ library(sf); library(dplyr); library(openxlsx) })

# 1) Carpeta de salida (usa la que ya venías usando o define una)
if (!exists("out_dir")) {
  out_dir <- "C:/Users/ASUS/Documents/Daniel/IGAC investigacion y Prospectiva/Viviendas resilentes/Levantamiento de informacion/CODIGOS FINALES"
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 2) Marcas de tiempo para versiones
stamp <- format(Sys.time(), "%Y%m%d_%H%M")

# 3) Rutas de salida
excel_path <- file.path(out_dir, paste0("viviendas_completas_", stamp, ".xlsx"))
shp_path   <- file.path(out_dir, paste0("viviendas_completas_", stamp, ".shp"))
gpkg_path  <- file.path(out_dir, paste0("viviendas_completas_", stamp, ".gpkg"))  # opcional

# 4) Exportar a Excel (sin geometría)
wb <- createWorkbook()
addWorksheet(wb, "viviendas_completas")
writeData(wb, "viviendas_completas", sf::st_drop_geometry(viviendas_completas))
saveWorkbook(wb, excel_path, overwrite = TRUE)

cat("\n[export] Excel escrito en: ", excel_path, "\n", sep = "")

# 5) Exportar a Shapefile
#    Nota: Shapefile trunca nombres de campo a 10 caracteres y no soporta tipos largos;
#    si necesitas nombres completos, usa también el .gpkg de abajo.
sf::st_write(viviendas_completas, shp_path, delete_layer = TRUE, quiet = TRUE)
cat("[export] Shapefile escrito en: ", shp_path, "\n", sep = "")

# 6) (Opcional RECOMENDADO) Exportar a GeoPackage para conservar nombres completos
sf::st_write(viviendas_completas, gpkg_path, layer = "viviendas_completas", delete_layer = TRUE, quiet = TRUE)
cat("[export] GeoPackage escrito en: ", gpkg_path, " (nombres de campos intactos)\n", sep = "")


# ============================================================
# TABLAS PARA PAPER: Logit de selección + Probabilidades (Excel)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(broom); library(openxlsx)
})

# --- Usar SOLO el modelo y data del universo general ---
if (!exists("m_logit")) stop("No existe el objeto 'm_logit'. Estímalo antes de generar tablas.")
if (!exists("z_df"))    stop("No existe el objeto 'z_df'. Construye la matriz Z antes de generar tablas.")

modelo_logit <- m_logit
base_z       <- z_df
modelo_tag   <- "Universo general"

# --- 1) Función para estrellas de significancia
sig_stars <- function(p){
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01,  "**",
                ifelse(p < 0.05,  "*",
                       ifelse(p < 0.10,  "·", ""))))
}

# --- 2) Tabla 1: Coeficientes, Error Est., OR, p-valor (con estrellas)
tab1 <- broom::tidy(modelo_logit) %>%
  mutate(
    odds_ratio = exp(estimate),
    estrella   = sig_stars(p.value),
    coef_est   = paste0(round(estimate, 3), " ", estrella),
    std.error  = round(std.error, 3),
    odds_ratio = round(odds_ratio, 3),
    `p-valor`  = signif(p.value, 3)
  ) %>%
  select(Variable = term,
         Coeficiente = coef_est,
         `Error estándar` = std.error,
         `Odds Ratio` = odds_ratio,
         `p-valor`)

# --- 3) Estadísticos del modelo
mcfadden_R2 <- 1 - (modelo_logit$deviance / modelo_logit$null.deviance)
lr          <- anova(modelo_logit, test = "Chisq")

# extraer LR chi2 y p del último renglón
lr_row <- tail(lr, 1)
lr_chi2 <- as.numeric(lr_row$Deviance)
lr_df   <- as.numeric(lr_row$Df)
lr_p    <- suppressWarnings(as.numeric(lr_row$`Pr(>Chi)`))

N_obs   <- nrow(base_z)

tab1_stats <- data.frame(
  Estadistico = c("McFadden R²", "LR χ²", "LR gl", "LR p-valor", "N observaciones", "Modelo"),
  Valor       = c(round(mcfadden_R2, 4),
                  round(lr_chi2, 3),
                  lr_df,
                  signif(lr_p, 4),
                  N_obs,
                  modelo_tag),
  check.names = FALSE
)

# --- 4) Tabla 2: Probabilidades predichas y soporte común
base_z$p_hat <- predict(modelo_logit, type = "response")

tab2 <- base_z %>%
  mutate(D = as.integer(D)) %>%
  group_by(D) %>%
  summarise(
    Min   = round(min(p_hat, na.rm = TRUE), 4),
    Max   = round(max(p_hat, na.rm = TRUE), 4),
    Media = round(mean(p_hat, na.rm = TRUE), 4),
    SD    = round(sd(p_hat, na.rm = TRUE), 4),
    n     = n(),
    .groups = "drop"
  ) %>%
  mutate(Grupo = ifelse(D == 1, "D = 1 (levantados)", "D = 0 (no levantados)")) %>%
  select(Grupo, n, Min, Max, Media, SD)

# --- 5) Exportar a Excel (dos hojas)
if (!exists("out_dir")) {
  out_dir <- "C:/Users/ASUS/Documents/Daniel/IGAC investigacion y Prospectiva/Viviendas resilentes/Levantamiento de informacion/Spatial_Join"
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
stamp <- format(Sys.time(), "%Y%m%d_%H%M")
xlsx_out <- file.path(out_dir, paste0("Tablas_Logit_Seleccion_", stamp, ".xlsx"))

wb <- createWorkbook()

# Hoja 1: Tabla 1 (coeficientes)
addWorksheet(wb, "Tabla 1 - Logit")
writeData(wb, "Tabla 1 - Logit", "Modelo Logit de selección (1 = predio levantado)", startRow = 1, startCol = 1)
writeData(wb, "Tabla 1 - Logit", paste0("Ámbito: ", modelo_tag), startRow = 2, startCol = 1)
writeData(wb, "Tabla 1 - Logit", tab1, startRow = 4, startCol = 1)

# Agregar estadísticos debajo
start_stats_row <- 4 + nrow(tab1) + 2
writeData(wb, "Tabla 1 - Logit", "Estadísticos del modelo", startRow = start_stats_row, startCol = 1)
writeData(wb, "Tabla 1 - Logit", tab1_stats, startRow = start_stats_row + 1, startCol = 1)

# Nota de estrellas (opcional)
nota_row <- start_stats_row + 1 + nrow(tab1_stats) + 2
writeData(wb, "Tabla 1 - Logit",
          "Notas: *** p<0.001, ** p<0.01, * p<0.05, · p<0.10.",
          startRow = nota_row, startCol = 1)

# Hoja 2: Tabla 2 (p_hat)
addWorksheet(wb, "Tabla 2 - p_hat")
writeData(wb, "Tabla 2 - p_hat", "Probabilidades predichas y soporte común", startRow = 1, startCol = 1)
writeData(wb, "Tabla 2 - p_hat", tab2, startRow = 3, startCol = 1)

saveWorkbook(wb, xlsx_out, overwrite = TRUE)

cat("\n[paper] Export listo:\n - ", xlsx_out, "\n", sep = "")



# Quitar la geometría para exportar solo la tabla
predios_tabla <- predios_sf %>%
  sf::st_drop_geometry()

# Ruta de salida en Excel (usa la misma carpeta out_dir)
ruta_excel_predios <- file.path(
  out_dir,
  "predios_valores_completos.xlsx"
)

# Exportar a Excel
openxlsx::write.xlsx(
  x = predios_tabla,
  file = ruta_excel_predios,
  sheetName = "predios_sf",
  overwrite = TRUE
)

