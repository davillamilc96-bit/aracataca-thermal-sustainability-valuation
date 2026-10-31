# ============================================================
# 01_modelos_lineales.R
#   - Carga base final de viviendas
#   - Deja todo listo para correr modelos lineales y RF
# ============================================================

# ---- Paquetes
pkgs <- c("dplyr","readxl","stringr","janitor")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly=TRUE)]
if(length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- Ruta del Excel exportado en el pipeline 1
ruta_excel <- "C:/Users/ASUS/Documents/Daniel/IGAC investigacion y Prospectiva/Viviendas resilentes/Levantamiento de informacion/Spatial_Join/viviendas_completas_20251031_1911.xlsx"

# ---- Cargar la hoja
viviendas_df <- readxl::read_excel(ruta_excel) %>%
  janitor::clean_names() %>%
  dplyr::mutate(numero_pre = as.character(numero_pre))

# ---- Verificación inicial
cat("\n[info] Base cargada:", nrow(viviendas_df), "filas y", ncol(viviendas_df), "columnas.\n")
str(viviendas_df[,1:10])


#=====================================================
#2.0 Índice con pesos “entropía” (datos definen pesos)
#======================================================= 

library(dplyr)

# 1) Winsorizar y normalizar a [0,1] (menor=mejor, invertimos)
minmax_01 <- function(x){
  rng <- range(x, na.rm=TRUE); (x - rng[1]) / (rng[2] - rng[1])
}

# --- Función auxiliar para winsorizar outliers (trunca en el percentil 99)
cap_at <- function(x, p = 0.99) {
  q <- quantile(x, p, na.rm = TRUE)
  pmin(x, q)
}

tmp <- viviendas_df %>%
  mutate(
    d_sal = cap_at(dist_salud_min_m),
    d_plz = cap_at(dist_plaza_m),
    d_col = cap_at(dist_colegio_min_m),
    x_sal = 1 - minmax_01(d_sal),
    x_plz = 1 - minmax_01(d_plz),
    x_col = 1 - minmax_01(d_col)
  ) %>%
  select(x_sal, x_plz, x_col)

# 2) Pesos por entropía (Shannon) en columnas x_sal/x_plz/x_col
X <- as.matrix(tmp)
# evitar ceros exactos
X[X < 1e-12] <- 1e-12
P <- sweep(X, 1, rowSums(X), "/")  # prob fila
k <- 1/log(ncol(X))
E <- -k * colSums(P * log(P))      # entropía por columna
d <- 1 - E                         # grado de diversificación
w <- d / sum(d)                    # pesos normalizados

# 3) Índice ponderado y escala 0–100
viviendas_df$walk_index <- 100 * as.numeric(X %*% w)

# Reporte de pesos
w

# ============================================================
# 3. TIPADO DE VARIABLES (numéricas vs. categóricas)
#  - estrato_soc: factor ordenado (1 < 2 < ... < 6)
#  - tipificaci : factor ordenado (orden inferido)
# ============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr) })

# --- Helpers
as_numeric_safe <- function(x){
  # Convierte preservando NA; útil si vienen strings tipo "123", "123,0", etc.
  x <- gsub(",", ".", as.character(x))
  suppressWarnings(as.numeric(x))
}

only_01 <- function(x){
  ux <- unique(na.omit(x))
  all(ux %in% c(0,1,"0","1"))
}

# --- Listas base según tu definición
vars_num <- c(
  "area_terre_x","area_const_x","ndvi","altura","altura_m","antigueda","arboles",
  "habitaci_3",   "log_dist_salud_min","log_dist_plaza","log_dist_colegio_min",
  "dist_salud_min_m","dist_plaza_m","dist_colegio_min_m",   "avaluo","av_comerci","valor_terr","valor_cons","v_m2_cons","valor_metr","integ_cons","int_terr",
  "log_v_m2_cons","log_valor_metr","log_integ_cons","log_int_terr","indice_vie","log_indice_vie",
  "p_hat","p_hat_clip","w_ipw","w_ipw_norm","factor_exp_nc","factor_exp_nc_norm",
  "walk_index","ancho_via"
)

vars_cat_nominal <- c(
  "cubierta","color_cubi","sombras","muros","fachada","ventilaci",  "emplazamie","material_v",
  "orientacio","orientac_1"
)

# Servicios: num si 0/1; de lo contrario, factor
vars_servicios <- c("energia","acueducto","alcantaril","gas_natural","recoleccio","acesso_inte")

# --- Asegurar existencia antes de convertir
vars_num <- intersect(vars_num, names(viviendas_df))
vars_cat_nominal <- intersect(vars_cat_nominal, names(viviendas_df))
vars_servicios <- intersect(vars_servicios, names(viviendas_df))

# --- 1) Numéricas puras
if (length(vars_num)){
  viviendas_df <- viviendas_df %>%
    mutate(across(all_of(vars_num), as_numeric_safe))
}

# --- 2) Servicios: si son 0/1 => num; si no, factor
if (length(vars_servicios)){
  for(v in vars_servicios){
    if (only_01(viviendas_df[[v]])) {
      viviendas_df[[v]] <- as_numeric_safe(viviendas_df[[v]])
    } else {
      viviendas_df[[v]] <- as.factor(viviendas_df[[v]])
    }
  }
}

# --- 3) Categóricas nominales (sin orden)
if (length(vars_cat_nominal)){
  viviendas_df <- viviendas_df %>%
    mutate(across(all_of(vars_cat_nominal), ~ as.factor(.x)))
}

# --- 4) estrato_soc como factor ORDENADO (1<2<...<6)
if ("estrato_soc" %in% names(viviendas_df)){
  # Detectar si viene como número o texto
  estr_raw <- viviendas_df$estrato_soc
  estr_num <- suppressWarnings(as.numeric(as.character(estr_raw)))
  # Si es NA (texto), intentar extraer dígitos
  if (all(is.na(estr_num))){
    estr_num <- suppressWarnings(as.numeric(stringr::str_extract(as.character(estr_raw), "\\d+")))
  }
  # Niveles ordenados por valor numérico observado
  levs <- sort(unique(na.omit(estr_num)))
  # Fallback si no encontramos números (ordena alfabético, pero mantiene ordered=TRUE)
  if (length(levs) == 0){
    levs <- sort(unique(as.character(estr_raw)))
    viviendas_df$estrato_soc <- factor(as.character(estr_raw), levels = levs, ordered = TRUE)
  } else {
    viviendas_df$estrato_soc <- factor(estr_num, levels = levs, ordered = TRUE)
  }
}

# --- 5) tipificaci como factor ORDENADO
# Regla:
#  - si es numérico (códigos), ordenar ascendente
#  - si es texto, ordenar alfabético (puedes cambiar el vector levels si tienes un orden normativo)
if ("tipificaci" %in% names(viviendas_df)){
  tipi_raw <- viviendas_df$tipificaci
  tipi_num <- suppressWarnings(as.numeric(as.character(tipi_raw)))
  if (all(is.na(tipi_num))){  # texto
    levs_tipi <- sort(unique(as.character(tipi_raw)))
    viviendas_df$tipificaci <- factor(as.character(tipi_raw), levels = levs_tipi, ordered = TRUE)
  } else {                    # numérico
    levs_tipi <- sort(unique(na.omit(tipi_num)))
    viviendas_df$tipificaci <- factor(tipi_num, levels = levs_tipi, ordered = TRUE)
  }
}

# --- 6) Identificador y geometry (se preservan tal cual si existen)
if ("numero_pre" %in% names(viviendas_df)) {
  viviendas_df$numero_pre <- as.character(viviendas_df$numero_pre)
}

# --- 7) Reporte breve de tipos
tipos <- sapply(viviendas_df[, setdiff(names(viviendas_df), "geometry"), drop = FALSE], function(x){
  cls <- class(x)[1]
  if (inherits(x, "ordered")) cls <- "ordered"
  cls
})
cat("\n[typing] Resumen de clases (sin geometry):\n")
print(sort(table(tipos)))

# Listado útil para revisión rápida
cat("\n[typing] Variables ordered (estrato_soc / tipificaci):\n")
if ("estrato_soc" %in% names(viviendas_df)) print(levels(viviendas_df$estrato_soc))
if ("tipificaci" %in% names(viviendas_df))  print(levels(viviendas_df$tipificaci))

# ============================================================
# 4. MATRICES DE CORRELACIÓN (NUMÉRICAS) Y ASOCIACIÓN (FACTORES)
#   Base: viviendas_df
#   - Incluye estrato_soc como variable ordinal numérica
#   - Incluye tipificaci como factor en asociación
#   - Exporta Excel con dos hojas
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(writexl); library(DescTools); library(tibble)
})

# --- 1) RUTA DE SALIDA (ajusta a tu carpeta base)
ruta_excel <- "C:/Users/ASUS/Documents/Daniel/IGAC investigacion y Prospectiva/Viviendas resilentes/Levantamiento de informacion/Spatial_Join/viviendas_df.xlsx"
carpeta_out <- dirname(ruta_excel)
archivo_salida <- file.path(carpeta_out, "matrices_cor_asoc.xlsx")

# --- 2) SEPARAR VARIABLES NUMÉRICAS Y CATEGÓRICAS ---
df_num <- viviendas_df %>%
  dplyr::select(where(is.numeric))

df_fac <- viviendas_df %>%
  dplyr::select(where(is.factor))

# --- 3) Incluir estrato_soc como numérica (ordinal) en la correlación ---
if ("estrato_soc" %in% names(viviendas_df)) {
  estr_numeric <- as.numeric(viviendas_df$estrato_soc)
  df_num <- df_num %>%
    mutate(estrato_soc = estr_numeric)
}

# --- 4) Asegurar tipificaci en factores (para Cramer's V) ---
if ("tipificaci" %in% names(viviendas_df) &&
    !("tipificaci" %in% names(df_fac))) {
  df_fac <- viviendas_df %>%
    dplyr::select(where(is.factor), tipificaci)
}

# --- 5) MATRIZ DE CORRELACIÓN (solo numéricas) ---
cor_num <- cor(df_num, use = "pairwise.complete.obs", method = "pearson")

# Convertir a data.frame largo para exportar
cor_num_df <- as.data.frame(cor_num) %>%
  tibble::rownames_to_column(var = "var")

# --- 6) MATRIZ DE ASOCIACIÓN (Cramer's V para factores) ---
cramersV_df <- NULL
if (ncol(df_fac) >= 2) {
  k <- ncol(df_fac)
  cramersV_mat <- matrix(NA_real_, nrow = k, ncol = k,
                         dimnames = list(names(df_fac), names(df_fac)))
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      x <- df_fac[[i]]; y <- df_fac[[j]]
      ok <- complete.cases(x, y)
      cramersV_mat[i, j] <- tryCatch(
        CramerV(table(x[ok], y[ok]), bias.correct = TRUE),
        error = function(e) NA_real_
      )
    }
  }
  cramersV_df <- as.data.frame(cramersV_mat) %>%
    tibble::rownames_to_column(var = "var")
} else {
  cramersV_df <- tibble(aviso = "No hay suficientes variables factor para calcular Cramer's V.")
}

# --- 7) VISTAS RÁPIDAS EN CONSOLA ---
cat("\n--- Correlación numérica (vista 10x10) ---\n")
print(cor_num[1:min(10, nrow(cor_num)), 1:min(10, ncol(cor_num))])

cat("\n--- Asociación Cramer's V (vista 10x10) ---\n")
if (!"aviso" %in% names(cramersV_df)) {
  mat_preview <- as.matrix(cramersV_df[,-1, drop=FALSE])
  rownames(mat_preview) <- cramersV_df$var
  print(mat_preview[1:min(10, nrow(mat_preview)), 1:min(10, ncol(mat_preview))])
} else print(cramersV_df)

# --- 8) EXPORTAR A EXCEL ---
write_xlsx(
  list(
    correlacion_numericas = cor_num_df,
    asociacion_factores = cramersV_df
  ),
  path = archivo_salida
)

cat("\n[✓] Archivo exportado exitosamente:\n", archivo_salida, "\n")

# --- 6bis) PARES CON ALTA CORRELACIÓN (|r| > umbral) ---
umbral_r <- 0.70

# Matriz de N por par (pairwise complete cases)
n_mat <- matrix(NA_integer_, nrow = ncol(df_num), ncol = ncol(df_num),
                dimnames = list(colnames(df_num), colnames(df_num)))
for (i in seq_len(ncol(df_num))) {
  for (j in seq_len(ncol(df_num))) {
    n_mat[i, j] <- sum(stats::complete.cases(df_num[, i], df_num[, j]))
  }
}

# Extraer triángulo superior en formato largo
get_pairs_from_matrix <- function(mat, nmat, thr = 0.70) {
  vars <- colnames(mat)
  pairs <- list()
  k <- 1
  for (i in seq_len(ncol(mat)-1)) {
    for (j in (i+1):ncol(mat)) {
      r_ij <- mat[i, j]
      if (!is.na(r_ij) && abs(r_ij) >= thr) {
        pairs[[k]] <- data.frame(
          var1  = vars[i],
          var2  = vars[j],
          r     = as.numeric(r_ij),
          abs_r = abs(as.numeric(r_ij)),
          n     = as.integer(nmat[i, j]),
          stringsAsFactors = FALSE
        )
        k <- k + 1
      }
    }
  }
  if (length(pairs) == 0) {
    return(dplyr::tibble(aviso = paste0("No hay pares con |r| >= ", thr)))
  }
  dplyr::bind_rows(pairs) %>%
    dplyr::arrange(dplyr::desc(abs_r))
}

pares_cor_alta_df <- get_pairs_from_matrix(cor_num, n_mat, thr = umbral_r)

write_xlsx(
  list(
    correlacion_numericas = cor_num_df,
    asociacion_factores   = cramersV_df,
    alerta_pares_cor_altas = pares_cor_alta_df
  ),
  path = archivo_salida
)

cat("\n[✓] Archivo exportado con 3 hojas:\n - correlacion_numericas\n - asociacion_factores\n - alerta_pares_cor_altas (|r| >= ", umbral_r, ")\n", sep = "")


# ============================================================
# 5. DEFINICIÓN DE GRUPOS DE VARIABLES PARA LOS MODELOS
#   Base: viviendas_df
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
})

# --- 1) Grupos iniciales según tu descripción ---

vars_construccion <- c(
  "cubierta", "color_cubi", "muros", "fachada",
  "ventilaci", "altura", "patio", "antigueda"
)

vars_terreno <- c(
  "ndvi", "sombras", "emplazamie",
  "material_v", "ancho_via", "estrato_soc",
  "walk_index", "orientacio", "indice_vie"
)

vars_servicios <- c(
   "acueducto"
)

# --- 2) Validar qué variables existen efectivamente en viviendas_df ---

check_exist <- function(vars, data) {
  vars_presentes <- intersect(vars, names(data))
  vars_faltantes <- setdiff(vars, vars_presentes)
  list(
    presentes = vars_presentes,
    faltantes = vars_faltantes
  )
}

grupo_construccion <- check_exist(vars_construccion, viviendas_df)
grupo_terreno      <- check_exist(vars_terreno,      viviendas_df)
grupo_servicios    <- check_exist(vars_servicios,    viviendas_df)

# --- 3) Mostrar resumen en consola ---

cat("\n[Grupo: Construcción]\n")
cat("Presentes:", paste(grupo_construccion$presentes, collapse = ", "), "\n")
if (length(grupo_construccion$faltantes) > 0)
  cat("⚠️ Faltantes:", paste(grupo_construccion$faltantes, collapse = ", "), "\n")

cat("\n[Grupo: Terreno]\n")
cat("Presentes:", paste(grupo_terreno$presentes, collapse = ", "), "\n")
if (length(grupo_terreno$faltantes) > 0)
  cat("⚠️ Faltantes:", paste(grupo_terreno$faltantes, collapse = ", "), "\n")

cat("\n[Grupo: Servicios]\n")
cat("Presentes:", paste(grupo_servicios$presentes, collapse = ", "), "\n")
if (length(grupo_servicios$faltantes) > 0)
  cat("⚠️ Faltantes:", paste(grupo_servicios$faltantes, collapse = ", "), "\n")

# --- 4) Crear listas finales (solo con las variables presentes) ---
vars_construccion_final <- grupo_construccion$presentes
vars_terreno_final      <- grupo_terreno$presentes
vars_servicios_final    <- grupo_servicios$presentes

# --- 5) Guardar los tres grupos en una lista general para facilitar uso posterior ---
grupos_modelo <- list(
  construccion = vars_construccion_final,
  terreno      = vars_terreno_final,
  servicios    = vars_servicios_final
)

# ============================================================
# 6. REGRESIÓN OLS ROBUSTA: Y = log_valor_metr
#   - X = grupo terreno + grupo servicios
#   - Pesos = factor_exp_nc_norm (IPW normalizado)
#   - Robustez: Heterocedasticidad (HC3)
#   - Diagnósticos: VIF, BP, JB, RESET, Kappa
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(lmtest); library(sandwich); library(car); library(tseries); library(broom)
})

# --- 1) Preparar datos
x_terreno   <- grupos_modelo$terreno
x_servicios <- grupos_modelo$servicios
peso_var    <- if ("factor_exp_nc_norm" %in% names(viviendas_df)) "factor_exp_nc_norm" else "factor_exp_nc"

x_vars <- unique(c(x_terreno, x_servicios))
form_str <- paste0("log_valor_metr ~ ", paste(x_vars, collapse = " + "))
fmla <- as.formula(form_str)
cat("[Fórmula]\n", deparse(fmla), "\n")

work_df <- viviendas_df %>%
  dplyr::select(all_of(c("log_valor_metr", x_vars, peso_var))) %>%
  filter(is.finite(.data[[peso_var]]), .data[[peso_var]] > 0) %>%
  tidyr::drop_na()

cat("[N] observaciones:", nrow(work_df), "\n")

# --- 2) Estimar modelo OLS ponderado
m_ols <- lm(fmla, data = work_df, weights = work_df[[peso_var]])
summary(m_ols)


# 1. Revisar leverage (hat values)
lev <- hatvalues(m_ols)
summary(lev)
which(lev > 0.99)   # observa cuáles son casi 1

# 2. Revisar colinealidad
car::vif(m_ols)

# 3. Revisar variación en dummies
sapply(work_df[, x_vars], function(x) length(unique(x)))

# Detecta variables sin variación o casi constantes
vars_quitar <- names(Filter(function(x) length(unique(x)) <= 1, work_df))
vars_quitar

# --- 3) Errores robustos (HC3)
vcov_hc3 <- sandwich::vcovHC(m_ols, type = "HC3")
coeftest_hc3 <- lmtest::coeftest(m_ols, vcov. = vcov_hc3)

cat("\n--- Resultados OLS robustos (HC3) ---\n")
print(coeftest_hc3)

# --- 4) Diagnósticos
cat("\n--- Diagnósticos ---\n")

# Colinealidad
vif_vals <- car::vif(m_ols)
cat("\n[VIF]\n"); print(sort(vif_vals, decreasing = TRUE))

# Número de condición (sensibilidad a colinealidad)
X <- model.matrix(m_ols)
kappa_val <- kappa(X[, -1, drop = FALSE])
cat("\n[Kappa] =", round(kappa_val, 2), "(>30 indica colinealidad potencial)\n")

# Heterocedasticidad (BP)
bp <- lmtest::bptest(m_ols)
cat("\n[Breusch-Pagan]\n"); print(bp)

# Normalidad (Jarque-Bera)
jb <- tseries::jarque.bera.test(residuals(m_ols))
cat("\n[Jarque-Bera]\n"); print(jb)

# Especificación funcional (RESET)
rst <- lmtest::resettest(m_ols, power = 2:3, type = "fitted")
cat("\n[RESET Test]\n"); print(rst)

# --- 5) Tabla ordenada para exportar
tidy_hc3 <- broom::tidy(m_ols) %>%
  mutate(
    std.error = sqrt(diag(vcov_hc3)),
    statistic = estimate / std.error,
    p.value   = 2 * pnorm(-abs(statistic)),
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE ~ ""
    )
  ) %>%
  select(term, estimate, std.error, statistic, p.value, stars)

cat("\n--- Coeficientes con estrellas ---\n")
print(tidy_hc3)

# --- 6) Métricas generales
r2    <- summary(m_ols)$r.squared
r2adj <- summary(m_ols)$adj.r.squared
aic_v <- AIC(m_ols); bic_v <- BIC(m_ols)
cat("\n[Métricas]\nR2:", round(r2,4), " | R2 adj:", round(r2adj,4),
    " | AIC:", round(aic_v,1), " | BIC:", round(bic_v,1), " | N:", nobs(m_ols), "\n")

# ================================================================
# 6.1 EXPORTAR RESULTADOS OLS (HC3) A UNA SOLA HOJA DE EXCEL
#   - Requiere un objeto de modelo: m_ols (lm con weights)
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(broom); library(openxlsx)
  library(lmtest); library(sandwich); library(car); library(tseries)
})

# --- 1) Coeficientes con errores robustos HC3 + estrellas ---
vcov_hc3 <- sandwich::vcovHC(m_ols, type = "HC3")
coeftest_hc3 <- lmtest::coeftest(m_ols, vcov. = vcov_hc3)

# Convertir SIEMPRE a matriz y armar el data frame por posición de columnas
coef_mat <- as.matrix(coeftest_hc3)

# Chequeo defensivo de dimensiones esperadas (4 columnas)
if (ncol(coef_mat) < 4) {
  stop("coeftest() no devolvió 4 columnas (Estimate, StdError, t, p). Revisa el modelo.")
}

coef_df <- data.frame(
  Variable = rownames(coef_mat),
  Estimate = as.numeric(coef_mat[, 1]),
  StdError = as.numeric(coef_mat[, 2]),
  t_value  = as.numeric(coef_mat[, 3]),
  p_value  = as.numeric(coef_mat[, 4]),
  check.names = FALSE,
  row.names = NULL
)

coef_df <- coef_df %>%
  dplyr::mutate(
    Signif = dplyr::case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE ~ ""
    ),
    Estimate = round(Estimate, 4),
    StdError = round(StdError, 4),
    t_value  = round(t_value, 3),
    p_value  = signif(p_value, 3)
  )

# --- 2) Estadísticos y pruebas ---
n_obs   <- nobs(m_ols)
r2      <- summary(m_ols)$r.squared
r2_adj  <- summary(m_ols)$adj.r.squared
aic_val <- AIC(m_ols)

vif_vals <- car::vif(m_ols)
vif_max  <- max(vif_vals, na.rm = TRUE)

lev      <- hatvalues(m_ols)
lev_max  <- max(lev, na.rm = TRUE)

bp  <- lmtest::bptest(m_ols)
jb  <- tseries::jarque.bera.test(residuals(m_ols))
rst <- lmtest::resettest(m_ols, power = 2:3, type = "fitted")

stats_df <- tibble(
  Indicador = c(
    "N observaciones",
    "R2",
    "R2 ajustado",
    "AIC",
    "VIF máximo",
    "Leverage máximo",
    "Breusch-Pagan (p)",
    "Jarque-Bera (p)",
    "RESET (p)"
  ),
  Valor = c(
    n_obs,
    round(r2, 4),
    round(r2_adj, 4),
    round(aic_val, 2),
    round(vif_max, 3),
    round(lev_max, 3),
    signif(bp$p.value, 4),
    signif(jb$p.value, 4),
    signif(rst$p.value, 4)
  )
)

# --- 3) Escribir TODO en UNA sola hoja ---
ruta_salida <- file.path(getwd(), "Resultados_OLS_log_valor_metr_UNA_HOJA.xlsx")

wb <- createWorkbook()
addWorksheet(wb, "Resultados")

# Título
writeData(wb, "Resultados", "Resultados OLS ponderado (HC3)", startRow = 1, startCol = 1)
addStyle(wb, "Resultados",
         createStyle(textDecoration = "bold", fontSize = 12),
         rows = 1, cols = 1, gridExpand = TRUE)

# Tabla de coeficientes
writeData(wb, "Resultados", coef_df, startRow = 3, startCol = 1, withFilter = TRUE)

# Fila donde empieza la parte de estadísticos
fila_stats <- nrow(coef_df) + 5

# Subtítulo
writeData(wb, "Resultados", "Estadísticos y pruebas del modelo", startRow = fila_stats, startCol = 1)
addStyle(wb, "Resultados",
         createStyle(textDecoration = "bold"),
         rows = fila_stats, cols = 1, gridExpand = TRUE)

# Estadísticos debajo
writeData(wb, "Resultados", stats_df, startRow = fila_stats + 2, startCol = 1)

# --- 4) Bordes y autoajuste (versión compatible con openxlsx >=4.3) ---
estilo_tabla <- createStyle(
  borderStyle = "thin",
  borderColour = "black"
)

# Bordes para la tabla de coeficientes
addStyle(
  wb, "Resultados", estilo_tabla,
  rows = 3:(3 + nrow(coef_df)), cols = 1:ncol(coef_df),
  gridExpand = TRUE, stack = TRUE
)

# Bordes para la tabla de estadísticos
addStyle(
  wb, "Resultados", estilo_tabla,
  rows = (fila_stats + 2):(fila_stats + 1 + nrow(stats_df)),
  cols = 1:2, gridExpand = TRUE, stack = TRUE
)

# Ajustar automáticamente el ancho de las columnas
setColWidths(wb, "Resultados", cols = 1:max(ncol(coef_df), 2), widths = "auto")

# Guardar el archivo Excel
saveWorkbook(wb, ruta_salida, overwrite = TRUE)



# ================================================================
# REGRESIÓN: log_v_m2_cons ~ (controles de construcción + servicios)
#   - Limpia categorías raras
#   - Filtra leverage extremos
#   - OLS ponderado con HC3 y fallback a HC2
#   - Diagnósticos + exportación a Excel (una hoja)
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(forcats); library(lmtest); library(sandwich)
  library(car); library(tseries); library(broom); library(openxlsx)
})

# -----------------------
# 0) Definiciones de vars
# -----------------------
x_construccion <- grupos_modelo$construccion
x_servicios    <- grupos_modelo$servicios
peso_var       <- if ("factor_exp_nc_norm" %in% names(viviendas_df)) "factor_exp_nc_norm" else "factor_exp_nc"

dep_var <- "log_v_m2_cons"
x_vars  <- unique(c(x_construccion, x_servicios))

# -----------------------
# 1) Preparar base
#   - seleccionar columnas
#   - dropear NA
#   - recodificar categorías raras (min 3 obs)
# -----------------------
min_count <- 3  # umbral de frecuencia mínima por categoría a conservar

work_df <- viviendas_df %>%
  dplyr::select(all_of(c(dep_var, x_vars, peso_var))) %>%
  filter(is.finite(.data[[peso_var]]), .data[[peso_var]] > 0) %>%
  tidyr::drop_na()

# Recodificar factores problemáticos si existen
for (v in c("fachada","cubierta","muros","ventilaci")) {
  if (v %in% names(work_df) && is.factor(work_df[[v]])) {
    work_df[[v]] <- work_df[[v]] %>%
      forcats::fct_lump_min(min = min_count) %>%
      forcats::fct_drop()
  }
}

# Si 'ancho_via' quedó como factor en esta especificación, lo dejamos como factor (en construcción no lo usamos)
# Si tienes alguna otra variable que debas numérica, conviértela aquí. Ej:
# if ("altura" %in% names(work_df)) work_df$altura <- as.numeric(work_df$altura)

# -----------------------
# 2) Fórmula y modelo base
# -----------------------
form_str <- paste0(dep_var, " ~ ", paste(x_vars, collapse = " + "))
fmla <- as.formula(form_str)
cat("[Fórmula]\n", deparse(fmla), "\n")

m0 <- lm(fmla, data = work_df, weights = work_df[[peso_var]])
summary(m0)
# -----------------------
# 3) Filtrar leverage extremos y refit
# -----------------------
lev0 <- hatvalues(m0)
hi_idx <- which(lev0 >= 0.99)

if (length(hi_idx)) {
  cat("Se filtran", length(hi_idx), "obs con leverage >= 0.99:", paste(hi_idx, collapse = ", "), "\n")
  work_df <- work_df[-hi_idx, , drop = FALSE]
}

m_ols <- lm(fmla, data = work_df, weights = work_df[[peso_var]])
sm    <- summary(m_ols)
summary(m_ols)
cat("\n--- Resumen OLS ponderado (base) ---\n"); print(sm)

# -----------------------
# 4) Varianzas robustas: HC3 con fallback a HC2 si hay NaN
# -----------------------
get_hc <- function(mod, type = "HC3") {
  vc <- try(sandwich::vcovHC(mod, type = type), silent = TRUE)
  if (inherits(vc, "try-error")) return(NULL)
  vc
}

vcov_hc <- get_hc(m_ols, "HC3")
use_type <- "HC3"

if (is.null(vcov_hc) || any(!is.finite(diag(vcov_hc)))) {
  vcov_hc <- get_hc(m_ols, "HC2")
  use_type <- "HC2"
}

coeftest_hc <- lmtest::coeftest(m_ols, vcov. = vcov_hc)
cat("\n--- Resultados OLS robustos (", use_type, ") ---\n", sep = ""); print(coeftest_hc)

# -----------------------
# 5) Diagnósticos
# -----------------------
cat("\n--- Diagnósticos ---\n")
# VIF (manejo de aliasing)
vif_vals <- tryCatch(car::vif(m_ols), error = function(e) e)
if (inherits(vif_vals, "error")) {
  cat("[VIF] No disponible por aliasing de coeficientes.\n")
  vif_max <- NA_real_
} else {
  cat("\n[VIF]\n"); print(sort(vif_vals, decreasing = TRUE))
  vif_max <- max(vif_vals, na.rm = TRUE)
}

# Kappa
X <- model.matrix(m_ols)
kappa_val <- kappa(X[, -1, drop = FALSE])
cat("\n[Kappa] =", round(kappa_val, 2), "(>30 indica colinealidad potencial)\n")

# Heterocedasticidad
bp <- lmtest::bptest(m_ols); cat("\n[Breusch-Pagan]\n"); print(bp)
# Normalidad
jb <- tseries::jarque.bera.test(residuals(m_ols)); cat("\n[Jarque-Bera]\n"); print(jb)
# RESET
rst <- lmtest::resettest(m_ols, power = 2:3, type = "fitted"); cat("\n[RESET]\n"); print(rst)
# Leverage máx
lev  <- hatvalues(m_ols); lev_max <- max(lev, na.rm = TRUE)

# -----------------------
# 6) Tabla ordenada con estrellas (robustas)
# -----------------------
coef_mat <- as.matrix(coeftest_hc)
coef_df <- data.frame(
  Variable = rownames(coef_mat),
  Estimate = as.numeric(coef_mat[,1]),
  StdError = as.numeric(coef_mat[,2]),
  t_value  = as.numeric(coef_mat[,3]),
  p_value  = as.numeric(coef_mat[,4]),
  check.names = FALSE, row.names = NULL
) %>%
  mutate(
    Signif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ ".",
      TRUE ~ ""
    ),
    Estimate = round(Estimate, 4),
    StdError = round(StdError, 4),
    t_value  = round(t_value, 3),
    p_value  = signif(p_value, 3)
  )

# -----------------------
# 7) Estadísticos y pruebas (bloque inferior)
# -----------------------
n_obs   <- nobs(m_ols)
r2      <- sm$r.squared
r2_adj  <- sm$adj.r.squared
aic_val <- AIC(m_ols)

stats_df <- tibble::tibble(
  Indicador = c(
    "N observaciones",
    "R2",
    "R2 ajustado",
    "AIC",
    "VIF máximo",
    "Leverage máximo",
    "Breusch-Pagan (p)",
    "Jarque-Bera (p)",
    "RESET (p)",
    "Varianza robusta"
  ),
  Valor = c(
    n_obs,
    round(r2, 4),
    round(r2_adj, 4),
    round(aic_val, 2),
    ifelse(is.na(vif_max), NA, round(vif_max, 3)),
    round(lev_max, 3),
    signif(bp$p.value, 4),
    signif(jb$p.value, 4),
    signif(rst$p.value, 4),
    use_type
  )
)

# -----------------------
# 8) Exportar a Excel (una sola hoja)
# -----------------------
ruta_salida <- file.path(getwd(), "Resultados_OLS_log_v_m2_cons_UNA_HOJA.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Resultados")

writeData(wb, "Resultados", "Resultados OLS ponderado (robusto)", startRow = 1, startCol = 1)
addStyle(wb, "Resultados",
         createStyle(textDecoration = "bold", fontSize = 12),
         rows = 1, cols = 1, gridExpand = TRUE)

# Coeficientes
writeData(wb, "Resultados", coef_df, startRow = 3, startCol = 1, withFilter = TRUE)

fila_stats <- nrow(coef_df) + 5
writeData(wb, "Resultados", "Estadísticos y pruebas del modelo", startRow = fila_stats, startCol = 1)
addStyle(wb, "Resultados",
         createStyle(textDecoration = "bold"),
         rows = fila_stats, cols = 1, gridExpand = TRUE)

writeData(wb, "Resultados", stats_df, startRow = fila_stats + 2, startCol = 1)

# Bordes + autoancho
box_style <- createStyle(borderStyle = "thin", borderColour = "black")
addStyle(wb, "Resultados", box_style,
         rows = 3:(3 + nrow(coef_df)), cols = 1:ncol(coef_df),
         gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Resultados", box_style,
         rows = (fila_stats + 2):(fila_stats + 1 + nrow(stats_df)),
         cols = 1:2, gridExpand = TRUE, stack = TRUE)

setColWidths(wb, "Resultados", cols = 1:max(ncol(coef_df), 2), widths = "auto")
saveWorkbook(wb, ruta_salida, overwrite = TRUE)

cat("\nArchivo Excel guardado en:\n", ruta_salida, "\n")


# ================================================================
# OLS ponderado (log_v_m2_cons) | Construcción + Servicios (sin 'fachada')
# - Pondera con factor_exp_nc_norm (fallback a factor_exp_nc)
# - Recodifica categorías raras
# - Filtra leverage extremos (hat >= 0.99) y re-estima
# - Varianzas robustas HC3 (fallback HC2)
# - Exporta a Excel (una sola hoja): coeficientes + métricas/pruebas
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(forcats); library(tidyr)
  library(lmtest); library(sandwich); library(broom)
  library(car); library(tseries); library(openxlsx)
})

# -----------------------
# 0) Definiciones
# -----------------------
dep_var <- "log_v_m2_cons"

# grupos_modelo$construccion: cubierta, color_cubi, muros, fachada, ventilaci, altura, patio, antigueda
x_construccion <- setdiff(grupos_modelo$construccion, "fachada")  # excluye 'fachada'
x_servicios    <- grupos_modelo$servicios                         # p.ej., alcantaril, acueducto
x_vars         <- unique(c(x_construccion, x_servicios))

peso_var <- if ("factor_exp_nc_norm" %in% names(viviendas_df)) "factor_exp_nc_norm" else "factor_exp_nc"

# -----------------------
# 1) Preparar base
# -----------------------
min_count <- 3  # mínimo de observaciones por categoría antes de agrupar en "Other"

work_df <- viviendas_df %>%
  dplyr::select(dplyr::all_of(c(dep_var, x_vars, peso_var))) %>%
  dplyr::filter(is.finite(.data[[peso_var]]), .data[[peso_var]] > 0) %>%
  tidyr::drop_na()

# Recodificar categorías poco frecuentes y dropear niveles sin uso
for (v in c("cubierta","color_cubi","muros","ventilaci")) {
  if (v %in% names(work_df) && is.factor(work_df[[v]])) {
    work_df[[v]] <- work_df[[v]] %>%
      forcats::fct_lump_min(min = min_count) %>%
      forcats::fct_drop()
  }
}

cat("[N inicial] =", nrow(work_df), "\n")

# -----------------------
# 2) Estimación base
# -----------------------
fmla <- as.formula(
  paste0(dep_var, " ~ ", paste(x_vars, collapse = " + "))
)
cat("[Fórmula]\n", deparse(fmla), "\n")

m0 <- lm(fmla, data = work_df, weights = work_df[[peso_var]])
cat("\n--- Resumen OLS ponderado (estimación base) ---\n")
print(summary(m0))

# -----------------------
# 3) Filtrar leverage extremos y re-estimar
# -----------------------
lev0   <- hatvalues(m0)
hi_idx <- which(lev0 >= 0.99)

if (length(hi_idx)) {
  cat("\n[Leverage] Se filtran", length(hi_idx), "obs con hat >= 0.99:", paste(hi_idx, collapse = ", "), "\n")
  work_df <- work_df[-hi_idx, , drop = FALSE]
} else {
  cat("\n[Leverage] No se detectaron observaciones con hat >= 0.99\n")
}

m_ols <- lm(fmla, data = work_df, weights = work_df[[peso_var]])
sm    <- summary(m_ols)
cat("\n--- Resumen OLS ponderado (refit sin leverage extremos) ---\n")
print(sm)

# -----------------------
# 4) Varianzas robustas (HC3 -> fallback HC2)
# -----------------------
get_hc <- function(mod, type = "HC3") {
  vc <- try(sandwich::vcovHC(mod, type = type), silent = TRUE)
  if (inherits(vc, "try-error")) return(NULL)
  vc
}

vcov_hc <- get_hc(m_ols, "HC3"); use_type <- "HC3"
if (is.null(vcov_hc) || any(!is.finite(diag(vcov_hc)))) {
  vcov_hc <- get_hc(m_ols, "HC2"); use_type <- "HC2"
}

coeftest_hc <- lmtest::coeftest(m_ols, vcov. = vcov_hc)
cat("\n--- Resultados OLS ponderados robustos (", use_type, ") ---\n", sep = "")
print(coeftest_hc)

# -----------------------
# 5) Tabla ordenada con estrellas (robustas)
# -----------------------
coef_mat <- as.matrix(coeftest_hc)
coef_df <- data.frame(
  Variable = rownames(coef_mat),
  Estimate = as.numeric(coef_mat[,1]),
  StdError = as.numeric(coef_mat[,2]),
  t_value  = as.numeric(coef_mat[,3]),
  p_value  = as.numeric(coef_mat[,4]),
  check.names = FALSE, row.names = NULL
) %>%
  dplyr::mutate(
    Signif = dplyr::case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ ".",
      TRUE ~ ""
    ),
    Estimate = round(Estimate, 4),
    StdError = round(StdError, 4),
    t_value  = round(t_value, 3),
    p_value  = signif(p_value, 3)
  )

cat("\n--- Coeficientes (robustos ", use_type, ") con estrellas ---\n", sep = "")
print(coef_df)

# -----------------------
# 6) Diagnósticos y pruebas
# -----------------------
# VIF (manejo de aliasing)
vif_try <- tryCatch(car::vif(m_ols), error = function(e) e)
if (inherits(vif_try, "error")) {
  cat("\n[VIF] No disponible por aliasing de coeficientes.\n")
  vif_max <- NA_real_
} else {
  vif_vals <- vif_try
  vif_max  <- max(vif_vals, na.rm = TRUE)
  cat("\n[VIF max] ", round(vif_max, 3), "\n", sep = "")
}

# Leverage máximo
lev  <- hatvalues(m_ols); lev_max <- max(lev, na.rm = TRUE)

# Heterocedasticidad (BP)
bp <- lmtest::bptest(m_ols)
print(bp)

# Normalidad de residuos (Jarque–Bera)
jb <- tseries::jarque.bera.test(residuals(m_ols))
print(jb)

# Especificación funcional (RESET)
rst <- lmtest::resettest(m_ols, power = 2:3, type = "fitted")

# Métricas
n_obs   <- nobs(m_ols)
r2      <- sm$r.squared
r2_adj  <- sm$adj.r.squared
aic_val <- AIC(m_ols)

stats_df <- tibble::tibble(
  Indicador = c(
    "N observaciones",
    "R2",
    "R2 ajustado",
    "AIC",
    "VIF máximo",
    "Leverage máximo",
    "Breusch-Pagan (p)",
    "Jarque-Bera (p)",
    "RESET (p)",
    "Varianza robusta"
  ),
  Valor = c(
    n_obs,
    round(r2, 4),
    round(r2_adj, 4),
    round(aic_val, 2),
    ifelse(is.na(vif_max), NA, round(vif_max, 3)),
    round(lev_max, 3),
    signif(bp$p.value, 4),
    signif(jb$p.value, 4),
    signif(rst$p.value, 4),
    use_type
  )
)

# -----------------------
# 7) Exportar a Excel (una sola hoja)
# -----------------------
ruta_salida <- file.path(getwd(), "Resultados_OLS_log_v_m2_cons_sin_fachada_UNA_HOJA.xlsx")

wb <- createWorkbook()
addWorksheet(wb, "Resultados")

# Título
writeData(wb, "Resultados", "Resultados OLS ponderado (robusto) - log_v_m2_cons (sin fachada)", startRow = 1, startCol = 1)
addStyle(wb, "Resultados",
         createStyle(textDecoration = "bold", fontSize = 12),
         rows = 1, cols = 1, gridExpand = TRUE)

# Coeficientes
writeData(wb, "Resultados", coef_df, startRow = 3, startCol = 1, withFilter = TRUE)

# Bloque inferior de estadísticos
fila_stats <- nrow(coef_df) + 5
writeData(wb, "Resultados", "Estadísticos y pruebas del modelo", startRow = fila_stats, startCol = 1)
addStyle(wb, "Resultados",
         createStyle(textDecoration = "bold"),
         rows = fila_stats, cols = 1, gridExpand = TRUE)

writeData(wb, "Resultados", stats_df, startRow = fila_stats + 2, startCol = 1)

# Bordes + autoancho
box_style <- createStyle(borderStyle = "thin", borderColour = "black")
addStyle(wb, "Resultados", box_style,
         rows = 3:(3 + nrow(coef_df)), cols = 1:ncol(coef_df),
         gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Resultados", box_style,
         rows = (fila_stats + 2):(fila_stats + 1 + nrow(stats_df)),
         cols = 1:2, gridExpand = TRUE, stack = TRUE)

setColWidths(wb, "Resultados", cols = 1:max(ncol(coef_df), 2), widths = "auto")

saveWorkbook(wb, ruta_salida, overwrite = TRUE)


# ================================================================
# OLS ponderado (log_int_terr)
# Controles: construcción + terreno + servicios
# ------------------------------------------------
# - Pondera con factor_exp_nc_norm (fallback a factor_exp_nc)
# - Recodifica categorías raras
# - Filtra leverage >= 0.99 y re-estima
# - Varianzas robustas HC3 (fallback HC2)
# - Exporta a Excel (coeficientes + pruebas + notas)
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(forcats); library(tidyr)
  library(lmtest); library(sandwich); library(broom)
  library(car); library(tseries); library(openxlsx)
})

# -----------------------
# 0) Definiciones
# -----------------------
dep_var <- "log_int_terr"

# Definir los grupos
x_construccion <- grupos_modelo$construccion
x_terreno      <- grupos_modelo$terreno
x_servicios    <- grupos_modelo$servicios

x_vars <- unique(c(x_construccion, x_terreno, x_servicios))

peso_var <- if ("factor_exp_nc_norm" %in% names(viviendas_df)) "factor_exp_nc_norm" else "factor_exp_nc"

# -----------------------
# 1) Preparar base
# -----------------------
min_count <- 3

work_df <- viviendas_df %>%
  dplyr::select(dplyr::all_of(c(dep_var, x_vars, peso_var))) %>%
  dplyr::filter(is.finite(.data[[peso_var]]), .data[[peso_var]] > 0) %>%
  tidyr::drop_na()

# Recodificar categorías con pocas observaciones
for (v in c("cubierta","color_cubi","muros","ventilaci","fachada")) {
  if (v %in% names(work_df) && is.factor(work_df[[v]])) {
    work_df[[v]] <- work_df[[v]] %>%
      forcats::fct_lump_min(min = min_count) %>%
      forcats::fct_drop()
  }
}

cat("[N inicial] =", nrow(work_df), "\n")

# -----------------------
# 2) Estimación base
# -----------------------
fmla <- as.formula(paste0(dep_var, " ~ ", paste(x_vars, collapse = " + ")))
cat("[Fórmula]\n", deparse(fmla), "\n")

m0 <- lm(fmla, data = work_df, weights = work_df[[peso_var]])
cat("\n--- Resumen OLS ponderado (estimación base) ---\n")
print(summary(m0))

# -----------------------
# 3) Filtrar leverage extremos y re-estimar
# -----------------------
lev0   <- hatvalues(m0)
hi_idx <- which(lev0 >= 0.99)

if (length(hi_idx)) {
  cat("\n[Leverage] Se filtran", length(hi_idx), "obs con hat >= 0.99:", paste(hi_idx, collapse = ", "), "\n")
  work_df <- work_df[-hi_idx, , drop = FALSE]
} else {
  cat("\n[Leverage] No se detectaron observaciones con hat >= 0.99\n")
}

m_ols <- lm(fmla, data = work_df, weights = work_df[[peso_var]])
sm    <- summary(m_ols)
cat("\n--- Resumen OLS ponderado (refit sin leverage extremos) ---\n")
print(sm)

# -----------------------
# 4) Varianzas robustas (HC3 -> fallback HC2)
# -----------------------
get_hc <- function(mod, type = "HC3") {
  vc <- try(sandwich::vcovHC(mod, type = type), silent = TRUE)
  if (inherits(vc, "try-error")) return(NULL)
  vc
}

vcov_hc <- get_hc(m_ols, "HC3"); use_type <- "HC3"
if (is.null(vcov_hc) || any(!is.finite(diag(vcov_hc)))) {
  vcov_hc <- get_hc(m_ols, "HC2"); use_type <- "HC2"
}

coeftest_hc <- lmtest::coeftest(m_ols, vcov. = vcov_hc)
cat("\n--- Resultados OLS ponderados robustos (", use_type, ") ---\n", sep = "")
print(coeftest_hc)

# -----------------------
# 5) Tabla ordenada con estrellas (robustas)
# -----------------------
coef_mat <- as.matrix(coeftest_hc)
coef_df <- data.frame(
  Variable = rownames(coef_mat),
  Estimate = as.numeric(coef_mat[,1]),
  StdError = as.numeric(coef_mat[,2]),
  t_value  = as.numeric(coef_mat[,3]),
  p_value  = as.numeric(coef_mat[,4]),
  check.names = FALSE, row.names = NULL
) %>%
  dplyr::mutate(
    Signif = dplyr::case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ ".",
      TRUE ~ ""
    ),
    Estimate = round(Estimate, 4),
    StdError = round(StdError, 4),
    t_value  = round(t_value, 3),
    p_value  = signif(p_value, 3)
  )

cat("\n--- Coeficientes (robustos ", use_type, ") con estrellas ---\n", sep = "")
print(coef_df)

# -----------------------
# 6) Diagnósticos y pruebas
# -----------------------
# VIF
vif_try <- tryCatch(car::vif(m_ols), error = function(e) e)
if (inherits(vif_try, "error")) {
  cat("\n[VIF] No disponible por aliasing de coeficientes.\n")
  vif_max <- NA_real_
} else {
  vif_vals <- vif_try
  vif_max  <- max(vif_vals, na.rm = TRUE)
  cat("\n[VIF max] ", round(vif_max, 3), "\n", sep = "")
}

# Leverage máximo
lev  <- hatvalues(m_ols); lev_max <- max(lev, na.rm = TRUE)

# Pruebas clásicas
bp  <- lmtest::bptest(m_ols)
print(bp)
jb  <- tseries::jarque.bera.test(residuals(m_ols))
print(jb)
rst <- lmtest::resettest(m_ols, power = 2:3, type = "fitted")

# Métricas generales
n_obs   <- nobs(m_ols)
r2      <- sm$r.squared
r2_adj  <- sm$adj.r.squared
aic_val <- AIC(m_ols)

stats_df <- tibble::tibble(
  Indicador = c(
    "N observaciones",
    "R2",
    "R2 ajustado",
    "AIC",
    "VIF máximo",
    "Leverage máximo",
    "Breusch-Pagan (p)",
    "Jarque-Bera (p)",
    "RESET (p)",
    "Varianza robusta"
  ),
  Valor = c(
    n_obs,
    round(r2, 4),
    round(r2_adj, 4),
    round(aic_val, 2),
    ifelse(is.na(vif_max), NA, round(vif_max, 3)),
    round(lev_max, 3),
    signif(bp$p.value, 4),
    signif(jb$p.value, 4),
    signif(rst$p.value, 4),
    use_type
  )
)

# -----------------------
# 7) Exportar a Excel (una sola hoja) + Notas metodológicas
# -----------------------
ruta_salida <- file.path(getwd(), "Resultados_OLS_log_int_terr_UNA_HOJA.xlsx")

wb <- createWorkbook()
addWorksheet(wb, "Resultados")

# Título
writeData(wb, "Resultados",
          "Resultados OLS ponderado (robusto) - log_int_terr",
          startRow = 1, startCol = 1)
addStyle(wb, "Resultados",
         createStyle(textDecoration = "bold", fontSize = 12),
         rows = 1, cols = 1, gridExpand = TRUE)

# Coeficientes
writeData(wb, "Resultados", coef_df, startRow = 3, startCol = 1, withFilter = TRUE)

# Bloque inferior de estadísticos
fila_stats <- nrow(coef_df) + 5
writeData(wb, "Resultados", "Estadísticos y pruebas del modelo",
          startRow = fila_stats, startCol = 1)
addStyle(wb, "Resultados",
         createStyle(textDecoration = "bold"),
         rows = fila_stats, cols = 1, gridExpand = TRUE)
writeData(wb, "Resultados", stats_df, startRow = fila_stats + 2, startCol = 1)

# ----- Notas metodológicas -----
fila_notas <- fila_stats + 2 + nrow(stats_df) + 2

ids_filtrados <- if (exists("hi_idx") && length(hi_idx)) paste(hi_idx, collapse = ", ") else "Ninguno"
n_filtrados   <- if (exists("hi_idx")) length(hi_idx) else 0
n_final       <- n_obs
n_inicial     <- if (exists("lev0")) length(lev0) else NA_integer_

nota_texto <- paste0(
  "Notas metodológicas:\n",
  "- Variable dependiente: ", dep_var, "\n",
  "- Especificación: ", deparse(fmla), "\n",
  "- Ponderación: ", peso_var, " (factor de expansión no calibrado",
  ifelse(peso_var == "factor_exp_nc_norm", " normalizado", ""), ")\n",
  "- Varianza robusta empleada: ", use_type, " (fallback automático en caso de inestabilidad)\n",
  "- Umbral de leverage: hat ≥ 0.99\n",
  "- Observaciones filtradas por leverage: ", n_filtrados,
  ifelse(n_filtrados > 0, paste0(" (IDs: ", ids_filtrados, ")"), ""), "\n",
  "- N inicial: ", n_inicial, " | N final: ", n_final, "\n",
  "- Agrupación de categorías raras (min=", min_count,
  ") aplicada a: cubierta, color_cubi, muros, ventilaci, fachada.\n",
  "- Errores estándar robustos (", use_type, ") reportados en la tabla.\n",
  "- Interpretación: coeficientes expresan semi-elasticidades (log puntos)."
)

writeData(wb, "Resultados", nota_texto, startRow = fila_notas, startCol = 1)
addStyle(wb, "Resultados",
         createStyle(italic = TRUE, fontSize = 10, halign = "left", valign = "top"),
         rows = fila_notas, cols = 1, gridExpand = TRUE)

# Bordes + autoancho
box_style <- createStyle(borderStyle = "thin", borderColour = "black")
addStyle(wb, "Resultados", box_style,
         rows = 3:(3 + nrow(coef_df)), cols = 1:ncol(coef_df),
         gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Resultados", box_style,
         rows = (fila_stats + 2):(fila_stats + 1 + nrow(stats_df)),
         cols = 1:2, gridExpand = TRUE, stack = TRUE)

setColWidths(wb, "Resultados", cols = 1:max(ncol(coef_df), 2), widths = "auto")

saveWorkbook(wb, ruta_salida, overwrite = TRUE)


# ================================================================
# OLS ponderado (log_int_terr)
# Controles: construcción + terreno + servicios (sin 'fachada' y 'patio')
# ------------------------------------------------
# - Pondera con factor_exp_nc_norm (fallback a factor_exp_nc)
# - Recodifica categorías raras
# - Filtra leverage >= 0.99 y re-estima
# - Varianzas robustas HC3 (fallback HC2)
# - Exporta a Excel (coeficientes + pruebas + notas)
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(forcats); library(tidyr)
  library(lmtest); library(sandwich); library(broom)
  library(car); library(tseries); library(openxlsx)
})

# -----------------------
# 0) Definiciones
# -----------------------
dep_var <- "log_int_terr"

# Definir los grupos (excluyendo fachada y patio)
x_construccion <- setdiff(grupos_modelo$construccion, c("fachada", "patio"))
x_terreno      <- grupos_modelo$terreno
x_servicios    <- grupos_modelo$servicios

x_vars <- unique(c(x_construccion, x_terreno, x_servicios))

peso_var <- if ("factor_exp_nc_norm" %in% names(viviendas_df)) "factor_exp_nc_norm" else "factor_exp_nc"

# -----------------------
# 1) Preparar base
# -----------------------
min_count <- 3

work_df <- viviendas_df %>%
  dplyr::select(dplyr::all_of(c(dep_var, x_vars, peso_var))) %>%
  dplyr::filter(is.finite(.data[[peso_var]]), .data[[peso_var]] > 0) %>%
  tidyr::drop_na()

# Recodificar categorías con pocas observaciones
for (v in c("cubierta","color_cubi","muros","ventilaci")) {
  if (v %in% names(work_df) && is.factor(work_df[[v]])) {
    work_df[[v]] <- work_df[[v]] %>%
      forcats::fct_lump_min(min = min_count) %>%
      forcats::fct_drop()
  }
}

cat("[N inicial] =", nrow(work_df), "\n")

# -----------------------
# 2) Estimación base
# -----------------------
fmla <- as.formula(paste0(dep_var, " ~ ", paste(x_vars, collapse = " + ")))
cat("[Fórmula]\n", deparse(fmla), "\n")

m0 <- lm(fmla, data = work_df, weights = work_df[[peso_var]])
cat("\n--- Resumen OLS ponderado (estimación base) ---\n")
print(summary(m0))

# -----------------------
# 3) Filtrar leverage extremos y re-estimar
# -----------------------
lev0   <- hatvalues(m0)
hi_idx <- which(lev0 >= 0.99)

if (length(hi_idx)) {
  cat("\n[Leverage] Se filtran", length(hi_idx), "obs con hat >= 0.99:", paste(hi_idx, collapse = ", "), "\n")
  work_df <- work_df[-hi_idx, , drop = FALSE]
} else {
  cat("\n[Leverage] No se detectaron observaciones con hat >= 0.99\n")
}

m_ols <- lm(fmla, data = work_df, weights = work_df[[peso_var]])
sm    <- summary(m_ols)
cat("\n--- Resumen OLS ponderado (refit sin leverage extremos) ---\n")
print(sm)

# -----------------------
# 4) Varianzas robustas (HC3 -> fallback HC2)
# -----------------------
get_hc <- function(mod, type = "HC3") {
  vc <- try(sandwich::vcovHC(mod, type = type), silent = TRUE)
  if (inherits(vc, "try-error")) return(NULL)
  vc
}

vcov_hc <- get_hc(m_ols, "HC3"); use_type <- "HC3"
if (is.null(vcov_hc) || any(!is.finite(diag(vcov_hc)))) {
  vcov_hc <- get_hc(m_ols, "HC2"); use_type <- "HC2"
}

coeftest_hc <- lmtest::coeftest(m_ols, vcov. = vcov_hc)
cat("\n--- Resultados OLS ponderados robustos (", use_type, ") ---\n", sep = "")
print(coeftest_hc)

# -----------------------
# 5) Tabla ordenada con estrellas (robustas)
# -----------------------
coef_mat <- as.matrix(coeftest_hc)
coef_df <- data.frame(
  Variable = rownames(coef_mat),
  Estimate = as.numeric(coef_mat[,1]),
  StdError = as.numeric(coef_mat[,2]),
  t_value  = as.numeric(coef_mat[,3]),
  p_value  = as.numeric(coef_mat[,4]),
  check.names = FALSE, row.names = NULL
) %>%
  dplyr::mutate(
    Signif = dplyr::case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ ".",
      TRUE ~ ""
    ),
    Estimate = round(Estimate, 4),
    StdError = round(StdError, 4),
    t_value  = round(t_value, 3),
    p_value  = signif(p_value, 3)
  )

cat("\n--- Coeficientes (robustos ", use_type, ") con estrellas ---\n", sep = "")
print(coef_df)

# -----------------------
# 6) Diagnósticos y pruebas
# -----------------------
# VIF
vif_try <- tryCatch(car::vif(m_ols), error = function(e) e)
if (inherits(vif_try, "error")) {
  cat("\n[VIF] No disponible por aliasing de coeficientes.\n")
  vif_max <- NA_real_
} else {
  vif_vals <- vif_try
  vif_max  <- max(vif_vals, na.rm = TRUE)
  cat("\n[VIF max] ", round(vif_max, 3), "\n", sep = "")
}

# Leverage máximo
lev  <- hatvalues(m_ols); lev_max <- max(lev, na.rm = TRUE)

# Pruebas
bp  <- lmtest::bptest(m_ols)
jb  <- tseries::jarque.bera.test(residuals(m_ols))
rst <- lmtest::resettest(m_ols, power = 2:3, type = "fitted")

# Métricas generales
n_obs   <- nobs(m_ols)
r2      <- sm$r.squared
r2_adj  <- sm$adj.r.squared
aic_val <- AIC(m_ols)

stats_df <- tibble::tibble(
  Indicador = c(
    "N observaciones",
    "R2",
    "R2 ajustado",
    "AIC",
    "VIF máximo",
    "Leverage máximo",
    "Breusch-Pagan (p)",
    "Jarque-Bera (p)",
    "RESET (p)",
    "Varianza robusta"
  ),
  Valor = c(
    n_obs,
    round(r2, 4),
    round(r2_adj, 4),
    round(aic_val, 2),
    ifelse(is.na(vif_max), NA, round(vif_max, 3)),
    round(lev_max, 3),
    signif(bp$p.value, 4),
    signif(jb$p.value, 4),
    signif(rst$p.value, 4),
    use_type
  )
)

# -----------------------
# 7) Exportar a Excel (una sola hoja) + Notas metodológicas
# -----------------------

# RUTA FORZADA DE EXPORTACIÓN
ruta_salida <- "C:/Users/ASUS/Documents/Daniel/IGAC investigacion y Prospectiva/Viviendas resilentes/Levantamiento de informacion/CODIGOS FINALES/Integral_de_terreno.xlsx"

# Crear carpeta si no existe
dir.create(dirname(ruta_salida), recursive = TRUE, showWarnings = FALSE)

wb <- createWorkbook()
addWorksheet(wb, "Resultados")

writeData(wb, "Resultados",
          "Resultados OLS ponderado (robusto) - log_int_terr (sin fachada y patio)",
          startRow = 1, startCol = 1)
addStyle(wb, "Resultados",
         createStyle(textDecoration = "bold", fontSize = 12),
         rows = 1, cols = 1, gridExpand = TRUE)

# Coeficientes
writeData(wb, "Resultados", coef_df, startRow = 3, startCol = 1, withFilter = TRUE)

# Bloque inferior de estadísticos
fila_stats <- nrow(coef_df) + 5
writeData(wb, "Resultados", "Estadísticos y pruebas del modelo",
          startRow = fila_stats, startCol = 1)
addStyle(wb, "Resultados", createStyle(textDecoration = "bold"),
         rows = fila_stats, cols = 1, gridExpand = TRUE)
writeData(wb, "Resultados", stats_df, startRow = fila_stats + 2, startCol = 1)

# ----- Notas metodológicas -----
fila_notas <- fila_stats + 2 + nrow(stats_df) + 2

ids_filtrados <- if (exists("hi_idx") && length(hi_idx)) paste(hi_idx, collapse = ", ") else "Ninguno"
n_filtrados   <- if (exists("hi_idx")) length(hi_idx) else 0
n_final       <- n_obs
n_inicial     <- if (exists("lev0")) length(lev0) else NA_integer_

nota_texto <- paste0(
  "Notas metodológicas:\n",
  "- Variable dependiente: ", dep_var, "\n",
  "- Especificación (sin fachada y patio): ", deparse(fmla), "\n",
  "- Ponderación: ", peso_var, " (factor de expansión no calibrado",
  ifelse(peso_var == "factor_exp_nc_norm", " normalizado", ""), ")\n",
  "- Varianza robusta empleada: ", use_type, " (fallback automático en caso de inestabilidad)\n",
  "- Umbral de leverage: hat ≥ 0.99\n",
  "- Observaciones filtradas por leverage: ", n_filtrados,
  ifelse(n_filtrados > 0, paste0(" (IDs: ", ids_filtrados, ")"), ""), "\n",
  "- N inicial: ", n_inicial, " | N final: ", n_final, "\n",
  "- Agrupación de categorías raras (min=", min_count,
  ") aplicada a: cubierta, color_cubi, muros, ventilaci.\n",
  "- Errores estándar robustos (", use_type, ") reportados en la tabla.\n",
  "- Interpretación: coeficientes expresan semi-elasticidades (log puntos)."
)

writeData(wb, "Resultados", nota_texto, startRow = fila_notas, startCol = 1)
addStyle(wb, "Resultados",
         createStyle(textDecoration = "italic", fontSize = 10, halign = "left", valign = "top"),
         rows = fila_notas, cols = 1, gridExpand = TRUE)

# Guardar Excel
tryCatch({
  saveWorkbook(wb, ruta_salida, overwrite = TRUE)
  cat("\n✅ Archivo exportado correctamente a:\n", normalizePath(ruta_salida, winslash="\\"), "\n")
}, error = function(e) {
  stop("\n⚠️ Error al guardar el archivo en la ruta forzada:\n", conditionMessage(e))
})