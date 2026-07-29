library(ggplot2)
library(ggpubr)
library(dplyr)
library(mapSpain)
library(sf)
library(ggcorrplot)
library(editrules)
library(VIM)
library(plotly)
library(stringr)
library(crosstalk)
library(patchwork)
library(scales)
library(nortest)
library(psych)
library(e1071)
library(factoextra)
library(cluster)
library(DescTools)
library(vcd)
library(FactoMineR)
library(corrplot)
library(gplots)

elecciones_final <- readRDS("../data/elecciones_final_limpio.rds")
# VALIDACIÓN DE LA IMPUTACIÓN kNN 

# 1. Obtenemos los nombres de todas las variables que son numéricas
vars_num <- names(elecciones)[sapply(elecciones, is.numeric)]

# 2. Creamos una tabla comparativa 
tabla_validacion <- data.frame(
  Variable = vars_num,
  Media_Original = sapply(elecciones[vars_num], mean, na.rm = TRUE),
  Media_Imputada = sapply(elecciones_final[vars_num], mean),
  SD_Original = sapply(elecciones[vars_num], sd, na.rm = TRUE),
  SD_Imputada = sapply(elecciones_final[vars_num], sd)
)

# 3. Calculamos la diferencia absoluta entre las medias 
tabla_validacion$Diferencia_Media <- abs(tabla_validacion$Media_Original - tabla_validacion$Media_Imputada)

# Redondeamos las columnas numéricas 
tabla_validacion[, -1] <- round(tabla_validacion[, -1], 3)


rownames(tabla_validacion) <- NULL
print(tabla_validacion)


# GRÁFICOS DE DENSIDAD: VALIDACIÓN DE LAS VARIABLES MÁS AFECTADAS

par(mfrow = c(1, 2))

# Gráfico 1: ocupados_total 
plot(density(elecciones_final$ocupados_total), 
     main = "Validación: ocupados_total", 
     col = "red", lwd = 2, lty = 2, 
     xlab = "Número de Ocupados", ylab = "Densidad")


lines(density(elecciones$ocupados_total, na.rm = TRUE), col = "blue", lwd = 2)


legend("topright", legend = c("Original (sin NAs)", "Imputada (kNN)"), 
       col = c("blue", "red"), lty = c(1, 2), lwd = 2, cex = 0.8)


# Gráfico 2: pob_total_est 
plot(density(elecciones_final$pob_total_est), 
     main = "Validación: pob_total_est", 
     col = "red", lwd = 2, lty = 2, 
     xlab = "Población Total Estatal", ylab = "Densidad")


lines(density(elecciones$pob_total_est, na.rm = TRUE), col = "blue", lwd = 2)


legend("topright", legend = c("Original (sin NAs)", "Imputada (kNN)"), 
       col = c("blue", "red"), lty = c(1, 2), lwd = 2, cex = 0.8)

par(mfrow = c(1, 1))


# Ponemos reglas para ver si hay valores incoherentes
reglas <- editmatrix(
  c(
    "renta >= 0",
    "edad >= 0",
    "edad <= 120",
    "censo >= 0",
    "parados >= 0",
    "ocupados_total >= 0",
    "pob_total_est >= 0",
    "abstencion_23 >= 0",
    "derecha_23 >= 0",
    "izquierda_23 >= 0",
    "resto_23 >= 0",
    "abstencion_23 <= censo"
  )
)

# Comprobamos las reglas sobre el dataset ya imputado
# para verificar que la imputación no ha generado valores incoherentes
comprobacion <- violatedEdits(reglas, elecciones_final)
summary(comprobacion)
}

#Caluclo de métricas descriptivas
vars_num <- elecciones_final[, sapply(elecciones_final, is.numeric)]

descriptivos_num <- psych::describe(vars_num)[, c("n", "mean", "sd", "median",
                                                  "min", "max", "skew", "kurtosis")]
descriptivos_num <- round(descriptivos_num, 2)
print(descriptivos_num)


#exploracion categoriar ordinales
vars_cual <- c("nivel_poblacion_mun",
               "nivel_mun", "nivel_comun_edu", "nivel_comun_ocupados")
vars_cual <- intersect(vars_cual, names(elecciones_final))

for (v in vars_cual) {
  tabla_abs <- table(elecciones_final[[v]], useNA = "ifany")
  tabla_rel <- round(prop.table(tabla_abs) * 100, 2)
  print(rbind(Absoluta = tabla_abs, Relativa_pct = tabla_rel))
}

# Estudio Outliers
{
  elecciones_cuant <- elecciones_final[, sapply(elecciones_final, is.numeric)]
  
  evaluar_outliers <- function(x) {
    if (is.numeric(x)) {
      Q1 <- quantile(x, 0.25, na.rm = TRUE)
      Q3 <- quantile(x, 0.75, na.rm = TRUE)
      IQR_val <- Q3 - Q1
      limite_inf <- Q1 - 1.5 * IQR_val
      limite_sup <- Q3 + 1.5 * IQR_val
      num_outliers <- sum(x < limite_inf |
                            x > limite_sup, na.rm = TRUE)
      porcentaje <- round((num_outliers / length(na.omit(x))) * 100, 2)
      return(c(Cantidad = num_outliers, Porcentaje_pct = porcentaje))
    }
  }
  
  tabla_outliers <- as.data.frame(t(sapply(elecciones_cuant, evaluar_outliers)))
  tabla_outliers <- tabla_outliers[order(-tabla_outliers$Cantidad), ]
  print(tabla_outliers)
}

# Cálculo correlaciones
{
  elecciones_cuant <- elecciones_final[, sapply(elecciones_final, is.numeric)]
  matriz_cor_general <- cor(elecciones_cuant, method = "spearman")
  
  matriz_cor_general_redondeada <- round(matriz_cor_general, 2)
  
  print(matriz_cor_general_redondeada)
  
  matriz_pvalores <- cor_pmat(elecciones_cuant, method = "spearman")
  
  ggcorrplot(
    matriz_cor_general,
    hc.order = TRUE,
    type = "lower",
    lab = TRUE,
    lab_size = 2.5,
    colors = c("red", "white", "blue"),
    title = "Matriz de Correlación de Spearman",
    ggtheme = theme_minimal(),
    p.mat = matriz_pvalores,
    sig.level = 0.05,
    insig = "blank"
  ) +
    theme(axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1
    ))
}

#Calculamos la correlacion(dependencia con la prueba del chi cuadrado)
tabla_cruce <- table(elecciones_final$nivel_mun,
                     elecciones_final$nivel_comun_edu)
test_chi <- chisq.test(tabla_cruce)
print(test_chi)

# Estudio normalidad algunas variables
{
  #Histograma renta
  hist_renta <- gghistogram(
    elecciones_final$renta,
    fill = "lightblue",
    color = "darkblue",
    title = "Distribución de la Renta",
    xlab = "Renta Media (€)",
    ylab = "Frecuencia"
  )
  
  # El QQ-Plot renta
  qq_renta <- ggqqplot(
    elecciones_final$renta,
    title = "QQ-Plot de la Renta",
    ylab = "Muestra",
    xlab = "Teórico"
  )
  
  # Juntamos los dos gráficos en uno solo
  ggarrange(hist_renta, qq_renta, ncol = 2, nrow = 1)
  
  test_renta <- lillie.test(elecciones_final$renta)
  print(test_renta)
  #Concluimos q la renta no sigue una distribucion normal
}

# Análisis de la variable EDAD
{
  hist_edad <- gghistogram(
    elecciones_final$edad,
    fill = "lightgreen",
    color = "darkgreen",
    title = "Distribución de la Edad",
    xlab = "Edad Media",
    ylab = "Frecuencia"
  )
  
  qq_edad <- ggqqplot(
    elecciones_final$edad,
    title = "QQ-Plot de la Edad",
    ylab = "Muestra",
    xlab = "Teórico"
  )
  
  ggarrange(hist_edad, qq_edad, ncol = 2, nrow = 1)
  
  # Test de Lilliefors sobre la edad (corregimos el nombre, antes reusaba test_renta por copia-pega)
  test_edad <- lillie.test(elecciones_final$edad)
  print(test_edad)
  #Concluimos q tampoco es normal pq hay una cantidad desproporcionada de secciones censales con una edad media altísima
}

# Calculamos algunas tasas
{
  elecciones_final$tasa_paro <- (
    elecciones_final$parados / (elecciones_final$ocupados_total + elecciones_final$parados)
  ) * 100
  elecciones_final$tasa_abs <- (elecciones_final$abstencion_23 / elecciones_final$censo) * 100
}

# Grafico con renta media por tipo de municipio
{
  # Primero calculamos la media exacta para cada grupo
  renta_por_municipio <- elecciones_final %>%
    filter(!is.na(nivel_mun)) %>%
    group_by(nivel_mun) %>%
    summarise(renta_media = weighted.mean(renta, w = censo, na.rm = TRUE))
  
  # Gráfico 1
  ggplot(renta_por_municipio,
         aes(x = nivel_mun, y = renta_media, fill = nivel_mun)) +
    geom_col(color = "black", alpha = 0.8)  +
    theme_minimal() + labs(
      title = "Renta Media según el Tipo de Municipio",
      subtitle = "",
      x = "",
      y = "Renta Media (€)"
    )
}

# Dispersión vs abstención en regresión lineal
{
  # 1. Calculamos el modelo y extraemos la R2 para que sea exacta
  modelo <- lm(tasa_abs ~ tasa_paro, data = elecciones_final)
  summary(modelo)
  valor_r2 <- round(summary(modelo)$r.squared, 3)
  
  # 2. Generamos el gráfico con las mejoras
  ggplot(elecciones_final, aes(x = tasa_paro, y = tasa_abs)) +
    geom_point(color = "midnightblue", alpha = 0.01) + 
    geom_smooth(method = "lm", color = "darkorange") +
    theme_minimal() +
    labs(
      title = "Efecto del desempleo sbre la participación electoral",
      subtitle = paste("Relación Lineal | R² =", valor_r2),
      x = "Tasa de Paro (%)",
      y = "Tasa de Abstención (%)"
    )
}

# Mapa interactivo
{
  abst_prov <- aggregate(cbind(abstencion_23, censo) ~ cod_prov,
                         data = elecciones_final,
                         FUN = sum)
  abst_prov$tasa <- (abst_prov$abstencion_23 / abst_prov$censo) * 100
  
  abst_prov$cod_prov <- str_pad(abst_prov$cod_prov, width = 2, pad = "0")
  
  mapa_sf <- esp_get_prov() %>% esp_move_can()
  mapa_listo <- merge(mapa_sf, abst_prov, by.x = "cpro", by.y = "cod_prov")
  
  mapa_base <- ggplot(mapa_listo) +
    geom_sf(aes(
      fill = tasa,
      text = paste(ine.prov.name, "| Abstención:", round(tasa, 1), "%")
    ),
    color = "white",
    linewidth = 0.2) +
    scale_fill_distiller(palette = "Blues",
                         direction = 1,
                         name = "% Abst.") +
    theme_void() +
    labs(title = "Abstención Electoral por Provincias (2023)")
  
  mapa_interactivo <- ggplotly(mapa_base, tooltip = "text") %>%
    layout(hovermode = "closest",
           hoverlabel = list(bgcolor = "white", font = list(size = 12))) %>%
    style(hoveron = "fills")
  
  mapa_interactivo
}

# 1. Creación de Tablas de Contingencia
{
  # Las tablas de contingencia resumen la relación entre varias variables categóricas
  
  # Tabla 1; Nominal vs Nominal; Comunidad Autónoma vs Provincia
  tabla_geo <- table(elecciones_final$nom_ccaa, elecciones_final$nom_prov)
  tabla_geo
  
  # Tabla 2; Nominal vs Ordinal; Provincia vs Nivel Educativo
  tabla_prov_edu <- table(elecciones_final$nom_prov, elecciones_final$nivel_comun_edu)
  tabla_prov_edu
  
  # Tabla 3; Ordinal vs Ordinal; Tipo de Municipio vs Nivel Educativo
  tabla_mun_edu <- table(elecciones_final$nivel_mun, elecciones_final$nivel_comun_edu)
  tabla_mun_edu
  
  # Tabla 4; Ordinal vs Ordinal Educación vs Ocupación
  tabla_edu_ocup <- table(elecciones_final$nivel_comun_edu, elecciones_final$nivel_comun_ocupados)
  tabla_edu_ocup
}

# 2. Medidas de Independencia
{
  # Test chi-cudriado y G-test para los pares de variables anteriores
  
  print("Contrastes Chi-cuadrado")
  
  print(chisq.test(tabla_geo, correct = FALSE))
  print(chisq.test(tabla_prov_edu, correct = FALSE))
  print(chisq.test(tabla_mun_edu, correct = FALSE))
  print(chisq.test(tabla_edu_ocup, correct = FALSE))
  
  #Test G para las variables anteriores
  print(" Contrastes G-test (Test del Cociente de Verosimilitudes) ")
  
  print(GTest(tabla_prov_edu))
  print(GTest(tabla_mun_edu))
  print(GTest(tabla_geo))
  print(GTest(tabla_edu_ocup))
}
#En todos los test salen p-valores muy pequeños por lo que se rechaza independencia

# 3. Medidas de Asociación: Escala Nominal
{
  # Para estudiar el grado de relación existente eliminando el efecto del tamaño muestral
  # Las aplicamos a las tablas donde al menos una variable es nominal
  
  print("Medidas Nominales: CCAA vs Provincia")
  print(paste("C de Pearson:", round(ContCoef(tabla_geo), 3)))
  print(paste("V de Cramer:", round(CramerV(tabla_geo), 3)))
  #Muy correlacionadas
  
  print("Medidas Nominales: Provincia vs Nivel Educativo")
  print(paste("C de Pearson:", round(ContCoef(tabla_prov_edu), 3)))
  print(paste("V de Cramer:", round(CramerV(tabla_prov_edu), 3)))
  #Relacion media baja
  
  # Con esta función, hacemos todo junto
  print(assocstats(tabla_prov_edu))
  print(assocstats(tabla_geo))
}

# 4. Medidas de Asociación: Escala Ordinal
{
  print("Medidas Ordinales: Tipo de Municipio vs Nivel Educativo")
  print(paste("Gamma:", round(GoodmanKruskalGamma(tabla_mun_edu)[1], 3)))
  print(paste("Somers D (C|R):", round(SomersDelta(tabla_mun_edu, direction="column")[1], 3)))
  print(paste("Kendall Tau-b:", round(KendallTauB(tabla_mun_edu)[1], 3)))
  print(paste("Kendall Tau-c:", round(StuartTauC(tabla_mun_edu)[1], 3)))
  #Relacion media
  
  print("Medidas Ordinales: Nivel Educativo vs Nivel de Ocupación")
  print(paste("Gamma:", round(GoodmanKruskalGamma(tabla_edu_ocup)[1], 3)))
  print(paste("Somers D (C|R):", round(SomersDelta(tabla_edu_ocup, direction="column")[1], 3)))
  print(paste("Kendall Tau-b:", round(KendallTauB(tabla_edu_ocup)[1], 3)))
  print(paste("Kendall Tau-c:", round(StuartTauC(tabla_edu_ocup)[1], 3)))
  #Relacion media alta
}