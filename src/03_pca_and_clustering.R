
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
# 1. Preparación de los datos
{
  # Seleccionamos estrictamente las variables numéricas de nuestro dataset imputado.
  elecciones_cuant <- elecciones_final[, sapply(elecciones_final, is.numeric)]
  
  # Ejecutamos el Análisis de Componentes Principales.
  pca_result <- prcomp(elecciones_cuant, scale = TRUE)
  pca_result
  print(get_eigenvalue(pca_result))
}

# 2. Elección del número de componentes principales
{
  # Calculamos los autovalores y la proporción de varianza explicada.
  pr.var <- pca_result$sdev^2
  pve <- pr.var / sum(pr.var)
  
  # CRITERIO 1: Criterio de Kaiser
  # Nos quedamos con las componentes cuyo autovalor sea > 1.
  print(" Autovalores (Varianzas) ")
  print(round(pr.var, 4))
  print(paste("Componentes con autovalor > 1 (Kaiser):", sum(pr.var > 1)))
  #Hay 3 autovalores menor que 1
  
  # CRITERIO 2: Varianza acumulada
  componentes_75_80 <- which(cumsum(pve) >= 0.75)[1]
  print(paste("Número de componentes para alcanzar al menos el 75% de varianza:", componentes_75_80))
  #Nos salen tres dimensiones a escoger
}

# 3. Representación Gráfica
{
  # Configuramos la ventana para ver los dos gráficos base.
  par(mfrow = c(1, 2))
  
  # Gráfico de la varianza explicada.
  plot(pve, xlab = "Componente Principal",
       ylab = "Proporción de Varianza Explicada",
       ylim = c(0, 1), type = "b", col = "blue", pch = 19)
  
  # Gráfico de la varianza acumulada.
  plot(cumsum(pve), xlab = "Componente Principal",
       ylab = "PVE Acumulado",
       ylim = c(0, 1), type = "b", col = "red", pch = 19)
  
  par(mfrow = c(1, 1))
  
  # CRITERIO 3: Gráfica de sedimentación
  # Buscamos el "codo" o cambio de tendencia donde los autovalores se estabilizan.
  grafico_sedimentacion <- fviz_eig(pca_result, addlabels = TRUE,
                                    ylim = c(0, 50),
                                    main = "Gráfica de Sedimentación (Scree Plot)")
  print(grafico_sedimentacion)
}

# 4. Biplot y Contribución de Variables (Solo Variables, sin observaciones)
{
  # 4 Biplot en el que solo se ven las variables
  biplot_mejorado <- fviz_pca_var(pca_result,
                                  col.var = "contrib",
                                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                  repel = TRUE,
                                  title = "Círculo de Correlaciones y Contribución de Variables")
  print(biplot_mejorado)
  
  # Visualización de la contribución exacta de cada variable
  # Muestra en un gráfico de barras qué variables aportan más a las dos primeras componentes
  grafico_contribucion <- fviz_contrib(pca_result, choice = "var", axes = 1:3, top = 16) +
    labs(title = "Contribución de variables a las 3 primeras componentes",
         x = "Dimensiones",
         y = "Porcentaje de varianza explicada")
  print(grafico_contribucion)
}

# 5. Loadings de las 3 primeras componentes principales
{
  loadings_pca <- round(pca_result$rotation[, 1:3], 3)
  loadings_pca <- loadings_pca[order(-abs(loadings_pca[, 1])), ]
  print("Loadings de PC1, PC2, PC3 ")
  print(loadings_pca)
}

# ANÁLISIS DE CORRESPONDENCIAS SIMPLE
{
  # Vamos a analizar la relación entre la Comunidad Autónoma y el Nivel de Ocupación.
  # Primero creamos la tabla de contingencia.
  tabla_ca <- table(elecciones_final$nom_ccaa, elecciones_final$nivel_comun_ocupados)
  
  # Hacemos el CA
  res.ca <- CA(tabla_ca, graph = FALSE)
  print(res.ca)
  
  # Valores propios y Varianza Explicada
  print("Autovalores y Varianza Explicada (CA) ")
  print(get_eigenvalue(res.ca))
  
  # Scree plot para ver cuántas dimensiones nos quedamos
  grafico_scree_ca <- fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 100),
                                     main = "Scree Plot - Correspondencias Simple") +
    labs(x = "Dimensiones",
         y = "Porcentaje de varianza explicada")
  print(grafico_scree_ca)
  
  # Calculamos coord, cos2, contrib
  filas_ca <- get_ca_row(res.ca)
  
  print("Contribuciones ordenadas por Dim1 ")
  contrib_ordenado <- filas_ca$contrib[order(-filas_ca$contrib[,1]), ]
  print(round(contrib_ordenado, 3))
  
  print("Cos2 ordenado por Dim1")
  cos2_ordenado <- filas_ca$cos2[order(-filas_ca$contrib[,1]), ]
  print(round(cos2_ordenado, 3))
  
  print("Coordenadas ordenadas por Dim1")
  coord_ordenado <- filas_ca$coord[order(-filas_ca$contrib[,1]), ]
  print(round(coord_ordenado, 3))
  
  #Gráfico cos2 repecto las comunidades
  corrplot(filas_ca$cos2, is.corr = FALSE, main = "Calidad de representación (Cos2) - CCAA")
  
  # 1.4 Biplot
  biplot_asim <- fviz_ca_biplot(res.ca, map = "colgreen", arrow = c(TRUE, FALSE),
                                repel = TRUE, title = "Biplot de Contribución (Asimétrico)")
  print(biplot_asim)
}

# PARTE 2: ANÁLISIS DE CORRESPONDENCIAS MÚLTIPLE (MCA)
{
  # Evitamos nom_mun porque tiene 8000 categorías y rompería la visualización.
  elecciones_cat <- elecciones_final[, c("nom_ccaa", "nivel_mun",
                                         "nivel_comun_edu", "nivel_comun_ocupados")]
  
  # Ejecutamos el MCA
  res.mca <- MCA(elecciones_cat, graph = FALSE)
  print(res.mca)
  
  # 2.1 Valores propios y Scree plot del MCA
  print("Autovalores y Varianza Explicada (MCA)")
  eigenvalues_mca <- get_eigenvalue(res.mca)
  print(eigenvalues_mca)
  
  # Calculamos las coord, cos2, y contrib
  var_mca <- get_mca_var(res.mca)
  
  print("Coordenadas de las categorías de las variables")
  print(round(var_mca$coord[, 1:4], 3))
  
  print("Calidad de representación (Cos2)")
  cos2_dim14 <- var_mca$cos2[, 1:4]
  cos2_dim14 <- cos2_dim14[order(-rowSums(cos2_dim14)), ]
  print(round(cos2_dim14, 3))
  
  print("Contribución a las dimensiones (%)")
  contrib_dim14 <- var_mca$contrib[, 1:4]
  contrib_dim14 <- contrib_dim14[order(-contrib_dim14[, 1]), ]
  print(round(contrib_dim14, 3))
  
  # Elección del número de dimensiones
  # En MCA la varianza explicada por dimensión es estructuralmente baja
  # por lo que no se aplica el criterio del 75-80% usado en PCA.
  # En su lugar combinamos tres criterios:
  
  # Criterio 1: Kaiser adaptado al MCA
  # Retenemos dimensiones cuyo autovalor supere 1/número de variables activas
  n_vars_activas <- 4
  umbral_kaiser_mca <- 1 / n_vars_activas
  dims_kaiser <- sum(eigenvalues_mca[, 1] > umbral_kaiser_mca)
  cat("Dimensiones con autovalor > 1/p (criterio Kaiser-MCA):", dims_kaiser, "\n")
  
  # Criterio 2: Scree plot con umbral de Kaiser marcado
  # Buscamos el codo donde la curva se estabiliza
  grafico_scree_mca <- fviz_screeplot(res.mca, addlabels = TRUE,
                                      main = "Scree Plot - Correspondencias Múltiple") +
    geom_hline(yintercept = umbral_kaiser_mca * 100,
               linetype = "dashed", color = "red") +
    annotate("text", x = 5, y = (1 / n_vars_activas) * 100 + 1,
             label = "Umbral Kaiser-MCA (1/p)", color = "red", size = 3) +
    labs(x = "Dimensiones", y = "Porcentaje de varianza explicada")
  print(grafico_scree_mca)
  
  # Criterio 3: Interpretabilidad y cos2
  # Nos quedamos con 4 dimensiones
  n_dims_mca <- 4
  cat("Varianza acumulada retenida con", n_dims_mca, "dimensiones:",
      round(eigenvalues_mca[n_dims_mca, 3], 1), "%\n")
  
  # Biplot Dim 1 y 2
  grafico_contrib_mca_12 <- fviz_mca_var(res.mca,
                                         axes = c(1, 2),
                                         col.var = "contrib",
                                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                         repel = TRUE,
                                         ggtheme = theme_minimal(),
                                         title = "MCA: Categorías coloreadas por su contribución (Dim1 vs Dim2)")
  print(grafico_contrib_mca_12)
  
  # Biplot Dim 3 y 4
  grafico_contrib_mca_34 <- fviz_mca_var(res.mca,
                                         axes = c(3, 4),
                                         col.var = "contrib",
                                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                         repel = TRUE,
                                         ggtheme = theme_minimal(),
                                         title = "MCA: Categorías coloreadas por su contribución (Dim3 vs Dim4)")
  print(grafico_contrib_mca_34)
  
  # Ver las variables que más contribuyen gráficamente a las 4 dimensiones
  contrib_dim1_4 <- fviz_contrib(res.mca, choice = "var", axes = 1:4, top = 15,
                                 title = "Top 15 categorías que más aportan a Dim 1, 2, 3 y 4")
  print(contrib_dim1_4)
}

# 1. Preparación de los datos para el clúster
{
  # Seleccionamos estrictamente las variables cuantitativas
  elecciones_cuant <- elecciones_final[, sapply(elecciones_final, is.numeric)]
  
  # Filtramos variables con varianza cero
  varianzas <- apply(elecciones_cuant, 2, var, na.rm = TRUE)
  elecciones_cuant <- elecciones_cuant[, varianzas > 0]
  
  # Escalamos los datos (media 0, varianza 1)
  datos_escalados <- scale(elecciones_cuant)
}

# 2. Búsqueda del número óptimo de clusters (K adecuada)
{
  # Muestreo estratificado por CCAA para garantizar representación proporcional
  set.seed(123)
  n_muestra <- 5000
  indices_muestra <- as.integer(
    unlist(tapply(seq_len(nrow(datos_escalados)), elecciones_final$nom_ccaa, function(idx) {
      n <- round(length(idx) / nrow(datos_escalados) * n_muestra)
      if (n < 1) n <- 1
      sample(idx, min(n, length(idx)))
    }))
  )
  muestra_codo <- datos_escalados[indices_muestra, ]
  
  # Método del Codo(para ver K optima)
  grafico_codo <- fviz_nbclust(muestra_codo, kmeans, method = "wss") +
    labs(title = "Número óptimo de clusters - Método del Codo",
         x = "Número de clusters k",
         y = "Suma total de cuadrados intra-cluster")
  print(grafico_codo)
  
  # Método de la SILUETA segundo criterio de validación.
  grafico_silueta <- fviz_nbclust(muestra_codo, kmeans, method = "silhouette") +
    labs(title = "Número óptimo de clusters - Método de la Silueta",
         x = "Número de clusters k",
         y = "Anchura media de la silueta")
  print(grafico_silueta)
  
  k_optima <- 2
}

# 3. Análisis Clúster No Jerárquico (K-Means) k = 2
{
  km_result <- kmeans(datos_escalados, centers = k_optima, nstart = 25)
  
  # Visualizamos los clusters en 2D SOLO con la muestra estratificada para no colapsar R
  grafico_kmeans <- fviz_cluster(list(data = muestra_codo, cluster = km_result$cluster[indices_muestra]),
                                 geom = "point",
                                 ellipse.type = "convex",
                                 main = paste("Clúster K-Means (K =", k_optima, ") - Muestra estratificada 5000"))
  print(grafico_kmeans)
  
  # Vemos los tamaños de cada clúster
  print(km_result$size)
  
  #Vemos los centroides de cada cluster
  print(round(km_result$centers, 2))
  
  # Pasamos los centroides a la escala original
  perfil_clusters <- aggregate(elecciones_cuant,
                               by = list(Cluster = km_result$cluster),
                               FUN = mean, na.rm = TRUE)
  print(round(perfil_clusters, 2))
}

# 4. Análisis Clúster No Jerárquico (K-Means) k = 3
{
  km_result <- kmeans(datos_escalados, centers = 3, nstart = 25)
  
  # Visualizamos los clusters en 2D SOLO con la muestra estratificada para no colapsar R
  grafico_kmeans <- fviz_cluster(list(data = muestra_codo, cluster = km_result$cluster[indices_muestra]),
                                 geom = "point",
                                 ellipse.type = "convex",
                                 main = "Clúster K-Means (K = 3) - Muestra estratificada 5000")
  print(grafico_kmeans)
  
  # Vemos los tamaños de cada clúster
  print(km_result$size)
  
  #Vemos los centroides de cada cluster
  print(round(km_result$centers, 2))
  
  # Pasamos los centroides a la escala original
  perfil_clusters <- aggregate(elecciones_cuant,
                               by = list(Cluster = km_result$cluster),
                               FUN = mean, na.rm = TRUE)
  print("Perfil medio de cada cluster (escala original)")
  print(round(perfil_clusters, 2))
}

# 5. K-means sobre las componentes principales del PCA
{
  # Hacemos los clústers sobre las componentes principales de PCA
  n_comp <- max(componentes_75_80, 3)
  scores_pca <- pca_result$x[, 1:n_comp]
  
  km_pca <- kmeans(scores_pca, centers = 3, nstart = 25)
  
  cat(" Tamaños de cluster (k-means sobre 3 componentes del PCA)")
  print(km_pca$size)
  
  grafico_kmeans_pca <- fviz_cluster(list(data = scores_pca[indices_muestra, ],
                                          cluster = km_pca$cluster[indices_muestra]),
                                     geom = "point",
                                     ellipse.type = "convex",
                                     main = "K-Means sobre componentes del PCA (K = 3)")
  print(grafico_kmeans_pca)
  
  # Comparamos las dos asignaciones (variables originales vs PCA)
  print("Concordancia kmeans original vs PCA (absoluta)")
  print(table(Original = km_result$cluster, PCA = km_pca$cluster))
  
  print("Concordancia kmeans original vs PCA (%)")
  print(round(prop.table(table(Original = km_result$cluster,
                               PCA = km_pca$cluster), 1) * 100, 1))
  
  print("Tamaños kmeans original")
  print(km_result$size)
  
  print("Tamaños kmeans PCA")
  print(km_pca$size)
}

# 6. Interpretación de clusters
{
  print("Centroides estandarizados")
  print(round(km_result$centers, 3))
  
  print("Peso de cada variable en cada cluster (%)")
  contrib <- apply(abs(km_result$centers), 1, function(x) round(x / sum(x) * 100, 2))
  print(t(contrib))
  
  print("Top 5 variables mas influyentes por cluster")
  for (i in 1:3) {
    cat("\nCluster", i, ":\n")
    vars_ordenadas <- sort(abs(km_result$centers[i, ]), decreasing = TRUE)
    print(round(vars_ordenadas[1:5], 3))
  }
  
  # CCAA mas representativas por cluster (solo las que superan el 5%)
  print("CCAA mas representativas por cluster (>5%)")
  tabla_ccaa <- round(prop.table(table(km_result$cluster,
                                       elecciones_final$nom_ccaa), 1) * 100, 1)
  for (i in rownames(tabla_ccaa)) {
    cat("\nCluster", i, ":\n")
    fila <- tabla_ccaa[i, ]
    print(sort(fila[fila >= 5], decreasing = TRUE))
  }
  
  # Tipo de municipio por cluster
  print("Tipo de municipio por cluster (%)")
  print(round(prop.table(table(km_result$cluster,
                               elecciones_final$nivel_mun), 1) * 100, 1))
  
  # Ocupacion por cluster
  print("Ocupacion por cluster (%)")
  print(round(prop.table(table(km_result$cluster,
                               elecciones_final$nivel_comun_ocupados), 1) * 100, 1))
  
  # Edad media por cluster
  print("Edad media por cluster")
  print(round(tapply(elecciones_final$edad,
                     km_result$cluster, mean), 1))
  
  # Etiqueta para los graficos
  elecciones_final$cluster_label <- factor(km_result$cluster,
                                           labels = c("C1", "C2", "C3"))
  
  # Grafico tipo de municipio
  datos_mun <- as.data.frame(
    round(prop.table(table(Cluster = km_result$cluster,
                           Municipio = elecciones_final$nivel_mun), 1) * 100, 1)
  )
  datos_mun$Cluster <- factor(datos_mun$Cluster,
                              labels = c("C1", "C2", "C3"))
  
  p_mun <- ggplot(datos_mun, aes(x = Municipio, y = Freq, fill = Cluster)) +
    geom_col(position = "dodge", color = "white") +
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
    theme_minimal() +
    labs(title = "Tipo de municipio por cluster",
         x = "", y = "% dentro del cluster", fill = "Cluster") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  print(p_mun)
  
  # Grafico nivel educativo
  datos_edu <- as.data.frame(
    round(prop.table(table(Cluster = km_result$cluster,
                           Educacion = elecciones_final$nivel_comun_edu), 1) * 100, 1)
  )
  datos_edu$Cluster <- factor(datos_edu$Cluster,
                              labels = c("C1", "C2", "C3"))
  
  p_edu <- ggplot(datos_edu, aes(x = Educacion, y = Freq, fill = Cluster)) +
    geom_col(position = "dodge", color = "white") +
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
    theme_minimal() +
    labs(title = "Nivel educativo por cluster",
         x = "", y = "% dentro del cluster", fill = "Cluster") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  print(p_edu)
  
  # Boxplots
  p_renta <- ggplot(elecciones_final,
                    aes(x = cluster_label, y = renta, fill = cluster_label)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
    theme_minimal() +
    labs(title = "Renta por cluster", x = "", y = "Renta media (€)") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 15, hjust = 1))
  
  p_paro <- ggplot(elecciones_final,
                   aes(x = cluster_label, y = tasa_paro, fill = cluster_label)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
    theme_minimal() +
    labs(title = "Tasa de paro por cluster", x = "", y = "Tasa de paro (%)") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 15, hjust = 1))
  
  p_abs <- ggplot(elecciones_final,
                  aes(x = cluster_label, y = tasa_abs, fill = cluster_label)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
    theme_minimal() +
    labs(title = "Abstencion por cluster", x = "", y = "Tasa de abstencion (%)") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 15, hjust = 1))
  
  p_edad <- ggplot(elecciones_final,
                   aes(x = cluster_label, y = edad, fill = cluster_label)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
    theme_minimal() +
    labs(title = "Edad por cluster", x = "", y = "Edad media") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 15, hjust = 1))
  
  print((p_renta + p_paro) / (p_abs + p_edad))
}

# 7. Análisis Clúster Jerárquico - EXPLORANDO VARIAS 'K'
{
  # Muestreo estratificado por CCAA de 1000 observaciones
  set.seed(123)
  n_jerarquico <- 1000
  indices_jerarquico <- as.integer(
    unlist(tapply(seq_len(nrow(datos_escalados)), elecciones_final$nom_ccaa, function(idx) {
      n <- round(length(idx) / nrow(datos_escalados) * n_jerarquico)
      if (n < 1) n <- 1
      sample(idx, min(n, length(idx)))
    }))
  )
  muestra_jerarquico <- datos_escalados[indices_jerarquico, ]
  
  # Matriz distancias euclídeas
  matriz_distancias <- dist(muestra_jerarquico, method = "euclidean")
  
  # Clustering jerárquico con método Ward
  hc_result <- hclust(matriz_distancias, method = "ward.D2")
  
  # CASO 1: EL ÓPTIMO, K = 2
  grupos_k2 <- cutree(hc_result, k = 2)
  print(table(grupos_k2))
  centroides_k2 <- aggregate(muestra_jerarquico, by = list(Cluster = grupos_k2), FUN = mean)
  print(round(centroides_k2, 2))
  
  # CASO 2: K = 3
  grupos_k3 <- cutree(hc_result, k = 3)
  print(table(grupos_k3))
  centroides_k3 <- aggregate(muestra_jerarquico, by = list(Cluster = grupos_k3), FUN = mean)
  print(round(centroides_k3, 2))
  
  # CASO 3: K = 4
  grupos_k4 <- cutree(hc_result, k = 4)
  print(table(grupos_k4))
  centroides_k4 <- aggregate(muestra_jerarquico, by = list(Cluster = grupos_k4), FUN = mean)
  print(round(centroides_k4, 2))
  
  # VISUALIZACIÓN DEL DENDROGRAMA
  k_dendro <- 4
  dendrograma <- fviz_dend(hc_result, k = k_dendro,
                           cex = 0.3,
                           show_labels = FALSE,
                           color_labels_by_k = TRUE,
                           rect = TRUE,
                           main = paste("Clúster Jerárquico (Coloreado para K =", k_dendro, ")"))
  print(dendrograma)
}






