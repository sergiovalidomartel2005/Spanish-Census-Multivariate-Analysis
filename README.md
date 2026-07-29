# Análisis Multivariante y Clustering Sociodemográfico de Secciones Censales en España

Este repositorio contiene un pipeline analítico completo desarrollado en **R** para el procesamiento, análisis exploratorio (EDA), reducción de dimensionalidad y segmentación (clustering) de datos electorales y demográficos a nivel de sección censal en España (Elecciones Generales 2023).

El objetivo principal de este proyecto es descubrir patrones latentes y relaciones no lineales entre variables socioeconómicas (renta, ocupación, nivel educativo, edad) y el comportamiento electoral, lidiando con un conjunto de datos masivo y con ruido del mundo real.

## 🛠️ Stack Tecnológico y Librerías Principales
* **Manipulación y Limpieza:** `dplyr`, `VIM` (K-Nearest Neighbors Imputation), `editrules` (Data Validation).
* **Análisis Multivariante:** `FactoMineR`, `factoextra`, `cluster`, `psych`, `DescTools`.
* **Visualización y Geoespacial:** `ggplot2`, `sf`, `mapSpain`, `plotly`, `crosstalk`.

## 🧠 Arquitectura del Análisis y Metodología

### 1. Preprocesamiento e Integridad de Datos (Data Pipeline)
* **Ingeniería de Variables:** Transformación de variables crudas en indicadores clave (e.g., agregación de bloques políticos, tasas de abstención y paro).
* **Imputación Estocástica:** Manejo de variables faltantes (NAs) mediante el algoritmo **k-Nearest Neighbors (kNN)**, validando la distribución de los datos imputados vs. originales mediante gráficos de densidad para evitar sesgos.
* **Control de Calidad:** Implementación de matrices lógicas (`editrules`) para garantizar la integridad estructural de las observaciones (ej. $Abstención \le Censo$).

### 2. Análisis Exploratorio y Contrastes de Hipótesis (EDA)
* **Detección de Outliers:** Identificación paramétrica usando el Rango Intercuartílico (IQR).
* **Tests de Normalidad:** Evaluación de distribuciones asimétricas (ej. Renta, Edad) mediante el Test de Lilliefors y gráficos QQ.
* **Correlación y Dependencia:** * Matrices de correlación de Spearman para variables cuantitativas no paramétricas.
  * Contrastes $\chi^2$ y *G-Test* (Cociente de Verosimilitudes) para tablas de contingencia.
  * Medidas de asociación avanzadas: V de Cramer, Goodman-Kruskal Gamma y Kendall Tau-b para medir la fuerza relacional en variables ordinales.

### 3. Reducción de Dimensionalidad
* **PCA (Principal Component Analysis):** Extracción de componentes principales sobre variables cuantitativas escaladas, utilizando los criterios de Kaiser y la varianza explicada acumulada para retener dimensiones. Representación en *biplots* y círculos de correlación.
* **CA & MCA (Multiple Correspondence Analysis):** Análisis sobre el espacio categórico (Comunidad Autónoma, Nivel Educativo, etc.) para identificar agrupaciones estructurales, visualizando la calidad de representación ($Cos^2$) y la contribución inercial de cada variable.

### 4. Segmentación (Clustering No Supervisado)
* **K-Means Clustering:** Búsqueda iterativa de $k$ óptima combinando el Método del Codo (*Elbow Method*) y el Análisis de Silueta (*Silhouette*) sobre una muestra estratificada representativa.
* **Clustering sobre PCA:** Validación de la consistencia y robustez de los clústeres ejecutando K-Means sobre las coordenadas proyectadas del PCA.
* **Clustering Jerárquico (Ward):** Construcción de dendrogramas basados en distancias euclídeas para evaluar la estructura jerárquica de la población española.

## 🚀 Cómo ejecutar el código

1. Clona el repositorio:
   ```bash
   git clone [https://github.com/tu-usuario/Spanish-Census-Multivariate-Analysis.git](https://github.com/tu-usuario/Spanish-Census-Multivariate-Analysis.git)