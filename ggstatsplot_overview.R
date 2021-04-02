library(pacman)

pacman::p_load(dplyr, tidyverse, rstatix, foreign, ggstatsplot, tidyr, readr, 
               tidytidbits)

data1 = foreign::read.spss("database_idc.sav", use.value.labels = TRUE, 
                           to.data.frame = TRUE)
glimpse(data1)

# Ejemplo 1: Comando ggbetweenstats (estadística paramétrica)

data1 %>%
  count(Nivel_educativo)

# Balancear los datos

set.seed(1)
data2 = sample_n_by(data1, Nivel_educativo, size = 13, replace = F)

data2 %>%
  count(Nivel_educativo)

data2 %>%
  group_by(Nivel_educativo) %>%
    shapiro_test(IDC)

data2 %>%
  levene_test(IDC ~ Nivel_educativo)

data2 %>%
  anova_test(IDC ~ Nivel_educativo, type = 1)

# Test de Welch

data2 %>%
  welch_anova_test(IDC ~ Nivel_educativo)

# Usando ggstatsplot

data2 %>%
  ggstatsplot::ggbetweenstats(
    x = Nivel_educativo,
    y = IDC,
    type = "p",
    xlab = "Nivel educativo",
    ylab = "Índice de consumo de frutas y verduras"
    )

data3 = data1 %>%
  arrange(Nivel_educativo) %>%
      slice(c(1:41, 123:160))

set.seed(1)
data4 = data1 %>%
  arrange(Nivel_educativo) %>%
  slice(42:122) %>%
  sample_frac(0.48, rep=FALSE)

data5 = bind_rows(data3, data4)

data5 %>%
  ggstatsplot::ggbetweenstats(
    x = Nivel_educativo,
    y = IDC,
    type = "p",
    xlab = "Nivel educativo",
    ylab = "Índice de consumo de frutas y verduras"
  )

# Ejemplo 2: Comando ggbetweenstats (no paramétrica y robusta)

data6 = foreign::read.spss("database_insulinoresistencia.sav", use.value.labels = TRUE, 
                           to.data.frame = TRUE)

data6 %>%
  group_by(estado_nutricional) %>%
  shapiro_test(HOMAIR)

data6 %>%
  levene_test(HOMAIR ~ estado_nutricional)

data6 %>%
  kruskal_test(HOMAIR ~ estado_nutricional) 

data6 %>%
  kruskal_effsize(HOMAIR ~ estado_nutricional)

data6 %>%
  ggstatsplot::ggbetweenstats(
    x = estado_nutricional,
    y = HOMAIR,
    xlab = "Estdo nutricional",
    ylab = "HOMA-IR",
    type = "np",
    pairwise.display = "all"
  ) 

data6 %>%
  ggstatsplot::ggbetweenstats(
    x = estado_nutricional,
    y = HOMAIR,
    xlab = "Estdo nutricional",
    ylab = "HOMA-IR",
    type = "r"
  ) 

# Ejemplo 3: Comando ggwithinstats (medidas repetidas)

data7 = readr::read_csv("analgesia_medidas_repetidas.csv")

# Preparar la data

data8 = gather(data7, "eva_hora", "eva_6horas","eva_12horas",
               key = "Tiempo_postoperatorio", value = "EVA")
data8$Tiempo_postoperatorio = factor(data8$Tiempo_postoperatorio, 
                                levels = c("eva_hora", "eva_6horas", "eva_12horas"))

# Friedman test (con el paquete rstatix)

data8 %>%
  friedman_test(EVA ~ Tiempo_postoperatorio |no_boleta)

data8 %>%
  friedman_effsize(EVA ~ Tiempo_postoperatorio |no_boleta)

data8 %>% 
  ggwithinstats(
    x = Tiempo_postoperatorio,
    y = EVA,
    xlab = "Tiempo postoperatorio",
    ylab = "EVA",
    type = "np"
  )

# Heteroscedastic one-way repeated measures ANOVA for trimmed means

data8 %>% 
  ggwithinstats(
    x = Tiempo_postoperatorio,
    y = EVA,
    xlab = "Tiempo postoperatorio",
    ylab = "EVA",
    type = "r"
  )

# Ejemplo 4: Comando ggscatterstats (correlación)

# cor_test de rstatix

data6 %>%
  cor_test(IMC, HOMAIR, method = "spearman") # cor.test

data6 %>% 
  ggscatterstats(
    x = HOMAIR,
    y = IMC,
    xlab = "HOMAIR",
    ylab = "IMC",
    type = "np"
  )

ggcorrmat(
  data = dplyr::select(data6, HOMAIR, IMC, HDL, LDL),
  type = "np"
)

# Ejemplo 5: Comando ggbarstats (variables categóricas)

tabla1 = data6 %>%
  cross_tabulate(estado_nutricional, Insulinoresistencia) 

chisq_test(tabla1)
fisher_test(tabla1)
pairwise_prop_test(tabla1)

data6 %>%
  ggbarstats(
    x = Insulinoresistencia,
    y = estado_nutricional,
    bf.message = FALSE,
    xlab = "Estado nutricional",
    ylab = "Prevalencia de resistencia a la insulina"
  )

# Ejemplo 6: Comando grouped_ (agrupación / estratificación por una tercera variable)

data9 = foreign::read.spss("database_lactancia_materna.sav", use.value.labels = TRUE, 
                           to.data.frame = TRUE)
data9 %>%
  grouped_ggbarstats(
    y = tipo_de_lactancia,
    x = desnutricion_severa,
    xlab = "Tipo de lactancia",
    ylab = "Lee y escribe",
    grouping.var = lee_y_escribe,
    bf.message = FALSE
  )

data8$analgesia_usada = factor(data8$analgesia_usada, labels = c("Bupivacaina", "Morfina"))

data8 %>% 
  grouped_ggwithinstats(
    x = Tiempo_postoperatorio,
    y = EVA,
    xlab = "Tiempo postoperatorio",
    ylab = "EVA",
    type = "r",
    grouping.var = analgesia_usada,
    results.subtitle = FALSE
  )

# Ejemplo 7: Centrándose en las gráficas

data6 %>%
  ggbarstats(
    x = Insulinoresistencia,
    y = estado_nutricional,
    xlab = "Estado nutricional",
    ylab = "Prevalencia de resistencia a la insulina",
    results.subtitle = FALSE,
    bf.message = FALSE
  )

data8 %>% 
  grouped_ggwithinstats(
    x = Tiempo_postoperatorio,
    y = EVA,
    xlab = "Tiempo postoperatorio",
    ylab = "EVA",
    type = "np",
    grouping.var = analgesia_usada,
    results.subtitle = FALSE,
    bf.message = FALSE,
    pairwise.comparisons = FALSE,
    centrality.path = FALSE,
    centrality.plotting = FALSE
  )

# Ejemplo 8: Matriz de correlaciones

ggcorrmat(
  data = dplyr::select(data6, HOMAIR, IMC, HDL, LDL),
  type = "np",
  output = "dataframe"
  )
  
  
# Trabajando desde jamovi

setwd("C:/curso_r/ggstatsplot")
