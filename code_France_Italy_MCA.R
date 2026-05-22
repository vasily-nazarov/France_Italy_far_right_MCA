#The entire Rscript 
# Load required packages without reinstalling them on every run
required_packages <- c(
  "patchwork", "questionr", "psych", "FactoMineR", "factoextra",
  "knitr", "kableExtra", "tidyjson", "dplyr", "tidyr", "ggplot2"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Install the missing packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(patchwork)
library(questionr)
library(psych)
library(FactoMineR)
library(factoextra)
library(knitr)
library(kableExtra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyjson)

a <- read.csv("~/Downloads/ESS11e04_1/ESS11e04_1.csv")
a <- a[a$lrscale %in% c(8, 9, 10), ]
a <- a[a$cntry %in% c("FR", "IT"), ]

{a$agea <- as.numeric(a$agea)
a$agea <- cut(a$agea,
              include.lowest = TRUE,
              right = FALSE,
              dig.lab = 4,
              breaks = c(0, 35, 50, 65, 120)
)
a$agea <- as.character(a$agea)
a$agea[a$agea == "[0,35)"] <- "≤34"
a$agea[a$agea == "[35,50)"] <- "35–49"
a$agea[a$agea == "[50,65)"] <- "50–64"
a$agea[a$agea == "[65,120]"] <- "≥65"
a$agea <- factor(a$agea,
                 levels = c("≤34", "35–49", "50–64", "≥65"))

a$prtvtffr <- as.character(a$prtvtffr)
a$prtvtffr <- ifelse(a$prtvtffr == "8", "RN",
                     ifelse(a$prtvtffr == "6", "LR-UDI",
                            ifelse(a$prtvtffr == "5", "Ensemble", NA)))
table(a$prtvteit)
## Recoding a$prtvteit
a$prtvteit <- as.character(a$prtvteit)
a$prtvteit <- ifelse(a$prtvteit == "1", "FdI",
                     ifelse(a$prtvteit == "4", "Lega",
                            ifelse(a$prtvteit == "5", "Forza", NA)))

a <- a %>%
  mutate(
    isco08 = case_when(
      
      isco08 %in% c("66666", "77777", "88888", "99999") ~ NA_character_,
      
      substr(isco08, 1, 1) == "0" ~ "Armées",
      substr(isco08, 1, 1) == "1" ~ "Cadres",
      substr(isco08, 1, 1) == "2" ~ "Professions sup.",
      substr(isco08, 1, 1) == "3" ~ "Prof. interm.",
      substr(isco08, 1, 1) == "4" ~ "Employés admin",
      substr(isco08, 1, 1) == "5" ~ "Services & vente",
      substr(isco08, 1, 1) == "6" ~ "Agriculture",
      substr(isco08, 1, 1) == "7" ~ "Ouvriers qual.",
      substr(isco08, 1, 1) == "8" ~ "Ouvriers indus.",
      substr(isco08, 1, 1) == "9" ~ "Ouvriers élém.",
      
      TRUE ~ NA_character_
    )
  )
a$party <- ifelse(!is.na(a$prtvtffr), a$prtvtffr, a$prtvteit)
a$imptrada <- as.character(a$imptrada)
a$imptrada[a$imptrada == "66"] <- NA
a$imptrada[a$imptrada == "77"] <- NA
a$imptrada[a$imptrada == "88"] <- NA
a$imptrada[a$imptrada == "99"] <- NA
a$imptrada <- as.numeric(a$imptrada)

a$ipfrulea <- as.character(a$ipfrulea)
a$ipfrulea[a$ipfrulea == "66"] <- NA
a$ipfrulea[a$ipfrulea == "77"] <- NA
a$ipfrulea[a$ipfrulea == "88"] <- NA
a$ipfrulea[a$ipfrulea == "99"] <- NA
a$ipfrulea <- as.numeric(a$ipfrulea)

a$ipbhprpa <- as.character(a$ipbhprpa)
a$ipbhprpa[a$ipbhprpa == "66"] <- NA
a$ipbhprpa[a$ipbhprpa == "77"] <- NA
a$ipbhprpa[a$ipbhprpa == "88"] <- NA
a$ipbhprpa[a$ipbhprpa == "99"] <- NA
a$ipbhprpa <- as.numeric(a$ipbhprpa)

a$rlgatnd <- as.character(a$rlgatnd)
a$rlgatnd[a$rlgatnd == "88"] <- NA
a$rlgatnd <- as.numeric(a$rlgatnd)

a$rlgdgr <- as.character(a$rlgdgr)
a$rlgdgr[a$rlgdgr == "77"] <- NA
a$rlgdgr[a$rlgdgr == "88"] <- NA
a$rlgdgr <- as.numeric(a$rlgdgr)

a$pray <- as.character(a$pray)
a$pray[a$pray == "88"] <- NA
a$pray[a$pray == "77"] <- NA
a$pray <- as.numeric(a$pray)

a$imwbcnt <- as.character(a$imwbcnt)
a$imwbcnt[a$imwbcnt == "88"] <- NA
a$imwbcnt <- as.numeric(a$imwbcnt)

a$impcntr <- as.character(a$impcntr)
a$impcntr[a$impcntr == "7"] <- NA
a$impcntr[a$impcntr == "8"] <- NA
a$impcntr <- as.numeric(a$impcntr)

a$imueclt <- as.character(a$imueclt)
a$imueclt[a$imueclt == "88"] <- NA
a$imueclt <- as.numeric(a$imueclt)

a$imbgeco <- as.character(a$imbgeco)
a$imbgeco[a$imbgeco == "88"] <- NA
a$imbgeco[a$imbgeco == "77"] <- NA
a$imbgeco <- as.numeric(a$imbgeco)

## Recoding a$imdfetn
a$imdfetn <- as.character(a$imdfetn)
a$imdfetn[a$imdfetn == "7"] <- NA
a$imdfetn[a$imdfetn == "8"] <- NA
a$imdfetn <- as.numeric(a$imdfetn)

#flipping the variables to match the rest of them so that the lowest value represents the highers opposition to migration
a$rlgdgr <- max(a$rlgdgr, na.rm = TRUE) - a$rlgdgr
a$impcntr <- max(a$impcntr, na.rm = TRUE) - a$impcntr
a$imdfetn <- as.numeric(a$imdfetn)
a$imdfetn <- max(a$imdfetn, na.rm = TRUE) - a$imdfetn}


#ALPHA DE CRONBACH checking for coherency
subset_H1 <- a[, c("imptrada", "ipfrulea", "ipbhprpa")]
subset_H1 <- data.frame(lapply(subset_H1, function(x) as.numeric(as.character(x))))
subset_H1 <- na.omit(subset_H1)
alpha_result <- psych::alpha(subset_H1)
print(alpha_result)

subset_H2 <- a[, c("rlgdgr", "rlgatnd", "pray")]
subset_H2 <- data.frame(lapply(subset_H2, function(x) as.numeric(as.character(x))))
subset_H2 <- na.omit(subset_H2)
alpha_result <- psych::alpha(subset_H2)
print(alpha_result)

subset_H3 <- a[, c("imwbcnt", "imueclt", "imdfetn", "imbgeco", "impcntr")]
subset_H3 <- data.frame(lapply(subset_H3, function(x) as.numeric(as.character(x))))
subset_H3 <- na.omit(subset_H3)
alpha_result <- psych::alpha(subset_H3)
print(alpha_result)


#CREATING INDEXES FOR BOXPLOTS
a$imptrada_rec_scale <- scale(a$imptrada)
a$ipfrulea_rec_scale <- scale(a$ipfrulea)
a$ipbhprpa_rec_scale <- scale(a$ipbhprpa)

#####STANDARDISER VARIABLES RELIGION#####

a$rlgdgr_scale <- scale(a$rlgdgr)
a$rlgatnd_rec_scale <- scale(a$rlgatnd)
a$pray_rec_scale <- scale(a$pray)

#####STANDARDISER VARIABLES IMMIGRATION#####

a$imwbcnt_rec_scale <- scale(a$imwbcnt)
a$imueclt_rec_scale <- scale(a$imueclt)
a$imdfetn_scale <- scale(a$imdfetn)
a$imbgeco_rec_scale <- scale(a$imbgeco)
a$impcntr_scale <- scale(a$impcntr)

a$index_trad <- ((a$imptrada_rec_scale + a$ipfrulea_rec_scale + a$ipbhprpa_rec_scale)/3)

#####CREER INDEX VALEURS RELIGION#####

a$index_rel <- ((a$rlgdgr_scale + a$rlgatnd_rec_scale + a$pray_rec_scale)/3)

#####CREER INDEX VALEURS IMMIGRATION#####
a$index_imm <- ((a$imwbcnt_rec_scale + a$imueclt_rec_scale + a$imdfetn_scale + a$imbgeco_rec_scale + a$impcntr_scale)/5)


#INVERSING THEM SO THAT HIGHER VALUE WOULD REPRESENT WORSE ATTITUDES 
a$index_rel <- a$index_rel * -1
a$index_trad <- a$index_trad * -1
a$index_imm <- a$index_imm * -1

#MAKING A BOXPLOT

par(mfrow=c(1,3), mar=c(4.5,4,3,1), oma=c(0.5,0,2.5,0)) # bottom, left, top, right

# Traditional Values
boxplot(index_trad ~ cntry, data = a,
        main = "Valeurs traditionnelles",
        xlab = "Pays",
        ylab = "Indice",
        border = "grey10", 
        col = adjustcolor(c("#000091", "#008C45"), alpha.f = 0.6),
        names = c("France","Italie"),
        ylim = c(-2,2),
        yaxt = "n",
        cex.main = 1.6, cex.lab  = 1.3, cex.axis = 1.3)
axis(side = 2, at = seq(-2,2, by = 0.5))

# Religious Values
boxplot(index_rel ~ cntry, data = a,
        main = "Religiosité",
        xlab = "Pays",
        ylab = "Indice",
        border = "grey10", 
        col = adjustcolor(c("#000091", "#008C45"), alpha.f = 0.6),
        names = c("France","Italie"),
        ylim = c(-2,2),
        yaxt = "n",
        cex.main = 1.6, cex.lab  = 1.3, cex.axis = 1.3)
axis(side = 2, at = seq(-2,2, by = 0.5))

# Immigration Attitudes
boxplot(index_imm ~ cntry, data = a,
        main = "Opposition à l'immigration",
        xlab = "Pays",
        ylab = "Indice",
        border = "grey10", 
        col = adjustcolor(c("#000091", "#008C45"), alpha.f = 0.6),
        names = c("France","Italie"),
        ylim = c(-2,2),
        yaxt = "n",
        cex.main = 1.6, cex.lab  = 1.3, cex.axis = 1.3)
axis(side = 2, at = seq(-2,2, by = 0.5))
mtext("Figure 1 — Comparaison des indices de valeurs", outer = TRUE, cex = 1.3, font = 2, line = 1)

a_fr <- a[a$cntry %in% "FR", ]
a_it <- a[a$cntry %in% "IT", ]

####FIGURE 2#######
{age_indexes_long <- a %>%
  select(agea, cntry, index_trad, index_rel, index_imm) %>%
  mutate(
    cntry = recode(cntry, "FR" = "France", "IT" = "Italie")
  ) %>%
  filter(!is.na(agea)) %>%
  pivot_longer(
    cols      = starts_with("index"),
    names_to  = "Index",
    values_to = "Score"
  ) %>%
  mutate(
    Index = recode(Index,
                   index_trad = "Tradition",
                   index_rel  = "Religiosite",
                   index_imm  = "Immigration"),
    Index = factor(Index, levels = c("Tradition", "Religiosite", "Immigration"))
  )

age_ci <- age_indexes_long %>%
  group_by(agea, Index, cntry) %>%
  summarise(
    Mean    = mean(Score, na.rm = TRUE),
    SE      = sd(Score, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    CI_low  = Mean - 1.96 * SE,
    CI_high = Mean + 1.96 * SE
  )

bg_facet <- data.frame(
  cntry = c("France", "Italie"),
  bg_fill = c("#000091", "#008C45")
)

ggplot(age_ci, aes(x = agea, y = Mean, fill = Index)) +
  geom_rect(
    data = bg_facet,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = bg_fill),
    inherit.aes = FALSE,
    alpha = 0.08
  ) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "gray40") +
  geom_vline(xintercept = c(1.5, 2.5, 3.5),
             linetype = "dashed", color = "gray70", linewidth = 0.4) +
  geom_col(
    position = position_dodge(0.75),
    width = 0.7,
    color = NA,
    alpha = 0.85
  ) +
  geom_errorbar(
    aes(ymin = CI_low, ymax = CI_high),
    position = position_dodge(0.75),
    width = 0.25,
    linewidth = 0.5,
    color = "gray25"
  ) +
  facet_wrap(~ cntry) +
  scale_fill_manual(
    values = c(palette_indexes, setNames(bg_facet$bg_fill, bg_facet$bg_fill)),
    breaks = levels(age_ci$Index)
  ) +
  coord_cartesian(ylim = c(-0.75, 0.85), clip = "off") +
  scale_y_continuous(breaks = seq(-0.75, 0.75, by = 0.25)) +
  labs(
    title = "Figure 2 - Lien entre âge et indices de valeurs",
    x = "Groupe d'âge",
    y = "Indice standardisé",
    fill = "Indice"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16,
                              margin = margin(b = 12)),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.4),
    panel.grid.minor.y = element_blank(),
    panel.spacing = unit(0, "lines"),
    panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.8)
  )
}


# Figure 3 — Data preparation
#--------------------------

{party_levels <- c("Ensemble", "LR-UDI", "RN", "Forza", "Lega", "FdI")

party_data <- a %>%
  filter(!is.na(party), party %in% party_levels) %>%
  select(party, cntry, index_trad, index_rel, index_imm) %>%
  mutate(party = factor(party, levels = party_levels))

party_long <- party_data %>%
  pivot_longer(
    cols = starts_with("index"),
    names_to = "Index",
    values_to = "Score"
  ) %>%
  mutate(
    Index = recode(
      Index,
      index_trad = "Tradition",
      index_rel  = "Religiosite",
      index_imm  = "Immigration"
    ),
    Index = factor(Index, levels = c("Tradition", "Religiosite", "Immigration"))
  )

  iqr_data_ci <- party_long %>%
  group_by(party, Index) %>%
  summarise(
    Mean = mean(Score, na.rm = TRUE),
    SE   = sd(Score, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    CI_low  = Mean - 1.96 * SE,
    CI_high = Mean + 1.96 * SE
  )

ggplot(iqr_data_ci, aes(x = party, y = Mean, fill = Index)) +
  annotate("rect", xmin = 0.5, xmax = 3.5, ymin = -Inf, ymax = Inf,
           fill = "#000091", alpha = 0.08) +
  annotate("rect", xmin = 3.5, xmax = 6.5, ymin = -Inf, ymax = Inf,
           fill = "#008C45", alpha = 0.08) +
  
  annotate("text", x = 2, y = 0.95, label = "France",
           fontface = "bold", color = "black", size = 5.5) +
  annotate("text", x = 5, y = 0.95, label = "Italie",
           fontface = "bold", color = "black", size = 5.5) +
  
  geom_vline(xintercept = c(1.5, 2.5, 4.5, 5.5),
             linetype = "dashed", color = "gray70", linewidth = 0.4) +
  
  geom_vline(xintercept = 3.5,
             color = "gray40", linewidth = 0.8) +
  
  geom_hline(yintercept = 0, linewidth = 0.5, color = "gray40") +
  
  geom_col(
    position = position_dodge(0.75),
    width = 0.7,
    color = NA,
    alpha = 0.85
  ) +
  
  geom_errorbar(
    aes(ymin = CI_low, ymax = CI_high),
    position = position_dodge(0.75),
    width = 0.25,
    linewidth = 0.5,
    color = "gray25"
  ) +
  
  scale_fill_manual(values = palette_indexes) +
  scale_x_discrete() +
  coord_cartesian(ylim = c(-0.85, 0.82), clip = "off") +
  scale_y_continuous(breaks = seq(-0.85, 0.8, by = 0.2)) +
  labs(
    title = "Figure 3 - Lien entre partis politiques et indices de valeurs",
    x = NULL,
    y = "Indice standardisé",
    fill = "Indice"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13),
    plot.title = element_text(face = "bold", hjust = 0.5, vjust = 5, size = 16,
                              margin = margin(b = 12)),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.4),
    panel.grid.minor.y = element_blank(),
    panel.spacing = unit(0, "lines"),
    panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.8),
    plot.margin = margin(t = 22, r = 10, b = 10, l = 10)
  )}

a_fr <- a[a$cntry %in% "FR", ]
a_it <- a[a$cntry %in% "IT", ]
#RECODING FOR ACM
{a_fr$rlgdgr <- as.numeric(a_fr$rlgdgr) 
  a_fr$rlgdgr <- cut(a_fr$rlgdgr,
                     
                     include.lowest = TRUE,
                     right = FALSE,
                     dig.lab = 4,
                     breaks = c(0, 2, 5, 8, 10)
                     
  )
  ## Recoding a_fr$relig
  a_fr$rlgdgr <- as.character(a_fr$rlgdgr)
  a_fr$rlgdgr[a_fr$rlgdgr == "[0,2)"] <- "relig++"
  a_fr$rlgdgr[a_fr$rlgdgr == "[2,5)"] <- "relig+"
  a_fr$rlgdgr[a_fr$rlgdgr == "[5,8)"] <- "relig-"
  a_fr$rlgdgr[a_fr$rlgdgr == "[8,10]"] <- "relig--"
  a_fr$relig <- a_fr$rlgdgr
  #PRAY
  a_fr$pray <- as.character(a_fr$pray)
  a_fr$pray <- as.numeric(a_fr$pray)
  ## Cutting a_fr$pray into a_fr$pray_rec
  a_fr$pray <- cut(a_fr$pray,
                   
                   include.lowest = TRUE,
                   right = FALSE,
                   dig.lab = 4,
                   breaks = c(0, 6, 7)
                   
  )
  ## Recoding a_fr$pray into a_fr$pray_rec
  a_fr$pray <- as.character(a_fr$pray)
  a_fr$pray[a_fr$pray == "[0,6)"] <- "attend+"
  a_fr$pray[a_fr$pray == "[6,7]"] <- "attend-"
  a_fr$attend <- a_fr$pray
  #RLGATND
  a_fr$rlgatnd <- as.character(a_fr$rlgatnd)
  a_fr$rlgatnd <- as.numeric(a_fr$rlgatnd)
  ## Cutting a_fr$rlgatnd into a_fr$rlgatnd
  a_fr$rlgatnd <- cut(a_fr$rlgatnd,
                      
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 6, 7)
                      
  )
  ## Recoding a_fr$rlgatnd
  a_fr$rlgatnd <- as.character(a_fr$rlgatnd)
  a_fr$rlgatnd[a_fr$rlgatnd == "[0,6)"] <- "pray+"
  a_fr$rlgatnd[a_fr$rlgatnd == "[6,7]"] <- "pray-"
  a_fr$pray <- a_fr$rlgatnd
  #IMWBCNT
  a_fr$imwbcnt <- as.character(a_fr$imwbcnt)
  a_fr$imwbcnt <- as.numeric(a_fr$imwbcnt)
  ## Cutting a_fr$imwbcnt into a_fr$imwbcnt
  a_fr$imwbcnt <- cut(a_fr$imwbcnt,
                      
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 1, 4, 8)
                      
  )
  ## Recoding a_fr$imwbcnt into a_fr$imwbcnt_rec
  a_fr$imwbcnt <- as.character(a_fr$imwbcnt)
  a_fr$imwbcnt[a_fr$imwbcnt == "[0,1)"] <- "better++" #the lower thevalue the more hate person feels hate towards towards migrants
  a_fr$imwbcnt[a_fr$imwbcnt == "[1,4)"] <- "better+"
  a_fr$imwbcnt[a_fr$imwbcnt == "[4,8]"] <- "better-"
  a_fr$better <- a_fr$imwbcnt
  #IMUECLT
  a_fr$imueclt <- as.character(a_fr$imueclt)
  a_fr$imueclt <- as.numeric(a_fr$imueclt)
  ## Cutting a_fr$imueclt into a_fr$imueclt
  a_fr$imueclt <- cut(a_fr$imueclt,
                      
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 1, 4, 9)
                      
  )
  a_fr$imueclt <- as.character(a_fr$imueclt)
  a_fr$imueclt[a_fr$imueclt == "[0,1)"] <- "culture++" #the lower the value the more hate person feels hate towards towards migrants
  a_fr$imueclt[a_fr$imueclt == "[1,4)"] <- "culture+"
  a_fr$imueclt[a_fr$imueclt == "[4,9]"] <- "culture-"
  a_fr$culture <- a_fr$imueclt
  #IMDFETN
  a_fr$imdfetn <- as.character(a_fr$imdfetn)
  a_fr$imdfetn <- as.numeric(a_fr$imdfetn)
  a_fr$imdfetn <- cut(a_fr$imdfetn,
                      
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 1, 2, 4)
                      
  )
  ## Recoding a_fr$imdfetn into a_fr$imdfetn_rec
  a_fr$imdfetn <- as.character(a_fr$imdfetn)
  a_fr$imdfetn[a_fr$imdfetn == "[0,1)"] <- "divers++"
  a_fr$imdfetn[a_fr$imdfetn == "[1,2)"] <- "divers+"
  a_fr$imdfetn[a_fr$imdfetn == "[2,4]"] <- "divers-"
  a_fr$divers <- a_fr$imdfetn
  #IMBGECO
  a_fr$imbgeco <- as.character(a_fr$imbgeco)
  a_fr$imbgeco <- as.numeric(a_fr$imbgeco)
  ## Cutting a_fr$imbgeco into a_fr$imbgeco
  a_fr$imbgeco <- cut(a_fr$imbgeco,
                      
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 2, 5, 10)
                      
  )
  ## Recoding a_fr$imbgeco into a_fr$imbgeco_rec
  a_fr$imbgeco <- as.character(a_fr$imbgeco)
  a_fr$imbgeco[a_fr$imbgeco == "[0,2)"] <- "econ++"
  a_fr$imbgeco[a_fr$imbgeco == "[2,5)"] <- "econ+"
  a_fr$imbgeco[a_fr$imbgeco == "[5,10]"] <- "econ-"
  a_fr$econ <- a_fr$imbgeco
  #IMPCNTR
  a_fr$impcntr <- as.character(a_fr$impcntr)
  a_fr$impcntr <- as.numeric(a_fr$impcntr)
  ## Cutting a_fr$impcntr into a_fr$impcntr
  a_fr$impcntr <- cut(a_fr$impcntr,
                      
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 0.5, 1.5, 3)
                      
  )
  ## Recoding a_fr$impcntr into a_fr$impcntr_rec
  a_fr$impcntr <- as.character(a_fr$impcntr)
  a_fr$impcntr[a_fr$impcntr == "[0,0.5)"] <- "entry++"
  a_fr$impcntr[a_fr$impcntr == "[0.5,1.5)"] <- "entry+"
  a_fr$impcntr[a_fr$impcntr == "[1.5,3]"] <- "entry-"
  a_fr$entry <- a_fr$impcntr
  #IMPTRADA
  a_fr$imptrada <- as.character(a_fr$imptrada)
  a_fr$imptrada <- as.numeric(a_fr$imptrada)
  ## Cutting a_fr$imptrada into a_fr$imptrada
  a_fr$imptrada <- cut(a_fr$imptrada,
                       
                       include.lowest = TRUE,
                       right = FALSE,
                       dig.lab = 4,
                       breaks = c(1, 2, 3, 5, 6)
                       
  )
  ## Recoding a_fr$imptrada
  a_fr$imptrada <- as.character(a_fr$imptrada)
  a_fr$imptrada[a_fr$imptrada == "[1,2)"] <- "trad++"
  a_fr$imptrada[a_fr$imptrada == "[2,3)"] <- "trad+"
  a_fr$imptrada[a_fr$imptrada == "[3,5)"] <- "trad-"
  a_fr$imptrada[a_fr$imptrada == "[5,6]"] <- "trad--"
  a_fr$trad <- a_fr$imptrada
  #IPFRULEA
  a_fr$ipfrulea <- as.character(a_fr$ipfrulea)
  a_fr$ipfrulea <- as.numeric(a_fr$ipfrulea)
  ## Cutting a_fr$ipfrulea into a_fr$ipfrulea
  a_fr$ipfrulea <- cut(a_fr$ipfrulea,
                       
                       include.lowest = TRUE,
                       right = FALSE,
                       dig.lab = 4,
                       breaks = c(0, 2.5, 4.5, 5.5, 6)
                       
  )
  ## Recoding a_fr$ipfrulea
  a_fr$ipfrulea <- as.character(a_fr$ipfrulea)
  a_fr$ipfrulea[a_fr$ipfrulea == "[0,2.5)"] <- "rules++"
  a_fr$ipfrulea[a_fr$ipfrulea == "[2.5,4.5)"] <- "rules+"
  a_fr$ipfrulea[a_fr$ipfrulea == "[4.5,5.5)"] <- "rules-"
  a_fr$ipfrulea[a_fr$ipfrulea == "[5.5,6]"] <- "rules--"
  a_fr$rules <- a_fr$ipfrulea
  #IPBHPRPA
  a_fr$ipbhprpa <- as.character(a_fr$ipbhprpa)
  a_fr$ipbhprpa <- as.numeric(a_fr$ipbhprpa)
  ## Cutting a_fr$ipbhprpa into a_fr$ipbhprpa
  a_fr$ipbhprpa <- cut(a_fr$ipbhprpa,
                       
                       include.lowest = TRUE,
                       right = FALSE,
                       dig.lab = 4,
                       breaks = c(1, 1.5, 2.5, 3.5, 6)
                       
  )
  ## Recoding a_fr$ipbhprpa
  a_fr$ipbhprpa <- as.character(a_fr$ipbhprpa)
  a_fr$ipbhprpa[a_fr$ipbhprpa == "[1,1.5)"] <- "behav++"
  a_fr$ipbhprpa[a_fr$ipbhprpa == "[1.5,2.5)"] <- "behav+"
  a_fr$ipbhprpa[a_fr$ipbhprpa == "[2.5,3.5)"] <- "behav-"
  a_fr$ipbhprpa[a_fr$ipbhprpa == "[3.5,6]"] <- "behav--"
  a_fr$behav <- a_fr$ipbhprpa
  #ITALY
  a_it$rlgdgr <- as.numeric(a_it$rlgdgr)
  a_it$rlgdgr <- cut(a_it$rlgdgr,
                     
                     include.lowest = TRUE,
                     right = FALSE,
                     dig.lab = 4,
                     breaks = c(0, 2, 5, 8, 10)
                     
  )
  a_it$rlgdgr <- as.character(a_it$rlgdgr)
  a_it$rlgdgr[a_it$rlgdgr == "[0,2)"] <- "relig++"
  a_it$rlgdgr[a_it$rlgdgr == "[2,5)"] <- "relig+"
  a_it$rlgdgr[a_it$rlgdgr == "[5,8)"] <- "relig-"
  a_it$rlgdgr[a_it$rlgdgr == "[8,10]"] <- "relig--"
  a_it$relig <- a_it$rlgdgr
  #PRAY
  a_it$pray <- as.character(a_it$pray)
  a_it$pray <- as.numeric(a_it$pray)
  ## Cutting a_it$pray into a_it$pray_rec
  a_it$pray <- cut(a_it$pray,
                   
                   include.lowest = TRUE,
                   right = FALSE,
                   dig.lab = 4,
                   breaks = c(0, 6, 7)
                   
  )
  ## Recoding a_it$pray into a_it$pray_rec
  a_it$pray <- as.character(a_it$pray)
  a_it$pray[a_it$pray == "[0,6)"] <- "attend+"
  a_it$pray[a_it$pray == "[6,7]"] <- "attend-"
  a_it$attend <- a_it$pray
  #RLGATND
  a_it$rlgatnd <- as.character(a_it$rlgatnd)
  a_it$rlgatnd <- as.numeric(a_it$rlgatnd)
  ## Cutting a_it$rlgatnd into a_it$rlgatnd
  a_it$rlgatnd <- cut(a_it$rlgatnd,
                      
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 6, 7)
                      
  )
  ## Recoding a_it$rlgatnd
  a_it$rlgatnd <- as.character(a_it$rlgatnd)
  a_it$rlgatnd[a_it$rlgatnd == "[0,6)"] <- "pray+"
  a_it$rlgatnd[a_it$rlgatnd == "[6,7]"] <- "pray-"
  a_it$pray <- a_it$rlgatnd
  #IMWBCNT
  a_it$imwbcnt <- as.character(a_it$imwbcnt)
  a_it$imwbcnt <- as.numeric(a_it$imwbcnt)
  ## Cutting a_it$imwbcnt into a_it$imwbcnt
  a_it$imwbcnt <- cut(a_it$imwbcnt,
                      
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 1, 4, 8)
                      
  )
  ## Recoding a_it$imwbcnt into a_it$imwbcnt_rec
  a_it$imwbcnt <- as.character(a_it$imwbcnt)
  a_it$imwbcnt[a_it$imwbcnt == "[0,1)"] <- "better++" #the lower the value the more hate person feels hate towards towards migrants
  a_it$imwbcnt[a_it$imwbcnt == "[1,4)"] <- "better+"
  a_it$imwbcnt[a_it$imwbcnt == "[4,8]"] <- "better-"
  a_it$better <- a_it$imwbcnt
  #IMUECLT
  a_it$imueclt <- as.character(a_it$imueclt)
  a_it$imueclt <- as.numeric(a_it$imueclt)
  ## Cutting a_it$imueclt into a_it$imueclt
  a_it$imueclt <- cut(a_it$imueclt,
                      
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 1, 4, 9)
                      
  )
  a_it$imueclt <- as.character(a_it$imueclt)
  a_it$imueclt[a_it$imueclt == "[0,1)"] <- "culture++" #the lower the value the more hate person feels hate towards towards migrants
  a_it$imueclt[a_it$imueclt == "[1,4)"] <- "culture+"
  a_it$imueclt[a_it$imueclt == "[4,9]"] <- "culture-"
  a_it$culture <- a_it$imueclt
  #IMDFETN
  a_it$imdfetn <- as.character(a_it$imdfetn)
  a_it$imdfetn <- as.numeric(a_it$imdfetn)
  a_it$imdfetn <- cut(a_it$imdfetn,
                      
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 1, 2, 4)
                      
  )
  ## Recoding a_it$imdfetn into a_it$imdfetn_rec
  a_it$imdfetn <- as.character(a_it$imdfetn)
  a_it$imdfetn[a_it$imdfetn == "[0,1)"] <- "divers++"
  a_it$imdfetn[a_it$imdfetn == "[1,2)"] <- "divers+"
  a_it$imdfetn[a_it$imdfetn == "[2,4]"] <- "divers-"
  a_it$divers <- a_it$imdfetn
  #IMBGECO
  a_it$imbgeco <- as.character(a_it$imbgeco)
  a_it$imbgeco <- as.numeric(a_it$imbgeco)
  ## Cutting a_it$imbgeco into a_it$imbgeco
  a_it$imbgeco <- cut(a_it$imbgeco,
                      
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 2, 5, 10)
                      
  )
  ## Recoding a_it$imbgeco into a_it$imbgeco_rec
  a_it$imbgeco <- as.character(a_it$imbgeco)
  a_it$imbgeco[a_it$imbgeco == "[0,2)"] <- "econ++"
  a_it$imbgeco[a_it$imbgeco == "[2,5)"] <- "econ+"
  a_it$imbgeco[a_it$imbgeco == "[5,10]"] <- "econ-"
  a_it$econ <- a_it$imbgeco
  #IMPCNTR
  a_it$impcntr <- as.character(a_it$impcntr)
  a_it$impcntr <- as.numeric(a_it$impcntr)
  ## Cutting a_it$impcntr into a_it$impcntr
  a_it$impcntr <- cut(a_it$impcntr,
                      
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 0.5, 1.5, 3)
                      
  )
  ## Recoding a_it$impcntr into a_it$impcntr_rec
  a_it$impcntr <- as.character(a_it$impcntr)
  a_it$impcntr[a_it$impcntr == "[0,0.5)"] <- "entry++"
  a_it$impcntr[a_it$impcntr == "[0.5,1.5)"] <- "entry+"
  a_it$impcntr[a_it$impcntr == "[1.5,3]"] <- "entry-"
  a_it$entry <- a_it$impcntr
  #IMPTRADA
  a_it$imptrada <- as.character(a_it$imptrada)
  a_it$imptrada <- as.numeric(a_it$imptrada)
  ## Cutting a_it$imptrada into a_it$imptrada
  a_it$imptrada <- cut(a_it$imptrada,
                       
                       include.lowest = TRUE,
                       right = FALSE,
                       dig.lab = 4,
                       breaks = c(1, 2, 3, 5, 6)
                       
  )
  ## Recoding a_it$imptrada
  a_it$imptrada <- as.character(a_it$imptrada)
  a_it$imptrada[a_it$imptrada == "[1,2)"] <- "trad++"
  a_it$imptrada[a_it$imptrada == "[2,3)"] <- "trad+"
  a_it$imptrada[a_it$imptrada == "[3,5)"] <- "trad-"
  a_it$imptrada[a_it$imptrada == "[5,6]"] <- "trad--"
  a_it$trad <- a_it$imptrada
  #IPFRULEA
  a_it$ipfrulea <- as.character(a_it$ipfrulea)
  a_it$ipfrulea <- as.numeric(a_it$ipfrulea)
  ## Cutting a_it$ipfrulea into a_it$ipfrulea
  a_it$ipfrulea <- cut(a_it$ipfrulea,
                       
                       include.lowest = TRUE,
                       right = FALSE,
                       dig.lab = 4,
                       breaks = c(0, 2.5, 4.5, 5.5, 6)
                       
  )
  ## Recoding a_it$ipfrulea
  a_it$ipfrulea <- as.character(a_it$ipfrulea)
  a_it$ipfrulea[a_it$ipfrulea == "[0,2.5)"] <- "rules++"
  a_it$ipfrulea[a_it$ipfrulea == "[2.5,4.5)"] <- "rules+"
  a_it$ipfrulea[a_it$ipfrulea == "[4.5,5.5)"] <- "rules-"
  a_it$ipfrulea[a_it$ipfrulea == "[5.5,6]"] <- "rules--"
  a_it$rules <- a_it$ipfrulea
  #IPBHPRPA
  a_it$ipbhprpa <- as.character(a_it$ipbhprpa)
  a_it$ipbhprpa <- as.numeric(a_it$ipbhprpa)
  ## Cutting a_it$ipbhprpa into a_it$ipbhprpa
  a_it$ipbhprpa <- cut(a_it$ipbhprpa,
                       
                       include.lowest = TRUE,
                       right = FALSE,
                       dig.lab = 4,
                       breaks = c(1, 1.5, 2.5, 3.5, 6)
                       
  )
  ## Recoding a_it$ipbhprpa
  a_it$ipbhprpa <- as.character(a_it$ipbhprpa)
  a_it$ipbhprpa[a_it$ipbhprpa == "[1,1.5)"] <- "behav++"
  a_it$ipbhprpa[a_it$ipbhprpa == "[1.5,2.5)"] <- "behav+"
  a_it$ipbhprpa[a_it$ipbhprpa == "[2.5,3.5)"] <- "behav-"
  a_it$ipbhprpa[a_it$ipbhprpa == "[3.5,6]"] <- "behav--"
  a_it$behav <- a_it$ipbhprpa

#RLGDGR
## Cutting a_fr$rlgdgr into a_fr$relig
a$rlgdgr <- as.numeric(a$rlgdgr) 
a$rlgdgr <- cut(a$rlgdgr,
                include.lowest = TRUE,
                right = FALSE,
                dig.lab = 4,
                breaks = c(0, 2, 5, 8, 10)
)
## Recoding a$relig
a$rlgdgr <- as.character(a$rlgdgr)
a$rlgdgr[a$rlgdgr == "[0,2)"] <- "relig++"
a$rlgdgr[a$rlgdgr == "[2,5)"] <- "relig+"
a$rlgdgr[a$rlgdgr == "[5,8)"] <- "relig-"
a$rlgdgr[a$rlgdgr == "[8,10]"] <- "relig--"
a$relig <- a$rlgdgr
#PRAY
a$pray <- as.character(a$pray)
a$pray <- as.numeric(a$pray)
## Cutting a$pray into a$pray_rec
a$pray <- cut(a$pray,
              include.lowest = TRUE,
              right = FALSE,
              dig.lab = 4,
              breaks = c(0, 6, 7)
)
## Recoding a$pray into a$pray_rec
a$pray <- as.character(a$pray)
a$pray[a$pray == "[0,6)"] <- "attend+"
a$pray[a$pray == "[6,7]"] <- "attend-"
a$attend <- a$pray
#RLGATND
a$rlgatnd <- as.character(a$rlgatnd)
a$rlgatnd <- as.numeric(a$rlgatnd)
## Cutting a$rlgatnd into a$rlgatnd
a$rlgatnd <- cut(a$rlgatnd,
                 include.lowest = TRUE,
                 right = FALSE,
                 dig.lab = 4,
                 breaks = c(0, 6, 7)
)
## Recoding a$rlgatnd
a$rlgatnd <- as.character(a$rlgatnd)
a$rlgatnd[a$rlgatnd == "[0,6)"] <- "pray+"
a$rlgatnd[a$rlgatnd == "[6,7]"] <- "pray-"
a$pray <- a$rlgatnd
#IMWBCNT
a$imwbcnt <- as.character(a$imwbcnt)
a$imwbcnt <- as.numeric(a$imwbcnt)
## Cutting a$imwbcnt into a$imwbcnt
a$imwbcnt <- cut(a$imwbcnt,
                 include.lowest = TRUE,
                 right = FALSE,
                 dig.lab = 4,
                 breaks = c(0, 1, 4, 8)
)
## Recoding a$imwbcnt into a$imwbcnt_rec
a$imwbcnt <- as.character(a$imwbcnt)
a$imwbcnt[a$imwbcnt == "[0,1)"] <- "better++" #the lower the value the more hate person feels hate towards towards migrants
a$imwbcnt[a$imwbcnt == "[1,4)"] <- "better+"
a$imwbcnt[a$imwbcnt == "[4,8]"] <- "better-"
a$better <- a$imwbcnt
#IMUECLT
a$imueclt <- as.character(a$imueclt)
a$imueclt <- as.numeric(a$imueclt)
## Cutting a$imueclt into a$imueclt
a$imueclt <- cut(a$imueclt,
                 include.lowest = TRUE,
                 right = FALSE,
                 dig.lab = 4,
                 breaks = c(0, 1, 4, 9)
)

a$imueclt <- as.character(a$imueclt)
a$imueclt[a$imueclt == "[0,1)"] <- "culture++" #the lower the value the more hate person feels hate towards towards migrants
a$imueclt[a$imueclt == "[1,4)"] <- "culture+"
a$imueclt[a$imueclt == "[4,9]"] <- "culture-"
a$culture <- a$imueclt
#IMDFETN

a$imdfetn <- as.character(a$imdfetn)
a$imdfetn <- as.numeric(a$imdfetn)

a$imdfetn <- cut(a$imdfetn,
                 include.lowest = TRUE,
                 right = FALSE,
                 dig.lab = 4,
                 breaks = c(0, 1, 2, 4)
)
## Recoding a$imdfetn into a$imdfetn_rec
a$imdfetn <- as.character(a$imdfetn)
a$imdfetn[a$imdfetn == "[0,1)"] <- "divers++"
a$imdfetn[a$imdfetn == "[1,2)"] <- "divers+"
a$imdfetn[a$imdfetn == "[2,4]"] <- "divers-"
a$divers <- a$imdfetn
#IMBGECO 

a$imbgeco <- as.character(a$imbgeco)
a$imbgeco <- as.numeric(a$imbgeco)
## Cutting a$imbgeco into a$imbgeco
a$imbgeco <- cut(a$imbgeco,
                 include.lowest = TRUE,
                 right = FALSE,
                 dig.lab = 4,
                 breaks = c(0, 2, 5, 10)
)
## Recoding a$imbgeco into a$imbgeco_rec
a$imbgeco <- as.character(a$imbgeco)
a$imbgeco[a$imbgeco == "[0,2)"] <- "econ++"
a$imbgeco[a$imbgeco == "[2,5)"] <- "econ+"
a$imbgeco[a$imbgeco == "[5,10]"] <- "econ-"
a$econ <- a$imbgeco
#IMPCNTR
a$impcntr <- as.character(a$impcntr)
a$impcntr <- as.numeric(a$impcntr)

## Cutting a$impcntr into a$impcntr
a$impcntr <- cut(a$impcntr,
                 include.lowest = TRUE,
                 right = FALSE,
                 dig.lab = 4,
                 breaks = c(0, 0.5, 1.5, 3)
)

## Recoding a$impcntr into a$impcntr_rec
a$impcntr <- as.character(a$impcntr)
a$impcntr[a$impcntr == "[0,0.5)"] <- "entry++"
a$impcntr[a$impcntr == "[0.5,1.5)"] <- "entry+"
a$impcntr[a$impcntr == "[1.5,3]"] <- "entry-"
a$entry <- a$impcntr
#IMPTRADA

a$imptrada <- as.character(a$imptrada)
a$imptrada <- as.numeric(a$imptrada)
## Cutting a$imptrada into a$imptrada
a$imptrada <- cut(a$imptrada,
                  include.lowest = TRUE,
                  right = FALSE,
                  dig.lab = 4,
                  breaks = c(1, 2, 3, 5, 6)
)
## Recoding a$imptrada
a$imptrada <- as.character(a$imptrada)
a$imptrada[a$imptrada == "[1,2)"] <- "trad++"
a$imptrada[a$imptrada == "[2,3)"] <- "trad+"
a$imptrada[a$imptrada == "[3,5)"] <- "trad-"
a$imptrada[a$imptrada == "[5,6]"] <- "trad--"
a$trad <- a$imptrada
#IPFRULEA

a$ipfrulea <- as.character(a$ipfrulea)
a$ipfrulea <- as.numeric(a$ipfrulea)
## Cutting a$ipfrulea into a$ipfrulea
a$ipfrulea <- cut(a$ipfrulea,
                  include.lowest = TRUE,
                  right = FALSE,
                  dig.lab = 4,
                  breaks = c(0, 2.5, 4.5, 6)
)
## Recoding a$ipfrulea
a$ipfrulea <- as.character(a$ipfrulea)
a$ipfrulea[a$ipfrulea == "[0,2.5)"] <- "rules++"
a$ipfrulea[a$ipfrulea == "[2.5,4.5)"] <- "rules+"
a$ipfrulea[a$ipfrulea == "[4.5,6]"] <- "rules-"
a$rules <- a$ipfrulea
#IPBHPRPA

a$ipbhprpa <- as.character(a$ipbhprpa)
a$ipbhprpa <- as.numeric(a$ipbhprpa)
## Cutting a$ipbhprpa into a$ipbhprpa
a$ipbhprpa <- cut(a$ipbhprpa,
                  include.lowest = TRUE,
                  right = FALSE,
                  dig.lab = 4,
                  breaks = c(1, 1.5, 2.5, 3.5, 6)
)
## Recoding a$ipbhprpa
a$ipbhprpa <- as.character(a$ipbhprpa)
a$ipbhprpa[a$ipbhprpa == "[1,1.5)"] <- "behav++"
a$ipbhprpa[a$ipbhprpa == "[1.5,2.5)"] <- "behav+"
a$ipbhprpa[a$ipbhprpa == "[2.5,3.5)"] <- "behav-"
a$ipbhprpa[a$ipbhprpa == "[3.5,6]"] <- "behav--"
a$behav <- a$ipbhprpa}

#ACM France

ACM_Fr <- a_fr[, c("relig","pray","attend","better","culture","divers","econ","entry","trad","rules","behav", "agea", "prtvtffr", "isco08")]
ACM_Fr <- na.omit(ACM_Fr)
ACM_Fr[] <- lapply(ACM_Fr, factor)


ACM_Fr_res <- MCA(ACM_Fr,
                  quali.sup = c("agea", "prtvtffr", "isco08"),
                  graph = F)

ACM_It <- a_it[, c("relig","pray","attend","better","culture","divers","econ","entry","trad","rules","behav","agea","prtvteit", "isco08")]
ACM_It <- na.omit(ACM_It)
ACM_It[] <- lapply(ACM_It, factor)

ACM_It_res <- MCA(ACM_It, 
                  quali.sup = c("agea", "prtvteit", "isco08"),
                  graph = F)
ACM_Fr_res$quali.sup$coord[, c("Dim 1", "Dim 2")]
ACM_It_res$quali.sup$coord[, c("Dim 1", "Dim 2")]
ACM_Fr_res$quali.sup$eta2[, c("Dim 1", "Dim 2")]
ACM_It_res$quali.sup$eta2[, c("Dim 1", "Dim 2")]
ACM_Fr_res$quali.sup$v.test[, c("Dim 1", "Dim 2")]
ACM_It_res$quali.sup$v.test[, c("Dim 1", "Dim 2")]

ACM_Fr_res$var$coord[, c("Dim 1", "Dim 2")]
ACM_It_res$var$coord[, c("Dim 1", "Dim 2")]
table(a_it$isco08)
ACM_Fr_res$var$contrib[, c("Dim 1", "Dim 2")]
# Benzécri correction for MCA eigenvalues
# Removes dimensions where eigenvalue < 1/number of active variables

K <- 11  # number of active variables
Q <- K / (K - 1)  # Benzécri correction factor

# --- France ---
eig_fr <- ACM_Fr_res$eig[, 1]  # raw eigenvalues

benz_fr <- eig_fr[eig_fr > 1/K]  # keep only eigenvalues above threshold
benz_fr_corrected <- (Q * (benz_fr - 1/K))^2
benz_fr_variance  <- benz_fr_corrected / sum(benz_fr_corrected) * 100

cat("=== France — Benzécri corrected variance ===\n")
for (i in seq_along(benz_fr_variance)) {
  cat(sprintf("  Dim %d: %.2f%%\n", i, benz_fr_variance[i]))
}
cat(sprintf("  Cumulative (Dim1+2): %.2f%%\n\n", sum(benz_fr_variance[1:2])))
### 55.1% + 19.47% = 74.57% FOR FRANCE
# --- Italy ---
eig_it <- ACM_It_res$eig[, 1]

benz_it <- eig_it[eig_it > 1/K]
benz_it_corrected <- (Q * (benz_it - 1/K))^2
benz_it_variance  <- benz_it_corrected / sum(benz_it_corrected) * 100

cat("=== Italie — Benzécri corrected variance ===\n")
for (i in seq_along(benz_it_variance))
  cat(sprintf("  Dim %d: %.2f%%\n", i, benz_it_variance[i]))

cat(sprintf("  Cumulative (Dim1+2): %.2f%%\n", sum(benz_it_variance[1:2])))
## 55.05% + 27% =82.05% FOR ITALY



#VISUALASING ACM
# Figure 4a
{# --- Active variable dataframes ---
  
  get_active <- function(mca_res) {
    df <- as.data.frame(mca_res$var$coord[, 1:2])
    colnames(df) <- c("x", "y")
    df$label <- rownames(df)
    
    df$hypothesis <- dplyr::case_when(
      grepl("^relig|^pray|^attend", df$label) ~ "Religiosité",
      grepl("^better|^culture|^divers|^econ|^entry", df$label) ~ "Immigration",
      grepl("^trad|^rules|^behav", df$label) ~ "Tradition",
      TRUE ~ NA_character_
    )
    
    df
  }
  active_fr <- get_active(ACM_Fr_res)
  active_it <- get_active(ACM_It_res_flipped)
  # --- Shared axis limits: active variables only ---
  
  all_active_coords <- rbind(
    active_fr[, c("x", "y")],
    active_it[, c("x", "y")]
  )
  
  x_lim_active <- range(all_active_coords$x)
  y_lim_active <- range(all_active_coords$y)
  
  # Optional: add small margins around the points
  x_pad <- diff(x_lim_active) * 0.08
  y_pad <- diff(y_lim_active) * 0.08
  
  x_lim_active <- x_lim_active + c(-x_pad, x_pad)
  y_lim_active <- y_lim_active + c(-y_pad, y_pad)
  
  # --- Palette ---
  
  hypothesis_palette <- c(
    "Tradition" = "#00897B",
    "Religiosité" = "#7B1FA2",
    "Immigration" = "#E64A19"
  )
  
  # --- Plot function: active variables only ---
  
  make_mca_active_plot <- function(active_df, title_text, x_lim, y_lim, palette,
                                   var_dim1, var_dim2, bg_fill) {
    
    ggplot(active_df, aes(x = x, y = y, label = label, color = hypothesis)) +
      annotate(
        "rect",
        xmin = -Inf, xmax = Inf,
        ymin = -Inf, ymax = Inf,
        fill = bg_fill,
        alpha = 0.08
      ) +
      geom_hline(yintercept = 0, linetype = "solid", color = "grey80") +
      geom_vline(xintercept = 0, linetype = "solid", color = "grey80") +
      ggrepel::geom_text_repel(
        fontface = "bold",
        size = 3.6,
        max.overlaps = Inf,
        box.padding = 0.3,
        point.padding = 0.15,
        force = 3,
        force_pull = 0.4,
        min.segment.length = 0.5,
        segment.color = "grey75",
        segment.size = 0.25,
        seed = 42,
        show.legend = TRUE
      ) +
      scale_color_manual(
        values = palette,
        breaks = names(palette),
        labels = names(palette),
        name = "Variables actives :"
      ) +
      coord_cartesian(xlim = x_lim, ylim = y_lim) +
      labs(
        title = title_text,
        x = paste0("Dim 1 — Immigration (", var_dim1, "%, Benzécri)"),
        y = paste0("Dim 2 — Traditionalisme (", var_dim2, "%, Benzécri)")
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14, margin = margin(b = 10)),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "bottom"
      ) +
      annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, colour = "black", size = 0.5) +
      annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, colour = "black", size = 0.5)
  }
  
  # --- France and Italy plots: active variables only ---
  
  p_fr_active <- make_mca_active_plot(
    active_fr,
    "France",
    x_lim_active,
    y_lim_active,
    hypothesis_palette,
    var_dim1 = 55.10,
    var_dim2 = 19.47,
    bg_fill = "#000091"
  )
  
  p_it_active <- make_mca_active_plot(
    active_it,
    "Italie",
    x_lim_active,
    y_lim_active,
    hypothesis_palette,
    var_dim1 = 55.05,
    var_dim2 = 27.00,
    bg_fill = "#008C45"
  )
  
  combined_active_plots <- ((p_fr_active / p_it_active) + plot_layout(guides = "collect")) &
    theme(
      legend.position = "bottom",
      legend.key.size = unit(0.75, "cm")
    )
  
  combined_active_plots <- combined_active_plots +
    plot_annotation(
      title = "Figure 4a — ACM des variables actives",
      theme = theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15)
      )
    )
  
  combined_active_plots
}

# Figure 4b
{sup_fr <- data.frame(
  label = c(
    "≤34", "35–49", "50–64", "≥65",
    "Ensemble", "LR-UDI", "RN",
    "Agriculture", "Employés admin", "Ouvriers qual.",
    "Ouvriers élém.", "Ouvriers indus.", "Cadres",
    "Professions sup.", "Services & vente", "Prof. interm."
  ),
  x = c(
    0.19013796, -0.34457536, -0.02098041,  0.15577700,
    -0.67537322, -0.19908391,  0.34978285,
    0.79865687,  0.01908280,  0.44953929,
    0.69949479,  0.16837995, -0.47627008,
    -0.32483376, -0.33912359, -0.26025767
  ),
  y = c(
    -0.36319725,  0.18373679, -0.13286378,  0.17399022,
    -0.11393767,  0.41195003, -0.11920494,
    0.31791695,  0.64560446, -0.18874118,
    -0.06389149, -0.44821991,  0.11343886,
    -0.16008172, -0.32767771,  0.31152676
  ),
  block = c(
    rep("Âge", 4),
    rep("Vote", 3),
    rep("Profession", 9)
  )
)

sup_it <- data.frame(
  label = c(
    "≤34", "35–49", "50–64", "≥65",
    "FdI", "Forza", "Lega",
    "Agriculture", "Employés admin", "Ouvriers qual.",
    "Ouvriers élém.", "Ouvriers indus.", "Cadres",
    "Professions sup.", "Services & vente", "Prof. interm."
  ),
  x = c(
    -0.006120876, -0.037879587,  0.001856142,  0.028491475,
    0.032328927, -0.063322704, -0.031268179,
    0.109842861,  0.371664565,  0.160266209,
    0.220894359, -0.124816415, -0.148498546,
    -0.033052565, -0.062151240, -0.132747559
  ),
  y = c(
    0.23369476,  0.54883812, -0.08918501, -0.37452071,
    -0.07324765,  0.09300563,  0.08714827,
    0.23815541, -0.21379511, -0.16419255,
    -0.26754196, -0.03605472, -0.22991833,
    -0.15548866,  0.14628002,  0.32205520
  ),
  block = c(
    rep("Âge", 4),
    rep("Vote", 3),
    rep("Profession", 9)
  )
)

# Flip Dim 2 for Italy to match ACM_It_res_flipped
sup_it$y <- -sup_it$y

sup_fr$nudge_x <- 0
sup_fr$nudge_y <- 0

sup_it$nudge_x <- 0
sup_it$nudge_y <- 0

sup_fr$nudge_y[sup_fr$label == "Cadres"] <- 0.10
sup_fr$nudge_y[sup_fr$label == "Professions sup."] <- -0.10

# --- Shared axis limits: same as Figure 4a if already defined ---

all_sup_coords <- rbind(
  sup_fr[, c("x", "y")],
  sup_it[, c("x", "y")]
)

x_lim_sup <- x_lim_active
y_lim_sup <- y_lim_active

x_pad <- diff(x_lim_sup) * 0.08
y_pad <- diff(y_lim_sup) * 0.08

x_lim_sup <- x_lim_sup + c(-x_pad, x_pad)
y_lim_sup <- y_lim_sup + c(-y_pad, y_pad)

# Optional but recommended:
# If you want Figure 4b to be directly comparable to Figure 4a,
# reuse x_lim_active and y_lim_active instead:
#
# x_lim_sup <- x_lim_active
# y_lim_sup <- y_lim_active

# --- Palette for supplementary blocks ---
# Different from Figure 4a, but still harmonious.

supp_palette <- c(
  "Âge" = "#1976D2",
  "Vote" = "#F9A825",
  "Profession" = "#6D4C41"
)

# --- Plot function: supplementary variables only ---

make_mca_sup_plot <- function(sup_df, title_text, x_lim, y_lim, palette,
                              var_dim1, var_dim2, bg_fill) {
  
  ggplot(sup_df, aes(x = x, y = y, label = label, color = block)) +
    annotate(
      "rect",
      xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = Inf,
      fill = bg_fill,
      alpha = 0.08
    ) +
    geom_hline(yintercept = 0, linetype = "solid", color = "grey80") +
    geom_vline(xintercept = 0, linetype = "solid", color = "grey80") +
    ggrepel::geom_text_repel(
      nudge_x = sup_df$nudge_x,
      nudge_y = sup_df$nudge_y,
      fontface = "bold",
      size = 3.6,
      max.overlaps = Inf,
      box.padding = 0.35,
      point.padding = 0.15,
      force = 4,
      force_pull = 0.35,
      min.segment.length = 0.4,
      segment.color = "grey75",
      segment.size = 0.25,
      seed = 42,
      show.legend = TRUE
    ) +
    scale_color_manual(
      values = palette,
      breaks = names(palette),
      labels = names(palette),
      name = "Variables supplémentaires :"
    ) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    labs(
      title = title_text,
      x = paste0("Dim 1 — Immigration (", var_dim1, "%, Benzécri)"),
      y = paste0("Dim 2 — Traditionalisme (", var_dim2, "%, Benzécri)")
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14, margin = margin(b = 10)),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 11),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.position = "bottom"
    ) +
    annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, colour = "black", size = 0.5) +
    annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, colour = "black", size = 0.5)
}

# --- France and Italy plots: supplementary variables only ---

p_fr_sup <- make_mca_sup_plot(
  sup_fr,
  "France",
  x_lim_sup,
  y_lim_sup,
  supp_palette,
  var_dim1 = 55.10,
  var_dim2 = 19.47,
  bg_fill = "#000091"
)

p_it_sup <- make_mca_sup_plot(
  sup_it,
  "Italie",
  x_lim_sup,
  y_lim_sup,
  supp_palette,
  var_dim1 = 55.05,
  var_dim2 = 27.00,
  bg_fill = "#008C45"
)

combined_sup_plots <- ((p_fr_sup / p_it_sup) + plot_layout(guides = "collect")) &
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.75, "cm")
  )

combined_sup_plots <- combined_sup_plots +
  plot_annotation(
    title = "Figure 4b — Projection des variables supplémentaires",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 15)
    )
  )

combined_sup_plots}

