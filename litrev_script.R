rm(list = ls())
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(forcats)
library(cowplot)
## Load the table
tab <-
  read_excel("data/literature.xlsx") %>%
  ## Order the trend levels
  mutate(Trend = factor(Trend, levels = c("Increase", "Stable", "Decrease")))

## Create the Table 1 ################

# Function to collapse the reference column
collapse_rows_df <- function(df, variable){

  group_var <- enquo(variable)

  df %>%
    group_by(!! group_var) %>%
    mutate(groupRow = 1:n()) %>%
    ungroup() %>%
    mutate(!!quo_name(group_var) := ifelse(groupRow == 1, as.character(!! group_var), "")) %>%
    select(-c(groupRow))
}

table1 <-
  tab %>%
  mutate(`Temporal grain (hour)` = as.numeric(`Temporal grain (hour)`),
         `Temporal lag (year)` = as.numeric(`Temporal lag (year)`),
         `Spatial extent (Km²)` = as.numeric(`Spatial extent (Km²)`),
         `Temporal extent (year)` = as.numeric(`Temporal extent (year)`)) %>%
  select(-Reference_) %>%
  as_tibble() %>%
  group_by(Reference) %>%
  collapse_rows_df(Reference)

## Create supplementary Figure 1, not accounting for pseudo replicates #################
Fig1a_supp <-
  table(tab$`Spatial grain (Km²)`, tab$Trend) %>%
  as.data.frame() %>%
  rename("Frequency" = Freq,
         "Spatial grain size" = Var1,
         "Trend" = Var2) %>%
  mutate("Spatial grain size" = fct_relevel(`Spatial grain size`,c("Local", "Regional", "National", "Global"))) %>%
  ggplot(aes(fill = Trend, x = `Spatial grain size`, y = Frequency))+
  geom_bar(position = "stack", stat = "identity")+
  ylab("Number of trends")+
  ggtitle("Spatial grain")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x=element_blank())+
  scale_fill_viridis_d(option = "turbo",
                       begin = .8, end = .5, direction = -1)+
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5))

Fig1b_supp <-
  table(tab %>% select(Metric, Trend)) %>%
  as.data.frame() %>%
  rename("Frequency" = Freq) %>%
  ggplot(aes(fill = Trend, y = `Frequency`, x = Metric))+
  geom_bar(position = "stack", stat = "identity")+
  ylab("Number of trends")+
  ggtitle("Metric")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x=element_blank(),
        axis.title.y = element_blank())+
  scale_fill_viridis_d(option = "turbo",
                       begin = .8, end = .5, direction = -1)+
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5))

Fig1c_supp <-
  table(tab %>% select(Metric, Trend, `Spatial grain (Km²)`)) %>%
  as.data.frame() %>%
  rename("Frequency" = Freq) %>%
  filter(Frequency != 0) %>%
  rename(`Spatial grain (Km²)` = `Spatial.grain..Km².`) %>%
  mutate(`Spatial grain (Km²)` = fct_relevel(`Spatial grain (Km²)`,c("Local", "Regional", "National", "Global"))) %>%
  ggplot(aes(fill = Trend, y = `Frequency`, x = Metric))+
  geom_bar(position = "stack", stat = "identity")+
  facet_wrap(~ `Spatial grain (Km²)`, nrow = 1, scales = "free_x")+
  ylab("Number of trends")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(face = "bold", color = "black", size = 10))+
  labs(fill = "Trend reported")+
  scale_fill_viridis_d(option = "turbo",
                       begin = .8, end = .5, direction = -1)

## Save the plot
jpeg("figures/supp_Fig1.jpg",
     units = "in",
     res = 1000,
     # paper = "a4r",
     width = 11,
     height = 8.27)

ggdraw()+
  draw_plot(Fig1a_supp, x = 0, y = .5, width = .3, height = .5)+
  draw_plot(Fig1b_supp, x = .3, y = .5, width = .7, height = .5)+
  draw_plot(Fig1c_supp,  x = 0, y = 0, width = 1, height = 0.5)+
  draw_plot_label(label = c("a", "b", "c"), size = 13,
                  x = c(0, 0.29, 0), y = c(1, 1, 0.5))

dev.off()

###############################################
# Create Fig2a with the temporal extent of the articles
jpeg("data/Fig2a.jpg",
     units = "in",
     res = 1000,
     # paper = "a4r",
     width = 11,
     height = 8.27)

tab %>%
  select(Reference_, `Temporal extent (year)`, `Temporal coverage`) %>%
  separate(`Temporal coverage`, c("start", "end"), "-") %>%
  mutate_at(vars("start", "end"), as.numeric) %>%
  filter(!duplicated(Reference_)) %>%
  mutate(coverage = (end-start)+1) %>%
  mutate(Reference_ = fct_reorder(Reference_, coverage)) %>%
  ggplot(aes(y = Reference_, yend = Reference_, x = start, xend = end))+
  geom_segment(size = 2)+
  theme_bw()+
  # theme_classic()+
  ylab("")+
  xlab("")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 15) )

dev.off()


################################################
## Some articles used the same dataset and report the same trend. Here, I take off the articles
## which report the same trend for a metric at a specific spatial scale


### Create the table with no spatial replicates ###

tab_nopseudoreplicates <-
  tab %>%
  ## Papers to take off:
  filter(
    !Reference_ == "Dornelas et al. 2014",
    !Reference_ == "La Sorte 2006",
    !(Reference_ == "Jarzyna and Jetz 2017" & (Metric == "sR" | Metric == "fDiv")),
    !(Reference_ == "La Sorte and Boecklen 2005" & Metric == "sR"),
    !(Reference_ == "Jarzyna and Jetz 2018" & Metric == "sR" & `Spatial grain (Km²)` == "Local"),
    !(Reference_ == "Jarzyna and Jetz 2018" & Metric == "sR" & `Spatial grain (Km²)` == "Regional"),
    !(Reference_ == "Jarzyna and Jetz 2018" & Metric == "tBetaDiv" & `Spatial grain (Km²)` == "Local"),
    !(Reference_ == "Schipper et al. 2016" & Metric == "sR" & `Spatial grain (Km²)` == "Local"),
    !(Reference_ == "Barnagaud et al. 2017" & Metric == "sR" & `Spatial grain (Km²)` == "Local"),
    !(Reference_ == "Chase et al. 2019" & Metric == "sR" & `Spatial grain (Km²)` == "Local"),
    !(Reference_ == "McGill et al. 2015" & `Spatial grain (Km²)` == "Local")
  )

## Create the bar plot without pseudoreplications

Fig3a <-
  table(tab_nopseudoreplicates$`Spatial grain (Km²)`, tab_nopseudoreplicates$Trend) %>%
  as.data.frame() %>%
  rename("Frequency" = Freq,
         "Spatial grain size" = Var1,
         "Trend" = Var2) %>%
  mutate("Spatial grain size" = fct_relevel(`Spatial grain size`,c("Local", "Regional", "National", "Global"))) %>%
  ggplot(aes(fill = Trend, x = `Spatial grain size`, y = Frequency))+
  geom_bar(position = "stack", stat = "identity")+
  ylab("Number of trends")+
  ggtitle("Spatial grain")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x=element_blank())+
  scale_fill_viridis_d(option = "turbo",
                       begin = .8, end = .5, direction = -1)+
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5))

Fig3b <-
  table(tab_nopseudoreplicates %>% select(Metric, Trend)) %>%
  as.data.frame() %>%
  rename("Frequency" = Freq) %>%
  ggplot(aes(fill = Trend, y = `Frequency`, x = Metric))+
  geom_bar(position = "stack", stat = "identity")+
  ylab("Number of trends")+
  ggtitle("Metric")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x=element_blank(),
        axis.title.y = element_blank())+
  scale_fill_viridis_d(option = "turbo",
                       begin = .8, end = .5, direction = -1)+
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5))


Fig3c <-
  tab_nopseudoreplicates %>%
  select(Metric, Trend, `Spatial grain (Km²)`) %>%
  table() %>%
  as.data.frame() %>% rename("Frequency" = Freq) %>%
  filter(Frequency != 0) %>%
  rename(`Spatial grain (Km²)` = `Spatial.grain..Km².`) %>%
  mutate(`Spatial grain (Km²)` = fct_relevel(`Spatial grain (Km²)`,c("Local", "Regional", "National", "Global"))) %>%
  ggplot(aes(fill = Trend, y = `Frequency`, x = Metric))+
  geom_bar(position = "stack", stat = "identity")+
  facet_wrap(~ `Spatial grain (Km²)`, nrow = 1, scales = "free_x")+
  ylab("Number of trends")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(face = "bold", color = "black", size = 10))+
  labs(fill = "Trend reported")+
  scale_fill_viridis_d(option = "turbo",
                       begin = .8, end = .5, direction = -1)

jpeg("figures/Fig3.jpg",
     units = "in",
     res = 1000,
     # paper = "a4r",
     width = 11,
     height = 8.27)

ggdraw()+
  draw_plot(Fig3a, x = 0, y = .5, width = .3, height = .5)+
  draw_plot(Fig3b, x = .3, y = .5, width = .7, height = .5)+
  draw_plot(Fig3c,  x = 0, y = 0, width = 1, height = 0.5)+
  draw_plot_label(label = c("a", "b", "c"), size = 13,
                  x = c(0, 0.29, 0), y = c(1, 1, 0.5))

dev.off()


### Create the table with the notes

tab_notes <- read_excel("data/notes.xlsx")

tab_sup <-
tab_notes %>%
  dplyr::mutate(`Temporal grain (hour)` = as.numeric(`Temporal grain (hour)`),
                `Temporal lag (year)` = as.numeric(`Temporal lag (year)`),
                `Spatial extent (Km²)` = as.numeric(`Spatial extent (Km²)`),
                `Temporal extent (year)` = as.numeric(`Temporal extent (year)`)) %>%
  select(Reference, Metric, `Spatial grain (Km²)`, Note) %>%
  as_tibble() %>%
  group_by(Reference) %>%
  collapse_rows_df(Reference)
