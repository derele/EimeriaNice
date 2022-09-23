library(tidyr)
library(ggplot2)
library(ggpubr)
library(mdthemes)


SOTA <- read.csv("https://raw.githubusercontent.com/derele/Mouse_Eimeria_Field/master/data_products/SOTA_Data_Product.csv")

Alice <- read.csv("https://raw.githubusercontent.com/derele/Mouse_Eimeria_Field/master/data_input/MiceTable_fullEimeriaInfos_2014to2017.csv")


as_tibble(SOTA) %>%
ggplot(aes(delta_ct_cewe_MminusE, OPG)) +
    geom_point() +
    scale_y_log10() + 
    geom_smooth()


dplyr::filter(SOTA, OPG>0) %>%
ggplot(aes(FEC_Eim_Ct, OPG)) +
    scale_y_log10() +
    scale_x_reverse() +
    geom_point() +
    geom_smooth(method="lm") +
    stat_regline_equation(label.x=-20, label.y=2) + 
    theme_minimal()

### MC.Eimeria.FEC is always FALSE or NA

dplyr::filter(Alice, OPG>0) %>%
    ggplot(aes(OPG, delta_ct_cewe_MminusE)) +
    scale_x_log10() +
    labs(x="Oocysts per gram feces",
         y="delta CT (Mouse DNA - *Eimeria* DNA)")+
    geom_point() +
    geom_smooth(method="lm") +
    theme_minimal() +
    theme(axis.title.y = ggtext::element_markdown())-> BadCor

ggsave("figures/BadCorOOtissue.png", width=5.5, height=4, bg="white")


summary(lm(delta_ct_cewe_MminusE ~ OPG, data=Alice))
