library(ggplot2)
library(ggpubr)

## reading raw data

DIS <- read.csv("https://raw.githubusercontent.com/derele/Mouse_Eimeria_Field/master/data_input/Mouse_data/HZ22_Dissections.csv")

DISO <- read.csv("https://raw.githubusercontent.com/derele/Mouse_Eimeria_Field/master/data_input/Mouse_data/HZ22_Non_Mus.csv")

TRAP <- read.csv("https://raw.githubusercontent.com/derele/Mouse_Eimeria_Field/master/data_input/Mouse_data/HZ22_Trap.csv")

TRAP$Area <- gsub(" *", "", TRAP$Area)

### localities
table(TRAP$Rodents_caught>0)
table(TRAP$Mus_caught>0)

## trap nights
sum(TRAP$Trap_number, na.rm=TRUE)

sum(TRAP$Rodents_caught, na.rm=TRUE)

sum(TRAP$Mus_caught, na.rm=TRUE)

trapHIST <- ggplot(TRAP, aes(Trap_number)) +
    geom_histogram(color="black") +
    theme_bw()

ggsave("figures/trapHist22.png", trapHIST, width=5, height=4)

rodHIST <- ggplot(TRAP, aes(Rodents_caught)) +
    geom_histogram(color="black") +
    theme_bw()

ggsave("figures/rodHist22.png", rodHIST, width=5, height=4)

rodHISTArea <- ggplot(TRAP, aes(Rodents_caught, fill=Area)) +
    geom_histogram(color="black") +
    theme_bw()

ggsave("figures/rodHistArea22.png", rodHISTArea, width=5, height=4)

table(DIS$Status)

DIS$Status <- gsub(" *", "", DIS$Status)

DIS$Status <- gsub("pregnant", "adult", DIS$Status)

WeiLen <- ggplot(DIS, aes(Body_Length, Body_Weight)) +
    geom_point() +
    stat_smooth(method="lm") +
    stat_regline_equation() + 
    theme_bw()

ggsave("figures/WeiLen.png", WeiLen, width=5, height=4)


WeiLenCol <- ggplot(DIS, aes(Body_Length, Body_Weight,
                             color=Status)) +
    geom_point() +
    stat_smooth(method="lm") +
    stat_regline_equation() + 
    theme_bw()

ggsave("figures/WeiLenCol.png", WeiLenCol, width=5, height=4)


WeiLenSex <- ggplot(DIS, aes(Body_Length, Body_Weight,
                             color=Sex)) +
    geom_point() +
    stat_smooth(method="lm") +
    stat_regline_equation() + 
    theme_bw()

ggsave("figures/WeiLenSex.png", WeiLenSex, width=5, height=4)




SplLen <- ggplot(DIS, aes(Body_Weight, Spleen,
                          color=Sex)) +
    geom_point() +
    stat_smooth(method="lm") +
    stat_regline_equation() + 
    theme_bw()

ggsave("figures/SplSex.png", SplLen, width=5, height=4)


