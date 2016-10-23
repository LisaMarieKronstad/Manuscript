#### Code to Profile IFN gamma responses at 7 hours UI vs H1 vs H3 (top) then 7 vs 24 hrs with UI values substracted
## 

require(ggplot2)
require(XLConnect)
require(reshape2)
require(stringr)
require(gdata)


###Simplified plot, only UI, H1, H3, no NK only or PMA/I, 7 hours only

setwd("/Users/LisaKronstad/Desktop/Manuscript/")

getwd()

Fig1 <- readWorksheetFromFile("Fig1IFNGsimplified.xlsx",sheet=1)
head (Fig1)
tail(Fig1)
Fig1$Donor <- factor(Fig1$Donor, ordered=TRUE, levels=c("LMK13", "LMK14", "LMK91", "LMK93", "LMK112", "LMK92F", "LMK33", "LMK59", "LMK60"))

Fig1$Timepoint <- factor(Fig1$Timepoint, ordered=TRUE, levels=c("7"))

Fig1$Function <- factor(Fig1$Function,ordered=TRUE, levels=c("Uninfected", "H3N2", "pH1N1"))

Fig1$Condition <- factor(Fig1$Condition, ordered=TRUE, levels=c("M_NK"))

str(Fig1)

unique(Fig1$Donor)

##Time to set the theme to black and white

theme_set(theme_bw())

#time to make a the IFNg plot for Figure 1


compiledplot3 <- ggplot(aes(Function, Value), data=Fig1) +
  geom_boxplot(aes(fill=Condition)) +  ##add scales="free" this will get rid of box
  geom_point(aes(color=Donor),size=3) + 
  scale_shape(solid = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ##theme(axis.line = element_line(colour = "black")) +
  labs(y="Percent Positive") +
  theme(axis.text=element_text(size=10, angle=0)) +
  theme(plot.background = element_rect(fill = "white")) +
  theme(legend.title=element_text(size=30)) + 
  theme(strip.text=element_text(size=20)) + 
  theme(legend.title=element_text(size=30)) +
  theme(legend.text=element_text(size=25)) +
  scale_fill_manual(values=c("gray100", "gray100", "gray100"))
compiledplot3



##Statistical tests

head(Fig1)
Fig1.c <- dcast(Fig1,Donor~Function+Condition+Timepoint,value.var="Value")
str(Fig1.c)

shapiro.test(Fig1.c$Uninfected_M_NK_7)

wilcox.test(Fig1.c$Uninfected_M_NK_7, Fig1.c$pH1N1_M_NK_7,paired=TRUE)
wilcox.test(Fig1.c$Uninfected_M_NK_7, Fig1.c$H3N2_M_NK_7,paired=TRUE)
wilcox.test(Fig1.c$pH1N1_M_NK_7, Fig1.c$H3N2_M_NK_7,paired=TRUE)


##IFNG comparing at 7 vxs 24 hours with UI values substracted (makes bioligcal sense because different time points will
##have different levels of background noise...I think)

setwd("/Users/LisaKronstad/Desktop/Manuscript/")

getwd()

Fig1 <- readWorksheetFromFile("FIG1_IFNG_7_24.xlsx",sheet=1)
head (Fig1)
tail(Fig1)
Fig1$Donor <- factor(Fig1$Donor, ordered=TRUE, levels=c("LMK13", "LMK14", "LMK91", "LMK107", "LMK93", "LMK112", "LMK113",  "LMK91M", "LMK110", "LMK62"))

Fig1$Timepoint <- factor(Fig1$Timepoint, ordered=TRUE, levels=c("7", "24"))

Fig1$Function <- factor(Fig1$Function,ordered=TRUE, levels=c("H3N2", "pH1N1"))

Fig1$Condition <- factor(Fig1$Condition, ordered=TRUE, levels=c("M_NK"))

str(Fig1)

unique(Fig1$Donor)

##Time to set the theme to black and white

theme_set(theme_bw())

#time to make a the IFNg plot for Figure 1


compiledplot5 <- ggplot(aes(Timepoint, Value), data=Fig1) +
  geom_boxplot(aes(fill=Timepoint), alpha=0.5) + 
  scale_fill_manual(values=c("gray100", "gray100")) +
  geom_line(aes(color=Donor, group=Donor),size=1) +
  geom_point(aes(color=Donor), size=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_grid(Condition~Function,scales="free")
compiledplot5
##Statistical test for difference in gamma levels at different times

head(Fig1)
Fig1.c <- dcast(Fig1,Donor~Function+Condition+Timepoint,value.var="Value")
str(Fig1.c)

shapiro.test(Fig1.c$Uninfected_M_NK_7)

wilcox.test(Fig1.c$H3N2_M_NK_7, Fig1.c$H3N2_M_NK_24,paired=TRUE)
wilcox.test(Fig1.c$pH1N1_M_NK_7, Fig1.c$pH1N1_M_NK_24,paired=TRUE)


wilcox.test(Fig1.c$Uninfected_M_NK_7, Fig1.c$H3N2_M_NK_7,paired=TRUE)
wilcox.test(Fig1.c$pH1N1_M_NK_7, Fig1.c$H3N2_M_NK_7,paired=TRUE)

