#################################
#### Plotting spiderdiagram #####
#################################


UCC <- read.csv("UCC_composition.csv")
ischia_data <- read.csv("ischia_data.csv",header = T)

lilehfse <- c("Sr","K2O","Rb","Ba","Ta","Nb","Ce","Zr","Hf","Sm","TiO2" ,"Y")
majors <- c("SiO2","TiO2","Al2O3","FeO","MnO","MgO","CaO","Na2O","K2O")
ree <- c("La","Ce","Pr","Nd","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu")
chalco <- c("Sn","As","Pb","Zn","Ag")
all <- as.character(c(lilehfse,majors,ree,chalco))

UCC.sel <- UCC$x
names(UCC.sel) <- UCC$X
UCC.sel <- UCC.sel[all]
# Normalizing
ischia_data.UCC <- data.frame(sweep(as.matrix(ischia_data[all]),MARGIN = 2,UCC.sel,FUN='/'))
ischia_data.UCC$Sample <- ischia_data$Sample
# Wide to long format 
ischia_data.lf = ischia_data.UCC %>% select(c(all,"Sample")) %>% pivot_longer(-Sample,names_to="Element",values_to="value")
ischia_data.lf$serie <- ifelse(ischia_data.lf$Element %in% majors, "A. Major Elements", ifelse(ischia_data.lf$Element %in% lilehfse, "B. LILE HFSE", ifelse(ischia_data.lf$Element %in% ree,"C. Rare Earth Elements","D. Chalcophile Elements")))
# Plotting
ggplot(ischia_data.lf,mapping=aes(x=Element,y=value,colour=Sample))+
 geom_line(aes(group=Sample))+ labs(y="Sample / UCC",x=NULL)+
scale_y_log10(labels=label_number()) +facet_wrap(ischia_data.lf$serie~.,scales ="free")+theme_dark()+theme(legend.position = "bottom",plot.background = "dark")
