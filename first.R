library(readxl)
library(reshape2)
library(ggplot2)
library(scales)
library(cowplot)
theme_set(theme_cowplot())

## import
{
  dep <- readxl::read_excel("data/antibio_conso.xlsx", sheet = "dep")
  reg <- as.data.frame(readxl::read_excel("data/antibio_conso.xlsx", 
                                          sheet = "reg"))
  fra <- as.data.frame(readxl::read_excel("data/antibio_conso.xlsx", 
                                          sheet = "fra"))
  region2019 <- read.csv("data/region2019.csv",
                         colClasses = "character")
}

## colnames
{
  colnames(fra)[3] <- "age"
  DDD <- colnames(fra)[grep("DDD", colnames(fra) )]
  colnames(fra)[grep("nb_pres", colnames(fra) )] <- 
    sub("_", "", colnames(fra)[grep("nb_pres", colnames(fra) )])
  family <- sapply(DDD, function(x) unlist(strsplit(x, split = "_"))[2], 
                   USE.NAMES = FALSE)
  fra$annee <- substr(as.character(fra$annee),3,4)
}

##---- France ----
## All classes
# fra$DDD <- rowSums(fra[, grep("DDD_J01[A-Z]", colnames(fra) )]) 
# fra$nb_pres <- rowSums(fra[, grepl("nb_pres", colnames(fra) )])

ggplot(fra, aes(annee, DDD_J01)) + 
  geom_line() + 
  # scale_x_continuous(breaks = pretty_breaks()) +
  theme(legend.title = element_blank())

ggplot(fra, aes(annee, DDD_J01, 
                group = factor(age),
                color = factor(age) )) + 
  geom_line() + 
  # scale_x_continuous(breaks = pretty_breaks()) +
  theme(legend.title = element_blank())


if (TRUE){
  fra_long <- reshape(fra, idvar = c("annee", "age"),
                    varying = list(
                      grep("DDD", colnames(fra), value = TRUE ),
                      grep("nbpres", colnames(fra), value = TRUE  ) ),
                      v.names = c("DDD", "nbpres"), direction = 'long')
  fra_long$time <- family[fra_long$time]
}

### DDD by family
# fra_ddd_long <- reshape(data = fra[, ],
#                     direction = "long",
#                     idvar = c("annee", "age"),
#                     ## without J01
#                     varying = grep("DDD", colnames(fra), value = TRUE )[-1],
#                     sep = "_"
#                     )
ggplot(fra_long, aes(annee, DDD, 
                group = factor(age),
                color = factor(age) )) + 
  geom_line() + 
  facet_wrap(vars(time), scales = "free_y") +
  theme(legend.title = element_blank())

## pre by family
ggplot(fra_long, aes(annee, nbpres, 
                         group = factor(age),
                         color = factor(age) )) + 
  geom_line() + 
  facet_wrap(vars(time), scales = "free_y") +
  theme(legend.title = element_blank())

### questions
# Les DDD sont définies pour les enfants ?
# Est-ce que DDD_J01 = sum(DDD_J01x) ?
# Est-ce q'une stratification par sexe ne serait aps intéressante ?

- reshape(data = df, direction = "long",
          varying = list(
            grep("morg1.*", names(df),
                 value = TRUE, perl = TRUE),
            grep("morg2.*", names(df),
                 value = TRUE, perl = TRUE)
          )
          ,v.names = c("morg1", "morg2")
)

### reg
{
  colnames(reg)[3] <- "age"
  DDD <- colnames(reg)[grep("DDD", colnames(reg) )]
  colnames(reg)[grep("nb_pres", colnames(reg) )] <- 
    sub("_", "", colnames(reg)[grep("nb_pres", colnames(reg) )])
  family <- sapply(DDD, function(x) unlist(strsplit(x, split = "_"))[2], 
                   USE.NAMES = FALSE)
  reg$annee <- substr(as.character(reg$annee),3,4)
}
reg_long <- reshape(reg, idvar = c("reg", "annee", "age"),
                    varying = list(
                      grep("DDD", colnames(reg), value = TRUE ),
                      grep("nbpres", colnames(reg), value = TRUE  ) ),
                    v.names = c("DDD", "nbpres"), direction = 'long')
reg_long$time <- family[reg_long$time]
head(reg_long)

reg2 <- reg[reg$age == 0,]
reg2$region <- region2019$ncc[match(reg2$reg, region2019$reg)]

## tweak second axis
summary(reg2$DDD_J01 / reg2$nbpres_J01)
coef <- 9

ggplot(reg2, aes(annee, DDD_J01, group = factor(region))) + 
  geom_line() +
  geom_line(aes(y = nbpres_J01 * coef), color = 2)+
  scale_y_continuous(
    # Features of the first axis
    name = "DDJ/1000hab/j",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~. / coef, name="P/1000hab/j")
  ) +
  facet_wrap(vars(region), scales = "free_y") +
  theme(    axis.title.y = element_text(color = "black"),
            axis.title.y.right = element_text(color = "red"),
            legend.title = element_blank())


