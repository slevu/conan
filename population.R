##---- population counts ----
## population counts by stratification sex * age * region
## use insee pop counts, as of 1/1/2020
## https://www.insee.fr/fr/statistiques/1893198
# https://www.insee.fr/fr/information/3720946

FNCODEDEP <- "data/departement2019.csv"
FNPOPDEP <- "data/estim-pop-dep-sexe-aq-1975-2020.xls"
FNREG <- "data/region2019.csv"

code_dep <- read.csv(FNCODEDEP, colClasses = "character",
                              encoding = "UTF-8")
region2019 <- read.csv(FNREG, colClasses = "character",
                       encoding = "UTF-8")

## add abbreviated regions
# code_region <- readRD S("data/code_region.rds")
abb <- c(GUA = "01", MAR = "02", GUY = "03", RUN = "04", MAY = "06",
         IDF = "11", CVL = "24", BFC = "27", NOR = "28", HDF = "32",
         GES = "44", PDL = "52", BRE = "53", NAQ = "75", OCC = "76", 
         ARA = "84", PAC = "93", COR = "94")
region2019$abb <- names(abb)[order(abb)]
# saveRDS(region2019, "data/region2019.rds")

##---- population
## FNPOPDEP: "A5:W109" ensemble , hommes : "x5:AR109" , femmes : "AS5:BL26"

x <- as.data.frame(
  readxl::read_excel(FNPOPDEP,
                     sheet = "2020", range = "A5:BM109")
  )
## get rid of subtotal rows
x <- x[ -grep("France métro|DOM", x[,1]),
                -grep("Total", colnames(x))]

## columns 3:22 = overall; 23:42 = male; 43:62 = female
## check ensemble = hommes + femmes
if ( !identical( sum(x[, 3:22]), sum(x[, 23:62]) ) ) warning("hommes + femmes != total") # 67063703



# colnames(insee)[1] <- "REG"
.i <- data.frame(dep = rep(x[,1], 2), # dep
                 sexe = factor(rep(1:2, each = nrow(x)),
                        levels = 1:2,
                        labels = c("Homme", "Femme")), # sex
                 rbind(as.matrix(x[, 23:42]),
                       as.matrix(x[, 43:62]) )
)
# str(.i)
rownames(.i) <- NULL
.i$reg <- factor(code_dep[match(.i$dep, .i$dep), "reg"])

##---- ageband ----
.ageband <- function(mi = 0, ma = 95, n = 5){
  a <- seq(mi, ma, n)
  b <- c(paste0("-", a[-1]-1), "+")
  # print(length(a))
  paste0(a, b)
}

df <-  .i[,-(1:2)]
colnames(df) <- .ageband(0, 95, 5)

## regroup age band with new boundaries
## assuming taht age band are in the form "0-4", "5-9", etc.
reagba2 <- function(df, cuts = c(4, 14, 34, 54, 64, 74, 84),
                    lbl = c("0-4", "5-14","15-34",
                            "35-54", "55-64", "65-74",
                            "75-84", "85+")){
  x <- colnames(df)
  lx <- 1:length(x)
  ## current upper bounds
  y <- sapply(x, function(a) as.numeric( unlist(strsplit(a, "-"))[2] ) )
  ## where to bin
  z <- match(cuts, y)
  if ( any(is.na(z)) ) stop("boundaries not found")
  z <- c(z, length(x)) ## right most columns
  ## initialize
  l <- vector(mode = "list", length = length(z))
  ## list of columns to bin
  for (i in 1:length(z)){
    l[[i]] <-  lx[ lx <= z[i] & lx > ifelse(i > 1, z[i-1], 0) ]
  }
  df2 <- data.frame(
    lapply(setNames(l, lbl), function(k){
      # print(k)
      if(length(k) == 1) df[,k] else rowSums(df[,k])
    }),
    stringsAsFactors = TRUE,
    check.names = FALSE
  )
  df2
}

pop0 <- cbind(.i[,c("dep", "reg", "sexe")],
                   reagba2(df = df,
                           cuts = c(4, 14, 34, 54, 64, 74, 84))
              )
# check
sum(df)
sum(pop0[,-(1:2)]) # 67063703
identical(rowSums(df), rowSums(pop0[,-(1:2)]))

## save
saveRDS(pop0, "data/pop2019.rds")
