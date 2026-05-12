setwd("/home/eric/Desktop/icp/parcial2/")

file <- "ministros2024ca.csv"
d <- read.csv(file = file, stringsAsFactors = FALSE, fileEncoding="utf-8")
## ## or from clipboard
## d <- read.delim("clipboard")
d[1,]
str(d)
colnames(d)

# make numeric
sel <- grep("CAND[.]*|nul|tot|NUMERO_PERSONAS_VOTARON|RECUADROS_NO_UTILIZADOS|lisnom", colnames(d))
v <- d[,sel]
v[1,]
for (i in 1:ncol(v)){
    v[,i] <- as.numeric(v[,i])
}
v[is.na(v)] <- 0
d[,sel] <- v

head(d)
str(d)
colnames(d)

# consolidate edon votes
for (i in sel){
    d[,i] <- ave(d[,i], as.factor(d$edon), FUN=function(x) sum(x, na.rm=TRUE))
}
head(d)

# drop redundant obs
d <- d[duplicated(d$edon)==FALSE,]

dim(d)
colnames(d)

## add edo
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
source( paste(pth, "edo2edon.r", sep = "/") )
rm(pth)
d$edo <- edon2edo(d$edon)
d$estado <- edon2estado(d$edon)

file2 <- "~/Downloads/tmpe.csv"
write.csv(d, file = file2, row.names = FALSE)

## analysis
setwd("/home/eric/Desktop/icp/parcial2/")
d <- read.csv("ministros2024edo.csv")
d[1,]

## subset female
self <- grep("CAND.*f|total.f", colnames(d))
## subset male
selm <- grep("CAND.*m|total.m", colnames(d))
f <- d[, -selm]
m <- d[, -self]




