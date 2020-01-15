library(readr)
library(robCompositions)
library(rrcov)?
library(compositions)
library(MASS)
library(HiDimDA)



######################
# Data Preprocessing #
######################

set.seed(123)

df <- read_csv("~/Statistik/coda/raw_data/Forensic_3D_All.csv")

# Mark NAs. Remove unknown Sex rows.
df[df == -9999] <- NA
colnames(df)[4] <- "Sex"
df <- df[df$Sex != "M?",]
df <- df[df$Sex != "unknown",]

# Remove columns with a lot of NA
na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
filter <- na_count < 50
df <- df[,as.vector(filter)]
df <- df[complete.cases(df),]

# Data Preprocessing: Points to Distances.
row_to_points <- function(row) {
  points <- matrix(row[-4:0], ncol = 3, byrow = TRUE)
  return(points)
}
dists <- t(apply(df, 1, function(x) dist(row_to_points(x))))


##################################
# Data Analysis (robCompostions) #
##################################
# Data Preprocessing: Use Pivot Coordinates using "robCompostions".
Z <- pivotCoord(dists)

# LDA and QDA using "rrcov"
# Standard
resLDA <- LdaClassic(Z, df$Sex)
predict(resLDA)

resQDA <- QdaClassic(Z, df$Sex)
predict(resQDA)

# Robust
resrLDA <- Linda(Z, df$Sex)
predict(resrLDA)

resrQDA <- QdaCov(Z, df$Sex)
predict(resrQDA)

# Fisher Discriminant Analysis (WHY IS THIS NOT WORKING)
res <- daFisher(dists, df$Sex, method="robust")

# Appropriate Evaluation of Error Rate using CROSSVALIDATION
res1 <- DACrossVal(Z, factor(df$Sex), TrainAlg = lda, Strfolds=FALSE, kfold = 5, CVrep = 100)
summary(res1[, , "Clerr"])
boxplot(res1[,,"Clerr"], xlab="Error rate", horizontal = TRUE, las =1)

# Compare apparent error rates to LDA without CV
summary(res1[, , "Clerr"])[4,]
1 - diag(predict(resLDA)@ct) / table(df$Sex)


###############################
# Data Analysis (compostions) #
###############################
# Data Preprocessing: Use Acomp Coordinates (???) using "compostions".
comps <- acomp(dists)

# LDA using "MASS"
res = lda(x=data.frame(ilr(comps)), grouping=df$Sex)
res
ilrInv(res$means, orig=comps)  # to make interpretable
V = ilrBase(comps)
rownames(V) = colnames(comps)
t(ilr2clr(t(res$scaling), V=V))
ldahist(predict(res)$x[,1], g=df$Sex)
ggplot(as.data.frame(predict(res)$x), aes(x=LD1, fill=factor(df$Sex))) + geom_histogram()
