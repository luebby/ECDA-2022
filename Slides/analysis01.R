
# load data:
dat <- read.csv2("Inference.csv")
head(dat)

table(dat$Event)

table(dat$C1.Correct, dat$C2.Correct, dnn=c("C1 correct", "C2 correct"))
prop.table(table(dat$C1.Correct, dat$C2.Correct, dnn=c("C1 correct", "C2 correct")))

mosaicplot(table(dat$C1, dat$C2, dnn=c("first", "second")))
mosaicplot(table(dat$C1.Correct, dat$C2.Correct, dnn=c("C1 correct", "C2 correct")))


# determine counts by event:
tabledat <- cbind.data.frame("event"=sprintf("e%02d",1:10),
                             "c00"=NA_integer_, "c01"=NA_integer_,
                             "c10"=NA_integer_, "c11"=NA_integer_,
                             stringsAsFactors=FALSE)
str(tabledat)
for (i in 1:10) {
  idx <- (dat$Event == paste0("e",i))
  tab <- table(dat[idx,"C1.Correct"], dat[idx,"C2.Correct"],
               dnn=c("C1", "C2"))
  tabledat[i,"c00"] <- tab[1,1]
  tabledat[i,"c01"] <- tab[1,2]
  tabledat[i,"c10"] <- tab[2,1]
  tabledat[i,"c11"] <- tab[2,2]  
}

# show counts:
tabledat

# compare to overall ("marginal") counts:
table(dat[,"C1.Correct"], dat[,"C2.Correct"], dnn=c("C1 correct", "C2 correct"))
# note: probability for 2nd correct answer decreases if first was correct:
prop.table(table(dat[,"C1.Correct"], dat[,"C2.Correct"], dnn=c("C1 correct", "C2 correct")), margin=1)

# compute overall ("marginal") log-OR: 
require("metafor")
escalc(measure="OR", ai=48, bi=85, ci=53, di=68)
# (log-OR is negative)

# determine log-ORs by event:
require("metafor")
es <- escalc(measure="OR",
             ai=c11, bi=c10,
             ci=c01, di=c00,
             slab=event,
             data=tabledat)
es


library("bayesmeta")
# illustrate estimates:
forestplot(es)
# frequentist analysis:
rma(es)
# Bayesian analysis:
bma <- bayesmeta(es, tau.prior=function(t){dhalfnormal(t, scale=0.5)})
forestplot(bma)
