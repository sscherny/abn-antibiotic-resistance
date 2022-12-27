sort(table(data_wide$Bacterium))
tmp <- data_wide[(data_wide$Bacterium=="klebsiella pneumoniae"),]
sort(table(tmp$Sample.Location))
#sample too small for blood, wound, sputum, etc
bact <- tmp[(tmp$Sample.Location=="urine"),]

sorted.vars <- sort(colSums(!is.na(bact[drug.vars])), decreasing = T)
plot(sorted.vars)
sorted.vars

# chose drugs based on scree plot
evars.all <- sort(names(sorted.vars)[1:14])

resist <- lapply(bact[evars.all], function(x) {
  return(prop.table(table(x)))
})
resist

resist.2 <- sapply(resist, "[[", 2)
# Resistance frequencies of drugs with most data
resist.2

# keep only if resistance >.03 or <.97
evars.reduced <- names(resist.2[resist.2 > .03 & resist.2 < .97])
evars.reduced

#polycormat <- hetcor(bact[evars.reduced], use="pairwise.complete.obs")
#polycormat
#round(polycormat[[1]],2)
#corvec <- reshape2::melt(polycormat[[1]])
#corvec[(corvec$value > .70 & corvec$value != 1),]

# drop CXM, LEX
# drop CIP
# drop CAZ
# drop AMC - small sample and correlated with TZP

evars <- c("Age","Sex",
           "Immunosuppression","Dementia","Diabetes","CRF",
           "Is.cephalosporin","Is.betalactam",
           "Is.aminoglycoside","Is.fluoroquinolone",
           "Is.other",
           "Days.Hospitalized","Nosocomial", 
               "Polymicrobial",
           "GEN","OFX","SXT","TZP")

#polycormat <- hetcor(bact[evars], use="pairwise.complete.obs")
#polycormat
#round(polycormat[[1]],2)

mydists<-list(Age="gaussian", Sex="binomial",
              Immunosuppression="binomial",Dementia="binomial",Diabetes="binomial",
              CRF="binomial",
              Is.cephalosporin="binomial",Is.betalactam="binomial",
              Is.aminoglycoside="binomial",Is.fluoroquinolone="binomial",
              Is.other="binomial",
              Days.Hospitalized="gaussian", Nosocomial="binomial",
            Polymicrobial="binomial",
             GEN="binomial", OFX="binomial",
             SXT="binomial", TZP="binomial")

banned<-matrix(c(0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, # age
                 1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, # sex
                 0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1, # Immuno
                 0,0,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1, # Dementia
                 0,0,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1, # Diabetes
                 0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1, # CRF
                 0,0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1, # Drug fam
                 0,0,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1, #
                 0,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1, #
                 0,0,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1, #
                 0,0,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1, #
                 0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0, # Days hospitalized
                 0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1, # Nosocomial
                 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, # Polymicrobial
                 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, # resistance
                 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),byrow=T,ncol=length(evars))
