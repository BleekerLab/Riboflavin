BiocManager::install('mixOmics')
library(mixOmics)


peak_data<-read.csv("List_Halle_peak_pos_selection_nontrans.csv", header=T, sep = ";", dec = ",")
group<-read.csv("Peak_list_grouping_selection.csv", header=T, sep = ";", dec = ",")


X <- as.matrix(peak_data)

Y <- as.factor(group[, 2])


list.keepX <- c(seq(10, 50, 10))
set.seed(2543) # for reproducibility here,
# to speed up the computational time, consider the cpu argument
# take ~ 4 min to run
tune.splsda <- tune.splsda(X, Y, ncomp = 4, validation = 'Mfold', folds = 5,
                           progressBar = FALSE, dist = 'max.dist',
                           test.keepX = list.keepX, nrepeat = 10) #nrepeat 50-100 for better estimate
# tune.splsda.srbct  #the various outputs

tune.splsda$choice.keepX
tune.splsda$choice.ncomp$ncomp

choice.ncomp <- tune.splsda$choice.ncomp$ncomp
choice.keepX <- tune.splsda$choice.keepX[1:choice.ncomp]

## sPLS-DA function
splsda.res <- splsda(X, Y, ncomp = choice.ncomp, keepX = choice.keepX)
splsda.res <- splsda(X, Y, ncomp = 2, keepX = choice.keepX)
perf.splsda <- perf(splsda.res, validation = "Mfold", folds = 5, 
                    progressBar = FALSE, auc = TRUE, nrepeat = 10) 

perf.splsda$error.rate

##selectVar(splsda.res, comp = 1)$value

# here we match the selected variables to the stable features
ind.match = match(selectVar(splsda.res, comp = 1)$name, 
                  names(perf.splsda$features$stable[[1]]))
#extract the frequency of selection of those selected variables
Freq = as.numeric(perf.splsda$features$stable[[1]][ind.match])

data.frame(selectVar(splsda.res, comp = 1)$value, Freq)

plotLoadings(splsda.res, comp = 1, title = 'Loadings on comp 1', 
             contrib = 'max', method = 'mean')

background = background.predict(splsda.res, comp.predicted = 2, dist = "max.dist" )

plotIndiv(splsda.res, ind.names = Y, background = background, legend = TRUE, ellipse =TRUE)
