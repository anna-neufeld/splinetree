#big_forest <- splineForest(BMI~HISP+WHITE+BLACK+HGC_MOTHER+HGC_FATHER+SEX+Num_sibs+Lib+News+Mag, BMI~AGE, "ID", nlsySample, degree=1, df=2, intercept=FALSE, cp=0.001, ntree=20)
#big_intercept_forest <- splineForest(BMI~HISP+WHITE+BLACK+HGC_FATHER+Num_sibs+HGC_MOTHER+SEX+News+Mag+Lib, BMI~AGE, "ID", nlsySample, degree=1, df=3, intercept=TRUE, cp=0.001, ntree=20)

#save(big_forest, file="big_forest.RData")
#save(big_intercept_forest, file="big_intercept_forest.RData")

#load("big_forest.RData")
#load("big_intercept_forest.RData")
#load("Big_forest_intercept_importance.RData")

#Yimp = varImp_Y_RF(big_intercept_forest, method="all")
#save(Yimp, file="Big_forest_intercept_importance.RData")
#plot_varimp(Yimp[,3])

### Variable importance for coeffs
#Cimp = varImp_coeff_RF(big_intercept_forest, method="all")
#plot_varimp(Cimp[,3])
