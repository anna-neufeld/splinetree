#### Test that forest is working.
context("Forest testing")

test_that("Building Forests", {
  set.seed(123)

  ### Just to have forest tests run a little faster, reducing dataset size
  nlsySample_small <- nlsySample[nlsySample$ID %in% sample(nlsySample$ID, 500),]
  sample_forest <- splineForest(BMI~HISP+WHITE+BLACK+HGC_MOTHER+HGC_FATHER+SEX+Num_sibs+Lib+News+Mag, BMI~AGE, "ID", nlsySample, degree=1, df=2, intercept=FALSE, cp=0.001, ntree=20)
  forest <- splineForest(BMI~HISP+WHITE+BLACK+HGC_FATHER+Num_sibs+HGC_MOTHER+SEX+News+Mag+Lib, BMI~AGE, "ID", nlsySample, degree=1, df=3, intercept=TRUE, cp=0.001, ntree=20)
  new_forest <- splineForest(BMI~HISP+WHITE+BLACK+HGC_MOTHER+SEX, BMI~AGE, "ID", nlsySample, degree=1, intercept=TRUE, cp=0.001, ntree=5, prob=2/3)
  expect_is(new_forest, "list")
  preds_coeffs = predictCoeffsForest(new_forest, method="all")
  expect_is(preds_coeffs, "data.frame")
  projectedR2Forest(new_forest, method="all")
  yR2Forest(new_forest, method="all")
})

# ### Test prediction functions
test_that("Forest Predictions", {
   preds_coeffs <- predictCoeffsForest(forest)
   expect_equal(dim(preds_coeffs), c(3, 1000))
   preds_y <- predictYForest(forest, method="oob")
   expect_equal(length(preds_y), 16126)
 })

test_that("Forest Projections", {
  ### Test R2 type measure for coefficients (option for oob, itb, all)
  expect_true(projectedR2Forest(forest, method="itb")>0)
  expect_true(projectedR2Forest(forest, method="oob")<1)
  expect_true(projectedR2Forest(forest, method="all")>0)
  expect_true(projectedR2Forest(forest, method="itb", removeIntercept = FALSE)>0)
  expect_true(projectedR2Forest(forest, method="oob", removeIntercept = FALSE)<1)
  expect_true(projectedR2Forest(forest, method="all", removeIntercept = FALSE)>0)
  expect_true(yR2Forest(forest, method="itb")>0)
  expect_true(yR2Forest(forest, method="oob")>0)
  expect_true(yR2Forest(forest, method="all")>0)
  Cimp = varImpCoeff(forest, method="oob")
  plotImp(Cimp[,3])
  })


