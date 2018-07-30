#### Test that forest is working.
context("Forest testing")

test_that("Building Forests", {
  new_forest <- splineForest(BMI~HISP+WHITE+BLACK+HGC_MOTHER+SEX, BMI~AGE, "ID", nlsySample, degree=1, intercept=TRUE, cp=0.001, ntree=3)
  expect_is(new_forest, "list")
  preds_coeffs = predict_coeffs_RF(new_forest, method="all")
  expect_is(preds_coeffs, "data.frame")
  forest_projection_R2(new_forest, method="all")
  forest_Y_R2(new_forest, method="all")
})

# ### Test prediction functions
test_that("Forest Predictions", {
   preds_coeffs <- predict_coeffs_RF(sample_intercept_forest)
   expect_equal(dim(preds_coeffs), c(3, 500))
   preds_y <- predict_y_RF(sample_intercept_forest, method="oob")
   expect_equal(length(preds_y), 8083)
 })

test_that("Forest Projections", {
  ### Test R2 type measure for coefficients (option for oob, itb, all)
  expect_true(forest_projection_R2(sample_intercept_forest, method="itb")>0)
  expect_true(forest_projection_R2(sample_intercept_forest, method="oob")<1)
  expect_true(forest_projection_R2(sample_intercept_forest, method="all")>0)
  expect_true(forest_projection_R2(sample_intercept_forest, method="itb", removeIntercept = FALSE)>0)
  expect_true(forest_projection_R2(sample_intercept_forest, method="oob", removeIntercept = FALSE)<1)
  expect_true(forest_projection_R2(sample_intercept_forest, method="all", removeIntercept = FALSE)>0)
  expect_true(forest_Y_R2(sample_intercept_forest, method="itb")>0)
  expect_true(forest_Y_R2(sample_intercept_forest, method="oob")>0)
  expect_true(forest_Y_R2(sample_intercept_forest, method="all")>0)
  #Yimp = varImp_Y_RF(sample_intercept_forest, method="oob")
  #plot_varimp(Yimp[,3])
  ### Variable importance for coeffs
  Cimp = varImp_coeff_RF(sample_forest, method="oob")
  plot_varimp(Cimp[,3])
  })


