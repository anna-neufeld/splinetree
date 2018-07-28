context("Tree testing")

test_that("One coefficient tree", {
  tree1 <- splineTree(BMI~HISP+WHITE+BLACK+HGC_MOTHER+SEX, BMI~AGE, "ID",
                      nlsySample, degree=1, intercept=FALSE, cp=0.005)
  expect_is(tree1, "rpart")
  R2 = R2_projected(tree1)
  expect_true(R2<1)
  expect_true(R2>0)
  stPlots(tree1)
  spaghettiPlot(tree1)
  treeSummary(tree1)
})

test_that("Two coefficient tree", {
  tree1k <- splineTree(BMI~HISP+WHITE+BLACK+HGC_MOTHER+SEX, BMI~AGE, "ID",
                       nlsySample, degree=1, df=2, intercept=FALSE, cp=0.005)
  expect_is(tree1k, "rpart")
  tree1k$frame
  stPlots(tree1k)
  spaghettiPlot(tree1k)
  treeSummary(tree1k)
})


test_that("More Complex Trees", {
  tree2 <- splineTree(BMI~HISP+WHITE+BLACK+HGC_MOTHER+SEX, BMI~AGE, "ID",
                    nlsySample, degree=2, intercept=TRUE, cp=0.005)
  tree2$frame
  stPlots(tree2)
  expect_is(tree2, "rpart")
  r21 = R2_y(tree2)
  expect_true(r21>0)
  r22= R2_projected(tree2)
  expect_true(r22>0)
  expect_true(r22<1)
  newdata = data.frame(HISP=0, WHITE=0, BLACK=1, HGC_MOTHER=13, SEX=2, AGE=24)
  pred = predict_y(tree2, newdata)
  expect_true(pred>0)
  tree3i <- splineTree(BMI~HISP+WHITE+BLACK+HGC_MOTHER+SEX, BMI~AGE, "ID",
                     nlsySample, degree=3, df=5, intercept=TRUE, cp=0.005)
  a=R2_y(tree3i)
  b=R2_projected(tree3i)
  stPlots(tree3i)
})


