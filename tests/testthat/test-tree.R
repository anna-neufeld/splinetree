context("Tree testing")

test_that("One coefficient tree", {
  tree1 <- splineTree(BMI~HISP+WHITE+BLACK+HGC_MOTHER+SEX, BMI~AGE, "ID",
                      nlsySample, degree=1, intercept=FALSE, cp=0.005)
  expect_is(tree1, "rpart")
  R2 = projectedR2(tree1)
  expect_true(R2<1)
  expect_true(R2>0)
  tree1k <- splineTree(BMI~HISP+WHITE+BLACK+HGC_MOTHER+SEX, BMI~AGE, "ID",
                       nlsySample, degree=1, df=2, intercept=FALSE, cp=0.005)
  expect_is(tree1k, "rpart")
  expect_is(tree1k$frame, "data.frame")
  expect_true(treeSimilarity(tree1, tree1k) < 1)
})

test_that("Two coefficient tree", {

})


test_that("More Complex Trees", {
  tree2 <- splineTree(BMI~HISP+WHITE+BLACK+HGC_MOTHER+SEX, BMI~AGE, "ID",
                    nlsySample, df=5, degree=2, intercept=TRUE, cp=0.005)
  expect_is(tree2$frame, "data.frame")
  expect_is(tree2, "rpart")
  r21 = yR2(tree2)
  expect_true(r21<1)
  r22= projectedR2(tree2)
  expect_true(r22>0)
  expect_true(r22<1)
  newdata = data.frame(HISP=0, WHITE=0, BLACK=1, HGC_MOTHER=13, SEX=2, AGE=24)
  pred = predictY(tree2, newdata)
  expect_true(pred>0)
})


