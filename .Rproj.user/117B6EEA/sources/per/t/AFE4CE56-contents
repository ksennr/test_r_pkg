context("GetChurnRate")

setup <- function(env = parent.frame()) {
  path <- "C:\\Users\\kilia\\Documents\\R Seminar\\Day 5\\"
  file1 <- "data_customer.csv"
  file2 <- "data_personal.csv"

  data_customer <- fread(paste0(path, file1))
  data_personal <- fread(paste0(path, file2))

  data_merged <- merge(data_customer, data_personal, by.x = "CustomerId", by.y = "CustomerId", all=TRUE)

  data_merged$Exited <- as.factor(data_merged$Exited)
  data_merged$Gender <- as.factor(data_merged$Gender)

  glm_fit <- glm(
    formula = Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary,
    family = "binomial",
    data = data_merged
  )

  data_merged$churn_rate_pred <- predict(glm_fit, data_merged, type = "response")

  return(data_merged)
}

test_that("getChurnRate testsuite", {
  data_merged <- setup()

  expect_equal(get_cust_id(data_merged, 15653251), 0.94059748)
  expect_error(get_cust_id(data_merged, 1))
  expect_lt(max(data_merged$churn_rate_pred), 1)
  expect_gt(min(data_merged$churn_rate_pred), 0)
  expect_lt(min(data_merged$churn_rate_pred), max(data_merged$churn_rate_pred))
})
