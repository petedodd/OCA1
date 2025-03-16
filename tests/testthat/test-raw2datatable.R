test_that("raw2datatable works", {
  
  

    df <- data.frame(
      cbind(c(1,2),matrix(rnorm(8),nrow = 2))
  )
  
  input_array <- as.matrix(df) #converting to a matrix.
  colnames(input_array) <- c("t", paste0("State[", 1:4, ",1,1,1,1,1,1]"))
  

  result <- raw2datatable(input_array)
  expect_s3_class(result, "data.table")
  expect_equal(ncol(result), 10)
  expect_equal(colnames(result), c("t","value","state","AgeGrp","sex","natcat","risk","post","strain","prot"))

})

test_that("raw2datatable converts data types correctly", {
  
  
  df <- data.frame(
    cbind(c(1,2),matrix(rnorm(8),nrow = 2))
  )
  
  input_array <- as.matrix(df) #converting to a matrix.
  colnames(input_array) <- c("t", paste0("State[", 1:4, ",1,1,1,1,1,1]"))
  
  
  result <- raw2datatable(input_array)
  
  expect_type(result$t, "double")
  expect_type(result$state, "character")
  expect_true(is.factor(result$AgeGrp))
  expect_true(is.factor(result$sex))
  expect_equal(levels(result$sex), c("M","F"))
  
  expect_type(result$natcat, "integer")
  expect_type(result$risk, "integer")
  expect_type(result$post, "integer")
})


test_that("raw2datatable does the right thing if not given a correct object",{
  
  ans <- NULL
  expect_error(raw2datatable(ans), "Incorrect data type for conversion")
  
  ans <- 1:10
  expect_error(raw2datatable(ans), "Incorrect data type for conversion")
  

})


test_that("raw2datatable does the right thing if columns are not formatted properly",{
 
  ans <- as.matrix(data.frame(A=1:10,B=1:10))
  expect_error(raw2datatable(ans), "State information not encoded correctly")
  

  df <- data.frame(
    cbind(c(1,2),matrix(rnorm(8),nrow = 2))
  )
  
  input_array <- as.matrix(df) #converting to a matrix.
  colnames(input_array) <- c("F", paste0("State[", 1:4, ",1,1,1,1,1,1]"))
  
  expect_error(raw2datatable(input_array), "First column to be converted must be called 't'")
  
  ## the [] not found
  colnames(input_array) <- c("t", paste0("State", 1:4, ",1,1,1,1,1,1"))
  expect_error(raw2datatable(input_array), "State information not encoded correctly")
  
  ## wrong number of variables
  colnames(input_array) <- c("t", paste0("State[", 1:4, ",1,1,1,1,1]"))
  expect_error(raw2datatable(input_array), "Incorrect number of variables found in object")
  
  
})
