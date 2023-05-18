#' Implements simple linear regression by gradient descent
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export

slr_gd <- function(dat, response, explanatory){

  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `simple_linear_regression`

    # Filters numeric variables
    vars_numeric <- sapply(dat, is.numeric)
    data_numeric <- dat[, vars_numeric]

    # Extract the response/explanatory variables
    y <- data_numeric[, response]
    X <- data_numeric[, explanatory]

    # Initializes intercept/slope/learning rate/max_interations variables
    intercept <- 0
    slope <- 0
    learning_rate <- 0.01
    max_interations <- 1000

    # Create dataframe thats gonna be returned
    coefficients <- data.frame(iteration = integer(max_interations),
                               slope = double(max_interations),
                               intercept = double(max_interations))

    # Gradient descent iterations
    for (iteration in 1:max_interations) {

      # predictions
      y_pred <- slope + intercept * X

      # gradients
      gradient_slope <- (1 / length(y)) * sum(y_pred - y)
      gradient_intercept <- (1 / length(y)) * sum((y_pred - y) * X)

      # Update
      slope <- slope - learning_rate * gradient_slope
      intercept <- intercept - learning_rate * gradient_intercept

      # Stores into dataframe
      coefficients[iteration, ] <- c(iteration, slope, intercept)
      results <- coefficients
    }

    return(results)
    #Refrenced Chat GPT
  }




#' Implements linear regression with many predictors by gradient descent
#'
#' This function computes coefficients for multiple regression by gradient descent
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export

mlr_gd <- function(dat, response) {

  # Filters numeric variables
  numeric_vars <- sapply(dat, is.numeric)
  data_numeric <- dat[, numeric_vars]

  # Extract response/explanatory variables
  y <- data_numeric[, response]
  X <- data_numeric[, !colnames(data_numeric) %in% response]

  # Initializes coefficients/learning_rate/max_interations rate variables
  num_explanatory_vars <- ncol(X)
  coefficients <- c(rep(0, num_explanatory_vars + 1))
  learning_rate <- 0.01
  max_interations <- 1000

  # Create dataframe thats gonna be returned
  coefficients_store <- data.frame(iteration = integer(max_interations))

  for (i in 1:(num_explanatory_vars + 1)) {
    coefficients_store[paste0("theta", i - 1)] <- double(max_interations)
  }

  # Gradient descent iterations
  for (iteration in 1:max_interations) {
    # Add intercept variable to the explanatory variables
    X_intercept <- cbind(1, X)

    # predictions
    y_pred <- sum(coefficients * X_intercept)

    # gradients
    gradients <- (1 / length(y)) * c(sum(y_pred - y), colSums((y_pred - y) * X_intercept))

    # Update
    coefficients <- coefficients - learning_rate * gradients

    # Store into dataframe
    coefficients_store[iteration, ] <- c(iteration, coefficients)
    results <- coefficients_store
  }

  return(results)
  #Refrenced Chat GPT
}





#' Implements linear regression with many predictors by matrix decomposition
#'
#' This function computes coefficients for multiple regression by QR matrix decomposition
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export

mlr_qr <- function(dat, response) {



  ### Compute coefficients by QR decomposition
  ### Return a data frame of the same form as in the `multiple_linear_regression`

    # Filter numeric variables
    vars_numeric <- sapply(dat, is.numeric)
    data_numeric <- dat[, vars_numeric]

    # Extract response/explanatory variables
    y <- data_numeric[, response]
    X <- data_numeric[, !colnames(data_numeric) %in% response]

    # Intercept Col
    X <- cbind(1, X)

    # QR decomposition
    qr_decomp <- qr(X)

    # Computes coefficients
    coefficients <- qr.coef(qr_decomp, y)

    # Makes dataframe that we are returning
    coefficients_store <- data.frame(coefficients = coefficients)
    results <- coefficients_store


  return(results)
  #Refrenced Chat GPT
  }
