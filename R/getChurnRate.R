#' get_max_PurchAmount
#'
#  Description
#' Function that returns the highest PurchAmount for a given date.
#'
#  Detail arguments like a data description
#' @details
#' \code{data} contains the transaction data. The data set must contain a
#'             column labeled "Customer" that allows unique customer identification
#'             and a column labeled "TransDate", indicating the purchase date.
#'             The column "PurchAmount" specifies the total spending per purchase.
#'
#  Arguments that are passed as input to the function
#' @param data a dataset with that has the columns \code{CustomerId} and \code{churn_rate_pred}
#' @param customer_id A customerId within the \code{CustomerId} column
#'
#  Returned values with a description of what the function returns
#' @return The \code{churn_rate_pred} for the customer with the given \code{CustomerId}
#'
#  Examples with a set of example R code on how to use the function
#' @examples
#' data(transactions)
#' get_cust_id(transactions, 15653251)
#'
#  Import packages that are required for using your package
#' @import data.table
#  Careful: some packages have functions with overlapping names. If this is the case,
#           only import specific functions from a package. Here, lubridate and data.table
#           share the function quarter(). To avoid conflicts, only load functions you need
#           from lubridate with @importFrom package function1 function2
#'
#  Include export to make sure roxygen2 knows to create the NAMESPACE file, to make
#  the package accessible to other users
#' @export

get_cust_id <- function(data, customer_id) {

  if (!customer_id %in% data$CustomerId) {
    stop("Data does not contain a your provided 'CustomerId'")
  } else {
    return(data[CustomerId == customer_id,churn_rate_pred])
  }

}
