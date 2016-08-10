#' Print the network
#' Print the network, doesn't show attributes
#' 
#' @param x output from \code{\link{build_network}}
#' 
#' @export
print.robnet <- function(x){
	attributes(x, "model") <- NULL
	print(x)
}