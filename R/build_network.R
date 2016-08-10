#' Build a network
#' Builds a network from time series data using vector autoregression
#' 
#' @param X matrix with columns for species and each row is a time-ordered observation
#' @param exogen matrix of exogenous variables with the same number of rows as \code{X}
#' 
#' @return
#' A list with two elements. The first element is the \code{graph}, an iGraph object. The second element is a length-3 list containing the adjacency matrix (\code{adj_mat}), the p-values for that matrix (\code{adj_mat_p}), and the vector autoregression model from \code{vars::VAR} (\code{var_mod}).
#' 
#' @export
build_network <- function(X, exogen){
	requireNamespace("igraph", quietly=TRUE)
	requireNamespace("vars", quietly=TRUE)

	cn <- colnames(X)
	spp_vars <- paste(make.names(cn), "l1", sep=".")

	var_mod <- vars::VAR(X, exogen=exogen)
	adj_mat_p0 <- as.matrix(as.data.frame(lapply(coef(var_mod), function(x)x[,'Pr(>|t|)'])))
	adj_mat0 <- as.matrix(as.data.frame(lapply(coef(var_mod), function(x)x[,'Estimate'])))
	adj_mat_p <- adj_mat_p0[spp_vars,]
	U_mat <- adj_mat0[!rownames(adj_mat0)%in%spp_vars,]
	adj_mat <- adj_mat0[spp_vars,]*(adj_mat_p<=0.1)

	var_graph <- igraph::graph_from_adjacency_matrix(adj_mat, mode="directed", weighted=TRUE)
	
	out <- list(graph=var_graph)
	attr(out, "model") <- list(adj_mat=adj_mat, adj_mat_p=adj_mat_p, var_mod=var_mod)
	class(out) <- "robnet"
	
	return(out)
}