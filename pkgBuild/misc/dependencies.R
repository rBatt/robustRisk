update_dependencies <- function(){
	devtools::use_package("data.table", type="Depends") # Basis for handling all data sets
	
	devtools::use_package("trawlData", type="Imports") # Meets basic requirements for data content and format
	devtools::use_package("trawlDiversity", type="Imports")
	devtools::use_package("igraph", type="Imports")
	
	devtools::use_package("rbLib", type="Imports")
	devtools::use_package("stats", type="Imports")
	devtools::use_package("methods", type="Imports")

	
	devtools::use_package("reshape2", type="Suggests")
}