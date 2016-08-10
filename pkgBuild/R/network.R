

library(nets)
library(vars)
library(rbLib)
library(trawlData)
library(trawlDiversity)

da <- copy(data_all)
sp2p <- c("Gadus morhua", "Squalus acanthias", "Merluccius bilinearis", "Hemitripterus americanus", "Lophius americanus", "Urophycis tenuis", "Clupea harengus", "Scomber scombrus", "Alosa pseudoharengus", "Ammodytes dubius", "Loligo pealeii", "Homarus americanus")
# sp2p <- names(da[reg=='neus', sort(colSums(table(haulid, spp)), decreasing=TRUE)[1:15]])
par(mfrow=auto.mfrow(length(sp2p))); for(i in 1:length(sp2p)){sppImg(sp2p[i])}

# dat <- da[reg=='neus',j={
# 	u_yr <- unique(year)
#
# 	yr_spp_tbl <- .SD[wtcpue>0 & !is.na(wtcpue), table(year, spp)]
# 	spp_all_yrs <- colnames(yr_spp_tbl)[colSums(yr_spp_tbl>0)==length(u_yr)] # just to make the next table() faster
#
# 	yr_haul_spp_tbl <- .SD[spp%in% spp_all_yrs & wtcpue>0 & !is.na(wtcpue), table(year, haulid, spp)]
# 	spp_all_yrs_20hauls_ind <- apply(apply(yr_haul_spp_tbl>0, c(1,3), sum)>=20, 2, all)
# 	spp_all_yrs_20hauls <- names(spp_all_yrs_20hauls_ind)[spp_all_yrs_20hauls_ind]
#
# 	hauls_time <- .SD[,list(n_hauls=length(unique(haulid))), by=c("year","season")]
# 	spp_mass <- .SD[spp%in%spp_all_yrs_20hauls, list(wtcpue=sum(wtcpue, na.rm=TRUE)), by=c("year","season", "spp","common")]
# 	merged <- merge(hauls_time, spp_mass)
# 	out <- merged[,list(year, season, spp, common, biomass=wtcpue/n_hauls)]
# 	out
# }]


dat <- da[reg=='neus',j={
	hauls_time <- .SD[,list(n_hauls=length(unique(haulid))), by=c("year","season")]
	spp_mass <- .SD[spp%in%sp2p, list(wtcpue=sum(wtcpue, na.rm=TRUE)), by=c("year","season", "spp","common")]
	merged <- merge(hauls_time, spp_mass)
	out <- merged[,list(year, season, spp, common, biomass=wtcpue/n_hauls, btemp=btemp)]
	out
}]

mat0 <- dcast(dat[,list(year,spp,biomass)], year~spp, value.var="biomass", fun.aggregate=sum, na.rm=TRUE)
mat <- copy(mat0)
mat <- scale(as.matrix(mat[,year:=NULL])) # 32 years, 20 species
btemp <- dat[,mean(btemp, na.rm=TRUE),by=c("year")][,V1]
exogen <- matrix(btemp, dimnames=list(NULL,c("btemp")))

build_network(mat, exogen)

edge_wt <- c(adj_mat[adj_mat!=0])
pdf("../Figures/neus_12spp_graph.pdf", width=5, height=5)
par(mar=c(1,1,1,1), ps=10, cex=1)
plot(var_graph, edge.arrow.size=0.9, edge.width=pmin((abs(edge_wt)),3), vertex.color="white", edge.color=c("red","forestgreen")[(sign(edge_wt)==1)+1], edge.curved=TRUE)
dev.off()




