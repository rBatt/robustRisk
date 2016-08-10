

library(vars)
library(rbLib) # github.com/rBatt/rbLib, used for plotting support
library(data.table)
library(trawlData) # github.com/rBatt/trawlData, don't need for data, but required by trawlDiversity and provides some plotting support
library(trawlDiversity) # github.com/rBatt/trawlDiversity, used for cleaned up data


# ===================================
# = Get Data Set and Choose Species =
# ===================================
da <- copy(trawlDiversity::data_all)
sp2p <- c("Gadus morhua", "Squalus acanthias", "Merluccius bilinearis", "Hemitripterus americanus", "Lophius americanus", "Urophycis tenuis", "Clupea harengus", "Scomber scombrus", "Alosa pseudoharengus", "Ammodytes dubius", "Loligo pealeii", "Homarus americanus")
# sp2p <- names(da[reg=='neus', sort(colSums(table(haulid, spp)), decreasing=TRUE)[1:15]])


# ===========================
# = Plot Species Being Used =
# ===========================
par(mfrow=rbLib::auto.mfrow(length(sp2p))); for(i in 1:length(sp2p)){trawlData::sppImg(sp2p[i])}


# =========================================
# = Subset Data Set to Species and Region =
# =========================================
dat <- da[reg=='neus',j={
	hauls_time <- .SD[,list(n_hauls=length(unique(haulid))), by=c("year","season")]
	spp_mass <- .SD[spp%in%sp2p, list(wtcpue=sum(wtcpue, na.rm=TRUE)), by=c("year","season", "spp","common")]
	merged <- merge(hauls_time, spp_mass)
	biomass <- wtcpue/n_hauls # biomass is wtcpue average per haul (including hauls w/o the spp)
	out <- merged[,list(year, season, spp, common, biomass=biomass, btemp=btemp)] 
	out
}]


# ============================
# = Format Data for Analysis =
# ============================
mat0 <- dcast(dat[,list(year,spp,biomass)], year~spp, value.var="biomass", fun.aggregate=sum, na.rm=TRUE)
mat <- copy(mat0)
mat <- scale(as.matrix(mat[,year:=NULL]))
btemp <- dat[,mean(btemp, na.rm=TRUE),by=c("year")][,V1]
exogen <- matrix(btemp, dimnames=list(NULL,c("btemp")))


# =====================
# = Construct Network =
# =====================
robustRisk::build_network(mat, exogen)


# ================
# = Plot Network =
# ================
edge_wt <- c(adj_mat[adj_mat!=0]) # ignore edges that are 0
pdf("../Figures/neus_12spp_graph.pdf", width=5, height=5)
par(mar=c(1,1,1,1), ps=10, cex=1)
plot(var_graph, edge.arrow.size=0.9, edge.width=pmin((abs(edge_wt)),3), vertex.color="white", edge.color=c("red","forestgreen")[(sign(edge_wt)==1)+1], edge.curved=TRUE)
dev.off()




