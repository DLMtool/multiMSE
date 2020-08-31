



CheckMPs = FALSE; timelimit = 1; Hist=FALSE; ntrials=50; fracD=0.05; CalcBlow=FALSE;
HZN=2; Bfrac=0.5; AnnualMSY=TRUE; silent=FALSE; PPD=FALSE; parallel=FALSE;
save_name=NULL; checks=FALSE; control=NULL

devtools::load_all()


nsim <- 12



MOM <- multiMSE::Albacore_TwoFleet

MPs <- c('AvC', 'curE')
MOM@interval = c(1,1)

#
#
# Albacore <- DLMtool::Albacore
#
# Albacore@Msd <- Albacore@Ksd <- Albacore@Linfsd <- c(0, 0)
#
# BB_fleet_1 <- DLMtool::IncE_HDom
#
# BB_fleet_1@L5 <- c(45, 50)
# BB_fleet_1@LFS <- c(55, 60)
# BB_fleet_1@Vmaxlen <- c(0.05, 0.2)
# BB_fleet_1@isRel <- FALSE
#
# BB_fleet_1@EffLower[3] <- 0.3
# BB_fleet_1@EffUpper[3] <- 0.4
#
# LL_fleet_2 <- DLMtool::FlatE_NDom
#
# LL_fleet_2@L5 <- c(75, 80)
# LL_fleet_2@LFS <- c(100, 110)
#
# LL_fleet_2@EffLower[3] <- 0.2
# LL_fleet_2@EffUpper[3] <- 0.4
#
# LL_fleet_2@isRel <- FALSE
#
# Stocks <- list(Albacore)
# Fleets <- list(list(BB_fleet_1, LL_fleet_2))
# Obs <- list(list(DLMtool::Precise_Unbiased, DLMtool::Precise_Unbiased))
# Imps <- list(list(DLMtool::Perfect_Imp, DLMtool::Perfect_Imp))
# CatchFrac <- list(matrix(rep(c(0.3, 0.7), each = nsim), nrow = nsim)) # Terminal year catch is 30%-70% ratio baitboat and longline, respectively
#
#
# Albacore_TwoFleet <- new("MOM", Stocks = Stocks, Fleets = Fleets, Obs = Obs, Imps = Imps, CatchFrac = CatchFrac, nsim = nsim)
# usethis::use_data(Albacore_TwoFleet, overwrite = TRUE)
#
#
# MOM <- Albacore_TwoFleet
