library(dplyr)
library(DLMtool)
library(multiMSE)

SSdir <- 'G:/Shared drives/BM shared/CDFW_MultiFleet/Halibut/SS3/North'

MOM <- multiMSE::SS2MOM(SSdir, nsim = 3)

setup()
Halibut_Hist <- multiMSE::multiMSE(MOM, Hist=TRUE)

# ---- SS3 import ----
replist <- multiMSE:::SS_import(SSdir, silent=TRUE)
mainyrs <- replist$startyr:replist$endyr
nyears <- length(mainyrs)

# ---- Compare N-at-Age in initial year ----
# female
SS <- replist$natage %>% dplyr::filter(Sex==1, Yr==mainyrs[1], `Beg/Mid`=='B') %>%
  dplyr::select(14:38)
Sim <- Halibut_Hist[[1]][[1]]@AtAge$Nage[1,,1]

data.frame(SS=as.numeric(SS[1,]), Sim=Sim) %>% round(2)

plot(as.numeric(SS[1,]), ylim=c(0, max(Sim)))
lines(Sim)


SS <- replist$natage %>% dplyr::filter(Sex==1, Yr==mainyrs[2], `Beg/Mid`=='B') %>%
  dplyr::select(14:38)
Sim <- Halibut_Hist[[1]][[1]]@AtAge$Nage[1,,2]

data.frame(SS=as.numeric(SS[1,]), Sim=Sim) %>% round(2)

plot(as.numeric(SS[1,]), ylim=c(0, max(c(max(SS[1,]),Sim))))
lines(Sim)

# Expected recruiment in year 2
replist$timeseries %>%dplyr::filter(Yr==1980)
replist$recruit %>%dplyr::filter(Yr==1980)

332.346 *exp(-Halibut_Hist[[1]][[1]]@AtAge$N.Mortality[1,1,2]) * MOM@cpars[[1]][[1]]$Perr_y[1, maxage+1]


# ---- Compare Historical Biomass ----
SS_DF <- data.frame(Year=replist$timeseries$Yr,
                    B=replist$timeseries$Bio_all,
                    SSB=replist$timeseries$SpawnBio)
SS_DF <- SS_DF %>% dplyr::filter(Year %in% mainyrs)

Sim_DF <- data.frame(Year=mainyrs,
                     B=Halibut_Hist[[1]][[1]]@TSdata$B[1,] + Halibut_Hist[[2]][[1]]@TSdata$B[1,],
                     SSB=Halibut_Hist[[1]][[1]]@TSdata$SSB[1,])

# Total Biomass
plot(SS_DF$Year, SS_DF$B, ylim=c(0, max(c(SS_DF$B, Sim_DF$B))), type="l")
lines(Sim_DF$Year, Sim_DF$B, col='blue')


# Spawning Biomass
plot(SS_DF$Year, SS_DF$SSB, ylim=c(0, max(c(SS_DF$SSB, Sim_DF$SSB))), type="l")
lines(Sim_DF$Year, Sim_DF$SSB, col='blue')


# ---- Compare Catch by Sex and Fleet -----
nstock <- length(MOM@Stocks)
nfleet <- length(MOM@Fleets[[1]])

List <- list()
count <- 0
sim <- 1
for (s in 1:nstock) {
  for (f in 1:nfleet) {
    count <- count+1
    SS <- MOM@cpars[[s]][[f]]$Data@Cat[1,]
    Sim <- Halibut_Hist[[s]][[f]]@TSdata$Catch[sim,]
    Catch <- data.frame(Source=c(rep('SS', nyears), rep('Sim', nyears)),
                        Catch=c(SS, Sim))
    List[[count]] <- data.frame(Year=rep(mainyrs,2), Catch, Stock=s, Fleet=f)

  }
}
CatchDF <- do.call('rbind', List)

library(ggplot2)

ggplot(CatchDF, aes(x=Year, y=Catch, color=Source)) +
  facet_wrap(Stock~Fleet, scales="free", ncol=5) +
  geom_line() + theme_classic()






# Check N-at-age for year 2 +
SS <- replist$natage %>% dplyr::filter(Sex==1, Yr==mainyrs[2], `Beg/Mid`=='B') %>%
  dplyr::select(14:38)

Sim <- Halibut_Hist[[1]][[1]]@AtAge$Nage[1,,2]

data.frame(SS=as.numeric(SS[1,]), Sim=Sim) %>% round(2)



# ---- Other things ----
Halibut_Hist[[1]][[1]]@Ref$SSB0[1]

replist$SBzero
replist$recruit$SpawnBio[1]
replist$timeseries$SpawnBio[1]

replist$timeseries$SpawnBio[match(max(mainyrs), replist$timeseries$Yr)]/replist$SBzero
replist$timeseries$SpawnBio[match(max(mainyrs), replist$timeseries$Yr)]/replist$recruit$SpawnBio[1]
replist$current_depletion

MOM@Stocks[[1]]@D



season_as_years <- FALSE
if(replist$nseasons == 1 && all(replist$seasduration < 1)) {
  if(!silent) message(paste("Season-as-years detected in SS model. There is one season in the year with duration of", replist$seasduration, "year."))
  season_as_years <- TRUE
  nseas <- 1/replist$seasduration
  if(!silent) message("DLMtool operating model is an annual model. Since the SS model is seasonal, we need to aggregate over seasons.\n")
} else {
  nseas <- replist$nseasons
  if(nseas > 1) {
    if(!silent) message("DLMtool operating model is an annual model. Since the SS model is seasonal, we need to aggregate over seasons.\n")
  }
}



mainyrs <- replist$startyr:replist$endyr
maxage <- 25
seas1_yind_full <- expand.grid(nseas = 1:nseas, true_year = 1:nyears) # Group assessment years to true years
seas1_yind_full$assess_year <- mainyrs
seas1_yind <- which(seas1_yind_full$nseas == 1) # Assessment years that correspond to first season of a true year
replist$recruit$true_Yr <- seas1_yind_full$true_year[match(replist$recruit$Yr, seas1_yind_full$assess_year)]
recruit <- summarise(group_by(replist$recruit, true_Yr), exp_recr = sum(exp_recr), pred_recr = sum(pred_recr)) # Need to sum over season_as_years
hist_dev <- c(rep(1, maxage - 1), recruit$pred_recr[!is.na(recruit$true_Yr)]/recruit$exp_recr[!is.na(recruit$true_Yr)])



Rec_early <- replist$recruit[vapply(c((min(mainyrs)-(Stock@maxage-1)):(min(mainyrs)-1)), match, numeric(1), table = replist$recruit$Yr, nomatch = NA), ]
# Rec_early <- replist$recruit[vapply(c((min(mainyrs)-(Stock@maxage)):(min(mainyrs)-1)), match, numeric(1), table = replist$recruit$Yr, nomatch = NA), ]
Rdev_early <- Rec_early$pred_recr/Rec_early$exp_recr
Rdev_early[is.na(Rdev_early)] <- 1

recs <- c(Rdev_early, Rdev)

Rdev_early[length(Rdev_early)] * MOM@Stocks[[1]]@R0


Rdev_early %>% length()
recs
recs[maxage-1] * MOM@Stocks[[1]]@R0


cbind(hist_dev, c(Rdev_early, Rdev))


# ---- Check if it is correct in DLMtool ----

SSdir <- 'C:/Users/User/Documents/GitHub/DLMDev/Case_Studies/Vermillion_Snapper_GOM_NOAA/data/SS'

OM <- MSEtool::SS2OM(SSdir, nsim = 3)


setup()
Hist <- runMSE(OM, Hist=TRUE)






