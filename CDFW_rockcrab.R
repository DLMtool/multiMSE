# devtools::install_github('DLMtool/multiMSE')

library(multiMSE)

# directory where rock crab OM files are located:
path <- 'G:/Shared drives/BM shared/CDFW_MultiFleet/RockCrabDLM'

# ---- Load Operating Model Components ----
BrownFOM <- XL2OM(file.path(path, 'OMBrownF.xlsx'))
BrownMOM <- XL2OM(file.path(path, 'OMBrownM.xlsx'))

RedFOM <- XL2OM(file.path(path, 'OMRedF.xlsx'))
RedMOM <- XL2OM(file.path(path, 'OMRedM.xlsx'))

YellowFOM <- XL2OM(file.path(path, 'OMYellowF.xlsx'))
YellowMOM <- XL2OM(file.path(path, 'OMYellowM.xlsx'))

# Stocks
BrownFStock <- SubOM(BrownFOM, 'Stock')
BrownFStock@Name <- 'Brown_Female'
BrownMStock <- SubOM(BrownMOM, 'Stock')
BrownFStock@Name <- 'Brown_Male'

RedFStock <- SubOM(RedFOM, 'Stock')
RedFStock@Name <- 'Red_Female'
RedMStock <- SubOM(RedMOM, 'Stock')
RedMStock@Name <- 'Red_Male'

YellowFStock <- SubOM(YellowFOM, 'Stock')
YellowFStock@Name <- 'Yellow_Female'
YellowMStock <- SubOM(YellowMOM, 'Stock')
YellowMStock@Name <- 'Yellow_Male'

# Fleet
BrownFFleet <- SubOM(BrownFOM, 'Fleet')
BrownFFleet@Name <- 'Brown_Fl_F'

BrownMFleet <- SubOM(BrownMOM, 'Fleet')
BrownMFleet@Name <- 'Brown_Fl_M'

RedFFleet <- SubOM(RedFOM, 'Fleet')
RedFFleet@Name <- 'Red_Fl_F'

RedMFleet <- SubOM(RedMOM, 'Fleet')
RedMFleet@Name <- 'Red_Fl_M'

YellowFFleet <- SubOM(YellowFOM, 'Fleet')
YellowFFleet@Name <- 'Yellow_Fl_F'

YellowMFleet <- SubOM(YellowMOM, 'Fleet')
YellowMFleet@Name <- 'Yellow_Fl_M'

# ---- Two-sex Model - Species separate ----
SexPars <- list()
# Stock (Row) spawn from SSB contributed by Stock (Column)
SexPars$SSBfrom <- matrix(c(1,0,
                            1,0),
                          byrow=TRUE,nrow=2)
SexPars$SSBfrom
nsim <- 5

MOM <- new("MOM",
           Stocks = list(RedFStock, RedMStock),
           Fleets = list(
             list(RedFFleet),
             list(RedMFleet)
           ),
           Obs = list(list(DLMtool::Perfect_Info),
                      list(DLMtool::Perfect_Info)
           ),
           Imps = list(list(DLMtool::Perfect_Imp),
                       list(DLMtool::Perfect_Imp)),
           CatchFrac = NULL,
           SexPars=SexPars,
           nsim = nsim)

setup()

MPs_bf<-list(list(c('NFref', 'NFref')),  #  Female
              list(c('curE', 'curE')) #  Male
)

# remove perr_y
# MOM@Stocks[[1]]@Perr <- c(0,0)
# MOM@Stocks[[2]]@Perr <- c(0,0)


RedCrab <- multiMSE::multiMSE(MOM, MPs=MPs_bf)


SB_hist <- apply(RedCrab@SSB_hist[1,2,,,], 2, sum)
SB_proj <- RedCrab@SSB[1,2,1,]

SB <- c(SB_hist, SB_proj)

plot(SB, type="l")
abline(h=RedCrab@OM[[1]][[1]]$SSB0[1], col='red')



# debug - step through runMSE - recruitment being weird?
devtools::load_all()
MPs=MPs_bf
CheckMPs = FALSE; timelimit = 1; Hist=FALSE; ntrials=50; fracD=0.05; CalcBlow=FALSE;
HZN=2; Bfrac=0.5; AnnualMSY=TRUE; silent=FALSE; PPD=FALSE; parallel=FALSE;
save_name=NULL; checks=FALSE; control=NULL


# ---- Management Procedures ----

# 1. Current Management
curE # current Effort (assumes effort stays at the current level (whatever it is))
curE1.5 <- function(x, Data, ...) { # 1.5 times current effort in the future (static)
  rec <- new('Rec')
  rec@Effort <- 1.5
  rec
}
class(curE1.5) <- 'MP'

curE2 <- function(x, Data, ...) { # 2 times current effort in the future (static)
  rec <- new('Rec')
  rec@Effort <- 2
  rec
}
class(curE2) <- 'MP'

# 2. Male only fishery
#                       Fleet 1
#                       MP1    MP2    MP3
MPs_bf<-list( list(c('NFref', 'NFref', 'NFref')),  #  Female
              list(c('curE',  'curE1.5', 'curE2')) #  Male
              )

MOM@maxF <- 10
RedCrab <- multiMSE::multiMSE(MOM, MPs=MPs_bf)


SB_hist <- apply(RedCrab@SSB_hist[1,1,,,], 2, sum)
SB_proj <- RedCrab@SSB[1,1,1,]

SB <- c(SB_hist, SB_proj)

plot(SB, type="l")
abline(h=RedCrab@OM[[1]][[1]]$SSB0[1], col='red')


RedCrab@Effort[1,2,1,,]

plot(RedCrab)


plot(apply(RedCrab@SSB[1,1,,], 2, sum), type="l")
abline(h=RedCrab@OM[[1]][[1]]$SSB0, col='red')

plot(apply(RedCrab@SSB[1,2,,], 2, sum), type="l")


apply(RedCrab@CB_hist[1,1,1,,,], 2, sum)
RedCrab@C[1,1,1,1,]


# 3. Species-Specific Size Limit

# 4. Effort Limit





plot(RedCrab)



# Compare historical





Obs_Stocks <- list(list(Perfect_Info
ImpList <- Perfect_Imp
