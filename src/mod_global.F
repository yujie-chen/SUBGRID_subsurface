MODULE GLOBAL
   USE PARAM
   IMPLICIT NONE

   SAVE

!MPI variables
# if defined (PARALLEL)
   INTEGER :: myid,ier
   INTEGER :: comm2d
   INTEGER :: n_west, n_east, n_suth, n_nrth
   INTEGER :: npx,npy
   INTEGER :: ndims=2
   INTEGER :: NumberProcessor
   INTEGER, DIMENSION(2) :: dims, coords
   LOGICAL, DIMENSION(2) :: periods
   LOGICAL :: reorder = .true.
# endif
   INTEGER :: px,py


! define parameters
    CHARACTER(LEN=80) TITLE
    CHARACTER(LEN=80) DEPTH_FILE
    CHARACTER(LEN=80) ETA_FILE
    CHARACTER(LEN=80) U_FILE
    CHARACTER(LEN=80) V_FILE
    CHARACTER(LEN=80) MASK_FILE
    CHARACTER(LEN=80) Coriolis_FILE
    CHARACTER(LEN=80) OBSTACLE_FILE
    CHARACTER(LEN=80) RESULT_FOLDER
    CHARACTER(LEN=80) STATIONS_FILE
    CHARACTER(LEN=80) INACTIVE_PNT_FILE
    CHARACTER(LEN=80) TMP_FileName
    LOGICAL :: NO_MASK_FILE = .TRUE.
    LOGICAL :: ADI = .FALSE.
    LOGICAL :: HOPSCOTCH = .TRUE.
    LOGICAL :: IMPLICIT = .TRUE.
    LOGICAL :: SUBGRID_UV_CORRECTION = .TRUE.
    LOGICAL :: CONSTRUCT_SUBGRID_UV = .TRUE.
    CHARACTER(LEN=14)::FORMAT_LEN
    CHARACTER(LEN=14)::FORMAT_LEN_SUB

! some physical and numerical variables
    INTEGER :: Nonlinear = 0
    REAL(SP) :: MinDepth=0.001_SP
    REAL(SP) :: MinDepFric=0.001_SP
    REAL(SP) :: CFL=0.15_SP
    REAL(SP) :: FroudeCap=10.0_SP
    REAL(SP) :: SWE_ETA_DEP=0.7_SP
    REAL(SP) :: NU=0.02_SP
                         REAL(SP) :: MaxDepth

! some global variables
! Mloc1=Mloc+1, Nloc1=Nloc+1
    INTEGER :: Mglob,Nglob,Mloc,Nloc,Mloc1,Nloc1
    INTEGER, PARAMETER :: Nghost = 1
    INTEGER :: Ibeg,Iend,Jbeg,Jend,Iend1,Jend1
    INTEGER :: MMglob,NNglob,MMloc,NNloc,IIbeg,IIend,JJbeg,JJend

    INTEGER :: BIN0,BIN1

    REAL(SP):: DX,DY,DX2,DY2,DXDY
    REAL(SP) :: dt_over_dx2,dt_over_dy2,dt_over_dx,dt_over_dy

    LOGICAL :: PERIODIC=.FALSE.
    LOGICAL :: HOT_START=.FALSE.
    LOGICAL :: CONSTANT_DT

    REAL(SP)::  DT_FIXED,DT,TIME,TOTAL_TIME,PLOT_INTV,PLOT_COUNT,&
          SCREEN_INTV,SCREEN_COUNT
    REAL(SP):: HOTSTART_INTV,HOTSTART_COUNT
    INTEGER :: icount=0 ! for output file number
    INTEGER :: icount_stations=0 ! for output station
    INTEGER :: OUTPUT_RES = 1
    INTEGER :: icount_hotstart=0
    INTEGER :: FileNumber_HOTSTART
    INTEGER :: ICOUNT_ECO=0 ! for ecology
    INTEGER :: ICOUNT_TAU=0


! some local variables
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: &
        A_u, A_v, B, C, Eta, Eta0, &
        U, V, P, Q, PQ, SourceX, SourceY, &
        Int2Flo, tmp4preview, EtaScreen

! depth H=Eta+Depth,
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: H,H0,H_u,H_v
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: ETA_OVER_DT

! wetting and drying
    INTEGER,DIMENSION(:,:),ALLOCATABLE :: &
        MASK, MASKu, MASKv, MASK_STRUC
    ! morphology part
    REAL(SP),DIMENSION(:,:,:,:),ALLOCATABLE :: MASK_DRY
    LOGICAL :: OBSTACLE
    LOGICAL :: INACTIVE_PNT

! Coriolis
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: Cori

! friction
    LOGICAL :: SUBGRID_CD_FIXED = .FALSE.
    REAL(SP):: Cd_fixed, Manning, FrcU, FrcV
    REAL(SP):: MinCd = 0.00001_SP
    REAL(SP):: MaxCd = 6.25_SP
    CHARACTER(LEN=80) MANNING_TYPE,MANNING_FILE,MANNINGSUBGRID_FILE
    CHARACTER(LEN=80) CD_FILE
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: ManningVary,Cdu,Cdv,TotResistFX,TotResistFY
    REAL(SP),DIMENSION(:,:,:,:),ALLOCATABLE :: TauSubGrid
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: TauSubGridOut
! boundary condition
# if defined(COUPLING)
    CHARACTER(LEN=80) COUPLING_FILE
    INTEGER :: icount_coupling = 1
    INTEGER :: N_COUPLING_EAST, N_COUPLING_WEST,&
         N_COUPLING_SOUTH, N_COUPLING_NORTH,&
         N_COUPLING_DATA,&
         J_START_EAST,  J_START_WEST,&
         I_START_SOUTH, I_START_NORTH
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: U_COUPLING_EAST,V_COUPLING_EAST,Z_COUPLING_EAST,&
                       U_COUPLING_WEST,V_COUPLING_WEST,Z_COUPLING_WEST,&
                       U_COUPLING_SOUTH,V_COUPLING_SOUTH,Z_COUPLING_SOUTH,&
                       U_COUPLING_NORTH,V_COUPLING_NORTH,Z_COUPLING_NORTH
    REAL(SP) :: TIME_COUPLING_1,TIME_COUPLING_2
    INTEGER :: Kstart_EAST, Kend_EAST, Kshift_EAST,&
         Kstart_WEST, Kend_WEST, Kshift_WEST,&
         Kstart_SOUTH, Kend_SOUTH, Kshift_SOUTH,&
         Kstart_NORTH, Kend_NORTH, Kshift_NORTH
    LOGICAL :: IN_DOMAIN_EAST = .FALSE., &
         IN_DOMAIN_WEST = .FALSE., &
         IN_DOMAIN_SOUTH = .FALSE., &
         IN_DOMAIN_NORTH = .FALSE.
# endif

! river discharge
    LOGICAL :: RIVER_CLAMPED = .FALSE.
    CHARACTER(LEN=80) RIVER_FILE
    INTEGER :: N_RIVER_EAST,  N_RIVER_WEST,&
         N_RIVER_SOUTH, N_RIVER_NORTH,&
         J_START_RIV_EAST,  J_START_RIV_WEST,&
         I_START_RIV_SOUTH, I_START_RIV_NORTH
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: FLUX_RIVER_EAST,CSED_RIVER_EAST,&
                       FLUX_RIVER_WEST,CSED_RIVER_WEST,&
                       FLUX_RIVER_SOUTH,CSED_RIVER_SOUTH,&
                       FLUX_RIVER_NORTH,CSED_RIVER_NORTH
    REAL(SP) :: TIME_RIVER_1,TIME_RIVER_2
    INTEGER :: Kstart_RIV_EAST, Kend_RIV_EAST, Kshift_RIV_EAST,&
         Kstart_RIV_WEST, Kend_RIV_WEST, Kshift_RIV_WEST,&
         Kstart_RIV_SOUTH, Kend_RIV_SOUTH, Kshift_RIV_SOUTH,&
         Kstart_RIV_NORTH, Kend_RIV_NORTH, Kshift_RIV_NORTH
    LOGICAL :: RIV_IN_DOMAIN_EAST = .FALSE., &
         RIV_IN_DOMAIN_WEST = .FALSE., &
         RIV_IN_DOMAIN_SOUTH = .FALSE., &
         RIV_IN_DOMAIN_NORTH = .FALSE.

! tide boundary conditions
    LOGICAL :: TIDE_CLAMPED = .FALSE.
    CHARACTER(LEN=80) TIDE_FILE
    INTEGER :: NumTidePoint, NumConstituent
    REAL(SP) :: TideStartDate
    REAL(SP),DIMENSION(:),  ALLOCATABLE :: TideFac, Tideu0
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: TidePeriod, TideAmp, TidePha
    INTEGER, DIMENSION(:),  ALLOCATABLE :: I_bnd, J_bnd


! sediment
# if defined(SEDIMENT)
    LOGICAL :: UPDATE_BED   = .FALSE.
    LOGICAL :: COHESIVE    = .FALSE.
    LOGICAL :: NONCOHESIVE   = .FALSE.

    LOGICAL :: INITIAL_SED   = .FALSE.
    LOGICAL :: BOUNDARY_SED  = .FALSE.
    LOGICAL :: SED_BC_CLAMPED = .FALSE.
    LOGICAL :: SED_BC_NEUMANN = .FALSE.
    LOGICAL :: SED_BC_EQUILIBRIUM = .FALSE.
    LOGICAL :: SED_BC_CONSTANT = .FALSE.
    CHARACTER(LEN=80) INITIAL_SED_FILE
    CHARACTER(LEN=80) BOUNDARY_SED_FILE
    CHARACTER(LEN=80) CONSTANT_SED_FILE
    CHARACTER(LEN=80) SandFormula
    ! diffusion coefficient
    REAL(SP) :: NU_SED=0.02
    ! settling velocity (mm/s)
    REAL(SP) :: Ws=0.1
    ! critical shear stress (N/m2)
    REAL(SP) :: Tau_CE=0.1
    REAL(SP) :: Tau_CD=0.1
    ! surface erosion rate (kg/m2/s)
    REAL(SP) :: ErosionRate=0.0005
    ! user-defined roughness height, better use spatial-varying
    REAL(SP) :: Hrgh = 0.05_SP
   ! threshold parameter theta_cr, van Rijn 1993, delft3d manual page 349
    REAL(SP) :: ThetaShields = 0.01_SP
    ! bulk density, 1600.0 for sand; 500.0 for mud
    REAL(SP) :: rho_sed_bulk=1600.0_SP
    REAL(SP) :: AlphaSand=1.0_SP
    REAL(SP) :: D50 = 200.0_SP
    REAL(SP) :: D90 = 260.0_SP
    REAL(SP) :: RatioD90D50=1.3_SP
    REAL(SP) :: DELTA_RHO = 1.65_SP
    REAL(SP) :: theta_cr = 0.047_SP
    ! bed porosity (Exner): 0.4-0.5 for sand, 0.8-0.9 for mud
    REAL(SP) :: BedPorosity=0.40
    REAL(SP) :: MorphFactor=1.0
    REAL(SP) :: TIME_MORPH_SPINUP=0.0_SP

    ! sediment boundary
    INTEGER :: N_SEDBND_EAST,&
         N_SEDBND_WEST,&
         N_SEDBND_SOUTH,&
         N_SEDBND_NORTH,&
         J_START_SED_EAST,&
         J_START_SED_WEST,&
         I_START_SED_SOUTH,&
         I_START_SED_NORTH,&
         NumSedPoint

    INTEGER, DIMENSION(:),ALLOCATABLE :: I_SEDBND,J_SEDBND
    REAL(SP),DIMENSION(:),ALLOCATABLE :: C_SEDBND
    REAL(SP),DIMENSION(:,:),ALLOCATABLE:: C_SEDBND_EAST,&
                       C_SEDBND_WEST,&
                       C_SEDBND_SOUTH,&
                       C_SEDBND_NORTH
    REAL(SP) :: TIME_SEDBND_1,TIME_SEDBND_2,SedimentStartDate
    INTEGER :: Kstart_SED_EAST, Kend_SED_EAST, Kshift_SED_EAST,&
         Kstart_SED_WEST, Kend_SED_WEST, Kshift_SED_WEST,&
         Kstart_SED_SOUTH, Kend_SED_SOUTH, Kshift_SED_SOUTH,&
         Kstart_SED_NORTH, Kend_SED_NORTH, Kshift_SED_NORTH
    LOGICAL :: SED_IN_DOMAIN_EAST = .FALSE., &
         SED_IN_DOMAIN_WEST = .FALSE., &
         SED_IN_DOMAIN_SOUTH = .FALSE., &
         SED_IN_DOMAIN_NORTH = .FALSE.

    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: CON,CON0
    REAL(SP),DIMENSION(:,:,:,:),ALLOCATABLE :: OldDepth
    REAL(SP),DIMENSION(:,:,:,:),ALLOCATABLE :: Cdus,Cdvs

    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: BedLevel,QXcenter,QYcenter,UScenter,VScenter,&
                       QsedU,QsedV,VelMagU,VelMagV,Q_LR,Q_UD
    REAL(SP),DIMENSION(:,:,:,:),ALLOCATABLE :: QsedX,QsedY
# endif

! vegetation
    REAL(SP) :: FU_VEG=0.0_SP
    REAL(SP) :: FV_VEG=0.0_SP
# if defined (VEGETATION)
    CHARACTER(LEN=80) VEG_TYPE,VEG_H_FILE,VEG_H_SUBGRID_FILE, &
                  VEG_N_FILE,VEG_N_SUBGRID_FILE, &
                  VEG_D_FILE,VEG_D_SUBGRID_FILE
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: VEG_H,VEG_N,VEG_D
    REAL(SP),DIMENSION(:,:,:,:),ALLOCATABLE :: VEG_H_SUB,VEG_N_SUB,VEG_D_SUB
# endif

! biomass and population
# if defined(ECO_MORPHOLOGY)
    LOGICAL :: CALCULATE_BIOMASS   = .FALSE., &
          INITIAL_BIOMASS    = .FALSE., &
          CALCULATE_POPULATION  = .FALSE., &
          INITIAL_POPULATION   = .FALSE., &
          CALCULATE_ESTABLISH  = .FALSE., &
          CALCULATE_DIFFUSION  = .FALSE., &
          CALCULATE_MORTALITY_FLOW  = .FALSE., &
          CALCULATE_MORTALITY_INUN  = .FALSE., &
          CALCULATE_SPECY_INTERACT  = .FALSE.
    CHARACTER(LEN=80) INITIAL_STEMDIAMETER_FILE, &
             INITIAL_STEMDENSITY_FILE, &
             INITIAL_STEMHEIGHT_FILE, &
             INITIAL_BIOMASS_FILE
    REAL(SP) :: DT_ECO, DT_ECO_YEAR, TEMPERATURE,MaxCarryBiomass,&
          P_ESTABLISH, N_ESTABLISH,D_ESTABLISH, H_ESTABLISH,&
          LateralGrowthRate, MaxStemDensity, STEMDENSITY_BARE,&
          VerticalGrowthRate,MaxStemHeight, &
          MinStemDiameter,MaxStemDiameter,DiameterGrowthRate, &
          COEF_DIFF_VEG, &
          TAU_CR_VEG, COEF_MORTALITY_FLOW, &
          HGT_CR_VEG, COEF_MORTALITY_INUN
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: BIOMASS, STEMDENSITY, STEMHEIGHT, STEMDIAMETER
    REAL(SP),DIMENSION(:,:,:,:),ALLOCATABLE :: BIOMASS0,MeanHighTide4,MeanValueTau4
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: MeanHighTide2,MeanValueTau2
    ! coefficients for logistical functions

# endif

! wind
# if defined (WIND)
    LOGICAL :: WindForce = .TRUE.
    CHARACTER(LEN=80) WIND_FILE
    INTEGER :: NumTimeWindData
    REAL(SP), DIMENSION(:,:),ALLOCATABLE :: WindU2D,WindV2D
    REAL(SP),DIMENSION(:),ALLOCATABLE :: TimeWind
    REAL(SP),DIMENSION(:),ALLOCATABLE :: WU,WV
    REAL(SP) :: Cdw,WindCrestPercent
    INTEGER :: icount_winddata = 1
    INTEGER,DIMENSION(:,:),ALLOCATABLE :: MASK_WIND
# endif

! subgrid
    INTEGER :: SubMainGridRatio, HalfRatio, NumPixel
    REAL(SP), DIMENSION(:,:),ALLOCATABLE :: Porosity
    REAL(SP), DIMENSION(:,:,:,:),ALLOCATABLE :: DepSubGrid,EtaSubGrid,HgtSubGrid,&
                          CdSubGrid,ManningSubGrid,&
                          AlphaSubU, AlphaSubV,&
                          USubGrid, VSubGrid
    ! us, vs are converted to pixel centers
    REAL(SP), DIMENSION(:,:,:,:),ALLOCATABLE :: UcSubGrid,VcSubGrid
    REAL(SP), DIMENSION(:,:),ALLOCATABLE :: HgtPixel, CdPixel, &
                        EtaSubGridOut, DepSubGridOut, &
                        USubGridOut, VSubGridOut

! pre-storage
    INTEGER :: Neta,PolyOrder
    REAL(SP) :: EtaMinVal,EtaMaxVal,D_Eta,AvgEta0
    REAL(SP), DIMENSION(:),ALLOCATABLE :: EtaTab
# if defined (INTERPOLATION)
    REAL(SP),DIMENSION(:,:,:),ALLOCATABLE :: PorosityTab
    REAL(SP),DIMENSION(:,:,:),ALLOCATABLE :: CduTab,CdvTab
# endif
# if defined (POLY_FRICTION)
    REAL(SP),DIMENSION(:,:,:),ALLOCATABLE :: Coef_CdEquivU,Coef_CdEquivV
    INTEGER, DIMENSION(:,:),ALLOCATABLE :: Kbeg_cdu,Kbeg_cdv
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: CduMeanSqErr,CduMeanAbsErr,CduMaxAbsErr, &
                       CdvMeanSqErr,CdvMeanAbsErr,CdvMaxAbsErr
# endif
# if defined (POLY_POROSITY)
    REAL(SP),DIMENSION(:,:,:),ALLOCATABLE :: Coef_Porosity,Coef_Hu,Coef_Hv
    INTEGER,DIMENSION(:,:),ALLOCATABLE :: Kbeg_por,Kend_por, GrdType
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: PorMeanSqErr,PorMeanAbsErr,PorMaxAbsErr
# endif

! debug
# if defined(DEBUG)
    REAL(SP), DIMENSION(:,:),ALLOCATABLE :: tmp,tmpu,tmpv
# endif

! station data
    INTEGER :: NumberStations
    INTEGER,DIMENSION(:),ALLOCATABLE :: ista,jsta,nsta,iista,jjsta
    REAL(SP):: PLOT_INTV_STATION,PLOT_COUNT_STATION

! output logical parameters
    LOGICAL :: OUT_ETA=.FALSE., OUT_MASK=.FALSE., &
         OUT_U  =.FALSE., OUT_V  =.FALSE., &
         OUT_P  =.FALSE., OUT_Q  =.FALSE., &
         OUT_DEPTH =.FALSE., &
         OUT_PORO =.FALSE., &
         OUT_FRIC =.FALSE., &
         OUT_SED=.FALSE., &
         OUT_BED=.FALSE., &
         OUT_RESIST_FORCE=.FALSE., &
         OUT_SUBGRID_TAU=.FALSE., &
         OUT_SUBGRID_UV = .FALSE., &
         OUT_SUBGRID_BED=.FALSE., &
         OUT_SUBGRID_ETA=.FALSE., &
         OUT_HVEG=.FALSE., &
         OUT_NVEG=.FALSE., &
         OUT_DVEG=.FALSE., &
         OUT_TMP=.FALSE.

END MODULE GLOBAL
