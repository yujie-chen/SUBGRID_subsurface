MODULE GLOBAL
   USE PARAM
   IMPLICIT NONE

   SAVE

!MPI variables

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


! vegetation
    REAL(SP) :: FU_VEG=0.0_SP
    REAL(SP) :: FV_VEG=0.0_SP








! biomass and population


! wind


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


! debug




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

