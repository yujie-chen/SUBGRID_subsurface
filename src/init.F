SUBROUTINE INITIALIZATION
    USE GLOBAL
    USE Input_Util
    IMPLICIT NONE
    REAL(SP) :: Hx,Ux,Hy,Uy
# if defined (PARALLEL)
     REAL(SP)::myvar
# endif
     REAL(SP),DIMENSION(:,:),ALLOCATABLE :: VarGlob

    ALLOCATE(P(Mloc1,Nloc),Q(Mloc,Nloc1),PQ(Mloc1,Nloc1),Eta(Mloc,Nloc), &
             Eta0(Mloc,Nloc), ETA_OVER_DT(Mloc,Nloc),POROSITY(Mloc,Nloc), &
             H(Mloc,Nloc),H0(Mloc,Nloc),H_u(Mloc1,Nloc),H_v(Mloc,Nloc1), &
             Cori(Mloc,Nloc),Cdu(Mloc1,Nloc), Cdv(Mloc,Nloc1),&
             MASK(Mloc,Nloc), &
             U(Mloc1,Nloc),V(Mloc,Nloc1), EtaScreen(Mloc,Nloc))

    ALLOCATE(A_u(Mloc1,Nloc),A_v(Mloc,Nloc1),B(Mloc1,Nloc),C(Mloc,Nloc1), &
             MASKu(Mloc1,Nloc),MASKv(Mloc,Nloc1))
    
!    IF(OUT_RESIST_FORCE)THEN
      ALLOCATE(TotResistFX(Mloc1,Nloc),TotResistFY(Mloc,Nloc1))
!    ENDIF
    
    ALLOCATE(Int2Flo(Mloc1,Nloc1))

# if defined (SUBGRID)

      ALLOCATE(DepSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),&
               EtaSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),&
               HgtSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),&
                USubGrid(Mloc1,Nloc,SubMainGridRatio,SubMainGridRatio),&
                VSubGrid(Mloc,Nloc1,SubMainGridRatio,SubMainGridRatio),&
                UcSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),&
                VcSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),&
                CdSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),&
                CdPixel(SubMainGridRatio,SubMainGridRatio) )

      IF(MANNING_TYPE=='SUBGRID')THEN
        ALLOCATE(ManningSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio))
        ALLOCATE(TauSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio))
      ENDIF


      HalfRatio=SubMainGridRatio/2
      NumPixel=SubMainGridRatio*SubMainGridRatio
      ALLOCATE(HgtPixel(SubMainGridRatio,SubMainGridRatio))
!     polynomial variables EtaTab
      Neta=NINT((EtaMaxVal-EtaMinVal)/D_Eta) + 1
      AvgEta0=ZERO
      ALLOCATE(EtaTab(Neta))
      DO I = 1, Neta
        EtaTab(I)=EtaMinVal+D_Eta*(real(I-1))
        AvgEta0=AvgEta0+EtaTab(I)/real(Neta)
      ENDDO

# if defined (POLY_FRICTION)
      ALLOCATE(Coef_CdEquivU(Mloc1,Nloc,PolyOrder+1) &
              ,Coef_CdEquivV(Mloc,Nloc1,PolyOrder+1))
      ALLOCATE(Kbeg_cdu(Mloc1,Nloc),Kbeg_cdv(Mloc,Nloc1))
# endif

# if defined (POLY_POROSITY)
      ALLOCATE(Coef_Porosity(Mloc,Nloc,PolyOrder+1))
!     ALLOCATE(Coef_Hu(Mloc1,Nloc,PolyOrder+1),Coef_Hv(Mloc,Nloc1,PolyOrder+1))
      ALLOCATE(GrdType(Mloc,Nloc),Kbeg_por(Mloc,Nloc),Kend_por(Mloc,Nloc))
# endif     

# if defined(SEDIMENT)
      ALLOCATE(Cdus(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio), &
               Cdvs(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio))

      ! relative sediment density
      DELTA_RHO = (RHO_SED-RHO_W)/RHO_W
      ! sand diameter: um->m
      D50 = D50*0.001_SP*0.001_SP
      D90 = D50*RatioD90D50
      ! non-dimensional grain size
      Dstar = D50*(grav*Delta_Rho/Nu_water**2)**(1.0_SP/3.0_SP)
      ! threshold parameter theta_cr, van Rijn 1993, delft3d manual page 349
      IF(Dstar.LE.4.0_SP)THEN
        ThetaShields=0.24_SP/Dstar
      ELSEIF(Dstar.LE.10.0_SP)THEN
        ThetaShields=0.14_SP*Dstar**(-0.64_SP)
      ELSEIF(Dstar.LE.20.0_SP)THEN
        ThetaShields=0.04_SP*Dstar**(-0.1_SP)
      ELSEIF(Dstar.LE.150.0_SP)THEN
        ThetaShields=0.013_SP*Dstar**(0.29_SP)
      ELSE
        ThetaShields=0.055
      ENDIF
      ! Ws: mm/s->m/s
      IF(COHESIVE)THEN
        Ws = Ws*0.001_SP
      ELSEIF(NONCOHESIVE)THEN
      ! van Rijn(1993), delft3d manual page 332
        IF(D50.LE.0.0001_SP)THEN
          Ws = 1.65_SP*grav*D50*D50/18.0_SP/NU_WATER
        ELSEIF(D50.LE.0.001_SP)THEN
          Ws = NU_WATER*10.0_SP/D50*(sqrt(1+1.65_SP*0.01_SP*grav*D50**3/NU_WATER**2)-1)
        ELSE
          Ws = 1.1_SP*sqrt(1.65_SP*grav*D50)
        ENDIF
      ENDIF
      
      IF(NONCOHESIVE)THEN
        ALLOCATE(QsedX(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio), &
                 QsedY(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio) )
        IF(UPDATE_BED)THEN
         ALLOCATE(BedLevel(SubMainGridRatio+2,SubMainGridRatio+2), &
                 QXcenter(SubMainGridRatio+2,SubMainGridRatio+2), &
                 QYcenter(SubMainGridRatio+2,SubMainGridRatio+2), &
                 UScenter(SubMainGridRatio+2,SubMainGridRatio+2), &
                 VScenter(SubMainGridRatio+2,SubMainGridRatio+2), &
                 QsedU(SubMainGridRatio+1,SubMainGridRatio+1),    &
                 QsedV(SubMainGridRatio+1,SubMainGridRatio+1) )
         IF(SubMainGridRatio==1)THEN
           ALLOCATE(VelMagU(Mloc1,Nloc),VelMagV(Mloc,Nloc1), &
                    Q_LR(Mloc1,Nloc),Q_UD(Mloc,Nloc1) )
         ENDIF
        ENDIF
      ENDIF
# endif

# if defined(ECO_MORPHOLOGY)
      ALLOCATE(MeanHighTide4(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio))
      ALLOCATE(MeanValueTau4(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio))
      ALLOCATE(MeanHighTide2(MMloc,NNloc),MeanValueTau2(MMloc,NNloc))
      MeanHighTide4 = SMALL
      MeanValueTau4 = ZERO
# endif

# endif

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: sediment coeff'     
#    else   
       WRITE(*,*) 'INITILIZATION: sediment coeff'
#    endif
# endif

    P=ZERO
    Q=ZERO
    U=ZERO
    V=ZERO
!   Eta=ZERO
    EtaScreen=ZERO
    A_u=ZERO
    A_v=ZERO
    B=ZERO
    C=ZERO
    MASK = 1
    MASKu = 1
    MASKv = 1
    POROSITY = 1.0_SP


! Added by Yujie CHEN
    IF(HOT_START)THEN
        IF(FileNumber_HOTSTART==1)THEN
            CALL GETFILE(ETA_FILE,ETA)
            U = ZERO
            V = ZERO
        ELSEIF(FileNumber_HOTSTART==2)THEN
            CALL GETFILE(ETA_FILE,ETA)
            CALL GETFILE(U_FILE,U)
            CALL GETFILE(V_FILE,V)
        ENDIF
    ELSE
        Eta = ZERO
        U = ZERO
        V = ZERO
    ENDIF


    TIME=0.0_SP
    BIN0 = 0
    BIN1 = 1

    PLOT_COUNT_STATION = ZERO
    PLOT_COUNT = ZERO

! -------------------- tmp gausian hump test
!# if defined (PARALLEL)
!if (myid==0)then
!print*,myid
!    ETA(25,25)=1.0
!else
!print*,myid
!endif
!# else
!    ETA(25,25)=1.0
!# endif


    Cdu = Cd_fixed
    Cdv = Cd_fixed
    Cori = ZERO

! read vegetation
# if defined(VEGETATION)
  IF(VEG_TYPE=='VARY')THEN
    ALLOCATE(VEG_H(Mloc,Nloc),VEG_N(Mloc,Nloc),VEG_D(Mloc,Nloc))
    CALL GetFile(VEG_H_FILE,VEG_H)
    CALL GetFile(VEG_N_FILE,VEG_N)
    CALL GetFile(VEG_D_FILE,VEG_D)
    ! in vegetation drag subroutine, average values are used
    ! now I think it's not necessary, but need to redistribute if vegetation
    ! changes with time. 2016-1-15

  ELSEIF(VEG_TYPE=='SUBGRID')THEN
    ALLOCATE(VEG_H_SUB(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio), &
             VEG_N_SUB(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio), &
             VEG_D_SUB(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio))
    CALL GetFile_SubGrid(VEG_H_SUBGRID_FILE,VEG_H_SUB)
    CALL GetFile_SubGrid(VEG_N_SUBGRID_FILE,VEG_N_SUB)
    CALL GetFile_SubGrid(VEG_D_SUBGRID_FILE,VEG_D_SUB)
  ENDIF

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: vegetation'     
#    else   
       WRITE(*,*) 'INITILIZATION: vegetation'
#    endif
# endif

# endif

! read depth and friction

! space varying manning
  IF(MANNING_TYPE=='VARY')THEN
    ALLOCATE(ManningVary(Mloc,Nloc))
    CALL GetFile (MANNING_FILE,ManningVary)

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: vary manning file'     
#    else   
       WRITE(*,*) 'INITILIZATION: vary manning file'
#    endif
# endif

  ENDIF

# if defined (SUBGRID)
   CALL GetFile_Subgrid (DEPTH_FILE,DepSubGrid)
   IF(MANNING_TYPE=='SUBGRID')THEN
     CALL GetFile_Subgrid (MANNINGSUBGRID_FILE,ManningSubGrid)

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: manning'     
#    else   
       WRITE(*,*) 'INITILIZATION: manning'
#    endif
# endif

   ENDIF

# if defined (INTERPOLATION)
     CALL INTERPOLATE_POROSITY
     CALL INTERPOLATE_FRICTION

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: interp porosity/friction'     
#    else   
       WRITE(*,*) 'INITILIZATION: interp porosity/friction'
#    endif
# endif
# endif

# if defined (POLY_FRICTION)
     CALL SETUP_POLYNOMIAL_FRICTION

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: polynomial_friction'     
#    else   
       WRITE(*,*) 'INITILIZATION: polynomial_friction'
#    endif
# endif
# endif
# if defined (POLY_POROSITY)
     CALL SETUP_POLYNOMIAL_POROSITY

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: polynomial_porosity'     
#    else   
       WRITE(*,*) 'INITILIZATION: polynomial_porosity'
#    endif
# endif

# endif

     CALL UPDATE_SUBGRID

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: update_subgrid'     
#    else   
       WRITE(*,*) 'INITILIZATION: update_subgrid'
#    endif
# endif

     CALL SETUP_INITIAL_ETA_SUBGRID

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: setup_eta'     
#    else   
       WRITE(*,*) 'INITILIZATION: setup_eta'
#    endif
# endif

# endif
! end subgrid

     MaxDepth = ZERO
     DO J=1,Nloc
     DO I=1,Mloc
       IF(MAX(H_u(I,J),H_v(I,J)).GT.MaxDepth) MaxDepth = MAX(H_u(I,J),H_v(I,J))
     ENDDO
     ENDDO

# if defined (PARALLEL)
     call MPI_ALLREDUCE(MaxDepth,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     MaxDepth = myvar
     IF(myid == 0)THEN
     WRITE(*,*)'MaxDepth= ',MaxDepth
     ENDIF
# else
     WRITE(*,*)'MaxDepth = ',MaxDepth
# endif

    IF(ADI)THEN
     DT = CFL*MIN(DX,DY)/SQRT(grav*MaxDepth)
    ELSE
     DT = 0.5_SP*CFL*MIN(DX,DY)/SQRT(grav*MaxDepth) ! for hopscotch
    ENDIF

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: get dt'     
#    else   
       WRITE(*,*) 'INITILIZATION: get dt'
#    endif
# endif

# if defined (PARALLEL)
     call MPI_ALLREDUCE(DT,myvar,1,MPI_SP,MPI_MIN,MPI_COMM_WORLD,ier)
     DT = myvar
     IF(myid == 0)THEN
     WRITE(*,*)'dt= ',DT
     ENDIF
# else
     WRITE(*,*)'dt= ',DT
# endif

    DX2=DX*DX
    DY2=DY*DY
    DXDY=DX*DY
    dt_over_dx=DT/DX
    dt_over_dy=DT/DY
    dt_over_dx2 = DT/DX/DX
    dt_over_dy2 = DT/DY/DY

    CALL INIT_BOUNDARY_CONDITIONS

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: init_bc'     
#    else   
       WRITE(*,*) 'INITILIZATION: init_bc'
#    endif
# endif

    IF(RIVER_CLAMPED)THEN
      CALL InitRiverFlux

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: InitRiverFlux'     
#    else   
       WRITE(*,*) 'INITILIZATION: InitRiverFlux'
#    endif
# endif

    ENDIF

    IF(TIDE_CLAMPED)THEN
      CALL InitTideBC

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: InitTideBC'     
#    else   
       WRITE(*,*) 'INITILIZATION: InitTideBC'
#    endif
# endif

    ENDIF


# if defined(COUPLING)
      CALL InitOneWayCoupling

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: InitOneWayCoupling'     
#    else   
       WRITE(*,*) 'INITILIZATION: InitOneWayCoupling'
#    endif
# endif

# endif


# if defined(SEDIMENT)
      CALL InitSedConcentration
# if defined(COUPLING)
      IF(BOUNDARY_SED)THEN
        IF(SED_BC_CLAMPED)THEN
          CALL InitSedimentCoupling
        ELSEIF(SED_BC_CONSTANT)THEN
          ! this subroutine is not tested yet
          CALL InitSedimentConstant
        ENDIF
      ENDIF
# endif

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: InitSedimentCoupling'     
#    else   
       WRITE(*,*) 'INITILIZATION: InitSedimentCoupling'
#    endif
# endif

# endif

# if defined(ECO_MORPHOLOGY)
      CALL INITIALIZE_POPULATION

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: population'     
#    else   
       WRITE(*,*) 'INITILIZATION: population'
#    endif
# endif

# endif


! read obstacle structures ! $$$
     IF(OBSTACLE)THEN
       IF(.not.check_exist(TRIM(OBSTACLE_FILE)))THEN
         WRITE(*,*)TRIM(OBSTACLE_FILE), ' specified in input.txt but does not exist in folder.'
         STOP
       ENDIF
       IF(.NOT.ALLOCATED(VarGlob)) ALLOCATE (VarGlob(Mloc,Nloc)) ! use local here
# if defined (PARALLEL)
       call GetFile (TRIM(OBSTACLE_FILE),VarGlob)
# else
       IF(.NOT.ALLOCATED(MASK_STRUC)) ALLOCATE (MASK_STRUC(Mloc,Nloc))
       OPEN(1,FILE=TRIM(OBSTACLE_FILE))
         DO J=Jbeg,Jend
           READ(1,*)(VarGlob(I,J),I=Ibeg,Iend)
         ENDDO
       CLOSE(1)
# endif
       MASK_STRUC = INT(VarGlob)
       DEALLOCATE(VarGlob)

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION: obc'     
#    else   
       WRITE(*,*) 'INITILIZATION: obc'
#    endif
# endif

     ENDIF


! read inactive points

     ALLOCATE (MASK_DRY(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio))
     IF(INACTIVE_PNT)THEN
       IF(.not.check_exist(TRIM(INACTIVE_PNT_FILE)))THEN
         WRITE(*,*)TRIM(INACTIVE_PNT_FILE), ' specified in input.txt but does not exist in folder.'
         STOP
       ENDIF
       call GetFile_Subgrid (INACTIVE_PNT_FILE,MASK_DRY)
     ELSE
      MASK_DRY=1
     ENDIF

       WRITE(*,*) 'INITILIZATION: OK'

! # if defined (DEBUG)
# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'initialization completed'
     WRITE(*,*)'initialization completed'
    ENDIF
# else
     WRITE(3,*)'initialization completed'
     WRITE(*,*)'initialization completed'
# endif
!# endif


END SUBROUTINE INITIALIZATION



SUBROUTINE INDEX
    USE GLOBAL
    IMPLICIT NONE

# if defined (PARALLEL)

    NumberProcessor = px*py
    dims(1) = px
    dims(2) = py
    periods(1) = .false.
    periods(2) = .false.
    IF(PERIODIC) periods(2) = .true.
    coords(1) = 0
    coords(2) = 0

    call MPI_CART_CREATE( MPI_COMM_WORLD, ndims, dims, &
         periods, reorder, comm2d, ier )
    call MPI_CART_COORDS( comm2d, myid, 2, coords, ier)

    npx = coords(1)
    npy = coords(2)

    call MPI_Cart_shift( comm2d, 0, 1, n_west, n_east, ier )
    call MPI_Cart_shift( comm2d, 1, 1, n_suth, n_nrth, ier )


# else
  px=1
  py=1
# endif

! now for serial code
    Mloc=Mglob/px+2*Nghost
    Nloc=Nglob/py+2*Nghost
    Mloc1=Mloc+1
    Nloc1=Nloc+1

    Ibeg=Nghost+1
    Iend=Mloc-Nghost
    Iend1=Mloc1-Nghost
    Jbeg=Nghost+1
    Jend=Nloc-Nghost
    Jend1=Nloc1-Nghost

! index for subgrid level with ghost subgrid
    MMglob = Mglob*SubMainGridRatio
    NNglob = Nglob*SubMainGridRatio
    MMloc =  Mloc*SubMainGridRatio
    NNloc =  Nloc*SubMainGridRatio

    IIbeg = Nghost*SubMainGridRatio+1
    IIend = MMloc-Nghost*SubMainGridRatio
    JJbeg = Nghost*SubMainGridRatio+1
    JJend = NNloc-Nghost*SubMainGridRatio

# if defined (DEBUG)
# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'index completed'
    ENDIF
# else
     WRITE(3,*)'index completed'
# endif
# endif

END SUBROUTINE INDEX



SUBROUTINE INIT_BOUNDARY_CONDITIONS
    USE GLOBAL
    IMPLICIT NONE

# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif

!  west
    DO J=Jbeg,Jend
      MASKu(Ibeg,J)=0
    ENDDO

# if defined (PARALLEL)
    endif
# endif

!  east
# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
    DO J=Jbeg,Jend
      MASKu(Iend1,J)=0
    ENDDO
# if defined (PARALLEL)
    endif
# endif


!  south
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
    DO I=Ibeg,Iend
      MASKv(I,Jbeg)=0
    ENDDO
# if defined (PARALLEL)
    endif
# endif

! north
# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
    DO I=Ibeg,Iend
       MASKv(I,Jend1)=0
    ENDDO

# if defined (PARALLEL)
    endif
# endif


# if defined(DEBUG)
# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'Subroutine init_boundary_conditions'
    ENDIF
# else
     WRITE(3,*)'Subroutine init_boundary_conditions'
# endif
# endif

END SUBROUTINE INIT_BOUNDARY_CONDITIONS



# if defined (SUBGRID)

# if defined (INTERPOLATION)
SUBROUTINE INTERPOLATE_FRICTION
     USE GLOBAL
     IMPLICIT NONE
     INTEGER  :: Keta,pcount,pcount1

     ALLOCATE(CduTab(Mloc1,Nloc,Neta))
     ALLOCATE(CdvTab(Mloc,Nloc1,Neta))

!    U-cell
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend1
        DO Keta=1,Neta
           CALL CALCULATE_SUBGRID_CD(1,EtaTab(Keta),CduTab(I,J,Keta))
        ENDDO
     ENDDO
     ENDDO

!    V-cell
     DO J=Jbeg,Jend1
     DO I=Ibeg,Iend
        DO Keta=1,Neta
           CALL CALCULATE_SUBGRID_CD(2,EtaTab(Keta),CdvTab(I,J,Keta))
        ENDDO
     ENDDO
     ENDDO

# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'interpolation of friction completed'
    ENDIF
# else
     WRITE(3,*)'interpolation of friction completed'
# endif

END SUBROUTINE INTERPOLATE_FRICTION



SUBROUTINE INTERPOLATE_POROSITY
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP) :: tmpv1
     INTEGER :: pcount,Keta

     ALLOCATE(PorosityTab(Mloc,Nloc,Neta))

     DO J=1,Nloc
     DO I=1,Mloc

       DO Keta=1,Neta
          pcount=0
          DO JJ=1,SubMainGridRatio
          DO II=1,SubMainGridRatio
             tmpv1=EtaTab(Keta)+DepSubGrid(I,J,II,JJ)
             IF(tmpv1.GT.ZERO)pcount=pcount+1
          ENDDO
          ENDDO
          PorosityTab(I,J,Keta)=REAL(pcount)/REAL(NumPixel)
       ENDDO

     ENDDO
     ENDDO
END SUBROUTINE INTERPOLATE_POROSITY
# endif



# if defined (POLY_FRICTION)
SUBROUTINE SETUP_POLYNOMIAL_FRICTION
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP),DIMENSION(:),ALLOCATABLE :: CdEquivU,CdEquivV
     INTEGER  :: Keta,pcount,pcount1
!    Cd=g*Manning^2/H^(1/3)

     ALLOCATE(CdEquivU(Neta),CdEquivV(Neta))
     ALLOCATE(CduMeanSqErr(Mloc1,Nloc),CduMeanAbsErr(Mloc1,Nloc),CduMaxAbsErr(Mloc1,Nloc))
     ALLOCATE(CdvMeanSqErr(Mloc,Nloc1),CdvMeanAbsErr(Mloc,Nloc1),CdvMaxAbsErr(Mloc,Nloc1))

     Coef_CdEquivU=ZERO
     Coef_CdEquivV=ZERO

!    U-cell
     DO J=1,Nloc
     DO I=1,Mloc1
        pcount1=0
        DO Keta=1,Neta
          CALL CALCULATE_SUBGRID_CD(1,EtaTab(Keta),CdEquivU(Keta))
          IF(CdEquivU(Keta)==ZERO)pcount1=pcount1+1
        ENDDO
        Kbeg_cdu(I,J)=pcount1+1

        IF((Neta-pcount1)>=(PolyOrder+1))THEN
          CALL POLY_FITTING(EtaTab(pcount1+1:Neta),CdEquivU(pcount1+1:Neta), &
                            Coef_CdEquivU(I,J,:), Neta-pcount1, PolyOrder+1, &
                            CduMeanSqErr(I,J),CduMeanAbsErr(I,J),CduMaxAbsErr(I,J))
        ELSE
!         Coef_CdEquivU(I,J,:)=ZERO
!         WRITE(*,*)'Cd_u: Polynomial order is larger than avaiable eta's'
!         WRITE(*,*)'I= ',I, ' J= ', J
        ENDIF
     ENDDO
     ENDDO

!    V-cell
     DO J=1,Nloc1
     DO I=1,Mloc
        pcount1=0
        DO Keta=1,Neta
          CALL CALCULATE_SUBGRID_CD(2,EtaTab(Keta),CdEquivV(Keta))
          IF(CdEquivV(Keta)==ZERO)pcount1=pcount1+1
        ENDDO

        Kbeg_cdv(I,J)=pcount1+1

        IF((Neta-pcount1)>=(PolyOrder+1))THEN
          CALL POLY_FITTING(EtaTab(pcount1+1:Neta),CdEquivV(pcount1+1:Neta), &
                            Coef_CdEquivV(I,J,:), Neta-pcount1, PolyOrder+1, &
                            CdvMeanSqErr(I,J),CdvMeanAbsErr(I,J),CdvMaxAbsErr(I,J))
        ELSE
!         Coef_CdEquivV(I,J,:)=ZERO
!         WRITE(*,*)'Cd_v: Polynomial order is larger than avaiable eta's'
!         WRITE(*,*)'I= ',I, ' J= ', J
        ENDIF

     ENDDO
     ENDDO

    DEALLOCATE(CdEquivU)
    DEALLOCATE(CdEquivV)

# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'polynomial of friction completed'
    ENDIF
# else
     WRITE(3,*)'polynomial of friction completed'
# endif

END SUBROUTINE SETUP_POLYNOMIAL_FRICTION

# endif
! end poly_friction



# if defined (POLY_POROSITY)
SUBROUTINE SETUP_POLYNOMIAL_POROSITY
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP) :: tmpv1,tmpv2,depmax,hwater,avgeta,avgdep,tmpv3,tmpv4
     INTEGER :: pcount,pcount1,pcount2,pcount3,Keta
     REAL(SP), DIMENSION(:), ALLOCATABLE :: TmpPorosity,TmpHu,TmpHv

     ALLOCATE(TmpPorosity(Neta))
     ALLOCATE(PorMeanSqErr(Mloc,Nloc), &
              PorMeanAbsErr(Mloc,Nloc),&
              PorMaxAbsErr(Mloc,Nloc))
     Coef_Porosity=ZERO
     GrdType=9

     DO J=1,Nloc
     DO I=1,Mloc

       pcount1=0
       pcount2=0
       pcount3=0
       ! loop all possible eta
       DO Keta=1,Neta

          pcount=0
          TmpPorosity(Keta)=ZERO
          DO JJ=1,SubMainGridRatio
          DO II=1,SubMainGridRatio
             tmpv1=EtaTab(Keta)+DepSubGrid(I,J,II,JJ)
             IF(tmpv1.GT.ZERO)THEN
               pcount=pcount+1
             ENDIF
          ENDDO
          ENDDO

          IF(pcount==0)THEN
            TmpPorosity(Keta)=ZERO
            pcount1=pcount1+1
          ELSE
            TmpPorosity(Keta)=REAL(pcount)/REAL(NumPixel)
            IF(pcount<NumPixel)pcount2=pcount2+1
            IF(pcount==NumPixel)pcount3=pcount3+1
          ENDIF

       ENDDO
       ! end loop

       ! beg/end for poly-fit
       Kbeg_por(I,J)=pcount1           ! +1
       Kend_por(I,J)=pcount1+pcount2

       IF(pcount2==0)THEN
          IF(pcount1==Neta.AND.pcount3==0)THEN
            !  always fully dry
            GrdType(I,J)=0
          ELSEIF(pcount1==0.AND.pcount3==Neta)THEN
            !  always fully wet
            GrdType(I,J)=1
          ELSE
            !  jump from fully dry to fully wet
            GrdType(I,J)=2
          ENDIF
       ELSEIF(pcount2>=1.AND.pcount2<(PolyOrder+1))THEN
          !  grow from dry to wet, but too few for poly-fit
          GrdType(I,J)=3
       ELSEIF(pcount2>=(PolyOrder+1))THEN
          !  partly wet section, when enough eta-points for fitting
          GrdType(I,J)=4
          CALL POLY_FITTING(EtaTab(pcount1+1:pcount1+pcount2),TmpPorosity(pcount1+1:pcount1+pcount2),&
                           Coef_Porosity(I,J,:), pcount2, PolyOrder+1, &
                           PorMeanSqErr(I,J),PorMeanAbsErr(I,J),PorMaxAbsErr(I,J))
       ENDIF

       ! check
       IF(GrdType(I,J)==9)THEN
         WRITE(*,*) 'GrdType = 9'
         WRITE(*,*) 'I = ', I, ' J= ', J
         STOP
       ENDIF

     ENDDO
     ENDDO
     DEALLOCATE(TmpPorosity)

!     open(55,file='grdtype.txt')
!     do j=1,Nloc
!       write(55,"(1000I3)")(GrdType(I,J),i=1,Mloc)
!     enddo
!     close(55)

END SUBROUTINE SETUP_POLYNOMIAL_POROSITY

# endif
! end poly_porosity


# endif
! end subgrid
