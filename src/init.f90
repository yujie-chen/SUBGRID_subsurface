SUBROUTINE INITIALIZATION
    USE GLOBAL
    USE Input_Util

!Added by YUJIE CHEN
    USE SUBSURFACE_MODULE

    IMPLICIT NONE
    REAL(SP) :: Hx,Ux,Hy,Uy



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



      ALLOCATE(DepSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),&
               EtaSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),&
               HgtSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),&
                USubGrid(Mloc1,Nloc,SubMainGridRatio,SubMainGridRatio),&
                VSubGrid(Mloc,Nloc1,SubMainGridRatio,SubMainGridRatio),&
                UcSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),&
                VcSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),&
                CdSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),&
                CdPixel(SubMainGridRatio,SubMainGridRatio))

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

































    P=ZERO
    Q=ZERO
 !   U=ZERO
 !   V=ZERO
 !  Eta=ZERO
    EtaScreen=ZERO
    A_u=ZERO
    A_v=ZERO
    B=ZERO
    C=ZERO
    MASK = 1
    MASKu = 1
    MASKv = 1
    POROSITY = 0.0_SP

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
    CALL FILLIN_ETA_GHOST  !Added by YUJIE CHEN 03-06-2019

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


! read depth and friction

! space varying manning
  IF(MANNING_TYPE=='VARY')THEN
    ALLOCATE(ManningVary(Mloc,Nloc))
    CALL GetFile (MANNING_FILE,ManningVary)









  ENDIF


   CALL GetFile_Subgrid (DEPTH_FILE,DepSubGrid)
   IF(MANNING_TYPE=='SUBGRID')THEN
   CALL GetFile_Subgrid (MANNINGSUBGRID_FILE,ManningSubGrid)








       ENDIF

!ADDED BY YUJIE CHEN 03-04-2019

     CALL SUBSURFACE_INITIAL






     CALL SETUP_INITIAL_ETA_SUBGRID      ! MODIFIED BY YUJIE CHEN 03-04-2019


     CALL UPDATE_SUBGRID_SUBSURFACE












!     CALL SETUP_INITIAL_ETA_SUBGRID      !ORIGINAL










! end subgrid

     MaxDepth = ZERO
     DO J=1,Nloc
     DO I=1,Mloc
       IF(MAX(H_u(I,J),H_v(I,J)).GT.MaxDepth) MaxDepth = MAX(H_u(I,J),H_v(I,J))
     ENDDO
     ENDDO








     WRITE(*,*)'MaxDepth = ',MaxDepth


    IF(ADI)THEN
     DT = CFL*MIN(DX,DY)/SQRT(grav*MaxDepth)
    ELSE
     DT = 0.5_SP*CFL*MIN(DX,DY)/SQRT(grav*MaxDepth) ! for hopscotch
    ENDIF
















     WRITE(*,*)'dt= ',DT


    DX2=DX*DX
    DY2=DY*DY
    DXDY=DX*DY
    dt_over_dx=DT/DX
    dt_over_dy=DT/DY
    dt_over_dx2 = DT/DX/DX
    dt_over_dy2 = DT/DY/DY

    CALL INIT_BOUNDARY_CONDITIONS










    IF(RIVER_CLAMPED)THEN
      CALL InitRiverFlux









    ENDIF

    IF(TIDE_CLAMPED)THEN
      CALL InitTideBC









    ENDIF










! read obstacle structures ! $$$
     IF(OBSTACLE)THEN
       IF(.not.check_exist(TRIM(OBSTACLE_FILE)))THEN
         WRITE(*,*)TRIM(OBSTACLE_FILE), ' specified in input.txt but does not exist in folder.'
         STOP
       ENDIF
       IF(.NOT.ALLOCATED(VarGlob)) ALLOCATE (VarGlob(Mloc,Nloc)) ! use local here



       IF(.NOT.ALLOCATED(MASK_STRUC)) ALLOCATE (MASK_STRUC(Mloc,Nloc))
       OPEN(1,FILE=TRIM(OBSTACLE_FILE))
         DO J=Jbeg,Jend
           READ(1,*)(VarGlob(I,J),I=Ibeg,Iend)
         ENDDO
       CLOSE(1)

       MASK_STRUC = INT(VarGlob)
       DEALLOCATE(VarGlob)









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






     WRITE(3,*)'initialization completed'
     WRITE(*,*)'initialization completed'

!# endif


END SUBROUTINE INITIALIZATION



SUBROUTINE INDEX
    USE GLOBAL
    IMPLICIT NONE


  px=1
  py=1


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



END SUBROUTINE INDEX



SUBROUTINE INIT_BOUNDARY_CONDITIONS
    USE GLOBAL
    IMPLICIT NONE





!  west
    DO J=Jbeg,Jend
      MASKu(Ibeg,J)=0
    ENDDO





!  east



    DO J=Jbeg,Jend
      MASKu(Iend1,J)=0
    ENDDO





!  south



    DO I=Ibeg,Iend
      MASKv(I,Jbeg)=0
    ENDDO




! north



    DO I=Ibeg,Iend
       MASKv(I,Jend1)=0
    ENDDO








END SUBROUTINE INIT_BOUNDARY_CONDITIONS










! end poly_friction




! end poly_porosity



! end subgrid

