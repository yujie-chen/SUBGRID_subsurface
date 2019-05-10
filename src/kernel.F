PROGRAM SUBGRID_MODEL

  USE GLOBAL
# if defined (SUBSURFACE)
         USE SUBSURFACE_MODULE
# endif

  IMPLICIT NONE

# if defined (PARALLEL)
  CALL MPI_INIT ( ier )
# endif

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'MPI_INIT'     
#    else   
       WRITE(*,*) 'PARALLEL is not used'
#    endif
# endif

  CALL READ_INPUT

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'READ_INPUT'     
#    else   
       WRITE(*,*) 'READ_INPUT'
#    endif
# endif

  CALL INDEX

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INDEX'     
#    else   
       WRITE(*,*) 'INDEX'
#    endif
# endif

  CALL INITIALIZATION

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'INITIALIZATION'     
#    else   
       WRITE(*,*) 'INITILIZATION'
#    endif
# endif


# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'CHECK_EXCH_SUBGRID'     
#    else   
       WRITE(*,*) 'CHECK_EXCH_SUBGRID'
#    endif
# endif

# if defined (SUBSURFACE)
    CALL UPDATE_MASKS  ! yjchen added this line 08/08/2018
    CALL SUBSURFACE_INITIAL

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'SUBSURFACE_INITIAL'     
#    else   
       WRITE(*,*) 'SUBSURFACE_INITIAL'
#    endif
# endif

# endif
! subsurface



  DO WHILE (TIME<TOTAL_TIME) ! begin main loop

   CALL UPDATE_MASKS

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'UPDATE_MASKS'     
#    else   
       WRITE(*,*) 'UPDATE_MASKS'
#    endif
# endif

   ! bc: coupling time-series
# if defined(COUPLING)
   CALL OneWayCoupling


# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'OneWayCoupling'     
#    else   
       WRITE(*,*) 'OneWayCoupling'
#    endif
# endif

# endif


   ! tide boundary
   IF(TIDE_CLAMPED)THEN
    CALL TIDE_BC
    CALL FILLIN_ETA_GHOST
# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'TIDE_BC'     
#    else   
       WRITE(*,*) 'TIDE_BC'
#    endif
# endif

   ENDIF

   ! river discharge
   IF(RIVER_CLAMPED)THEN
    CALL RIVER_FLUX_BC

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'RIVER_FLUX_BC'     
#    else   
       WRITE(*,*) 'RIVER_FLUX_BC'
#    endif
# endif

   ENDIF

   ! sediment boundary
# if defined(SEDIMENT)
   IF(COHESIVE.AND.BOUNDARY_SED)THEN
    IF(SED_BC_NEUMANN)THEN
     CALL SEDIMENT_NEUMANN_BOUNDARY
# if defined(COUPLING)
    ELSEIF(SED_BC_CLAMPED)THEN
     CALL SEDIMENT_CLAMPED_BOUNDARY
    ELSEIF(SED_BC_CONSTANT)THEN
     CALL SEDIMENT_CONSTANT_BOUNDARY
# endif
    ENDIF
   ENDIF

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'SEDIMENT_MODULE'     
#    else   
       WRITE(*,*) 'SEDIMENT_MODULE'
#    endif
# endif

# endif

   ! calculate A B C
   CALL CALCULATE_ABC

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'CALCULATE_ABC'     
#    else   
       WRITE(*,*) 'CALCULATE_ABC'
#    endif
# endif

   ! calculate eta
   IF(HOPSCOTCH)THEN
    CALL HOPSCOTCH_SCHEME
   ELSE
    CALL TRIDIAGONAL
   ENDIF

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'CALCULATE ETA'     
#    else   
       WRITE(*,*) 'CALCULATE ETA'
#    endif
# endif

   ! calculate P Q
   CALL CALCULATE_H_PQ_UV

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'CALCULATE_H_PQ_UV'     
#    else   
       WRITE(*,*) 'CALCULATE_H_PQ_UV'
#    endif
# endif

   ! construct subgrid eta, uv
   IF(MANNING_TYPE=='SUBGRID'.AND.SubMainGridRatio.GT.1)THEN
    CALL CONSTRUCT_SUBGRID_ETA
    CALL GET_PIXEL_CENTER_VARIABLES

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'CONTRUCT_SUBGRID_ETA'     
#    else   
       WRITE(*,*) 'CONSTRUCT_SUBGRID_ETA'
#    endif
# endif
   ENDIF

   ! structures
   IF(OBSTACLE)THEN
    CALL STRUCTURE_CONDITIONS

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'STRUCTURE CONDITIONS'     
#    else   
       WRITE(*,*) 'STRUCTURE CONDITIONS'
#    endif
# endif

   ENDIF

# if defined (SUBSURFACE)
     CALL UPDATE_SUBSURFACE
# endif

   ! sediment model
# if defined(SEDIMENT)
   CALL UPDATE_SED

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'UPDATE_SED'     
#    else   
       WRITE(*,*) 'UPDATE_SED'
#    endif
# endif
# endif

   ! update porosity h
   CALL UPDATE_SUBGRID

# if defined (SUBSURFACE)
     DO J=1,Nloc
     DO I=1,Mloc
!        IF (MASK(I,J)==0)THEN
!          ETA(I,J) = ETA_ground(I,J)
!        ENDIF
        IF (MASK(I,J)==1)THEN
          ETA_ground(I,J) = ETA(I,J)
        ENDIF
     ENDDO
     ENDDO

# if defined (PARALLEL)
     CALL PHI_EXCH_1(ETA_ground)
# endif

# endif

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'UPDATE_SUBGRID'     
#    else   
       WRITE(*,*) 'READ_INPUT'
#    endif
# endif

   ! eco-morphology model
# if defined(ECO_MORPHOLOGY)
   IF(TIME-(ICOUNT_ECO+1)*DT_ECO*DAY2SEC/MorphFactor>=0.0_SP)THEN
    IF(CALCULATE_POPULATION)CALL UPDATE_POPULATION
    IF(CALCULATE_BIOMASS)CALL UPDATE_BIOMASS

    ! output vegetation dynamics
    CALL PREVIEW_ECO
# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'ECO_MORPHOLOGY'     
#    else   
       WRITE(*,*) 'ECO_MORPHOLOGY'
#    endif
# endif

   ENDIF
# endif


   ! output stations
   IF(NumberStations>0)THEN
    PLOT_COUNT_STATION=PLOT_COUNT_STATION+DT
    IF(PLOT_COUNT_STATION>=PLOT_INTV_STATION)THEN
     PLOT_COUNT_STATION=PLOT_COUNT_STATION-PLOT_INTV_STATION
     CALL STATIONS
# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'STATIONS'     
#    else   
       WRITE(*,*) 'STATIONS'
#    endif
# endif

    ENDIF
   ENDIF

   ! output maps
   IF(TIME-(ICOUNT+1)*PLOT_INTV>=0.0_SP)THEN
    CALL PREVIEW
# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'PREVIEW'     
#    else   
       WRITE(*,*) 'PREVIEW'
#    endif
# endif
   ENDIF

   ! output screen
   SCREEN_COUNT=SCREEN_COUNT+DT
   IF(SCREEN_COUNT>=SCREEN_INTV)THEN
    SCREEN_COUNT=SCREEN_COUNT-SCREEN_INTV
    CALL STATISTICS
   ENDIF

   ! time stepping
   IF(CONSTANT_DT)THEN
    DT=DT_FIXED
    TIME=TIME+DT
   ELSE
  CALL ESTIMATE_DT(Mloc,Nloc,DX,DY,U,V,H_u,H_v,MinDepth,DT,CFL,TIME)

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'ESTIMATE_DT'     
#    else   
       WRITE(*,*) 'ESTIMATE_DT'
#    endif
# endif
   ENDIF

# if defined (DEBUG)
# if defined (PARALLEL)
  IF(myid == 0)THEN
   WRITE(3,*)'time = ', time
  ENDIF
# else
   WRITE(3,*)'time = ', time
# endif
# endif

  ENDDO  ! end main loop


# if defined (PARALLEL)
   call MPI_FINALIZE ( ier )
   stop
# endif

END PROGRAM

!=============================================================================



SUBROUTINE CHECK_EXCH_SUBGRID
   USE GLOBAL
   IMPLICIT NONE
   REAL(SP),DIMENSION(Mloc*SubMainGridRatio,Nloc*SubMainGridRatio):: mycheck
   INTEGER::KI,KJ,KII,KJJ
   CHARACTER(LEN=80)::TMP_NAME

   DO I = Ibeg,Iend
   DO J = Jbeg,Jend
    DO II = 1, SubMainGridRatio
    DO JJ = 1, SubMainGridRatio
     DepSubGrid(I,J,II,JJ) = DepSubGrid(I,J,II,JJ)+10.0_SP
    ENDDO
    ENDDO
   ENDDO
   ENDDO

   ALLOCATE(DepSubGridOut(MMloc,NNloc))
   CALL FLOW2ECO(DepSubGrid,DepSubGridOut)

   TMP_NAME = 'check_global.txt'
   CALL PutFile_SubGrid(1,TMP_NAME,DepSubGridOut,MMloc,NNloc)
   DEALLOCATE(DepSubGridOut)

   DO J=1,Nloc
   DO I=1,Mloc
    DO JJ=1,SubMainGridRatio
    DO II=1,SubMainGridRatio
     KI = (I-1)*SubMainGridRatio+II
     KJ = (J-1)*SubMainGridRatio+JJ
     mycheck(KI,KJ)=DepSubGrid(I,J,II,JJ)
    ENDDO
    ENDDO
   ENDDO
   ENDDO

# if(PARALLEL)
   IF(myid == 0)THEN
    open(55,file='check_local_id0.txt')
    DO J=1,Nloc*SubMainGridRatio
     write(55,521)(mycheck(I,J),I=1,Mloc*SubMainGridRatio)
    ENDDO
    close(55)
   ELSEIF (myid==1)THEN
    open(55,file='check_local_id1.txt')
    DO J=1,Nloc*SubMainGridRatio
     write(55,521)(mycheck(I,J),I=1,Mloc*SubMainGridRatio)
    ENDDO
    close(55)
   ELSEIF (myid==2)THEN
    open(55,file='check_local_id2.txt')
    DO J=1,Nloc*SubMainGridRatio
     write(55,521)(mycheck(I,J),I=1,Mloc*SubMainGridRatio)
    ENDDO
    close(55)
   ENDIF
# endif

521 FORMAT(1000F8.4)

END SUBROUTINE CHECK_EXCH_SUBGRID



SUBROUTINE ESTIMATE_DT(M,N,DX,DY,U,V,H_u,H_v,MinDepth,DT,CFL,TIME)
   USE PARAM
# if defined (PARALLEL)
   USE GLOBAL, ONLY : ier
# endif
   IMPLICIT NONE
   INTEGER,INTENT(IN)::M,N
# if defined (PARALLEL)
   REAL(SP) :: myvar
# endif

   REAL(SP),INTENT(IN)::DX,DY

   REAL(SP),INTENT(IN),DIMENSION(M+1,N)::U,H_u
   REAL(SP),INTENT(IN),DIMENSION(M,N+1)::V,H_v
   REAL(SP),INTENT(IN)::CFL,MinDepth
   REAL(SP),INTENT(OUT)::DT
   REAL(SP),INTENT(INOUT)::TIME

   TMP3=LARGE
   DO J=1,N
   DO I=1,M
! x direction
   TMP1=ABS(U(I,J))+SQRT(GRAV*MAX(H_u(I,J),MinDepth))
   IF(TMP1<SMALL)THEN
    TMP2=DX/SMALL
   ELSE
    TMP2=DX/TMP1
   ENDIF
   IF(TMP2<TMP3)TMP3=TMP2
! y direction
   TMP1=ABS(V(I,J))+SQRT(GRAV*MAX(H_v(I,J),MinDepth))
   IF(TMP1<SMALL)THEN
    TMP2=DY/SMALL
   ELSE
    TMP2=DY/TMP1
   ENDIF
   IF(TMP2<TMP3)TMP3=TMP2
   ENDDO
   ENDDO
# if defined (PARALLEL)
   call MPI_ALLREDUCE (TMP3,myvar,1,MPI_SP,MPI_MIN,&
     MPI_COMM_WORLD,ier)
   TMP3 = myvar
# endif
   DT=0.5*CFL*TMP3  ! 0.5 for Hopscotch
! TEMP
   TIME=TIME+DT

END SUBROUTINE ESTIMATE_DT




# if defined (PARALLEL)

SUBROUTINE UPDATE_MASKS
  USE GLOBAL
  IMPLICIT NONE

 if ( n_west .eq. MPI_PROC_NULL.and.n_east .ne. MPI_PROC_NULL ) then
  DO J=1,Nloc
  DO I=Ibeg+1,Mloc1
   IF(H_u(I,J)>MinDepth)THEN
    MASKu(I,J)=1
   ELSE
    MASKu(I,J)=0
   ENDIF
  ENDDO
  ENDDO
 else if ( n_west .eq. MPI_PROC_NULL.and.n_east .eq. MPI_PROC_NULL ) then
  DO J=1,Nloc
  DO I=Ibeg+1,Iend1-1
   IF(H_u(I,J)>MinDepth)THEN
    MASKu(I,J)=1
   ELSE
    MASKu(I,J)=0
   ENDIF
  ENDDO
  ENDDO
 else if ( n_west .ne. MPI_PROC_NULL.and.n_east .eq. MPI_PROC_NULL ) then
  DO J=1,Nloc
  DO I=1,Iend1-1
   IF(H_u(I,J)>MinDepth)THEN
    MASKu(I,J)=1
   ELSE
    MASKu(I,J)=0
   ENDIF
  ENDDO
  ENDDO

  else
  DO J=1,Nloc
  DO I=1,Mloc1
   IF(H_u(I,J)>MinDepth)THEN
    MASKu(I,J)=1
   ELSE
    MASKu(I,J)=0
   ENDIF
  ENDDO
  ENDDO

 endif


  if ( n_suth .eq. MPI_PROC_NULL.and.n_nrth .ne. MPI_PROC_NULL ) then
  DO J=Jbeg+1,Nloc1
  DO I=1,Mloc
   IF(H_v(I,J)>MinDepth)THEN
    MASKv(I,J)=1
   ELSE
    MASKv(I,J)=0
   ENDIF
  ENDDO
  ENDDO

  else if ( n_suth .eq. MPI_PROC_NULL.and.n_nrth .eq. MPI_PROC_NULL ) then

  DO J=Jbeg+1,Jend1-1
  DO I=1,Mloc
   IF(H_v(I,J)>MinDepth)THEN
    MASKv(I,J)=1
   ELSE
    MASKv(I,J)=0
   ENDIF
  ENDDO
  ENDDO

  else if ( n_suth .ne. MPI_PROC_NULL.and.n_nrth .eq. MPI_PROC_NULL ) then

  DO J=1,Jend1-1
  DO I=1,Mloc
   IF(H_v(I,J)>MinDepth)THEN
    MASKv(I,J)=1
   ELSE
    MASKv(I,J)=0
   ENDIF
  ENDDO
  ENDDO

  else
  DO J=1,Nloc1
  DO I=1,Mloc
   IF(H_v(I,J)>MinDepth)THEN
    MASKv(I,J)=1
   ELSE
    MASKv(I,J)=0
   ENDIF
  ENDDO
  ENDDO
  endif


  IF(OBSTACLE)THEN
   DO J=Jbeg,Jend
   DO I=Ibeg,Iend
    IF(MASK_STRUC(I,J)==1)THEN
     MASKu(I,J)=0
    ENDIF
    IF(MASK_STRUC(I,J)==2)THEN
     MASKv(I,J)=0
    ENDIF
   ENDDO
   ENDDO
  ENDIF  ! end obstacle

! add this 2016-01-17
! I found this is un-necessary! 2016-01-20
# if defined(PARALLEL)
  CALL phi_int_exch_2(MASKu)
  CALL phi_int_exch_3(MASKv)
# endif

# if defined(DEBUG)
  IF(myid == 0)THEN
   WRITE(3,*)'Subroutine update_sed'
  ENDIF
# endif

END SUBROUTINE UPDATE_MASKS


# else
! not parallel
SUBROUTINE UPDATE_MASKS
  USE GLOBAL
  IMPLICIT NONE

  DO J=1,Nloc
  DO I=Ibeg+1,Iend1-1
   IF(H_u(I,J)>MinDepth)THEN
    MASKu(I,J)=1
   ELSE
    MASKu(I,J)=0
   ENDIF
  ENDDO
  ENDDO


  DO J=Jbeg+1,Jend1-1
  DO I=1,Mloc
   IF(H_v(I,J)>MinDepth)THEN
    MASKv(I,J)=1
   ELSE
    MASKv(I,J)=0
   ENDIF
  ENDDO
  ENDDO

  IF(OBSTACLE)THEN
   DO J=Jbeg,Jend
   DO I=Ibeg,Iend
    IF(MASK_STRUC(I,J)==1)THEN
     MASKu(I,J)=0
    ENDIF
    IF(MASK_STRUC(I,J)==2)THEN
     MASKv(I,J)=0
    ENDIF
   ENDDO
   ENDDO
  ENDIF  ! end obstacle

# if defined(DEBUG)
   WRITE(3,*)'Subroutine update_sed'
# endif

END SUBROUTINE UPDATE_MASKS
# endif
! end parallel



SUBROUTINE STRUCTURE_CONDITIONS
  USE GLOBAL
  IMPLICIT NONE
  REAL :: HEAD
  INTEGER :: kount
  INTEGER :: HeadRange = 5

!   REAL,DIMENSION(Mloc,Nloc,10) :: Eta_Ser
!   REAL,DIMENSION(Mloc,Nloc) :: Eta_Avg

!    DO J=Jbeg,Jend
!    DO I=Ibeg,Iend
!     Eta_Avg(I,J)=ZERO
!     DO K=1,9
!      Eta_Ser(I,J,K)=Eta_Ser(I,J,K+1)
!      Eta_Avg(I,J)=Eta_Avg(I,J)+Eta_Ser(I,J,K)
!     ENDDO
!      Eta_Ser(I,J,10)=Eta(I,J)
!      Eta_Avg(I,J)=Eta_Avg(I,J)+Eta_Ser(I,J,10)
!      Eta_Avg(I,J)=Eta_Avg(I,J)/10.0;
!    ENDDO
!    ENDDO


   DO J=Jbeg,Jend
   DO I=Ibeg,Iend
    IF(MASK_STRUC(I,J)==11)THEN

!  pi*D4dP/(128*nu*L)
     tmp1=ZERO
     tmp2=ZERO

     kount=0
     DO JJ=MAX(1,J-HeadRange),MIN(Nloc,J+HeadRange)
     DO II=MAX(1,I-2*HeadRange-1),I-1
      IF(MASK(II,JJ)>0)THEN
      kount=kount+1
      tmp1=Eta(II,JJ)+tmp1
      ENDIF
     ENDDO
     ENDDO
     IF(kount>0)THEN
      tmp1=tmp1/kount
     ENDIF

     kount=0
     DO JJ=MAX(1,J-HeadRange),MIN(Nloc,J+HeadRange)
     DO II=I,MIN(Mloc,I+2*HeadRange)
     IF(MASK(II,JJ)>0)THEN
      kount=kount+1
      tmp2=Eta(II,JJ)+tmp2
     ENDIF
     ENDDO
     ENDDO
     IF(kount>0)THEN
      tmp2=tmp2/kount
     ENDIF
     HEAD=tmp1-tmp2
     IF(ABS(HEAD)<0.01)HEAD=ZERO

     P(I,J)=1.0*HEAD

     IF(ABS(P(I,J))>1.0)P(I,J)=SIGN(0.2,P(I,J))
!print*,P(I,J),I,J

!print*,head
!      IF(ABS(HEAD)>0.5)THEN
!       P(I,J)=SIGN(2.0,HEAD)
!      ELSE
!       P(I,J)=SIGN(1.0,HEAD)
!      ENDIF

    ENDIF ! end structure
   ENDDO
   ENDDO

END SUBROUTINE STRUCTURE_CONDITIONS



SUBROUTINE CALCULATE_H_PQ_UV

  USE GLOBAL

# if defined (SUBSURFACE)
     USE SUBSURFACE_MODULE
# endif
  IMPLICIT NONE
  REAL(SP) :: Utotal,Utheta,Fr,Pgrad

! P and Q
  DO J=Jbeg,Jend
  DO I=Ibeg,Iend1
# if defined (SUBSURFACE) 
!$$$
               IF(MASKu(I,J)==0)THEN
                 P(I,J)=P_ground(I,J)
                 U(I,J) =P(I,J)/MAX(MinDepth,Du_ground(I,J))
               ELSE

                 Pgrad=ETA(I,J)-ETA(I-1,J)
                 IF(MASK(I-1,J)<1) Pgrad=MAX(ZERO,ETA(I,J)-EtaScreen(I-1,J)) 
                 IF(MASK(I,J)<1) Pgrad=MIN(ZERO,EtaScreen(I,J)-Eta(I-1,J))       

                 P(I,J) =-A_u(I,J)*Pgrad/DX+B(I,J)
                 U(I,J) =P(I,J)/MAX(MinDepth,H_u(I,J))
               ENDIF
# else
                 Pgrad=ETA(I,J)-ETA(I-1,J)
                 IF(MASK(I-1,J)<1) Pgrad=MAX(ZERO,ETA(I,J)-EtaScreen(I-1,J)) 
                 IF(MASK(I,J)<1) Pgrad=MIN(ZERO,EtaScreen(I,J)-Eta(I-1,J))

                 P(I,J) =-A_u(I,J)*Pgrad/DX+B(I,J)
                 P(I,J) =P(I,J)*MASKu(I,J)
                 U(I,J) =P(I,J)/MAX(MinDepth,H_u(I,J))

# endif
  ENDDO
  ENDDO

  DO J=Jbeg,Jend1
  DO I=Ibeg,Iend
# if defined (SUBSURFACE)
               IF(MASKv(I,J)==0)THEN
                 Q(I,J)=Q_ground(I,J)
                 V(I,J) =Q(I,J)/MAX(MinDepth,Dv_ground(I,J))
               ELSE

                 Pgrad=ETA(I,J)-ETA(I,J-1)
                 IF(MASK(I,J-1)<1) Pgrad=MAX(ZERO,ETA(I,J)-EtaScreen(I,J-1)) 
                 IF(MASK(I,J)<1) Pgrad=MIN(ZERO,EtaScreen(I,J)-Eta(I,J-1))

                 Q(I,J) =-A_v(I,J)*Pgrad/DY+C(I,J)
                 V(I,J) =Q(I,J)/MAX(MinDepth,H_v(I,J))
               ENDIF
# else
                 Pgrad=ETA(I,J)-ETA(I,J-1)
                 IF(MASK(I,J-1)<1) Pgrad=MAX(ZERO,ETA(I,J)-EtaScreen(I,J-1))  
                 IF(MASK(I,J)<1) Pgrad=MIN(ZERO,EtaScreen(I,J)-Eta(I,J-1))

                 Q(I,J) =-A_v(I,J)*Pgrad/DY+C(I,J)

                 Q(I,J) =Q(I,J)*MASKv(I,J)
                 V(I,J) =Q(I,J)/MAX(MinDepth,H_v(I,J))
# endif
   
  ENDDO
  ENDDO

! apply froude cap in directions

   DO J=Jbeg,Jend
   DO I=Ibeg,Iend1
    Fr=SQRT(GRAV*Max(MinDepth,H_u(I,J)))
    IF(U(I,J)/Fr.gt.FroudeCap)THEN
     U(I,J)=FroudeCap*Fr
     P(I,J)=U(I,J)*Max(H_u(I,J),MinDepth)
    ENDIF
   ENDDO
   ENDDO

   DO J=Jbeg,Jend1
   DO I=Ibeg,Iend
    Fr=SQRT(GRAV*Max(MinDepth,H_v(I,J)))
    IF(V(I,J)/Fr.gt.FroudeCap)THEN
     V(I,J)=FroudeCap*Fr
     Q(I,J)=V(I,J)*Max(H_v(I,J),MinDepth)
    ENDIF
   ENDDO
   ENDDO


# if defined (PARALLEL)
  CALL PHI_EXCH_2(P)
  CALL PHI_EXCH_2(U)
  CALL PHI_EXCH_3(Q)
  CALL PHI_EXCH_3(V)
# endif

!  fill in boundary ghost cells
  CALL FILLIN_BOUNDARY_GHOST

# if defined(DEBUG)
# if defined (PARALLEL)
  IF(myid == 0)THEN
   WRITE(3,*)'Subroutine calculate_h_pq_uv'
  ENDIF
# else
   WRITE(3,*)'Subroutine calculate_h_pq_uv'
# endif
# endif

END SUBROUTINE CALCULATE_H_PQ_UV



SUBROUTINE FILLIN_BOUNDARY_GHOST
  USE GLOBAL
  IMPLICIT NONE
!  for adv-dif
!  free-slip bc
!  allow water to flow out of boundary freely

! west
# if defined(PARALLEL)
 if( n_west .eq. MPI_PROC_NULL )then
# endif

  DO J=Jbeg,Jend
  DO I=1,Nghost
   MASKu(I,J)=MASKu(Ibeg,J)
   P(I,J)=P(Ibeg,J)
   U(I,J)=U(Ibeg,J)
   H_u(I,J)=H_u(Ibeg,J)
  ENDDO
  ENDDO

  DO J=Jbeg,Jend1
  DO I=1,Nghost
   MASKv(I,J)=MASKv(Ibeg,J)
   Q(I,J)=Q(Ibeg,J)
   V(I,J)=V(Ibeg,J)
   H_v(I,J)=H_v(Ibeg,J)
  ENDDO
  ENDDO

# if defined(PARALLEL)
 endif
# endif

! east
# if defined(PARALLEL)
 if( n_east .eq. MPI_PROC_NULL )then
# endif

  DO J=Jbeg,Jend
  DO I=Iend1+1,Mloc1
   MASKu(I,J)=MASKu(Iend1,J)
   P(I,J)=P(Iend1,J)
   U(I,J)=U(Iend1,J)
   H_u(I,J)=H_u(Iend1,J)
  ENDDO
  ENDDO

  DO J=Jbeg,Jend1
  DO I=Iend1,Mloc
   MASKv(I,J)=MASKv(Iend,J)
   Q(I,J)=Q(Iend,J)
   V(I,J)=V(Iend,J)
   H_v(I,J)=H_v(Iend,J)
  ENDDO
  ENDDO

# if defined(PARALLEL)
 endif
# endif

! south
# if defined(PARALLEL)
 if( n_suth .eq. MPI_PROC_NULL )then
# endif

  DO J=1,Nghost
  DO I=1,Mloc
   MASKv(I,J)=MASKv(I,Jbeg)
   Q(I,J)=Q(I,Jbeg)
   V(I,J)=V(I,Jbeg)
   H_v(I,J)=H_v(I,Jbeg)
  ENDDO
  ENDDO

  DO J=1,Nghost
  DO I=1,Mloc1
   MASKu(I,J)=MASKu(I,Jbeg)
   P(I,J)=P(I,Jbeg)
   U(I,J)=U(I,Jbeg)
   H_u(I,J)=H_u(I,Jbeg)
  ENDDO
  ENDDO

# if defined(PARALLEL)
 endif
# endif

! north
# if defined(PARALLEL)
 if( n_nrth .eq. MPI_PROC_NULL )then
# endif

  DO J=Jend1+1,Nloc1
  DO I=1,Mloc
   MASKv(I,J)=MASKv(I,Jend1)
   Q(I,J)=Q(I,Jend1)
   V(I,J)=V(I,Jend1)
   H_v(I,J)=H_v(I,Jend1)
  ENDDO
  ENDDO

  DO J=Jend1,Nloc
  DO I=1,Mloc1
   MASKu(I,J)=MASKu(I,Jend)
   P(I,J)=P(I,Jend)
   U(I,J)=U(I,Jend)
   H_u(I,J)=H_u(I,Jend)
  ENDDO
  ENDDO

# if defined(PARALLEL)
 endif
# endif

END SUBROUTINE FILLIN_BOUNDARY_GHOST



SUBROUTINE CALCULATE_ABC
  USE GLOBAL
  IMPLICIT NONE
  LOGICAL :: UPWINDING = .TRUE.
  REAL(SP) :: upwind,Rxx,Rxy,Ryx,Ryy
  INTEGER :: UorV

!  calculate pq for cross-derivatives
  DO J=Jbeg,Jend1
  DO I=Ibeg,Iend1
   IF(Maskv(I-1,J)==1.AND.Masku(I,J-1)==1.AND.Maskv(I,J)==1.AND.Masku(I,J)==1)THEN
   PQ(I,J) = 0.25_SP*(P(I,J)+P(I,J-1))*(Q(I,J)+Q(I-1,J))  &
           /MAX(MinDepth,0.25_SP*(H_u(I,J)+H_u(I,J-1)+H_v(I-1,J)+H_v(I,J)))
   ELSE
   PQ(I,J) = ZERO
   ENDIF
  ENDDO
  ENDDO

!  A_u
  DO J=Jbeg,Jend
  DO I=Ibeg,Iend1

   ! advection terms
   ! note: I made a mistake here and its commented now below. same for Q. 2016/01/13
   IF(UPWINDING)THEN
    ! upwind
    IF(P(I,J)>ZERO)THEN
     IF(MASKu(I-1,J)==1)THEN
      upwind= (P(I,J)**2/Max(MinDepth,H_u(I,J))  &
          -P(I-1,J)**2/Max(MinDepth,H_u(I-1,J)))/DX
     ! version 2: BAD! (this was used in v13~v14)
     ! ELSEIF(MASKu(I+1,J)==1)THEN
     !  upwind= (0.25*(P(I,J)+P(I+1,J))**2/Max(MinDepth,0.5_SP*(H_u(I+1,J)+H_u(I,J))) &
     !         -P(I,J)**2/Max(MinDepth,H_u(I,J)))/DX*2
     ! version 1: BAD! (this was used in v1)
     ! ELSE IF(MASKu(I+1,J)==1)THEN
     !  upwind= (P(I+1,J)**2/Max(MinDepth,H_u(I+1,J)) &
     !     -P(I,J)**2/Max(MinDepth,H_u(I,J)))/DX
     ELSE
      upwind = ZERO
     ENDIF
    ELSE
     IF(MASKu(I+1,J)==1)THEN
      upwind= (P(I+1,J)**2/Max(MinDepth,H_u(I+1,J))  &
          -P(I,J)**2/Max(MinDepth,H_u(I,J)))/DX
     ! version 2: BAD!
     ! ELSEIF(MASKu(I-1,J)==1)THEN
     !  upwind= (P(I,J)**2/Max(MinDepth,H_u(I,J)) &
     !      -0.25*(P(I-1,J)+P(I,J))**2/Max(MinDepth,0.5_SP*(H_u(I-1,J)+H_u(I,J))))/DX*2
     ! version 1: BAD!
     ! ELSE IF(MASKu(I-1,J)==1)THEN
     !  upwind= (P(I,J)**2/Max(MinDepth,H_u(I,J)) &
     !    -P(I-1,J)**2/Max(MinDepth,H_u(I-1,J)))/DX
     ELSE
      upwind= ZERO
     ENDIF
    ENDIF
   ! central diff (not tested)
   ELSE
    IF(MASKu(I+1,J)*MASKu(I-1,J)==1)THEN
     upwind= 0.25_SP*( (P(I+1,J)+P(I,J))**2/Max( MinDepth,0.5_SP*(H_u(I+1,J)+H_u(I,J)) )  &
              -(P(I-1,J)+P(I,J))**2/Max( MinDepth,0.5_SP*(H_u(I-1,J)+H_u(I,J)) ) )/DX
    ELSE
     upwind= ZERO
    ENDIF
   ENDIF

   ! diffusion terms
   IF(MASKu(I,J)*MASKu(I+1,J)*MASKu(I-1,J)==1)THEN
    Rxx = 2.0_SP*(P(I+1,J)-2.0*P(I,J)+P(I-1,J))/DX2
   ELSE
    Rxx = ZERO
   ENDIF

   IF(MASKu(I,J+1)*MASKu(I,J)*MASKu(I,J-1)==1)THEN
    Rxy = (P(I,J+1)-2.0*P(I,J)+P(I,J-1))/DY2
   ELSE
    Rxy = ZERO
   ENDIF
   IF(MASKv(I,J+1)*MASKv(I-1,J+1)*MASKv(I,J)*MASKv(I-1,J)==1)THEN
    Rxy = Rxy + ( (Q(I,J+1)-Q(I-1,J+1))-(Q(I,J)-Q(I-1,J)) )/DXDY
   ELSE
    Rxy = Rxy + ZERO
   ENDIF

   ! friction and vegetation
   CALL CALCULATE_FRCU

   IF(OUT_RESIST_FORCE)THEN
    TotResistFX(I,J)=rho_w*FrcU*U(I,J)
   ENDIF


   IF(IMPLICIT)THEN
    A_u(I,J)=grav*H_u(I,J)*H_u(I,J)*DT &
        /(Max(MinDepth,H_u(I,J)) + FrcU*DT)
    A_u(I,J)=A_u(I,J)*MASKu(I,J)

    B(I,J)=DT/(1.0_SP+FrcU*DT/MAX(MinDepth,H_u(I,J))) &
       *(      &
         Cori(I,J)*0.25_SP*(Q(I,J)+Q(I,J+1)+Q(I-1,J)+Q(I-1,J+1)) &
       - Nonlinear*upwind &
       - Nonlinear*(PQ(I,J+1)-PQ(I,J))/DY &
       + NU*(Rxx + Rxy) &
       + P(I,J)/DT )
   ENDIF

   B(I,J)=B(I,J)*MASKu(I,J)

  ENDDO
  ENDDO

! A_v
  DO J=Jbeg,Jend1
  DO I=Ibeg,Iend
   ! advection terms
   IF(UPWINDING)THEN
    ! upwind
    IF(Q(I,J)>ZERO)THEN
     IF(MASKv(I,J-1)==1)THEN
       upwind= (Q(I,J)**2/Max(MinDepth,H_v(I,J)) &
           -Q(I,J-1)**2/Max(MinDepth,H_v(I,J-1)))/DY
     ! version 2: BAD!
     ! ELSEIF(MASKv(I,J+1)==1)THEN
     !  upwind= ( 0.25_SP*(Q(I,J+1)+Q(I,J))**2/Max(MinDepth,0.5_SP*(H_v(I,J+1)+H_v(I,J)))  &
     !      -Q(I,J)**2/Max(MinDepth,H_v(I,J)))/DY*2
     ! version 1: BAD!
     ! ELSE IF(MASKv(I,J+1)==1)THEN
     !  upwind= (Q(I,J+1)**2/Max(MinDepth,H_v(I,J+1))  &
     !      -Q(I,J)**2/Max(MinDepth,H_v(I,J)))/DY
     ELSE
       upwind= ZERO
     ENDIF
    ELSE
     IF(MASKv(I,J+1)==1)THEN
       upwind= (Q(I,J+1)**2/Max(MinDepth,H_v(I,J+1)) &
           -Q(I,J)**2/Max(MinDepth,H_v(I,J)))/DY
     ! version 2: BAD!
     ! ELSEIF(MASKv(I,J-1)==1)THEN
     !  upwind= (Q(I,J)**2/Max(MinDepth,H_v(I,J)) &
     !      -0.25_SP*(Q(I,J-1)+Q(I,J))**2/Max(MinDepth,0.5_SP*(H_v(I,J-1)+H_v(I,J))))/DY*2
     ! version 1: BAD!
     ! ELSE IF(MASKv(I,J-1)==1)THEN
     !  upwind= (Q(I,J)**2/Max(MinDepth,H_v(I,J)) &
     !      -Q(I,J-1)**2/Max(MinDepth,H_v(I,J-1)))/DY
     ELSE
       upwind=ZERO
     ENDIF
    ENDIF
   ! central dif
   ELSE
    IF(MASKv(I,J-1)*MASKv(I,J+1)==1)THEN
     upwind= (Q(I,J+1)**2/Max(MinDepth,H_v(I,J+1))  &
          -Q(I,J-1)**2/Max(MinDepth,H_v(I,J-1)))/DY/2.0_SP
    ELSE
     upwind = ZERO
    ENDIF
   ENDIF

   ! diffusion terms
   IF(MASKv(I,J+1)*MASKv(I,J)*MASKv(I,J-1)==1)THEN
    Ryy = 2.0_SP*(Q(I,J+1)-2.0*Q(I,J)+Q(I,J-1))/DY2
   ELSE
    Ryy = ZERO
   ENDIF

   IF(MASKv(I+1,J)*MASKv(I,J)*MASKv(I-1,J)==1)THEN
    Ryx = (Q(I+1,J)-2.0*Q(I,J)+Q(I-1,J))/DX2
   ELSE
    Ryx = ZERO
   ENDIF
   IF(MASKu(I+1,J)*MASKu(I+1,J-1)*MASKu(I,J)*MASKu(I,J-1)==1)THEN
    Ryx = Ryx + ((P(I+1,J)-P(I+1,J-1))-(P(I,J)-P(I,J-1)))/DXDY
   ELSE
    Ryx = Ryx + ZERO
   ENDIF

   ! friction and vegetation
   CALL CALCULATE_FRCV

   IF(OUT_RESIST_FORCE)THEN
    TotResistFY(I,J)=rho_w*FrcV*V(I,J)
   ENDIF

   IF(IMPLICIT)THEN
    A_v(I,J)=grav*H_v(I,J)*H_v(I,J)*DT &
        /(Max(MinDepth,H_v(I,J)) + FrcV*DT)

    A_v(I,J)=A_v(I,J)*MASKv(I,J)

    C(I,J)=DT/(1.0_SP+FrcV*DT/MAX(Mindepth,H_v(I,J))) &
       *(-Cori(I,J)*0.25_SP*(P(I,J)+P(I+1,J)+P(I,J-1)+P(I+1,J-1)) &
       - Nonlinear*upwind &
       - Nonlinear*(PQ(I+1,J)-PQ(I,J))/DX &
       + NU*(Ryx + Ryy) &
       + Q(I,J)/DT )
   ENDIF


   C(I,J)=C(I,J)*MASKv(I,J)

  ENDDO
  ENDDO

# if defined(DEBUG)
# if defined (PARALLEL)
  IF(myid == 0)THEN
   WRITE(3,*)'Subroutine calculate_abc'
  ENDIF
# else
   WRITE(3,*)'Subroutine calculate_abc'
# endif
# endif

END SUBROUTINE CALCULATE_ABC



SUBROUTINE CALCULATE_FRCU
  USE GLOBAL
  IMPLICIT NONE
  REAL(SP) :: AvgEta,Cv
  INTEGER :: Ktmp,tmpk1,tmpk2

  AvgEta=0.5_SP*(Eta(MAX(1,I-1),J)+Eta(I,J))
  IF(MASK(I-1,J)==0.AND.MASK(I,J)==1)AvgEta=Eta(I,J)
  IF(MASK(I,J)==0.AND.MASK(I-1,J)==1)AvgEta=Eta(I-1,J)
  IF(MASK(I-1,J)==0.AND.MASK(I,J)==0)AvgEta=MIN(Eta(I-1,J),Eta(I,J))

!  U-points
  IF(MANNING_TYPE=='VARY')Manning=ManningVary(I,J)
  IF(MANNING_TYPE=='UNIFORM'.OR.MANNING_TYPE=='VARY')THEN
   IF(Manning.GT.ZERO)THEN
    Cdu(I,J)=grav*Manning**2/(Max(H_u(I,J), MinDepFric))**(1.0_SP/3.0_SP)
   ENDIF
# if defined(VEGETATION) || defined(ECO_MORPHOLOGY)
   CALL CALCULATE_VEG_DRAG(1,AvgEta,Cv)
   Cdu(I,J)=Cdu(I,J)+Cv
# endif

# if defined (SUBGRID)
  ELSEIF(MANNING_TYPE=='SUBGRID')THEN

# if defined (INTERPOLATION)
   AvgEta=MIN(AvgEta,EtaMaxVal)
   AvgEta=MAX(AvgEta,EtaMinVal)

   tmpk1 = INT((AvgEta-EtaMinVal)/D_Eta+1.0_SP)
   tmpk2 = MIN(tmpk1+1,Neta)
   IF(CduTab(I,J,tmpk2)<=ZERO)THEN
    Cdu(I,J)=MinCd
   ELSE
    Cdu(I,J)=CduTab(I,J,tmpk1) + (AvgEta-EtaTab(tmpk1)) &
        *(CduTab(I,J,tmpk2)-CduTab(I,J,tmpk1))/D_Eta
   ENDIF
# endif

# if defined (POLY_FRICTION)
   ! sum polynomial terms
   Cdu(I,J)=ZERO
   IF( AvgEta >= EtaTab(Kbeg_cdu(I,J)) )THEN
    DO Ktmp=1,PolyOrder+1
     Cdu(I,J)=Cdu(I,J)+Coef_CdEquivU(I,J,Ktmp)*(AvgEta-AvgEta0)**(Ktmp-1)
    ENDDO
   ENDIF

   ! when too few eta's for polyfit, just simply assign n=0.02
   IF( (Neta-Kbeg_cdu(I,J)+1) < (PolyOrder+1) ) &
     Cdu(I,J)=grav*(0.02_SP)**2/(Max(H_u(I,J), MinDepFric))**(1.0_SP/3.0_SP)
# endif

# if !defined(POLY_FRICTION)
# if !defined(INTERPOLATION)
   CALL CALCULATE_SUBGRID_CD(1,AvgEta,Cdu(I,J))
# endif
# endif

# endif
! end subgrid

  ENDIF
!  end manning

  Cdu(I,J)=MAX(Cdu(I,J), MinCd)
  Cdu(I,J)=MIN(Cdu(I,J), MaxCd)

  IF(IMPLICIT)THEN
   FrcU=Cdu(I,J)*SQRT(U(I,J)**2 &
      +(0.25_SP*(V(I-1,J)+V(I,J)+V(I-1,J+1)+V(I,J+1)))**2)
  ENDIF

END SUBROUTINE CALCULATE_FRCU



SUBROUTINE CALCULATE_FRCV
  USE GLOBAL
  IMPLICIT NONE
  REAL(SP) :: AvgEta,Cv
  INTEGER :: Ktmp,tmpk1,tmpk2

  AvgEta=0.5_SP*(Eta(I,MAX(1,J-1))+Eta(I,J))
  IF(MASK(I,J-1)==0.AND.MASK(I,J)==1)AvgEta=Eta(I,J)
  IF(MASK(I,J)==0.AND.MASK(I,J-1)==1)AvgEta=Eta(I,J-1)
  IF(MASK(I,J-1)==0.AND.MASK(I,J)==0)AvgEta=MIN(Eta(I,MAX(1,J-1)),Eta(I,J))

  IF(MANNING_TYPE=='VARY')Manning=ManningVary(I,J)
  IF(MANNING_TYPE=='UNIFORM'.OR.MANNING_TYPE=='VARY')THEN
   IF(Manning.GT.ZERO)THEN
    Cdv(I,J)=grav*Manning**2/(Max(H_v(I,J), MinDepFric))**(1.0_SP/3.0_SP)
   ENDIF
# if defined(VEGETATION) || defined(ECO_MORPHOLOGY)
   CALL CALCULATE_VEG_DRAG(2,AvgEta,Cv)
   Cdv(I,J)=Cdv(I,J)+Cv
# endif

# if defined(SUBGRID)
  ELSEIF(MANNING_TYPE=='SUBGRID')THEN

# if defined (INTERPOLATION)
   AvgEta=MIN(AvgEta,EtaMaxVal)
   AvgEta=MAX(AvgEta,EtaMinVal)

   tmpk1 = INT((AvgEta-EtaMinVal)/D_Eta+1.0_SP)
   tmpk2 = MIN(tmpk1+1,Neta)
   IF(CdvTab(I,J,tmpk2)<=ZERO)THEN
    Cdv(I,J)=MinCd
   ELSE
    Cdv(I,J)=CdvTab(I,J,tmpk1) + (AvgEta-EtaTab(tmpk1)) &
        *(CdvTab(I,J,tmpk2)-CdvTab(I,J,tmpk1))/D_Eta
   ENDIF
# endif

# if defined(POLY_FRICTION)
   ! sum polynomial terms
   Cdv(I,J)=ZERO
   IF( AvgEta >= EtaTab(Kbeg_cdv(I,J)) )THEN
    DO Ktmp=1,PolyOrder+1
     Cdv(I,J)=Cdv(I,J)+Coef_CdEquivV(I,J,Ktmp)*(AvgEta-AvgEta0)**(Ktmp-1)
    ENDDO
   ENDIF

   ! when too few eta's for polyfit, just simply assign n=0.02
   IF( (Neta-Kbeg_cdv(I,J)+1) < (PolyOrder+1) ) &
     Cdv(I,J)=grav*(0.02_SP)**2/(Max(H_v(I,J), MinDepFric))**(1.0_SP/3.0_SP)
# endif

# if !defined(POLY_FRICTION)
# if !defined(INTERPOLATION)
   CALL CALCULATE_SUBGRID_CD(2,AvgEta,Cdv(I,J))
# endif
# endif

# endif
! end subgrid

  ENDIF
!  end manning

  Cdv(I,J)=MAX(Cdv(I,J), MinCd)
  Cdv(I,J)=MIN(Cdv(I,J), MaxCd)

  IF(IMPLICIT)THEN
   FrcV=Cdv(I,J)*SQRT(V(I,J)**2 &
         +(0.25_SP*(U(I,J)+U(I,J-1)+U(I+1,J)+U(I+1,J-1)))**2)
  ENDIF


END SUBROUTINE CALCULATE_FRCV



# if defined (SUBGRID)
SUBROUTINE CALCULATE_SUBGRID_CD(UorV, AvgEta, Cd)
  USE GLOBAL
  IMPLICIT NONE
  INTEGER, INTENT(IN)  :: UorV
  REAL(SP),INTENT(IN)  :: AvgEta
  REAL(SP),INTENT(OUT) :: Cd
  REAL(SP) :: Volume_AY,TmpCd,FricDepth,TmpFricDepth,TmpManning, Cv, &
        TotFlux,TmpFlux,DeltaFluxL,DeltaFluxR,DeltaFluxU,DeltaFluxD

  INTEGER :: pcount

  Cd=ZERO
  TmpCd=ZERO
  Volume_AY=ZERO
  FricDepth=ZERO
  TmpFricDepth=ZERO
  CdPixel=ZERO
  pcount=0

  DO JJ=1,SubMainGridRatio
  DO II=1,SubMainGridRatio

   ! different computational domain regarding to u or v direction
   IF(UorV==1)THEN
    ! u-direction
    IF(II.LE.HalfRatio)THEN
     ! left part of U cell
     ! HgtPixel(II,JJ)=AvgEta+DepSubGrid(I-1,J,II+HalfRatio,JJ)
     HgtPixel(II,JJ)=EtaSubGrid(I-1,J,II+HalfRatio,JJ)+DepSubGrid(I-1,J,II+HalfRatio,JJ)
     TmpManning=ManningSubGrid(I-1,J,II+HalfRatio,JJ)
    ELSE
     ! right part of U cell
     ! HgtPixel(II,JJ)=AvgEta+DepSubGrid(I,J,II-HalfRatio,JJ)
     HgtPixel(II,JJ)=EtaSubGrid(I,J,II-HalfRatio,JJ)+DepSubGrid(I,J,II-HalfRatio,JJ)
     TmpManning=ManningSubGrid(I,J,II-HalfRatio,JJ)
    ENDIF
   ELSE
    ! v-direction
    IF(JJ.LE.HalfRatio)THEN
     ! lower part of V cell
     ! HgtPixel(II,JJ)=AvgEta+DepSubGrid(I,J-1,II,JJ+HalfRatio)
     HgtPixel(II,JJ)=EtaSubGrid(I,J-1,II,JJ+HalfRatio)+DepSubGrid(I,J-1,II,JJ+HalfRatio)
     TmpManning=ManningSubGrid(I,J-1,II,JJ+HalfRatio)
    ELSE
     ! upper part of V cell
     ! HgtPixel(II,JJ)=AvgEta+DepSubGrid(I,J,II,JJ-HalfRatio)
     HgtPixel(II,JJ)=EtaSubGrid(I,J,II,JJ-HalfRatio)+DepSubGrid(I,J,II,JJ-HalfRatio)
     TmpManning=ManningSubGrid(I,J,II,JJ-HalfRatio)
    ENDIF
   ENDIF

   IF(HgtPixel(II,JJ).GT.ZERO)THEN
    pcount=pcount+1
    Volume_AY=Volume_AY+HgtPixel(II,JJ)
    IF(SUBGRID_CD_FIXED)THEN
     ! just for test
     CdPixel(II,JJ)=Cd_fixed
    ELSE
     ! cd = g*Manning^2/H^(1/3)
     CdPixel(II,JJ)=grav*TmpManning**2 &
               /MAX(MinDepFric, HgtPixel(II,JJ))**(1.0_SP/3.0_SP)
     ! vegetation drag
# if defined(VEGETATION) || defined(ECO_MORPHOLOGY)
     CALL CALCULATE_VEG_DRAG(UorV,AvgEta,Cv)
     CdPixel(II,JJ)=CdPixel(II,JJ)+Cv
# endif
    ENDIF
    TmpFricDepth=TmpFricDepth + &
           HgtPixel(II,JJ)*sqrt(HgtPixel(II,JJ)/CdPixel(II,JJ))
   ELSE
    CdPixel(II,JJ)=ZERO
   ENDIF

  ENDDO
  ENDDO

  ! 'friction depth' in Volp et al. (2013) and Wu et al. (2016)
  IF(real(pcount).GT.ZERO)THEN
   FricDepth=(TmpFricDepth/Volume_AY)**2
  ENDIF

  ! the equivalent friction coeffcient (finally used in coarse grid)
  DO JJ=1,SubMainGridRatio
  DO II=1,SubMainGridRatio
   IF(HgtPixel(II,JJ).GT.ZERO)THEN
    TmpCd = TmpCd + HgtPixel(II,JJ)/FricDepth
   ENDIF
  ENDDO
  ENDDO
  Cd = TmpCd/real(NumPixel)


  ! re-construct and adjust subgrid u
  IF(CONSTRUCT_SUBGRID_UV)THEN

   ! uniform direction, linear factor: alpha_s
   IF(UorV==1)THEN
    DO JJ=1,SubMainGridRatio
    DO II=1,SubMainGridRatio
     IF(HgtPixel(II,JJ).GT.ZERO)THEN
      USubGrid(I,J,II,JJ) = U(I,J)/( sqrt(CdPixel(II,JJ)/HgtPixel(II,JJ)) &
                      *TmpFricDepth/Volume_AY )
     ELSE
      USubGrid(I,J,II,JJ) = ZERO
     ENDIF
    ENDDO
    ENDDO
   ELSEIF(UorV==2)THEN
    DO JJ=1,SubMainGridRatio
    DO II=1,SubMainGridRatio
     IF(HgtPixel(II,JJ).GT.ZERO)THEN
      VSubGrid(I,J,II,JJ) = V(I,J)/( sqrt(CdPixel(II,JJ)/HgtPixel(II,JJ)) &
                      *TmpFricDepth/Volume_AY )
     ELSE
      VSubGrid(I,J,II,JJ) = ZERO
     ENDIF
    ENDDO
    ENDDO
   ENDIF

   ! adjust subgrid u to make mass conserve
   IF(SUBGRID_UV_CORRECTION)THEN

    IF(UorV==1)THEN
     DO II = 1,SubMainGridRatio
      ! total flux through U coarse cell face
      TmpFlux = ZERO
      DO JJ = 1, SubMainGridRatio
       TmpFlux = TmpFlux+USubGrid(I,J,II,JJ)*MAX(HgtPixel(II,JJ),ZERO)
      ENDDO

      IF(TmpFlux.NE.ZERO)THEN
       ! flux difference with neighbor cells
       DeltaFluxL = (P(I,J)-P(I-1,J))/SubMainGridRatio*(II-0.5_SP)
       DeltaFluxR = (P(I+1,J)-P(I,J))/SubMainGridRatio*(II-HalfRatio-0.5_SP)
       DO JJ = 1, SubMainGridRatio
        ! left half cell
        IF(II.LE.HalfRatio)THEN
         IF(P(I-1,J).EQ.ZERO)THEN
          USubGrid(I,J,II,JJ) = USubGrid(I,J,II,JJ)*P(I,J)*REAL(SubMainGridRatio)/TmpFlux
         ELSE
          USubGrid(I,J,II,JJ) = USubGrid(I,J,II,JJ)*(0.5*(P(I,J)+P(I-1,J))+DeltaFluxL)*REAL(SubMainGridRatio)/TmpFlux
         ENDIF
        ! right half cell
        ELSE
         IF(P(I+1,J).EQ.ZERO)THEN
          USubGrid(I,J,II,JJ) = USubGrid(I,J,II,JJ)*P(I,J)*REAL(SubMainGridRatio)/TmpFlux
         ELSE
          USubGrid(I,J,II,JJ) = USubGrid(I,J,II,JJ)*(P(I,J)+DeltaFluxR)*REAL(SubMainGridRatio)/TmpFlux
         ENDIF
        ENDIF
       ENDDO
      ENDIF
      ! end correction
     ENDDO

    ELSEIF(UorV==2)THEN
     DO JJ = 1,SubMainGridRatio
      ! total flux through U coarse cell face
      TmpFlux = ZERO
      DO II = 1, SubMainGridRatio
       TmpFlux = TmpFlux+VSubGrid(I,J,II,JJ)*MAX(HgtPixel(II,JJ),ZERO)
      ENDDO

      IF(TmpFlux.NE.ZERO)THEN
       ! flux difference with neighbor cells
       DeltaFluxD = (Q(I,J)-Q(I,J-1))/REAL(SubMainGridRatio)*(JJ-0.5_SP)
       DeltaFluxU = (Q(I,J+1)-Q(I,J))/REAL(SubMainGridRatio)*(JJ-HalfRatio-0.5_SP)
       DO II = 1,SubMainGridRatio
        ! lower half cell
        IF(JJ.LE.HalfRatio)THEN
         IF(Q(I,J-1).EQ.ZERO)THEN
          VSubGrid(I,J,II,JJ) = VSubGrid(I,J,II,JJ)*Q(I,J)*REAL(SubMainGridRatio)/TmpFlux
         ELSE
          VSubGrid(I,J,II,JJ) = VSubGrid(I,J,II,JJ)*(0.5_SP*(Q(I,J)+Q(I,J-1))+DeltaFluxD)*REAL(SubMainGridRatio)/TmpFlux
         ENDIF
        ! higher half cell
        ELSE
         IF(Q(I,J+1).EQ.ZERO)THEN
          VSubGrid(I,J,II,JJ) = VSubGrid(I,J,II,JJ)*Q(I,J)*REAL(SubMainGridRatio)/TmpFlux
         ELSE
          VSubGrid(I,J,II,JJ) = VSubGrid(I,J,II,JJ)*(Q(I,J)+DeltaFluxU)*REAL(SubMainGridRatio)/TmpFlux
         ENDIF
        ENDIF
       ENDDO
      ENDIF
     ENDDO

    ENDIF
    ! end UorV
   ENDIF
   ! end correction uv

  ENDIF
  ! end construct subgrid uv

END SUBROUTINE CALCULATE_SUBGRID_CD
# endif


SUBROUTINE GET_PIXEL_CENTER_VARIABLES
  USE GLOBAL
  IMPLICIT NONE


  ! subgrid total depth and friction cd
  HgtSubGrid = EtaSubGrid+DepSubGrid

  IF(MANNING_TYPE=='SUBGRID')THEN
    !  convert to u v at pixel centers
    IF(SubMainGridRatio.GT.1)THEN
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
      ! velocity at pixel centers
      DO JJ=1,SubMainGridRatio
       DO II=1,HalfRatio
        UcSubGrid(I,J,II,JJ)=USubGrid(I,J,II+HalfRatio,JJ)
       ENDDO
       DO II=HalfRatio+1,SubMainGridRatio
        UcSubGrid(I,J,II,JJ)=USubGrid(I+1,J,II-HalfRatio,JJ)
       ENDDO
      ENDDO
      DO II=1,SubMainGridRatio
       DO JJ=1,HalfRatio
        VcSubGrid(I,J,II,JJ)=VSubGrid(I,J,II,JJ+HalfRatio)
       ENDDO
       DO JJ=HalfRatio+1,SubMainGridRatio
        VcSubGrid(I,J,II,JJ)=VSubGrid(I,J+1,II,JJ-HalfRatio)
       ENDDO
      ENDDO
     ENDDO
     ENDDO
    ELSEIF(SubMainGridRatio==1)THEN
     ! interpolate to coarse cell centers
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
      UcSubGrid(I,J,1,1)=0.5_SP*(U(I,J)+U(I+1,J))
      VcSubGrid(I,J,1,1)=0.5_SP*(V(I,J)+V(I,J+1))
     ENDDO
     ENDDO
    ENDIF

    DO J=Jbeg,Jend
    DO I=Ibeg,Iend
     DO JJ=1,SubMainGridRatio
     DO II=1,SubMainGridRatio
      IF(HgtSubGrid(I,J,II,JJ).GT.ZERO)THEN
       CdSubGrid(I,J,II,JJ)=grav*ManningSubGrid(I,J,II,JJ)**2 &
            /MAX(HgtSubGrid(I,J,II,JJ),MinDepFric)**(1.0_SP/3.0_SP)
      ELSE
       CdSubGrid(I,J,II,JJ)=ZERO
      ENDIF
      TauSubGrid(I,J,II,JJ)=rho_w*CdSubGrid(I,J,II,JJ)* &
             (UcSubGrid(I,J,II,JJ)**2+VcSubGrid(I,J,II,JJ)**2)
     ENDDO
     ENDDO
    ENDDO
    ENDDO
  ENDIF

END SUBROUTINE GET_PIXEL_CENTER_VARIABLES



# if defined(VEGETATION) || defined(ECO_MORPHOLOGY)
SUBROUTINE CALCULATE_VEG_DRAG(UorV, AvgEta, Cv)
  USE GLOBAL
  IMPLICIT NONE
  INTEGER,INTENT(IN):: UorV
  REAL(SP),INTENT(IN):: AvgEta
  REAL(SP),INTENT(OUT) :: Cv
  REAL(SP) :: tmpVEG_H,tmpVEG_N,tmpVEG_D
  REAL(SP) :: CD_VEG,tmpH

  Cv = 0.0_SP
  CD_VEG = 1.0_SP
  IF(VEG_TYPE=='VARY')THEN
   IF(UorV==1)THEN
    tmpVEG_H = 0.5_SP*(VEG_H(I-1,J)+VEG_H(I,J))
    tmpVEG_D = 0.5_SP*(VEG_D(I-1,J)+VEG_D(I,J))
    tmpVEG_N = 0.5_SP*(VEG_N(I-1,J)+VEG_N(I,J))
    Cv = 0.5_SP*CD_VEG*tmpVEG_D*tmpVEG_N*MIN(tmpVEG_H,H_u(I,J))
   ELSEIF(UorV==2)THEN
    tmpVEG_H = 0.5_SP*(VEG_H(I,J-1)+VEG_H(I,J))
    tmpVEG_D = 0.5_SP*(VEG_D(I,J-1)+VEG_D(I,J))
    tmpVEG_N = 0.5_SP*(VEG_N(I,J-1)+VEG_N(I,J))
    Cv = 0.5_SP*CD_VEG*tmpVEG_D*tmpVEG_N*MIN(tmpVEG_H,H_v(I,J))
   ENDIF
  ELSEIF(VEG_TYPE=='SUBGRID')THEN
   ! u direction
   IF(UorV==1)THEN

     IF(II.LE.HalfRatio)THEN
      ! tmpH = AvgEta + DepSubGrid(I-1,J,II+HalfRatio,JJ)
      tmpH = HgtSubGrid(I-1,J,II+HalfRatio,JJ)
      IF(tmpH.GT.ZERO)THEN
       Cv = 0.5_SP*CD_VEG*VEG_D_SUB(I-1,J,II+HalfRatio,JJ)*VEG_N_SUB(I-1,J,II+HalfRatio,JJ)* &
          MIN(tmpH,VEG_H_SUB(I-1,J,II+HalfRatio,JJ))
      ENDIF
     ELSE
      ! tmpH = AvgEta + DepSubGrid(I,J,II-HalfRatio,JJ)
      tmpH = HgtSubGrid(I,J,II-HalfRatio,JJ)
      IF(tmpH.GT.ZERO)THEN
       Cv = 0.5_SP*CD_VEG*VEG_D_SUB(I,J,II-HalfRatio,JJ)*VEG_N_SUB(I,J,II-HalfRatio,JJ)* &
          MIN(tmpH,VEG_H_SUB(I,J,II-HalfRatio,JJ))
      ENDIF
     ENDIF

   ! v direction
   ELSEIF(UorV==2)THEN

     IF(JJ.LE.HalfRatio)THEN
      ! tmpH = AvgEta + DepSubGrid(I,J-1,II,JJ+HalfRatio)
      tmpH = HgtSubGrid(I,J-1,II,JJ+HalfRatio)
      IF(tmpH.GT.ZERO)THEN
       Cv = 0.5_SP*CD_VEG*VEG_D_SUB(I,J-1,II,JJ+HalfRatio)*VEG_N_SUB(I,J-1,II,JJ+HalfRatio)* &
          MIN(tmpH,VEG_H_SUB(I,J-1,II,JJ+HalfRatio))
      ENDIF
     ELSE
      ! tmpH = AvgEta + DepSubGrid(I,J,II,JJ-HalfRatio)
      tmpH = HgtSubGrid(I,J,II,JJ-HalfRatio)
      IF(tmpH.GT.ZERO)THEN
       Cv = 0.5_SP*CD_VEG*VEG_D_SUB(I,J,II,JJ-HalfRatio)*VEG_N_SUB(I,J,II,JJ-HalfRatio)* &
          MIN(tmpH,VEG_H_SUB(I,J,II,JJ-HalfRatio))
      ENDIF
     ENDIF

   ENDIF

  ENDIF

END SUBROUTINE CALCULATE_VEG_DRAG



!SUBROUTINE CALCULATE_VEG_BAPTIST(UorV,FX_VEG,FY_VEG)
!  USE GLOBAL
!  IMPLICIT NONE
!  INTEGER,INTENT(IN):: UorV
!  REAL(SP),INTENT(OUT) :: FX_VEG,FY_VEG
!  REAL(SP) :: tmpVEG_H,tmpVEG_N,tmpVEG_D
!  REAL(SP) :: CD_VEG,tmpEta,tmpH,tmpDrag
!
!  FX_VEG = 0.0_SP
!  FY_VEG = 0.0_SP
!  CD_VEG = 1.0_SP
!  IF(VEG_TYPE=='VARY')THEN
!   IF(UorV==1)THEN
!    tmpVEG_H = 0.5_SP*(VEG_H(I,J)+VEG_H(I+1,J))
!    tmpVEG_D = 0.5_SP*(VEG_D(I,J)+VEG_D(I+1,J))
!    tmpVEG_N = 0.5_SP*(VEG_N(I,J)+VEG_N(I+1,J))
!    IF(H_u(I,J).LE.tmpVEG_H)THEN
!     FX_VEG = 0.5_SP*CD_VEG*tmpVEG_D*tmpVEG_N*H_u(I,J)* &
!         SQRT(U(I,J)**2+(0.25_SP*(V(I-1,J)+V(I,J)+V(I-1,J+1)+V(I,J+1)))**2)
!    ELSE
!    ! to be completed
!     FX_VEG =
!    ENDIF
!   ELSEIF(UorV==2)THEN
!    tmpVEG_H = 0.5_SP*(VEG_H(I,J)+VEG_H(I,J+1))
!    tmpVEG_D = 0.5_SP*(VEG_D(I,J)+VEG_D(I,J+1))
!    tmpVEG_N = 0.5_SP*(VEG_N(I,J)+VEG_N(I,J+1))
!    FY_VEG = 0.5_SP*CD_VEG*tmpVEG_D*tmpVEG_N*MIN(tmpVEG_H,H_v(I,J))* &
!        SQRT(V(I,J)**2+(0.25_SP*(U(I,J)+U(I,J-1)+U(I+1,J)+U(I+1,J-1)))**2)
!   ENDIF
!  ELSEIF(VEG_TYPE=='SUBGRID')THEN
!END SUBROUTINE CALCULATE_VEG_BAPTIST

# endif



SUBROUTINE CONSTRUCT_SUBGRID_ETA
  USE GLOBAL
  IMPLICIT NONE
  REAL(SP) :: fac0,fac1,fac2

  ! interpolate using nearest 3 eta, for each quadrant
  DO J=Jbeg,Jend
  DO I=Ibeg,Iend
   ! 1st quadrant
   IF(MASKu(I+1,J)*MASKv(I,J+1)==1)THEN
    ! neighboring wet cells
    DO JJ=HalfRatio+1,SubMainGridRatio
    DO II=HalfRatio+1,SubMainGridRatio
     fac1=(II-HalfRatio-0.5_SP)/REAL(SubMainGridRatio)
     fac2=(JJ-HalfRatio-0.5_SP)/REAL(SubMainGridRatio)
     fac0=1.0_SP-fac1-fac2
     EtaSubGrid(I,J,II,JJ)=fac0*Eta(I,J)+fac1*Eta(I+1,J)+fac2*Eta(I,J+1)
    ENDDO
    ENDDO
   ELSE
    ! neighboring a dry cell
    DO JJ=HalfRatio+1,SubMainGridRatio
    DO II=HalfRatio+1,SubMainGridRatio
     EtaSubGrid(I,J,II,JJ)=Eta(I,J)
    ENDDO
    ENDDO
   ENDIF
   ! 2nd quadrant
   IF(MASKu(I,J)*MASKv(I,J+1)==1)THEN
    DO JJ=HalfRatio+1,SubMainGridRatio
    DO II=1,HalfRatio
     fac1=(HalfRatio-II+0.5_SP)/REAL(SubMainGridRatio)
     fac2=(JJ-HalfRatio-0.5_SP)/REAL(SubMainGridRatio)
     fac0=1.0_SP-fac1-fac2
     EtaSubGrid(I,J,II,JJ)=fac0*Eta(I,J)+fac1*Eta(I-1,J)+fac2*Eta(I,J+1)
    ENDDO
    ENDDO
   ELSE
    ! neighboring a dry cell
    DO JJ=HalfRatio+1,SubMainGridRatio
    DO II=1,HalfRatio
     EtaSubGrid(I,J,II,JJ)=Eta(I,J)
    ENDDO
    ENDDO
   ENDIF
   ! 3rd quadrant
   IF(MASKu(I,J)*MASKv(I,J)==1)THEN
    DO JJ=1,HalfRatio
    DO II=1,HalfRatio
     fac1=(HalfRatio-II+0.5_SP)/REAL(SubMainGridRatio)
     fac2=(HalfRatio-JJ+0.5_SP)/REAL(SubMainGridRatio)
     fac0=1.0_SP-fac1-fac2
     EtaSubGrid(I,J,II,JJ)=fac0*Eta(I,J)+fac1*Eta(I-1,J)+fac2*Eta(I,J-1)
    ENDDO
    ENDDO
   ELSE
    DO JJ=1,HalfRatio
    DO II=1,HalfRatio
     EtaSubGrid(I,J,II,JJ)=Eta(I,J)
    ENDDO
    ENDDO
   ENDIF
   ! 4th quadrant
   IF(MASKu(I+1,J)*MASKv(I,J)==1)THEN
    DO JJ=1,HalfRatio
    DO II=HalfRatio+1,SubMainGridRatio
     fac1=(II-HalfRatio-0.5_SP)/REAL(SubMainGridRatio)
     fac2=(HalfRatio-JJ+0.5_SP)/REAL(SubMainGridRatio)
     fac0=1.0_SP-fac1-fac2
     EtaSubGrid(I,J,II,JJ)=fac0*Eta(I,J)+fac1*Eta(I+1,J)+fac2*Eta(I,J-1)
    ENDDO
    ENDDO
   ELSE
    DO JJ=1,HalfRatio
    DO II=HalfRatio+1,SubMainGridRatio
     EtaSubGrid(I,J,II,JJ)=Eta(I,J)
    ENDDO
    ENDDO
   ENDIF

  ENDDO
  ENDDO

# if defined(PARALLEL)
  CALL PHI_EXCH_SUBGRID(EtaSubGrid)
# endif

END SUBROUTINE CONSTRUCT_SUBGRID_ETA



SUBROUTINE TRIDIAGONAL
  USE GLOBAL
  IMPLICIT NONE
  REAL(SP),DIMENSION(Mloc,Nloc) :: myA,myC,myD,myF
  REAL(SP),DIMENSION(Mloc) :: myAx,myCx,myDx,myFx
  REAL(SP),DIMENSION(Nloc) :: myAy,myCy,myDy,myFy


  ETA0=ETA
! x-direction
  DO J=Jbeg,Jend
  DO I=Ibeg,Iend
   tmp1=-dt_over_dx2*0.5_SP*A_u(I,J)
   tmp3=-dt_over_dx2*0.5_SP*A_u(I+1,J)
   tmp2=1.0_SP*POROSITY(I,J)+dt_over_dx2*0.5_SP*A_u(I+1,J) + dt_over_dx2*0.5_SP*A_u(I,J)
   tmp4=ETA0(I,J)*POROSITY(I,J) &
          +dt_over_dy2*0.5_SP*(A_v(I,J+1)*ETA0(I,J+1)-A_v(I,J+1)*ETA0(I,J)) &
          -dt_over_dy2*0.5_SP*(A_v(I,J)*ETA0(I,J)-A_v(I,J)*ETA0(I,J-1)) &
          -dt_over_dx*0.5_SP*(B(I+1,J)-B(I,J)) &
          -dt_over_dy*0.5_SP*(C(I,J+1)-C(I,J))
   IF(tmp2.NE.0.0_SP)THEN
    myA(I,J)=tmp1/tmp2
    myC(I,J)=tmp3/tmp2
    myD(I,J)=tmp4/tmp2
   ELSE
    myA(I,J)=ZERO
    myC(I,J)=ZERO
    myD(I,J)=ZERO
   ENDIF

  ENDDO ! end I

   call TRIG(myA(:,J),myC(:,J),myD(:,J),myF(:,J),Mloc,Ibeg,Iend)
   ETA(Ibeg:Iend,J) = myF(Ibeg:Iend,J)

  ENDDO  ! end J

  ETA0=ETA

! y direction
  DO I=Ibeg,Iend
  DO J=Jbeg,Jend
   tmp1=-dt_over_dy2*0.5_SP*A_v(I,J)
   tmp3=-dt_over_dy2*0.5_SP*A_v(I,J+1)
   tmp2=1.0_SP*POROSITY(I,J)+dt_over_dy2*0.5_SP*A_v(I,J+1) + dt_over_dy2*0.5_SP*A_v(I,J)
   tmp4=ETA0(I,J)*POROSITY(I,J) &
          +dt_over_dx2*0.5_SP*(A_u(I+1,J)*ETA0(I+1,J)-A_u(I+1,J)*ETA0(I,J)) &
          -dt_over_dx2*0.5_SP*(A_u(I,J)*ETA0(I,J)-A_u(I,J)*ETA0(I-1,J)) &
          -dt_over_dx*0.5_SP*(B(I+1,J)-B(I,J)) &
          -dt_over_dy*0.5_SP*(C(I,J+1)-C(I,J))
   IF(tmp2.NE.0.0_SP)THEN
    myA(I,J)=tmp1/tmp2
    myC(I,J)=tmp3/tmp2
    myD(I,J)=tmp4/tmp2
   ELSE
    myA(I,J)=ZERO
    myC(I,J)=ZERO
    myD(I,J)=ZERO
   ENDIF

  ENDDO ! end J

   call TRIG(myA(I,:),myC(I,:),myD(I,:),myF(I,:),Nloc,Jbeg,Jend)
   ETA(I,Jbeg:Jend) = myF(I,Jbeg:Jend)

  ENDDO ! end I

100  continue


END SUBROUTINE TRIDIAGONAL



SUBROUTINE TRIG(A,C,D,Z,M,Ibeg,Iend)
    USE PARAM
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: M,Ibeg,Iend
    REAL(SP),DIMENSION(M),INTENT(INOUT) :: A,C,D
    REAL(SP),DIMENSION(M),INTENT(OUT) :: Z

    DO II=Ibeg+1,Iend
     IF(A(II).NE.ZERO)THEN
      C(II)=C(II)/A(II)/(1.0_SP/A(II)-C(II-1))
      D(II)=(D(II)/A(II)-D(II-1))/(1.0_SP/A(II)-C(II-1))
     ENDIF
    ENDDO

    Z(Iend)=D(Iend)

    DO II=Iend-1,Ibeg,-1
     Z(II)=D(II)-C(II)*Z(II+1)
    ENDDO

END SUBROUTINE TRIG



# if defined (HOPSCOTCH_2STEP_OLD_ETA)
SUBROUTINE HOPSCOTCH_SCHEME
  USE GLOBAL
  IMPLICIT NONE
  REAL :: incr

  ! eta0: previous time step
  ETA0=ETA

  DO J=Jbeg,Jend
   BIN0 = MOD(J+BIN1,2)
  DO I=Ibeg+BIN0,Iend,2
   incr = DT/DX2*(A_u(I+1,J)*(ETA(I+1,J)-ETA(I,J)) &
                  -A_u(I,J)*(ETA(I,J)-ETA(I-1,J))) &
             + DT/DY2*(A_v(I,J+1)*(ETA(I,J+1)-ETA(I,J)) &
                  -A_v(I,J)*(ETA(I,J)-ETA(I,J-1))) &
             - DT/DX*(B(I+1,J)-B(I,J)) &
             - DT/DY*(C(I,J+1)-C(I,J))

   IF(POROSITY(I,J)>ZERO)THEN
    ETA(I,J) = ETA(I,J) + incr/POROSITY(I,J)
   ELSE
    ETA(I,J) = ETA(I,J) + incr
   ENDIF
  ENDDO
  ENDDO

  DO J=Jbeg,Jend
   BIN0 = MOD(J+BIN1+1,2)
  DO I=Ibeg+BIN0,Iend,2
   incr = DT/DX2*(A_u(I+1,J)*(ETA(I+1,J)-ETA(I,J)) &
                  -A_u(I,J)*(ETA(I,J)-ETA(I-1,J))) &
             + DT/DY2*(A_v(I,J+1)*(ETA(I,J+1)-ETA(I,J)) &
                  -A_v(I,J)*(ETA(I,J)-ETA(I,J-1))) &
             - DT/DX*(B(I+1,J)-B(I,J)) &
             - DT/DY*(C(I,J+1)-C(I,J))
   IF(POROSITY(I,J)>ZERO)THEN
    ETA(I,J) = ETA(I,J) + incr/POROSITY(I,J)
   ELSE
    ETA(I,J) = ETA(I,J) + incr
   ENDIF
  ENDDO
  ENDDO

  IF(BIN1==0)THEN
   BIN1=1
  ELSE
   BIN1=0
  ENDIF

# if defined (PARALLEL)
  CALL PHI_EXCH_1(ETA)
# endif

  ! for use in sediment model
  ETA_OVER_DT = (ETA-ETA0)/DT

END SUBROUTINE HOPSCOTCH_SCHEME

# else
SUBROUTINE HOPSCOTCH_SCHEME
  USE GLOBAL
# if defined (SUBSURFACE)
      USE SUBSURFACE_MODULE
# endif
  IMPLICIT NONE
  REAL :: incr1,incr2,poro
  REAL :: Pgrad1,Pgrad2,Pgrad3,Pgrad4,eta_i1,eta_i2,eta_j1,eta_j2

  ! eta0: previous time step
  ETA0=ETA

  DO J=Jbeg,Jend
   BIN0 = MOD(J+BIN1,2)
  DO I=Ibeg+BIN0,Iend,2

   IF(POROSITY(I,J)>ZERO)THEN
    poro=POROSITY(I,J)
   ELSE
    poro=1.0
   ENDIF

   Pgrad1 = ETA(I+1,J)-ETA(I,J)
   Pgrad2 = ETA(I,J)-ETA(I-1,J)
   Pgrad3 = ETA(I,J+1)-ETA(I,J)
   Pgrad4 = ETA(I,J)-ETA(I,J-1)
  
   IF(MASK(I+1,J)<1) Pgrad1=MIN(ZERO,EtaScreen(I+1,J)-Eta(I,J))
   IF(MASK(I-1,J)<1) Pgrad2=MAX(ZERO,Eta(I,J)-EtaScreen(I-1,J))
   IF(MASK(I,J+1)<1) Pgrad3=MIN(ZERO,EtaScreen(I,J+1)-Eta(I,J))
   IF(MASK(I,J-1)<1) Pgrad4=MAX(ZERO,Eta(I,J)-EtaScreen(I,J-1))

   incr1 = DT/DX2*(A_u(I+1,J)*Pgrad1 &
                         -A_u(I,J)*Pgrad2) &
                         + DT/DY2*(A_v(I,J+1)*Pgrad3 &
                         -A_v(I,J)*Pgrad4) &
                         - DT/DX*(B(I+1,J)-B(I,J)) &
                         - DT/DY*(C(I,J+1)-C(I,J))

# if defined (SUBSURFACE) 
! $$$
                 IF(MASK(I,J)>0.AND.MASKu(I,J)==0)THEN
                   incr1=incr1+DT/DX*P_ground(I,J)
                 ENDIF
                 IF(MASK(I,J)>0.AND.MASKu(I+1,J)==0)THEN
                   incr1=incr1-DT/DX*P_ground(I+1,J)
                 ENDIF
                 IF(MASK(I,J)>0.AND.MASKv(I,J)==0)THEN
                   incr1=incr1+DT/DY*Q_ground(I,J)
                 ENDIF
                 IF(MASK(I,J)>0.AND.MASKv(I,J+1)==0)THEN
                   incr1=incr1+DT/DY*Q_ground(I,J+1)
                 ENDIF

# endif

                 ETA(I,J)=ETA(I,J) + incr1/poro

  ENDDO
  ENDDO

  DO J=Jbeg,Jend
   BIN0 = MOD(J+BIN1+1,2)
  DO I=Ibeg+BIN0,Iend,2

   IF(POROSITY(I,J)>ZERO)THEN
    poro=POROSITY(I,J)
   ELSE
    poro=1.0
   ENDIF

   eta_i1=ETA(I+1,J)
   Pgrad1 = 1.0_SP
   IF(MASK(I+1,J)<1) THEN
     eta_i1 = EtaScreen(I+1,J)
     IF(EtaScreen(I+1,J)-ETA(I,J)>ZERO) Pgrad1 = ZERO
   ENDIF

   eta_i2=ETA(I-1,J)
   Pgrad2 = 1.0_SP
   IF(MASK(I-1,J)<1) THEN
     eta_i2 = EtaScreen(I-1,J)
     IF(ETA(I,J)-EtaScreen(I-1,J)<ZERO) Pgrad2 = ZERO
   ENDIF

   eta_j1=ETA(I,J+1)
   Pgrad3 = 1.0_SP
   IF(MASK(I,J+1)<1) THEN
     eta_j1 = EtaScreen(I,J+1)
     IF(EtaScreen(I,J-1)-ETA(I,J)>ZERO) Pgrad3 = ZERO
   ENDIF

   eta_j2=ETA(I,J-1)
   Pgrad4 = 1.0_SP
   IF(MASK(I,J-1)<1) THEN
     eta_j2 = EtaScreen(I,J-1)
     IF(ETA(I,J)-EtaScreen(I,J-1)<ZERO) Pgrad4 = ZERO
   ENDIF

   incr1 = DT/DX2*(A_u(I+1,J)*eta_i1*Pgrad1 &
                         +A_u(I,J)*eta_i2*Pgrad2) &
                         + DT/DY2*(A_v(I,J+1)*eta_j1*Pgrad3 &
                         +A_v(I,J)*eta_j2*Pgrad4) &
                         - DT/DX*(B(I+1,J)-B(I,J)) &
                         - DT/DY*(C(I,J+1)-C(I,J))

# if defined (SUBSURFACE) 
!$$$
                 IF(MASK(I,J)>0.AND.MASKu(I,J)==0)THEN
                   incr1=incr1+DT/DX*P_ground(I,J)
                 ENDIF
                 IF(MASK(I,J)>0.AND.MASKu(I+1,J)==0)THEN
                   incr1=incr1-DT/DX*P_ground(I+1,J)
                 ENDIF
                 IF(MASK(I,J)>0.AND.MASKv(I,J)==0)THEN
                   incr1=incr1+DT/DY*Q_ground(I,J)
                 ENDIF
                 IF(MASK(I,J)>0.AND.MASKv(I,J+1)==0)THEN
                   incr1=incr1+DT/DY*Q_ground(I,J+1)
                 ENDIF

# endif

   incr2 = DT/DX2*(A_u(I+1,J)*Pgrad1+A_u(I,J)*Pgrad2) &
                         + DT/DY2*(A_v(I,J+1)*Pgrad3+A_v(I,J)*Pgrad4)

                 ETA(I,J)=(ETA(I,J)*poro + incr1)/(poro+incr2)

  ENDDO
  ENDDO

  IF(BIN1==0)THEN
   BIN1=1
  ELSE
   BIN1=0
  ENDIF

# if defined (PARALLEL)
  CALL PHI_EXCH_1(ETA)
# endif

  ! for use in sediment model
  ETA_OVER_DT = (ETA-ETA0)/DT


# if defined(DEBUG)
# if defined (PARALLEL)
  IF(myid == 0)THEN
   WRITE(3,*)'Subroutine hopscotch_scheme'
  ENDIF
# else
   WRITE(3,*)'Subroutine hopscotch_scheme'
# endif
# endif

        CALL FILLIN_ETA_GHOST


END SUBROUTINE HOPSCOTCH_SCHEME

SUBROUTINE FILLIN_ETA_GHOST
  USE GLOBAL
  IMPLICIT NONE

! west
# if defined(PARALLEL)
 if( n_west .eq. MPI_PROC_NULL )then
# endif

  DO J=1,Nloc
  DO I=1,Nghost
   ETA(I,J)=ETA(Ibeg,J)
  ENDDO
  ENDDO

# if defined(PARALLEL)
 endif
# endif

! east
# if defined(PARALLEL)
 if( n_east .eq. MPI_PROC_NULL )then
# endif

  DO J=1,Nloc
  DO I=Iend+1,Mloc
   ETA(I,J)=ETA(Iend,J)
  ENDDO
  ENDDO

# if defined(PARALLEL)
 endif
# endif

! south
# if defined(PARALLEL)
 if( n_suth .eq. MPI_PROC_NULL )then
# endif

  DO J=1,Nghost
  DO I=1,Mloc
   ETA(I,J)=ETA(I,Jbeg)
  ENDDO
  ENDDO

# if defined(PARALLEL)
 endif
# endif

! north
# if defined(PARALLEL)
 if( n_nrth .eq. MPI_PROC_NULL )then
# endif

  DO J=Jend+1,Nloc
  DO I=1,Mloc
   ETA(I,J)=ETA(I,Jend)
  ENDDO
  ENDDO

# if defined(PARALLEL)
 endif
# endif

END SUBROUTINE FILLIN_ETA_GHOST

# endif
