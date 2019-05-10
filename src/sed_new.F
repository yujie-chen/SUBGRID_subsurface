# if defined (SEDIMENT)
SUBROUTINE SEDIMENT_TRANSPORT
    USE GLOBAL
    IMPLICIT NONE
! for cohesive sediment:
!   1) calculate concentration
!   2) calculate bed change
! for non-cohesive sediment:
!   1) calculate total load, or
!   2) calculate bedload and suspended load
!   3) adjust bedload for bed slope effect
!   4) calcaulte concentration
!   5) update bed level


    ! calculate tau
    CALL CAL_BED_SHEAR_STRESS

    ! erosion/deposition
    IF(COHESIVE)THEN
      ! P-K formula
      CALL PARTHENIADS_KRONE
    ELSE
      ! sand flux, (m^2/s)
      IF(SubMainGridRatio==1)THEN
        CALL SAND_TRANSPORT_NO_SUBGRID
      ELSE
        CALL SAND_TRANSPORT_SUBGRID
      ENDIF 

      ! bed slope effect
      IF(BedSlopeEffect)THEN
        CALL ADJUST_BEDLOAD
      ENDIF

      ! update concentration
      IF(SUSPENDED_LOAD.AND.VR93)THEN
        CALL CAL_SAND_CONCENTRATION
      ENDIF
    ENDIF

    ! update water depth
    IF(UPDATE_BED.AND.TIME.GT.TIME_MORPH_SPINUP)THEN
      CALL UPDATE_BED_LEVEL
    ENDIF


# if defined(DEBUG)
# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'Subroutine SEDIMENT_TRANSPORT'
    ENDIF
# else
     WRITE(3,*)'Subroutine SEDIMENT_TRANSPORT'
# endif
# endif

# endif
! sediment transport



# if defined (SEDIMENT)
SUBROUTINE InitSedConcentration
    USE GLOBAL
    IMPLICIT NONE

    IF(COHESIVE.OR.SUSPENDED_LOAD)THEN
      ALLOCATE(CON(Mloc,Nloc),CON0(Mloc,Nloc))
      IF(INITIAL_SED)THEN
        CALL GetFile (INITIAL_SED_FILE,CON)
      ELSE
        CON = ZERO
      ENDIF
    ENDIF

# if defined (DEBUG)
# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'sediment initialization completed'
    ENDIF
# else
     WRITE(3,*)'sediment initialization completed'
# endif
# endif
END SUBROUTINE InitSedConcentration
# endif


# if defined(SEDIMENT)
SUBROUTINE CAL_BED_SHEAR_STRESS
     USE GLOBAL
     IMPLICIT NONE
! calculate bed stress at:
! a) subgrid pixel centers
! b) coarse grid cell edges

!!!! 2016-1-12 CduSubGrid and CdvSubGrid are deleted so this subroutine has to be modified!!

     !!!! define and allocate tau_u tau_v,VelMagC

     IF(SubMainGridRatio.GT.1)THEN

       DO J=Jbeg,Jend
       DO I=Ibeg,Iend

         ! velocity at pixel centers
         DO JJ=1,SubMainGridRatio
           DO II=1,HalfRatio
             Us(I,J,II,JJ)=USubGrid(I,J,II+HalfRatio,JJ)
             Cdus(I,J,II,JJ)=CduSubGrid(I,J,II+HalfRatio,JJ)
           ENDDO
           DO II=HalfRatio+1,SubMainGridRatio
             Us(I,J,II,JJ)=USubGrid(I+1,J,II-HalfRatio,JJ)
             Cdus(I,J,II,JJ)=CduSubGrid(I+1,J,II-HalfRatio,JJ)
           ENDDO
         ENDDO
         DO II=1,SubMainGridRatio
           DO JJ=1,HalfRatio
             Vs(I,J,II,JJ)=VSubGrid(I,J,II,JJ+HalfRatio)
             Cdvs(I,J,II,JJ)=CdvSubGrid(I,J,II,JJ+HalfRatio)
           ENDDO
           DO JJ=HalfRatio+1,SubMainGridRatio
             Vs(I,J,II,JJ)=VSubGrid(I,J+1,II,JJ-HalfRatio)
             Cdvs(I,J,II,JJ)=CdvSubGrid(I,J+1,II,JJ-HalfRatio)
           ENDDO
         ENDDO

       ENDDO
       ENDDO
       
     ELSEIF(SubMainGridRatio==1)THEN
      
       ! interpolate to coarse cell centers
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend
         Us(I,J,1,1)=0.5_SP*(U(I,J)+U(I+1,J))
         Vs(I,J,1,1)=0.5_SP*(V(I,J)+V(I,J+1))
         Cdus(I,J,1,1)=0.5_SP*(Cdu(I,J)+Cdu(I+1,J))
         Cdvs(I,J,1,1)=0.5_SP*(Cdv(I,J)+Cdv(I,J+1))
       ENDDO
       ENDDO
       
     ENDIF

     ! bed stress at coarse grid and pixel centers
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
       DO JJ=1,SubMainGridRatio
       DO II=1,SubMainGridRatio
          TAU(I,J,II,JJ)=RHO_W*SQRT( Cdus(I,J,II,JJ)**2 + Cdvs(I,J,II,JJ)**2 ) * &
                       (Us(I,J,II,JJ)**2+Vs(I,J,II,JJ)**2)
       ENDDO
       ENDDO
     ENDDO
     ENDDO

     IF(SubMainGridRatio==1)THEN
       
       ! velocity magnitude and tau at cell edges
       DO I=Ibeg,Iend1
       DO J=Jbeg,Jend
         IF(MASKv(I,J)*MASKv(I,J+1)*MASKv(I-1,J)*MASKv(I-1,J+1)==1)THEN
           VelMagU(I,J)=sqrt( U(I,J)**2 + 0.25_SP**2* &
                             (V(I,J)+V(I,J+1)+V(I-1,J)+V(I-1,J+1))**2 )
         ELSE
           VelMagU(I,J)=abs(U(I,J))
         ENDIF
         TAU_U(I,J)=RHO_W*Cdu(I,J)*VelMagU(I,J)*U(I,J)
       ENDDO
       ENDDO

       DO I=Ibeg,Iend
       DO J=Jbeg,Jend1
         IF(MASKu(I,J)*MASKu(I+1,J)*MASKu(I,J-1)*MASKu(I+1,J-1)==1)THEN
           VelMagV(I,J)=sqrt( V(I,J)**2 + 0.25_SP**2* &
                             (U(I,J)+U(I+1,J)+U(I,J-1)+U(I+1,J-1))**2 )
         ELSE
           VelMagV(I,J)=abs(V(I,J))
         ENDIF
         TAU_V(I,J)=RHO_W*Cdv(I,J)*VelMagV(I,J)*V(I,J)
       ENDDO
       ENDDO
       
       DO I=Ibeg,Iend
       DO J=Jbeg,Jend
         VelMagC(I,J)=0.25_SP*sqrt( (VelMagU(I,J)+VelMagU(I+1,J))**2 + &
                                    (VelMagV(I,J)+VelMagV(I,J+1))**2 ) 
       ENDDO
       ENDDO
       
     ENDIF
     
END SUBROUTINE CAL_BED_SHEAR_STRESS
# endif



# if defined (SEDIMENT)
SUBROUTINE PARTHENIADS_KRONE
    USE GLOBAL
    IMPLICIT NONE
    
    !!!! define and allocate erosion/deposit,DSED_OVER_DT    
    EROSION = ZERO
    DEPOSIT = ZERO
    DO J = Jbeg, Jend
    DO I = Ibeg, Iend     
      DO JJ = 1, SubMainGridRatio
      DO II = 1, SubMainGridRatio
        EROSION(I,J) = EROSION(I,J) + ErosionRate*(1.0-BedPorosity)/NumPixel* &
                                      MAX((Tau(I,J,II,JJ)/Tau_CE-1.0),ZERO)
        DEPOSIT(I,J) = DEPOSIT(I,J) + Ws/NumPixel* &
                                      MAX((1.0-Tau(I,J,II,JJ)/Tau_CD),ZERO)
      ENDDO
      ENDDO
    ENDDO
    ENDDO

    ! scalar advection-diffusion
    CON0 = CON
    CALL ADVECTION_DIFFUSION(CON0,DSED_OVER_DT)
        
    ! update concentration
    CALL CAL_MUD_CONCENTRATION(EROSION,DEPOSIT,CON)

# if defined(PARALLEL)
    ! exchange concentration
    CALL PHI_EXCH_1(CON)
# endif

END SUBROUTINE PARTHENIADS_KRONE
# endif



# if defined(SEDIMENT)
SUBROUTINE CAL_MUD_CONCENTRATION(EROSION,DEPOSIT,CON)
    USE GLOBAL
    IMPLICIT NONE
    REAL(SP),DIMENSION(Mloc,Nloc),INTENT(IN) :: EROSION,DEPOSIT,DSED_OVER_DT
    REAL(SP),DIMENSION(Mloc,Nloc),INTENT(OUT):: CON
    
    ! implicit for deposit  
    DO J = Jbeg, Jend
    DO I = Ibeg, Iend
      CON(I,J) = 1.0_SP/( MAX(H(I,J),MinDepth)/DT +       &
                          POROSITY(I,J)*ETA_OVER_DT(I,J)+ &
                          DEPOSIT(I,J) )* &
                        ( MAX(H(I,J),MinDepth)*CON0(I,J)/DT + &
                          DSED_OVER_DT(I,J) + &
                          EROSION(I,J) )
    ENDDO
    ENDDO    
      
END SUBROUTINE CAL_MUD_CONCENTRATION
# endif



# if defined(SEDIMENT)
SUBROUTINE CAL_SAND_CONCENTRATION
    USE GLOBAL
    IMPLICIT NONE
    
    ! advection-diffusion
    CALL ADVECTION_DIFFUSION(CON0,DSED_OVER_DT)
    
    ! equillibrium concentration
    DO J = Jbeg, Jend
    DO I = Ibeg, Iend
      CON_EQ(I,J) = 0.25_SP*(QsedU(I,J)+QsedU(I+1,J)+QsedV(I,J)+QsedV(I+1,J))
    ENDDO
    ENDDO 
    
END SUBROUTINE CAL_SAND_CONCENTRATION
# endif



# if defined (SEDIMENT)
SUBROUTINE ADVECTION_DIFFUSION(CON,DSED_OVER_DT)
    USE GLOBAL
    IMPLICIT NONE
    REAL(SP),DIMENSION(Mloc,Nloc),INTENT(IN) :: CON
    REAL(SP),DIMENSION(Mloc,Nloc),INTENT(OUT):: DSED_OVER_DT
    
    DO J = Jbeg, Jend
    DO I = Ibeg, Iend
         
       ! advection terms, upwind
       ADVX = (MAX(P(I+1,J),ZERO)*CON(I,J) + MIN(P(I+1,J),ZERO)*CON(I+1,J) &
              -MAX(P(I,J),ZERO)*CON(I-1,J) - MIN(P(I,J),ZERO)*CON(I,J) )/DX
       ADVY = (MAX(Q(I,J+1),ZERO)*CON(I,J) + MIN(Q(I,J+1),ZERO)*CON(I,J+1) &
              -MAX(Q(I,J),ZERO)*CON(I,J-1) - MIN(Q(I,J),ZERO)*CON(I,J) )/DY

       ! diffusion terms
       ! DIFX = NU_SED*H(I,J)*(CON0(I+1,J)-2.0_SP*CON0(I,J)+CON0(I-1,J))/DX2
       ! DIFY = NU_SED*H(I,J)*(CON0(I,J+1)-2.0_SP*CON0(I,J)+CON0(I,J-1))/DY2
       DIFX = NU_SED*( H_u(I+1,J)*(CON(I+1,J)-CON(I,J)) &
                       - H_u(I,J)*(CON(I,J)-CON(I-1,J)) )/DX2
       DIFY = NU_SED*( H_v(I,J+1)*(CON(I,J+1)-CON(I,J)) &
                       - H_v(I,J)*(CON(I,J)-CON(I,J-1)) )/DY2      
    
       ! DSED_OVER_DT
       DSED_OVER_DT(I,J)= - ADVX - ADVY + DIFX + DIFY
    
    ENDDO
    ENDDO

END SUBROUTINE ADVECTION_DIFFUSION
# endif



# if defined(SEDIMENT)
SUBROUTINE UPDATE_BED_LEVEL
    USE GLOBAL
    IMPLICIT NONE
    
    
    IF(SubMainGridRatio==1)THEN
      ! sed sink/source
      IF(NONCOHESIVE)THEN
        DO J=Jbeg,Jend
        DO I=Ibeg,Iend
          SinkSource(I,J)=(QsedU(I+1,J)-QsedU(I,J))/DX + &
                          (QsedV(I,J+1)-QsedV(I,J))/DY
        ENDDO
        ENDDO
      ELSE
        DO J=Jbeg,Jend
        DO I=Ibeg,Iend
          SinkSource(I,J)= ErosionRate*MAX((Tau(I,J)/Tau_CE-1.0),ZERO) &
                         - CON(I,J)*Ws*MAX((1.0-Tau(I,J)/Tau_CD),ZERO)
        ENDDO
        ENDDO
        ! be aware of the unit when cohesive formula!
        SinkSource = SinkSource/rho_sed_bulk
      ENDIF
      
      ! exner equation
      DO J=Jbeg,Jend
      DO I=Ibeg,Iend
        DepSubGrid(I,J,1,1) = DepSubGrid(I,J,1,1) + MASK_DRY(I,J,1,1)/(1.0_SP-BedPorosity)*&
                              DT*MorphFactor*SinkSource(I,J)
                    ! + 1.0_SP/(1.0-BedPorosity)*(CON(I,J)*MAX(H(I,J),ZERO)-CON0(I,J)*MAX(H0(I,J),ZERO))/rho_sed_bulk
      ENDDO
      ENDDO
    ELSE
      ! to be done
      
    ENDIF         
    
# if defined (PARALLEL)
    ! exchange depth
    CALL PHI_EXCH_SUBGRID(DepSubGrid)
    CALL UPDATE_BND_GHOST(DepSubGrid)
# endif

END SUBROUTINE UPDATE_BED_LEVEL
# endif



# if defined(SEDIMENT)
SUBROUTINE SAND_TRANSPORT_NO_SUBGRID
     USE GLOBAL
     IMPLICIT NONE

!!! define and allocate QsedU, QsedV
     IF(EH67)THEN
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend1
         CALL ENGELUND_HANSEN(VelMagU(I,J),U(I,J),Cdu(I,J),D50,QsedU(I,J))
       ENDDO
       ENDDO
       DO J=Jbeg,Jend1
       DO I=Ibeg,Iend
         CALL ENGELUND_HANSEN(VelMagV(I,J),V(I,J),Cdv(I,J),D50,QsedV(I,J))
       ENDDO
       ENDDO
     ELSEIF(MPM48)THEN
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend1
         CALL MEYER_PETER_MULLER(VelMagU(I,J),U(I,J),Cdu(I,J),D50,QsedU(I,J))
       ENDDO
       ENDDO
       DO J=Jbeg,Jend1
       DO I=Ibeg,Iend
         CALL MEYER_PETER_MULLER(VelMagV(I,J),V(I,J),Cdv(I,J),D50,QsedV(I,J))
       ENDDO
       ENDDO
     ELSEIF(SR97)THEN
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend1
         CALL SOULSBY_VANRIJN(VelMagU(I,J),U(I,J),VelMagC(I,J),Cdu(I,J),D50,D90,H_u(I,J), &
                              QsedU(I,J))
       ENDDO
       ENDDO
       DO J=Jbeg,Jend1
       DO I=Ibeg,Iend
         CALL SOULSBY_VANRIJN(VelMagV(I,J),V(I,J),VelMagC(I,J),Cdv(I,J),D50,D90,H_v(I,J), &
                              QsedV(I,J))
       ENDDO
       ENDDO       
     ELSEIF(VR93)THEN
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend1
         CALL VAN_RIJN93(VelMagU(I,J),U(I,J),TAU_U(I,J),D50,D90,H_u(I,J),QsedU(I,J))
       ENDDO
       ENDDO
       DO J=Jbeg,Jend1
       DO I=Ibeg,Iend
         CALL VAN_RIJN93(VelMagV(I,J),V(I,J),TAU_V(I,J),D50,D90,H_v(I,J),QsedV(I,J))
       ENDDO
       ENDDO
     ENDIF
         
END SUBROUTINE SAND_TRANSPORT_NO_SUBGRID
# endif



# if defined(SEDIMENT)
SUBROUTINE ADJUST_BEDLOAD
    USE GLOBAL
    IMPLICIT NONE
    
    ! to be implemented
    
END SUBROUTINE ADJUST_BEDLOAD
# endif



! --- Engelund & Hansen (1967)
# if defined (SEDIMENT)
SUBROUTINE ENGELUND_HANSEN(VelMag,U,Cd,D50,Qsed)
    USE GLOBAL
    IMPLICIT NONE
    REAL(SP),INTENT(IN):: VelMag, U, Cd, D50
    REAL(SP),INTENT(OUT):: Qsed

    Qsed = 0.05*AlphaSand*VelMag**4.0_SP*U* &
           Cdu**1.5_SP/grav**2/DELTA_RHO**2/D50

END SUBROUTINE ENGELUND_HANSEN
# endif



! --- Meyer Peter & Muller (1948)
# if defined (SEDIMENT)
SUBROUTINE MEYER_PETER_MULLER(VelMag,U,Cd,D50,Qsed)
    USE GLOBAL
    IMPLICIT NONE
    REAL(SP),INTENT(IN):: VelMag, U, Cd, D50
    REAL(SP),INTENT(OUT):: Qsed
    REAL(SP):: theta
    
    theta = VelMag*VelMag*Cd/grav/Delta_rho/D50
    IF(VelMag==ZERO)THEN
      Qsed = ZERO
    ELSE
      Qsed = 8.0_SP*AlphaSand*D50*sqrt(Delta_Rho*grav*D50)* &
             (theta-theta_cr)**1.5_SP*U/VelMagU
    ENDIF

END SUBROUTINE MEYER_PETER_MULLER
# endif



! ---- Soulsby & van Rijn (1997)
# if defined (SEDIMENT)
SUBROUTINE SOULSBY_VANRIJN(VelMag,U,VelMagC,Cd,D50,D90,H,Qsed)
    USE GLOBAL
    IMPLICIT NONE
    REAL(SP),INTENT(IN)::  VelMag,U,VelMagC,Cd,D50,D90,H
    REAL(SP),INTENT(OUT):: Qsed
    REAL(SP):: Ucr,fac_xi,Ass,Asb
    ! a) No wave effects included now
    ! b) D50 SHOULD be in 0.1-2 mm
    ! Dstar = D50*(grav*Delta_Rho/Nu_water**2)**(1.0_SP/3.0_SP)
    
    if(D50.le.0.0005_SP)then
      Ucr=0.19_SP*D50**0.1_SP*LOG10(MAX(SMALL,4.0_SP*H/D90))
    else
      Ucr=8.50_SP*D50**0.6_SP*LOG10(MAX(SMALL,4.0_SP*H/D90))     
    endif
    fac_xi = (VelMag - Ucr)**2.4_SP
    ! bedload
    IF(H>ZERO)THEN
      Asb = 0.005_SP*H*(D50/H)**1.2_SP/(Delta_Rho*grav*D50)**1.2_SP
    ELSE
      Asb = ZERO
    ENDIF    
    ! suspended
    Ass = 0.012_SP*D50*Dstar**(-0.6_SP)/(Delta_rho*grav*D50)**1.2_SP
    ! total load
    Qsed = AlphaSand*(Asb+Ass)*U*fac_xi

END SUBROUTINE SOULSBY_VANRIJN
# endif



!--- van Rijn (1993)
# if defined(SEDIMENT)
SUBROUTINE VAN_RIJN93(VelMag,U,Tau,D50,D90,H,Sb)
    USE GLOBAL
    IMPLICIT NONE
    REAL(SP),INTENT(IN)::  VelMag,U,Tau,H,D50,D90
    REAL(SP),INTENT(OUT):: Sb
    REAL(SP):: Ta,Ucr,Me
    
    ! Tau_cr = (RHO_SED-RHO_W)*grav*D50*ThetaShields
    ! ! non-dimensional bed stress
    ! Ta = (Tau - Tau_cr)/Tau_cr
    ! ! reference height and concentration
    ! hgt_ref = MIN(MAX(0.01_SP*H, Hrgh),0.2_SP*H)   
    ! con_ref = 0.015_SP*RHO_SED*D50*Ta**1.5_SP/Dstar**0.3_SP/hgt_ref

    ! critical velocity, 50-500 um, 500um-2mm
    H=MAX(H,MinDepth)
    IF(D50.LE.0.0005_SP)THEN
      Ucr = 0.19_SP*D50**0.1_SP*LOG10(12.0_SP*H/3.0_SP/D90)
    ELSE
      Ucr = 8.5_SP*D50**0.6_SP*LOG10(12.0_SP*H/3.0_SP/D90)
    ENDIF
    ! bedload flux without wave, Rijn2007
    Me = (VelMag-Ucr)/sqrt(Delta_RHO*grav*D50)
    Sb = 0.015_SP*RHO_SED*U*H*(D50/H)**1.2_SP*Me**1.5_SP    
    
    
END SUBROUTINE VAN_RIJN93
# endif



# if defined(SEDIMENT)
SUBROUTINE SAND_TRANSPORT_SUBGRID
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP):: VelMag,CdsMag,Qsed,theta_tmp,&
          dbed_x,dbed_y,dq_x,dq_y,bed_celerity_x,bed_celerity_y

     ! sediment flux(bedload or sus+bed load), at pixel center
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
       DO JJ=1,SubMainGridRatio
       DO II=1,SubMainGridRatio
         VelMag=sqrt(Us(I,J,II,JJ)**2+Vs(I,J,II,JJ)**2)
         CdsMag=sqrt(Cdus(I,J,II,JJ)**2 + Cdvs(I,J,II,JJ)**2)
         ! Engelund-Hansen 1967
         ! q = 0.05*alpha*|u|^5/sqrt(g)/C^3/DeltaRho^2/D50
         !   = 0.05*alpha*|u|^5*Cds^1.5/(g^2*DeltaRho^2*D50)
         IF(SandFormula=='EH')THEN
           Qsed = 0.05*AlphaSand*VelMag**5.0_SP*CdsMag**1.5_SP/grav**2/DELTA_RHO**2/D50
         ! Meyer-Peter-Muller 1948
         ELSEIF(SandFormula=='MP')THEN
           theta_tmp=VelMag**2*CdsMag/grav/Delta_rho/D50
           Qsed = 8.0*AlphaSand*D50*sqrt(Delta_Rho*grav*D50)*(theta_tmp-theta_cr)**1.5_SP
           Qsed = MAX(Qsed,ZERO)
         ENDIF

         IF(VelMag.ne.ZERO)THEN
           QsedX(I,J,II,JJ) = Qsed*Us(I,J,II,JJ)/VelMag
           QsedY(I,J,II,JJ) = Qsed*Vs(I,J,II,JJ)/VelMag
         ELSE
           QsedX(I,J,II,JJ) = ZERO
           QsedY(I,J,II,JJ) = ZERO
         ENDIF

         ! add bed slope effect
         ! to be added
       ENDDO
       ENDDO
     ENDDO
     ENDDO

# if defined (PARALLEL)
     CALL PHI_EXCH_SUBGRID(QsedX)
     CALL PHI_EXCH_SUBGRID(QsedY)
# endif
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
        ! re-organize bed level and Qsed_{center} array
        DO JJ=2,SubMainGridRatio+1
        DO II=2,SubMainGridRatio+1
          BedLevel(II,JJ)=-DepSubGrid(I,J,II-1,JJ-1)
          QXcenter(II,JJ)=QsedX(I,J,II-1,JJ-1)
          QYcenter(II,JJ)=QsedY(I,J,II-1,JJ-1)
          UScenter(II,JJ)=Us(I,J,II-1,JJ-1)
          VScenter(II,JJ)=Vs(I,J,II-1,JJ-1)
        ENDDO
        ENDDO
        DO JJ=2,SubMainGridRatio+1
          BedLevel(1,JJ)=-DepSubGrid(I-1,J,SubMainGridRatio,JJ-1)
          BedLevel(SubMainGridRatio+2,JJ)=-DepSubGrid(I+1,J,1,JJ-1)

          QXcenter(1,JJ)=QsedX(I-1,J,SubMainGridRatio,JJ-1)
          QXcenter(SubMainGridRatio+2,JJ)=QsedX(I+1,J,1,JJ-1)

          QYcenter(1,JJ)=QsedY(I-1,J,SubMainGridRatio,JJ-1)
          QYcenter(SubMainGridRatio+2,JJ)=QsedY(I+1,J,1,JJ-1)

          UScenter(1,JJ)=Us(I-1,J,SubMainGridRatio,JJ-1)
          UScenter(SubMainGridRatio+2,JJ)=Us(I+1,J,1,JJ-1)

          VScenter(1,JJ)=Vs(I-1,J,SubMainGridRatio,JJ-1)
          VScenter(SubMainGridRatio+2,JJ)=Vs(I+1,J,1,JJ-1)
        ENDDO
        DO II=2,SubMainGridRatio+1
          BedLevel(II,1)=-DepSubGrid(I,J-1,II-1,SubMainGridRatio)
          BedLevel(II,SubMainGridRatio+2)=-DepSubGrid(I,J+1,II-1,1)

          QXcenter(II,1)=QsedX(I,J-1,II-1,SubMainGridRatio)
          QXcenter(II,SubMainGridRatio+2)=QsedX(I,J+1,II-1,1)

          QYcenter(II,1)=QsedY(I,J-1,II-1,SubMainGridRatio)
          QYcenter(II,SubMainGridRatio+2)=QsedY(I,J+1,II-1,1)

          UScenter(II,1)=Us(I,J-1,II-1,SubMainGridRatio)
          UScenter(II,SubMainGridRatio+2)=Us(I,J+1,II-1,1)

          VScenter(II,1)=Vs(I,J-1,II-1,SubMainGridRatio)
          VScenter(II,SubMainGridRatio+2)=Vs(I,J+1,II-1,1)
        ENDDO

        ! judge bed celerity direction to upwind
        ! sediment flux at pixel edges
        DO JJ=1,SubMainGridRatio+1
        DO II=1,SubMainGridRatio+1
          dbed_x=BedLevel(II+1,JJ)-BedLevel(II,JJ)
          dbed_y=BedLevel(II,JJ+1)-BedLevel(II,JJ)
          dq_x=QXcenter(II+1,JJ)-QXcenter(II,JJ)
          dq_y=QYcenter(II,JJ+1)-QYcenter(II,JJ)

          IF(dbed_x.ne.ZERO)THEN
            ! judge by bed celerity
            bed_celerity_x = dq_x/dbed_x
            bed_celerity_y = dq_y/dbed_y

            IF(bed_celerity_x.ge.zero)THEN
              QsedU(II,JJ)=QsedX(I,J,II,JJ)
            ELSE
              QsedU(II,JJ)=QsedX(I,J,II+1,JJ)
            ENDIF

            IF(bed_celerity_y.ge.zero)THEN
              QsedV(II,JJ)=QsedY(I,J,II,JJ)
            ELSE
              QsedV(II,JJ)=QsedY(I,J,II,JJ+1)
            ENDIF
          ELSE
          ! judge by flow velocity if flat bottom
            IF(UScenter(II,JJ).lt.zero)THEN
              QsedU(II,JJ)=QsedX(I,J,II,JJ)
            ELSE
              QsedU(II,JJ)=QsedX(I-1,J,SubMainGridRatio,JJ)
            ENDIF

            IF(VScenter(II,JJ).lt.zero)THEN
              QsedV(II,JJ)=QsedY(I,J,II,JJ)
            ELSE
              QsedV(II,JJ)=QsedY(I,J-1,II,SubMainGridRatio)
            ENDIF
          ENDIF
        ENDDO
        ENDDO

        ! update bed
        DO JJ=1,SubMainGridRatio
        DO II=1,SubMainGridRatio
          IF(INACTIVE_PNT)THEN
            DepSubGrid(I,J,II,JJ)=DepSubGrid(I,J,II,JJ) + DT*MorphFactor/(1.0_SP-BedPorosity)*&
                 MASK_DRY(I,J,II,JJ)*( (QsedU(II+1,JJ)-QsedU(II,JJ))/DX &
                                      +(QsedV(II,JJ+1)-QsedV(II,JJ))/DY )/rho_sed_bulk
          ELSE
            DepSubGrid(I,J,II,JJ)=DepSubGrid(I,J,II,JJ) + DT*MorphFactor/(1.0_SP-BedPorosity)*&
                 ( (QsedU(II+1,JJ)-QsedU(II,JJ))/DX &
                  +(QsedV(II,JJ+1)-QsedV(II,JJ))/DY )/rho_sed_bulk
          ENDIF
        ENDDO
        ENDDO
     ENDDO
     ENDDO

END SUBROUTINE SAND_TRANSPORT_SUBGRID
# endif




SUBROUTINE UPDATE_BND_GHOST(PHI)
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP),INTENT(INOUT) :: PHI(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio)

    ! update bed at real boundary ghost cells


# if defined (PARALLEL)
    IF(n_west.eq.MPI_PROC_NULL)THEN
# endif
      DO I=1,Nghost
      DO J=Jbeg,Jend
        DO II=1,SubMainGridRatio
        DO JJ=1,SubMainGridRatio
          PHI(I,J,II,JJ)=PHI(Ibeg,J,SubMainGridRatio+1-II,JJ)
        ENDDO
        ENDDO
      ENDDO
      ENDDO
# if defined (PARALLEL)
    ENDIF
# endif

# if defined (PARALLEL)
    IF(n_east.eq.MPI_PROC_NULL)THEN
# endif
      DO I=Iend+1,Mloc
      DO J=Jbeg,Jend
        DO II=1,SubMainGridRatio
        DO JJ=1,SubMainGridRatio
          PHI(I,J,II,JJ)=PHI(Iend,J,SubMainGridRatio+1-II,JJ)
        ENDDO
        ENDDO
      ENDDO
      ENDDO
# if defined (PARALLEL)
    ENDIF
# endif

# if defined (PARALLEL)
    IF(n_suth.eq.MPI_PROC_NULL)THEN
# endif
      DO I=Ibeg,Iend
      DO J=1,Nghost
        DO II=1,SubMainGridRatio
        DO JJ=1,SubMainGridRatio
          PHI(I,J,II,JJ)=PHI(I,Jbeg,II,SubMainGridRatio+1-JJ)
        ENDDO
        ENDDO
      ENDDO
      ENDDO
# if defined (PARALLEL)
    ENDIF
# endif

# if defined (PARALLEL)
    IF(n_nrth.eq.MPI_PROC_NULL)THEN
# endif
      DO I=Ibeg,Iend
      DO J=Jend+1,Nloc
        DO II=1,SubMainGridRatio
        DO JJ=1,SubMainGridRatio
          PHI(I,J,II,JJ)=PHI(I,Jend,II,SubMainGridRatio+1-JJ)
        ENDDO
        ENDDO
      ENDDO
      ENDDO
# if defined (PARALLEL)
    ENDIF
# endif

END SUBROUTINE UPDATE_BND_GHOST



