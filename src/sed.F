# if defined (SEDIMENT)

SUBROUTINE INITIALIZE_SEDIMENT
    USE GLOBAL
    IMPLICIT NONE

    ALLOCATE(CON(Mloc,Nloc),CON0(Mloc,Nloc))
    IF(UPDATE_BED)THEN
      ALLOCATE(OldDepth(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio))
    ENDIF
    IF(INITIAL_SED)THEN
      CALL GetFile (INITIAL_SED_FILE,CON)
    ELSE
      CON=ZERO
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
END SUBROUTINE INITIALIZE_SEDIMENT
# endif




# if defined (SEDIMENT)

SUBROUTINE UPDATE_SED
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP) :: ADVX, ADVY, DIFX, DIFY, Erosion,Deposit, SedFlux,TmpH

     CON0 = CON
     ICOUNT_TAU = ICOUNT_TAU+1

     ! get tau at pixel centers
     CALL GET_TAU_PIXEL_CENTER

     ! cohesive sediment transport, Partheniades-Krone formula
     IF(COHESIVE)THEN
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend
         ! advection terms, upwind
         ADVX = (MAX(P(I+1,J),ZERO)*CON0(I,J) + MIN(P(I+1,J),ZERO)*CON0(I+1,J) &
                -MAX(P(I,J),ZERO)*CON0(I-1,J) - MIN(P(I,J),ZERO)*CON0(I,J) )/DX
         ADVY = (MAX(Q(I,J+1),ZERO)*CON0(I,J) + MIN(Q(I,J+1),ZERO)*CON0(I,J+1) &
                -MAX(Q(I,J),ZERO)*CON0(I,J-1) - MIN(Q(I,J),ZERO)*CON0(I,J) )/DY

         ! diffusion terms
         ! DIFX = NU_SED*H(I,J)*(CON0(I+1,J)-2.0_SP*CON0(I,J)+CON0(I-1,J))/DX2
         ! DIFY = NU_SED*H(I,J)*(CON0(I,J+1)-2.0_SP*CON0(I,J)+CON0(I,J-1))/DY2
         DIFX = NU_SED*( H_u(I+1,J)*(CON0(I+1,J)-CON0(I,J)) &
                         - H_u(I,J)*(CON0(I,J)-CON0(I-1,J)) )/DX2
         DIFY = NU_SED*( H_v(I,J+1)*(CON0(I,J+1)-CON0(I,J)) &
                         - H_v(I,J)*(CON0(I,J)-CON0(I,J-1)) )/DY2

         ! erosion and deposition
         Erosion = ZERO
         Deposit = ZERO

         DO JJ=1,SubMainGridRatio
         DO II=1,SubMainGridRatio
           ! tmpH = MAX(Eta(I,J)+DepSubGrid(I,J,II,JJ),ZERO)
           IF(Tau(I,J,II,JJ).GT.ZERO) THEN
             Erosion = Erosion + ErosionRate*(1.0-BedPorosity)* &
                       MAX((Tau(I,J,II,JJ)/Tau_CE-1.0),ZERO)/NumPixel
             Deposit = Deposit + Ws*0.001_SP*MAX((1.0-Tau(I,J,II,JJ)/Tau_CD),ZERO)/NumPixel
           ENDIF
         ENDDO
         ENDDO

         ! update sediment concetration
         CON(I,J) = 1.0_SP/( MAX(H(I,J),MinDepth)/DT +       &
                             POROSITY(I,J)*ETA_OVER_DT(I,J)+ &
                             Deposit)* &
                    ( MAX(H(I,J),MinDepth)*CON0(I,J)/DT - ADVX - ADVY + DIFX +DIFY + Erosion)

         ! update bed level, cohesive bed
         IF(UPDATE_BED.AND.TIME.GT.TIME_MORPH_SPINUP)THEN
           DO JJ=1,SubMainGridRatio
           DO II=1,SubMainGridRatio
             ! sediment flux, need /NumPixel??
             SedFlux = ZERO
             IF(Tau(I,J,II,JJ).GT.ZERO)THEN
               SedFlux = ErosionRate*MAX((Tau(I,J,II,JJ)/Tau_CE-1.0),ZERO) &
                       - CON(I,J)*Ws*0.001_SP*MAX((1.0-Tau(I,J,II,JJ)/Tau_CD),ZERO)
             ENDIF

             IF(INACTIVE_PNT)THEN
                 DepSubGrid(I,J,II,JJ)=DepSubGrid(I,J,II,JJ) + &
                     MASK_DRY(I,J,II,JJ)*DT*MorphFactor/(1.0_SP-BedPorosity)*SedFlux/rho_sed_bulk
                    ! 1.0_SP/(1.0-BedPorosity)*(CON(I,J)*MAX(H(I,J),ZERO)-CON0(I,J)*MAX(H0(I,J),ZERO))/rho_sed_bulk
             ELSE
                 DepSubGrid(I,J,II,JJ)=DepSubGrid(I,J,II,JJ) +  &
                     DT*MorphFactor/(1.0_SP-BedPorosity)*SedFlux/rho_sed_bulk
                     ! + 1.0_SP/(1.0-BedPorosity)*(CON(I,J)*MAX(H(I,J),ZERO)-CON0(I,J)*MAX(H0(I,J),ZERO))/rho_sed_bulk
             ENDIF
           ENDDO
           ENDDO
         ENDIF

       ENDDO
       ENDDO

     ENDIF

     IF(NONCOHESIVE.AND.UPDATE_BED.AND.TIME.GT.TIME_MORPH_SPINUP)THEN
       IF(SubMainGridRatio.GE.4)THEN
         CALL SAND_TRANSPORT_SUBGRID
       ELSE
         CALL SAND_TRANSPORT_NO_SUBGRID
       ENDIF
     ENDIF

# if defined (PARALLEL)
    CALL PHI_EXCH_1(CON)
    IF(UPDATE_BED)THEN
      CALL PHI_EXCH_SUBGRID(DepSubGrid)
      CALL UPDATE_BND_GHOST(DepSubGrid)
    ENDIF
# endif

# if defined(DEBUG)
# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'Subroutine update_sed'
    ENDIF
# else
     WRITE(3,*)'Subroutine update_sed'
# endif
# endif

END SUBROUTINE UPDATE_SED
# endif


# if defined(SEDIMENT)
SUBROUTINE GET_TAU_PIXEL_CENTER
     USE GLOBAL
     IMPLICIT NONE

     ! get velocity at pixel centers
     IF(SubMainGridRatio.GE.4)THEN
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend
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
     ELSE
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend
         Us(I,J,1,1)=0.5_SP*(U(I,J)+U(I+1,J))
         Vs(I,J,1,1)=0.5_SP*(V(I,J)+V(I,J+1))
         Cdus(I,J,1,1)=0.5_SP*(Cdu(I,J)+Cdu(I+1,J))
         Cdvs(I,J,1,1)=0.5_SP*(Cdv(I,J)+Cdv(I,J+1))
       ENDDO
       ENDDO
     ENDIF

     ! calculate shear stress
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

END SUBROUTINE GET_TAU_PIXEL_CENTER
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

# if defined(SEDIMENT)
SUBROUTINE SAND_TRANSPORT_NO_SUBGRID
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP) :: theta_tmp

     IF(.NOT.ALLOCATED(Q_LR))ALLOCATE(Q_LR(Mloc1,Nloc))
     IF(.NOT.ALLOCATED(Q_UD))ALLOCATE(Q_UD(Mloc,Nloc1))

     ! calculate velocity magnitude at cell edges
     DO I=Ibeg,Iend1
     DO J=Jbeg,Jend
       IF(MASKv(I,J)*MASKv(I,J+1)*MASKv(I-1,J)*MASKv(I-1,J+1)==1)THEN
         VelMagU(I,J)=sqrt( U(I,J)**2 + 0.25_SP**2*(V(I,J)+V(I,J+1)+V(I-1,J)+V(I-1,J+1))**2 )
       ELSE
         VelMagU(I,J)=abs(U(I,J))
       ENDIF
     ENDDO
     ENDDO
     DO I=Ibeg,Iend
     DO J=Jbeg,Jend1
       IF(MASKu(I,J)*MASKu(I+1,J)*MASKu(I,J-1)*MASKu(I+1,J-1)==1)THEN
         VelMagV(I,J)=sqrt( V(I,J)**2 + 0.25_SP**2*(U(I,J)+U(I+1,J)+U(I,J-1)+U(I+1,J-1))**2 )
       ELSE
         VelMagV(I,J)=abs(V(I,J))
       ENDIF
     ENDDO
     ENDDO

     IF(SandFormula=='EH')THEN
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend1
         Q_LR(I,J) = 0.05*AlphaSand*VelMagU(I,J)**4.0_SP*U(I,J)* &
                         Cdu(I,J)**1.5_SP/grav**2/DELTA_RHO**2/D50
       ENDDO
       ENDDO
       DO J=Jbeg,Jend1
       DO I=Ibeg,Iend
         Q_UD(I,J) = 0.05*AlphaSand*VelMagV(I,J)**4.0_SP*V(I,J)* &
                         Cdv(I,J)**1.5_SP/grav**2/DELTA_RHO**2/D50
       ENDDO
       ENDDO
     ELSEIF(SandFormula=='MP')THEN
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend1
         theta_tmp=VelMagU(I,J)**2*Cdu(I,J)/grav/Delta_rho/D50
         IF(VelMagU(I,J).GT.ZERO)THEN
           Q_LR(I,J) = 8.0*AlphaSand*D50*sqrt(Delta_Rho*grav*D50)* &
                       (theta_tmp-theta_cr)**1.5_SP*U(I,J)/VelMagU(I,J)
         ELSE
           Q_LR(I,J) = 0.0_SP
         ENDIF
       ENDDO
       ENDDO
       DO J=Jbeg,Jend1
       DO I=Ibeg,Iend
         theta_tmp=VelMagV(I,J)**2*Cdv(I,J)/grav/Delta_rho/D50
         IF(VelMagV(I,J).GT.ZERO)THEN
           Q_UD(I,J) = 8.0*AlphaSand*D50*sqrt(Delta_Rho*grav*D50)* &
                       (theta_tmp-theta_cr)**1.5_SP*V(I,J)/VelMagV(I,J)
         ELSE
           Q_UD(I,J) = 0.0_SP
         ENDIF
       ENDDO
       ENDDO
     ENDIF

     ! update bed
     IF(INACTIVE_PNT)THEN
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend
         DepSubGrid(I,J,1,1)=DepSubGrid(I,J,1,1) + DT*MorphFactor/(1.0_SP-BedPorosity)*&
                MASK_DRY(I,J,1,1)*((Q_LR(I+1,J)-Q_LR(I,J))/DX +(Q_UD(I,J+1)-Q_UD(I,J))/DY)/rho_sed_bulk
       ENDDO
       ENDDO
     ELSE
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend
         DepSubGrid(I,J,1,1)=DepSubGrid(I,J,1,1) + DT*MorphFactor/(1.0_SP-BedPorosity)*&
                ((Q_LR(I+1,J)-Q_LR(I,J))/DX +(Q_UD(I,J+1)-Q_UD(I,J))/DY)/rho_sed_bulk
       ENDDO
       ENDDO
     ENDIF

END SUBROUTINE SAND_TRANSPORT_NO_SUBGRID
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



