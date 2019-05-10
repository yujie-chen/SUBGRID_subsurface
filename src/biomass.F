# if defined (ECO_MORPHOLOGY)

SUBROUTINE INITIALIZE_POPULATION
    USE GLOBAL
    IMPLICIT NONE

    ! ecology time step: day to year
    DT_ECO_YEAR = DT_ECO/YEAR2DAY

    ! initialize biomass
    IF(CALCULATE_BIOMASS)THEN
      ALLOCATE(BIOMASS0(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio), &
               BIOMASS(MMloc,NNloc))
      IF(INITIAL_BIOMASS)THEN
        CALL GetFile_Subgrid(INITIAL_BIOMASS_FILE,BIOMASS0)
      ELSE
        BIOMASS0 = ZERO
      ENDIF

      CALL FLOW2ECO(BIOMASS0,BIOMASS)
    ENDIF

    ! initialize stem density
    IF(CALCULATE_POPULATION)THEN
      IF(.NOT.ALLOCATED(VEG_H_SUB))&
          ALLOCATE(VEG_H_SUB(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio))
      IF(.NOT.ALLOCATED(VEG_N_SUB))&
          ALLOCATE(VEG_N_SUB(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio))
      IF(.NOT.ALLOCATED(VEG_D_SUB))&
          ALLOCATE(VEG_D_SUB(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio))

      IF(INITIAL_POPULATION)THEN
        CALL GetFile_SubGrid(INITIAL_STEMHEIGHT_FILE,VEG_H_SUB)
        CALL GetFile_SubGrid(INITIAL_STEMDENSITY_FILE,VEG_N_SUB)
        CALL GetFile_SubGrid(INITIAL_STEMDIAMETER_FILE,VEG_D_SUB)
      ELSE
        VEG_N_SUB=ZERO
        VEG_D_SUB=ZERO
        VEG_H_SUB=ZERO
      ENDIF

      ALLOCATE(STEMDENSITY(MMloc,NNloc), &
               STEMHEIGHT(MMloc,NNloc),  &
               STEMDIAMETER(MMloc,NNloc) )
      CALL FLOW2ECO(VEG_N_SUB,STEMDENSITY)
      CALL FLOW2ECO(VEG_H_SUB,STEMHEIGHT)
      CALL FLOW2ECO(VEG_D_SUB,STEMDIAMETER)

    ENDIF

# if defined (DEBUG)
# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'initialization dynamic population completed'
    ENDIF
# else
     WRITE(3,*)'initialization dynamic population completed'
# endif
# endif

END SUBROUTINE INITIALIZE_POPULATION
# endif



SUBROUTINE FLOW2ECO(PHI1,PHI2)
    USE GLOBAL
    IMPLICIT NONE
    REAL(SP),DIMENSION(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),INTENT(IN):: PHI1
    REAL(SP),DIMENSION(MMloc,NNloc),INTENT(OUT)::PHI2
    INTEGER :: KI,KJ,KII,KJJ

# if defined (PARALLEL)
    CALL PHI_EXCH_SUBGRID(PHI1)
# endif

    ! convert 4- to 2-dimensions
    DO J = 1,NNloc
    DO I = 1,MMloc
      KI = INT((I-1)/SubMainGridRatio)+1
      KJ = INT((J-1)/SubMainGridRatio)+1
      KII = I-(KI-1)*SubMainGridRatio
      KJJ = J-(KJ-1)*SubMainGridRatio
      PHI2(I,J)=PHI1(KI,KJ,KII,KJJ)
    ENDDO
    ENDDO

! check
!open(2,file='tmp1.txt')
!     do j=1,(NGlob+Nghost*2)*SubMainGridRatio
!      write(2,192) (PHI2(i,j),i=1,(MGlob+2*Nghost)*SubMainGridRatio)
!     enddo
!close(2)
!192   format(3000f12.6)

END SUBROUTINE FLOW2ECO



SUBROUTINE ECO2FLOW(PHI1,PHI2)
    USE GLOBAL
    IMPLICIT NONE
    REAL(SP),DIMENSION(MMloc,NNloc),INTENT(IN)::PHI1
    REAL(SP),DIMENSION(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),INTENT(OUT):: PHI2
    INTEGER :: KI,KJ

    ! convert 2-dimension to 4-
    DO J=1,Nloc
    DO I=1,Mloc
      DO JJ=1,SubMainGridRatio
      DO II=1,SubMainGridRatio
        KI = (I-1)*SubMainGridRatio+II
        KJ = (J-1)*SubMainGridRatio+JJ
        PHI2(I,J,II,JJ)=PHI1(KI,KJ)
      ENDDO
      ENDDO
    ENDDO
    ENDDO

# if defined (PARALLEL)
    CALL PHI_EXCH_SUBGRID(PHI2)
# endif

END SUBROUTINE ECO2FLOW



# if defined (ECO_MORPHOLOGY)
SUBROUTINE UPDATE_POPULATION
    USE GLOBAL
    IMPLICIT NONE
    REAL(SP) :: RANDOM_ESTABLISH, &
                LateralGrowth, &
                VerticalGrowth, &
                LateralDiffusion, &
                MortalityBedStress, &
                MortalityInundation, &
                Prandom
    !!! All units are in years

    ! LateralGrowthRate = f(temperature)

# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'debug0'
    ENDIF
# else
     WRITE(3,*)'debug0'
# endif

    CALL FLOW2ECO(VEG_N_SUB,STEMDENSITY)
    CALL FLOW2ECO(VEG_H_SUB,STEMHEIGHT)
    CALL FLOW2ECO(VEG_D_SUB,STEMDIAMETER)

# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'debug1'
    ENDIF
# else
     WRITE(3,*)'debug1'
# endif

    IF(CALCULATE_MORTALITY_INUN)CALL FLOW2ECO(MeanHighTide4,MeanHighTide2)
    IF(CALCULATE_MORTALITY_FLOW)THEN
      MeanValueTau4 = MeanValueTau4/ICOUNT_TAU
      CALL FLOW2ECO(MeanValueTau4,MeanValueTau2)
    ENDIF

# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'debug2'
    ENDIF
# else
     WRITE(3,*)'debug2'
# endif

    DO J = JJbeg, JJend
    DO I = IIbeg, IIend

      RANDOM_ESTABLISH = ZERO
      LateralGrowth = ZERO
      VerticalGrowth = ZERO
      LateralDiffusion = ZERO
      MortalityBedStress = ZERO
      MortalityInundation = ZERO

      ! new vegetation establish on bare flats
      CALL RANDOM_SEED()
      CALL RANDOM_NUMBER(Prandom)
      IF(CALCULATE_ESTABLISH)THEN
        IF(STEMDENSITY(I,J).LT.STEMDENSITY_BARE)THEN
          IF(Prandom.LT.1-(1-P_ESTABLISH)**(MorphFactor*DT_ECO_YEAR))THEN
            RANDOM_ESTABLISH = N_ESTABLISH
          ENDIF
        ENDIF
      ENDIF

      IF (RANDOM_ESTABLISH > ZERO)THEN    !! new established veg
        STEMDENSITY(I,J) = STEMDENSITY(I,J) + N_ESTABLISH*DT_ECO_YEAR*MorphFactor
        STEMHEIGHT(I,J)  = H_ESTABLISH
        STEMDIAMETER(I,J)= D_ESTABLISH
      ELSE                                !! dynamic growth/mortality
        !! Stem density
        LateralGrowth = LateralGrowthRate*STEMDENSITY(I,J)* &
                       (1.0_SP-STEMDENSITY(I,J)/MaxStemDensity)

        IF(CALCULATE_DIFFUSION) &
          LateralDiffusion = COEF_DIFF_VEG* &
             ( (STEMDENSITY(I+1,J)-2.0*STEMDENSITY(I,J)+STEMDENSITY(I-1,J))/DX*SubMainGridRatio + &
               (STEMDENSITY(I,J+1)-2.0*STEMDENSITY(I,J)+STEMDENSITY(I,J-1))/DY*SubMainGridRatio )

        IF(CALCULATE_MORTALITY_FLOW) &
          MortalityBedStress = COEF_MORTALITY_FLOW*MAX(ZERO, (MeanValueTau2(I,J)-TAU_CR_VEG))

        IF(CALCULATE_MORTALITY_INUN) &
          MortalityInundation = COEF_MORTALITY_INUN*MAX(ZERO, (MeanHighTide2(I,J)-HGT_CR_VEG))

        STEMDENSITY(I,J) = STEMDENSITY(I,J) + DT_ECO_YEAR * MorphFactor * &
                  (LateralGrowth+LateralDiffusion-MortalityBedStress-MortalityInundation)
        STEMDENSITY(I,J) = MAX(STEMDENSITY(I,J),ZERO)
        STEMDENSITY(I,J) = MIN(STEMDENSITY(I,J),MaxStemDensity)

        !! Stem height. But why always grow never decay?
        VerticalGrowth  = VerticalGrowthRate*STEMHEIGHT(I,J)*(1.0_SP-STEMHEIGHT(I,J)/MaxStemHeight)
        STEMHEIGHT(I,J) = STEMHEIGHT(I,J) + DT_ECO_YEAR*MorphFactor*VerticalGrowth
        STEMHEIGHT(I,J) = MAX(STEMHEIGHT(I,J),ZERO)
        STEMHEIGHT(I,J) = MIN(STEMHEIGHT(I,J),MaxStemHeight)

        !! Stem diameter. Always grow?
        STEMDIAMETER(I,J) = STEMDIAMETER(I,J) + DT_ECO_YEAR*MorphFactor*DiameterGrowthRate
        STEMDIAMETER(I,J) = MAX(STEMDIAMETER(I,J), MinStemDiameter)
        STEMDIAMETER(I,J) = MIN(STEMDIAMETER(I,J), MaxStemDiameter)
      ENDIF

    ENDDO
    ENDDO

# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'debug3'
    ENDIF
# else
     WRITE(3,*)'debug3'
# endif

    CALL PHI_EXCH_SUBGRID(STEMDENSITY)
    CALL PHI_EXCH_SUBGRID(STEMHEIGHT)
    CALL PHI_EXCH_SUBGRID(STEMDIAMETER)

# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'debug4'
    ENDIF
# else
     WRITE(3,*)'debug4'
# endif

    ! feedback to flow model
    CALL ECO2FLOW(STEMDIAMETER,VEG_D_SUB)
    CALL ECO2FLOW(STEMDENSITY,VEG_N_SUB)
    CALL ECO2FLOW(STEMHEIGHT,VEG_H_SUB)

# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'debug5'
    ENDIF
# else
     WRITE(3,*)'debug5'
# endif

    ! averaged over DT_ECO, so set back to small for next DT_ECO
    MeanHighTide4 = SMALL
    MeanValueTau4 = ZERO
    ICOUNT_TAU = 0

END SUBROUTINE UPDATE_POPULATION
# endif



# if defined (ECO_MORPHOLOGY)
SUBROUTINE UPDATE_BIOMASS
    USE GLOBAL
    IMPLICIT NONE

END SUBROUTINE UPDATE_BIOMASS
# endif
