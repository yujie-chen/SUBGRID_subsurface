SUBROUTINE InitTideBC
   USE GLOBAL
   IMPLICIT NONE
! number of boundary points and their locations
! number of tidal components, period, amp, pha

   OPEN(14,FILE=TRIM(TIDE_FILE))

   READ(14,*)  ! title
   READ(14,*)TideStartDate
   READ(14,*) NumConstituent

   ALLOCATE (TideFac(NumConstituent),Tideu0(NumConstituent))
   DO I = 1,NumConstituent
      READ(14,*) TideFac(I),Tideu0(I)
   ENDDO

   READ(14,*) NumTidePoint
   ALLOCATE (I_bnd(NumTidePoint),J_bnd(NumTidePoint))
   ALLOCATE (TidePeriod(NumConstituent,NumTidePoint), &
             TideAmp(NumConstituent,NumTidePoint),&
             TidePha(NumConstituent,NumTidePoint))

   DO I=1,NumTidePoint
      READ(14,*)I_bnd(I),J_bnd(I)
      DO J=1,NumConstituent
         READ(14,*)TidePeriod(J,I),TideAmp(J,I),TidePha(J,I)
      ENDDO
   ENDDO

   CLOSE(14)

! need to open masks for boundary points???

END SUBROUTINE InitTideBC



SUBROUTINE TIDE_BC
    USE GLOBAL
    IMPLICIT NONE
    INTEGER :: Kpnt,Kcon,Ip,Jp
    REAL(SP):: RealTime,EtaComponent

    IF(TIDE_CLAMPED)THEN

      RealTime = TideStartDate*24.0_SP*3600.0_SP+TIME

      DO Kpnt=1,NumTidePoint

        EtaComponent = ZERO
        DO Kcon = 1,NumConstituent
          EtaComponent = EtaComponent + TideFac(Kcon)*TideAmp(Kcon,Kpnt)* &
            SIN( 2.0_SP*pi/(TidePeriod(Kcon,Kpnt)*3600.0_SP)*RealTime &
                 - TidePha(Kcon,Kpnt)*DEG2RAD+Tideu0(Kcon)*DEG2RAD )
        ENDDO



        Eta(I_bnd(Kpnt)+Nghost,J_bnd(Kpnt)+Nghost) = EtaComponent ! *TANH(TIME/3600.0_SP/6.0_SP)



!       add the tanh() term to spin-up gradually, tanh(2)~1.0

      ENDDO

    ENDIF


END SUBROUTINE TIDE_BC

