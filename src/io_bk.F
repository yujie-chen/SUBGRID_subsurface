SUBROUTINE READ_INPUT
    USE GLOBAL
    USE Input_Util
    IMPLICIT NONE
    CHARACTER(LEN=80) FILE_NAME
    INTEGER::LINE
    INTEGER :: ierr
    INTEGER :: I_comp

# if defined (PARALLEL)
    call MPI_COMM_RANK (MPI_COMM_WORLD, myid, ier)
# endif

      OPEN(3,FILE='LOG.txt')

! read everything from input.txt
      FILE_NAME='input.txt'

! title
      CALL GET_STRING_VAL(TITLE,FILE_NAME,'TITLE',line,ierr)
      IF(ierr==1)THEN
        !write(*,*) 'No TITLE in ', FILE_NAME, 'use default'
        TITLE='---TEST RUN---'
      ENDIF
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,*)'---- LOG FILE ---'
      if (myid.eq.0) WRITE(3,*)TITLE
      if (myid.eq.0) WRITE(3,*)' --------------input start --------------'
# else
      WRITE(3,*)'---- LOG FILE ---'
      WRITE(3,*)TITLE
      WRITE(3,*)' --------------input start --------------'
# endif

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,*)'                                         '
      if (myid.eq.0) WRITE(3,*)'-------------- PARALLEL -----------------'    
# endif

# if defined (PARALLEL)
! parallel info
      CALL GET_INTEGER_VAL(PX,FILE_NAME,'PX',line,ierr)
      IF(ierr == 1) THEN
        PX = 1
        if (myid.eq.0)write(*,*) 'No PX sepecified ', 'use PX=1'
        if (myid.eq.0)WRITE(3,'(A20,A20)')'No PX sepecified ', 'use PX=1'
      ENDIF
      CALL GET_INTEGER_VAL(PY,FILE_NAME,'PY',line,ierr)
      IF(ierr == 1) THEN
        PY = 1
        if (myid.eq.0)write(*,*) 'No PY sepecified ', 'use PY=1'
        if (myid.eq.0)WRITE(3,'(A20,A20)')'No PY sepecified ', 'use PY=1'
      ENDIF

      if (myid.eq.0) WRITE(3,'(A7,I3,A7,I3)') 'PX   =',PX,'PY   =', PY
# else
      PX = 1
      PY = 1
# endif

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,*)'                                         '
      if (myid.eq.0) WRITE(3,*)'-------------- GRID INFO -----------------'
# else
      WRITE(3,*)'                                         '
      WRITE(3,*)'-------------- GRID INFO -----------------'   
# endif

! dimension
      CALL GET_INTEGER_VAL(Mglob,FILE_NAME,'Mglob',line,ierr)

      IF(ierr==1)THEN
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'Mglob:', 'NOT DEFINED, STOP'
         WRITE(3,'(A40,A40)')'Mglob:', 'NOT DEFINED, STOP'
      endif
       call MPI_FINALIZE ( ier )
# else
         WRITE(*,'(A40,A40)')'Mglob:', 'NOT DEFINED, STOP'
         WRITE(3,'(A40,A40)')'Mglob:', 'NOT DEFINED, STOP'
# endif
        STOP
      ENDIF

      CALL GET_INTEGER_VAL(Nglob,FILE_NAME,'Nglob',line,ierr)

      IF(ierr==1)THEN
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'Nglob:', 'NOT DEFINED, STOP'
         WRITE(3,'(A40,A40)')'Nglob:', 'NOT DEFINED, STOP'
      endif
       call MPI_FINALIZE ( ier )
# else
         WRITE(*,'(A40,A40)')'Nglob:', 'NOT DEFINED, STOP'
         WRITE(3,'(A40,A40)')'Nglob:', 'NOT DEFINED, STOP'
# endif
        STOP
      ENDIF

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A7,I3,A7,I3)') 'Mglob=',Mglob,'Nglob=', Nglob
# else
      WRITE(3,'(A7,I3,A7,I3)') 'Mglob=',Mglob,'Nglob=', Nglob
# endif

! grid
      CALL GET_Float_VAL(DX,FILE_NAME,'DX',line,ierr)

      IF(ierr==1)THEN
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'DX:', 'NOT DEFINED, STOP'
         WRITE(3,'(A40,A40)')'DX:', 'NOT DEFINED, STOP'
      endif
       call MPI_FINALIZE ( ier )
# else
         WRITE(*,'(A40,A40)')'DX:', 'NOT DEFINED, STOP'
         WRITE(3,'(A40,A40)')'DX:', 'NOT DEFINED, STOP'
# endif
        STOP
      ENDIF

      CALL GET_Float_VAL(DY,FILE_NAME,'DY',line,ierr)

      IF(ierr==1)THEN
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'DY:', 'NOT DEFINED, STOP'
         WRITE(3,'(A40,A40)')'DY:', 'NOT DEFINED, STOP'
      endif
       call MPI_FINALIZE ( ier )
# else
         WRITE(*,'(A40,A40)')'DY:', 'NOT DEFINED, STOP'
         WRITE(3,'(A40,A40)')'DY:', 'NOT DEFINED, STOP'
# endif
        STOP
      ENDIF

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A4,F12.2,A4,F12.2)')'DX=',DX,'DY=',DY
# else
      WRITE(3,'(A4,F12.2,A4,F12.2)')'DX=',DX,'DY=',DY
# endif

! result folder
      CALL GET_STRING_VAL(RESULT_FOLDER,FILE_NAME,'RESULT_FOLDER',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A50)')'RESULT_FOLDER:', RESULT_FOLDER
# else
      WRITE(3,'(A15,A50)')'RESULT_FOLDER:', RESULT_FOLDER
# endif

! depth
      CALL GET_STRING_VAL(DEPTH_FILE,FILE_NAME,'DEPTH_FILE',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A12,A50)')'DEPTH_FILE:', DEPTH_FILE
# else
      WRITE(3,'(A12,A50)')'DEPTH_FILE:', DEPTH_FILE
# endif

! time
      CALL GET_LOGICAL_VAL(CONSTANT_DT,FILE_NAME,'CONSTANT_DT',line,ierr)
      IF(CONSTANT_DT)THEN
        CALL GET_Float_VAL(DT_FIXED,FILE_NAME,'DT_FIXED',line,ierr)
        IF(ierr==1)THEN
# if defined (PARALLEL)
         if (myid.eq.0) THEN
            WRITE(*,'(A40)') 'DT_FIXED NOT DEFINED, STOP'
            WRITE(3,'(A40)') 'DT_FIXED NOT DEFINED, STOP'
         endif
           call MPI_FINALIZE ( ier )
# else
            WRITE(*,'(A40)') 'DT_FIXED NOT DEFINED, STOP'
            WRITE(3,'(A40)') 'DT_FIXED NOT DEFINED, STOP'
# endif
         STOP
        ENDIF

      ENDIF ! end constant dt

      CALL GET_Float_VAL(TOTAL_TIME,FILE_NAME,'TOTAL_TIME',line,ierr)

      IF(ierr==1)THEN
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'TOTAL_TIME:', 'NOT FOUND, STOP'
         WRITE(3,'(A40,A40)')'TOTAL_TIME:', 'NOT FOUND, STOP'
      endif
       call MPI_FINALIZE ( ier )
# else
         WRITE(*,'(A40,A40)')'TOTAL_TIME:', 'NOT FOUND, STOP'
         WRITE(3,'(A40,A40)')'TOTAL_TIME:', 'NOT FOUND, STOP'
# endif
        STOP
      ENDIF


      CALL GET_Float_VAL(PLOT_INTV,FILE_NAME,'PLOT_INTV',line,ierr)

      IF(ierr==1)THEN
        PLOT_INTV = 1000.0
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'PLOT_INTV Default:  1000.0 s'
         WRITE(3,'(A40)')'PLOT_INTV Default:  1000.0 s'
      endif
# else
         WRITE(*,'(A40)')'PLOT_INTV Default:  1000.0 s'
         WRITE(3,'(A40)')'PLOT_INTV Default:  1000.0 s'
# endif
       ENDIF

      CALL GET_Float_VAL(PLOT_INTV_STATION,FILE_NAME,'PLOT_INTV_STATION',line,ierr)

      IF(ierr==1)THEN
        PLOT_INTV_STATION = 1.0
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'PLOT_INTV_STATION Default:  1.0 s'
         WRITE(3,'(A40)')'PLOT_INTV_STATION Default:  1.0 s'
      endif
# else
         WRITE(*,'(A40)')'PLOT_INTV_STATION Default:  1.0 s'
         WRITE(3,'(A40)')'PLOT_INTV_STATION Default:  1.0 s'
# endif
       ENDIF


      CALL GET_Float_VAL(SCREEN_INTV,FILE_NAME,'SCREEN_INTV',line,ierr)

      IF(ierr==1)THEN
        SCREEN_INTV = 1.0
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'SCREEN_INTV Default:  1.0 s'
         WRITE(3,'(A40)')'SCREEN_INTV Default:  1.0 s'
      endif
# else
         WRITE(*,'(A40)')'SCREEN_INTV Default:  1.0 s'
         WRITE(3,'(A40)')'SCREEN_INTV Default:  1.0 s'
# endif
       ENDIF


# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'TOTAL_TIME=', TOTAL_TIME
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'PLOT_INTV= ', PLOT_INTV
      if (myid.eq.0) WRITE(3,'(A13,F12.2)')'SCREEN_INTV=', SCREEN_INTV
# else
      WRITE(3,'(A12,F12.2)')'TOTAL_TIME=', TOTAL_TIME
      WRITE(3,'(A12,F12.2)')'PLOT_INTV= ', PLOT_INTV
      WRITE(3,'(A13,F12.2)')'SCREEN_INTV=', SCREEN_INTV
# endif


! boundary conditions
# if defined (COUPLING)
      CALL GET_STRING_VAL(COUPLING_FILE,FILE_NAME,'COUPLING_FILE',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A50)')'COUPLING_FILE:', COUPLING_FILE
# else
      WRITE(3,'(A15,A50)')'COUPLING_FILE:', COUPLING_FILE
# endif
# endif

! tide and river boundary
      CALL GET_LOGICAL_VAL(RIVER_CLAMPED,FILE_NAME,'RIVER_CLAMPED',line,ierr)

      IF(ierr==1)THEN
        RIVER_CLAMPED = .FALSE.
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'no river clamped condition'
         WRITE(3,'(A40)')'no river clamped condition'
      endif
# else
         WRITE(*,'(A40)')'no river clamped condition'
         WRITE(3,'(A40)')'no river clamped condition'
# endif
       ENDIF


      CALL GET_LOGICAL_VAL(TIDE_CLAMPED, FILE_NAME,'TIDE_CLAMPED',line,ierr)

      IF(ierr==1)THEN
        RIVER_CLAMPED = .FALSE.
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'no tide clamped condition'
         WRITE(3,'(A40)')'no tide clamped condition'
      endif
# else
         WRITE(*,'(A40)')'no tide clamped condition'
         WRITE(3,'(A40)')'no tide clamped condition'
# endif
       ENDIF

     IF(RIVER_CLAMPED) THEN
      CALL GET_STRING_VAL(RIVER_FILE,FILE_NAME,'RIVER_FILE',line,ierr)
      IF(ierr==1)THEN
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'RIVER_FILE:', 'NOT FOUND, STOP'
         WRITE(3,'(A40,A40)')'RIVER_FILE:', 'NOT FOUND, STOP'
      endif
       call MPI_FINALIZE ( ier )
# else
         WRITE(*,'(A40,A40)')'RIVER_FILE:', 'NOT FOUND, STOP'
         WRITE(3,'(A40,A40)')'RIVER_FILE:', 'NOT FOUND, STOP'
# endif
        STOP
      ENDIF

     ENDIF ! end river clamped 

     IF(TIDE_CLAMPED) THEN
      CALL GET_STRING_VAL(TIDE_FILE, FILE_NAME,'TIDE_FILE',line,ierr)

      IF(ierr==1)THEN
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'TIDE_FILE:', 'NOT FOUND, STOP'
         WRITE(3,'(A40,A40)')'TIDE_FILE:', 'NOT FOUND, STOP'
      endif
       call MPI_FINALIZE ( ier )
# else
         WRITE(*,'(A40,A40)')'TIDE_FILE:', 'NOT FOUND, STOP'
         WRITE(3,'(A40,A40)')'TIDE_FILE:', 'NOT FOUND, STOP'
# endif
        STOP
      ENDIF

     ENDIF ! end tide clamped 


# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A12,A50)')'RIVER_FILE:', RIVER_FILE
      if (myid.eq.0) WRITE(3,'(A12,A50)')'TIDE_FILE:',  TIDE_FILE
# else
      WRITE(3,'(A12,A50)')'RIVER_FILE:', RIVER_FILE
      WRITE(3,'(A12,A50)')'TIDE_FILE:',  TIDE_FILE
# endif


! physics
      CALL GET_INTEGER_VAL(Nonlinear,FILE_NAME,'Nonlinear',line,ierr)

      IF(ierr==1)THEN
        Nonlinear = 1
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'Nearline:', 'True'
         WRITE(3,'(A40,A40)')'Nearline:', 'True'
      endif
# else
         WRITE(*,'(A40,A40)')'Nearline:', 'True'
         WRITE(3,'(A40,A40)')'Nearline:', 'True'
# endif
      ENDIF


      CALL GET_Float_VAL(NU,FILE_NAME,'NU',line,ierr)
      IF(ierr==1)THEN
        NU = 0.0
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'NU: Use default,', 'ZERO'
         WRITE(3,'(A40,A40)')'NU: Use default,', 'ZERO'
      endif
# else
         WRITE(*,'(A40,A40)')'NU: Use default,', 'ZERO'
         WRITE(3,'(A40,A40)')'NU: Use default,', 'ZERO'
# endif
      ENDIF

# if defined (PARALLEL)
       if (myid.eq.0) WRITE(3,'(A10,I6)')'Nonlinear = ', Nonlinear
       if (myid.eq.0) WRITE(3,'(A17,F12.6)')'Eddy Viscosity = ', NU
# else
       WRITE(3,'(A10,I6)')'Nonlinear = ', Nonlinear
       WRITE(3,'(A17,F12.6)')'Eddy Viscosity = ', NU
# endif

! friction
      CALL GET_LOGICAL_VAL(SUBGRID_CD_FIXED,FILE_NAME,'SUBGRID_CD_FIXED',line,ierr)
      IF(ierr==1)THEN
        SUBGRID_CD_FIXED = .TRUE.
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'SUBGRID_CD_FIXED: Use default,', 'TRUE'
         WRITE(3,'(A40,A40)')'SUBGRID_CD_FIXED: Use default,', 'TRUE'
      endif
# else
         WRITE(*,'(A40,A40)')'SUBGRID_CD_FIXED: Use default,', 'TRUE'
         WRITE(3,'(A40,A40)')'SUBGRID_CD_FIXED: Use default,', 'TRUE'
# endif
      ENDIF

      CALL GET_LOGICAL_VAL(CONSTRUCT_SUBGRID_UV,FILE_NAME,'CONSTRUCT_SUBGRID_UV',line,ierr)
      IF(ierr==1)THEN
        CONSTRUCT_SUBGRID_UV = .TRUE.
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'CONSTRUCT_SUBGRID_UV: Use default,', 'TRUE'
         WRITE(3,'(A40,A40)')'CONSTRUCT_SUBGRID_UV: Use default,', 'TRUE'
      endif
# else
         WRITE(*,'(A40,A40)')'CONSTRUCT_SUBGRID_UV: Use default,', 'TRUE'
         WRITE(3,'(A40,A40)')'CONSTRUCT_SUBGRID_UV: Use default,', 'TRUE'
# endif
      ENDIF

      CALL GET_LOGICAL_VAL(SUBGRID_UV_CORRECTION,FILE_NAME,'SUBGRID_UV_CORRECTION',line,ierr)
      IF(ierr==1)THEN
        SUBGRID_UV_CORRECTION = .TRUE.
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'SUBGRID_UV_CORRECTION: Use default,', 'TRUE'
         WRITE(3,'(A40,A40)')'SUBGRID_UV_CORRECTION: Use default,', 'TRUE'
      endif
# else
         WRITE(*,'(A40,A40)')'SUBGRID_UV_CORRECTION: Use default,', 'TRUE'
         WRITE(3,'(A40,A40)')'SUBGRID_UV_CORRECTION: Use default,', 'TRUE'
# endif
      ENDIF

     IF(SUBGRID_CD_FIXED)THEN
      CALL GET_Float_VAL(Cd_fixed,FILE_NAME,'Cd_fixed',line,ierr)
      IF(ierr==1)THEN
        Cd_fixed = 0.002
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'Cd_fixed: Use default,', '0.002'
         WRITE(3,'(A40,A40)')'Cd_fixed: Use default,', '0.002'
      endif
# else
         WRITE(*,'(A40,A40)')'Cd_fixed: Use default,', '0.002'
         WRITE(3,'(A40,A40)')'Cd_fixed: Use default,', '0.002'
# endif
      ENDIF

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A13,F12.6)')'Cd_fixed   =', Cd_fixed
# else
      WRITE(3,'(A13,F12.6)')'Cd_fixed   =', Cd_fixed
# endif

     ELSE
      CALL GET_Float_VAL(Manning,FILE_NAME,'Manning',line,ierr)
      IF(ierr==1)THEN
        Manning = 0.01
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'Manning: Use default,', '0.01'
         WRITE(3,'(A40,A40)')'Manning: Use default,', '0.01'
      endif
# else
         WRITE(*,'(A40,A40)')'Manning: Use default,', '0.01'
         WRITE(3,'(A40,A40)')'Manning: Use default,', '0.01'
# endif
      ENDIF

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A13,F12.6)')'Manning    =', Manning
# else
      WRITE(3,'(A13,F12.6)')'Manning    =', Manning
# endif

      CALL GET_STRING_VAL(MANNING_TYPE,FILE_NAME,'MANNING_TYPE',line,ierr)
!     MANNING_TYPE: UNIFORM/VARY/SUBGRID
      IF(ierr==1)THEN
        MANNING_TYPE = 'UNIFORM'
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'MANNING_TYPE: Use default,', 'UNIFORM'
         WRITE(3,'(A40,A40)')'MANNING_TYPE: Use default,', 'UNIFORM'
      endif
# else
         WRITE(*,'(A40,A40)')'MANNING_TYPE: Use default,', 'UNIFORM'
         WRITE(3,'(A40,A40)')'MANNING_TYPE: Use default,', 'UNIFORM'
# endif
      ENDIF

     IF(MANNING_TYPE(1:4)=='VARY')THEN
      CALL GET_STRING_VAL(MANNING_FILE,FILE_NAME,'MANNING_FILE',line,ierr)
      IF(ierr==1)THEN
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'MANNING_FILE:', 'NOT FOUND, STOP'
         WRITE(3,'(A40,A40)')'MANNING_FILE:', 'NOT FOUND, STOP'
      endif
       call MPI_FINALIZE ( ier )
# else
         WRITE(*,'(A40,A40)')'MANNING_FILE:', 'NOT FOUND, STOP'
         WRITE(3,'(A40,A40)')'MANNING_FILE:', 'NOT FOUND, STOP'
# endif
        STOP
      ENDIF
     ENDIF ! vary manning

     IF(MANNING_TYPE(1:4)=='SUBG')THEN
      CALL GET_STRING_VAL(MANNINGSUBGRID_FILE,FILE_NAME,'MANNINGSUBGRID_FILE',line,ierr)
      IF(ierr==1)THEN
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'MANNINGSUBGRID_FILE:', 'NOT FOUND, STOP'
         WRITE(3,'(A40,A40)')'MANNINGSUBGRID_FILE:', 'NOT FOUND, STOP'
      endif
       call MPI_FINALIZE ( ier )
# else
         WRITE(*,'(A40,A40)')'MANNINGSUBGRID_FILE:', 'NOT FOUND, STOP'
         WRITE(3,'(A40,A40)')'MANNINGSUBGRID_FILE:', 'NOT FOUND, STOP'
# endif
        STOP
      ENDIF
     ENDIF ! subgrid manning

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A13,F12.6)')'Cd_fixed   =', Cd_fixed
      if (myid.eq.0) WRITE(3,'(A13,F12.6)')'Manning    =', Manning
      if (myid.eq.0) WRITE(3,'(A15,A50)')'MANNING_TYPE:', MANNING_TYPE
      if (myid.eq.0) WRITE(3,'(A15,A50)')'MANNING_FILE:', MANNING_FILE
      if (myid.eq.0) WRITE(3,'(A20,A50)')'MANNINGSUBGRID_FILE:', MANNINGSUBGRID_FILE
# else
      WRITE(3,'(A13,F12.6)')'Cd_fixed   =', Cd_fixed
      WRITE(3,'(A13,F12.6)')'Manning    =', Manning
      WRITE(3,'(A15,A50)')'MANNING_TYPE:', MANNING_TYPE
      WRITE(3,'(A15,A50)')'MANNING_FILE:', MANNING_FILE
      WRITE(3,'(A20,A50)')'MANNINGSUBGRID_FILE:', MANNINGSUBGRID_FILE
# endif

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A50)')'MANNING_TYPE:', MANNING_TYPE
      if (myid.eq.0) WRITE(3,'(A15,A50)')'MANNING_FILE:', MANNING_FILE
      if (myid.eq.0) WRITE(3,'(A20,A50)')'MANNINGSUBGRID_FILE:', MANNINGSUBGRID_FILE
# else
      WRITE(3,'(A15,A50)')'MANNING_TYPE:', MANNING_TYPE
      WRITE(3,'(A15,A50)')'MANNING_FILE:', MANNING_FILE
      WRITE(3,'(A20,A50)')'MANNINGSUBGRID_FILE:', MANNINGSUBGRID_FILE
# endif

     ENDIF ! end cd fixed

! vegetation
! vegetation， sediment eco-morpho parts need reading protection
! --------------------------------------------------------------
# if defined(VEGETATION)
      CALL GET_STRING_VAL(VEG_TYPE,FILE_NAME,'VEG_TYPE',line,ierr)
      CALL GET_STRING_VAL(VEG_H_FILE,FILE_NAME,'VEG_H_FILE',line,ierr)
      CALL GET_STRING_VAL(VEG_N_FILE,FILE_NAME,'VEG_N_FILE',line,ierr)
      CALL GET_STRING_VAL(VEG_D_FILE,FILE_NAME,'VEG_D_FILE',line,ierr)
      CALL GET_STRING_VAL(VEG_H_SUBGRID_FILE,FILE_NAME,'VEG_H_SUBGRID_FILE',line,ierr)
      CALL GET_STRING_VAL(VEG_N_SUBGRID_FILE,FILE_NAME,'VEG_N_SUBGRID_FILE',line,ierr)
      CALL GET_STRING_VAL(VEG_D_SUBGRID_FILE,FILE_NAME,'VEG_D_SUBGRID_FILE',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,A50)')'VEG_TYPE:', VEG_TYPE
      if (myid.eq.0) WRITE(3,'(A12,A50)')'VEG_H_FILE:', VEG_H_FILE
      if (myid.eq.0) WRITE(3,'(A12,A50)')'VEG_N_FILE:', VEG_N_FILE
      if (myid.eq.0) WRITE(3,'(A12,A50)')'VEG_D_FILE:', VEG_D_FILE
      if (myid.eq.0) WRITE(3,'(A19,A50)')'VEG_H_SUBGRID_FILE:', VEG_H_SUBGRID_FILE
      if (myid.eq.0) WRITE(3,'(A19,A50)')'VEG_N_SUBGRID_FILE:', VEG_N_SUBGRID_FILE
      if (myid.eq.0) WRITE(3,'(A19,A50)')'VEG_D_SUBGRID_FILE:', VEG_D_SUBGRID_FILE
# else
      WRITE(3,'(A10,A50)')'VEG_TYPE:', VEG_TYPE
      WRITE(3,'(A12,A50)')'VEG_H_FILE:', VEG_H_FILE
      WRITE(3,'(A12,A50)')'VEG_N_FILE:', VEG_N_FILE
      WRITE(3,'(A12,A50)')'VEG_D_FILE:', VEG_D_FILE
      WRITE(3,'(A19,A50)')'VEG_H_SUBGRID_FILE:', VEG_H_SUBGRID_FILE
      WRITE(3,'(A19,A50)')'VEG_N_SUBGRID_FILE:', VEG_N_SUBGRID_FILE
      WRITE(3,'(A19,A50)')'VEG_D_SUBGRID_FILE:', VEG_D_SUBGRID_FILE
# endif
# endif
! sediment
# if defined (SEDIMENT)
      CALL GET_LOGICAL_VAL(UPDATE_BED,FILE_NAME,'UPDATE_BED',line,ierr)
      CALL GET_LOGICAL_VAL(COHESIVE,FILE_NAME,'COHESIVE',line,ierr)
      CALL GET_LOGICAL_VAL(NONCOHESIVE,FILE_NAME,'NONCOHESIVE',line,ierr)
      CALL GET_LOGICAL_VAL(INITIAL_SED,FILE_NAME,'INITIAL_SED',line,ierr)
      CALL GET_LOGICAL_VAL(BOUNDARY_SED,FILE_NAME,'BOUNDARY_SED',line,ierr)
      CALL GET_LOGICAL_VAL(SED_BC_CLAMPED,FILE_NAME,'SED_BC_CLAMPED',line,ierr)
      CALL GET_LOGICAL_VAL(SED_BC_CONSTANT,FILE_NAME,'SED_BC_CONSTANT',line,ierr)
      CALL GET_LOGICAL_VAL(SED_BC_NEUMANN,FILE_NAME,'SED_BC_NEUMANN',line,ierr)
      CALL GET_STRING_VAL(INITIAL_SED_FILE,FILE_NAME,'INITIAL_SED_FILE',line,ierr)
      CALL GET_STRING_VAL(BOUNDARY_SED_FILE,FILE_NAME,'BOUNDARY_SED_FILE',line,ierr)
      CALL GET_STRING_VAL(BOUNDARY_SED_FILE,FILE_NAME,'CONSTANT_SED_FILE',line,ierr)
      CALL GET_STRING_VAL(SandFormula,FILE_NAME,'SandFormula',line,ierr)
      ! mud
      CALL GET_Float_VAL(Ws,FILE_NAME,'Ws',line,ierr)
      CALL GET_Float_VAL(NU_SED,FILE_NAME,'NU_SED',line,ierr)
      CALL GET_Float_VAL(Tau_CE,FILE_NAME,'Tau_CE',line,ierr)
      CALL GET_Float_VAL(Tau_CD,FILE_NAME,'Tau_CD',line,ierr)
      CALL GET_Float_VAL(ErosionRate,FILE_NAME,'ErosionRate',line,ierr)
      ! sand
      CALL GET_Float_VAL(D50,FILE_NAME,'D50',line,ierr)
      CALL GET_Float_VAL(RHO_SED_BULK,FILE_NAME,'RHO_SED_BULK',line,ierr)

      CALL GET_Float_VAL(BedPorosity,FILE_NAME,'BedPorosity',line,ierr)
      CALL GET_Float_VAL(MorphFactor,FILE_NAME,'MorphFactor',line,ierr)
      CALL GET_Float_VAL(TIME_MORPH_SPINUP,FILE_NAME,'TIME_MORPH_SPINUP',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A3,F8.4)')'Ws=',Ws
      if (myid.eq.0) WRITE(3,'(A7,F8.4)')'NU_SED=',NU_SED
      if (myid.eq.0) WRITE(3,'(A7,F8.4)')'Tau_CE=',Tau_CE
      if (myid.eq.0) WRITE(3,'(A7,F8.4)')'Tau_CD=',Tau_CD
      if (myid.eq.0) WRITE(3,'(A12,F10.5)')'ErosionRate=',ErosionRate
      if (myid.eq.0) WRITE(3,'(A12,F8.4)')'BedPorosity=',BedPorosity
      if (myid.eq.0) WRITE(3,'(A12,F8.4)')'MorphFactor=',MorphFactor
      if (myid.eq.0) WRITE(3,'(A18,F12.4)')'TIME_MORPH_SPINUP=',TIME_MORPH_SPINUP
      if (myid.eq.0) WRITE(3,'(A19,A50)')'INITIAL_SED_FILE:', INITIAL_SED_FILE
      if (myid.eq.0) WRITE(3,'(A20,A50)')'BOUNDARY_SED_FILE:', BOUNDARY_SED_FILE
      if (myid.eq.0) WRITE(3,'(A20,A50)')'CONSTANT_SED_FILE:', CONSTANT_SED_FILE
      if (myid.eq.0) WRITE(3,'(A5,F8.3)')'D50=',D50
      if (myid.eq.0) WRITE(3,'(A14,F8.2)')'RHO_SED_BULK=',RHO_SED_BULK
      if (myid.eq.0) WRITE(3,'(A12,A50)')'SandFormula:', SandFormula
# else
      WRITE(3,'(A3,F8.4)')'Ws=',Ws
      WRITE(3,'(A7,F8.4)')'NU_SED=',NU_SED
      WRITE(3,'(A7,F8.4)')'Tau_CE=',Tau_CE
      WRITE(3,'(A7,F8.4)')'Tau_CD=',Tau_CD
      WRITE(3,'(A12,F10.5)')'ErosionRate=',ErosionRate
      WRITE(3,'(A12,F8.4)')'BedPorosity=',BedPorosity
      WRITE(3,'(A12,F8.4)')'MorphFactor=',MorphFactor
      WRITE(3,'(A18,F12.4)')'TIME_MORPH_SPINUP=',TIME_MORPH_SPINUP
      WRITE(3,'(A19,A50)')'INITIAL_SED_FILE:', INITIAL_SED_FILE
      WRITE(3,'(A20,A50)')'BOUNDARY_SED_FILE:', BOUNDARY_SED_FILE
      WRITE(3,'(A20,A50)')'CONSTANT_SED_FILE:', CONSTANT_SED_FILE
      WRITE(3,'(A5,F8.3)')'D50=',D50
      WRITE(3,'(A14,F8.2)')'RHO_SED_BULK=',RHO_SED_BULK
      WRITE(3,'(A12,A50)')'SandFormula:', SandFormula
# endif
# endif

# if defined(ECO_MORPHOLOGY)
     CALL GET_LOGICAL_VAL(CALCULATE_BIOMASS,FILE_NAME,'CALCULATE_BIOMASS',line,ierr)
     CALL GET_LOGICAL_VAL(INITIAL_BIOMASS,FILE_NAME,'INITIAL_BIOMASS',line,ierr)
     CALL GET_LOGICAL_VAL(CALCULATE_POPULATION,FILE_NAME,'CALCULATE_POPULATION',line,ierr)
     CALL GET_LOGICAL_VAL(INITIAL_POPULATION,FILE_NAME,'INITIAL_POPULATION',line,ierr)
     CALL GET_LOGICAL_VAL(CALCULATE_ESTABLISH,FILE_NAME,'CALCULATE_ESTABLISH',line,ierr)
     CALL GET_LOGICAL_VAL(CALCULATE_DIFFUSION,FILE_NAME,'CALCULATE_DIFFUSION',line,ierr)
     CALL GET_LOGICAL_VAL(CALCULATE_MORTALITY_FLOW,FILE_NAME,'CALCULATE_MORTALITY_FLOW',line,ierr)
     CALL GET_LOGICAL_VAL(CALCULATE_MORTALITY_INUN,FILE_NAME,'CALCULATE_MORTALITY_INUN',line,ierr)
     CALL GET_LOGICAL_VAL(CALCULATE_SPECY_INTERACT,FILE_NAME,'CALCULATE_SPECY_INTERACT',line,ierr)

     CALL GET_STRING_VAL(INITIAL_BIOMASS_FILE,FILE_NAME,'INITIAL_BIOMASS_FILE',line,ierr)
     CALL GET_STRING_VAL(INITIAL_STEMHEIGHT_FILE,FILE_NAME,'INITIAL_STEMHEIGHT_FILE',line,ierr)
     CALL GET_STRING_VAL(INITIAL_STEMDENSITY_FILE,FILE_NAME,'INITIAL_STEMDENSITY_FILE',line,ierr)
     CALL GET_STRING_VAL(INITIAL_STEMDIAMETER_FILE,FILE_NAME,'INITIAL_STEMDIAMETER_FILE',line,ierr)

     CALL GET_Float_VAL(DT_ECO,FILE_NAME,'DT_ECO',line,ierr)
     CALL GET_Float_VAL(STEMDENSITY_BARE,FILE_NAME,'STEMDENSITY_BARE',line,ierr)
     CALL GET_Float_VAL(P_ESTABLISH,FILE_NAME,'P_ESTABLISH',line,ierr)
     CALL GET_Float_VAL(H_ESTABLISH,FILE_NAME,'H_ESTABLISH',line,ierr)
     CALL GET_Float_VAL(N_ESTABLISH,FILE_NAME,'N_ESTABLISH',line,ierr)
     CALL GET_Float_VAL(D_ESTABLISH,FILE_NAME,'D_ESTABLISH',line,ierr)
     CALL GET_Float_VAL(MaxCarryBiomass,FILE_NAME,'MaxCarryBiomass',line,ierr)
     CALL GET_Float_VAL(LateralGrowthRate,FILE_NAME,'LateralGrowthRate',line,ierr)
     CALL GET_Float_VAL(VerticalGrowthRate,FILE_NAME,'VerticalGrowthRate',line,ierr)
     CALL GET_Float_VAL(MaxStemDensity,FILE_NAME,'MaxStemDensity',line,ierr)
     CALL GET_Float_VAL(MaxStemHeight,FILE_NAME,'MaxStemHeight',line,ierr)
     CALL GET_Float_VAL(MaxStemDiameter,FILE_NAME,'MaxStemDiameter',line,ierr)
     CALL GET_Float_VAL(MinStemDiameter,FILE_NAME,'MinStemDiameter',line,ierr)
     CALL GET_Float_VAL(DiameterGrowthRate,FILE_NAME,'DiameterGrowthRate',line,ierr)
     CALL GET_Float_VAL(COEF_DIFF_VEG,FILE_NAME,'COEF_DIFF_VEG',line,ierr)
     CALL GET_Float_VAL(TAU_CR_VEG,FILE_NAME,'TAU_CR_VEG',line,ierr)
     CALL GET_Float_VAL(HGT_CR_VEG,FILE_NAME,'HGT_CR_VEG',line,ierr)
     CALL GET_Float_VAL(COEF_MORTALITY_INUN,FILE_NAME,'COEF_MORTALITY_INUN',line,ierr)
     CALL GET_Float_VAL(COEF_MORTALITY_FLOW,FILE_NAME,'COEF_MORTALITY_FLOW',line,ierr)

# if defined (PARALLEL)
       if (myid.eq.0) WRITE(3,'(A28,A50)')'INITIAL_STEMHEIGHT_FILE = ', INITIAL_STEMHEIGHT_FILE
       if (myid.eq.0) WRITE(3,'(A28,A50)')'INITIAL_STEMDENSITY_FILE = ', INITIAL_STEMDENSITY_FILE
       if (myid.eq.0) WRITE(3,'(A28,A50)')'INITIAL_STEMDIAMETER_FILE = ', INITIAL_STEMDIAMETER_FILE
       if (myid.eq.0) WRITE(3,'(A10,F8.4)')'DT_ECO = ', DT_ECO
       if (myid.eq.0) WRITE(3,'(A20,F8.4)')'STEMDENSITY_BARE = ', STEMDENSITY_BARE
       if (myid.eq.0) WRITE(3,'(A17,F8.4)')'P_ESTABLISH = ', P_ESTABLISH
       if (myid.eq.0) WRITE(3,'(A17,F8.4)')'H_ESTABLISH = ', H_ESTABLISH
       if (myid.eq.0) WRITE(3,'(A17,F8.4)')'N_ESTABLISH = ', N_ESTABLISH
       if (myid.eq.0) WRITE(3,'(A17,F10.6)')'D_ESTABLISH = ', D_ESTABLISH
       if (myid.eq.0) WRITE(3,'(A17,F10.4)')'MaxCarryBiomass = ', MaxCarryBiomass
       if (myid.eq.0) WRITE(3,'(A17,F8.4)')'LateralGrowthRate = ', LateralGrowthRate
       if (myid.eq.0) WRITE(3,'(A17,F8.4)')'VerticalGrowthRate = ', VerticalGrowthRate
       if (myid.eq.0) WRITE(3,'(A17,F8.4)')'MaxStemDensity = ', MaxStemDensity
       if (myid.eq.0) WRITE(3,'(A17,F8.4)')'MaxStemHeight = ', MaxStemHeight
       if (myid.eq.0) WRITE(3,'(A17,F10.6)')'MaxStemDiameter = ', MaxStemDiameter
       if (myid.eq.0) WRITE(3,'(A17,F10.6)')'MinStemDiameter = ', MinStemDiameter
       if (myid.eq.0) WRITE(3,'(A17,F10.6)')'DiameterGrowthRate = ', DiameterGrowthRate
       if (myid.eq.0) WRITE(3,'(A17,F8.4)')'COEF_DIFF_VEG = ', COEF_DIFF_VEG
       if (myid.eq.0) WRITE(3,'(A17,F8.4)')'TAU_CR_VEG = ', TAU_CR_VEG
       if (myid.eq.0) WRITE(3,'(A17,F8.4)')'HGT_CR_VEG = ', HGT_CR_VEG
       if (myid.eq.0) WRITE(3,'(A17,F8.2)')'COEF_MORTALITY_INUN = ', COEF_MORTALITY_INUN
       if (myid.eq.0) WRITE(3,'(A17,F8.2)')'COEF_MORTALITY_FLOW = ', COEF_MORTALITY_FLOW
# else
       WRITE(3,'(A28,A50)')'INITIAL_STEMHEIGHT_FILE = ', INITIAL_STEMHEIGHT_FILE
       WRITE(3,'(A28,A50)')'INITIAL_STEMDENSITY_FILE = ', INITIAL_STEMDENSITY_FILE
       WRITE(3,'(A28,A50)')'INITIAL_STEMDIAMETER_FILE = ', INITIAL_STEMDIAMETER_FILE
       WRITE(3,'(A10,F8.4)')'DT_ECO = ', DT_ECO
       WRITE(3,'(A20,F8.4)')'STEMDENSITY_BARE = ', STEMDENSITY_BARE
       WRITE(3,'(A17,F8.4)')'P_ESTABLISH = ', P_ESTABLISH
       WRITE(3,'(A17,F8.4)')'H_ESTABLISH = ', H_ESTABLISH
       WRITE(3,'(A17,F8.4)')'N_ESTABLISH = ', N_ESTABLISH
       WRITE(3,'(A17,F10.6)')'D_ESTABLISH = ', D_ESTABLISH
       WRITE(3,'(A17,F10.4)')'MaxCarryBiomass = ', MaxCarryBiomass
       WRITE(3,'(A17,F8.4)')'LateralGrowthRate = ', LateralGrowthRate
       WRITE(3,'(A17,F8.4)')'VerticalGrowthRate = ', VerticalGrowthRate
       WRITE(3,'(A17,F8.4)')'MaxStemDensity = ', MaxStemDensity
       WRITE(3,'(A17,F8.4)')'MaxStemHeight = ', MaxStemHeight
       WRITE(3,'(A17,F10.6)')'MaxStemDiameter = ', MaxStemDiameter
       WRITE(3,'(A17,F10.6)')'MinStemDiameter = ', MinStemDiameter
       WRITE(3,'(A17,F10.6)')'DiameterGrowthRate = ', DiameterGrowthRate
       WRITE(3,'(A17,F8.4)')'COEF_DIFF_VEG = ', COEF_DIFF_VEG
       WRITE(3,'(A17,F8.4)')'TAU_CR_VEG = ', TAU_CR_VEG
       WRITE(3,'(A17,F8.4)')'HGT_CR_VEG = ', HGT_CR_VEG
       WRITE(3,'(A17,F8.2)')'COEF_MORTALITY_INUN = ', COEF_MORTALITY_INUN
       WRITE(3,'(A17,F8.2)')'COEF_MORTALITY_FLOW = ', COEF_MORTALITY_FLOW
# endif
# endif

! sediment, eco-morpho and vegetation parts (above) need reading protection
! -------------------------------------------------------------------------

! numerics schemes
      CALL GET_LOGICAL_VAL(ADI,FILE_NAME,'ADI',line,ierr)

      IF(ierr==1)THEN
        ADI = .FALSE.
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'ADI: Use default,', 'FALSE'
         WRITE(3,'(A40,A40)')'ADI: Use default,', 'FALSE'
      endif
# else
         WRITE(*,'(A40,A40)')'ADI: Use default,', 'FALSE'
         WRITE(3,'(A40,A40)')'ADI: Use default,', 'FALSE'
# endif
      ENDIF

      CALL GET_LOGICAL_VAL(HOPSCOTCH,FILE_NAME,'HOPSCOTCH',line,ierr)

      IF(ierr==1)THEN
        HOPSCOTCH = .TRUE.
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'HOPSCOTCH: Use default,', 'TRUE'
         WRITE(3,'(A40,A40)')'HOPSCOTCH: Use default,', 'TRUE'
      endif
# else
         WRITE(*,'(A40,A40)')'HOPSCOTCH: Use default,', 'TRUE'
         WRITE(3,'(A40,A40)')'HOPSCOTCH: Use default,', 'TRUE'
# endif
      ENDIF

      CALL GET_LOGICAL_VAL(IMPLICIT,FILE_NAME,'IMPLICIT',line,ierr)
      IF(ierr==1)THEN
        IMPLICIT = .TRUE.
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'IMPLICIT: Use default,', 'TRUE'
         WRITE(3,'(A40,A40)')'IMPLICIT: Use default,', 'TRUE'
      endif
# else
         WRITE(*,'(A40,A40)')'IMPLICIT: Use default,', 'TRUE'
         WRITE(3,'(A40,A40)')'IMPLICIT: Use default,', 'TRUE'
# endif
      ENDIF


!     IF(MANNING_TYPE=='SUBGRID')IMPLICIT=.FALSE. ! wu did this, dont know why

      CALL GET_Float_VAL(CFL,FILE_NAME,'CFL',line,ierr)

      IF(ierr==1)THEN
        CFL = 0.5
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'CFL: Use default,', '0.5'
         WRITE(3,'(A40,A40)')'CFL: Use default,', '0.5'
      endif
# else
         WRITE(*,'(A40,A40)')'CFL: Use default,', '0.5'
         WRITE(3,'(A40,A40)')'CFL: Use default,', '0.5'
# endif
      ENDIF


# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A5,F12.2)')'CFL=', CFL
# else
      WRITE(3,'(A5,F12.2)')'CFL=', CFL
# endif

! Froude Number Cap
      CALL GET_Float_VAL(FroudeCap,FILE_NAME,'FroudeCap',line,ierr)

      IF(ierr==1)THEN
        FroudeCap = 3.0
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'FroudeCap: Use default,', '3.0'
         WRITE(3,'(A40,A40)')'FroudeCap: Use default,', '3.0'
      endif
# else
         WRITE(*,'(A40,A40)')'FroudeCap: Use default,', '3.0'
         WRITE(3,'(A40,A40)')'FroudeCap: Use default,', '3.0'
# endif
      ENDIF

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A5,F12.2)')'FroudeCap=', FroudeCap
# else
      WRITE(3,'(A5,F12.2)')'FroudeCap=', FroudeCap
# endif

! MinDepth etc
      CALL GET_Float_VAL(MinDepth,FILE_NAME,'MinDepth',line,ierr)

      IF(ierr==1)THEN
        MinDepth = 0.1
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'MinDepth: Use default,', '0.1 m'
         WRITE(3,'(A40,A40)')'MinDepth: Use default,', '0.1 m'
      endif
# else
         WRITE(*,'(A40,A40)')'MinDepth: Use default,', '0.1 m'
         WRITE(3,'(A40,A40)')'MinDepth: Use default,', '0.1 m'
# endif
      ENDIF

      CALL GET_Float_VAL(MinDepFric,FILE_NAME,'MinDepFric',line,ierr)

      IF(ierr==1)THEN
        MinDepFric = MinDepth
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'MinDepthFric: Use default,', 'same as MinDepFric'
         WRITE(3,'(A40,A40)')'MinDepthFric: Use default,', 'same as MinDepFric'
      endif
# else
         WRITE(*,'(A40,A40)')'MinDepthFric: Use default,', 'same as MinDepFric'
         WRITE(3,'(A40,A40)')'MinDepthFric: Use default,', 'same as MinDepFric'
# endif
      ENDIF


# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.6)')'MinDepth=', MinDepth
      if (myid.eq.0) WRITE(3,'(A12,F12.6)')'MinDepFric=', MinDepFric
# else
      WRITE(3,'(A10,F12.6)')'MinDepth=', MinDepth
      WRITE(3,'(A12,F12.6)')'MinDepFric=', MinDepFric
# endif

! subgrid
# if defined (SUBGRID)
      CALL GET_INTEGER_VAL(SubMainGridRatio,FILE_NAME,'SubMainGridRatio',line,ierr)

      IF(ierr==1)THEN
        SubMainGridRatio = 1
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'SubMainGridRatio: not defined', 'Use 1'
         WRITE(3,'(A40,A40)')'SubMainGridRatio: not defined', 'Use 1'
      endif
# else
         WRITE(*,'(A40,A40)')'SubMainGridRatio: not defined', 'Use 1'
         WRITE(3,'(A40,A40)')'SubMainGridRatio: not defined', 'Use 1'
# endif
      ENDIF

      CALL GET_Float_VAL(EtaMinVal,FILE_NAME,'EtaMinVal',line,ierr)

      IF(ierr==1)THEN
        EtaMinVal = -1.0
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'EtaMinVal: not defined', 'Use -1.0'
         WRITE(3,'(A40,A40)')'EtaMinVal: not defined', 'Use -1.0'
      endif
# else
         WRITE(*,'(A40,A40)')'EtaMinVal: not defined', 'Use -1.0'
         WRITE(3,'(A40,A40)')'EtaMinVal: not defined', 'Use -1.0'
# endif
      ENDIF

      CALL GET_Float_VAL(EtaMaxVal,FILE_NAME,'EtaMaxVal',line,ierr)

      IF(ierr==1)THEN
        EtaMaxVal = 1.0
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'EtaMaxVal: not defined', 'Use 1.0'
         WRITE(3,'(A40,A40)')'EtaMaxVal: not defined', 'Use 1.0'
      endif
# else
         WRITE(*,'(A40,A40)')'EtaMaxVal: not defined', 'Use 1.0'
         WRITE(3,'(A40,A40)')'EtaMaxVal: not defined', 'Use 1.0'
# endif
      ENDIF

      CALL GET_Float_VAL(D_Eta,FILE_NAME,'D_Eta',line,ierr)

      IF(ierr==1)THEN
        D_Eta = 1.0
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'D_Eta: not defined', 'Use 1.0'
         WRITE(3,'(A40,A40)')'D_Eta: not defined', 'Use 1.0'
      endif
# else
         WRITE(*,'(A40,A40)')'D_Eta: not defined', 'Use 1.0'
         WRITE(3,'(A40,A40)')'D_Eta: not defined', 'Use 1.0'
# endif
      ENDIF

      CALL GET_INTEGER_VAL(PolyOrder,FILE_NAME,'PolyOrder',line,ierr)
      IF(ierr==1)THEN
        PolyOrder = 2.0
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'PolyOrder: not defined', 'Use 2.0'
         WRITE(3,'(A40,A40)')'PolyOrder: not defined', 'Use 2.0'
      endif
# else
         WRITE(*,'(A40,A40)')'PolyOrder: not defined', 'Use 2.0'
         WRITE(3,'(A40,A40)')'PolyOrder: not defined', 'Use 2.0'
# endif
      ENDIF

# endif
! end subgrid

! obstacle structures
      CALL GET_STRING_VAL(OBSTACLE_FILE,FILE_NAME,'OBSTACLE_FILE',line,ierr)
      IF(ierr==1)THEN
        OBSTACLE=.FALSE.
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A5)')'OBSTACLE_FILE:', 'NO'
# else
      WRITE(3,'(A15,A5)')'OBSTACLE_FILE:', 'NO'
# endif
      ELSE
        OBSTACLE=.TRUE.
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A50)')'OBSTACLE_FILE:', OBSTACLE_FILE
# else
      WRITE(3,'(A15,A50)')'OBSTACLE_FILE:', OBSTACLE_FILE
# endif
      ENDIF

! morphology inactive points
      CALL GET_STRING_VAL(INACTIVE_PNT_FILE,FILE_NAME,'INACTIVE_PNT_FILE',line,ierr)
      IF(ierr==1)THEN
        INACTIVE_PNT=.FALSE.
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A18,A5)')'INACTIVE_PNT_FILE:', 'NO'
# else
      WRITE(3,'(A18,A5)')'INACTIVE_PNT_FILE:', 'NO'
# endif
      ELSE
        INACTIVE_PNT=.TRUE.
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A18,A50)')'INACTIVE_PNT_FILE:', INACTIVE_PNT_FILE
# else
      WRITE(3,'(A18,A50)')'INACTIVE_PNT_FILE:', INACTIVE_PNT_FILE
# endif
      ENDIF

! station files
      CALL GET_INTEGER_VAL(NumberStations,FILE_NAME,'NumberStations',line,ierr)
      IF(NumberStations>0)THEN
      CALL GET_STRING_VAL(STATIONS_FILE,FILE_NAME,'STATIONS_FILE',line,ierr)

      IF(ierr==1)THEN
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40,A40)')'STATIONS_FILE:', 'NOT FOUND, STOP'
         WRITE(3,'(A40,A40)')'STATIONS_FILE:', 'NOT FOUND, STOP'
      endif
       call MPI_FINALIZE ( ier )
# else
         WRITE(*,'(A40,A40)')'STATIONS_FILE:', 'NOT FOUND, STOP'
         WRITE(3,'(A40,A40)')'STATIONS_FILE:', 'NOT FOUND, STOP'
# endif
        STOP
      ENDIF

      ENDIF ! end number of station
      
! output parameters
      CALL GET_LOGICAL_VAL(OUT_DEPTH,FILE_NAME,'DEPTH_OUT',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_U,FILE_NAME,'U',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_V,FILE_NAME,'V',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_ETA,FILE_NAME,'ETA',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_P,FILE_NAME,'P',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_Q,FILE_NAME,'Q',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_MASK,FILE_NAME,'MASK',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_PORO,FILE_NAME,'PORO',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_FRIC,FILE_NAME,'FRIC',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_SED,FILE_NAME,'SED',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_BED,FILE_NAME,'BED',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_HVEG,FILE_NAME,'Hveg',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_NVEG,FILE_NAME,'Nveg',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_DVEG,FILE_NAME,'Dveg',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_SUBGRID_UV,FILE_NAME,'SUBGRID_UV',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_SUBGRID_ETA,FILE_NAME,'SUBGRID_ETA',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_SUBGRID_TAU,FILE_NAME,'SUBGRID_TAU',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_RESIST_FORCE,FILE_NAME,'RESIST_FORCE',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_TMP,FILE_NAME,'TMP',line,ierr)

!
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,*)' --------------input end --------------'
# else
      WRITE(3,*)' --------------input end --------------'
# endif

END SUBROUTINE READ_INPUT

SUBROUTINE STATIONS
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: iunit
     INTEGER :: dum1,dum2,dum11,dum22
     CHARACTER(LEN=80)::FILE_NAME
     CHARACTER(LEN=80)::TMP_NAME
     CHARACTER(LEN=80)::FDIR

! initialize stations
     FDIR=TRIM(RESULT_FOLDER)
     icount_stations=icount_stations+1
     if (icount_stations.eq.1) then
       ALLOCATE(ista(NumberStations),&
                jsta(NumberStations),&
                nsta(NumberStations),&
                iista(NumberStations),&
                jjsta(NumberStations) )
! calculate how many output components

       open(100,FILE=TRIM(STATIONS_FILE))
       do i=1,NumberStations
          read(100,*) dum1,dum2 ! ,dum11,dum22          
          dum11 = INT((dum1-1)/SubMainGridRatio)+1
          dum22 = INT((dum2-1)/SubMainGridRatio)+1
          iista(i) = mod(dum1,SubMainGridRatio)
          jjsta(i) = mod(dum2,SubMainGridRatio)
          ! in case ii=0
          if(iista(i)==0)iista(i)=SubMainGridRatio
          if(jjsta(i)==0)jjsta(i)=SubMainGridRatio      
    
# if defined (PARALLEL)
          ista(i) = Nghost+dum11-npx*Mglob/px
          jsta(i) = Nghost+dum22-npy*Nglob/py
          if ((ista(i).ge.Ibeg).and.(ista(i).le.Iend).and.&
              (jsta(i).ge.Jbeg).and.(jsta(i).le.Jend)) then
             nsta(i) = 1
             write(file_name(1:4),'(I4.4)') i
             TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
             iunit=100+i
             open(iunit,FILE=TMP_NAME)
          else
             nsta(i) = 0
          endif
# else
          ista(i) = Nghost+dum11
          jsta(i) = Nghost+dum22
          if ((ista(i).ge.Ibeg).and.(ista(i).le.Iend).and.&
              (jsta(i).ge.Jbeg).and.(jsta(i).le.Jend)) then
             nsta(i) = 1
             write(file_name(1:4),'(I4.4)') i
             TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
             iunit=100+i
             open(iunit,FILE=TMP_NAME)
          else
             nsta(i) = 0
          endif

# endif

       enddo
     endif


! write to stations

     do i=1,NumberStations
       if (nsta(i).eq.1) then
          iunit=100+i

          write (iunit,'(20E16.5)') time,  eta(ista(i),jsta(i)),&
                          u(ista(i),jsta(i)),v(ista(i),jsta(i)),&
                          p(ista(i),jsta(i)),q(ista(i),jsta(i)),&
                          porosity(ista(i),jsta(i))
                        ! &,   etasubgrid(ista(i),jsta(i),iista(i),jjsta(i)),&
                        !  ucsubgrid(ista(i),jsta(i),iista(i),jjsta(i)),&
                        !  vcsubgrid(ista(i),jsta(i),iista(i),jjsta(i)),&
                        !  tausubgrid(ista(i),jsta(i),iista(i),jjsta(i)),&
                        !  totresistfx(ista(i),jsta(i)),totresistfy(ista(i),jsta(i))
# if defined(SEDIMENT)
                          &, con(ista(i),jsta(i))
# endif
       endif
     enddo

! close station files
     if (TIME.ge.TOTAL_TIME) then
       do i=1,NumberStations
          if (nsta(i).eq.1) then
             iunit=100+i
             close(iunit)
          endif
       enddo
     endif

END SUBROUTINE STATIONS

SUBROUTINE PREVIEW
     USE GLOBAL
# if defined (SUBSURFACE)
     USE SUBSURFACE_MODULE
# endif
     IMPLICIT NONE
     REAL(SP),DIMENSION(Mloc,Nloc) :: ETA_MASKOUT

     CHARACTER(LEN=4)::FILE_NAME = ' '
     CHARACTER(LEN=80)::TMP_NAME = ' '
     CHARACTER(LEN=80)::FDIR

     INTEGER :: tmpI,tmpJ

     FDIR=TRIM(RESULT_FOLDER)

     ICOUNT=ICOUNT+1

# if defined (PARALLEL)
        if (myid.eq.0)then
        WRITE(3,102)'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME,'/',Total_Time
        WRITE(*,102)'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME,'/',Total_Time
        endif
# else
        WRITE(*,102)'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME,'/',Total_Time
# endif
102     FORMAT(A20,I4,A14,F12.3,A2,F12.3)

        itmp1=mod(icount/1000,10)
        itmp2=mod(icount/100,10)
        itmp3=mod(icount/10,10)
        itmp4=mod(icount,10)

        write(file_name(1:1),'(I1)')itmp1
        write(file_name(2:2),'(I1)')itmp2
        write(file_name(3:3),'(I1)')itmp3
        write(file_name(4:4),'(I1)')itmp4

    IF(icount==1)THEN
      ! do nothing right now
    ENDIF

    IF(OUT_ETA)THEN
      TMP_NAME = TRIM(FDIR)//'eta_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME,Eta,Mloc,Nloc)
    ENDIF
    
    IF(OUT_SUBGRID_ETA.AND.SubMainGridRatio.GT.1)THEN
      ALLOCATE(EtaSubGridOut(MMloc,NNloc))
      CALL FLOW2ECO(EtaSubGrid,EtaSubGridOut)
      TMP_NAME = TRIM(FDIR)//'eta_sub_'//TRIM(FILE_NAME)
      call PutFile_SubGrid(ICOUNT,TMP_NAME,EtaSubGridOut,MMloc,NNloc)
      DEALLOCATE(EtaSubGridOut)
    ENDIF

    IF(OUT_U)THEN
        TMP_NAME = TRIM(FDIR)//'u_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,U,Mloc1,Nloc)
     ENDIF

     IF(OUT_V)THEN
        TMP_NAME = TRIM(FDIR)//'v_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,V,Mloc,Nloc1)
     ENDIF
     
     IF(OUT_SUBGRID_UV.AND.CONSTRUCT_SUBGRID_UV.AND. &
        MANNING_TYPE=='SUBGRID'.AND.SubMainGridRatio.GT.1)THEN           
       
       ALLOCATE(USubGridOut(MMloc,NNloc))    
       CALL FLOW2ECO(UcSubGrid,USubGridOut)
       TMP_NAME = TRIM(FDIR)//'usub_'//TRIM(FILE_NAME)
       call PutFile_SubGrid(ICOUNT,TMP_NAME,USubGridOut,MMloc,NNloc)
       DEALLOCATE(USubGridOut)
       
       ALLOCATE(VSubGridOut(MMloc,NNloc))
       CALL FLOW2ECO(VcSubGrid,VSubGridOut)
       TMP_NAME = TRIM(FDIR)//'vsub_'//TRIM(FILE_NAME)
       call PutFile_SubGrid(ICOUNT,TMP_NAME,VSubGridOut,MMloc,NNloc)
       DEALLOCATE(VSubGridOut)
       
     ENDIF
     
     IF(OUT_MASK)THEN
        TMP_NAME = TRIM(FDIR)//'masku_'//TRIM(FILE_NAME)
        Int2Flo(1:Mloc1,1:Nloc)=MASKu(1:Mloc1,1:Nloc)
        call PutFile(TMP_NAME,Int2Flo(1:Mloc1,1:Nloc),Mloc1,Nloc)
       TMP_NAME = TRIM(FDIR)//'maskv_'//TRIM(FILE_NAME)
        Int2Flo(1:Mloc,1:Nloc1)=MASKv(1:Mloc,1:Nloc1)
        call PutFile(TMP_NAME,Int2Flo(1:Mloc,1:Nloc1),Mloc,Nloc1)
       TMP_NAME = TRIM(FDIR)//'mask_'//TRIM(FILE_NAME)
        Int2Flo(1:Mloc,1:Nloc)=MASK(1:Mloc,1:Nloc)
        call PutFile(TMP_NAME,Int2Flo(1:Mloc,1:Nloc),Mloc,Nloc)
     ENDIF

     IF(OUT_P)THEN
        TMP_NAME = TRIM(FDIR)//'p_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME,P,Mloc1,Nloc)
     ENDIF

     IF(OUT_Q)THEN
        TMP_NAME = TRIM(FDIR)//'q_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME,Q,Mloc,Nloc)
     ENDIF

     IF(OUT_PORO)THEN
        TMP_NAME = TRIM(FDIR)//'por_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,POROSITY,Mloc,Nloc)

        TMP_NAME = TRIM(FDIR)//'hu_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,H_u,Mloc1,Nloc)

        TMP_NAME = TRIM(FDIR)//'hv_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,H_v,Mloc,Nloc1)
     ENDIF
     
     IF(OUT_SUBGRID_TAU.AND.MANNING_TYPE=='SUBGRID')THEN
        ALLOCATE(TauSubGridOut(MMloc,NNloc))       
        CALL FLOW2ECO(TauSubGrid,TauSubGridOut)
        TMP_NAME = TRIM(FDIR)//'tau_'//TRIM(FILE_NAME)
        CALL PutFile_SubGrid(ICOUNT,TMP_NAME,TauSubGridOut,MMloc,NNloc)
        DEALLOCATE(TauSubGridOut)
     ENDIF
     
     IF(OUT_FRIC)THEN
# if defined(PARALLEL)
        CALL PHI_EXCH_2(Cdu)
        CALL PHI_EXCH_3(Cdv)
# endif
        TMP_NAME = TRIM(FDIR)//'cdu_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,Cdu,Mloc1,Nloc)

        TMP_NAME = TRIM(FDIR)//'cdv_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,Cdv,Mloc,Nloc1)
     ENDIF

     IF(OUT_RESIST_FORCE)THEN
# if defined(PARALLEL)
        CALL PHI_EXCH_2(TotResistFX)
        CALL PHI_EXCH_3(TotResistFY)
# endif
        TMP_NAME = TRIM(FDIR)//'fx_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,TotResistFX,Mloc1,Nloc)
        TMP_NAME = TRIM(FDIR)//'fy_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,TotResistFY,Mloc,Nloc1)
     ENDIF
          
# if defined(SEDIMENT)
     IF(OUT_SED)THEN
        TMP_NAME = TRIM(FDIR)//'sed_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,CON,Mloc,Nloc)
     ENDIF

     IF(UPDATE_BED.AND.OUT_BED)THEN
        ALLOCATE(DepSubGridOut(MMloc,NNloc))
        CALL FLOW2ECO(DepSubGrid,DepSubGridOut)
        TMP_NAME = TRIM(FDIR)//'bed_'//TRIM(FILE_NAME)
        CALL PutFile_SubGrid(ICOUNT,TMP_NAME,DepSubGridOut,MMloc,NNloc)
        DEALLOCATE(DepSubGridOut)
     ENDIF
# endif

# if defined (SUBSURFACE)

     IF(OUT_ETA_GROUND)THEN
        TMP_NAME = TRIM(FDIR)//'etagrn_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,ETA_ground,Mloc,Nloc)
     ENDIF

     IF(OUT_P_GROUND)THEN
        TMP_NAME = TRIM(FDIR)//'pgrn_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,P_ground,Mloc1,Nloc)
     ENDIF

     IF(OUT_Q_GROUND)THEN
        TMP_NAME = TRIM(FDIR)//'qgrn_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,Q_ground,Mloc,Nloc1)
     ENDIF

!     IF(OUT_MASK_GROUND)THEN
!        TMP_NAME = TRIM(FDIR)//'maskgro_'//TRIM(FILE_NAME)
!        Int2Flo(1:Mloc,1:Nloc)=MASK_GROUND(1:Mloc,1:Nloc)
!        call PutFile(TMP_NAME,Int2Flo(1:Mloc,1:Nloc),Mloc,Nloc)
!     ENDIF

!     IF(OUT_MASKu_GROUND)THEN
!        TMP_NAME = TRIM(FDIR)//'maskugrn_'//TRIM(FILE_NAME)
!        Int2Flo(1:Mloc1,1:Nloc)=MASKu_GROUND(1:Mloc1,1:Nloc)
!        call PutFile(TMP_NAME,Int2Flo(1:Mloc1,1:Nloc),Mloc1,Nloc)
!     ENDIF

!     IF(OUT_MASKv_GROUND)THEN
!        TMP_NAME = TRIM(FDIR)//'maskvgrn_'//TRIM(FILE_NAME)
!        Int2Flo(1:Mloc,1:Nloc1)=MASKv_GROUND(1:Mloc,1:Nloc1)
!        call PutFile(TMP_NAME,Int2Flo(1:Mloc,1:Nloc1),Mloc,Nloc1)
!     ENDIF


# endif
! end subsurface


! ******** tmp output
     IF(OUT_TMP)THEN
        TMP_NAME = TRIM(FDIR)//'tmp_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,tmp4preview,Mloc,Nloc)
     ENDIF


END SUBROUTINE PREVIEW

# if defined(ECO_MORPHOLOGY)
SUBROUTINE PREVIEW_ECO
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP),DIMENSION(MMloc,NNloc) :: ETA_MASKOUT
     CHARACTER(LEN=4) ::FILE_NAME = ' '
     CHARACTER(LEN=80)::TMP_NAME = ' '
     CHARACTER(LEN=80)::FDIR
     INTEGER :: tmpI,tmpJ, ktmp1,ktmp2,ktmp3,ktmp4

     FDIR=TRIM(RESULT_FOLDER)

     ICOUNT_ECO=ICOUNT_ECO+1

# if defined (PARALLEL)
        if (myid.eq.0)then
        WRITE(3,102)'PRINTING ECOLOGY FILE NO.', icount_eco, ' TIME/TOTAL: ', TIME,'/',Total_Time
        WRITE(*,102)'PRINTING ECOLOGY FILE NO.', icount_eco, ' TIME/TOTAL: ', TIME,'/',Total_Time
        endif
# else
        WRITE(*,102)'PRINTING ECOLOGY FILE NO.', icount_eco, ' TIME/TOTAL: ', TIME,'/',Total_Time
# endif
102     FORMAT(A20,I4,A14,F12.3,A2,F12.3)

        ktmp1=mod(icount_eco/1000,10)
        ktmp2=mod(icount_eco/100,10)
        ktmp3=mod(icount_eco/10,10)
        ktmp4=mod(icount_eco,10)

        write(file_name(1:1),'(I1)')ktmp1
        write(file_name(2:2),'(I1)')ktmp2
        write(file_name(3:3),'(I1)')ktmp3
        write(file_name(4:4),'(I1)')ktmp4

    IF(icount_eco==1)THEN
      ! do nothing right now
    ENDIF

     IF(OUT_HVEG)THEN
        TMP_NAME = TRIM(FDIR)//'Hveg_'//TRIM(FILE_NAME)
        call PutFile_SubGrid(ICOUNT_ECO,TMP_NAME,STEMHEIGHT,MMloc,NNloc)
     ENDIF
     IF(OUT_NVEG)THEN
        TMP_NAME = TRIM(FDIR)//'Nveg_'//TRIM(FILE_NAME)
        call PutFile_SubGrid(ICOUNT_ECO,TMP_NAME,STEMDENSITY,MMloc,NNloc)
     ENDIF
     IF(OUT_DVEG)THEN
        TMP_NAME = TRIM(FDIR)//'Dveg_'//TRIM(FILE_NAME)
        call PutFile_SubGrid(ICOUNT_ECO,TMP_NAME,STEMDIAMETER,MMloc,NNloc)
     ENDIF

END SUBROUTINE PREVIEW_ECO
# endif

# if defined (PARALLEL)
SUBROUTINE GetFile (FILE,PHI)
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: l
     ! could be max. procs
     INTEGER,DIMENSION(NumberProcessor) :: npxs,npys
     REAL(SP),DIMENSION(NumberProcessor) :: xx
     REAL(SP),DIMENSION(MGlob+2*Nghost,NGlob+2*Nghost) :: PHIGLOB
     CHARACTER(LEN=80) FILE
     REAL(SP),DIMENSION(Mloc,Nloc),INTENT(OUT) :: PHI

! TEMP

     if (myid.eq.0) then
        OPEN(1,FILE=TRIM(FILE))
        DO J=Nghost+1,NGlob+NGhost
           READ(1,*)(PHIGLOB(I,J),I=Nghost+1,MGlob+Nghost)
        ENDDO
        CLOSE(1)
! ghost cells
        DO I=Nghost+1,MGlob+Nghost
           DO J=1,Nghost
              PHIGLOB(I,J)=PHIGLOB(I,Nghost+1)
           ENDDO
           DO J=NGlob+Nghost+1,NGlob+2*Nghost
              PHIGLOB(I,J)=PHIGLOB(I,NGlob+Nghost)
           ENDDO
        ENDDO
        DO J=1,NGlob+2*Nghost
           DO I=1,Nghost
              PHIGLOB(I,J)=PHIGLOB(Nghost+1,J)
           ENDDO
           DO I=MGlob+Nghost+1,MGlob+2*Nghost
              PHIGLOB(I,J)=PHIGLOB(MGlob+Nghost,J)
           ENDDO
        ENDDO
     endif

     call MPI_Gather(npx,1,MPI_INTEGER,npxs,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)
     call MPI_Gather(npy,1,MPI_INTEGER,npys,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)

     do i=1,Mloc
     do j=1,Nloc
        if (myid.eq.0) then
           do l=1,px*py
              xx(l) = PHIGLOB(i+npxs(l)*(Iend-Ibeg+1),&
                   j+npys(l)*(Jend-Jbeg+1))
           enddo
        endif
        call MPI_Scatter(xx,1,MPI_SP,&
             PHI(i,j),1,MPI_SP,0,MPI_COMM_WORLD,ier)
     enddo
     enddo

END SUBROUTINE Getfile

# else

SUBROUTINE GetFile (FILE,PHI)
     USE GLOBAL
     IMPLICIT NONE
     CHARACTER(LEN=80) FILE
     REAL(SP),DIMENSION(Mloc,Nloc),INTENT(OUT) :: PHI

     OPEN(1,FILE=TRIM(FILE))
     DO J=Jbeg,Jend
        READ(1,*)(PHI(I,J),I=Ibeg,Iend)
     ENDDO
     CLOSE(1)
! ghost cells
     DO I=Ibeg,Jend
       DO J=1,Nghost
         PHI(I,J)=PHI(I,Jbeg)
       ENDDO
       DO J=Jend1,Nloc
         PHI(I,J)=PHI(I,Jend)
       ENDDO
     ENDDO
     DO J=1,Nloc
       DO I=1,Nghost
         PHI(I,J)=PHI(Ibeg,J)
       ENDDO
       DO I=Iend1,Mloc
         PHI(I,J)=PHI(Iend,J)
       ENDDO
     ENDDO

END SUBROUTINE Getfile
# endif
! end parallel

! subgrid
# if defined (SUBGRID)
# if defined (PARALLEL)
SUBROUTINE GetFile_SubGrid (FILE,PHI)
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: l
     ! could be max. procs
     INTEGER,DIMENSION(NumberProcessor) :: npxs,npys
     REAL(SP),DIMENSION(NumberProcessor) :: xx
     REAL(SP),DIMENSION(:,:),ALLOCATABLE :: PHIGLOB

!     REAL(SP),DIMENSION((MGlob+2*Nghost)*SubMainGridRatio, &
!                        (NGlob+2*Nghost)*SubMainGridRatio) :: PHIGLOB
     CHARACTER(LEN=80) FILE
     REAL(SP),DIMENSION(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),INTENT(OUT) :: PHI
     REAL(SP),DIMENSION(Mloc*SubMainGridRatio,Nloc*SubMainGridRatio):: PHI_tmp


     ALLOCATE(PHIGLOB((MGlob+2*Nghost)*SubMainGridRatio, &
                        (NGlob+2*Nghost)*SubMainGridRatio))

     if (myid.eq.0) then
        OPEN(1,FILE=TRIM(FILE))
        DO J=Nghost*SubMainGridRatio+1,(NGlob+Nghost)*SubMainGridRatio
           READ(1,*)(PHIGLOB(I,J),I=Nghost*SubMainGridRatio+1, &
                                    (MGlob+Nghost)*SubMainGridRatio)
        ENDDO
        CLOSE(1)
! ghost cells
        DO I=Nghost*SubMainGridRatio+1,(MGlob+Nghost)*SubMainGridRatio
           DO J=1,Nghost*SubMainGridRatio
              PHIGLOB(I,J)=PHIGLOB(I,Nghost*SubMainGridRatio+1)
           ENDDO
           DO J=(NGlob+Nghost)*SubMainGridRatio+1,  &
                (NGlob+2*Nghost)*SubMainGridRatio
              PHIGLOB(I,J)=PHIGLOB(I,(NGlob+Nghost)*SubMainGridRatio)
           ENDDO
        ENDDO
        DO J=1,(NGlob+2*Nghost)*SubMainGridRatio
           DO I=1,Nghost*SubMainGridRatio
              PHIGLOB(I,J)=PHIGLOB(Nghost*SubMainGridRatio+1,J)
           ENDDO
           DO I=(MGlob+Nghost)*SubMainGridRatio+1,  &
                (MGlob+2*Nghost)*SubMainGridRatio
              PHIGLOB(I,J)=PHIGLOB((MGlob+Nghost)*SubMainGridRatio,J)
           ENDDO
        ENDDO
     endif

     call MPI_Gather(npx,1,MPI_INTEGER,npxs,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)
     call MPI_Gather(npy,1,MPI_INTEGER,npys,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)

     do i=1,Mloc*SubMainGridRatio
     do j=1,Nloc*SubMainGridRatio
        if (myid.eq.0) then
           do l=1,px*py
              xx(l) = PHIGLOB(i+npxs(l)*(Mloc-2*Nghost)*SubMainGridRatio,&
                   j+npys(l)*(Nloc-2*Nghost)*SubMainGridRatio)
           enddo
        endif
        call MPI_Scatter(xx,1,MPI_SP,&
             PHI_tmp(i,j),1,MPI_SP,0,MPI_COMM_WORLD,ier)
     enddo
     enddo

     DO J=1,Nloc
     DO I=1,Mloc
       DO JJ=1,SubMainGridRatio
       DO II=1,SubMainGridRatio
         PHI(I,J,II,JJ)=PHI_tmp((I-1)*SubMainGridRatio+II,(J-1)*SubMainGridRatio+JJ)
       ENDDO
       ENDDO
     ENDDO
     ENDDO

! check
!open(2,file='tmp0.txt')
!     do j=1,(NGlob+Nghost*2)*SubMainGridRatio
!      write(2,192) (phiglob(i,j),i=1,(MGlob+2*Nghost)*SubMainGridRatio)
!     enddo
!close(2)
!192   format(3000f12.6)

     DEALLOCATE(PHIGLOB)

END SUBROUTINE Getfile_Subgrid
# else
! not paralle
SUBROUTINE GetFile_SubGrid (FILE,PHI)
     USE GLOBAL
     IMPLICIT NONE
     INTEGER :: l
     ! could be max. procs
!     REAL(SP),DIMENSION((MGlob+2*Nghost)*SubMainGridRatio, &
!                        (NGlob+2*Nghost)*SubMainGridRatio) :: PHIGLOB
     REAL(SP),DIMENSION(:,:),ALLOCATABLE :: PHIGLOB

     CHARACTER(LEN=80) FILE
     REAL(SP),DIMENSION(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),INTENT(OUT) :: PHI

!     REAL(SP),DIMENSION(Mloc*SubMainGridRatio,Nloc*SubMainGridRatio):: PHI_tmp

     ALLOCATE(PHIGLOB((MGlob+2*Nghost)*SubMainGridRatio, &
                        (NGlob+2*Nghost)*SubMainGridRatio))

        OPEN(1,FILE=TRIM(FILE))
        DO J=Nghost*SubMainGridRatio+1,(NGlob+Nghost)*SubMainGridRatio
           READ(1,*)(PHIGLOB(I,J),I=Nghost*SubMainGridRatio+1, &
                                    (MGlob+Nghost)*SubMainGridRatio)
        ENDDO
        CLOSE(1)

! ghost cells
        DO I=Nghost*SubMainGridRatio+1,(MGlob+Nghost)*SubMainGridRatio
           DO J=1,Nghost*SubMainGridRatio
              PHIGLOB(I,J)=PHIGLOB(I,Nghost*SubMainGridRatio+1)
           ENDDO
           DO J=(NGlob+Nghost)*SubMainGridRatio+1,  &
                (NGlob+2*Nghost)*SubMainGridRatio
              PHIGLOB(I,J)=PHIGLOB(I,(NGlob+Nghost)*SubMainGridRatio)
           ENDDO
        ENDDO
        DO J=1,(NGlob+2*Nghost)*SubMainGridRatio
           DO I=1,Nghost*SubMainGridRatio
              PHIGLOB(I,J)=PHIGLOB(Nghost*SubMainGridRatio+1,J)
           ENDDO
           DO I=(MGlob+Nghost)*SubMainGridRatio+1,  &
                (MGlob+2*Nghost)*SubMainGridRatio
              PHIGLOB(I,J)=PHIGLOB((MGlob+Nghost)*SubMainGridRatio,J)
           ENDDO
        ENDDO

     DO J=1,Nloc
     DO I=1,Mloc
       DO JJ=1,SubMainGridRatio
       DO II=1,SubMainGridRatio
         PHI(I,J,II,JJ)=PHIGLOB((I-1)*SubMainGridRatio+II,(J-1)*SubMainGridRatio+JJ)
       ENDDO
       ENDDO
     ENDDO
     ENDDO

! check
!open(2,file='tmp0.txt')
!     do j=1,(NGlob+Nghost*2)*SubMainGridRatio
!      write(2,192) (phiglob(i,j),i=1,(MGlob+2*Nghost)*SubMainGridRatio)
!     enddo
!close(2)
!192   format(3000f12.6)

     DEALLOCATE(PHIGLOB)

END SUBROUTINE GetFile_SubGrid
# endif
! end parallel

# endif
! endsubgrid


# if defined (PARALLEL)
SUBROUTINE PutFile (FILE,PHI,M,N)
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: l
     ! could be max. procs
     INTEGER,DIMENSION(NumberProcessor) :: npxs,npys
     INTEGER,INTENT(IN) :: M,N
     REAL(SP),DIMENSION(NumberProcessor) :: xx
     REAL(SP),DIMENSION(MGlob+2*Nghost,NGlob+2*Nghost) :: PHIGLOB
     CHARACTER(LEN=80) FILE
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: PHI


! first time call
     IF(icount.EQ.1)THEN
! format length
        write(FORMAT_LEN(1:1),'(A1)') '('
        write(FORMAT_LEN(2:8),'(I7)') Mglob
        write(FORMAT_LEN(9:13),'(A5)') 'E16.6'
        write(FORMAT_LEN(14:14),'(A1)') ')'
     ENDIF

     call MPI_Gather(npx,1,MPI_INTEGER,npxs,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)
     call MPI_Gather(npy,1,MPI_INTEGER,npys,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)

     do i=1,Mloc
     do j=1,Nloc
        call MPI_Gather(PHI(i,j),1,MPI_SP,&
             xx,1,MPI_SP,0,MPI_COMM_WORLD,ier)

        if (j.eq.1) call MPI_Barrier(MPI_COMM_WORLD,ier)

        if (myid.eq.0) then
           do l=1,px*py
              PHIGLOB(i+npxs(l)*(Iend-Ibeg+1),&
                   j+npys(l)*(Jend-Jbeg+1)) = xx(l)
           enddo
        endif
     enddo
     enddo

     if (myid.eq.0) then
        OPEN(1,FILE=TRIM(FILE))
        DO J=Nghost+1,NGlob+NGhost,OUTPUT_RES
           WRITE(1,FORMAT_LEN)(real(PHIGLOB(I,J)),I=Nghost+1,MGlob+Nghost,OUTPUT_RES)
        ENDDO
!100  FORMAT(5000E16.6)
!100   FORMAT(FORMAT_LEN)
        CLOSE(1)
     endif

END SUBROUTINE Putfile

# else
SUBROUTINE PutFile(FILE,PHI,M,N)
     USE PARAM
     USE GLOBAL
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: PHI
     CHARACTER(LEN=80),INTENT(IN) :: FILE

! first time call
     IF(icount.EQ.1)THEN
! format length
        write(FORMAT_LEN(1:1),'(A1)') '('
        write(FORMAT_LEN(2:8),'(I7)') Mloc1
        write(FORMAT_LEN(9:13),'(A5)') 'E16.6'
        write(FORMAT_LEN(14:14),'(A1)') ')'
     ENDIF

        OPEN(1,FILE=TRIM(FILE))
# if defined (DEBUG)
        DO J=1,N
           WRITE(1,100)(real(PHI(I,J)),I=1,M)
        ENDDO
# else
        DO J=Nghost+1,Nloc-Nghost,OUTPUT_RES
           WRITE(1,FORMAT_LEN)(real(PHI(I,J)),I=Nghost+1,Mloc-Nghost,OUTPUT_RES)
        ENDDO
# endif
100  FORMAT(5000E16.6)
!100   FORMAT(FORMAT_LEN)
        CLOSE(1)
END SUBROUTINE PutFile

# endif


# if defined (PARALLEL)
SUBROUTINE PutFile_SubGrid (IcountFile,FILE,PHI,M,N)
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: l
     ! could be max. procs
     INTEGER,DIMENSION(NumberProcessor) :: npxs,npys
     INTEGER,INTENT(IN) :: IcountFile,M,N
     REAL(SP),DIMENSION(NumberProcessor) :: xx
     REAL(SP),DIMENSION(MMglob+2*Nghost*SubMainGridRatio,NNglob+2*Nghost*SubMainGridRatio) :: PHIGLOB
     CHARACTER(LEN=80) FILE
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: PHI


! first time call
     IF(IcountFile.EQ.1)THEN
! format length
        write(FORMAT_LEN_SUB(1:1),'(A1)') '('
        write(FORMAT_LEN_SUB(2:8),'(I7)') MMglob
        write(FORMAT_LEN_SUB(9:13),'(A5)') 'E16.6'
        write(FORMAT_LEN_SUB(14:14),'(A1)') ')'
     ENDIF

     call MPI_Gather(npx,1,MPI_INTEGER,npxs,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)
     call MPI_Gather(npy,1,MPI_INTEGER,npys,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)

     do i=1,MMloc
     do j=1,NNloc
        call MPI_Gather(PHI(i,j),1,MPI_SP,&
             xx,1,MPI_SP,0,MPI_COMM_WORLD,ier)

        if (j.eq.1) call MPI_Barrier(MPI_COMM_WORLD,ier)

        if (myid.eq.0) then
           do l=1,px*py
              PHIGLOB(i+npxs(l)*(IIend-IIbeg+1),&
                   j+npys(l)*(JJend-JJbeg+1)) = xx(l)
           enddo
        endif
     enddo
     enddo

     if (myid.eq.0) then
        OPEN(1,FILE=TRIM(FILE))
        DO J=Nghost*SubMainGridRatio+1,(Nglob+Nghost)*SubMainGridRatio,OUTPUT_RES
           WRITE(1,FORMAT_LEN_SUB)(real(PHIGLOB(I,J)), &
                 I=Nghost*SubMainGridRatio+1,(Mglob+Nghost)*SubMainGridRatio,OUTPUT_RES)
        ENDDO
!100  FORMAT(5000E16.6)
        CLOSE(1)
     endif

END SUBROUTINE Putfile_SubGrid

# else
SUBROUTINE PutFile_SubGrid(IcountFile,FILE,PHI,M,N)
     USE PARAM
     USE GLOBAL
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: IcountFile,M,N
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: PHI
     CHARACTER(LEN=80),INTENT(IN) :: FILE

! first time call
     IF(IcountFile.EQ.1)THEN
! format length
        write(FORMAT_LEN_SUB(1:1),'(A1)') '('
        write(FORMAT_LEN_SUB(2:8),'(I7)') MMglob
        write(FORMAT_LEN_SUB(9:13),'(A5)') 'E16.6'
        write(FORMAT_LEN_SUB(14:14),'(A1)') ')'
     ENDIF

        OPEN(1,FILE=TRIM(FILE))
# if defined (DEBUG)
        DO J=1,N
           WRITE(1,100)(real(PHI(I,J)),I=1,M)
        ENDDO
# else
        DO J=Nghost*SubMainGridRatio+1,(Nglob+Nghost)*SubMainGridRatio,OUTPUT_RES
           WRITE(1,FORMAT_LEN_SUB)(real(PHI(I,J)),I=Nghost*SubMainGridRatio+1,(Mglob+Nghost)*SubMainGridRatio,OUTPUT_RES)
        ENDDO
# endif
100  FORMAT(5000E16.6)
        CLOSE(1)
END SUBROUTINE PutFile_SubGrid

# endif


!# if defined (SUBGRID)
!# if defined (PARALLEL)
!SUBROUTINE PutFile_SubGrid (FILE,PHI,M,N)
!     USE GLOBAL
!     IMPLICIT NONE
!
!     INTEGER :: l
!     ! could be max. procs
!     INTEGER,DIMENSION(NumberProcessor) :: npxs,npys
!     INTEGER,INTENT(IN) :: M,N
!     REAL(SP),DIMENSION(NumberProcessor) :: xx
!     REAL(SP),DIMENSION(MGlob*SubMainGridRatio,NGlob*SubMainGridRatio) :: PHIGLOB
!     CHARACTER(LEN=80) FILE
!     REAL(SP),DIMENSION(M,N),INTENT(IN) :: PHI
!
!
!! first time call
!     IF(icount.EQ.1)THEN
!! format length
!        write(FORMAT_LEN(1:1),'(A1)') '('
!        write(FORMAT_LEN(2:8),'(I7)') Mglob*SubMainGridRatio
!        write(FORMAT_LEN(9:13),'(A5)') 'E16.6'
!        write(FORMAT_LEN(14:14),'(A1)') ')'
!     ENDIF
!
!     call MPI_Gather(npx,1,MPI_INTEGER,npxs,1,MPI_INTEGER,&
!          0,MPI_COMM_WORLD,ier)
!     call MPI_Gather(npy,1,MPI_INTEGER,npys,1,MPI_INTEGER,&
!          0,MPI_COMM_WORLD,ier)
!
!     do i=1,Mloc*SubMainGridRatio
!     do j=1,Nloc*SubMainGridRatio
!        call MPI_Gather(PHI(i,j),1,MPI_SP,&
!             xx,1,MPI_SP,0,MPI_COMM_WORLD,ier)
!
!        if (j.eq.1) call MPI_Barrier(MPI_COMM_WORLD,ier)
!
!        if (myid.eq.0) then
!           do l=1,px*py
!              PHIGLOB(i+npxs(l)*Mloc*SubMainGridRatio,&
!                   j+npys(l)*Nloc*SubMainGridRatio) = xx(l)
!           enddo
!        endif
!     enddo
!     enddo
!
!     if (myid.eq.0) then
!        OPEN(1,FILE=TRIM(FILE))
!        DO J=1,NGlob*SubMainGridRatio,OUTPUT_RES
!           WRITE(1,FORMAT_LEN)(real(PHIGLOB(I,J)),I=1,MGlob*SubMainGridRatio,OUTPUT_RES)
!        ENDDO
!!100  FORMAT(5000E16.6)
!!100   FORMAT(FORMAT_LEN)
!        CLOSE(1)
!     endif
!
!END SUBROUTINE Putfile_SubGrid
!
!# else
!SUBROUTINE PutFile_SubGrid(FILE,PHI,M,N)
!     USE PARAM
!     USE GLOBAL
!     IMPLICIT NONE
!     INTEGER,INTENT(IN) :: M,N
!     REAL(SP),DIMENSION(M*SubMainGridRatio,N*SubMainGridRatio),INTENT(IN) :: PHI
!     CHARACTER(LEN=80),INTENT(IN) :: FILE
!
!! first time call
!     IF(icount.EQ.1)THEN
!! format length
!        write(FORMAT_LEN(1:1),'(A1)') '('
!        write(FORMAT_LEN(2:8),'(I7)') Mglob*SubMainGridRatio
!        write(FORMAT_LEN(9:13),'(A5)') 'E16.6'
!        write(FORMAT_LEN(14:14),'(A1)') ')'
!     ENDIF
!
!        OPEN(1,FILE=TRIM(FILE))
!# if defined (DEBUG)
!        DO J=1,N*SubMainGridRatio
!           WRITE(1,100)(real(PHI(I,J)),I=1,M*SubMainGridRatio)
!        ENDDO
!# else
!        DO J=1,N*SubMainGridRatio,OUTPUT_RES
!           WRITE(1,FORMAT_LEN)(real(PHI(I,J)),I=1,Mloc*SubMainGridRatio,OUTPUT_RES)
!        ENDDO
!# endif
!100  FORMAT(5000E16.6)
!!100   FORMAT(FORMAT_LEN)
!        CLOSE(1)
!END SUBROUTINE PutFile_SubGrid
!
!# endif
!# endif

SUBROUTINE STATISTICS
     USE GLOBAL
     IMPLICIT NONE

     REAL(SP)::MassVolume=ZERO,Energy=ZERO,MaxEta=ZERO,MinEta=ZERO, &
              MaxU=ZERO,MaxV=ZERO,FrU=ZERO,UTotal=ZERO,UTotalMax=ZERO, &
              FrV=ZERO,FrMax=ZERO,MaxSed=ZERO,MinSed=ZERO
# if defined (PARALLEL)
     REAL(SP)::myvar
# endif
!

     MassVolume=ZERO
     Energy=ZERO
     UTotalMax=ZERO

     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

! Vol=SUM(Eta*dx*dy), reference is at z=0
! Energy=SUM(1/2*g*H^2*dx*dy+0.5*u^2*H*dx*dy)

! estimate not exact
       MassVolume=MassVolume+Eta(I,J)*DX*DY
       Energy=Energy+0.5_SP*H_u(I,J)*H_u(I,J)*GRAV*DX*DY &
             +0.5_SP*U(I,J)*U(I,J)*H_u(I,J)*DX*DY &
             +0.5_SP*V(I,J)*V(I,J)*H_u(I,J)*DX*DY

     ENDDO
     ENDDO

     MaxEta=MAXVAL(Eta(Ibeg:Iend,Jbeg:Jend))
     MinEta=MINVAL(Eta(Ibeg:Iend,Jbeg:Jend))
     MaxU=MAXVAL(ABS(U(Ibeg:Iend1,Jbeg:Jend)))
     MaxV=MAXVAL(ABS(V(Ibeg:Iend,Jbeg:Jend1)))
# if defined(SEDIMENT)
     MaxSed=MAXVAL(CON(Ibeg:Iend,Jbeg:Jend))
     MinSed=MINVAL(CON(Ibeg:Iend,Jbeg:Jend))
# endif

! found Froude vs. max speed

     FrMax=ZERO
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend1
       FrU=U(I,J)/SQRT(GRAV*Max(H_u(I,J),MinDepth))
       IF(FrU.gt.FrMax)THEN
         FrMax=FrU
       ENDIF
     ENDDO
     ENDDO
     DO J=Jbeg,Jend1
     DO I=Ibeg,Iend
       FrV=V(I,J)/SQRT(GRAV*Max(H_v(I,J),MinDepth))
       IF(FrV.gt.FrMax)THEN
         FrMax=FrV
       ENDIF
     ENDDO
     ENDDO

# if defined (PARALLEL)
     call MPI_ALLREDUCE(MassVolume,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     MassVolume = myvar
     call MPI_ALLREDUCE(Energy,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     Energy = myvar
     call MPI_ALLREDUCE(MaxEta,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     MaxEta = myvar
     call MPI_ALLREDUCE(MinEta,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     MinEta = myvar
     call MPI_ALLREDUCE(MaxU,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     MaxU = myvar
     call MPI_ALLREDUCE(MaxV,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     MaxV = myvar
!     call MPI_ALLREDUCE(UTotalMax,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
!     UTotalMax = myvar
     call MPI_ALLREDUCE(FrMax,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     FrMax = myvar
# if defined(SEDIMENT)
     call MPI_ALLREDUCE(MaxSed,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     MaxSed = myvar
     call MPI_ALLREDUCE(MinSed,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     MinSed = myvar
# endif
# endif

# if defined (PARALLEL)
     if (myid.eq.0) then
# endif

! print screen
     WRITE(*,*) '----------------- STATISTICS ----------------'
     WRITE(*,*) ' TIME          DT'
     WRITE(*,101) Time, DT
     WRITE(*,*) ' MassVolume    Energy        MaxEta        MinEta        Max U         Max V '
     WRITE(*,101)  MassVolume,Energy,MaxEta,MinEta,MaxU,MaxV
     WRITE(*,*) ' Froude        MaxSed        MinSed        MaxDepChange'
     WRITE(*,101) FrMax,MaxSed,MinSed
! print log file
     WRITE(3,*) '----------------- STATISTICS ----------------'
     WRITE(3,*) ' TIME          DT'
     WRITE(3,101) Time, DT
     WRITE(3,*) ' MassVolume    Energy        MaxEta        MinEta        Max U         Max V '
     WRITE(3,101)  MassVolume,Energy,MaxEta,MinEta,MaxU,MaxV
     WRITE(3,*) ' Froude        MaxSed        MinSed        MaxDepChange'
     WRITE(3,101) FrMax,MaxSed,MinSed
# if defined (PARALLEL)
     endif
# endif

101  FORMAT(6E14.6)

END SUBROUTINE STATISTICS




