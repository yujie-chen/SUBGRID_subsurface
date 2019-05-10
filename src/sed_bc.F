# if defined (SEDIMENT)
# if defined (COUPLING)
SUBROUTINE InitSedimentCoupling
   USE GLOBAL
   IMPLICIT NONE
       OPEN(12,FILE=TRIM(BOUNDARY_SED_FILE))
         READ(12,*)  ! title
         READ(12,*)  ! boundary info
!  bnd points num, start point
         READ(12,*)  ! east
         READ(12,*) N_SEDBND_EAST,J_START_EAST
         READ(12,*)  ! west
         READ(12,*) N_SEDBND_WEST,J_START_WEST
         READ(12,*)  ! south
         READ(12,*) N_SEDBND_SOUTH,I_START_SOUTH
         READ(12,*)  ! north
         READ(12,*) N_SEDBND_NORTH,I_START_NORTH

! read time and variable at the first level
         READ(12,*) ! time start title
         READ(12,*) TIME_SEDBND_1
! initialize time_2
         TIME_SEDBND_2 = TIME_SEDBND_1

! east
         IF(N_SEDBND_EAST.GT.0)THEN
           ALLOCATE(C_SEDBND_EAST(N_SEDBND_EAST,2))
             READ(12,*)   ! east
             READ(12,119)(C_SEDBND_EAST(I,2),I=1,N_SEDBND_EAST)
           ! initialize first step
             C_SEDBND_EAST(:,1)=C_SEDBND_EAST(:,2)
         ELSE
             READ(12,*)
         ENDIF ! n_SEDBND_east
119      FORMAT(5E16.6)

! west
         IF(N_SEDBND_WEST.GT.0)THEN
           ALLOCATE(C_SEDBND_WEST(N_SEDBND_WEST,2))
             READ(12,*)   ! west
             READ(12,119)(C_SEDBND_WEST(I,2),I=1,N_SEDBND_WEST)
           ! initialize first step
             C_SEDBND_WEST(:,1)=C_SEDBND_WEST(:,2)
         ELSE
             READ(12,*)
         ENDIF ! n_SEDBND_west

! south
         IF(N_SEDBND_SOUTH.GT.0)THEN
           ALLOCATE(C_SEDBND_SOUTH(N_SEDBND_SOUTH,2))
             READ(12,*)   ! south
             READ(12,119)(C_SEDBND_SOUTH(I,2),I=1,N_SEDBND_SOUTH)
           ! initialize first step
             C_SEDBND_SOUTH(:,1)=C_SEDBND_SOUTH(:,2)
         ELSE
             READ(12,*)
         ENDIF ! n_SEDBND_south

! north
         IF(N_SEDBND_NORTH.GT.0)THEN
           ALLOCATE(C_SEDBND_NORTH(N_SEDBND_NORTH,2))
             READ(12,*)   ! north
             READ(12,119)(C_SEDBND_NORTH(I,2),I=1,N_SEDBND_NORTH)
           ! initialize first step
             C_SEDBND_NORTH(:,1)=C_SEDBND_NORTH(:,2)
         ELSE
             READ(12,*)
         ENDIF ! n_SEDBND_north


! specify boundary start points

! west boundary
   IF(N_SEDBND_WEST>0)THEN
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
      Kstart_SED_WEST=J_start_SED_WEST+Nghost -npy*Nglob/py
      Kend_SED_WEST = J_start_SED_WEST+Nghost+N_SEDBND_WEST-1 -npy*Nglob/py
      IF((Kstart_SED_WEST>Nghost.AND.Kstart_SED_WEST<Nloc-Nghost+1).OR.&
     (Kend_SED_WEST<Nloc-Nghost+1.AND.Kend_SED_WEST>Nghost))THEN
       IF(Kstart_SED_WEST<Nghost+1)THEN
         Kshift_SED_WEST= -Kstart_SED_WEST+Nghost+1
         Kstart_SED_WEST=Nghost+1
       ELSE
         Kshift_SED_WEST=-(Kstart_SED_WEST-Nghost)+1
       ENDIF
       IF(Kend_SED_WEST>Nloc-Nghost)THEN
         Kend_SED_WEST=Nloc-Nghost
       ENDIF
       SED_IN_DOMAIN_WEST=.TRUE.
      ELSE
       IF(Kstart_SED_WEST<=Nghost.AND.Kend_SED_WEST>=Nloc-Nghost+1)THEN
         Kshift_SED_WEST=-Kstart_SED_WEST+Nghost+1
         Kstart_SED_WEST = Nghost+1
         Kend_SED_WEST = Nloc-Nghost
         SED_IN_DOMAIN_WEST=.TRUE.
       ELSE
         SED_IN_DOMAIN_WEST=.FALSE.
       ENDIF
      ENDIF
! check print*,myid,Kshift_SED_WEST,Kstart_SED_WEST,Kend_SED_WEST
     endif
# else
      Kstart_SED_WEST=J_start_SED_WEST+Nghost
      Kend_SED_WEST = J_start_SED_WEST+Nghost+N_SEDBND_WEST-1
      Kshift_SED_WEST = -(Kstart_SED_WEST-Nghost)+1
      SED_IN_DOMAIN_WEST = .TRUE.
# endif

   ENDIF

! east boundary
   IF(N_SEDBND_EAST>0)THEN
# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
      Kstart_SED_EAST=J_start_SED_EAST+Nghost -npy*Nglob/py
      Kend_SED_EAST = J_start_SED_EAST+Nghost+N_SEDBND_EAST-1 -npy*Nglob/py
      IF((Kstart_SED_EAST>Nghost.AND.Kstart_SED_EAST<Nloc-Nghost+1).OR.&
     (Kend_SED_EAST<Nloc-Nghost+1.AND.Kend_SED_EAST>Nghost))THEN
       IF(Kstart_SED_EAST<Nghost+1)THEN
         Kshift_SED_EAST= -Kstart_SED_EAST+Nghost+1
         Kstart_SED_EAST=Nghost+1
       ELSE
         Kshift_SED_EAST=-(Kstart_SED_EAST-Nghost)+1
       ENDIF
       IF(Kend_SED_EAST>Nloc-Nghost)THEN
         Kend_SED_EAST=Nloc-Nghost
       ENDIF
       SED_IN_DOMAIN_EAST=.TRUE.
      ELSE

       IF(Kstart_SED_EAST<=Nghost.AND.Kend_SED_EAST>=Nloc-Nghost+1)THEN
         Kshift_SED_EAST=-Kstart_SED_EAST+Nghost+1
         Kstart_SED_EAST = Nghost+1
         Kend_SED_EAST = Nloc-Nghost
         SED_IN_DOMAIN_EAST=.TRUE.
       ELSE
         SED_IN_DOMAIN_EAST=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_SED_EAST=J_start_SED_EAST+Nghost
      Kend_SED_EAST = J_start_SED_EAST+Nghost+N_SEDBND_EAST-1
      Kshift_SED_EAST = -(Kstart_SED_EAST-Nghost)+1
      SED_IN_DOMAIN_EAST = .TRUE.
# endif
    ENDIF

! south boundary
   IF(N_SEDBND_SOUTH>0)THEN
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
      Kstart_SED_SOUTH=I_start_SED_SOUTH+Nghost -npx*Mglob/px
      Kend_SED_SOUTH = I_start_SED_SOUTH+Nghost+N_SEDBND_SOUTH-1 -npx*Mglob/px
      IF((Kstart_SED_SOUTH>Nghost.AND.Kstart_SED_SOUTH<Mloc-Nghost+1).OR.&
     (Kend_SED_SOUTH<Mloc-Nghost+1.AND.Kend_SED_SOUTH>Nghost))THEN
       IF(Kstart_SED_SOUTH<Nghost+1)THEN
         Kshift_SED_SOUTH= -Kstart_SED_SOUTH+Nghost+1
         Kstart_SED_SOUTH=Nghost+1
       ELSE
         Kshift_SED_SOUTH=-(Kstart_SED_SOUTH-Nghost)+1
       ENDIF
       IF(Kend_SED_SOUTH>Mloc-Nghost)THEN
         Kend_SED_SOUTH=Mloc-Nghost
       ENDIF
       SED_IN_DOMAIN_SOUTH=.TRUE.
      ELSE

       IF(Kstart_SED_SOUTH<=Nghost.AND.Kend_SED_SOUTH>=Mloc-Nghost+1)THEN
         Kshift_SED_SOUTH=-Kstart_SED_SOUTH+Nghost+1
         Kstart_SED_SOUTH = Nghost+1
         Kend_SED_SOUTH = Mloc-Nghost
         SED_IN_DOMAIN_SOUTH=.TRUE.
       ELSE
         SED_IN_DOMAIN_SOUTH=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_SED_SOUTH=I_start_SED_SOUTH+Nghost
      Kend_SED_SOUTH = I_start_SED_SOUTH+Nghost+N_SEDBND_SOUTH-1
      Kshift_SED_SOUTH = -(Kstart_SED_SOUTH-Nghost)+1
      SED_IN_DOMAIN_SOUTH = .TRUE.
# endif
   ENDIF

! north boundary
   IF(N_SEDBND_NORTH>0)THEN
# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
      Kstart_SED_NORTH=I_start_SED_NORTH+Nghost -npx*Mglob/px
      Kend_SED_NORTH = I_start_SED_NORTH+Nghost+N_SEDBND_NORTH-1 -npx*Mglob/px
      IF((Kstart_SED_NORTH>Nghost.AND.Kstart_SED_NORTH<Mloc-Nghost+1).OR.&
     (Kend_SED_NORTH<Mloc-Nghost+1.AND.Kend_SED_NORTH>Nghost))THEN
       IF(Kstart_SED_NORTH<Nghost+1)THEN
         Kshift_SED_NORTH= -Kstart_SED_NORTH+Nghost+1
         Kstart_SED_NORTH=Nghost+1
       ELSE
         Kshift_SED_NORTH=-(Kstart_SED_NORTH-Nghost)+1
       ENDIF
       IF(Kend_SED_NORTH>Mloc-Nghost)THEN
         Kend_SED_NORTH=Mloc-Nghost
       ENDIF
       SED_IN_DOMAIN_NORTH=.TRUE.
      ELSE

       IF(Kstart_SED_NORTH<=Nghost.AND.Kend_SED_NORTH>=Mloc-Nghost+1)THEN
         Kshift_SED_NORTH=-Kstart_SED_NORTH+Nghost+1
         Kstart_SED_NORTH = Nghost+1
         Kend_SED_NORTH = Mloc-Nghost
         SED_IN_DOMAIN_NORTH=.TRUE.
       ELSE
         SED_IN_DOMAIN_NORTH=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_SED_NORTH=I_start_SED_NORTH+Nghost
      Kend_SED_NORTH = I_start_SED_NORTH+Nghost+N_SEDBND_NORTH-1
      Kshift_SED_NORTH = -(Kstart_SED_NORTH-Nghost)+1
      SED_IN_DOMAIN_NORTH = .TRUE.
# endif
   ENDIF

! open masks for coupling points
     IF(SED_IN_DOMAIN_WEST)THEN
        DO J=Kstart_SED_WEST,Kend_SED_WEST
          MASKu(Ibeg,J)=1
        ENDDO
     ENDIF
     IF(SED_IN_DOMAIN_EAST)THEN
        DO J=Kstart_SED_EAST,Kend_SED_EAST
          MASKu(Iend1,J)=1
        ENDDO
     ENDIF
     IF(SED_IN_DOMAIN_SOUTH)THEN
        DO I=Kstart_SED_SOUTH,Kend_SED_SOUTH
          MASKv(I,Jbeg)=1
        ENDDO
     ENDIF
     IF(SED_IN_DOMAIN_NORTH)THEN
        DO I=Kstart_SED_NORTH,Kend_SED_NORTH
          MASKv(I,Jend1)=1
        ENDDO
     ENDIF

# if defined(DEBUG)
# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'Subroutine InitSedimentCoupling'
    ENDIF
# else
     WRITE(3,*)'Subroutine InitSedimentCoupling'
# endif
# endif

END SUBROUTINE InitSedimentCoupling
# endif
# endif


# if defined (SEDIMENT)
# if defined (COUPLING)
SUBROUTINE SEDIMENT_CLAMPED_BOUNDARY
     USE GLOBAL
     IMPLICIT NONE
119      FORMAT(5E16.6)  ! this is a fixed format for I/O

! determine time slot

    IF(TIME>TIME_SEDBND_1.AND.TIME>TIME_SEDBND_2) THEN

         TIME_SEDBND_1=TIME_SEDBND_2

         READ(12,*,END=120) TIME_SEDBND_2
! east
         IF(N_SEDBND_EAST.GT.0)THEN
             READ(12,*,END=120)   ! east
             C_SEDBND_EAST(:,1)=C_SEDBND_EAST(:,2)
             READ(12,119,END=120)(C_SEDBND_EAST(I,2),I=1,N_SEDBND_EAST)
         ELSE
             READ(12,*,END=120)   ! east
         ENDIF
! west
         IF(N_SEDBND_WEST.GT.0)THEN
             READ(12,*,END=120)   ! west
             C_SEDBND_WEST(:,1)=C_SEDBND_WEST(:,2)
             READ(12,119,END=120)(C_SEDBND_WEST(I,2),I=1,N_SEDBND_WEST)
         ELSE
             READ(12,*,END=120)   ! west
         ENDIF
! south
         IF(N_SEDBND_SOUTH.GT.0)THEN
             READ(12,*,END=120)   ! south
             C_SEDBND_SOUTH(:,1)=C_SEDBND_SOUTH(:,2)
             READ(12,119,END=120)(C_SEDBND_SOUTH(I,2),I=1,N_SEDBND_SOUTH)
         ELSE
             READ(12,*,END=120)   ! south
         ENDIF
! north
         IF(N_SEDBND_NORTH.GT.0)THEN
             READ(12,*,END=120)   ! north
             C_SEDBND_NORTH(:,1)=C_SEDBND_NORTH(:,2)
             READ(12,119,END=120)(C_SEDBND_NORTH(I,2),I=1,N_SEDBND_NORTH)
         ELSE
             READ(12,*,END=120)   ! north
         ENDIF

    ENDIF  ! time>time_2 and time_1

120 CONTINUE

    tmp2=ZERO
    tmp1=ZERO

    IF(TIME>TIME_SEDBND_1)THEN
      IF(TIME_SEDBND_1.EQ.TIME_SEDBND_2)THEN
        ! no more data
        tmp2=ZERO
        tmp1=ZERO
      ELSE
      tmp2=(TIME_SEDBND_2-TIME) &
            /MAX(SMALL, ABS(TIME_SEDBND_2-TIME_SEDBND_1))
      tmp1=1.0_SP - tmp2;
      ENDIF  ! no more data?
    ENDIF ! time>time_1

! assign to boundary cell center instead of only ghost cells
! west boundary
   IF(N_SEDBND_WEST>0)THEN
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif
     IF(SED_IN_DOMAIN_WEST)THEN
      DO J=Kstart_SED_WEST,Kend_SED_WEST
      DO I=1,Nghost+1
        CON(I,J)=C_SEDBND_WEST(J-Nghost+Kshift_SED_WEST,2)*tmp1&
                +C_SEDBND_WEST(J-Nghost+Kshift_SED_WEST,1)*tmp2
      ENDDO
      ENDDO
# if defined (FREESLIP_BC)
      IF(Kstart_SED_WEST.GT.Nghost+1)THEN
        DO J=1,Kstart_SED_WEST-1
        DO I=1,Nghost
          CON(I,J)=CON(Ibeg,J)
        ENDDO
        ENDDO
      ENDIF
      IF(Kend_SED_WEST.LT.Nloc)THEN
        DO J=Kend_SED_WEST+1, Nloc
        DO I=1,Nghost
          CON(I,J)=CON(Ibeg,J)
        ENDDO
        ENDDO
      ENDIF
# endif
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif

# if defined (FREESLIP_BC)
   ELSE
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif
     DO J=1,Nloc
     DO I=1,Nghost
       CON(I,J)=CON(Ibeg,J)
     ENDDO
     ENDDO
# if defined (PARALLEL)
    endif
# endif

# endif
   ENDIF ! end of n_SEDBND_west>0

! east boundary
   IF(N_SEDBND_EAST>0)THEN
# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
     IF(SED_IN_DOMAIN_EAST)THEN
      DO J=Kstart_SED_EAST,Kend_SED_EAST
      DO I=Iend,Iend+Nghost
        CON(I,J)=C_SEDBND_EAST(J-Nghost+Kshift_SED_EAST,2)*tmp1&
                +C_SEDBND_EAST(J-Nghost+Kshift_SED_EAST,1)*tmp2
      ENDDO
      ENDDO
# if defined (FREESLIP_BC)
      IF(Kstart_SED_EAST.GT.Nghost+1)THEN
        DO J=1,Kstart_SED_EAST-1
        DO I=Iend+1,Iend+Nghost
          CON(I,J)=CON(Iend,J)
        ENDDO
        ENDDO
      ENDIF
      IF(Kstart_SED_EAST.LT.Nloc)THEN
        DO J=Kend_SED_EAST+1,Nloc
        DO I=Iend+1,Iend+Nghost
          CON(I,J)=CON(Iend,J)
        ENDDO
        ENDDO
      ENDIF
# endif
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
# if defined (FREESLIP_BC)
   ELSE
# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
     DO J=1,Nloc
     DO I=Iend+1,Iend+Nghost
       CON(I,J)=CON(Iend,J)
     ENDDO
     ENDDO
# if defined (PARALLEL)
    endif
# endif

# endif
   ENDIF ! end of n_SEDBND_east>0

! south boundary
   IF(N_SEDBND_SOUTH>0)THEN
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
     IF(SED_IN_DOMAIN_SOUTH)THEN
      DO I=Kstart_SED_SOUTH,Kend_SED_SOUTH
      DO J=1,Nghost+1
        CON(I,J)=C_SEDBND_SOUTH(I-Nghost+Kshift_SED_SOUTH,2)*tmp1&
                +C_SEDBND_SOUTH(I-Nghost+Kshift_SED_SOUTH,1)*tmp2
      ENDDO
      ENDDO
# if defined (FREESLIP_BC)
      IF(Kstart_SED_SOUTH.GT.Nghost+1)THEN
        DO J=1,Nghost
        DO I=1,Kstart_SED_SOUTH-1
          CON(I,J)=CON(I,Jbeg)
        ENDDO
        ENDDO
      ENDIF
      IF(Kend_SED_SOUTH.LT.Mloc)THEN
        DO J=1,Nghost
        DO I=Kend_SED_SOUTH+1,Mloc
          CON(I,J)=CON(I,Jbeg)
        ENDDO
        ENDDO
      ENDIF
# endif
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
# if defined (FREESLIP_BC)
   ELSE
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
     DO J=1,Nghost
     DO I=1,Mloc
       CON(I,J)=CON(I,Jbeg)
     ENDDO
     ENDDO
# if defined (PARALLEL)
    endif
# endif

# endif
   ENDIF ! end of n_SEDBND_south>0

! north boundary
   IF(N_SEDBND_NORTH>0)THEN
# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
     IF(SED_IN_DOMAIN_NORTH)THEN
      DO I=Kstart_SED_NORTH,Kend_SED_NORTH
      DO J=Jend,Jend+Nghost
        CON(I,J)=C_SEDBND_NORTH(I-Nghost+Kshift_SED_NORTH,2)*tmp1&
                +C_SEDBND_NORTH(I-Nghost+Kshift_SED_NORTH,1)*tmp2
      ENDDO
      ENDDO
# if defined (FREESLIP_BC)
      IF(Kstart_SED_NORTH.GT.Nghost+1)THEN
        DO J=Jend+1,Jend+Nghost
        DO I=1,Kstart_SED_NORTH-1
          CON(I,J)=CON(I,Jend)
        ENDDO
        ENDDO
      ENDIF
      IF(Kend_SED_NORTH.LT.Mloc)THEN
        DO J=Jend+1,Jend+Nghost
        DO I=Kend_SED_NORTH+1,Mloc
          CON(I,J)=CON(I,Jend)
        ENDDO
        ENDDO
      ENDIF
# endif
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
# if defined (FREESLIP_BC)
   ELSE
# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
     DO J=Jend+1,Jend+Nghost
     DO I=1,Mloc
       CON(I,J)=CON(I,Jend)
     ENDDO
     ENDDO
# if defined (PARALLEL)
    endif
# endif
# endif
   ENDIF ! end of n_SEDBND_north>0


# if defined(DEBUG)
# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'Subroutine sediment_clamped_boundary'
    ENDIF
# else
     WRITE(3,*)'Subroutine sediment_clamped_boundary'
# endif
# endif


END SUBROUTINE SEDIMENT_CLAMPED_BOUNDARY
# endif
# endif



# if defined (SEDIMENT)

# if defined (COUPLING)
SUBROUTINE SEDIMENT_NEUMANN_BOUNDARY
     USE GLOBAL
     IMPLICIT NONE
! to be improved: the two long subroutine can be combined. 08/31/15
!
! zero concentration gradient at the boundary.
! if free slip bc, set ghost cell values and 1st grid to the 2nd grid value
! 1st grid: by calculation or equal to 2nd grid?

# if defined(FREESLIP_BC)

!  west boundary

# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif
      DO J=Jbeg,Jend
      DO I=1,Nghost+1
        CON(I,J)=CON(Ibeg+1,J)
      ENDDO
      ENDDO
# if defined (PARALLEL)
    endif
# endif

! east boundary

# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
      DO J=Jbeg,Jend
      DO I=Iend,Mloc
        CON(I,J)=CON(Iend-1,J)
      ENDDO
      ENDDO
# if defined (PARALLEL)
    endif
# endif

! south boundary

# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
      DO I=Ibeg,Iend
      DO J=1,Nghost+1
        CON(I,J)=CON(I,Jbeg+1)
      ENDDO
      ENDDO
# if defined (PARALLEL)
    endif
# endif

! north boundary

# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
      DO I=Ibeg,Iend
      DO J=Jend,Nloc
        CON(I,J)=CON(I,Jend-1)
      ENDDO
      ENDDO
# if defined (PARALLEL)
    endif
# endif

! if no slip bc
# else

! west boundary
   IF(N_SEDBND_WEST>0)THEN
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif
     IF(SED_IN_DOMAIN_WEST)THEN
      DO J=Kstart_SED_WEST,Kend_SED_WEST
      DO I=1,Nghost+1
        CON(I,J)=CON(Ibeg+1,J)
      ENDDO
      ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_SEDBND_west>0

! east boundary
   IF(N_SEDBND_EAST>0)THEN
# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
     IF(SED_IN_DOMAIN_EAST)THEN
      DO J=Kstart_SED_EAST,Kend_SED_EAST
      DO I=Iend,Iend+Nghost
        CON(I,J)=CON(Iend-1,J)
      ENDDO
      ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_SEDBND_east>0

! south boundary
   IF(N_SEDBND_SOUTH>0)THEN
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
     IF(SED_IN_DOMAIN_SOUTH)THEN
      DO I=Kstart_SED_SOUTH,Kend_SED_SOUTH
      DO J=1,Nghost+1
        CON(I,J)=CON(I,Jbeg+1)
      ENDDO
      ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_SEDBND_south>0

! north boundary
   IF(N_SEDBND_NORTH>0)THEN
# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
     IF(SED_IN_DOMAIN_NORTH)THEN
      DO I=Kstart_SED_NORTH,Kend_SED_NORTH
      DO J=Jend,Jend+Nghost
        CON(I,J)=CON(I,Jend-1)
      ENDDO
      ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_SEDBND_north>0

# if defined(DEBUG)
# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'Subroutine sediment_neumann_boundary'
    ENDIF
# else
     WRITE(3,*)'Subroutine sediment_neumann_boundary'
# endif
# endif

# endif
END SUBROUTINE SEDIMENT_NEUMANN_BOUNDARY

# else
SUBROUTINE SEDIMENT_NEUMANN_BOUNDARY
     USE GLOBAL
     IMPLICIT NONE
     INTEGER :: Kpnt,Ip,Jp

     DO Kpnt=1,NumSedPoint

# if defined (PARALLEL)
        Ip = Nghost + I_SEDBND(Kpnt) - npx*Mglob/px
        Jp = Nghost + J_SEDBND(Kpnt) - npy*Nglob/py

        ! west boundary
        if (n_west .eq. MPI_PROC_NULL) then
          DO I = 1, Nghost + 1
             CON(I,Jp)=CON(Ibeg+1,Jp)
          ENDDO
        endif

        ! east boundary
        if (n_east .eq. MPI_PROC_NULL) then
          DO I = Iend, Iend+Nghost
             CON(I,Jp)=CON(Iend-1,Jp)
          ENDDO
        endif

        ! south boundary
        if (n_suth .eq. MPI_PROC_NULL) then
          DO J = 1, Nghost + 1
             CON(Ip,J)=CON(Ip,Jbeg+1)
          ENDDO
        endif

        ! north boundary
        if (n_nrth .eq. MPI_PROC_NULL) then
          DO J = Jend, Jend + Nghost
             CON(Ip,J)=CON(Ip,Jend-1)
          ENDDO
        endif
# else
        ! west
        IF(I_SEDBND(NumSedPoint).eq.1)THEN
          DO I = 1, Nghost+1
            CON(I,J_SEDBND(NumSedPoint)+Nghost) = CON(Ibeg+1,J_SEDBND(NumSedPoint)+Nghost)
          ENDDO
        ENDIF

        ! east
        IF(I_SEDBND(NumSedPoint).eq.Mglob)THEN
          DO I = Iend,Iend+Nghost
            CON(I,J_SEDBND(NumSedPoint)+Nghost) = CON(Iend-1,J_SEDBND(NumSedPoint)+Nghost)
          ENDDO
        ENDIF

        ! south
        IF(J_SEDBND(NumSedPoint).eq.1)THEN
          DO J = 1, Nghost+1
            CON(I_SEDBND(NumSedPoint)+Nghost,J) = CON(I_SEDBND(NumSedPoint)+Nghost,Jbeg+1)
          ENDDO
        ENDIF

        ! north
        IF(J_SEDBND(NumSedPoint).eq.Nglob)THEN
          DO J = Jend,Jend+Nghost
            CON(I_SEDBND(NumSedPoint)+Nghost,J) = CON(I_SEDBND(NumSedPoint)+Nghost,Jend-1)
          ENDDO
        ENDIF

# endif

     ENDDO

END SUBROUTINE SEDIMENT_NEUMANN_BOUNDARY
# endif
! end coupling
# endif
! end sediment

# if defined(SEDIMENT)
SUBROUTINE InitSedimentConstant
   USE GLOBAL
   IMPLICIT NONE

   OPEN(15,FILE=TRIM(CONSTANT_SED_FILE))

   READ(15,*)  ! title
   READ(15,*) SedimentStartDate
   READ(15,*) NumSedPoint
   ALLOCATE (I_SEDBND(NumSedPoint), &
             J_SEDBND(NumSedPoint), &
             C_SEDBND(NumSedPoint) )

   DO I=1,NumSedPoint
      READ(15,*)I_SEDBND(I),J_SEDBND(I), C_SEDBND(I)
   ENDDO
   CLOSE(15)

! need to open masks for boundary points

END SUBROUTINE InitSedimentConstant
# endif



# if defined(SEDIMENT)
SUBROUTINE SEDIMENT_CONSTANT_BOUNDARY
     USE GLOBAL
     IMPLICIT NONE
    INTEGER :: Kpnt,Ip,Jp

     DO Kpnt=1,NumSedPoint

# if defined (PARALLEL)
        Ip=Nghost+I_SEDBND(Kpnt) - npx*Mglob/px
        Jp=Nghost+J_SEDBND(Kpnt) - npy*Nglob/py

        IF(Ip>Mloc.or.Ip<1.or.Jp>Nloc.or.Jp<1)THEN
         ! do nothing
        ELSE
         CON(Ip,Jp)=C_SEDBND(Kpnt)
        ENDIF
# else
        ECON(I_SEDBND(Kpnt)+Nghost,J_SEDBND(Kpnt)+Nghost) = C_SEDBND(Kpnt)
# endif
     ENDDO


END SUBROUTINE SEDIMENT_CONSTANT_BOUNDARY
# endif
