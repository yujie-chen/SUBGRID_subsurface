SUBROUTINE InitRiverFlux
   USE GLOBAL
   IMPLICIT NONE
       OPEN(13,FILE=TRIM(RIVER_FILE))
         READ(13,*)  ! title
         READ(13,*)  ! boundary info
! boundary basic info including point number of river, start point, etc
! east
         READ(13,*)  ! east
         READ(13,*) N_RIVER_EAST,J_START_RIV_EAST
! west
         READ(13,*)  ! west
         READ(13,*) N_RIVER_WEST,J_START_RIV_WEST
! south
         READ(13,*)  ! south
         READ(13,*) N_RIVER_SOUTH,I_START_RIV_SOUTH
! north
         READ(13,*)  ! north
         READ(13,*) N_RIVER_NORTH,I_START_RIV_NORTH

! read time and variable at the first level

         READ(13,*) ! time start title
         READ(13,*) TIME_RIVER_1
! initialize time_2
         TIME_RIVER_2 = TIME_RIVER_1

! east
         IF(N_RIVER_EAST.GT.0)THEN
           ALLOCATE(FLUX_RIVER_EAST(N_RIVER_EAST,2))
           ALLOCATE(CSED_RIVER_EAST(N_RIVER_EAST,2))

             READ(13,*)   ! east
             READ(13,119)(FLUX_RIVER_EAST(I,2),I=1,N_RIVER_EAST)
             READ(13,119)(CSED_RIVER_EAST(I,2),I=1,N_RIVER_EAST)

!   initialize first step
             FLUX_RIVER_EAST(:,1)=FLUX_RIVER_EAST(:,2)
             CSED_RIVER_EAST(:,1)=CSED_RIVER_EAST(:,2)
         ELSE
             READ(13,*)
         ENDIF ! n_RIVER_east
119      FORMAT(5E16.6)

! west
         IF(N_RIVER_WEST.GT.0)THEN
           ALLOCATE(FLUX_RIVER_WEST(N_RIVER_WEST,2))
           ALLOCATE(CSED_RIVER_WEST(N_RIVER_WEST,2))

             READ(13,*)   ! WEST
             READ(13,119)(FLUX_RIVER_WEST(I,2),I=1,N_RIVER_WEST)
             READ(13,119)(CSED_RIVER_WEST(I,2),I=1,N_RIVER_WEST)

!   initialize first step
             FLUX_RIVER_WEST(:,1)=FLUX_RIVER_WEST(:,2)
             CSED_RIVER_WEST(:,1)=CSED_RIVER_WEST(:,2)
         ELSE
             READ(13,*)
         ENDIF ! n_RIVER_WEST

! south
         IF(N_RIVER_SOUTH.GT.0)THEN
           ALLOCATE(FLUX_RIVER_SOUTH(N_RIVER_SOUTH,2))
           ALLOCATE(CSED_RIVER_SOUTH(N_RIVER_SOUTH,2))

             READ(13,*)   ! SOUTH
             READ(13,119)(FLUX_RIVER_SOUTH(I,2),I=1,N_RIVER_SOUTH)
             READ(13,119)(CSED_RIVER_SOUTH(I,2),I=1,N_RIVER_SOUTH)

!   initialize first step
             FLUX_RIVER_SOUTH(:,1)=FLUX_RIVER_SOUTH(:,2)
             CSED_RIVER_SOUTH(:,1)=CSED_RIVER_SOUTH(:,2)
         ELSE
             READ(13,*)
         ENDIF ! n_RIVER_SOUTH

! north
         IF(N_RIVER_NORTH.GT.0)THEN
           ALLOCATE(FLUX_RIVER_NORTH(N_RIVER_NORTH,2))
           ALLOCATE(CSED_RIVER_NORTH(N_RIVER_NORTH,2))

             READ(13,*)   ! NORTH
             READ(13,119)(FLUX_RIVER_NORTH(I,2),I=1,N_RIVER_NORTH)
             READ(13,119)(CSED_RIVER_NORTH(I,2),I=1,N_RIVER_NORTH)

!   initialize first step
             FLUX_RIVER_NORTH(:,1)=FLUX_RIVER_NORTH(:,2)
             CSED_RIVER_NORTH(:,1)=CSED_RIVER_NORTH(:,2)
         ELSE
             READ(13,*)
         ENDIF ! n_RIVER_NORTH


! specify boundary start points

! west boundary
   IF(N_RIVER_WEST>0)THEN
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
      Kstart_RIV_WEST=J_START_RIV_WEST+Nghost -npy*Nglob/py
      Kend_RIV_WEST = J_START_RIV_WEST+Nghost+N_RIVER_WEST-1 -npy*Nglob/py
      IF((Kstart_RIV_WEST>Nghost.AND.Kstart_RIV_WEST<Nloc-Nghost+1).OR.&
     (Kend_RIV_WEST<Nloc-Nghost+1.AND.Kend_RIV_WEST>Nghost))THEN
       IF(Kstart_RIV_WEST<Nghost+1)THEN
         Kshift_RIV_WEST= -Kstart_RIV_WEST+Nghost+1
         Kstart_RIV_WEST=Nghost+1
       ELSE
         Kshift_RIV_WEST=-(Kstart_RIV_WEST-Nghost)+1
       ENDIF
       IF(Kend_RIV_WEST>Nloc-Nghost)THEN
         Kend_RIV_WEST=Nloc-Nghost
       ENDIF
       RIV_IN_DOMAIN_WEST=.TRUE.
      ELSE
       IF(Kstart_RIV_WEST<=Nghost.AND.Kend_RIV_WEST>=Nloc-Nghost+1)THEN
         Kshift_RIV_WEST=-Kstart_RIV_WEST+Nghost+1
         Kstart_RIV_WEST = Nghost+1
         Kend_RIV_WEST = Nloc-Nghost
         RIV_IN_DOMAIN_WEST=.TRUE.
       ELSE
         RIV_IN_DOMAIN_WEST=.FALSE.
       ENDIF
      ENDIF
! check print*,myid,Kshift_RIV_WEST,Kstart_RIV_WEST,Kend_RIV_WEST
     endif
# else
      Kstart_RIV_WEST=J_START_RIV_WEST+Nghost
      Kend_RIV_WEST = J_START_RIV_WEST+Nghost+N_RIVER_WEST-1
      Kshift_RIV_WEST = -(Kstart_RIV_WEST-Nghost)+1
      RIV_IN_DOMAIN_WEST = .TRUE.
# endif

   ENDIF

! east boundary
   IF(N_RIVER_EAST>0)THEN
# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
      Kstart_RIV_EAST=J_START_RIV_EAST+Nghost -npy*Nglob/py
      Kend_RIV_EAST = J_START_RIV_EAST+Nghost+N_RIVER_EAST-1 -npy*Nglob/py
      IF((Kstart_RIV_EAST>Nghost.AND.Kstart_RIV_EAST<Nloc-Nghost+1).OR.&
     (Kend_RIV_EAST<Nloc-Nghost+1.AND.Kend_RIV_EAST>Nghost))THEN
       IF(Kstart_RIV_EAST<Nghost+1)THEN
         Kshift_RIV_EAST= -Kstart_RIV_EAST+Nghost+1
         Kstart_RIV_EAST=Nghost+1
       ELSE
         Kshift_RIV_EAST=-(Kstart_RIV_EAST-Nghost)+1
       ENDIF
       IF(Kend_RIV_EAST>Nloc-Nghost)THEN
         Kend_RIV_EAST=Nloc-Nghost
       ENDIF
       RIV_IN_DOMAIN_EAST=.TRUE.
      ELSE

       IF(Kstart_RIV_EAST<=Nghost.AND.Kend_RIV_EAST>=Nloc-Nghost+1)THEN
         Kshift_RIV_EAST=-Kstart_RIV_EAST+Nghost+1
         Kstart_RIV_EAST = Nghost+1
         Kend_RIV_EAST = Nloc-Nghost
         RIV_IN_DOMAIN_EAST=.TRUE.
       ELSE
         RIV_IN_DOMAIN_EAST=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_RIV_EAST=J_START_RIV_EAST+Nghost
      Kend_RIV_EAST = J_START_RIV_EAST+Nghost+N_RIVER_EAST-1
      Kshift_RIV_EAST = -(Kstart_RIV_EAST-Nghost)+1
      RIV_IN_DOMAIN_EAST = .TRUE.
# endif
    ENDIF

! south boundary
   IF(N_RIVER_SOUTH>0)THEN
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
      Kstart_RIV_SOUTH=I_START_RIV_SOUTH+Nghost -npx*Mglob/px
      Kend_RIV_SOUTH = I_START_RIV_SOUTH+Nghost+N_RIVER_SOUTH-1 -npx*Mglob/px
      IF((Kstart_RIV_SOUTH>Nghost.AND.Kstart_RIV_SOUTH<Mloc-Nghost+1).OR.&
     (Kend_RIV_SOUTH<Mloc-Nghost+1.AND.Kend_RIV_SOUTH>Nghost))THEN
       IF(Kstart_RIV_SOUTH<Nghost+1)THEN
         Kshift_RIV_SOUTH= -Kstart_RIV_SOUTH+Nghost+1
         Kstart_RIV_SOUTH=Nghost+1
       ELSE
         Kshift_RIV_SOUTH=-(Kstart_RIV_SOUTH-Nghost)+1
       ENDIF
       IF(Kend_RIV_SOUTH>Mloc-Nghost)THEN
         Kend_RIV_SOUTH=Mloc-Nghost
       ENDIF
       RIV_IN_DOMAIN_SOUTH=.TRUE.
      ELSE

       IF(Kstart_RIV_SOUTH<=Nghost.AND.Kend_RIV_SOUTH>=Mloc-Nghost+1)THEN
         Kshift_RIV_SOUTH=-Kstart_RIV_SOUTH+Nghost+1
         Kstart_RIV_SOUTH = Nghost+1
         Kend_RIV_SOUTH = Mloc-Nghost
         RIV_IN_DOMAIN_SOUTH=.TRUE.
       ELSE
         RIV_IN_DOMAIN_SOUTH=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_RIV_SOUTH=I_START_RIV_SOUTH+Nghost
      Kend_RIV_SOUTH = I_START_RIV_SOUTH+Nghost+N_RIVER_SOUTH-1
      Kshift_RIV_SOUTH = -(Kstart_RIV_SOUTH-Nghost)+1
      RIV_IN_DOMAIN_SOUTH = .TRUE.
# endif
   ENDIF

! north boundary
   IF(N_RIVER_NORTH>0)THEN
# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
      Kstart_RIV_NORTH=I_START_RIV_NORTH+Nghost -npx*Mglob/px
      Kend_RIV_NORTH = I_START_RIV_NORTH+Nghost+N_RIVER_NORTH-1 -npx*Mglob/px
      IF((Kstart_RIV_NORTH>Nghost.AND.Kstart_RIV_NORTH<Mloc-Nghost+1).OR.&
     (Kend_RIV_NORTH<Mloc-Nghost+1.AND.Kend_RIV_NORTH>Nghost))THEN
       IF(Kstart_RIV_NORTH<Nghost+1)THEN
         Kshift_RIV_NORTH= -Kstart_RIV_NORTH+Nghost+1
         Kstart_RIV_NORTH=Nghost+1
       ELSE
         Kshift_RIV_NORTH=-(Kstart_RIV_NORTH-Nghost)+1
       ENDIF
       IF(Kend_RIV_NORTH>Mloc-Nghost)THEN
         Kend_RIV_NORTH=Mloc-Nghost
       ENDIF
       RIV_IN_DOMAIN_NORTH=.TRUE.
      ELSE

       IF(Kstart_RIV_NORTH<=Nghost.AND.Kend_RIV_NORTH>=Mloc-Nghost+1)THEN
         Kshift_RIV_NORTH=-Kstart_RIV_NORTH+Nghost+1
         Kstart_RIV_NORTH = Nghost+1
         Kend_RIV_NORTH = Mloc-Nghost
         RIV_IN_DOMAIN_NORTH=.TRUE.
       ELSE
         RIV_IN_DOMAIN_NORTH=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_RIV_NORTH=I_START_RIV_NORTH+Nghost
      Kend_RIV_NORTH = I_START_RIV_NORTH+Nghost+N_RIVER_NORTH-1
      Kshift_RIV_NORTH = -(Kstart_RIV_NORTH-Nghost)+1
      RIV_IN_DOMAIN_NORTH = .TRUE.
# endif
   ENDIF

! open masks for RIVER points
     IF(RIV_IN_DOMAIN_WEST)THEN
        DO J=Kstart_RIV_WEST,Kend_RIV_WEST
          MASKu(Ibeg,J)=1
        ENDDO
     ENDIF
     IF(RIV_IN_DOMAIN_EAST)THEN
        DO J=Kstart_RIV_EAST,Kend_RIV_EAST
          MASKu(Iend1,J)=1
        ENDDO
     ENDIF
     IF(RIV_IN_DOMAIN_SOUTH)THEN
        DO I=Kstart_RIV_SOUTH,Kend_RIV_SOUTH
          MASKv(I,Jbeg)=1
        ENDDO
     ENDIF
     IF(RIV_IN_DOMAIN_NORTH)THEN
        DO I=Kstart_RIV_NORTH,Kend_RIV_NORTH
          MASKv(I,Jend1)=1
        ENDDO
     ENDIF

END SUBROUTINE InitRiverFlux
! end initilize river



SUBROUTINE RIVER_FLUX_BC
     USE GLOBAL
     IMPLICIT NONE
119      FORMAT(5E16.6)  ! this is a fixed format for I/O
! bc given on the 1st grid

! determine time slot

    IF(TIME>TIME_RIVER_1.AND.TIME>TIME_RIVER_2) THEN
         TIME_RIVER_1=TIME_RIVER_2

         READ(13,*,END=120) TIME_RIVER_2
! east
         IF(N_RIVER_EAST.GT.0)THEN
             READ(13,*,END=120)   ! east

             FLUX_RIVER_EAST(:,1)=FLUX_RIVER_EAST(:,2)
             CSED_RIVER_EAST(:,1)=CSED_RIVER_EAST(:,2)

             READ(13,119,END=120)(FLUX_RIVER_EAST(I,2),I=1,N_RIVER_EAST)
             READ(13,119,END=120)(CSED_RIVER_EAST(I,2),I=1,N_RIVER_EAST)
         ELSE
             READ(13,*,END=120)   ! east
         ENDIF

! west
         IF(N_RIVER_WEST.GT.0)THEN
             READ(13,*,END=120)   ! WEST

             FLUX_RIVER_WEST(:,1)=FLUX_RIVER_WEST(:,2)
             CSED_RIVER_WEST(:,1)=CSED_RIVER_WEST(:,2)

             READ(13,119,END=120)(FLUX_RIVER_WEST(I,2),I=1,N_RIVER_WEST)
             READ(13,119,END=120)(CSED_RIVER_WEST(I,2),I=1,N_RIVER_WEST)
         ELSE
             READ(13,*,END=120)   ! WEST
         ENDIF

! south
         IF(N_RIVER_SOUTH.GT.0)THEN
             READ(13,*,END=120)   ! SOUTH

             FLUX_RIVER_SOUTH(:,1)=FLUX_RIVER_SOUTH(:,2)
             CSED_RIVER_SOUTH(:,1)=CSED_RIVER_SOUTH(:,2)

             READ(13,119,END=120)(FLUX_RIVER_SOUTH(I,2),I=1,N_RIVER_SOUTH)
             READ(13,119,END=120)(CSED_RIVER_SOUTH(I,2),I=1,N_RIVER_SOUTH)
         ELSE
             READ(13,*,END=120)   ! SOUTH
         ENDIF

! north
         IF(N_RIVER_NORTH.GT.0)THEN
             READ(13,*,END=120)   ! NORTH

             FLUX_RIVER_NORTH(:,1)=FLUX_RIVER_NORTH(:,2)
             CSED_RIVER_NORTH(:,1)=CSED_RIVER_NORTH(:,2)

             READ(13,119,END=120)(FLUX_RIVER_NORTH(I,2),I=1,N_RIVER_NORTH)
             READ(13,119,END=120)(CSED_RIVER_NORTH(I,2),I=1,N_RIVER_NORTH)
         ELSE
             READ(13,*,END=120)   ! NORTH
         ENDIF

    ENDIF  ! time>time_2 and time_1

120 CONTINUE

    tmp2=ZERO
    tmp1=ZERO

    IF(TIME>TIME_RIVER_1)THEN
      IF(TIME_RIVER_1.EQ.TIME_RIVER_2)THEN
        ! no more data
        tmp2=ZERO
        tmp1=ZERO
      ELSE
      tmp2=(TIME_RIVER_2-TIME) &
            /MAX(SMALL, ABS(TIME_RIVER_2-TIME_RIVER_1))
      tmp1=1.0_SP - tmp2;
      ENDIF  ! no more data?
    ENDIF ! time>time_1


! west boundary
   IF(N_RIVER_WEST>0)THEN
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif
     IF(RIV_IN_DOMAIN_WEST)THEN
       DO J=Kstart_RIV_WEST,Kend_RIV_WEST
       DO I=1,Nghost+1
         P(I,J)=FLUX_RIVER_WEST(J-Nghost+Kshift_RIV_WEST,2)*tmp1&
                +FLUX_RIVER_WEST(J-Nghost+Kshift_RIV_WEST,1)*tmp2
# if defined(SEDIMENT)
         CON(I,J)=CSED_RIVER_WEST(J-Nghost+Kshift_RIV_WEST,2)*tmp1&
                +CSED_RIVER_WEST(J-Nghost+Kshift_RIV_WEST,1)*tmp2
# endif
       ENDDO
       ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_RIVER_west>0

! east boundary
   IF(N_RIVER_EAST>0)THEN
# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
     IF(RIV_IN_DOMAIN_EAST)THEN
       DO J=Kstart_RIV_EAST,Kend_RIV_EAST
       DO I=Iend,Iend+Nghost
         P(I,J)=FLUX_RIVER_EAST(J-Nghost+Kshift_RIV_EAST,2)*tmp1&
                +FLUX_RIVER_EAST(J-Nghost+Kshift_RIV_EAST,1)*tmp2
# if defined(SEDIMENT)
         CON(I,J)=CSED_RIVER_EAST(J-Nghost+Kshift_RIV_EAST,2)*tmp1&
                +CSED_RIVER_EAST(J-Nghost+Kshift_RIV_EAST,1)*tmp2
# endif
       ENDDO
       ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_RIVER_east>0

! south boundary
   IF(N_RIVER_SOUTH>0)THEN
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
     IF(RIV_IN_DOMAIN_SOUTH)THEN
       DO I=Kstart_RIV_SOUTH,Kend_RIV_SOUTH
       DO J=1,Nghost+1
         Q(I,J)=FLUX_RIVER_SOUTH(I-Nghost+Kshift_RIV_SOUTH,2)*tmp1&
                +FLUX_RIVER_SOUTH(I-Nghost+Kshift_RIV_SOUTH,1)*tmp2
# if defined(SEDIMENT)
         CON(I,J)=CSED_RIVER_SOUTH(I-Nghost+Kshift_RIV_SOUTH,2)*tmp1&
                +CSED_RIVER_SOUTH(I-Nghost+Kshift_RIV_SOUTH,1)*tmp2
# endif
       ENDDO
       ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_RIVER_south>0

! north boundary
   IF(N_RIVER_NORTH>0)THEN
# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
     IF(RIV_IN_DOMAIN_NORTH)THEN
       DO I=Kstart_RIV_NORTH,Kend_RIV_NORTH
       DO J=Jend+1,Jend+Nghost
         Q(I,J)=FLUX_RIVER_NORTH(I-Nghost+Kshift_RIV_NORTH,2)*tmp1&
                +FLUX_RIVER_NORTH(I-Nghost+Kshift_RIV_NORTH,1)*tmp2
# if defined(SEDIMENT)
         CON(I,J)=CSED_RIVER_NORTH(I-Nghost+Kshift_RIV_NORTH,2)*tmp1&
                +CSED_RIVER_NORTH(I-Nghost+Kshift_RIV_NORTH,1)*tmp2
# endif
       ENDDO
       ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_RIVER_north>0


END SUBROUTINE RIVER_FLUX_BC
