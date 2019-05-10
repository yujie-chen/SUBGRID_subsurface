# if defined(SUBGRID)
  SUBROUTINE POLY_FITTING(X, Y, A, N, M, DT1, DT2, DT3)

! USE PARAM, ONLY :: SP
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N, M
  REAL(4),DIMENSION(N), INTENT(IN) :: X, Y
  REAL(4),DIMENSION(M), INTENT(OUT) :: A
  REAL(4), INTENT(OUT) :: DT1, DT2, DT3

  REAL(4),DIMENSION(:), ALLOCATABLE :: S, T, B
  REAL(4) :: Z, D1, P, C, D2, G, Q, DT
  INTEGER :: I,J,K

  ALLOCATE(S(M),T(M),B(M))
  A = 0.0

  B(1) = 1.0
  D1  = real(N)

! mean value of X
  DO I = 1, N
    Z = Z + X(I)/D1
  ENDDO

! P: mean of (X_i-X_mean)
! C: mean of Y
  P = 0.0
  C = 0.0
  DO I = 1, N
    P = P + (X(I) - Z)
    C = C + Y(I)
  ENDDO

  C = C/D1
  P = P/D1

  A(1) = C*B(1)
  IF(M.GT.1)THEN
    T(2) = 1.0
    T(1) = -P
    D2 = 0.0
    C = 0.0
    G = 0.0
    DO I=1, N
      Q = X(I)-Z-P
      D2= D2+Q*Q
      C = Y(I)*Q+C
      G = (X(I)-Z)*Q*Q+G
    ENDDO
!   IF(D2==0.0)WRITE(*,*)'D2=ZERO! Divided by ZERO!'
!   IF(D1==0.0)WRITE(*,*)'D1=ZERO! Divided by ZERO!'

    C = C/D2
    P = G/D2
    Q = D2/D1
    D1 = D2
    A(2) = C* T(2)
    A(1) = C* T(1) + A(1)
  ENDIF

  DO J = 3, M
    S(J) = T(J-1)
    S(J-1) = -P*T(J-1) + T(J-2)
    IF (J.GE.4) THEN
      DO K = J-2,2,-1
         S(K) = -P*T(K) + T(K-1) - Q*B(K)
      ENDDO
    ENDIF
    S(1) = -P*T(1) - Q*B(1)
    D2 = 0.0
    C = 0.0
    G = 0.0

    DO I = 1, N
      Q = S(J)
      DO K = J-1, 1, -1
         Q = Q*(X(I)-Z) + S(K)
      ENDDO
      D2 = D2+Q*Q
      C = Y(I)*Q+C
      G = (X(I)-Z)*Q*Q+G
    ENDDO

    IF(D2==0.0)WRITE(*,*)'D2=ZERO! Divided by ZERO!!'
    IF(D1==0.0)WRITE(*,*)'D1=ZERO! Divided by ZERO!!'
    C = C/D2
    P = G/D2
    Q = D2/D1
    D1 = D2
    A(J) = C*S(J)
    T(J) = S(J)

    DO K = J-1, 1, -1
      A(K) = C*S(K) + A(K)
      B(K) = T(K)
      T(K) = S(K)
    ENDDO

  ENDDO

  DT1 = 0.0
  DT2 = 0.0
  DT3 = 0.0
  DO I = 1, N
    Q = A(M)
    DO K = M-1,1,-1
      Q = Q*(X(I)-Z) + A(K)
    ENDDO
    DT = Q - Y(I)
    IF (ABS(DT).GT.DT3) DT3 = ABS(DT)
    DT1 = DT1 + DT*DT
    DT2 = DT2 + ABS(DT)
  ENDDO
  DT1=DT1/N
  DT2=DT2/N

!  WRITE(*,*)'Polynomial Fitting Finished!'

  END SUBROUTINE POLY_FITTING

# endif
