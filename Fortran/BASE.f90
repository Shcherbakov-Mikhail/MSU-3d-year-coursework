MODULE BASE ! БАЗОВЫЙ МОДУЛЬ КОНЕЧНО- И ГРАНИЧНО-ЭЛЕМЕНТНЫХ ПАКЕТОВ

!**********************************************************************
IMPLICIT NONE
!**********************************************************************
CHARACTER (LEN=*), PARAMETER, PRIVATE :: CHMDL="BASE"
!**********************************************************************

CHARACTER (LEN=*), PARAMETER, PUBLIC  :: CHMSG0=" PAБOTAET ПОДПРОГРАММА "

CHARACTER (LEN=*), PARAMETER, PUBLIC  :: CHERR0=" *** BHИMAHИE. PAБOTAET ПОДПРОГРАММА "
CHARACTER (LEN=*), PARAMETER, PUBLIC  :: CHERR2=" ОБНАРУЖЕНА ОШИБКА"
CHARACTER (LEN=*), PARAMETER, PUBLIC  :: CHERR3=" BЫПOЛHEHИE ПPOГPAMMЫ ПPEPBAHO"

!**********************************************************************

REAL,    ALLOCATABLE, PROTECTED :: CPUTIME(:) ! МАССИВ МОМЕНТОВ ВРЕМЕНИ

INTEGER,              PRIVATE   :: NTIME=10   ! KOЛИЧECTBO МОМЕНТОВ ВРЕМЕНИ
INTEGER,              PRIVATE   :: LTIME=0    ! СЧЕТЧИК    МОМЕНТОВ ВРЕМЕНИ
INTEGER,              PRIVATE   :: IER  =0    ! КОД ОШИБКИ ПРИ РАЗМЕЩЕНИИ МАССИВА
INTEGER,              PRIVATE   :: IW   =0    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

!**********************************************************************

                 ! ПОДПРОГРАММЫ
PUBLIC  CPUTIME0 ! РАЗМЕЩЕНИЕ МАССИВА МОМЕНТОВ ВРЕМЕНИ
                 ! ФИКСАЦИЯ НАЧАЛЬНОГО МОМЕНТА ВРЕМЕНИ
PUBLIC  CPUTIME1 ! ФИКСАЦИЯ ТЕКУЩЕГО   МОМЕНТА ВРЕМЕНИ
PUBLIC  WCPUTIME ! ПЕЧАТЬ ПРОЦЕССОРНОГО ВРЕМЕНИ ВЫПОЛНЕНИЯ ФРАГМЕНТОВ ПРОГРАММЫ

!**********************************************************************
CONTAINS
!**********************************************************************

SUBROUTINE CPUTIME0 (KTIME, & ! KOЛИЧECTBO МОМЕНТОВ ВРЕМЕНИ
                     KW)      ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! РАЗМЕЩЕНИЕ МАССИВА МОМЕНТОВ ВРЕМЕНИ
! ФИКСАЦИЯ НАЧАЛЬНОГО МОМЕНТА ВРЕМЕНИ

INTEGER, INTENT (IN), OPTIONAL :: KTIME ! KOЛИЧECTBO МОМЕНТОВ ВРЕМЕНИ
INTEGER, INTENT (IN), OPTIONAL :: KW    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="CPUTIME0"
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR2A=" ОШИБКА ПРИ РАЗМЕЩЕНИИ МАССИВА "
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL

IF ( PRESENT(KTIME) ) THEN ! ТИП KOЛИЧECTBO МОМЕНТОВ ВРЕМЕНИ
   IF ( KTIME > NTIME ) NTIME=KTIME ! УВЕЛИЧЕНИЕ
END IF

IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
   IW=KW
END IF

ALLOCATE (CPUTIME(0:NTIME), STAT=IER) ! МАССИВ МОМЕНТОВ ВРЕМЕНИ

IF ( IER /= 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКЕ
   WRITE (3,'(/A/2A,I0/A)') CHERR1, CHERR2A, "CPUTIME - IER=", IER, CHERR3
   STOP
END IF

CALL CPU_TIME(CPUTIME(LTIME)) ! ФИКСАЦИЯ МОМЕНТА ВРЕМЕНИ

! ПЕЧАТЬ СООБЩЕНИЯ О ФИКСАЦИИ МОМЕНТА ВРЕМЕНИ
IF ( IW == 1 ) THEN
   WRITE (3,'(/A,A,I0)') CHMSG1, ". ФИКСАЦИЯ МОМЕНТА ВРЕМЕНИ LTIME=", LTIME
END IF

END SUBROUTINE CPUTIME0

!**********************************************************************

SUBROUTINE CPUTIME1 (KW) ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

INTEGER, INTENT (IN), OPTIONAL :: KW ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="CPUTIME1"
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL

IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
   IW=KW
END IF

LTIME=LTIME+1 ! СЧЕТЧИК МОМЕНТОВ ВРЕМЕНИ

IF ( LTIME > NTIME ) THEN
   WRITE (3,'(/A/A/A/A,I0/A)') CHERR1, CHERR2,                      &
   " НЕОБХОДИМО УВЕЛИЧИТЬ КОЛИЧЕСТВО ФИКСИРУЕМЫХ МОМЕНТОВ ВРЕМЕНИ", &
   " СЕЙЧАС ОНО РАВНО NTIME=", NTIME, CHERR3
   STOP
END IF

CALL CPU_TIME(CPUTIME(LTIME)) ! ФИКСАЦИЯ МОМЕНТА  ВРЕМЕНИ

! ПЕЧАТЬ СООБЩЕНИЯ О ФИКСАЦИИ МОМЕНТА ВРЕМЕНИ
IF ( IW == 1 ) THEN
   WRITE (3,'(/A,A,I0)') CHMSG1, ". ФИКСАЦИЯ МОМЕНТА ВРЕМЕНИ LTIME=", LTIME
END IF

END SUBROUTINE CPUTIME1

!**********************************************************************

SUBROUTINE WCPUTIME ! ПЕЧАТЬ ПРОЦЕССОРНОГО ВРЕМЕНИ ВЫПОЛНЕНИЯ ФРАГМЕНТОВ ПРОГРАММЫ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="WCPUTIME"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL

INTEGER :: KTIME ! НОМЕР МОМЕНТА ВРЕМЕНИ

WRITE (3,'(/A)') CHMSG1

DO KTIME=1,LTIME
   WRITE (3,'(1X,I3,2(A,E12.6))') KTIME, "-TIME=", (CPUTIME(KTIME)-CPUTIME(KTIME-1)), "   BTIME=", (CPUTIME(KTIME)-CPUTIME(0))
END DO

END SUBROUTINE WCPUTIME

!**********************************************************************

END MODULE BASE
