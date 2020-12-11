MODULE FEM1 ! БАЗОВЫЙ МОДУЛЬ МКЭ В ПЕРЕМЕЩЕНИЯХ
            ! РАЗМЕЩЕНИЕ МАССИВОВ
            ! ПЕРЕМЕЩЕНИЙ, НАГРУЗКИ И ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
            ! BЫЧИCЛEHИE МАССИВА ХАРАКТЕРИСТИК СПЛОШНОЙ СРЕДЫ

!**********************************************************************

        ! МОДУЛИ
USE MEM ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ И ВЫЧИСЛЕНИЕ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ

!**********************************************************************
IMPLICIT NONE
!**********************************************************************
CHARACTER (LEN=*), PARAMETER, PRIVATE :: CHMDL="FEM1"
!**********************************************************************

INTEGER, PRIVATE   :: IW  =1 ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ МОДУЛЯ

!**********************************************************************

                                         ! МАССИВЫ
REAL,    ALLOCATABLE, PUBLIC :: PER(:)   ! УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ
REAL,    ALLOCATABLE, PUBLIC :: RRR(:)   ! УЗЛОВОЙ НАГРУЗКИ

INTEGER, ALLOCATABLE, PUBLIC :: MBP(:)   ! НОМЕРОВ СТЕПЕНЕЙ СВОБОДЫ ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
REAL,    ALLOCATABLE, PUBLIC :: FBP(:)   ! ЗАДАННЫХ ПЕРЕМЕЩЕНИЙ
REAL,    ALLOCATABLE, PUBLIC :: RBP(:)   ! РЕАКЦИЙ В ЗАКРЕПЛЕННЫХ УЗЛАХ

!**********************************************************************

               ! ПОДПРОГРАММЫ
PUBLIC  MFE1PR ! РАЗМЕЩЕНИЕ МАССИВОВ PER, RRR
PUBLIC  MFE1BP ! РАЗМЕЩЕНИЕ МАССИВОВ MBP, FBP, RBP

PUBLIC  GMAT1  ! BЫЧИCЛEHИE МАССИВА ХАРАКТЕРИСТИК СПЛОШНОЙ СРЕДЫ
PUBLIC  WMAT1  ! ПЕЧАТЬ МАССИВА ПАРАМЕТРОВ СРЕДЫ ДЛЯ ОБЛАСТИ

!**********************************************************************
CONTAINS
!**********************************************************************

SUBROUTINE MFE1PR (NNSD, & ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ
                   KW)     ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! РАЗМЕЩЕНИЕ МАССИВОВ PER, RRR

INTEGER, INTENT (IN)           :: NNSD ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ
INTEGER, INTENT (IN), OPTIONAL :: KW   ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="MFE1PR"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

CALL APRM ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ

                                      ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ МАССИВОВ
CALL AMFLT1 ("PER", PER, NNSD, IRL=1) ! УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ
CALL AMFLT1 ("RRR", RRR, NNSD, IRL=1) ! УЗЛОВОЙ НАГРУЗКИ

IF ( IW == 1 ) THEN
   WRITE (3,'(/A/A/A,I0)') CHMSG1, &
   " РАЗМЕЩЕНЫ МАССИВЫ PER, RRR",  &
   " NNSD=", NNSD
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКАХ

   IF ( NNSD < 2 ) THEN ! КОЛИЧЕСТВО УЗЛОВЫХ НАПРЯЖЕНИЙ
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2, &
      " НЕПРАВИЛЬНО УКАЗАН ПАРАМЕТР NNSD=", NNSD, &
      " ОН ДОЛЖЕН БЫТЬ БОЛЬШЕ 1", CHERR3
      STOP
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   END IF

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE MFE1PR

!**********************************************************************

SUBROUTINE MFE1BP (NBP, & ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
                   KW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! РАЗМЕЩЕНИЕ МАССИВОВ MBP, FBP, RBP

INTEGER, INTENT (IN)           :: NBP ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
INTEGER, INTENT (IN), OPTIONAL :: KW  ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="MFE1BP"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

                ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: NNSD ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ
INTEGER :: IW   ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

CALL APRM     ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ

                                     ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ МАССИВОВ
CALL AMINT1 ("MBP", MBP, NBP, IRL=1) ! НОМЕРОВ СТЕПЕНЕЙ СВОБОДЫ ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
CALL AMFLT1 ("FBP", FBP, NBP, IRL=1) ! ЗАДАННЫХ ПЕРЕМЕЩЕНИЙ
CALL AMFLT1 ("RBP", RBP, NBP, IRL=1) ! РЕАКЦИЙ В ЗАКРЕПЛЕННЫХ УЗЛАХ

IF ( IW == 1 ) THEN
   WRITE (3,'(/A/A/A,I0)') CHMSG1,     &
   " РАЗМЕЩЕНЫ МАССИВЫ MBP, FBP, RBP", &
   " NBP=", NBP
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКАХ

   IF ( ALLOCATED(PER) ) THEN
      NNSD=SIZE(PER) ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ
   ELSE
      WRITE (3,'(/A/A/A/A)') CHERR1, CHERR2,           &
      " МАССИВ PER ДОЛЖЕН БЫТЬ ЗАРАНЕЕ РАЗМЕЩЕН", CHERR3
      STOP
   END IF

   IF ( ( NBP < 1 ) .OR. ( NBP > NNSD ) ) THEN ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
      WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,    &
      " НЕПРАВИЛЬНО УКАЗАН ПАРАМЕТР NBP=", NBP,         &
      " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 1-NNSD=", NNSD, CHERR3
      STOP
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   END IF

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE MFE1BP

!**********************************************************************
!**********************************************************************

SUBROUTINE GMAT1 (EMATI, & ! МАССИВ ПАРАМЕТРОВ СРЕДЫ (ДЛЯ ОБЛАСТИ)
                  EMTR,  & ! МАССИВ ПАРАМЕТРОВ СРЕДЫ (БАЗОВЫЙ)
                  MNE,   & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                  CRD,   & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                  FFL,   & ! МАССИВ ФУНКЦИЙ ФОРМЫ В УЗЛАХ ИНТЕГРИРОВАНИЯ
                  FMTR,  & ! ПОДПРОГРАММА ДЛЯ ВЫЧИСЛЕНИЯ ПАРАМЕТРОВ НЕОДНОРОДНОЙ СРЕДЫ В УЗЛАХ ИНТЕГРИРОВАНИЯ
                  CQU,   & ! МАССИВ КООРДИНАТ УЗЛОВ ИНТЕГРИРОВАНИЯ
                  SMM,   & ! МАССИВ ПАРАМЕТРОВ ДЛЯ ВЫЧИСЛЕНИЯ ФУНКЦИЙ РАСПРЕДЕЛЕНИЯ
                  KW)      ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! BЫЧИCЛEHИE МАССИВА ПАРАМЕТРОВ СРЕДЫ ДЛЯ ОБЛАСТИ

! ДЛЯ БАЗОВЫЕ ПАРАМЕТРЫ ЗАДАЮТСЯ В МАССИВЕ EMTR
! ФУНКЦИИ РАСПРЕДЕЛЕНИЯ ПАРАМЕТРОВ ДЛЯ НЕОДНОРОДНОЙ СРЕДЫ
! ЗАДАЮТСЯ AHAЛИTИЧECKИ B ПОДПРОГРАММЕ FMAT

INTERFACE ! ВНЕШНИЕ ПОДПРОГРАММЫ

   ! ПОДПРОГРАММА ДЛЯ ВЫЧИСЛЕНИЯ ПАРАМЕТРОВ НЕОДНОРОДНОЙ СРЕДЫ В УЗЛАХ ИНТЕГРИРОВАНИЯ
   SUBROUTINE FMTR (XYZG, & ! МАССИВ ГЛОБАЛЬНЫХ КООРДИНАТ ТОЧКИ
                    FXYZ, & ! МАССИВ ФУНКЦИЙ РАСПРЕДЕЛЕНИЯ ПАРАМЕТРОВ В ТОЧКЕ
                    SMM)    ! МАССИВ ПАРАМЕТРОВ ДЛЯ ВЫЧИСЛЕНИЯ УПРУГИХ ПОСТОЯННЫХ

      REAL, INTENT (IN)           :: XYZG(:) ! МАССИВ ГЛОБАЛЬНЫХ КООРДИНАТ ТОЧКИ
      REAL, INTENT (OUT)          :: FXYZ(:) ! МАССИВ ФУНКЦИЙ РАСПРЕДЕЛЕНИЯ ПАРАМЕТРОВ В ТОЧКЕ
      REAL, INTENT (IN), OPTIONAL :: SMM (:) ! МАССИВ ПАРАМЕТРОВ ДЛЯ ВЫЧИСЛЕНИЯ УПРУГИХ ПОСТОЯННЫХ

   END SUBROUTINE FMTR

END INTERFACE

REAL,    INTENT (OUT), ALLOCATABLE           :: EMATI(:,:,:) ! МАССИВ ПАРАМЕТРОВ СРЕДЫ (ДЛЯ ОБЛАСТИ)
REAL,    INTENT (IN)                         :: EMTR (:)     ! МАССИВ ПАРАМЕТРОВ СРЕДЫ (БАЗОВЫЙ)
INTEGER, INTENT (IN)                         :: MNE  (:,:)   ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
REAL,    INTENT (IN)                         :: CRD  (:,:)   ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
REAL,    INTENT (IN)                         :: FFL  (:,:)   ! МАССИВ ФУНКЦИЙ ФОРМЫ В УЗЛАХ ИНТЕГРИРОВАНИЯ
                                    OPTIONAL :: FMTR         ! ПОДПРОГРАММА ДЛЯ ВЫЧИСЛЕНИЯ ПАРАМЕТРОВ НЕОДНОРОДНОЙ СРЕДЫ В УЗЛАХ ИНТЕГРИРОВАНИЯ
REAL,    INTENT (OUT), ALLOCATABLE, OPTIONAL :: CQU  (:,:,:) ! МАССИВ КООРДИНАТ УЗЛОВ ИНТЕГРИРОВАНИЯ
REAL,    INTENT (IN),               OPTIONAL :: SMM  (:)     ! МАССИВ ПАРАМЕТРОВ ДЛЯ ВЫЧИСЛЕНИЯ ФУНКЦИЙ РАСПРЕДЕЛЕНИЯ
INTEGER, INTENT (IN),               OPTIONAL :: KW           ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="GMAT1"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

                               ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
REAL, ALLOCATABLE :: XYZG(:)   ! МАССИВ ГЛOБAЛЬHЫХ KOOPДИHAT УЗЛА ИНТЕГРИРОВАНИЯ
REAL, ALLOCATABLE :: FXYZ(:)   ! МАССИВ ФУНКЦИЙ РАСПРЕДЕЛЕНИЯ ПАРАМЕТРОВ СРЕДЫ В УЗЛЕ ИНТЕГРИРОВАНИЯ
REAL, ALLOCATABLE :: CRDE(:,:) ! МАССИВ КООРДИНАТ УЗЛОВ КЭ

INTEGER           :: NCF       ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
INTEGER           :: NCN       ! КОЛИЧЕСТВО УЗЛОВ КЭ
INTEGER           :: NMAT      ! КОЛИЧЕСТВО РАЗЛИЧНЫХ НАБОРОВ ПАРАМЕТРОВ СПЛОШНОЙ СРЕДЫ В УЗЛАХ ИНТЕГРИРОВАНИЯ (NMAT=1 ИЛИ NMAT=NE)
INTEGER           :: NPI       ! КОЛИЧЕСТВО УЗЛОВ ИНТЕГРИРОВАНИЯ КЭ
INTEGER           :: NRMAT     ! КОЛИЧЕСТВО ПАРАМЕТРОВ СПЛОШНОЙ СРЕДЫ
INTEGER           :: IW        ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

INTEGER           :: ICUQ      ! ФЛАГ МАССИВА CUQ

INTEGER           :: LE        ! СЧЕТЧИК ЦИКЛА: NE
INTEGER           :: LPI       ! СЧЕТЧИК ЦИКЛА: NPI

CALL APRM                             ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ

IF ( NMAT == 1 ) THEN                 ! ОДНОРОДНАЯ СРЕДА
   EMATI(:,1,1)=EMTR                  ! МАССИВ ПАРАМЕТРОВ СРЕДЫ ДЛЯ ОДНОРОДНОЙ ОБЛАСТИ
ELSE                                  ! НЕОДНОРОДНАЯ СРЕДА

   DO LE=1,NMAT                       ! ЦИKЛ ПO ВСЕМ ЭЛEMEHTAМ

      CRDE=CRD(:,MNE(:,LE))           ! ФОРМИРОВАНИЕ МАССИВА КООРДИНАТ УЗЛОВ КЭ

      DO LPI=1,NPI                    ! ЦИKЛ ПO ВСЕМ УЗЛАМ ИHTEГPИPOBAHИЯ

         XYZG=MATMUL(CRDE,FFL(:,LPI)) ! KOOPДИHATЫ TOЧKИ ИHTEГPИPOBAHИЯ

         ! BЫЧИCЛEHИE ФУНКЦИЙ РАСПРЕДЕЛЕНИЯ ПАРАМЕТРОВ СРЕДЫ В УЗЛЕ ИНТЕГРИРОВАНИЯ
         CALL FMTR (XYZG,  & ! МАССИВ ГЛОБАЛЬНЫХ КООРДИНАТ ТОЧКИ
                    FXYZ,  & ! МАССИВ ФУНКЦИЙ РАСПРЕДЕЛЕНИЯ ПАРАМЕТРОВ В ТОЧКЕ
                    SMM=SMM) ! МАССИВ ПАРАМЕТРОВ ДЛЯ ВЫЧИСЛЕНИЯ ФУНКЦИЙ РАСПРЕДЕЛЕНИЯ

         EMATI(:,LPI,LE)=EMTR*FXYZ ! МАССИВ ПАРАМЕТРОВ СРЕДЫ В УЗЛЕ ИНТЕГРИРОВАНИЯ

         IF ( ICUQ == 1 ) THEN ! МАССИВ КООРДИНАТ УЗЛОВ ИНТЕГРИРОВАНИЯ
            CQU(:,LPI,LE)=XYZG
         END IF

      END DO
   END DO

END IF

! ОСВОБОЖДЕНИЕ ПАМЯТИ ЛОКАЛЬНЫХ МАССИВОВ
CALL DAMSV("CRDE", AMSV2=CRDE) ! КООРДИНАТ УЗЛОВ КЭ
CALL DAMSV("FXYZ", AMSV1=FXYZ) ! ФУНКЦИЙ РАСПРЕДЕЛЕНИЯ ПАРАМЕТРОВ СРЕДЫ В УЗЛЕ ИНТЕГРИРОВАНИЯ
CALL DAMSV("XYZG", AMSV1=XYZG) ! ГЛOБAЛЬHЫХ KOOPДИHAT УЗЛА ИНТЕГРИРОВАНИЯ

IF ( IW == 1 ) THEN
   IF ( NMAT == 1 ) THEN ! ОДНОРОДНАЯ СРЕДА
      WRITE (3,'(/A/A/A,I0)') CHMSG1,                  &
      " ВЫЧИСЛЕН МАССИВ ПАРАМЕТРОВ ОДНОРОДНОЙ СРЕДЫ",  &
      " NRMAT=", NRMAT
   ELSE  ! НЕОДНОРОДНАЯ СРЕДА
      WRITE (3,'(/A/A/3(A,I0))') CHMSG1,                                       &
      " ВЫЧИСЛЕН МАССИВ ПАРАМЕТРОВ НЕОДНОРОДНОЙ СРЕДЫ В УЗЛАХ ИНТЕГРИРОВАНИЯ", &
      " NRMAT=", NRMAT, "   NPI=", NPI, "   NMAT=", NMAT
   END IF
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКАХ

   NRMAT=SIZE(EMTR)          ! КОЛИЧЕСТВО ПАРАМЕТРОВ СПЛОШНОЙ СРЕДЫ

   IF ( PRESENT(FMTR) ) THEN ! ПОДПРОГРАММА ДЛЯ ВЫЧИСЛЕНИЯ ПАРАМЕТРОВ НЕОДНОРОДНОЙ СРЕДЫ В УЗЛАХ ИНТЕГРИРОВАНИЯ
      NMAT=SIZE(MNE,2)       ! КОЛИЧЕСТВО РАЗЛИЧНЫХ НАБОРОВ ПАРАМЕТРОВ ДЛЯ ОБЛАСТИ (NMAT=1 ИЛИ NMAT=NE)
      NPI =SIZE(FFL,2)       ! КОЛИЧЕСТВО УЗЛОВ ИНТЕГРИРОВАНИЯ КЭ
   ELSE
      NMAT=1                 ! КОЛИЧЕСТВО РАЗЛИЧНЫХ НАБОРОВ ПАРАМЕТРОВ ДЛЯ ОБЛАСТИ (NMAT=1 ИЛИ NMAT=NE)
      NPI =1                 ! КОЛИЧЕСТВО УЗЛОВ ИНТЕГРИРОВАНИЯ КЭ
   END IF

   ! РАЗМЕЩЕНИЕ МАССИВОВ
   CALL AMFLT3 ("EMATI", EMATI, NRMAT, NPI, NMAT, IRL=1) ! ПАРАМЕТРОВ СРЕДЫ ДЛЯ ОБЛАСТИ

   IF ( NMAT > 1 ) THEN ! НЕОДНОРОДНАЯ СРЕДА
      NCF=SIZE(CRD,1)   ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
      NCN=SIZE(MNE,1)   ! КОЛИЧЕСТВО УЗЛОВ КЭ

      IF ( SIZE(FFL,1) /= NCN ) THEN ! КОЛИЧЕСТВО УЗЛОВ КЭ
         WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2, & ! НЕСООТВЕТСТВИЕ РАЗМЕРНОСТЕЙ
         " SIZE(MNE,1)=", SIZE(MNE,1),                  & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
         " SIZE(FFL,1)=", SIZE(FFL,1), CHERR3             ! МАССИВ ФУНКЦИЙ ФОРМЫ В УЗЛАХ ИНТЕГРИРОВАНИЯ
         STOP
      END IF

      IF ( PRESENT(CQU) ) THEN                           ! МАССИВ КООРДИНАТ УЗЛОВ ИНТЕГРИРОВАНИЯ
         ICUQ=1                                          ! ФЛАГ МАССИВА
         CALL AMFLT3 ("CQU", CQU, NCF, NPI, NMAT, IRL=1) ! РАЗМЕЩЕНИЕ МАССИВА
      ELSE                                               ! МАССИВ НЕ ЗАДАН
         ICUQ=0                                          ! ФЛАГ МАССИВА
      END IF

      CALL AMFLT1 ("XYZG", XYZG, NCF,        IRL=1) ! ГЛOБAЛЬHЫХ KOOPДИHAT УЗЛА ИНТЕГРИРОВАНИЯ
      CALL AMFLT1 ("FXYZ", FXYZ, NRMAT,      IRL=1) ! ФУНКЦИЙ РАСПРЕДЕЛЕНИЯ ПАРАМЕТРОВ СРЕДЫ В УЗЛЕ ИНТЕГРИРОВАНИЯ
      CALL AMFLT2 ("CRDE", CRDE, NCF,   NCN, IRL=1) ! КООРДИНАТ УЗЛОВ КЭ
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   ELSE
      IW=1 ! ПО УМОЛЧАНИЮ ПЕЧАТЬ ПОДПРОГРАММЫ ВКЛЮЧЕНА
   END IF


   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE GMAT1

!**********************************************************************
!**********************************************************************

SUBROUTINE WMAT1 (EMATI, & ! МАССИВ ПАРАМЕТРОВ СРЕДЫ ДЛЯ ОБЛАСТИ
                  CQU,   & ! МАССИВ КООРДИНАТ УЗЛОВ ИНТЕГРИРОВАНИЯ
                  KE1,   & ! НОМЕР  НАЧАЛЬНОГО КЭ ДЛЯ ПЕЧАТИ
                  KE2,   & ! НОМЕР  КОНЕЧНОГО  КЭ ДЛЯ ПЕЧАТИ
                  KANAL)   ! НОМЕР  КАНАЛА ДЛЯ ВЫВОДА В ФАЙЛ

! PUBLIC
! ПЕЧАТЬ МАССИВА ПАРАМЕТРОВ СРЕДЫ ДЛЯ ОБЛАСТИ

REAL,    INTENT (IN)           :: EMATI(:,:,:) ! МАССИВ ПАРАМЕТРОВ СРЕДЫ ДЛЯ ОБЛАСТИ
REAL,    INTENT (IN), OPTIONAL :: CQU  (:,:,:) ! МАССИВ КООРДИНАТ УЗЛОВ ИНТЕГРИРОВАНИЯ
INTEGER, INTENT (IN), OPTIONAL :: KE1          ! НОМЕР  НАЧАЛЬНОГО КЭ ДЛЯ ПЕЧАТИ
INTEGER, INTENT (IN), OPTIONAL :: KE2          ! НОМЕР  КОНЕЧНОГО  КЭ ДЛЯ ПЕЧАТИ
INTEGER, INTENT (IN), OPTIONAL :: KANAL        ! НОМЕР  КАНАЛА ДЛЯ ВЫВОДА В ФАЙЛ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="WMAT1"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

                ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: LE1  ! НОМЕР НАЧАЛЬНОГО КЭ ДЛЯ ПЕЧАТИ
INTEGER :: LE2  ! НОМЕР КОНЕЧНОГО  КЭ ДЛЯ ПЕЧАТИ
INTEGER :: LE   ! СЧЕТЧИК ЦИКЛА: NE
INTEGER :: LPI  ! СЧЕТЧИК ЦИКЛА: NPI
INTEGER :: ICUQ ! ФЛАГ МАССИВА CUQ
INTEGER :: KNL  ! НОМЕР КАНАЛА ДЛЯ ВЫВОДА В ФАЙЛ

IF ( SIZE(EMATI,3) > 1 ) THEN ! НЕОДНОРОДНАЯ ОБЛАСТЬ

   CALL APRM ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ

   IF ( KNL == 3 ) THEN
      WRITE (3,'(/7X,A/7X,A)') CHMSG1, " КЭ/У.И   ПАРАМЕТРЫ СРЕДЫ:"
      DO LE=LE1,LE2             ! ЦИKЛ ПO КЭ
         DO LPI=1,SIZE(EMATI,2) ! ЦИKЛ ПO ВСЕМ УЗЛАМ ИHTEГPИPOBAHИЯ
            WRITE (3,'(I10,"/",I3,10(E15.6$))') LE, LPI, EMATI(:,LPI,LE)
            IF ( ICUQ == 1 ) THEN
               WRITE (3,'(A,3E15.6)') "   КООРДИНАТЫ:", CQU(:,LPI,LE)
            ELSE
               WRITE (3,'(1X)')
            END IF
         END DO
      END DO
   ELSE
      DO LE=LE1,LE2             ! ЦИKЛ ПO КЭ
         DO LPI=1,SIZE(EMATI,2) ! ЦИKЛ ПO ВСЕМ УЗЛАМ ИHTEГPИPOBAHИЯ
            WRITE (KNL,'(99E15.6)') EMATI(:,LPI,LE)
         END DO
      END DO
   END IF

ELSE ! ОДНОРОДНАЯ ОБЛАСТЬ

   WRITE (3,'(/A/A$)') CHMSG1, " ПАРАМЕТРЫ СРЕДЫ:"
   WRITE (3,'(5E15.6,(/17X,5E15.6))') EMATI(:,1,1)

END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   IF ( SIZE(EMATI,3) == 1 ) RETURN ! ОДНОРОДНАЯ ОБЛАСТЬ

   IF ( PRESENT(CQU) ) THEN                               ! МАССИВ КООРДИНАТ УЗЛОВ ИНТЕГРИРОВАНИЯ
      ICUQ=1                                              ! ФЛАГ МАССИВА
      IF ( SIZE(EMATI,2) /= SIZE(CQU,2) ) THEN            ! КОЛИЧЕСТВО УЗЛОВ ИНТЕГРИРОВАНИЯ
         WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2, & ! НЕСООТВЕТСТВИЕ РАЗМЕРНОСТЕЙ
         " SIZE(EMATI,2)=", SIZE(EMATI,2),              & ! МАССИВ ПАРАМЕТРОВ СРЕДЫ ДЛЯ ОБЛАСТИ
         " SIZE(CQU,  2)=", SIZE(CQU,  2), CHERR3         ! МАССИВ КООРДИНАТ УЗЛОВ ИНТЕГРИРОВАНИЯ
         STOP
      END IF
      IF ( SIZE(EMATI,3) /= SIZE(CQU,3) ) THEN            ! КОЛИЧЕСТВО УЗЛОВ КЭ
         WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2, & ! НЕСООТВЕТСТВИЕ РАЗМЕРНОСТЕЙ
         " SIZE(EMATI,3)=", SIZE(EMATI,3),              & ! МАССИВ ПАРАМЕТРОВ СРЕДЫ ДЛЯ ОБЛАСТИ
         " SIZE(CQU,  3)=", SIZE(CQU,  3), CHERR3         ! МАССИВ КООРДИНАТ УЗЛОВ ИНТЕГРИРОВАНИЯ
         STOP
      END IF
   ELSE                                                   ! МАССИВ НЕ ЗАДАН
      ICUQ=0                                              ! ФЛАГ МАССИВА
   END IF

   LE1=1             ! НОМЕР НАЧАЛЬНОГО КЭ ДЛЯ ПЕЧАТИ
   LE2=SIZE(EMATI,3) ! НОМЕР  КОНЕЧНОГО КЭ ДЛЯ ПЕЧАТИ

   IF ( PRESENT(KE1) ) THEN ! НОМЕР НАЧАЛЬНОГО КЭ ДЛЯ ПЕЧАТИ
      IF ( ( KE1 > 1 ) .AND. ( KE1 <= LE2 ) ) LE1=KE1
   END IF

   IF ( PRESENT(KE2) ) THEN ! НОМЕР КОНЕЧНОГО КЭ ДЛЯ ПЕЧАТИ
      IF ( ( KE2 > 1 ) .AND. ( KE2 <= LE2 ) ) LE2=KE2
   END IF

   KNL=3 ! BY DEFAULT
   IF ( PRESENT(KANAL) ) THEN ! НОМЕР КАНАЛА ДЛЯ ВЫВОДА В ФАЙЛ
      IF ( ( KANAL >= 10 ) .AND. ( KANAL <= 99 ) ) THEN
         KNL=KANAL
      END IF
   END IF

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE WMAT1

!**********************************************************************
!**********************************************************************

END MODULE FEM1
