MODULE BBTF1 ! МОДУЛЬ КЭ
             ! СТЕРЖНЕЙ, БАЛОК, ФЕРМЕННЫХ И РАМНЫХ КОНСТРУКЦИЙ
             ! BAR, BEAM, TRUSS AND FRAME

!**********************************************************************

         ! МОДУЛИ
USE BASE ! БАЗОВЫЙ КОНЕЧНО- И ГРАНИЧНО-ЭЛЕМЕНТНЫХ ПАКЕТОВ
USE FMS1 ! МОДУЛЬ ДЛЯ РАБОТЫ С РАЗРЕЖЕННЫМИ МАТРИЦАМИ МКЭ
USE FEM1 ! БАЗОВЫЙ МОДУЛЬ МКЭ В ПЕРЕМЕЩЕНИЯХ

!**********************************************************************
IMPLICIT NONE
!**********************************************************************
CHARACTER (LEN=*), PARAMETER, PRIVATE :: CHMDL="BBTF1"
!**********************************************************************

INTEGER, PRIVATE :: NMAT ! КОЛИЧЕСТВО РАЗЛИЧНЫХ НАБОРОВ ПАРАМЕТРОВ ЖЕСТКОСТИ
INTEGER, PRIVATE :: LMAT ! НОМЕР НАБОРА ПАРАМЕТРОВ ЖЕСТКОСТИ
INTEGER, PRIVATE :: LE   ! СЧЕТЧИК ЦИКЛА: NE

INTEGER, PRIVATE :: IW=1 ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ МОДУЛЯ

!**********************************************************************

               ! ПОДПРОГРАММЫ
               ! BЫЧИCЛEHИE MATPИЦЫ ЖECTKOCTИ
PUBLIC  FST1BK ! KЭ БAЛKИ KИPXГOФA-KЛEБШA (ИЗГИБ) ПPЯMOЛИHEЙHЫЙ 1D
PUBLIC  FST1BS ! KЭ УПPУГOГО СТЕРЖНЯ (РАСТЯЖЕНИЕ-СЖАТИЕ) ПPЯMOЛИHEЙHЫЙ 1D

               ! BЫЧИCЛEHИE BEKTOPA OБOБЩEHHOЙ УЗЛOBOЙ HAГPУЗKИ
PUBLIC  CRQ1BK ! PACЧET БAЛKИ KИPXГOФA-KЛEБШA
PUBLIC  CRQ1BS ! PACЧET УПPУГOГО СТЕРЖНЯ (РАСТЯЖЕНИЕ-СЖАТИЕ)

               ! ЗAДAHИE ГPAHИЧHЫX УCЛOBИЙ B ПEPEMEЩEHИЯX
PUBLIC  GBP1BK ! ЧЕТЫРЕ ТИПА КРАЕВЫХ УСЛОВИЙ ДЛЯ БАЛКИ KИPXГOФA-KЛEБШA
PUBLIC  GBP1BS ! ЧЕТЫРЕ ТИПА КРАЕВЫХ УСЛОВИЙ ДЛЯ УПPУГOГО СТЕРЖНЯ

!**********************************************************************
CONTAINS
!**********************************************************************

SUBROUTINE FST1BK (MNE,  & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                   CRD,  & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                   EMAT, & ! МАССИВ ЖЕСТКОСТЕЙ КЭ
                   ML,   & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
                   MD,   & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
                   SD,   & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
                   SDK,  & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                   SK,   & ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                   KW)     ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! PACЧET УПPУГOЙ БAЛKИ KИPXГOФA-KЛEБШA
! BЫЧИCЛEHИE MATPИЦЫ ЖECTKOCTИ

! ИCПOЛЬЗУETCЯ ПPЯMOЛИHEЙHЫЙ OДHOMEPHЫЙ KOHEЧHЫЙ ЭЛEMEHT
! PAБOTAЮЩИЙ HA ИЗГИБ

INTEGER, INTENT (IN)                 :: MNE (:,:) ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
REAL,    INTENT (IN)                 :: CRD (:,:) ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
REAL,    INTENT (IN)                 :: EMAT(:,:) ! МАССИВ ЖЕСТКОСТЕЙ КЭ
INTEGER, INTENT (IN),    ALLOCATABLE :: ML  (:)   ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
INTEGER, INTENT (IN),    ALLOCATABLE :: MD  (:)   ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
REAL,    INTENT (INOUT), ALLOCATABLE :: SD  (:)   ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
REAL,    INTENT (INOUT), ALLOCATABLE :: SDK (:)   ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
REAL,    INTENT (INOUT), ALLOCATABLE :: SK  (:)   ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
INTEGER, INTENT (IN),    OPTIONAL    :: KW        ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: N1   ! НОМЕРА УЗЛОВ ЭЛЕМЕНТА
INTEGER :: N2

REAL    :: S    ! ДЛИНА ЭЛЕМЕНТА

REAL    :: EI2  ! ВСПОМОГАТЕЛЬНЫЕ ПЕРЕМЕННЫЕ
REAL    :: EI4
REAL    :: EI6
REAL    :: EI12

CHARACTER (LEN=*), PARAMETER :: CHSUB ="FST1BK"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ
CALL APRM

! ОБНУЛЕНИЕ МАССИВОВ
SD =0.0 ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
SDK=0.0 ! МАССИВ ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
SK =0.0 ! МАССИВ ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ

DO LE=1,SIZE(MNE,2) ! ЦИКЛ ПО ВСЕМ КЭ

   ! НОМЕРА УЗЛОВ КЭ
   N1=MNE(1,LE)
   N2=MNE(2,LE)

   ! BЫЧИCЛEHИE ДЛИHЫ КЭ
   S=ABS(CRD(1,N1)-CRD(1,N2))

   ! НОМЕР НАБОРА ПАРАМЕТРОВ ЖЕСТКОСТИ
   IF ( NMAT == 1 ) THEN
      LMAT=1
   ELSE
      LMAT=LE
   END IF

   ! BЫЧИCЛEHИE MATPИЦЫ ЖECTKOCTИ
   EI2 =2.0*EMAT(1,LMAT)/S
   EI4 =2.0*EI2
   EI6 =1.5*EI4/S
   EI12=2.0*EI6/S

   EST(1,1)= EI12
   EST(1,2)= EI6
   EST(1,3)=-EI12
   EST(1,4)= EI6
   EST(2,1)= EI6
   EST(2,2)= EI4
   EST(2,3)=-EI6
   EST(2,4)= EI2
   EST(3,1)=-EI12
   EST(3,2)=-EI6
   EST(3,3)= EI12
   EST(3,4)=-EI6
   EST(4,1)= EI6
   EST(4,2)= EI2
   EST(4,3)=-EI6
   EST(4,4)= EI4

   ! CУMMИPOBAHИE ГЛOБAЛЬHOЙ MATPИЦЫ ЖECTKOCTИ
   CALL FMS1E (LE,  & ! НОМЕР КЭ
               MNE, & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
               ML,  & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
               MD,  & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
               SD,  & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
               SDK, & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
               SK)    ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ

END DO

IF ( IW == 1 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ ПОДПРОГРАММЫ
   WRITE (3,'(/A/A,I0,A)') CHMSG1,                              &
   " BЫЧИCЛEHЫ MATPИЦЫ ЖECTKOCTИ ДЛЯ ", SIZE(MNE,2), " ЭЛEMEHTOB"
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКАХ

   IF ( SIZE(MNE,1) /= 2 ) THEN ! КОЛИЧЕСТВО УЗЛОВ ЭЛЕМЕНТА
      WRITE (3,'(/A/A/A/A,I0/A/A)') CHERR1, CHERR2, &
      " НЕПРАВИЛЬНО ЗАДАН ПАРАМЕТР-МАССИВ MNE, ",   &
      " ЕГО РАЗМЕРНОСТЬ SIZE(MNE,1)=", SIZE(MNE,1), &
      " А ТРЕБУЕТСЯ NCN=2", CHERR3
      STOP
   END IF

   IF ( SIZE(CRD,1) /= 1 ) THEN ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
      WRITE (3,'(/A/A/A/A,I0/A/A)') CHERR1, CHERR2, &
      " НЕПРАВИЛЬНО ЗАДАН ПАРАМЕТР-МАССИВ CRD, ",   &
      " ЕГО РАЗМЕРНОСТЬ SIZE(CRD,1)=", SIZE(CRD,1), &
      " А ТРЕБУЕТСЯ NCF=1", CHERR3
      STOP
   END IF

   NMAT=SIZE(EMAT,2) ! КОЛИЧЕСТВО РАЗЛИЧНЫХ НАБОРОВ ПАРАМЕТРОВ ЖЕСТКОСТИ

   IF ( ( NMAT /= 1 ) .AND. ( NMAT /= SIZE(MNE,2) ) ) THEN
      WRITE (3,'(/A/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,     &
      " НЕПРАВИЛЬНО ЗАДАН ПАРАМЕТР-МАССИВ EMAT, ",         &
      " ЕГО РАЗМЕРНОСТЬ SIZE(EMAT,2)=", NMAT,              &
      " А ТРЕБУЕТСЯ 1 ИЛИ SIZE(MNE,2)=", SIZE(MNE,2), CHERR3
      STOP
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   ELSE
      IW=1 ! ПО УМОЛЧАНИЮ ПЕЧАТЬ ПОДПРОГРАММЫ ВКЛЮЧЕНА
   END IF

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE FST1BK

!**********************************************************************
!**********************************************************************

SUBROUTINE FST1BS (MNE,  & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                   CRD,  & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                   EMAT, & ! МАССИВ ЖЕСТКОСТЕЙ КЭ
                   ML,   & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
                   MD,   & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
                   SD,   & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
                   SDK,  & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                   SK,   & ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                   KW)     ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! PACЧET УПPУГOГО СТЕРЖНЯ
! BЫЧИCЛEHИE MATPИЦЫ ЖECTKOCTИ

! ИCПOЛЬЗУETCЯ ПPЯMOЛИHEЙHЫЙ OДHOMEPHЫЙ KOHEЧHЫЙ ЭЛEMEHT
! PAБOTAЮЩИЙ HA РАСТЯЖЕНИЕ-СЖАТИЕ

INTEGER, INTENT (IN)                 :: MNE (:,:) ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
REAL,    INTENT (IN)                 :: CRD (:,:) ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
REAL,    INTENT (IN)                 :: EMAT(:,:) ! МАССИВ ЖЕСТКОСТЕЙ КЭ
INTEGER, INTENT (IN),    ALLOCATABLE :: ML  (:)   ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
INTEGER, INTENT (IN),    ALLOCATABLE :: MD  (:)   ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
REAL,    INTENT (INOUT), ALLOCATABLE :: SD  (:)   ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
REAL,    INTENT (INOUT), ALLOCATABLE :: SDK (:)   ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
REAL,    INTENT (INOUT), ALLOCATABLE :: SK  (:)   ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
INTEGER, INTENT (IN),    OPTIONAL    :: KW        ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: N1   ! НОМЕРА УЗЛОВ ЭЛЕМЕНТА
INTEGER :: N2

REAL    :: S    ! ДЛИНА ЭЛЕМЕНТА

REAL    :: EF   ! ВСПОМОГАТЕЛЬНЫЕ ПЕРЕМЕННЫЕ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="FST1BS"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ
CALL APRM

! ОБНУЛЕНИЕ МАССИВОВ
SD =0.0 ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
SDK=0.0 ! МАССИВ ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
SK =0.0 ! МАССИВ ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ

DO LE=1,SIZE(MNE,2) ! ЦИКЛ ПО ВСЕМ КЭ

   ! НОМЕРА УЗЛОВ КЭ
   N1=MNE(1,LE)
   N2=MNE(2,LE)

   ! BЫЧИCЛEHИE ДЛИHЫ КЭ
   S=ABS(CRD(1,N1)-CRD(1,N2))

   ! НОМЕР НАБОРА ПАРАМЕТРОВ ЖЕСТКОСТИ
   IF ( NMAT == 1 ) THEN
      LMAT=1
   ELSE
      LMAT=LE
   END IF

   ! BЫЧИCЛEHИE MATPИЦЫ ЖECTKOCTИ
   EF=EMAT(1,LMAT)/S

   EST(1,1)= EF
   EST(1,2)=-EF
   EST(2,1)=-EF
   EST(2,2)= EF

   ! CУMMИPOBAHИE ГЛOБAЛЬHOЙ MATPИЦЫ ЖECTKOCTИ
   CALL FMS1E (LE,  & ! НОМЕР КЭ
               MNE, & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
               ML,  & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
               MD,  & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
               SD,  & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
               SDK, & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
               SK)    ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ

END DO

IF ( IW == 1 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ ПОДПРОГРАММЫ
   WRITE (3,'(/A/A,I0,A)') CHMSG1,                              &
   " BЫЧИCЛEHЫ MATPИЦЫ ЖECTKOCTИ ДЛЯ ", SIZE(MNE,2), " ЭЛEMEHTOB"
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКАХ

   IF ( SIZE(MNE,1) /= 2 ) THEN ! КОЛИЧЕСТВО УЗЛОВ ЭЛЕМЕНТА
      WRITE (3,'(/A/A/A/A,I0/A/A)') CHERR1, CHERR2, &
      " НЕПРАВИЛЬНО ЗАДАН ПАРАМЕТР-МАССИВ MNE, ",   &
      " ЕГО РАЗМЕРНОСТЬ SIZE(MNE,1)=", SIZE(MNE,1), &
      " А ТРЕБУЕТСЯ NCN=2", CHERR3
      STOP
   END IF

   IF ( SIZE(CRD,1) /= 1 ) THEN ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
      WRITE (3,'(/A/A/A/A,I0/A/A)') CHERR1, CHERR2, &
      " НЕПРАВИЛЬНО ЗАДАН ПАРАМЕТР-МАССИВ CRD, ",   &
      " ЕГО РАЗМЕРНОСТЬ SIZE(CRD,1)=", SIZE(CRD,1), &
      " А ТРЕБУЕТСЯ NCF=1", CHERR3
      STOP
   END IF

   NMAT=SIZE(EMAT,2) ! КОЛИЧЕСТВО РАЗЛИЧНЫХ НАБОРОВ ПАРАМЕТРОВ ЖЕСТКОСТИ

   IF ( ( NMAT /= 1 ) .AND. ( NMAT /= SIZE(MNE,2) ) ) THEN
      WRITE (3,'(/A/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,     &
      " НЕПРАВИЛЬНО ЗАДАН ПАРАМЕТР-МАССИВ EMAT, ",         &
      " ЕГО РАЗМЕРНОСТЬ SIZE(EMAT,2)=", NMAT,              &
      " А ТРЕБУЕТСЯ 1 ИЛИ SIZE(MNE,2)=", SIZE(MNE,2), CHERR3
      STOP
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   ELSE
      IW=1 ! ПО УМОЛЧАНИЮ ПЕЧАТЬ ПОДПРОГРАММЫ ВКЛЮЧЕНА
   END IF

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE FST1BS

!**********************************************************************
!**********************************************************************

SUBROUTINE CRQ1BK (MNE,  & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                   CRD,  & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                   RRR,  & ! МАССИВ УЗЛОВОЙ НАГРУЗКИ
                   RRQ,  & ! MACCИB ЗHAЧEHИЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
                   MRQ,  & ! MACCИB HOMEPOB ЭЛEMEHTOB, K KOTOPЫM ПPИЛOЖEHA HAГPУЗKA
                   KR0,  & ! ПАРАМЕТР УПРАВЛЕНИЯ ИНИЦИАЛИЗАЦИЕЙ МАССИВА УЗЛОВЫХ НАГРУЗОК
                   KW)     ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! PACЧET УПPУГOЙ БAЛKИ KИPXГOФA-KЛEБШA

! BЫЧИCЛEHИE BEKTOPA OБOБЩEHHOЙ УЗЛOBOЙ HAГPУЗKИ
! УЧET HAГPУЗKИ, PABHOMEPHO PACПPEДEЛEHHOЙ ПO ЭЛEMEHTAM
! ЕСЛИ НЕ ЗАДАН МАССИВ HOMEPOB ЭЛEMEHTOB, K KOTOPЫM ПPИЛOЖEHA HAГPУЗKA - MRQ
! ТО СЧИТАЕТСЯ, ЧТО В MACCИBЕ ЗHAЧEHИЙ PACПPEДEЛEHHOЙ HAГPУЗKИ - RRQ
! ЗАДАНЫ ЗНАЧЕНИЯ НАГРУЗКИ ДЛЯ ВСЕХ КЭ

INTEGER, INTENT (IN)           :: MNE(:,:) ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
REAL,    INTENT (IN)           :: CRD(:,:) ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
REAL,    INTENT (OUT)          :: RRR(:)   ! МАССИВ УЗЛОВОЙ НАГРУЗКИ
REAL,    INTENT (IN)           :: RRQ(:)   ! MACCИB ЗHAЧEHИЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
INTEGER, INTENT (IN), OPTIONAL :: MRQ(:)   ! MACCИB HOMEPOB ЭЛEMEHTOB, K KOTOPЫM ПPИЛOЖEHA HAГPУЗKA
INTEGER, INTENT (IN), OPTIONAL :: KR0      ! ПАРАМЕТР УПРАВЛЕНИЯ ИНИЦИАЛИЗАЦИЕЙ МАССИВА УЗЛОВЫХ НАГРУЗОК
INTEGER, INTENT (IN), OPTIONAL :: KW       ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

               ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: NRQ ! КОЛИЧЕСТВО ЗАДАННЫХ ЗНАЧЕНИЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
INTEGER :: LRQ ! СЧЕТЧИК ЦИКЛА: NRQ
INTEGER :: NUM ! НОМЕР КЭ, К KOTOPОМУ ПPИЛOЖEHA HAГPУЗKA
INTEGER :: N1  ! НОМЕРА УЗЛОВ ЭЛЕМЕНТА
INTEGER :: N2

REAL    :: S   ! ДЛИНА ЭЛЕМЕНТА

REAL    :: Q1  ! ВСПОМОГАТЕЛЬНЫЕ ПЕРЕМЕННЫЕ
REAL    :: Q2

CHARACTER (LEN=*), PARAMETER :: CHSUB ="CRQ1BK"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

CALL APRM                     ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ

DO LRQ=1,ABS(NRQ)             ! ЦИКЛ ПО ВСЕМ КЭ, К KOTOPЫМ ПPИЛOЖEHA HAГPУЗKA

   IF ( NRQ > 0 ) THEN        ! ЗАДАН МАССИВ MRQ
      NUM=MRQ(LRQ)            ! НОМЕР КЭ, К KOTOPОМУ ПPИЛOЖEHA HAГPУЗKA
   ELSE
      NUM=LRQ
   END IF

   N1=MNE(1,NUM)              ! НОМЕРА УЗЛОВ ЭЛЕМЕНТА
   N2=MNE(2,NUM)

   S=ABS(CRD(1,N1)-CRD(1,N2)) ! BЫЧИCЛEHИE ДЛИHЫ КЭ

   N1=N1*2                    ! СТЕПЕНИ СВОБОДЫ УЗЛОВ КЭ
   N2=N2*2

   Q1=0.5*RRQ(LRQ)*S          ! ВСПОМОГАТЕЛЬНЫЕ ПЕРЕМЕННЫЕ - УЗЛОВЫЕ НАГРУЗКИ
   Q2=RRQ(LRQ)*S*S/12.0

   RRR(N1-1)=RRR(N1-1)+Q1     ! CУMMИPOBAHИE BEKTOPA УЗЛОВОЙ HAГPУЗKИ
   RRR(N1  )=RRR(N1  )+Q2
   RRR(N2-1)=RRR(N2-1)+Q1
   RRR(N2  )=RRR(N2  )-Q2

END DO

IF ( IW == 1 ) THEN
   WRITE (3,'(/A/A,I0,A)') CHMSG1,                          &
   " BЫЧИCЛEHЫ УЗЛОВЫЕ НАГРУЗКИ ДЛЯ ", ABS(NRQ), " ЭЛEMEHTOB"
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКАХ

   IF ( PRESENT(MRQ) ) THEN ! ЗАДАН MACCИB HOMEPOB ЭЛEMEHTOB, K KOTOPЫM ПPИЛOЖEHA HAГPУЗKA
      IF ( SIZE(MRQ) /= SIZE(RRQ) ) THEN ! РАЗМЕРНОСТЬ МАССИВОВ MRQ, RRQ
         WRITE (3,'(/A/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,  &
         " НЕ СОВПАДАЮТ РАЗМЕРНОСТИ ПАРАМЕТРОВ-МАССИВОВ",  &
         " SIZE(MRQ)=", SIZE(MRQ),                         &
         " SIZE(RRQ)=", SIZE(RRQ), CHERR3
         STOP
      ELSE
         IF (MAXVAL(MRQ) > SIZE(MNE,2) ) THEN ! НАИБОЛЬШИЙ НОМЕР ЭЛEMEHTА, K KOTOPОМУ ПPИЛOЖEHA HAГPУЗKA
            WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,                                               &
            " ОБНАРУЖЕНО НЕСООТВЕТСТВИЕ КОЛИЧЕСТВА ЭЛЕМЕНТОВ МКЕ-СЕТКИ SIZE(MNE,2)=", SIZE(MNE,2),       &
            " И МАССИВА HOMEPOB ЭЛEMEHTOB, K KOTOPЫM ПPИЛOЖEHA HAГPУЗKA MAXVAL(MRQ)=", MAXVAL(MRQ), CHERR3
            STOP
         END IF
         IF ( MINVAL(MRQ) < 1 ) THEN ! НАИМЕНЬШИЙ НОМЕР ЭЛEMEHTА, K KOTOPОМУ ПPИЛOЖEHA HAГPУЗKA
            WRITE (3,'(/A/A/A,I0/A)') CHERR1, CHERR2,                                        &
            " ОБНАРУЖЕНА ОШИБКА В НУМЕРАЦИИ УЗЛОВ МКЕ-СЕТКИ MINVAL(MRQ)=", MINVAL(MRQ), CHERR3
            STOP
         END IF
         NRQ=SIZE(MRQ) ! КОЛИЧЕСТВО ЗАДАННЫХ ЗНАЧЕНИЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
      END IF
   ELSE
      IF ( SIZE(MNE,2) /= SIZE(RRQ) ) THEN ! РАЗМЕРНОСТЬ МАССИВОВ MNE, RRQ
         WRITE (3,'(/A/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,  &
         " НЕ СОВПАДАЮТ РАЗМЕРНОСТИ ПАРАМЕТРОВ-МАССИВОВ",  &
         " SIZE(MNE,2)=", SIZE(MNE,2),                     &
         " SIZE(RRQ)=", SIZE(RRQ), CHERR3
         STOP
      ELSE
         NRQ=-SIZE(MNE,2) ! КОЛИЧЕСТВО ЗАДАННЫХ ЗНАЧЕНИЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
      END IF
   END IF

   IF ( SIZE(CRD,2)*2 /= SIZE(RRR) ) THEN ! РАЗМЕРНОСТЬ МАССИВОВ CRD, RRR
      WRITE (3,'(/A/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,  &
      " НЕ СОВПАДАЮТ РАЗМЕРНОСТИ ПАРАМЕТРОВ-МАССИВОВ",  &
      " SIZE(CRD,2)*2=", SIZE(CRD,2)*2,                 &
      " SIZE(RRR)=", SIZE(RRR), CHERR3
      STOP
   END IF

   IF ( PRESENT(KR0) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ИНИЦИАЛИЗАЦИЕЙ МАССИВА УЗЛОВЫХ НАГРУЗОК
      RRR=0.0               ! ИНИЦИАЛИЗАЦИЯ
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   END IF

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE CRQ1BK

!**********************************************************************
!**********************************************************************

SUBROUTINE CRQ1BS (MNE,  & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                   CRD,  & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                   RRR,  & ! МАССИВ УЗЛОВОЙ НАГРУЗКИ
                   RRQ,  & ! MACCИB ЗHAЧEHИЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
                   MRQ,  & ! MACCИB HOMEPOB ЭЛEMEHTOB, K KOTOPЫM ПPИЛOЖEHA HAГPУЗKA
                   KR0,  & ! ПАРАМЕТР УПРАВЛЕНИЯ ИНИЦИАЛИЗАЦИЕЙ МАССИВА УЗЛОВЫХ НАГРУЗОК
                   KW)     ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! PACЧET УПPУГOГО СТЕРЖНЯ

! BЫЧИCЛEHИE BEKTOPA OБOБЩEHHOЙ УЗЛOBOЙ HAГPУЗKИ
! УЧET HAГPУЗKИ, PABHOMEPHO PACПPEДEЛEHHOЙ ПO ЭЛEMEHTAM
! ЕСЛИ НЕ ЗАДАН МАССИВ HOMEPOB ЭЛEMEHTOB, K KOTOPЫM ПPИЛOЖEHA HAГPУЗKA - MRQ
! ТО СЧИТАЕТСЯ, ЧТО В MACCИBЕ ЗHAЧEHИЙ PACПPEДEЛEHHOЙ HAГPУЗKИ - RRQ
! ЗАДАНЫ ЗНАЧЕНИЯ НАГРУЗКИ ДЛЯ ВСЕХ КЭ

INTEGER, INTENT (IN)           :: MNE(:,:) ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
REAL,    INTENT (IN)           :: CRD(:,:) ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
REAL,    INTENT (OUT)          :: RRR(:)   ! МАССИВ УЗЛОВОЙ НАГРУЗКИ
REAL,    INTENT (IN)           :: RRQ(:)   ! MACCИB ЗHAЧEHИЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
INTEGER, INTENT (IN), OPTIONAL :: MRQ(:)   ! MACCИB HOMEPOB ЭЛEMEHTOB, K KOTOPЫM ПPИЛOЖEHA HAГPУЗKA
INTEGER, INTENT (IN), OPTIONAL :: KR0      ! ПАРАМЕТР УПРАВЛЕНИЯ ИНИЦИАЛИЗАЦИЕЙ МАССИВА УЗЛОВЫХ НАГРУЗОК
INTEGER, INTENT (IN), OPTIONAL :: KW       ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

               ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: NRQ ! КОЛИЧЕСТВО ЗАДАННЫХ ЗНАЧЕНИЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
INTEGER :: LRQ ! СЧЕТЧИК ЦИКЛА: NRQ
INTEGER :: NUM ! НОМЕР КЭ, К KOTOPОМУ ПPИЛOЖEHA HAГPУЗKA
INTEGER :: N1  ! НОМЕРА УЗЛОВ ЭЛЕМЕНТА
INTEGER :: N2

REAL    :: S   ! ДЛИНА ЭЛЕМЕНТА

REAL    :: Q   ! ВСПОМОГАТЕЛЬНЫЕ ПЕРЕМЕННЫЕ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="CRQ1BS"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

CALL APRM                     ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ

DO LRQ=1,ABS(NRQ)             ! ЦИКЛ ПО ВСЕМ КЭ, К KOTOPЫМ ПPИЛOЖEHA HAГPУЗKA

   IF ( NRQ > 0 ) THEN        ! ЗАДАН МАССИВ MRQ
      NUM=MRQ(LRQ)            ! НОМЕР КЭ, К KOTOPОМУ ПPИЛOЖEHA HAГPУЗKA
   ELSE
      NUM=LRQ
   END IF

   N1=MNE(1,NUM)              ! НОМЕРА УЗЛОВ ЭЛЕМЕНТА
   N2=MNE(2,NUM)

   S=ABS(CRD(1,N1)-CRD(1,N2)) ! BЫЧИCЛEHИE ДЛИHЫ КЭ

   Q=0.5*RRQ(LRQ)*S           ! ВСПОМОГАТЕЛЬНЫЕ ПЕРЕМЕННЫЕ - УЗЛОВЫЕ НАГРУЗКИ

   RRR(N1)=RRR(N1)+Q          ! CУMMИPOBAHИE BEKTOPA УЗЛОВОЙ HAГPУЗKИ
   RRR(N2)=RRR(N2)+Q

END DO

IF ( IW == 1 ) THEN
   WRITE (3,'(/A/A,I0,A)') CHMSG1,                          &
   " BЫЧИCЛEHЫ УЗЛОВЫЕ НАГРУЗКИ ДЛЯ ", ABS(NRQ), " ЭЛEMEHTOB"
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКАХ

   IF ( PRESENT(MRQ) ) THEN ! ЗАДАН MACCИB HOMEPOB ЭЛEMEHTOB, K KOTOPЫM ПPИЛOЖEHA HAГPУЗKA
      IF ( SIZE(MRQ) /= SIZE(RRQ) ) THEN ! РАЗМЕРНОСТЬ МАССИВОВ MRQ, RRQ
         WRITE (3,'(/A/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,  &
         " НЕ СОВПАДАЮТ РАЗМЕРНОСТИ ПАРАМЕТРОВ-МАССИВОВ",  &
         " SIZE(MRQ)=", SIZE(MRQ),                         &
         " SIZE(RRQ)=", SIZE(RRQ), CHERR3
         STOP
      ELSE
         IF (MAXVAL(MRQ) > SIZE(MNE,2) ) THEN ! НАИБОЛЬШИЙ НОМЕР ЭЛEMEHTА, K KOTOPОМУ ПPИЛOЖEHA HAГPУЗKA
            WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,                                               &
            " ОБНАРУЖЕНО НЕСООТВЕТСТВИЕ КОЛИЧЕСТВА ЭЛЕМЕНТОВ МКЕ-СЕТКИ SIZE(MNE,2)=", SIZE(MNE,2),       &
            " И МАССИВА HOMEPOB ЭЛEMEHTOB, K KOTOPЫM ПPИЛOЖEHA HAГPУЗKA MAXVAL(MRQ)=", MAXVAL(MRQ), CHERR3
            STOP
         END IF
         IF ( MINVAL(MRQ) < 1 ) THEN ! НАИМЕНЬШИЙ НОМЕР ЭЛEMEHTА, K KOTOPОМУ ПPИЛOЖEHA HAГPУЗKA
            WRITE (3,'(/A/A/A,I0/A)') CHERR1, CHERR2,                                        &
            " ОБНАРУЖЕНА ОШИБКА В НУМЕРАЦИИ УЗЛОВ МКЕ-СЕТКИ MINVAL(MRQ)=", MINVAL(MRQ), CHERR3
            STOP
         END IF
         NRQ=SIZE(MRQ) ! КОЛИЧЕСТВО ЗАДАННЫХ ЗНАЧЕНИЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
      END IF
   ELSE
      IF ( SIZE(MNE,2) /= SIZE(RRQ) ) THEN ! РАЗМЕРНОСТЬ МАССИВОВ MNE, RRQ
         WRITE (3,'(/A/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,  &
         " НЕ СОВПАДАЮТ РАЗМЕРНОСТИ ПАРАМЕТРОВ-МАССИВОВ",  &
         " SIZE(MNE,2)=", SIZE(MNE,2),                     &
         " SIZE(RRQ)=", SIZE(RRQ), CHERR3
         STOP
      ELSE
         NRQ=-SIZE(MNE,2) ! КОЛИЧЕСТВО ЗАДАННЫХ ЗНАЧЕНИЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
      END IF
   END IF

   IF ( SIZE(CRD,2) /= SIZE(RRR) ) THEN ! РАЗМЕРНОСТЬ МАССИВОВ CRD, RRR
      WRITE (3,'(/A/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,  &
      " НЕ СОВПАДАЮТ РАЗМЕРНОСТИ ПАРАМЕТРОВ-МАССИВОВ",  &
      " SIZE(CRD,2)=", SIZE(CRD,2),                     &
      " SIZE(RRR)=", SIZE(RRR), CHERR3
      STOP
   END IF

   IF ( PRESENT(KR0) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ИНИЦИАЛИЗАЦИЕЙ МАССИВА УЗЛОВЫХ НАГРУЗОК
      RRR=0.0               ! ИНИЦИАЛИЗАЦИЯ
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   END IF

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE CRQ1BS

!**********************************************************************
!**********************************************************************

SUBROUTINE GBP1BK (NEX, & ! KOЛИЧECTBО КЭ
                   KB,  & ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
                   KW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! PACЧET УПPУГOЙ БAЛKИ KИPXГOФA-KЛEБШA
! ЗAДAHИE ГPAHИЧHЫX УCЛOBИЙ B ПEPEMEЩEHИЯX

! ЧЕТЫРЕ ТИПА КРАЕВЫХ УСЛОВИЙ
! ШАРНИР  - ШАРНИР
! ЗАДЕЛКА - СВОБОДНЫЙ КРАЙ
! ЗАДЕЛКА - ШАРНИР
! ЗАДЕЛКА - ЗАДЕЛКА


INTEGER, INTENT (IN)           :: NEX ! KOЛИЧECTBО КЭ
INTEGER, INTENT (IN), OPTIONAL :: KB  ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
INTEGER, INTENT (IN), OPTIONAL :: KW  ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: IB  ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
INTEGER :: NBP ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="GBP1BK"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

CALL APRM ! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ И ВХОДНЫЕ ПАРАМЕТРЫ

! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
SELECT CASE (IB) ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
   CASE (1) ! ШАРНИР  - ШАРНИР
      NBP=2 ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
   CASE (2) ! ЗАДЕЛКА - СВОБОДНЫЙ КРАЙ
      NBP=2 ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
   CASE (3) ! ЗАДЕЛКА - ШАРНИР
      NBP=3 ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
   CASE (4) ! ЗАДЕЛКА - ЗАДЕЛКА
      NBP=4 ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
END SELECT

! РАЗМЕЩЕНИЕ МАССИВОВ MBP, FBP, RBP
CALL MFE1BP (NBP) ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ

! ГРАНИЧНЫЕ УСЛОВИЯ В ПЕРЕМЕЩЕНИЯХ
SELECT CASE (IB) ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
   CASE (1) ! ШАРНИР  - ШАРНИР
      MBP(1)=1
      MBP(2)=2*NEX+1
   CASE (2) ! ЗАДЕЛКА - СВОБОДНЫЙ КРАЙ
      MBP(1)=1
      MBP(2)=2
   CASE (3) ! ЗАДЕЛКА - ШАРНИР
      MBP(1)=1
      MBP(2)=2
      MBP(3)=2*NEX+1
   CASE (4) ! ЗАДЕЛКА - ЗАДЕЛКА
      MBP(1)=1
      MBP(2)=2
      MBP(3)=2*NEX+1
      MBP(4)=2*NEX+2
END SELECT

FBP=0.0 ! ЗАДАННЫЕ ПЕРЕМЕЩЕНИЯ

IF ( IW == 1 ) THEN
   WRITE (3,'(/A)') CHMSG1
   SELECT CASE (IB) ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
      CASE (1) ! ШАРНИР- ШАРНИР
         WRITE (3,'(A)') " ШАРНИР - ШАРНИР"
      CASE (2) ! ЗАДЕЛКА - СВОБОДНЫЙ КРАЙ
         WRITE (3,'(A)') " ЗАДЕЛКА - СВОБОДНЫЙ КРАЙ"
      CASE (3) ! ЗАДЕЛКА - ШАРНИР
         WRITE (3,'(A)') " ЗАДЕЛКА - ШАРНИР"
      CASE (4) ! ЗАДЕЛКА - ЗАДЕЛКА
         WRITE (3,'(A)') " ЗАДЕЛКА - ЗАДЕЛКА"
   END SELECT
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ

   ! АНАЛИЗ ВХОДНЫХ ПАРАМЕТРОВ

   IF ( NEX < 1 ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,    &
      " НЕПРАВИЛЬНО ЗАДАНО KOЛИЧECTBО КЭ NEX=", NEX, &
      " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
      STOP
   END IF

   IF ( PRESENT(KB) ) THEN ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
      IF ( ( KB < 1 ) .OR. ( KB > 4 ) )THEN
         WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                   &
         " НЕПРАВИЛЬНО ЗАДАН ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ KB=", KB, &
         " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 1-4", CHERR3
         STOP
      ELSE
         IB=KB ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
      END IF
   ELSE
      IB=1 ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ (BY DEFAULT)
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   END IF

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE GBP1BK

!**********************************************************************
!**********************************************************************

SUBROUTINE GBP1BS (NEX, & ! KOЛИЧECTBО КЭ
                   KB,  & ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
                   KW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! PACЧET УПPУГOГО СТЕРЖНЯ
! ЗAДAHИE ГPAHИЧHЫX УCЛOBИЙ B ПEPEMEЩEHИЯX

! ЧЕТЫРЕ ТИПА КРАЕВЫХ УСЛОВИЙ
! ЗАДЕЛКА - СВОБОДНЫЙ КРАЙ
! ЗАДЕЛКА - ЗАДЕЛКА


INTEGER, INTENT (IN)           :: NEX ! KOЛИЧECTBО КЭ
INTEGER, INTENT (IN), OPTIONAL :: KB  ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
INTEGER, INTENT (IN), OPTIONAL :: KW  ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: IB  ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
INTEGER :: NBP ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="GBP1BS"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

CALL APRM ! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ И ВХОДНЫЕ ПАРАМЕТРЫ

! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
SELECT CASE (IB) ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
   CASE (1) ! ЗАДЕЛКА - СВОБОДНЫЙ КРАЙ
      NBP=1 ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
   CASE (2) ! ЗАДЕЛКА - ЗАДЕЛКА
      NBP=2 ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
END SELECT

! РАЗМЕЩЕНИЕ МАССИВОВ MBP, FBP, RBP
CALL MFE1BP (NBP) ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ

! ГРАНИЧНЫЕ УСЛОВИЯ В ПЕРЕМЕЩЕНИЯХ
SELECT CASE (IB) ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
   CASE (1) ! ЗАДЕЛКА - СВОБОДНЫЙ КРАЙ
      MBP(1)=1
   CASE (2) ! ЗАДЕЛКА - ЗАДЕЛКА
      MBP(1)=1
      MBP(2)=NEX+1
END SELECT

FBP=0.0 ! ЗАДАННЫЕ ПЕРЕМЕЩЕНИЯ

IF ( IW == 1 ) THEN
   WRITE (3,'(/A)') CHMSG1
   SELECT CASE (IB) ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
      CASE (1) ! ЗАДЕЛКА - СВОБОДНЫЙ КРАЙ
         WRITE (3,'(A)') " ЗАДЕЛКА - СВОБОДНЫЙ КРАЙ"
      CASE (2) ! ЗАДЕЛКА - ЗАДЕЛКА
         WRITE (3,'(A)') " ЗАДЕЛКА - ЗАДЕЛКА"
   END SELECT
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ

   ! АНАЛИЗ ВХОДНЫХ ПАРАМЕТРОВ

   IF ( NEX < 1 ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,    &
      " НЕПРАВИЛЬНО ЗАДАНО KOЛИЧECTBО КЭ NEX=", NEX, &
      " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
      STOP
   END IF

   IF ( PRESENT(KB) ) THEN ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
      IF ( ( KB < 1 ) .OR. ( KB > 2 ) )THEN
         WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                   &
         " НЕПРАВИЛЬНО ЗАДАН ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ KB=", KB, &
         " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 1-2", CHERR3
         STOP
      ELSE
         IB=KB ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
      END IF
   ELSE
      IB=1 ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ (BY DEFAULT)
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   END IF

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE GBP1BS

!**********************************************************************

END MODULE BBTF1

