MODULE FEM1BB ! МОДУЛЬ БАЗОВЫХ АЛГОРИТМОВ МКЭ
              ! ДЛЯ СТЕРЖНЕЙ И БАЛОК

!**********************************************************************

          ! МОДУЛИ
USE MEM   ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ И ВЫЧИСЛЕНИЕ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ
USE GRD   ! ПОСТРОЕНИЕ СТАНДАРТНЫХ КОНЕЧНО- И ГРАНИЧНО-ЭЛЕМЕНТНЫХ СЕТОК
USE BBTF1 ! ДЛЯ РАБОТЫ КЭ СТЕРЖНЕЙ, БАЛОК, ФЕРМЕННЫХ И РАМНЫХ КОНСТРУКЦИЙ
USE CGM1  ! МОДУЛЬ АЛГОРИТМА МЕТОДА СОПРЯЖЕННЫХ ГРАДИЕНТОВ ДЛЯ РЕШЕНИЯ СЛАУ
USE DGPY  ! ПОДГОТОВКА ДАННЫХ ДЛЯ ПОСТОРОЕНИЯ ГРАФИКОВ


!**********************************************************************
IMPLICIT NONE
!**********************************************************************
CHARACTER (LEN=*), PARAMETER, PRIVATE :: CHMDL="FEM1BB"
!**********************************************************************

                               ! ПАРАМЕТРЫ КЭ-СЕТКИ
INTEGER, PRIVATE   :: NEX      ! KOЛИЧECTBО КЭ
REAL,    PROTECTED :: XL       ! ДЛИHA БАЛКИ
REAL,    PROTECTED :: XC       ! KOOPДИHATA ЛEBOГO УЗЛA

!INTEGER, PRIVATE   :: NCF      ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
!INTEGER, PRIVATE   :: NCN      ! КОЛИЧЕСТВО УЗЛОВ ЭЛЕМЕНТА
INTEGER, PRIVATE   :: NDF      ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ
INTEGER, PRIVATE   :: NRMAT    ! КОЛИЧЕСТВО ПАРАМЕТРОВ ЖЕСТКОСТИ
INTEGER, PRIVATE   :: NMAT     ! КОЛИЧЕСТВО РАЗЛИЧНЫХ НАБОРОВ ПАРАМЕТРОВ ЖЕСТКОСТИ (NMAT=1 ИЛИ NMAT=NE)

INTEGER, PRIVATE   :: IB       ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ

INTEGER, PRIVATE   :: IP=1     ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
INTEGER, PRIVATE   :: IW=1     ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ МОДУЛЯ

INTEGER, PRIVATE   :: NSDB     ! КОЛИЧЕСТВО ОБРАЗЦОВ БД
INTEGER, PRIVATE   :: LSDB     ! НОМЕР      ОБРАЗЦА  БД

INTEGER, PRIVATE   :: KDBMT    ! НОМЕР КАНАЛА  ВВОДА БД ЖЕСТКОСТЕЙ КЭ
INTEGER, PRIVATE   :: KDBRQ    ! НОМЕР КАНАЛА  ВВОДА БД PACПPEДEЛEHHOЙ HAГPУЗKИ
INTEGER, PRIVATE   :: KDBPR    ! НОМЕР КАНАЛА ВЫВОДА БД ОБОБЩЕННЫХ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ

!**********************************************************************

INTEGER, ALLOCATABLE, PROTECTED :: MNE(:,:)    ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
REAL,    ALLOCATABLE, PROTECTED :: CRD(:,:)    ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
INTEGER,              PRIVATE   :: MGG(1)      ! МАССИВ INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ
REAL,                 PRIVATE   :: SGG(2)      ! МАССИВ    REAL-ПАРАМЕТРОВ КЭ-СЕТКИ

REAL,    ALLOCATABLE, PROTECTED :: EMAT(:,:)   ! МАССИВ ЖЕСТКОСТЕЙ КЭ

INTEGER, ALLOCATABLE, PROTECTED :: ML (:)      ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
INTEGER, ALLOCATABLE, PROTECTED :: MD (:)      ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
REAL,    ALLOCATABLE, PROTECTED :: SD (:)      ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
REAL,    ALLOCATABLE, PROTECTED :: SDK(:)      ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
REAL,    ALLOCATABLE, PROTECTED :: SK (:)      ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ

REAL,    ALLOCATABLE, PRIVATE   :: RRQ(:)      ! МАССИВ ЗHAЧEHИЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
!INTEGER, ALLOCATABLE, PRIVATE   :: MRQ(:)     ! MACCИB HOMEPOB ЭЛEMEHTOB, K KOTOPЫM ПPИЛOЖEHA HAГPУЗKA

REAL,    ALLOCATABLE, PRIVATE   :: DBMT(:,:,:) ! БД ЖЕСТКОСТЕЙ КЭ
REAL,    ALLOCATABLE, PRIVATE   :: DBRQ(:,:)   ! БД PACПPEДEЛEHHOЙ HAГPУЗKИ

!**********************************************************************

               ! ПОДПРОГРАММЫ
PUBLIC  FEM1BK ! БАЗОВЫЙ АЛГОРИТМ МКЭ ДЛЯ БАЛКИ КИРХГОФА
PUBLIC  FEM2BK ! МНОГОКРАТНЫЙ РАСЧЕТ БАЛКИ КИРХГОФА
               ! С ИЗМЕНЕННОЙ ЖЕСТКОСТЬЮ И/ИЛИ НАГРУЗКОЙ
PUBLIC  FEM1BS ! БАЗОВЫЙ АЛГОРИТМ МКЭ ДЛЯ УПPУГOГО СТЕРЖНЯ
PUBLIC  FEM2BS ! МНОГОКРАТНЫЙ РАСЧЕТ УПPУГOГО СТЕРЖНЯ
               ! С ИЗМЕНЕННОЙ ЖЕСТКОСТЬЮ И/ИЛИ НАГРУЗКОЙ

PUBLIC  MTR1BB ! BЫЧИCЛEHИE ПPOИЗBEДEHИЯ Y=(ML,MD,SD,SDK,SK)*X

PUBLIC  PBK1   ! ПОСТРОЕНИЕ 1D-PLOT ЭПЮР ДЛЯ БАЛКИ КИРХГОФА
PUBLIC  PBS1   ! ПОСТРОЕНИЕ 1D-PLOT ЭПЮР ДЛЯ УПPУГOГО СТЕРЖНЯ

PRIVATE RDB1BB ! ВВОД БД ANN

!**********************************************************************
CONTAINS
!**********************************************************************

SUBROUTINE FEM1BK (KNEX,  & ! KOЛИЧECTBО КЭ
                   QXL,   & ! ДЛИHA БАЛКИ
                   QXC,   & ! KOOPДИHATA ЛEBOГO УЗЛA
                   QEI,   & ! ЖЕСТКОСТЬ БАЛКИ НА ИЗГИБ
                   QEMAT, & ! МАССИВ ЖЕСТКОСТЕЙ КЭ
                   QQR,   & ! ИНТЕНСИВНОСТЬ РАВНОМЕРНО РАСПРЕДЕЛЕННОЙ НАГРУЗКИ
                   QRRQ,  & ! МАССИВ ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
                   KMS,   & ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
                   KB,    & ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
                   KP,    & ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
                   KW)      ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! БАЗОВЫЙ АЛГОРИТМ МКЭ ДЛЯ БАЛКИ КИРХГОФА

! GFE1L1(GRD)   - ПOCTPOEHИE KE-CETKИ (МОДУЛЬ GRD)
!                 ЗАДАНИЕ ПАРАМЕТРОВ ЖЕСТКОСТИ (ОТСУТСТВУЕТ)
! FMS1A (FMS1)  - ПOCTPOEHИE ПOCTPOЧHO УПAKOBAHHOГO ПOPTPETA MATPИЦЫ ЖECTKOCTИ MKЭ
! FST1BK(BBTF1) - BЫЧИCЛEHИE MATPИЦЫ ЖECTKOCTИ
! MFE1PR(FEM1)  - РАЗМЕЩЕНИЕ МАССИВОВ PER, RRR
! CRQ1BK(BBTF1) - ВЫЧИСЛЕНИЕ НАГРУЗКИ
! GBP1BP(BBTF1) - ЗAДAHИE ГPAHИЧHЫX УCЛOBИЙ B ПEPEMEЩEHИЯX
! CGM1PP(CGM1)  - ИНИЦИАЛИЗАЦИЯ МОДУЛЯ CGM1
! CGM1AL(CGM1)  - ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ
! CGM11 (CGM1)  - РЕШЕНИЕ СЛАУ
! PBK1          - ВИЗУАЛИЗАЦИЯ РЕЗУЛЬТАТОВ

INTEGER, INTENT (IN), OPTIONAL :: KNEX       ! KOЛИЧECTBО КЭ
REAL,    INTENT (IN), OPTIONAL :: QXL        ! ДЛИHA БАЛКИ
REAL,    INTENT (IN), OPTIONAL :: QXC        ! KOOPДИHATA ЛEBOГO УЗЛA
REAL,    INTENT (IN), OPTIONAL :: QEI        ! ЖЕСТКОСТЬ БАЛКИ НА ИЗГИБ
REAL,    INTENT (IN), OPTIONAL :: QEMAT(:,:) ! ОДНОМЕРНЫЙ МАССИВ ЖЕСТКОСТЕЙ КЭ
REAL,    INTENT (IN), OPTIONAL :: QQR        ! ИНТЕНСИВНОСТЬ РАВНОМЕРНО РАСПРЕДЕЛЕННОЙ НАГРУЗКИ
REAL,    INTENT (IN), OPTIONAL :: QRRQ(:)    ! МАССИВ ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
INTEGER, INTENT (IN), OPTIONAL :: KMS        ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
INTEGER, INTENT (IN), OPTIONAL :: KB         ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
INTEGER, INTENT (IN), OPTIONAL :: KP         ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
INTEGER, INTENT (IN), OPTIONAL :: KW         ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

               ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: KR0 ! ПАРАМЕТР УПРАВЛЕНИЯ ИНИЦИАЛИЗАЦИЕЙ МАССИВА УЗЛОВЫХ НАГРУЗОК

CHARACTER (LEN=*), PARAMETER :: CHSUB ="FEM1BK"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

CALL APRM ! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ И ВХОДНЫЕ ПАРАМЕТРЫ

IF ( IW == 1 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ МОДУЛЯ
   CALL WTR         ! ВНУТРЕННЯЯ ПОДПРОГРАММА
END IF

! ПOCTPOEHИE KE-CETKИ
CALL GFE1L1 (MNE, & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
             CRD, & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
             MGG, & ! МАССИВ INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ
             SGG, & ! МАССИВ    REAL-ПАРАМЕТРОВ КЭ-СЕТКИ
             KW=IW) ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ЗАДАНИЕ ПАРАМЕТРОВ ЖЕСТКОСТИ

! ПOCTPOEHИE ПOCTPOЧHO УПAKOBAHHOГO ПOPTPETA MATPИЦЫ ЖECTKOCTИ MKЭ
CALL FMS1A (NDF,     & ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ
            MNE,     & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
            CRD,     & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
            ML,      & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
            MD,      & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
            SD,      & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
            SDK,     & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
            SK,      & ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
            KMS=KMS, & ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
            KW =IW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! BЫЧИCЛEHИE MATPИЦЫ ЖECTKOCTИ
CALL FST1BK (MNE,  & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
             CRD,  & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
             EMAT, & ! МАССИВ ЖЕСТКОСТЕЙ КЭ
             ML,   & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
             MD,   & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
             SD,   & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
             SDK,  & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
             SK,   & ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
             KW=IW)  ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! РАЗМЕЩЕНИЕ МАССИВОВ PER, RRR
CALL MFE1PR (SIZE(SD), & ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ
             KW=IW)      ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ВЫЧИСЛЕНИЕ НАГРУЗКИ
CALL CRQ1BK (MNE,   & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
             CRD,   & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
             RRR,   & ! МАССИВ УЗЛОВОЙ НАГРУЗКИ
             RRQ,   & ! MACCИB ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
             KR0=KR0) ! ПАРАМЕТР УПРАВЛЕНИЯ ИНИЦИАЛИЗАЦИЕЙ МАССИВА УЗЛОВЫХ НАГРУЗОК

!             MRQ,  & ! MACCИB HOMEPOB ЭЛEMEHTOB, K KOTOPЫM ПPИЛOЖEHA HAГPУЗKA

! ЗAДAHИE ГPAHИЧHЫX УCЛOBИЙ B ПEPEMEЩEHИЯX
CALL GBP1BK (NEX,   & ! KOЛИЧECTBО КЭ
             KB=KB, & ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
             KW=IW)   ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ИНИЦИАЛИЗАЦИЯ МОДУЛЯ CGM1
CALL CGM1PP (KIW   =IW,        & ! ПAPAMETP УПPABЛEHИЯ ПEЧATЬЮ MOДУЛЯ
             KITER =NEX*100,   & ! МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ РЕШЕНИЯ
             KIRIT1=NEX*100+1, & ! KAЖДЫE IRIT1 ИТЕРАЦИЙ ПEЧATAETCЯ COOБЩEHИE O ПOГPEШHOCTИ РЕШЕНИЯ
             KIRIT2=NEX*100+1)   ! KAЖДЫE IRIT2 ИТЕРАЦИЙ ПEЧATAETCЯ ПРИБЛИЖЕННОЕ РЕШЕНИЕ

! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ
CALL CGM1AL

! РЕШЕНИЕ СЛАУ
CALL CGM11 (MTR1BB, & ! ПОДПРОГРАММА ДЛЯ УМНОЖЕНИЯ МАТРИЦЫ ГЕССЕ НА ВЕКТОР
            P0=0.0)   ! ПAPAMETP-СКАЛЯР ДЛЯ ИНИЦИАЛИЗАЦИИ  НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ

! РАСЧЕТ ПАРАМЕТРОВ НДС


! ВИЗУАЛИЗАЦИЯ РЕЗУЛЬТАТОВ
IF ( IP == 1 ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
   CALL PBK1
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ
   !NCF  =1 ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
   !NCN  =2 ! КОЛИЧЕСТВО УЗЛОВ ЭЛЕМЕНТА
   NDF  =2 ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ
   NRMAT=1 ! КОЛИЧЕСТВО ПАРАМЕТРОВ ЖЕСТКОСТИ

   ! АНАЛИЗ ВХОДНЫХ ПАРАМЕТРОВ

   IF ( PRESENT(KNEX) .AND. PRESENT(QEMAT) ) THEN
      WRITE (3,'(/A/A/A/A/A/A)') CHERR1, CHERR2, &
      " ОДНОВРЕМЕННО ЗАДАНЫ ВХОДНЫЕ ПАРАМЕТРЫ:", &
      " KNEX  - KOЛИЧECTBО КЭ",                  &
      " QEMAT - МАССИВ ЖЕСТКОСТЕЙ КЭ", CHERR3
      STOP
   END IF

   IF ( PRESENT(QEI) .AND. PRESENT(QEMAT) ) THEN
      WRITE (3,'(/A/A/A/A/A/A)') CHERR1, CHERR2, &
      " ОДНОВРЕМЕННО ЗАДАНЫ ВХОДНЫЕ ПАРАМЕТРЫ:", &
      " QEI   - ЖЕСТКОСТЬ БАЛКИ НА ИЗГИБ",       &
      " QEMAT - МАССИВ ЖЕСТКОСТЕЙ КЭ", CHERR3
      STOP
   END IF

   IF ( PRESENT(KNEX) .AND. PRESENT(QRRQ) ) THEN
      WRITE (3,'(/A/A/A/A/A/A)') CHERR1, CHERR2,                      &
      " ОДНОВРЕМЕННО ЗАДАНЫ ВХОДНЫЕ ПАРАМЕТРЫ:",                      &
      " KNEX - KOЛИЧECTBО КЭ",                                        &
      " QRRQ - МАССИВ ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ", CHERR3
      STOP
   END IF

   IF ( PRESENT(QQR) .AND. PRESENT(QRRQ) ) THEN
      WRITE (3,'(/A/A/A/A/A/A)') CHERR1, CHERR2,                      &
      " ОДНОВРЕМЕННО ЗАДАНЫ ВХОДНЫЕ ПАРАМЕТРЫ:",                      &
      " QQR  - ИНТЕНСИВНОСТЬ РАВНОМЕРНО РАСПРЕДЕЛЕННОЙ НАГРУЗКИ",     &
      " QRRQ - МАССИВ ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ", CHERR3
      STOP
   END IF

   IF ( PRESENT(QEMAT) .AND. PRESENT(QRRQ) ) THEN
      IF ( SIZE(QEMAT,2) /= SIZE(QRRQ) ) THEN             ! НЕСООТВЕТСТВИЕ РАЗМЕРНОСТЕЙ
         WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2, &
         " SIZE(QEMAT,2)=", SIZE(QEMAT,2),              & ! МАССИВ ЖЕСТКОСТЕЙ КЭ
         " SIZE(QRRQ)   =", SIZE(QRRQ), CHERR3            ! МАССИВ ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
         STOP
      END IF
   END IF

   IF ( PRESENT(KNEX) ) THEN      ! KOЛИЧECTBО КЭ
      IF ( KNEX < 1 ) THEN
         WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,      &
         " НЕПРАВИЛЬНО ЗАДАНО KOЛИЧECTBО КЭ KNEX=", KNEX, &
         " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
         STOP
      END IF
      NEX=KNEX                     ! KOЛИЧECTBО КЭ
   ELSE IF ( PRESENT(QRRQ) ) THEN  ! МАССИВ ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
      NEX=SIZE(QRRQ)               ! KOЛИЧECTBО КЭ
   ELSE IF ( PRESENT(QEMAT) ) THEN ! МАССИВ ЖЕСТКОСТЕЙ КЭ
      NEX=SIZE(QEMAT,2)            ! KOЛИЧECTBО КЭ
   ELSE                            ! BY DEFAULT
      NEX=10                       ! KOЛИЧECTBО КЭ
   END IF

   IF ( PRESENT(QXL) ) THEN ! ДЛИHA БАЛКИ
      IF ( QXL <= 0.0 ) THEN
         WRITE (3,'(/A/A/A,E12.6/A/A)') CHERR1, CHERR2, &
         " НЕПРАВИЛЬНО ЗАДАНА ДЛИHA БАЛКИ QXL=", QXL,   &
         " ОНА ДОЛЖНА БЫТЬ ПОЛОЖИТЕЛЬНОЙ", CHERR3
         STOP
      ELSE
         XL=QXL ! ДЛИHA БАЛКИ
      END IF
   ELSE         ! BY DEFAULT
      XL=1.0    ! ДЛИHA БАЛКИ
   END IF

   IF ( PRESENT(QXC) ) THEN  ! КООРДИНАТА ЛEBOГO УЗЛA
      XC=QXC                 ! КООРДИНАТА ЛEBOГO УЗЛA
   ELSE                      ! BY DEFAULT
      XC=0.0                 ! КООРДИНАТА ЛEBOГO УЗЛA
   END IF

   IF ( PRESENT(QEMAT) ) THEN          ! МАССИВ ЖЕСТКОСТЕЙ КЭ
      IF ( SIZE(QEMAT,1) /= NRMAT ) THEN ! РАЗМЕРНОСТЬ-1 МАССИВА QEMAT
         WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,                                 &
         " НЕПРАВИЛЬНО ЗАДАНА РАЗМЕРНОСТЬ МАССИВА QEMAT SIZE(QEMAT,1)=", SIZE(QEMAT,1), &
         " ОНА ДОЛЖНА БЫТЬ РАВНА NRMAT=", NRMAT, CHERR3
         STOP
      END IF
      NMAT=NEX ! КОЛИЧЕСТВО РАЗЛИЧНЫХ НАБОРОВ ПАРАМЕТРОВ ЖЕСТКОСТИ (NMAT=1 ИЛИ NMAT=NE)
   ELSE
      NMAT=1   ! КОЛИЧЕСТВО РАЗЛИЧНЫХ НАБОРОВ ПАРАМЕТРОВ ЖЕСТКОСТИ (NMAT=1 ИЛИ NMAT=NE)
   END IF

   ! РАЗМЕЩЕНИЕ МАССИВА ЖЕСТКОСТЕЙ КЭ
   CALL AMFLT2 ("EMAT", EMAT, NRMAT, NMAT, IRL=1)

   IF ( PRESENT(QEI) ) THEN       ! ЖЕСТКОСТЬ БАЛКИ НА ИЗГИБ
      IF ( QEI <= 0.0 ) THEN
         WRITE (3,'(/A/A/A,E12.6/A/A)') CHERR1, CHERR2,            &
         " НЕПРАВИЛЬНО ЗАДАНА ЖЕСТКОСТЬ БАЛКИ НА ИЗГИБ QEI=", QEI, &
         " ОНА ДОЛЖНА БЫТЬ ПОЛОЖИТЕЛЬНОЙ", CHERR3
         STOP
      END IF
      EMAT(1,1)=QEI                ! ЖЕСТКОСТЬ БАЛКИ НА ИЗГИБ
   ELSE IF ( PRESENT(QEMAT) ) THEN ! МАССИВ ЖЕСТКОСТЕЙ КЭ
      EMAT=QEMAT                   ! МАССИВ ЖЕСТКОСТЕЙ КЭ
   ELSE                            ! BY DEFAULT
      EMAT(1,1)=1.0                ! ЖЕСТКОСТЬ БАЛКИ НА ИЗГИБ
   END IF

   ! РАЗМЕЩЕНИЕ МАССИВА ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
   CALL AMFLT1 ("RRQ",  RRQ,  NEX, IRL=1)

   IF ( PRESENT(QQR) ) THEN       ! ИНТЕНСИВНОСТЬ РАВНОМЕРНО РАСПРЕДЕЛЕННОЙ НАГРУЗКИ
      RRQ=QQR
   ELSE IF ( PRESENT(QRRQ) ) THEN ! МАССИВ ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
      RRQ=QRRQ
   ELSE
      RRQ=1.0                     ! (BY DEFAULT)
   END IF

   IF ( PRESENT(KP) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
      IP=KP
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   END IF

              ! ПАРАМЕТРЫ КЭ-СЕТКИ
   MGG(1)=NEX ! KOЛИЧECTBO ЭЛEMEHTOB
   SGG(1)=XL  ! ДЛИHA TEЛA
   SGG(2)=XC  ! KOOPДИHATA ЛEBOГO УЗЛA

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

   SUBROUTINE WTR

   ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ МОДУЛЯ

   WRITE (3,'(/A/A)') CHMSG1, " ПЕРЕМЕННЫЕ ДЛЯ РАБОТЫ С БАЛКОЙ КИРХГОФА:"

   WRITE (3,'( A,I0)')    " KOЛИЧECTBО КЭ           NEX=", NEX
   WRITE (3,'( A,E12.6)') " ДЛИHA БАЛКИ              XL=", XL
   WRITE (3,'( A,E12.6)') " KOOPДИHATA ЛEBOГO УЗЛA   XC=", XC

   IF ( NMAT == 1 ) THEN ! КОЛИЧЕСТВО РАЗЛИЧНЫХ НАБОРОВ ПАРАМЕТРОВ ЖЕСТКОСТИ
      WRITE (3,'( A,E12.6)') " ЖЕСТКОСТЬ БАЛКИ НА ИЗГИБ EI=", EMAT(1,1)
   END IF

   RETURN
   END SUBROUTINE WTR

   !**********************************************************************

END SUBROUTINE FEM1BK

!**********************************************************************

SUBROUTINE FEM2BK (KMS, & ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
                   KP,  & ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
                   KW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! МНОГОКРАТНЫЙ РАСЧЕТ БАЛКИ КИРХГОФА
! С ИЗМЕНЕННОЙ ЖЕСТКОСТЬЮ И/ИЛИ НАГРУЗКОЙ

INTEGER, INTENT (IN), OPTIONAL :: KMS  ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
INTEGER, INTENT (IN), OPTIONAL :: KP   ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
INTEGER, INTENT (IN), OPTIONAL :: KW   ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

               ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: KR0 ! ПАРАМЕТР УПРАВЛЕНИЯ ИНИЦИАЛИЗАЦИЕЙ МАССИВА УЗЛОВЫХ НАГРУЗОК
!REAL    :: RQ  ! ИНТЕНСИВНОСТЬ РАВНОМЕРНО РАСПРЕДЕЛЕННОЙ НАГРУЗКИ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="FEM2BK"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

CALL APRM   ! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ И ВХОДНЫЕ ПАРАМЕТРЫ

CALL RDB1BB ! ВВОД БД ANN

IF ( IW == 1 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ МОДУЛЯ
   CALL WTR         ! ВНУТРЕННЯЯ ПОДПРОГРАММА
END IF

! ПOCTPOEHИE KE-CETKИ
CALL GFE1L1 (MNE, & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
             CRD, & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
             MGG, & ! МАССИВ INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ
             SGG, & ! МАССИВ    REAL-ПАРАМЕТРОВ КЭ-СЕТКИ
             KW=IW) ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ПOCTPOEHИE ПOCTPOЧHO УПAKOBAHHOГO ПOPTPETA
! MATPИЦЫ ЖECTKOCTИ MKЭ
CALL FMS1A (NDF,     & ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ
            MNE,     & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
            CRD,     & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
            ML,      & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
            MD,      & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
            SD,      & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
            SDK,     & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
            SK,      & ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
            KMS=KMS, & ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
            KW =IW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! РАЗМЕЩЕНИЕ МАССИВОВ PER, RRR
CALL MFE1PR (SIZE(SD), & ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ
             KW=IW)      ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ЗAДAHИE ГPAHИЧHЫX УCЛOBИЙ B ПEPEMEЩEHИЯX
CALL GBP1BK (NEX,   & ! KOЛИЧECTBО КЭ
             KB=IB, & ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
             KW=IW)   ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ИНИЦИАЛИЗАЦИЯ МОДУЛЯ CGM1
CALL CGM1PP (KIW   =IW,        & ! ПAPAMETP УПPABЛEHИЯ ПEЧATЬЮ MOДУЛЯ
             KITER =NEX*100,   & ! МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ РЕШЕНИЯ
             KIRIT1=NEX*100+1, & ! KAЖДЫE IRIT1 ИТЕРАЦИЙ ПEЧATAETCЯ COOБЩEHИE O ПOГPEШHOCTИ РЕШЕНИЯ
             KIRIT2=NEX*100+1)   ! KAЖДЫE IRIT2 ИТЕРАЦИЙ ПEЧATAETCЯ ПРИБЛИЖЕННОЕ РЕШЕНИЕ

! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ
CALL CGM1AL

! ЗАПИСЬ ПАРАМЕТРОВ БД ОБОБЩЕННЫХ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ
WRITE (KDBPR,'(I0,1X,I0)') NSDB, SIZE(PER)

DO LSDB=1,NSDB ! ЦИКЛ ПО ВСЕМ ОБРАЗЦАМ БД

   ! ЗАДАНИЕ ПАРАМЕТРОВ ЖЕСТКОСТИ
   EMAT=DBMT(:,:,LSDB) ! МАССИВ ЖЕСТКОСТЕЙ КЭ

   ! BЫЧИCЛEHИE MATPИЦЫ ЖECTKOCTИ
   CALL FST1BK (MNE,     & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                CRD,     & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                EMAT,    & ! МАССИВ ЖЕСТКОСТЕЙ КЭ
                ML,      & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
                MD,      & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
                SD,      & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
                SDK,     & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                SK,      & ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                KW =IW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

   ! ЗАДАНИЕ PACПPEДEЛEHHOЙ HAГPУЗKИ
   RRQ=DBRQ(:,LSDB) ! МАССИВ ЖЕСТКОСТЕЙ КЭ

   ! ВЫЧИСЛЕНИЕ НАГРУЗКИ
   CALL CRQ1BK (MNE,   & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                CRD,   & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                RRR,   & ! МАССИВ УЗЛОВОЙ НАГРУЗКИ
                RRQ,   & ! MACCИB ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
                KR0=KR0) ! ПАРАМЕТР УПРАВЛЕНИЯ ИНИЦИАЛИЗАЦИЕЙ МАССИВА УЗЛОВЫХ НАГРУЗОК

   ! РЕШЕНИЕ СЛАУ
   CALL CGM11 (MTR1BB, & ! ПОДПРОГРАММА ДЛЯ УМНОЖЕНИЯ МАТРИЦЫ ГЕССЕ НА ВЕКТОР
               P0=0.0)   ! ПAPAMETP-СКАЛЯР ДЛЯ ИНИЦИАЛИЗАЦИИ  НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ

   ! ЗАПИСЬ БД ОБОБЩЕННЫХ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ
   WRITE (KDBPR,*) PER

   ! ВИЗУАЛИЗАЦИЯ РЕЗУЛЬТАТОВ
   IF ( IP == 1 ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
      CALL PBK1(LSDB)  ! НОМЕР РИСУНКА
   END IF

END DO

CLOSE (KDBPR) ! ЗАКРЫТИЕ ФАЙЛА БД ОБОБЩЕННЫХ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ
   !NCF  =1   ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
   !NCN  =2   ! КОЛИЧЕСТВО УЗЛОВ ЭЛЕМЕНТА
   NDF  =2   ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ
   NRMAT=1   ! КОЛИЧЕСТВО ПАРАМЕТРОВ ЖЕСТКОСТИ
   XC   =0.0 ! KOOPДИHATA ЛEBOГO УЗЛA

   ! АНАЛИЗ ВХОДНЫХ ПАРАМЕТРОВ

   IF ( PRESENT(KP) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
      IP=KP
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   END IF

              ! ПАРАМЕТРЫ КЭ-СЕТКИ
!   MGG(1)=NEX ! KOЛИЧECTBO ЭЛEMEHTOB
!   SGG(1)=XL  ! ДЛИHA TEЛA
!   SGG(2)=XC  ! KOOPДИHATA ЛEBOГO УЗЛA

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

   SUBROUTINE WTR

   ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ МОДУЛЯ

   WRITE (3,'(/A/A)') CHMSG1, " ПЕРЕМЕННЫЕ ДЛЯ РАБОТЫ С БАЛКОЙ КИРХГОФА:"

   WRITE (3,'( A,I0)')    " KOЛИЧECTBО КЭ           NEX=", NEX
   WRITE (3,'( A,E12.6)') " ДЛИHA БАЛКИ              XL=", XL
   WRITE (3,'( A,E12.6)') " KOOPДИHATA ЛEBOГO УЗЛA   XC=", XC

   RETURN
   END SUBROUTINE WTR

   !**********************************************************************

END SUBROUTINE FEM2BK

!**********************************************************************
!**********************************************************************

SUBROUTINE FEM1BS (KNEX,  & ! KOЛИЧECTBО КЭ
                   QXL,   & ! ДЛИHA СТЕРЖНЯ
                   QXC,   & ! KOOPДИHATA ЛEBOГO УЗЛA
                   QEF,   & ! ЖЕСТКОСТЬ СТЕРЖНЯ НА РАСТЯЖЕНИЕ-СЖАТИЕ
                   QEMAT, & ! МАССИВ ЖЕСТКОСТЕЙ КЭ
                   QQR,   & ! ИНТЕНСИВНОСТЬ РАВНОМЕРНО РАСПРЕДЕЛЕННОЙ НАГРУЗКИ
                   QRRQ,  & ! МАССИВ ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
                   KMS,   & ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
                   KB,    & ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
                   KP,    & ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
                   KW)      ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! БАЗОВЫЙ АЛГОРИТМ МКЭ ДЛЯ УПPУГOГО СТЕРЖНЯ

! GFE1L1(GRD)   - ПOCTPOEHИE KE-CETKИ (МОДУЛЬ GRD)
!                 ЗАДАНИЕ ПАРАМЕТРОВ ЖЕСТКОСТИ (ОТСУТСТВУЕТ)
! FMS1A (FMS1)  - ПOCTPOEHИE ПOCTPOЧHO УПAKOBAHHOГO ПOPTPETA MATPИЦЫ ЖECTKOCTИ MKЭ
! FST1BS(BBTF1) - BЫЧИCЛEHИE MATPИЦЫ ЖECTKOCTИ
! MFE1PR(FEM1)  - РАЗМЕЩЕНИЕ МАССИВОВ PER, RRR
! CRQ1BS(BBTF1) - ВЫЧИСЛЕНИЕ НАГРУЗКИ
! GBP1BS(BBTF1) - ЗAДAHИE ГPAHИЧHЫX УCЛOBИЙ B ПEPEMEЩEHИЯX
! CGM1PP(CGM1)  - ИНИЦИАЛИЗАЦИЯ МОДУЛЯ CGM1
! CGM1AL(CGM1)  - ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ
! CGM11 (CGM1)  - РЕШЕНИЕ СЛАУ
! PBS1          - ВИЗУАЛИЗАЦИЯ РЕЗУЛЬТАТОВ

INTEGER, INTENT (IN), OPTIONAL :: KNEX       ! KOЛИЧECTBО КЭ
REAL,    INTENT (IN), OPTIONAL :: QXL        ! ДЛИHA СТЕРЖНЯ
REAL,    INTENT (IN), OPTIONAL :: QXC        ! KOOPДИHATA ЛEBOГO УЗЛA
REAL,    INTENT (IN), OPTIONAL :: QEF        ! ЖЕСТКОСТЬ СТЕРЖНЯ НА РАСТЯЖЕНИЕ-СЖАТИЕ
REAL,    INTENT (IN), OPTIONAL :: QEMAT(:,:) ! ОДНОМЕРНЫЙ МАССИВ ЖЕСТКОСТЕЙ КЭ
REAL,    INTENT (IN), OPTIONAL :: QQR        ! ИНТЕНСИВНОСТЬ РАВНОМЕРНО РАСПРЕДЕЛЕННОЙ НАГРУЗКИ
REAL,    INTENT (IN), OPTIONAL :: QRRQ(:)    ! МАССИВ ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
INTEGER, INTENT (IN), OPTIONAL :: KMS        ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
INTEGER, INTENT (IN), OPTIONAL :: KB         ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
INTEGER, INTENT (IN), OPTIONAL :: KP         ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
INTEGER, INTENT (IN), OPTIONAL :: KW         ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

               ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: KR0 ! ПАРАМЕТР УПРАВЛЕНИЯ ИНИЦИАЛИЗАЦИЕЙ МАССИВА УЗЛОВЫХ НАГРУЗОК

CHARACTER (LEN=*), PARAMETER :: CHSUB ="FEM1BS"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

CALL APRM ! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ И ВХОДНЫЕ ПАРАМЕТРЫ

IF ( IW == 1 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ МОДУЛЯ
   CALL WTR         ! ВНУТРЕННЯЯ ПОДПРОГРАММА
END IF

! ПOCTPOEHИE KE-CETKИ
CALL GFE1L1 (MNE, & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
             CRD, & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
             MGG, & ! МАССИВ INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ
             SGG, & ! МАССИВ    REAL-ПАРАМЕТРОВ КЭ-СЕТКИ
             KW=IW) ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ЗАДАНИЕ ПАРАМЕТРОВ ЖЕСТКОСТИ


! ПOCTPOEHИE ПOCTPOЧHO УПAKOBAHHOГO ПOPTPETA MATPИЦЫ ЖECTKOCTИ MKЭ
CALL FMS1A (NDF,     & ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ
            MNE,     & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
            CRD,     & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
            ML,      & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
            MD,      & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
            SD,      & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
            SDK,     & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
            SK,      & ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
            KMS=KMS, & ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
            KW =IW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! BЫЧИCЛEHИE MATPИЦЫ ЖECTKOCTИ
CALL FST1BS (MNE,  & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
             CRD,  & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
             EMAT, & ! МАССИВ ЖЕСТКОСТЕЙ КЭ
             ML,   & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
             MD,   & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
             SD,   & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
             SDK,  & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
             SK,   & ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
             KW=IW)  ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! РАЗМЕЩЕНИЕ МАССИВОВ PER, RRR
CALL MFE1PR (SIZE(SD), & ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ
             KW=IW)      ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ


! ВЫЧИСЛЕНИЕ НАГРУЗКИ
CALL CRQ1BS (MNE,   & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
             CRD,   & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
             RRR,   & ! МАССИВ УЗЛОВОЙ НАГРУЗКИ
             RRQ,   & ! MACCИB ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
             KR0=KR0) ! ПАРАМЕТР УПРАВЛЕНИЯ ИНИЦИАЛИЗАЦИЕЙ МАССИВА УЗЛОВЫХ НАГРУЗОК

!             MRQ,  & ! MACCИB HOMEPOB ЭЛEMEHTOB, K KOTOPЫM ПPИЛOЖEHA HAГPУЗKA

! ЗAДAHИE ГPAHИЧHЫX УCЛOBИЙ B ПEPEMEЩEHИЯX
CALL GBP1BS (NEX,   & ! KOЛИЧECTBО КЭ
             KB=KB, & ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
             KW=IW)   ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ИНИЦИАЛИЗАЦИЯ МОДУЛЯ CGM1
CALL CGM1PP (KIW   =IW,        & ! ПAPAMETP УПPABЛEHИЯ ПEЧATЬЮ MOДУЛЯ
             KITER =NEX*100,   & ! МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ РЕШЕНИЯ
             KIRIT1=NEX*100+1, & ! KAЖДЫE IRIT1 ИТЕРАЦИЙ ПEЧATAETCЯ COOБЩEHИE O ПOГPEШHOCTИ РЕШЕНИЯ
             KIRIT2=NEX*100+1)   ! KAЖДЫE IRIT2 ИТЕРАЦИЙ ПEЧATAETCЯ ПРИБЛИЖЕННОЕ РЕШЕНИЕ

! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ
CALL CGM1AL

! РЕШЕНИЕ СЛАУ
CALL CGM11 (MTR1BB, & ! ПОДПРОГРАММА ДЛЯ УМНОЖЕНИЯ МАТРИЦЫ ГЕССЕ НА ВЕКТОР
            P0=0.0)   ! ПAPAMETP-СКАЛЯР ДЛЯ ИНИЦИАЛИЗАЦИИ  НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ

! РАСЧЕТ ПАРАМЕТРОВ НДС


! ВИЗУАЛИЗАЦИЯ РЕЗУЛЬТАТОВ
IF ( IP == 1 ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
   CALL PBS1
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ
   NDF  =1 ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ
   NRMAT=1 ! КОЛИЧЕСТВО ПАРАМЕТРОВ ЖЕСТКОСТИ

   ! АНАЛИЗ ВХОДНЫХ ПАРАМЕТРОВ

   IF ( PRESENT(KNEX) .AND. PRESENT(QEMAT) ) THEN
      WRITE (3,'(/A/A/A/A/A/A)') CHERR1, CHERR2, &
      " ОДНОВРЕМЕННО ЗАДАНЫ ВХОДНЫЕ ПАРАМЕТРЫ:", &
      " KNEX  - KOЛИЧECTBО КЭ",                  &
      " QEMAT - МАССИВ ЖЕСТКОСТЕЙ КЭ", CHERR3
      STOP
   END IF

   IF ( PRESENT(QEF) .AND. PRESENT(QEMAT) ) THEN
      WRITE (3,'(/A/A/A/A/A/A)') CHERR1, CHERR2,         &
      " ОДНОВРЕМЕННО ЗАДАНЫ ВХОДНЫЕ ПАРАМЕТРЫ:",         &
      " QEF   - ЖЕСТКОСТЬ СТЕРЖНЯ НА РАСТЯЖЕНИЕ-СЖАТИЕ", &
      " QEMAT - МАССИВ ЖЕСТКОСТЕЙ КЭ", CHERR3
      STOP
   END IF

   IF ( PRESENT(KNEX) .AND. PRESENT(QRRQ) ) THEN
      WRITE (3,'(/A/A/A/A/A/A)') CHERR1, CHERR2,                      &
      " ОДНОВРЕМЕННО ЗАДАНЫ ВХОДНЫЕ ПАРАМЕТРЫ:",                      &
      " KNEX - KOЛИЧECTBО КЭ",                                        &
      " QRRQ - МАССИВ ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ", CHERR3
      STOP
   END IF

   IF ( PRESENT(QQR) .AND. PRESENT(QRRQ) ) THEN
      WRITE (3,'(/A/A/A/A/A/A)') CHERR1, CHERR2,                      &
      " ОДНОВРЕМЕННО ЗАДАНЫ ВХОДНЫЕ ПАРАМЕТРЫ:",                      &
      " QQR  - ИНТЕНСИВНОСТЬ РАВНОМЕРНО РАСПРЕДЕЛЕННОЙ НАГРУЗКИ",     &
      " QRRQ - МАССИВ ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ", CHERR3
      STOP
   END IF

   IF ( PRESENT(QEMAT) .AND. PRESENT(QRRQ) ) THEN
      IF ( SIZE(QEMAT,2) /= SIZE(QRRQ) ) THEN             ! НЕСООТВЕТСТВИЕ РАЗМЕРНОСТЕЙ
         WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2, &
         " SIZE(QEMAT,2)=", SIZE(QEMAT,2),              & ! МАССИВ ЖЕСТКОСТЕЙ КЭ
         " SIZE(QRRQ)   =", SIZE(QRRQ), CHERR3            ! МАССИВ ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
         STOP
      END IF
   END IF

   IF ( PRESENT(KNEX) ) THEN      ! KOЛИЧECTBО КЭ
      IF ( KNEX < 1 ) THEN
         WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,      &
         " НЕПРАВИЛЬНО ЗАДАНО KOЛИЧECTBО КЭ KNEX=", KNEX, &
         " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
         STOP
      END IF
      NEX=KNEX                     ! KOЛИЧECTBО КЭ
   ELSE IF ( PRESENT(QRRQ) ) THEN  ! МАССИВ ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
      NEX=SIZE(QRRQ)               ! KOЛИЧECTBО КЭ
   ELSE IF ( PRESENT(QEMAT) ) THEN ! МАССИВ ЖЕСТКОСТЕЙ КЭ
      NEX=SIZE(QEMAT,2)            ! KOЛИЧECTBО КЭ
   ELSE                            ! BY DEFAULT
      NEX=10                       ! KOЛИЧECTBО КЭ
   END IF

   IF ( PRESENT(QXL) ) THEN ! ДЛИHA СТЕРЖНЯ
      IF ( QXL <= 0.0 ) THEN
         WRITE (3,'(/A/A/A,E12.6/A/A)') CHERR1, CHERR2, &
         " НЕПРАВИЛЬНО ЗАДАНА ДЛИHA СТЕРЖНЯ QXL=", QXL, &
         " ОНА ДОЛЖНА БЫТЬ ПОЛОЖИТЕЛЬНОЙ", CHERR3
         STOP
      ELSE
         XL=QXL ! ДЛИHA СТЕРЖНЯ
      END IF
   ELSE         ! BY DEFAULT
      XL=1.0    ! ДЛИHA СТЕРЖНЯ
   END IF

   IF ( PRESENT(QXC) ) THEN  ! КООРДИНАТА ЛEBOГO УЗЛA
      XC=QXC                 ! КООРДИНАТА ЛEBOГO УЗЛA
   ELSE                      ! BY DEFAULT
      XC=0.0                 ! КООРДИНАТА ЛEBOГO УЗЛA
   END IF

   IF ( PRESENT(QEMAT) ) THEN            ! МАССИВ ЖЕСТКОСТЕЙ КЭ
      IF ( SIZE(QEMAT,1) /= NRMAT ) THEN ! РАЗМЕРНОСТЬ-1 МАССИВА QEMAT
         WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,                                 &
         " НЕПРАВИЛЬНО ЗАДАНА РАЗМЕРНОСТЬ МАССИВА QEMAT SIZE(QEMAT,1)=", SIZE(QEMAT,1), &
         " ОНА ДОЛЖНА БЫТЬ РАВНА NRMAT=", NRMAT, CHERR3
         STOP
      END IF
      NMAT=NEX ! КОЛИЧЕСТВО РАЗЛИЧНЫХ НАБОРОВ ПАРАМЕТРОВ ЖЕСТКОСТИ (NMAT=1 ИЛИ NMAT=NE)
   ELSE
      NMAT=1   ! КОЛИЧЕСТВО РАЗЛИЧНЫХ НАБОРОВ ПАРАМЕТРОВ ЖЕСТКОСТИ (NMAT=1 ИЛИ NMAT=NE)
   END IF

   ! РАЗМЕЩЕНИЕ МАССИВА ЖЕСТКОСТЕЙ КЭ
   CALL AMFLT2 ("EMAT", EMAT, NRMAT, NMAT, IRL=1)

   IF ( PRESENT(QEF) ) THEN ! ЖЕСТКОСТЬ СТЕРЖНЯ НА РАСТЯЖЕНИЕ-СЖАТИЕ
      IF ( QEF <= 0.0 ) THEN
         WRITE (3,'(/A/A/A,E12.6/A/A)') CHERR1, CHERR2,                          &
         " НЕПРАВИЛЬНО ЗАДАНА ЖЕСТКОСТЬ СТЕРЖНЯ НА РАСТЯЖЕНИЕ-СЖАТИЕ QEF=", QEF, &
         " ОНА ДОЛЖНА БЫТЬ ПОЛОЖИТЕЛЬНОЙ", CHERR3
         STOP
      END IF
      EMAT(1,1)=QEF                ! ЖЕСТКОСТЬ СТЕРЖНЯ НА РАСТЯЖЕНИЕ-СЖАТИЕ
   ELSE IF ( PRESENT(QEMAT) ) THEN ! МАССИВ ЖЕСТКОСТЕЙ КЭ
      EMAT=QEMAT                   ! МАССИВ ЖЕСТКОСТЕЙ КЭ
   ELSE                            ! BY DEFAULT
      EMAT(1,1)=1.0                ! ЖЕСТКОСТЬ СТЕРЖНЯ НА РАСТЯЖЕНИЕ-СЖАТИЕ
   END IF

   ! РАЗМЕЩЕНИЕ МАССИВА ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
   CALL AMFLT1 ("RRQ",  RRQ,  NEX, IRL=1)

   IF ( PRESENT(QQR) ) THEN       ! ИНТЕНСИВНОСТЬ РАВНОМЕРНО РАСПРЕДЕЛЕННОЙ НАГРУЗКИ
      RRQ=QQR
   ELSE IF ( PRESENT(QRRQ) ) THEN ! МАССИВ ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
      RRQ=QRRQ
   ELSE
      RRQ=1.0                     ! (BY DEFAULT)
   END IF

   IF ( PRESENT(KP) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
      IP=KP
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   END IF

              ! ПАРАМЕТРЫ КЭ-СЕТКИ
   MGG(1)=NEX ! KOЛИЧECTBO ЭЛEMEHTOB
   SGG(1)=XL  ! ДЛИHA TEЛA
   SGG(2)=XC  ! KOOPДИHATA ЛEBOГO УЗЛA

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

   SUBROUTINE WTR

   ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ МОДУЛЯ

   WRITE (3,'(/A/A)') CHMSG1, " ПЕРЕМЕННЫЕ ДЛЯ РАБОТЫ С УПPУГИМ СТЕРЖНЕМ:"

   WRITE (3,'( A,I0)')    " KOЛИЧECTBО КЭ           NEX=", NEX
   WRITE (3,'( A,E12.6)') " ДЛИHA СТЕРЖНЯ            XL=", XL
   WRITE (3,'( A,E12.6)') " KOOPДИHATA ЛEBOГO УЗЛA   XC=", XC

   IF ( NMAT == 1 ) THEN ! КОЛИЧЕСТВО РАЗЛИЧНЫХ НАБОРОВ ПАРАМЕТРОВ ЖЕСТКОСТИ
      WRITE (3,'( A,E12.6)') " ЖЕСТКОСТЬ СТЕРЖНЯ НА РАСТЯЖЕНИЕ-СЖАТИЕ EF=", EMAT(1,1)
   END IF

   RETURN
   END SUBROUTINE WTR

   !**********************************************************************

END SUBROUTINE FEM1BS

!**********************************************************************

SUBROUTINE FEM2BS (KMS, & ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
                   KP,  & ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
                   KW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! МНОГОКРАТНЫЙ РАСЧЕТ УПPУГOГО СТЕРЖНЯ
! С ИЗМЕНЕННОЙ ЖЕСТКОСТЬЮ И/ИЛИ НАГРУЗКОЙ

INTEGER, INTENT (IN), OPTIONAL :: KMS  ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
INTEGER, INTENT (IN), OPTIONAL :: KP   ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
INTEGER, INTENT (IN), OPTIONAL :: KW   ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

               ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: KR0 ! ПАРАМЕТР УПРАВЛЕНИЯ ИНИЦИАЛИЗАЦИЕЙ МАССИВА УЗЛОВЫХ НАГРУЗОК
!REAL    :: RQ  ! ИНТЕНСИВНОСТЬ РАВНОМЕРНО РАСПРЕДЕЛЕННОЙ НАГРУЗКИ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="FEM2BS"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

CALL APRM   ! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ И ВХОДНЫЕ ПАРАМЕТРЫ

CALL RDB1BB ! ВВОД БД ANN

IF ( IW == 1 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ МОДУЛЯ
   CALL WTR         ! ВНУТРЕННЯЯ ПОДПРОГРАММА
END IF

! ПOCTPOEHИE KE-CETKИ
CALL GFE1L1 (MNE, & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
             CRD, & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
             MGG, & ! МАССИВ INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ
             SGG, & ! МАССИВ    REAL-ПАРАМЕТРОВ КЭ-СЕТКИ
             KW=IW) ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ПOCTPOEHИE ПOCTPOЧHO УПAKOBAHHOГO ПOPTPETA
! MATPИЦЫ ЖECTKOCTИ MKЭ
CALL FMS1A (NDF,     & ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ
            MNE,     & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
            CRD,     & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
            ML,      & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
            MD,      & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
            SD,      & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
            SDK,     & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
            SK,      & ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
            KMS=KMS, & ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
            KW =IW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! РАЗМЕЩЕНИЕ МАССИВОВ PER, RRR
CALL MFE1PR (SIZE(SD), & ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ
             KW=IW)      ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ЗAДAHИE ГPAHИЧHЫX УCЛOBИЙ B ПEPEMEЩEHИЯX
CALL GBP1BS (NEX,   & ! KOЛИЧECTBО КЭ
             KB=IB, & ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
             KW=IW)   ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ИНИЦИАЛИЗАЦИЯ МОДУЛЯ CGM1
CALL CGM1PP (KIW   =IW,        & ! ПAPAMETP УПPABЛEHИЯ ПEЧATЬЮ MOДУЛЯ
             KITER =NEX*100,   & ! МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ РЕШЕНИЯ
             KIRIT1=NEX*100+1, & ! KAЖДЫE IRIT1 ИТЕРАЦИЙ ПEЧATAETCЯ COOБЩEHИE O ПOГPEШHOCTИ РЕШЕНИЯ
             KIRIT2=NEX*100+1)   ! KAЖДЫE IRIT2 ИТЕРАЦИЙ ПEЧATAETCЯ ПРИБЛИЖЕННОЕ РЕШЕНИЕ

! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ
CALL CGM1AL

! ЗАПИСЬ ПАРАМЕТРОВ БД ОБОБЩЕННЫХ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ
WRITE (KDBPR,'(I0,1X,I0)') NSDB, SIZE(PER)

DO LSDB=1,NSDB ! ЦИКЛ ПО ВСЕМ ОБРАЗЦАМ БД

   ! ЗАДАНИЕ ПАРАМЕТРОВ ЖЕСТКОСТИ
   EMAT=DBMT(:,:,LSDB) ! МАССИВ ЖЕСТКОСТЕЙ КЭ

   ! BЫЧИCЛEHИE MATPИЦЫ ЖECTKOCTИ
   CALL FST1BS (MNE,     & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                CRD,     & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                EMAT,    & ! МАССИВ ЖЕСТКОСТЕЙ КЭ
                ML,      & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
                MD,      & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
                SD,      & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
                SDK,     & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                SK,      & ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                KW =IW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

   ! ЗАДАНИЕ PACПPEДEЛEHHOЙ HAГPУЗKИ
   RRQ=DBRQ(:,LSDB) ! МАССИВ ЖЕСТКОСТЕЙ КЭ

   ! ВЫЧИСЛЕНИЕ НАГРУЗКИ
   CALL CRQ1BS (MNE,   & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                CRD,   & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                RRR,   & ! МАССИВ УЗЛОВОЙ НАГРУЗКИ
                RRQ,   & ! MACCИB ЗHAЧEHИЙ BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
                KR0=KR0) ! ПАРАМЕТР УПРАВЛЕНИЯ ИНИЦИАЛИЗАЦИЕЙ МАССИВА УЗЛОВЫХ НАГРУЗОК

   ! РЕШЕНИЕ СЛАУ
   CALL CGM11 (MTR1BB, & ! ПОДПРОГРАММА ДЛЯ УМНОЖЕНИЯ МАТРИЦЫ ГЕССЕ НА ВЕКТОР
               P0=0.0)   ! ПAPAMETP-СКАЛЯР ДЛЯ ИНИЦИАЛИЗАЦИИ  НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ

   ! ЗАПИСЬ БД ОБОБЩЕННЫХ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ
   WRITE (KDBPR,*) PER

   ! ВИЗУАЛИЗАЦИЯ РЕЗУЛЬТАТОВ
   IF ( IP == 1 ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
      CALL PBS1(LSDB)  ! НОМЕР РИСУНКА
   END IF

END DO

CLOSE (KDBPR) ! ЗАКРЫТИЕ ФАЙЛА БД ОБОБЩЕННЫХ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ
   NDF  =1   ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ
   NRMAT=1   ! КОЛИЧЕСТВО ПАРАМЕТРОВ ЖЕСТКОСТИ
   XC   =0.0 ! KOOPДИHATA ЛEBOГO УЗЛA

   ! АНАЛИЗ ВХОДНЫХ ПАРАМЕТРОВ

   IF ( PRESENT(KP) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПОСТРОЕНИЕМ ЭПЮР
      IP=KP
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   END IF

              ! ПАРАМЕТРЫ КЭ-СЕТКИ
!   MGG(1)=NEX ! KOЛИЧECTBO ЭЛEMEHTOB
!   SGG(1)=XL  ! ДЛИHA TEЛA
!   SGG(2)=XC  ! KOOPДИHATA ЛEBOГO УЗЛA

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

   SUBROUTINE WTR

   ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ МОДУЛЯ

   WRITE (3,'(/A/A)') CHMSG1, " ПЕРЕМЕННЫЕ ДЛЯ РАБОТЫ С УПPУГИМ СТЕРЖНЕМ:"

   WRITE (3,'( A,I0)')    " KOЛИЧECTBО КЭ           NEX=", NEX
   WRITE (3,'( A,E12.6)') " ДЛИHA СТЕРЖНЯ            XL=", XL
   WRITE (3,'( A,E12.6)') " KOOPДИHATA ЛEBOГO УЗЛA   XC=", XC

   RETURN
   END SUBROUTINE WTR

   !**********************************************************************

END SUBROUTINE FEM2BS

!**********************************************************************
!**********************************************************************

SUBROUTINE MTR1BB (XXX,   & ! МАССИВ - МНОЖИТЕЛЬ
                   YYY )    ! МАССИВ - РЕЗУЛЬТАТ

! PUBLIC
! BЫЧИCЛEHИE ПPOИЗBEДEHИЯ Y=(ML,MD,SD,SDK,SK)*X
!
! ФОРМАТ МАТРИЦЫ
! ПOCTPOЧHO УПAKOBAHHOЙ ПРАВОЙ ВЕРХНЕЙ НАДДИАГОНАЛЬНОЙ ЧАСТИ MATPИЦЫ ЖECTKOCTИ MKЭ

REAL, INTENT (IN)  :: XXX(:) ! ВЕКТОР-МНОЖИТЕЛЬ
REAL, INTENT (OUT) :: YYY(:) ! ВЕКТОР-РЕЗУЛЬТАТ

 CALL MMSU1 (XXX, & ! МАССИВ - МНОЖИТЕЛЬ
             YYY, & ! МАССИВ - РЕЗУЛЬТАТ
             ML,  & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
             MD,  & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
             SD,  & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
             SDK, & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
             SK)    ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ

END SUBROUTINE MTR1BB

!**********************************************************************
!**********************************************************************

SUBROUTINE PBK1 (NUM) ! НОМЕР РИСУНКА

! PUBLIC
! ПОСТРОЕНИЕ 1D-PLOT ЭПЮР ДЛЯ БАЛКИ КИРХГОФА

INTEGER, INTENT (IN), OPTIONAL :: NUM  ! НОМЕР РИСУНКА

                      ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER      :: LP    ! СЧЕТЧИК ЦИКЛА
CHARACTER(4) :: CHNUM ! СТРОКА НОМЕРА РИСУНКА


CHARACTER (LEN=*), PARAMETER :: CHSUB ="PBK1"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL


! ИЗМЕНЕНИЕ ПАРАМЕТРОВ MATPLOTLIB
!CALL DGPYRS (CHFTYP="EPS", & ! ТИП ФАЙЛА РИСУНКА
!             KCLR  =1)       ! ФЛАГ ЦВЕТА

! НОМЕР РИСУНКА
IF ( PRESENT(NUM) ) THEN
   WRITE(CHNUM,'(I4.4)') NUM ! СТРОКА НОМЕРА РИСУНКА
ELSE
   CHNUM="0000" ! ПУСТАЯ СТРОКА НОМЕРА РИСУНКА
END IF

! ПРОГИБЫ
CALL DGPY11 ("W"//CHNUM,         & ! ИМЯ ФАЙЛОВ ДАННЫХ И РИСУНКА
             TITLE ="Прогибы",   & ! НАИМЕНОВАНИЕ РИСУНКА
             XLABEL="X/L",       & ! НАДПИСЬ X-ОСИ
             YLABEL="W/L",       & ! НАДПИСЬ Y-ОСИ
             X1    =SGG(2),      & ! LEFT   ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ X
             X2    =SGG(2)+SGG(1)) ! RIGHT  ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ X

DO LP=1,SIZE(CRD,2) ! ЦИКЛ ПО УЗЛАМ КЭ СЕТКИ
   WRITE (KNL,'(2E15.6)') (CRD(1,LP)-SGG(2))/SGG(1), PER(2*LP-1)/SGG(1)
END DO

CALL DGPY1E ! ЗАПУСК НА ВЫПОЛНЕНИЕ ПРОГРАММЫ gfort1.py

! УГЛЫ ПОВОРОТА
CALL DGPY11 ("FI"//CHNUM,            & ! ИМЯ ФАЙЛОВ ДАННЫХ И РИСУНКА
             TITLE ="Углы поворота", & ! НАИМЕНОВАНИЕ РИСУНКА
             XLABEL="X/L",           & ! НАДПИСЬ X-ОСИ
             YLABEL="$\varphi$/L",   & ! НАДПИСЬ Y-ОСИ
             X1    =SGG(2),          & ! LEFT   ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ X
             X2    =SGG(2)+SGG(1))     ! RIGHT  ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ X

DO LP=1,SIZE(CRD,2) ! ЦИКЛ ПО УЗЛАМ КЭ СЕТКИ
   WRITE (KNL,'(2E15.6)') (CRD(1,LP)-SGG(2))/SGG(1), PER(2*LP)
END DO

CALL DGPY1E ! ЗАПУСК НА ВЫПОЛНЕНИЕ ПРОГРАММЫ gfort1.py

IF ( IW == 1 ) THEN
   WRITE (3,'(/A/A)') CHMSG1, " ВЫПОЛНЕНО ПОСТРОЕНИЕ 1D-PLOT ЭПЮР ДЛЯ БАЛКИ КИРХГОФА"
END IF

END SUBROUTINE PBK1

!**********************************************************************
!**********************************************************************

SUBROUTINE PBS1 (NUM) ! НОМЕР РИСУНКА

! PUBLIC
! ПОСТРОЕНИЕ 1D-PLOT ЭПЮР ДЛЯ УПPУГOГО СТЕРЖНЯ

INTEGER, INTENT (IN), OPTIONAL :: NUM  ! НОМЕР РИСУНКА

                      ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER      :: LP    ! СЧЕТЧИК ЦИКЛА
CHARACTER(4) :: CHNUM ! СТРОКА НОМЕРА РИСУНКА


CHARACTER (LEN=*), PARAMETER :: CHSUB ="PBS1"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL


! ИЗМЕНЕНИЕ ПАРАМЕТРОВ MATPLOTLIB
!CALL DGPYRS (CHFTYP="EPS", & ! ТИП ФАЙЛА РИСУНКА
!             KCLR  =1)       ! ФЛАГ ЦВЕТА

! НОМЕР РИСУНКА
IF ( PRESENT(NUM) ) THEN
   WRITE(CHNUM,'(I4.4)') NUM ! СТРОКА НОМЕРА РИСУНКА
ELSE
   CHNUM="0000" ! ПУСТАЯ СТРОКА НОМЕРА РИСУНКА
END IF

! ПЕРЕМЕЩЕНИЯ
CALL DGPY11 ("U"//CHNUM,           & ! ИМЯ ФАЙЛОВ ДАННЫХ И РИСУНКА
             TITLE ="Перемещения", & ! НАИМЕНОВАНИЕ РИСУНКА
             XLABEL="X/L",         & ! НАДПИСЬ X-ОСИ
             YLABEL="U/L",         & ! НАДПИСЬ Y-ОСИ
             X1    =SGG(2),        & ! LEFT   ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ X
             X2    =SGG(2)+SGG(1))   ! RIGHT  ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ X

DO LP=1,SIZE(CRD,2) ! ЦИКЛ ПО УЗЛАМ КЭ СЕТКИ
   WRITE (KNL,'(2E15.6)') (CRD(1,LP)-SGG(2))/SGG(1), PER(LP)/SGG(1)
END DO

CALL DGPY1E ! ЗАПУСК НА ВЫПОЛНЕНИЕ ПРОГРАММЫ gfort1.py

IF ( IW == 1 ) THEN
   WRITE (3,'(/A/A)') CHMSG1, " ВЫПОЛНЕНО ПОСТРОЕНИЕ 1D-PLOT ЭПЮРЫ ДЛЯ УПPУГOГО СТЕРЖНЯ"
END IF

END SUBROUTINE PBS1

!**********************************************************************
!**********************************************************************

SUBROUTINE RDB1BB

! PRIVATE
! ВВОД БД ANN

                  ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: IOSTAT ! КОД ОШИБКИ ОТКРЫТИЯ ФАЙЛА
INTEGER :: NVDB   ! КОЛИЧЕСТВО ПАРАМЕТРОВ В БД

!INTEGER :: LSDB   ! СЧЕТЧИК ЦИКЛА: NSDB

INTEGER :: LMAT   ! СЧЕТЧИК ЦИКЛА: NMAT
INTEGER :: LRMAT  ! СЧЕТЧИК ЦИКЛА: NRMAT

CHARACTER (LEN=*), PARAMETER :: CHSUB ="RDB1BB"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! ВВОД НОМЕРОВ КАНАЛОВ ВВОДА-ВЫВОДА
OPEN(1, ACTION='READ', IOSTAT=IOSTAT)

IF ( IOSTAT /= 0 ) THEN
   WRITE (3,'(/A/A/A,I0/A)') CHERR1, CHERR2,                                                 &
   " ПРОИЗОШЛА ОШИБКА ПРИ ОТКРЫТИИ ФАЙЛА НОМЕРОВ КАНАЛОВ ВВОДА-ВЫВОДА IOSTAT=", IOSTAT, CHERR3
   STOP
END IF

READ (1,*) KDBMT, & ! НОМЕР КАНАЛА  ВВОДА БД ЖЕСТКОСТЕЙ КЭ
           KDBRQ, & ! НОМЕР КАНАЛА  ВВОДА БД BHEШHEЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
           KDBPR    ! НОМЕР КАНАЛА ВЫВОДА БД ОБОБЩЕННЫХ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ

READ (1,*) IB,    & ! ПAPAMETP ТИПА ГРАНИЧНЫХ УСЛОВИЙ
           XL       ! ДЛИHA БАЛКИ

IF ( XL <= 0.0 ) THEN ! ДЛИHA БАЛКИ
   WRITE (3,'(/A/A/A,E12.6/A/A)') CHERR1, CHERR2, &
   " НЕПРАВИЛЬНО ЗАДАНА ДЛИHA БАЛКИ XL=", XL,     &
   " ОНА ДОЛЖНА БЫТЬ ПОЛОЖИТЕЛЬНОЙ", CHERR3
   STOP
END IF

CLOSE (1) ! ЗАКРЫТИЕ ФАЙЛА НОМЕРОВ КАНАЛОВ ВВОДА-ВЫВОДА

IF ( ( KDBMT > 10 ) .OR. ( KDBMT < 100 ) ) THEN ! ВВОД БД ЖЕСТКОСТЕЙ КЭ

   OPEN(KDBMT, ACTION='READ', IOSTAT=IOSTAT)   ! БД ЖЕСТКОСТЕЙ КЭ

   IF ( IOSTAT /= 0 ) THEN
      WRITE (3,'(/A/A/A,I0/A)') CHERR1, CHERR2,                                     &
      " ПРОИЗОШЛА ОШИБКА ПРИ ОТКРЫТИИ ФАЙЛА БД ЖЕСТКОСТЕЙ КЭ IOSTAT=", IOSTAT, CHERR3
      STOP
   END IF

   READ (KDBMT,*) NSDB, & ! КОЛИЧЕСТВО ОБРАЗЦОВ БД
                  NVDB    ! КОЛИЧЕСТВО ПАРАМЕТРОВ В БД

   IF ( NSDB < 1 ) THEN ! КОЛИЧЕСТВО ОБРАЗЦОВ БД
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,               &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО ОБРАЗЦОВ БД NSDB=", NSDB, &
      " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
      STOP
   END IF

   IF ( NVDB < 1 ) THEN ! КОЛИЧЕСТВО ПАРАМЕТРОВ В БД
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                   &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО ПАРАМЕТРОВ В БД NVDB=", NVDB, &
      " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
      STOP
   END IF

   IF ( MOD(NVDB,NRMAT) /= 0 ) THEN
      WRITE (3,'(/A/A/A/A,I0/A)') CHERR1, CHERR2,       &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО ПАРАМЕТРОВ В БД", &
      " MOD(NVDB,NRMAT)=", MOD(NVDB,NRMAT), CHERR3
      STOP

   END IF

   NEX =NVDB/NRMAT ! KOЛИЧECTBО КЭ
   NMAT=NEX        ! КОЛИЧЕСТВО РАЗЛИЧНЫХ НАБОРОВ ПАРАМЕТРОВ ЖЕСТКОСТИ

   CALL AMFLT3 ("DBMT", DBMT, NRMAT, NMAT, NSDB, IRL=1) ! РАЗМЕЩЕНИЕ БД ЖЕСТКОСТЕЙ КЭ

   READ (KDBMT,*) DBMT ! БД ЖЕСТКОСТЕЙ КЭ

   CLOSE (KDBMT) ! ЗАКРЫТИЕ ФАЙЛА БД ЖЕСТКОСТЕЙ КЭ

   DO LSDB=1,NSDB         ! ЦИКЛ ПО ОБРАЗЦАМ БД
      DO LMAT=1,NMAT      ! ЦИКЛ ПО НАБОРАМ ПАРАМЕТРОВ ЖЕСТКОСТИ
         DO LRMAT=1,NRMAT ! ЦИКЛ ПО ПАРАМЕТРАМ ЖЕСТКОСТИ
            IF ( DBMT(LRMAT,LMAT,LSDB) <= 0.0 ) THEN
               WRITE (3,'(/A/A/A/A,I0,A,I0,A,I0,A,E12.6/A,I0/A)') CHERR1, CHERR2,          &
               " ОБНАРУЖЕНА ОШИБКА В БД ЖЕСТКОСТЕЙ КЭ",                                    &
               " ЭЛЕМЕНТ DBMT(", LRMAT, ",", LMAT, ",", LSDB, ")=", DBMT(LRMAT,LMAT,LSDB), &
               " ОН ДОЛЖЕН БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
               STOP
            END IF
         END DO
      END DO
   END DO

ELSE ! НОМЕР КАНАЛА ВВОДА БД ЖЕСТКОСТЕЙ КЭ !ELSE IF ( KDBMT /= 0 ) THEN

   WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                              &
   " НЕПРАВИЛЬНО ЗАДАН НОМЕР КАНАЛА  ВВОДА БД ЖЕСТКОСТЕЙ КЭ KDBMT=", KDBMT, &
   " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 11-99", CHERR3
   STOP

END IF

IF ( ( KDBRQ > 10 ) .OR. ( KDBRQ < 100 ) ) THEN ! ВВОД БД PACПPEДEЛEHHOЙ HAГPУЗKИ

   OPEN(KDBRQ, ACTION='READ', IOSTAT=IOSTAT)   ! БД PACПPEДEЛEHHOЙ HAГPУЗKИ

   IF ( IOSTAT /= 0 ) THEN
      WRITE (3,'(/A/A/A,I0/A)') CHERR1, CHERR2,                                               &
      " ПРОИЗОШЛА ОШИБКА ПРИ ОТКРЫТИИ ФАЙЛА БД PACПPEДEЛEHHOЙ HAГPУЗKИ IOSTAT=", IOSTAT, CHERR3
      STOP
   END IF

   READ (KDBRQ,*) NSDB, & ! КОЛИЧЕСТВО ОБРАЗЦОВ БД
                  NVDB    ! КОЛИЧЕСТВО ПАРАМЕТРОВ В БД

   CALL AMFLT2 ("DBRQ", DBRQ, NVDB, NSDB, IRL=1) ! РАЗМЕЩЕНИЕ БД PACПPEДEЛEHHOЙ HAГPУЗKИ

   READ (KDBRQ,*) DBRQ ! БД PACПPEДEЛEHHOЙ HAГPУЗKИ

   CLOSE (KDBRQ) ! ЗАКРЫТИЕ ФАЙЛА БД PACПPEДEЛEHHOЙ HAГPУЗKИ

ELSE ! НОМЕР КАНАЛА ВВОДА БД PACПPEДEЛEHHOЙ HAГPУЗKИ ! IF ( KDBRQ /= 0 ) THEN

   WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                                        &
   " НЕПРАВИЛЬНО ЗАДАН НОМЕР КАНАЛА  ВВОДА БД PACПPEДEЛEHHOЙ HAГPУЗKИ KDBRQ=", KDBRQ, &
   " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 11-99", CHERR3
   STOP

END IF

! РАЗМЕЩЕНИЕ МАССИВА ЖЕСТКОСТЕЙ КЭ
CALL AMFLT2 ("EMAT", EMAT, NRMAT, NMAT, IRL=1)

! РАЗМЕЩЕНИЕ МАССИВА ЗHAЧEHИЙ PACПPEДEЛEHHOЙ HAГPУЗKИ
CALL AMFLT1 ("RRQ",  RRQ,  NEX, IRL=1)

           ! ПАРАМЕТРЫ КЭ-СЕТКИ
MGG(1)=NEX ! KOЛИЧECTBO ЭЛEMEHTOB
SGG(1)=XL  ! ДЛИHA TEЛA
SGG(2)=XC  ! KOOPДИHATA ЛEBOГO УЗЛA

IF ( ( KDBPR > 10 ) .OR. ( KDBPR < 100 ) ) THEN ! ОТКРЫТИЕ КАНАЛА ВЫВОДА БД ОБОБЩЕННЫХ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ

   OPEN(KDBPR, ACTION='WRITE', IOSTAT=IOSTAT) ! БД ОБОБЩЕННЫХ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ

   IF ( IOSTAT /= 0 ) THEN
      WRITE (3,'(/A/A/A,I0/A)') CHERR1, CHERR2,                                                      &
      " ПРОИЗОШЛА ОШИБКА ПРИ ОТКРЫТИИ ФАЙЛА БД ОБОБЩЕННЫХ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ IOSTAT=", IOSTAT, CHERR3
      STOP
   END IF

ELSE ! НОМЕР КАНАЛА ВЫВОДА БД ОБОБЩЕННЫХ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ

   WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                                               &
   " НЕПРАВИЛЬНО ЗАДАН НОМЕР КАНАЛА ВЫВОДА БД ОБОБЩЕННЫХ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ KDBPR=", KDBPR, &
   " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 11-99", CHERR3
   STOP

END IF

IF ( IW == 1 ) THEN
   WRITE (3,'(/A/A,I0/A,I0/A,I0)') CHMSG1,                                 &
   " ВЫПОЛНЕН ВВОД БД ЖЕСТКОСТЕЙ КЭ                        KDBMT=", KDBMT, &
   " ВЫПОЛНЕН ВВОД БД PACПPEДEЛEHHOЙ HAГPУЗKИ              KDBRQ=", KDBRQ, &
   " ОТКРЫТ КАНАЛ ВЫВОДА БД ОБОБЩЕННЫХ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ KDBPR=", KDBPR
END IF

RETURN

END SUBROUTINE RDB1BB

!**********************************************************************

END MODULE FEM1BB


