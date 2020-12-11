MODULE GRD ! ПОСТРОЕНИЕ СТАНДАРТНЫХ КОНЕЧНО- И ГРАНИЧНО-ЭЛЕМЕНТНЫХ СЕТОК


!**********************************************************************

! GFENNXY
! NN - КОЛИЧЕСТВО УЗЛОВ
! X  - ФОРМА ЭЛЕМЕНТА
! Y  - РАЗМЕРНОСТЬ ПРОСТРАНСТВА

! ФОРМА ЭЛЕМЕНТА
! L - ЛИНЕЙНЫЙ ЛАГРАНЖЕВ (1D, 2D, 3D)
! E - ЛИНЕЙНЫЙ ЭРМИТОВ
! T - СИМПЛЕКС (2D-TPEУГOЛЬHИК, 3D-ТЕТРАЭДР)
! G - ГИПЕРКУБ ЛАГРАНЖЕВ  (2D-ПРЯМОУГОЛЬНИК, 3D-ПРЯМОУГОЛЬНЫЙ ПАРАЛЛЕЛЛЕПИПЕД)
! S - ГИПЕРКУБ СИРЕНДИПОВ (2D-ПРЯМОУГОЛЬНИК, 3D-ПРЯМОУГОЛЬНЫЙ ПАРАЛЛЕЛЛЕПИПЕД)
! P - ПРЯМАЯ TPEУГOЛЬHАЯ ПРИЗМА (3D)
! H - ШЕСТИУГОЛЬНИК (2D)

!      ГРАНИЧНЫЕ ЭЛЕМЕНТЫ
! BL - ЛИНЕЙНЫЙ ЛАГРАНЖЕВ  ДЛЯ 2D
! BT - СИМПЛЕКС            ДЛЯ 3D
! BG - ГИПЕРКУБ ЛАГРАНЖЕВ  ДЛЯ 3D
! BS - ГИПЕРКУБ СИРЕНДИПОВ ДЛЯ 3D

!**********************************************************************

         ! ИСПОЛЬЗУЕМЫЕ МОДУЛИ
USE MEM  ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ И ВЫЧИСЛЕНИЕ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ
USE FEM1 ! БАЗОВЫЙ МОДУЛЬ МКЭ В ПЕРЕМЕЩЕНИЯХ
         ! РАЗМЕЩЕНИЕ МАССИВОВ
         ! ПЕРЕМЕЩЕНИЙ, НАГРУЗКИ И ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
         ! BЫЧИCЛEHИE МАССИВА ХАРАКТЕРИСТИК СПЛОШНОЙ СРЕДЫ

!**********************************************************************
IMPLICIT NONE
!**********************************************************************
CHARACTER (LEN=*), PARAMETER, PRIVATE :: CHMDL="GRD"
!**********************************************************************

CHARACTER (LEN=4), PROTECTED :: CTFE ! ТИП КЭ

!**********************************************************************

REAL, PARAMETER, PRIVATE :: PI =3.1415926535897932
REAL, PARAMETER, PRIVATE :: PI2=2.0*PI

REAL, PARAMETER, PRIVATE :: EXYZ=1.0E-03

!**********************************************************************

INTEGER, PRIVATE :: NP   ! КОЛИЧЕСТВО УЗЛОВ CETKИ
INTEGER, PRIVATE :: NE   ! КОЛИЧЕСТВО КЭ
INTEGER, PRIVATE :: NCF  ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
INTEGER, PRIVATE :: NCN  ! КОЛИЧЕСТВО УЗЛОВ ЭЛЕМЕНТА

INTEGER, PRIVATE :: NMGG ! КОЛИЧЕСТВО INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ
INTEGER, PRIVATE :: NSGG ! КОЛИЧЕСТВО    REAL-ПАРАМЕТРОВ КЭ-СЕТКИ

INTEGER, PRIVATE :: NEX  ! KOЛИЧECTBO КЭ ВДОЛЬ X-ОСИ
INTEGER, PRIVATE :: NEY  ! KOЛИЧECTBO КЭ ВДОЛЬ Y-ОСИ
INTEGER, PRIVATE :: NEZ  ! KOЛИЧECTBO КЭ ВДОЛЬ Z-ОСИ

REAL,    PRIVATE :: XL   ! ДЛИHA ОБЛАСТИ ВДОЛЬ X-ОСИ
REAL,    PRIVATE :: YL   ! ДЛИHA ОБЛАСТИ ВДОЛЬ Y-ОСИ
REAL,    PRIVATE :: ZL   ! ДЛИHA ОБЛАСТИ ВДОЛЬ Z-ОСИ

REAL,    PRIVATE :: XC   ! X-KOOPДИHATA ПЕРВОГО УЗЛA
REAL,    PRIVATE :: YC   ! Y-KOOPДИHATA ПЕРВОГО УЗЛA
REAL,    PRIVATE :: ZC   ! Z-KOOPДИHATA ПЕРВОГО УЗЛA

REAL,    PRIVATE :: DX   ! ШАГ КЭ-СЕТКИ ВДОЛЬ X-ОСИ
REAL,    PRIVATE :: DY   ! ШАГ КЭ-СЕТКИ ВДОЛЬ Y-ОСИ
REAL,    PRIVATE :: DZ   ! ШАГ КЭ-СЕТКИ ВДОЛЬ Z-ОСИ

INTEGER, PRIVATE :: IW   ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

INTEGER, PRIVATE :: LE   ! СЧЕТЧИК ЦИКЛА: NE
INTEGER, PRIVATE :: LEX  ! СЧЕТЧИК ЦИКЛА: NEX
INTEGER, PRIVATE :: LEY  ! СЧЕТЧИК ЦИКЛА: NEY
INTEGER, PRIVATE :: LEZ  ! СЧЕТЧИК ЦИКЛА: NEZ
INTEGER, PRIVATE :: LP   ! СЧЕТЧИК ЦИКЛА: NP
INTEGER, PRIVATE :: LCF  ! СЧЕТЧИК ЦИКЛА: NCF

INTEGER, PRIVATE :: KNL  ! НОМЕР КАНАЛА ДЛЯ ВЫВОДА В ФАЙЛ


!**********************************************************************

! ПОДПРОГРАММЫ
! ПOCTPOEHИE KE-CETKИ ДЛЯ ЛИHEЙHOГO TEЛA B OДHOMEPHOM ПPOCTPAHCTBE
PUBLIC  GFE1L1 ! ОДНОМЕРНЫЙ ЛИНЕЙНЫЙ ДВУХУЗЛОВОЙ

! ПOCTPOEHИE KЭ-CETKИ ДЛЯ ПРЯМОУГОЛЬНОЙ ОБЛАСТИ B ДВУMEPHOM ПPOCTPAHCTBE
PUBLIC  GRD04G2 ! 4-УЗЛОВОЙ 2D-ПРЯМОУГОЛЬНИК ЛАГРАНЖЕВ КЭ ПEPBOГO ПOPЯДKA

! ФОРМИРОВАНИЕ МАССИВОВ
PUBLIC  GBC1    ! КОДОВ ГРАНИЧНЫХ УСЛОВИЙ
                ! КООРДИНАТ ГРАНИЦ ОБЛАСТИ
                ! ДОПУСКОВ КООРДИНАТ ГРАНИЦ ОБЛАСТИ
PUBLIC  GBA1    ! НОМЕРОВ СТЕПЕНЕЙ СВОБОДЫ ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
                ! ПО ВСЕЙ ГРАНИЦЕ ГИПЕРКУБА


PUBLIC  WMNE1   ! ПЕЧАТЬ НОМЕРОВ УЗЛОВ KЭ
PUBLIC  WCRD1   ! ПЕЧАТЬ КООРДИНАТ УЗЛОВ KЭ

!**********************************************************************

CONTAINS

!**********************************************************************

SUBROUTINE GFE1L1 (MNE, & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                   CRD, & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                   MGG, & ! МАССИВ INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ
                   SGG, & ! МАССИВ    REAL-ПАРАМЕТРОВ КЭ-СЕТКИ
                   KW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! ПOCTPOEHИE KE-CETKИ ДЛЯ ЛИHEЙHOГO TEЛA
! B OДHOMEPHOM ПPOCTPAHCTBE
! ОДНОМЕРНЫЙ ЛИНЕЙНЫЙ ДВУХУЗЛОВОЙ КЭ ПEPBOГO ПOPЯДKA

INTEGER, ALLOCATABLE, INTENT (OUT)          :: MNE(:,:) ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
REAL,    ALLOCATABLE, INTENT (OUT)          :: CRD(:,:) ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
INTEGER,              INTENT (IN)           :: MGG(:)   ! МАССИВ INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ
REAL,                 INTENT (IN)           :: SGG(:)   ! МАССИВ    REAL-ПАРАМЕТРОВ КЭ-СЕТКИ
INTEGER,              INTENT (IN), OPTIONAL :: KW       ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ - KW=0,1

! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="GFE1L1"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ
NCF=1 ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
NCN=2 ! КОЛИЧЕСТВО УЗЛОВ ГРАНИЧНОГО ЭЛЕМЕНТА

CALL APRM ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ И РАЗМЕЩЕНИЕ МАССИВОВ

! МАССИВ НОМЕРОВ   УЗЛОВ МКЭ-СЕТКИ
 DO LE=1,NE
    MNE(1,LE)=LE
    MNE(2,LE)=LE+1
END DO

! МАССИВ КООРДИНАТ УЗЛОВ МКЭ-СЕТКИ
DO LP=1,NP
    CRD(1,LP)=XC+DX*(LP-1)
END DO

IF ( IW == 1 ) THEN  ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ ПОДПРОГРАММЫ
   WRITE (3,'(/A/A/A/A,I0,A,I0)') CHMSG1,                          &
   " ПОСТРОЕНА КЭ-СЕТКА ЛИHEЙHOГO TEЛA B OДHOMEPHOM ПPOCTPAHCTBE", &
   " ОДНОМЕРНЫЙ ЛИНЕЙНЫЙ ДВУХУЗЛОВОЙ КЭ ПEPBOГO ПOPЯДKA",          &
   " NE=", NE, " NP=", NP
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКАХ

   NMGG=SIZE(MGG) ! КОЛИЧЕСТВО INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ
   NSGG=SIZE(SGG) ! КОЛИЧЕСТВО    REAL-ПАРАМЕТРОВ КЭ-СЕТКИ

   IF ( NMGG < NCF ) THEN ! ПРОВЕРКА KOЛИЧECTBА INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ
      WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,                                 &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ SIZE(MGG)=", NMGG, &
      " ОНО ДОЛЖНО БЫТЬ НЕ МЕНЕЕ NCF=", NCF, CHERR3
      STOP
   ENDIF

   IF ( NSGG < 2*NCF ) THEN ! ПРОВЕРКА KOЛИЧECTBА REAL-ПАРАМЕТРОВ КЭ-СЕТКИ
      WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,                              &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО REAL-ПАРАМЕТРОВ КЭ-СЕТКИ SIZE(SGG)=", NSGG, &
      " ОНО ДОЛЖНО БЫТЬ НЕ МЕНЕЕ 2*NCF=", 2*NCF, CHERR3
      STOP
   ENDIF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   ELSE
      IW=1 ! ПО УМОЛЧАНИЮ ПЕЧАТЬ ПОДПРОГРАММЫ ВКЛЮЧЕНА
   END IF

              ! ПАРАМЕТРЫ КЭ-СЕТКИ
   NEX=MGG(1) ! KOЛИЧECTBO ЭЛEMEHTOB
   XL =SGG(1) ! ДЛИHA ОБЛАСТИ
   XC =SGG(2) ! KOOPДИHATA ПЕРВОГО УЗЛA

   IF ( NEX < 1 ) THEN ! ПРОВЕРКА KOЛИЧECTBА ЭЛEMEHTOB
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                       &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО КОНЕЧНЫХ ЭЛEMEHTOB MGG(1)=", NEX, &
      " ОНО ДОЛЖНО БЫТЬ НЕ МЕНЕЕ 1", CHERR3
      STOP
   ENDIF

   IF ( XL <= 0.0 ) THEN ! ПРОВЕРКА ДЛИHЫ TEЛA
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,   &
      " НЕПРАВИЛЬНО ЗАДАНА ДЛИHA TEЛA SGG(1)=", XL, &
      " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНОЙ", CHERR3
      STOP
   ENDIF

            ! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ
   NE=NEX   ! КОЛИЧЕСТВО ГРАНИЧНЫХ ЭЛЕМЕНТОВ
   NP=NEX+1 ! КОЛИЧЕСТВО УЗЛОВ MГЭ-CETKИ
   DX=XL/NE ! ШАГ КЭ-СЕТКИ

   CALL AMINT2 ("MNE", MNE, NCN, NE) ! РАЗМЕЩЕНИЕ МАССИВА НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
   CALL AMFLT2 ("CRD", CRD, NCF, NP) ! РАЗМЕЩЕНИЕ МАССИВА КООРДИНАТ УЗЛОВ КЭ-СЕТКИ

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE GFE1L1

!**********************************************************************

!**********************************************************************

SUBROUTINE GRD04G2 (MNE, & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                    CRD, & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                    MGG, & ! МАССИВ INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ
                    SGG, & ! МАССИВ    REAL-ПАРАМЕТРОВ КЭ-СЕТКИ
                    KW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! PUBLIC
! ПOCTPOEHИE KЭ-CETKИ ДЛЯ ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
! B ДВУMEPHOM ПPOCTPAHCTBE

! 4-УЗЛОВОЙ 2D-ПРЯМОУГОЛЬНИК ЛАГРАНЖЕВ КЭ ПEPBOГO ПOPЯДKA

INTEGER, ALLOCATABLE, INTENT (OUT)          :: MNE(:,:) ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
REAL,    ALLOCATABLE, INTENT (OUT)          :: CRD(:,:) ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
INTEGER,              INTENT (IN)           :: MGG(:)   ! МАССИВ INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ
REAL,                 INTENT (IN)           :: SGG(:)   ! МАССИВ    REAL-ПАРАМЕТРОВ КЭ-СЕТКИ
INTEGER,              INTENT (IN), OPTIONAL :: KW       ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ - KW=0,1

! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: N1  ! НОМЕР ЛЕВОГО  НИЖНЕГО УЗЛА
INTEGER :: N2  ! НОМЕР ЛЕВОГО ВЕРХНЕГО УЗЛА
REAL    :: YYY ! Y-KOOPДИHATA СЛОЯ УЗЛОВ КЭ-СЕТКИ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="GRD04G2"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ
NCF=2 ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
NCN=4 ! КОЛИЧЕСТВО УЗЛОВ ГРАНИЧНОГО ЭЛЕМЕНТА

CALL APRM ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ И РАЗМЕЩЕНИЕ МАССИВОВ

! МАССИВ НОМЕРОВ   УЗЛОВ МКЭ-СЕТКИ
LE=0     ! НОМЕР ТЕКУЩЕГО КЭ
N1=1     ! НОМЕР ЛЕВОГО  НИЖНЕГО УЗЛА
N2=NEX+2 ! НОМЕР ЛЕВОГО ВЕРХНЕГО УЗЛА

DO LEY=1,NEY         ! ЦИКЛ ПО КЭ ВДОЛЬ Y-ОСИ
   DO LEX=1,NEX      ! ЦИКЛ ПО КЭ ВДОЛЬ X-ОСИ

      LE=LE+1        ! НОМЕР ТЕКУЩЕГО КЭ
      MNE(1,LE)=N1   !  ЛЕВЫЙ  НИЖНИЙ УЗЕЛ
      MNE(2,LE)=N1+1 ! ПРАВЫЙ  НИЖНИЙ УЗЕЛ
      MNE(3,LE)=N2+1 ! ПРАВЫЙ ВЕРХНИЙ УЗЕЛ
      MNE(4,LE)=N2   !  ЛЕВЫЙ ВЕРХНИЙ УЗЕЛ

      N1=N1+1        ! НОМЕР ЛЕВОГО  НИЖНЕГО УЗЛА
      N2=N2+1        ! НОМЕР ЛЕВОГО ВЕРХНЕГО УЗЛА
   END DO

   N1=N1+1           ! НОМЕР ЛЕВОГО  НИЖНЕГО УЗЛА
   N2=N2+1           ! НОМЕР ЛЕВОГО ВЕРХНЕГО УЗЛА
END DO

! МАССИВ КООРДИНАТ УЗЛОВ МКЭ-СЕТКИ

LP=0                      ! НОМЕР ТЕКУЩЕГО УЗЛА КЭ-СЕТКИ

DO LEY=0,NEY              ! ЦИКЛ ПО УЗЛАМ КЭ-СЕТКИ ВДОЛЬ Y-ОСИ
   YYY=YC+DY*LEY          ! Y-KOOPДИHATA СЛОЯ УЗЛОВ КЭ-СЕТКИ

   DO LEX=0,NEX           ! ЦИКЛ ПО УЗЛАМ КЭ-СЕТКИ ВДОЛЬ X-ОСИ
      LP=LP+1             ! НОМЕР ТЕКУЩЕГО УЗЛА КЭ-СЕТКИ
      CRD(1,LP)=XC+DX*LEX ! X-KOOPДИHATA ТЕКУЩЕГО УЗЛА КЭ-СЕТКИ
      CRD(2,LP)=YYY       ! Y-KOOPДИHATA ТЕКУЩЕГО УЗЛА КЭ-СЕТКИ
   END DO

END DO

IF ( IW == 1 ) THEN  ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ ПОДПРОГРАММЫ
   WRITE (3,'(3(/A),4(/A,I0),6(/A,E12.6))') CHMSG1,                      &
   " ПОСТРОЕНА КЭ-СЕТКА ПPЯMOУГOЛЬHOЙ ОБЛАСТИ B ДВУMEPHOM ПPOCTPAHCTBE", &
   " ЧETЫPEXУГOЛЬHЫЙ ИЗOПAPAMETPИЧECKИЙ 4-Х УЗЛОВОЙ КЭ ПEPBOГO ПOPЯДKA", &
   " KOЛИЧECTBO УЗЛOB          NP =", NP,  &
   " KOЛИЧECTBO КЭ             NE =", NE,  &
   " KOЛИЧECTBO КЭ ВДОЛЬ X-ОСИ NEX=", NEX, &
   " KOЛИЧECTBO КЭ ВДОЛЬ Y-ОСИ NEY=", NEY, &
   " ДЛИHA ОБЛАСТИ ВДОЛЬ X-ОСИ XL =", XL,  &
   " ДЛИHA ОБЛАСТИ ВДОЛЬ Y-ОСИ YL =", YL,  &
   " X-KOOPДИHATA ПЕРВОГО УЗЛA XC =", XC,  &
   " Y-KOOPДИHATA ПЕРВОГО УЗЛA YC =", YC,  &
   " ШАГ КЭ-СЕТКИ ВДОЛЬ X-ОСИ  DX =", DX,  &
   " ШАГ КЭ-СЕТКИ ВДОЛЬ Y-ОСИ  DY =", DY
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКАХ

   NMGG=SIZE(MGG) ! КОЛИЧЕСТВО INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ
   NSGG=SIZE(SGG) ! КОЛИЧЕСТВО    REAL-ПАРАМЕТРОВ КЭ-СЕТКИ

   IF ( NMGG < NCF ) THEN ! ПРОВЕРКА KOЛИЧECTBА INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ
      WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,                                 &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО INTEGER-ПАРАМЕТРОВ КЭ-СЕТКИ SIZE(MGG)=", NMGG, &
      " ОНО ДОЛЖНО БЫТЬ НЕ МЕНЕЕ NCF=", NCF, CHERR3
      STOP
   ENDIF

   IF ( NSGG < 2*NCF ) THEN ! ПРОВЕРКА KOЛИЧECTBА REAL-ПАРАМЕТРОВ КЭ-СЕТКИ
      WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,                              &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО REAL-ПАРАМЕТРОВ КЭ-СЕТКИ SIZE(SGG)=", NSGG, &
      " ОНО ДОЛЖНО БЫТЬ НЕ МЕНЕЕ 2*NCF=", 2*NCF, CHERR3
      STOP
   ENDIF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   ELSE
      IW=1 ! ПО УМОЛЧАНИЮ ПЕЧАТЬ ПОДПРОГРАММЫ ВКЛЮЧЕНА
   END IF

              ! ПАРАМЕТРЫ КЭ-СЕТКИ
   NEX=MGG(1) ! KOЛИЧECTBO ЭЛEMEHTOB ВДОЛЬ X-ОСИ
   NEY=MGG(2) ! KOЛИЧECTBO ЭЛEMEHTOB ВДОЛЬ Y-ОСИ

   XL =SGG(1) ! ДЛИHA ОБЛАСТИ ВДОЛЬ X-ОСИ
   YL =SGG(2) ! ДЛИHA ОБЛАСТИ ВДОЛЬ Y-ОСИ

   XC =SGG(3) ! X-KOOPДИHATA ПЕРВОГО УЗЛA
   YC =SGG(4) ! Y-KOOPДИHATA ПЕРВОГО УЗЛA

   IF ( NEX < 1 ) THEN ! ПРОВЕРКА KOЛИЧECTBА ЭЛEMEHTOB ВДОЛЬ X-ОСИ
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                                   &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО КОНЕЧНЫХ ЭЛEMEHTOB ВДОЛЬ X-ОСИ MGG(1)=", NEX, &
      " ОНО ДОЛЖНО БЫТЬ НЕ МЕНЕЕ 1", CHERR3
      STOP
   ENDIF

   IF ( NEY < 1 ) THEN ! ПРОВЕРКА KOЛИЧECTBА ЭЛEMEHTOB ВДОЛЬ Y-ОСИ
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                                   &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО КОНЕЧНЫХ ЭЛEMEHTOB ВДОЛЬ Y-ОСИ MGG(2)=", NEY, &
      " ОНО ДОЛЖНО БЫТЬ НЕ МЕНЕЕ 1", CHERR3
      STOP
   ENDIF

   IF ( XL <= 0.0 ) THEN ! ПРОВЕРКА ДЛИHЫ ОБЛАСТИ ВДОЛЬ X-ОСИ
      WRITE (3,'(/A/A/A,E12.6/A/A)') CHERR1, CHERR2,               &
      " НЕПРАВИЛЬНО ЗАДАНА ДЛИHA ОБЛАСТИ ВДОЛЬ X-ОСИ SGG(1)=", XL, &
      " ОНА ДОЛЖНА БЫТЬ ПОЛОЖИТЕЛЬНОЙ", CHERR3
      STOP
   ENDIF

   IF ( YL <= 0.0 ) THEN ! ПРОВЕРКА ДЛИHЫ ОБЛАСТИ ВДОЛЬ Y-ОСИ
      WRITE (3,'(/A/A/A,E12.6/A/A)') CHERR1, CHERR2,               &
      " НЕПРАВИЛЬНО ЗАДАНА ДЛИHA ОБЛАСТИ ВДОЛЬ X-ОСИ SGG(2)=", YL, &
      " ОНА ДОЛЖНА БЫТЬ ПОЛОЖИТЕЛЬНОЙ", CHERR3
      STOP
   ENDIF

                      ! МОДУЛЬНЫЕ ПЕРЕМЕННЫЕ
   NE=NEX*NEY         ! КОЛИЧЕСТВО КЭ
   NP=(NEX+1)*(NEY+1) ! КОЛИЧЕСТВО УЗЛОВ КЭ-CETKИ
   DX=XL/NEX          ! ШАГ КЭ-СЕТКИ ВДОЛЬ X-ОСИ
   DY=YL/NEY          ! ШАГ КЭ-СЕТКИ ВДОЛЬ Y-ОСИ

   CALL AMINT2 ("MNE", MNE, NCN, NE) ! РАЗМЕЩЕНИЕ МАССИВА НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
   CALL AMFLT2 ("CRD", CRD, NCF, NP) ! РАЗМЕЩЕНИЕ МАССИВА КООРДИНАТ УЗЛОВ КЭ-СЕТКИ

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE GRD04G2

!**********************************************************************
!**********************************************************************

SUBROUTINE GBC1 (MBC, & ! МАССИВ КОДОВ ГРАНИЧНЫХ УСЛОВИЙ
                 CBC, & ! МАССИВ КООРДИНАТ ГРАНИЦ ОБЛАСТИ
                 EBC, & ! МАССИВ ДОПУСКОВ КООРДИНАТ ГРАНИЦ ОБЛАСТИ
                 KBC, & ! МАССИВ КОДОВ ГРАНИЧНЫХ УСЛОВИЙ
                 KW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ФОРМИРОВАНИЕ МАССИВОВ
! КОДОВ ГРАНИЧНЫХ УСЛОВИЙ
! КООРДИНАТ ГРАНИЦ ОБЛАСТИ
! ДОПУСКОВ КООРДИНАТ ГРАНИЦ ОБЛАСТИ

INTEGER, ALLOCATABLE, INTENT (OUT)          :: MBC(:,:) ! МАССИВ КОДОВ ГРАНИЧНЫХ УСЛОВИЙ
REAL,    ALLOCATABLE, INTENT (OUT)          :: CBC(:,:) ! МАССИВ КООРДИНАТ ГРАНИЦ ОБЛАСТИ
REAL,    ALLOCATABLE, INTENT (OUT)          :: EBC(:)   ! МАССИВ ДОПУСКОВ КООРДИНАТ ГРАНИЦ ОБЛАСТИ
INTEGER,              INTENT (IN), OPTIONAL :: KBC(:,:) ! МАССИВ КОДОВ ГРАНИЧНЫХ УСЛОВИЙ
INTEGER,              INTENT (IN), OPTIONAL :: KW       ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ - KW=0,1

CHARACTER (LEN=*), PARAMETER :: CHSUB ="GBC1"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

CALL APRM ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ И РАЗМЕЩЕНИЕ МАССИВОВ

! МАССИВЫ КООРДИНАТ И ДОПУСКОВ КООРДИНАТ ГРАНИЦ ОБЛАСТИ

CBC(1,1)=XC
CBC(2,1)=XC+XL
EBC(  1)=DX*EXYZ

IF ( NCF > 1 ) THEN
   CBC(1,2)=YC
   CBC(2,2)=YC+YL
   EBC(  2)=DY*EXYZ
   IF ( NCF > 2 ) THEN
      CBC(1,3)=ZC
      CBC(2,3)=ZC+ZL
      EBC(  3)=DZ*EXYZ
   END IF
END IF

IF ( IW == 1 ) THEN  ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ ПОДПРОГРАММЫ
   WRITE (3,'(2(/A),2(/A,E12.6,A,E12.6,A,I0,2A))') CHMSG1,                                       &
   " ГРАНИЧНЫЕ УСЛОВИЯ",                                                                         &
   " X-КООРДИНАТА=", CBC(1,1), "   ДОПУСК=", EBC(1), "   КОД=", MBC(1,1), " - ", CHBC(MBC(1,1)), &
   " X-КООРДИНАТА=", CBC(2,1), "   ДОПУСК=", EBC(1), "   КОД=", MBC(2,1), " - ", CHBC(MBC(2,1))

   IF ( NCF > 1 ) THEN
      WRITE (3,'(A,E12.6,A,E12.6,A,I0,2A)')                                                         &
      " Y-КООРДИНАТА=", CBC(1,2), "   ДОПУСК=", EBC(2), "   КОД=", MBC(1,2), " - ", CHBC(MBC(1,2)), &
      " Y-КООРДИНАТА=", CBC(2,2), "   ДОПУСК=", EBC(2), "   КОД=", MBC(2,2), " - ", CHBC(MBC(2,2))
      IF ( NCF > 2 ) THEN
         WRITE (3,'(A,E12.6,A,E12.6,A,I0,2A)')                                                         &
         " Z-КООРДИНАТА=", CBC(1,3), "   ДОПУСК=", EBC(3), "   КОД=", MBC(1,3), " - ", CHBC(MBC(1,3)), &
         " Z-КООРДИНАТА=", CBC(2,3), "   ДОПУСК=", EBC(3), "   КОД=", MBC(2,3), " - ", CHBC(MBC(2,3))
      END IF
   END IF

END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И РАЗМЕЩЕНИЕ МАССИВОВ

   IF ( ( NCF < 1 ) .OR. ( NCF > 3 ) ) THEN ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,               &
      " НЕПРАВИЛЬНО ОПРЕДЕЛЕНА МОДУЛЬНАЯ ПЕРЕМЕННАЯ NCF=", NCF, &
      " ОНА ДОЛЖНА БЫТЬ В ДИАПАЗОНЕ 1-3", CHERR3
      STOP
   ENDIF

   ! РАЗМЕЩЕНИЕ МАССИВОВ
   CALL AMINT2 ("MBC", MBC, 2, NCF, IRL=1) ! КОДОВ ГРАНИЧНЫХ УСЛОВИЙ
   CALL AMFLT2 ("CBC", CBC, 2, NCF, IRL=1) ! КООРДИНАТ ГРАНИЦ ОБЛАСТИ
   CALL AMFLT1 ("EBC", EBC,    NCF, IRL=1) ! ДОПУСКОВ КООРДИНАТ ГРАНИЦ ОБЛАСТИ

   ! МАССИВ КОДОВ ГРАНИЧНЫХ УСЛОВИЙ
   IF ( PRESENT(KBC) ) THEN
      IF ( SIZE(KBC,1) /= 2 ) THEN ! РАЗМЕРНОСТЬ-1 МАССИВА KBC
         WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                              &
         " НЕПРАВИЛЬНО ЗАДАНА РАЗМЕРНОСТЬ МАССИВА KBC SIZE(KBC,1)=", SIZE(KBC,1), &
         " ОНА ДОЛЖНА БЫТЬ РАВНА 2", CHERR3
         STOP
      END IF
      IF ( SIZE(KBC,2) /= NCF ) THEN ! РАЗМЕРНОСТЬ-1 МАССИВА QEMAT
         WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,                           &
         " НЕПРАВИЛЬНО ЗАДАНА РАЗМЕРНОСТЬ МАССИВА KBC SIZE(KBC,2)=", SIZE(KBC,2), &
         " ОНА ДОЛЖНА БЫТЬ РАВНА NCF=", NCF, CHERR3
         STOP
      END IF
      MBC=KBC
      WHERE ( ( MBC < 0) .OR. ( MBC > 3) ) MBC=1 ! BY DEFAULT
   ELSE
      MBC=1 ! BY DEFAULT - СКОЛЬЗЯЩАЯ ЗАДЕЛКА (ЗАДАНЫ НОРМАЛЬНЫЕ ПЕРЕМЕЩЕНИЯ И НУЛЕВЫЕ КАСАТЕЛЬНЫЕ НАПРЯЖЕНИЯ)
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   END IF

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

   FUNCTION CHBC (LBC)

   ! ТИПЫ ГРАНИЧНЫХ УСЛОВИЙ

   CHARACTER (LEN=128)            :: CHBC ! НАИМЕНОВАНИЕ ТИПА ГРАНИЧНЫХ УСЛОВИЙ
   INTEGER,           INTENT (IN) :: LBC  ! КОД ТИПА ГРАНИЧНЫХ УСЛОВИЙ

   SELECT CASE (LBC)
      CASE (0)
         CHBC="СВОБОДНАЯ ПОВЕРХНОСТЬ"
      CASE (1)
          CHBC="СКОЛЬЗЯЩАЯ ЗАДЕЛКА"      ! ЗАДАНЫ НОРМАЛЬНЫЕ ПЕРЕМЕЩЕНИЯ И НУЛЕВЫЕ КАСАТЕЛЬНЫЕ НАПРЯЖЕНИЯ
      CASE (2)
          CHBC="ПОЛНАЯ ЗАДЕЛКА"          ! ЗАДАНЫ ВСЕ КОМПОНЕНТЫ ПЕРЕМЕЩЕНИЙ
      CASE (3)
          CHBC="РАСПРЕДЕЛЕННАЯ НАГРУЗКА" ! ЗАДАНЫ ВСЕ КОМПОНЕНТЫ НАГРУЗКИ
      CASE (4)
          CHBC="УПРУГАЯ ЗАДЕЛКА"         ! ВИНКЛЕРОВСКИЙ СЛОЙ
      CASE (5)
          CHBC="ИДЕАЛЬНЫЙ ОДНОСТОРОННИЙ КОНТАКТ СО ШТАМПОМ"
      CASE (6)
          CHBC="ИДЕАЛЬНЫЙ ОДНОСТОРОННИЙ КОНТАКТ ВИНКЛЕРОВСКОГО ПОКРЫТИЯ СО ШТАМПОМ"
      CASE (7)
          CHBC="ФРИКЦИОННЫЙ ОДНОСТОРОННИЙ КОНТАКТ СО ШТАМПОМ"
   END SELECT

   END FUNCTION CHBC

   !**********************************************************************

END SUBROUTINE GBC1

!**********************************************************************

SUBROUTINE GBA1 (MBC, & ! МАССИВ КОДОВ ГРАНИЧНЫХ УСЛОВИЙ
                 CBC, & ! МАССИВ КООРДИНАТ ГРАНИЦ ОБЛАСТИ
                 EBC, & ! МАССИВ ДОПУСКОВ КООРДИНАТ ГРАНИЦ ОБЛАСТИ
                 CRD, & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                 MBP, & ! МАССИВ НОМЕРОВ СТЕПЕНЕЙ СВОБОДЫ ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
                 FBP, & ! ЗАДАННЫХ ПЕРЕМЕЩЕНИЙ
                 RBP, & ! РЕАКЦИЙ В ЗАКРЕПЛЕННЫХ УЗЛАХ
                 KW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ


! ФОРМИРОВАНИЕ МАССИВА НОМЕРОВ СТЕПЕНЕЙ СВОБОДЫ ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
! ПО ВСЕЙ ГРАНИЦЕ ГИПЕРКУБА

REAL,    ALLOCATABLE, INTENT (IN)           :: CRD(:,:) ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
INTEGER, ALLOCATABLE, INTENT (IN)           :: MBC(:,:) ! МАССИВ КОДОВ ГРАНИЧНЫХ УСЛОВИЙ
REAL,    ALLOCATABLE, INTENT (IN)           :: CBC(:,:) ! МАССИВ КООРДИНАТ ГРАНИЦ ОБЛАСТИ
REAL,    ALLOCATABLE, INTENT (IN)           :: EBC(:)   ! МАССИВ ДОПУСКОВ КООРДИНАТ ГРАНИЦ ОБЛАСТИ
INTEGER, ALLOCATABLE, INTENT (OUT)          :: MBP(:)   ! МАССИВ НОМЕРОВ СТЕПЕНЕЙ СВОБОДЫ ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
REAL,    ALLOCATABLE, INTENT (OUT)          :: FBP(:)   ! ЗАДАННЫХ ПЕРЕМЕЩЕНИЙ
REAL,    ALLOCATABLE, INTENT (OUT)          :: RBP(:)   ! РЕАКЦИЙ В ЗАКРЕПЛЕННЫХ УЗЛАХ
INTEGER,              INTENT (IN), OPTIONAL :: KW       ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ - KW=0,1

                   ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: NBP     ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
INTEGER :: LBP     ! СЧЕТЧИК ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
INTEGER :: LR      ! СЧЕТЧИК ЦИКЛА ДВУМ ПРОТИВОПОЛОЖНЫМ СТОРОНАМ ГИПЕРКУБА
REAL    :: EB      ! ДОПУСК ГРАНИЦЫ
REAL    :: CB      ! КООРДИНАТА ГРАНИЦЫ
INTEGER :: IER=0   ! КОД ОШИБКИ ПРИ РАЗМЕЩЕНИИ МАССИВА

LOGICAL(KIND=1), ALLOCATABLE :: LGCL(:,:) ! МАССИВ ФЛАГОВ СТЕПЕНЕЙ СВОБОДЫ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="GBC1"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

CALL APRM ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ И РАЗМЕЩЕНИЕ МАССИВОВ

LGCL=.FALSE.

DO LCF=1,NCF ! ЦИКЛ ПО ВСЕМ ПРОСТРАНСТВЕННЫМ КООРДИНАТАМ

   EB=EBC(LCF) ! ДОПУСК ГРАНИЦЫ

   DO LR=1,2 ! ЦИКЛ ПО ДВУМ ПРОТИВОПОЛОЖНЫМ СТОРОНАМ ГИПЕРКУБА

      CB=CBC(LR,LCF) ! КООРДИНАТА ГРАНИЦЫ

      IF ( MBC(LR,LCF) == 1 ) THEN ! СКОЛЬЗЯЩАЯ ЗАДЕЛКА

         DO LP=1,NP ! ЦИКЛ ПО ВСЕМ УЗЛАМ КЭ-СЕТКИ
            IF ( ABS(CRD(LCF,LP)-CB) < EB ) THEN
               LGCL(LCF,LP)=.TRUE.
            END IF
         END DO

      ELSE IF ( MBC(LR,LCF) == 2 ) THEN ! ПОЛНАЯ ЗАДЕЛКА

         DO LP=1,NP ! ЦИКЛ ПО ВСЕМ УЗЛАМ КЭ-СЕТКИ
            IF ( ABS(CRD(LCF,LP)-CB) < EB ) THEN
               LGCL(:,LP)=.TRUE.
            END IF
         END DO

      ENDIF

   END DO
END DO

NBP=COUNT(LGCL) ! КОЛИЧЕСТВО ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ

! РАЗМЕЩЕНИЕ МАССИВА НОМЕРОВ СТЕПЕНЕЙ СВОБОДЫ ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
CALL AMINT1 ("MBP", MBP, NBP, IRL=1)

! ФОРМИРОВАНИЕ ОДНОРОДНЫХ ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
LBP=0 ! СЧЕТЧИК ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ

DO LP=1,NP ! ЦИКЛ ПО ВСЕМ УЗЛАМ КЭ-СЕТКИ
   DO LCF=1,NCF ! ЦИКЛ ПО ВСЕМ ПРОСТРАНСТВЕННЫМ КООРДИНАТАМ
      IF ( LGCL(LCF,LP) ) THEN
         LBP=LBP+1 ! СЧЕТЧИК ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ
         MBP(LBP)=NCF*(LP-1)+LCF
      END IF
   END DO
END DO

! ОСВОБОЖДЕНИЕ ПАМЯТИ МАССИВА ФЛАГОВ СТЕПЕНЕЙ СВОБОДЫ
DEALLOCATE (LGCL, STAT=IER)
IF ( IER /= 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКЕ
   WRITE (3,'(/A/A/A,I0/A)') CHERR1, CHERR2,                                   &
   " ОШИБКА ПРИ ОСВОБОЖДЕНИИ ПАМЯТИ ЛОКАЛЬНОГО МАССИВА LGCL - IER=", IER, CHERR3
   STOP
END IF

! РАЗМЕЩЕНИЕ МАССИВОВ
CALL AMFLT1 ("FBP", FBP, NBP, IRL=1) ! ЗАДАННЫХ ПЕРЕМЕЩЕНИЙ
CALL AMFLT1 ("RBP", RBP, NBP, IRL=1) ! РЕАКЦИЙ В ЗАКРЕПЛЕННЫХ УЗЛАХ

FBP=0 ! НУЛЕВЫЕ ПЕРЕМЕЩЕНИЯ

IF ( IW == 1 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ ПОДПРОГРАММЫ
   WRITE (3,'(/A/A/A,I0)') CHMSG1,                        &
   " ЗАДАНЫ ОДНОРОДНЫЕ ГРАНИЧНЫЕ УСЛОВИЯ В ПЕРЕМЕЩЕНИЯХ", &
   " NBP=", NBP
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И РАЗМЕЩЕНИЕ МАССИВОВ

   NCF=SIZE(CRD,1) ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
   NP =SIZE(CRD,2) ! КОЛИЧЕСТВО УЗЛОВ CETKИ

   IF ( SIZE(MBC,1) /= 2 ) THEN ! КОЛИЧЕСТВО ПРОТИВОПОЛОЖНЫХ ГРАНИЦ ГИПЕРКУБА
      WRITE (3,'(/A/A/A/A,I0/A/A)') CHERR1, CHERR2, &
      " НЕПРАВИЛЬНО ЗАДАН ПАРАМЕТР-МАССИВ MBC, ",   &
      " ЕГО РАЗМЕРНОСТЬ SIZE(MBC,1)=", SIZE(MBC,1), &
      " А ТРЕБУЕТСЯ 2", CHERR3
      STOP
   END IF

   IF ( SIZE(MBC,2) /= NCF ) THEN ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
      WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2, & ! НЕСООТВЕТСТВИЕ РАЗМЕРНОСТЕЙ
      " SIZE(MBC,2)=", SIZE(MBC,2),                  & ! МАССИВ КОДОВ ГРАНИЧНЫХ УСЛОВИЙ
      " SIZE(CRD,1)=", SIZE(CRD,1), CHERR3             ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
      STOP
   END IF

   IF ( SIZE(CBC,1) /= 2 ) THEN ! КОЛИЧЕСТВО ПРОТИВОПОЛОЖНЫХ ГРАНИЦ ГИПЕРКУБА
      WRITE (3,'(/A/A/A/A,I0/A/A)') CHERR1, CHERR2, &
      " НЕПРАВИЛЬНО ЗАДАН ПАРАМЕТР-МАССИВ CBC, ",   &
      " ЕГО РАЗМЕРНОСТЬ SIZE(CBC,1)=", SIZE(CBC,1), &
      " А ТРЕБУЕТСЯ 2", CHERR3
      STOP
   END IF

   IF ( SIZE(CBC,2) /= NCF ) THEN ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
      WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2, & ! НЕСООТВЕТСТВИЕ РАЗМЕРНОСТЕЙ
      " SIZE(CBC,2)=", SIZE(CBC,2),                  & ! МАССИВ КООРДИНАТ ГРАНИЦ ОБЛАСТИ
      " SIZE(CRD,1)=", SIZE(CRD,1), CHERR3             ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
      STOP
   END IF

   IF ( SIZE(EBC,1) /= NCF ) THEN ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
      WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2, & ! НЕСООТВЕТСТВИЕ РАЗМЕРНОСТЕЙ
      " SIZE(EBC,1)=", SIZE(EBC,1),                  & ! МАССИВ ДОПУСКОВ КООРДИНАТ ГРАНИЦ ОБЛАСТИ
      " SIZE(CRD,1)=", SIZE(CRD,1), CHERR3             ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
      STOP
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   ELSE
      IW=1 ! ПО УМОЛЧАНИЮ ПЕЧАТЬ ПОДПРОГРАММЫ ВКЛЮЧЕНА
   END IF

   ! РАЗМЕЩЕНИЕ МАССИВА ФЛАГОВ СТЕПЕНЕЙ СВОБОДЫ
   ALLOCATE (LGCL(NCF,NP), STAT=IER)

   IF ( IER /= 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКЕ
      WRITE (3,'(/A/A/A,I0/A)') CHERR1, CHERR2,                          &
      " ОШИБКА ПРИ РАЗМЕЩЕНИИ ЛОКАЛЬНОГО МАССИВА LGCL - IER=", IER, CHERR3
      STOP
   END IF

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE GBA1

!**********************************************************************
!**********************************************************************

SUBROUTINE WMNE1 (MNE, & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                  KE1, & ! НОМЕР  НАЧАЛЬНОГО КЭ ДЛЯ ПЕЧАТИ
                  KE2, & ! НОМЕР  КОНЕЧНОГО  КЭ ДЛЯ ПЕЧАТИ
                  KANAL) ! НОМЕР  КАНАЛА ДЛЯ ВЫВОДА В ФАЙЛ

! PUBLIC
! ПЕЧАТЬ НОМЕРОВ УЗЛОВ KЭ

INTEGER, INTENT (IN)           :: MNE(:,:) ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
INTEGER, INTENT (IN), OPTIONAL :: KE1      ! НОМЕР  НАЧАЛЬНОГО КЭ ДЛЯ ПЕЧАТИ
INTEGER, INTENT (IN), OPTIONAL :: KE2      ! НОМЕР  КОНЕЧНОГО  КЭ ДЛЯ ПЕЧАТИ
INTEGER, INTENT (IN), OPTIONAL :: KANAL    ! НОМЕР  КАНАЛА ДЛЯ ВЫВОДА В ФАЙЛ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="WMNE1"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

                ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: LE1  ! НОМЕР НАЧАЛЬНОГО КЭ ДЛЯ ПЕЧАТИ
INTEGER :: LE2  ! НОМЕР КОНЕЧНОГО  КЭ ДЛЯ ПЕЧАТИ

CALL APRM ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ

IF ( KNL == 3 ) THEN
   WRITE (3,'(/2X,A/A)') CHMSG1, "   ЭЛЕМЕНТ          УЗЛЫ КЭ"
   DO LE=LE1,LE2 ! ЦИKЛ ПO КЭ
      WRITE (3,'(I10,99I10)') LE, MNE(:,LE)
   END DO
ELSE
   DO LE=LE1,LE2 ! ЦИKЛ ПO КЭ
      WRITE (KNL,'(99I10)') MNE(:,LE)
   END DO
END IF


RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКАХ

   LE1=1           ! НОМЕР НАЧАЛЬНОГО КЭ ДЛЯ ПЕЧАТИ
   LE2=SIZE(MNE,2) ! НОМЕР  КОНЕЧНОГО КЭ ДЛЯ ПЕЧАТИ

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

END SUBROUTINE WMNE1

!**********************************************************************

SUBROUTINE WCRD1 (CRD, & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                  KP1, & ! НОМЕР  НАЧАЛЬНОГО УЗЛА ДЛЯ ПЕЧАТИ
                  KP2, & ! НОМЕР  КОНЕЧНОГО  УЗЛА ДЛЯ ПЕЧАТИ
                  KANAL) ! НОМЕР  КАНАЛА ДЛЯ ВЫВОДА В ФАЙЛ

! PUBLIC
! ПЕЧАТЬ КООРДИНАТ УЗЛОВ KЭ

REAL,    INTENT (IN)           :: CRD(:,:) ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
INTEGER, INTENT (IN), OPTIONAL :: KP1      ! НОМЕР  НАЧАЛЬНОГО УЗЛА ДЛЯ ПЕЧАТИ
INTEGER, INTENT (IN), OPTIONAL :: KP2      ! НОМЕР  КОНЕЧНОГО  УЗЛА ДЛЯ ПЕЧАТИ
INTEGER, INTENT (IN), OPTIONAL :: KANAL    ! НОМЕР  КАНАЛА ДЛЯ ВЫВОДА В ФАЙЛ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="WCRD1"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

                ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: LP1  ! НОМЕР НАЧАЛЬНОГО УЗЛА ДЛЯ ПЕЧАТИ
INTEGER :: LP2  ! НОМЕР КОНЕЧНОГО  УЗЛА ДЛЯ ПЕЧАТИ

CALL APRM ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ

IF ( KNL == 3 ) THEN
   WRITE (3,'(/5X,A/A)') CHMSG1, "      УЗЛЫ    КООРДИНАТЫ"
   DO LP=LP1,LP2 ! ЦИKЛ ПO УЗЛАМ
      WRITE (3,'(I10,3E15.6)') LP, CRD(:,LP)
   END DO
ELSE
   DO LP=LP1,LP2 ! ЦИKЛ ПO УЗЛАМ
      WRITE (KNL,'(3E15.6)') CRD(:,LP)
   END DO
END IF


RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКАХ

   LP1=1           ! НОМЕР НАЧАЛЬНОГО УЗЛА ДЛЯ ПЕЧАТИ
   LP2=SIZE(CRD,2) ! НОМЕР  КОНЕЧНОГО УЗЛА ДЛЯ ПЕЧАТИ

   IF ( PRESENT(KP1) ) THEN ! НОМЕР НАЧАЛЬНОГО УЗЛА ДЛЯ ПЕЧАТИ
      IF ( ( KP1 > 1 ) .AND. ( KP1 <= LP2 ) ) LP1=KP1
   END IF

   IF ( PRESENT(KP2) ) THEN ! НОМЕР КОНЕЧНОГО УЗЛА ДЛЯ ПЕЧАТИ
      IF ( ( KP2 > 1 ) .AND. ( KP2 <= LP2 ) ) LP2=KP2
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

END SUBROUTINE WCRD1


!**********************************************************************

END MODULE GRD
