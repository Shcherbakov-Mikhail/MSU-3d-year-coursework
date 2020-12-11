MODULE DGPY ! ПОДГОТОВКА ДАННЫХ ДЛЯ ПОСТОРОЕНИЯ ГРАФИКОВ

!**********************************************************************

           ! МОДУЛИ
USE BASE   ! БАЗОВЫЙ КОНЕЧНО- И ГРАНИЧНО-ЭЛЕМЕНТНЫХ ПАКЕТОВ

!**********************************************************************
IMPLICIT NONE
!**********************************************************************
CHARACTER (LEN=*), PARAMETER, PRIVATE :: CHMDL="DGPY"
!**********************************************************************

INTEGER,                      PROTECTED :: KNL   =50     ! НОМЕР КАНАЛА ВЫВОДА ДАННЫХ
CHARACTER (LEN=2),            PRIVATE   :: CHKNL ="50"   ! НОМЕР КАНАЛА ВЫВОДА ДАННЫХ
CHARACTER (LEN=3),            PRIVATE   :: CHFT  ="jpg"  ! ТИП ФАЙЛА РИСУНКА
INTEGER,                      PRIVATE   :: NCLR  =0      ! ФЛАГ ЦВЕТА
CHARACTER (LEN=*), PARAMETER, PRIVATE   :: CHDATA="#***" ! ИДЕНТИФИКАТОР БЛОКА ЧИСЛОВЫХ ДАННЫХ
CHARACTER (LEN=*), PARAMETER, PRIVATE   :: PYTHON='python '
CHARACTER (LEN=*), PARAMETER, PRIVATE   :: GFORT1='"D:\Eclipse\Workspace\WSPython\STA2BS\Source\gfort1.py"'
CHARACTER (LEN=*), PARAMETER, PRIVATE   :: GFORT2='"D:\Eclipse\Workspace\WSPython\STA2BS\Source\gfort2.py"'
CHARACTER (LEN=*), PARAMETER, PRIVATE   :: GFORT3='"D:\Eclipse\Workspace\WSPython\STA2BS\Source\gfort3.py"'

!**********************************************************************

                  ! ПОДПРОГРАММЫ
PUBLIC  :: DGPYRS ! ИЗМЕНЕНИЕ ПАРАМЕТРОВ MATPLOTLIB

                  ! ПОСТРОЕНИЕ 1D-ГРАФИКОВ ФУНКЦИЙ Y=F(X)
PUBLIC  :: DGPY11 ! ФОРМИРОВАНИЕ ЗАГОЛОВКА ФАЙЛА ДАННЫХ
PUBLIC  :: DGPY12 ! LEGEND ДЛЯ ДОПОЛНИТЕЛЬНОЙ КРИВОЙ НА ГРАФИКЕ
PUBLIC  :: DGPY1E ! ЗАПУСК НА ВЫПОЛНЕНИЕ ПРОГРАММЫ gfort1.py

PUBLIC  :: DGPY21 ! ПОСТРОЕНИЕ 2D-IMAGES       ФУНКЦИИ Z=F(X,Y) В ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
PUBLIC  :: DGPY31 ! ПОСТРОЕНИЕ 3D-SURFACE_PLOT ФУНКЦИИ Z=F(X,Y) В ПРЯМОУГОЛЬНОЙ ОБЛАСТИ

!**********************************************************************
CONTAINS
!**********************************************************************

SUBROUTINE DGPYRS (CHFTYP, & ! ТИП ФАЙЛА РИСУНКА
                   KANAL,  & ! НОМЕР КАНАЛА ВЫВОДА ДАННЫХ
                   KCLR)     ! ФЛАГ ЦВЕТА

! PUBLIC
! ИЗМЕНЕНИЕ ПАРАМЕТРОВ MATPLOTLIB

CHARACTER (LEN=*), INTENT (IN), OPTIONAL :: CHFTYP ! ТИП ФАЙЛА РИСУНКА
INTEGER,           INTENT (IN), OPTIONAL :: KANAL  ! НОМЕР КАНАЛА ВЫВОДА ДАННЫХ
INTEGER,           INTENT (IN), OPTIONAL :: KCLR   ! ФЛАГ ЦВЕТА

! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
CHARACTER (LEN=*), PARAMETER :: CHSUB ="DGPYRS"
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! ТИП ФАЙЛА РИСУНКА
IF ( PRESENT(CHFTYP) ) THEN
   SELECT CASE(CHFTYP)

      CASE ("svg", "SVG")
         CHFT="svg"

      CASE ("png", "PNG")
         CHFT="png"

      CASE ("eps", "EPS")
         CHFT="eps"

      CASE ("pdf", "PDF")
         CHFT="pdf"

      CASE DEFAULT
         WRITE (3,'(/A/A/A,A/A/A)') CHERR1, CHERR2,                            &
         " НЕПРАВИЛЬНО УКАЗАН ТИП ФАЙЛА РИСУНКА CHFTYP=", CHFTYP,              &
         " ОН ДОЛЖЕН БЫТЬ ИЗ СПИСКА: svg/SVG, png/PNG, eps/EPS, pdf/PDF", CHERR3
         STOP

   END SELECT
END IF

! НОМЕР КАНАЛА ВЫВОДА ДАННЫХ
IF ( PRESENT(KANAL) ) THEN
   IF ( ( KANAL < 20 ) .OR. ( KANAL > 99 ) ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                       &
      " НЕПРАВИЛЬНО УКАЗАН НОМЕР КАНАЛА ВЫВОДА ДАННЫХ KANAL=", KANAL,   &
      " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 20-99", CHERR3
      STOP
   ENDIF
   KNL=KANAL
   WRITE (CHKNL,'(I2)') KNL
END IF

! ФЛАГ ЦВЕТА
IF ( PRESENT(KCLR) ) THEN
   IF ( ( KCLR < 0 ) .OR. ( KCLR > 1 ) ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,    &
      " НЕПРАВИЛЬНО УКАЗАН ФЛАГ ЦВЕТА KCLR=", KCLR,  &
      " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 0-1", CHERR3
      STOP
   ENDIF
   NCLR=KCLR
END IF

END SUBROUTINE DGPYRS

!**********************************************************************
!**********************************************************************

SUBROUTINE DGPY11 (FLNAME, & ! ИМЯ ФАЙЛОВ ДАННЫХ И РИСУНКА
                   KANAL,  & ! НОМЕР КАНАЛА ВЫВОДА ДАННЫХ
                   LNMRKR, & ! КОД ЛИНИЯ/МАРКЕР
                   TITLE,  & ! НАИМЕНОВАНИЕ РИСУНКА
                   XLABEL, & ! НАДПИСЬ X-ОСИ
                   YLABEL, & ! НАДПИСЬ Y-ОСИ
                   XUNIT,  & ! ЕДИНИЦЫ ИЗМЕРЕНИЯ X-ОСИ
                   YUNIT,  & ! ЕДИНИЦЫ ИЗМЕРЕНИЯ Y-ОСИ
                   X1,     & ! LEFT   ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ X
                   X2,     & ! RIGHT  ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ X
                   Y1,     & ! BOTTOM ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ Y
                   Y2,     & ! TOP    ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ Y
                   INTX,   & ! XINTEGER ЦЕЛЫЕ ЗНАЧЕНИЯ ПЕРЕМЕННОЙ X
                   LEGEND)   ! LEGEND ДЛЯ ПЕРВОЙ КРИВОЙ НА ГРАФИКЕ

! PUBLIC
! ПОСТРОЕНИЕ 1D-ГРАФИКОВ ФУНКЦИЙ Y=F(X)
! ФОРМИРОВАНИЕ ЗАГОЛОВКА ФАЙЛА ДАННЫХ

CHARACTER (LEN=*), INTENT (IN)           :: FLNAME ! ИМЯ ФАЙЛОВ ДАННЫХ И РИСУНКА
INTEGER,           INTENT (IN), OPTIONAL :: KANAL  ! НОМЕР КАНАЛА ВЫВОДА ДАННЫХ
INTEGER,           INTENT (IN), OPTIONAL :: LNMRKR ! КОД ЛИНИЯ/МАРКЕР
CHARACTER (LEN=*), INTENT (IN), OPTIONAL :: TITLE  ! НАИМЕНОВАНИЕ РИСУНКА
CHARACTER (LEN=*), INTENT (IN), OPTIONAL :: XLABEL ! НАДПИСЬ X-ОСИ
CHARACTER (LEN=*), INTENT (IN), OPTIONAL :: YLABEL ! НАДПИСЬ Y-ОСИ
CHARACTER (LEN=*), INTENT (IN), OPTIONAL :: XUNIT  ! ЕДИНИЦЫ ИЗМЕРЕНИЯ X-ОСИ
CHARACTER (LEN=*), INTENT (IN), OPTIONAL :: YUNIT  ! ЕДИНИЦЫ ИЗМЕРЕНИЯ Y-ОСИ
REAL,              INTENT (IN), OPTIONAL :: X1     ! LEFT   ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ X
REAL,              INTENT (IN), OPTIONAL :: X2     ! RIGHT  ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ X
REAL,              INTENT (IN), OPTIONAL :: Y1     ! BOTTOM ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ Y
REAL,              INTENT (IN), OPTIONAL :: Y2     ! TOP    ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ Y
INTEGER,           INTENT (IN), OPTIONAL :: INTX   ! XINTEGER ЦЕЛЫЕ ЗНАЧЕНИЯ ПЕРЕМЕННОЙ X
CHARACTER (LEN=*), INTENT (IN), OPTIONAL :: LEGEND ! LEGEND ДЛЯ ПЕРВОЙ КРИВОЙ НА ГРАФИКЕ

! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
CHARACTER (LEN=*), PARAMETER :: CHSUB ="DGPY11"
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! НОМЕР КАНАЛА ВЫВОДА ДАННЫХ
IF ( PRESENT(KANAL) ) THEN
   IF ( ( KANAL < 20 ) .OR. ( KANAL > 99 ) ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                       &
      " НЕПРАВИЛЬНО УКАЗАН НОМЕР КАНАЛА ВЫВОДА ДАННЫХ KANAL=", KANAL,   &
      " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 20-99", CHERR3
      STOP
   ENDIF
   KNL=KANAL
ELSE
   KNL=51 ! BY DEFAULT
END IF
WRITE (CHKNL,'(I2)') KNL

! ИМЯ ФАЙЛОВ ДАННЫХ И РИСУНКА
WRITE (KNL,'(A)') FLNAME//"."//CHFT

! ФЛАГ ЦВЕТА
IF ( NCLR /= 0 ) THEN
   WRITE (KNL,'(A,I0)') "NCOLOR ", NCLR
END IF

! КОД ЛИНИЯ/МАРКЕР
! 0 - ЛИНИЯ
! 1 - МАРКЕР
IF ( PRESENT(LNMRKR) ) THEN
   IF ( ( LNMRKR < 0 ) .OR. ( LNMRKR > 1 ) ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,              &
      " НЕПРАВИЛЬНО ЗАДАН КОД ЛИНИЯ/МАРКЕР LNMRKR=", LNMRKR,   &
      " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 0-1", CHERR3
      STOP
   ENDIF
   WRITE (KNL,'(A,I0)') "LINEMARKER ", LNMRKR
END IF

! НАИМЕНОВАНИЕ РИСУНКА
IF ( PRESENT(TITLE) ) THEN
    WRITE (KNL,'(A)') "TITLE "//TITLE
END IF

! НАДПИСЬ X-ОСИ
IF ( PRESENT(XLABEL) ) THEN
    WRITE (KNL,'(A)') "XLABEL "//XLABEL
END IF

! НАДПИСЬ Y-ОСИ
IF ( PRESENT(YLABEL) ) THEN
    WRITE (KNL,'(A)') "YLABEL "//YLABEL
END IF

! ЕДИНИЦЫ ИЗМЕРЕНИЯ X-ОСИ
IF ( PRESENT(XUNIT) ) THEN
    WRITE (KNL,'(A)') "XUNIT "//XUNIT
END IF

! ЕДИНИЦЫ ИЗМЕРЕНИЯ Y-ОСИ
IF ( PRESENT(YUNIT) ) THEN
    WRITE (KNL,'(A)') "YUNIT "//YUNIT
END IF

! LEFT ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ X
IF ( PRESENT(X1) ) THEN
    WRITE (KNL,'(A,E12.6)') "XLEFT  ", X1
END IF

! RIGHT ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ X
IF ( PRESENT(X2) ) THEN
    WRITE (KNL,'(A,E12.6)') "XRIGHT ", X2
END IF

! BOTTOM ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ Y
IF ( PRESENT(Y1) ) THEN
    WRITE (KNL,'(A,E12.6)') "YBOTTOM ", Y1
END IF

! TOP ГРАНИЦА ДИАПАЗОНА ИЗМЕНЕНИЯ ПЕРЕМЕННОЙ Y
IF ( PRESENT(Y2) ) THEN
    WRITE (KNL,'(A,E12.6)') "YTOP ", Y2
END IF

! ЦЕЛЫЕ ЗНАЧЕНИЯ ПЕРЕМЕННОЙ X
IF ( PRESENT(INTX) ) THEN
   IF ( ( INTX < 0 ) .OR. ( INTX > 1 ) ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,   &
      " НЕПРАВИЛЬНО УКАЗАН ПАРАМЕТР INTX=", INTX,   &
      " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 0-1", CHERR3
      STOP
   ENDIF
   WRITE (KNL,'(A,I0)') "XINTEGER ", INTX
END IF

! ИДЕНТИФИКАТОР БЛОКА ЧИСЛОВЫХ ДАННЫХ + LEGEND ДЛЯ ПЕРВОЙ КРИВОЙ НА ГРАФИКЕ
IF ( PRESENT(LEGEND) ) THEN
    WRITE (KNL,'(A)') CHDATA//" "//LEGEND
ELSE
    WRITE (KNL,'(A)') CHDATA ! БЕЗ ЛЕГЕНДЫ
END IF

END SUBROUTINE DGPY11

!**********************************************************************


SUBROUTINE DGPY12 (LEGEND)

! PUBLIC
! ПОСТРОЕНИЕ 1D-ГРАФИКОВ ФУНКЦИЙ Y=F(X)
! ПЕЧАТЬ В ФАЙЛ ДАННЫХ
! ИДЕНТИФИКАТОР БЛОКА ЧИСЛОВЫХ ДАННЫХ + LEGEND ДЛЯ ПЕРВОЙ КРИВОЙ НА ГРАФИКЕ

CHARACTER (LEN=*), INTENT (IN), OPTIONAL :: LEGEND ! LEGEND ДЛЯ ПЕРВОЙ КРИВОЙ НА ГРАФИКЕ

IF ( PRESENT(LEGEND) ) THEN
    WRITE (KNL,'(A)') CHDATA//" "//LEGEND
ELSE
    WRITE (KNL,'(A)') CHDATA ! БЕЗ ЛЕГЕНДЫ
END IF

END SUBROUTINE DGPY12

!**********************************************************************

SUBROUTINE DGPY1E

! PUBLIC
! ПОСТРОЕНИЕ 1D-ГРАФИКОВ ФУНКЦИЙ Y=F(X)
! ЗАПУСК НА ВЫПОЛНЕНИЕ ПРОГРАММЫ gfort1.py

CLOSE(KNL)

CALL EXECUTE_COMMAND_LINE(PYTHON//GFORT1//" "//CHKNL) ! , WAIT=.FALSE.

END SUBROUTINE DGPY1E

!**********************************************************************
!**********************************************************************

SUBROUTINE DGPY21 (FLNAME, & ! ИМЯ ФАЙЛОВ ДАННЫХ И РИСУНКА
                   NEX,    & ! KOЛИЧECTBO ТОЧЕК СЕТКИ ПO X-OCИ
                   NEY,    & ! KOЛИЧECTBO ТОЧЕК СЕТКИ ПO Y-OCИ
                   X1,     & ! LEFT   ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
                   X2,     & ! RIGHT  ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
                   Y1,     & ! BOTTOM ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
                   Y2,     & ! TOP    ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
                   Z,      & ! МАССИВ ЗНАЧЕНИЙ ФУНКЦИИ Z=F(X,Y)
                   QZMK,   & ! МАСШТАБИРУЮЩИЙ МНОЖИТЕЛЬ ДЛЯ Z
                   KMASK,  & ! ФЛАГ УЧЕТА МАСОК ПРИ МАСШТАБИРОВАНИИ
                   QMAMAX, & ! МАКСИМАЛЬНОЕ ЗНАЧЕНИЕ FOR MASK
                   QMAMIN, & !  МИНИМАЛЬНОЕ ЗНАЧЕНИЕ FOR MASK
                   QSCALE, & ! МАСШТАБИРОВАНИЕ УРОВНЯ ЗНАЧЕНИЙ
                   LEVELS, & ! КОЛИЧЕСТВО ИНТЕРВАЛОВ МЕЖДУ КОНТУРАМИ
                   NODES,  & ! ФЛАГ ПОРЯДОК ЭЛЕМЕНТА 0/1
                   KFILL,  & ! ФЛАГ ЦВЕТОВОЙ ЗАЛИВКИ
                   KANAL,  & ! НОМЕР КАНАЛА ВЫВОДА ДАННЫХ
                   TITLE)    ! НАИМЕНОВАНИЕ РИСУНКА

! PUBLIC
! ПОСТРОЕНИЕ 2D-IMAGES ФУНКЦИИ Z=F(X,Y) В ПРЯМОУГОЛЬНОЙ ОБЛАСТИ

CHARACTER (LEN=*), INTENT (IN)           :: FLNAME ! ИМЯ ФАЙЛОВ ДАННЫХ И РИСУНКА
INTEGER,           INTENT (IN)           :: NEX    ! KOЛИЧECTBO ТОЧЕК СЕТКИ ПO X-OCИ
INTEGER,           INTENT (IN)           :: NEY    ! KOЛИЧECTBO ТОЧЕК СЕТКИ ПO Y-OCИ
REAL,              INTENT (IN)           :: X1     ! LEFT   ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
REAL,              INTENT (IN)           :: X2     ! RIGHT  ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
REAL,              INTENT (IN)           :: Y1     ! BOTTOM ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
REAL,              INTENT (IN)           :: Y2     ! TOP    ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
REAL,              INTENT (IN)           :: Z(:)   ! МАССИВ КООРДИНАТ УЗЛОВ МГЭ-СЕТКИ
REAL,              INTENT (IN), OPTIONAL :: QZMK   ! МАСШТАБИРУЮЩИЙ МНОЖИТЕЛЬ ДЛЯ Z
INTEGER,           INTENT (IN), OPTIONAL :: KMASK  ! ФЛАГ УЧЕТА МАСОК ПРИ МАСШТАБИРОВАНИИ
REAL,              INTENT (IN), OPTIONAL :: QMAMAX ! МАКСИМАЛЬНОЕ ЗНАЧЕНИЕ FOR MASK
REAL,              INTENT (IN), OPTIONAL :: QMAMIN !  МИНИМАЛЬНОЕ ЗНАЧЕНИЕ FOR MASK
REAL,              INTENT (IN), OPTIONAL :: QSCALE ! МАСШТАБИРОВАНИЕ УРОВНЯ ЗНАЧЕНИЙ
INTEGER,           INTENT (IN), OPTIONAL :: NODES  ! ФЛАГ ПОРЯДОК ЭЛЕМЕНТА 0/1
INTEGER,           INTENT (IN), OPTIONAL :: LEVELS ! КОЛИЧЕСТВО ИНТЕРВАЛОВ МЕЖДУ КОНТУРАМИ
INTEGER,           INTENT (IN), OPTIONAL :: KFILL  ! ФЛАГ ЦВЕТОВОЙ ЗАЛИВКИ
INTEGER,           INTENT (IN), OPTIONAL :: KANAL  ! НОМЕР КАНАЛА ВЫВОДА ДАННЫХ
CHARACTER (LEN=*), INTENT (IN), OPTIONAL :: TITLE  ! НАИМЕНОВАНИЕ РИСУНКА

! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
CHARACTER (LEN=*), PARAMETER :: CHSUB ="DGPY21"
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

INTEGER :: IES ! КОД ВОЗВРАТА PYTHON
REAL    :: ZMK ! МАСШТАБИРУЮЩИЙ МНОЖИТЕЛЬ ДЛЯ Z

! НОМЕР КАНАЛА ВЫВОДА ДАННЫХ
IF ( PRESENT(KANAL) ) THEN
   IF ( ( KANAL < 20 ) .OR. ( KANAL > 99 ) ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                       &
      " НЕПРАВИЛЬНО УКАЗАН НОМЕР КАНАЛА ВЫВОДА ДАННЫХ KANAL=", KANAL,   &
      " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 20-99", CHERR3
      STOP
   ENDIF
   KNL=KANAL
ELSE
   KNL=52 ! BY DEFAULT
END IF
WRITE (CHKNL,'(I2)') KNL

! ИМЯ ФАЙЛОВ ДАННЫХ И РИСУНКА
WRITE (KNL,'(A)') FLNAME//"."//CHFT

IF ( NEX > 0 ) THEN ! KOЛИЧECTBO ТОЧЕК СЕТКИ ПO X-OCИ
   WRITE (KNL,'(A,I0)') "NEX ", NEX
ELSE
   WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                        &
   " НЕПРАВИЛЬНО ЗАДАНО KOЛИЧECTBO ТОЧЕК СЕТКИ ПO X-OCИ NEX=", NEX,   &
   " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
   STOP
END IF

IF ( NEY > 0 ) THEN ! KOЛИЧECTBO ТОЧЕК СЕТКИ ПO Y-OCИ
   WRITE (KNL,'(A,I0)') "NEY ", NEY
ELSE
   WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                        &
   " НЕПРАВИЛЬНО ЗАДАНО KOЛИЧECTBO ТОЧЕК СЕТКИ ПO Y-OCИ NEY=", NEY,   &
   " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
   STOP
END IF

! LEFT ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
WRITE (KNL,'(A,E12.6)') "XLEFT ", X1

! RIGHT ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
IF ( X2 > X1 ) THEN
    WRITE (KNL,'(A,E12.6)') "XRIGHT ", X2
ELSE
   WRITE (3,'(/A/A/A,E12.6/A,E12.6/A)') CHERR1, CHERR2,                       &
   " НЕПРАВИЛЬНО ЗАДАНА    RIGHT ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ X2=", X2,      &
   " ОНА ДОЛЖНА БЫТЬ БОЛЬШЕ LEFT ГРАНИЦЫ ПРЯМОУГОЛЬНОЙ ОБЛАСТИ X1=", X1, CHERR3
   STOP
END IF

! BOTTOM ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
WRITE (KNL,'(A,E12.6)') "YBOTTOM ", Y1

! TOP ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
IF ( Y2 > Y1 ) THEN
    WRITE (KNL,'(A,E12.6)') "YTOP ", Y2
ELSE
   WRITE (3,'(/A/A/A,E12.6/A,E12.6/A)') CHERR1, CHERR2,                         &
   " НЕПРАВИЛЬНО ЗАДАНА     TOP    ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ Y2=", Y2,      &
   " ОНА ДОЛЖНА БЫТЬ БОЛЬШЕ BOTTOM ГРАНИЦЫ ПРЯМОУГОЛЬНОЙ ОБЛАСТИ Y1=", Y1, CHERR3
   STOP
END IF

IF ( SIZE(Z) /= (NEX*NEY) ) THEN
   WRITE (3,'(/A/A/A,I0/3(A,I0)/A)') CHERR1, CHERR2,                           &
   " РАЗМЕРНОСТЬ МАССИВА Z SIZE(Z)=", SIZE(Z),                                 &
   " НЕ СООТВЕТСТВУЕТ РАЗМЕРНОСТИ СЕТКИ ", NEX, "*", NEY, "=", (NEX*NEY), CHERR3
   STOP
END IF

! НАИМЕНОВАНИЕ РИСУНКА
IF ( PRESENT(TITLE) ) THEN
    WRITE (KNL,'(A)') "TITLE "//TITLE
END IF

! ФЛАГ УЧЕТА МАСОК ПРИ МАСШТАБИРОВАНИИ
IF ( PRESENT(KMASK) ) THEN
   WRITE (KNL,'(A,I0)') "MASK ", KMASK
END IF

! МАКСИМАЛЬНОЕ ЗНАЧЕНИЕ FOR MASK
IF ( PRESENT(QMAMAX) ) THEN
    WRITE (KNL,'(A,E12.6)') "MAMAX ", QMAMAX
END IF

! МИНИМАЛЬНОЕ ЗНАЧЕНИЕ FOR MASK
IF ( PRESENT(QMAMIN) ) THEN
    WRITE (KNL,'(A,E12.6)') "MAMIN ", QMAMIN
END IF

! МАСШТАБИРОВАНИЕ УРОВНЯ ЗНАЧЕНИЙ
IF ( PRESENT(QSCALE) ) THEN
    WRITE (KNL,'(A,E12.6)') "SCALE ", QSCALE
END IF

! КОЛИЧЕСТВО ИНТЕРВАЛОВ МЕЖДУ КОНТУРАМИ
IF ( PRESENT(LEVELS) ) THEN
   WRITE (KNL,'(A,I0)') "LEVELS ", LEVELS
END IF

! ФЛАГ ПОРЯДОК ЭЛЕМЕНТА 0/1
IF ( PRESENT(NODES) ) THEN
   WRITE (KNL,'(A,I0)') "NODES ", NODES
END IF

! ФЛАГ ЦВЕТОВОЙ ЗАЛИВКИ
IF ( PRESENT(KFILL) ) THEN
    WRITE (KNL,'(A,I0)') "FILLED ", KFILL
END IF

! ИДЕНТИФИКАТОР БЛОКА ЧИСЛОВЫХ ДАННЫХ
WRITE (KNL,'(A)') CHDATA

! МАСШТАБИРУЮЩИЙ МНОЖИТЕЛЬ ДЛЯ Z
IF ( PRESENT(QZMK) ) THEN
    ZMK=QZMK
ELSE
    ZMK=1.0 ! BY DEFAULT
END IF

! ПЕЧАТЬ МАССИВА ЗНАЧЕНИЙ ФУНКЦИИ Z=F(X,Y) * МАСШТАБИРУЮЩИЙ МНОЖИТЕЛЬ
WRITE (KNL,'(E12.6)') Z*ZMK

CLOSE(KNL) ! ЗАКРЫТИЕ КАНАЛА ВЫВОДА ДАННЫХ

CALL EXECUTE_COMMAND_LINE(PYTHON//GFORT2//" "//CHKNL, EXITSTAT=IES) ! , WAIT=.FALSE.

IF ( IES /= 0 ) THEN
   WRITE (3,'(/A/A/A,I0/2A/A)') CHERR1, CHERR2, &
   " КОД ВОЗВРАТА PYTHON EXITSTAT=", IES,       &
   " GFORT2=", GFORT2, CHERR3
   STOP
END IF

END SUBROUTINE DGPY21

!**********************************************************************
!**********************************************************************

SUBROUTINE DGPY31 (FLNAME, & ! ИМЯ ФАЙЛОВ ДАННЫХ И РИСУНКА
                   NEX,    & ! KOЛИЧECTBO ТОЧЕК СЕТКИ ПO X-OCИ
                   NEY,    & ! KOЛИЧECTBO ТОЧЕК СЕТКИ ПO Y-OCИ
                   X1,     & ! LEFT   ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
                   X2,     & ! RIGHT  ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
                   Y1,     & ! BOTTOM ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
                   Y2,     & ! TOP    ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
                   Z,      & ! МАССИВ ЗНАЧЕНИЙ ФУНКЦИИ Z=F(X,Y)
                   QZMK,   & ! МАСШТАБИРУЮЩИЙ МНОЖИТЕЛЬ ДЛЯ Z
                   KANAL,  & ! НОМЕР КАНАЛА ВЫВОДА ДАННЫХ
                   TITLE)    ! НАИМЕНОВАНИЕ РИСУНКА

! PUBLIC
! ПОСТРОЕНИЕ 3D-SURFACE_PLOT ФУНКЦИИ Z=F(X,Y) В ПРЯМОУГОЛЬНОЙ ОБЛАСТИ

CHARACTER (LEN=*), INTENT (IN)           :: FLNAME ! ИМЯ ФАЙЛОВ ДАННЫХ И РИСУНКА
INTEGER,           INTENT (IN)           :: NEX    ! KOЛИЧECTBO ТОЧЕК СЕТКИ ПO X-OCИ
INTEGER,           INTENT (IN)           :: NEY    ! KOЛИЧECTBO ТОЧЕК СЕТКИ ПO Y-OCИ
REAL,              INTENT (IN)           :: X1     ! LEFT   ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
REAL,              INTENT (IN)           :: X2     ! RIGHT  ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
REAL,              INTENT (IN)           :: Y1     ! BOTTOM ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
REAL,              INTENT (IN)           :: Y2     ! TOP    ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
REAL,              INTENT (IN)           :: Z(:)   ! МАССИВ КООРДИНАТ УЗЛОВ МГЭ-СЕТКИ
REAL,              INTENT (IN), OPTIONAL :: QZMK   ! МАСШТАБИРУЮЩИЙ МНОЖИТЕЛЬ ДЛЯ Z
INTEGER,           INTENT (IN), OPTIONAL :: KANAL  ! НОМЕР КАНАЛА ВЫВОДА ДАННЫХ
CHARACTER (LEN=*), INTENT (IN), OPTIONAL :: TITLE  ! НАИМЕНОВАНИЕ РИСУНКА

! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
CHARACTER (LEN=*), PARAMETER :: CHSUB ="DGPY31"
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

REAL :: ZMK ! МАСШТАБИРУЮЩИЙ МНОЖИТЕЛЬ ДЛЯ Z

! НОМЕР КАНАЛА ВЫВОДА ДАННЫХ
IF ( PRESENT(KANAL) ) THEN
   IF ( ( KANAL < 20 ) .OR. ( KANAL > 99 ) ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                       &
      " НЕПРАВИЛЬНО УКАЗАН НОМЕР КАНАЛА ВЫВОДА ДАННЫХ KANAL=", KANAL,   &
      " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 20-99", CHERR3
      STOP
   ENDIF
   KNL=KANAL
ELSE
   KNL=53 ! BY DEFAULT
END IF
WRITE (CHKNL,'(I2)') KNL

! ИМЯ ФАЙЛОВ ДАННЫХ И РИСУНКА
WRITE (KNL,'(A)') FLNAME//"."//CHFT

IF ( NEX > 0 ) THEN ! KOЛИЧECTBO ТОЧЕК СЕТКИ ПO X-OCИ
   WRITE (KNL,'(A,I0)') "NEX ", NEX
ELSE
   WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                        &
   " НЕПРАВИЛЬНО ЗАДАНО KOЛИЧECTBO ТОЧЕК СЕТКИ ПO X-OCИ NEX=", NEX,   &
   " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
   STOP
END IF

IF ( NEY > 0 ) THEN ! KOЛИЧECTBO ТОЧЕК СЕТКИ ПO Y-OCИ
   WRITE (KNL,'(A,I0)') "NEY ", NEY
ELSE
   WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                        &
   " НЕПРАВИЛЬНО ЗАДАНО KOЛИЧECTBO ТОЧЕК СЕТКИ ПO Y-OCИ NEY=", NEY,   &
   " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
   STOP
END IF

! LEFT ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
WRITE (KNL,'(A,E12.6)') "XLEFT ", X1

! RIGHT ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
IF ( X2 > X1 ) THEN
    WRITE (KNL,'(A,E12.6)') "XRIGHT ", X2
ELSE
   WRITE (3,'(/A/A/A,E12.6/A,E12.6/A)') CHERR1, CHERR2,                       &
   " НЕПРАВИЛЬНО ЗАДАНА    RIGHT ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ X2=", X2,      &
   " ОНА ДОЛЖНА БЫТЬ БОЛЬШЕ LEFT ГРАНИЦЫ ПРЯМОУГОЛЬНОЙ ОБЛАСТИ X1=", X1, CHERR3
   STOP
END IF

! BOTTOM ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
WRITE (KNL,'(A,E12.6)') "YBOTTOM ", Y1

! TOP ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ
IF ( Y2 > Y1 ) THEN
    WRITE (KNL,'(A,E12.6)') "YTOP ", Y2
ELSE
   WRITE (3,'(/A/A/A,E12.6/A,E12.6/A)') CHERR1, CHERR2,                         &
   " НЕПРАВИЛЬНО ЗАДАНА     TOP    ГРАНИЦА ПРЯМОУГОЛЬНОЙ ОБЛАСТИ Y2=", Y2,      &
   " ОНА ДОЛЖНА БЫТЬ БОЛЬШЕ BOTTOM ГРАНИЦЫ ПРЯМОУГОЛЬНОЙ ОБЛАСТИ Y1=", Y1, CHERR3
   STOP
END IF

IF ( SIZE(Z) /= (NEX*NEY) ) THEN
   WRITE (3,'(/A/A/A,I0/3(A,I0)/A)') CHERR1, CHERR2,                           &
   " РАЗМЕРНОСТЬ МАССИВА Z SIZE(Z)=", SIZE(Z),                                 &
   " НЕ СООТВЕТСТВУЕТ РАЗМЕРНОСТИ СЕТКИ ", NEX, "*", NEY, "=", (NEX*NEY), CHERR3
   STOP
END IF

! НАИМЕНОВАНИЕ РИСУНКА
IF ( PRESENT(TITLE) ) THEN
    WRITE (KNL,'(A)') "TITLE "//TITLE
END IF

! ИДЕНТИФИКАТОР БЛОКА ЧИСЛОВЫХ ДАННЫХ
WRITE (KNL,'(A)') CHDATA

! МАСШТАБИРУЮЩИЙ МНОЖИТЕЛЬ ДЛЯ Z
IF ( PRESENT(QZMK) ) THEN
    ZMK=QZMK
ELSE
    ZMK=1.0 ! BY DEFAULT
END IF

! ПЕЧАТЬ МАССИВА ЗНАЧЕНИЙ ФУНКЦИИ Z=F(X,Y) * МАСШТАБИРУЮЩИЙ МНОЖИТЕЛЬ
WRITE (KNL,'(E12.6)') Z*ZMK

CLOSE(KNL) ! ЗАКРЫТИЕ КАНАЛА ВЫВОДА ДАННЫХ

CALL EXECUTE_COMMAND_LINE(PYTHON//GFORT3//" "//CHKNL) ! , WAIT=.FALSE.

END SUBROUTINE DGPY31

END MODULE DGPY
