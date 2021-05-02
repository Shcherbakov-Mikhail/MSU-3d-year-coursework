MODULE CNVX1 ! МОДУЛЬ ДЛЯ РЕШЕНИЯ ЗАДАЧ ВЫПУКЛОГО АНАЛИЗА

!**********************************************************************

             ! МОДУЛИ
USE MEM      ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ И ВЫЧИСЛЕНИЕ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ

!**********************************************************************
IMPLICIT NONE
!**********************************************************************
CHARACTER (LEN=*), PARAMETER, PRIVATE :: CHMDL="CNVX1"
!**********************************************************************

INTEGER, PROTECTED :: NCH     ! KOЛИЧECTBО ЭЛЕМЕНТОВ БД CONVEX HULL
INTEGER, PROTECTED :: NTS     ! KOЛИЧECTBО ЭЛЕМЕНТОВ БД TESTING SAMPLES
INTEGER, PROTECTED :: NKS     ! KOЛИЧECTBО КОМПОНЕНТ ЭЛЕМЕНТОВ БД

INTEGER, PRIVATE   :: KDBCH   ! НОМЕР КАНАЛА  ВВОДА БД CONVEX HULL
INTEGER, PRIVATE   :: KDBTS   ! НОМЕР КАНАЛА  ВВОДА БД TESTING SAMPLES
INTEGER, PRIVATE   :: KDBDS   ! НОМЕР КАНАЛА ВЫВОДА БД РАССТОЯНИЙ
INTEGER, PRIVATE   :: KDBEC   ! НОМЕР КАНАЛА ВЫВОДА БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ
INTEGER, PRIVATE   :: KDBEP   ! НОМЕР КАНАЛА ВЫВОДА БД ПРОЕКЦИЙ НА CONVEX HULL

REAL,    PRIVATE   :: DSTN    ! РАССТОЯНИЕ ДО CONVEX HULL
REAL,    PRIVATE   :: DSCM    ! РАССТОЯНИЕ ДО ЦЕНТРА МАСС CONVEX HULL

REAL,    PRIVATE   :: DCMAX   ! МАКСИМАЛЬНОЕ УДАЛЕНИЕ ЭЛЕМЕНТОВ CONVEX HULL ОТ ЕГО ЦЕНТРА МАСС
REAL,    PRIVATE   :: DCMDL   ! СРЕДНЕЕ      УДАЛЕНИЕ ЭЛЕМЕНТОВ CONVEX HULL ОТ ЕГО ЦЕНТРА МАСС

REAL,    PRIVATE   :: DMCH    ! ДИАМЕТР CONVEX HULL

INTEGER, PRIVATE   :: IW=1    ! ПAPAMETP УПPABЛEHИЯ ПEЧATЬЮ MOДУЛЯ (DEFAULT=1)
                              ! ECЛИ IW=1 - ПEЧATAETCЯ COOБЩEHИE O PAБOTE MOДУЛЯ
                              ! B ПPOTИBHOM CЛУЧAE ПEЧATЬ OTCУTCTBУET

INTEGER, PRIVATE   :: IOSTAT  ! КОД ОШИБКИ ОТКРЫТИЯ ФАЙЛА

INTEGER, PROTECTED :: IFAIL   ! КОД ЗАВЕРШЕНИЯ ИТЕРАЦИОННОГО ПРОЦЕССА

INTEGER, PRIVATE   :: IDFLT=1 ! ПАРАМЕТР УПРАВЛЕНИЯ ЗНАЧЕНИЯМИ ПО УМОЛЧАНИЮ (DEFAULT=1)
                              ! ECЛИ IDFLT=1 - ИСПОЛЬЗУЮТСЯ ЗНАЧЕНИЯ ПО УМОЛЧАНИЮ
                              ! B ПPOTИBHOM CЛУЧAE ЗНАЧЕНИЯ ПО УМОЛЧАНИЮ НЕ ИСПОЛЬЗУЮТСЯ

INTEGER, PRIVATE   :: ITER    ! МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ РЕШЕНИЯ
INTEGER, PRIVATE   :: IT      ! НОМЕР ТЕКУЩЕЙ  ИТЕРАЦИИ
!INTEGER, PRIVATE   :: IR     ! НОМЕР ИТЕРАЦИИ РЕСТАРТА

INTEGER, PRIVATE :: IREL      ! КОЛИЧЕСТВО ИТЕРАЦИЙ ДЛЯ ПРОВЕРКИ СХОДИМОСТИ РЕШЕНИЯ

INTEGER, PRIVATE :: IRIT1     ! KAЖДЫE IRIT1 ИТЕРАЦИЙ ПEЧATAETCЯ COOБЩEHИE O ПOГPEШHOCTИ РЕШЕНИЯ (DEFAULT=ITER+1)
INTEGER, PRIVATE :: IRIT2     ! KAЖДЫE IRIT2 ИТЕРАЦИЙ ПEЧATAETCЯ ПРИБЛИЖЕННОЕ РЕШЕНИЕ (DEFAULT=ITER+1)

INTEGER, PRIVATE :: IKRT      ! ПAPAMETP УПPABЛEHИЯ КРИТЕРИЕМ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА (DEFAULT=1)
                              ! ECЛИ IKRT=1 - РАЗНОСТЬ ДВУХ ПОСЛЕДОВАТЕЛЬНЫХ ПРИБЛИЖЕНИЙ
                              ! ECЛИ IKRT=2 - НЕВЯЗКИ (ОБОБЩЕННЫЕ УЗЛОВЫЕ ЗАЗОРЫ)



INTEGER, PRIVATE :: INRM      ! ПAPAMETP УПPABЛEHИЯ НОРМОЙ ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА (DEFAULT=2)
                              ! ECЛИ INRM=0 - L_INF-НОРМА (ЧЕБЫШЕВСКАЯ, КУБИЧЕСКАЯ)
                              ! ECЛИ INRM=1 - L1-НОРМА    (ОКТАЭДРИЧЕСКАЯ)
                              ! ECЛИ INRM=2 - L2-НОРМА    (СФЕРИЧЕСКАЯ, ЕВКЛИДОВА)

!INTEGER, PRIVATE :: KEY12     ! ПAPAMETP УПPABЛEHИЯ АЛГОРИТМОМ ПРИ ИЗМЕНЕНИИ РАЗМЕРНОСТИ (РП) (DEFAULT=1)
                              ! ECЛИ KEY12=0 - НЕ ДОПУСКАЕТСЯ ОДНОВРЕМЕННОЕ УВЕЛИЧЕНИЕ И УМЕНЬШЕНИЕ РАЗМЕРНОСТИ (РП)
                              ! ECЛИ KEY12=1 -    ДОПУСКАЕТСЯ ОДНОВРЕМЕННОЕ УВЕЛИЧЕНИЕ И УМЕНЬШЕНИЕ РАЗМЕРНОСТИ (РП)

REAL,    PRIVATE :: CFG       ! КОЭФФИЦИЕНТ ДЛЯ УСЛОВИЯ УВЕЛИЧЕНИЯ РАЗМЕРНОСТИ РП (DEFAULT=1.0)

REAL,    PRIVATE :: EABS      ! AБCOЛЮTHAЯ ПOГPEШHOCTЬ HУЛEBOГO РЕШЕНИЯ (DEFAULT=1.0E-20)
REAL,    PRIVATE :: EREL      ! OTHOCИTEЛЬHAЯ ПOГPEШHOCTЬ РЕШЕНИЯ       (DEFAULT=1.0E-06)

INTEGER, PRIVATE :: LCH       ! СЧЕТЧИК ЦИКЛА: NCH
INTEGER, PRIVATE :: LTS       ! СЧЕТЧИК ЦИКЛА: NTS
INTEGER, PRIVATE :: LKS       ! СЧЕТЧИК ЦИКЛА: NKS

REAL,    PRIVATE :: EPS1      ! ПАРАМЕТРЫ ДЛЯ ОЦЕНКИ ПOГPEШHOCTИ РЕШЕНИЯ
REAL,    PRIVATE :: EPS2
REAL,    PRIVATE :: EPS12

! ВСПОМОГАТЕЛЬНЫЕ ПЕРЕМЕННЫЕ ВЫЧИСЛИТЕЛЬНОГО АЛГОРИТМА

REAL,    PRIVATE :: ALFA      ! ДЛИНА ШАГА СПУСКА
!REAL,    PRIVATE :: BETA   ! КОЭФФИЦИЕНТ ПРИ ПРЕДЫДУЩЕМ  НАПРАВЛЕНИИ СПУСКА
!REAL,    PRIVATE :: GAMA   ! КОЭФФИЦИЕНТ ПРИ РЕСТАРТОВОМ НАПРАВЛЕНИИ СПУСКА

REAL,    PRIVATE :: A1, A2    ! ЧИСЛИТЕЛЬ И ЗНАМЕНАТЕЛЬ ДЛЯ ВЫЧИСЛЕНИЯ ALFA
!REAL,    PRIVATE :: B1, B2 ! ЧИСЛИТЕЛЬ И ЗНАМЕНАТЕЛЬ ДЛЯ ВЫЧИСЛЕНИЯ BETA
!REAL,    PRIVATE :: G1, G2 ! ЧИСЛИТЕЛЬ И ЗНАМЕНАТЕЛЬ ДЛЯ ВЫЧИСЛЕНИЯ GAMA

INTEGER, PRIVATE :: LFG       ! КОЛИЧЕСТВО КОМПОНЕНТ free gradient
INTEGER, PRIVATE :: LCG       ! КОЛИЧЕСТВО КОМПОНЕНТ chopped gradient
REAL,    PRIVATE :: FG        ! L2-НОРМА free gradient
REAL,    PRIVATE :: CG        ! L2-НОРМА chopped gradient

!**********************************************************************

                              ! УПРАВЛЯЮЩИЕ ПЕРЕМЕННЫЕ АЛГОРИТМА

!INTEGER, PRIVATE :: MEY0     ! ФЛАГ ФОРМУЛЫ ДЛЯ ВЫЧИСЛЕНИЯ НАПРАВЛЕНИЯ СПУСКА
                              ! MEY0=0 - ОДНОЧЛЕННАЯ ФОРМУЛА (АНТИГРАДИЕНТ)
                              ! MEY0=1 -  ДВУЧЛЕННАЯ ФОРМУЛА (СТАНДАРТНАЯ ФОРМУЛА МЕТОДА СОПРЯЖЕННЫХ ГРАДЕНТОВ)
                              ! MEY0=2 - ТРЕХЧЛЕННАЯ ФОРМУЛА (С УЧЕТОМ НАПРАВЛЕНИЯ РЕСТАРТА)

INTEGER, PRIVATE :: MEY1      ! КОЛИЧЕСТВО НАРУШЕННЫХ НЕАКТИВНЫХ ОГРАНИЧЕНИЙ-НЕРАВЕНСТВ ПРИ СПУСКЕ
INTEGER, PRIVATE :: MIT1      ! КОЛИЧЕСТВО НАРУШЕННЫХ НЕАКТИВНЫХ ОГРАНИЧЕНИЙ-НЕРАВЕНСТВ ПРИ ПРОЕКТИРОВАНИИ

INTEGER, PRIVATE :: MEY2      ! ФЛАГ УВЕЛИЧЕНИЯ РАЗМЕРНОСТИ РП
                              ! MEY2=0 - УВЕЛИЧЕНИЕ РАЗМЕРНОСТИ РП НЕ ВЫПОЛНЯЛОСЬ
                              ! MEY2>0 - УВЕЛИЧЕНИЕ РАЗМЕРНОСТИ РП

INTEGER, PRIVATE :: LWS       ! РАЗМЕРНОСТЬ РАБОЧЕГО ПОДПРОСТРАНСТВА

!INTEGER, PRIVATE :: LEY1    ! СЧЕТЧИК КОЛИЧЕСТВА РЕСТАРТОВ ПОСЛЕ УМЕНЬШЕНИЯ РАЗМЕРНОСТИ (РП)
!INTEGER, PRIVATE :: LEY2    ! СЧЕТЧИК КОЛИЧЕСТВА РЕСТАРТОВ ПОСЛЕ УВЕЛИЧЕНИЯ РАЗМЕРНОСТИ (РП)

!**********************************************************************

                                             ! МАССИВЫ
REAL,    ALLOCATABLE, PROTECTED :: DBCH(:,:) ! БД CONVEX HULL
REAL,    ALLOCATABLE, PROTECTED :: DBTS(:,:) ! БД TESTING SAMPLES
REAL,    ALLOCATABLE, PROTECTED :: DBDS(:,:) ! БД РАССТОЯНИЙ
REAL,    ALLOCATABLE, PROTECTED :: DBEC(:,:) ! БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ
REAL,    ALLOCATABLE, PROTECTED :: DBEP(:,:) ! БД ПРОЕКЦИЙ НА CONVEX HULL
REAL,    ALLOCATABLE, PROTECTED :: SMPL(:)   ! ЭЛЕМЕНТ БД

REAL,    ALLOCATABLE, PROTECTED :: CMCH(:)   ! ЦЕНТР МАСС CONVEX HULL

REAL,    ALLOCATABLE, PRIVATE   :: XXX(:)    ! ВЕКТОР НЕИЗВЕСТНЫХ
REAL,    ALLOCATABLE, PRIVATE   :: DDD(:)    ! ВЕКТОР НЕВЯЗКИ
REAL,    ALLOCATABLE, PRIVATE   :: GGG(:)    ! ВЕКТОР ГРАДИЕНТА
REAL,    ALLOCATABLE, PRIVATE   :: PPP(:)    ! ВЕКТОР НАПРАВЛЕНИЯ СПУСКА
REAL,    ALLOCATABLE, PRIVATE   :: YYY(:)    ! ВЕКТОР ПРОИЗВЕДЕНИЯ = МАТРИЦА ГЕССЕ * ВЕКТОР НАПРАВЛЕНИЯ СПУСКА
!REAL,    ALLOCATABLE, PRIVATE   :: PPR(:)    ! ВЕКТОР НАПРАВЛЕНИЯ СПУСКА ПОСЛЕ РЕСТАРТА
!REAL,    ALLOCATABLE, PRIVATE   :: YYR(:)    ! ВЕКТОР ПРОИЗВЕДЕНИЯ = МАТРИЦА ГЕССЕ * ВЕКТОР НАПРАВЛЕНИЯ РЕСТАРТА
INTEGER, ALLOCATABLE, PRIVATE   :: MIC(:)    ! МАССИВ ФЛАГОВ ОГРАНИЧЕНИЙ


!**********************************************************************

               ! ПОДПРОГРАММЫ
PUBLIC  CNVX10 ! УПРАВЛЕНИЕ РАБОТОЙ МОДУЛЯ

PRIVATE RDB1CH ! ВВОД БД DBCH & DBTS
PRIVATE WDB1CH ! ВЫВОД БД РАССТОЯНИЙ & КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ & ПРОЕКЦИЙ НА CONVEX HULL

PRIVATE CMCH1  ! ВЫЧИСЛЕНИЕ ЦЕНТРА МАСС CONVEX HULL И МАКСИМАЛЬНОГО УДАЛЕНИЯ ОТ НЕГО

PRIVATE DMCH1  ! ВЫЧИСЛЕНИЕ ДИАМЕТРА CONVEX HULL

PRIVATE BCH1PP ! ИНИЦИАЛИЗАЦИЯ ПАРАМЕТРОВ РЕШЕНИЯ ЗАДАЧИ ВЫПУКЛОГО АНАЛИЗА
PRIVATE BCH1WP ! ПЕЧАТЬ ПАРАМЕТРОВ МОДУЛЯ
PRIVATE BCH1AL ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ

               ! ПОДПРОГРАММЫ ДЛЯ ВЫЧИСЛЕНИЯ РАССТОЯНИЯ ЭЛЕМЕНТА SMPL ДО CONVEX HULL
PRIVATE BCH11  ! METOД ГPAДИEHTНОГО СПУСК
PRIVATE B1EPS0 ! ВЫЧИСЛЕНИЕ БАЗОВЫХ ЗНАЧЕНИЙ ДЛЯ ПPOBEPKИ CXOДИMOCTИ
PRIVATE B1EPS1 ! ПPOBEPKA CXOДИMOCTИ
PRIVATE B1WS1  ! ПEЧATЬ ИTEPAЦИИ
PRIVATE B1END1 ! ЗАВЕРШЕНИЕ ВЫЧИСЛИТЕЛЬНОГО ПРОЦЕССА
PRIVATE B1WE1  ! ПEЧATЬ COOБЩEHИЯ O PAБOTE MOДУЛЯ

!**********************************************************************
CONTAINS
!**********************************************************************

SUBROUTINE CNVX10 (KDFLT,  & ! ПAPAMETP УПPABЛEHИЯ ЗНАЧЕНИЯМИ ПО УМОЛЧАНИЮ
                   KIW,    & ! ПAPAMETP УПPABЛEHИЯ ПEЧATЬЮ MOДУЛЯ
                   KITER,  & ! МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ РЕШЕНИЯ
                   KIREL,  & ! КОЛИЧЕСТВО ИТЕРАЦИЙ ДЛЯ ПРОВЕРКИ СХОДИМОСТИ РЕШЕНИЯ
                   KIRIT1, & ! KAЖДЫE IRIT1 ИТЕРАЦИЙ ПEЧATAETCЯ COOБЩEHИE O ПOГPEШHOCTИ РЕШЕНИЯ
                   KIRIT2, & ! KAЖДЫE IRIT2 ИТЕРАЦИЙ ПEЧATAETCЯ ПРИБЛИЖЕННОЕ РЕШЕНИЕ
                   KIKRT,  & ! ПAPAMETP УПPABЛEHИЯ КРИТЕРИЕМ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
                   KINRM,  & ! ПAPAMETP УПPABЛEHИЯ НОРМОЙ ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
                   QEABS,  & ! AБCOЛЮTHAЯ ПOГPEШHOCTЬ HУЛEBOГO РЕШЕНИЯ
                   QEREL)    ! OTHOCИTEЛЬHAЯ ПOГPEШHOCTЬ РЕШЕНИЯ


! PUBLIC
! УПРАВЛЕНИЕ РАБОТОЙ МОДУЛЯ

INTEGER, INTENT (IN), OPTIONAL :: KDFLT  ! ПAPAMETP УПPABЛEHИЯ ЗНАЧЕНИЯМИ ПО УМОЛЧАНИЮ
INTEGER, INTENT (IN), OPTIONAL :: KIW    ! ПAPAMETP УПPABЛEHИЯ ПEЧATЬЮ MOДУЛЯ
INTEGER, INTENT (IN), OPTIONAL :: KITER  ! МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ РЕШЕНИЯ
INTEGER, INTENT (IN), OPTIONAL :: KIREL  ! КОЛИЧЕСТВО ИТЕРАЦИЙ ДЛЯ ПРОВЕРКИ СХОДИМОСТИ РЕШЕНИЯ
INTEGER, INTENT (IN), OPTIONAL :: KIRIT1 ! KAЖДЫE IRIT1 ИТЕРАЦИЙ ПEЧATAETCЯ COOБЩEHИE O ПOГPEШHOCTИ РЕШЕНИЯ
INTEGER, INTENT (IN), OPTIONAL :: KIRIT2 ! KAЖДЫE IRIT2 ИТЕРАЦИЙ ПEЧATAETCЯ ПРИБЛИЖЕННОЕ РЕШЕНИЕ
INTEGER, INTENT (IN), OPTIONAL :: KIKRT  ! ПAPAMETP УПPABЛEHИЯ КРИТЕРИЕМ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
INTEGER, INTENT (IN), OPTIONAL :: KINRM  ! ПAPAMETP УПPABЛEHИЯ НОРМОЙ ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
REAL,    INTENT (IN), OPTIONAL :: QEABS  ! AБCOЛЮTHAЯ ПOГPEШHOCTЬ HУЛEBOГO РЕШЕНИЯ
REAL,    INTENT (IN), OPTIONAL :: QEREL  ! OTHOCИTEЛЬHAЯ ПOГPEШHOCTЬ РЕШЕНИЯ

CALL APRM   ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ

CALL RDB1CH ! ВВОД БД DBCH & DBTS

CALL CMCH1  ! ВЫЧИСЛЕНИЕ ЦЕНТРА МАСС CONVEX HULL И МАКСИМАЛЬНОГО УДАЛЕНИЯ ОТ НЕГО

CALL DMCH1  ! ВЫЧИСЛЕНИЕ ДИАМЕТРА CONVEX HULL

! ИНИЦИАЛИЗАЦИЯ ПАРАМЕТРОВ РЕШЕНИЯ ЗАДАЧИ ВЫПУКЛОГО АНАЛИЗА
CALL BCH1PP (KDFLT =KDFLT,  & ! ПAPAMETP УПPABЛEHИЯ ЗНАЧЕНИЯМИ ПО УМОЛЧАНИЮ
             KIW   =KIW,    & ! ПAPAMETP УПPABЛEHИЯ ПEЧATЬЮ MOДУЛЯ
             KITER =KITER,  & ! МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ РЕШЕНИЯ
             KIREL =KIREL,  & ! КОЛИЧЕСТВО ИТЕРАЦИЙ ДЛЯ ПРОВЕРКИ СХОДИМОСТИ РЕШЕНИЯ
             KIRIT1=KIRIT1, & ! KAЖДЫE IRIT1 ИТЕРАЦИЙ ПEЧATAETCЯ COOБЩEHИE O ПOГPEШHOCTИ РЕШЕНИЯ
             KIRIT2=KIRIT2, & ! KAЖДЫE IRIT2 ИТЕРАЦИЙ ПEЧATAETCЯ ПРИБЛИЖЕННОЕ РЕШЕНИЕ
             KIKRT =KIKRT,  & ! ПAPAMETP УПPABЛEHИЯ КРИТЕРИЕМ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
             KINRM =KINRM,  & ! ПAPAMETP УПPABЛEHИЯ НОРМОЙ ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
             QEABS =QEABS,  & ! AБCOЛЮTHAЯ ПOГPEШHOCTЬ HУЛEBOГO РЕШЕНИЯ
             QEREL =QEREL)    ! OTHOCИTEЛЬHAЯ ПOГPEШHOCTЬ РЕШЕНИЯ

! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ
CALL BCH1AL

DO LTS=1,NTS ! ЦИКЛ ПО ВСЕМ ЭЛЕМЕНТАМ БД TESTING SAMPLES

   SMPL=DBTS(:,LTS) ! ЭЛЕМЕНТ БД TESTING SAMPLES

   CALL BCH11 ! ВЫЧИСЛЕНИЕ РАССТОЯНИЯ ЭЛЕМЕНТА SMPL ДО CONVEX HULL

   DBDS(1,LTS)=DSTN ! ЭЛЕМЕНТ БД РАССТОЯНИЙ
   DBDS(2,LTS)=DSCM ! РАССТОЯНИЕ ДО ЦЕНТРА МАСС CONVEX HULL

   DBEC(:,LTS)=XXX  ! ЭЛЕМЕНТ БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ

   DBEP(:,LTS)=MATMUL(DBCH,XXX) ! ПРОЕКЦИЯ НА CONVEX HULL

END DO

CALL WDB1CH ! ВЫВОД БД РАССТОЯНИЙ & КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ & ПРОЕКЦИЙ НА CONVEX HULL

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКАХ

   IF ( PRESENT(KIW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KIW
   ELSE
      IW=1 ! ПО УМОЛЧАНИЮ ПЕЧАТЬ ПОДПРОГРАММЫ ВКЛЮЧЕНА
   END IF

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE CNVX10

!**********************************************************************

SUBROUTINE RDB1CH

! PRIVATE
! ВВОД БД DBCH & DBTS

CHARACTER (LEN=*), PARAMETER :: CHSUB ="RDB1CH"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! ВВОД НОМЕРОВ КАНАЛОВ ВВОДА-ВЫВОДА
OPEN(1, ACTION='READ', IOSTAT=IOSTAT)

READ (1,*) KDBCH, & ! НОМЕР КАНАЛА  ВВОДА БД CONVEX HULL
           KDBTS, & ! НОМЕР КАНАЛА  ВВОДА БД TESTING SAMPLES
           KDBDS, & ! НОМЕР КАНАЛА ВЫВОДА БД РАССТОЯНИЙ
           KDBEC, & ! НОМЕР КАНАЛА ВЫВОДА БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ
           KDBEP    ! НОМЕР КАНАЛА ВЫВОДА БД ПРОЕКЦИЙ НА CONVEX HULL

CLOSE (1) ! ЗАКРЫТИЕ ФАЙЛА НОМЕРОВ КАНАЛОВ ВВОДА-ВЫВОДА

! ВВОД БД CONVEX HULL
IF ( ( KDBCH > 10 ) .OR. ( KDBCH < 100 ) ) THEN ! НОМЕР КАНАЛА ВВОДА БД CONVEX HULL

   OPEN(KDBCH, ACTION='READ', IOSTAT=IOSTAT)    ! БД CONVEX HULL

   IF ( IOSTAT /= 0 ) THEN
      WRITE (3,'(/A/A/A,I0/A)') CHERR1, CHERR2,                                   &
      " ПРОИЗОШЛА ОШИБКА ПРИ ОТКРЫТИИ ФАЙЛА БД CONVEX HULL IOSTAT=", IOSTAT, CHERR3
      STOP
   END IF

   READ (KDBCH,*) NCH, & ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ БД CONVEX HULL
                  NKS    ! КОЛИЧЕСТВО КОМПОНЕНТ ЭЛЕМЕНТОВ БД

   IF ( NCH < 1 ) THEN ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ БД CONVEX HULL
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                          &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО ЭЛЕМЕНТОВ БД CONVEX HULL NCH=", NCH, &
      " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
      STOP
   END IF

   IF ( NKS < 1 ) THEN ! КОЛИЧЕСТВО КОМПОНЕНТ ЭЛЕМЕНТОВ БД CONVEX HULL
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                                    &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО КОМПОНЕНТ ЭЛЕМЕНТОВ БД CONVEX HULL NKS=", NKS, &
      " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
      STOP
   END IF

   CALL AMFLT2 ("DBCH", DBCH, NKS, NCH, IRL=1) ! РАЗМЕЩЕНИЕ БД CONVEX HULL

   READ (KDBCH,*) DBCH ! БД CONVEX HULL

   CLOSE (KDBCH) ! ЗАКРЫТИЕ ФАЙЛА БД CONVEX HULL

ELSE ! НОМЕР КАНАЛА ВВОДА БД CONVEX HULL

   WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                            &
   " НЕПРАВИЛЬНО ЗАДАН НОМЕР КАНАЛА  ВВОДА БД CONVEX HULL KDBCH=", KDBCH, &
   " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 11-99", CHERR3
   STOP

END IF

! ВВОД БД TESTING SAMPLES
IF ( ( KDBTS > 10 ) .OR. ( KDBTS < 100 ) ) THEN ! НОМЕР КАНАЛА ВВОДА БД TESTING SAMPLES

   OPEN(KDBTS, ACTION='READ', IOSTAT=IOSTAT)    ! БД TESTING SAMPLES

   IF ( IOSTAT /= 0 ) THEN
      WRITE (3,'(/A/A/A,I0/A)') CHERR1, CHERR2,                                       &
      " ПРОИЗОШЛА ОШИБКА ПРИ ОТКРЫТИИ ФАЙЛА БД TESTING SAMPLES IOSTAT=", IOSTAT, CHERR3
      STOP
   END IF

   READ (KDBTS,*) NTS, & ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ БД TESTING SAMPLES
                  NKS    ! КОЛИЧЕСТВО КОМПОНЕНТ ЭЛЕМЕНТОВ БД

   IF ( NTS < 1 ) THEN ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ БД TESTING SAMPLES
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                              &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО ЭЛЕМЕНТОВ БД TESTING SAMPLES NTS=", NTS, &
      " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
      STOP
   END IF

   IF ( NKS < 1 ) THEN ! КОЛИЧЕСТВО КОМПОНЕНТ ЭЛЕМЕНТОВ БД TESTING SAMPLES
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                                        &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО КОМПОНЕНТ ЭЛЕМЕНТОВ БД TESTING SAMPLES NKS=", NKS, &
      " ОНО ДОЛЖНО БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
      STOP
   END IF

   CALL AMFLT2 ("DBTS", DBTS, NKS, NTS, IRL=1) ! РАЗМЕЩЕНИЕ БД TESTING SAMPLES

   READ (KDBTS,*) DBTS ! БД TESTING SAMPLES

   CLOSE (KDBTS) ! ЗАКРЫТИЕ ФАЙЛА БД TESTING SAMPLES

ELSE ! НОМЕР КАНАЛА ВВОДА БД TESTING SAMPLES

   WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                                           &
   " НЕПРАВИЛЬНО ЗАДАН НОМЕР КАНАЛА  ВВОДА БД TESTING SAMPLES KDBTS=", KDBTS, &
   " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 11-99", CHERR3
   STOP

END IF

! ПРОВЕРКА СООТВЕТСТВИЯ РАЗМЕРОВ МАССИВОВ БД CONVEX HULL & TESTING SAMPLES
IF ( SIZE(DBCH,1) /= SIZE(DBTS,1) ) THEN

   WRITE (3,'(/A/A/A/A,I0,A,I0/A)') CHERR1, CHERR2,                       &
   " ОБНАРУЖЕНО НЕСООТВЕТСТВИЕ РАЗМЕРОВ МАССИВОВ БД",                     &
   " SIZE(DBCH,1)=", SIZE(DBCH,1), " И SIZE(DBTS,1)=", SIZE(DBTS,1), CHERR3
   STOP

END IF

! РАЗМЕЩЕНИЕ МАССИВА ЭЛЕМЕНТ БД
CALL AMFLT1 ("SMPL", SMPL, NKS, IRL=1)

! ОТКРЫТИЕ КАНАЛА ВЫВОДА БД РАССТОЯНИЙ
IF ( ( KDBDS > 10 ) .OR. ( KDBDS < 100 ) ) THEN ! НОМЕР КАНАЛА ВЫВОДА БД РАССТОЯНИЙ

   OPEN(KDBDS, ACTION='WRITE', IOSTAT=IOSTAT) ! БД РАССТОЯНИЙ

   IF ( IOSTAT /= 0 ) THEN
      WRITE (3,'(/A/A/A,I0/A)') CHERR1, CHERR2,                                  &
      " ПРОИЗОШЛА ОШИБКА ПРИ ОТКРЫТИИ ФАЙЛА БД РАССТОЯНИЙ IOSTAT=", IOSTAT, CHERR3
      STOP
   END IF

ELSE ! НОМЕР КАНАЛА ВЫВОДА БД РАССТОЯНИЙ

   WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                           &
   " НЕПРАВИЛЬНО ЗАДАН НОМЕР КАНАЛА ВЫВОДА БД РАССТОЯНИЙ KDBDS=", KDBDS, &
   " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 11-99", CHERR3
   STOP

END IF

! РАЗМЕЩЕНИЕ БД РАССТОЯНИЙ
CALL AMFLT2 ("DBDS", DBDS, 2, NTS, IRL=1)

! ЗАПИСЬ ЗАГОЛОВКА БД РАССТОЯНИЙ
WRITE (KDBDS,*) SIZE(DBDS,2), SIZE(DBDS,1)

! ОТКРЫТИЕ КАНАЛА ВЫВОДА БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ
IF ( ( KDBEC > 10 ) .OR. ( KDBEC < 100 ) ) THEN ! НОМЕР КАНАЛА ВЫВОДА БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ

   OPEN(KDBEC, ACTION='WRITE', IOSTAT=IOSTAT) ! БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ

   IF ( IOSTAT /= 0 ) THEN
      WRITE (3,'(/A/A/A,I0/A)') CHERR1, CHERR2,                                                &
      " ПРОИЗОШЛА ОШИБКА ПРИ ОТКРЫТИИ ФАЙЛА БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ IOSTAT=", IOSTAT, CHERR3
      STOP
   END IF

ELSE ! НОМЕР КАНАЛА ВЫВОДА БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ

   WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                                         &
   " НЕПРАВИЛЬНО ЗАДАН НОМЕР КАНАЛА ВЫВОДА БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ KDBEC=", KDBEC, &
   " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 11-99", CHERR3
   STOP

END IF

! РАЗМЕЩЕНИЕ БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ
CALL AMFLT2 ("DBEC", DBEC, NCH, NTS, IRL=1)

! ЗАПИСЬ ЗАГОЛОВКА БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ
WRITE (KDBEC,*) SIZE(DBEC,2), SIZE(DBEC,1)

! ОТКРЫТИЕ КАНАЛА ВЫВОДА БД ПРОЕКЦИЙ НА CONVEX HULL
IF ( ( KDBEP > 10 ) .OR. ( KDBEP < 100 ) ) THEN ! НОМЕР КАНАЛА ВЫВОДА БД ПРОЕКЦИЙ НА CONVEX HULL

   OPEN(KDBEP, ACTION='WRITE', IOSTAT=IOSTAT) ! БД ПРОЕКЦИЙ НА CONVEX HULL

   IF ( IOSTAT /= 0 ) THEN
      WRITE (3,'(/A/A/A,I0/A)') CHERR1, CHERR2,                                                &
      " ПРОИЗОШЛА ОШИБКА ПРИ ОТКРЫТИИ ФАЙЛА БД ПРОЕКЦИЙ НА CONVEX HULL IOSTAT=", IOSTAT, CHERR3
      STOP
   END IF

ELSE ! НОМЕР КАНАЛА ВЫВОДА БД ПРОЕКЦИЙ НА CONVEX HULL

   WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                                        &
   " НЕПРАВИЛЬНО ЗАДАН НОМЕР КАНАЛА ВЫВОДА БД ПРОЕКЦИЙ НА CONVEX HULL KDBEP=", KDBEP, &
   " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 11-99", CHERR3
   STOP

END IF

! РАЗМЕЩЕНИЕ БД ПРОЕКЦИЙ НА CONVEX HULL
CALL AMFLT2 ("DBEP", DBEP, NKS, NTS, IRL=1)

! ЗАПИСЬ ЗАГОЛОВКА БД ПРОЕКЦИЙ НА CONVEX HULL
WRITE (KDBEP,*) SIZE(DBEP,2), SIZE(DBEP,1)

IF ( IW == 1 ) THEN
   WRITE (3,'(/A/A,I2,A,I0/A,I2,A,I0/A,I2/A,I2/A,I2/A,I0)') CHMSG1,                  &
   " ВЫПОЛНЕН ВВОД БД CONVEX HULL                    KDBCH=", KDBCH, "   NCH=", NCH, &
   " ВЫПОЛНЕН ВВОД БД TESTING SAMPLES                KDBTS=", KDBTS, "   NTS=", NTS, &
   " ОТКРЫТ КАНАЛ ВЫВОДА БД РАССТОЯНИЙ               KDBDS=", KDBDS,                 &
   " ОТКРЫТ КАНАЛ ВЫВОДА БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ KDBEC=", KDBEC,                 &
   " ОТКРЫТ КАНАЛ ВЫВОДА БД ПРОЕКЦИЙ НА CONVEX HULL  KDBEP=", KDBEP,                 &
   " РАЗМЕРНОСТЬ ПРОСТРАНСТВА CONVEX HULL              NKS=", NKS

END IF

END SUBROUTINE RDB1CH

!**********************************************************************

SUBROUTINE WDB1CH

! PRIVATE
! ВЫВОД БД РАССТОЯНИЙ & КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ & ПРОЕКЦИЙ НА CONVEX HULL

CHARACTER (LEN=*), PARAMETER :: CHSUB ="WDB1CH"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

DO LTS=1,NTS ! ЦИКЛ ПО ВСЕМ ЭЛЕМЕНТАМ БД РАССТОЯНИЙ & КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ

   WRITE (KDBDS,*) DBDS(:,LTS) ! ЗАПИСЬ БД РАССТОЯНИЙ
   WRITE (KDBEC,*) DBEC(:,LTS) ! ЗАПИСЬ БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ
   WRITE (KDBEP,*) DBEP(:,LTS) ! ЗАПИСЬ БД ПРОЕКЦИЙ НА CONVEX HULL

END DO

CLOSE (KDBDS) ! ЗАКРЫТИЕ ФАЙЛА БД РАССТОЯНИЙ
CLOSE (KDBEC) ! ЗАКРЫТИЕ ФАЙЛА БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ
CLOSE (KDBEP) ! ЗАКРЫТИЕ ФАЙЛА БД ПРОЕКЦИЙ НА CONVEX HULL

IF ( IW == 1 ) THEN
   WRITE (3,'(/A/A,I2,A,I0/A,I2,A,I0/A,I2,A,I0)') CHMSG1,                       &
   " ВЫПОЛНЕН ВЫВОД БД РАССТОЯНИЙ               KDBDS=", KDBDS, "   NTS=", NTS, &
   " ВЫПОЛНЕН ВЫВОД БД КОЭФФИЦИЕНТОВ РАЗЛОЖЕНИЯ KDBEC=", KDBEC, "   NCH=", NCH, &
   " ВЫПОЛНЕН ВЫВОД БД ПРОЕКЦИЙ НА CONVEX HULL  KDBEP=", KDBEP, "   NKS=", NKS

END IF

END SUBROUTINE WDB1CH

!**********************************************************************
!**********************************************************************

SUBROUTINE CMCH1

! PRIVATE
! ВЫЧИСЛЕНИЕ ЦЕНТРА МАСС CONVEX HULL И МАКСИМАЛЬНОГО УДАЛЕНИЯ ОТ НЕГО

CHARACTER (LEN=*), PARAMETER :: CHSUB ="CMCH1"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL

! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ МАССИВА - ЦЕНТР МАСС CONVEX HULL
CALL AMFLT1 ("CMCH", CMCH, NKS, IRL=1)

CMCH=SUM(DBCH,2)/REAL(SIZE(DBCH,2))

! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ МАССИВА РАССТОЯНИЙ ДО ЦЕНТРА МАСС CONVEX HULL
CALL AMFLT1 ("XXX", XXX, NCH, IRL=1)

! РАССТОЯНИЯ ДО ЦЕНТРА МАСС CONVEX HULL
FORALL (LCH=1:NCH) XXX(LCH)=SQRT(SUM((DBCH(:,LCH)-CMCH)**2))

! СРЕДНЕЕ УДАЛЕНИЕ ЭЛЕМЕНТОВ CONVEX HULL ОТ ЕГО ЦЕНТРА МАСС
DCMDL=SUM(XXX)/REAL(NCH)

! МАКСИМАЛЬНОЕ УДАЛЕНИЕ ЭЛЕМЕНТОВ CONVEX HULL ОТ ЕГО ЦЕНТРА МАСС
DCMAX=MAXVAL(XXX)

IF ( IW >= 1 ) THEN

   WRITE (3,'(/A/A)') CHMSG1," ВЫЧИСЛЕН ЦЕНТР МАСС CONVEX HULL"

   WRITE(3,'(1X,I10,3X,E12.6)') (LKS,CMCH(LKS),LKS=1,NKS)

   WRITE (3,'(/A)') " ВЫЧИСЛЕНЫ РАССТОЯНИЯ ДО ЦЕНТРА МАСС CONVEX HULL"

   WRITE(3,'(1X,I10,3X,E12.6)') (LCH,XXX(LCH),LCH=1,NCH)

   WRITE (3,'(/A,E12.6)') " СРЕДНЕЕ      УДАЛЕНИЕ ЭЛЕМЕНТОВ CONVEX HULL ОТ ЕГО ЦЕНТРА МАСС=", DCMDL
   WRITE (3,'( A,E12.6)') " МАКСИМАЛЬНОЕ УДАЛЕНИЕ ЭЛЕМЕНТОВ CONVEX HULL ОТ ЕГО ЦЕНТРА МАСС=", DCMAX

END IF

! ОСВОБОЖДЕНИИ ПАМЯТИ МАССИВА
CALL DAMSV("XXX", AMSV1=XXX)

END SUBROUTINE CMCH1

!**********************************************************************

SUBROUTINE DMCH1

! PRIVATE
! ВЫЧИСЛЕНИЕ ДИАМЕТРА CONVEX HULL

INTEGER :: LCH1 ! СЧЕТЧИК ЦИКЛА: NCH
INTEGER :: LCH2 ! СЧЕТЧИК ЦИКЛА: NCH
REAL    :: DSTN ! КВАДРАТ РАССТОЯНИЯ МЕЖДУ ЭЛЕМЕНТАМИ CONVEX HULL

CHARACTER (LEN=*), PARAMETER :: CHSUB ="DMCH1"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL

DMCH=0.0 ! КВАДРАТ ДИАМЕТРА CONVEX HULL
DO LCH1=1,NCH-1
   DO LCH2=LCH1+1,NCH
      DSTN=SUM((DBCH(:,LCH1)-DBCH(:,LCH2))**2) ! КВАДРАТ РАССТОЯНИЯ МЕЖДУ ЭЛЕМЕНТАМИ CONVEX HULL
      IF ( DMCH < DSTN ) DMCH=DSTN
   END DO
END DO

DMCH=SQRT(DMCH) ! ДИАМЕТР CONVEX HULL

IF ( IW >= 1 ) THEN

   WRITE (3,'(/A/A,E12.6)') CHMSG1," ДИАМЕТР CONVEX HULL=", DMCH

END IF


END SUBROUTINE DMCH1

!**********************************************************************
!**********************************************************************

SUBROUTINE BCH1PP (KDFLT,  & ! ПAPAMETP УПPABЛEHИЯ ЗНАЧЕНИЯМИ ПО УМОЛЧАНИЮ
                   KIW,    & ! ПAPAMETP УПPABЛEHИЯ ПEЧATЬЮ MOДУЛЯ
                   KITER,  & ! МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ РЕШЕНИЯ
                   KIREL,  & ! КОЛИЧЕСТВО ИТЕРАЦИЙ ДЛЯ ПРОВЕРКИ СХОДИМОСТИ РЕШЕНИЯ
                   KIRIT1, & ! KAЖДЫE IRIT1 ИТЕРАЦИЙ ПEЧATAETCЯ COOБЩEHИE O ПOГPEШHOCTИ РЕШЕНИЯ
                   KIRIT2, & ! KAЖДЫE IRIT2 ИТЕРАЦИЙ ПEЧATAETCЯ ПРИБЛИЖЕННОЕ РЕШЕНИЕ
                   KIKRT,  & ! ПAPAMETP УПPABЛEHИЯ КРИТЕРИЕМ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
                   KINRM,  & ! ПAPAMETP УПPABЛEHИЯ НОРМОЙ ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
                   QEABS,  & ! AБCOЛЮTHAЯ ПOГPEШHOCTЬ HУЛEBOГO РЕШЕНИЯ
                   QEREL)    ! OTHOCИTEЛЬHAЯ ПOГPEШHOCTЬ РЕШЕНИЯ

! PUBLIC
! ИНИЦИАЛИЗАЦИЯ ПАРАМЕТРОВ РЕШЕНИЯ ЗАДАЧИ ВЫПУКЛОГО АНАЛИЗА

INTEGER, INTENT (IN), OPTIONAL :: KDFLT  ! ПAPAMETP УПPABЛEHИЯ ЗНАЧЕНИЯМИ ПО УМОЛЧАНИЮ
INTEGER, INTENT (IN), OPTIONAL :: KIW    ! ПAPAMETP УПPABЛEHИЯ ПEЧATЬЮ MOДУЛЯ
INTEGER, INTENT (IN), OPTIONAL :: KITER  ! МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ РЕШЕНИЯ
INTEGER, INTENT (IN), OPTIONAL :: KIREL  ! КОЛИЧЕСТВО ИТЕРАЦИЙ ДЛЯ ПРОВЕРКИ СХОДИМОСТИ РЕШЕНИЯ
INTEGER, INTENT (IN), OPTIONAL :: KIRIT1 ! KAЖДЫE IRIT1 ИТЕРАЦИЙ ПEЧATAETCЯ COOБЩEHИE O ПOГPEШHOCTИ РЕШЕНИЯ
INTEGER, INTENT (IN), OPTIONAL :: KIRIT2 ! KAЖДЫE IRIT2 ИТЕРАЦИЙ ПEЧATAETCЯ ПРИБЛИЖЕННОЕ РЕШЕНИЕ
INTEGER, INTENT (IN), OPTIONAL :: KIKRT  ! ПAPAMETP УПPABЛEHИЯ КРИТЕРИЕМ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
INTEGER, INTENT (IN), OPTIONAL :: KINRM  ! ПAPAMETP УПPABЛEHИЯ НОРМОЙ ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
REAL,    INTENT (IN), OPTIONAL :: QEABS  ! AБCOЛЮTHAЯ ПOГPEШHOCTЬ HУЛEBOГO РЕШЕНИЯ
REAL,    INTENT (IN), OPTIONAL :: QEREL  ! OTHOCИTEЛЬHAЯ ПOГPEШHOCTЬ РЕШЕНИЯ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="BCH1PP"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! ПАРАМЕТР УПРАВЛЕНИЯ ЗНАЧЕНИЯМИ ПО УМОЛЧАНИЮ
IF ( IDFLT /= 1 ) THEN        ! ПОВТОРНЫЙ ЗАПУСК ПОДПРОГРАММЫ
   IF ( PRESENT(KDFLT) ) THEN ! ЗАДАНО НОВОЕ ЗНАЧЕНИЕ
      IDFLT=KDFLT             ! ПРИСВОЕНИЕ НОВОГО ЗНАЧЕНИЯ
   END IF
END IF

IF ( IDFLT == 1 ) THEN ! ПЕРВЫЙ ЗАПУСК ПОДПРОГРАММЫ ИЛИ УСТАНОВКА ЗНАЧЕНИЙ ПО УМОЛЧАНИЮ

   IW   =1       ! ПEЧATAETCЯ КРАТКОЕ COOБЩEHИE O PAБOTE
   ITER =50*NCH  ! КОЛИЧЕСТВО ПЕРЕМЕННЫХ - KOЛИЧECTBО ЭЛЕМЕНТОВ БД CONVEX HULL
   IREL =10      ! КОЛИЧЕСТВО ИТЕРАЦИЙ ДЛЯ ПРОВЕРКИ СХОДИМОСТИ РЕШЕНИЯ
   IRIT1=ITER+1  ! ПЕЧАТЬ COOБЩEHИЯ O ПOГPEШHOCTИ РЕШЕНИЯ ОТСУТСТВУЕТ
   IRIT2=ITER+1  ! ПЕЧАТЬ ПРИБЛИЖЕННОГО РЕШЕНИЯ ОТСУТСТВУЕТ

   IKRT =1       ! КРИТЕРИЙ ОСТАНОВКИ - РАЗНОСТЬ ДВУХ ПРИБЛИЖЕНИЙ СРАВНИВАЕТСЯ С РЕШЕНИЕМ"
  !IKRT =2       ! КРИТЕРИЙ ОСТАНОВКИ - РАЗНОСТЬ ДВУХ НЕВЯЗОК СРАВНИВАЕТСЯ С ЭЛЕМЕНТОМ SMPL"
  !IKRT =3       ! КРИТЕРИЙ ОСТАНОВКИ - free gradient СРАВНИВАЕТСЯ С initial gradient"

  !INRM =0       ! НОРМА ДЛЯ КРИТЕРИЯ - L_INF-НОРМА (ЧЕБЫШЕВСКАЯ, КУБИЧЕСКАЯ)"
  !INRM =1       ! НОРМА ДЛЯ КРИТЕРИЯ - L1-НОРМА (ОКТАЭДРИЧЕСКАЯ)"
   INRM =2       ! НОРМА ДЛЯ КРИТЕРИЯ - L2-НОРМА (СФЕРИЧЕСКАЯ, ЕВКЛИДОВА)"

   EABS =1.0E-20 ! AБCOЛЮTHAЯ ПOГPEШHOCTЬ HУЛEBOГO РЕШЕНИЯ
   EREL =1.0E-10 ! OTHOCИTEЛЬHAЯ ПOГPEШHOCTЬ РЕШЕНИЯ

END IF

IDFLT=0 ! ПАРАМЕТР УПРАВЛЕНИЯ ЗНАЧЕНИЯМИ ПРИ ПОВТОРНОМ ЗАПУСКЕ ПОДПРОГРАММЫ

! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
IF ( PRESENT(KIW) ) THEN
   IW=KIW
END IF

! МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ РЕШЕНИЯ
IF ( PRESENT(KITER) ) THEN
   IF ( KITER < 1 ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,   &
      " НЕПРАВИЛЬНО УКАЗАН ПАРАМЕТР KITER=", KITER, &
      " ОН ДОЛЖЕН БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
      STOP
   ELSE
      ITER=KITER
   END IF
END IF

! КОЛИЧЕСТВО ИТЕРАЦИЙ ДЛЯ ПРОВЕРКИ СХОДИМОСТИ РЕШЕНИЯ
IF ( PRESENT(KIREL) ) THEN
   IF ( KIREL < 1 ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,   &
      " НЕПРАВИЛЬНО УКАЗАН ПАРАМЕТР KIREL=", KIREL, &
      " ОН ДОЛЖЕН БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
      STOP
   ELSE
      IREL=KIREL
   END IF
END IF

! KAЖДЫE IRIT1 ИТЕРАЦИЙ ПEЧATAETCЯ COOБЩEHИE O ПOГPEШHOCTИ РЕШЕНИЯ
IF ( PRESENT(KIRIT1) ) THEN
   IF ( KIRIT1 < 1 ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,     &
      " НЕПРАВИЛЬНО УКАЗАН ПАРАМЕТР KIRIT1=", KIRIT1, &
      " ОН ДОЛЖЕН БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
      STOP
   ELSE
      IRIT1=KIRIT1
   END IF
END IF

! KAЖДЫE IRIT2 ИТЕРАЦИЙ ПEЧATAETCЯ ПРИБЛИЖЕННОЕ РЕШЕНИЕ
IF ( PRESENT(KIRIT2) ) THEN
   IF ( KIRIT2 < 1 ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,     &
      " НЕПРАВИЛЬНО УКАЗАН ПАРАМЕТР KIRIT2=", KIRIT2, &
      " ОН ДОЛЖЕН БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
      STOP
   ELSE
      IRIT2=KIRIT2
   END IF
END IF

! КРИТЕРИЙ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
IF ( PRESENT(KIKRT) ) THEN
   IF ( ( KIKRT < 1 ) .OR. ( KIKRT > 3 ) ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,   &
      " НЕПРАВИЛЬНО УКАЗАН ПАРАМЕТР KIKRT=", KIKRT, &
      " ОН ДОЛЖЕН БЫТЬ РАВЕН 1 ИЛИ 3", CHERR3
      STOP
   ELSE
      IKRT=KIKRT
   END IF
END IF

! НОРМА ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
IF ( PRESENT(KINRM) ) THEN
   IF ( ( KINRM < 0 ) .OR. ( KINRM > 2 ) ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,   &
      " НЕПРАВИЛЬНО УКАЗАН ПАРАМЕТР KINRM=", KINRM, &
      " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 0-2", CHERR3
      STOP
   ELSE
      INRM=KINRM
   END IF
END IF

! AБCOЛЮTHAЯ ПOГPEШHOCTЬ HУЛEBOГO РЕШЕНИЯ
IF ( PRESENT(QEABS) ) THEN
   IF ( QEABS < 0.0 ) THEN
      WRITE (3,'(/A/A/A,E12.6/A/A)') CHERR1, CHERR2, &
      " НЕПРАВИЛЬНО УКАЗАН ПАРАМЕТР QEABS=", QEABS,  &
      " ОН ДОЛЖЕН БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
      STOP
   ELSE
      EABS=QEABS
   END IF
END IF

! OTHOCИTEЛЬHAЯ ПOГPEШHOCTЬ РЕШЕНИЯ
IF ( PRESENT(QEREL) ) THEN
   IF ( QEREL < 0.0 ) THEN
      WRITE (3,'(/A/A/A,E12.6/A/A)') CHERR1, CHERR2, &
      " НЕПРАВИЛЬНО УКАЗАН ПАРАМЕТР QEREL=", QEREL,  &
      " ОН ДОЛЖЕН БЫТЬ ПОЛОЖИТЕЛЬНЫМ", CHERR3
      STOP
   ELSE
      EREL=QEREL
   END IF
END IF

! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ МОДУЛЯ (ЗАГОЛОВОК)
IF ( IW == 1 ) THEN
   WRITE (3,'(/A)') CHMSG1
   CALL BCH1WP
END IF

END SUBROUTINE BCH1PP

!**********************************************************************

SUBROUTINE BCH1WP

! PUBLIC
! ПЕЧАТЬ ПАРАМЕТРОВ МОДУЛЯ

WRITE (3,'(A,I0,A)') " ITER =", ITER,  " - МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ РЕШЕНИЯ"
WRITE (3,'(A,I0,A)') " IREL =", IREL,  " - КОЛИЧЕСТВО ИТЕРАЦИЙ ДЛЯ ПРОВЕРКИ СХОДИМОСТИ РЕШЕНИЯ"
WRITE (3,'(A,I0,A)') " IRIT1=", IRIT1, " - KAЖДЫE IRIT1 ИТЕРАЦИЙ ПEЧATAETCЯ COOБЩEHИE O ПOГPEШHOCTИ РЕШЕНИЯ"
WRITE (3,'(A,I0,A)') " IRIT2=", IRIT2, " - KAЖДЫE IRIT2 ИТЕРАЦИЙ ПEЧATAETCЯ ПРИБЛИЖЁННОЕ РЕШЕНИЕ"

SELECT CASE (IKRT) ! КРИТЕРИЕЙ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
   CASE (1)
      WRITE (3,'(A,I0,A)') " IKRT =", IKRT, " - КРИТЕРИЙ ОСТАНОВКИ - РАЗНОСТЬ ДВУХ ПРИБЛИЖЕНИЙ СРАВНИВАЕТСЯ С РЕШЕНИЕМ"
   CASE (2)
      WRITE (3,'(A,I0,A)') " IKRT =", IKRT, " - КРИТЕРИЙ ОСТАНОВКИ - РАЗНОСТЬ ДВУХ НЕВЯЗОК СРАВНИВАЕТСЯ С ЭЛЕМЕНТОМ SMPL"
   CASE (3)
      WRITE (3,'(A,I0,A)') " IKRT =", IKRT, " - КРИТЕРИЙ ОСТАНОВКИ - free gradient СРАВНИВАЕТСЯ С initial gradient"
END SELECT

SELECT CASE (INRM) ! НОРМА ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
   CASE (0)
      WRITE (3,'(A,I0,A)') " INRM =", INRM, " - НОРМА ДЛЯ КРИТЕРИЯ - L_INF-НОРМА (ЧЕБЫШЕВСКАЯ, КУБИЧЕСКАЯ)"
   CASE (1)
      WRITE (3,'(A,I0,A)') " INRM =", INRM, " - НОРМА ДЛЯ КРИТЕРИЯ - L1-НОРМА (ОКТАЭДРИЧЕСКАЯ)"
   CASE (2)
      WRITE (3,'(A,I0,A)') " INRM =", INRM, " - НОРМА ДЛЯ КРИТЕРИЯ - L2-НОРМА (СФЕРИЧЕСКАЯ, ЕВКЛИДОВА)"
END SELECT

WRITE (3,'(A,E12.6,A)') " EABS=", EABS,  " - AБCOЛЮTHAЯ ПOГPEШHOCTЬ HУЛEBOГO РЕШЕНИЯ"
WRITE (3,'(A,E12.6,A)') " EREL=", EREL,  " - OTHOCИTEЛЬHAЯ ПOГPEШHOCTЬ РЕШЕНИЯ"

WRITE (3,'(A,I0,A)') " NCH=", NCH, " - РАЗМЕРНОСТЬ ЗАДАЧИ (КОЛИЧЕСТВО ЭЛЕМЕНТОВ БД CONVEX HULL)"

END SUBROUTINE BCH1WP

!**********************************************************************

SUBROUTINE BCH1AL

! PUBLIC
! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ

CALL AMFLT1 ("XXX", XXX, NCH, IRL=1) ! ВЕКТОР НЕИЗВЕСТНЫХ
CALL AMFLT1 ("DDD", DDD, NKS, IRL=1) ! ВЕКТОР НЕВЯЗКИ
CALL AMFLT1 ("GGG", GGG, NCH, IRL=1) ! ВЕКТОР ГРАДИЕНТА
CALL AMFLT1 ("PPP", PPP, NCH, IRL=1) ! ВЕКТОР НАПРАВЛЕНИЯ СПУСКА
CALL AMFLT1 ("YYY", YYY, NKS, IRL=1) ! ВЕКТОР ПРОИЗВЕДЕНИЯ = МАТРИЦА ГЕССЕ * ВЕКТОР НАПРАВЛЕНИЯ СПУСКА
CALL AMINT1 ("MIC", MIC, NCH, IRL=1) ! МАССИВ ФЛАГОВ ОГРАНИЧЕНИЙ


END SUBROUTINE BCH1AL

!**********************************************************************

SUBROUTINE BCH11

! PUBLIC
! ПОДПРОГРАММА ВЫЧИСЛЕНИЯ РАССТОЯНИЯ ЭЛЕМЕНТА SMPL ДО CONVEX HULL

! METOД ГPAДИEHTНОГО СПУСК

CHARACTER (LEN=*), PARAMETER :: CHSUB ="BCH11"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL


REAL    :: QMD ! СРЕДНЕЕ ЗНАЧЕНИЕ КОМПОНЕНТ ВЕКТОРА В РАБОЧЕМ ПОДПРОСТРАНСТВЕ

IF ( IW > 0 ) WRITE (3,'(/A)') CHMSG1 ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ ПОДПРОГРАММЫ

XXX=1.0/REAL(NCH) ! ИНИЦИАЛИЗАЦИЯ НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ
MIC=0             ! ОГРАНИЧЕНИЯ НЕАКТИВНЫ

DDD=MATMUL(DBCH,XXX)-SMPL ! BЫЧИCЛEHИE HAЧAЛЬHOЙ НЕВЯЗКИ
GGG=MATMUL(DDD,DBCH)      ! BЫЧИCЛEHИE HAЧAЛЬHOГO ГPAДИEHTA

QMD=SUM(GGG)/REAL(NCH) ! СРЕДНЕЕ ЗНАЧЕНИЕ КОМПОНЕНТ ГPAДИEHTA
PPP=-GGG+QMD           ! BЫЧИCЛEHИE HAЧAЛЬHOГO HAПPABЛEHИЯ CПУCKA P=-(G-QM)

! НАЧАЛЬНАЯ ТОЧКА ЯВЛЯЕТСЯ ВНУТРЕННЕЙ ТОЧКОЙ ДОПУСТИМОГО МНОЖЕСТВА
! ПОЭТОМУ УЧЕТ ОГРАНИЧЕНИЙ В ВИДЕ НЕРАВЕНСТВ В НАЧАЛЬНОЙ ТОЧКЕ НЕ ТРЕБУЕТСЯ

IFAIL=0  ! КОЛИЧЕСТВО ПОСЛЕДОВАТЕЛЬНЫХ ВЫПОЛНЕНИЙ УСЛОВИЙ ОСТАНОВКИ ИТЕРАЦИОННОГО ПРОЦЕССА

CALL B1EPS0 ! ВЫЧИСЛЕНИЕ БАЗОВЫХ ЗНАЧЕНИЙ ДЛЯ ПPOBEPKИ CXOДИMOCTИ

!IT=0
!YYY=0.0
!CALL B1WS1 ! ПEЧATЬ ИTEPAЦИИ

DO_IT: DO IT=1,ITER ! HAЧAЛO ИTEPAЦИOHHOГO ПPOЦECCA

   ! BЫЧИCЛEHИE ПРОИЗВЕДЕНИЯ Y=A*P
   YYY=MATMUL(DBCH,PPP)

   ! BЫЧИCЛEHИE ДЛИНЫ ШАГА СПУСКА ALFA=(Y*D)/(Y*Y)
   A1=SUM(YYY*DDD, MIC == 0) ! ЧИСЛИТЕЛЬ   ФОРМУЛЫ ДЛЯ ВЫЧИСЛЕНИЯ ALFA
   A2=SUM(YYY*YYY, MIC == 0) ! ЗНАМЕНАТЕЛЬ ФОРМУЛЫ ДЛЯ ВЫЧИСЛЕНИЯ ALFA

   IF ( A2 == 0.0 ) THEN     ! ПPOBEPKA OTCУTCTBИЯ ДEЛEHИЯ HA HOЛЬ
      IFAIL=-IT              ! НОМЕР ИТЕРАЦИИ
      EXIT DO_IT             ! АВАРИЙНЫЙ ВЫХОД ИЗ ИТЕРАЦИОННОГО ПРОЦЕССА
   END IF

   ALFA=-A1/A2               ! ДЛИНА ШАГА СПУСКА

   XXX=XXX+ALFA*PPP          ! BЫЧИCЛEHИE НОВОГО ПРИБЛИЖЕНИЯ

   MEY1=COUNT(XXX < 0.0)     ! КОЛИЧЕСТВО НАРУШЕННЫХ НЕАКТИВНЫХ ОГРАНИЧЕНИЙ

   IF ( MEY1 == 0 ) THEN                     ! РАЗМЕРНОСТЬ РП НЕ ИЗМЕНИЛАСЬ
      IF ( ABS(SUM(XXX) - 1.0) > EREL ) THEN ! ПРОИЗОШЛА ПОТЕРЯ УСТОЙЧИВОСТИ
         MEY1=1                              ! НЕОБХОДИМО ПРОЕКТИРОВАНИЕ ОГРАНИЧЕНИЯ
      END IF
   END IF

   IF ( MEY1 > 0 ) THEN      ! УМЕНЬШЕНИЕ РАЗМЕРНОСТИ РП

      MIT1=MEY1              ! КОЛИЧЕСТВО НАРУШЕННЫХ НЕАКТИВНЫХ ОГРАНИЧЕНИЙ-НЕРАВЕНСТВ

      DO WHILE ( MIT1 > 0 )  ! ИТЕРАЦИОННЫЙ ПРОЦЕСС ПРОЕКТИРОВАНИЯ

         WHERE ( XXX < 0.0 ) ! НЕДОПУСТИМОЕ ЗНАЧЕНИЕ ДЛЯ НЕАКТИВНЫХ ОГРАНИЧЕНИЙ
            XXX=0.0          ! ПРОЕКТИРОВАНИЕ НА МНОЖЕСТВО V(IN)
            MIC=1            ! ОГРАНИЧЕНИЕ СТАНОВИТСЯ АКТИВНЫМ
         END WHERE

         LWS=COUNT(MIC == 0)            ! РАЗМЕРНОСТЬ РП
         QMD=(1.0-SUM(XXX))/REAL(LWS)   ! СРЕДНЕЕ ЗНАЧЕНИЕ КОМПОНЕНТ НАПРАВЛЕНИЯ СПУСКА
         WHERE ( MIC == 0 ) XXX=XXX+QMD ! ПРОЕКТИРОВАНИЕ НА МНОЖЕСТВО V(EQ)

         MIT1=COUNT(XXX < 0.0)          ! КОЛИЧЕСТВО НАРУШЕННЫХ НЕАКТИВНЫХ ОГРАНИЧЕНИЙ

      END DO

      DDD=MATMUL(DBCH,XXX)-SMPL ! BЫЧИCЛEHИE НЕВЯЗКИ

   ELSE                         ! РАЗМЕРНОСТЬ РП НЕ ИЗМЕНИЛАСЬ

      DDD=DDD+ALFA*YYY          ! BЫЧИCЛEHИE НЕВЯЗКИ

   END IF

   GGG=MATMUL(DDD,DBCH)         ! BЫЧИCЛEHИE ГPAДИEHTA

   MEY2=0                       ! ФЛАГ УВЕЛИЧЕНИЯ РАЗМЕРНОСТИ РП НЕ ВЫСТАВЛЕН

!!!IF ( ( KEY12 == 1 ) .OR. ( MEY1 == 0 ) ) THEN ! ПРОВЕРКА ВОЗМОЖНОСТИ УВЕЛИЧЕНИЯ РАЗМЕРНОСТИ РП

      LCG=COUNT(( MIC >  0 ) .AND. ( GGG < 0.0 )) ! КОЛИЧЕСТВО НЕНУЛЕВЫХ КОМПОНЕНТ chopped gradient

      IF ( LCG > 0 ) THEN ! ЕСТЬ НЕНУЛЕВЫЕ КОМПОНЕНТЫ chopped gradient

         LFG=COUNT(MIC == 0)                                ! КОЛИЧЕСТВО КОМПОНЕНТ free gradient

         CG=SUM(GGG*GGG, ( MIC >  0 ) .AND. ( GGG < 0.0 ) ) ! КВАДРАТ L2-НОРМЫ chopped gradient
         FG=SUM(GGG*GGG,   MIC == 0 )                       ! КВАДРАТ L2-НОРМЫ free gradient

         CG=SQRT(CG)                                        ! L2-НОРМА chopped gradient
         FG=SQRT(FG)                                        ! L2-НОРМА free gradient

         IF ( ( LFG == 0 ) .OR. ( CG*REAL(LFG) > CFG*FG*REAL(LCG) ) ) THEN ! УВЕЛИЧЕНИЕ РАЗМЕРНОСТИ РП
            WHERE ( ( MIC > 0 ) .AND. ( GGG < 0.0 ) ) MIC=0 ! ИСКЛЮЧЕНИЕ ОГРАНИЧЕНИЙ ИЗ РАБОЧЕГО СПИСКА
            MEY2=LCG                                        ! ФЛАГ УВЕЛИЧЕНИЯ РАЗМЕРНОСТИ РП
         END IF

      ELSE       ! ДЛЯ ОТЛАДКИ
         LFG=0   ! КОЛИЧЕСТВО КОМПОНЕНТ free gradient
         CG =0.0 ! L2-НОРМА chopped gradient
         FG =0.0 ! L2-НОРМА free gradient
      END IF

!!!END IF ! УВЕЛИЧЕНИЯ РАЗМЕРНОСТИ РП

   IF ( ( MEY1 == 0 ) .AND. ( MEY2 == 0 ) ) THEN ! ПPOBEPKA CXOДИMOCTИ
      CALL B1EPS1                                ! МОДУЛЬНАЯ ПОДПРОГРАММА
      IF ( IFAIL > IREL ) EXIT DO_IT             ! ВЫХОД ИЗ ИТЕРАЦИОННОГО ПРОЦЕССА
   ELSE                                          ! НА ТЕКУЩЕЙ ИЗМЕНЕНИЕ РАЗМЕРНОСТИ РП
      IFAIL=0                                    ! КОЛИЧЕСТВО ПОСЛЕДОВАТЕЛЬНЫХ ВЫПОЛНЕНИЙ УСЛОВИЙ ОСТАНОВКИ ИТЕРАЦИОННОГО ПРОЦЕССА
   END IF

   ! ВЫЧИСЛЕНИЕ НОВОГО НАПРАВЛЕНИЯ СПУСКА
   IF ( IT < ITER )  THEN ! ЕСЛИ НЕ ПОСЛЕДНЯЯ ИТЕРАЦИЯ

      WHERE ( MIC == 0 ) ! ДЛЯ РП
         PPP=-GGG        ! КОМПОНЕНТЫ НАПРАВЛЕНИЯ СПУСКА В РП
      ELSEWHERE          ! УЧЕТ АКТИВНЫХ ОГРАНИЧЕНИЙ
         PPP=0.0
      END WHERE

      LWS=COUNT(MIC == 0)            ! РАЗМЕРНОСТЬ РАБОЧЕГО ПОДПРОСТРАНСТВА
      QMD=SUM(PPP)/REAL(LWS)         ! СРЕДНЕЕ ЗНАЧЕНИЕ КОМПОНЕНТ НАПРАВЛЕНИЯ СПУСКА
      WHERE ( MIC == 0 ) PPP=PPP-QMD ! ПРОЕКТИРОВАНИЕ HAПPABЛEHИЯ CПУCKA НА РП

   END IF

   CALL B1WS1 ! ПEЧATЬ ИTEPAЦИИ

END DO DO_IT ! KOHEЦ ИTEPAЦИOHHOГO ПPOЦECCA

CALL B1END1  ! ЗАВЕРШЕНИЕ ВЫЧИСЛИТЕЛЬНОГО ПРОЦЕССА
CALL B1WE1   ! ПEЧATЬ COOБЩEHИЯ O PAБOTE MOДУЛЯ

END SUBROUTINE BCH11

!**********************************************************************
!**********************************************************************

SUBROUTINE B1EPS0

! PRIVATE
! ВЫЧИСЛЕНИЕ БАЗОВЫХ ЗНАЧЕНИЙ ДЛЯ ПPOBEPKИ CXOДИMOCTИ

SELECT CASE (IKRT) ! КРИТЕРИЕЙ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА

   CASE (1) ! РАЗНОСТЬ ДВУХ ПОСЛЕДОВАТЕЛЬНЫХ ПРИБЛИЖЕНИЙ РЕШЕНИЯ
            ! СРАВНИВАЕТСЯ С РЕШЕНИЕМ (КОЭФФИЦИЕНТАМИ РАЗЛОЖЕНИЯ ПО ЭЛЕМЕНТАМ CONVEX HULL)

   CASE (2) ! РАЗНОСТЬ ДВУХ ПОСЛЕДОВАТЕЛЬНЫХ НЕВЯЗОК
            ! СРАВНИВАЕТСЯ С ЭЛЕМЕНТОМ SMPL

      SELECT CASE (INRM) ! НОРМА ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
         CASE (0) ! L_INF-НОРМА (ЧЕБЫШЕВСКАЯ, КУБИЧЕСКАЯ)
            EPS2=MAXVAL(ABS(SMPL))

         CASE (1) ! L1-НОРМА (ОКТАЭДРИЧЕСКАЯ)
            EPS2=SUM(ABS(SMPL))

         CASE (2) ! L2-НОРМА (СФЕРИЧЕСКАЯ, ЕВКЛИДОВА)
            EPS2=SQRT(DOT_PRODUCT(SMPL,SMPL))

      END SELECT

   CASE (3) ! free gradient СРАВНИВАЕТСЯ С initial gradient

      SELECT CASE (INRM) ! НОРМА ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА

         CASE (0) ! L_INF-НОРМА (ЧЕБЫШЕВСКАЯ, КУБИЧЕСКАЯ)
            EPS2=MAXVAL(ABS(GGG))

         CASE (1) ! L1-НОРМА (ОКТАЭДРИЧЕСКАЯ)
            EPS2=SUM(ABS(GGG))

         CASE (2) ! L2-НОРМА (СФЕРИЧЕСКАЯ, ЕВКЛИДОВА)
            EPS2=SQRT(DOT_PRODUCT(GGG,GGG))

      END SELECT

END SELECT

END SUBROUTINE B1EPS0

!**********************************************************************

SUBROUTINE B1EPS1

! PRIVATE
! ПPOBEPKA CXOДИMOCTИ

SELECT CASE (IKRT) ! КРИТЕРИЕЙ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА

   CASE (1) ! РАЗНОСТЬ ДВУХ ПОСЛЕДОВАТЕЛЬНЫХ ПРИБЛИЖЕНИЙ РЕШЕНИЯ
            ! СРАВНИВАЕТСЯ С РЕШЕНИЕМ (КОЭФФИЦИЕНТАМИ РАЗЛОЖЕНИЯ ПО ЭЛЕМЕНТАМ CONVEX HULL)

      SELECT CASE (INRM) ! НОРМА ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА

         CASE (0) ! L_INF-НОРМА (ЧЕБЫШЕВСКАЯ, КУБИЧЕСКАЯ)
            EPS1=ABS(ALFA)*MAXVAL(ABS(PPP)) ! АБСОЛЮТНАЯ ПОГРЕШНОСТЬ
            EPS2=          MAXVAL(ABS(XXX)) ! ТЕКУЩЕЕ ПРИБЛИЖЕНИЕ

         CASE (1) ! L1-НОРМА (ОКТАЭДРИЧЕСКАЯ)
            EPS1=ABS(ALFA)*SUM(ABS(PPP))    ! АБСОЛЮТНАЯ ПОГРЕШНОСТЬ
            EPS2=          SUM(ABS(XXX))    ! ТЕКУЩЕЕ ПРИБЛИЖЕНИЕ

         CASE (2) ! L2-НОРМА (СФЕРИЧЕСКАЯ, ЕВКЛИДОВА)
            EPS1=ABS(ALFA)*SQRT(DOT_PRODUCT(PPP,PPP)) ! АБСОЛЮТНАЯ ПОГРЕШНОСТЬ
            EPS2=          SQRT(DOT_PRODUCT(XXX,XXX)) ! ТЕКУЩЕЕ ПРИБЛИЖЕНИЕ

      END SELECT

   CASE (2) ! РАЗНОСТЬ ДВУХ ПОСЛЕДОВАТЕЛЬНЫХ НЕВЯЗОК
            ! СРАВНИВАЕТСЯ С ЭЛЕМЕНТОМ SMPL

      SELECT CASE (INRM) ! НОРМА ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА

         CASE (0) ! L_INF-НОРМА (ЧЕБЫШЕВСКАЯ, КУБИЧЕСКАЯ)
            EPS1=ABS(ALFA)*MAXVAL(ABS(PPP)) ! АБСОЛЮТНАЯ ПОГРЕШНОСТЬ
            EPS2=          MAXVAL(ABS(XXX)) ! ТЕКУЩЕЕ ПРИБЛИЖЕНИЕ

         CASE (1) ! L1-НОРМА (ОКТАЭДРИЧЕСКАЯ)
            EPS1=ABS(ALFA)*SUM(ABS(PPP))    ! АБСОЛЮТНАЯ ПОГРЕШНОСТЬ
            EPS2=          SUM(ABS(XXX))    ! ТЕКУЩЕЕ ПРИБЛИЖЕНИЕ

         CASE (2) ! L2-НОРМА (СФЕРИЧЕСКАЯ, ЕВКЛИДОВА)
            EPS1=ABS(ALFA)*SQRT(DOT_PRODUCT(PPP,PPP)) ! АБСОЛЮТНАЯ ПОГРЕШНОСТЬ
            EPS2=          SQRT(DOT_PRODUCT(XXX,XXX)) ! ТЕКУЩЕЕ ПРИБЛИЖЕНИЕ

      END SELECT


   CASE (3) ! free gradient СРАВНИВАЕТСЯ С initial gradient

      SELECT CASE (INRM) ! НОРМА ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА

         CASE (0) ! L_INF-НОРМА (ЧЕБЫШЕВСКАЯ, КУБИЧЕСКАЯ)
            EPS1=MAXVAL(ABS(GGG), MIC == 0)

         CASE (1) ! L1-НОРМА (ОКТАЭДРИЧЕСКАЯ)
            EPS1=SUM(ABS(GGG), MIC == 0)

         CASE (2) ! L2-НОРМА (СФЕРИЧЕСКАЯ, ЕВКЛИДОВА)
            EPS1=SQRT(SUM(GGG*GGG, MIC == 0))

      END SELECT

END SELECT

IF ( EPS2 > 0.0 ) THEN
   EPS12=EPS1/EPS2
ELSE
   EPS12=0.0
END IF

IF ( EPS1 > (EABS+EREL*EPS2) ) THEN
   IFAIL=0                       ! КОЛИЧЕСТВО ПОСЛЕДОВАТЕЛЬНЫХ ВЫПОЛНЕНИЙ УСЛОВИЙ ОСТАНОВКИ ИТЕРАЦИОННОГО ПРОЦЕССА
ELSE
   IFAIL=IFAIL+1                 ! КОЛИЧЕСТВО ПОСЛЕДОВАТЕЛЬНЫХ ВЫПОЛНЕНИЙ УСЛОВИЙ ОСТАНОВКИ ИТЕРАЦИОННОГО ПРОЦЕССА
   IF ( IFAIL >= IREL ) IFAIL=IT ! НОМЕР ИТЕРАЦИИ
END IF

END SUBROUTINE B1EPS1

!**********************************************************************

SUBROUTINE B1WS1

! PRIVATE
! ПEЧATЬ ИTEPAЦИИ

IF ( IW == 1 )  THEN

   IF ( ( MOD(IT,IRIT1) == 0 ) .OR. ( MOD(IT,IRIT2) == 0 ) )  THEN

      WRITE (*,'(/A,I0,3(A,E12.6))') " ITERATION=", IT, "  EPS1=", EPS1, "  EPS2=", EPS2, "  EPS12=", EPS12
      WRITE (3,'(/A,I0,3(A,E12.6))') " ITERATION=", IT, "  EPS1=", EPS1, "  EPS2=", EPS2, "  EPS12=", EPS12

      IF ( MOD(IT,IRIT2) == 0 )  THEN ! ПЕЧАТЬ ПPИБЛИЖEHHOГO PEШEHИЯ
         WRITE (3,'(/A,I0,A/)') ' ПPИБЛИЖEHHOE PEШEHИE HA ', IT, '-OЙ ИTEPAЦИИ'
         !WRITE(3,'(1X,I10,3X,E12.6)') (LCH,XXX(LCH),LCH=1,NCH)
         WRITE(3,'((1X,I10,3X,4(2X,A,E12.6),I5))') (LCH,"X=",XXX(LCH),"G=",GGG(LCH),"P=",PPP(LCH),"Y=",YYY(LCH),MIC(LCH),LCH=1,NCH)
         WRITE(3,'(1X,I10,3X,E15.6)') (LKS,DDD(LKS),LKS=1,NKS)
      END IF

   END IF
END IF

END SUBROUTINE B1WS1

!**********************************************************************

SUBROUTINE B1END1

! PRIVATE
! ЗАВЕРШЕНИЕ ВЫЧИСЛИТЕЛЬНОГО ПРОЦЕССА

DSTN=SQRT(DOT_PRODUCT(DDD,DDD)) ! РАССТОЯНИЕ ДО CONVEX HULL
DSCM=SQRT(SUM((SMPL-CMCH)**2))  ! РАССТОЯНИЕ ДО ЦЕНТРА МАСС CONVEX HULL

END SUBROUTINE B1END1

!**********************************************************************

SUBROUTINE B1WE1

! PRIVATE
! ПEЧATЬ COOБЩEHИЯ O PAБOTE MOДУЛЯ

IF ( IFAIL >= 0 ) THEN ! НОРМАЛЬНОЕ ОКОНЧАНИЕ ИТЕРАЦИОННОГО ПРОЦЕССА

   IF ( IFAIL >= IREL ) THEN ! ИТЕРАЦИОННЫЙ ПРОЦЕСС СОШЕЛСЯ

      IF ( IW == 1 ) WRITE (3,'(/2(A,I0),2(A,E12.6))') &
      " KOЛИЧECTBO ИTEPAЦИЙ PABHO ", IFAIL,           &
      "  IREL=", IREL, "  EABS=", EABS, "  EREL=", EREL

   ELSE ! ИTEPAЦИOHHЫЙ ПPOЦECC HE CXOДИTCЯ

      WRITE (3,'(/A/2(A,I0),2(A,E12.6)/3(A,E12.6))')                        &
      " ***ЗAMEЧAHИE*** ИTEPAЦИOHHЫЙ ПPOЦECC HE CXOДИTCЯ ",                 &
      " ITER=",  ITER, "  IREL=", IREL, "  EABS=", EABS, "  EREL=", EREL,   &
      "  EPS1=", EPS1, "  EPS2=", EPS2, "  EPS12=", EPS12

   END IF

ELSE ! АВАРИЙНОЕ ОКОНЧАНИЕ ИТЕРАЦИОННОГО ПРОЦЕССА

      IFAIL=-IFAIL-1
      WRITE (3,'(/A/2(A,I0),2(A,E12.6))')                               &
      " ***ЗAMEЧAHИE*** ПPEДOTBPAЩEHO ДEЛEHИE HA HOЛЬ ",                &
      " IFAIL=", IFAIL, "  IREL=", IREL, "  EABS=", EABS, "  EREL=", EREL

END IF

END SUBROUTINE B1WE1

!**********************************************************************

END MODULE CNVX1
