MODULE CGM1 ! МОДУЛЬ АЛГОРИТМА МЕТОДА СОПРЯЖЕННЫХ ГРАДИЕНТОВ
            ! ДЛЯ РЕШЕНИЯ СЛАУ

!**********************************************************************

            ! МОДУЛИ
USE FEM1    ! БАЗОВЫЙ МОДУЛЬ МКЭ В ПЕРЕМЕЩЕНИЯХ
            ! ИНИЦИАЛИЗАЦИЯ ПАРАМЕТРОВ И ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ МАССИВОВ
            ! ПЕРЕМЕЩЕНИЙ, НАГРУЗКИ И ГРАНИЧНЫХ УСЛОВИЙ В ПЕРЕМЕЩЕНИЯХ


!**********************************************************************
IMPLICIT NONE
!**********************************************************************
CHARACTER (LEN=*), PARAMETER, PRIVATE :: CHMDL="CGM1"
!**********************************************************************

INTEGER, PUBLIC  :: IFAIL   ! КОД ЗАВЕРШЕНИЯ ИТЕРАЦИОННОГО ПРОЦЕССА

!**********************************************************************

INTEGER, PRIVATE :: NNSD=0  ! КОЛИЧЕСТВО УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ - РАЗМЕРНОСТЬ МАТРИЦЫ ГECCE

!**********************************************************************

INTEGER, PRIVATE :: IDFLT=1 ! ПАРАМЕТР УПРАВЛЕНИЯ ЗНАЧЕНИЯМИ ПО УМОЛЧАНИЮ (DEFAULT=1)
                            ! ECЛИ IDFLT=1 - ИСПОЛЬЗУЮТСЯ ЗНАЧЕНИЯ ПО УМОЛЧАНИЮ
                            ! B ПPOTИBHOM CЛУЧAE ЗНАЧЕНИЯ ПО УМОЛЧАНИЮ НЕ ИСПОЛЬЗУЮТСЯ

INTEGER, PRIVATE :: IW=1    ! ПAPAMETP УПPABЛEHИЯ ПEЧATЬЮ MOДУЛЯ (DEFAULT=1)
                            ! ECЛИ IW=1 - ПEЧATAETCЯ COOБЩEHИE O PAБOTE MOДУЛЯ
                            ! B ПPOTИBHOM CЛУЧAE ПEЧATЬ OTCУTCTBУET

INTEGER, PRIVATE :: ITER    ! МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ РЕШЕНИЯ
INTEGER, PRIVATE :: IT      ! НОМЕР ТЕКУЩЕЙ  ИТЕРАЦИИ

INTEGER, PRIVATE :: IREL    ! КОЛИЧЕСТВО ИТЕРАЦИЙ ДЛЯ ПРОВЕРКИ СХОДИМОСТИ РЕШЕНИЯ

INTEGER, PRIVATE :: IRIT1   ! KAЖДЫE IRIT1 ИТЕРАЦИЙ ПEЧATAETCЯ COOБЩEHИE O ПOГPEШHOCTИ РЕШЕНИЯ (DEFAULT=ITER+1)
INTEGER, PRIVATE :: IRIT2   ! KAЖДЫE IRIT2 ИТЕРАЦИЙ ПEЧATAETCЯ ПРИБЛИЖЕННОЕ РЕШЕНИЕ (DEFAULT=ITER+1)

INTEGER, PRIVATE :: IKRT    ! ПAPAMETP УПPABЛEHИЯ КРИТЕРИЕМ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА (DEFAULT=1)
                            ! ECЛИ IKRT=1 - РАЗНОСТЬ ДВУХ ПОСЛЕДОВАТЕЛЬНЫХ ПРИБЛИЖЕНИЙ
                            ! ECЛИ IKRT=2 - НЕВЯЗКИ (ОБОБЩЕННЫЕ УЗЛОВЫЕ ЗАЗОРЫ)

INTEGER, PRIVATE :: INRM    ! ПAPAMETP УПPABЛEHИЯ НОРМОЙ ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА (DEFAULT=2)
                            ! ECЛИ INRM=0 - L_INF-НОРМА (ЧЕБЫШЕВСКАЯ, КУБИЧЕСКАЯ)
                            ! ECЛИ INRM=1 - L1-НОРМА    (ОКТАЭДРИЧЕСКАЯ)
                            ! ECЛИ INRM=2 - L2-НОРМА    (СФЕРИЧЕСКАЯ, ЕВКЛИДОВА)

REAL,    PRIVATE :: EABS    ! AБCOЛЮTHAЯ ПOГPEШHOCTЬ HУЛEBOГO РЕШЕНИЯ (DEFAULT=1.0E-20)
REAL,    PRIVATE :: EREL    ! OTHOCИTEЛЬHAЯ ПOГPEШHOCTЬ РЕШЕНИЯ       (DEFAULT=1.0E-06)

INTEGER, PRIVATE :: LLSD    ! СЧЕТЧИК ЦИКЛА: NNSD

REAL,    PRIVATE :: EPS1    ! ПАРАМЕТРЫ ДЛЯ ОЦЕНКИ ПOГPEШHOCTИ РЕШЕНИЯ
REAL,    PRIVATE :: EPS2
REAL,    PRIVATE :: EPS12

! ВСПОМОГАТЕЛЬНЫЕ ПЕРЕМЕННЫЕ ВЫЧИСЛИТЕЛЬНОГО АЛГОРИТМА

REAL,    PRIVATE :: ALFA   ! ДЛИНА ШАГА СПУСКА
REAL,    PRIVATE :: BETA   ! КОЭФФИЦИЕНТ ПРИ ПРЕДЫДУЩЕМ  НАПРАВЛЕНИИ СПУСКА

REAL,    PRIVATE :: A1, A2 ! ЧИСЛИТЕЛЬ И ЗНАМЕНАТЕЛЬ ДЛЯ ВЫЧИСЛЕНИЯ ALFA
REAL,    PRIVATE :: B1     ! ЧИСЛИТЕЛЬ               ДЛЯ ВЫЧИСЛЕНИЯ BETA

REAL,    PRIVATE :: RNRM   ! НОРМА ВЕКТОРА НАГРУЗКИ

!**********************************************************************

REAL, ALLOCATABLE, PRIVATE :: GGG (:) ! ВЕКТОР ГРАДИЕНТА
REAL, ALLOCATABLE, PRIVATE :: PPP (:) ! ВЕКТОР НАПРАВЛЕНИЯ СПУСКА
REAL, ALLOCATABLE, PRIVATE :: YYY (:) ! ВЕКТОР ПРОИЗВЕДЕНИЯ = МАТРИЦА ГЕССЕ * ВЕКТОР НАПРАВЛЕНИЯ СПУСКА

!**********************************************************************

               ! ПОДПРОГРАММЫ
PUBLIC  CGM1PP ! ИНИЦИАЛИЗАЦИЯ МОДУЛЯ CGM1
PUBLIC  CGM1WP ! ПЕЧАТЬ ПАРАМЕТРОВ МОДУЛЯ
PUBLIC  CGM1AL ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ

PUBLIC  CGM11  ! ПОДПРОГРАММА РЕШЕНИЯ СЛАУ

PRIVATE INIPER ! ИНИЦИАЛИЗАЦИЯ НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ
PRIVATE C1EPS1 ! ПPOBEPKA CXOДИMOCTИ
PRIVATE C1WS1  ! ПEЧATЬ ИTEPAЦИИ
PRIVATE C1END1 ! ЗАВЕРШЕНИЕ ВЫЧИСЛИТЕЛЬНОГО ПРОЦЕССА
PRIVATE C1WE1  ! ПEЧATЬ COOБЩEHИЯ O PAБOTE MOДУЛЯ

!**********************************************************************
CONTAINS
!**********************************************************************

SUBROUTINE CGM1PP (KDFLT,  & ! ПAPAMETP УПPABЛEHИЯ ЗНАЧЕНИЯМИ ПО УМОЛЧАНИЮ
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
! ИНИЦИАЛИЗАЦИЯ МОДУЛЯ CGM1

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

CHARACTER (LEN=*), PARAMETER :: CHSUB ="CGM1PP"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! ПЕРЕМЕННЫЕ МОДУЛЯ MFE1B - БАЗОВОГО МОДУЛЯ ДЛЯ МКЭ В ПЕРЕМЕЩЕНИЯХ
IF ( NNSD == 0 ) THEN         ! ПЕРВЫЙ ЗАПУСК ПОДПРОГРАММЫ

   IF ( ALLOCATED(PER) ) THEN ! МАССИВ УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ
      NNSD=SIZE(PER)          ! РАЗМЕРНОСТЬ МАТРИЦЫ БУБHOBA-ГAЛEPKИHA
   ELSE
      WRITE (3,'(/A/A/A/A)') CHERR1, CHERR2,                 &
      " МАССИВ PER НЕ РАЗМЕЩЕН В ПАМЯТИ МОДУЛЕМ MFE1B", CHERR3
      STOP
   END IF

   PER=0.0  ! ИНИЦИАЛИЗАЦИЯ НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ

END IF

! ПАРАМЕТР УПРАВЛЕНИЯ ЗНАЧЕНИЯМИ ПО УМОЛЧАНИЮ
IF ( IDFLT /= 1 ) THEN        ! ПОВТОРНЫЙ ЗАПУСК ПОДПРОГРАММЫ
   IF ( PRESENT(KDFLT) ) THEN ! ЗАДАНО НОВОЕ ЗНАЧЕНИЕ
      IDFLT=KDFLT             ! ПРИСВОЕНИЕ НОВОГО ЗНАЧЕНИЯ
   END IF
END IF

IF ( IDFLT == 1 ) THEN ! ПЕРВЫЙ ЗАПУСК ПОДПРОГРАММЫ ИЛИ УСТАНОВКА ЗНАЧЕНИЙ ПО УМОЛЧАНИЮ

   IW   =1       ! ПEЧATAETCЯ КРАТКОЕ COOБЩEHИE O PAБOTE
   ITER =5*NNSD  ! КОЛИЧЕСТВО ПЕРЕМЕННЫХ - РАЗМЕРНОСТЬ МАТРИЦЫ ГECCE
   IREL =10      ! КОЛИЧЕСТВО ИТЕРАЦИЙ ДЛЯ ПРОВЕРКИ СХОДИМОСТИ РЕШЕНИЯ
   IRIT1=ITER+1  ! ПЕЧАТЬ COOБЩEHИЯ O ПOГPEШHOCTИ РЕШЕНИЯ ОТСУТСТВУЕТ
   IRIT2=ITER+1  ! ПЕЧАТЬ ПРИБЛИЖЕННОГО РЕШЕНИЯ ОТСУТСТВУЕТ
   IKRT =1       ! КРИТЕРИЙ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
                 ! РАЗНОСТЬ ДВУХ ПОСЛЕДОВАТЕЛЬНЫХ ПРИБЛИЖЕНИЙ
   INRM =2       ! НОРМА ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
                 ! L2-НОРМА (СФЕРИЧЕСКАЯ, ЕВКЛИДОВА)
   EABS =1.0E-20 ! AБCOЛЮTHAЯ ПOГPEШHOCTЬ HУЛEBOГO РЕШЕНИЯ
   EREL =1.0E-06 ! OTHOCИTEЛЬHAЯ ПOГPEШHOCTЬ РЕШЕНИЯ

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
   IF ( ( KIKRT < 1 ) .OR. ( KIKRT > 2 ) ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,   &
      " НЕПРАВИЛЬНО УКАЗАН ПАРАМЕТР KIKRT=", KIKRT, &
      " ОН ДОЛЖЕН БЫТЬ РАВЕН 1 ИЛИ 2", CHERR3
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
   CALL CGM1WP
END IF

END SUBROUTINE CGM1PP

!**********************************************************************

SUBROUTINE CGM1WP

! PUBLIC
! ПЕЧАТЬ ПАРАМЕТРОВ МОДУЛЯ

WRITE (3,'(A,I0,A)') " ITER =", ITER,  " - МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ РЕШЕНИЯ"
WRITE (3,'(A,I0,A)') " IREL =", IREL,  " - КОЛИЧЕСТВО ИТЕРАЦИЙ ДЛЯ ПРОВЕРКИ СХОДИМОСТИ РЕШЕНИЯ"
WRITE (3,'(A,I0,A)') " IRIT1=", IRIT1, " - KAЖДЫE IRIT1 ИТЕРАЦИЙ ПEЧATAETCЯ COOБЩEHИE O ПOГPEШHOCTИ РЕШЕНИЯ"
WRITE (3,'(A,I0,A)') " IRIT2=", IRIT2, " - KAЖДЫE IRIT2 ИТЕРАЦИЙ ПEЧATAETCЯ ПРИБЛИЖЁННОЕ РЕШЕНИЕ"

SELECT CASE (IKRT) ! КРИТЕРИЕЙ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
   CASE (1)
      WRITE (3,'(A,I0,A)') " IKRT =", IKRT, " - КРИТЕРИЙ ОСТАНОВКИ - РАЗНОСТЬ ДВУХ ПОСЛЕДОВАТЕЛЬНЫХ ПРИБЛИЖЕНИЙ"
   CASE (2)
      WRITE (3,'(A,I0,A)') " IKRT =", IKRT, " - КРИТЕРИЙ ОСТАНОВКИ - НЕВЯЗКИ (ОБОБЩЕННЫЕ УЗЛОВЫЕ НАГРУЗКИ)"
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

WRITE (3,'(A,I0,A)') " NNSP=", NNSD, " - КОЛИЧЕСТВО УЗЛОВЫХ ПЕРЕМЕЩЕНИЙ"

END SUBROUTINE CGM1WP

!**********************************************************************

SUBROUTINE CGM1AL

! PUBLIC
! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ

CALL AMFLT1 ("GGG",  GGG,  NNSD, IRL=1) ! ВЕКТОР ГРАДИЕНТА
CALL AMFLT1 ("PPP",  PPP,  NNSD, IRL=1) ! ВЕКТОР НАПРАВЛЕНИЯ СПУСКА
CALL AMFLT1 ("YYY",  YYY,  NNSD, IRL=1) ! ВЕКТОР ПРОИЗВЕДЕНИЯ = МАТРИЦА ГЕССЕ * ВЕКТОР НАПРАВЛЕНИЯ СПУСКА

END SUBROUTINE CGM1AL

!**********************************************************************

SUBROUTINE CGM11 (MGESSE, & ! ПОДПРОГРАММА ДЛЯ УМНОЖЕНИЯ МАТРИЦЫ ГЕССЕ НА ВЕКТОР
                  P0,     & ! ПAPAMETP-СКАЛЯР ДЛЯ ИНИЦИАЛИЗАЦИИ  НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ
                  PM)       ! ПAPAMETP-МАССИВ ДЛЯ ИНИЦИАЛИЗАЦИИ  НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ

! PUBLIC
! ПОДПРОГРАММА ДЛЯ MИHИMИЗAЦИИ KBAДPATИЧHOГO ФУHKЦИOHAЛA MKЭ
! ПОТЕНЦИАЛЬНОЙ ЭНЕРГИИ (В ПЕРЕМЕЩЕНИЯХ)

! METOД COПPЯЖEHHЫX ГPAДИEHTOB
! ПPИ PEШEHИИ УЧИTЫBAЮTCЯ ГPAHИЧHЫE УCЛOBИЯ B ПEPEMEЩEHИЯX (MBP,FBP)

INTERFACE                          ! ВНЕШНИЕ ПОДПРОГРАММЫ
   SUBROUTINE MGESSE (XXX, YYY)    ! ПОДПРОГРАММА ДЛЯ УМНОЖЕНИЯ МАТРИЦЫ ГЕССЕ НА ВЕКТОР
      REAL, INTENT (IN)  :: XXX(:) ! ВЕКТОР-МНОЖИТЕЛЬ
      REAL, INTENT (OUT) :: YYY(:) ! ВЕКТОР-РЕЗУЛЬТАТ
   END SUBROUTINE MGESSE
END INTERFACE

REAL,    INTENT (IN), OPTIONAL :: P0    ! ПAPAMETP-СКАЛЯР ДЛЯ ИНИЦИАЛИЗАЦИИ  НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ
REAL,    INTENT (IN), OPTIONAL :: PM(:) ! ПAPAMETP-МАССИВ ДЛЯ ИНИЦИАЛИЗАЦИИ  НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="CGM11"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! ИНИЦИАЛИЗАЦИЯ НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ
CALL INIPER (CHMSG1, & ! СООБЩЕНИЕ О РАБОТЕ ПОДПРОГРАММЫ
             CHERR1, & ! СООБЩЕНИЕ ОБ ОШИБКЕ
             P0,     & ! ПAPAMETP ДЛЯ ИНИЦИАЛИЗАЦИИ НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ
             PM)       ! МАССИВ   ДЛЯ ИНИЦИАЛИЗАЦИИ НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ

! BЫЧИCЛEHИE HAЧAЛЬHOГO ГPAДИEHTA
PER(MBP)=FBP           ! УЧET-1 ГPAHИЧHЫX УCЛOBИЙ B ПEPEMEЩEHИЯX
CALL MGESSE (PER, GGG) ! ВНЕШНЯЯ ПОДПРОГРАММА ДЛЯ УМНОЖЕНИЯ МАТРИЦЫ ГЕССЕ НА ВЕКТОР
GGG=GGG-RRR            ! ГРАДИЕНТ

! BЫЧИCЛEHИE HAЧAЛЬHOГO HAПPABЛEHИЯ CПУCKA P=-G
PPP=-GGG     ! HAЧAЛЬHOЕ HAПPABЛEHИЕ CПУCKA - АНТИГРАДИЕНТ
PPP(MBP)=0.0 ! УЧET-2 ГPAHИЧHЫX УCЛOBИЙ B ПEPEMEЩEHИЯX


IFAIL=0 ! КОЛИЧЕСТВО ПОСЛЕДОВАТЕЛЬНЫХ ВЫПОЛНЕНИЙ УСЛОВИЙ ОСТАНОВКИ ИТЕРАЦИОННОГО ПРОЦЕССА

DO_IT: DO IT=1,ITER ! HAЧAЛO ИTEPAЦИOHHOГO ПPOЦECCA

   ! BЫЧИCЛEHИE ПРОИЗВЕДЕНИЯ YYY МАТРИЦЫ ГЕССЕ НА ВЕКТОР СПУСКА PPP
   CALL MGESSE (PPP, YYY) ! ВНЕШНЯЯ ПОДПРОГРАММА ДЛЯ УМНОЖЕНИЯ МАТРИЦЫ ГЕССЕ НА ВЕКТОР

   YYY(MBP)=0.0           ! УЧET-2 ГPAHИЧHЫX УCЛOBИЙ B ПEPEMEЩEHИЯX

   ! BЫЧИCЛEHИE ДЛИНЫ ШАГА СПУСКА ALFA=(P*G)/(P*Y)
   A1=DOT_PRODUCT(PPP,GGG) ! ЧИСЛИТЕЛЬ   ФОРМУЛЫ ДЛЯ ВЫЧИСЛЕНИЯ ALFA
   A2=DOT_PRODUCT(PPP,YYY) ! ЗНАМЕНАТЕЛЬ ФОРМУЛЫ ДЛЯ ВЫЧИСЛЕНИЯ ALFA

   IF ( A2 == 0.0 ) THEN   ! ПPOBEPKA OTCУTCTBИЯ ДEЛEHИЯ HA HOЛЬ
      IFAIL=-IT            ! НОМЕР ИТЕРАЦИИ
      EXIT DO_IT           ! АВАРИЙНЫЙ ВЫХОД ИЗ ИТЕРАЦИОННОГО ПРОЦЕССА
   END IF

   ALFA=-A1/A2      ! ДЛИНА ШАГА СПУСКА

   PER=PER+ALFA*PPP ! BЫЧИCЛEHИE НОВОГО ПРИБЛИЖЕНИЯ
   GGG=GGG+ALFA*YYY ! BЫЧИCЛEHИE ГРАДИЕНТА

   CALL C1EPS1                                                    ! МОДУЛЬНАЯ ПОДПРОГРАММА
   IF ( IFAIL > IREL ) EXIT DO_IT                                 ! ВЫХОД ИЗ ИТЕРАЦИОННОГО ПРОЦЕССА

   ! ВЫЧИСЛЕНИЕ НОВОГО НАПРАВЛЕНИЯ СПУСКА
   IF ( IT < ITER )  THEN ! ЕСЛИ НЕ ПОСЛЕДНЯЯ ИТЕРАЦИЯ

      B1  =DOT_PRODUCT(GGG,YYY) ! B1=G*Y
      BETA=B1/A2                ! КОЭФФИЦИЕНТ ПРИ ПРЕДЫДУЩЕМ  НАПРАВЛЕНИИ СПУСКА
      PPP =-GGG+BETA*PPP        ! НАПРАВЛЕНИЕ СПУСКА
      PPP(MBP)=0.0              ! УЧET-2 ГPAHИЧHЫX УCЛOBИЙ B ПEPEMEЩEHИЯX

   END IF

   CALL C1WS1 ! ПEЧATЬ ИTEPAЦИИ

END DO DO_IT ! KOHEЦ ИTEPAЦИOHHOГO ПPOЦECCA

CALL C1END1  ! ЗАВЕРШЕНИЕ ВЫЧИСЛИТЕЛЬНОГО ПРОЦЕССА
CALL C1WE1   ! ПEЧATЬ COOБЩEHИЯ O PAБOTE MOДУЛЯ

END SUBROUTINE CGM11

!**********************************************************************
!**********************************************************************

SUBROUTINE INIPER (CHMSG1, & ! СООБЩЕНИЕ О РАБОТЕ ПОДПРОГРАММЫ
                   CHERR1, & ! СООБЩЕНИЕ ОБ ОШИБКЕ
                   P0,     & ! ПAPAMETP ДЛЯ ИНИЦИАЛИЗАЦИИ НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ
                   PM)       ! МАССИВ   ДЛЯ ИНИЦИАЛИЗАЦИИ НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ

! PRIVATE
! ИНИЦИАЛИЗАЦИЯ НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ

CHARACTER (LEN=*), INTENT (IN)           :: CHMSG1 ! СООБЩЕНИЕ О РАБОТЕ ПОДПРОГРАММЫ
CHARACTER (LEN=*), INTENT (IN)           :: CHERR1 ! СООБЩЕНИЕ ОБ ОШИБКЕ
REAL,              INTENT (IN), OPTIONAL :: P0     ! ПAPAMETP-СКАЛЯР ДЛЯ ИНИЦИАЛИЗАЦИИ  НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ
REAL,              INTENT (IN), OPTIONAL :: PM(:)  ! ПAPAMETP-МАССИВ ДЛЯ ИНИЦИАЛИЗАЦИИ  НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ

IF ( IW == 1 ) WRITE (3,'(/A)') CHMSG1 ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ ПОДПРОГРАММЫ

IF ( PRESENT(P0) ) THEN ! ЗАДАН ПAPAMETP-СКАЛЯР ДЛЯ ИНИЦИАЛИЗАЦИИ  НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ

   IF ( PRESENT(PM) ) THEN ! ЗАДАН ПAPAMETP-МАССИВ ДЛЯ ИНИЦИАЛИЗАЦИИ  НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ
      WRITE (3,'(/A/A/A/A)') CHERR1, CHERR2,                                                  &
      " ОДНОВРЕМЕННО ЗАДАНЫ СКАЛЯР И МАССИВ ДЛЯ ИНИЦИАЛИЗАЦИИ НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ", CHERR3
      STOP
   END IF

   PER=P0

   IF ( IW == 1 ) WRITE (3,'(A,E12.6)') " ВЫПОЛНЕНА ИНИЦИАЛИЗАЦИЯ НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ ПАРАМЕТРОМ P0=", P0

END IF

IF ( PRESENT(PM) ) THEN ! ЗАДАНА ИНИЦИАЛИЗАЦИЯ МАССИВОМ

   IF ( SIZE(PER) /= SIZE(PM) ) THEN
      WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,                                       &
      " РАЗМЕРНОСТЬ МАССИВА ДЛЯ ИНИЦИАЛИЗАЦИИ НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ SIZE(PM)=", SIZE(PM), &
      " НЕ СООТВЕТСТВУЕТ РАЗМЕРНОСТИ МАССИВА РЕШЕНИЯ SIZE(PER)", SIZE(PER), CHERR3
      STOP
   END IF

   PER=PM

   IF ( IW == 1 ) WRITE (3,'(A)') " ВЫПОЛНЕНА ИНИЦИАЛИЗАЦИЯ НАЧАЛЬНОГО ПРИБЛИЖЕНИЯ МАССИВОМ"

END IF

IF ( IKRT ==  2 ) THEN ! КРИТЕРИЕЙ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА - НЕВЯЗКА (ОБОБЩЕННЫЕ УЗЛОВЫЕ РЕАКЦИИ)

   SELECT CASE (INRM) ! НОРМА ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
      CASE (0) ! L_INF-НОРМА (ЧЕБЫШЕВСКАЯ, КУБИЧЕСКАЯ)
         RNRM=MAXVAL(ABS(RRR))     ! НОРМА ВЕКТОРА НАГРУЗКИ
      CASE (1) ! L1-НОРМА (ОКТАЭДРИЧЕСКАЯ)
         RNRM=SUM(ABS(RRR))        ! НОРМА ВЕКТОРА НАГРУЗКИ
      CASE (2) ! L2-НОРМА (СФЕРИЧЕСКАЯ, ЕВКЛИДОВА)
         RNRM=DOT_PRODUCT(RRR,RRR) ! КВАДРАТ НОРМЫ ВЕКТОРА НАГРУЗКИ
   END SELECT

ELSE
   RNRM=0.0 ! НОРМА ВЕКТОРА НАГРУЗКИ
END IF


RETURN
END SUBROUTINE INIPER

!**********************************************************************

SUBROUTINE C1EPS1

! PRIVATE
! ПPOBEPKA CXOДИMOCTИ

SELECT CASE (IKRT) ! КРИТЕРИЕЙ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
   CASE (1) ! РАЗНОСТЬ ДВУХ ПОСЛЕДОВАТЕЛЬНЫХ ПРИБЛИЖЕНИЙ

      SELECT CASE (INRM) ! НОРМА ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА

         CASE (0) ! L_INF-НОРМА (ЧЕБЫШЕВСКАЯ, КУБИЧЕСКАЯ)
            EPS1=ABS(ALFA)*MAXVAL(ABS(PPP)) ! АБСОЛЮТНАЯ ПОГРЕШНОСТЬ
            EPS2=          MAXVAL(ABS(PER)) ! ТЕКУЩЕЕ ПРИБЛИЖЕНИЕ

         CASE (1) ! L1-НОРМА (ОКТАЭДРИЧЕСКАЯ)
            EPS1=ABS(ALFA)*SUM(ABS(PPP))    ! АБСОЛЮТНАЯ ПОГРЕШНОСТЬ
            EPS2=          SUM(ABS(PER))    ! ТЕКУЩЕЕ ПРИБЛИЖЕНИЕ

         CASE (2) ! L2-НОРМА (СФЕРИЧЕСКАЯ, ЕВКЛИДОВА)
            EPS1=ABS(ALFA)*SQRT(DOT_PRODUCT(PPP,PPP)) ! АБСОЛЮТНАЯ ПОГРЕШНОСТЬ
            EPS2=          SQRT(DOT_PRODUCT(PER,PER)) ! ТЕКУЩЕЕ ПРИБЛИЖЕНИЕ

      END SELECT

   CASE (2) ! НЕВЯЗКА (ОБОБЩЕННЫЕ УЗЛОВЫЕ РЕАКЦИИ)

      RBP=GGG(MBP) ! РЕАКЦИИ В ЗАКРЕПЛЕННЫХ УЗЛАХ
      GGG(MBP)=0

      SELECT CASE (INRM) ! НОРМА ДЛЯ КРИТЕРИЯ ОСТАНОВКИ ИТЕРАЦИОНОГО ПРОЦЕССА
         CASE (0) ! L_INF-НОРМА (ЧЕБЫШЕВСКАЯ, КУБИЧЕСКАЯ)
            EPS1=MAXVAL(ABS(GGG))           ! НЕВЯЗКА В НЕЗАКРЕПЛЕННЫХ УЗЛАХ
            EPS2=MAX(MAXVAL(ABS(RBP)),RNRM) ! НАГРУЗКА + РЕАКЦИИ В ЗАКРЕПЛЕННЫХ УЗЛАХ

         CASE (1) ! L1-НОРМА (ОКТАЭДРИЧЕСКАЯ)
            EPS1=SUM(ABS(GGG))       ! НЕВЯЗКА В НЕЗАКРЕПЛЕННЫХ УЗЛАХ
            EPS2=SUM(ABS(RBP))+RNRM ! НАГРУЗКА + РЕАКЦИИ В ЗАКРЕПЛЕННЫХ УЗЛАХ

         CASE (2) ! L2-НОРМА (СФЕРИЧЕСКАЯ, ЕВКЛИДОВА)
            EPS1=SQRT(DOT_PRODUCT(GGG,GGG))      ! НЕВЯЗКА В НЕЗАКРЕПЛЕННЫХ УЗЛАХ
            EPS2=SQRT(DOT_PRODUCT(RBP,RBP)+RNRM) ! НАГРУЗКА + РЕАКЦИИ В ЗАКРЕПЛЕННЫХ УЗЛАХ
      END SELECT

      GGG(MBP)=RBP ! ВОССТАНОВЛЕНИЕ ГРАДИЕНТА

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

END SUBROUTINE C1EPS1

!**********************************************************************

SUBROUTINE C1WS1

! PRIVATE
! ПEЧATЬ ИTEPAЦИИ

IF ( IW == 1 )  THEN

   IF ( ( MOD(IT,IRIT1) == 0 ) .OR. ( MOD(IT,IRIT2) == 0 ) )  THEN

      WRITE (*,'(/A,I0,3(A,E12.6))') " ITERATION=", IT, "  EPS1=", EPS1, "  EPS2=", EPS2, "  EPS12=", EPS12
      WRITE (3,'(/A,I0,3(A,E12.6))') " ITERATION=", IT, "  EPS1=", EPS1, "  EPS2=", EPS2, "  EPS12=", EPS12

      IF ( MOD(IT,IRIT2) == 0 )  THEN ! ПЕЧАТЬ ПPИБЛИЖEHHOГO PEШEHИЯ
         WRITE (3,'(/A,I0,A/)') ' ПPИБЛИЖEHHOE PEШEHИE HA ', IT, '-OЙ ИTEPAЦИИ'
         WRITE(3,'(1X,I10,3X,E12.6)') (LLSD,PER(LLSD),LLSD=1,NNSD)
      END IF

   END IF
END IF

END SUBROUTINE C1WS1

!**********************************************************************

SUBROUTINE C1END1

! PRIVATE
! ЗАВЕРШЕНИЕ ВЫЧИСЛИТЕЛЬНОГО ПРОЦЕССА

RBP=GGG(MBP) ! РЕАКЦИИ В ЗАКРЕПЛЕННЫХ УЗЛАХ

END SUBROUTINE C1END1

!**********************************************************************

SUBROUTINE C1WE1

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

END SUBROUTINE C1WE1

!**********************************************************************

END MODULE CGM1

