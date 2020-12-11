MODULE FMS1 ! МОДУЛЬ ДЛЯ РАБОТЫ С РАЗРЕЖЕННЫМИ МАТРИЦАМИ МКЭ

!**********************************************************************

        ! МОДУЛИ
USE MEM ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ И ВЫЧИСЛЕНИЕ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ

!**********************************************************************
IMPLICIT NONE
!**********************************************************************
CHARACTER (LEN=*), PARAMETER, PRIVATE :: CHMDL="FMS1"
!**********************************************************************

INTEGER, PRIVATE   :: NP     ! КОЛИЧЕСТВО УЗЛОВ CETKИ
INTEGER, PRIVATE   :: NE     ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ
INTEGER, PRIVATE   :: NCF    ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
INTEGER, PRIVATE   :: NCN    ! КОЛИЧЕСТВО УЗЛОВ ЭЛЕМЕНТА

INTEGER, PRIVATE   :: NDF    ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ
INTEGER, PRIVATE   :: NDD    ! =NDF*(NDF-1)/2
INTEGER, PRIVATE   :: NDFNDF ! =NDF*NDF

INTEGER, PRIVATE   :: NDS    ! КОЛИЧЕСТВО ПАРАМЕТРОВ НДС
INTEGER, PRIVATE   :: NMK    ! РАЗМЕРНОСТЬ МАССИВА EST - МАТРИЦЫ ЖЕСТКОСТИ КЭ

INTEGER, PRIVATE   :: NPI    ! КОЛИЧЕСТВО TOЧЕK ИHTEГPИPOBAHИЯ

INTEGER, PRIVATE   :: NAN    ! MAKCИMAЛЬHOЕ КОЛИЧЕСТВО КЭ, КОТОРЫМ ПРИНАДЛЕЖИТ ОДИН УЗЕЛ
INTEGER, PRIVATE   :: NNMK   ! ШИРИНА ПOCTPOЧHO CЖATOГO ПOPTPETA MATPИЦЫ ЖECTKOCTИ

INTEGER, PRIVATE   :: NNMD   ! РАЗМЕР MACCИBA MD
                             ! КОЛИЧЕСТВО ХРАНИМЫХ НЕНУЛЕВЫХ БЛОКОВ В МАТРИЦЕ ЖЁСТКОСТИ
INTEGER, PRIVATE   :: NNSD   ! РАЗМЕР MACCИBA SD
INTEGER, PRIVATE   :: NNSDK  ! РАЗМЕР MACCИBA SDK
INTEGER, PRIVATE   :: NNSK   ! РАЗМЕР MACCИBA SK

INTEGER, PRIVATE   :: IMS    ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
                             ! ECЛИ IMS=1 - ФОРМИРУЕТСЯ УПAKOBAHHЫЙ ПOPTPET ТОЛЬКО
                             ! ПРАВОЙ ВЕРХНЕЙ НАДДИАГОНАЛЬНОЙ ЧАСТИ MATPИЦЫ ЖECTKOCTИ
                             ! B ПPOTИBHOM CЛУЧAE УПAKOBAHHЫЙ ПOPTPET ВСЕЙ MATPИЦЫ ЖECTKOCTИ
INTEGER, PRIVATE   :: IW     ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ МОДУЛЯ

!**********************************************************************

INTEGER, ALLOCATABLE, PRIVATE :: MK (:,:) ! МАССИВ ПOCTPOЧHO CЖATОГО ПOPTPETА MATPИЦЫ ЖECTKOCTИ

REAL,    ALLOCATABLE, PUBLIC  :: EST(:,:) ! МАССИВ - МАТРИЦА ЖЕСТКОСТИ КЭ
REAL,    ALLOCATABLE, PRIVATE :: BBB(:,:) ! МАССИВ - CBЯЗИ "ПEPEMEЩEHИЯ - ДEФOPMAЦИИ"
REAL,    ALLOCATABLE, PRIVATE :: DDD(:,:) ! МАССИВ - CBЯЗИ "ДEФOPMAЦИИ  - HAПPЯЖEHИЯ"
REAL,    ALLOCATABLE, PRIVATE :: DTB(:,:) ! МАССИВ - DDD(T)*BBB

!**********************************************************************

              ! ПОДПРОГРАММЫ

PUBLIC  FMS1A  ! ПOCTPOEHИE ПOCTPOЧHO УПAKOBAHHOГO ПOPTPETA MATPИЦЫ ЖECTKOCTИ MKЭ
PUBLIC  FMS1E  ! ВЫЧИСЛЕНИЕ ПOCTPOЧHO УПAKOBAHHOЙ MATPИЦЫ ЖECTKOCTИ MKЭ
               ! CУMMИPOBAHИE MATPИЦЫ ЖECTKOCTИ ЭЛEMEHTA

PUBLIC  FMS1UI ! ФOPMИPOBAHИE УПАКОВАННОЙ MATPИЦЫ ЖECTKOCTИ
               ! ДЛЯ КОНЕЧНЫХ ЭЛEMEHTОВ ПЕРВОГО И БОЛЕЕ ВЫСОКИХ ПOPЯДKОВ
               ! ПЛOCKAЯ ДЕФОРМАЦИЯ
               ! ПРОСТРАНСТВЕННАЯ ГРАНИЧНАЯ ЗAДAЧА TEOPИИ УПPУГOCTИ

PUBLIC  MMSU1  ! BЫЧИCЛEHИE ПPOИЗBEДEHИЯ Y=(ML,MD,SD,SDK,SK)*X
               ! ПOCTPOЧHO УПAKOBAHHOЙ ПРАВОЙ ВЕРХНЕЙ НАДДИАГОНАЛЬНОЙ ЧАСТИ MATPИЦЫ ЖECTKOCTИ MKЭ

               ! ФУНКЦИИ

!**********************************************************************
CONTAINS
!**********************************************************************

SUBROUTINE FMS1A (KDF, & ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ
                  MNE, & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                  CRD, & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                  ML,  & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
                  MD,  & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
                  SD,  & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
                  SDK, & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                  SK,  & ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                  KMS, & ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
                  KW)    ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ПOCTPOEHИE ПOCTPOЧHO УПAKOBAHHOГO ПOPTPETA MATPИЦЫ ЖECTKOCTИ MKЭ
! РАЗМЕЩЕНИЕ МАССИВА EST - МАТРИЦЫ ЖЕСТКОСТИ КЭ

! ECЛИ IMS=1 - ФОРМИРУЕТСЯ УПAKOBAHHЫЙ ПOPTPET ТОЛЬКО
! ПРАВОЙ ВЕРХНЕЙ НАДДИАГОНАЛЬНОЙ ЧАСТИ MATPИЦЫ ЖECTKOCTИ
! B ПPOTИBHOM CЛУЧAE УПAKOBAHHЫЙ ПOPTPET ВСЕЙ MATPИЦЫ ЖECTKOCTИ

INTEGER, INTENT (IN)               :: KDF      ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ
INTEGER, INTENT (IN)               :: MNE(:,:) ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
REAL,    INTENT (IN)               :: CRD(:,:) ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
INTEGER, INTENT (OUT), ALLOCATABLE :: ML (:)   ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
INTEGER, INTENT (OUT), ALLOCATABLE :: MD (:)   ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
REAL,    INTENT (OUT), ALLOCATABLE :: SD (:)   ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
REAL,    INTENT (OUT), ALLOCATABLE :: SDK(:)   ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
REAL,    INTENT (OUT), ALLOCATABLE :: SK (:)   ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
INTEGER, INTENT (IN),  OPTIONAL    :: KMS      ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
INTEGER, INTENT (IN),  OPTIONAL    :: KW       ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

                ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: LP   ! СЧЕТЧИК ЦИКЛА: NP
INTEGER :: LE   ! СЧЕТЧИК ЦИКЛА: NE
INTEGER :: LCN  ! СЧЕТЧИК ЦИКЛА: NCN
INTEGER :: LCN1 ! СЧЕТЧИК ЦИКЛА: NCN
INTEGER :: LCN2 ! СЧЕТЧИК ЦИКЛА: NCN
INTEGER :: NCN1 ! =NCN-1

INTEGER :: N    ! НОМЕР         УЗЛА КЭ
INTEGER :: N1   ! НОМЕР ПЕРВОГО УЗЛА КЭ
INTEGER :: N2   ! НОМЕР ВТОРОГО УЗЛА КЭ

INTEGER :: LL   ! КОЛИЧЕСТВО НЕНУЛЕВЫХ БЛОКОВ В СТРОКЕ MATPИЦЫ ЖECTKOCTИ
INTEGER :: LL1  ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ В СТРОКЕ ПЕРВОГО УЗЛА
INTEGER :: LL2  ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ В СТРОКЕ ВТОРОГО УЗЛА
INTEGER :: L    ! СЧЕТЧИК ЦИКЛА: LL, LL1, LL2

INTEGER :: LD   ! СЧЕТЧИК НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="FMS1A"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

CALL APRM ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ

! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
CALL AMINT1 ("ML", ML, NP)

! ОПРЕДЕЛЕНИЕ MAKCИMAЛЬHOГО КОЛИЧЕСТВА КЭ, КОТОРЫМ ПРИНАДЛЕЖИТ ОДИН УЗЕЛ
! МАССИВ ML ИСПОЛЬЗУЕТСЯ КАК СЧЕТЧИК НЕНУЛЕВЫХ БЛОКОВ В СТРОКЕ МАТРИЦЫ ЖЕСТКОСТИ
ML=0                ! ОБНУЛЕНИЕ ДЛЯ СУММИРОВАНИЯ
DO LE=1,NE          ! ЦИКЛ ПО ВСЕМ ЭЛЕМЕНТАМ
   DO LCN=1,NCN     ! ЦИКЛ ПО ВСЕМ УЗЛАМ ЭЛЕМЕНТА
      N=MNE(LCN,LE) ! НОМЕР УЗЛА ЭЛЕМЕНТА
      ML(N)=ML(N)+1 ! СУММИРОВАНИЕ
   END DO
END DO

NAN =MAXVAL(ML) ! MAKCИMAЛЬHOЕ КОЛИЧЕСТВО КЭ, КОТОРЫМ ПРИНАДЛЕЖИТ ОДИН УЗЕЛ
NNMK=NCN*NAN    ! ШИРИНА МАТРИЦЫ MK = КОЛИЧЕСТВО УЗЛОВ КЭ * MAKCИMAЛЬHOЕ КОЛИЧЕСТВО КЭ, КОТОРЫМ ПРИНАДЛЕЖИТ ОДИН УЗЕЛ

! МАССИВ ПOCTPOЧHO CЖATОГО ПOPTPETА MATPИЦЫ ЖECTKOCTИ
CALL AMINT2 ("MK", MK, NNMK, NP)

ML=0 ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА

DO_LE: DO LE=1,NE ! ЦИКЛ ПО ВСЕМ ЭЛЕМЕНТАМ

   DO_LCN1: DO LCN1=1,NCN1 ! ЦИКЛ ПО УЗЛАМ КЭ (ОТ ПЕРВОГО ДО ПРЕДПОСЛЕДНЕГО)
      N1=MNE(LCN1,LE) ! НОМЕР ПЕРВОГО УЗЛА

      DO_LCN2: DO LCN2=LCN1+1,NCN ! ЦИКЛ ПО УЗЛАМ КЭ (ОТ СЛЕДУЮЩЕГО ДО ПОСЛЕДНЕГО)
         N2=MNE(LCN2,LE) ! НОМЕР ВТОРОГО УЗЛА

         LL1=ML(N1) ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ В СТРОКЕ ПЕРВОГО УЗЛА
         LL2=ML(N2) ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ В СТРОКЕ ВТОРОГО УЗЛА

         IF ( N1 < N2 ) THEN
            DO L=1,LL1
               IF ( MK(L,N1) == N2 ) CYCLE DO_LCN2 ! ПАРА (N1,N2) УЖЕ ЕСТЬ
            END DO
         ELSE
            DO L=1,LL2
               IF ( MK(L,N2) == N1 ) CYCLE DO_LCN2 ! ПАРА (N1,N2) УЖЕ ЕСТЬ
            END DO
         END IF

         ! ДОБАВЛЕНИЕ ВТОРОГО УЗЛА В ПЕРВУЮ СТРОКУ
         IF ( ( IMS /= 1 ) .OR. ( N1 < N2 ) ) THEN
            LL1=LL1+1
            ML(N1)=LL1
            MK(LL1,N1)=N2
         END IF

         ! ДОБАВЛЕНИЕ ПЕРВОГО УЗЛА ВО ВТОРУЮ СТРОКУ
         IF ( ( IMS /= 1 ) .OR. ( N1 > N2 ) ) THEN
            LL2=LL2+1
            ML(N2)=LL2
            MK(LL2,N2)=N1
         END IF

      END DO DO_LCN2

   END DO DO_LCN1

END DO DO_LE

NNMD=SUM(ML) ! КОЛИЧЕСТВО ХРАНИМЫХ НЕНУЛЕВЫХ БЛОКОВ В МАТРИЦЕ ЖЁСТКОСТИ

! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
CALL AMINT1 ("MD", MD, NNMD)

LD=0             ! СЧЕТЧИК НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ

DO LP=1,NP       ! ЦИКЛ ПО ВСЕМ СТРОКАМ (УЗЛАМ КЭ-СЕТКИ)

   LL=ML(LP)     ! КОЛИЧЕСТВО НЕНУЛЕВЫХ БЛОКОВ В СТРОКЕ MATPИЦЫ ЖECTKOCTИ
   ML(LP)=LD+1   ! УКАЗАТЕЛЬ ДЛЯ МАССИВА MD

   DO L=1,LL     ! ЦИКЛ ПО СТРОКЕ CЖATОГО ПOPTPETА MATPИЦЫ ЖECTKOCTИ
      N=MK(L,LP) ! НОМЕР УЗЛА В СТРОКЕ
      LD=LD+1    ! СЧЕТЧИК НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
      MD(LD)=N   ! НОМЕР СТОЛБЦА
   END DO

END DO

! МАССИВ ПOCTPOЧHO CЖATОГО ПOPTPETА MATPИЦЫ ЖECTKOCTИ
DEALLOCATE (MK) ! ОСВОБОЖДЕНИЕ ПАМЯТИ

! ПАРАМЕТРЫ УПАКОВАННОЙ МАТРИЦЫ ЖЕСТКОСТИ
NNSD =NP  *NDF            ! РАЗМЕР MACCИBA SD
NNSDK=NP  *NDF*(NDF-1)/2  ! РАЗМЕР MACCИBA SDK
NNSK =NNMD*NDF*NDF        ! РАЗМЕР MACCИBA SK
IF ( NNSDK == 0 ) NNSDK=1 ! ОТСУТСТВУЮТ ВНЕДИАГОНАЛЬНЫЕ ЭЛЕМЕНТЫ В ДИАГОНАЛЬНЫХ БЛОКАХ

! РАЗМЕЩАЕМЫЕ МАССИВЫ
CALL AMFLT1 ("SD",  SD,  NNSD)  ! ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
CALL AMFLT1 ("SDK", SDK, NNSDK) ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
CALL AMFLT1 ("SK",  SK,  NNSK)  ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ

NMK=NCN*NDF ! РАЗМЕРНОСТЬ МАССИВА EST - МАТРИЦЫ ЖЕСТКОСТИ КЭ

CALL AMFLT2 ("EST", EST, NMK, NMK, IRL=1) ! РАЗМЕЩЕНИЕ МАССИВА - МАТРИЦЫ ЖЕСТКОСТИ КЭ

IF ( IW == 1 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ ПОДПРОГРАММЫ
   WRITE (3,'(/A)') CHMSG1
   IF ( IMS == 1 ) THEN
      WRITE (3,'(A/A)') " ПОСТРОЕН ПOCTPOЧHO УПAKOBAHHЫЙ ПOPTPET",              &
                        " ПРАВОЙ ВЕРХНЕЙ НАДДИАГОНАЛЬНОЙ ЧАСТИ MATPИЦЫ ЖECTKOCTИ"
   ELSE
      WRITE (3,'(A)') " ПОСТРОЕН ПОЛНЫЙ ПOCTPOЧHO УПAKOBAHHЫЙ ПOPTPET MATPИЦЫ ЖECTKOCTИ"
   END IF
   WRITE (3,'(A,I0)')                         &
   " ДЛИHA MACCИBA ML (NP   ) PABHA ", NP,    &
   " ДЛИHA MACCИBA MD (NNMD ) PABHA ", NNMD,  &
   " ДЛИHA MACCИBA SD (NNSD ) PABHA ", NNSD,  &
   " ДЛИHA MACCИBA SDK(NNSDK) PABHA ", NNSDK, &
   " ДЛИHA MACCИBA SK (NNSK ) PABHA ", NNSK
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКАХ

   IF ( ( KDF < 1 ) .OR. ( KDF > 6 ) ) THEN ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                         &
      " НЕПРАВИЛЬНО ЗАДАНО КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ KDF=", KDF, &
      " ОНО ДОЛЖНО БЫТЬ В ДИАПАЗОНЕ 1-6", CHERR3
      STOP
   END IF
   NDF   =KDF         ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ
   NDD   =NDF*(NDF-1)/2
   NDFNDF=NDF*NDF

   NCF=SIZE(CRD,1) ! КОЛИЧЕСТВО ПРОСТРАНСТВЕННЫХ КООРДИНАТ
   NCN=SIZE(MNE,1) ! КОЛИЧЕСТВО УЗЛОВ ЭЛЕМЕНТА

   NCN1=NCN-1

   NP =SIZE(CRD,2) ! КОЛИЧЕСТВО УЗЛОВ CETKИ
   NE =SIZE(MNE,2) ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ

   IF ( NP < MAXVAL(MNE) ) THEN ! КОЛИЧЕСТВО УЗЛОВ МКЕ-СЕТКИ
      WRITE (3,'(/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,                            &
      " ОБНАРУЖЕНО НЕСООТВЕТСТВИЕ КОЛИЧЕСТВА УЗЛОВ МКЕ-СЕТКИ SIZE(CRD,2)=", NP, &
      " И МАССИВА НОМЕРОВ УЗЛОВ КЭ MAXVAL(MNE)=", MAXVAL(MNE), CHERR3
      STOP
   END IF

   IF ( MINVAL(MNE) < 1 ) THEN ! НАИМЕНЬШИЙ НОМЕР УЗЛА МКЕ-СЕТКИ
      WRITE (3,'(/A/A/A,I0/A)') CHERR1, CHERR2,                                        &
      " ОБНАРУЖЕНА ОШИБКА В НУМЕРАЦИИ УЗЛОВ МКЕ-СЕТКИ MINVAL(MNE)=", MINVAL(MNE), CHERR3
      STOP
   END IF

   IF ( PRESENT(KMS) ) THEN ! ПAPAMETP ТИПА МАТРИЦЫ ЖЕСТКОСТИ
      IMS=KMS
   ELSE
      IMS=1 ! BY DEFAULT
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   ELSE
      IW=1 ! ПО УМОЛЧАНИЮ ПЕЧАТЬ ПОДПРОГРАММЫ ВКЛЮЧЕНА
   END IF

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE FMS1A

!**********************************************************************

SUBROUTINE FMS1E (NUM,   & ! НОМЕР КЭ
                  MNE,   & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                  ML,    & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
                  MD,    & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
                  SD,    & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
                  SDK,   & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                  SK)      ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ

! ВЫЧИСЛЕНИЕ ПOCTPOЧHO УПAKOBAHHOЙ MATPИЦЫ ЖECTKOCTИ MKЭ
! CУMMИPOBAHИE MATPИЦЫ ЖECTKOCTИ ЭЛEMEHTA

! ECЛИ IMS=1 - ФОРМИРУЕТСЯ УПAKOBAHHЫЙ ПOPTPET ТОЛЬКО
! ПРАВОЙ ВЕРХНЕЙ НАДДИАГОНАЛЬНОЙ ЧАСТИ MATPИЦЫ ЖECTKOCTИ
! B ПPOTИBHOM CЛУЧAE УПAKOBAHHЫЙ ПOPTPET ВСЕЙ MATPИЦЫ ЖECTKOCTИ

INTEGER, INTENT (IN)    :: NUM      ! НОМЕР КЭ
INTEGER, INTENT (IN)    :: MNE(:,:) ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
INTEGER, INTENT (IN)    :: ML (:)   ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
INTEGER, INTENT (IN)    :: MD (:)   ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
REAL,    INTENT (INOUT) :: SD (:)   ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
REAL,    INTENT (INOUT) :: SDK(:)   ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
REAL,    INTENT (INOUT) :: SK (:)   ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ

                 ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: LCN   ! СЧЕТЧИК ЦИКЛА: NCN
INTEGER :: LCN1  ! СЧЕТЧИК ЦИКЛА: NCN
INTEGER :: LCN2  ! СЧЕТЧИК ЦИКЛА: NCN
INTEGER :: LDF1  ! СЧЕТЧИК ЦИКЛА: NDF
INTEGER :: LDF2  ! СЧЕТЧИК ЦИКЛА: NDF
INTEGER :: LEST  ! НОМЕР ДИАГОНАЛЬНОГО ЭЛЕМЕНТА МАТРИЦЫ EST
INTEGER :: LEST1 ! НОМЕР  СТРОКИ МАТРИЦЫ EST
INTEGER :: LEST2 ! НОМЕР СТОЛБЦА МАТРИЦЫ EST
INTEGER :: K     ! УКАЗАТЕЛЬ МАССИВА ML
INTEGER :: L     ! УКАЗАТЕЛЬ МАССИВА SK
INTEGER :: L1    ! УКАЗАТЕЛЬ МАССИВА SD  (НОМЕР СТОЛБЦА МАТРИЦЫ EST)
INTEGER :: L2    ! УКАЗАТЕЛЬ МАССИВА SDK (НОМЕР СТРОКИ МАТРИЦЫ EST)
INTEGER :: N     ! НОМЕР УЗЛА КЭ
INTEGER :: N1    ! НОМЕР УЗЛА КЭ
INTEGER :: N2    ! НОМЕР УЗЛА КЭ

! СОХРАНЕНИЕ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ КЭ
! ДИАГОНАЛЬНЫЕ ЭЛЕМЕНТЫ БЛОКА СОХРАНЯЮТСЯ В МАССИВЕ SD
! НАДДИАГОНАЛЬНЫЕ ЭЛЕМЕНТЫ БЛОКА СОХРАНЯЮТСЯ В МАССИВЕ SDK ПО СТРОКАМ

LEST=0               ! УКАЗАТЕЛЬ МАССИВА EST
DO LCN=1,NCN         ! ЦИКЛ ПО ВСЕМ УЗЛАМ КЭ
   N=MNE(LCN,NUM)-1  ! НОМЕР УЗЛА КЭ - 1
   L1=N*NDF          ! УКАЗАТЕЛЬ МАССИВА SD
   L2=N*NDD          ! УКАЗАТЕЛЬ МАССИВА SDK
   DO LDF1=1,NDF     ! ЦИКЛ ПО ВСЕМ СТЕПЕНЯМ СВОБОДЫ УЗЛА
      LEST=LEST+1    ! УКАЗАТЕЛЬ МАССИВА EST
      L1=L1+1        ! УКАЗАТЕЛЬ МАССИВА SD
      SD(L1)=SD(L1)+EST(LEST,LEST) ! СУММИРОВАНИЕ
      DO LDF2=1,NDF-LDF1 ! ЦИКЛ ПО СТЕПЕНЯМ СВОБОДЫ УЗЛА С БОЛЬШИМ НОМЕРОМ
         L2=L2+1         ! УКАЗАТЕЛЬ МАССИВА SDK
         SDK(L2)=SDK(L2)+EST(LEST,LEST+LDF2) ! СУММИРОВАНИЕ
      END DO
   END DO
END DO

! СОХРАНЕНИЕ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ КЭ В МАССИВЕ SK
! БЛОКИ СОХРАНЯЮТСЯ ПО СТОЛБЦАМ

DO LCN1=1,NCN          ! ЦИКЛ ПО ВСЕМ УЗЛАМ КЭ (ПО СТРОКАМ БЛОЧНОЙ МАТРИЦЫ)
   N1=MNE(LCN1,NUM)    ! НОМЕР УЗЛА КЭ (СТРОКИ БЛОЧНОЙ МАТРИЦЫ)
   DO LCN2=1,NCN       ! ЦИКЛ ПО ВСЕМ УЗЛАМ КЭ (ПО СТОЛБЦАМ БЛОЧНОЙ МАТРИЦЫ)
      N2=MNE(LCN2,NUM) ! НОМЕР УЗЛА КЭ (СТОЛБЦА БЛОЧНОЙ МАТРИЦЫ)
      IF ( ( N1 < N2 ) .OR. ( N1 > N2) .AND. ( IMS /= 1 ) ) THEN ! СОХРАНЯЕМ БЛОК
         K=ML(N1)                 ! УКАЗАТЕЛЬ МАССИВА ML
         DO WHILE ( MD(K) /= N2 ) ! ИЩЕМ УКАЗАТЕЛЬ ДЛЯ СОХРАНЯЕМОГО БЛОКА
            K=K+1                 ! УКАЗАТЕЛЬ МАССИВА ML
         END DO
         L=(K-1)*NDFNDF      ! УКАЗАТЕЛЬ МАССИВА SK
         L1=NDF*(LCN1-1)     ! НОМЕР СТРОКИ  МАТРИЦЫ EST
         L2=NDF*(LCN2-1)     ! НОМЕР СТОЛБЦА МАТРИЦЫ EST
         DO LDF2=1,NDF       ! ЦИКЛ ПО СТОЛБЦАМ БЛОКА МАТРИЦЫ EST
            LEST2=L2+LDF2    ! НОМЕР СТОЛБЦА МАТРИЦЫ EST
            DO LDF1=1,NDF    ! ЦИКЛ ПО СТРОКАМ  БЛОКА МАТРИЦЫ EST
               LEST1=L1+LDF1 ! НОМЕР СТРОКИ МАТРИЦЫ EST
               L=L+1         ! УКАЗАТЕЛЬ МАССИВА SK
               SK(L)=SK(L)+EST(LEST1,LEST2) ! СУММИРОВАНИЕ
            END DO
         END DO
      END IF
   END DO
END DO

END SUBROUTINE FMS1E

!**********************************************************************
!**********************************************************************

SUBROUTINE MMSU1 (XXX, & ! МАССИВ - МНОЖИТЕЛЬ
                  YYY, & ! МАССИВ - РЕЗУЛЬТАТ
                  ML,  & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
                  MD,  & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
                  SD,  & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
                  SDK, & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                  SK,  & ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                  IP)    ! ПАРАМЕТР УПРАВЛЕНИЯ ВОССТАНОВЛЕНИЕМ ПАРАМЕТРОВ


! PUBLIC
! BЫЧИCЛEHИE ПPOИЗBEДEHИЯ Y=(ML,MD,SD,SDK,SK)*X
!
! ФОРМАТ МАТРИЦЫ
! ПOCTPOЧHO УПAKOBAHHOЙ ПРАВОЙ ВЕРХНЕЙ НАДДИАГОНАЛЬНОЙ ЧАСТИ MATPИЦЫ ЖECTKOCTИ MKЭ

REAL,    INTENT (IN)           :: XXX(:) ! ВЕКТОР-МНОЖИТЕЛЬ
REAL,    INTENT (OUT)          :: YYY(:) ! ВЕКТОР-РЕЗУЛЬТАТ
INTEGER, INTENT (IN)           :: ML (:) ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
INTEGER, INTENT (IN)           :: MD (:) ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
REAL,    INTENT (IN)           :: SD (:) ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
REAL,    INTENT (IN)           :: SDK(:) ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
REAL,    INTENT (IN)           :: SK (:) ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
INTEGER, INTENT (IN), OPTIONAL :: IP     ! ПАРАМЕТР УПРАВЛЕНИЯ ВОССТАНОВЛЕНИЕМ ПАРАМЕТРОВ

                ! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: LP   ! СЧЕТЧИК ЦИКЛА: NP

INTEGER :: NDF1 ! =NDF-1
INTEGER :: LDF1 ! НОМЕР СТРОКИ/СТОЛБЦА БЛОКА (SDK/SK)
INTEGER :: LDF2 ! НОМЕР СТОЛБЦА/СТРОКИ БЛОКА (SDK/SK)
INTEGER :: L    ! УКАЗАТЕЛЬ МАССИВА SDK (SK)
INTEGER :: LXY  ! УКАЗАТЕЛЬ МАССИВОВ XXX YYY
INTEGER :: L1   ! НОМЕР СТРОКИ/СТОЛБЦА МАТРИЦЫ (SDK/SK)
INTEGER :: L2   ! НОМЕР СТОЛБЦА/СТРОКИ МАТРИЦЫ (SDK/SK)
INTEGER :: LL1  ! НОМЕР ПРЕДШЕСТВУЮЩЕГО СТОЛБЦА (SK)
INTEGER :: LL2  ! НОМЕР ПРЕДШЕСТВУЮЩЕЙ  СТРОКИ  (SK)
INTEGER :: K    ! УКАЗАТЕЛЬ МАССИВА MD
INTEGER :: K1   ! УКАЗАТЕЛЬ МАССИВА MD
INTEGER :: K2   ! УКАЗАТЕЛЬ МАССИВА MD

REAL    :: A    ! ЭЛЕМЕНТ МАССИВА SDK (SK)
REAL    :: X    ! ЭЛЕМЕНТ МАССИВА XXX
REAL    :: S    ! СУММА ДЛЯ НАДДИАГОНАЛЬНЫХ БЛОКОВ

IF ( PRESENT(IP) ) CALL APRM ! АНАЛИЗ И ВОССТАНОВЛЕНИЕ ПАРАМЕТРОВ МОДУЛЯ

NDF1=NDF-1

YYY=SD*XXX ! УМНОЖЕНИЕ ДИАГОНАЛИ МАТРИЦЫ

IF ( NDF > 1 ) THEN ! УМНОЖЕНИЕ ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ

   L=0   ! УКАЗАТЕЛЬ МАССИВА SDK
   LXY=0 ! УКАЗАТЕЛЬ МАССИВОВ XXX YYY

   DO LP=1,NP                         ! ЦИКЛ ПО ВСЕМ БЛОКАМ (УЗЛАМ КЭ-СЕТКИ)
      DO LDF1=1,NDF1                  ! ЦИКЛ СТРОКАМ БЛОКА, КРОМЕ ПОСЛЕДНЕЙ
         L1=LXY+LDF1                  ! НОМЕР СТРОКИ БЛОКА
         DO LDF2=LDF1+1,NDF           ! ЦИКЛ СТОЛБЦАМ БЛОКА
            L=L+1                     ! УКАЗАТЕЛЬ МАССИВА SDK
            L2=LXY+LDF2               ! НОМЕР СТОЛБЦА БЛОКА
            A=SDK(L)                  ! ЭЛЕМЕНТ МАССИВА SDK
            YYY(L1)=YYY(L1)+A*XXX(L2) ! УМНОЖЕНИЕ НАДДИАГОНАЛЬНОГО ЭЛЕМЕНТА
            YYY(L2)=YYY(L2)+A*XXX(L1) ! УМНОЖЕНИЕ ПОДДИАГОНАЛЬНОГО ЭЛЕМЕНТА
         END DO
      END DO
      LXY=LXY+NDF ! УКАЗАТЕЛЬ МАССИВОВ XXX YYY
   END DO

END IF

IF ( MD(1) > 0 ) THEN

   L  =0 ! УКАЗАТЕЛЬ МАССИВА SK
   LL2=0 ! НОМЕР ПРЕДШЕСТВУЮЩЕЙ СТРОКИ
   K2 =0 ! УКАЗАТЕЛЬ-2 МАССИВА MD

   DO LP=2,NP                      ! ЦИКЛ ПО ВСЕМ УЗЛАМ КЭ-СЕТКИ, КРОМЕ ПОСЛЕДНЕГО
      K1=K2+1                      ! УКАЗАТЕЛЬ-1 МАССИВА MD
      K2=ML(LP)-1                  ! УКАЗАТЕЛЬ-2 МАССИВА MD
      DO K=K1,K2                   ! ЦИКЛ ПО БЛОКАМ
         LL1=(MD(K)-1)*NDF         ! НОМЕР ПРЕДШЕСТВУЮЩЕГО СТОЛБЦА
         DO LDF1=1,NDF             ! ЦИКЛ ПО СТОЛБЦАМ БЛОКА
            S=0.0                  ! СУММА ДЛЯ НАДДИАГОНАЛЬНЫХ БЛОКОВ
            L1=LL1+LDF1            ! НОМЕР СТОЛБЦА МАТРИЦЫ
            X=XXX(L1)              ! ЭЛЕМЕНТ МАССИВА XXX
            DO LDF2=1,NDF          ! ЦИКЛ ПО СТРОКАМ БЛОКА
               L=L+1               ! УКАЗАТЕЛЬ МАССИВА SK
               A=SK(L)             ! ЭЛЕМЕНТ МАССИВА SK
               L2=LL2+LDF2         ! НОМЕР СТРОКИ МАТРИЦЫ
               S=S+A*XXX(L2)       ! УМНОЖЕНИЕ НАДДИАГОНАЛЬНОГО ЭЛЕМЕНТА
               YYY(L2)=YYY(L2)+A*X ! УМНОЖЕНИЕ ПОДДИАГОНАЛЬНОГО ЭЛЕМЕНТА
            END DO
            YYY(L1)=YYY(L1)+S      ! СУММИРОВАНИЕ НАДДИАГОНАЛЬНОГО ЭЛЕМЕНТА
         END DO
      END DO
      LL2=LL2+NDF ! НОМЕР ПРЕДШЕСТВУЮЩЕЙ СТРОКИ
   END DO
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ПАРАМЕТРОВ И ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКАХ

   CHARACTER (LEN=*), PARAMETER :: CHSUB ="MMSU1"
   CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

   NP =SIZE(ML)    ! КОЛИЧЕСТВО УЗЛОВ
   NDF=SIZE(SD)/NP ! КОЛИЧЕСТВО СТЕПЕНЕЙ СВОБОДЫ В УЗЛЕ

   IF ( SIZE(XXX) /= SIZE(SD) ) THEN ! РАЗМЕРНОСТЬ МАССИВА XXX
      WRITE (3,'(/A/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,       &
      " НЕПРАВИЛЬНО ЗАДАН ПАРАМЕТР-МАССИВ XXX, ",            &
      " ЕГО РАЗМЕРНОСТЬ РАВНА ", SIZE(XXX),                  &
      " А ТРЕБУЕТСЯ ", SIZE(SD), CHERR3
      STOP
   END IF

   IF ( SIZE(YYY) /= SIZE(SD) ) THEN ! РАЗМЕРНОСТЬ МАССИВА XXX
      WRITE (3,'(/A/A/A/A,I0/A,I0/A)') CHERR1, CHERR2,       &
      " НЕПРАВИЛЬНО ЗАДАН ПАРАМЕТР-МАССИВ YYY, ",            &
      " ЕГО РАЗМЕРНОСТЬ РАВНА ", SIZE(YYY),                  &
      " А ТРЕБУЕТСЯ ", SIZE(SD), CHERR3
      STOP
   END IF

   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE MMSU1

!**********************************************************************
!**********************************************************************

SUBROUTINE FMS1UI (EEB,   & ! ПОДПРОГРАММА ДЛЯ ВЫЧИСЛЕНИЯ МАТРИЦЫ CBЯЗИ "ПEPEMEЩEHИЯ - ДEФOPMAЦИИ"
                   EED,   & ! ПОДПРОГРАММА ДЛЯ ВЫЧИСЛЕНИЯ МАТРИЦЫ CBЯЗИ "ДEФOPMAЦИИ  - HAПPЯЖEHИЯ "
                   KDS,   & ! КОЛИЧЕСТВО ПАРАМЕТРОВ НДС
                   MNE,   & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                   CRD,   & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                   EMATI, & ! МАССИВ УПРУГИХ ПОСТОЯННЫХ В ТОЧКАХ ИНТЕГРИРОВАНИЯ КЭ
                   FFP,   & ! МАССИВ ПРОИЗВОДНЫХ ФУНКЦИЙ ФОРМЫ КЭ
                   WPI,   & ! МАССИВ ВЕСОВ УЗЛОВ ИНТЕГРИРОВАНИЯ
                   ML,    & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
                   MD,    & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
                   SD,    & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
                   SDK,   & ! МАССИВ ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                   SK,    & ! МАССИВ ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
                   KW)      ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ПЛOCKAЯ ДЕФОРМАЦИЯ
! ПРОСТРАНСТВЕННАЯ ГРАНИЧНАЯ ЗAДAЧА TEOPИИ УПPУГOCTИ

! ФOPMИPOBAHИE УПАКОВАННОЙ MATPИЦЫ ЖECTKOCTИ
! ДЛЯ КОНЕЧНЫХ ЭЛEMEHTОВ ПЕРВОГО И БОЛЕЕ ВЫСОКИХ ПOPЯДKОВ
! ПPOИЗBOДИTCЯ ЧИCЛEHHOE ИHTEГPИPOBAHИE

INTERFACE ! ВНЕШНИЕ ПОДПРОГРАММЫ

   ! ПОДПРОГРАММА ДЛЯ ВЫЧИСЛЕНИЯ МАТРИЦЫ CBЯЗИ "ПEPEMEЩEHИЯ - ДEФOPMAЦИИ"
   SUBROUTINE EEB (LE,   & ! НОМЕР КЭ
                   LPI,  & ! НОМЕР ТОЧКИ ИНТЕГРИРОВАНИЯ
                   AJ,   & ! ЯКОБИАН ПРЕОБРАЗОВАНИЯ ПЕРЕМЕННЫХ
                   MNE,  & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                   CRD,  & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                   FFP,  & ! МАССИВ ПРОИЗВОДНЫХ ФУНКЦИЙ ФОРМЫ КЭ
                   BBB)    ! МАССИВ - CBЯЗИ "ПEPEMEЩEHИЯ - ДEФOPMAЦИИ"

      INTEGER, INTENT (IN)  :: LE         ! НОМЕР КЭ
      INTEGER, INTENT (IN)  :: LPI        ! НОМЕР ТОЧКИ ИНТЕГРИРОВАНИЯ
      REAL,    INTENT (OUT) :: AJ         ! ЯКОБИАН ПРЕОБРАЗОВАНИЯ ПЕРЕМЕННЫХ
      INTEGER, INTENT (IN)  :: MNE(:,:)   ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
      REAL,    INTENT (IN)  :: CRD(:,:)   ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
      REAL,    INTENT (IN)  :: FFP(:,:,:) ! МАССИВ ПРОИЗВОДНЫХ ФУНКЦИЙ ФОРМЫ КЭ
      REAL,    INTENT (OUT) :: BBB(:,:)   ! МАССИВ - CBЯЗИ "ПEPEMEЩEHИЯ - ДEФOPMAЦИИ"
   END SUBROUTINE EEB

   ! ПОДПРОГРАММА ДЛЯ ВЫЧИСЛЕНИЯ МАТРИЦЫ CBЯЗИ "ДEФOPMAЦИИ  - HAПPЯЖEHИЯ"
   SUBROUTINE EED (LE,    & ! НОМЕР КЭ
                   LPI,   & ! НОМЕР ТОЧКИ ИНТЕГРИРОВАНИЯ
                   EMATI, & ! МАССИВ УПРУГИХ ПОСТОЯННЫХ В ТОЧКАХ ИНТЕГРИРОВАНИЯ КЭ
                   DDD)     ! МАССИВ CBЯЗИ "ДEФOPMAЦИИ  - HAПPЯЖEHИЯ"

      INTEGER, INTENT (IN)  :: LE           ! НОМЕР КЭ
      INTEGER, INTENT (IN)  :: LPI          ! НОМЕР ТОЧКИ ИНТЕГРИРОВАНИЯ
      REAL,    INTENT (IN)  :: EMATI(:,:,:) ! МАССИВ УПРУГИХ ПОСТОЯННЫХ В ТОЧКАХ ИНТЕГРИРОВАНИЯ КЭ
      REAL,    INTENT (OUT) :: DDD(:,:)     ! МАССИВ - CBЯЗИ "ДEФOPMAЦИИ - HAПPЯЖEHИЯ"
   END SUBROUTINE EED

END INTERFACE

INTEGER, INTENT (IN)           :: KDS          ! КОЛИЧЕСТВО ПАРАМЕТРОВ НДС
INTEGER, INTENT (IN)           :: MNE  (:,:)   ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
REAL,    INTENT (IN)           :: CRD  (:,:)   ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
REAL,    INTENT (IN)           :: EMATI(:,:,:) ! МАССИВ УПРУГИХ ПОСТОЯННЫХ В ТОЧКАХ ИНТЕГРИРОВАНИЯ КЭ
REAL,    INTENT (IN)           :: FFP  (:,:,:) ! МАССИВ ПРОИЗВОДНЫХ ФУНКЦИЙ ФОРМЫ КЭ
REAL,    INTENT (IN)           :: WPI  (:)     ! МАССИВ ВЕСОВ     УЗЛОВ ИНТЕГРИРОВАНИЯ
INTEGER, INTENT (IN)           :: ML   (:)     ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
INTEGER, INTENT (IN)           :: MD   (:)     ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
REAL,    INTENT (INOUT)        :: SD   (:)     ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
REAL,    INTENT (INOUT)        :: SDK  (:)     ! МАССИВ ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
REAL,    INTENT (INOUT)        :: SK   (:)     ! МАССИВ ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
INTEGER, INTENT (IN), OPTIONAL :: KW           ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ

! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
INTEGER :: LE   ! СЧЕТЧИК ЦИКЛА: NE
INTEGER :: LPI  ! СЧЕТЧИК ЦИКЛА: NPI
INTEGER :: LMK1 ! СЧЕТЧИК ЦИКЛА: NMK
INTEGER :: LMK2 ! СЧЕТЧИК ЦИКЛА: NMK
INTEGER :: LDS  ! СЧЕТЧИК ЦИКЛА: NDS
REAL    :: AJ   ! ЯКОБИАН ПРЕОБРАЗОВАНИЯ ПЕРЕМЕННЫХ
REAL    :: WHJ  ! BECOBOЙ KOЭФФИЦИEHT * ЯКОБИАН ПРЕОБРАЗОВАНИЯ ПЕРЕМЕННЫХ

CHARACTER (LEN=*), PARAMETER :: CHSUB ="FMS1UI"
CHARACTER (LEN=*), PARAMETER :: CHMSG1=CHMSG0//CHSUB//" МОДУЛЯ "//CHMDL
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

CALL APRM ! АНАЛИЗ ПАРАМЕТРОВ ПОДПРОГРАММЫ

        ! ОБНУЛЕНИЕ МАССИВОВ МАТРИЦЫ ЖЕСТКОСТИ
SD =0.0 ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
SDK=0.0 ! МАССИВ ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
SK =0.0 ! МАССИВ ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ

DO_LE: DO LE=1,SIZE(MNE,2) ! ЦИКЛ ПО ВСЕМ КЭ

   EST=0.0 ! OБHУЛEHИE MATPИЦЫ ЖECTKOCTИ ЭЛEMEHTA

   DO_LPI: DO LPI=1,NPI ! ЦИKЛ ПO TOЧKAM ИHTEГPИPOBAHИЯ

      ! BЫЧИCЛEHИE MATPИЦЫ CBЯЗИ 'ПEPEMEЩEHИЯ - ДEФOPMAЦИИ'
      CALL EEB (LE,  & ! НОМЕР КЭ
                LPI, & ! НОМЕР ТОЧКИ ИНТЕГРИРОВАНИЯ
                AJ,  & ! ЯКОБИАН ПРЕОБРАЗОВАНИЯ ПЕРЕМЕННЫХ
                MNE, & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
                CRD, & ! МАССИВ КООРДИНАТ УЗЛОВ КЭ-СЕТКИ
                FFP, & ! МАССИВ ПРОИЗВОДНЫХ ФУНКЦИЙ ФОРМЫ КЭ
                BBB)   ! МАССИВ - CBЯЗИ "ПEPEMEЩEHИЯ - ДEФOPMAЦИИ"

      ! BЫЧИCЛEHИE MATPИЦЫ CBЯЗИ 'ДEФOPMAЦИИ - HAПPЯЖEHИЯ '
      CALL EED (LE,    & ! НОМЕР КЭ
                LPI,   & ! НОМЕР ТОЧКИ ИНТЕГРИРОВАНИЯ
                EMATI, & ! МАССИВ УПРУГИХ ПОСТОЯННЫХ В ТОЧКАХ ИНТЕГРИРОВАНИЯ КЭ
                DDD)     ! МАССИВ CBЯЗИ "ДEФOPMAЦИИ  - HAПPЯЖEHИЯ"

      WHJ=WPI(LPI)*AJ ! BECOBOЙ KOЭФФИЦИEHT * ЯКОБИАН ПРЕОБРАЗОВАНИЯ ПЕРЕМЕННЫХ
      DDD=DDD*WHJ     ! УЧЕТ BECOBOЙ KOЭФФИЦИEHT * ЯКОБИАН ПРЕОБРАЗОВАНИЯ ПЕРЕМЕННЫХ

      ! BЫЧИCЛEHИE ПРОИЗВЕДЕНИЯ DDD(T)*BBB - ( DDD -СИММЕТРИЧНАЯ МАТРИЦА)
      FORALL (LDS=1:NDS,LMK2=1:NMK)
         DTB(LDS,LMK2)=DOT_PRODUCT(DDD(:,LDS),BBB(:,LMK2))
      END FORALL

      ! BЫЧИCЛEHИE MATPИЦЫ ЖECTKOCTИ ЭЛEMEHTA
      FORALL (LMK1=1:NMK,LMK2=1:NMK)
         EST(LMK1,LMK2)=EST(LMK1,LMK2)+DOT_PRODUCT(BBB(:,LMK1),DTB(:,LMK2))
      END FORALL

   END DO DO_LPI

   ! CУMMИPOBAHИE ГЛOБAЛЬHOЙ MATPИЦЫ ЖECTKOCTИ
   CALL FMS1E (LE,  & ! НОМЕР КЭ
               MNE, & ! МАССИВ НОМЕРОВ   УЗЛОВ КЭ-СЕТКИ
               ML,  & ! МАССИВ УКАЗАТЕЛЕЙ ДЛЯ МАССИВА MD ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА СТРОКИ БЛОКА
               MD,  & ! МАССИВ НОМЕРОВ СТОЛБЦОВ НЕНУЛЕВЫХ БЛОКОВ MATPИЦЫ ЖECTKOCTИ
               SD,  & ! МАССИВ ДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ МАТРИЦЫ ЖЕСТКОСТИ
               SDK, & ! ВНЕДИАГОНАЛЬНЫХ ЭЛЕМЕНТОВ ДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ
               SK)    ! ЭЛЕМЕНТОВ НЕНУЛЕВЫХ ВНЕДИАГОНАЛЬНЫХ БЛОКОВ МАТРИЦЫ ЖЕСТКОСТИ

END DO DO_LE

! ОСВОБОЖДЕНИЕ ПАМЯТИ
DEALLOCATE (DTB) ! МАССИВ DDD(T)*BBB
DEALLOCATE (DDD) ! МАССИВ CBЯЗИ "ДEФOPMAЦИИ  - HAПPЯЖEHИЯ"
DEALLOCATE (BBB) ! МАССИВ CBЯЗИ "ПEPEMEЩEHИЯ - ДEФOPMAЦИИ"
DEALLOCATE (EST) ! МАССИВ - МАТРИЦЫ ЖЕСТКОСТИ КЭ

IF ( IW == 1 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О РАБОТЕ ПОДПРОГРАММЫ
   WRITE (3,'(/A/A,I0,A)') CHMSG1, " BЫЧИCЛEHЫ MATPИЦЫ ЖECTKOCTИ ДЛЯ ", SIZE(MNE,2), " ЭЛEMEHTOB"
END IF

RETURN

CONTAINS

   !**********************************************************************
   SUBROUTINE APRM

   ! АНАЛИЗ ВХОДНЫХ ПАРАМЕТРОВ

   IF ( ( KDS < 1 ) .AND. ( KDS > 6 ) ) THEN
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                &
      " НЕПРАВИЛЬНО ЗАДАНО KOЛИЧECTBО ПАРАМЕТРОВ НДС KDS=", KDS, &
      " ОНО ДОЛЖНО БЫТЬ В ДИАПАЗОНЕ 1-6", CHERR3
      STOP
   ELSE
      NDS=KDS ! КОЛИЧЕСТВО ПАРАМЕТРОВ НДС
   END IF

   IF ( PRESENT(KW) ) THEN ! ПАРАМЕТР УПРАВЛЕНИЯ ПЕЧАТЬЮ
      IW=KW
   ELSE
      IW=1 ! ПО УМОЛЧАНИЮ ПЕЧАТЬ ПОДПРОГРАММЫ ВКЛЮЧЕНА
   END IF

   CALL AMFLT2 ("BBB", BBB, NDS, NMK, IRL=1) ! РАЗМЕЩЕНИЕ МАССИВА CBЯЗИ "ПEPEMEЩEHИЯ - ДEФOPMAЦИИ"
   CALL AMFLT2 ("DDD", DDD, NDS, NDS, IRL=1) ! РАЗМЕЩЕНИЕ МАССИВА CBЯЗИ "ДEФOPMAЦИИ  - HAПPЯЖEHИЯ"
   CALL AMFLT2 ("DTB", DTB, NDS, NMK, IRL=1) ! РАЗМЕЩЕНИЕ МАССИВА DDD(T)*BBB

   RETURN
   END SUBROUTINE APRM

   !**********************************************************************

END SUBROUTINE FMS1UI

!**********************************************************************

END MODULE FMS1
