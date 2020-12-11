MODULE MEM ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМЫХ МАССИВОВ И ВЫЧИСЛЕНИЕ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ

!**********************************************************************
USE BASE  ! БАЗОВЫЙ КОНЕЧНО- И ГРАНИЧНО-ЭЛЕМЕНТНЫХ ПАКЕТОВ
!**********************************************************************
IMPLICIT NONE
!**********************************************************************
CHARACTER (LEN=*), PARAMETER, PRIVATE :: CHMDL="MEM"
!**********************************************************************
CHARACTER (LEN=*), PARAMETER, PRIVATE :: CHERR2A=" ОШИБКА ПРИ РАЗМЕЩЕНИИ МАССИВА "
CHARACTER (LEN=*), PARAMETER, PRIVATE :: CHERR2D=" ОШИБКА ПРИ ОСВОБОЖДЕНИИ ПАМЯТИ МАССИВА "
CHARACTER (LEN=*), PARAMETER, PRIVATE :: CHERR2E=" ОШИБКА - ТРЕБУЕТСЯ ПЕРЕРАЗМЕЩЕНИЕ МАССИВА "
!**********************************************************************

                                                     ! ПАРАМЕТРЫ МОДУЛЯ
INTEGER,                      PUBLIC  :: IWMEM =0    ! ПAPAMETP УПPABЛEHИЯ ПEЧATЬЮ MOДУЛЯ
                                                     ! ECЛИ IWMEM>0 - ПEЧATAETCЯ COOБЩEHИE O ВЫДЕЛЕНИИ ПАМЯТИ ДЛЯ МАССИВА
                                                     ! ECЛИ IWMEM=2 - ПEЧATAETCЯ COOБЩEHИE O ВЫДЕЛЕНИИ ПАМЯТИ ДЛЯ МАССИВА И ОБЩЕМ ОБЪЕМЕ ПАМЯТИ
                                                     ! B ПPOTИBHOM CЛУЧAE ПEЧATЬ OTCУTCTBУET

INTEGER, PARAMETER,           PRIVATE :: NMSV0 =100  ! МАКСИМАЛЬНО ДОПУСТИМОЕ КОЛИЧЕСТВО РАЗМЕЩАЕМЫХ МАССИВОВ
INTEGER,                      PRIVATE :: NMSV  =0    ! ТЕКУЩЕЕ КОЛИЧЕСТВО РАЗМЕЩЕННЫХ МАССИВОВ

INTEGER, PARAMETER,           PRIVATE :: NPREF =5    ! МАКСИМАЛЬНО ДОПУСТИМОЕ КОЛИЧЕСТВО ПРЕФИКСОВ ОБЪЕМА ПАМЯТИ
                                                     ! ECЛИ NPREF=1 - МАКСИМАЛЬНЫЙ ОБЪЕМ В     БАЙТАХ
                                                     !      NPREF=2 - МАКСИМАЛЬНЫЙ ОБЪЕМ В КИЛОБАЙТАХ
                                                     !      NPREF=3 - МАКСИМАЛЬНЫЙ ОБЪЕМ В МЕГАБАЙТАХ
                                                     !      NPREF=4 - МАКСИМАЛЬНЫЙ ОБЪЕМ В ГИГАБАЙТАХ
                                                     !      NPREF=5 - МАКСИМАЛЬНЫЙ ОБЪЕМ В ТЕРАБАЙТАХ

INTEGER, PARAMETER,           PRIVATE :: NMLEN = 8   ! МАКСИМАЛЬНО ДОПУСТИМАЯ ДЛИНА ИМЕНИ РАЗМЕЩАЕМОГО МАССИВА
INTEGER, PARAMETER,           PRIVATE :: NSLEN =64   ! МАКСИМАЛЬНО ДОПУСТИМАЯ ДЛИНА СЛЕДУЮЩЕЙ СТРОКИ
CHARACTER (LEN=NSLEN),        PRIVATE :: CHMSV       ! СТРОКА - "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)"
INTEGER,                      PRIVATE :: IER   =0    ! КОД ОШИБКИ ПРИ РАЗМЕЩЕНИИ МАССИВА

!**********************************************************************

                                                     ! РАЗМЕЩАЕМЫЕ МАССИВЫ
INTEGER, ALLOCATABLE,         PRIVATE :: MEMMSV(:,:) ! КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ДЛЯ ОТДЕЛЬНЫХ МАССИВОВ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ
                                                     ! MEMMSV(1,*) - ОБЪЕМ В     БАЙТАХ
                                                     ! MEMMSV(2,*) - ОБЪЕМ В КИЛОБАЙТАХ
                                                     ! MEMMSV(3,*) - ОБЪЕМ В МЕГАБАЙТАХ
                                                     ! MEMMSV(4,*) - ОБЪЕМ В ГИГАБАЙТАХ
                                                     ! MEMMSV(5,*) - ОБЪЕМ В ТЕРАБАЙТАХ
                                                     ! *           - НОМЕР В СПИСКЕ РАЗМЕЩЕННЫХ МАССИВОВ

INTEGER, ALLOCATABLE,         PRIVATE :: MEMALL(:,:) ! ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ДЛЯ МАССИВОВ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ И ТИПАМ
                                                     ! MEMALL(1,*) - ОБЪЕМ В     БАЙТАХ
                                                     ! MEMALL(2,*) - ОБЪЕМ В КИЛОБАЙТАХ
                                                     ! MEMALL(3,*) - ОБЪЕМ В МЕГАБАЙТАХ
                                                     ! MEMALL(4,*) - ОБЪЕМ В ГИГАБАЙТАХ
                                                     ! MEMALL(5,*) - ОБЪЕМ В ТЕРАБАЙТАХ
                                                     ! *=1         - ОБЪЕМ ПАМЯТИ ТИПА INTEGER
                                                     ! *=2         - ОБЪЕМ ПАМЯТИ ТИПА REAL
                                                     ! *=3         - ОБЩИЙ ПАМЯТИ ТИПА COMPLEX
                                                     ! *=4         - ОБЩИЙ ОБЪЕМ ПАМЯТИ

CHARACTER (LEN=NMLEN), ALLOCATABLE, PRIVATE :: MEMNAM(:) ! ИМЕН РАЗМЕЩАЕМЫХ МАССИВОВ ПАМЯТИ
INTEGER,               ALLOCATABLE, PRIVATE :: MEMKND(:) ! ПАРАМЕТРОВ РАЗНОВИДНОСТИ ТИПА

!**********************************************************************

               ! ПОДПРОГРАММЫ PUBLIC
PUBLIC  INIMEM ! РАЗМЕЩЕНИЕ МАССИВОВ ДЛЯ УЧЕТА ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ (MEMMSV, MEMALL, MEMNAM, MEMKND)
               ! ИНИЦИАЛИЗАЦИЯ МАССИВА - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ДЛЯ МАССИВОВ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ И ТИПАМ
PUBLIC  AMINT1 ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО ОДНОМЕРНОГО МАССИВА ТИПА INTEGER
PUBLIC  AMINT2 ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО  ДВУМЕРНОГО МАССИВА ТИПА INTEGER
PUBLIC  AMINT3 ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО ТРЕХМЕРНОГО МАССИВА ТИПА INTEGER

PUBLIC  AMFLT1 ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО ОДНОМЕРНОГО МАССИВА ТИПА REAL
PUBLIC  AMFLT2 ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО  ДВУМЕРНОГО МАССИВА ТИПА REAL
PUBLIC  AMFLT3 ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО ТРЕХМЕРНОГО МАССИВА ТИПА REAL

PUBLIC  AMCMP1 ! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО ОДНОМЕРНОГО МАССИВА ТИПА COMPLEX

PUBLIC  DAMSV  ! ОСВОБОЖДЕНИИ ПАМЯТИ МАССИВА

PUBLIC  WMEM1  ! ПЕЧАТЬ  СООБЩЕНИЯ О РАЗМЕРЕ ВСЕЙ ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ РАЗМЕЩАЕМЫХ МАССИВОВ
PUBLIC  WMEM2  ! ПЕЧАТЬ ТАБЛИЦЫ ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ РАЗМЕЩАЕМЫХ МАССИВОВ

               ! ПОДПРОГРАММЫ PRIVATE
PRIVATE CMEM1  ! ОПРЕДЕЛЕНИЕ РАЗМЕРА ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ МАССИВА
               ! ВНЕСЕНИЕ ДАННЫХ В МАССИВЫ:
               ! MEMMSV - КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ДЛЯ ОТДЕЛЬНЫХ МАССИВОВ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ
               ! MEMALL - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ДЛЯ МАССИВОВ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ И ТИПАМ
PRIVATE WMEM0  ! ПЕЧАТЬ СООБЩЕНИЯ О РАЗМЕРЕ ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ ОТДЕЛЬНОГО МАССИВА
               ! ПЕЧАТЬ СООБЩЕНИЯ О РАЗМЕРЕ ВСЕЙ ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ РАЗМЕЩАЕМЫХ МАССИВОВ

!**********************************************************************

CONTAINS

!**********************************************************************

SUBROUTINE INIMEM (KWMEM)


! PUBLIC
! РАЗМЕЩЕНИЕ МАССИВОВ ДЛЯ УЧЕТА ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ
! ИНИЦИАЛИЗАЦИЯ МАССИВА - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ДЛЯ МАССИВОВ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ

INTEGER, INTENT (IN), OPTIONAL :: KWMEM ! ПAPAMETP УПPABЛEHИЯ ПEЧATЬЮ MOДУЛЯ

CHARACTER (LEN=*), PARAMETER   :: CHSUB ="INIMEM"
CHARACTER (LEN=*), PARAMETER   :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

IF ( PRESENT(KWMEM) ) THEN ! ПAPAMETP УПPABЛEHИЯ ПEЧATЬЮ MOДУЛЯ
   IWMEM=KWMEM
END IF

ALLOCATE (MEMMSV(NPREF,NMSV0), STAT=IER) ! МАССИВ - КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ДЛЯ ОТДЕЛЬНЫХ МАССИВОВ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ

IF ( IER /= 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКЕ
   WRITE (3,'(/A/2A,I0/A)') CHERR1, CHERR2A, "MEMMSV - IER=", IER, CHERR3
   STOP
END IF

ALLOCATE (MEMALL(NPREF,4), STAT=IER) ! МАССИВ - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ДЛЯ МАССИВОВ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ И ТИПАМ

IF ( IER /= 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКЕ
   WRITE (3,'(/A/2A,I0/A)') CHERR1, CHERR2A, "MEMALL - IER=", IER, CHERR3
   STOP
END IF

MEMALL=0 ! ИНИЦИАЛИЗАЦИЯ МАССИВА - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ДЛЯ МАССИВОВ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ И ТИПАМ

ALLOCATE (MEMNAM(NMSV0), STAT=IER) ! МАССИВ - ИМЕН РАЗМЕЩАЕМЫХ МАССИВОВ ПАМЯТИ

IF ( IER /= 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКЕ
   WRITE (3,'(/A/2A,I0/A)') CHERR1, CHERR2A, "MEMNAM - IER=", IER, CHERR3
   STOP
END IF

ALLOCATE (MEMKND(NMSV0), STAT=IER) ! МАССИВ - ПАРАМЕТРОВ РАЗНОВИДНОСТИ ТИПА

IF ( IER /= 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКЕ
   WRITE (3,'(/A/2A,I0/A)') CHERR1, CHERR2A, "MEMKND - IER=", IER, CHERR3
   STOP
END IF

RETURN
END SUBROUTINE INIMEM

!**********************************************************************

SUBROUTINE AMINT1 (CHNAME, & ! СТРОКА - ИМЯ МАССИВА
                   IMSV,   & ! РАЗМЕЩАЕМЫЙ ОДНОМЕРНЫЙ МАССИВ ТИПА INTEGER
                   ISIZE1, & ! РАЗМЕР МАССИВА
                   IRL,    & ! REALLOCATE
                   INULL)    ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

! PUBLIC
! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО ОДНОМЕРНОГО МАССИВА ТИПА INTEGER
! ЕСЛИ ПРИСУТСТВУЕТ ПАРАМЕТР IRL МАССИВ СЛЕДУЕТ ПЕРЕРАЗМЕСТИТЬ
! ЕСЛИ ПРИСУТСТВУЕТ ПАРАМЕТР INULL ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

CHARACTER (LEN=*),    INTENT (IN)           :: CHNAME  ! СТРОКА - ИМЯ МАССИВА
INTEGER, ALLOCATABLE, INTENT (INOUT)        :: IMSV(:) ! РАЗМЕЩАЕМЫЙ ОДНОМЕРНЫЙ МАССИВ ТИПА INTEGER
INTEGER,              INTENT (IN)           :: ISIZE1  ! РАЗМЕР-1 МАССИВА
INTEGER,              INTENT (IN), OPTIONAL :: IRL     ! REALLOCATE
INTEGER,              INTENT (IN), OPTIONAL :: INULL   ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

CHARACTER (LEN=*),    PARAMETER             :: CHSUB ="AMINT1"
CHARACTER (LEN=*),    PARAMETER             :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! ПЕЧАТЬ "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)" В СТРОКУ
CHMSV=REPEAT(' ', NSLEN) ! ОЧИСТКА СТРОКИ
WRITE(CHMSV,'(2A,I0,A)') CHNAME, "(", ISIZE1, ")"

IF ( ALLOCATED(IMSV) ) THEN ! МАССИВ УЖЕ РАЗМЕЩЕН

   IF ( ISIZE1 == SIZE(IMSV,1) ) THEN ! РАЗМЕРНОСТИ МАССИВОВ СОВПАЛИ

      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ
         WRITE (3,'(/A)') " **** МАССИВ "//TRIM(ADJUSTL(CHMSV))//" РАНЕЕ РАЗМЕЩЕН В ПАМЯТИ"
      END IF
      RETURN ! НОВОЕ РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ НЕ ПРОИЗВОДИТСЯ

   ELSE ! РАЗМЕРНОСТИ МАССИВОВ НЕ СОВПАДАЮТ

      IF ( PRESENT(IRL) ) THEN  ! МАССИВ СЛЕДУЕТ ПЕРЕРАЗМЕСТИТЬ
         CALL DAMSV(CHNAME,   & ! СТРОКА - ИМЯ МАССИВА
                    IMSV1=IMSV) ! ОДНОМЕРНЫЙ МАССИВ ТИПА INTEGER
      ELSE                      ! ОШИБКА - ТРЕБУЕТСЯ ПЕРЕРАЗМЕЩЕНИЕ МАССИВА
         WRITE (3,'(/A/2A/A)') CHERR1, CHERR2E, CHNAME, CHERR3
         STOP
      END IF

   END IF

END IF

! РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ
IF ( PRESENT(INULL) ) THEN ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ
   ALLOCATE (IMSV(0:ISIZE1-1), STAT=IER)
ELSE
   ALLOCATE (IMSV(ISIZE1), STAT=IER)
ENDIF

IF ( IER /= 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКЕ
   WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2A, CHNAME, " - IER=", IER, CHERR3
   STOP
END IF

CALL CMEM1 (SIZE(IMSV), & ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА
            KIND(IMSV), & ! РАЗМЕР ЭЛЕМЕНТА МАССИВА В Byte
            1,          & ! ТИП ЭЛЕМЕНТОВ МАССИВА - INTEGER
            CHNAME)       ! СТРОКА - ИМЯ МАССИВА

RETURN
END SUBROUTINE AMINT1

!**********************************************************************

SUBROUTINE AMINT2 (CHNAME, & ! СТРОКА - ИМЯ МАССИВА
                   IMSV,   & ! РАЗМЕЩАЕМЫЙ ДВУМЕРНЫЙ МАССИВ ТИПА INTEGER
                   ISIZE1, & ! РАЗМЕР-1 МАССИВА
                   ISIZE2, & ! РАЗМЕР-2 МАССИВА
                   IRL,    & ! REALLOCATE
                   INULL)    ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

! PUBLIC
! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО ДВУМЕРНОГО МАССИВА ТИПА INTEGER
! ЕСЛИ ПРИСУТСТВУЕТ ПАРАМЕТР IRL МАССИВ СЛЕДУЕТ ПЕРЕРАЗМЕСТИТЬ
! ЕСЛИ ПРИСУТСТВУЕТ ПАРАМЕТР INULL ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

CHARACTER (LEN=*),    INTENT (IN)           :: CHNAME    ! СТРОКА - ИМЯ МАССИВА
INTEGER, ALLOCATABLE, INTENT (INOUT)        :: IMSV(:,:) ! РАЗМЕЩАЕМЫЙ ДВУМЕРНЫЙ МАССИВ ТИПА INTEGER
INTEGER,              INTENT (IN)           :: ISIZE1    ! РАЗМЕР-1 МАССИВА
INTEGER,              INTENT (IN)           :: ISIZE2    ! РАЗМЕР-2 МАССИВА
INTEGER,              INTENT (IN), OPTIONAL :: IRL       ! REALLOCATE
INTEGER,              INTENT (IN), OPTIONAL :: INULL     ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

CHARACTER (LEN=*),    PARAMETER             :: CHSUB ="AMINT2"
CHARACTER (LEN=*),    PARAMETER             :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! ПЕЧАТЬ "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)" В СТРОКУ
CHMSV=REPEAT(' ', NSLEN)      ! ОЧИСТКА СТРОКИ
WRITE(CHMSV,'(2A,I0,A,I0,A)') CHNAME, "(", ISIZE1, ",", ISIZE2, ")"

IF ( ALLOCATED(IMSV) ) THEN ! МАССИВ УЖЕ РАЗМЕЩЕН

   IF ( ( ISIZE1 == SIZE(IMSV,1) )  .AND. &
        ( ISIZE2 == SIZE(IMSV,2) ) ) THEN ! РАЗМЕРНОСТИ МАССИВОВ СОВПАЛИ

      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О ВЫДЕЛЕННОЙ ДЛЯ МАССИВА ПАМЯТИ
         WRITE (3,'(/A)') " **** МАССИВ "//TRIM(ADJUSTL(CHMSV))//" РАНЕЕ РАЗМЕЩЕН В ПАМЯТИ"
      END IF
      RETURN ! НОВОЕ РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ НЕ ПРОИЗВОДИТСЯ

   ELSE ! РАЗМЕРНОСТИ МАССИВОВ НЕ СОВПАДАЮТ

      IF ( PRESENT(IRL) ) THEN  ! МАССИВ СЛЕДУЕТ ПЕРЕРАЗМЕСТИТЬ
         CALL DAMSV(CHNAME,   & ! СТРОКА - ИМЯ МАССИВА
                    IMSV2=IMSV) ! ДВУМЕРНЫЙ МАССИВ ТИПА INTEGER
      ELSE                      ! ОШИБКА - ТРЕБУЕТСЯ ПЕРЕРАЗМЕЩЕНИЕ МАССИВА
         WRITE (3,'(/A/2A/A)') CHERR1, CHERR2E, CHNAME, CHERR3
         STOP
      END IF

   END IF

END IF

! РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ
IF ( PRESENT(INULL) ) THEN ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ
   ALLOCATE (IMSV(0:ISIZE1-1,0:ISIZE2-1), STAT=IER)
ELSE
   ALLOCATE (IMSV(ISIZE1,ISIZE2), STAT=IER)
ENDIF

IF ( IER /= 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКЕ
   WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2A, CHNAME, " - IER=", IER, CHERR3
   STOP
END IF

CALL CMEM1 (SIZE(IMSV), & ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА
            KIND(IMSV), & ! РАЗМЕР ЭЛЕМЕНТА МАССИВА В Byte
            1,          & ! ТИП ЭЛЕМЕНТОВ МАССИВА - INTEGER
            CHNAME)       ! СТРОКА - ИМЯ МАССИВА

RETURN
END SUBROUTINE AMINT2

!**********************************************************************

SUBROUTINE AMINT3 (CHNAME, & ! СТРОКА - ИМЯ МАССИВА
                   IMSV,   & ! РАЗМЕЩАЕМЫЙ ТРЕХМЕРНЫЙ МАССИВ ТИПА INTEGER
                   ISIZE1, & ! РАЗМЕР-1 МАССИВА
                   ISIZE2, & ! РАЗМЕР-2 МАССИВА
                   ISIZE3, & ! РАЗМЕР-3 МАССИВА
                   IRL,    & ! REALLOCATE
                   INULL)    ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

! PUBLIC
! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО ТРЕХМЕРНОГО МАССИВА ТИПА INTEGER
! ЕСЛИ ПРИСУТСТВУЕТ ПАРАМЕТР IRL МАССИВ СЛЕДУЕТ ПЕРЕРАЗМЕСТИТЬ
! ЕСЛИ ПРИСУТСТВУЕТ ПАРАМЕТР INULL ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

CHARACTER (LEN=*),    INTENT (IN)           :: CHNAME      ! СТРОКА - ИМЯ МАССИВА
INTEGER, ALLOCATABLE, INTENT (INOUT)        :: IMSV(:,:,:) ! РАЗМЕЩАЕМЫЙ ТРЕХМЕРНЫЙ МАССИВ ТИПА INTEGER
INTEGER,              INTENT (IN)           :: ISIZE1      ! РАЗМЕР-1 МАССИВА
INTEGER,              INTENT (IN)           :: ISIZE2      ! РАЗМЕР-2 МАССИВА
INTEGER,              INTENT (IN)           :: ISIZE3      ! РАЗМЕР-3 МАССИВА
INTEGER,              INTENT (IN), OPTIONAL :: IRL         ! REALLOCATE
INTEGER,              INTENT (IN), OPTIONAL :: INULL       ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

CHARACTER (LEN=*),    PARAMETER             :: CHSUB ="AMINT3"
CHARACTER (LEN=*),    PARAMETER             :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! ПЕЧАТЬ "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)" В СТРОКУ
CHMSV=REPEAT(' ', NSLEN)           ! ОЧИСТКА СТРОКИ
WRITE(CHMSV,'(2A,I0,A,I0,A,I0,A)') CHNAME, "(", ISIZE1, ",", ISIZE2, ",", ISIZE3, ")"

IF ( ALLOCATED(IMSV) ) THEN ! МАССИВ УЖЕ РАЗМЕЩЕН

   IF ( ( ISIZE1 == SIZE(IMSV,1) )  .AND. &
        ( ISIZE2 == SIZE(IMSV,2) )  .AND. &
        ( ISIZE3 == SIZE(IMSV,3) ) ) THEN ! РАЗМЕРНОСТИ МАССИВОВ СОВПАЛИ

      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О ВЫДЕЛЕННОЙ ДЛЯ МАССИВА ПАМЯТИ
         WRITE (3,'(/A)') " **** МАССИВ "//TRIM(ADJUSTL(CHMSV))//" РАНЕЕ РАЗМЕЩЕН В ПАМЯТИ"
      END IF
      RETURN ! НОВОЕ РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ НЕ ПРОИЗВОДИТСЯ

   ELSE ! РАЗМЕРНОСТИ МАССИВОВ НЕ СОВПАДАЮТ

      IF ( PRESENT(IRL) ) THEN  ! МАССИВ СЛЕДУЕТ ПЕРЕРАЗМЕСТИТЬ
         CALL DAMSV(CHNAME,   & ! СТРОКА - ИМЯ МАССИВА
                    IMSV3=IMSV) ! ТРЁХМЕРНЫЙ МАССИВ ТИПА INTEGER
      ELSE                      ! ОШИБКА - ТРЕБУЕТСЯ ПЕРЕРАЗМЕЩЕНИЕ МАССИВА
         WRITE (3,'(/A/2A/A)') CHERR1, CHERR2E, CHNAME, CHERR3
         STOP
      END IF

   END IF

END IF

! РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ
IF ( PRESENT(INULL) ) THEN ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ
   ALLOCATE (IMSV(0:ISIZE1-1,0:ISIZE2-1,0:ISIZE3-1), STAT=IER)
ELSE
   ALLOCATE (IMSV(ISIZE1,ISIZE2,ISIZE3), STAT=IER)
ENDIF

IF ( IER /= 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКЕ
   WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2A, CHNAME, " - IER=", IER, CHERR3
   STOP
END IF

CALL CMEM1 (SIZE(IMSV), & ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА
            KIND(IMSV), & ! РАЗМЕР ЭЛЕМЕНТА МАССИВА В Byte
            1,          & ! ТИП ЭЛЕМЕНТОВ МАССИВА - INTEGER
            CHNAME)       ! СТРОКА - ИМЯ МАССИВА

RETURN
END SUBROUTINE AMINT3

!**********************************************************************

SUBROUTINE AMFLT1 (CHNAME, & ! СТРОКА - ИМЯ МАССИВА
                   AMSV,   & ! РАЗМЕЩАЕМЫЙ ОДНОМЕРНЫЙ МАССИВ ТИПА REAL
                   ISIZE1, & ! РАЗМЕР МАССИВА
                   IRL,    & ! REALLOCATE
                   INULL)    ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

! PUBLIC
! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО ОДНОМЕРНОГО МАССИВА ТИПА REAL
! ЕСЛИ ПРИСУТСТВУЕТ ПАРАМЕТР IRL МАССИВ СЛЕДУЕТ ПЕРЕРАЗМЕСТИТЬ
! ЕСЛИ ПРИСУТСТВУЕТ ПАРАМЕТР INULL ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

CHARACTER (LEN=*), INTENT (IN)           :: CHNAME  ! СТРОКА - ИМЯ МАССИВА
REAL, ALLOCATABLE, INTENT (INOUT)        :: AMSV(:) ! РАЗМЕЩАЕМЫЙ ОДНОМЕРНЫЙ МАССИВ ТИПА REAL
INTEGER,           INTENT (IN)           :: ISIZE1  ! РАЗМЕР-1 МАССИВА
INTEGER,           INTENT (IN), OPTIONAL :: IRL     ! REALLOCATE
INTEGER,           INTENT (IN), OPTIONAL :: INULL   ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

CHARACTER (LEN=*), PARAMETER             :: CHSUB ="AMFLT1"
CHARACTER (LEN=*), PARAMETER             :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! ПЕЧАТЬ "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)" В СТРОКУ
CHMSV=REPEAT(' ', NSLEN) ! ОЧИСТКА СТРОКИ
WRITE(CHMSV,'(2A,I0,A)') CHNAME, "(", ISIZE1, ")"

IF ( ALLOCATED(AMSV) ) THEN ! МАССИВ УЖЕ РАЗМЕЩЕН

   IF ( ISIZE1 == SIZE(AMSV,1) ) THEN ! РАЗМЕРНОСТИ МАССИВОВ СОВПАЛИ

      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О ВЫДЕЛЕННОЙ ДЛЯ МАССИВА ПАМЯТИ
         WRITE (3,'(/A)') " **** МАССИВ "//TRIM(ADJUSTL(CHMSV))//" РАНЕЕ РАЗМЕЩЕН В ПАМЯТИ"
      END IF
      RETURN ! НОВОЕ РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ НЕ ПРОИЗВОДИТСЯ

   ELSE ! РАЗМЕРНОСТИ МАССИВОВ НЕ СОВПАДАЮТ

      IF ( PRESENT(IRL) ) THEN  ! МАССИВ СЛЕДУЕТ ПЕРЕРАЗМЕСТИТЬ
         CALL DAMSV(CHNAME,   & ! СТРОКА - ИМЯ МАССИВА
                    AMSV1=AMSV) ! ОДНОМЕРНЫЙ МАССИВ ТИПА REAL
      ELSE                      ! ОШИБКА - ТРЕБУЕТСЯ ПЕРЕРАЗМЕЩЕНИЕ МАССИВА
         WRITE (3,'(/A/2A/A)') CHERR1, CHERR2E, CHNAME, CHERR3
         STOP
      END IF

   END IF

END IF

! РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ
IF ( PRESENT(INULL) ) THEN ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ
   ALLOCATE (AMSV(0:ISIZE1-1), STAT=IER)
ELSE
   ALLOCATE (AMSV(ISIZE1), STAT=IER)
ENDIF

IF ( IER /= 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКЕ
   WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2A, CHNAME, " - IER=", IER, CHERR3
   STOP
END IF

CALL CMEM1 (SIZE(AMSV), & ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА
            KIND(AMSV), & ! РАЗМЕР ЭЛЕМЕНТА МАССИВА В Byte
            2,          & ! ТИП ЭЛЕМЕНТОВ МАССИВА - REAL
            CHNAME)       ! СТРОКА - ИМЯ МАССИВА

RETURN
END SUBROUTINE AMFLT1

!**********************************************************************

SUBROUTINE AMFLT2 (CHNAME, & ! СТРОКА - ИМЯ МАССИВА
                   AMSV,   & ! РАЗМЕЩАЕМЫЙ ДВУМЕРНЫЙ МАССИВ ТИПА REAL
                   ISIZE1, & ! РАЗМЕР-1 МАССИВА
                   ISIZE2, & ! РАЗМЕР-2 МАССИВА
                   IRL,    & ! REALLOCATE
                   INULL)    ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

! PUBLIC
! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО ДВУМЕРНОГО МАССИВА ТИПА REAL
! ЕСЛИ ПРИСУТСТВУЕТ ПАРАМЕТР IRL МАССИВ СЛЕДУЕТ ПЕРЕРАЗМЕСТИТЬ
! ЕСЛИ ПРИСУТСТВУЕТ ПАРАМЕТР INULL ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

CHARACTER (LEN=*), INTENT (IN)           :: CHNAME    ! СТРОКА - ИМЯ МАССИВА
REAL, ALLOCATABLE, INTENT (INOUT)        :: AMSV(:,:) ! РАЗМЕЩАЕМЫЙ ДВУМЕРНЫЙ МАССИВ ТИПА REAL
INTEGER,           INTENT (IN)           :: ISIZE1    ! РАЗМЕР-1 МАССИВА
INTEGER,           INTENT (IN)           :: ISIZE2    ! РАЗМЕР-2 МАССИВА
INTEGER,           INTENT (IN), OPTIONAL :: IRL       ! REALLOCATE
INTEGER,           INTENT (IN), OPTIONAL :: INULL     ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

CHARACTER (LEN=*), PARAMETER             :: CHSUB ="AMFLT2"
CHARACTER (LEN=*), PARAMETER             :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! ПЕЧАТЬ "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)" В СТРОКУ
CHMSV=REPEAT(' ', NSLEN)      ! ОЧИСТКА СТРОКИ
WRITE(CHMSV,'(2A,I0,A,I0,A)') CHNAME, "(", ISIZE1, ",", ISIZE2, ")"

IF ( ALLOCATED(AMSV) ) THEN ! МАССИВ УЖЕ РАЗМЕЩЕН

   IF ( ( ISIZE1 == SIZE(AMSV,1) )  .AND. &
        ( ISIZE2 == SIZE(AMSV,2) ) ) THEN ! РАЗМЕРНОСТИ МАССИВОВ СОВПАЛИ

      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О ВЫДЕЛЕННОЙ ДЛЯ МАССИВА ПАМЯТИ
         WRITE (3,'(/A)') " **** МАССИВ "//TRIM(ADJUSTL(CHMSV))//" РАНЕЕ РАЗМЕЩЕН В ПАМЯТИ"
      END IF
      RETURN ! НОВОЕ РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ НЕ ПРОИЗВОДИТСЯ

   ELSE ! РАЗМЕРНОСТИ МАССИВОВ НЕ СОВПАДАЮТ

      IF ( PRESENT(IRL) ) THEN  ! МАССИВ СЛЕДУЕТ ПЕРЕРАЗМЕСТИТЬ
         CALL DAMSV(CHNAME,   & ! СТРОКА - ИМЯ МАССИВА
                    AMSV2=AMSV) ! ДВУМЕРНЫЙ МАССИВ ТИПА REAL
      ELSE                      ! ОШИБКА - ТРЕБУЕТСЯ ПЕРЕРАЗМЕЩЕНИЕ МАССИВА
         WRITE (3,'(/A/2A/A)') CHERR1, CHERR2E, CHNAME, CHERR3
         STOP
      END IF

   END IF

END IF

! РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ
IF ( PRESENT(INULL) ) THEN ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ
   ALLOCATE (AMSV(0:ISIZE1-1,0:ISIZE2-1), STAT=IER)
ELSE
   ALLOCATE (AMSV(ISIZE1,ISIZE2), STAT=IER)
ENDIF

IF ( IER /= 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКЕ
   WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2A, CHNAME, " - IER=", IER, CHERR3
   STOP
END IF

CALL CMEM1 (SIZE(AMSV), & ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА
            KIND(AMSV), & ! РАЗМЕР ЭЛЕМЕНТА МАССИВА В Byte
            2,          & ! ТИП ЭЛЕМЕНТОВ МАССИВА - REAL
            CHNAME)       ! СТРОКА - ИМЯ МАССИВА

RETURN
END SUBROUTINE AMFLT2

!**********************************************************************

SUBROUTINE AMFLT3 (CHNAME, & ! СТРОКА - ИМЯ МАССИВА
                   AMSV,   & ! РАЗМЕЩАЕМЫЙ ТРЕХМЕРНЫЙ МАССИВ ТИПА REAL
                   ISIZE1, & ! РАЗМЕР-1 МАССИВА
                   ISIZE2, & ! РАЗМЕР-2 МАССИВА
                   ISIZE3, & ! РАЗМЕР-3 МАССИВА
                   IRL,    & ! REALLOCATE
                   INULL)    ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

! PUBLIC
! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО ТРЕХМЕРНОГО МАССИВА ТИПА REAL
! ЕСЛИ ПРИСУТСТВУЕТ ПАРАМЕТР IRL МАССИВ СЛЕДУЕТ ПЕРЕРАЗМЕСТИТЬ
! ЕСЛИ ПРИСУТСТВУЕТ ПАРАМЕТР INULL ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

CHARACTER (LEN=*), INTENT (IN)           :: CHNAME      ! СТРОКА - ИМЯ МАССИВА
REAL, ALLOCATABLE, INTENT (INOUT)        :: AMSV(:,:,:) ! РАЗМЕЩАЕМЫЙ ТРЕХМЕРНЫЙ МАССИВ ТИПА REAL
INTEGER,           INTENT (IN)           :: ISIZE1      ! РАЗМЕР-1 МАССИВА
INTEGER,           INTENT (IN)           :: ISIZE2      ! РАЗМЕР-2 МАССИВА
INTEGER,           INTENT (IN)           :: ISIZE3      ! РАЗМЕР-3 МАССИВА
INTEGER,           INTENT (IN), OPTIONAL :: IRL         ! REALLOCATE
INTEGER,           INTENT (IN), OPTIONAL :: INULL       ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

CHARACTER (LEN=*), PARAMETER             :: CHSUB ="AMFLT3"
CHARACTER (LEN=*), PARAMETER             :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! ПЕЧАТЬ "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)" В СТРОКУ
CHMSV=REPEAT(' ', NSLEN)           ! ОЧИСТКА СТРОКИ
WRITE(CHMSV,'(2A,I0,A,I0,A,I0,A)') CHNAME, "(", ISIZE1, ",", ISIZE2, ",", ISIZE3, ")"

IF ( ALLOCATED(AMSV) ) THEN ! МАССИВ УЖЕ РАЗМЕЩЕН

   IF ( ( ISIZE1 == SIZE(AMSV,1) )  .AND. &
        ( ISIZE2 == SIZE(AMSV,2) )  .AND. &
        ( ISIZE3 == SIZE(AMSV,3) ) ) THEN ! РАЗМЕРНОСТИ МАССИВОВ СОВПАЛИ

      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О ВЫДЕЛЕННОЙ ДЛЯ МАССИВА ПАМЯТИ
         WRITE (3,'(/A)') " **** МАССИВ "//TRIM(ADJUSTL(CHMSV))//" РАНЕЕ РАЗМЕЩЕН В ПАМЯТИ"
      END IF
      RETURN ! НОВОЕ РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ НЕ ПРОИЗВОДИТСЯ

   ELSE ! РАЗМЕРНОСТИ МАССИВОВ НЕ СОВПАДАЮТ

      IF ( PRESENT(IRL) ) THEN  ! МАССИВ СЛЕДУЕТ ПЕРЕРАЗМЕСТИТЬ
         CALL DAMSV(CHNAME,   & ! СТРОКА - ИМЯ МАССИВА
                    AMSV3=AMSV) ! ТРЁХМЕРНЫЙ МАССИВ ТИПА REAL
      ELSE                      ! ОШИБКА - ТРЕБУЕТСЯ ПЕРЕРАЗМЕЩЕНИЕ МАССИВА
         WRITE (3,'(/A/2A/A)') CHERR1, CHERR2E, CHNAME, CHERR3
         STOP
      END IF

   END IF

END IF

! РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ
IF ( PRESENT(INULL) ) THEN ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ
   ALLOCATE (AMSV(0:ISIZE1-1,0:ISIZE2-1,0:ISIZE3-1), STAT=IER)
ELSE
   ALLOCATE (AMSV(ISIZE1,ISIZE2,ISIZE3), STAT=IER)
ENDIF

IF ( IER /= 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКЕ
   WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2A, CHNAME, " - IER=", IER, CHERR3
   STOP
END IF

CALL CMEM1 (SIZE(AMSV), & ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА
            KIND(AMSV), & ! РАЗМЕР ЭЛЕМЕНТА МАССИВА В Byte
            2,          & ! ТИП ЭЛЕМЕНТОВ МАССИВА - REAL
            CHNAME)       ! СТРОКА -  ИМЯ МАССИВА

RETURN
END SUBROUTINE AMFLT3

!**********************************************************************

SUBROUTINE AMCMP1 (CHNAME, & ! СТРОКА - ИМЯ МАССИВА
                   ZMSV,   & ! РАЗМЕЩАЕМЫЙ ОДНОМЕРНЫЙ МАССИВ ТИПА COMPLEX
                   ISIZE1, & ! РАЗМЕР МАССИВА
                   IRL,    & ! REALLOCATE
                   INULL)    ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

! PUBLIC
! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО ОДНОМЕРНОГО МАССИВА ТИПА COMPLEX
! ЕСЛИ ПРИСУТСТВУЕТ ПАРАМЕТР IRL МАССИВ СЛЕДУЕТ ПЕРЕРАЗМЕСТИТЬ
! ЕСЛИ ПРИСУТСТВУЕТ ПАРАМЕТР INULL ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

CHARACTER (LEN=*),    INTENT (IN)           :: CHNAME  ! СТРОКА - ИМЯ МАССИВА
COMPLEX, ALLOCATABLE, INTENT (INOUT)        :: ZMSV(:) ! РАЗМЕЩАЕМЫЙ ОДНОМЕРНЫЙ МАССИВ ТИПА COMPLEX
INTEGER,              INTENT (IN)           :: ISIZE1  ! РАЗМЕР-1 МАССИВА
INTEGER,              INTENT (IN), OPTIONAL :: IRL     ! REALLOCATE
INTEGER,              INTENT (IN), OPTIONAL :: INULL   ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ

CHARACTER (LEN=*), PARAMETER             :: CHSUB ="AMCMP1"
CHARACTER (LEN=*), PARAMETER             :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! ПЕЧАТЬ "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)" В СТРОКУ
CHMSV=REPEAT(' ', NSLEN) ! ОЧИСТКА СТРОКИ
WRITE(CHMSV,'(2A,I0,A)') CHNAME, "(", ISIZE1, ")"

IF ( ALLOCATED(ZMSV) ) THEN ! МАССИВ УЖЕ РАЗМЕЩЕН

   IF ( ISIZE1 == SIZE(ZMSV,1) ) THEN ! РАЗМЕРНОСТИ МАССИВОВ СОВПАЛИ

      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ О ВЫДЕЛЕННОЙ ДЛЯ МАССИВА ПАМЯТИ
         WRITE (3,'(/A)') " **** МАССИВ "//TRIM(ADJUSTL(CHMSV))//" РАНЕЕ РАЗМЕЩЕН В ПАМЯТИ"
      END IF
      RETURN ! НОВОЕ РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ НЕ ПРОИЗВОДИТСЯ

   ELSE ! РАЗМЕРНОСТИ МАССИВОВ НЕ СОВПАДАЮТ

      IF ( PRESENT(IRL) ) THEN  ! МАССИВ СЛЕДУЕТ ПЕРЕРАЗМЕСТИТЬ
         CALL DAMSV(CHNAME,   & ! СТРОКА - ИМЯ МАССИВА
                    ZMSV1=ZMSV) ! ОДНОМЕРНЫЙ МАССИВ ТИПА COMPLEX
      ELSE                      ! ОШИБКА - ТРЕБУЕТСЯ ПЕРЕРАЗМЕЩЕНИЕ МАССИВА
         WRITE (3,'(/A/2A/A)') CHERR1, CHERR2E, CHNAME, CHERR3
         STOP
      END IF

   END IF

END IF

! РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ
IF ( PRESENT(INULL) ) THEN ! ИНДЕКСАЦИЯ МАССИВА НАЧИНАЕТСЯ С НУЛЯ
   ALLOCATE (ZMSV(0:ISIZE1-1), STAT=IER)
ELSE
   ALLOCATE (ZMSV(ISIZE1), STAT=IER)
ENDIF

IF ( IER /= 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОШИБКЕ
   WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2A, CHNAME, " - IER=", IER, CHERR3
   STOP
END IF

CALL CMEM1 (SIZE(ZMSV),   & ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА
            KIND(ZMSV)*2, & ! РАЗМЕР ЭЛЕМЕНТА МАССИВА В Byte
            3,            & ! ТИП ЭЛЕМЕНТОВ МАССИВА - COMPLEX
            CHNAME)         ! СТРОКА - ИМЯ МАССИВА

RETURN
END SUBROUTINE AMCMP1

!**********************************************************************
!**********************************************************************

SUBROUTINE CMEM1 (ISIZE,  & ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА
                  IKIND,  & ! РАЗМЕР ЭЛЕМЕНТА МАССИВА В Byte
                  ITYPE,  & ! ТИП ЭЛЕМЕНТОВ МАССИВА ( 1 - INTEGER, 2 - REAL, 3 - COMPLEX )
                  CHNAME)   ! СТРОКА - ИМЯ МАССИВА

! PRIVATE
! ОПРЕДЕЛЕНИЕ РАЗМЕРА ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ МАССИВА
! ВНЕСЕНИЕ ДАННЫХ В МАССИВЫ:
! MEMMSV - КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ДЛЯ ОТДЕЛЬНЫХ МАССИВОВ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ
! MEMALL - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ДЛЯ МАССИВОВ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ

INTEGER,           INTENT (IN) :: ISIZE  ! РАЗМЕР МАССИВА
INTEGER,           INTENT (IN) :: IKIND  ! РАЗМЕР ЭЛЕМЕНТА МАССИВА В Byte
INTEGER,           INTENT (IN) :: ITYPE  ! ТИП ЭЛЕМЕНТОВ МАССИВА ( 1 - INTEGER, 2 - REAL, 3 - COMPLEX )
CHARACTER (LEN=*), INTENT (IN) :: CHNAME ! СТРОКА - ИМЯ МАССИВА


INTEGER, PARAMETER :: LB1024=1024        ! ПРИМЕРНО 10**3
INTEGER            :: LE1024             ! КОЭФФИЦИЕНТ - КОЛИЧЕСТВО IKIND-БАЙТОВЫХ ЭЛЕМЕНТОВ В 1-ОМ КИЛОБАЙТЕ
INTEGER            :: LSIZE              ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА
INTEGER            :: LLL                ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА ДЛЯ ТЕКУЩЕГО ПРЕФИКСА
INTEGER            :: LPREF              ! СЧЕТЧИК ЦИКЛА: NPREF

CHARACTER (LEN=4)  :: CHKIND             ! СТРОКА ДЛЯ ПЕЧАТИ РАЗМЕРА ЭЛЕМЕНТА МАССИВА В Byte
CHARACTER (LEN=8)  :: CHTYPE             ! СТРОКА ДЛЯ ПЕЧАТИ ТИПА ЭЛЕМЕНТОВ МАССИВА

INTEGER            :: LNEXT(4)           ! ЛОКАЛЬНЫЙ МАССИВ ДЛЯ ПЕРЕНОСА ЕДИНИЦ В СЛЕДУЮЩИЙ ПРЕФИКС

CHARACTER (LEN=*), PARAMETER :: CHSUB ="CMEM1"
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

! ПРОВЕРКА ПАРАМЕТРОВ
IF ( NMSV == NMSV0 ) THEN
   WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,                     &
   " ТРЕБУЕТСЯ УВЕЛИЧИТЬ ЗНАЧЕНИЕ ПAPAMETPА NMSV0=", NMSV0,        &
   " КОЛИЧЕСТВО РАЗМЕЩАЕМЫХ МАССИВОВ ПРЕВЫШАЕТ ЭТО ЗНАЧЕНИЕ", CHERR3
   STOP
END IF


SELECT CASE (ITYPE) ! ТИП ЭЛЕМЕНТОВ МАССИВА
   CASE (1)
      CHTYPE="INTEGER"
   CASE (2)
      CHTYPE="REAL"
   CASE (3)
      CHTYPE="COMPLEX"
   CASE DEFAULT
      WRITE (3,'(/A/A/A,I0/A/A)') CHERR1, CHERR2,  &
      " HEПPABИЛЬHO ЗAДAH ПAPAMETP ITYPE=", ITYPE, &
      " ОН ДОЛЖЕН БЫТЬ В ДИАПАЗОНЕ 1-3", CHERR3
      STOP
END SELECT

WRITE(CHKIND,'(I4)') IKIND ! ПЕЧАТЬ РАЗМЕРА ЭЛЕМЕНТА МАССИВА В Byte В СТРОКУ

NMSV=NMSV+1                ! ТЕКУЩЕЕ КОЛИЧЕСТВО РАЗМЕЩЕННЫХ МАССИВОВ

MEMNAM(NMSV)=CHNAME        ! ИМЯ РАЗМЕЩАЕМОГО МАССИВА
MEMKND(NMSV)=IKIND         ! ПАРАМЕТРОВ РАЗНОВИДНОСТИ ТИПА

LE1024=LB1024/IKIND        ! КОЭФФИЦИЕНТ - КОЛИЧЕСТВО IKIND-БАЙТОВЫХ ЭЛЕМЕНТОВ В 1-ОМ КИЛОБАЙТЕ

LSIZE =ISIZE               ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА

LNEXT=0                    ! ЛОКАЛЬНЫЙ МАССИВ ДЛЯ ПЕРЕНОСА ЕДИНИЦ В СЛЕДУЮЩИЙ ПРЕФИКС

!DO L4=1,4                  ! ЦИКЛ ДЛЯ ВСЕХ СТРОК МАССИВА MEMALL
!   LNEXT(L4)=0             ! ПЕРЕНОС ЕДИНИЦЫ В БАЙТЫ НЕ ТРЕБУЕТСЯ
!END DO

DO LPREF=1,NPREF           ! ЦИКЛ ПО ВСЕМ ПРЕФИКСАМ

  IF ( LPREF == 1 ) THEN
      LLL=MOD(LSIZE,LE1024)        ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА ДЛЯ ТЕКУЩЕГО ПРЕФИКСА
      MEMMSV(LPREF,NMSV)=LLL*IKIND ! КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ МАССИВА ДЛЯ ТЕКУЩЕГО ПРЕФИКСА
      LSIZE=LSIZE/LE1024           ! СДВИГ КОЛИЧЕСТВА ЭЛЕМЕНТОВ МАССИВА НА ОДИН ПРЕФИКС
   ELSE
      LLL=MOD(LSIZE,LB1024)        ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА ДЛЯ ТЕКУЩЕГО ПРЕФИКСА
      MEMMSV(LPREF,NMSV)=LLL       ! КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ МАССИВА ДЛЯ ТЕКУЩЕГО ПРЕФИКСА
      LSIZE=LSIZE/LB1024           ! СДВИГ КОЛИЧЕСТВА ЭЛЕМЕНТОВ МАССИВА НА ОДИН ПРЕФИКС
   END IF

   MEMALL(LPREF,ITYPE)=MEMALL(LPREF,ITYPE)+MEMMSV(LPREF,NMSV)+LNEXT(ITYPE) ! СУММИРОВАНИЕ КОЛИЧЕСТВА ВЫДЕЛЕННОЙ ПАМЯТИ ТИПА ITYPE ДЛЯ ТЕКУЩЕГО ПРЕФИКСА
   MEMALL(LPREF,    4)=MEMALL(LPREF,    4)+MEMMSV(LPREF,NMSV)+LNEXT(    4) ! СУММИРОВАНИЕ ОБЩЕГО КОЛИЧЕСТВА ВЫДЕЛЕННОЙ ПАМЯТИ     ДЛЯ ТЕКУЩЕГО ПРЕФИКСА

   IF ( MEMALL(LPREF,ITYPE) >= LB1024 ) THEN         ! ТРЕБУЕТСЯ НОРМАЛИЗАЦИЯ ТЕКУЩЕГО ПРЕФИКСА ДЛЯ ПАМЯТИ ТИПА ITYPE
      MEMALL(LPREF,ITYPE)=MEMALL(LPREF,ITYPE)-LB1024 ! НОРМАЛИЗАЦИЯ ТЕКУЩЕГО ПРЕФИКСА ДЛЯ ПАМЯТИ ТИПА ITYPE
      LNEXT(ITYPE)=1                                 ! ПЕРЕНОС ЕДИНИЦЫ В СЛЕДУЮЩИЙ ПРЕФИКС ДЛЯ ПАМЯТИ ТИПА ITYPE
   ELSE                                              ! НОРМАЛИЗАЦИЯ ТЕКУЩЕГО ПРЕФИКСА ДЛЯ ПАМЯТИ ТИПА ITYPE НЕ ТРЕБУЕТСЯ
      LNEXT(ITYPE)=0                                 ! ПЕРЕНОС ЕДИНИЦЫ В СЛЕДУЮЩИЙ ПРЕФИКС ДЛЯ ПАМЯТИ ТИПА ITYPE НЕ ВЫПОЛНЯЕТСЯ
   END IF

   IF ( MEMALL(LPREF,4) >= LB1024 ) THEN             ! ТРЕБУЕТСЯ НОРМАЛИЗАЦИЯ ТЕКУЩЕГО ПРЕФИКСА ДЛЯ ОБЩЕГО КОЛИЧЕСТВА ВЫДЕЛЕННОЙ ПАМЯТИ
      MEMALL(LPREF,4)=MEMALL(LPREF,4)-LB1024         ! НОРМАЛИЗАЦИЯ ТЕКУЩЕГО ПРЕФИКСА ДЛЯ ОБЩЕГО КОЛИЧЕСТВА ВЫДЕЛЕННОЙ ПАМЯТИ
      LNEXT(4)=1                                     ! ПЕРЕНОС ЕДИНИЦЫ В СЛЕДУЮЩИЙ ПРЕФИКС ДЛЯ ОБЩЕГО КОЛИЧЕСТВА ВЫДЕЛЕННОЙ ПАМЯТИ
   ELSE                                              ! НОРМАЛИЗАЦИЯ ТЕКУЩЕГО ПРЕФИКСА ДЛЯ ОБЩЕГО КОЛИЧЕСТВА ВЫДЕЛЕННОЙ ПАМЯТИ НЕ ТРЕБУЕТСЯ
      LNEXT(4)=0                                     ! ПЕРЕНОС ЕДИНИЦЫ В СЛЕДУЮЩИЙ ПРЕФИКС ДЛЯ ОБЩЕГО КОЛИЧЕСТВА ВЫДЕЛЕННОЙ ПАМЯТИ НЕ ВЫПОЛНЯЕТСЯ
   END IF

END DO

IF ( LSIZE > 0 ) THEN ! НЕДОПУСТИМАЯ СИТУАЦИЯ
   WRITE (3,'(/A/A/A,I0,A/A,I0/A)') CHERR1, CHERR2,                                                              &
   " ПРИ ВЫЧИСЛЕНИИ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ МАССИВА ", CHNAME, " ТРЕБУЕТСЯ УВЕЛИЧИТЬ КОЛИЧЕСТВО ПРЕФИКСОВ", &
   " СЕЙЧАС ПАРАМЕТР NPREF (КОЛИЧЕСТВО ПРЕФИКСОВ ОБЪЕМА ПАМЯТИ) РАВЕН ", NPREF, CHERR3
   STOP
END IF

IF ( LNEXT(4) == 1 ) THEN ! НЕДОПУСТИМАЯ СИТУАЦИЯ
   WRITE (3,'(/A/A/A/A,I0/A)') CHERR1, CHERR2,                                                      &
   " ПРИ ВЫЧИСЛЕНИИ ОБЩЕГО ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ ТРЕБУЕТСЯ ПЕРЕНОС ЕДИНИЦЫ В СЛЕДУЮЩИЙ ПРЕФИКС", &
   " СЕЙЧАС ПАРАМЕТР NPREF (КОЛИЧЕСТВО ПРЕФИКСОВ ОБЪЕМА ПАМЯТИ) РАВЕН ", NPREF, CHERR3
   STOP
END IF

IF ( IWMEM > 0 ) THEN   ! ПЕЧАТЬ СООБЩЕНИЯ О ВЫДЕЛЕННОЙ ДЛЯ МАССИВА ПАМЯТИ

   CALL WMEM0 (MEMMSV(1,NMSV),  & ! МАССИВ - КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ МАССИВА CHNAME, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ
               "**** ДЛЯ МАССИВА "//TRIM(ADJUSTL(CHMSV))//" ВЫДЕЛЕНО ПАМЯТИ ("// &
               TRIM(ADJUSTL(CHTYPE))//", KIND="//TRIM(ADJUSTL(CHKIND))//"):") ! СТРОКА ПРЕФИКСНОГО СООБЩЕНИЯ

END IF

IF ( IWMEM == 2 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ ОБ ОБЩЕМ ОБЪЕМЕ ВЫДЕЛЕННОЙ ПАМЯТИ

   CALL WMEM0 (MEMALL(1,1), & ! МАССИВ - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ
               "**** ВЫДЕЛЕНО ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ МАССИВОВ ТИПА INTEGER") ! СТРОКА ПРЕФИКСНОГО СООБЩЕНИЯ

   CALL WMEM0 (MEMALL(1,2), & ! МАССИВ - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ
               "**** ВЫДЕЛЕНО ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ МАССИВОВ ТИПА REAL")    ! СТРОКА ПРЕФИКСНОГО СООБЩЕНИЯ

   CALL WMEM0 (MEMALL(1,3), & ! МАССИВ - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ
               "**** ВЫДЕЛЕНО ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ МАССИВОВ ТИПА COMPLEX") ! СТРОКА ПРЕФИКСНОГО СООБЩЕНИЯ

   CALL WMEM0 (MEMALL(1,4), & ! МАССИВ - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ
               "**** ВСЕГО ВЫДЕЛЕНО ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ МАССИВОВ")        ! СТРОКА ПРЕФИКСНОГО СООБЩЕНИЯ

END IF

END SUBROUTINE CMEM1

!**********************************************************************

SUBROUTINE WMEM0 (MEM,   & ! МАССИВ - КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ
                  CH)      ! СТРОКА ПРЕФИКСНОГО СООБЩЕНИЯ

! PRIVATE
! ПЕЧАТЬ СООБЩЕНИЯ О РАЗМЕРЕ ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ ОТДЕЛЬНОГО МАССИВА
! ПЕЧАТЬ СООБЩЕНИЯ О РАЗМЕРЕ ВСЕЙ ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ РАЗМЕЩАЕМЫХ МАССИВОВ

INTEGER, INTENT (IN)           :: MEM(NPREF) ! МАССИВ - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ
CHARACTER (LEN=*), INTENT (IN) :: CH         ! СТРОКА ПРЕФИКСНОГО СООБЩЕНИЯ

INTEGER                        :: LPREF      ! СЧЕТЧИК ЦИКЛА: NPREF

CHARACTER (LEN=*), PARAMETER :: CHSUB ="WMEM0"
CHARACTER (LEN=*), PARAMETER :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

IF ( SUM(MEM) == 0 ) RETURN ! ВЫДЕЛЕНИЕ ПАМЯТИ ДАННОГО ТИПА ОТСУТСТВУЕТ

WRITE (3,'(/A,A)') " ", CH ! ПРОПУСК ОДНОЙ СТРОКИ И ПЕЧАТЬ ПРЕФИКСНОГО СООБЩЕНИЯ

DO LPREF=NPREF,1,-1   ! ОБРАТНЫЙ ЦИКЛ ПО ВСЕМ ПРЕФИКСАМ

   SELECT CASE (LPREF) ! ТЕКУЩИЙ ПРЕФИКС

      CASE (1) ! ОБЪЕМ В БАЙТАХ
         IF ( MEM(1) > 0 ) THEN
            WRITE (3,'(I5,A)')  MEM(1), " BYTE "
         END IF

      CASE (2) ! ОБЪЕМ В КИЛОБАЙТАХ
         IF ( MEM(2) > 0 ) THEN
            WRITE (3,'(I5,A)')  MEM(2), " KB "
         END IF

      CASE (3) ! ОБЪЕМ В МЕГАБАЙТАХ
         IF ( MEM(3) > 0 ) THEN
            WRITE (3,'(I5,A)')  MEM(3), " MB "
         END IF

      CASE (4) ! ОБЪЕМ В ГИГАБАЙТАХ
         IF ( MEM(4) > 0 ) THEN
            WRITE (3,'(I5,A)')  MEM(4), " GB "
         END IF

      CASE (5) ! ОБЪЕМ В ТЕРАБАЙТАХ
         IF ( MEM(5) > 0 ) THEN
            WRITE (3,'(I5,A)')  MEM(5), " TB "
         END IF

      CASE DEFAULT
         WRITE (3,'(/A/A/A/A,I0/A)') CHERR1, CHERR2,                                       &
         " ПРИ ПЕЧАТИ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ ПРЕВЫШЕНО КОЛИЧЕСТВО ДОПУСТИМЫХ ПРЕФИКСОВ", &
         " ПАРАМЕТР NPREF (КОЛИЧЕСТВО ПРЕФИКСОВ ОБЪЕМА ПАМЯТИ) РАВЕН ", NPREF,             &
         " МАКСИМАЛЬНЫЙ ДОПУСТИМЫЙ ПРЕФИКС ТЕРА - NPREF=5"
         RETURN

   END SELECT

END DO

END SUBROUTINE WMEM0

!**********************************************************************

SUBROUTINE WMEM1

! PUBLIC
! ПЕЧАТЬ  СООБЩЕНИЯ О РАЗМЕРЕ ВСЕЙ ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ РАЗМЕЩАЕМЫХ МАССИВОВ

CALL WMEM0 (MEMALL(1,1), & ! МАССИВ - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ
            "**** ВЫДЕЛЕНО ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ МАССИВОВ ТИПА INTEGER") ! СТРОКА ПРЕФИКСНОГО СООБЩЕНИЯ

CALL WMEM0 (MEMALL(1,2), & ! МАССИВ - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ
            "**** ВЫДЕЛЕНО ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ МАССИВОВ ТИПА REAL")    ! СТРОКА ПРЕФИКСНОГО СООБЩЕНИЯ

CALL WMEM0 (MEMALL(1,3), & ! МАССИВ - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ
            "**** ВЫДЕЛЕНО ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ МАССИВОВ ТИПА COMPLEX") ! СТРОКА ПРЕФИКСНОГО СООБЩЕНИЯ

CALL WMEM0 (MEMALL(1,4), & ! МАССИВ - ОБЩЕЕ КОЛИЧЕСТВО ВЫДЕЛЕННОЙ ПАМЯТИ, РАЗДЕЛЕННОЕ ПО ПРЕФИКСАМ
            "**** ВСЕГО ВЫДЕЛЕНО ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ МАССИВОВ")        ! СТРОКА ПРЕФИКСНОГО СООБЩЕНИЯ

END SUBROUTINE WMEM1

!**********************************************************************

SUBROUTINE WMEM2

! PUBLIC
! ПЕЧАТЬ ТАБЛИЦЫ ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ РАЗМЕЩАЕМЫХ МАССИВОВ

CHARACTER (LEN=NMLEN) :: CH0        ! СТРОКА ДЛЯ ПЕЧАТИ ИМЕНИ МАССИВА
CHARACTER (LEN=NMLEN) :: CHK        ! СТРОКА ДЛЯ ПЕЧАТИ ИМЕНИ МАССИВА
CHARACTER (LEN=4    ) :: CH1(NPREF) ! СТРОКА ДЛЯ ПЕЧАТИ РАЗМЕРОВ ВЫДЕЛЕННОЙ ПАМЯТИ ПО ПРЕФИКСАМ
CHARACTER (LEN=4    ) :: CH="    "  ! ПУСТАЯ СТРОКА

INTEGER               :: LPREF      ! СЧЕТЧИК ЦИКЛА: NPREF
INTEGER               :: LMSV       ! СЧЕТЧИК ЦИКЛА: NMSV
INTEGER               :: L4         ! СЧЕТЧИК ЦИКЛА: =1,4

WRITE (3,'(/1X,A)') "ТАБЛИЦА ВЫДЕЛЕННОЙ ПАМЯТИ ДЛЯ ЭЛЕМЕНТОВ РАЗМЕЩАЕМЫХ МАССИВОВ"

CH0="MASSIV"  ! ИМЯ МАССИВА
CHK="KIND"    ! ПАРАМЕТР РАЗНОВИДНОСТИ ТИПА
              ! ОБЪЕМ ВЫДЕЛЕННОЙ ПАМЯТИ В
CH1(1)="BYTE" !     БАЙТАХ
CH1(2)="  KB" ! КИЛОБАЙТАХ
CH1(3)="  MB" ! МЕГАБАЙТАХ
CH1(4)="  GB" ! ГИГАБАЙТАХ
CH1(5)="  TB" ! ТЕРАБАЙТАХ

WRITE (3,'(/1X,A,6(2X,A))') CH0, CHK, (CH1(LPREF),LPREF=NPREF,1,-1)

DO LMSV=1,NMSV ! ЦИКЛ ПО ВСЕМ РАЗМЕЩЕННЫМ МАССИВАМ

   ! ИМЯ МАССИВА
   CH0=MEMNAM(LMSV)

   ! ПАРАМЕТР РАЗНОВИДНОСТИ ТИПА
   WRITE(CHK,'(I4)') MEMKND(LMSV)

   ! ПЕЧАТЬ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ В БАЙТАХ В СТРОКУ
   IF ( MEMMSV(1,LMSV) > 0 ) THEN
      WRITE(CH1(1),'(I4)') MEMMSV(1,LMSV)
   ELSE
      CH1(1)=CH ! ПУСТАЯ СТРОКА
   END IF

    ! ПЕЧАТЬ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ В КИЛОБАЙТАХ В СТРОКУ
    IF ( MEMMSV(2,LMSV) > 0 ) THEN
      WRITE(CH1(2),'(I4)') MEMMSV(2,LMSV)
   ELSE
      CH1(2)=CH ! ПУСТАЯ СТРОКА
   END IF

   ! ПЕЧАТЬ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ В МЕГАБАЙТАХ В СТРОКУ
   IF ( MEMMSV(3,LMSV) > 0 ) THEN
      WRITE(CH1(3),'(I4)') MEMMSV(3,LMSV)
   ELSE
      CH1(3)=CH ! ПУСТАЯ СТРОКА
   END IF

   ! ПЕЧАТЬ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ В ГИГАБАЙТАХ В СТРОКУ
   IF ( MEMMSV(4,LMSV) > 0 ) THEN
      WRITE(CH1(4),'(I4)') MEMMSV(4,LMSV)
   ELSE
      CH1(4)=CH ! ПУСТАЯ СТРОКА
   END IF

   ! ПЕЧАТЬ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ В ТЕРАБАЙТАХ В СТРОКУ
   IF ( MEMMSV(5,LMSV) > 0 ) THEN
      WRITE(CH1(5),'(I4)') MEMMSV(5,LMSV)
   ELSE
      CH1(5)=CH ! ПУСТАЯ СТРОКА
   END IF

   WRITE (3,'(1X,A,6(2X,A))') CH0, CHK, (CH1(LPREF),LPREF=NPREF,1,-1) ! ПЕЧАТЬ СФОРМИРОВАННОЙ СТРОКИ

END DO

! ФОРМИРОВАНИЕ ИТОГОВЫХ СТРОК
WRITE (3,'(1X)')

DO L4=1,4 ! ЦИКЛ ДЛЯ ВСЕХ СТРОК МАССИВА MEMALL

   IF ( SUM(MEMALL(:,L4)) == 0 ) CYCLE ! ВЫДЕЛЕНИЕ ПАМЯТИ ДАННОГО ТИПА ОТСУТСТВУЕТ

   SELECT CASE (L4)
      CASE (1)
         CH0="INTEGER"
      CASE (2)
         CH0="REAL"
      CASE (3)
         CH0="COMPLEX"
      CASE (4)
         CH0="ALL"
   END SELECT

   ! ПАРАМЕТР РАЗНОВИДНОСТИ ТИПА
   CHK=CH ! ПУСТАЯ СТРОКА

   ! ПЕЧАТЬ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ В БАЙТАХ В СТРОКУ
   IF ( MEMALL(1,L4) > 0 ) THEN
      WRITE(CH1(1),'(I4)') MEMALL(1,L4)
   ELSE
      CH1(1)=CH ! ПУСТАЯ СТРОКА
   END IF

   ! ПЕЧАТЬ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ В КИЛОБАЙТАХ В СТРОКУ
   IF ( MEMALL(2,L4) > 0 ) THEN
      WRITE(CH1(2),'(I4)') MEMALL(2,L4)
   ELSE
      CH1(2)=CH ! ПУСТАЯ СТРОКА
   END IF

   ! ПЕЧАТЬ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ В МЕГАБАЙТАХ В СТРОКУ
   IF ( MEMALL(3,L4) > 0 ) THEN
      WRITE(CH1(3),'(I4)') MEMALL(3,L4)
   ELSE
      CH1(3)=CH ! ПУСТАЯ СТРОКА
   END IF

   ! ПЕЧАТЬ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ В ГИГАБАЙТАХ В СТРОКУ
   IF ( MEMALL(4,L4) > 0 ) THEN
      WRITE(CH1(4),'(I4)') MEMALL(4,L4)
   ELSE
      CH1(4)=CH ! ПУСТАЯ СТРОКА
   END IF

   ! ПЕЧАТЬ ОБЪЕМА ВЫДЕЛЕННОЙ ПАМЯТИ В ТЕРАБАЙТАХ В СТРОКУ
   IF ( MEMALL(5,L4) > 0 ) THEN
      WRITE(CH1(5),'(I4)') MEMALL(5,L4)
   ELSE
      CH1(5)=CH ! ПУСТАЯ СТРОКА
   END IF

   WRITE (3,'(1X,A,6(2X,A))') CH0, CHK, (CH1(LPREF),LPREF=NPREF,1,-1) ! ПЕЧАТЬ СФОРМИРОВАННОЙ СТРОКИ

END DO

END SUBROUTINE WMEM2

!**********************************************************************
!**********************************************************************

SUBROUTINE DAMSV(CHNAME, & ! СТРОКА - ИМЯ МАССИВА
                 IMSV1,  & ! ОДНОМЕРНЫЙ МАССИВ ТИПА INTEGER
                 IMSV2,  & !  ДВУМЕРНЫЙ МАССИВ ТИПА INTEGER
                 IMSV3,  & ! ТРЁХМЕРНЫЙ МАССИВ ТИПА INTEGER
                 AMSV1,  & ! ОДНОМЕРНЫЙ МАССИВ ТИПА REAL
                 AMSV2,  & !  ДВУМЕРНЫЙ МАССИВ ТИПА REAL
                 AMSV3,  & ! ТРЁХМЕРНЫЙ МАССИВ ТИПА REAL
                 ZMSV1,  & ! ОДНОМЕРНЫЙ МАССИВ ТИПА COMPLEX
                 ZMSV2,  & !  ДВУМЕРНЫЙ МАССИВ ТИПА COMPLEX
                 ZMSV3)    ! ТРЁХМЕРНЫЙ МАССИВ ТИПА COMPLEX

! PUBLIC
! ОСВОБОЖДЕНИИ ПАМЯТИ МАССИВА

CHARACTER (LEN=*),    INTENT (IN)              :: CHNAME       ! СТРОКА - ИМЯ МАССИВА
INTEGER, ALLOCATABLE, INTENT (INOUT), OPTIONAL :: IMSV1(:)     ! ОДНОМЕРНЫЙ МАССИВ ТИПА INTEGER
INTEGER, ALLOCATABLE, INTENT (INOUT), OPTIONAL :: IMSV2(:,:)   !  ДВУМЕРНЫЙ МАССИВ ТИПА INTEGER
INTEGER, ALLOCATABLE, INTENT (INOUT), OPTIONAL :: IMSV3(:,:,:) ! ТРЁХМЕРНЫЙ МАССИВ ТИПА INTEGER
REAL,    ALLOCATABLE, INTENT (INOUT), OPTIONAL :: AMSV1(:)     ! ОДНОМЕРНЫЙ МАССИВ ТИПА REAL
REAL,    ALLOCATABLE, INTENT (INOUT), OPTIONAL :: AMSV2(:,:)   !  ДВУМЕРНЫЙ МАССИВ ТИПА REAL
REAL,    ALLOCATABLE, INTENT (INOUT), OPTIONAL :: AMSV3(:,:,:) ! ТРЁХМЕРНЫЙ МАССИВ ТИПА REAL
COMPLEX, ALLOCATABLE, INTENT (INOUT), OPTIONAL :: ZMSV1(:)     ! ОДНОМЕРНЫЙ МАССИВ ТИПА COMPLEX
COMPLEX, ALLOCATABLE, INTENT (INOUT), OPTIONAL :: ZMSV2(:,:)   !  ДВУМЕРНЫЙ МАССИВ ТИПА COMPLEX
COMPLEX, ALLOCATABLE, INTENT (INOUT), OPTIONAL :: ZMSV3(:,:,:) ! ТРЁХМЕРНЫЙ МАССИВ ТИПА COMPLEX

CHARACTER (LEN=*),    PARAMETER                :: CHSUB ="DAMSV"
CHARACTER (LEN=*),    PARAMETER                :: CHERR1=CHERR0//CHSUB//" МОДУЛЯ "//CHMDL

IF ( PRESENT(IMSV1) ) THEN ! ОДНОМЕРНЫЙ МАССИВ ТИПА INTEGER
   IF ( ALLOCATED(IMSV1) ) THEN
      DEALLOCATE (IMSV1, STAT=IER)
      IF ( IER /= 0 ) THEN
         WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2D, CHNAME, " - IER=", IER, CHERR3
         STOP
      END IF
      RETURN
   ELSE
      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ
         WRITE (3,'(/A)') " **** МАССИВ "//CHNAME//" НЕ РАЗМЕЩЕН - ОСВОБОЖДЕНИЕ ПАМЯТИ НЕ ПРОИЗВОДИТСЯ"
      END IF
   END IF
END IF

IF ( PRESENT(IMSV2) ) THEN ! ДВУМЕРНЫЙ МАССИВ ТИПА INTEGER
   IF ( ALLOCATED(IMSV2) ) THEN
      DEALLOCATE (IMSV2, STAT=IER)
      IF ( IER /= 0 ) THEN
         WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2D, CHNAME, " - IER=", IER, CHERR3
         STOP
      END IF
      RETURN
   ELSE
      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ
         WRITE (3,'(/A)') " **** МАССИВ "//CHNAME//" НЕ РАЗМЕЩЕН - ОСВОБОЖДЕНИЕ ПАМЯТИ НЕ ПРОИЗВОДИТСЯ"
      END IF
   END IF
END IF

IF ( PRESENT(IMSV3) ) THEN ! ТРЁХМЕРНЫЙ МАССИВ ТИПА INTEGER
   IF ( ALLOCATED(IMSV3) ) THEN
      DEALLOCATE (IMSV3, STAT=IER)
      IF ( IER /= 0 ) THEN
         WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2D, CHNAME, " - IER=", IER, CHERR3
         STOP
      END IF
      RETURN
   ELSE
      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ
         WRITE (3,'(/A)') " **** МАССИВ "//CHNAME//" НЕ РАЗМЕЩЕН - ОСВОБОЖДЕНИЕ ПАМЯТИ НЕ ПРОИЗВОДИТСЯ"
      END IF
   END IF
END IF

IF ( PRESENT(AMSV1) ) THEN ! ОДНОМЕРНЫЙ МАССИВ ТИПА REAL
   IF ( ALLOCATED(AMSV1) ) THEN
      DEALLOCATE (AMSV1, STAT=IER)
      IF ( IER /= 0 ) THEN
         WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2D, CHNAME, " - IER=", IER, CHERR3
         STOP
      END IF
      RETURN
   ELSE
      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ
         WRITE (3,'(/A)') " **** МАССИВ "//CHNAME//" НЕ РАЗМЕЩЕН - ОСВОБОЖДЕНИЕ ПАМЯТИ НЕ ПРОИЗВОДИТСЯ"
      END IF
   END IF
END IF

IF ( PRESENT(AMSV2) ) THEN ! ДВУМЕРНЫЙ МАССИВ ТИПА REAL
   IF ( ALLOCATED(AMSV2) ) THEN
      DEALLOCATE (AMSV2, STAT=IER)
      IF ( IER /= 0 ) THEN
         WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2D, CHNAME, " - IER=", IER, CHERR3
         STOP
      END IF
      RETURN
   ELSE
      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ
         WRITE (3,'(/A)') " **** МАССИВ "//CHNAME//" НЕ РАЗМЕЩЕН - ОСВОБОЖДЕНИЕ ПАМЯТИ НЕ ПРОИЗВОДИТСЯ"
      END IF
   END IF
END IF

IF ( PRESENT(AMSV3) ) THEN ! ТРЁХМЕРНЫЙ МАССИВ ТИПА REAL
   IF ( ALLOCATED(AMSV3) ) THEN
      DEALLOCATE (AMSV3, STAT=IER)
      IF ( IER /= 0 ) THEN
         WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2D, CHNAME, " - IER=", IER, CHERR3
         STOP
      END IF
      RETURN
   ELSE
      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ
         WRITE (3,'(/A)') " **** МАССИВ "//CHNAME//" НЕ РАЗМЕЩЕН - ОСВОБОЖДЕНИЕ ПАМЯТИ НЕ ПРОИЗВОДИТСЯ"
      END IF
   END IF
END IF

IF ( PRESENT(ZMSV1) ) THEN ! ОДНОМЕРНЫЙ МАССИВ ТИПА COMPLEX
   IF ( ALLOCATED(ZMSV1) ) THEN
      DEALLOCATE (ZMSV1, STAT=IER)
      IF ( IER /= 0 ) THEN
         WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2D, CHNAME, " - IER=", IER, CHERR3
         STOP
      END IF
      RETURN
   ELSE
      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ
         WRITE (3,'(/A)') " **** МАССИВ "//CHNAME//" НЕ РАЗМЕЩЕН - ОСВОБОЖДЕНИЕ ПАМЯТИ НЕ ПРОИЗВОДИТСЯ"
      END IF
   END IF
END IF

IF ( PRESENT(ZMSV2) ) THEN ! ДВУМЕРНЫЙ МАССИВ ТИПА COMPLEX
   IF ( ALLOCATED(ZMSV2) ) THEN
      DEALLOCATE (ZMSV2, STAT=IER)
      IF ( IER /= 0 ) THEN
         WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2D, CHNAME, " - IER=", IER, CHERR3
         STOP
      END IF
      RETURN
   ELSE
      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ
         WRITE (3,'(/A)') " **** МАССИВ "//CHNAME//" НЕ РАЗМЕЩЕН - ОСВОБОЖДЕНИЕ ПАМЯТИ НЕ ПРОИЗВОДИТСЯ"
      END IF
   END IF
END IF

IF ( PRESENT(ZMSV3) ) THEN ! ТРЁХМЕРНЫЙ МАССИВ ТИПА COMPLEX
   IF ( ALLOCATED(ZMSV3) ) THEN
      DEALLOCATE (ZMSV3, STAT=IER)
      IF ( IER /= 0 ) THEN
         WRITE (3,'(/A/3A,I0/A)') CHERR1, CHERR2D, CHNAME, " - IER=", IER, CHERR3
         STOP
      END IF
      RETURN
   ELSE
      IF ( IWMEM > 0 ) THEN ! ПЕЧАТЬ СООБЩЕНИЯ
         WRITE (3,'(/A)') " **** МАССИВ "//CHNAME//" НЕ РАЗМЕЩЕН - ОСВОБОЖДЕНИЕ ПАМЯТИ НЕ ПРОИЗВОДИТСЯ"
      END IF
   END IF
END IF

END SUBROUTINE DAMSV

!**********************************************************************
!**********************************************************************

SUBROUTINE AMINT0(CHNAME, & ! СТРОКА - ИМЯ МАССИВА
                  IMSV,   & ! РАЗМЕЩАЕМЫЙ МАССИВ ТИПА INTEGER
                  ISIZE1, & ! РАЗМЕР-1 МАССИВА
                  ISIZE2, & ! РАЗМЕР-2 МАССИВА
                  ISIZE3)   ! РАЗМЕР-3 МАССИВА

! PUBLIC
! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО МАССИВА ТИПА INTEGER

!IMPLICIT NONE

CHARACTER (LEN=*)                    :: CHNAME  ! СТРОКА - ИМЯ МАССИВА

INTEGER, ALLOCATABLE, INTENT (INOUT) :: IMSV(:) ! РАЗМЕЩАЕМЫЙ МАССИВ ТИПА INTEGER


INTEGER, INTENT (IN)                 :: ISIZE1  ! РАЗМЕР-1 МАССИВА
INTEGER, INTENT (IN), OPTIONAL       :: ISIZE2  ! РАЗМЕР-2 МАССИВА
INTEGER, INTENT (IN), OPTIONAL       :: ISIZE3  ! РАЗМЕР-3 МАССИВА

! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
!INTEGER                              :: ISIZE   ! ОБЩИЙ РАЗМЕР МАССИВА
!INTEGER                              :: IER     ! КОД ОШИБКИ ПРИ РАЗМЕЩЕНИИ МАССИВА

! ПРОВЕРКА ПАРАМЕТРОВ
IF ( ISIZE1 < 1 ) THEN
   WRITE (3,'(/A/A,I0,A/A)') " *** BHИMAHИE. PAБOTAET ПОДПРОГРАММА AMINT MOДУЛЯ MEM.",      &
   " ОБНАРУЖЕНА ОШИБКА. РАЗМЕР-1 МАССИВА - ISIZE1=", ISIZE1, " ДОЛЖЕН БЫТЬ ПОЛОЖИТЕЛЬНЫМ",  &
   " BЫПOЛHEHИE ПPOГPAMMЫ ПPEPBAHO."
   STOP
ELSE
   !ISIZE=ISIZE1 ! ОБЩИЙ РАЗМЕР МАССИВА
END IF

IF (PRESENT(ISIZE2)) THEN ! РАЗМЕР-2 УКАЗАН
   IF ( ISIZE2 < 1 ) THEN
      WRITE (3,'(/A/A,I0,A/A)') " *** BHИMAHИE. PAБOTAET ПОДПРОГРАММА AMINT MOДУЛЯ MEM.",      &
      " ОБНАРУЖЕНА ОШИБКА. РАЗМЕР-2 МАССИВА - ISIZE2=", ISIZE2, " ДОЛЖЕН БЫТЬ ПОЛОЖИТЕЛЬНЫМ",  &
      " BЫПOЛHEHИE ПPOГPAMMЫ ПPEPBAHO."
      STOP
   END IF
ELSE
   !ISIZE=ISIZE*ISIZE2 ! ОБЩИЙ РАЗМЕР МАССИВА
END IF

IF (PRESENT(ISIZE3)) THEN ! РАЗМЕР-3 УКАЗАН
   IF ( ISIZE3 < 1 ) THEN
      WRITE (3,'(/A/A,I0,A/A)') " *** BHИMAHИE. PAБOTAET ПОДПРОГРАММА AMINT MOДУЛЯ MEM.",      &
      " ОБНАРУЖЕНА ОШИБКА. РАЗМЕР-3 МАССИВА - ISIZE3=", ISIZE3, " ДОЛЖЕН БЫТЬ ПОЛОЖИТЕЛЬНЫМ",  &
      " BЫПOЛHEHИE ПPOГPAMMЫ ПPEPBAHO."
      STOP
   END IF
ELSE
   !ISIZE=ISIZE*ISIZE3 ! ОБЩИЙ РАЗМЕР МАССИВА
END IF

! РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ
IF (PRESENT(ISIZE3)) THEN ! ТРЕХМЕРНЫЙ РАЗМЕЩАЕМЫЙ МАССИВ

   !ALLOCATE (IMSV(ISIZE1,ISIZE2,ISIZE3), STAT=IER) ! ТРЕХМЕРНЫЙ РАЗМЕЩАЕМЫЙ МАССИВ ТИПА INTEGER

ELSE IF (PRESENT(ISIZE2)) THEN ! ДВУМЕРНЫЙ РАЗМЕЩАЕМЫЙ МАССИВ

   !ALLOCATE (IMSV(ISIZE1,ISIZE2), STAT=IER) ! ДВУМЕРНЫЙ РАЗМЕЩАЕМЫЙ МАССИВ ТИПА INTEGER

ELSE ! ОДНОМЕРНЫЙ РАЗМЕЩАЕМЫЙ

   ALLOCATE (IMSV(ISIZE1), STAT=IER) ! ОДНОМЕРНЫЙ РАЗМЕЩАЕМЫЙ МАССИВ ТИПА INTEGER

END IF

IF ( IER /= 0 ) THEN
   WRITE (3,'(/A/A,A,A,I0/A)') " *** BHИMAHИE. PAБOTAET ПОДПРОГРАММА AMINT MOДУЛЯ MEM.",   &
   " ОШИБКА ПРИ РАЗМЕЩЕНИИ МАССИВА ", CHNAME, " - IER=", IER,                              &
   " BЫПOЛHEHИE ПPOГPAMMЫ ПPEPBAHO."
   STOP
END IF

! ПЕЧАТЬ "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)" В СТРОКУ
IF (PRESENT(ISIZE3)) THEN ! ТРЕХМЕРНЫЙ РАЗМЕЩАЕМЫЙ МАССИВ
   WRITE(CHMSV,'(2A,I0,A,I0,A,I0,A)') CHNAME, "(", ISIZE1, ":", ISIZE2, ":", ISIZE3, ")" ! ПЕЧАТЬ "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)" В СТРОКУ
ELSE IF (PRESENT(ISIZE2)) THEN ! ДВУМЕРНЫЙ РАЗМЕЩАЕМЫЙ МАССИВ
   WRITE(CHMSV,'(2A,I0,A,I0,A)') CHNAME, "(", ISIZE1, ":", ISIZE2, ")" ! ПЕЧАТЬ "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)" В СТРОКУ
ELSE ! ОДНОМЕРНЫЙ РАЗМЕЩАЕМЫЙ
   WRITE(CHMSV,'(2A,I0,A)') CHNAME, "(", ISIZE1, ")"
END IF

! ВНЕСЕНИЕ ИНФОРМАЦИИ О РАЗМЕЩЕНИИ МАССИВА В БД
!CALL CMEM1 (ISIZE,      & ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА
!            KIND(IMSV), & ! РАЗМЕР ЭЛЕМЕНТА МАССИВА В Byte
!            1,          & ! ТИП ЭЛЕМЕНТОВ МАССИВА - INTEGER
!            CHNAME)       ! СТРОКА - ИМЯ МАССИВА

RETURN
END SUBROUTINE AMINT0

!**********************************************************************

SUBROUTINE AMFLT0(CHNAME, & ! СТРОКА - ИМЯ МАССИВА
                  AMSV,   & ! РАЗМЕЩАЕМЫЙ ОДНОМЕРНЫЙ МАССИВ ТИПА REAL
                  ISIZE1, & ! РАЗМЕР-1 МАССИВА
                  ISIZE2, & ! РАЗМЕР-2 МАССИВА
                  ISIZE3)   ! РАЗМЕР-3 МАССИВА

! PUBLIC
! ВЫДЕЛЕНИЕ ПАМЯТИ ДЛЯ РАЗМЕЩАЕМОГО МАССИВА ТИПА REAL

!IMPLICIT NONE

CHARACTER (LEN=*)                    :: CHNAME  ! СТРОКА - ИМЯ МАССИВА

INTEGER, ALLOCATABLE, INTENT (INOUT) :: AMSV(:) ! РАЗМЕЩАЕМЫЙ МАССИВ ТИПА REAL

CHARACTER (LEN=NSLEN)                :: CHMSV   ! СТРОКА - "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)"

INTEGER, INTENT (IN)                 :: ISIZE1  ! РАЗМЕР-1 МАССИВА
INTEGER, INTENT (IN), OPTIONAL       :: ISIZE2  ! РАЗМЕР-2 МАССИВА
INTEGER, INTENT (IN), OPTIONAL       :: ISIZE3  ! РАЗМЕР-3 МАССИВА

! ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ
!INTEGER                              :: ISIZE   ! ОБЩИЙ РАЗМЕР МАССИВА
!INTEGER                              :: IER     ! КОД ОШИБКИ ПРИ РАЗМЕЩЕНИИ МАССИВА

! ПРОВЕРКА ПАРАМЕТРОВ
IF ( ISIZE1 < 1 ) THEN
   WRITE (3,'(/A/A,I0,A/A)') " *** BHИMAHИE. PAБOTAET ПОДПРОГРАММА AMFLT MOДУЛЯ MEM.",      &
   " ОБНАРУЖЕНА ОШИБКА. РАЗМЕР-1 МАССИВА - ISIZE1=", ISIZE1, " ДОЛЖЕН БЫТЬ ПОЛОЖИТЕЛЬНЫМ",  &
   " BЫПOЛHEHИE ПPOГPAMMЫ ПPEPBAHO."
   STOP
ELSE
   !ISIZE=ISIZE1 ! ОБЩИЙ РАЗМЕР МАССИВА
END IF

IF (PRESENT(ISIZE2)) THEN ! РАЗМЕР-2 УКАЗАН
   IF ( ISIZE2 < 1 ) THEN
      WRITE (3,'(/A/A,I0,A/A)') " *** BHИMAHИE. PAБOTAET ПОДПРОГРАММА AMFLT MOДУЛЯ MEM.",      &
      " ОБНАРУЖЕНА ОШИБКА. РАЗМЕР-2 МАССИВА - ISIZE2=", ISIZE2, " ДОЛЖЕН БЫТЬ ПОЛОЖИТЕЛЬНЫМ",  &
      " BЫПOЛHEHИE ПPOГPAMMЫ ПPEPBAHO."
      STOP
   END IF
ELSE
   !ISIZE=ISIZE*ISIZE2 ! ОБЩИЙ РАЗМЕР МАССИВА
END IF

IF (PRESENT(ISIZE3)) THEN ! РАЗМЕР-3 УКАЗАН
   IF ( ISIZE3 < 1 ) THEN
      WRITE (3,'(/A/A,I0,A/A)') " *** BHИMAHИE. PAБOTAET ПОДПРОГРАММА AMFLT MOДУЛЯ MEM.",      &
      " ОБНАРУЖЕНА ОШИБКА. РАЗМЕР-3 МАССИВА - ISIZE3=", ISIZE3, " ДОЛЖЕН БЫТЬ ПОЛОЖИТЕЛЬНЫМ",  &
      " BЫПOЛHEHИE ПPOГPAMMЫ ПPEPBAHO."
      STOP
   END IF
ELSE
   !ISIZE=ISIZE*ISIZE3 ! ОБЩИЙ РАЗМЕР МАССИВА
END IF

! РАЗМЕЩЕНИЕ МАССИВА В ПАМЯТИ
IF (PRESENT(ISIZE3)) THEN ! ТРЕХМЕРНЫЙ РАЗМЕЩАЕМЫЙ МАССИВ

   !ALLOCATE (AMSV(ISIZE1,ISIZE2,ISIZE3), STAT=IER) ! ТРЕХМЕРНЫЙ РАЗМЕЩАЕМЫЙ МАССИВ ТИПА REAL

ELSE IF (PRESENT(ISIZE2)) THEN ! ДВУМЕРНЫЙ РАЗМЕЩАЕМЫЙ МАССИВ

   !ALLOCATE (AMSV(ISIZE1,ISIZE2), STAT=IER) ! ДВУМЕРНЫЙ РАЗМЕЩАЕМЫЙ МАССИВ ТИПА REAL

ELSE ! ОДНОМЕРНЫЙ РАЗМЕЩАЕМЫЙ

   ALLOCATE (AMSV(ISIZE1), STAT=IER) ! ОДНОМЕРНЫЙ РАЗМЕЩАЕМЫЙ МАССИВ ТИПА REAL

END IF

IF ( IER /= 0 ) THEN
   WRITE (3,'(/A/A,A,A,I0/A)') " *** BHИMAHИE. PAБOTAET ПОДПРОГРАММА AMFLT MOДУЛЯ MEM.",   &
   " ОШИБКА ПРИ РАЗМЕЩЕНИИ МАССИВА ", CHNAME, " - IER=", IER,                              &
   " BЫПOЛHEHИE ПPOГPAMMЫ ПPEPBAHO."
   STOP
END IF

! ПЕЧАТЬ "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)" В СТРОКУ
IF (PRESENT(ISIZE3)) THEN ! ТРЕХМЕРНЫЙ РАЗМЕЩАЕМЫЙ МАССИВ
   WRITE(CHMSV,'(2A,I0,A,I0,A,I0,A)') CHNAME, "(", ISIZE1, ":", ISIZE2, ":", ISIZE3, ")" ! ПЕЧАТЬ "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)" В СТРОКУ
ELSE IF (PRESENT(ISIZE2)) THEN ! ДВУМЕРНЫЙ РАЗМЕЩАЕМЫЙ МАССИВ
   WRITE(CHMSV,'(2A,I0,A,I0,A)') CHNAME, "(", ISIZE1, ":", ISIZE2, ")" ! ПЕЧАТЬ "ИМЯ_РАЗМЕЩАЕМОГО_МАССИВА(РАЗМЕРЫ)" В СТРОКУ
ELSE ! ОДНОМЕРНЫЙ РАЗМЕЩАЕМЫЙ
   WRITE(CHMSV,'(2A,I0,A)') CHNAME, "(", ISIZE1, ")"
END IF

! ВНЕСЕНИЕ ИНФОРМАЦИИ О РАЗМЕЩЕНИИ МАССИВА В БД
!CALL CMEM1 (ISIZE,      & ! КОЛИЧЕСТВО ЭЛЕМЕНТОВ МАССИВА
!            KIND(AMSV), & ! РАЗМЕР ЭЛЕМЕНТА МАССИВА В Byte
!            1,          & ! ТИП ЭЛЕМЕНТОВ МАССИВА - INTEGER
!            CHNAME)       ! СТРОКА - ИМЯ МАССИВА

RETURN
END SUBROUTINE AMFLT0

!**********************************************************************

END MODULE MEM

