C SMGSUBS.FOR
C
C V01 11-DEC-00 CS  INITIAL RELEASE FOR PORTUGAL
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C**********************************************
C SUBROUTINE SMG_INPNUM
C**********************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SMG_INPNUM(IPB,IKEYB,IDISP2,IDISP3,
     *			       OUTVAR,ILIN,ICOL,ISIZE,IMIN,IMAX,IST)
	IMPLICIT NONE

	INCLUDE '($SMGDEF)'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
C EXTERNAL VARIABLES
C
	INTEGER		IPB, IKEYB, IDISP2, IDISP3
	INTEGER*4	OUTVAR, ILIN, ICOL, ISIZE, IMIN, IMAX, IST
C 
C LOCAL VARIABLES
C
	CHARACTER*10	CIDX
	CHARACTER*76	CMSG

	INTEGER		SMG$ERASE_DISPLAY, SMG$PUT_CHARS, SMG$RING_BELL

	INTEGER*4	ICIDXLEN, INEXT
	INTEGER*4	AUXOUTVAR

	LOGICAL		REPEAT
C
C
C BEGIN OF CODE
C
	REPEAT = .TRUE.

	DO WHILE(REPEAT)
	    INEXT = 2
	    IF(OUTVAR.EQ.0) THEN
		CIDX = '          '
	    ELSE  	    
		WRITE(CIDX,100) OUTVAR
	    ENDIF

	    CALL INPUT(IPB, IKEYB, IDISP2, ILIN, ICOL, ISIZE, 1, 'N', 
     *		       CIDX, ICIDXLEN, INEXT)

	    DECODE(ICIDXLEN,100,CIDX) AUXOUTVAR

	    IF(AUXOUTVAR.LT.IMIN .OR. AUXOUTVAR.GT.IMAX) THEN
		WRITE (CMSG,200) IMIN, IMAX
		IST = SMG$PUT_CHARS (IDISP3, CMSG, 1,5,
     *                               SMG$M_ERASE_LINE, SMG$M_BOLD)
		IST = SMG$RING_BELL(IDISP3, 2)
	    ELSE
		REPEAT = .FALSE.
		IST = SMG$ERASE_DISPLAY(IDISP3)
		OUTVAR = AUXOUTVAR

		IF (INEXT.EQ.-1) THEN
                   IST = -2
		ELSE
		   IST = 1
		ENDIF
	    ENDIF
	ENDDO

	RETURN
100	FORMAT(I<ISIZE>)
200	FORMAT('VALOR INCORRETO, LIMITES SAO DE ',I10,' A ',I10)
	END

C**********************************************
C SUBROUTINE SMG_INPTEXT
C
C ICLR  --> 0 Inicialize COUTVAR with blanks
C       --> 1 Get default value
C
C IFLAG --> 0 Accept blanks
C       --> 1 Do not accept blanks
C
C**********************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SMG_INPTEXT(IPB, IKEYB, IDISP1, IDISP2,
     *			       COUTVAR, ICLR, ILIN, ICOL, ISIZE, IFLAG, IST)
	IMPLICIT NONE

	INCLUDE '($SMGDEF)'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
C EXTERNAL VARIABLES
C
	INTEGER		IPB, IKEYB, IDISP1, IDISP2
	INTEGER*4	IST, ILIN, ICOL, ISIZE, IFLAG, ICLR

	CHARACTER	COUTVAR*(*)
C
C LOCAL VARIABLES
C
	INTEGER		STR$Trim,	  SMG$ERASE_DISPLAY,	      
     *			SMG$PUT_CHARS,  SMG$RING_BELL

	CHARACTER*76	CMSG

        CHARACTER*100	CAUXVAR

	INTEGER*4	ICIDXLEN, ISTATUS, ICNT, IIND

	LOGICAL		REPEAT
C
C CODE
C
	REPEAT = .TRUE.
	DO WHILE(REPEAT)

           ! If not to clear, copy default value
           IF (ICLR) THEN
              ISTATUS = STR$Trim (CAUXVAR, COUTVAR(1:ISIZE), ICNT)
           ELSE
              DO IIND = 1, 100
                 CAUXVAR(IIND:IIND) = ' '
              ENDDO
           ENDIF

	   ISTATUS = 2
	   CALL INPUT(IPB, IKEYB, IDISP1, ILIN, ICOL, ISIZE, 0, 'A',
     *		      CAUXVAR, ICIDXLEN, ISTATUS)

	   IF (ICIDXLEN .EQ. 0 .AND. IFLAG .EQ. 1) THEN
              CMSG = 'CAMPO COM DIGITACAO OBRIGATORIA.'
     *             //' DIGITE CAMPO OU <NEXT> PARA SAIR'

              ISTATUS = SMG$PUT_CHARS (IDISP2, CMSG, 1, 5,
     *                                 SMG$M_ERASE_LINE, SMG$M_BOLD)
	      ISTATUS = SMG$RING_BELL(IDISP2, 2)
	   ELSE
	      REPEAT  = .FALSE.
              IST = STR$Trim (COUTVAR, CAUXVAR(1:ISIZE), ICNT)
	      IST = SMG$ERASE_DISPLAY(IDISP2)

	      IF (ISTATUS .EQ. -1) THEN
		 IST = -2		!<Next> key pressioned
	      ELSE
		 IST = 1
	      ENDIF
	   ENDIF
	ENDDO

	RETURN
	END

C**********************************************
C SUBROUTINE SMG_INPSMONY
C
C IMAX(INTEGER*4) = 21.474.836,47
C
C**********************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SMG_INPSMONY(IPB, IKEYB, IDISP1, IDISP2, INVAL,
     *			        ILIN, ICOL, ISIZE, IMIN, IMAX, IST)
	IMPLICIT NONE

	INCLUDE '($SMGDEF)'
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INTEGER SMG$ERASE_DISPLAY, SMG$PUT_CHARS,
     *		SMG$RING_BELL
C
C EXTERNAL VARIABLES
C
	INTEGER   IPB, IKEYB, IDISP1, IDISP2

	INTEGER*4 IST, INVAL, ILIN, ICOL, ISIZE, IMIN, IMAX, INEXT
C 
C LOCAL VARIABLES
C
	CHARACTER*13 CIDX   /'             '/,
     *               CAUX
	CHARACTER*76 CMSG

	INTEGER*4    ICIDXLEN, ISIZAUX, IPRCSIZ, IOUTVAL, ISTATUS, IIND

	LOGICAL	     LREPEAT


        ! Max = 10 bytes
        IF (ISIZE .GT. 10)    ISIZE = 10

	LREPEAT = .TRUE.
        DO WHILE (LREPEAT)

           IOUTVAL = INVAL

           ! # of characteres - CMONY function
           IPRCSIZ = ISIZE + (ISIZE/3)

           IF (IOUTVAL .EQ. 0) THEN
              DO IIND = 1, IPRCSIZ
                 CIDX(IIND:IIND) = ' '
              ENDDO
	   ELSE
              CIDX    = CMONY(IOUTVAL, IPRCSIZ, VALUNIT)
              ISTATUS = SMG$Put_Chars (IDISP1, CIDX, ILIN, ICOL,,
     *                                 SMG$M_REVERSE, SMG$M_BOLD)
	   ENDIF

	   INEXT = 2
	   CALL INPUT(IPB, IKEYB, IDISP1, ILIN, ICOL, IPRCSIZ, 0, 'M',
     *		      CIDX, ICIDXLEN, INEXT)

           DO IIND = 1, IPRCSIZ
              CAUX(IIND:IIND) = '0'
           ENDDO

	   ISIZAUX = IPRCSIZ
	   DO ICIDXLEN = ICIDXLEN, 1, -1
	      IF (CIDX(ICIDXLEN:ICIDXLEN).GE.'0' .AND. 
     *	         CIDX(ICIDXLEN:ICIDXLEN).LE.'9') THEN
	         CAUX(ISIZAUX:ISIZAUX) = CIDX(ICIDXLEN:ICIDXLEN)
                 ISIZAUX = ISIZAUX - 1
	      ENDIF
	   ENDDO

	   DECODE(IPRCSIZ,100,CAUX) IOUTVAL

           IF (IOUTVAL .LT. 0 .OR.
     *         IOUTVAL .LT. IMIN .OR. IOUTVAL .GT. IMAX) THEN

	      WRITE (CMSG,200) IMIN, IMAX
              ISTATUS = SMG$PUT_CHARS (IDISP2, CMSG, 1, 5,
     *                                 SMG$M_ERASE_LINE, SMG$M_BOLD)
	      ISTATUS = SMG$RING_BELL(IDISP2, 2)
           ELSE
              LREPEAT = .FALSE.
              INVAL   = IOUTVAL

              CIDX    = CMONY(IOUTVAL, IPRCSIZ, VALUNIT)
              ISTATUS = SMG$Put_Chars (IDISP1, CIDX, ILIN, ICOL,,
     *                                 SMG$M_REVERSE, SMG$M_BOLD)
              ISTATUS = SMG$ERASE_DISPLAY(IDISP2)

	      IF (INEXT .EQ. -1) THEN
	         IST  = -2
	      ELSE
                 IST  = 1
	      ENDIF
           ENDIF
        ENDDO

	RETURN
100	FORMAT(I<IPRCSIZ>)
200	FORMAT('VALOR INCORRETO, LIMITES DE ',
     *         I<IPRCSIZ>,' A ',I<IPRCSIZ>, '. <Next> PARA SAIR.')
	END

C**********************************************
C SUBROUTINE SMG_INPMONY
C**********************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SMG_INPMONY(IPB,IKEYB,IDISP2,IDISP3,
     *			       OUTVAR,ILIN,ICOL,ISIZE,IST)
	IMPLICIT NONE

	INCLUDE '($SMGDEF)'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C 
C LOCAL VARIABLES
C
	CHARACTER*12 CIDX,CAUX
	CHARACTER*76 CMSG

     	INTEGER	SMG$ERASE_DISPLAY,
     *		SMG$PUT_CHARS,
     *		SMG$RING_BELL

	INTEGER   IPB,IKEYB,IDISP2,IDISP3

	INTEGER*4 IST,ILIN,ICOL,ISIZE,ICIDXLEN,INEXT
	INTEGER*4 SIZAUX,PRCSIZ

	INTEGER*8 OUTVAR,AUXVAR

	INEXT = 2
	IF(OUTVAR.EQ.0) THEN
	    CIDX = '            '
	ELSE
	    WRITE(CIDX,100) OUTVAR
	ENDIF

	CALL INPUT(IPB, IKEYB, IDISP2, ILIN, ICOL, ISIZE, 0, 'M', 
     *		   CIDX, ICIDXLEN, INEXT)

	IF(INEXT.EQ.-1) THEN
	    IST = -2
	    RETURN
	ENDIF

	CAUX='0000000000'
	SIZAUX = ISIZE

	DO PRCSIZ = ISIZE, 1, -1
	    IF(CIDX(PRCSIZ:PRCSIZ).GE.'0' .AND. 
     *	       CIDX(PRCSIZ:PRCSIZ).LE.'9') THEN
		CAUX(SIZAUX:SIZAUX) = CIDX(PRCSIZ:PRCSIZ)
                SIZAUX = SIZAUX - 1
	    ENDIF
	ENDDO

	DECODE(ISIZE,100,CAUX) OUTVAR

	CMSG = ' DIGITE VALOR NOVAMENTE, POR FAVOR'
	IST = SMG$PUT_CHARS(IDISP3, CMSG, 1,1,SMG$M_ERASE_LINE)
	IST = SMG$RING_BELL(IDISP3, 2)

	INEXT = 2
	CIDX = '          '
	CALL INPUT(IPB,IKEYB, IDISP2, ILIN, ICOL, ISIZE, 0, 'M', 
     *			   CIDX, ICIDXLEN, INEXT)

        IST = SMG$ERASE_DISPLAY(IDISP3)

	IF(INEXT.EQ.-1) THEN
	    IST = -2
	    RETURN
	ENDIF

	CAUX='000000000000'
	SIZAUX = ISIZE
	DO PRCSIZ = ISIZE, 1, -1
	    IF(CIDX(PRCSIZ:PRCSIZ).GE.'0' .AND. 
     *	       CIDX(PRCSIZ:PRCSIZ).LE.'9') THEN
      	      CAUX(SIZAUX:SIZAUX) = CIDX(PRCSIZ:PRCSIZ)
      	      SIZAUX = SIZAUX - 1
	    ENDIF
	ENDDO

	DECODE(ISIZE,100,CAUX) AUXVAR

	IF(AUXVAR.NE.OUTVAR) THEN
	    IST = -1
	ELSE
	    IST = 0
	ENDIF

	RETURN
100	FORMAT(I<ISIZE>)
	END

C=============================================================================
C
C Subroutine INPUT()
C
C V01 12-MAR-97 GIL Released for CEF - Alpha
C
C Subroutine to read a typed string from keyboard and to store it in a variable
C
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C=======OPTIONS /CHECK=NOOVERFLOW

      subroutine INPUT (PB, KB, VD, LINE, COL_INI, TAM_MAX, FLAG, KIND,
     *			OUTSTRING, COUNT, NEXT_LINE)

      implicit NONE
      include '($SMGDEF)'

      integer     SMG$Put_Chars
      integer     SMG$Read_From_Display,	SMG$Set_Cursor_Abs
      integer     SMG$Set_Cursor_Mode
      integer     SMG$Read_Keystroke
      integer     SMG$Repaint_Screen
      integer     SMG$Set_Keypad_Mode,		STR$Trim

      integer       PB              ! PASTEBOARD
      integer       KB              ! VIRTUAL KEYBOARD
      integer       VD              ! VIRTUAL DISPLAY
      integer       LINE            ! LINE OF THE FIELD
      integer       COL_INI         ! INITIAL COLUMN OF THE FIELD
      integer       TAM_MAX         ! MAXIMUM SIZE OF STRING
      integer       FLAG            ! INDICATES FIELD ASPECT OF STRING INPUT:
                                    !       0 - STRING = ______
                                    !       1 - STRING = |_|_|_|_|
      character*1   KIND            ! STRING TYPE:
                                    !       'N' - NUMERIC STRING
                                    !       'A' - ALPHANUMERIC STRING
      character*(*) OUTSTRING       ! VARIABLE RECEIVING THE TYPED CHARACTERS
      integer       COUNT           ! TYPED CHARACTERS COUNTER
      integer       NEXT_LINE       ! LINE ATTITUDE (first/prev/stay/next/last)

C***************************************************************************

      character*80  INSTRING         ! internal variable of user typed chars
      character*1   CARACTERE  /' '/ ! user typed char
      integer       COL_BASE   /0/   ! column of string begining
      integer       COL        /0/   ! column position of cursor
      integer       ST         /0/   ! smg$ error code
      integer       NESQ       /0/   ! left hand of cursor char counter
      integer       KEY        /0/   ! ASCII code of typed key
      integer       STEP       /0/
      integer       I, J, K, L
      integer       IK, KK
      integer       IRET       /0/
      integer       LF         /0/
      integer       COMMA      /0/
      logical       VALID      /.false./
      logical       EDITA      /.true./
      logical       UPDOWN_KEY /.false./  ! enables up/down keys as return field
      logical       PRVNXT_KEY /.false./  ! enables also prev/next keys as return field

C*****************************************************************************
C2345#***  Checks if input() end with ENTER or reaching the end of field

      ST = SMG$Set_Keypad_Mode (KB, 0)         ! INIBIR QUANDO DEBUGAR!!!
      if (COUNT .eq. -1) then
         IRET = -1                   ! ends input at reaching the end of field
      else
         IRET = 0                    ! ends input w/ENTER
      end if
      COUNT = 0

C2345#***  Builds data input field: underlined (___) or spaced (|_|)
      do I = 1, TAM_MAX              ! initialize resulting string
         INSTRING(I:I) = ' '
      end do
      COMMA = 0
      ! receives previous string and removes trailing blanks
      ST = STR$Trim (INSTRING, OUTSTRING, COUNT) ! dest<-sour, c
      if (FLAG .eq. 1) then           ! format = spaced field
         COL_BASE = COL_INI + 1
         STEP = 2
         ST = SMG$Put_Chars (VD, '|', LINE, COL_INI,, SMG$M_REVERSE, SMG$M_BOLD)
         do I = 1, COUNT
            ST = SMG$Put_Chars (VD, INSTRING(I:I)//'|', LINE, COL_BASE+2*(I-1),
     *                          ,SMG$M_REVERSE, SMG$M_BOLD)
            if ((INSTRING(I:I) .eq. ',') .or. (INSTRING(I:I) .eq. '.')) COMMA = COMMA + 1
         end do
         if (COUNT .lt. TAM_MAX) then
            do I = COUNT+1, TAM_MAX
               ST = SMG$Put_Chars (VD, '_|', LINE, COL_BASE+2*(I-1),, SMG$M_REVERSE, SMG$M_BOLD)
            end do
         endif
         K = 0
      else                            ! format = underlined field
         COL_BASE = COL_INI
         STEP = 1
         do I = 1, COUNT
            ST = SMG$Put_Chars (VD, INSTRING(I:I), LINE, (COL_BASE - 1 + I),, SMG$M_REVERSE, SMG$M_BOLD)
            if ((INSTRING(I:I) .eq. ',') .or. (INSTRING(I:I) .eq. '.')) COMMA = COMMA + 1
         end do
         if (COUNT .lt. TAM_MAX) then
            do I = COUNT+1, TAM_MAX
               ST = SMG$Put_Chars (VD, '_', LINE, (COL_BASE - 1 + I),, SMG$M_REVERSE, SMG$M_BOLD)
            end do
         endif
      end if

      COL = COL_BASE + COUNT*STEP                ! position cursor at end of string
      NESQ = COUNT
      KEY = 0
      ST = SMG$Set_Cursor_Mode (PB, SMG$M_CURSOR_ON)
      if (NEXT_LINE .eq. 0) then
	UPDOWN_KEY = .false.
	PRVNXT_KEY = .false.
      elseif (NEXT_LINE .eq. 1) then
	UPDOWN_KEY = .true.
	PRVNXT_KEY = .false.
      elseif (NEXT_LINE .eq. 2) then
	UPDOWN_KEY = .true.
	PRVNXT_KEY = .true.
      endif

C2345#*****  TYPING STRING CHARS (while not return keys => accepts data) ******
      EDITA = .true.
      do while (EDITA)
         ST = SMG$Set_Cursor_Abs (VD, LINE, COL)
         ST = SMG$Read_Keystroke (KB, KEY, , VD)

C2345#*****  aborts edition  *******
         if (KEY .eq. 26) then    			! [CTRL+Z] aborts and gets off
            ST = STR$Trim (OUTSTRING, OUTSTRING, COUNT) ! keeps out string unchanged
            ST = SMG$Set_Cursor_Mode (PB, SMG$M_CURSOR_OFF)
	    EDITA = .false.
            return

C2345#*****  deletes one character  *******

         elseif ((KEY .eq. SMG$K_TRM_DELETE) .or. (KEY .eq. 311)) then       ! DEL key = #127
            if (COL .gt. COL_BASE) then                  ! count > 0
              if (KIND .eq. 'N') then
                 ST = SMG$Set_Cursor_Abs (VD, LINE, COL - STEP)
                 ST = SMG$Read_From_Display (VD, CARACTERE)
                 if (CARACTERE .eq. ',') COMMA = COMMA - 1
                 ST = SMG$Set_Cursor_Abs (VD, LINE, COL)
              end if
              J = COL_BASE + (TAM_MAX-1)*STEP           ! picks last field position
              do I = COL, J, STEP                       ! from curret to last position ...
                 ST = SMG$Set_Cursor_Abs (VD, LINE, I)
                 ST = SMG$Read_From_Display (VD, CARACTERE)
                 ST = SMG$Put_Chars (VD, CARACTERE, LINE, I-STEP,, SMG$M_REVERSE, SMG$M_BOLD)
              end do
              ST = SMG$Put_Chars (VD, '_', LINE, J, , SMG$M_REVERSE, SMG$M_BOLD)

C             do I = J,(COL_BASE+TAM_MAX-1)*STEP, STEP
C                 ST = SMG$Put_Chars (VD, '_', LINE, I, , SMG$M_REVERSE, SMG$M_BOLD)
C             end do

              COUNT = COUNT - 1
              NESQ = NESQ - 1
              COL = COL - STEP
              ST = SMG$Set_Cursor_Abs (VD, LINE, COL)
            end if

C2345#*****  clear all characters  *******

         elseif (KEY .eq. 313) then                            ! REMOVE key
            COL = COUNT*STEP + COL_BASE
            ST = SMG$Set_Cursor_Abs (VD, LINE, COL)
            do while (COL_BASE .lt. COL)
              COL = COL - STEP
              ST = SMG$Set_Cursor_Abs (VD, LINE, COL)
              ST = SMG$Put_Chars (VD, '_', LINE, COL, , SMG$M_REVERSE, SMG$M_BOLD)
            end do
	    COMMA = 0
            COUNT = 0
            NESQ = 0

C2345#*****  refresh screen  ******
         elseif ((KEY .eq. 23) .or. (KEY .eq. 312)) then     ! CTRL+W or INSERT key
            ST = SMG$Repaint_Screen (PB)
            ST = SMG$Set_Cursor_Abs (VD, LINE, COL)
         end if


C2345#*****  arrows allowed only in alphanumeric field  *******************

         if (KIND .eq. 'A') then

C2345#*****  moves cursor to the right  *******
           if (KEY .eq. 277) then               ! RIGHT ARROW key
              if (COL .lt. (COL_BASE + COUNT*STEP)) then
                 COL = COL + STEP
                 NESQ = NESQ + 1
              else                              ! if reach end, comes to begining
                 COL = COL_BASE
                 NESQ = 0
              end if
           end if

C2345#*****  moves cursor to the left  ******
           if (KEY .eq. 276) then               ! LEFT ARROW key
              if (COL .gt. COL_BASE) then
                 COL = COL - STEP
                 NESQ = NESQ - 1
              else                              ! if reach begining, goes to end
                 COL = COL_BASE + COUNT*STEP
                 NESQ = COUNT
              end if
           end if
        end if                                  ! end of cursor moving keys

C2345#*****  Checks if typed character is valid  ******

        if ((COUNT .lt. TAM_MAX) .or. (NESQ .lt. COUNT)) then

           ! *****  only numeric variables (unsigned integer) *****
           if (KIND .eq. 'N') then
              if ((KEY .ge. 48) .and. (KEY .le. 57)) then
                 VALID = .true.                         ! digits
              else
                 VALID = .false.                        ! invalid key
              end if

           ! *****  currency (cents, sign & comma) ******
           else if (KIND .eq. 'M') then                 ! numeric var only
              if ((KEY .ge. 48) .and. (KEY .le. 57)) then
                 VALID = .true.                         ! digits
              else if (((KEY .eq. 44) .or. (KEY .eq. 46)) .and. (COMMA .eq.  0)) then
                 KEY = 44                               ! only ','
                 COMMA = 1                              ! only one comma allowed
                 VALID = .true.
              else if ((KEY .eq. 45) .and. (COUNT .eq. 0)) then
                 VALID = .true.                         ! minus sign allowed at firs column
              else
                 VALID = .false.                        ! invalid key
              end if

           ! *****  alphanumeric variables  ******
           else if (KIND .eq. 'A') then
              if ((KEY .ge. 32) .and. (KEY .le. 126)) then
                 VALID = .true.                         ! characters (except control)
              else
                 VALID = .false.                        ! invalid key
              end if
           end if

C2345#*****  Writes typed character on screen  *******

           if (VALID) then
              CARACTERE = Char (KEY)
              ST = SMG$Set_Cursor_Abs (VD, LINE, COL)
              ST = SMG$Put_Chars (VD, CARACTERE, LINE, COL,, SMG$M_REVERSE, SMG$M_BOLD)
              COL = COL + STEP
              COUNT = COUNT + 1
              NESQ = NESQ + 1
              if (NESQ .lt. COUNT) then                 ! deals overwrite
                COUNT = COUNT - 1
              end if

c2345#*****  Checks if reached end of field and doesn't need ENTER to input

              if ((COUNT .eq. TAM_MAX) .and. (IRET .eq. -1)) KEY = 13                   ! implicit ENTER
           end if                               ! (VALID)
        end if                                  ! (COUNT < TAM_MAX)

	if (PRVNXT_KEY) then
          if (KEY .eq. 315) then  		! PREV key
	    NEXT_LINE = 1
	    EDITA = .false.
          end if
          if (KEY .eq. 316) then		! NEXT key
	    NEXT_LINE = -1
	    EDITA = .false.
          end if
	endif

	if (UPDOWN_KEY) then
          if (KEY .eq. 274) then  		! UP ARROW key
	    NEXT_LINE = LINE - 1
	    EDITA = .false.
          end if
          if (KEY .eq. 275) then		! DOWN ARROW key
	    NEXT_LINE = LINE + 1
	    EDITA = .false.
          end if
	endif

        if (KEY .eq. 13) then
	  NEXT_LINE = LINE + 1	! RETURN key
	  EDITA = .false.
	endif

      enddo		! while (EDITA)

C2345#*******  Reads field from the display and stores into OUTVAR  ******

      if (FLAG .eq. 0) then                                     ! underlined field
         ST = SMG$Set_Cursor_Abs (VD, LINE, COL_INI)
         ST = SMG$Read_From_Display (VD, INSTRING(1:COUNT))     ! spots typed chars
      else                                                      ! spaced field
         IK = 0
         L = COL_INI + 1
         LF = COL_INI + (COUNT*2)
         do KK = L,LF,2
            ST = SMG$Set_Cursor_Abs (VD, LINE, KK)
            IK = IK + 1
            ST = SMG$Read_From_Display (VD, INSTRING(IK:IK))
         end do
      end if
      ST = SMG$Set_Cursor_Mode (PB, SMG$M_CURSOR_OFF)
      do I = COUNT+1,80
         INSTRING(I:I) = ' '
      enddo
      do I = 1, TAM_MAX
         OUTSTRING(I:I) = ' '
      enddo
      ST = STR$Trim (OUTSTRING, INSTRING, COUNT)        ! dest <- sour, c
      ST = SMG$Set_Keypad_Mode (KB, SMG$M_KEYPAD_APPLICATION)
      return
      end

C**********************************************
C SUBROUTINE SMG_WERROR
C**********************************************
C WRITE ERROR MESSAGE AND WAITS FOR KEYSTRIKE
C
C=======OPTIONS	    /CHECK=NOOVERFLOW
	SUBROUTINE  SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	IMPLICIT    NONE

	INCLUDE	    'INCLIB:GLOBAL.DEF'
	INCLUDE	    'INCLIB:GTECHSMG.DEF'
C
C ROUTINE PARAMETERS
C
	CHARACTER*(*)	CMSG
	INTEGER		DISP3, KEYB, KEY
C
C LOCAL VARIABLES
C
	INTEGER*4	ST

	ST = SMG$PUT_CHARS (DISP3, CMSG, 1, 1, SMG$M_ERASE_LINE)
	ST = SMG$RING_BELL(DISP3, 2)
	ST = SMG$READ_KEYSTROKE(KEYB, KEY, , DISP3)
	ST = SMG$ERASE_DISPLAY(DISP3)

	RETURN
	END
C
C**********************************************
C SUBROUTINE SMG_CRTFIL
C**********************************************
C SAME AS CRTFIL, WITHOUT DISPLAYED MESSAGES

C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SMG_CRTFIL(NAME,LEN,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 NAME(5)
	INTEGER*4 SECTOR,LEN
	INTEGER*4 STATUS
C
	CHARACTER*20 CXNAME
	INTEGER*4 I4NAME(5)
	EQUIVALENCE (CXNAME, I4NAME)
C
	INTEGER*4 NREC,ST
C
C ALLOCATE AND CLEAR FILE
C
	STATUS=0
	SECTOR=LEN
	NREC=SECTOR/200
	IF(NREC*200.NE.SECTOR) THEN
	  NREC=NREC+1
	  SECTOR=NREC*200
	ENDIF
	CALL DFILW(NAME,0,0,ST)
C
	I4NAME(1) = NAME(1)
	I4NAME(2) = NAME(2)
	I4NAME(3) = NAME(3)
	I4NAME(4) = NAME(4)
	I4NAME(5) = NAME(5)
C
	CALL SMG_NEWFIL(1, CXNAME, SECTOR, ST)
	IF(ST.NE.0) THEN
	  STATUS=-1
	  RETURN
	ENDIF
C
	RETURN
	END
C
C**********************************************
C SUBROUTINE SMG_NEWFIL
C**********************************************
C SAME AS NEWFIL, WITHOUT DISPLAYED MESSAGES

C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SMG_NEWFIL(LUN, FILENAME, SECTORS, ST)
	IMPLICIT NONE
C
	INTEGER*4	LUN
	CHARACTER	FILENAME*(*)
	INTEGER*4	SECTORS
	INTEGER*4	ST
C
C
	IF(SECTORS.LT.1)THEN
	  ST = -1
	  GOTO 9000
	ENDIF
C
C	Create the file using CFILX
C
	CALL CFILX(FILENAME, 0, 0, SECTORS*2, 0,0,0, ST)
	IF(ST.NE.0) THEN
	  GOTO 9000
	ENDIF
C
C Now re-open the file and fill it with zeroes
C
	CALL SMG_SUBCLRFIL(LUN, FILENAME, ST)
C
9000	CONTINUE
	RETURN
C
	END
C
C**********************************************
C SUBROUTINE SMG_SUBCLRFIL
C**********************************************
C SAME AS SUBCLRFIL, WITHOUT DISPLAYED MESSAGES

C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SMG_SUBCLRFIL(LUN, FILENAME, ST)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4	LUN
	CHARACTER	FILENAME*(*)
	INTEGER*4	ST
C
	INTEGER*4	SECTORS
	INTEGER*4	FULLBLOCKS
	INTEGER*4	PARTBLOCKS
	INTEGER*4	BLOCK
	INTEGER*4	FDB(7)
	INTEGER*4	NAMLEN
	INTEGER*4	K
C
	INTEGER*4	BUFSIZE
	PARAMETER	(BUFSIZE = 512*1000)	!1000 SECTORS AT A TIME
	BYTE		BIGBUF(BUFSIZE)/BUFSIZE*0/
C
C
C
C
C Get the length of the non-blank name
C
	DO 110 K = LEN(FILENAME), 2, -1
	  IF(FILENAME(K:K).NE.' ')GOTO 120
110	CONTINUE
	K = 1
120	CONTINUE
	NAMLEN = K
C
C Open the file and fill it with zeroes
C
	CALL OPENX(LUN, FILENAME(1:NAMLEN), 4, 0, 0, ST)
	IF(ST.NE.0)THEN
	  GOTO 9000
	ENDIF
C
C Get size of file in sectors, then in bytes
C
	CALL VAXGETFSIZ(LUN, SECTORS)
	CLOSE(LUN)
C
C
	FULLBLOCKS = SECTORS / (BUFSIZE/512)
	PARTBLOCKS = MOD (SECTORS, BUFSIZE/512)
C
	CALL OPENQX(LUN, FILENAME(1:NAMLEN), 4, 0, 0, ST)
	IF(ST.NE.0)THEN
	  GOTO 9000
	ENDIF
C
	CALL IOQINIT(FDB, LUN, 512)
C
C
	BLOCK = 1
	DO 1100 K = 1, FULLBLOCKS
	  CALL WRITEQIO( FDB, BLOCK, BIGBUF, BUFSIZE, ST)
	  BLOCK = BLOCK + (BUFSIZE/512)
1100	CONTINUE
C
	IF(PARTBLOCKS.NE.0)THEN
	  PARTBLOCKS = PARTBLOCKS*512
	  CALL WRITEQIO( FDB, BLOCK, BIGBUF, PARTBLOCKS, ST)
	ENDIF
C
	CALL USRCLOSQ1(LUN)
	ST = 0
C
9000	CONTINUE
	RETURN
	END
