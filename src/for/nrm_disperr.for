C
C SUBROUTINE DISPERR
C
C DISPERR.FOR
C
C V01 02-FEB-2001 EPH RELEASED FOR PORTUGAL
C
C
C SUBROUTINE TO REPORT ERRORS TO OPERATOR
C
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
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C       ****************************************
	SUBROUTINE DISPERR (LUN,
     *                      STR1, VAL1,
     *                      STR2, VAL2,
     *                      STR3, VAL3)
C       ****************************************
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'

	INTEGER*4     LUN        
        CHARACTER*(*) STR1, STR2, STR3
        INTEGER*4     VAL1, VAL2, VAL3

	CHARACTER*78  SEPLIN /'=============================================================================='/
        CHARACTER*100 AUXSTR2
 	CHARACTER*20  AUXSTR
        CHARACTER*23  SYSTIME
        INTEGER*4     SZVAL, SZSTR
        CHARACTER*8   PROC_NAME

	CALL LIB$DATE_TIME (SYSTIME)
	CALL GETNAM(%REF(PROC_NAME))

	WRITE (LUN,100) SEPLIN(1:1), SYSTIME, SEPLIN(1:10), PROC_NAME, SEPLIN(1:16), ' ERROR MESSAGE ', SEPLIN(1:1)
100     FORMAT (/,1X, A1, ' ', A23, ' ', A10, ' ', A8, ' ', A16, A15, A1)

        SZSTR = LEN(STR1)
        IF (SZSTR.GT.1) THEN
           IF (VAL1.NE.0) THEN
	      AUXSTR = ITOC(VAL1,SZVAL)
              AUXSTR2 = STR1(1:SZSTR) // AUXSTR(1:SZVAL)
           ELSE
              AUXSTR2 = STR1(1:SZSTR)
           ENDIF
           WRITE (LUN,900) SEPLIN(1:1), AUXSTR2(1:74), SEPLIN(1:1)
900        FORMAT (1X, A1,' ',A74,' ',A1)
        ENDIF

        SZSTR = LEN(STR2)
        IF (SZSTR.GT.1) THEN
           IF (VAL2.NE.0) THEN
	      AUXSTR = ITOC(VAL2,SZVAL)
              AUXSTR2 = STR2(1:SZSTR) // AUXSTR(1:SZVAL)
           ELSE
              AUXSTR2 = STR2(1:SZSTR)
           ENDIF
           WRITE (LUN,900) SEPLIN(1:1), AUXSTR2(1:74), SEPLIN(1:1)
        ENDIF

        SZSTR = LEN(STR3)
        IF (SZSTR.GT.1) THEN
           IF (VAL3.NE.0) THEN
	      AUXSTR = ITOC(VAL3,SZVAL)
              AUXSTR2 = STR3(1:SZSTR) // AUXSTR(1:SZVAL)
           ELSE
              AUXSTR2 = STR3(1:SZSTR)
           ENDIF
           WRITE (LUN,900) SEPLIN(1:1), AUXSTR2(1:74), SEPLIN(1:1)
        ENDIF

	WRITE(LUN,901) SEPLIN
901     FORMAT (1X, A78, /)

	RETURN
        END
C
C       Example:
C
C       =  2-FEB-2001 12:34:00 ========== EPH      =================== ERROR MESSAGE =
C       = Wrong draw number                                                          =
C       = Draw = 25				                                     =
C       ==============================================================================
C
