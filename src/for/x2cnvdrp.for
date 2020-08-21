C
C SUBROUTINE X2CNVDRP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CNVDRP.FOV                                 $
C  $Date::   17 Apr 1996 16:14:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V01 01-DEC-91 DAS INITIAL RELEASE                       
C
C   FUNCTION: CONVERT DROP ADDRESS TO TERMINAL ID
C
C   INPUT: C2DROP - TWO CHARACTER DROP ADDRESS
C                   IF SINGLE CHARACTER ADDRESS LEFT JUSTIFIED
C                   AND ZERO EXTENDED.  
C   OUTPUT: TERM_ID - TERMINAL IDENTIFIER
C
C   ERRORS: IF TERMINAL ID IS -1 THEN INVALID DROP HAS BEEN 
C           ENTERED.  
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2CNVDRP(C2DROP, TERM_ID)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:X2XPRM.DEF'
C
        INTEGER*4 TERM_ID
        CHARACTER*2 C2DROP
C
C.............................................................
C         NOTE: GTX TERMINAL ID RANGES START AT 0 - 156
C               HOST TERMINAL ID RANGES START AT 1 - 157 
C               TAKEN FROM COMTECH SPEC "PROTOCOL:GSC 81-9001-01
C.............................................................
C
C         EXTENDED ADDRESSING (TERMINAL ID 31 - 61)         
C
          IF(C2DROP(1:1).EQ.'%') THEN
            IF(C2DROP(2:2).EQ.'>'.OR.C2DROP(2:2).EQ.'?') THEN
              TERM_ID = ICHAR(C2DROP(2:2))-1 
            ELSE
              TERM_ID = 30+ICHAR(C2DROP(2:2))- 63
            ENDIF
C
C         EXTENDED ADDRESSING (TERMINAL ID 62 - 94)
C
          ELSE IF(C2DROP(1:1).EQ.'#') THEN
            IF(C2DROP(2:2).EQ.'>'.OR.C2DROP(2:2).EQ.'?') THEN
              TERM_ID = 32 + ICHAR(C2DROP(2:2)) - 1 
            ELSE
              TERM_ID = 62 + ICHAR(C2DROP(2:2)) - 63
            ENDIF
C
C        EXTENDED ADDRESSING (TERMINAL ID 95 - 126)
C
          ELSE IF(C2DROP(1:1).EQ.'$') THEN
            IF(C2DROP(2:2).EQ.'>'.OR.C2DROP(2:2).EQ.'?') THEN
              TERM_ID = 64 + ICHAR(C2DROP(2:2)) - 1 
            ELSE
              TERM_ID = 94 + ICHAR(C2DROP(2:2)) - 63
            ENDIF
C
C        EXTENDED ADDRESSING (TERMINAL ID 127 - 157)
C          NOTE: THAT DROP "!?" IS NOT VALID AND WILL BE REJECTED
C
C
         ELSE IF(C2DROP(1:1).EQ.'!') THEN
            IF(C2DROP(2:2).EQ.'>'.OR.C2DROP(2:2).EQ.'?') THEN
              TERM_ID = 96 + ICHAR(C2DROP(2:2)) - 1 
            ELSE
              TERM_ID = 126 + ICHAR(C2DROP(2:2)) - 63
            ENDIF
C
C        NORMAL ADDRESSING (TERMINAL ID 1 - 30)
C
         ELSE
            TERM_ID = ICHAR(C2DROP(1:1)) - 63
         ENDIF
C
         IF(TERM_ID.LE.0.OR.TERM_ID.GT.X2X_MAXTERMS) THEN
	     TERM_ID = -1
          ENDIF
C
	RETURN
	END
