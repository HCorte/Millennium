C
C SUBROUTINE HTOA
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]HTOA.FOV                                     $
C  $Date::   17 Apr 1996 13:33:20                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - atoh.for;1 **
C
C
C ===============================================================
C HTOA.FTN
C
C Calling sequence:
C
C     CALL HTOA(ASCSTR,START,LENGTH,INTSTR,ERR)
C
C Input parameters:
C
C     INTSTR      Int*4(2)    Binary Coded Decimal string
C     START       Int*4       Starting BCD digit (2 digits per byte)
C     LENGTH      Int*4       Number of BCD digits to convert
C
C Output parameters:
C
C     ASCSTR      Char*1(*)   ASC string of converted BCD
C     ERR         Int*4       Return error
C                             -1 = invalid string
C                             -2 = invalid length
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE HTOA(ASCSTR,START,LENGTH,INTSTR,ERR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER*1 ASCSTR(*)               !ASCII string
	INTEGER*4   START                   !Start position into string
	INTEGER*4   LENGTH                  !Length to convert
	INTEGER*4   INTSTR(2)               !Output BCD value
	INTEGER*4   ERR                     !Conversion error
	INTEGER*4   I,K,IDX                 !Work variables
	INTEGER*4   VALUE                   !Conversion variable
C
        INTEGER*4   FF
        PARAMETER   (FF='0F'X)
C
        INTEGER*4   LO_WD,HI_WD             ! Words in Quadword
        PARAMETER   (LO_WD=2,HI_WD=1)
C
       IF(LENGTH.LT.0 .OR. LENGTH.GT.16) THEN
          ERR=-2
          GO TO 8000
       ENDIF  
C
C INITIALIZE VARIABLES.
C
	ERR=0
C
	DO 60 I=START,START+LENGTH-1
          IF (I.LE.8) THEN        
            IDX=(I-1)*4          
            VALUE= IAND(INTSTR(LO_WD),ISHFT(FF,IDX))
            VALUE= ISHFT(VALUE,-IDX)
          ELSE
            IDX=(I-8-1)*4
            VALUE= IAND(INTSTR(HI_WD),ISHFT(FF,IDX))
            VALUE=ISHFT(VALUE,-IDX)
          ENDIF
C
          K=START+LENGTH-I
	  IF(VALUE.GE. 0 .AND. VALUE.LE.9) THEN
 	    ASCSTR(K)= CHAR(VALUE + ICHAR('0'))
	  ELSEIF(VALUE.GE.'0A'X .AND. VALUE.LE.'0F'X) THEN
 	    ASCSTR(K)= CHAR(VALUE -10 + ICHAR('A'))
          ELSE 
	    ERR=-1
	    GOTO 8000
	  ENDIF
60	CONTINUE
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
	END
