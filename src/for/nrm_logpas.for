C LOGPAS.FOR
C
C V03 30-SET-2013 SCML Net pay amount added
C                      Free space added
C V02 01-JAN-2010 FJG ePassive
C V01 11-DEC-00 CS  INITIAL RELEASE FOR PORTUGAL
C
C SUBROUTINE TO CONVERT VALIDATION FILE RECORD TO
C INTERNAL VALIDATION RECORD.
C
C CALLING SEQUENCE:
C CALL LOGPAS(VALREC,V4BUF_PAS)
C INPUT
C     V4BUF_PAS   - VALIDATION FILE RECORD.
C OUTPUT
C     VALREC      - VALIDATION INTERNAL FORMAT.
C
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
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE LOGPAS(VALREC,V4BUF_PAS)
	IMPLICIT   NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VALPASFIL.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
C
C LOCAL VARIABLES
C
	INTEGER*2 I2TEMP(2)
	BYTE      I1TEMP(4)
	INTEGER*4 RECTYP, I4TEMP, X, INDMSK
C
	DATA      INDMSK/Z3FFFFFFF/
	EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
C
C
	CALL FASTSET(0,VALREC,SIZEOF(VALREC)/4)
C
	I4TEMP            = IAND(V4BUF_PAS(1),INDMSK)
	VALREC(VTCKT)     = IAND(I4TEMP,'00FFFFFF'X)         !TICKET#
	VALREC(VSERN)     = ISHFT(I4TEMP,-24)                !SERIE
C
	I4TEMP            = V4BUF_PAS(2)
	VALREC(VPFRAC)    = ZEXT(I1TEMP(1))                  !FRACTION
C
	X                 = ZEXT(I1TEMP(3))
	RECTYP            = ISHFT(X,-5)                      !RECTYP
	VALREC(VSTAT)     = IAND(X,'1F'X)                    !STATUS
	VALREC(VGAM)      = ZEXT(I1TEMP(4))                  !GAME #
C
	VALREC(VCSER)     = V4BUF_PAS(3)                     !CASHING SERIAL
C
	VALREC(VPAMT)     = V4BUF_PAS(5)                     !PAY AMOUNT
C 
	VALREC(VOPSAMT)   = V4BUF_PAS(6)                     !NET PAY AMOUNT !V03
C
C CONVERT SHORT RECORDS
C
	IF(RECTYP.EQ.VAL_SHORT) THEN
		I4TEMP          = V4BUF_PAS(7)
		VALREC(VEXP)    = ZEXT(I2TEMP(1))                  !EXPIRE DRAW
		VALREC(VEXTR)   = VALREC(VEXP)                     !DRAW NUMBER
		VALREC(VCTER)   = ZEXT(I2TEMP(2))                  !CASHING TER
C
		I4TEMP          = V4BUF_PAS(8)
		VALREC(VCCDC)   = ZEXT(I2TEMP(1))                  !CASHING CDC
		VALREC(VWCDC)   = ZEXT(I2TEMP(2))                  !WINSEL  CDC
C
		I4TEMP          = V4BUF_PAS(9)
		VALREC(VPRGCDC) = ZEXT(I2TEMP(1))                  !PURGE CDC
		X               = ZEXT(I1TEMP(3))
		VALREC(VGTYP)   = ISHFT(X,-3)                      !GAME TYPE
		VALREC(VGIND)   = IAND(X,'07'X)                    !GAME INDEX
		VALREC(VPASTYP) = ZEXT(I1TEMP(4))                  !PASSIVE TYPE
!+++++++++++Based on the ticket type the fields are reused for one purpose or other
		IF(VALREC(VPASTYP).EQ.VPASOFF) THEN
			VALREC(VVALN) = V4BUF_PAS(4)                     !VALIDATION NUMBER
		ELSE
			VALREC(VSSER) = V4BUF_PAS(4)	                   !SERIAL NUMBER
		ENDIF
C
		I4TEMP          = V4BUF_PAS(10)
		IF(VALREC(VPASTYP).EQ.VPASONL) THEN
			VALREC(VSCDC) = ZEXT(I2TEMP(1))                  !SELLING CDC
		ENDIF
		VALREC(VOFFTER) = ZEXT(I2TEMP(2))                  !OFFLINE CASH TER
C
		VALREC(VPDATA)  = V4BUF_PAS(11)                    !PRIZE DATA
		VALREC(VPZOFF)  = 1                                !PRIZE INDEX
C
C CONVERT REGULAR VALIDATION
C
	ELSEIF(RECTYP.EQ.VAL_REG) THEN
C
		I4TEMP          = V4BUF_PAS(7)
		VALREC(VSCDC)   = ZEXT(I2TEMP(1)) ! This should has values only if VPASONL  !SELLING CDC
		VALREC(VOFFTER) = ZEXT(I2TEMP(2))                  !OFFLINE CASH TER
C
		I4TEMP          = V4BUF_PAS(8)
		VALREC(VEXP)    = ZEXT(I2TEMP(1))                  !EXPIRE DRAW
		VALREC(VEXTR)   = VALREC(VEXP)                     !DRAW NUMBER
		VALREC(VCTER)   = ZEXT(I2TEMP(2))                  !CASHING TER
C
		I4TEMP          = V4BUF_PAS(9)
		VALREC(VCCDC)   = ZEXT(I2TEMP(1))                  !CASHING CDC
		VALREC(VWCDC)   = ZEXT(I2TEMP(2))                  !WINSEL CDC
C
		I4TEMP          = V4BUF_PAS(10)
		VALREC(VPRGCDC) = ZEXT(I2TEMP(1))                  !PURGE CDC
		X               = ZEXT(I1TEMP(3))
		VALREC(VGTYP)   = ISHFT(X,-3)                      !GAME TYPE
		VALREC(VGIND)   = IAND(X,'07'X)                    !GAME INDEX
		VALREC(VPZOFF)  = ZEXT(I1TEMP(4))                  !PRIZE INDEX
C
		I4TEMP          = V4BUF_PAS(11)
		VALREC(VFRAC)   = ZEXT(I1TEMP(1))                  !# FRACTIONS
		VALREC(VPASTYP) = ZEXT(I1TEMP(2))   	             !TYPE/TICKET
!+++++++++++Based on the ticket type the fields are reused for one purpose or other
		IF(VALREC(VPASTYP).EQ.VPASOFF) THEN
			VALREC(VVALN)  = V4BUF_PAS(4)                    !VALIDATION #
		ELSE
			VALREC(VSSER)  = V4BUF_PAS(4)	                   !SERIAL #
		ENDIF
C
C FREE SPACE
C
	I4TEMP             = V4BUF_PAS(12)                   !FREE SPACE
	I4TEMP             = V4BUF_PAS(13)                   !FREE SPACE !V03
	I4TEMP             = V4BUF_PAS(14)                   !FREE SPACE !V03
	I4TEMP             = V4BUF_PAS(15)                   !FREE SPACE !V03
C
		CALL FASTMOV(V4BUF_PAS(16),VALREC(VPDATA),VPDATLEN)!PRIZE DATA
	ENDIF

	RETURN
 	END
C
C SUBROUTINE TO CONVERT DETAIL PRIZE DATA
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE DLOGPAS(VALREC,VDETAIL)
	IMPLICIT   NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VDETAIL.DEF'
C
	INTEGER*4 DRWOFF, I, PRZCNT
C
	INTEGER*4 TEMP,OFF
	BYTE	  B1TEMP(4)
	INTEGER*2 I2TEMP(2)
	EQUIVALENCE (TEMP,I2TEMP,B1TEMP)
C
C
	PRZCNT = VALREC(VPZOFF)
	CALL FASTSET(0,VDETAIL,SIZEOF(VDETAIL)/4)
	OFF    = 0
	IF(PRZCNT.GT.VMAX) PRZCNT = VMAX
C
	DO I = 1,PRZCNT
		TEMP            = VALREC(VPDATA+OFF)
		OFF             = OFF + 1
		DRWOFF          = ZEXT(B1TEMP(1))
		VDETAIL(VDRW,I) = VALREC(VEXP)-DRWOFF+1
		VDETAIL(VDIV,I) = ZEXT(B1TEMP(2))
		VDETAIL(VSHR,I) = ZEXT(B1TEMP(3))
CC	   X               = ZEXT(B1TEMP(4))
CC	   IF(IAND(X,'80'X).NE.0) VDETAIL(VUPD,I) = 1
CC	   IF(IAND(X,'40'X).NE.0) VDETAIL(VBDR,I) = 1
CC	   IF(IAND(X,'20'X).NE.0) VDETAIL(VKIK,I) = 1
	ENDDO
C
	RETURN
	END
