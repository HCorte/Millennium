C PASLOG.FOR
C
C V03 30-SET-2013 SCML Net pay amount added
C                      Free space added
C V02 01-JAN-2010 FJG ePassive
C V01 11-DEC-00 CS  INITIAL RELEASE FOR PORTUGAL
C
C SUBROUTINE TO CONVERT INTERNAL VALIDATION RECORD TO
C VALIDATION FILE RECORD.
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE PASLOG(VALREC,V4BUF_PAS)
	IMPLICIT   NONE

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
	INTEGER*4 RECTYP, NUMREC, I4TEMP
	INTEGER*4 INDLEN(4)
C
	EQUIVALENCE (I2TEMP,I4TEMP,I1TEMP)
	DATA INDLEN/Z00000000,Z40000000,Z80000000,ZC0000000/
C
C DETERMINE TYPE AND NUMBER OF RECORDS REQUIRED
C
	NUMREC = 1
	RECTYP = VAL_SHORT
	CALL FASTSET(0,V4BUF_PAS,SIZEOF(V4BUF_PAS)/4)
C
	IF(VALREC(VPZOFF).GT.1) THEN
		NUMREC = 2
		RECTYP = VAL_REG
		IF(VALREC(VPZOFF).GT.9) NUMREC=3
		IF(VALREC(VPZOFF).GT.19)NUMREC=4
	ENDIF
C
	I4TEMP = ISHFT(VALREC(VSERN),24) + VALREC(VTCKT)
	V4BUF_PAS(1)   = IOR(I4TEMP,INDLEN(NUMREC))                         !RECID/SERIE/TICKET# (4 bytes = 2 bits/6 bits/3 bytes)
C
	I1TEMP(1)      = VALREC(VPFRAC)                                     !FRACTION         (1 byte)
	I1TEMP(2)      = 0                                                  !FREE SPACE       (1 byte)
	I1TEMP(3)      = ISHFT(RECTYP,5) + IAND(VALREC(VSTAT),'1F'X)        !RECTYP/STATUS    (1 byte = 5 bits/5 bits)
	I1TEMP(4)      = VALREC(VGAM)                                       !GAME #           (1 byte)
	V4BUF_PAS(2)   = I4TEMP
C
	V4BUF_PAS(3)   = VALREC(VCSER)                                      !CASHING SERIAL   (4 bytes)
C	
	IF(VALREC(VPASTYP).EQ.VPASOFF) THEN
		V4BUF_PAS(4) = VALREC(VVALN)                                      !VALIDATION #     (4 bytes) Offline Ticket
	ELSE
		V4BUF_PAS(4) = VALREC(VSSER)                                      !SERIAL #         (4 bytes) Online Ticket
	ENDIF
C
	V4BUF_PAS(5)   = VALREC(VPAMT)                                      !PAY AMOUNT       (4 bytes)
C
C
	V4BUF_PAS(6)   = VALREC(VOPSAMT)                                    !NET PAY AMOUNT   (4 bytes) !V03
C
C CONVERT SHORT VALIDATION RECORDS
C
	IF(RECTYP.EQ.VAL_SHORT) THEN
		I2TEMP(1)    = VALREC(VEXP)                                       !EXPIRE DRAW      (2 bytes)
		I2TEMP(2)    = VALREC(VCTER)                                      !CASHING TER      (2 bytes)
		V4BUF_PAS(7) = I4TEMP
C
		I2TEMP(1)    = VALREC(VCCDC)                                      !CASHING CDC      (2 bytes)
		I2TEMP(2)    = VALREC(VWCDC)                                      !WINSEL  CDC      (2 bytes)
		V4BUF_PAS(8) = I4TEMP
C
		I2TEMP(1)    = VALREC(VPRGCDC)                                    !PURGE CDC        (2 bytes)
		I1TEMP(3)    = ISHFT(VALREC(VGTYP),3) + IAND(VALREC(VGIND),'07'X) !GTYP/GIND        (1 byte = 3 bits/5 bits)
		I1TEMP(4)    = VALREC(VPASTYP)                                    !TYPE/TICKET      (1 byte)
		V4BUF_PAS(9) = I4TEMP
C
		IF(VALREC(VPASTYP).EQ.VPASONL) THEN
			I2TEMP(1)  = VALREC(VSCDC)                                      !SELLING CDC      (2 bytes) Online Ticket
		ELSE
			I2TEMP(1)  = 0                                                  !SELLING CDC      (2 bytes) Offline Ticket
		ENDIF
		I2TEMP(2)    = VALREC(VOFFTER)                                    !OFFLINE CASH TER (2 bytes)
		V4BUF_PAS(10) = I4TEMP
C
		V4BUF_PAS(11)= VALREC(VPDATA)                                     !PRIZE DATA       (36 bytes)
C
C CONVERT REGULAR VALIDATION
C
	ELSEIF(RECTYP.EQ.VAL_REG) THEN
		IF(VALREC(VPASTYP).EQ.VPASONL) THEN
			I2TEMP(1)  = VALREC(VSCDC)                                      !SELLING CDC      (2 bytes) Online Ticket
		ELSE
			I2TEMP(1)  = 0                                                  !SELLING CDC      (2 bytes) Offline Ticket
		ENDIF
		I2TEMP(2)    = VALREC(VOFFTER)                                    !OFFLINE CASH TER (2 bytes)
	  V4BUF_PAS(7) = I4TEMP
C
		I2TEMP(1)    = VALREC(VEXP)                                       !EXPIRE DRAW      (2 bytes)
		I2TEMP(2)    = VALREC(VCTER)                                      !CASHING TER      (2 bytes))
		V4BUF_PAS(8) = I4TEMP
C
		I2TEMP(1)    = VALREC(VCCDC)                                      !CASHING CDC      (2 bytes)
		I2TEMP(2)    = VALREC(VWCDC)                                      !WINSEL  CDC      (2 bytes)
		V4BUF_PAS(9) = I4TEMP
C
		I2TEMP(1)    = VALREC(VPRGCDC)                                    !PURGE CDC        (2 bytes)
		I1TEMP(3)    = ISHFT(VALREC(VGTYP),3) + IAND(VALREC(VGIND),'07'X) !GTYP/GIND        (1 byte = 3 bits/5 bits) 
		I1TEMP(4)    = VALREC(VPZOFF)                                     !PRIZE INDEX      (1 byte)
		V4BUF_PAS(10) = I4TEMP
C
		I4TEMP       = 0                                                  !FREE SPACE       (2 bytes)
		I1TEMP(1)    = VALREC(VFRAC)                                      !# FRACTIONS      (1 byte)
		I1TEMP(2)    = VALREC(VPASTYP)                                    !TYPE/TICKET      (1 byte)
		V4BUF_PAS(11)= I4TEMP
C
		V4BUF_PAS(12)= 0                                                  !FREE PACE        (4 bytes)
		V4BUF_PAS(13)= 0                                                  !FREE PACE        (4 bytes) !V03
		V4BUF_PAS(14)= 0                                                  !FREE PACE        (4 bytes) !V03
		V4BUF_PAS(15)= 0                                                  !FREE PACE        (4 bytes) !V03
C
		CALL FASTMOV(VALREC(VPDATA),V4BUF_PAS(16),VPDATLEN)               !PRIZE DATA       (116 bytes)
	ENDIF
C
	RETURN
	END
C
C
C SUBROUTINE TO CONVERT DETAIL PRIZE DATA
C
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE DPASLOG(VALREC,VDETAIL)
	IMPLICIT   NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:PRMVPF.DEF'
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
	OFF    = 0
	PRZCNT = VALREC(VPZOFF)
	IF(PRZCNT.GT.VMAX) PRZCNT=VMAX

	CALL FASTSET(0,VALREC(VPDATA),VPDATLEN)
	DO I=1,PRZCNT
	   TEMP      = 0
           DRWOFF    = VALREC(VEXP)-VDETAIL(VDRW,I) + 1
           B1TEMP(1) = DRWOFF
	   B1TEMP(2) = VDETAIL(VDIV,I)
 	   B1TEMP(3) = VDETAIL(VSHR,I)
CC	   IF(VDETAIL(VUPD,I).NE.0) B1TEMP(4) = B1TEMP(4)+'80'X
CC	   IF(VDETAIL(VBDR,I).NE.0) B1TEMP(4) = B1TEMP(4)+'40'X
CC	   IF(VDETAIL(VKIK,I).NE.0) B1TEMP(4) = B1TEMP(4)+'20'X
	   VALREC(VPDATA+OFF) = TEMP
	   OFF       = OFF + 1
	ENDDO

	RETURN
	END
