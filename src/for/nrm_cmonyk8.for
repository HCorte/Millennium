C
C FUNCTION CMONYK8
C
C V01 06-JUN-2014 FRP INITIAL RELEASE
C
C THIS CHARACTER FUNCTION
C
C Calling sequence:
C
C     STRING=CMONYK8(AMOUNT,LEN,FACTOR)
C
C Input parametrs:
C
C     AMOUNT     Int*8         !THE AMOUNT OF MONEY IN UNITS WE USE
C     LEN        Int*4         !THE LENGTH OF THE STRING RETURNED
C     FACTOR     Int*4         !SHOWS WHICH VALUE OF ONE UNIT TO USE
C
C Output parameters:
C
C     CMONYK8    Char*32       !THE STRING TO BE PRINTED ON REPORTS
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
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	CHARACTER FUNCTION CMONYK8*32(AMOUNT,LEN,FACTOR)
	IMPLICIT NONE
    	INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
C
	INTEGER*8	AMOUNT		!AMOUNT IN CENTS
        INTEGER*4       FACTOR          !SHOWS WHICH UNIT OF MONEY TO USE
	INTEGER*4	LEN		!LENGTH OF OUTPUT
	INTEGER*4	DOLL_LEN	!LENGTH OF DOLLARS
	INTEGER*4	I		!LOOP COUNTER
        INTEGER*4       LMASK, HMASK, TEMP
        INTEGER*4       MON_UNIT
        CHARACTER*1     STAR
C
	CHARACTER*32	CTEMP		!TEMPORARY CHR VARIABLE
	INTEGER*4	ITEMP(8)	!TEMPORARY INT VARIABLE
	EQUIVALENCE    (CTEMP, ITEMP)
        DOUBLE PRECISION BIG, SBIT
        DATA LMASK/Z7FFFFFFF/, HMASK/Z80000000/
        DATA SBIT/2147483648.0D0/
        DATA STAR/'*'/
C
C ASSIGNING VALUE TO THE UNIT OF MONEY
C
        IF (FACTOR .EQ. VALUNIT) THEN
          MON_UNIT=DYN_VALUNIT
        ELSE IF (FACTOR .EQ. BETUNIT) THEN
          MON_UNIT=DYN_BETUNIT
        ELSE
          MON_UNIT=FACTOR
        END IF
C
C # OF DOLLAR CHARACTERS TO PRINT
C
	IF(CENT_LEN.EQ.0) THEN
	    DOLL_LEN = LEN
	ELSE
	    DOLL_LEN = LEN - CENT_LEN - 1
	ENDIF
C       IF (IAND(AMOUNT,HMASK) .NE. 0) THEN
C         TEMP=IAND(AMOUNT,LMASK)
C         BIG=(SBIT+DFLOAT(TEMP))*MON_UNIT/DOLL_BASE
C       ELSE
C         BIG=DFLOAT(AMOUNT)*MON_UNIT/DOLL_BASE
          BIG=DFLOTK(AMOUNT)*MON_UNIT/DOLL_BASE
C       END IF
        IF(BIG .LT. 10.0**(DOLL_LEN+1)) THEN
	  IF(CENT_LEN.EQ.0) THEN
             ENCODE(LEN,8201,CTEMP) INT(BIG)
	  ELSE
             ENCODE(LEN,8200,CTEMP) BIG
	  ENDIF
        ELSE
          ENCODE(LEN,8300,CTEMP) (STAR, I = 1, LEN)
        END IF
8200    FORMAT(F<LEN>.<CENT_LEN>)
8201    FORMAT(I<LEN>)
8300    FORMAT(<LEN>A1)
        CMONYK8=CTEMP
C
	RETURN
	END
