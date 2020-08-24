C
C SUBROUTINE VCALEN
C
C V04 19-JUL-2000 UXN Changed day names to be English
C V03 24-SEP-1993 GXA Changed month names to Finish format on customer request.
C V02 29-JUN-1993 GXA Released for Finland Dec Conversion / Oddset.
C                     Changed FDAY to reflect Nov 1,1989 and changed day 
C                     names to be in Finish.
C V01 01-AUG-1990 XXX RELEASED FOR VAX
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE VCALEN
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INTEGER*4  FDAY              !MAY 1, 2001 FOR PORTUGAL
        PARAMETER (FDAY=8886)        
C
        INTEGER*2 DATE(12), DBUF(4), DTAB(14)
	INTEGER*2 LDATE(14)
	INTEGER*2 LDBUF(5)
        INTEGER*4 MONTAB(12),DAYTAB(7),DAYCNT(12)
        INTEGER*4 MTAB(12)
        INTEGER*4 MONAME, TEMP, I, D, ST1, Y, ST2, M
        INTEGER*4 J, TST, YN, VD, C, L, R, K
        CHARACTER CMON(4),CTEMP(4)
        CHARACTER*8 CXDBUF
	CHARACTER*10 LCXDBUF
C
        EQUIVALENCE (DTAB(1),DAYTAB(1))
        EQUIVALENCE (TEMP,CTEMP)
        EQUIVALENCE (CMON,MONAME)
        EQUIVALENCE (DBUF(1),CXDBUF)
        EQUIVALENCE (LDBUF(1),LCXDBUF)
C
C***    DATA MONTAB/'Jan ','Feb ','Mar ','Apr ','May ','Jun ',
C*** *   'Jul ','Aug ','Sep ','Oct ','Nov ','Dec '/
C***    DATA MTAB/'JAN ','FEB ','MAR ','APR ','MAY ','JUN ',
C*** *   'JUL ','AUG ','SEP ','OCT ','NOV ','DEC '/
C***
C
        DATA MONTAB/'.01.','.02.','.03.','.04.','.05.','.06.',
     *   '.07.','.08.','.09.','.10.','.11.','.12.'/
        DATA MTAB/'.01.','.02.','.03.','.04.','.05.','.06.',
     *   '.07.','.08.','.09.','.10.','.11.','.12.'/
        DATA DAYCNT/0,31,59,90,120,151,181,212,243,273,304,334/
        DATA DAYTAB/' Mo ',' Tu ',' We ',' Th ',' Fr ',' Sa ',' Su '/

C    ASCII TO BINARY
        ENTRY AADATE(DATE)
        ENTRY ADATE(DATE)

        DBUF(1)=DATE(9)
        DBUF(2)=DATE(10)
        READ   (CXDBUF,100) MONAME
        TEMP=0
        DO 5 I=1,3
        IF(CMON(I).GE.'a'.AND.CMON(I).LE.'z') THEN
          CTEMP(4)=CMON(I)
          TEMP=TEMP-32
          CMON(I)=CTEMP(4)
        ENDIF
5       CONTINUE
        CALL ASCBIN(DATE,20,2,D,ST1)
        CALL ASCBIN(DATE,23,2,Y,ST2)
        IF(ST1.NE.0.AND.ST2.NE.0) GOTO 900
100     FORMAT(A3,1X)
        IF(Y.LT.77) Y=Y+100
        DO 110,M=1,12
        IF(MTAB(M).EQ.MONAME)GOTO 120
  110   CONTINUE
        GOTO 900
  120   IF(D.LE.0.OR.D.GT.31) GOTO 900
        DATE(1)=M
        DATE(2)=D
        DATE(3)=Y
        GOTO 125

C     BINARY TO JULIAN
        ENTRY BDATE(DATE)

 125    M=DATE(1)
        D=DATE(2)
        Y=DATE(3)
        IF(D.LE.0.OR.D.GT.31.OR.M.LE.0.OR.M.GT.12)GOTO 900
        J=DAYCNT(M)+D
        TST=IAND(Y,3)
        IF(TST.EQ.0.AND.M.GT.2)J=J+1
        DATE(4)=J
        GOTO 130

C     JULIAN TO CDC
        ENTRY JDATE(DATE)

  130   Y=MOD(DATE(3),100)
        J=DATE(4)
        IF (J.LE.0.OR.J.GT.366) GOTO 900
        IF (Y.LT.0) GOTO 900
        IF (Y.LT.77) Y=Y+100
        YN=Y-77
        VD=J+365*YN+YN/4
        C=VD-FDAY
        DATE(5)=C
        GOTO 140

C     CDC TO JULIAN
        ENTRY CDATE(DATE)

  140   C=DATE(5)
        VD=C+FDAY-1
        L=VD/1461
        R=MOD(VD,1461)
        Y=MOD(R/365+4*L+77,100)
        J=MOD(R,365)+1
        IF(R.NE.1460)GOTO 193
        Y=Y-1
        J=366
  193   CONTINUE
        DATE(3)=Y
        DATE(4)=J

C     JULIAN TO BINARY

        TST=IAND(Y,3)
        IF (TST.NE.0.OR.J.NE.60)GOTO 195
        M=2
        GOTO 210
  195   IF(TST.EQ.0.AND.J.GE.60)J=J-1
        DO 200,M=12,1,-1
        IF(J.GT.DAYCNT(M)) GOTO 210
  200    CONTINUE
  210   D=J-DAYCNT(M)
        DATE(1)=M
        DATE(2)=D

C     BINARY TO ASCII

        WRITE  (CXDBUF,211) D,MONTAB(M),Y
211     FORMAT(I2.2,A4,I2.2)
        DO 212 I=1,4
        DATE(I+8)=DBUF(I)
212     CONTINUE
C     SET DOW AND DAY NAME
        K=MOD(C+FDAY+4,7)+1
        DATE(6)=K
        K=K+K
        DATE(7)=DTAB(K-1)
        DATE(8)=DTAB(K)
        RETURN
C
C    ASCII TO BINARY
C
        ENTRY LAADATE(LDATE)
        ENTRY LADATE(LDATE)

        LDBUF(1)=LDATE(9)
        LDBUF(2)=LDATE(10)
        READ(LCXDBUF,100) MONAME
        TEMP=0
        DO I=1,3
          IF(CMON(I).GE.'a'.AND.CMON(I).LE.'z') THEN
            CTEMP(4)=CMON(I)
            TEMP=TEMP-32
            CMON(I)=CTEMP(4)
          ENDIF
	ENDDO
        CALL ASCBIN(LDATE,20,2,D,ST1)
        CALL ASCBIN(LDATE,23,4,Y,ST2)
        IF(ST1.NE.0.AND.ST2.NE.0) GOTO 9001
        DO M=1,12
          IF(MTAB(M).EQ.MONAME)GOTO 220
	ENDDO
        GOTO 9001
  220   IF(D.LE.0.OR.D.GT.31) GOTO 9001
        LDATE(1)=M
        LDATE(2)=D
        LDATE(3)=MOD(Y,100)
	LDATE(14)=Y
        GOTO 225

C     BINARY TO JULIAN
        ENTRY LBDATE(LDATE)

 225    M=LDATE(1)
        D=LDATE(2)
        Y=LDATE(3)
        IF(D.LE.0.OR.D.GT.31.OR.M.LE.0.OR.M.GT.12)GOTO 9001
        J=DAYCNT(M)+D
        TST=IAND(Y,3)
        IF(TST.EQ.0.AND.M.GT.2)J=J+1
        LDATE(4)=J
        GOTO 230

C     JULIAN TO CDC
        ENTRY LJDATE(LDATE)

  230   Y=MOD(LDATE(3),100)
        J=LDATE(4)
        IF (J.LE.0.OR.J.GT.366) GOTO 9001
        IF (Y.LT.0) GOTO 9001
        IF (Y.LT.77) Y=Y+100
        YN=Y-77
        VD=J+365*YN+YN/4
        C=VD-FDAY
        LDATE(5)=C
        GOTO 240

C
C LCDATE - Long CDATE will return date in format DD.MM.YYYY
C
        ENTRY LCDATE(LDATE)   ! Long CDATE 
240     C=LDATE(5)
        VD=C+FDAY-1
        L=VD/1461
        R=MOD(VD,1461)
        Y=MOD(R/365+4*L+77,100)
        J=MOD(R,365)+1
        IF(R.EQ.1460) THEN
          Y=Y-1
          J=366
	ENDIF
        LDATE(3)=Y
        LDATE(4)=J

C     JULIAN TO BINARY

        TST=IAND(Y,3)
        IF (TST.NE.0.OR.J.NE.60) THEN
         IF(TST.EQ.0.AND.J.GE.60) J=J-1
         DO M=12,1,-1
            IF(J.GT.DAYCNT(M)) GOTO 310
  	 ENDDO
	ELSE
          M=2
	ENDIF
310     D=J-DAYCNT(M)
        LDATE(1)=M
        LDATE(2)=D

C     BINARY TO ASCII
	IF(Y.LT.77) THEN    
	    Y = Y+2000
	ELSE
	    Y = Y+1900
	ENDIF
        LDATE(14)=Y
        WRITE  (LCXDBUF,311) D,MONTAB(M),Y
311     FORMAT(I2.2,A4,I4.4)
        DO I=1,5
          LDATE(I+8)=LDBUF(I)
	ENDDO
C     SET DOW AND DAY NAME
        K=MOD(C+FDAY+4,7)+1
        LDATE(6)=K
        K=K+K
        LDATE(7)=DTAB(K-1)
        LDATE(8)=DTAB(K)
        RETURN

C     ERROR RETURN

  900   DATE(6)=0
        RETURN
C     ERROR RETURN
9001    LDATE(6)=0
        RETURN
        END
