C PDATE.FOR
C
C V01 18-JAN-95 WXM RELEASED FOR VAX
C
C This will display current or entered date in all available formats
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM  PDATE
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*2   DATE(SDATE_LEN)
	INTEGER*2   LDATE(LDATE_LEN)
        INTEGER*4   CDC, EXT, WEEK, YEAR0, YEAR, YEARW, I
        CHARACTER*3 MON(12)
        DATA        MON/'JAN','FEB','MAR','APR','MAY','JUN',
     *                  'JUL','AUG','SEP','OCT','NOV','DEC'/
        CHARACTER*9 DAY(7)
        DATA        DAY/'MONDAY   ','TUESDAY  ','WEDNESDAY',
     *                  'THURSDAY ','FRIDAY   ','SATURDAY ','SUNDAY   '/
C
C
C GET YEAR FOR CDC = 1
C
        DATE(VCDC) = 1
        CALL CDATE(DATE)
        YEAR0 = DATE(VYEAR)
C
C GET CDC DATE
C
10	CONTINUE
        CDC=0
        TYPE 700
        CALL DAT_UNPACK(CDC,YEAR0,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
        DATE(VCDC)=CDC
        CALL CDATE(DATE)
	LDATE(VCDC) = CDC
	CALL LCDATE(LDATE)
        YEARW = DATE(VYEAR)
        CALL FIGWEK(CDC,WEEK,YEARW)
        YEAR = DATE(VYEAR)+1900
        IF(DATE(VYEAR).LT.77) YEAR=YEAR+100
C
        TYPE 800,DATE(VDAY),MON(DATE(VMON)),YEAR,CDC,DATE(VJUL),
     *       DAY(DATE(VDOW)),WEEK,YEARW,
     *       (DATE(I),I=7,12),(LDATE(I),I=7,13)
C
	GOTO 10
C
C
700     FORMAT(1X,'Maximum date allowed: 31-Dec-2049',/)
800     FORMAT(//,1X,'CALENDAR DATE',4X,'CDC',2X,'JULIAN',
     *           2X,'WEEK DAY ',2X,'WEEK/YEAR',
     *         /,2X,I2,'-',A3,'-',I4,3X,I5,3X,I3,4X,A9,4X,I2,'/',I4,
     *         /,2X,6A2,2X,7A2,/)
        END



C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE DAT_UNPACK(CDC,YEAR0,EXT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*2 SDAT(12)
        CHARACTER CBUF(20)
        CHARACTER CXINBUF*(20)
        INTEGER*4 YEAR0
        INTEGER*4 INBUF(5),DDAT(3), EXT, ST
        INTEGER*4 L, K, N, INC, CDC, I, DIG
        INTEGER*4 M, D, Y
C
        EQUIVALENCE(INBUF,CBUF,CXINBUF)
        EQUIVALENCE(DDAT,SDAT(7))
C
C
C
40      CONTINUE
        EXT=0
        DO 41 I=1,12
          SDAT(I)=0
41      CONTINUE
C
C
        CALL WIMG(5,'Enter date, Julian date, or day number ')
        READ(5,900) INBUF
C
C CHECK FOR INPUT
C
        IF(CXINBUF.EQ.' ')GOTO 65
C
C CHECK FOR EXIT
C
        IF(CBUF(1).EQ.'E'.OR.CBUF(1).EQ.'e') THEN
          EXT=-1
          GOTO 9000
        ENDIF
C
C
        L=0
        K=1
        DO 50 I=1,20
         IF(CBUF(I).EQ.CHAR(0)) GOTO 50
         IF(CBUF(I).EQ.' ') GOTO 50
         IF(CBUF(I).EQ.'-') GOTO 45
         IF(CBUF(I).EQ.'/') GOTO 45
         IF(CBUF(I).LT.'0') GOTO 60
         IF(CBUF(I).GT.'9') GOTO 60
         CALL ASCBIN(INBUF,I,1,DIG,ST)
         SDAT(K)=SDAT(K)*10+DIG
         L=L+1
         IF(L.GE.4) GOTO 60
         GOTO 50
45       CONTINUE
         L=0
         K=K+1
         IF(K.GE.4) GOTO 60
50      CONTINUE
C
C
        IF(K.NE.3) GOTO 55
        CALL BDATE(SDAT)
        IF(SDAT(VDOW).EQ.0) GOTO 60
        N=SDAT(VCDC)
        GOTO 90
C
C
55      CONTINUE
        IF(K.NE.2) GOTO 60
        SDAT(VJUL)=SDAT(1)
        SDAT(VYEAR)=SDAT(2)
        CALL JDATE(SDAT)
        IF(SDAT(VDOW).EQ.0) GOTO 60
        N=SDAT(VCDC)
        GOTO 90
C
C
60      CONTINUE
        DDAT(1)=INBUF(1)
        DDAT(2)=INBUF(2)
        DDAT(3)=INBUF(3)
        CALL AADATE(SDAT)
        IF(SDAT(VDOW).EQ.0) GOTO 70
        N=SDAT(VCDC)
        GOTO 90
C
C
65      CONTINUE
        CALL GDATE(M,D,Y)
        SDAT(VMON)=M
        SDAT(VDAY)=D
        SDAT(VYEAR)=Y
        CALL BDATE(SDAT)
        N=SDAT(VCDC)
        GOTO 90
C
C
70      CONTINUE
        L=0
        N=0
        K=1
        IF(CBUF(1).EQ.'A'.OR.CBUF(1).EQ.'B'.OR.
     *       CBUF(1).EQ.'a'.OR.CBUF(1).EQ.'b') K=2
        DO 80 I=K,20
        IF(CBUF(I).EQ.' ') GOTO 80
        IF(CBUF(I).EQ.CHAR(0)) GOTO 80
        IF(CBUF(I).LT.'0') GOTO 120
        IF(CBUF(I).GT.'9') GOTO 120
        CALL ASCBIN(INBUF,I,1,DIG,ST)
        N=N*10+DIG
        L=L+1
        IF(L.GT.6) GOTO 120
80      CONTINUE
        IF(CBUF(1).NE.'A'.AND.CBUF(1).NE.'B'.AND.
     *       CBUF(1).NE.'a'.AND.CBUF(1).NE.'b') GOTO 90
        INC=N
        IF(CBUF(1).EQ.'B'.OR.CBUF(1).EQ.'b') INC=N-1
        N=CDC+INC
C
C
90      CONTINUE
        SDAT(VCDC)=N
        IF(SDAT(VYEAR).GT.49.AND.SDAT(VYEAR).LT.YEAR0) GOTO 120
        IF(SDAT(VCDC).LT.0) GOTO 120
        CALL CDATE(SDAT)
        CDC=SDAT(VCDC)
        GOTO 9000
C
C
120     CONTINUE
        WRITE(5,904)IAM(),INBUF
        GOTO 40
C

9000    CONTINUE
        RETURN
C
900     FORMAT(5A4)
904     FORMAT(1X,A,'Invalid input: ',5A4)
        END
