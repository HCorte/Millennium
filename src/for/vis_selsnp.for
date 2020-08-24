C SUBROUTINE SELSNP
C
C V15 31-MAY-2000 PXO Subroutine name from TSLSNP -> SELSNP
C V14 23-SEP-1999 UXN Format statement changed.
C V13 27-NOV-1997 UXN Singles,doubles added for PITKA.
C V12 23-MAR-1995 HXK Fix for Purge amount
C V11 23-NOV-1993 SXH FIXED OVERFLOW WITH TOTAL AND NET AMOUNTS
C V10 21-NOV-1993 SXH FIX OVERFLOW PROBLEM
C V09 17-OCT-1993 GXA One more change for 1 2 3 to 1 X 2.
C V08 22-SEP-1993 GXA Changed 1x2 again.
C V07 13-SEP-1993 GXA Swaped 1 2 3 with 1 X 2.
C V06 16-AUG-1993 HXN Displayed 1X2 instead of 12X order.
C V05 26-JUL-1993 SXH Display all 40 events
C V04 13-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 18-OCT-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C TOTO SELECT GAME SNAPSHOT
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE SELSNP(NUM,GIND,ROW)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:TSLCOM.DEF'
        INCLUDE 'INCLIB:DTSREC.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
C
        ! arguments
        INTEGER*4  NUM                    !
        INTEGER*4  GIND                   !
        INTEGER*4  ROW                    !

        ! variables
        INTEGER*4  FDB(7)                 !
        INTEGER*4  ROWSTS(MAXSRW)         !
        INTEGER*4  GNUM                   !
        INTEGER*4  DRAW                   !
        INTEGER*4  ST                     !
        INTEGER*4  FINODS                 !
        INTEGER*4  TAX                    !
        INTEGER*4  NET                    !
        INTEGER*4  I                      !
        INTEGER*4  K                      !
        INTEGER*4  R                      !
        INTEGER*4  LNS                    !
        INTEGER*4  SALTAX/0/              !TEMPORARILY SET UNTIL FURTHER .....
        INTEGER*4  ENDROW                 !
        INTEGER*4  PRG_AMT                !

        INTEGER*2  DBUF(LDATE_LEN)        !
        INTEGER*2  RDBUF(LDATE_LEN)       !
        INTEGER*2  BEGSAL(LDATE_LEN)      !
        INTEGER*2  ENDSAL(LDATE_LEN)      !

        REAL*8     TXPOOL                 !

        CHARACTER     RESULT(5)           !
        CHARACTER     RTYPE(0:3)           !
        CHARACTER*3   FINRES(5)           !
        CHARACTER*3   FINTYP(3)           !
        CHARACTER*17  POLSTS(11)          !
        CHARACTER*3   RWSTS(11)           !
        CHARACTER*31  HLIN(24)            !

        DATA RESULT/'-','1','2','X','C'/
        DATA RTYPE/' ','1','2',' '/

        DATA FINRES/' - ',' 1 ',' 2 ',' X ','can'/
        DATA FINTYP/' 1 ',' 2 ','   '/

        DATA POLSTS/'Not initialized  ','No drawing       ',
     *              'Info entered     ','Game open        ',
     *              'End of game      ','Results entered  ',
     *              'Results verified ','Drawing completed',
     *              'Results are final','Refund/cancelled ',
     *              'Refunds enabled  '/

        DATA RWSTS/'   ','cls','ent','opn','cls','rin','ver',
     *             'drw','fin','can','ref'/
C
C
        DRAW=NUM
        IF(GIND.LT.1.OR.GIND.GT.MAXIND) THEN
            WRITE(CLIN23,3000) GTNAMES(TTSL)
            RETURN
        ENDIF
C
C
        GNUM=GTNTAB(TTSL,GIND)
        IF(GNUM.LT.1) THEN
            WRITE(CLIN23,3010) GTNAMES(TTSL),GIND
            RETURN
        ENDIF
        IF(DRAW.LT.1) DRAW=DAYDRW(GNUM)
        IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)

        IF(ROW.LT.1 .OR. ROW.GT.MAXSRW) THEN
            ROW = 1
            ENDROW = 18
        END IF

        IF(ROW.GT.23) THEN
            ROW = 23
            ENDROW=40
        ELSE 
            ENDROW = ROW+17
        END IF
C
C GET DATA FROM COMMON OR DISK
C
        IF(DRAW.EQ.DAYDRW(GNUM)) THEN
            CALL GAMLOG(TTSL,GIND,DTSREC,TSLSTS)
            GOTO 100
        ENDIF
C
C
        SMODE=.TRUE.
        CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
        CALL IOINIT(FDB,1,DTSSEC*256)
        IF(ST.NE.0) THEN
            WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
            CALL USRCLOS1(     1)
            RETURN
        ENDIF
        CALL READW(FDB,DRAW,DTSREC,ST)
        IF(ST.NE.0) THEN
            WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
            CALL USRCLOS1(     1)
            RETURN
        ENDIF
        IF(DTSSTS.EQ.0) THEN
            WRITE(CLIN23,3040) GTNAMES(TTSL),GIND,DRAW
            CALL USRCLOS1(     1)
            RETURN
        ENDIF
        CALL USRCLOS1(     1)
C
100     CONTINUE
        IF(DTSSTS.GE.GFINAL) THEN
            DBUF(5)=DAYCDC
            BEGSAL(5)=DTSBSD
            ENDSAL(5)=DTSESD
            CALL LCDATE(DBUF)
            CALL LCDATE(BEGSAL)
            CALL LCDATE(ENDSAL)
            DO LNS=1,24
                WRITE(HLIN(LNS),333)
            END DO

            WRITE(CLIN1,800) GTNAMES(TTSL),GIND
            WRITE(CLIN2,1001) DTSDRW,POLSTS(DTSSTS+1)
            WRITE(CLIN3,1130)
            WRITE(CLIN4,888)
            WRITE(HLIN(5),1105) (BEGSAL(I),I=7,13)
            WRITE(HLIN(6),1106) (ENDSAL(I),I=7,13)
            WRITE(CLIN7,888)
            WRITE(CLIN8,888)
            WRITE(HLIN(9),1109) CSMONY(DTSSAL,12,BETUNIT)
            WRITE(HLIN(10),1110) CSMONY(DTSREF,12,BETUNIT)
            WRITE(HLIN(11),1111) CSMONY((DTSSAL-DTSREF),12,BETUNIT)
            WRITE(HLIN(12),1112) CSMONY((DTSWON-DTSREF),12,BETUNIT)
            WRITE(HLIN(13),1113) CSMONY(DTSTAX,12,BETUNIT)
            WRITE(HLIN(14),1114) CSMONY((DTSPAD-DTSPRF),12,BETUNIT)
            WRITE(HLIN(15),1115) CSMONY(DTSPRF,12,BETUNIT)
            IF(DTSPUP.NE.0) THEN
               PRG_AMT = (DTSWON-DTSREF) - (DTSPAD-DTSPRF)
            ELSE
               PRG_AMT = 0
            ENDIF
            WRITE(HLIN(16),1116) CSMONY(PRG_AMT,12,BETUNIT)
            WRITE(HLIN(17),1117) DFLOAT(DTSBRK)/100.D0
            WRITE(CLIN18,888)
            WRITE(CLIN19,888)
            WRITE(CLIN20,888)
            WRITE(CLIN21,888)
            WRITE(CLIN22,888)

            LNS=4
            DO R = ROW, ENDROW
                IF(DTSSTA(R).EQ.0) THEN
                    WRITE(XNEW(  LNS),1199) R, HLIN(LNS)
                ELSE
                    IF(DTSWIN(R).EQ.ROWCAN) THEN
                        FINODS=100
                    ELSE
                        FINODS=DTSODS(1,R)
                        IF(DTSWIN(R).EQ.ROWLOS) FINODS=DTSODS(2,R)
                        IF(DTSWIN(R).EQ.ROWTIE) FINODS=DTSODS(3,R)
                    ENDIF
                    WRITE(XNEW(LNS),1120) R,(DTSNMS(K,1,R),K=1,3),
     *                             (DTSNMS(K,2,R),K=1,3),
     *                              FINTYP(DTSROWTYP(R)),FINRES(DTSWIN(R)+1),
     *                              FINODS/100,MOD(FINODS,100),HLIN(LNS)
                ENDIF

                LNS=LNS+1
                IF(LNS.GT.24) RETURN
            END DO

            RETURN
        ENDIF
C
C FIGURE POOL TOTALS
C
        TXPOOL=DFLOAT(DTSSAL)
        TXPOOL=TXPOOL*CALPER(SALTAX)
        TAX=IDINT(TXPOOL)
        NET=DTSSAL-TAX
C
C
        DBUF(5)=DAYCDC
        BEGSAL(5)=DTSBSD
        ENDSAL(5)=DTSESD
        CALL LCDATE(DBUF)
        CALL LCDATE(BEGSAL)
        CALL LCDATE(ENDSAL)
C
C
        DO I=1,MAXSRW
            ROWSTS(I)=DTSWIN(I)+1
            IF(DTSSTA(I).EQ.GAMCAN) ROWSTS(I)=5
        END DO
C
C
        WRITE(CLIN1,1000) GTNAMES(TTSL),GIND,
     *                    (BEGSAL(I),I=7,13),(ENDSAL(I),I=7,13)
        WRITE(CLIN2,1001) DTSDRW,POLSTS(DTSSTS+1)
        WRITE(CLIN3,1002)
        LNS=4

        DO R = ROW, ENDROW
            IF(DTSSTA(R).EQ.0) THEN
                WRITE(XNEW(  LNS),999) R
            ELSE
                RDBUF(5)=DTSDAT(R)
                CALL LCDATE(RDBUF)
C               WRITE (XNEW(  LNS),1003) R,(DTSNMS(I,1,R),I=1,3),
C     *             (DTSNMS(I,2,R),I=1,3),DTSODS(1,R)/100,MOD(DTSODS(1,R),100),
C     *              DTSODS(2,R)/100,MOD(DTSODS(2,R),100),DTSODS(3,R)/100,
C     *              MOD(DTSODS(3,R),100),DTSDAT(R),(RDBUF(I),I=7,13),
C     *              RWSTS(DTSSTA(R)+1),RESULT(ROWSTS(R))

                WRITE (XNEW(  LNS),1003) R,(DTSNMS(I,1,R),I=1,3),
     *              (DTSNMS(I,2,R),I=1,3),RTYPE(DTSROWTYP(R)),
     *               DTSODS(1,R)/100,MOD(DTSODS(1,R),100),
     *               DTSODS(3,R)/100,MOD(DTSODS(3,R),100),DTSODS(2,R)/100,
     *               MOD(DTSODS(2,R),100),DTSDAT(R),(RDBUF(I),I=7,13),
     *               RWSTS(DTSSTA(R)+1),RESULT(ROWSTS(R))
            ENDIF
            LNS=LNS+1
        END DO

        WRITE(CLIN22,1004) CMONY(DTSSAL,12,BETUNIT)

        RETURN
C
C       FORMAT STATEMENTS
C
333     FORMAT(31(' '))
800     FORMAT('* ',A8,1X,I1,' *',33(' '))
888     FORMAT(80(' '))
999     FORMAT(1X,I2.2,77(' '))
1000    FORMAT(A8,1X,I1,7A2,' -',7A2)
1001    FORMAT('  Event code- ',I4,20(' '),'* ',A17,' *')
1002    FORMAT('Row',2X,'Team names',18X,'Typ',2X,
     *   '1',5X,'X',5X,'2',4X,'Cdc',
     *    2X,'Date',9X,'Sts',1X,'Res')
1003    FORMAT(1X,I2.2,2X,3A4,' - ',3A4,2X,A1,1X,
     *   I2,'.',I2.2,1X,I2,'.',I2.2,
     *   1X,I2,'.',I2.2,1X,I4,7A2,1X,A3,2X,A1)
1004    FORMAT(4(' '),'Total pool ',A12,26(' '))
1105    FORMAT('Beginning Sales ',7A2,1(' '))
1106    FORMAT('Ending    Sales ',7A2,1(' '))
1109    FORMAT('Total Sales    ',2(' '),A12,' ')
1110    FORMAT('Total Refunds  ',2(' '),A12,' ')
1111    FORMAT('Net Sales      ',2(' '),A12,' ')
1112    FORMAT('Winning Amount ',2(' '),A12,' ')
1113    FORMAT('Winning Tax    ',2(' '),A12,' ')
1114    FORMAT('Winners Paid   ',2(' '),A12,' ')
1115    FORMAT('Refunds paid   ',2(' '),A12,' ')
1116    FORMAT('Amount Purged  ',2(' '),A12,' ')
1117    FORMAT('Rounding Pot   ',2(' '),F12.2,' ')
1120    FORMAT(1(' '),I2.2,1(' '),2A4,A1,' - ',2A4,A1,
     *   2(' '),2X,A3,3X,A3,2(' '),I2,'.',I2.2,4(' '),A31)
1130    FORMAT('Row',1(' '),'Team names',15(' '),'Typ',3(' '),
     *   'Res',3(' '),'Odds',4(' '),'Game totals',19(' '))
1199    FORMAT(1(' '),I2.2,46(' '),A31)
3000    FORMAT('Enter !',A8,' game index')
3010    FORMAT(A8,1X,I1,' game not active')
3020    FORMAT(5A4,' open error ',I4)
3030    FORMAT(5A4,' read error ',I4,' record > ',I4)
3040    FORMAT(A8,1X,I1,' game not initialized event > ',I4)

        END
