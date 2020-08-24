C VALDIS.FOR
C
C V14 03-JAN-2000 UXN VLW and %PURGE file reading added.
C V13 25)NOV-1999 UXN WAP ADDED FOR TEBE.
C V12 19-NOV-1999 OXK MOVED VALDI TO A SEPARATE MODULE
C V11 17-MAY-1999 UXN SUPER TRIPLE ADDED.
C V10 19-JAN-1999 UXN TEBE WINNERS ADDED.
C V09 02-OCT-1997 WXM INITIAL RELEASE FOR FINLAND
C V08 05-DEC-1996 SLK MULTI-KENO RELEASE
C V07 26-SEP-1995 GLS RELEASED FOR ESTONIA BINGO
C V06 12-JUN-1995 SLK ADDED INFO ON FROZEN PRIZES IN KENO TICKET
C V05 07-MAR-1995 SLK UPDATED FOR KENO ACCORDING TO CZECH 
C V04 19-APR-1994 SLK INITIAL RELEASE FOR ESTONIA
C ----------------------------------------------------------
C V03 11-FEB-1994 SMH Added KENO
C V02 18-SEP-1993 JJS INITIAL RELEASE FOR CZECHO-SLOVAKIA
C V01 18-AUG-1992 WLM INITIAL RELEASE FOR NETHERLANDS
C
C PROGRAM TO SCAN VLF AND DISPLAY REQUESTED RECORD
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        OPTIONS /CHECK=NOOVERFLOW
        PROGRAM VALDIS
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
	INCLUDE 'INCLIB:PRGREC.DEF'
C
        INTEGER*4 TUBSIZ
        PARAMETER (TUBSIZ=I4BUCSIZ*7)
        INTEGER*4 VLFBUF(TUBSIZ)
C
        INTEGER*4 I, NUM, KK
        INTEGER*4 ST, L, J
        INTEGER*4 EXT, CNTR, CNTD, OPT
        INTEGER*4 DIV_FILT, DRW_FILT, LU
C
        INTEGER*4 MAXCRIT, MAXCDC, MAXDRAW
        PARAMETER (MAXCRIT=18)          !
        PARAMETER (MAXCDC=5000)         !ARBITRALLY SET
        PARAMETER (MAXDRAW=9999)        !ARBITRALLY SET
        INTEGER*4 CRIT(2,MAXCRIT)
C
        LOGICAL DISPLAY, FILE
	INTEGER*4 BLK,IND,LENGTH,FDB(7)
C
        INTEGER*4 VLFNAM(5)
        CHARACTER*20 VLFNAMC
        EQUIVALENCE (VLFNAM(1),VLFNAMC)
	LOGICAL SEQUENTIAL
	INTEGER*4 IO_SIZE, BLK_MAXIND

	INTEGER*4 VLWREC(72*113)    
	INTEGER*4 VLWBUF(72,113)
	EQUIVALENCE(VLWREC,VLWBUF)
	INTEGER*4 FILTYP
C
        CHARACTER * 20 USERPASWRD     ! USER ENTER PASSWORD
C
C TO RUN VALDIS TASK USER NEEDS TO KNOW THE PASSWORD
C
        CALL CLRSCR(5)
	TYPE *, IAM()
	TYPE *, IAM(), '* * * * * * * * * * * * * * * * * * * * * * *' 
	TYPE *, IAM(), '* Valdis Task Needs Password Autoritation.  *'
	TYPE *, IAM(), '* * * * * * * * * * * * * * * * * * * * * * *' 
	TYPE *, IAM()
        CALL PASSWORD(20, USERPASWRD)
        IF(USERPASWRD(1:11) .NE. 'VALDISBARNA') THEN
  	  TYPE *, IAM()
	  TYPE *, IAM(), 'Invalid Password Entered ...'
	  TYPE *, IAM()
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
	TYPE *, IAM()
C
C GET INPUT VALIDATION FILE NAME
C
50      CONTINUE
	SEQUENTIAL = .FALSE.
        CALL FASTSET(0,VLFNAM,5)
        CALL PRMTEXT('Enter input validation file name  ',VLFNAMC, ST)
	IF(ST.LE.0) CALL GSTOP(GEXIT_OPABORT)

	CALL PRMNUM('Enter file type 1-VLF, 2-VLW, 3-PURGE',
     *               FILTYP, 1, 3, ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
	IF(FILTYP.EQ.2.OR.FILTYP.EQ.3) SEQUENTIAL = .TRUE.
	IF(FILTYP.EQ.2) THEN
	    IO_SIZE = 8192*4
	    BLK_MAXIND = 113
	ELSEIF(FILTYP.EQ.3) THEN
	    IO_SIZE = PRGSEC*256
	    BLK_MAXIND = PRGBLC 
	ENDIF	   
C
70      CONTINUE
        CALL FASTSET(-1,CRIT,2*MAXCRIT)
C
C DEFINE SEARCH CRITERIA
C
        KK=0
        CALL CLRSCR(6)
        WRITE(6,800)
        L = 0
        NUM = 0
        DO 30 I=1,MAXCRIT
          IF(I.EQ.2) L=I
          CALL INPNUM('Enter criterion number (E-exit): ',
     *               CRIT(1,I),L,MAXCRIT,EXT)
          IF(EXT.NE.0.OR.CRIT(1,I).LE.1) GOTO 40
          IF(CRIT(1,I).EQ.2) THEN
            WRITE(6,720)
            CALL INPNUM('Enter status',CRIT(2,I),0,VSBNKM,EXT)
          ELSEIF(CRIT(1,I).EQ.3) THEN
            CALL INPNUM('Enter game number',CRIT(2,I),1,MAXGAM,EXT)
          ELSEIF(CRIT(1,I).EQ.4) THEN
            CALL INPNUM('Enter game type',CRIT(2,I),1,MAXTYP,EXT)
          ELSEIF(CRIT(1,I).EQ.5) THEN
            CALL INPNUM('Enter selling CDC',CRIT(2,I),1,MAXCDC,EXT)
          ELSEIF(CRIT(1,I).EQ.6) THEN
            CALL INPNUM('Enter selling terminal',CRIT(2,I),1,NUMAGT,EXT)
          ELSEIF(CRIT(1,I).EQ.7) THEN
            CALL INPNUM('Enter cashing CDC',CRIT(2,I),1,MAXCDC,EXT)
          ELSEIF(CRIT(1,I).EQ.8) THEN
            CALL INPNUM('Enter cashing terminal',CRIT(2,I),1,NUMAGT,EXT)
          ELSEIF(CRIT(1,I).EQ.9) THEN
            CALL INPNUM('Enter claim CDC',CRIT(2,I),1,MAXCDC,EXT)
          ELSEIF(CRIT(1,I).EQ.10) THEN
            CALL INPNUM('Enter claim terminal',CRIT(2,I),1,NUMAGT,EXT)
          ELSEIF(CRIT(1,I).EQ.11) THEN
            CALL INPNUM('Enter expiry draw',CRIT(2,I),1,MAXDRAW,EXT)
          ELSEIF(CRIT(1,I).EQ.12) THEN
            CALL INPMONY('Enter ticket pay amount',CRIT(2,I),VALUNIT,EXT) 
          ELSEIF(CRIT(1,I).EQ.13) THEN             
            CALL INPNUM('Enter prize detail draw #   ',CRIT(2,13),
     *                    1,MAXDRAW,EXT)
          ELSEIF (CRIT(1,I).EQ.14) THEN             
            CALL INPNUM('Enter prize division',CRIT(2,14),1,99,EXT)
          ELSEIF (CRIT(1,I).EQ.15) THEN
            CALL INPMONY('Pay amount greater than',CRIT(2,I),
     *                    VALUNIT,EXT) 
          ELSEIF (CRIT(1,I).EQ.16) THEN
            CALL INPNUM('Bonus draw winner flag',CRIT(2,I),0,1,EXT)
          ELSEIF (CRIT(1,I).EQ.17) THEN
            CALL INPNUM('Lotto Extra winners',CRIT(2,I),0,1,EXT)
          ELSEIF (CRIT(1,I).EQ.18) THEN
            CALL INPNUM('Enter [0-old TEBE,1-VRU,2-WWW,3-WAP]',
     *                  CRIT(2,I),0,3,EXT)
          ENDIF
          IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
C
          NUM = NUM+1
30      CONTINUE
40      CONTINUE
        DISPLAY=.FALSE.
        FILE=.FALSE.
        LU=6
        WRITE(6,840)
        CALL INPNUM('Select option',OPT,1,3,EXT)
        IF(OPT.EQ.1) DISPLAY=.TRUE.
        IF(OPT.EQ.2) THEN
          FILE=.TRUE.
          LU=7
          CALL ROPEN('VALDIS.REP',LU,ST)
          IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C DEFINE CRITERIA VALUES
C
        IF(CRIT(1,1).EQ.0) THEN
          NUM=1
        ELSE IF(CRIT(1,1).EQ.1) THEN
          CALL CRITSER(V4BUF)
          CALL LOGVAL(VALREC,V4BUF)
          CALL VALDI(VALREC,LU) 
          CALL GSTOP(GEXIT_SUCCESS)
        ELSE
          IF(NUM.EQ.0) CALL GSTOP(GEXIT_SUCCESS)
          DO 400, I=1,NUM
            IF((CRIT(1,I).EQ.3.AND.GNTTAB(1,CRIT(2,I)).EQ.TKIK).OR.
     *         (CRIT(1,I).EQ.4.AND.CRIT(2,I).EQ.TKIK)) GOTO 500
            IF(CRIT(1,I).EQ.13.OR.CRIT(1,I).EQ.14) GOTO 450
400       CONTINUE
450       CONTINUE
          CALL CLRSCR(6)
          WRITE(6,810)
          CALL INPNUM('Enter joker option',KK,1,3,EXT)
          IF(EXT.LT.0) KK=1
500       CONTINUE
        ENDIF
C
C SCAN VLF FILE AND CHECK CRITERIA
C
C
C OPEN INPUT VALIDATION FILE
C
	IF(SEQUENTIAL) THEN
	    CALL OPENW(2,VLFNAM(1),4,0,0,ST)
            CALL IOINIT(FDB,2,IO_SIZE)
            IF(ST.NE.0) CALL FILERR(VLFNAM(1),1,ST,0)
	ELSE
      	    CALL IOPEN(VLFNAM(1),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
      	    IF(ST.NE.0) CALL FILERR(VLFNAM(1),1,ST,0)
      	    CALL ITUBSIZE(VLF,TUBSIZ)
        ENDIF
        CNTR = 0
        CNTD = 0

	BLK  = 0
90	CONTINUE
	IF(SEQUENTIAL) THEN
	   BLK = BLK + 1
	   IND = 1
	   IF(FILTYP.EQ.2) THEN	
              CALL READW(FDB,BLK,VLWREC,ST)
           ELSEIF(FILTYP.EQ.3) THEN
              CALL READW(FDB,BLK,UPREC,ST)
	   ENDIF
           IF(ST.NE.0) CALL FILERR(VLFNAM(1),3,ST,BLK)
	ENDIF
100     CONTINUE
	IF(SEQUENTIAL) THEN
	   IF(FILTYP.EQ.2) THEN
              CALL LOGVAL(VALREC,VLWBUF(1,IND))
	      LENGTH = 1
	   ELSEIF(FILTYP.EQ.3) THEN
              CALL LOGVAL(VALREC,UPBUF(1,IND))
              LENGTH=ISHFT(UPBUF(VFSSER,IND),-30)+1
	   ENDIF
           IF(VALREC(VSSER).EQ.0) GOTO 1000
	ELSE
      	    CALL ISREAD(V4BUF,VLF,VLFBUF,ST)
      	    IF(ST.EQ.ERREND) GOTO 1000
      	    IF(ST.NE.0) CALL FILERR(VLFNAM(1),2,ST,0)
            CALL LOGVAL(VALREC,V4BUF)
        ENDIF
        CNTR = CNTR+1
C
        CALL DLOGVAL(VALREC,VDETAIL)
C
C CHECK IF PRODUCT OF NON-ZERO CRITERIA MET
C
        DO 120 I=1,NUM
          IF(CRIT(1,I).EQ.0) GOTO 120
          IF(CRIT(1,I).EQ.2.AND.VALREC(VSTAT).EQ.CRIT(2,I)) GOTO 120
          IF(CRIT(1,I).EQ.3.AND.VALREC(VGAM).EQ.CRIT(2,I)) GOTO 120
          IF(CRIT(1,I).EQ.4.AND.VALREC(VGTYP).EQ.CRIT(2,I)) GOTO 120
          IF(CRIT(1,I).EQ.5.AND.VALREC(VSCDC).EQ.CRIT(2,I)) GOTO 120
          IF(CRIT(1,I).EQ.6.AND.VALREC(VSTER).EQ.CRIT(2,I)) GOTO 120
          IF(CRIT(1,I).EQ.7.AND.VALREC(VCCDC).EQ.CRIT(2,I)) GOTO 120
          IF(CRIT(1,I).EQ.8.AND.VALREC(VCTER).EQ.CRIT(2,I)) GOTO 120
          IF(CRIT(1,I).EQ.9.AND.VALREC(VLCDC).EQ.CRIT(2,I)) GOTO 120
          IF(CRIT(1,I).EQ.10.AND.VALREC(VLTER).EQ.CRIT(2,I)) GOTO 120
          IF(CRIT(1,I).EQ.11.AND.VALREC(VEXP).EQ.CRIT(2,I)) GOTO 120
          IF(CRIT(1,I).EQ.12.AND.VALREC(VPAMT).EQ.CRIT(2,I)) GOTO 120
          IF (CRIT(1,I).EQ.13) THEN
            DO 200 J=1,VALREC(VPZOFF)
              IF((KK.EQ.2.AND.VDETAIL(VKIK,J).EQ.1).OR.
     *           (KK.EQ.3.AND.VDETAIL(VKIK,J).NE.1)) GOTO 200
              DRW_FILT=VDETAIL(VDRW,J)
              IF (DRW_FILT.EQ.CRIT(2,13)) GOTO 120
200         CONTINUE         
          ENDIF
          IF (CRIT(1,I).EQ.14) THEN
            DO 300 J=1,VALREC(VPZOFF)
              IF((KK.EQ.2.AND.VDETAIL(VKIK,J).EQ.1).OR.
     *           (KK.EQ.3.AND.VDETAIL(VKIK,J).NE.1)) GOTO 300
              DIV_FILT=VDETAIL(VDIV,J)
              IF (DIV_FILT.EQ.CRIT(2,14)) GOTO 120
300         CONTINUE         
          ENDIF
          IF (CRIT(1,I).EQ.15.AND.VALREC(VPAMT).GT.CRIT(2,I)) GOTO 120
          IF(CRIT(1,I).EQ.16) THEN
             IF(CRIT(2,I).GT.0) THEN
                DO 410 J=1,VALREC(VPZOFF)
                   IF(VDETAIL(VBDR,J).EQ.CRIT(2,I)) GOTO 120
410             CONTINUE
             ELSE
                GOTO 120
             ENDIF
          ENDIF
          IF(CRIT(1,I).EQ.17) THEN
             IF(VALREC(VGAM).EQ.20) GOTO 100   !skip bingo
             DO 420 J=1,VALREC(VPZOFF)
                IF(VDETAIL(VSUB,J).EQ.CRIT(2,I)) GOTO 120
420          CONTINUE
          ENDIF
C	  IF(CRIT(1,I).EQ.18.AND.VALREC(VBNKID).GT.10000000.AND.
C     *       VALREC(VFTBSF).EQ.CRIT(2,I)) GOTO 120
        GOTO 100
120     CONTINUE
C
C
        CNTD = CNTD+1
        IF(DISPLAY) THEN
          CALL VALDI(VALREC,LU)
          CALL PRMYESNO('Continue? (Y/N/E)',EXT)
          IF(EXT.EQ.2) GOTO 1000
          IF(EXT.EQ.3) GOTO 2000
        ELSEIF(FILE) THEN
          CALL VALDI(VALREC,LU)
        ELSE
          WRITE(6,850) CNTR, CNTD
        ENDIF
C
C
	IF(SEQUENTIAL) THEN
	   IND = IND + LENGTH
	   IF(IND.GT.BLK_MAXIND) GOTO 90
	ENDIF

        GOTO 100
C
1000    CONTINUE
        WRITE(6,850) CNTR, CNTD
	
	IF(SEQUENTIAL) THEN
	    CALL CLOSEFIL(FDB)
	ELSE
      	    CALL ICLOSE(VLF,VLFBUF,ST)
      	    IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),4,ST,0)
      	    IF(FILE) CALL USRCLOS1(7)
	ENDIF
C
        CALL PRMYESNO('New criteria? (Y/N/E)',EXT)
        IF(EXT.NE.1) GOTO 2000
        CALL PRMYESNO('New validation file? (Y/N/E)',EXT)
        IF(EXT.EQ.1) GOTO 50
        IF(EXT.EQ.2) GOTO 70
C
2000    CONTINUE
        CALL GSTOP(GEXIT_SUCCESS)
C
C     ==================== FORMAT STATEMENTS ===================
C
720     FORMAT(1X,'Validation status codes:',//,5X
     *    ' 0 - VNOWIN     5 - VCXL      10 - VCLAM     15 - VBANK ' /,5X,
     *    ' 1 - VUNCSH     6 - VHOLD     11 - VCLAMX    16 - VSBNK ',/,5X,
     *    ' 2 - VCASH      7 - VNOPAY    12 - VPRPAY    17 - VSBNKM',/,5X,
     *    ' 3 - VCASHX     8 - VNOPRZ    13 - VPPNPZ ',/,5X,
     *    ' 4 - VDEL       9 - VPOST     14 - VPRPOST',/)
800     FORMAT(1X,'Selection criteria (ANDed):',//,5X,
     *                 ' 0 - all tickets',/,5X,
     *                 ' 1 - wager serial number (one only)',/,5X,
     *                 ' 2 - validation status',/,5X,
     *                 ' 3 - game number',/,5X,
     *                 ' 4 - game type',/,5X,
     *                 ' 5 - selling  CDC date',/,5X,
     *                 ' 6 - selling  terminal',/,5X,
     *                 ' 7 - cashing  CDC date',/,5X,
     *                 ' 8 - cashing  terminal',/,5X,
     *                 ' 9 - claiming CDC date',/,5X,
     *                 '10 - claiming terminal',/,5X,
     *                 '11 - expiry draw',/,5X,
     *                 '12 - regular pay amount',/,5X,
     *                 '13 - prize detail draw #',/,5X,
     *                 '14 - prize division',/,5X,
     *                 '15 - total prize amount greater than',/,5X,
     *                 '16 - winners with bonus draws'/,5X,
     *                 '17 - Lotto extra winners',/,5X,
     *                 '18 - TEBE winners'/)
8801    FORMAT(A)
810     FORMAT(1X,'Selecting joker prizes: ',/,5X,
     *                 '1 - Joker prizes scanned as others',/,5X,
     *                 '2 - Joker prizes skipped',/,5X,
     *                 '3 - Only joker winners selected',/)
840     FORMAT(1X,'Output modes:',/,5X,
     *                 '1 - display on the screen',/,5X,
     *                 '2 - write to file',/,5X,
     *                 '3 - count only',/)
850     FORMAT('+Tickets scanned:',I8,10X,'Tickets selected:',I8)
        END
C
C
C
        OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE CRITSER(V4BUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
C
C
        INTEGER*4 KEY(2), ST1, K
        INTEGER*4 ST, SER, CDC
        INTEGER*4 SCRAM, CHECK, CHKERR, EXT, INPVER
        INTEGER*4 BIGBUF(2500)
C
C
C GET DATE OF THE WAGER
C
        WRITE(5,700) IAM()
        CALL INPDAT(CDC,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_SUCCESS)
C
C GET SERIAL NUMBER
C
        CALL INPNUM('Enter External serial number: ',
     *               SCRAM,1,99999999,EXT)
        IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
        CALL INPNUM('Enter Check digits          : ',
     *               CHECK,0,999,EXT)
        IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
        CHKERR=INPVER(CDC,SCRAM,SER,CHECK)
        IF(CHKERR.NE.0) THEN
          WRITE(5,710) IAM()
          CALL GSTOP(GEXIT_OPABORT)
        ENDIF
C
C READ SPECIFIED TRANSACTION FROM THE VALIDATION FILE
C
        IF(CDC.LT.0) CDC=1
        IF(SER.LE.0) SER=1
        KEY(1)=CDC
        KEY(2)=SER
        CALL IOPEN(SFNAMES(1,VLF),1,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
        IF(ST.NE.0) THEN
          WRITE(5,800) (SFNAMES(K,VLF),K=1,5),ST
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
        CALL IREAD(KEY,V4BUF,1,ST)
        CALL ICLOSE(1,BIGBUF,ST1)
        IF(ST.NE.0) THEN
          WRITE(5,810) CDC,SER
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        RETURN
C
700     FORMAT(1X,A,'Specify wager date.')
710     FORMAT(1X,A,'Wrong Check digits.')
800     FORMAT(1X,5A4,' open error> ',I4)
810     FORMAT(1X,' Record not found for cdc - ',I4,' serial - ',I8)
C
          END
