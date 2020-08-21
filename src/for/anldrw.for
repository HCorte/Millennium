C ANLDRW.FOR
C
C V03 01-FEB-2000 UXN Fractions changed.
C V02 21-MAY-1999 UXN MAXGAM changes.
C V01 16-SEP-1993 HXN Initial revision.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK/EXT
	PROGRAM ANLDRW
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
C
C THE FIRST INDEX IS STATUS     (TSTAT)
C THE SECOND INDEX IS FILE TYPE (TFIL)
C THE THIRD  INDEX IS GAME      (TGAM)
C THE FOURTH INDEX IS TRANSACTION TYPE (TTYP), EXCEPT THAT
C SPECIAL SERVICE XACTIONS ARE BROKEN DOWN INTO ADDITIONAL
C TYPES BY ADDING 7 TO TSFUN
C
	INTEGER*4  TYPSON,TYPDLL,TYPRPT,TYPRPN
	PARAMETER (TYPSON=8)           !SIGNON
	PARAMETER (TYPDLL=9)           !DOWNLOAD
	PARAMETER (TYPRPT=10)          !REPORT
	PARAMETER (TYPRPN=11)          !REPRINT
C
	INTEGER*4  MAXNDX1, MAXNDX2, MAXNDX3, MAXNDX4
	PARAMETER (MAXNDX1=FRAC)   !max valid transaction status    FRAC=11
	PARAMETER (MAXNDX2=CDEAD)  !max valid file type             CDEAD=5
	PARAMETER (MAXNDX3=MAXGAM) !max valid games
	PARAMETER (MAXNDX4=TCMD)   !max valid transaction type ...  TCMD=8
C
	INTEGER*4  NRMCNT(MAXNDX1+1,MAXNDX2+1,MAXGAM+1,MAXNDX4+1)      !COUNT
	INTEGER*4  NRMAMT(MAXNDX1+1,MAXNDX2+1,MAXGAM+1,MAXNDX4+1)      !AMOUNT

	INTEGER*4  FDB(7)
	INTEGER*4  BUF(64*128)   !8192

	INTEGER*4  I
	INTEGER*4  J
	INTEGER*4  K
	INTEGER*4  L
	INTEGER*4  ST
	INTEGER*4  TOTERR
	INTEGER*4  PAGE
	INTEGER*4  EOFCNT
	INTEGER*4  THSSER
	INTEGER*4  BLK
	INTEGER*4  OFF
	INTEGER*4  RTYP
	INTEGER*4  LSTRTP
	INTEGER*4  BEGOFF
	INTEGER*4  ENDOFF
	INTEGER*4  ERRFLG
	INTEGER*4  EMPTYBLK
	INTEGER*4  EMPTYOFF
	INTEGER*4  EMPTYCNT
	INTEGER*4  QNDX1
	INTEGER*4  QNDX2
	INTEGER*4  QNDX3
	INTEGER*4  QNDXK3
	INTEGER*4  QNDX4
	INTEGER*4  NDX1
	INTEGER*4  NDX2
	INTEGER*4  NDX3
	INTEGER*4  NDX4
	INTEGER*4  AMT
	INTEGER*4  KAMT
	INTEGER*4  PASS
	INTEGER*4  POFF
	INTEGER*4  PMAX
	INTEGER*4  UNIT
	INTEGER*4  AMT_PENNY/0/,KAMT_PENNY/0/

	CHARACTER*24 INPNAM
	CHARACTER*10 XXREPNAM/'ANLDRW.REP'/
C
	CHARACTER*20 NDX1NM(MAXNDX1)
	DATA NDX1NM/'        GOOD        ','        VOID        ',
     *	            ' INTERNALLY DELETED ','    NEW EXCHANGE    ',
     *	            '        CASHED      ','      REJECTED      ',
     *	            '      EXCHANGED     ',' CASHED WITH EXCHNG ',
     *	            '       CLAIMED      ',' CLAIMD WITH EXCHNG ',
     *	            '        FRAC        '/
C
	CHARACTER*4 NDX2NM(MAXNDX2)
	DATA NDX2NM/'ERLY','LATE','CARY','POST','DEAD'/
C
	CHARACTER*12 NDX4NM(MAXNDX4)
	DATA NDX4NM/'wagers      ','cancels     ','int cancels ',
     *	            'validations ','claims      ','refund      ',
     *	            'spe func    ','commands    '
     *             /
C	DATA NDX4NM/'wagers      ','cancels     ','int cancels ',
C     *	            'validations ','claims      ','refund      ',
C     *	            'spe func    ','commands    ','sign-offs   ',
C     *	            'downloads   ','getkey      ','sales report',
C     *              'game reports','reprints    ','adjustments ',
C     *              'slochk      ','news message','jackpot rpts',
C     *              'X2X specials','loopback    ','term stats  ',
C     *              'faults      ','last 4 win #','bonus win # ',
C     *              'ticket msgs ','other ttyp  '/
C
C
	CHARACTER BELL/Z07/

	LOGICAL CHKBLK
	LOGICAL ANYGUD
	LOGICAL CRYFIL   /.FALSE./
	LOGICAL DRWFIL
	LOGICAL ENDBLK



	INTEGER*4 FRAC_CNT (MAXGAM)
	INTEGER*4 FINANCE_CNT  (MAXNDX1+1,MAXNDX2+1,MAXGAM+1,MAXNDX4+1),!VALID COUNT
     *            FINANCE_AMT  (MAXNDX1+1,MAXNDX2+1,MAXGAM+1,MAXNDX4+1) !VALID COUNT


C *********************************************
C
	CALL COPYRITE
	TYPE *,'<<<<<<<<<< ANLDRW V01.2 >>>>>>>>>>'


C CLEAR TOTALS
C ------------
	DO 110 L= 1, MAXNDX4+1
	DO 110 K= 1, MAXGAM+1
	DO 110 J = 1, MAXNDX2+1
	DO 110 I = 1, MAXNDX1+1
	   NRMCNT (I,J,K,L)=0
	   NRMAMT (I,J,K,L)=0
	   FINANCE_CNT(I,J,K,L)=0
	   FINANCE_AMT(I,J,K,L)=0
110	CONTINUE

	TOTERR=0


C OPEN REPORT FILE
C ----------------
	PAGE=0
	CALL ROPEN(XXREPNAM,6,ST)
	IF(ST.NE.0)THEN
	  TYPE *,'cannot open ',XXREPNAM,' status ',ST
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF

	CALL TITLE('ANALYSIS OF '//INPNAM,'ANLDRW  ',01,6,PAGE,
     *	            DAYCDC)
	WRITE(6,501)
501	FORMAT(///,X,30('*'),' ANOMALIES ',30('*'),/)



C OPEN DRAW FILE
C ------------
200	CONTINUE

 
	CALL WIMG(5,'Input file or device:             ')
	ACCEPT 201,INPNAM
201	FORMAT(A24)


	CALL OPENX(1,INPNAM,0,0,0,ST)
	DRWFIL=.TRUE.
	CALL IOINIT(FDB,1,128*256)




C NOW, SCAN DRW FILE
C -----------------
	CHKBLK = .TRUE.
	EOFCNT = 0
	THSSER = 0
	BLK    = 0

C       READ NEXT BLOCK
C       ---------------
1000	CONTINUE

	IF (MOD(BLK,1000).EQ.0) TYPE*,' ANLDRW in progress...',BLK

	BLK=BLK+1

	  CALL READW(FDB,BLK,BUF,ST)
	  IF(ST.NE.0)THEN
	    IF(ST.EQ.'88'X) THEN
	      TYPE *,'End of file encountered'
	    ELSE IF(ST.EQ.'90'X) THEN
	      TYPE*,' End of medium '
	    ELSE
	      TYPE*,'cannot read block ',BLK,' error = ',ST
	    ENDIF
	    CALL USRCLOS1(     1)
C*          IF(.NOT.CRYFIL)TYPE *,'CANNOT READ BLK ',BLK,' ERROR = ',ST
	    GO TO 6000
	  ENDIF





	EMPTYBLK=0
	EMPTYOFF=0
	EMPTYCNT=0
	ANYGUD=.FALSE.
	LSTRTP=0
	BEGOFF=LHDR+1
	IF(DRWFIL .OR. CRYFIL)BEGOFF=1
	ENDOFF=DBLOCK
	IF(DRWFIL)ENDOFF=DBLOCK*4

	OFF=BEGOFF-LREC
2000	CONTINUE
	OFF=OFF+LREC
	IF(OFF.GT.ENDOFF)GOTO 5000


	  ENDBLK=.FALSE.
	  THSSER=THSSER+1


	CALL ILBYTE(RTYP, BUF(OFF),(LREC*4)-1)
	RTYP=IAND(RTYP,'0F'X)

	ERRFLG=0

	IF(RTYP.EQ.0)THEN                    !IF 0, SHD BE EMPTY
	  DO 2010 K=0,LREC-1
	    IF(BUF(OFF+K).NE.0)ERRFLG=1
2010	  CONTINUE

	  IF(.NOT.ENDBLK .AND. .NOT.DRWFIL .AND. .NOT.CRYFIL)THEN
	    EMPTYCNT=EMPTYCNT+1
	    IF(EMPTYBLK.EQ.0)THEN
	      EMPTYBLK=BLK
	      EMPTYOFF=OFF
	    ENDIF
	  ENDIF

	ELSE IF(RTYP.EQ.LREG .OR. RTYP.EQ.LONE)THEN
	  IF(ENDBLK)ERRFLG=2
	  IF(LSTRTP.EQ.LTWO)ERRFLG=3

	ELSE IF(RTYP.EQ.LTWO)THEN
	  IF(LSTRTP.NE.LONE)ERRFLG=4

	ELSE IF(RTYP.EQ.LEND)THEN
	  IF(LSTRTP.NE.LONE .AND. LSTRTP.NE.LTWO)ERRFLG=5

	ELSE
	  ERRFLG=6
	ENDIF



	IF(ERRFLG.NE.0)THEN
	  WRITE(6,2002)BLK,OFF,RTYP,LSTRTP,ERRFLG
2002	  FORMAT(X,'BLK ',I8,'/',I4,' BAD RTYP=',I2,' LAST=',I2,
     *	           ' ERRFLG=',I2)
	  TOTERR=TOTERR+1
	  LSTRTP=0
	  GOTO 2000
	ENDIF
C
	LSTRTP=RTYP
	IF(RTYP.EQ.0)THEN
	  EOFCNT = EOFCNT+1
	  IF(.NOT.CRYFIL .AND. EOFCNT.GT.2000)GOTO 6000
	ELSE
	  EOFCNT = 0
	ENDIF
C
	IF(RTYP.NE.LREG .AND. RTYP.NE.LONE)GOTO 2000
C


	CALL LOGTRA(TRABUF,BUF(OFF))


C CHECK FOR END OF FILE
C ---------------------
	IF(TRABUF(TSTAT).EQ.NUSD) THEN
	  IF(RTYP.NE.0)THEN
	    ERRFLG=7
	    WRITE(6,2002)BLK,OFF,RTYP,LSTRTP,ERRFLG
	    TOTERR=TOTERR+1
	    LSTRTP=0
	  ENDIF
	  GOTO 2000
	ENDIF



C Display approximate serial number, when there is hole.
C -----------------------------------------------------
C	IF(EMPTYCNT.NE.0)THEN
C	  TEMPBLK=EMPTYBLK-1        !determine the block
C	  TEMPBLK=TEMPBLK*LBLK      !125 words in block
C	  TEMPOFF=(EMPTYOFF-1)/LREC !from word offset determine record
C	  TEMPSER=TEMPBLK+TEMPOFF   !serial in question
C	  TYPE*,' Approximate serial number of hole ',TEMPSER
C	  WRITE(6,2003)EMPTYCNT,EMPTYBLK,EMPTYOFF,TEMPSER
C 2003	  FORMAT(X,I8,' EMPTY RECORDS STARTING AT BLK ',I8,'/',I4,
C     *	  ' Approximate serial number ',I8)
C	  EMPTYCNT=0
C	  EMPTYBLK=0
C	  EMPTYOFF=0
C	ENDIF



	EOFCNT=0
	ANYGUD=.TRUE.





C CHECK FOR VALID VALUES OF TSTAT,TFIL,TGAM 
C -----------------------------------------
	QNDX1 = MIN (MAXNDX1,TRABUF(TSTAT))
	IF (QNDX1.LT.1) QNDX1 = MAXNDX1+1

	QNDX2=MIN(MAXNDX2,TRABUF(TFIL))
	IF(QNDX2.LT.1)QNDX2=MAXNDX2+1

	QNDX3=MIN(MAXGAM,TRABUF(TGAM))
	IF(QNDX3.LT.1)QNDX3=MAXGAM+1

	IF (TRABUF(TTYP).EQ.TVAL) THEN
 	    QNDXK3=MIN(MAXGAM,TRABUF(TVKGME))
	ELSE                                     ! (TRABUF(TTYP).EQ.TWAG) THEN
 	    QNDXK3=MIN(MAXGAM,TRABUF(TWKGME))
	ENDIF

C*** 	QNDXK3=MIN(MAXGAM,TRABUF(TWKGME))
	IF(QNDXK3.LT.1)QNDXK3=MAXGAM+1

	QNDX4 = TRABUF(TTYP)
	IF(QNDX4.GT.TCMD .OR. QNDX4.LT.1)THEN
	  QNDX4 = MAXNDX4+1
	ENDIF


C****************************************************************

	AMT=0
	KAMT=0

	IF(TRABUF(TTYP).EQ.TWAG)THEN

	   IF((TRABUF(TSTAT).EQ.GOOD.AND.TRABUF(TWFFLG).NE.1) .OR.
     *         TRABUF(TSTAT).EQ.FRAC) THEN
	       FRAC_CNT(TRABUF(TGAM)) = FRAC_CNT(TRABUF(TGAM)) + 1
           ENDIF


	    IF(QNDX2.EQ.MAXNDX2+1 .OR. QNDX3.EQ.MAXGAM+1)THEN
	      IF(TRABUF(TSTAT).NE.REJT)THEN
		WRITE(6,3001)BLK,OFF,TRABUF(TSER),'TGAM or TFIL',
     *                     TRABUF(TGAM),TRABUF(TFIL),TRABUF(TSTAT)
		TOTERR=TOTERR+1
	      ENDIF
	    ELSEIF(TRABUF(TSTAT).EQ.GOOD.OR.
     *             TRABUF(TSTAT).EQ.FRAC) THEN
	        AMT= TRABUF(TWAMT)*TRABUF(TWDUR)
	        KAMT=TRABUF(TWKAMT)*TRABUF(TWKDUR)
                IF(TRABUF(TFAMTFLG).EQ.1) THEN
                   CALL ADD_PENNY(AMT_PENNY, AMT, TRABUF(TNFRAC))
                   CALL ADD_PENNY(KAMT_PENNY, KAMT, TRABUF(TNFRAC))
                ENDIF		
	    ENDIF
 3001	FORMAT(X,'BLK ',I8,'/',I4,' SRL ',I9,X,A,I5,I5,I5)

	ELSE
	  WRITE(6,3001)BLK,OFF,TRABUF(TSER),'TTYP',TRABUF(TTYP)
	  TOTERR=TOTERR+1
	ENDIF


C--------------------------------------------
	IF (TRABUF(TGAM).EQ.QNDXK3) THEN  !that is the jokeri game
	    NRMCNT(QNDX1,QNDX2,QNDXK3,QNDX4)=NRMCNT(QNDX1,QNDX2,QNDXK3,QNDX4)+1
	    NRMAMT(QNDX1,QNDX2,QNDXK3,QNDX4)=NRMAMT(QNDX1,QNDX2,QNDXK3,QNDX4)+AMT
	ELSE 
	    NRMCNT(QNDX1,QNDX2,QNDX3,QNDX4) = NRMCNT(QNDX1,QNDX2,QNDX3,QNDX4)+1
	    NRMAMT(QNDX1,QNDX2,QNDX3,QNDX4) = NRMAMT(QNDX1,QNDX2,QNDX3,QNDX4)+AMT

	    IF (KAMT.NE.0) THEN !IF kicker played
C***	      TYPE*,' QNDXK3,TTYP = ',QNDXK3,TRABUF(TTYP)
	      NRMCNT(QNDX1,QNDX2,QNDXK3,QNDX4)=NRMCNT(QNDX1,QNDX2,QNDXK3,QNDX4)+1
	      NRMAMT(QNDX1,QNDX2,QNDXK3,QNDX4)=NRMAMT(QNDX1,QNDX2,QNDXK3,QNDX4)+KAMT
	    ENDIF
	ENDIF

C--------------------------------------------

	GOTO 2000   !Read next record

C************************************************************

5000	CONTINUE
	GO TO 1000  !Read next block





C Come here when all done
C -----------------------
6000	CONTINUE



	DO 6500 PASS=1,(MAXNDX1+4)/5
	    POFF=(PASS-1)*5+1
	    PMAX=MIN(POFF+4,MAXNDX1)


	    CALL TITLE('ANALYSIS OF '//INPNAM,'ANLDRW  ',01,6,PAGE,
     *	                DAYCDC)
	    WRITE(6,6002)(NDX1NM(K),K=POFF,PMAX)
6002	    FORMAT(//,X,T25,X,5A20)

	    WRITE(6,6003)
6003	    FORMAT(   X,T25,5('   COUNT','      AMOUNT'),/)


	    DO 6240 NDX3=1,MAXGAM
	    DO 6230 NDX2=1,MAXNDX2
	    DO 6220 NDX4=1,MAXNDX4
	       DO 6210 NDX1=POFF,PMAX
	          IF(NRMCNT(NDX1,NDX2,NDX3,NDX4).NE.0)GO TO 6215
6210	       CONTINUE
	       GO TO 6220

6215	       CONTINUE

	       UNIT = BETUNIT

               IF (NDX3 .EQ. MAXGAM+1) THEN
                  WRITE(6,6101)NDX2NM(NDX2),'   ',NDX4NM(NDX4),
     *	                       (NRMCNT(K,NDX2,NDX3,NDX4),
     *	                       CMONY(NRMAMT(K,NDX2,NDX3,NDX4),12,
     *                         UNIT),K=POFF,PMAX)
               ELSE
                  WRITE(6,6101)NDX2NM(NDX2),GSNAMES(NDX3),
     *                         NDX4NM(NDX4),
     *	                       (NRMCNT(K,NDX2,NDX3,NDX4),
     *	                       CMONY(NRMAMT(K,NDX2,NDX3,NDX4),12,
     *                         UNIT),K=POFF,PMAX)
               END IF

6101	       FORMAT(X,A,X,A,X,A,T25,5(I8,A12))

6220	    CONTINUE
6230	    CONTINUE
6240	    CONTINUE
6500	CONTINUE



C Financial report (to check against Vision)
C ------------------------------------------

	DO 4450 PASS=1,(MAXNDX1+4)/5
	    POFF=(PASS-1)*5+1
	    PMAX=MIN(POFF+4,MAXNDX1)


	    CALL TITLE('ANALYSIS OF '//INPNAM,'ANLDRW  ',01,6,PAGE,
     *	                DAYCDC)
	    WRITE(6,6002)(NDX1NM(K),K=POFF,PMAX)

	    WRITE(6,6003)


	    DO 4440 NDX3=1,MAXGAM
	    DO 4430 NDX2=1,MAXNDX2
	    DO 4420 NDX4=1,MAXNDX4
	       DO 4410 NDX1=POFF,PMAX
	          IF(FINANCE_CNT(NDX1,NDX2,NDX3,NDX4).NE.0)GO TO 4415
4410	       CONTINUE
	       GO TO 4420

4415	       CONTINUE


	       UNIT = BETUNIT
               IF (NDX3 .EQ. MAXGAM+1) THEN
                  WRITE(6,6101)NDX2NM(NDX2),'   ',NDX4NM(NDX4),
     *	                       (FINANCE_CNT(K,NDX2,NDX3,NDX4),
     *	                       CMONY(FINANCE_AMT(K,NDX2,NDX3,NDX4),12,
     *                         UNIT),K=POFF,PMAX)
               ELSE
                  WRITE(6,6101)NDX2NM(NDX2),GSNAMES(NDX3),
     *                         NDX4NM(NDX4),
     *	                       (FINANCE_CNT(K,NDX2,NDX3,NDX4),
     *	                       CMONY(FINANCE_AMT(K,NDX2,NDX3,NDX4),12,
     *                         UNIT),K=POFF,PMAX)
               END IF


4420	    CONTINUE
4430	    CONTINUE
4440	    CONTINUE
4450	CONTINUE


	DO I=1,MAXGAM
	   WRITE(6,7071) GSNAMES(I),FRAC_CNT(I)
 7071      FORMAT(1X,A,' : ',I10)
        END DO



	CALL USRCLOS1(1)
	CALL USRCLOS1(6)


C	COPIES = 0
C	CALL SPOOL(XXREPNAM,COPIES,ST)
C	TYPE *,'ANLDRW.REP HAS BEEN SPOOLED'

	IF(TOTERR.NE.0)THEN
	  TYPE *,BELL,BELL,BELL
	  TYPE *,' '
	  TYPE *,' ***************************************** '
	  TYPE *,' '
	  TYPE *,' **** ANOMALIES WERE FOUND ... SEE REPORT ****'
	  TYPE *,' '
	  TYPE *,BELL,BELL,BELL
	ENDIF


	CALL GSTOP(GEXIT_SUCCESS)

	END
