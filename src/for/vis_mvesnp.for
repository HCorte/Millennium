C SUBROUTINE SSCSNP
C  
C V05 05-JUN-2000 OXK SSOREC replaced w/ SSOCOM
C V04 15-MAR-2000 OXK Layout fix
C V03 21-SEP-1999 UXN CMB parameter added.
C V02 29-JUL-1998 RXK Formats fixed for very big odds
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE MVESNP(NUM,GIND,CMB)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:SSCCOM.DEF'
	INCLUDE 'INCLIB:SSPCOM.DEF'
	INCLUDE 'INCLIB:SSOCOM.DEF'
	INCLUDE 'INCLIB:DSSREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
        ! arguments
        INTEGER*4  NUM      
        INTEGER*4  GIND     
        INTEGER*4  CMB(3)

        INTEGER*4  GNUM
        INTEGER*4  DRAW
        INTEGER*4  NUMEVE
        INTEGER*4  ST
        INTEGER*4  FDB(7)
        INTEGER*4  NETSAL
        INTEGER*4  WINSHR
        INTEGER*4  PRG_AMT
        INTEGER*4  I,J,K

        INTEGER*2  DBUF(LDATE_LEN)
        INTEGER*2  BEGSAL(LDATE_LEN)
        INTEGER*2  ENDSAL(LDATE_LEN)
        INTEGER*2  DPUR(LDATE_LEN)

        INTEGER*4  SCORE(6)
        INTEGER*4  AMT
        INTEGER*4  BUCKET
        INTEGER*4  ENTR
        INTEGER*4  FTOP,NHEX

        REAL*8     NETPOL
        REAL*8     TOTPOL
        REAL*8     ODDS
	INTEGER*4  NAM_LEN
	PARAMETER  (NAM_LEN=SSNMS_LEN/4)
	INTEGER*4  BLANK(NAM_LEN)/NAM_LEN*'    '/
	INTEGER*2  BLANK_DATE(7)/7*'  '/
	INTEGER*4  IONEROW(13)

	CHARACTER*52 ONEROW
	CHARACTER    OROWS(52)
	CHARACTER*2  MUUT/'Mu'/
	CHARACTER*2  MUUT1,MUUT2,MUUT3,MUUT4,MUUT5,MUUT6

	EQUIVALENCE  (ONEROW,OROWS)
	EQUIVALENCE  (IONEROW,ONEROW)

        EQUIVALENCE  (OROWS(17),MUUT1),(OROWS(20),MUUT2),(OROWS(24),MUUT3)
        EQUIVALENCE  (OROWS(27),MUUT4),(OROWS(31),MUUT5),(OROWS(34),MUUT6)

	CHARACTER*70 ACROSS
	CHARACTER    AROWS(70)
	CHARACTER*2  MUUR1,MUUR2

	EQUIVALENCE  (ACROSS,AROWS)
	EQUIVALENCE  (AROWS(5),MUUR1),(AROWS(10),MUUR2)

        CHARACTER*1  SCORELOC
        CHARACTER*17 POLSTS(11)     !pool status

        DATA POLSTS/'Not initialized  ','No drawing       ',
     *              'Info entered     ','Game open        ',
     *              'End of game      ','Results entered  ',
     *              'Results verified ','Drawing completed',
     *              'Results are final','Cancelled/Ref dis',
     *              'Cancelled/Ref ena'/



	DRAW=NUM
        IF(GIND.LE.1) GIND = 1
        IF (GIND .GT. MAXIND) THEN
          WRITE(CLIN23,3000) GTNAMES(TSSC)
          RETURN
        END IF

        GNUM=GTNTAB(TSSC,GIND)
        IF(GNUM.LT.1) THEN
          WRITE(CLIN23,3010) GTNAMES(TSSC),GIND
          RETURN
        ENDIF

        IF(DRAW.LT.1) DRAW=DAYDRW(GNUM)
        IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)
        IF (DRAW .LT. 1) THEN
          DO I=1,22
             WRITE(XNEW(I),3050)
          ENDDO
          WRITE(CLIN23,3010) GTNAMES(TSSC),GIND
          RETURN
        END IF
C
C GET DATA FROM COMMON OR DISK
C
        IF(DRAW.EQ.DAYDRW(GNUM)) THEN
          CALL GAMLOG(TSSC,GIND,DSSREC,SSCSTS)
          GOTO 100
        ENDIF
        GOTO 200

C
C BUILD SNAPSHOT FROM MEMORY
C HEADER

100     CONTINUE

        NETPOL = DFLOAT(SSCSAL(GIND)) * CALPER(DSSSPR)
        TOTPOL = NETPOL + DFLOAT(DSSPOL(1))

        BEGSAL(5) = DSSBSD
        ENDSAL(5) = DSSESD
        DBUF(5) = DSSDAT
        CALL LCDATE(DBUF)
        CALL LCDATE(BEGSAL)
        CALL LCDATE(ENDSAL)
        CALL LCDATE(DBUF)
        WRITE(CLIN1,901) GTNAMES(TSSC),GIND,
     *                   (BEGSAL(I),I=7,13),(ENDSAL(I),I=9,13)
        WRITE(CLIN2,902) (DSSMNM(I),I=1,SSNMS_LEN/4),DRAW,
     *                   POLSTS(DSSSTS+1),(DBUF(I),I=7,13)

        WRITE(CLIN3,3050)

        NUMEVE = 1
        WRITE(CLIN4,904) 1,(DSSSNM(I,1),I=1,SSNMS_LEN/4)
        IF(DSSEST(2).EQ.GAMOPN) THEN
           WRITE(CLIN5,904) 2,(DSSSNM(I,2),I=1,SSNMS_LEN/4)
           NUMEVE = NUMEVE + 1
	ELSE
           WRITE(CLIN5,9041) 2
	ENDIF
        IF(DSSEST(3).EQ.GAMOPN) THEN
           WRITE(CLIN6,904) 3,(DSSSNM(I,3),I=1,SSNMS_LEN/4)
           NUMEVE = NUMEVE + 1
	ELSE
           WRITE(CLIN6,9041) 3
	ENDIF

        WRITE(CLIN7,3050)

        WRITE(CLIN8,908)  (DSSDES(I),I=1,7), 
     *			  CMONY(SSCSAL(GIND),10,BETUNIT) 
        WRITE(CLIN9,9081) (DSSDES(I),I=8,14), 
     *                    CMONY(IDNINT(NETPOL),10,BETUNIT)
        WRITE(CLIN10,9082)(DSSDES(I),I=15,21),
     *	                  CMONY(DSSPOL(1),10,BETUNIT)
        WRITE(CLIN11,909) CMONY(IDNINT(TOTPOL),10,BETUNIT)
        WRITE(CLIN12,911) CSMONY(DSSPRC,6,BETUNIT)
        WRITE(CLIN13,3050)

        IF(DSSSTS.LT.GFINAL) THEN
           WRITE(CLIN14,912) SSPCMB(GIND)
           WRITE(CLIN15,913) SSPONUM(GIND)
           WRITE(CLIN16,914) SSPDCMB(GIND)
           WRITE(CLIN17,915) SSPNBA(GIND)
           WRITE(CLIN18,916) SSPNBU(GIND)
        ENDIF
C
C GET SCORE
C
        IF(DSSSTS.LT.GFINAL) THEN
	   NHEX = 0
	   DO I=1,3
	      SCORE(2*I-1) = IAND(CMB(I),'0F'X)
	      NHEX = IOR(ISHFT(NHEX,4),SCORE(2*I-1)) 
	      SCORE(2*I)   = IAND(ISHFT(CMB(I),-16),'0F'X)
	      NHEX = IOR(ISHFT(NHEX,4),SCORE(2*I)) 
	   ENDDO

           CALL GETCCITT(NHEX,1,3,BUCKET)
           CALL RND64(BUCKET,GIND,1,65535,6)
	   AMT = 0
           SCORELOC = '*'
C
C SEARCH MAIN TABLE FOR UCID
C
           DO J=1,SSPNBA(GIND)
              DO ENTR=1,SSGISZ
                 IF(IAND(ISSPMAIN(ENTR,BUCKET,GIND),'00FFFFFF'X).EQ.
     *              NHEX) THEN
                    AMT = SSPCAMT(ENTR,BUCKET,GIND)
                    CALL MOVBYT(ISSPMAIN(ENTR,BUCKET,GIND),SSGTEL,FTOP,1,1)
                    IF(FTOP.GT.0.AND.FTOP.NE.255) THEN
                       SCORELOC = 't'
 		    ELSEIF(FTOP.NE.255) THEN  
                       SCORELOC = 'm'
		    ENDIF  
                    GOTO 150
                 ENDIF
              ENDDO
              BUCKET=MOD(BUCKET+1,65536)
           ENDDO
C
C SEARCH OVERFLOW TABLE FOR UCID
C
      	  DO I=1,SSPONUM(GIND)
      	     IF(SSOCOMCMB(I,GIND).EQ.NHEX) THEN
      		AMT=SSOCOMAMT(I,GIND)
      		SCORELOC = 'o'
      		GOTO 150
      	     ENDIF
      	  ENDDO
C
C CALCULATE ODDS
C
150        CONTINUE
           IF(AMT.EQ.0) THEN
              ODDS=0.D0
           ELSE
              ODDS = TOTPOL /DFLOAT(AMT)
           ENDIF
           WRITE(CLIN20,3050)
C
	   CALL LIB$MOVC3(SIZEOF(IONEROW),ICHAR(' '),IONEROW)
C
           IF(NUMEVE.EQ.3) THEN
	     WRITE(ONEROW,930) (SCORE(K),K=1,6),SCORELOC 
	     IF(SCORE(3).EQ.15) MUUT3=MUUT
	     IF(SCORE(4).EQ.15) MUUT4=MUUT
	     IF(SCORE(5).EQ.15) MUUT5=MUUT
             IF(SCORE(6).EQ.15) MUUT6=MUUT
	   ENDIF
C
	   IF(NUMEVE.EQ.2) THEN
	     WRITE(ONEROW,9301) (SCORE(K),K=1,4),SCORELOC 
	     IF(SCORE(3).EQ.15) MUUT3=MUUT
	     IF(SCORE(4).EQ.15) MUUT4=MUUT
	   ENDIF
C
           IF(NUMEVE.EQ.1) WRITE(ONEROW,9302) (SCORE(K),K=1,2),SCORELOC 
C
           IF(SCORE(1).EQ.15) MUUT1=MUUT
           IF(SCORE(2).EQ.15) MUUT2=MUUT
C
	   WRITE(CLIN20,951) ONEROW
C
           WRITE(CLIN21,931) CMONY(AMT,8,BETUNIT),ODDS
C
        ENDIF 
	WRITE(CLIN23,953)
        RETURN
C
C BUILD SNAPSHOT FROM FILE
C
200     CONTINUE

        SMODE=.TRUE.

        CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
        CALL IOINIT(FDB,1,DSSSEC*256)
        IF(ST.NE.0) THEN
          WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
          CALL USRCLOS1(     1)
          RETURN
        ENDIF
        CALL READW(FDB,DRAW,DSSREC,ST)
        IF(ST.NE.0) THEN
          WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
          CALL USRCLOS1(     1)
          RETURN
        ENDIF
        CALL USRCLOS1(     1)

        IF (DSSSTS .EQ. 0) THEN
          WRITE(CLIN23,3040) GTNAMES(TSSC),GIND,DRAW
          RETURN
        END IF

        BEGSAL(5) = DSSBSD
        ENDSAL(5) = DSSESD
        DBUF(5) = DSSDAT
        DPUR(5)=DSSPUP   
        CALL LCDATE(DBUF)
        CALL LCDATE(BEGSAL)
        CALL LCDATE(ENDSAL)
        CALL LCDATE(DBUF)
        IF(DSSPUP.NE.0) THEN
	  CALL LCDATE(DPUR)
	ELSE
	  CALL MOVBYT(BLANK_DATE,1,DPUR,7,7*2)
	ENDIF
        NETSAL=DSSSAL-DSSREF
	WINSHR=NETSAL*CALPER(DSSSPR)

        WRITE(CLIN1,901) GTNAMES(TSSC),GIND,
     *                   (BEGSAL(I),I=7,13),(ENDSAL(I),I=9,13)
        WRITE(CLIN2,902) (DSSMNM(I),I=1,SSNMS_LEN/4),DRAW,
     *                   POLSTS(DSSSTS+1),(DBUF(I),I=7,13)

C
C Odds,  amount bet and # winners.
C
        IF(DSSSTS.GE.GFINAL) WRITE(CLIN4,950) DSSODS/100,MOD(DSSODS,100),
     *                          CMONY(DSSABW,10,BETUNIT),
     *			        DSSWPR(1,PRWON)
C
C Beginning sales.
C
        WRITE(CLIN6,940) (DSSSNM(I,1),I=1,SSNMS_LEN/4),
     *                   (BEGSAL(I),I=7,13)
C
C End sales.
C
	IF(DSSSTS.LT.GAMENV) THEN
           WRITE(CLIN7,9411) (ENDSAL(I),I=7,13)   
	ELSEIF(DSSSTS.EQ.GAMCAN.OR.DSSSTS.EQ.GAMREF) THEN
           WRITE(CLIN7,9412) (ENDSAL(I),I=7,13)   
	ELSE
           WRITE(ACROSS,941) DSSWIN(1,1),DSSWIN(2,1),(ENDSAL(I),I=7,13)
	   IF(DSSWIN(1,1).EQ.15) MUUR1=MUUT
	   IF(DSSWIN(2,1).EQ.15) MUUR2=MUUT
           WRITE(CLIN7,952) ACROSS
	ENDIF
C
C Draw date.
C
        WRITE(CLIN8,942) (DBUF(I),I=7,13)
C
C Purge date.
C
        IF(DSSPUP.EQ.0) THEN
	   IF(DSSEST(2).NE.GAMNUL) THEN
	     WRITE(CLIN9,917) '2.',(DSSSNM(I,2),I=1,SSNMS_LEN/4),
     *                        (BLANK_DATE(I),I=1,7)
	   ELSE
             WRITE(CLIN9,917) '  ',(BLANK(I),I=1,SSNMS_LEN/4),
     *                        (BLANK_DATE(I),I=1,7)
	   ENDIF
        ELSE
	   IF(DSSEST(2).NE.GAMNUL) THEN
             WRITE(CLIN9,917) '2.',(DSSSNM(I,2),I=1,SSNMS_LEN/4),
     *                        (DPUR(I),I=7,13)
	   ELSE
             WRITE(CLIN9,917) '  ',(BLANK(I),I=1,SSNMS_LEN/4),
     *                        (DPUR(I),I=7,13)
	   ENDIF
        ENDIF
C
C Display score for second set... 
C
	IF(DSSEST(2).NE.GAMNUL) THEN
	  IF(DSSSTS.LT.GAMENV) THEN
	    WRITE(CLIN10,9441) CMONY(DSSSAL,10,BETUNIT)
	  ELSEIF(DSSSTS.EQ.GAMCAN.OR.DSSSTS.EQ.GAMREF) THEN
	    WRITE(CLIN10,9442) CMONY(DSSSAL,10,BETUNIT)
	  ELSE
	    WRITE(ACROSS,944) DSSWIN(1,2),DSSWIN(2,2),CMONY(DSSSAL,10,BETUNIT)
	    IF(DSSWIN(1,2).EQ.15) MUUR1=MUUT
	    IF(DSSWIN(2,2).EQ.15) MUUR2=MUUT
            WRITE(CLIN10,952) ACROSS
	  ENDIF
	ELSE
	  WRITE(CLIN10,945) CMONY(DSSSAL,10,BETUNIT)
        ENDIF
C
C Total refunds.
C
        WRITE(CLIN11,919)  CMONY(DSSREF,10,BETUNIT)
C
C Net Sales and Winners share.
C
        IF(DSSEST(3).NE.GAMNUL) THEN
	  WRITE(CLIN12,946) (DSSSNM(I,3),I=1,SSNMS_LEN/4),
     *			     CMONY(NETSAL,10,BETUNIT)
	  IF(DSSSTS.LT.GAMENV) THEN
	    WRITE(CLIN13,9481) CMONY(WINSHR,10,BETUNIT)
	  ELSEIF(DSSSTS.EQ.GAMCAN.OR.DSSSTS.EQ.GAMREF) THEN
	    WRITE(CLIN13,9482) CMONY(WINSHR,10,BETUNIT)
	  ELSE 
	    WRITE(ACROSS,948) DSSWIN(1,3),DSSWIN(2,3),CMONY(WINSHR,10,BETUNIT)
	    IF(DSSWIN(1,3).EQ.15) MUUR1=MUUT
	    IF(DSSWIN(2,3).EQ.15) MUUR2=MUUT
            WRITE(CLIN13,952) ACROSS
	  ENDIF
	ELSE
	  WRITE(CLIN12,947) CMONY(NETSAL,10,BETUNIT)
	  WRITE(CLIN13,949) CMONY(WINSHR,10,BETUNIT) 
	ENDIF
C
C Extra amount and rounding pot.
C
	WRITE(CLIN14,922) CSMONY(DSSPOL(1),10,BETUNIT)
	WRITE(CLIN15,923) CSMONY(DSSBRK(1),10,BETUNIT)
C
C Minimum stake and Winning amount
C
	WRITE(CLIN16,924) CSMONY(DSSPRC,6,BETUNIT),
     *                    CMONY(DSSWON-DSSREF,10,BETUNIT)
C
C Roll pot, winners paid, refunds paid, Amount purged.
C
	WRITE(CLIN17,925) CSMONY(DSSPOL(2),10,BETUNIT)
	WRITE(CLIN18,926) CMONY(DSSPAD-DSSPRF,10,BETUNIT)
	WRITE(CLIN19,927) CMONY(DSSPRF,10,BETUNIT)
        IF(DSSPUP.NE.0) THEN
	   PRG_AMT=DSSWON - DSSPAD
        ELSE
	   PRG_AMT=0
        ENDIF
	WRITE(CLIN20,928) CMONY(PRG_AMT,10,BETUNIT)
	WRITE(CLIN23,954)
C
3000    FORMAT ('Enter !',A8,' game index ')
3010    FORMAT (A8,1X,I1,' game not active')
3020    FORMAT (5A4,' open error ',I4)
3021    FORMAT (5A4,' open error ',I4)
3030    FORMAT (5A4,' read error ',I4,' record > ',I4)
3040    FORMAT (A8,1X,I1,' game not initialized event > ',I4)
3050    FORMAT (80(' '))
901     FORMAT ('*',A8,1X,I1,'*',7A2,'-',5A2,'*')
902     FORMAT (<SSNMS_LEN/4>A4,'*Event code',I3,'*',A17,'*Draw',7A2)
904     FORMAT (1X,'Set ',I1,' : ',<SSNMS_LEN/4>A4)
9041    FORMAT (1X,'Set ',I1,' : Not open')
908	FORMAT (1X,'(',7A4,')',T40,'Total sales      ',A10)
9081	FORMAT (1X,'(',7A4,')',T40,'Net sales        ',A10)
9082	FORMAT (1X,'(',7A4,')',T40,'Extra            ',A10)
909	FORMAT (1X,T40,'Total            ',A10)
911	FORMAT (1X,'Minimum stake  :',A6)
912	FORMAT (1X,'Number of combinations played   :',I6)
913	FORMAT (1X,'Number of combinations overflown:',I6)
914	FORMAT (1X,'Number of combinations dropped  :',I6)
915	FORMAT (1X,'Number of buckets assigned      :',I6)
916	FORMAT (1X,'Maximum number of buckets used  :',I6)
905     FORMAT (1X,'Set 1 : ',<SSNMS_LEN/4>A4,    T40,'Beginning sales ',6A2)
906     FORMAT (1X,'Set 2 : ',<SSNMS_LEN/4>A4,    T40,'Ending    sales ',6A2)
907     FORMAT (1X,'Set 3 : ',<SSNMS_LEN/4>A4,    T40,'Draw            ',6A2)
917     FORMAT (1X,A2,1X,<SSNMS_LEN/4>A4,T40,'Purge           ',7A2)
918     FORMAT (1X,'Set 1 Score ',I2.2,' - ',I2.2,T40,'Total Sales       ',A10)
919     FORMAT (T40,'Total Refunds     ',A10)
920     FORMAT (1X,'Set 3 Score ',I2.2,' - ',I2.2,T40,'Net Sales         ',A10)
921     FORMAT (1X,'Odds ',I6,'.',I2.2, T40,'Winners Share     ',A10)
922     FORMAT (T40,'Extra Amount      ',A10)
923     FORMAT (1X,T40,'Rounding Pot      ',A10)
924     FORMAT (1X,'Minimum stake ',A6,T40,'Winning Amount    ',A10)
925     FORMAT (T40,'Roll Pot          ',A10)
926     FORMAT (T40,'Winners Paid      ',A10)
927     FORMAT (T40,'Refunds Paid      ',A10)
928     FORMAT (T40,'Amount Purged     ',A10)
930     FORMAT(1X,'*** Score  ',T17,3(I2.2,'-',I2.2,2X),T47,'*',A1,'*')
9301    FORMAT(1X,'*** Score  ',T17,2(I2.2,'-',I2.2,2X),7(' '),T47,'*',A1,'*')
9302    FORMAT(1X,'*** Score  ',T17,1(I2.2,'-',I2.2,2X),14(' '),T47,'*',A1,'*')
931     FORMAT(1X,'*** Invested ',A8,'  Odds ',F16.2,T47'***')
940     FORMAT (1X,'1. ',<SSNMS_LEN/4>A4,T40,'Beginning sales ',7A2)
941     FORMAT (4X,I2.2,' - ',I2.2,T40,        'Ending    sales ',7A2)
9411    FORMAT (11X,T40,        'Ending    sales ',7A2)
9412    FORMAT (11X,T7,'-',T40,        'Ending    sales ',7A2)
942     FORMAT (T40,'Draw            ',7A2)
943     FORMAT (1X,A2,1X,<SSNMS_LEN/4>A4,T40,A16,A10)
944     FORMAT (4X,I2.2,' - ',I2.2,T40,'Total Sales       ',A10)
9441    FORMAT (11X,T40,'Total Sales       ',A10)
9442    FORMAT (11X,T7,'-',T40,'Total Sales       ',A10)
945     FORMAT (4X,T40,'Total Sales       ',A10)
946     FORMAT (1X,'3. ',<SSNMS_LEN/4>A4,T40,'Net Sales         ',A10)
947     FORMAT (4X,T40,'Net Sales         ',A10)
948     FORMAT (4X,I2.2,' - ',I2.2,T40,'Winners Share     ',A10)
9481    FORMAT (11X,T40,'Winners Share     ',A10)
9482    FORMAT (11X,T7,'-',T40,'Winners Share     ',A10)
949     FORMAT (4X,T40,'Winners Share     ',A10)
950	FORMAT (4X,'Odds ',I10,'.',I2.2,2X,'Amount bet ',A10,2X,
     *          '# winners  ',I8)
951	FORMAT (2A52)
952	FORMAT (2A70)
953     FORMAT('Enter !game index or event number or score ?XX:XX-XX:XX-XX:XX')
954     FORMAT('Enter !game index or event number')
        END

