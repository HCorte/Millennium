C SUBROUTINE TULSNP
C
C V12 31-MAY-2000 PXO Subroutine name from SCRSNP -> TULSNP
C V11 23-SEP-1999 UXN Header changed.
C V10 30-OCT-1995 RXK Draw date and purge date displayed now 
C V09 23-MAR-1995 HXK Fix for Purge amount
C V08 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V07 21-MAY-1994 HXK SHOW SPACES FOR SCORES IF RESULTS ARE NOT IN.
C V06 03-FEB-1994 HXK cosmetic chnage
C V05 02-FEB-1994 HXK BUG FIX.
C V04 02-FEB-1994 HXK CHANGES MADE FOR ODDSET INSTALLATION
C V03 23-JAN-1994 JXP Changed refunds enabled to cancelled
C V02 14-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 16-OCT-1991 GXA INITIAL RELEASE FOR NETHERLANDS
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE SCRSNP(NUM,GIND)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:SCRCOM.DEF'
	INCLUDE 'INCLIB:DSCREC.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*4   GIND			    ! Game Index
	INTEGER*4   GNUM			    ! Game Number
	INTEGER*4   DRAW			    ! Draw number to access
	INTEGER*4   ST				    ! Status variable
	INTEGER*4   NETSAL			    ! Net sales
	INTEGER*4   TAX				    ! Total tax amount
	INTEGER*4   WINSHR			    ! Winners share
	INTEGER*4   LNS				    ! Line number
	INTEGER*4   NUM				    ! Draw number passed in
	INTEGER*4   TOTSAL			    ! Total sales
	INTEGER*4   NETPOL			    ! Net Pool
	INTEGER*4   TOTPOL			    ! Net Pool + Rollover
	INTEGER*4   SALTAX/0/			    ! Sales Tax (NOT DEFINED)
	INTEGER*4   I,K				    ! Loop variables   
	INTEGER*4   DSCORES(2,MAXD),FDB(7)
        INTEGER*4   PRG_AMT
	INTEGER*2   DBUF(LDATE_LEN),BEGSAL(LDATE_LEN)
	INTEGER*2   ENDSAL(LDATE_LEN),DBUF2(LDATE_LEN)
	CHARACTER*17 POLSTS(11)
	DATA POLSTS/'Not initialized  ','No drawing       ',
     *	            'Info entered     ','Game open        ',
     *	            'End of game      ','Results entered  ',
     *	            'Results verified ','Drawing completed',
     *	            'Results are final','Refund/cancelled ',
     *	            'Cancelled        '/
C
C
	DRAW=NUM
	IF(GIND.LT.1.OR.GIND.GT.MAXIND) THEN
	  WRITE(CLIN23,3000) GTNAMES(TSCR)
	  RETURN
	ENDIF
C
C
10	CONTINUE
	GNUM=GTNTAB(TSCR,GIND)
	IF(GNUM.LT.1) THEN
	  WRITE(CLIN23,3010) GTNAMES(TSCR),GIND
	  RETURN
	ENDIF
	IF(DRAW.LT.1) DRAW=DAYDRW(GNUM)
	IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)
C
C GET DATA FROM COMMON OR DISK
C
	IF(DRAW.EQ.DAYDRW(GNUM)) THEN
	  CALL GAMLOG(TSCR,GIND,DSCREC,SCRSTS)
	  GOTO 100
	ENDIF
C
C
	SMODE=.TRUE.
	CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,1,DSCSEC*256)
	IF(ST.NE.0) THEN
	  WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
	  CALL USRCLOS1(     1)
	  RETURN
	ENDIF
	CALL READW(FDB,DRAW,DSCREC,ST)
	IF(ST.NE.0) THEN
	  WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
	  CALL USRCLOS1(     1)
	  RETURN
	ENDIF
	CALL USRCLOS1(     1)
C
C
	IF(DSCSTS.EQ.0) THEN
	  WRITE(CLIN23,3040) GTNAMES(TSCR),GIND,DRAW
	  RETURN
	ENDIF
	DBUF(5)=DAYCDC
	BEGSAL(5)=DSCBSD
	ENDSAL(5)=DSCESD
	NETSAL=DSCSAL-DSCREF
	TAX=IDNINT(DFLOAT(NETSAL)*CALPER(SALTAX))
	WINSHR=(NETSAL-TAX) * CALPER(DSCSPR)
	CALL LCDATE(BEGSAL)
	CALL LCDATE(ENDSAL)
	CALL LCDATE(DBUF)
C
	WRITE(CLIN1,900) GTNAMES(TSCR),GIND
	WRITE(CLIN2,902) DRAW,(DSCNM1(I),I=1,3),(DSCNM2(I),I=1,3),
     *	                  POLSTS(DSCSTS+1)
	WRITE(CLIN3,999)
	WRITE(CLIN4,1004)
	IF(DSCSTS.GE.GFINAL) THEN
	   WRITE(CLIN5,1005) (DSCNM1(I),I=1,3),DSCWIN(1)
        ELSE
           WRITE(CLIN5,10051) (DSCNM1(I),I=1,3)
        ENDIF
	WRITE(CLIN6,1006) DSCODS/100,MOD(DSCODS,100),
     *		  CMONY(DSCABW,9,BETUNIT)
	IF(DSCSTS.GE.GFINAL) THEN
	   WRITE(CLIN7,1007) (DSCNM2(I),I=1,3),DSCWIN(2)
        ELSE
           WRITE(CLIN7,10071) (DSCNM2(I),I=1,3)
	ENDIF
	WRITE(CLIN8,1008)
        DBUF2(5)=DSCDAT
        CALL LCDATE(DBUF2)
	WRITE(CLIN9,1009) (BEGSAL(I),I=7,13),(DBUF2(I),I=7,13)
        IF(DSCPUP.GT.0) THEN 
           DBUF2(5)=DSCPUP
           CALL LCDATE(DBUF2)
	   WRITE(CLIN10,10101) (ENDSAL(I),I=7,13),(DBUF2(I),I=7,13)
        ELSE
	   WRITE(CLIN10,1010) (ENDSAL(I),I=7,13)
        ENDIF
	WRITE(CLIN11,999)
	WRITE(CLIN12,1012) CMONY(DSCSAL,10,BETUNIT)
	WRITE(CLIN13,1013) CMONY(DSCREF,10,BETUNIT)
	WRITE(CLIN14,1014) CMONY(NETSAL,10,BETUNIT)
	WRITE(CLIN15,1015) CMONY(WINSHR,10,BETUNIT)
	WRITE(CLIN16,1016) CSMONY(DSCPOL(1),10,BETUNIT)
	WRITE(CLIN17,1017) CSMONY(DSCBRK(2),10,BETUNIT)
	WRITE(CLIN18,1018) CMONY(DSCWON-DSCREF,10,BETUNIT)
        WRITE(CLIN19,1019) CSMONY(DSCPOL(2),10,BETUNIT)
	WRITE(CLIN20,1020) CMONY(DSCPAD-DSCPRF,10,BETUNIT)
	WRITE(CLIN21,1021) CMONY(DSCPRF,10,BETUNIT)
        IF(DSCPUP.NE.0) THEN
           PRG_AMT = DSCWON - DSCPAD
        ELSE
           PRG_AMT = 0
        ENDIF
	WRITE(CLIN22,1022) CMONY(PRG_AMT,10,BETUNIT)
	RETURN
C
C BUILD SNAPSHOT FROM MEMORY
C
C
100	CONTINUE
	IF(DSCSTS.EQ.0) THEN
	  WRITE(CLIN23,3040) GTNAMES(TSCR),GIND,DRAW
	  RETURN
	ENDIF
	DBUF(5)=DAYCDC
	DBUF2(5)=DSCDAT
	BEGSAL(5)=DSCBSD
	ENDSAL(5)=DSCESD
	CALL LCDATE(BEGSAL)
	CALL LCDATE(ENDSAL)
	CALL LCDATE(DBUF)
	CALL LCDATE(DBUF2)
	TOTSAL=0
	NETPOL=0
	TOTPOL=0
	TAX=0
C
C
	DO 110 I=1,SLEN
	TOTSAL=TOTSAL+SCPOOL(I,2,2,GIND)
110	CONTINUE
	TAX=IDNINT(DFLOAT(TOTSAL)*CALPER(SALTAX))
	NETPOL=(TOTSAL*CALPER(DSCSPR))-TAX+DSCBRK(1)
	TOTPOL=(TOTSAL*CALPER(DSCSPR))-TAX+DSCBRK(1)+DSCPOL(1)
C
C
	DO 120 I=1,MAXD
	CALL INDPOL(DSCORES(1,I),DSCORES(2,I),DPOOL(I,SPSCOR,GIND))
120	CONTINUE
	WRITE(CLIN1,901) GTNAMES(TSCR),GIND,
     *			 (BEGSAL(I),I=7,13),(ENDSAL(I),I=7,13)
	WRITE(CLIN2,902) DRAW,(DSCNM1(I),I=1,3),(DSCNM2(I),I=1,3),
     *	      POLSTS(DSCSTS+1)
	WRITE(CLIN3,903) (DBUF2(I),I=7,13)
	WRITE(CLIN4,904)
	WRITE(CLIN5,905)
	LNS=6
	DO 130 I=1,15
	IF(LNS.LT.12) THEN
	  WRITE(XNEW(  LNS),906) DSCORES(1,I),DSCORES(2,I),
     *	 DPOOL(I,SPDODS,GIND)/100,MOD(DPOOL(I,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I,SPDAMT,GIND),9,BETUNIT),
     *	 DSCORES(1,I+15),DSCORES(2,I+15),
     *	 DPOOL(I+15,SPDODS,GIND)/100,MOD(DPOOL(I+15,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I+15,SPDAMT,GIND),9,BETUNIT),
     *	 DSCORES(1,I+30),DSCORES(2,I+30),
     *	 DPOOL(I+30,SPDODS,GIND)/100,MOD(DPOOL(I+30,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I+30,SPDAMT,GIND),9,BETUNIT)
	ENDIF
	IF(LNS.EQ.12) THEN
	WRITE(XNEW(  LNS),907) DSCORES(1,I),DSCORES(2,I),
     *	 DPOOL(I,SPDODS,GIND)/100,MOD(DPOOL(I,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I,SPDAMT,GIND),9,BETUNIT),
     *	 OPOOL(2,SPDODS,GIND)/100,MOD(OPOOL(2,SPDODS,GIND),100),
     *   CMONY(OPOOL(2,SPDAMT,GIND),9,BETUNIT),
     *	 DSCORES(1,I+30),DSCORES(2,I+30),
     *	 DPOOL(I+30,SPDODS,GIND)/100,MOD(DPOOL(I+30,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I+30,SPDAMT,GIND),9,BETUNIT)
	ENDIF
	IF(LNS.EQ.13) THEN
         WRITE(XNEW(  LNS),908) DSCORES(1,I),DSCORES(2,I),
     *	 DPOOL(I,SPDODS,GIND)/100,MOD(DPOOL(I,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I,SPDAMT,GIND),9,BETUNIT),
     *	 DSCORES(1,I+30),DSCORES(2,I+30),
     *	 DPOOL(I+30,SPDODS,GIND)/100,MOD(DPOOL(I+30,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I+30,SPDAMT,GIND),9,BETUNIT)
	ENDIF
	IF(LNS.EQ.14.OR.LNS.EQ.15) THEN
         WRITE(XNEW(  LNS),909) DSCORES(1,I),DSCORES(2,I),
     *   DPOOL(I,SPDODS,GIND)/100,MOD(DPOOL(I,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I,SPDAMT,GIND),9,BETUNIT),
     *   DSCORES(1,I+30),DSCORES(2,I+30),
     *   DPOOL(I+30,SPDODS,GIND)/100,MOD(DPOOL(I+30,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I+30,SPDAMT,GIND),9,BETUNIT)
	ENDIF
	IF(LNS.EQ.16) THEN
         WRITE(XNEW(  LNS),910) DSCORES(1,I),DSCORES(2,I),
     *	 DPOOL(I,SPDODS,GIND)/100,MOD(DPOOL(I,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I,SPDAMT,GIND),9,BETUNIT),
     *	 CMONY(TOTSAL,10,BETUNIT),
     *	 DSCORES(1,I+30),DSCORES(2,I+30),
     *	 DPOOL(I+30,SPDODS,GIND)/100,MOD(DPOOL(I+30,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I+30,SPDAMT,GIND),9,BETUNIT)
	ENDIF
	IF(LNS.EQ.17) THEN
         WRITE(XNEW(  LNS),911) DSCORES(1,I),DSCORES(2,I),
     *	 DPOOL(I,SPDODS,GIND)/100,MOD(DPOOL(I,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I,SPDAMT,GIND),9,BETUNIT),
     *	 CMONY(NETPOL,10,BETUNIT),
     *	 DSCORES(1,I+30),DSCORES(2,I+30),
     *	 DPOOL(I+30,SPDODS,GIND)/100,MOD(DPOOL(I+30,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I+30,SPDAMT,GIND),9,BETUNIT)
	ENDIF
	IF(LNS.EQ.18) THEN
         WRITE(XNEW(  LNS),912) DSCORES(1,I),DSCORES(2,I),
     *	 DPOOL(I,SPDODS,GIND)/100,MOD(DPOOL(I,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I,SPDAMT,GIND),9,BETUNIT),
     *	 CMONY(DSCPOL(1),10,BETUNIT),
     *	 DSCORES(1,I+30),DSCORES(2,I+30),
     *	 DPOOL(I+30,SPDODS,GIND)/100,MOD(DPOOL(I+30,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I+30,SPDAMT,GIND),9,BETUNIT)
	ENDIF
	IF(LNS.EQ.19) THEN
         WRITE(XNEW(  LNS),913) DSCORES(1,I),DSCORES(2,I),
     *	 DPOOL(I,SPDODS,GIND)/100,MOD(DPOOL(I,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I,SPDAMT,GIND),9,BETUNIT),
     *	 CMONY(DSCBRK(1),13,BETUNIT),
     *	 DSCORES(1,I+30),DSCORES(2,I+30),
     *	 DPOOL(I+30,SPDODS,GIND)/100,MOD(DPOOL(I+30,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I+30,SPDAMT,GIND),9,BETUNIT)
	ENDIF
	IF(LNS.EQ.20) THEN
         WRITE(XNEW(  LNS),914) DSCORES(1,I),DSCORES(2,I),
     *	 DPOOL(I,SPDODS,GIND)/100,MOD(DPOOL(I,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I,SPDAMT,GIND),9,BETUNIT),
     *	 CMONY(TOTPOL,10,BETUNIT),
     *	 DSCORES(1,I+30),DSCORES(2,I+30),
     *	 DPOOL(I+30,SPDODS,GIND)/100,MOD(DPOOL(I+30,SPDODS,GIND),100),
     *	 CMONY(DPOOL(I+30,SPDAMT,GIND),9,BETUNIT)
	ENDIF
	LNS=LNS+1
130	CONTINUE
	WRITE(CLIN21,921)OPOOL(1,SPDODS,GIND)/100,
     *   MOD(OPOOL(1,SPDODS,GIND),100),
     *   CMONY(OPOOL(1,SPDAMT,GIND),9,BETUNIT),
     *	 OPOOL(3,SPDODS,GIND)/100,MOD(OPOOL(3,SPDODS,GIND),100),
     *   CMONY(OPOOL(3,SPDAMT,GIND),9,BETUNIT)
	WRITE(CLIN22,999)
	RETURN
C
C
900	FORMAT('* ',A8,1X,I1,' *',33(' '))
901	FORMAT(A8,1X,I1,7A2,' -',7A2)
902     FORMAT('Event code - ',I4,4(' '),3A4,' - ',3A4,5(' '),
     *	       '* ',A17,' *')
903     FORMAT('Draw : ',7A2)
904	FORMAT(3(' '),'Home win  ',17(' '),'Tie     ',14(' '),
     *	 'Away win  ')
905	FORMAT('Score',4(' '),'Odds',4(' '),'Amount',3(' '),'Score',
     *	   4(' '),'Odds',
     *	       4(' '),'Amount',3(' '),'Score',4(' '),'Odds',4(' '),
     *	   'Amount')
906	FORMAT(I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9,
     *	    3(' '),I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9,
     *	    3(' '),I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9)
907	FORMAT(I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9,3(' '),
     *	 ' Rest ',1(' '),
     *	   I3,'.',I2.2,1(' '),A9,3(' '),
     *     I2,'-',I2,2(' '),I3,'.',I2.2,
     *	       1(' '),A9)
908	FORMAT(I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9,3(' '),
     *	       23('-'),
     *	    3(' '),I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9)
909	FORMAT(I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9,3(' '),
     *	       23(' '),
     *	    3(' '),I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9)
910	FORMAT(I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9,3(' '),
     *	       'Total sales',2(' '),A10,
     *	    3(' '),I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9)
911	FORMAT(I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9,3(' '),
     *	       'Net pool',5(' '),A10,
     *	    3(' '),I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9)
912	FORMAT(I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9,3(' '),
     *	       'Extra   ',5(' '),A10,
     *	    3(' '),I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9)
913	FORMAT(I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9,3(' '),
     *	       'Round pool',A13,
     *	    3(' '),I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9)
914	FORMAT(I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9,3(' '),
     *	       'Tot pool',5(' '),A10,
     *	    3(' '),I2,'-',I2,2(' '),I3,'.',I2.2,1(' '),A9)
921	FORMAT(' Rest ',1(' '),I3,'.',I2.2,1X,A9,29(' '),
     *	       ' Rest ',1(' '),I3,'.',I2.2,1(' '),A9,1X)
999	FORMAT(80(' '))
1004	FORMAT(15(' '),'Teams',7(' '),'Score',4(' '),'Odds',7(' '),
     *	  'Amount bet',19(' '))
1005	FORMAT(15(' '),3A4,4(' '),I2,45(' '))
10051   FORMAT(15(' '),3A4,51(' '))
1006	FORMAT(33(' '),I6,'.',I2.2,7(' '),A9,19(' '))
1007	FORMAT(15(' '),3A4,4(' '),I2,45(' '))
10071   FORMAT(15(' '),3A4,51(' '))
1008	FORMAT(10(' '),54('-'),12(' '))
1009	FORMAT(10(' '),'Beginning sales ',7A2,5(' '),'Draw ',7A2,14(' '))
1010	FORMAT(10(' '),'Ending    sales ',7A2,40(' '))
10101	FORMAT(10(' '),'Ending    sales ',7A2,5(' '),'Purge',7A2,14(' '))
1012	FORMAT(15(' '),'Total Sales     ',5(' '),A10,33(' '))
1013	FORMAT(15(' '),'Total Refunds   ',5(' '),A10,33(' '))
1014	FORMAT(15(' '),'Net Sales       ',5(' '),A10,33(' '))
1015	FORMAT(15(' '),'Winners Share   ',5(' '),A10,33(' '))
1016	FORMAT(15(' '),'Extra Amount    ',5(' '),A10,33(' '))
1017	FORMAT(15(' '),'Rounding Pot    ',5(' '),A10,33(' '))
1018	FORMAT(15(' '),'Winning Amount  ',5(' '),A10,33(' '))
1019	FORMAT(15(' '),'Roll Pot        ',5(' '),A10,33(' '))
1020	FORMAT(15(' '),'Winners Paid    ',5(' '),A10,33(' '))
1021	FORMAT(15(' '),'Refunds Paid    ',5(' '),A10,33(' '))
1022	FORMAT(15(' '),'Amount Purged   ',5(' '),A10,33(' '))
3000	FORMAT('Enter !',A8,' game index ')
3010	FORMAT(A8,1X,I1,' game not active')
3020	FORMAT(5A4,' open error ',I4)
3030	FORMAT(5A4,' read error ',I4,' record > ',I4)
3040	FORMAT(A8,1X,I1,' game not initialized event > ',I4)
	END
