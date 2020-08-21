C SUBROUTINE CLOSNP
C
C V20 18-JAN-2011 FJG OOB
C V19 06-MAR-2009 FRP Show KIKESD(GI) instaed of KIKDAT(CURDRW,GI).
C V18 01-DEC-2000 UXN TOTOGOLO ADDED.
C V17 05-APR-2000 OXK SPTESD printed for Sports, not SPTDAT(CURDRW)
C V16 13-OCT-1999 RXK World Tour added.
C V15 13-MAY-1999 UXN Super Triple added.
C V14 19-APR-1996 HXK Merge of updates from Finland (Rita, Wojtek, Siew Mun)
C V13 18-APR-1996 HXK Merge of LIVE code from Finland (RXK,WXW)
C V12 16-APR-1996 RXK Closing time for Pitka added
C V11 14-FEB-1996 RXK Fix for the pagenumbers
C V10 11-FEB-1996 HXK Allow for new page after Score (TSCR) game
C V09 03-JAN-1996 PXB Display bug fixed
C V08 13-DEC-1995 PXB Added paging because all games would not fit on 1 page.
C V07 08-DEC-1994 PXB Display zeros for date when that date is at cdc 0.
C V06 07-DEC-1994 PXB change TSCR and TWIT games to look at ESD date instead 
C                     of DAT date.
C V05 08-NOV-1994 HXK Added Bingo
C V04 23-JUL-1993 SXH Added RAVI and SPEDEN
C V03 11-JUN-1993 HXK ADDED AGTINF.DEF, PRMAGT.DEF
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE CLOSNP(PAGE_NUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:NBRCOM.DEF'
        INCLUDE 'INCLIB:SCRCOM.DEF'
	INCLUDE 'INCLIB:WITCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:DBLCOM.DEF'
        INCLUDE 'INCLIB:CPLCOM.DEF'
        INCLUDE 'INCLIB:SSCCOM.DEF'
        INCLUDE 'INCLIB:TRPCOM.DEF'
        INCLUDE 'INCLIB:STRCOM.DEF'
        INCLUDE 'INCLIB:TSLCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
C
C
	INTEGER*4 K, TIME, GNUM, GI, GT, IND, STAT
	INTEGER*4 PAGE_NUM, I
	INTEGER*4 STR_GAM, END_GAM  
	INTEGER*4 DRAW
	INTEGER*4 GAM_IN_PAGE
	PARAMETER (GAM_IN_PAGE=18)
C

	INTEGER*2 DATE(LDATE_LEN)
	CHARACTER*17 GSTAT(0:10)
	DATA GSTAT/'Not initialized  ','No drawing       ',
     *	            'Info entered     ','Game open        ',
     *	            'End of game      ','Results entered  ',
     *	            'Results verified ','Drawing completed',
     *	            'Results are final','Refund/cancelled ',
     *	            'Refunds enabled  '/


	STAT = 0
	WRITE(CLIN2,902)
	IND = 3

	IF(PAGE_NUM.LT.1) PAGE_NUM = 1
	IF((PAGE_NUM-1)*GAM_IN_PAGE.GT.MAXGAM) PAGE_NUM = 1
	
	STR_GAM = (PAGE_NUM-1)*GAM_IN_PAGE+1
	END_GAM = MIN(PAGE_NUM*GAM_IN_PAGE,MAXGAM)

	DO 100 GNUM = STR_GAM, END_GAM
           
	   GI = GNTTAB(GAMIDX,GNUM)
	   GT = GNTTAB(GAMTYP,GNUM)
	   IF(GI.EQ.0.OR.GT.EQ.0) GOTO 100

           DRAW = DAYDRW(GNUM)

	   IF(GT.EQ.TLTO) THEN
              TIME=LTOTIM(GI)
              STAT=LTOSTS(GI)
	      DATE(VCDC)=LTOESD(GI)
	   ELSEIF(GT.EQ.TSPT) THEN
	      TIME=SPTTIM(GI)
	      STAT=SPTSTS(GI)
	      DATE(VCDC)=SPTESD(GI)
	   ELSEIF(GT.EQ.TTGL) THEN
	      TIME=TGLTIM(GI)
	      STAT=TGLSTS(GI)
	      DATE(VCDC)=TGLESD(GI)
           ELSEIF(GT.EQ.TBNG) THEN
              TIME=BNGTIM(GI)
              STAT=BNGSTS(GI)
              DATE(VCDC)=BNGDAT(CURDRW,GI)
	   ELSEIF(GT.EQ.TKIK) THEN
	      TIME=KIKTIM(GI)
	      STAT=KIKSTS(GI)
	      DATE(VCDC)=KIKESD(GI)
           ELSEIF(GT.EQ.TNBR) THEN
              TIME=NBRTIM(GI)
              STAT=NBRSTS(GI)
              DATE(VCDC)=NBRDAT(CURDRW,GI)
           ELSEIF(GT.EQ.TSCR) THEN
              TIME=SCRTIM(GI)
              STAT=SCRSTS(GI)
              DATE(VCDC) = SCRESD(GI)
           ELSEIF(GT.EQ.TWIT) THEN
              TIME=WITTIM(GI)
              STAT=WITSTS(GI)
              DATE(VCDC)=WITESD(GI)
           ELSEIF(GT.EQ.TDBL) THEN
              TIME = DBLTIM(GI)
              STAT = DBLSTS(GI)
              DATE(VCDC) = DBLESD(GI)
           ELSEIF(GT.EQ.TCPL) THEN
              TIME = CPLTIM(GI)
              STAT = CPLSTS(GI)
              DATE(VCDC) = CPLESD(GI)
           ELSEIF(GT.EQ.TSSC) THEN
              TIME = SSCTIM(GI)
              STAT = SSCSTS(GI)
              DATE(VCDC) = SSCESD(GI)
           ELSEIF(GT.EQ.TTRP) THEN
              TIME = TRPTIM(GI)
              STAT = TRPSTS(GI)
              DATE(VCDC) = TRPESD(GI)
           ELSEIF(GT.EQ.TSTR) THEN
              TIME = STRTIM(GI)
              STAT = STRSTS(GI)
              DATE(VCDC) = STRESD(GI)
           ELSEIF(GT.EQ.TTSL) THEN
              TIME = 0
              DO I=1,MAXSRW
                 IF(TSLDAT(I,GI).EQ.TSLESD(GI)) THEN
                    IF(TIME.LT.TSLTIM(I,GI)) TIME=TSLTIM(I,GI) 
                 ENDIF
              ENDDO   
              STAT = TSLSTS(GI)
              DATE(VCDC) = TSLESD(GI)
           ELSEIF(GT.EQ.TPAS) THEN
             if(pascurdrw(GI).LE.0) GOTO 100
             TIME = PASTIM(pascurdrw(GI), GI)
             STAT = PASSTS(pascurdrw(GI), GI)
             DATE(VCDC) = PASESD(pascurdrw(GI), GI)
           ENDIF
		
           IF (DATE(VCDC) .NE. 0) THEN
	      CALL LCDATE(DATE)
	      WRITE(XNEW(  IND),903) (GLNAMES(K,GNUM),K=1,4),
     *					  GTNAMES(GT),
     *	                                  GI,DRAW,
     *					  (DATE(K),K=7,13),
     *					  DISTIM(TIME),
     *	                                  GSTAT(STAT)
	   ELSE
	      WRITE(XNEW(  IND),905) (GLNAMES(K,GNUM),K=1,4),
     *					  GTNAMES(GT),
     *	                                  GI,DRAW,
     *					  DISTIM(TIME),
     *	                                  GSTAT(STAT)
	   ENDIF
	   IND = IND + 1

100	CONTINUE

	DO I = IND,22
	   WRITE(XNEW(  I),906) 
	END DO

	RETURN
C
902	FORMAT('NAME',15X,'TYPE / IND',9X,'CLOSE DATE',2X,
     *	       'CLOSE TIME',2X,'STATUS')
903	FORMAT(4A4,3X,A8,I2,1X,I4,1X,7A2,2X,A8,4X,A17)
904	FORMAT(4A4,3X,A8,I2,6X,14('-'),' see SCLosnp ',14('-'))
905	FORMAT(4A4,3X,A8,I2,1X,I4,1X,'              ',2X,A8,4X,A17)
906	FORMAT(80(' '))
C
	END
