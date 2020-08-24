C
C SUBROUTINE HRSSNP
C  
C V07 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V06 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V05 04-MAY-1994 HXK FIXED TYPO.
C V04 04-MAY-1994 HXK FIX FROM IRFSS #10128
C V03 11-JUN-1993 HXK ADDED AGTINF.FCC, PRMAGT.FCC
C V02 21-JAN-1993 DAB Initial Release Based on Netherlands Bible,
C                     12/92, and Comm 1/93 update DEC Baseline
C V01 15-FEB-1991 WOL INITIAL RELEASE FOR VAX
C
C
C VIS_HRSSNP.FOR
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
	SUBROUTINE HRSSNP(GNUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
        INTEGER*4 GNUM,GTYP,GIND
	INTEGER*4 I,IND,GTOTAL,NTOTAL
	INTEGER*4 TIME(2,24),GROSS(24),NET(24)
	DATA TIME/1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 7,8, 8,9, 9,10, 
     *            10,11, 11,12, 12,13, 13,14, 14,15, 15,16, 16,17,
     *            17,18, 18,19, 19,20, 20,21, 21,22, 22,23, 23,24,
     *            24,1/
C
C GET GAME NUMBER
C
	IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
	  RETURN
	ENDIF
	GTYP=GNTTAB(GAMTYP,GNUM)
	GIND=GNTTAB(GAMIDX,GNUM)
	IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) THEN
	  WRITE(CLIN23,923) GNUM
	  RETURN
	ENDIF
	SMODE=.TRUE.
	WRITE(CLIN1,901)
        WRITE(CLIN3,903) GNUM,(GLNAMES(I,GNUM),I=1,4),
     *                   GTNAMES(GTYP),GIND
        WRITE(CLIN5,905)
	IND=6
	GTOTAL=0
	NTOTAL=0
	DO 100 I=1,24
	   NET(I)=HOURSAL(GNUM,2,I)-NTOTAL
	   GROSS(I)=HOURSAL(GNUM,1,I)-GTOTAL
	   NTOTAL=NTOTAL+NET(I)
	   GTOTAL=GTOTAL+GROSS(I)
	   IF(NET(I).LT.0) NET(I)=0
	   IF(GROSS(I).LT.0) GROSS(I)=0
100	CONTINUE
	DO 200 I=1,12
	WRITE(XNEW(IND),906) TIME(1,I),TIME(2,I),
     *                       CMONY(GROSS(I),10,BETUNIT),
     *	                     CMONY(NET(I),10,BETUNIT),
     *                       TIME(1,I+12),TIME(2,I+12),
     *                       CMONY(GROSS(I+12),10,BETUNIT),
     *                       CMONY(NET(I+12),10,BETUNIT)
 
	IND=IND+1
200	CONTINUE
	RETURN
C
C
901	FORMAT(3X,'Games Hours Snapshot')
903     FORMAT(3X,'Game ',I2,':   ',4A4,'  (',A8,'/',I1,')')
905     FORMAT(2(3X,'| hours |gross sales   net  sales  '))
906     FORMAT(2(3X,'| ',I2,'-',I2,' | ',A10,'   ',A10,'  '))
923	FORMAT('Sorry, game ',I2,' not active ')
	END
