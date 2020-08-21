C
C SUBROUTINE ETHSNP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]ETHSNP.FOV                                   $
C  $Date::   17 Apr 1996 13:05:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_ethsnp.for **
C
C ETHSNP.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, AGTINF.DEF for Finland.
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 21-NOV-89 MBK ORIGINAL FOR FINLAND
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ETHSNP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4 TSK2, TSK1, TSKIND, PNT, QUECNT
C
	IF(LANGO.NE.LANPROUP) THEN
	   WRITE(CLIN23,990)
990	   FORMAT('Network not initialized yet ... please wait ')
	   RETURN
	ENDIF
C
	WRITE(CLIN1,901) LANTIMER
901	FORMAT('ETHERNET snapshot ',' at ',I9)
C
	WRITE(CLIN2,900)
C
	WRITE(CLIN3,903)  QUECNT(LANFREE),
     *	                  QUECNT(LANEXTRA)
903	FORMAT('Global free list.:  ',I15,5X,
     *	       'Extra list.......:  ',I15)
C
	WRITE(CLIN4,904)  QUECNT(LANEXEC),NUMSAP
904	FORMAT('To process list..:  ',I15,5X,
     *	       'Active local saps:  ',I15)
C
	WRITE(CLIN5,900)
900	FORMAT(80(' '))
C
	PNT=6
	IF(LANMAXTSK.GT.1) THEN
	   DO 100 TSKIND=1,LANMAXTSK/2
	   TSK1=TSKIND*2-1
	   TSK2=TSKIND*2
         WRITE(XNEW(  PNT),906) LANTASKS(TSK1),QUECNT(LANAPP(1,TSK1)),
     *	                          LANTASKS(TSK2),QUECNT(LANAPP(1,TSK2))
906      FORMAT(A8,'sap appl list',': ',I12,5X,A8,'sap appl list',': ',
     *	          I12)
	   PNT=PNT+1
100	   CONTINUE
	ENDIF
	IF(MOD(LANMAXTSK,2).NE.0) THEN
	   TSK1=LANMAXTSK
	   WRITE(XNEW(  PNT),907) LANTASKS(TSK1),QUECNT(LANAPP(1,TSK1))
907	   FORMAT(A8,'sap appl list',': ',I12)
	   PNT=PNT+1
	ENDIF
C
	WRITE(CLIN8,900)
	PNT=PNT+1
C
	IF(LANMAXTSK.GT.1) THEN
	   DO 200 TSKIND=1,LANMAXTSK/2
	   TSK1=TSKIND*2-1
	   TSK2=TSKIND*2
         WRITE(XNEW(  PNT),908) LANTASKS(TSK1),QUECNT(LANFRAP(1,TSK1)),
     *                          LANTASKS(TSK2),QUECNT(LANFRAP(1,TSK2))
908      FORMAT(A8,'sap free list',': ',I12,5X,A8,'sap free list',': ',
     *	          I12)
	   PNT=PNT+1
200	   CONTINUE
	ENDIF
	IF(MOD(LANMAXTSK,2).NE.0) THEN
	   TSK1=LANMAXTSK
         WRITE(XNEW(  PNT),909) LANTASKS(TSK1),QUECNT(LANFRAP(1,TSK1))
909	   FORMAT(A8,'sap free list',': ',I12)
	   PNT=PNT+1
	ENDIF
C
	RETURN
	END
