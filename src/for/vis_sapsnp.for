C
C SUBROUTINE SAPSNP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]SAPSNP.FOV                                   $
C  $Date::   17 Apr 1996 14:50:12                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_sapsnp.for **
C
C SAPSNP.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, AGTINF.DEF for Finland.
C
C V02 18-FEB-94 GPR USE I3 FORMAT FOR SAP TYPE-OUTS
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
	SUBROUTINE SAPSNP(SAP)
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
	INTEGER*4 K, CONN, S, STAT2, STAT1, CON2, CON1, SAP2, SAP1, PNT
	INTEGER*4 SAP
C
	CHARACTER*10 ACONN(0:6)
C
	DATA ACONN /'illegal   ',
     *	            'closed    ',
     *	            'find pennd',
     *	            'conn pennd',
     *	            'open      ',
     *	            'idle      ',
     *	            'disc pennd'/
C
	IF(LANGO.NE.LANPROUP) THEN
	   WRITE(CLIN23,990)
990	   FORMAT('Network not initialized yet ... please wait ')
	   RETURN
	ENDIF
C
	IF(SAP.LE.0.OR.SAP.GT.MAXSAP) THEN
	   SAP=1
	ENDIF
C
	WRITE(CLIN1,901) SAP,LANTIMER
901	FORMAT('SAP snapshot ',I3.3,' at ',I9)				! V02
C
	WRITE(CLIN2,900)
C
	WRITE(CLIN3,903) LANSAPSTS(SAP),QUESAP(SAP)
903	FORMAT('Sap status.......:  ',I10,10X,
     *	       'Application queue:  ',I10)
C
	WRITE(CLIN4,904) LANTYPE(SAP),LANMODE(SAP)
904	FORMAT('Sap type.........:  ',I10,10X,
     *	       'Sap mode.........:  ',I10)
C
	WRITE(CLIN5,900)
900	FORMAT(80(' '))
C
	PNT=6
C
	SAP1=0
	SAP2=0
	CON1=0
	CON2=0
	STAT1=0
	STAT2=0
	DO 100 S=1,MAXSAP
	CONN=CONNECTION(SAP,S)
	IF(LANCONN(CONN).NE.CSAPCLO) THEN
	   IF(SAP1.EQ.0) THEN
	      SAP1=S
	      CON1=CONN
	      STAT1=LANCONN(CONN)
	   ELSEIF(SAP2.EQ.0) THEN
	      SAP2=S
	      CON2=CONN
	      STAT2=LANCONN(CONN)
	      WRITE(XNEW(  PNT),905) SAP1,CON1,ACONN(STAT1),
     *	                             SAP2,CON2,ACONN(STAT2)
905	      FORMAT('RemSAP [CON-STAT]:',I4,'[',I4,'-',A10,']',1X,
     *	             'RemSAP [CON-STAT]:',I4,'[',I4,'-',A10,']',1X)	! V02
	      PNT=PNT+1
	      SAP1=0
	      SAP2=0
	      CON1=0
	      CON2=0
	      STAT1=0
	      STAT2=0
	   ENDIF
	ENDIF
100	CONTINUE
	IF(SAP1.GT.0) THEN
	   WRITE(XNEW(  PNT),906) SAP1,CON1,ACONN(STAT1)
906	   FORMAT('RemSAP [CON-STAT]:  ',I4,'[',I4,'-',A10,']')		! V02
	   PNT=PNT+1
	ENDIF
C
CCC	DO 300 K=PNT,PNT+MAXSAP/2-1					! V02
	DO 300 K=PNT,24			! Only 24 line Display.		! V02
					! This could cause an aritmetic	! V02
					! trap if a SAP has > (24-6+1)*2! V02
					! connections to other SAPs!!!	! V02
	WRITE(XNEW(  K),900)
300	CONTINUE
C
	RETURN
	END
