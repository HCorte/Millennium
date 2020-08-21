C
C SUBROUTINE BOX
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CLR_GTNSCR.FOV                               $
C  $Date::   17 Apr 1996 12:37:06                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, AGTINF.DEF for Finland.
C
C ** Source - vis_gtnsnp.for **
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
C
C  SUBROUTINE TO CLEAR A 'BOX' ON THE GTNET SCREEN
C  CORRESPONDING TO A SYSTEM WHICH IS NOT CONNETCTED TO US
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CLR_GTNSCR(ROW,COL)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
C
	INTEGER*4  DWNLEN, ACRLEN
	PARAMETER (DWNLEN=4)
	PARAMETER (ACRLEN=11)
C
	INTEGER*4 I, K, SPAC, COL, ROW
C
	DATA SPAC /'    '/
C
	DO 60 K=1,ACRLEN
	DO 60 I=1,DWNLEN
	WRITE (XNEW(ROW+I-1)(COL+K-1:COL+K-1),900) SPAC
60	CONTINUE
C
900	FORMAT(A1)
	RETURN
	END
