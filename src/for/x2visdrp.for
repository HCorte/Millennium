C
C SUBROUTINE X2VISDRP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2VISDRP.FOV                                 $
C  $Date::   17 Apr 1996 16:40:02                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_x2vissub.for;1 **
C
C
C V02 31-JUL-95 DAS Added call to X2CNVDRP
C
C
C ===========================================================
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
	SUBROUTINE X2VISDRP(SCRIDX,LEVEL,DRP,LEN,ERR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
C
	INTEGER*4   SCRIDX          !Screen
	INTEGER*4   LEVEL           !Level of input
	INTEGER*4   DRP             !Drop address
	INTEGER*4   LEN             !Length of input
	INTEGER*4   ERR             !Return error code
	INTEGER*4   BINDAT	    !Binary data
	CHARACTER   CDROP*2	    !Character drop address
C
	ERR=0
	IF(DRP.NE.0) THEN
	  X2FLDINF(XDRPIDX)=DRP
	  X2SCRN(X2SCRN_KEYLEV3+LEVEL,SCRIDX)=XDRPIDX
C
C ATTEMPT TO GET THE DROP BASED ON OTHER INFORMATION.
C
	ELSE IF(X2FLDINF(XTERIDX).NE.0) THEN
	  CDROP=X2XT_DROP_AD(X2FLDINF(XTERIDX))
          CALL X2CNVDRP(CDROP, BINDAT)             !...V02
	  X2FLDINF(XDRPIDX)=BINDAT
	ENDIF
C
	RETURN
	END
