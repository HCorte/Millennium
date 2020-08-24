C
C SUBROUTINE X2STRIP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2STRIP.FOV                                  $
C  $Date::   17 Apr 1996 16:37:24                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2strip.for;1 **
C
C X2STRIP.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This subroutine will strip off the PTL header from
C a message passed to X2XPRO by X2XMGR.  This routine
C is necessary to pass information to X2XREL in the
C original format as received by X2XMGR.
C
C Input parameters:
C
C     PROBUF      Int*4       PROCOM buffer
C     LAYER       Int*4       Layer message came in on
C
C Output parameters:
C
C     MESLEN      Int*2       Length of message (without PTL header)
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
	SUBROUTINE X2STRIP(PROBUF,LAYER,MESLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XPTL.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
C
	INTEGER*4   PROBUF          !Procom buffer number
	INTEGER*4   LAYER           !Protocol layer
	INTEGER*4   LENGTH          !Length of data portion
	INTEGER*2   MESLEN          !Length of output message
	INTEGER*4   TEMP, I, PTLHDROFF
C
C GET THE OFFSET OF THE MESSAGE FROM THE PTL HEADER.
C ALSO GET THE LENGTH FROM THE FE HEADER.
C
	CALL ILBYTE(PTLHDROFF,PRO(INPTAB,PROBUF),X2PRO_OFFSET-1)
	PTLHDROFF=PTLHDROFF-1
C
C IF FRONT END LAYER GET THE LENGTH.
C
	LENGTH=0
	IF(LAYER.EQ.X2X_TRATYP_FE) THEN
	  CALL MOV2TOI4(LENGTH,PRO(INPTAB,PROBUF),
     *	              X2FEMES_MESLEN-1+PTLHDROFF)
	ELSE IF(LAYER.EQ.X2X_TRATYP_STTN) THEN
	  LENGTH=HPRO(INPLEN,PROBUF)-PTLHDROFF
	ENDIF
C
C SHIFT OFF THE PTL HEADER.
C
	MESLEN=0
	DO 100 I=PTLHDROFF+1,PTLHDROFF+1+(LENGTH-1)
	  MESLEN=MESLEN+1
	  CALL ILBYTE(TEMP,PRO(INPTAB,PROBUF),I-1)
	  CALL ISBYTE(TEMP,PRO(INPTAB,PROBUF),MESLEN-1)
100	CONTINUE
C
	RETURN
	END
