C
C SUBROUTINE X2VISADR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2VISADR.FOV                                 $
C  $Date::   17 Apr 1996 16:39:44                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - vis_x2vissub.for **
C
C V03 12-DEC-94 DAS Integrate UK changes into X2X Baseline
C V02 18-AUG-94 GPR HANDLE ASCII DATA FOR X25 ADDRESS
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
	SUBROUTINE X2VISADR(SCRIDX,LEVEL,ADR,LEN,ERR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'				!V02
C
	CHARACTER   ADR*(*)         !Station address in ascii	!V02
        CHARACTER   NULL_X2XADR*(LXADR) ! ALL 0'S.		!V02
	INTEGER*4   SCRIDX          !Screen
	INTEGER*4   LEVEL           !Level of input
	INTEGER*4   LEN             !Length of input
	INTEGER*4   ERR             !Return error code
	INTEGER*4   BCDADR(2)       !BCD station address
	INTEGER*4   ST, STN, I
	LOGICAL*4   DONE	    !Done looking for address	!V02
C
C CONVERT THE BINARY STRING TO THE BCD STRING.
C
	ERR=0
	BCDADR(1)=0
	BCDADR(2)=0
        DO I = 1, LXADR
          NULL_X2XADR(I:I) = '0'
	ENDDO
C
C IF INPUT DATA HAS BEEN PASSED, SEARCH COMMON FOR THE
C INPUT ADDRESS TO FIND THE STATION NUMBER.
C
	IF(ADR.GT.NULL_X2XADR) THEN
	  CALL ATOH(ADR,1,MIN0(LEN,LXADR),BCDADR,ERR)
	  IF(ERR.NE.0) RETURN
	  CALL X2BINSRC(MIN0(LEN,LXADR),BCDADR,STN,ST)
C
C	  Save the station if the address was found
C
	  IF(ST.EQ.0) THEN
	    X2FLDINF(XSTNIDX)=STN
	    X2SCRN(X2SCRN_KEYLEV3+LEVEL,SCRIDX)=XADRIDX
C
C	  Otherwise get the next address just less than
C
	  ELSE
	    I=2
	    DONE=.FALSE.
	    DO WHILE ((I.LE.X2X_STATIONS).AND.(.NOT.DONE))
	      IF((X2X_SORTED_ADR(1,I).GE.BCDADR(1)).AND.
     *           (X2X_SORTED_ADR(2,I).GE.BCDADR(2))) THEN
                X2FLDINF(XSTNIDX)=X2X_SORTED_ADR(0,I-1)
	        DONE=.TRUE.
	      ELSE
	        I=I+1
	      ENDIF
	    ENDDO
	  ENDIF
	ENDIF
C
	RETURN
	END
