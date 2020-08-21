C
C SUBROUTINE CMDXTER
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CMDXTER.FOV                                  $
C  $Date::   17 Apr 1996 12:40:02                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - cmdxsub.for **
C
C V02 31-JUK-95 DAS Added call to x2cnvdrp
C
C
C =============================================================
C CMDXTER
C
C This subroutine loads the terminal information into
C common.
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
	SUBROUTINE CMDXTER(FIELD,ALLREC,ADDFLG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XTER.DEF'
C
	INTEGER*4   FIELD           !Modified field
	INTEGER*4   ALLREC(128)      !Record buffer
	INTEGER*4   ADDFLG          !New network port
	INTEGER*4   TER             !Terminal number
	INTEGER*4   INDX            !Array index
	INTEGER*4   TEMP            !Work variable
	CHARACTER   C2DROP*2	    !Drop address
C
C UPDATE COMMON WITH THE TERMINAL INFORMATION.
C
	CALL FASTMOV(ALLREC,X2XTER_REC,128)
	TER=X2XTER_TER
        IF(TER.LE.0 .OR. TER.GT.X2X_TERMS) GOTO 8000
	IF(ADDFLG.EQ.0) THEN
	  X2XS_NUM_TERMS(X2XTER_PORT,X2XTER_STN) =
     *	    X2XS_NUM_TERMS(X2XTER_PORT,X2XTER_STN) + 1
  	  C2DROP=X2XTER_DROP
          CALL X2CNVDRP(C2DROP, INDX)       !.....v02
          IF(INDX.LT.0) GOTO 8000
          X2XS_TERMS(INDX,X2XTER_PORT,X2XTER_STN)=X2XTER_TER
	ENDIF
C
	IF(FIELD.EQ.2) X2XT_STATION_NO(TER)=X2XTER_STN
	IF(FIELD.EQ.3) CALL ISBYTE(X2XTER_PORT,
     *	                      IX2XT_STATION_PORT,TER-1)
	IF(FIELD.EQ.4) THEN
          X2XT_DROP_AD(TER) = X2XTER_DROP
	ENDIF
C
	IF(FIELD.EQ.5) CALL ISBYTE(X2XTER_STATE,IX2XT_STATE,TER-1)
C
8000    CONTINUE
	RETURN
	END
