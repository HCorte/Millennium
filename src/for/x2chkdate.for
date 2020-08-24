C
C SUBROUTINE X2CHKDATE
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CHKDATE.FOV                                $
C  $Date::   30 May 1996  9:27:52                                         $
C  $Revision::   1.1                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2chkdate.for **
C
C X2CHKDATE.FOR
C
C V04 12-FEB-2001 EPH Accept any size for AGTINF
C V03 01-SEP-1999 UXN COMMON variable ERRORS added.
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, rearranged AGTINF.DEF for Finland
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will check the start/end dates in 
C
C This subroutine will check the start/end dates in the ASF file
C and will disable or enable terminals in the X2X database.
C
C Input parameters:
C
C	UNIT	Int*4	    Logical unit X2XTER opened on
C	TER	Int*4	    Terminal number
C	AGTINF	Int*4(128)  Agent record
C
C Output parameters:
C
C	None
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
	SUBROUTINE X2CHKDATE(UNIT,TER,AGTINF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XTER.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*2   DATBUF(12)
	INTEGER*4   TER
	INTEGER*4   AGTINF(*)   !v04
	INTEGER*4   DAY,MON,YEAR
	INTEGER*4   CERR,ST
	INTEGER*4   BEGCDC,ENDCDC
	INTEGER*4   UNIT
	CHARACTER   BELL   /'07'X/
	LOGICAL	    UPDATE /.FALSE./
	INTEGER*4   ERRORS(NUMAGT)
	COMMON      ERRORS
C
C EXTRACT THE STARTING DATE.
C
	CERR=0
	CALL ASCBIN(AGTINF,SSDAT,2,DAY,CERR)
	IF(DAY.LT.1 .OR. DAY.GT.31) CERR=-1
	IF(CERR.EQ.0)CALL ASCBIN(AGTINF,SSDAT+2,2,MON,CERR)
	IF(MON.LT.1 .OR. MON.GT.12) CERR=-1
	IF(CERR.EQ.0) CALL ASCBIN(AGTINF,SSDAT+4,2,YEAR,CERR)
	IF(CERR.NE.0) THEN
C  	  TYPE *,IAM(),' Invalid start date in ASF for terminal ',TER,BELL
	  GOTO 8000
	ENDIF
	DATBUF(VMON)=MON
	DATBUF(VDAY)=DAY
	DATBUF(VYEAR)=YEAR
	CALL BDATE(DATBUF)
	BEGCDC=DATBUF(VCDC)
C
C EXTRACT THE ENDING DATE.
C
	CERR=0
	CALL ASCBIN(AGTINF,SEDAT,2,DAY,CERR)
	IF(DAY.LT.1 .OR. DAY.GT.31) CERR=-1
	IF(CERR.EQ.0) CALL ASCBIN(AGTINF,SEDAT+2,2,MON,CERR)
	IF(MON.LT.1 .OR. MON.GT.12) CERR=-1
	IF(CERR.EQ.0) CALL ASCBIN(AGTINF,SEDAT+4,2,YEAR,CERR)
	IF(CERR.NE.0) THEN
	  ENDCDC=99999999
	ELSE
	  DATBUF(VMON)=MON
	  DATBUF(VDAY)=DAY
	  DATBUF(VYEAR)=YEAR
	  CALL BDATE(DATBUF)
	  ENDCDC=DATBUF(VCDC)
	ENDIF
C
C READ THE TERMINAL RECORD FROM THE X2X DATABASE.
C
	CALL READX2X(UNIT,TER,X2XTER_REC,ST)
	IF(X2XTER_REC(1).LE.0) THEN
	  ERRORS(TER) = 1 ! terminal not defined in X2X database
	  GOTO 8000
	ENDIF
C
C CHECK THE TERMINAL STATE IN MEMORY TO VERIFY THAT THE 
C TERMINAL HAS THE CORRECT STATE.
C
	UPDATE=.FALSE.
	IF(BEGCDC.LE.DAYCDC .AND.
     *     ENDCDC.GT.DAYCDC) THEN
	  IF(X2XTER_STATE.EQ.X2XTS_DISABLED) THEN
	    BX2XT_STATE(TER)=X2XTS_DEFINED
	    X2XTER_STATE=X2XTS_DEFINED
	    UPDATE=.TRUE.
C....	    TYPE *,IAM(),'Terminal ',TER,' has been enabled '
	  ENDIF
	ELSE
	  IF(X2XTER_STATE.NE.X2XTS_DISABLED) THEN
	    BX2XT_STATE(TER)=X2XTS_DISABLED
	    X2XTER_STATE=X2XTS_DISABLED
	    UPDATE=.TRUE.
	    TYPE *,IAM(),'Terminal ',TER,' has been disabled ',BELL
D           TYPE *,IAM(),'Daycdc: ',DAYCDC
D           TYPE *,IAM(),'Begcdc: ',BEGCDC,' Endcdc: ',ENDCDC
	  ENDIF
	ENDIF
C
C IF AN UPDATE IS REQUIRED, REWRITE THE TERMINAL RECORD.
C
	IF(UPDATE) THEN
	  CALL WRITX2X(UNIT,TER,X2XTER_REC,ST)
	ENDIF
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
	END
