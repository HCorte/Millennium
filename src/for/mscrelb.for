C
C *** SUBROUTINE MSCRELB ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSCRELB.FOV                                  $
C  $Date::   17 Apr 1996 14:06:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - mscrelb.for ***
C
C V01 10-FEB-91 RRB RELEASED FOR VAX
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MSCRELB(BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSCCOM.DEF'
C
	INTEGER*4 BUF,STATUS,TIMES
C
	IF(BUF.LT.1.OR.BUF.GT.MSCBNUM) THEN
	  TYPE*,IAM(),'**** ILLEGAL REL BUF NUMBER ... ',BUF
	  CALL GPAUSE
	  TYPE*,IAM(),'**** 1 ILLEGAL REL BUF NUMBER ... ',BUF
	  RETURN
	ENDIF
C
	IF(MSC_TEST.NE.0) THEN
	  IF(BUF.LT.1.OR.BUF.GT.MSCBNUM) THEN
	    TYPE*,IAM(),'**** ILLEGAL REL BUF NUMBER ...: ',BUF
	    CALL GPAUSE
	    TYPE*,IAM(),'**** 2 ILLEGAL REL BUF NUMBER...: ',BUF
	    RETURN
  	  ENDIF
	ENDIF
C
	IF(MSC_TEST.NE.0) THEN
	 CALL CHKQUEUE(BUF,MSCFREE,TIMES)
	 IF(TIMES.NE.0) THEN
	  TYPE*,IAM(),'**** FREE LIST CORRUPTED (BUF,COUN)...: ',BUF,TIMES
	  CALL GPAUSE
	  TYPE*,IAM(),'**** 3 FREE LIST CORRUPTED (BUF,COUN)...: ',BUF,TIMES
	  RETURN
	 ENDIF
	ENDIF
C
	STATUS=0
C
	CALL ABL(BUF,MSCFREE,STATUS)
C
	IF(STATUS.NE.0) THEN
D        TYPE*,IAM(),'**** BAD FREE LIST (REL) BUF..: ',BUF
         CALL OPS('MSCRELB: **** FREE LIST CORRUPTED ****',BUF)
	ENDIF
C
	BUF=0
	RETURN
	END
