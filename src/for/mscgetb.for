C
C *** SUBROUTINE MSCGETB ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSCGETB.FOV                                  $
C  $Date::   17 Apr 1996 14:05:54                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - mscgetb.for ***
C
C V01 10-SEP-90 MRM RELEASED FOR VAX
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
	SUBROUTINE MSCGETB(BUF,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:MSCCOM.DEF'
C
	INTEGER*4 STATUS, BUF
C
	STATUS=0
	BUF=0
	CALL RTL(BUF,MSCFREE,STATUS)
	IF(STATUS.NE.2) THEN
	 IF(MSC_TEST.NE.0) THEN
	  IF(BUF.LT.1.OR.BUF.GT.MSCBNUM) THEN
	   TYPE*,IAM(),'.... ILLEGAL GET BUF NUMBER ...: ',BUF
	   STATUS=2
	  ENDIF
	 ENDIF
	 CALL FASTSET(0,MSCBUF(1,BUF),MSCBLEN)
D	 TYPE*,IAM(),'**** GETB BUF ****[',BUF,']'
	ELSE
D	 TYPE*,IAM(),'**** GETB NO BUFFERS AVAILABLE - BUF = ',BUF
	ENDIF
	RETURN
	END
