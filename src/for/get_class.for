C
C This subroutine returns base class for equivalence class
C specified in STNDEF.FIL
C
C V01 7-MAY-98 UXN    Initial release.
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	SUBROUTINE GET_CLASS(EQUIV)
	IMPLICIT NONE
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE '(LIB$ROUTINES)'
C
	INTEGER*4	EQUIV(X2XC_CLASSES)
	INTEGER*4	ST,LUN,I
	INTEGER*4	CLS,TMP
	CHARACTER*80	CLINE
C
	ST = LIB$GET_LUN(LUN)
	IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
	OPEN(UNIT=LUN,FILE='STNDEF.FIL',STATUS='OLD',IOSTAT=ST,
     *       ORGANIZATION='SEQUENTIAL',ACCESS='SEQUENTIAL')
	IF(ST.NE.0) RETURN
	READ(UNIT=LUN,IOSTAT=ST,FMT=900) CLINE
	DO WHILE(ST.EQ.0)	
	  IF(CLINE(1:1).EQ.' ') THEN
	    READ(CLINE(3:6),FMT='(I4)') CLS
	    IF(CLS.GE.1.AND.CLS.LE.X2XC_CLASSES) EQUIV(CLS) = CLS
	    DO I=0,9
              READ(CLINE(23+I*5:26+I*5),FMT='(I4)') TMP
	      IF(TMP.GE.1.AND.TMP.LE.X2XC_CLASSES) EQUIV(TMP)=CLS
	    ENDDO
	  ENDIF
	  READ(UNIT=LUN,IOSTAT=ST,FMT=900) CLINE
	ENDDO
	CLOSE(LUN)
	ST=LIB$FREE_LUN(LUN)
900	FORMAT(A80)
	END
