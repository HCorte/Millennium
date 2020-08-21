C SUBROUTINE X2BSET.FOR
C  
C V02 15-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V01 13-DEC-1994 GPR RELEASED FOR UK
C
C Subroutine to update x2post queue and bitmap in the record
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	SUBROUTINE X2BSET(BITMAP,FIELD,FILE,RECORD)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
	INCLUDE 'INCLIB:X2XPOST.DEF'
	INCLUDE 'INCLIB:X2XFIL.DEF'
C
	INTEGER*4 BITMAP(*)	!BITMAP TO BE UPDATED
	INTEGER*4 FIELD		!FIELD TO BE UPDATED
	INTEGER*4 FILE		!FILE NO TO BE UPDATED
	INTEGER*4 RECORD	!RECORD NO UPDATED

	INTEGER*4 STATUS
	INTEGER*4 LAST_STATUS/0/	
	INTEGER*4 QUE_REF(100) /100*0/
	INTEGER*4 UPDATE_FILE, NEXT_FILE


C
	CALL BSET(BITMAP,FIELD)
	IF (X2POST_COMMON_STATUS.NE.X2POST_COMMONS_ONLINE) RETURN
C
C	FIND THE QUEUE TO BE ADDED TO
C
	IF (QUE_REF(FILE).EQ.0) THEN
	    DO 100, NEXT_FILE=1,X2XFIL_MAX_FILES
		IF (X2XFIL_FILE_LIST(NEXT_FILE).EQ.FILE) THEN
		    QUE_REF(FILE)=NEXT_FILE
		    GOTO 110
		ENDIF
100	    CONTINUE
	    RETURN	    !FILE NOT FOUND
	ENDIF

110	CONTINUE
	UPDATE_FILE=QUE_REF(FILE)
C
	CALL ABL(RECORD,X2POST_QUE(1,UPDATE_FILE),STATUS)
	IF (STATUS.EQ.GLIST_STAT_FULL) THEN
	    IF (LAST_STATUS.NE.GLIST_STAT_FULL) THEN
     	        TYPE *,'post queue full, will not be able to modify ',
     *			  'more online '
		TYPE *,'This element will not added ',
     *			  'without full/fast edit check'
	    ENDIF
	ENDIF
	LAST_STATUS = STATUS	
C
	RETURN
	END
