C
C WAIT TILL ALL THE CURRENT OUTSTANDING JOBS IN APULOG DONE
C
C V01 30-MAR-1999 WXS Initial release.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	SUBROUTINE WAIT_APUQUE
	IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:GLIST.DEF'

	INTEGER*4 ST
	INTEGER*4 ADDED_TO_THE_QUEUE
C
C	GET NUMBER OF ELEMENTS	ADDED
	
	 ADDED_TO_THE_QUEUE = QUETAB(GLIST_ABL_CNT,APU) 


C	NUMBER OF ELEMENTS ADDED HAS TO BE AT LEAST EQUAL 
C	NUMBER OF ELEMENTS REMOVED TO MAKE SURE THAT WE PROCESSED
C	EVERYTHING OUTSTANDING. IN GENERAL CASE THIS STATEMENT MAY NOT BE TRUE.
C	ONE MAY HAVE TO ADD ELEMENTS ADDED ON THE TOP OF THE QUEUE AND REMOVED
C	FROM THE BOTTOM OF THE QUEUE. THE QUEUE WILL MOST LIKELY NEED NOT TO 
C	BE LOCKED IF WE ADD ELEMENTS THAT ONLY INCREMENT AND COMPARE THEM

100	CONTINUE

	IF (QUETAB(GLIST_RTL_CNT,APU).LT.  ADDED_TO_THE_QUEUE) THEN
	    CALL XWAIT(50,1,ST)
	    GOTO 100
	ENDIF
	RETURN
	END
