C
C *** SUBROUTINE MSCFLUSH ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSCFLUSH.FOV                                 $
C  $Date::   17 Apr 1996 14:05:50                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msc_sndbuf.for ***
C
C V03 09-DEC-93 RRB INITIATE READ AFTER WRITE AND WAIT FOR RESPONSE
C                   OR TIMEOUT. USED TO BE ASYNCHRONOUS IN NATURE
C                   BUT TELENEX CHANGED THEIR WAY OF DOING THINGS
C		    WITH REV 8.0 AND HIGHER. THIS SIMPLIFIES THIS
C                   LOGIC CONSIDERABLY, RELATIVE TO THE WAY WE 
C                   ACTUALLY TALK TO THE SWITCH.
C V02 10-NOV-93 RRB INCREMENT WRITE TIMOUT COUNTER
C V01 23-DEC-92 RRB VAX INITIAL RELEASE (ISOLATED FROM MSCMGR.FOR)
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
C Purpose:
C	FLUSH EXEC QUEUE ON DROPPED CONNECTION.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MSCFLUSH
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSCCOM.DEF'
C
	INTEGER*4 BUF, STATUS
C
100     CONTINUE
          CALL RTL(BUF,MSCEXEC,STATUS)
          IF(STATUS.EQ.2) RETURN
          CALL MSCRELB(BUF)
        GOTO 100
	END
