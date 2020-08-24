C
C SUBROUTINE LAN_BUFFER_DUMP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]LAN_BUFFER_DUMP.FOV                          $
C  $Date::   17 Apr 1996 13:47:54                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lan_buffer_dump.for;1 **
C
C LAN_BUFFER_DUMP.FOR
C
C V01 30-MAY-92 JWE   Initial relesase
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
	SUBROUTINE  LAN_BUFFER_DUMP(BUFFER_NUMBER)
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4   BUFFER_NUMBER
C
	INTEGER*4   NUMBER_OF_TIMES_RUN	/0/
	SAVE	    NUMBER_OF_TIMES_RUN
C
	INTEGER*4   BUFFER_INDEX
	INTEGER*4   LINE_INDEX
	CHARACTER   OUTPUT_BUFFER*80
C
	BUFFER_INDEX	=   0
	NUMBER_OF_TIMES_RUN =	NUMBER_OF_TIMES_RUN + 1
	IF(NUMBER_OF_TIMES_RUN .GT. 25)GOTO 8000
C
	DO 1000 LINE_INDEX = 1, LANBNUM / 4
	    WRITE(OUTPUT_BUFFER, '(4(1X, Z8))')
	1	(LANBUF(BUFFER_INDEX, BUFFER_NUMBER),
	2	 BUFFER_INDEX = BUFFER_INDEX, BUFFER_INDEX + 3)
	    CALL OPS(OUTPUT_BUFFER, LINE_INDEX, BUFFER_INDEX)
1000	CONTINUE
C
8000	CONTINUE
	RETURN
	END
