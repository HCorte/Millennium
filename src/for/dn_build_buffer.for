C
C SUBROUTINE DN_BUILD_BUFFER
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_BUILD_BUFFER.FOV                          $
C  $Date::   17 Apr 1996 12:56:46                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - dn_build_buffer.for;1 **
C
C DN_BUILD_OPEN.FOR
C
C V01 16-APR-91 JWE Initial release
C
C Build an open connection buffer for DCNPRO
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
	SUBROUTINE DN_BUILD_BUFFER(BUFFER,FUNCTION_CODE,SOURCE,DATA,
	1   DATA_LENGTH,PBLOCK,SYSTEM)
	IMPLICIT NONE
C
	INCLUDE '($SSDEF)'
	INCLUDE '($SYSSRVNAM)'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'
	INCLUDE 'INCLIB:DN_BLOCK.DEF'
C
	RECORD/DN_BUFFER_STRUCT/BUFFER	!Buffer header to be built
	INTEGER*4   FUNCTION_CODE	!Function code for open
	INTEGER*4   SOURCE		!Task Number of source
	INTEGER*4   DATA_LENGTH		!How long is the data buffer
	BYTE	    DATA	!Buffer to send
	INTEGER*4   PBLOCK(*)		!PBLOCK for NETLOG
	INTEGER*4   SYSTEM		!System to open
C
	INTEGER*4 STATUS		!Return value
	INTEGER*2 ERROR_LENGTH
	CHARACTER ERROR_TEXT*256	!SYS$GETMSG error string
C
	BUFFER.COMMAND=FUNCTION_CODE
	BUFFER.SOURCE=SOURCE
	BUFFER.LINK=SYSTEM
	IF(DATA_LENGTH.NE.0)THEN
		BUFFER.DBUFFER=%LOC(DATA)-%LOC(FRST_NETCOM(1))
	ELSE
		BUFFER.DBUFFER=0
	ENDIF
	BUFFER.DBUFFER_SIZE=DATA_LENGTH
	BUFFER.PBLOCK=%LOC(PBLOCK)
	STATUS=SYS$GETTIM(BUFFER.TIME)
	IF(STATUS.NE.SS$_NORMAL)THEN
	       CALL SYS$GETMSG(%VAL(STATUS),	!Get text for error
	1	  ERROR_LENGTH,			!How long was it
	2	  ERROR_TEXT,			!error text
	3	  ,				!Generate default message    
	4	  )
	       CALL OPS(ERROR_TEXT,STATUS,3)
	ENDIF
	RETURN
	END	   
