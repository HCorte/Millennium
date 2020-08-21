C
C V03 13-JUL-2011 FJG Change default CPU to 0
C V02 25-JAN-2001 UXN Try forever if SYS$PROCESS_AFFINITY fails.
C V01 31-JUL-2000 UXN Initial release.
C
C This subroutine is used to start all online processes.
C All processes will be created as detached processes and will
C run on CPU 1.
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
	SUBROUTINE START(I8PRCNAM)
	IMPLICIT NONE
	INCLUDE '($SYSSRVNAM)'
	INCLUDE '($SSDEF)'
	INCLUDE '($SYIDEF)'
	INCLUDE '($CAPDEF)'

	INTEGER*8 I8PRCNAM

	INTEGER*4 CPUID                      ! RUN ON CPU 1 ONLY
	PARAMETER (CPUID = 0)

	INTEGER*4 ST
	INTEGER*8 SELECT_MASK
	INTEGER*8 MODIFY_MASK
	INTEGER*8 PREV_MASK
	INTEGER*4 I,CPUCNT

	STRUCTURE /ITEM/    
	    INTEGER*2 LEN
	    INTEGER*2 CODE
	    INTEGER*4 BUFF
	    INTEGER*4 RETL
	END STRUCTURE
	
	RECORD/ITEM/ ITEMS(2)
	INTEGER*4 RETL

        INTEGER*2 ERROR_LENGTH
        CHARACTER ERROR_TEXT*256        !SYS$GETMSG error string
C
	INTEGER*8    TEMP
	CHARACTER*8  PROGNAM
	EQUIVALENCE(PROGNAM,TEMP)
	INTEGER*4    I4PRCNAM,PREFIX_LEN,LEN
C
	CHARACTER*15 PRCNAM 
	EQUIVALENCE(I4PRCNAM,PRCNAM)
	INTEGER*8 I8IMAGE_NAME,I8IMG
C
	INTEGER*4 WAIT_CNT
C
	I8IMAGE_NAME = I8PRCNAM
	GOTO 10
C
	ENTRY START2(I8IMG, I8PRCNAM)
C
	I8IMAGE_NAME = I8IMG
C
10	CONTINUE
C
C Start detached process.
C
	CALL XRUNTSK_DET(I8IMAGE_NAME, I8PRCNAM, .FALSE.)
C
	WAIT_CNT = 0
50	CONTINUE
C
C GET CPU COUNT
C
	ITEMS(1).LEN  = 4
	ITEMS(1).CODE = SYI$_ACTIVECPU_CNT
	ITEMS(1).BUFF = %LOC(CPUCNT) 
	ITEMS(1).RETL = %LOC(RETL)
	
	ITEMS(2).LEN  = 0   ! TO TERMINATE THE LIST
	ITEMS(2).CODE = 0

	ST = SYS$GETSYIW(,,,ITEMS,,,)
	IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
	IF(CPUCNT.LT.2.OR.CPUCNT.LE.CPUID) RETURN
C
	CALL GETPRFX(I4PRCNAM,PREFIX_LEN)
C
	TEMP = I8PRCNAM
C
	LEN = INDEX(PROGNAM,' ') - 1
	IF(LEN .LE. 0) LEN = 8
	
	PRCNAM(PREFIX_LEN+1:) = PROGNAM(1:LEN)


D	TYPE*,'There are ', CPUCNT, ' CPUs in the system'
C
	SELECT_MASK = 0
	DO I=0,CPUCNT-1
	   SELECT_MASK = IBSET(SELECT_MASK,I)
	ENDDO
	MODIFY_MASK = ISHFT(1,CPUID)         

	ST = SYS$PROCESS_AFFINITY(,PRCNAM(:PREFIX_LEN+LEN),
     *                            SELECT_MASK,MODIFY_MASK,PREV_MASK,)
	IF(ST.EQ.SS$_NONEXPR) THEN
D	    TYPE*,PRCNAM(:PREFIX_LEN+LEN),' DOES NOT EXIST'
	ELSEIF(.NOT.ST) THEN
	    CALL SYS$GETMSG(%VAL(ST),ERROR_LENGTH,ERROR_TEXT,,)
	    CALL OPSTXT('NRM_START():'//ERROR_TEXT(:ERROR_LENGTH)// 
     *                  ' --- Retrying...')
	    WAIT_CNT = WAIT_CNT + 1
	    IF(WAIT_CNT.GT.3) THEN
	       CALL OPSTXT('Failed to affinitize '//PRCNAM// ' to CPU 1')
	       RETURN
	    ENDIF
	    CALL XWAIT(1,2,ST)
	    GOTO 50
	ENDIF
C
	END
