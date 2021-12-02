        SUBROUTINE READXX_AUX(RAB, FDB, BEGBLK, BUFFER, STATUS)
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DISKIO.DEF'
	INCLUDE	'($SYSSRVNAM)'
	INCLUDE '($RABDEF)'
	INCLUDE '($RMSDEF)'
C
	RECORD	    /RABDEF/ RAB
	INTEGER*4   FDB(FDB_LENGTH)
	INTEGER*4   BUFFER(*)
	INTEGER*4   STATUS
C
C
C
C
C
C	set # of bytes to transfer, beginning block #, and
C	user buffer address
C
	RAB.RAB$W_USZ = FDB(FDB_BYTSZ)
	RAB.RAB$L_BKT = BEGBLK
C       LOC — Returns the address of a variable
C       %LOC(arg) The %LOC() construct is an expression that yields the value of the location of its argument,
C       arg, in memory. The size of the type of the expression depends on the system—typically, it is equivalent to either
C       INTEGER(KIND=1) or INTEGER(KIND=2), though it is actually type INTEGER(KIND=7).        
        RAB.RAB$L_UBF = %LOC(BUFFER)    !User record buffer address (4 bytes)
C
C	be sure asynchronous bit is OFF
C
C record-processing options -- (ROP)
C The ROP is a 32-bit field in which each record-processing option has a
C corresponding bit assignment to let you specify multiple options (multiple
C bits can be set), when applicable. Each option has a unique symbolic offset and a
C unique mask value but you need only specify the appropriate 3-letter mnemonic
C when coding a function. For example, the end-of-file option is assigned symbolic
C offset RAB$V_EOF, but to specify the option, you use the following MACRO
C statement: ROP=EOF
	RAB.RAB$L_ROP = RAB.RAB$L_ROP .AND. .NOT.RAB$M_ASY
C
C Now do the READ
C
C
C Service Name | Macro Name  | Description
C SYS$READ     | $READ       | Retrieves a specified number of bytes from 
C                            | a file, beginning on block boundaries
C
	STATUS = SYS$READ(RAB)
	FDB(FDB_IOLEN) = ZEXT(RAB.RAB$W_RSZ) !(ZEXT is zero extend)
	IF(STATUS)THEN
C
C If we didn't get everything, return 144 for warning
C
	  IF(FDB(FDB_IOLEN).NE.FDB(FDB_BYTSZ))THEN
	    STATUS = 144
	  ELSE
	    STATUS = 0		    ! FOR COMPATIBILITY
	  ENDIF
	ENDIF
C
	IF(STATUS .EQ. RMS$_EOF)THEN
	  STATUS = 144		    ! FOR COMPATIBILITY
	ENDIF
C
	FDB(FDB_STAT) = STATUS
C
	RETURN
	END