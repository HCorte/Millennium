C
C LOADSMF.FOR
C
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]LOADSMF.FOV                                  $
C  $Date::   18 Dec 1996 12:00:26                                         $
C  $Revision::   1.2                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C
C V01 18-NOV-95 DAS REMOVED THIS SUBROUTINE FROM X2RESET AND MADE EXTERNAL
C
C
C SUBROUTINE TO LOAD SYSTEM MESSAGE FILE
C
C=======OPTIONS /CHECK=NOOVERFLOW

	SUBROUTINE LOADSMF(UNIT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:MSGCOM.DEF'
C
	INTEGER*4 FDB(7), ST, UNIT
        INTEGER*4 SMF_FILE(5)/'SMF.','FIL ',3*0/
        INTEGER*4 LENGTH
C
C
	TYPE*,IAM(),'Loading message common from SMF file'
	CALL OPENQFILE(UNIT,SMF,ST)
	CALL IOQINIT(FDB,UNIT,16*256)
	IF(ST.NE.0) CALL FILERR(SMF_FILE,1,ST,0)
        LENGTH=%LOC(LAST_MSGCOM)-%LOC(FRST_MSGCOM)+4
	CALL READQIO(FDB,1,FRST_MSGCOM,LENGTH,ST)
	IF(ST.NE.0) CALL FILERR(SMF_FILE,2,ST,1)
	CALL USRCLOSQ1(UNIT)
	TYPE*,IAM(),'Message common load complete'
	RETURN
	END
