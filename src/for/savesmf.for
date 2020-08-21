C
C SAVESMF.FOR
C
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]SAVESMF.FOV                                  $
C  $Date::   18 Dec 1996 12:01:50                                         $
C  $Revision::   1.1                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C
c V01 15-Sep-95 xxx Initial Version 
C
C SUBROUTINE TO LOAD SYSTEM MESSAGE FILE
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE SAVESMF(UNIT)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:MSGCOM.DEF'
C
	INTEGER*4 FDB(7), ST, UNIT, LENGTH
        INTEGER*4 SMF_FILE(5)/'SMF.','FIL ',3*0/
C
C
C
	TYPE*,IAM(),' Saving download in SMF file'
	CALL OPENQFILE(UNIT,SMF,ST)
	CALL IOQINIT(FDB,UNIT,16*256)
	IF(ST.NE.0) CALL FILERR(SMF_FILE,1,ST,0)
	LENGTH=%LOC(LAST_MSGCOM)-%LOC(FRST_MSGCOM(1))+4
C
	CALL WRITEQIO(FDB,1,FRST_MSGCOM,LENGTH,ST)
	IF(ST.NE.0) CALL FILERR(SMF_FILE,1,ST,0)
	CALL USRCLOSQ1(UNIT)
	TYPE*,IAM(),' Saving download in SMF file complete'
	RETURN
	END
C
