C
C SUBROUTINE INPNUM2
C $Log:   GXAFXT:[GOLS]INPNUM2.FOV  $
C  
C     Version: 1.0
C     Creation date: 28 Mar 2008 14:23:24   HXK
C
C ** Source - nrm_inpmod.for **
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE INPNUM2(STRING,NUM,LOW,HIGH,EXT)
	IMPLICIT NONE
C
	CHARACTER   STRING*(*)
	INTEGER*4   NUM,LOW,HIGH,EXT
C
	CALL XXXNUM2(.FALSE., STRING,NUM,LOW,HIGH,EXT)
	RETURN
	END
