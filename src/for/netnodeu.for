C
C FUNCTION NETNODEU
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NETNODEU.FOV                                 $
C  $Date::   17 Apr 1996 14:10:26                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92
C  DEC Baseline
C
C ** Source - ctimtrap.for **
C
C
C
C
	LOGICAL FUNCTION NETNODEU(NODE,WAY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
C
	INTEGER*4  WAY, NODE
C
	NETNODEU=NETTIM(NODE,WAY).GT.0
C
C**   NETNODEU=.TRUE. ;TEST ONLY
C
	RETURN
	END
