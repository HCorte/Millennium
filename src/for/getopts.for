C
C  GXSRC:GETOPTS.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]GETOPTS.FOV                                  $
C  $Date::   17 Apr 1996 13:21:36                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V01 14-APR-92 GCAN INITIAL RELEASE
C
C THIS SUBROUTINE WILL GET COMMAND LINE PARAMETERS.
C
C INPUT:
C	    LENGTH      - LENGTH OF CMDLIN_PARS (LENGTH OF PARAMETERS TO RETURN)
C OUTPUT:
C	    CMDLIN_PARS - RETURNED COMMAND LINE PARAMETERS.
C                         (AS THEY APPEAR, NO SYNTAX CHECKING IS DONE)
C
C NOTE!!!! TASK HAVE TO BE STARTED WITH 'SRUN TASK NAME'
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	SUBROUTINE GETOPTS(CMDLIN_PARS,LENGTH)	
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4   LENGTH		!Length of CMDLIN_PARS to return.
	CHARACTER   CMDLIN_PARS*(*)
C
	INTEGER*4   CMDLEN
	CHARACTER   CMDLIN*80   
C
	INTEGER*4   ST
C
	INTEGER*4   LIB$GET_FOREIGN
	EXTERNAL    LIB$GET_FOREIGN
C
	INTEGER*4   NOFTLSIG
	EXTERNAL    NOFTLSIG
C
	IF(LENGTH.LE.0) RETURN
	IF(LENGTH.GT.80) LENGTH = 80
C
	CALL LIB$ESTABLISH(NOFTLSIG)
C
C Get the command line with which this task was started
C
        ST = LIB$GET_FOREIGN(CMDLIN,,CMDLEN,)
        IF(.NOT.ST)THEN
          TYPE *,IAM(),'GETOPTS - UNABLE TO PROCESS COMMAND'
	  LENGTH = -1
	  RETURN
        ENDIF
C
	CMDLIN_PARS = CMDLIN(1:LENGTH)
C
	RETURN
	END
