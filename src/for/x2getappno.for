C
C X2GETAPPNO.FOR
C
C
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GETAPPNO.FOV                               $
C  $Date::   17 Apr 1996 16:19:06                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C
C
C V01 18-AUG-95 XXX INITIAL RELEASE
C 
C THIS ROUTINE GETS THE APPLICATION NUMBER ASSOCIATED WITH THE REVISION
C NUMBER. WILDCARDS ARE PROCESSED 
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2GETAPPNO(ACT_ROMREV,APPLICATION_NO,FORE_BACKGND)
C
	IMPLICIT    NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSGCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
        INCLUDE 'INCLIB:X2RLOD.DEF'
	
	BYTE	    ACT_ROMREV(4)
	INTEGER*4   APPLICATION_NO
	INTEGER*4   FORE_BACKGND
C
	INTEGER*4   LOAD
	BYTE	    WILDCARD /42/	    ! = ASCII '*' USED AS WILDCARD IN
					    ! ACT_ROMREV
C
	INTEGER*4 I4
	INTEGER*2 I2(2)
	EQUIVALENCE (I4,I2)
	BYTE	  I1(4)
	EQUIVALENCE (I4,I1)
C
	INTEGER*4 OFFSET
C
	DO 200,APPLICATION_NO=1,MAXAPP
	    I2(1) = SMFDLTAB(MCP_LOAD_NO,DLL_ROMREV, APPLICATION_NO)
	    I2(2) = SMFDLTAB(MCP_LOAD_NO,DLL_ROMREV1,APPLICATION_NO)
C
	    DO OFFSET=1,4
	       IF (I1(OFFSET).NE.ACT_ROMREV(OFFSET).AND.
     *		   I1(OFFSET).NE.WILDCARD) GOTO 200
	    ENDDO
C
C           ROMREV'S MATCH
C           NOW CHECK IF APPLICATION IS FOREGROUND OR BACKGROUND
C           WE HAVE TO LOOK AT ALL LOADS, SOME LOADS COULD BE MISSING,
C           SO WE GET 0 IN THE FORGROUND_FLAG FIELD
C
	    DO LOAD=1,MAXLOADS
	      IF (SMFDLTAB(LOAD,FOREGROUND_FLAG,APPLICATION_NO)
     *		    .EQ.FORE_BACKGND) RETURN  
	    ENDDO
200	CONTINUE
C
	APPLICATION_NO = -1
C
	RETURN
	END
