C REQPER.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]REQPER.FOV                                   $
C  $Date::   17 Apr 1996 14:43:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C
C V05 15-Sep-95 das CHANGES FOR background loads
C V04 03-JUL-95 DXG CHANGES FOR MISSING LOADS
C V03 07-JUN-93 WS CHANGES FOR COMPRESSED LOAD
C V02 28-SEP-92 JAN SPEEDED UP
C V01 27-JUL-91 DCA SUBROUTINE TO RETURN PERCENTAGE OF LOAD COMPLETE
C
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
C officer of GTECH, pursuant to written agreement
 
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE REQPER(LOAD,APPLICATION_NO,SEG,PERCENT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:MSGCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 LOAD
	INTEGER*4 APPLICATION_NO
	INTEGER*4 SEG
	REAL*8    PERCENT
	INTEGER*4 MAXL
	PARAMETER(MAXL=(MAXLOADS+2)*MAXAPP)
	REAL*8    PERCENT_OF_LOAD(-1:MAXLOADS,MAXAPP)/MAXL*0.0/
	INTEGER*4 TOTSEG(MAXAPP)/MAXAPP*-1/
	INTEGER*4 LAST_NUM_SEG(MAXAPP) /MAXAPP*0/
	INTEGER*4 LAST_LOAD(MAXAPP)/MAXAPP*0/
	INTEGER*4 I, NBR_SEG
C
	PERCENT=0.D0
C*****	RETURN			!XXXXXXX
C
C
C CALCULATE TOTAL SEGMENTS FOR ENTIRE TERMINAL APPLICATION
C AND CALCULATE TOTAL SEGMENTS REQUESTED BY THIS TERMINAL
C
	IF(TOTSEG(APPLICATION_NO).LT.0) THEN
	   TOTSEG(APPLICATION_NO)=0
	   DO 10 I=1,MAXLOADS
	      NBR_SEG=SMFDLTAB(I,NBRSEG,APPLICATION_NO)
	      IF (SMFDLTAB(I,C_NBRSEG,APPLICATION_NO).NE.0) 
     *		      NBR_SEG=SMFDLTAB(I,C_NBRSEG,APPLICATION_NO)
	      IF(NBR_SEG.EQ.0) GOTO 10
	      TOTSEG(APPLICATION_NO)=TOTSEG(APPLICATION_NO)+NBR_SEG-2
	      LAST_NUM_SEG(APPLICATION_NO)=NBR_SEG-2
	      LAST_LOAD(APPLICATION_NO)=I
10	   CONTINUE
	   IF(TOTSEG(APPLICATION_NO).GT.0) THEN
	    DO 20 I=1,MAXLOADS
	      NBR_SEG=SMFDLTAB(I,NBRSEG,APPLICATION_NO)
	      IF (SMFDLTAB(I,C_NBRSEG,APPLICATION_NO).NE.0) 
     *		  NBR_SEG=SMFDLTAB(I,C_NBRSEG,APPLICATION_NO)
C****V04	      IF(NBR_SEG.EQ.0) GOTO 20
	      IF (NBR_SEG.EQ.0) THEN			    !V04
	          PERCENT_OF_LOAD(I,APPLICATION_NO)=	    !V04
     *		     PERCENT_OF_LOAD(I-1,APPLICATION_NO)    !V04
	      ELSE
	          PERCENT_OF_LOAD(I,APPLICATION_NO)=
     *		    PERCENT_OF_LOAD(I-1,APPLICATION_NO)+
     *	            DFLOAT(NBR_SEG-2)/DFLOAT(TOTSEG(APPLICATION_NO))*100
	      ENDIF
20	    CONTINUE
	   ENDIF
	ENDIF
C
	IF(TOTSEG(APPLICATION_NO).EQ.0)			    RETURN
	PERCENT=PERCENT_OF_LOAD(LOAD-1,APPLICATION_NO)+
     *	      100*DFLOAT(SEG)/DFLOAT(TOTSEG(APPLICATION_NO))
	IF(LOAD.LT.LAST_LOAD(APPLICATION_NO))		    RETURN
	IF(SEG.LT.LAST_NUM_SEG(APPLICATION_NO))		    RETURN
	PERCENT=100.D0
C
	RETURN
	END
