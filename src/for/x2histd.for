C
C SUBROUTINE X2HISTD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2HISTD.FOV                                  $
C  $Date::   17 Apr 1996 16:20:28                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_x2dlysnp.for **
C
C
C
C**************************************************************
C
C
C X2HISTD.FTN
C
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, AGTINF.DEF for Finland.
C
C V02 13-DEC-94 GPR Integrate UK changes into X2X Baseline
C V01 18-JUL-91 MF INITIAL RELEASE.
C
C This routine displays the input "distribution" vector in the
C form of a normalized histogram. Counts for entries larger
C than NROW are summed in the last display entry.
C
C The screen layout is as follows:
C
C          -----------------------------------------------
C        1 | Vision Title                     System zone|
C        2 | X25 distribution...                         |
C          -----------------------------------------------
C        3 |(Range Cnt    Relative Frequency )           |
C  (1)   4 | nn     mm  :                                |
C        5 | nn     mm  *****                            |
C          | nn     mm  *****************                |
C          | nn     mm  ****                             |
C          | nn     mm  **                               |
C          | nn     mm  :                                |
C (NROW)22 |max     mm  *                                |
C          -----------------------------------------------
C       23 | Menu / Response                             |
C          -----------------------------------------------
C       24 | Command                                     |
C          -----------------------------------------------
C                       x -------> dispmax
C                      col0
C Calling sequence:
C
C  CALL X2HISTD(VCOUNT,LEN,TYPE)					    !V02
C   where:
C     I*4  VCOUNT  - array of counts
C     I*4  LEN     - its dimension
C     I*4  TYPE	   - type of histogram					    !V02
C		   ---- 1: show delay snapshot histogram
C		   ---- 2: show request-segments snapshot histogram	    !V02
C		   ---- 3: show transactions per GTX  snapshot histogram    !V02
C
C********************
 
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
	SUBROUTINE X2HISTD(VECTOR,START,LEN,INTERVAL,HEADER)		    !V02
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
C
	INTEGER*4     VECTOR(*)      ! array of values
	INTEGER*4     LEN            ! its dimension
	INTEGER*4     START	     ! start of histogram		    !V02
	INTEGER*4     INTERVAL	     ! interval of histogram		    !V02
	CHARACTER     HEADER(*)	     ! header string			    !V02
        INTEGER*4     I, J
C
C Local declarations
C
	INTEGER*4    ROW1,ROW2,NROWS   ! actual VISION row range
	 parameter ( ROW1 = 4,
     *	             ROW2 = 22,
     *	             NROWS= ROW2-ROW1+1 )   ! here: appr 20 rows
C
	REAL*4      VALUE(NROWS)       ! normalized counts/accum. tail
	REAL*4      VALMAX/0./,
     *	            VALTOT/0./         ! used for normalizing
	REAL*4   TEMP
C
	INTEGER*4   COL0,DISPMAX        ! min,max display idx
	 PARAMETER (COL0   = 22,					    !V02
     *	            DISPMAX = 50)
	INTEGER*4   ICX                ! display index
C
C Initialize
C
	DO 10 I= ROW1-1,24             ! clear rows
	 DO 10 J = 1,80
	   CNEW(J,I) = ' '
10	CONTINUE
C
	WRITE(CLIN3,901),(HEADER(I),I=1,4)               ! header	    !V02
C
C  Transfer to local vector of values and find max value
C
C***  IAVE =0
C
	VALTOT = 0.0							    !V02
	VALMAX = 0.0							    !V02
C
	DO 100 I=1,NROWS
	  VALUE(I) = ABS(VECTOR(I+START-1))    ! one never knows...	    !V02
	  VALTOT  = VALTOT + VALUE(I)
	  VALMAX  = MAX(VALMAX, VALUE(I))
100	CONTINUE
C
C Tail of distribution ...
C
C	***** Start V02 changes *****
C
	IF (START.GT.1) THEN
	  DO I=1,START-1
	    TEMP = ABS(VECTOR(I))
	    VALTOT = VALTOT + TEMP
	    VALMAX = MAX(VALMAX,TEMP)
	  END DO
	ENDIF
C
	IF (LEN .GT. NROWS) THEN
	  DO I = NROWS+1,LEN
	    TEMP = ABS(VECTOR(I+START-1))
	    VALTOT = VALTOT + TEMP
	    VALMAX = MAX(VALMAX,TEMP)
	  END DO
	ENDIF
C
	VALMAX = MAX(VALMAX,1.)                      ! if all zeros
C
C Normalize to max value and  display size (DISPMAX cols), and display
C
C**     CAVG = VALAVE * DISPMAX / VALMAX            ;show average
C
	DO 200 I = 1,MIN0(NROWS,LEN)
C
	  ICX = VALUE(I) * DISPMAX/VALMAX
C
	  IF (INTERVAL.GT.1) THEN
	     WRITE( XNEW(  ROW1+I-1),890) (I-2+START)*INTERVAL+1,
     *	      (I-1+START)*INTERVAL,VALUE(I)
	  ELSE
	     WRITE( XNEW(  ROW1+I-1),900) (I-1+START),
     *	      VALUE(I)
	  ENDIF
C
C	***** End V02 changes *****
C
	  IF (VALUE(I) .EQ. 0)  THEN
	    CNEW(COL0,ROW1+I-1) = ':'                ! show min ( = 0)
	  ELSE
	    CNEW(COL0,ROW1+I-1) = '*'                ! show min (<> 0)
	  ENDIF
C
C**     CNEW(COL0+CAVG,ROW1+I-1) = "|"             ; show average
	  CNEW(COL0+DISPMAX,ROW1+I-1) = ':'           ! show max
C
	  IF ( ICX .GT. 0 ) THEN
	    DO 201 J = 1, min(ICX,DISPMAX) -1
	      CNEW(COL0+J,ROW1+I-1) = '*'   ! display in range: col0..DISPmax
201	    CONTINUE
	  ENDIF
C
200	CONTINUE
C
890	FORMAT(1X,I4,'-',I4,T11,F9.0)					    !V02
900	FORMAT(3X,I4,T11,F9.0)						    !V02
901	FORMAT(1X,'[',4A,']       [cnt]   Relative Distribution')	    !V02
	RETURN
	END
