C
C SUBROUTINE X2GETPER
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GETPER.FOV                                 $
C  $Date::   17 Apr 1996 16:19:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_x2relsnp.for;1 **
C
C 
C V02 01-SEP-95 DAS INCORPORATED LEIPZIG CHANGES (DXG) FOR MULTIPLE APPLICATIONS
C V01 02-JUN-95 XXX INITIAL RELEASE
C
C ========================================================
C This subroutine will determine the percentage of the
C download which has been sent.
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
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE X2GETPER(APPLICATION_NO,LOAD,SEGMENT,
     *                      PERALL,PERSEL,PERLOAD)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:MSGCOM.DEF'
	INCLUDE 'INCLIB:PRMDLL.DEF'				     
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        REAL*4   PERALL         !PERCNET LEFT
        REAL*4   PERSEL         !PERCENT LEFT FOR SELECTIVE
        REAL*4   PERLOAD        !PERCNET LEFT IN CURRENT LOAD
C
        INTEGER*4   SEGMENT, I
        INTEGER*4   APPLICATION_NO !WHICH APPLICATION
        INTEGER*4   LOAD           !CURRENT LOAD NUMBER
        INTEGER*4   CURCNT         !CURRENT LOAD SIZE
        INTEGER*4   TOTSEG /0/     !TOTAL NUMBER OF SEGMENTS
        INTEGER*4   TOTSEL /0/     !TOTAL SEGMENTS FOR SELECTIVE
        INTEGER*4   CURSEG /0/     !LEFT SEGMENTS
        INTEGER*4   CURSEL /0/     !LEFT SEGMENT FOR SELECTIVE
C
        PERALL=0.0
        PERSEL=0.0
        PERLOAD=0.0
        TOTSEG=0

        TOTSEL=0
        IF(LOAD.LE.0 .OR. LOAD.GT.MAXLOADS) RETURN
        IF(SEGMENT.LE.0 .OR. SEGMENT.GT.SMFDLTAB(LOAD,NBRSEG,
     *                       APPLICATION_NO)) RETURN
C
C COUNT THE TOTAL NUMBER OF SEGMENTS.
C
        IF(LOAD.LT.MAXLOADS) THEN
          DO 100 I=1,MAXLOADS
            IF(SMFDLTAB(I,LODADR,APPLICATION_NO).NE.0) THEN
              TOTSEG=TOTSEG+SMFDLTAB(I,NBRSEG,APPLICATION_NO)
              IF(SMFDLTAB(I,SNDFLG,APPLICATION_NO).EQ.1) THEN
                TOTSEL=TOTSEL+SMFDLTAB(I,NBRSEG,APPLICATION_NO)
              ENDIF
            ENDIF
100     CONTINUE
        ENDIF
C
C DETERMINE THE NUMBER OF SEGMENTS LEFT
C
        CURSEL=0
        CURSEG=0
        IF(LOAD.LT.MAXLOADS) THEN
          DO 200 I=MAXLOADS,LOAD+1,-1
            CURSEG=CURSEG+SMFDLTAB(I,NBRSEG,APPLICATION_NO)
            IF(SMFDLTAB(I,SNDFLG,APPLICATION_NO).EQ.1) THEN
              CURSEL=CURSEL+SMFDLTAB(I,NBRSEG,APPLICATION_NO)
            ENDIF
200       CONTINUE
        ENDIF
C
C GET THE CURRENT LOAD COUNT.
C
        CURCNT=SMFDLTAB(LOAD,NBRSEG,APPLICATION_NO)
        CURSEG=CURSEG+(CURCNT-SEGMENT)
        CURSEL=CURSEL+(CURCNT-SEGMENT)
C
C CALCULATE THE PERCENTAGE LEFT.
C
        IF(TOTSEG.GT.0) THEN
          PERALL=(FLOAT(CURSEG)/TOTSEG)*100
        ELSE
          PERALL=0.0
        ENDIF
        IF(TOTSEL.GT.0) THEN
          PERSEL=(FLOAT(CURSEL)/TOTSEL)*100
        ELSE
          PERSEL=0.0
        ENDIF
        IF(CURCNT.GT.0) THEN
          PERLOAD=(FLOAT((CURCNT-SEGMENT))/CURCNT)*100
        ELSE
          PERLOAD=0.0
        ENDIF
        RETURN
        END
