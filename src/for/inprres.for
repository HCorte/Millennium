C
C SUBROUTINE INPRRES
C
C V01 30-OCT-2003 FRP  Modify for Batch2 Totobola Changes.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
      SUBROUTINE INPRRES(STRING, NUM, CANCEL_EVENT, CHECK_COHERENCE, EXT)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:PRMSPT.DEF'
C
      ! arguments
      CHARACTER  STRING*(*)
C
      INTEGER * 4  NUM
      INTEGER * 4  EXT
      INTEGER * 4 CANCEL_EVENT
C
      LOGICAL CHECK_COHERENCE
C
      ! variables
      INTEGER * 4  INBUF(5)
C
      CHARACTER  CBUF(20)
C
      EQUIVALENCE(INBUF,CBUF)
C
10    CONTINUE
      EXT = 0
      NUM = 0
      CALL WIMG(5,STRING)
      READ (5,900) INBUF
C
C CHECK FOR EXIT
C
      IF((CBUF(1) .EQ. 'E' .OR. CBUF(1) .EQ. 'e') .AND.  CBUF(2) .EQ. ' ') THEN
         EXT = -1
         RETURN
      ENDIF
C
      IF(CBUF(1) .EQ. '0' .AND. CBUF(2) .EQ. ' ') THEN
         NUM = 1
         IF(CANCEL_EVENT .NE. 0 .AND. CHECK_COHERENCE .EQ. .TRUE.) THEN
           WRITE(5, 902)
           GOTO 10
         ENDIF
         RETURN
      ENDIF
C
      IF(CBUF(1) .EQ. '1' .AND. CBUF(2) .EQ. ' ') THEN
         NUM = 2
         IF(CANCEL_EVENT .NE. 0 .AND. CHECK_COHERENCE .EQ. .TRUE.) THEN
           WRITE(5, 902)
           GOTO 10
         ENDIF
         RETURN
      ENDIF
C
      IF((CBUF(1) .EQ. 'M' .OR. CBUF(1) .EQ. 'm') .AND. CBUF(2) .EQ. ' ') THEN
         NUM = 4
         IF(CANCEL_EVENT .NE. 0 .AND. CHECK_COHERENCE .EQ. .TRUE.) THEN
           WRITE(5, 902)
           GOTO 10
         ENDIF
         RETURN
      ENDIF
C
      IF((CBUF(1) .EQ. 'C' .OR. CBUF(1) .EQ. 'c') .AND. CBUF(2) .EQ. ' ') THEN
         NUM = WINNUM_CANEVENT ! WINNUM_CANEVENT = 3
         IF(CANCEL_EVENT .EQ. 0 .AND. CHECK_COHERENCE .EQ. .TRUE.) THEN
           WRITE(5, 903)
           GOTO 10
         ENDIF
         RETURN
      ENDIF
C
      WRITE(5,901)
      GOTO 10
C
C
900   FORMAT(5A4)
901   FORMAT(' Invalid input - Enter 0, 1, M or C only')
902   FORMAT(X, 'Invalid input, Event cancelled only "C" input is allowed')
903   FORMAT(X, 'Invalid input, Event not cancelled "C" input isn''t allowed, enter 0, 1 or M')
C
      END
