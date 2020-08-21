C
C ISDETACHED - THIS FUNCTION RETURNS -
C                    .TRUE.  - IF THE CALLING PROCESS IS A DETACHED PROCESS, 
C		     .FALSE. - OTHERWISE.
C
C V02 18-MAR-2011 RXK Call of GETPRCTYP (copied from Nigeria) replaces previous
C V01 24-JUL-2000 UXN INITIAL RELEASE.
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
C=======OPTIONS/CHECK=NOOVERFLOW
	LOGICAL*4 FUNCTION ISDETACHED
	IMPLICIT NONE
C
        INTEGER*4 ST/-1/

        IF(ST.LT.0)CALL GETPRCTYP(ST)
        IF(ST.EQ.2)THEN
            ISDETACHED=.TRUE.
        ELSE
            ISDETACHED=.FALSE.
        ENDIF
        END

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        SUBROUTINE GETPRCTYP(STATUS)
        IMPLICIT NONE
C
        INCLUDE     '($SYSSRVNAM)'
        INCLUDE     '($JPIDEF)'
        INCLUDE     '($PRCDEF)'
C
        INTEGER*4 STATUS
C
        INTEGER*4   MODE,OWNER
C
        STRUCTURE   /JPISTRUC/
            INTEGER*2   BUFLEN
            INTEGER*2   ITMCOD
            INTEGER*4   BUFADR
            INTEGER*4   LENADR
        END STRUCTURE
C
        RECORD      /JPISTRUC/ ITEMLIST(3)
C
        INTEGER*4   RETURN_LEN
C
        INTEGER*4   ST
C
        ITEMLIST(1).BUFLEN = 4
        ITEMLIST(1).ITMCOD = JPI$_CREPRC_FLAGS
        ITEMLIST(1).BUFADR = %LOC(MODE)
        ITEMLIST(1).LENADR = %LOC(RETURN_LEN)
C
        ITEMLIST(2).BUFLEN = 4
        ITEMLIST(2).ITMCOD = JPI$_OWNER
        ITEMLIST(2).BUFADR = %LOC(OWNER)
        ITEMLIST(2).LENADR = %LOC(RETURN_LEN)
C
        ITEMLIST(3).BUFLEN = 0                  !TO TERMINATE LIST
        ITEMLIST(3).ITMCOD = 0                  !TO TERMINATE LIST
C
        ST = SYS$GETJPIW( ,%VAL(0),,ITEMLIST(1),,,)
        IF(.NOT.ST)THEN
C          CALL LIB$SIGNAL(%VAL(ST))
          ST=-1
          GOTO 10000                            !NORMAL PROMPT
        ENDIF

D       TYPE*,'JPI$_CREPRC_FLAGS',MODE,OWNER
        IF(OWNER.EQ.0)THEN
            STATUS=0
            IF(BTEST(MODE,PRC$V_INTER))THEN
D             TYPE*,'INTERACIVE'
            ELSEIF(BTEST(MODE,PRC$V_DETACH))THEN
D             TYPE*,'DETACH'
              STATUS=2
            ENDIF
        ELSE
D            TYPE*,'SUBRUN'
             STATUS=1
        ENDIF
C
10000   CONTINUE
C
        END
