* RFULLCHK.FOR                         
C                                                                               
C V02 30-MAR-2015 MTK Modified Super 14 game
C V01 11-NOV-2003 FRP Modify for Batch2 Totobola Changes.
C
C CHECK IF MATCHES SUPER14 ROW WITHIN SPORTS GAME
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
C=======OPTIONS /CHECK=NOOVERFLOW/CHECK=NOUNDER
      SUBROUTINE RFULLCHK(SYSBET,WIN,NUMROW,BTYPE,HISHR,SHARES)
      IMPLICIT NONE                                                   
                                                                                
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'   
C                                                                               
      INTEGER*2 SYSBET(2,1,1)      ! TABLE OF SYSTEM BETS                       
      INTEGER*4 WIN(*)             ! TABLE OF WINS                              
      INTEGER*4 NUMROW             ! NUMBER OF TOTAL ROWS
      INTEGER*4 BTYPE              ! BONUS ROW TYPE
      INTEGER*4 HISHR              ! HIGHEST SHARE (TO BE UPDATED)              
      INTEGER*4 SHARES(*)          ! SHARES TABLE TO BE UPDATED                 
C
      LOGICAL WINNER

      INTEGER*4 BET1,BET2
      INTEGER*4 SCORE(2)

      WINNER = .FALSE.

C SUPER 14 RESULTS TYPE 
	IF(BTYPE.EQ.1) THEN
          BET1 = SYSBET(1,1,1)
          BET2 = SYSBET(2,1,1)
          SCORE(1)=ISHFT(WIN(NUMROW),-4)
          SCORE(2)=IAND(WIN(NUMROW),'0F'X)
          IF(IAND(BET1,SCORE(1)).NE.0 .AND. IAND(BET2,SCORE(2)).NE.0) WINNER = .TRUE.
          IF(SCORE(1) .EQ. 0 .AND. SCORE(2) .EQ. WINNUM_CANEVENT) WINNER = .TRUE.
	ENDIF

C SUPER 14 STANDARD 1X2 TYPE
        IF(BTYPE.EQ.2) THEN
          BET1 = SYSBET(1,1,1)
          SCORE(1)=WIN(NUMROW)
          IF(IAND(BET1,SCORE(1)).NE.0) WINNER = .TRUE.
          IF(SCORE(1) .EQ. WINNUM_CANEVENT) WINNER = .TRUE.
        ENDIF


      IF(WINNER) THEN
        HISHR = 1
        SHARES(1) = SHARES(1) + 1
      ENDIF

      RETURN                                                                    
      END                                                                       
