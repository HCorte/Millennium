C
C MODOPS.FOR
C
C V01 08-APR-2011 FRP INITIAL RELEASE FOR PORTUGAL
C
C PROGRAM TOOL TO MODIFY DATA FIELDS FROM OPS RECORD.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
C Island, and contains confidential and trade secret information. It            
C may not be transferred from the custody or control of GTECH except            
C as authorized in writing by an officer of GTECH. Neither this item            
C nor the information it contains may be used, transferred,                     
C reproduced, published, or disclosed, in whole or in part, and                 
C directly or indirectly, except as expressly authorized by an                  
C officer of GTECH, pursuant to written agreement.                              
C                                                                               
C Copyright 1993 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
      PROGRAM MODOPS
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:OPS_REC.DEF'
C
      INTEGER*4 ST,OPT
      CHARACTER KEY1*15,KEY2*14,YN*1
      INTEGER*4 NEW_PROC_PAID_CDC,NEW_PAYABLE_CDC,NEW_PAID_CDC
      INTEGER*4 NEW_WINS(6),NEW_JOKER_DIV,NEW_TOTAL_GAME,NEW_TOTAL_JOKER
      INTEGER*4 NEW_GENERATED,NEW_PAID_SENT_SAP,NEW_HI_PRIZE,NEW_PAID_MANUALLY
C
      CALL CLRSCR(6)
      TYPE*,IAM()
      CALL COPYRITE
      TYPE*,IAM()
      TYPE*,IAM(),'<<< Program to modify data fields from OPS record file >>>'
      TYPE*,IAM()
C
C Open OPS file
      CALL OPEN_OPS('KEYED',ST)
      IF(ST.NE.0) THEN
        TYPE*,IAM(),'OPS Open Error ',ST
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
C Ask OPS key and read OPS record
111   CONTINUE
      TYPE*,IAM()
      TYPE*,IAM(),'1 - Game and Draw and Order key'
      TYPE*,IAM(),'2 - External Serial Number key'
      TYPE*,IAM()
      CALL INPNUM('Enter Option: ',OPT,1,2,ST)
      IF(ST.LT.0) THEN
        CLOSE(OPS_LUN)
        CALL GSTOP(GEXIT_SUCCESS)
      ENDIF
C
      IF(OPT.EQ.2) GOTO 222
C
      TYPE*,IAM()
      TYPE*,IAM(),'Enter GGAAAACCCOOOOOO (15 digits) where'
      TYPE*,IAM(),'  GG      = Game Number (zero-padded)'
      TYPE*,IAM(),'  AAAACCC = Year and Draw Week (CCC zero-padded)'
      TYPE*,IAM(),'  OOOOOO  = Payment Order Number (zero-padded)'
      TYPE*,IAM()
      READ(5,100) KEY1
100   FORMAT(A15)
      READ(OPS_LUN,KEYID=0,KEY=KEY1,IOSTAT=ST) OPS_REC
      GOTO 333
C
222   CONTINUE
      TYPE*,IAM()
      TYPE*,IAM(),'Enter JJJSSSSSSSSCHK key (14 digits) where'
      TYPE*,IAM(),'  JJJ      = Julian Day (zero-padded)'
      TYPE*,IAM(),'  SSSSSSSS = Serial Number (zero-padded)'
      TYPE*,IAM(),'  CHK = Check Digits (zero-padded)'
      TYPE*,IAM()
      READ(5,200) KEY2
200   FORMAT(A14)
      READ(OPS_LUN,KEYID=1,KEY=KEY2,IOSTAT=ST) OPS_REC
C
333   CONTINUE
      IF(ST.NE.0) THEN
        TYPE*,IAM(),'OPS Read Error ',ST
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
C Display OPS record
      TYPE*,IAM()
      TYPE*,IAM(),'GAME:           ',OPS_REC.GAME
      TYPE*,IAM(),'YEARWEEK:       ',OPS_REC.YEARWEEK
      TYPE*,IAM(),'ORDER:          ',OPS_REC.ORDER
      TYPE*,IAM(),'BILHETE:        ',OPS_REC.BILHETE
      TYPE*,IAM(),'CWEEK:          ',OPS_REC.CWEEK
      TYPE*,IAM(),'SPLITTED:       ',YN(OPS_REC.SPLITTED)
      TYPE*,IAM(),'AGENT:          ',OPS_REC.AGENT
      TYPE*,IAM(),'PRINTED_BY_OFF: ',YN(OPS_REC.PRINTED_BY_OFF)
      TYPE*,IAM(),'CLAIM:          ',YN(OPS_REC.CLAIM)
      TYPE*,IAM(),'PROC_PAID_CDC:  ',OPS_REC.PROC_PAID_CDC
      TYPE*,IAM(),'GENERATED:      ',YN(OPS_REC.GENERATED)
      TYPE*,IAM(),'PAYABLE_CDC:    ',OPS_REC.PAYABLE_CDC
      TYPE*,IAM(),'PAID_CDC:       ',OPS_REC.PAID_CDC
      TYPE*,IAM(),'PAID_SENT_SAP:  ',YN(OPS_REC.PAID_SENT_SAP)
      TYPE*,IAM(),'ONLINE_ORDER:   ',YN(OPS_REC.ONLINE_ORDER)
      TYPE*,IAM(),'HI_PRIZE:       ',YN(OPS_REC.HI_PRIZE)
      TYPE*,IAM(),'WINS(1):        ',OPS_REC.WINS(1)
      TYPE*,IAM(),'WINS(2):        ',OPS_REC.WINS(2)
      TYPE*,IAM(),'WINS(3):        ',OPS_REC.WINS(3)
      TYPE*,IAM(),'WINS(4):        ',OPS_REC.WINS(4)
      TYPE*,IAM(),'WINS(5):        ',OPS_REC.WINS(5)
      TYPE*,IAM(),'WINS(6):        ',OPS_REC.WINS(6)
      TYPE*,IAM(),'JOKER_DIV:      ',OPS_REC.JOKER_DIV
      TYPE*,IAM(),'TOTAL_GAME:     ',CMONY(OPS_REC.TOTAL_GAME,12,VALUNIT)
      TYPE*,IAM(),'TOTAL_JOKER:    ',CMONY(OPS_REC.TOTAL_JOKER,12,VALUNIT)
      TYPE*,IAM(),'BANK:           ',OPS_REC.BANK
      TYPE*,IAM(),'BRANCH:         ',OPS_REC.BRANCH
      TYPE*,IAM(),'PAID_MANUALLY:  ',YN(OPS_REC.PAID_MANUALLY)
C
C Ask OPS record changes
      TYPE*,IAM()
      CALL PRMYESNO('Modify OPS Record [Y/N] ?',ST)
      IF(ST.NE.1) GOTO 111
C
      TYPE*,IAM()
      NEW_PROC_PAID_CDC = -1
      CALL INPNUM('Enter PROC_PAID_CDC [E - No change]:',NEW_PROC_PAID_CDC,0,9999,ST)
      NEW_GENERATED = -1
      CALL INPNUM('Enter GENERATED [0 - No; 1 - Yes; E - No change]:',NEW_GENERATED,0,1,ST)
      NEW_PAYABLE_CDC = -1
      CALL INPNUM('Enter PAYABLE_CDC [E - No change]:',NEW_PAYABLE_CDC,0,9999,ST)
      NEW_PAID_CDC = -1
      CALL INPNUM('Enter PAID_CDC [E - No change]:',NEW_PAID_CDC,0,9999,ST)
      NEW_PAID_SENT_SAP = -1
      CALL INPNUM('Enter PAID_SENT_SAP [0 - No; 1 - Yes; E - No change]:',NEW_PAID_SENT_SAP,0,1,ST)
      NEW_HI_PRIZE = -1
      CALL INPNUM('Enter HI_PRIZE [0 - No; 1 - Yes; E - No change]:',NEW_HI_PRIZE,0,1,ST)
      NEW_WINS(1) = -1
      CALL INPNUM('Enter WINS(1) [E - No change]:',NEW_WINS(1),0,999,ST)
      NEW_WINS(2) = -1
      CALL INPNUM('Enter WINS(2) [E - No change]:',NEW_WINS(2),0,999,ST)
      NEW_WINS(3) = -1
      CALL INPNUM('Enter WINS(3) [E - No change]:',NEW_WINS(3),0,999,ST)
      NEW_WINS(4) = -1
      CALL INPNUM('Enter WINS(4) [E - No change]:',NEW_WINS(4),0,999,ST)
      NEW_WINS(5) = -1
      CALL INPNUM('Enter WINS(5) [E - No change]:',NEW_WINS(5),0,999,ST)
      NEW_WINS(6) = -1
      CALL INPNUM('Enter WINS(6) [E - No change]:',NEW_WINS(6),0,999,ST)
      NEW_JOKER_DIV = -1
      CALL INPNUM('Enter JOKER_DIV [E - No change]:',NEW_JOKER_DIV,1,6,ST)
      NEW_TOTAL_GAME = -1
      CALL PRMMONY('Enter TOTAL_GAME [E - No change]:',NEW_TOTAL_GAME,VALUNIT,ST)
      NEW_TOTAL_JOKER = -1
      CALL PRMMONY('Enter TOTAL_JOKER [E - No change]:',NEW_TOTAL_JOKER,VALUNIT,ST)
      NEW_PAID_MANUALLY = -1
      CALL INPNUM('Enter PAID_MANUALLY [0 - No; 1 - Yes; E - No change]:',NEW_PAID_MANUALLY,0,1,ST)
C
C Display OPS modified record
      TYPE*,IAM()
      TYPE*,IAM(),'GAME:           ',OPS_REC.GAME
      TYPE*,IAM(),'YEARWEEK:       ',OPS_REC.YEARWEEK
      TYPE*,IAM(),'ORDER:          ',OPS_REC.ORDER
      TYPE*,IAM(),'BILHETE:        ',OPS_REC.BILHETE
      TYPE*,IAM(),'CWEEK:          ',OPS_REC.CWEEK
      TYPE*,IAM(),'SPLITTED:       ',YN(OPS_REC.SPLITTED)
      TYPE*,IAM(),'AGENT:          ',OPS_REC.AGENT
      TYPE*,IAM(),'PRINTED_BY_OFF: ',YN(OPS_REC.PRINTED_BY_OFF)
      TYPE*,IAM(),'CLAIM:          ',YN(OPS_REC.CLAIM)
      IF(NEW_PROC_PAID_CDC .NE. -1) OPS_REC.PROC_PAID_CDC = NEW_PROC_PAID_CDC
      TYPE*,IAM(),'PROC_PAID_CDC:  ',OPS_REC.PROC_PAID_CDC
      IF(NEW_GENERATED .NE. -1) THEN
        OPS_REC.GENERATED = .FALSE.
        IF(NEW_GENERATED .EQ. 1) OPS_REC.GENERATED = .TRUE.
      ENDIF
      TYPE*,IAM(),'GENERATED:      ',YN(OPS_REC.GENERATED)
      IF(NEW_PAYABLE_CDC .NE. -1) OPS_REC.PAYABLE_CDC = NEW_PAYABLE_CDC
      TYPE*,IAM(),'PAYABLE_CDC:    ',OPS_REC.PAYABLE_CDC
      IF(NEW_PAID_CDC .NE. -1) OPS_REC.PAID_CDC = NEW_PAID_CDC
      TYPE*,IAM(),'PAID_CDC:       ',OPS_REC.PAID_CDC
      IF(NEW_PAID_SENT_SAP .NE. -1) THEN
        OPS_REC.PAID_SENT_SAP = .FALSE.
        IF(NEW_PAID_SENT_SAP .EQ. 1) OPS_REC.PAID_SENT_SAP = .TRUE.
      ENDIF
      TYPE*,IAM(),'PAID_SENT_SAP:  ',YN(OPS_REC.PAID_SENT_SAP)
      TYPE*,IAM(),'ONLINE_ORDER:   ',YN(OPS_REC.ONLINE_ORDER)
      IF(NEW_HI_PRIZE .NE. -1) THEN
        OPS_REC.HI_PRIZE = .FALSE.
        IF(NEW_HI_PRIZE .EQ. 1) OPS_REC.HI_PRIZE = .TRUE.
      ENDIF
      TYPE*,IAM(),'HI_PRIZE:       ',YN(OPS_REC.HI_PRIZE)
      IF(NEW_WINS(1) .NE. -1) OPS_REC.WINS(1) = NEW_WINS(1)
      TYPE*,IAM(),'WINS(1):        ',OPS_REC.WINS(1)
      IF(NEW_WINS(2) .NE. -1) OPS_REC.WINS(2) = NEW_WINS(2)
      TYPE*,IAM(),'WINS(2):        ',OPS_REC.WINS(2)
      IF(NEW_WINS(3) .NE. -1) OPS_REC.WINS(3) = NEW_WINS(3)
      TYPE*,IAM(),'WINS(3):        ',OPS_REC.WINS(3)
      IF(NEW_WINS(4) .NE. -1) OPS_REC.WINS(4) = NEW_WINS(4)
      TYPE*,IAM(),'WINS(4):        ',OPS_REC.WINS(4)
      IF(NEW_WINS(5) .NE. -1) OPS_REC.WINS(5) = NEW_WINS(5)
      TYPE*,IAM(),'WINS(5):        ',OPS_REC.WINS(5)
      IF(NEW_WINS(6) .NE. -1) OPS_REC.WINS(6) = NEW_WINS(6)
      TYPE*,IAM(),'WINS(6):        ',OPS_REC.WINS(6)
      IF(NEW_JOKER_DIV .NE. -1) OPS_REC.JOKER_DIV = NEW_JOKER_DIV
      TYPE*,IAM(),'JOKER_DIV:      ',OPS_REC.JOKER_DIV
      IF(NEW_TOTAL_GAME .NE. -1) OPS_REC.TOTAL_GAME = NEW_TOTAL_GAME
      TYPE*,IAM(),'TOTAL_GAME:     ',CMONY(OPS_REC.TOTAL_GAME,12,VALUNIT)
      IF(NEW_TOTAL_JOKER .NE. -1) OPS_REC.TOTAL_JOKER = NEW_TOTAL_JOKER
      TYPE*,IAM(),'TOTAL_JOKER:    ',CMONY(OPS_REC.TOTAL_JOKER,12,VALUNIT)
      TYPE*,IAM(),'BANK:           ',OPS_REC.BANK
      TYPE*,IAM(),'BRANCH:         ',OPS_REC.BRANCH
      IF(NEW_PAID_MANUALLY .NE. -1) THEN
        OPS_REC.PAID_MANUALLY = .FALSE.
        IF(NEW_PAID_MANUALLY .EQ. 1) OPS_REC.PAID_MANUALLY = .TRUE.
      ENDIF
      TYPE*,IAM(),'PAID_MANUALLY:  ',YN(OPS_REC.PAID_MANUALLY)
C
C Modify OPS record
      TYPE*,IAM()
      CALL PRMYESNO('Are You Sure to Modify OPS Record [Y/N] ?',ST)
      IF(ST.NE.1) GOTO 111
C
      REWRITE(OPS_LUN,IOSTAT=ST) OPS_REC
      IF(ST.NE.0) THEN
        TYPE*,IAM(),'OPS Write Error ',ST
        CALL GSTOP (GEXIT_FATAL)
      ENDIF
C
      TYPE*,IAM()
      TYPE*,IAM(),'OPS Record Modified'
      GOTO 111
C
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
      CHARACTER*1 FUNCTION YN(LOGVAR)
      IMPLICIT NONE
C
      LOGICAL*4 LOGVAR
C
      IF(LOGVAR) THEN
        YN='Y'
      ELSE
        YN='N'
      ENDIF
C
      RETURN
      END

