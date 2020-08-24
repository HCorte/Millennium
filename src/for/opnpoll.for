C
C V10 26-MAR-99 RXK Game type/game index change.
C  
C     Rev 1.0   17 Apr 1996 14:19:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.7   27 Nov 1993 19:36:44   SXH
C  ADDED RETRY CODE
C  
C     Rev 1.6   23 Nov 1993 14:41:18   SXH
C  PUT BACK LINE OF CODE SOMEHOW DELETED
C  
C     Rev 1.5   23 Nov 1993 14:11:28   SXH
C  BUG FIX FROM LAST MOD (FORGOT ABOUT MESSAGE BACK TO TERMINAL!)
C  
C     Rev 1.4   23 Nov 1993 13:35:06   SXH
C  LOG OPN POLL ID NOT NUMBER
C  
C     Rev 1.3   19 Nov 1993 14:58:50   GXA
C  Increment index correctly.
C  
C     Rev 1.2   15 Nov 1993 19:59:36   GXA
C  Conformed to new formats and methodes.
C
C OPNPOLL.FTN                                                                   
C                                                                               
c V02 14-MAY-93   HUGH     CONVERTED TO VAX; ALLOW 4 OPINION POLLS.
C V01 06-OCT-89   LOU R.   INITIAL RELEASE FINLAND.                             
C                                                                               
C SUBROUTINE TO PROCESS AGENTS OPINION POLL TRANSACTIONS.                       
C                                                                               
C CALLING SEQUENCE:                                                             
C      CALL OPNPOLL(TRABUF,MESTAB,OUTLEN)                                       
C INPUT                                                                         
C     TRABUF - INTERNAL TRANSACTION FORMAT                                      
C     MESTAB - TERMINAL INPUT MESSAGE                                           
C                                                                               
C OUTPUT                                                                        
C     MESTAB - TERMINAL OUTPUT MESSAGE                                          
C     OUTLEN - OUTPUT MESSAGE LENGTH                                            
C                                                                               
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
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE OPNPOLL(TRABUF,MESTAB,OUTLEN)
        IMPLICIT NONE                      
C
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'                  
        INCLUDE 'INCLIB:CONCOM.DEF'                  
        INCLUDE 'INCLIB:DESTRA.DEF'  
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
C                                                                               
C                                                                               
        BYTE      MESTAB(*)
C
        INTEGER*4 OFF
	INTEGER*4 IND
	INTEGER*4 CHECK				!Serial# check digits.
        INTEGER*4 CHKLEN
        INTEGER*4 MYCHKSUM
        INTEGER*4 I4TEMP
	INTEGER*4 REVNUM
        INTEGER*4 POLLID                        ! OPN POLL ID (NOT NUMBER 1->4)
        INTEGER*4 POLLNUM                       ! OPN POLL NUM (1->4)
        INTEGER*4 TER                           ! TERMINAL NUMBER
C
        INTEGER*2 OUTLEN 
        INTEGER*2 ERRTYP /Z90/
        BYTE      I1TEMP(4)

        EQUIVALENCE(I4TEMP,I1TEMP)
  
C                                                                               
C GET OPINION DATA                                             
C    
        TER = TRABUF(TTER)

        OFF = 5  
        POLLID  = 0
        POLLNUM = 0

        POLLNUM =  MESTAB(OFF)      !OPINION POLL # (NOT ID!)
	IF(POLLNUM.LT.1 .OR. POLLNUM.GT.PRM_NUMOPN) THEN
	   TRABUF(TERR) = SYNT
	ENDIF

        POLLID = SCC_OPNID(POLLNUM)
        TRABUF(TSOLD) = POLLID         ! LOG POLL ID   
	OFF = OFF + 1
C
C
C GET REVISION #.
C
	I4TEMP = 0
	I1TEMP(2) = MESTAB(OFF)
	OFF = OFF + 1
	I1TEMP(1) = MESTAB(OFF)
	OFF = OFF + 1
C
	CALL OPNPOL_CNTRL(POLLNUM,REVNUM)
	IF(REVNUM.NE.I4TEMP) THEN
           TYPE *, 'REVNUM = ',REVNUM,'I4TEMP = ',I4TEMP
	   TRABUF(TERR) = GREV
	   TRABUF(TSUBERR) = GREV_GAMCLT
	ENDIF	
C                                                                               
C CHECK FOR INVALID TRANS                                                       
C                                                                               
        IF(TRABUF(TERR).NE.NOER) THEN 
           TRABUF(TSTAT) = REJT
           GOTO 1000           
        ENDIF
C
C GET OPINION BITMAPS
C
        CALL MOVBYT(MESTAB,OFF,TRABUF(TSDT2),1,24)  !GET BITMAP OF MARKS
	OFF = OFF + 24

        ! CHECK FOR RETRY
        IF (TRABUF(TSTAT) .EQ. GOOD .AND.
     *      TRABUF(TTRN) .EQ. AGTHTB(ATRNUM,TER)) THEN 

            ! USE OLD JOKER 
            TRABUF(TSDT1) = AGTTAB(AGTLKN,TER)
            TRABUF(TERR)  = RETY
            TRABUF(TSTAT) = REJT
            GOTO 100
        END IF
C
C GET KICKER NUMBER
C
	CALL MOVBYT(MESTAB,OFF,TRABUF(TSDT1),1,3)	
C                                                                               
C BUILD OUTPUT MESSAGE BACK TO TERMINAL                                         
C                 
100     CONTINUE
                                                              
        IND = 5
C
	MESTAB(IND)   = TNBR          	! Type 
	MESTAB(IND+1) = POLLNUM	        ! Index
	IND = IND + 2
C
	I4TEMP = TRABUF(TTIM)
	CALL PUTIME(I4TEMP,MESTAB,IND)
C
	CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),I4TEMP,CHECK)
	MESTAB(IND+0) = I1TEMP(3)
	MESTAB(IND+1) = I1TEMP(2)
	MESTAB(IND+2) = I1TEMP(1)
	MESTAB(IND+3) = CHECK
	IND = IND + 4
C
	I4TEMP = TRABUF(TSDT1)				!Kicker #
	MESTAB(IND+0) = I1TEMP(4)
	MESTAB(IND+1) = I1TEMP(3)
	MESTAB(IND+2) = I1TEMP(2)
	MESTAB(IND+3) = I1TEMP(1)
	IND = IND + 4
C
        OUTLEN = IND - 1 
        GOTO 9000  
C                                                                               
C RETURN ERROR                                                                  
C
1000    CONTINUE
        TRABUF(TSTAT)=REJT
        MESTAB(2) = ERRTYP                                          
        MESTAB(5) = TRABUF(TERR)                                    
        MESTAB(6) = TRABUF(TSUBERR) 
        OUTLEN = 6 
C                                                                               
C CALCULATE CHECKSUM AND RETURN                                                 
C                                                                               
9000  CONTINUE                                                                  
      I4CCITT = TRABUF(TCHK)
      MESTAB(3) = I1CCITT(2)
      MESTAB(4) = I1CCITT(1)
      CHKLEN = OUTLEN - 1
      CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
      I4CCITT = MYCHKSUM
      MESTAB(3) = I1CCITT(2)
      MESTAB(4) = I1CCITT(1)
      RETURN                                                                    
      END                                                                       
