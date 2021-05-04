C
C SUBROUTINE CMDLTO
C $Log:   GXAFXT:[GOLS]CMDLTO.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:38:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   04 May 1993 12:01:30   SXH
C  Added command to change estimated Viking Lotto jackpot
C  
C     Rev 1.0   21 Jan 1993 15:55:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - cmdlto.for **
C
C CMDLTO.FOR
C
C V03 26-NOV-2010 MAC LUCKY NUMBER
C V02 07-MAR-91 JPJ INITIAL RELEASE FOR MARYLAND
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS LOTTO GAME COMMANDS
C
C
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
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CMDLTO(TRABUF,MESS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

        ! arguments
	INTEGER*4 MESS(EDLEN)        !

        ! variables
	INTEGER*4 TEMP               !
	INTEGER*4 BIND               !
	INTEGER*4 IND                !
	INTEGER*4 OFF                !
	INTEGER*4 GIND               !
        INTEGER*4 CMDNUM             !
	INTEGER*4 VALDOL             !
	
        INTEGER*2 I2TEMP(2)          !
	
        EQUIVALENCE (TEMP,I2TEMP)    !
C
C
C
	CMDNUM=TRABUF(TCMNUM)
	GOTO (10,20,30,40,50,60,70,80,90,100,110) CMDNUM
	GOTO 1000
C
C CHANGE LOTTO GAME STATUS
C
10	CONTINUE
	GIND=TRABUF(TCMDT1)
	TRABUF(TCMOLD)=LTOSTS(GIND)
	LTOSTS(GIND)=TRABUF(TCMNEW)
	IF(TRABUF(TCMNEW).EQ.GAMBFD) THEN !(GAMBFD=4)  !END OF GAME/BEFORE DRAWING
	    LTOCTM(GIND)=TRABUF(TTIM)
		CALL BSET(LTOTIM(GIND),1)
C            CALL OPSTXT('game control revision update')
C            CALL OPS('control revision before',LTOREV(GIND),LTOREV(GIND))
            TEMP=LTOREV(GIND)
            I2TEMP(1)=I2TEMP(1)+1
			LTOREV(GIND)=TEMP
C            CALL OPS('control revision now/updated',LTOREV(GIND),LTOREV(GIND))
	    CALL CLRSUM
	ENDIF

	MESS(2)=TECMD
	MESS(3)=3
	MESS(6)=GIND
	MESS(9)=TRABUF(TCMOLD)
	MESS(10)=TRABUF(TCMNEW)

	RETURN
C
C SET LOTTO WINNING NUMBERS
C
20	CONTINUE
	GIND=TRABUF(TCMDT1)
	OFF=0
	IND=1
	BIND=1
21	CONTINUE
	CALL ILBYTE(TEMP,TRABUF(TCMDT2),OFF)
	IF(IND.GT.LTONUM(GIND)) THEN
	  IF(LTOBFL(GIND).GE.BIND) THEN
	    LTOBNM(BIND,TRABUF(TCMNEW),GIND)=TEMP
	    BIND=BIND+1
            OFF=OFF+1
            GOTO 21
	  ENDIF
	  IF(LTOLFL(GIND).GT.0) THEN
	    LTOLNM(TRABUF(TCMNEW),GIND)=TEMP
	  ENDIF
 	  MESS(2)=TECMD
	  MESS(3)=4
	  MESS(6)=GIND
          RETURN
	ENDIF
	LTOWIN(IND,TRABUF(TCMNEW),GIND)=TEMP
	IND=IND+1
	OFF=OFF+1
	GOTO 21
C
C CHANGE LOTTO OFFLINE SALES
C
30	CONTINUE
	GIND=TRABUF(TCMDT1)
	TRABUF(TCMOLD)=LTOSAL(2,GIND)    !OFFLINE SALES TABLE
	LTOSAL(2,GIND)=TRABUF(TCMNEW)
	MESS(2)=TECMD
	MESS(3)=3
	MESS(6)=GIND
	MESS(9)=TRABUF(TCMOLD)
	MESS(10)=TRABUF(TCMNEW)

	RETURN
C
C UPDATE ROLLOVER FOR DIVISION ONE ONLY
C
40	CONTINUE
	GIND=TRABUF(TCMDT1)
	TRABUF(TCMOLD)=LTOPOL(1,GIND)    !ROLLOVER BY DIVISON
	LTOPOL(1,GIND)=TRABUF(TCMNEW)
	MESS(2)=TECMD
	MESS(3)=3
	MESS(6)=GIND
	MESS(9)=TRABUF(TCMOLD)
	MESS(10)=TRABUF(TCMNEW)
	RETURN
C
C SET LOTTO PRIZE VALUES
C
50	CONTINUE
	GIND=TRABUF(TCMDT1)
	TRABUF(TCMOLD)=LTOLSV(TRABUF(TCMDT2),TRABUF(TCMDT3),GIND)
	LTOLSV(TRABUF(TCMDT2),TRABUF(TCMDT3),GIND)=TRABUF(TCMNEW)
	MESS(2)=TECMD
	MESS(3)=3
	MESS(6)=GIND
	MESS(9)=TRABUF(TCMOLD)
	MESS(10)=TRABUF(TCMNEW)
	RETURN
C
C SET LOTTO TOTAL SHARE COUNT
C
60	CONTINUE
	GIND=TRABUF(TCMDT1)
	TRABUF(TCMOLD)=LTOTSR(TRABUF(TCMDT2),TRABUF(TCMDT3),GIND)
	LTOTSR(TRABUF(TCMDT2),TRABUF(TCMDT3),GIND)=TRABUF(TCMNEW)
	MESS(2)=TECMD
	MESS(3)=3
	MESS(6)=GIND
	MESS(9)=TRABUF(TCMOLD)
	MESS(10)=TRABUF(TCMNEW)
	RETURN
C
C CHANGE LOTTO OVERRIDE POOL AMOUNT
C
70      CONTINUE
        GIND=TRABUF(TCMDT1)
        TRABUF(TCMOLD)=LTOOPA(GIND)
        LTOOPA(GIND)=TRABUF(TCMNEW)
        MESS(2)=TECMD
        MESS(3)=3
        MESS(6)=GIND
        MESS(9)=VALDOL(TRABUF(TCMOLD))
        MESS(10)=VALDOL(TRABUF(TCMNEW))
	RETURN
C
C SET LAST DRAWS STATUS
C
80	CONTINUE
	GIND=TRABUF(TCMDT1)
	TRABUF(TCMOLD)=LTOLST(GIND)
	LTOLST(GIND)=TRABUF(TCMNEW)
	MESS(2)=TECMD
	MESS(3)=3
	MESS(6)=GIND
	MESS(9)=TRABUF(TCMOLD)
	MESS(10)=TRABUF(TCMNEW)
	RETURN
C
C SET LOTTO ANNUITY PRIZE VALUES
C
90	CONTINUE
	GIND=TRABUF(TCMDT1)
	TRABUF(TCMOLD)=LTOLAN(TRABUF(TCMDT2),TRABUF(TCMDT3),GIND)
	LTOLAN(TRABUF(TCMDT2),TRABUF(TCMDT3),GIND)=TRABUF(TCMNEW)
	MESS(2)=TECMD
	MESS(3)=3
	MESS(6)=GIND
	MESS(9)=TRABUF(TCMOLD)
	MESS(10)=TRABUF(TCMNEW)
	RETURN
C
C CHANGE VIKING LOTTO ESTIMATED JACKPOT                                         
C                                                                               
100     CONTINUE                                                                  
        GIND=TRABUF(TCMDT1)                                                       
        TRABUF(TCMOLD)=LTOEST(GIND)                                               
        LTOEST(GIND)=TRABUF(TCMNEW)                                               
        MESS(2)=TECMD                                                             
        MESS(3)=3                                                                 
        MESS(6)=GIND                                                              
        MESS(9)=TRABUF(TCMOLD)                                                    
        MESS(10)=TRABUF(TCMNEW)                                                   

        RETURN                                                                    
C
C ADD TO POOL (DIVISION 1) SPECIAL AMOUNT 
C
110	CONTINUE
	GIND = TRABUF(TCMDT1)
	TRABUF(TCMOLD) = LTOPOL(1,GIND)
	LTOPOL(1,GIND) = LTOPOL(1,GIND) + TRABUF(TCMNEW) 
	MESS(2)=TECMD
	MESS(3)=3
        MESS(6)=GIND                                                              
        MESS(9)=TRABUF(TCMOLD)                                                    
        MESS(10)=TRABUF(TCMNEW)                                                   

	RETURN
C
C PUT NEXT LOTTO COMMAND HERE
C
C INVALID COMMAND NUMBER
C
1000	CONTINUE
	TRABUF(TSTAT)=REJT
	TRABUF(TERR)=INVL
	MESS(2)=TECMD
	MESS(3)=1
	MESS(4)=TRABUF(TCMTYP)
	MESS(5)=TRABUF(TCMNUM)
	RETURN
	END
