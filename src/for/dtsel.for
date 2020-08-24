C
C SUBROUTINE DTSEL
C
C V21 02-FEB-2000 UXN TNFRAC added.
C V20 28-DEC-1999 OXK Fixed game revision error setting.
C V19 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V18 07-NOV-1995 HXK Tightened up Dcode routines, removed kicker
C V17 28-FEB-1994 JXP Return if CHKLIA rejects wager
C V16 07-NOV-1993 GXA Correction to earlier correction, Move checksum check to
C                     after the Kicker #'s have bean decoded ans message length
C                     is final.
C V15 06-NOV-1993 GXA Moved check of checksum earlier in code and only check
C                     kicker optional things if kicker is requested.	  
C V14 14-OCT-1993 HXK Change of error codes
C V13 17-AUG-1993 HXK Fix for mktmes rev bugs
C V12 11-AUG-1993 GXA Do not override TWAMT for Full System bets.
C V11 27-JUL-1993 SXH Corrected bank numbers
C V10 19-JUL-1993 GXA Corrected retreval of Kicker#'s added by GETQUE.
C V09 19-JUL-1993 GXA Changed check for revisions to use secondary error filed
C                     for bitmap (TSUBERR) and added check to see if we have
C                     ticket text before we check ticket text rev
C V08 15-JUL-1993 GXA Removed duplicate SYNTAX ERROR code of 115.
C V07 28-JUN-1993 GXA Changed Control/Text error checking to accumulate the
C                     error bits.
C V06 22-JUN-1993 GXA Released for Finland Dec Conversion / Oddset.
C V05 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C                     and Comm 1/93 update. DEC Baseline
C V04 25-AUG-1992 GCAN REMOVED SYNTAX ERROR FOR LIABILITY LIMITS.
C V03 08-JUL-1992 GCAN RE-RELEASED FOR THE NETHERLANDS.
C		       (CORRECTED CONTROL AND TEXT REVISIONS, AND 
C	 	       ADDED KICKER GAME)
C V02 01-NOV-1991 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C SUBROUTINE TO DECODE TOTO SELECT WAGERS
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DTSEL(TERMES,TRABUF,MESLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
	BYTE	TERMES(*)	    ! Terminal Message.
C
	INTEGER*2 MESLEN	    ! Message length for checksumming.
	INTEGER*2 I2TEMP(2)	    ! Temp Variable.
	INTEGER*2 I2CREV(2)         ! I2 Game Control Revision #.
	INTEGER*2 I2TREV(2)         ! I2 Game Text Revision #.
	INTEGER*2 I2TIKTREV(2)      ! I2 Game Ticket Text Revision #.
        INTEGER*2 I2JOKCREV(2)      ! I2 Joker Control revision
        INTEGER*2 I2JOKTREV(2)      ! I2 Joker Text revision
        INTEGER*2 I2JOKTIKTREV(2)   ! I2 Joker Ticket text revision
C
	INTEGER*4 CREV		    ! Game Control Revision #.
	INTEGER*4 TREV		    ! Game Text Revision #.
	INTEGER*4 TIKTREV	    ! Game Ticket Text Revision #.
        INTEGER*4 JOKCREV           ! Joker Control revision
        INTEGER*4 JOKTREV           ! Joker Text revision
        INTEGER*4 JOKTIKTREV        ! Joker Ticket text revision
	INTEGER*4 I	            ! Loop Variable.
	INTEGER*4 TEMP		    ! Temp Variable.
	INTEGER*4 TEMP1		    ! Temp Variable.
	INTEGER*4 TEMP2             ! Temp Variable.
	INTEGER*4 TEMP3             ! Temp Variable.
	INTEGER*4 TEMP4		    ! Temp Variable.
	INTEGER*4 OPTIONS           ! options byte
	INTEGER*4 IND               ! offset counter in terminal message
	INTEGER*4 CHKLEN            ! check sum length
	INTEGER*4 MYCHKSUM          ! calculated checksum
	INTEGER*4 ST                ! return status
	INTEGER*4 OFF	            ! Temp. Bet offset.
	INTEGER*4 BETOFF(3)         ! Bet Offsets.
	INTEGER*4 KGIND		    ! Kicker Game Index.
	INTEGER*4 BNKLO
	INTEGER*4 BNKHI
C
        LOGICAL   JOK1_PART         ! joker 1 participation 
        LOGICAL   JOK2_PART         ! joker 2 participation 
        LOGICAL   JOK1_REQ          ! joker 1 number requested
        LOGICAL   JOK2_REQ          ! joker 2 number requested
	LOGICAL   ADVANCE
C
	EQUIVALENCE (TEMP,I2TEMP)
	EQUIVALENCE (CREV,I2CREV)
	EQUIVALENCE (TREV,I2TREV)
	EQUIVALENCE (TIKTREV,I2TIKTREV)           
        EQUIVALENCE (JOKCREV,I2JOKCREV)           
        EQUIVALENCE (JOKTREV,I2JOKTREV)           
        EQUIVALENCE (JOKTIKTREV,I2JOKTIKTREV)         

C
        DATA BETOFF/TWTAMT1,TWTAMT2,TWTAMT3/
C
C GET GENERIC GAME INFORMATION
C
        JOK1_PART       = .FALSE.
        JOK2_PART       = .FALSE.
        JOK1_REQ        = .FALSE.
        JOK2_REQ        = .FALSE.
	ADVANCE		= .FALSE.
	SYNTERRCOD	= 0
	TRABUF(TSTAT)	= REJT
	TRABUF(TGAMTYP)	= TTSL
C
C GET TRANSACTION NUMBER
C
	TEMP = ZEXT(TERMES(1))
	TRABUF(TTRN)=IAND(TEMP,15)
C
C GET CHECKSUM
C
        TEMP1 = ZEXT(TERMES(3))
        TEMP2 = ZEXT(TERMES(4))
        TRABUF(TCHK) = ISHFT(TEMP1,8) + TEMP2
C
C GET STATISTICS
C
        TRABUF(TTSTCS) = ZEXT(TERMES(5))
C
C GET GAME INDEX AND SYSTEM TYPE
C
        TEMP = ZEXT(TERMES(7))
	TRABUF(TWSYST)=IAND(TEMP,15)
	TRABUF(TGAMIND)=ISHFT(TEMP,-4)
	IF(TRABUF(TGAMIND).GE.1.AND.TRABUF(TGAMIND).LE.MAXIND)
     *	  TRABUF(TGAM)=GTNTAB(TTSL,TRABUF(TGAMIND))
	IF(TRABUF(TGAM).LT.1.OR.TRABUF(TGAM).GT.MAXGAM) THEN
	  TRABUF(TERR)=SYNT
	  SYNTERRCOD=5
	  RETURN
	ENDIF
C
C GET DURATION AND NUMBER OF BOARDS
C
        TEMP    = ZEXT(TERMES(8))
	TRABUF(TWNBET)=IAND(TEMP,15)
	TRABUF(TWDUR)=ISHFT(TEMP,-4)
C
C SET MAXIMUM FRACTIONS
C
	TRABUF(TFRAC)  = MAXFRC(TRABUF(TGAM))
	TRABUF(TNFRAC) = 1
C
C SET OPTIONS FIELD
C
	TEMP1 = ZEXT(TERMES(9))
	TEMP2 = ZEXT(TERMES(10))
        OPTIONS = ISHFT(TEMP1,8) + TEMP2
        IND = 11
C
C CHECK FOR CONTROL REVISION
C
        IF(IAND(OPTIONS,'8000'X).NE.0) THEN
           CREV = 0
           CALL MOVBYT(TERMES,IND,CREV,3,2)
	   IND = IND + 2
           TEMP = TSLREV(TRABUF(TGAMIND))
 	   IF(I2CREV(2).NE.I2TEMP(1)) THEN
	      TRABUF(TERR) = GREV
              TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_GAMCLT)
	   ENDIF
        ENDIF
C
C CHECK FOR TEXT REVISION
C
        IF(IAND(OPTIONS,'4000'X).NE.0) THEN
	   TREV = 0
           CALL MOVBYT(TERMES,IND,TREV,3,2)
           IND = IND + 2
           TEMP = TSLREV(TRABUF(TGAMIND))
           IF(I2TREV(2).NE.I2TEMP(2)) THEN
	      TRABUF(TERR) = GREV
              TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_GAMTXT)
	   ENDIF
        ENDIF
C
C CHECK FOR TICKET TEXT REVISION 
C
        IF(IAND(OPTIONS,'2000'X).NE.0) THEN
	   TIKTREV = 0
           CALL MOVBYT(TERMES,IND,TIKTREV,3,2)
           IND = IND + 2
           TEMP = TKTMRV(TRABUF(TGAM))
	   IF(I2TIKTREV(2).NE.I2TEMP(1).AND.
     *	      TKTMLN(TRABUF(TGAM)).GT.0)		THEN
	      TRABUF(TERR) = GREV 
              TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_GAMTKT)
	   ENDIF
        ENDIF
C
C CHECK FOR JOKER 1 PARTICIPATION 
C
	IF(IAND(OPTIONS,'0080'X).NE.0) THEN
           JOK1_PART = .TRUE.
           TRABUF(TWKFLG) = 1
	   TRABUF(TERR)=SYNT
	   SYNTERRCOD=821
	   RETURN
	ENDIF
C
C CHECK FOR JOKER 2 PARTICIPATION 
C
	IF(IAND(OPTIONS,'0040'X).NE.0) THEN
           JOK2_PART = .TRUE.
           TRABUF(TWKFLG2) = 1
	   TRABUF(TERR)=SYNT
	   SYNTERRCOD=822
	   RETURN
	ENDIF
C
C CHECK FOR JOKER 1 REQUEST, 
C
	IF(IAND(OPTIONS,'0020'X).NE.0) THEN
	   JOK1_REQ =.TRUE.
	   TRABUF(TERR)=SYNT
	   SYNTERRCOD=823
	   RETURN
        ENDIF
C
C CHECK FOR JOKER 2 REQUEST
C
	IF(IAND(OPTIONS,'0010'X).NE.0) THEN
	   JOK2_REQ =.TRUE.
	   TRABUF(TERR)=SYNT
	   SYNTERRCOD=824
	   RETURN
        ENDIF
C
C CHECK FOR JOKER 1 PROBLEM - PARTICIPATION BUT NO REQUEST
C
        IF(JOK1_PART.AND..NOT.JOK1_REQ) THEN
           TRABUF(TERR) = SYNT
           SYNTERRCOD = 115
           RETURN
        ENDIF
C
C CHECK FOR JOKER 2 PROBLEM - PARTICIPATION BUT NO REQUEST
C
        IF(JOK2_PART.AND..NOT.JOK2_REQ) THEN
           TRABUF(TERR) = SYNT
           SYNTERRCOD = 120
           RETURN
        ENDIF
C
C CHECK FOR QP FLAGS
C
	IF(IAND(OPTIONS,'0200'X).NE.0) THEN
	   TEMP1 = ZEXT(TERMES(IND+0))
	   TEMP2 = ZEXT(TERMES(IND+1))
	   TRABUF(TWQPF) = ISHFT(TEMP1,8) + TEMP2
	   IND = IND + 2
	ENDIF
C
C CHECK FOR SYSTEM BET FLAG IF TRUE GET SYSTEM NUMBER
C
        IF(IAND(OPTIONS,'0100'X).NE.0) THEN
           IF(TRABUF(TWSYST).EQ.NOSYS) THEN
              TRABUF(TERR) = SYNT
              SYNTERRCOD = 20
              RETURN
           ENDIF
	   TEMP1 = ZEXT(TERMES(IND+0))
	   TEMP2 = ZEXT(TERMES(IND+1))
           TRABUF(TWSYSN) = ISHFT(TEMP1,8) + TEMP2
           IND = IND + 2
        ENDIF
C
C CHECK MESSAGE CHECKSUM
C
        IF(P(SUPSUM).EQ.0) THEN
	  IF(.NOT.BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTSUM)) THEN
            I4CCITT   = IAND(BASECHKSUM+TRABUF(TTER),'FFFF'X)
            TERMES(3) = I1CCITT(2)
            TERMES(4) = I1CCITT(1)
            CHKLEN=MESLEN-1
            CALL GETCCITT(TERMES,1,CHKLEN,MYCHKSUM)
            IF(MYCHKSUM.NE.TRABUF(TCHK)) THEN
              TRABUF(TERR) = CBAD
              SYNTERRCOD = 10
              RETURN
            ENDIF
	  ENDIF
        ENDIF
C                                                                               
C GET BANK NUMBER AND ACCOUNT NUMBER                                            
C                                                                               
        IF(IAND(OPTIONS,'0008'X).NE.0) THEN                                  
            TEMP1 = ZEXT(TERMES(IND))
            TEMP2 = ZEXT(TERMES(IND+1))
            TEMP3 = ZEXT(TERMES(IND+2))
            TEMP4 = ZEXT(TERMES(IND+3))
            TRABUF(TWBNKID) =  ISHFT(TEMP1,24) + ISHFT(TEMP2,16) +
     *                         ISHFT(TEMP3,8) + TEMP4

            IND=IND+4                                                        
            TEMP1 = ZEXT(TERMES(IND))
            TEMP2 = ZEXT(TERMES(IND+1))
            TEMP3 = ZEXT(TERMES(IND+2))
            TEMP4 = ZEXT(TERMES(IND+3))
            TRABUF(TWBNKNM) =  ISHFT(TEMP1,24) + ISHFT(TEMP2,16) +
     *                         ISHFT(TEMP3,8) + TEMP4
C
           IND = IND + 4                                                        
C
           BNKLO = 0
           BNKHI = 999999
           IF(BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTTBA)) THEN
             BNKLO = 10000000
             BNKHI = 16000000
           ENDIF
           IF(TRABUF(TWBNKID).LT.BNKLO.OR.TRABUF(TWBNKID).GT.BNKHI) THEN
             TRABUF(TERR) = INVL
             SYNTERRCOD = 160
             RETURN
           ENDIF
           IF(TRABUF(TWBNKNM).LT.0.OR.TRABUF(TWBNKNM).GT.99999999)THEN
             TRABUF(TERR) = INVL
             SYNTERRCOD = 162
             RETURN
           ENDIF
        ENDIF                                                                
C
C CHECK GAME NUMBER
C
	IF(TRABUF(TGAM).LT.1.OR.TRABUF(TGAM).GT.MAXGAM) THEN
	   TRABUF(TERR) = SYNT
	   SYNTERRCOD = 30
	   RETURN
	ENDIF
C
C CHECK SYSTEM DRAW NUMBER
C
	IF(TSLDRW(TRABUF(TGAMIND)).LT.1) THEN
	   TRABUF(TERR) = GREV
           TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_GAMCLT)
	   SYNTERRCOD = 40
	   RETURN
	ENDIF
C
C CHECK DURATION
C
	IF(TRABUF(TWDUR).NE.1) THEN
	   TRABUF(TERR) = SYNT
	   SYNTERRCOD = 100
	   RETURN
	ENDIF
C
C
	IF(TRABUF(TWNBET).LT.1.OR.TRABUF(TWNBET).GT.3) THEN
	   TRABUF(TERR) = SYNT
	   SYNTERRCOD = 70
	   RETURN
	ENDIF
C
C
	IF(TRABUF(TWNBET).GT.1.AND.TRABUF(TSIZE).LE.1) THEN
	   TRABUF(TERR) = SYNT
	   SYNTERRCOD = 80
	   RETURN
	ENDIF
C
C
	IF(TRABUF(TWSYST).GT.FULSYS) THEN
	   TRABUF(TERR) = SYNT
	   SYNTERRCOD = 110
	   RETURN
	ENDIF
C
C
	IF(TRABUF(TWSYST).EQ.FULSYS) THEN
	   IF(TRABUF(TWNBET).GT.1.OR.
     *	      TRABUF(TWSYSN).LT.3.OR.TRABUF(TWSYSN).GT.4) THEN
	      TRABUF(TERR) = SYNT
	      SYNTERRCOD = 170
	      RETURN
	   ENDIF
	ENDIF
C
C GET AND CHECK TOTO SELECT BET DATA
C
	DO I=1,TRABUF(TWNBET)
	   OFF = BETOFF(I)
	   CALL GETTSL(TERMES,TRABUF(TGAMIND),IND,TRABUF(OFF))
	   CALL CHKTSL(TRABUF(OFF),TRABUF(TGAMIND),TRABUF(TERR))
	   IF(TRABUF(TERR).NE.NOER) RETURN
 	   CALL CHKLIA(TRABUF,I-1)   
	   IF(TRABUF(TERR).NE.NOER) RETURN
	END DO
C
C CHECK TOTO SELECT SYSTEM BETS AND GET AMOUNT BET BASED ON THE SYSTEM (TWAMT)
C
	IF(TRABUF(TWSYST).EQ.FULSYS) THEN
	   CALL DTSELCHKSYS(TRABUF)
	   IF(TRABUF(TERR).NE.NOER) RETURN
	ENDIF
C
C
	IF(TSLSTS(TRABUF(TGAMIND)).NE.GAMOPN) THEN
	   TRABUF(TERR) = SDRW
	   RETURN
	ENDIF
C
C CHECK FOR ERROR SINCE WE COULD LET REVISION ERROR TROUGH IN ORDER TO
C ACCUMULATE THEM AND CATCH SYNTAX ONES.
C
	IF(TRABUF(TERR).NE.NOER) RETURN
C
C TWAMT FOR FULSYSTEMS IS SET IN DTSELCHKSYS, NO NEED TO OVERRIDE.
C
	IF(TRABUF(TWSYST).NE.FULSYS) THEN
	   TRABUF(TWAMT) = TRABUF(TWTAMT1) + TRABUF(TWTAMT2)+
     *                     TRABUF(TWTAMT3)
        ENDIF
C
	TRABUF(TSTAT) = GOOD
	TRABUF(TWMFRAC) = TRABUF(TFRAC)
	TRABUF(TWBEG) = TSLDRW(TRABUF(TGAMIND))
	TRABUF(TWEND) = TRABUF(TWBEG) + TRABUF(TWDUR) - 1
	TRABUF(TFIL)  = EARLY
	TRABUF(TWTKC) = TKTCHG(TRABUF(TGAM))
	TRABUF(TWTOT) = TRABUF(TWAMT) * TRABUF(TWDUR) + 
     *		        TRABUF(TWKAMT) * TRABUF(TWKDUR) + 
     *			TRABUF(TWTKC)
C
	RETURN
C
	END
