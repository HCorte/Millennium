C SUBROUTINE DSTRIPLE
C
C V04 02-FEB-2000 UXN TNFRAC added.
C V03 28-DEC-1999 OXK Fixed game revision error setting.
C V02 01-JUN-1999 UXN Bet amount is now in 4 bytes.
C V01 13-MAY-1999 UXN Initial release
C
C SUBROUTINE TO DECODE Super Triple WAGERS
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

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	SUBROUTINE DSTRIPLE(TERMES,TRABUF,MESLEN)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:STRCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'

C---- Local Variables.

	BYTE	TERMES(*)	    ! Terminal Message.

	INTEGER*2 MESLEN	    ! Message length for checksumming.
	INTEGER*2 I2TEMP(2)	    ! Temp Variable.
	INTEGER*2 I2CREV(2)         ! I2 Game Control Revision #.
	INTEGER*2 I2TREV(2)         ! I2 Game Text Revision #.
	INTEGER*2 I2TIKTREV(2)      ! I2 Game Ticket Text Revision #.
        INTEGER*2 I2JOKCREV(2)      ! I2 Joker Control revision
        INTEGER*2 I2JOKTREV(2)      ! I2 Joker Text revision
        INTEGER*2 I2JOKTIKTREV(2)   ! I2 Joker Ticket text revision

	INTEGER*4 CREV		    ! Game Control Revision #.
	INTEGER*4 TREV		    ! Game Text Revision #.
	INTEGER*4 TIKTREV	    ! Game Ticket Text Revision #.
        INTEGER*4 JOKCREV           ! Joker Control revision
        INTEGER*4 JOKTREV           ! Joker Text revision
        INTEGER*4 JOKTIKTREV        ! Joker Ticket text revision
	INTEGER*4 I,J,K,CMB,OFF2
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
	INTEGER*4 KGIND		    ! Kicker Game Index.
	INTEGER*4 ROW		    ! Row Number played.
	INTEGER*4 DRWNUM	    ! Draw Number played on (from terminal)
        INTEGER*4 BNKLO
        INTEGER*4 BNKHI
	INTEGER*4 GIND
C
        LOGICAL   JOK1_PART         ! joker 1 participation 
        LOGICAL   JOK2_PART         ! joker 2 participation 
        LOGICAL   JOK1_REQ          ! joker 1 number requested
        LOGICAL   JOK2_REQ          ! joker 2 number requested

	EQUIVALENCE (TEMP,I2TEMP)
	EQUIVALENCE (CREV,I2CREV)
	EQUIVALENCE (TREV,I2TREV)
	EQUIVALENCE (TIKTREV,I2TIKTREV)           
        EQUIVALENCE (JOKCREV,I2JOKCREV)           
        EQUIVALENCE (JOKTREV,I2JOKTREV)           
        EQUIVALENCE (JOKTIKTREV,I2JOKTIKTREV)         

C--------------------------- Start of Code -------------------------------

C---- Get generic game information.

        JOK1_PART       = .FALSE.
        JOK2_PART       = .FALSE.
        JOK1_REQ        = .FALSE.
        JOK2_REQ        = .FALSE.
	SYNTERRCOD      = 0
	TRABUF(TSTAT)   = REJT
	TRABUF(TGAMTYP) = TSTR

C---- Get transaction number.

	TEMP = ZEXT(TERMES(1))
	TRABUF(TTRN)=IAND(TEMP,15)

C---- Get checksum.

        TEMP1 = ZEXT(TERMES(3))
        TEMP2 = ZEXT(TERMES(4))
        TRABUF(TCHK) = ISHFT(TEMP1,8) + TEMP2

C---- Get statistics.

        TRABUF(TTSTCS) = ZEXT(TERMES(5))

C---- Get game index and system type.

        TEMP = ZEXT(TERMES(7))
	TRABUF(TWSYST) = IAND(TEMP,15)
	TRABUF(TGAMIND) = ISHFT(TEMP,-4)
	GIND = TRABUF(TGAMIND)
	IF (GIND.LT.1.OR.GIND.GT.NUMSTR) THEN
	   TRABUF(TERR) = SYNT
	   SYNTERRCOD = 4
	   RETURN
	ENDIF
        TRABUF(TGAM) = GTNTAB(TSTR,GIND)
 
	IF (TRABUF(TGAM) .LT. 1 .OR.
     *	    TRABUF(TGAM) .GT. MAXGAM) THEN
	  TRABUF(TERR) = SYNT
	  SYNTERRCOD = 5
	  RETURN
	END IF

C---- Get duration and number of boards.

        TEMP = ZEXT(TERMES(8))
	TRABUF(TWNBET) = IAND(TEMP,15)
	TRABUF(TWDUR) = ISHFT(TEMP,-4)

C---- Set maximum fractions.

	TRABUF(TFRAC)  = MAXFRC(TRABUF(TGAM))
	TRABUF(TNFRAC) = 1

C---- Set options field.

	TEMP1 = ZEXT(TERMES(9))
	TEMP2 = ZEXT(TERMES(10))
        OPTIONS = ISHFT(TEMP1,8) + TEMP2
        IND = 11

C---- Check for control revision.

        IF (IAND(OPTIONS,'8000'X) .NE. 0) THEN
          CREV = 0
          CALL MOVBYT(TERMES,IND,CREV,3,2)
	  IND = IND + 2
          TEMP = STRREV(GIND)
 	  IF (I2CREV(2) .NE. I2TEMP(1)) THEN
	    TRABUF(TERR) = GREV
      	    TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_GAMCLT)
	  END IF
        END IF

C---- Check for text revision.

        IF (IAND(OPTIONS,'4000'X) .NE. 0) THEN
	  TREV = 0
          CALL MOVBYT(TERMES,IND,TREV,3,2)
          IND = IND + 2
          TEMP = STRREV(GIND)
          IF (I2TREV(2) .NE. I2TEMP(2)) THEN
	    TRABUF(TERR) = GREV
       	    TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_GAMTXT)
	  END IF
        END IF

C---- Check for ticket text revision.

        IF (IAND(OPTIONS,'2000'X) .NE. 0) THEN
	  TIKTREV = 0
          CALL MOVBYT(TERMES,IND,TIKTREV,3,2)
          IND = IND + 2
          TEMP = TKTMRV(TRABUF(TGAM))
	  IF (I2TIKTREV(2) .NE. I2TEMP(1) .AND.
     *	      TKTMRV(TRABUF(TGAM)) .GT. 0) THEN 
	    TRABUF(TERR) = GREV
      	    TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_GAMTKT)
	  END IF
        END IF

C---- Check for joker 1 participation.

	IF (IAND(OPTIONS,'0080'X) .NE. 0) THEN
          JOK1_PART = .TRUE.
          TRABUF(TWKFLG) = 1
          TRABUF(TERR) = SYNT
          SYNTERRCOD = 801
          RETURN
	END IF

C---- Check for joker 2 participation.

	IF (IAND(OPTIONS,'0040'X) .NE. 0) THEN
          JOK2_PART = .TRUE.
          TRABUF(TWKFLG2) = 1
          TRABUF(TERR) = SYNT
          SYNTERRCOD = 802
          RETURN
	END IF

C---- Check for joker 1 request.

	IF (IAND(OPTIONS,'0020'X) .NE. 0) THEN
	  JOK1_REQ =.TRUE.
          TRABUF(TERR) = SYNT
          SYNTERRCOD = 803
          RETURN
        END IF

C---- Check for joker 2 request.

	IF (IAND(OPTIONS,'0010'X) .NE. 0) THEN
	  JOK2_REQ =.TRUE.
          TRABUF(TERR) = SYNT
          SYNTERRCOD = 804
          RETURN
        END IF

C---- Check for joker 1 problem - participation but no request.

        IF (JOK1_PART .AND. .NOT. JOK1_REQ) THEN
          TRABUF(TERR) = SYNT
          SYNTERRCOD = 115
          RETURN
        END IF

C---- Check for joker 2 problem - participation but no request.

        IF (JOK2_PART .AND. .NOT. JOK2_REQ) THEN
          TRABUF(TERR) = SYNT
          SYNTERRCOD = 120
          RETURN
        END IF

C---- Check for QP flags.

	IF (IAND(OPTIONS,'0200'X) .NE. 0) THEN
	  TEMP1 = ZEXT(TERMES(IND+0))
	  TEMP2 = ZEXT(TERMES(IND+1))
	  TRABUF(TWQPF) = ISHFT(TEMP1,8) + TEMP2
	  IND = IND + 2
	END IF

C---- Check for system bet flag if true get system number.

        IF (IAND(OPTIONS,'0100'X) .NE. 0) THEN
          IF (TRABUF(TWSYST) .EQ. NOSYS) THEN
            TRABUF(TERR) = SYNT
            SYNTERRCOD = 20
            RETURN
          END IF
	  TEMP1 = ZEXT(TERMES(IND+0))
	  TEMP2 = ZEXT(TERMES(IND+1))
          TRABUF(TWSYSN) = ISHFT(TEMP1,8) + TEMP2
          IND = IND + 2
        END IF
C
	IF(TRABUF(TWSYST).EQ.NOSYS) TRABUF(TWSYSN) = 1
 
        IF(TRABUF(TWSYSN).LE.0) THEN
            SYNTERRCOD = 21
            TRABUF(TERR)=SYNT
            RETURN
        ENDIF
 

C---- Get bank number and account number.
                                                                               
        IF (IAND(OPTIONS,'0008'X) .NE. 0) THEN                                  
          TEMP1 = ZEXT(TERMES(IND))
          TEMP2 = ZEXT(TERMES(IND+1))
          TEMP3 = ZEXT(TERMES(IND+2))
          TEMP4 = ZEXT(TERMES(IND+3))
          TRABUF(TWBNKID) =  ISHFT(TEMP1,24) + ISHFT(TEMP2,16) +
     *                       ISHFT(TEMP3,8) + TEMP4

          IND = IND + 4                                                        
          TEMP1 = ZEXT(TERMES(IND))
          TEMP2 = ZEXT(TERMES(IND+1))
          TEMP3 = ZEXT(TERMES(IND+2))
          TEMP4 = ZEXT(TERMES(IND+3))
          TRABUF(TWBNKNM) =  ISHFT(TEMP1,24) + ISHFT(TEMP2,16) +
     *                       ISHFT(TEMP3,8) + TEMP4
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
        END IF

C---- Check message checksum.

        IF (P(SUPSUM) .EQ. 0) THEN
          IF(.NOT.BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTSUM)) THEN
            I4CCITT   = IAND(BASECHKSUM+TRABUF(TTER),'FFFF'X)
            TERMES(3) = I1CCITT(2)
            TERMES(4) = I1CCITT(1)
            CHKLEN = MESLEN-1
            CALL GETCCITT(TERMES,1,CHKLEN,MYCHKSUM)
            IF (MYCHKSUM .NE. TRABUF(TCHK)) THEN
              TRABUF(TERR) = CBAD
              SYNTERRCOD = 10
              RETURN
            END IF
          END IF
	ENDIF
C

C---- Get Coupon ID.

	TRABUF(TWSTCOUPID) = ZEXT(TERMES(IND))
	IND = IND + 1
C
C---- Get draw number.
C
        TEMP1 = ZEXT(TERMES(IND+0))
        TEMP2 = ZEXT(TERMES(IND+1))
        DRWNUM = ISHFT(TEMP1,8) + TEMP2
	IND = IND + 2
C
C---- Get # no of rows of event A
C
        TRABUF(TWSTM1) = ZEXT(TERMES(IND+0))
	IND = IND + 1
C
C---- Get # no of rows of event B
C
        TRABUF(TWSTM2) = ZEXT(TERMES(IND+0))
	IND = IND + 1
C
C---- Get # no of rows of event C
C
        TRABUF(TWSTM3) = ZEXT(TERMES(IND+0))
	IND = IND + 1
C
	IF(TRABUF(TWSTM1)+TRABUF(TWSTM2)+TRABUF(TWSTM3).GT.TWSTBMAX) THEN
	  TRABUF(TERR) = SYNT
          SYNTERRCOD = 29
          RETURN
	ENDIF
C
C---- Check game number.

	IF (TRABUF(TGAM) .LT. 1 .OR.
     *	    TRABUF(TGAM).GT.MAXGAM) THEN
	  TRABUF(TERR) = SYNT
	  SYNTERRCOD = 30
	  RETURN
	END IF

C---- Check game status.

	IF (STRSTS(GIND) .NE. GAMOPN) THEN
	  TRABUF(TERR) = SDRW
	  RETURN
	END IF

C---- Check durarion.

	IF (TRABUF(TWDUR) .NE. 1) THEN
	  TRABUF(TERR) = SYNT
	  SYNTERRCOD = 100
	  RETURN
	END IF

        IF (TRABUF(TWNBET) .NE. 1) THEN    
          TRABUF(TERR) = SYNT
          SYNTERRCOD = 70
          RETURN
        END IF

C---- Get Super Triple bet data.

        OFF=0
        DO I=0,TRABUF(TWSTM1)-1
           TRABUF(TWSTBET+OFF+I)=ZEXT(TERMES(IND))
           IND=IND+1 
        ENDDO
        OFF=OFF+TRABUF(TWSTM1)
        DO I=0,TRABUF(TWSTM2)-1
           TRABUF(TWSTBET+OFF+I)=ZEXT(TERMES(IND))
           IND=IND+1 
        ENDDO
        OFF=OFF+TRABUF(TWSTM2)
        DO I=0,TRABUF(TWSTM3)-1
           TRABUF(TWSTBET+OFF+I)=ZEXT(TERMES(IND))
           IND=IND+1 
        ENDDO

        TEMP1 = ZEXT(TERMES(IND))
        TEMP2 = ZEXT(TERMES(IND+1))
        TEMP3 = ZEXT(TERMES(IND+2))
        TEMP4 = ZEXT(TERMES(IND+3))
        TRABUF(TWSTAMT) = (ISHFT(TEMP1,24) + ISHFT(TEMP2,16)+
     *                     ISHFT(TEMP3,8) + TEMP4)/TRABUF(TWSYSN)
	IND = IND + 4
        IF(TRABUF(TWSTAMT).LT.STRPRC(GIND)) THEN
	    TRABUF(TERR) = SYNT 
	    SYNTERRCOD = 140	
	    RETURN
        ENDIF

C---- Check Todays Triple bet data

        OFF=0
        DO I=0,TRABUF(TWSTM1) -1
           ROW = TRABUF(TWSTBET+OFF+I)
	   IF (ROW .LT. 1. OR. ROW .GT. MAXSTRRW) THEN
	      TRABUF(TERR) = SYNT
    	      SYNTERRCOD = 141
	      RETURN
	   END IF
           IF(STRSTA(ROW,GIND).NE.GAMOPN) THEN
	      TRABUF(TERR) = GREV 
       	      TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_GAMCLT)
           ENDIF
	   IF(STRPCC(GIND).EQ.1) THEN
	     IF(ROW.EQ.17.OR.ROW.EQ.18) THEN
                TRABUF(TERR) = SYNT
                SYNTERRCOD = 83
                RETURN
             ENDIF
	   ENDIF

        ENDDO

        OFF=OFF+TRABUF(TWSTM1)
        DO I=0,TRABUF(TWSTM2) -1
           ROW = TRABUF(TWSTBET+OFF+I)
	   IF (ROW .LT. 1. OR. ROW .GT. MAXSTRRW) THEN
	      TRABUF(TERR) = SYNT
    	      SYNTERRCOD = 141
	      RETURN
	   END IF
           IF(STRSTA(ROW,GIND).NE.GAMOPN) THEN
	     TRABUF(TERR) = GREV 
      	     TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_GAMCLT)
           ENDIF
	   IF(STRPCC(GIND).EQ.1) THEN
	     IF(ROW.EQ.16.OR.ROW.EQ.18) THEN
                TRABUF(TERR) = SYNT
                SYNTERRCOD = 84
                RETURN
             ENDIF
	   ENDIF

        ENDDO

        OFF=OFF+TRABUF(TWSTM2)
        DO I=0,TRABUF(TWSTM3) -1
           ROW = TRABUF(TWSTBET+OFF+I)
	   IF (ROW .LT. 1. OR. ROW .GT. MAXSTRRW) THEN
	      TRABUF(TERR) = SYNT
    	      SYNTERRCOD = 141
	      RETURN
	   END IF
           IF(STRSTA(ROW,GIND).NE.GAMOPN) THEN
	     TRABUF(TERR) = GREV 
      	     TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_GAMCLT)
           ENDIF
	   IF(STRPCC(GIND).EQ.1) THEN
	     IF(ROW.EQ.16.OR.ROW.EQ.17) THEN
                TRABUF(TERR) = SYNT
                SYNTERRCOD = 85
                RETURN
             ENDIF
	   ENDIF
        ENDDO
C
C Calculate # of combinations.
C
	CMB  = 0
	OFF  = TWSTBET + TRABUF(TWSTM1)
	OFF2 = OFF + TRABUF(TWSTM2)

        DO I=0,TRABUF(TWSTM1) - 1
	   DO J=0, TRABUF(TWSTM2) - 1
              DO K=0,TRABUF(TWSTM3) - 1
		 IF(TRABUF(TWSTBET+I).NE.TRABUF(OFF+J).AND.
     *              TRABUF(TWSTBET+I).NE.TRABUF(OFF2+K).AND.
     *              TRABUF(OFF+J).NE.TRABUF(OFF2+K)) CMB = CMB + 1
	      ENDDO
	   ENDDO
	ENDDO
C
	IF(CMB.NE.TRABUF(TWSYSN)) THEN
	   TRABUF(TERR) = SYNT
           SYNTERRCOD = 190
           RETURN
	ENDIF
C
	TRABUF(TWAMT) = TRABUF(TWSTAMT)*TRABUF(TWSYSN)

C---- Check bet amount

        IF(TRABUF(TWSTAMT).GT.P(MAXSTR)) THEN
           TRABUF(TERR) = SYNT
           SYNTERRCOD = 191
           RETURN
        ENDIF
        IF (TRABUF(TWAMT).GT.P(MAXSTRS)) THEN
           TRABUF(TERR) = SYNT
           SYNTERRCOD = 192
           RETURN
        END IF

C---- Check for error since we could let revision error trough in order to
C---- accumulate them and catch syntax ones.

	IF (TRABUF(TERR) .NE. NOER) RETURN

	TRABUF(TSTAT) = GOOD
	TRABUF(TWMFRAC) = TRABUF(TFRAC)
	TRABUF(TWBEG) = STRDRW(GIND)
	TRABUF(TWEND) = TRABUF(TWBEG) + TRABUF(TWDUR) - 1
	TRABUF(TFIL) = EARLY
	TRABUF(TWTKC) = TKTCHG(TRABUF(TGAM))
	TRABUF(TWTOT) = TRABUF(TWAMT) * TRABUF(TWDUR) + 
     *		        TRABUF(TWKAMT) * TRABUF(TWKDUR) + 
     *			TRABUF(TWTKC)

	RETURN

	END
