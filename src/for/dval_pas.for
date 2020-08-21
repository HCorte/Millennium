C DVAL_PAS.FOR
C
C V08 24-APR-14 SCML eTickets SERN zero is valid.
C V07 23-SET-13 SCML New validation subtype added
C                    TVPLIDTYP added
C V06 15-DEC-10 FJG TPOFFTER terminal gets all privileges
C V05 05-NOV-10 FJG PRIV & BANK error flags
C V04 05-OCT-10 FJG pTickets VALN zero is valid
C                   Privileged with OFF agents in pCDC
C V03 14-APR-10 RXK Fix for Player Card and NIB
C               FJG ePassive
C V02 31-MAR-10 RXK Changes for ePassive
C V01 15-DEC-00 CS  INITIAL RELEASE FOR PORTUGAL
C
C
C SUBROUTINE TO DECODE (PASSIVE) VALIDATION 
C MESSAGE FROM WAGERING TERMINAL
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
C Copyright 1992 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C PROJECT LOTARIA CLASSICA EM SERIES (PLCS)
C ADD NEW FIELD ON FILE
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DVAL_PAS(TERMES,TRABUF,MESLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
C LOCAL VARIABLES
C
	BYTE	    TERMES(*),I1TEMP(4)

	INTEGER*2   MESLEN,I2TEMP(2),WEEK,YEAR

	INTEGER*4   TEMP, I4TEMP, GIND, GNUM, SUBTYP
	INTEGER*4   AUXEMIS,OFF_AUXEMIS
	INTEGER*4   CHKLEN, MYCHKSUM, ENCMES, ENCACT
	INTEGER*4   TICKETS,NUMTCKS,OFFSET,OFFSET_TRA,AGTNUMAUX,INDTAB
        INTEGER*4   TEMP1,TEMP2,TEMP3
        INTEGER*4   IND,JUL,SER,CDIG,CERR,WCDC

        INTEGER*8   NIB
        BYTE        I1NIB(8)
        EQUIVALENCE (NIB,I1NIB)    

	EQUIVALENCE (I1TEMP,I2TEMP,I4TEMP)

C       LOGICAL     FOUND
	LOGICAL     isprv 
C
C EXTERNAL FUNCTIONS
C
	INTEGER*4   GETDRW
	EXTERNAL    GETDRW
        INTEGER*4   INPVER
        EXTERNAL    INPVER
C
C CLEAR SOME VARIABLES
C
	SYNTERRCOD = 0
C
C GET TRANSACTION SEQUENCE NUMBER
C
	TEMP         = ZEXT(TERMES(1))
	TRABUF(TTRN) = IAND(TEMP,15)
C
C GET TERMINAL CHECKSUM
C
	TEMP         = ZEXT(TERMES(3))
	I4TEMP       = ZEXT(TERMES(4))
	TRABUF(TCHK) = ISHFT(TEMP,8) + I4TEMP
C
C GET SUBTYPE
C
	TEMP         = ZEXT(TERMES(2))
	SUBTYP       = IAND(TEMP,'0F'X)
C
C GET TRANSACTION TYPE ACCORDING TO SUBTYPE
C
        if(subtyp.eq.vppinq) then             !subtype 08 => old layout
          trabuf(TVEPVAL) = 0     
          trabuf(TVEPTYP) = 0                       
        elseif(subtyp.eq.vppreg) then         !subtype 09 => old layout
          trabuf(TVEPVAL) = 0                  
          trabuf(TVEPTYP) = 0                                 
        elseif(subtyp.eq.vpnreg) then         !subtype 10 => new layout
          trabuf(TVEPVAL) = 1                
        elseif(subtyp.eq.vpninq) then         !subtype 11 => new layout
          trabuf(TVEPVAL) = 1                
        elseif(subtyp.eq.vpndon) then         !subtype 12 => new layout
          trabuf(TVEPVAL) = 1             
        elseif(subtyp.eq.vpnbnk) then         !subtype 13 => new layout
          trabuf(TVEPVAL) = 1
        elseif(subtyp.eq.vpnibo) then         !subtype 14 => new layout !V07
          trabuf(TVEPVAL) = 1
	else
	  trabuf(TSTAT) = REJT
	  trabuf(TERR)  = SYNT
	  synterrcod    = 50
	  return
	endif

	trabuf(TGAMTYP) = tpas
        trabuf(TVTYPE)  = subtyp
        IF(TRABUF(TERR).NE.INVL .AND. SUBTYP.EQ.VPPINQ ) TRABUF(TERR)=VINQ
C
C GET STATISTICS
C
 	TRABUF(TTSTCS)  = ZEXT(TERMES(5))
C
C GET GAME AND GAME INDEX
C
	TEMP = ZEXT(TERMES(6))

	TRABUF(TGAMIND) = ISHFT(TEMP,-4)
	IF (TRABUF(TGAMIND).GT.0 .AND. TRABUF(TGAMIND).LE.NUMPAS)
     *     TRABUF(TGAM) = GTNTAB(TPAS,TRABUF(TGAMIND))

	IF (TRABUF(TGAM).LT.1 .OR. TRABUF(TGAM).GT.MAXGAM) THEN
	    TRABUF(TSTAT) = REJT
	    TRABUF(TERR)  = SYNT
	    SYNTERRCOD    = 55
	    RETURN
	ENDIF
C
C CHECK IF VALIDATIONS ARE SUPRESSED FOR THIS GAME, FOR ALL TERMINALS (WINPAS CHANGED IT ON DRAW DATES)
C
	IF  (TSBIT(P(SUPGVA),TRABUF(TGAM))) THEN
	    TRABUF(TSTAT) = REJT
	    TRABUF(TERR)  = SUPR
	    RETURN
	ENDIF
C
	GIND = TRABUF(TGAMIND)
	GNUM = TRABUF(TGAM)
C
C OFFLINE/ONLINE AGENT (FROM PRIVILEGED TERMINAL)
C
	I1TEMP(4)      = ZEXT(TERMES(7))
	I1TEMP(3)      = ZEXT(TERMES(8))
	I1TEMP(2)      = ZEXT(TERMES(9))
	I1TEMP(1)      = ZEXT(TERMES(10))
        AGTNUMAUX      = I4TEMP
C
        TRABUF(TPOFFTER) = 0
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!       V06 COMPLETE REWRITE: A TPOFFTER TERMINAL GETS ALL THE PRIVS
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
C
C CHECK FOR PASSIVE AGENT PAYMENTS ON PRIV. TERMINALS
C
        isprv = .FALSE.
        if(TSBIT(agttab(AGTTYP,trabuf(TTER)),AGTPRV)) then      
	  isprv = .TRUE.          
          if(agtnumaux.GT.0) then
            do indtab=1,NUMAGT
              if(agttab(AGTNUM,indtab).eq.agtnumaux) exit
            enddo
!            
            if(indtab.ge.1.and.indtab.le.NUMAGT) then
              if(indtab.ne.trabuf(TTER)) then
                trabuf(TPOFFTER) = indtab
                IF(TSBIT(AGTGAM(GFLAGS,GNUM,indtab),AGTVAL) ) THEN
                  TRABUF(TSTAT) = REJT						
                  TRABUF(TERR)  = SUPR
                ENDIF
                IF(TSBIT(AGTGAM(GFLAGS,TRABUF(TGAM),indtab),AGTVAL)) THEN
                  TRABUF(TSTAT)  = REJT	
                  TRABUF(TERR)   = SUPR	    
                ENDIF		
              else
                IF(TSBIT(AGTGAM(GFLAGS,GNUM,TRABUF(TTER)),AGTVAL) ) THEN
                  TRABUF(TSTAT) = REJT						
                  TRABUF(TERR)  = SUPR
                ENDIF
                IF(TSBIT(AGTGAM(GFLAGS,TRABUF(TGAM),TRABUF(TTER)),AGTVAL)) THEN
                  TRABUF(TSTAT)  = REJT	
                  TRABUF(TERR)   = SUPR	    
                ENDIF		
              endif   
            else
              trabuf(TSTAT) = REJT
              trabuf(TERR)  = BTOPSN            
            endif
          else
            trabuf(TSTAT) = REJT
            trabuf(TERR)  = BTOPSN            
          endif
        else
!+++++++++AS IT WAS, NO CHANGE++++++++++++++++++++++++++++++++++++++++++++++++++          
          IF(.NOT. TSBIT(AGTTAB(AGTTYP,TRABUF(TTER)),AGTTON) ) THEN ! NOT ONLINE
            TRABUF(TSTAT) = REJT
            TRABUF(TERR)  = SUPR
	  ENDIF
	  IF (TSBIT(AGTTAB(AGTTYP,TRABUF(TTER)),AGTVAL)) THEN
            TRABUF(TSTAT)  = REJT	
	    TRABUF(TERR)   = SUPR	    
	  ENDIF
          IF (TSBIT(AGTGAM(GFLAGS,TRABUF(TGAM),TRABUF(TTER)),AGTVAL)) THEN
            TRABUF(TSTAT)  = REJT	
            TRABUF(TERR)   = SUPR	    
          ENDIF
        endif
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!       IF  ( TSBIT(AGTTAB(AGTTYP,TRABUF(TTER)),AGTPRV) ) THEN
!           FOUND  = .FALSE.
!           INDTAB = 1
!           DO  WHILE(INDTAB.LE.NUMAGT .AND. .NOT. FOUND .AND. AGTNUMAUX.GT.0)
!               IF  ( AGTTAB(AGTNUM,INDTAB) .EQ. AGTNUMAUX ) THEN
!                   FOUND = .TRUE.
!               ELSE
!                   INDTAB = INDTAB + 1
!               ENDIF
!           ENDDO
!
!           IF (.NOT. FOUND) THEN
!               TRABUF(TSTAT) = REJT
!               TRABUF(TERR)  = BTOPSN
!           ELSE
!               NOPRV = .FALSE.                              
!               TRABUF(TPOFFTER) = INDTAB
!               IF (TSBIT(AGTGAM(GFLAGS,GNUM,TRABUF(TPOFFTER)),AGTVAL) ) THEN	  ! PASSIVE AGENT ??
!                  TRABUF(TSTAT) = REJT						  ! THIS IS A NON PASSIVE AGENT....
!                  TRABUF(TERR)  = SUPR
!	        ENDIF
!               IF (TSBIT(AGTGAM(GFLAGS,TRABUF(TGAM),TRABUF(TPOFFTER)),AGTVAL)) THEN
!                  TRABUF(TSTAT)  = REJT	
!                  TRABUF(TERR)   = SUPR	    
!               ENDIF		
!	        IF (TSBIT(AGTTAB(AGTTYP,TRABUF(TPOFFTER)),AGTPRV).AND.
!     *              .NOT.TSBIT(AGTTAB(AGTTYP,TRABUF(TPOFFTER)),AGTBNK)) THEN
!                  TRABUF(TPOFFTER) = 0
!               ENDIF
!           ENDIF
!         ELSE
!           IF ( .NOT. TSBIT(AGTTAB(AGTTYP,TRABUF(TTER)),AGTTON) ) THEN	          ! NOT ONLINE ??
!              TRABUF(TSTAT) = REJT						  !
!              TRABUF(TERR)  = SUPR						  ! WAITING TO BE ONLINE (WITH A ALTURA TERM)
!	    ENDIF
!	    IF (TSBIT(AGTTAB(AGTTYP,TRABUF(TTER)),AGTVAL)) THEN
!             TRABUF(TSTAT)  = REJT	
!	      TRABUF(TERR)   = SUPR	    
!	    ENDIF
!           IF (TSBIT(AGTGAM(GFLAGS,TRABUF(TGAM),TRABUF(TTER)),AGTVAL)) THEN
!             TRABUF(TSTAT)  = REJT	
!             TRABUF(TERR)   = SUPR	    
!           ENDIF
!	ENDIF
C
	IF  (TRABUF(TSTAT).EQ.REJT) RETURN					  ! RETURN ERROR CODE
C
C GET NUMBER OF TICKETS TO VALIDATE
C
	NUMTCKS = ZEXT(TERMES(11))
	TRABUF(TPTCK) = NUMTCKS
C
C PPASSIVE VALIDATIONS
C ALLOW ONLY "MAXNUMTCKS" TICKETS A TIME
C
        IF(TRABUF(TVEPVAL).EQ.0) THEN
	  IF(NUMTCKS.LE.0 .OR. NUMTCKS.GT.MAXNUMTCKS) THEN
	    TRABUF(TSTAT) = REJT
	    TRABUF(TERR)  = SYNT
	    SYNTERRCOD    = 60
	    RETURN
	  ENDIF
C
C IF EPASSIVE OR PAYMENT TO BANK THEN ONLY 1 TICKET ALLOWED
C
        ELSE
           IF(NUMTCKS.NE.1) THEN
	      TRABUF(TSTAT) = REJT
	      TRABUF(TERR)  = SYNT
	      SYNTERRCOD    = 60
              RETURN
           ENDIF
        ENDIF
C
C GET ALL TICKETS
C
	DO TICKETS = 1,NUMTCKS
	    OFFSET_TRA = (TICKETS-1)*OFFTRA
	    OFFSET     = (TICKETS-1)*12
C
C           TRABUF(TPSTS1+OFFSET_TRA) = NWIN ! Down
C
C GET WEEK(2 DIGITS) AND YEAR(ONE DIGIT IF BARCODE READING)
C
	    WEEK       = ZEXT(TERMES(12+OFFSET))
	    YEAR       = ZEXT(TERMES(13+OFFSET))
            IF(TRABUF(TVEPVAL).EQ.1) THEN
               TRABUF(TVEPWK) = WEEK
               TRABUF(TVEPYR) = YEAR
            ENDIF           
            YEAR = MOD(YEAR,10)
C
C CONVERT YEAR TO TWO DIGITS(YY) AND CHECK DECADE CHANGES
C
C	    DATE(VCDC) = DAYCDC
C	    CALL CDATE(DATE)
C	    AUXYEAR    = MOD(DATE(VYEAR),10)
C	    IF  (YEAR.EQ.AUXYEAR) THEN
C     		YEAR   = DATE(VYEAR)
C	    ELSEIF(YEAR.EQ.AUXYEAR+1 .OR. YEAR+10.EQ.AUXYEAR+1) THEN
C     		YEAR   = DATE(VYEAR)+1
C	    ELSEIF(YEAR.EQ.AUXYEAR-1 .OR. YEAR-10.EQ.AUXYEAR-1) THEN
C     		YEAR   = DATE(VYEAR)-1
C	    ELSE
C		YEAR   = -1		    !FORCE GETDRW ERROR
C	    ENDIF
C
C CHECK SPECIAL CASE EMISSION = 90 (EXTRAORDINARY DRAWS HAVE TWO SERIES)
C AND WE DO NOT HAVE SERIES # IN PASSIVE BARCODE.
C
C(PLCS)
C	    IF  (WEEK.EQ.90) THEN
C		I4TEMP = 50
C	       TRABUF(TSTAT)= REJT
C	       TRABUF(TERR)=SYNT
C	       SYNTERRCOD = 61
C	       RETURN
C	    ELSE
C		     I4TEMP = WEEK
C	    ENDIF
C(PLCS)
            if(week.gt.0.and.week.le.PMAXWEK) then
              off_auxemis = pasextdrw(week,year,gind)
              if(off_auxemis.gt.0.and.off_auxemis.le.pagemi) then
                auxemis = pasemis(off_auxemis,gind)
                if(auxemis.gt.0) then
	          trabuf(TPEMIS1+offset_tra) = auxemis
	          if(passts(off_auxemis,gind).lt.GFINAL) then
		    trabuf(TPSTS1+offset_tra) = NWSEL
		  else
		    if(passubsts(off_auxemis,gind).eq.pdrwclo) then
                      trabuf(TPSTS1+offset_tra) = PURGED
		    elseif(passubsts(off_auxemis,gind).eq.pdrwpur) then
		      if(isprv) then
	                trabuf(TPSTS1+offset_tra) = NWIN                      
                      else
		        trabuf(TPSTS1+offset_tra) = PURGED      
	              endif
                    elseif(passubsts(off_auxemis,gind).eq.pdrwval) then
	              trabuf(TPSTS1+offset_tra) = NWIN                      
                    else
                      trabuf(TPSTS1+offset_tra) = WEMIS
                    endif
                  endif
                else
                  trabuf(TPSTS1+offset_tra) = WEMIS                
                endif
              else
                trabuf(TPSTS1+offset_tra) = WEMIS                
              endif
            else
              trabuf(TPSTS1+offset_tra) = WEMIS
            endif
C
C DECODE TICKET NUMBER
C
	    I1TEMP(4) = ZEXT(TERMES(14+OFFSET))
	    I1TEMP(3) = ZEXT(TERMES(15+OFFSET))
	    I1TEMP(2) = ZEXT(TERMES(16+OFFSET))
	    I1TEMP(1) = ZEXT(TERMES(17+OFFSET))
	    TRABUF(TPNUM1+OFFSET_TRA) = I4TEMP
C
C VERIFY TICKET NUMBER
C
	    IF(OFF_AUXEMIS.GT.0 .AND. OFF_AUXEMIS.LE.PAGEMI) THEN
		IF( TRABUF(TPNUM1+OFFSET_TRA).LT.0 .OR. 
     *		    TRABUF(TPNUM1+OFFSET_TRA).GT.
     *		    PASNUMTCK(OFF_AUXEMIS,GIND) ) THEN
		    TRABUF(TPSTS1+OFFSET_TRA) = BTKT
		ENDIF
	    ENDIF
C
C DECODE TICKET SERIES
C
	    TEMP = ZEXT(TERMES(18+OFFSET))
C
C SET SERIES = 2 WHEN EMISSION = 90 (WE MUST RECEIVE SERIES = 0 FROM BARCODE)
C ** NOTE ** FOR MANUALLY ENTRY WE WILL RECEIVE SERIES # <> 0
C
C(PLCS)
	    IF  (TEMP.EQ.0) THEN	! READ FROM BARCODE ??
		IF  (WEEK.EQ.90) THEN	! YES ...
		    TEMP = 2
		ELSE
		    TEMP = 1
		ENDIF
	    ENDIF
C(PLCS)
	    TRABUF(TPSER1+OFFSET_TRA) = TEMP
C
C VERIFY SERIE
C
	    IF(OFF_AUXEMIS.GT.0 .AND. OFF_AUXEMIS.LE.PAGEMI) THEN
		IF( TRABUF(TPSER1+OFFSET_TRA).LE.0 .OR. 
     *		    TRABUF(TPSER1+OFFSET_TRA).GT.
     *		    PASNUMSER(OFF_AUXEMIS,GIND) ) THEN
		    TRABUF(TPSTS1+OFFSET_TRA) = BDSER
		ENDIF
	    ENDIF
C
C DECODE TICKET FRACTION
C
	    TEMP = ZEXT(TERMES(19+OFFSET))
	    TRABUF(TPTEN1+OFFSET_TRA) = TEMP
C
C VERIFY FRACTION
C
	    IF (OFF_AUXEMIS.GT.0 .AND. OFF_AUXEMIS.LE.PAGEMI) THEN
	       IF  ( TRABUF(TPTEN1+OFFSET_TRA).LE.0 .OR. 
     *	             TRABUF(TPTEN1+OFFSET_TRA).GT.PASNOFFRA(OFF_AUXEMIS,GIND) ) THEN
		   TRABUF(TPSTS1+OFFSET_TRA) = BTEN
	       ENDIF
	    ENDIF
C
C DECODE TICKET VALIDATION NUMBER
C
	    I1TEMP(4) = ZEXT(TERMES(20+OFFSET))
	    I1TEMP(3) = ZEXT(TERMES(21+OFFSET))
	    I1TEMP(2) = ZEXT(TERMES(22+OFFSET))
	    I1TEMP(1) = ZEXT(TERMES(23+OFFSET))
	    TRABUF(TPKEY1+OFFSET_TRA) = I4TEMP
	ENDDO
C
C EPASSIVE VALIDATIONS
C GET JULIAN DATE
C
        IF(trabuf(TVEPVAL).gt.0) THEN  ! New Layout
          IND = 24 
          TEMP1 = ZEXT(TERMES(IND+0))
          TEMP2 = ZEXT(TERMES(IND+1))
          JUL   = ISHFT(TEMP1,8) + TEMP2
          IND = IND + 2
C
C GET SERIAL NUMBER AND CHECK DIGITS
C
          TEMP1 = ZEXT(TERMES(IND+0))
          TEMP2 = ZEXT(TERMES(IND+1))
          TEMP3 = ZEXT(TERMES(IND+2))
          SER   = ISHFT(TEMP1,16) + ISHFT(TEMP2,8) + TEMP3
          CDIG  = ZEXT(TERMES(IND+3))
          IND   = IND + 4
          CERR  = 0          
!=========LOGIC CHANGES: FIRST eTICKETS=========================================
!          IF(JUL.GT.0.OR.SER.GT.0) THEN
!            TRABUF(TVEPTYP) = 1            
!            IF(TRABUF(TPKEY1).GT.0.OR.JUL.EQ.0.OR.SER.EQ.0) THEN
!               CERR = -1           ! NOTE: ZERO CAN BE WRONG BUT...
!            ELSE
          IF(JUL.GT.0) THEN                                       !V08
            TRABUF(TVEPTYP) = 1                                   !V08
            IF(TRABUF(TPKEY1).GT.0.OR.SER.LT.0.OR.CDIG.LT.0) THEN !V08
              CERR = -1                                           !V08
            ELSE                                                  !V08
              YEAR=DAYYER
              CALL JULCDC(JUL,TRABUF(TVCDC),YEAR)
              CERR=INPVER(TRABUF(TVCDC),SER,TRABUF(TVSER),CDIG)
              IF(CERR.NE.0) THEN
                YEAR=DAYYER-1
                IF(YEAR.LT.0) YEAR=99
                CALL JULCDC(JUL,WCDC,YEAR)
                IF(WCDC.GT.0) CERR = INPVER(WCDC,SER,TRABUF(TVSER),CDIG)
                TRABUF(TVCDC) = WCDC
              ENDIF                            
            ENDIF
          ELSE
            TRABUF(TVEPTYP) = 0   ! VALUE WILL BE CHECKED IN VALN
          ENDIF
!===============================================================================
          IF(CERR.NE.0) THEN
            TRABUF(TERR)=INVL
            TRABUF(TVCODE)=BSER
            trabuf(TPSTS1)=BSERL
          ENDIF

          IF(TRABUF(TVTYPE).EQ.VPNBNK) THEN
C
C GET PLAYER ID TYPE !V07
C
             TEMP = ZEXT(TERMES(IND+0)) !V07
             TRABUF(TVPLIDTYP) = TEMP   !V07
             IND = IND + 1              !V07
C
C GET PLAYER CARD
C
             I1TEMP(4) = ZEXT(TERMES(IND+0))
             I1TEMP(3) = ZEXT(TERMES(IND+1))
             I1TEMP(2) = ZEXT(TERMES(IND+2))
             I1TEMP(1) = ZEXT(TERMES(IND+3))
             TRABUF(TVPLCARD) = I4TEMP
             IND = IND + 4
C
C GET NIB
C
             TEMP1 = ZEXT(TERMES(IND+0))
             TEMP2 = ZEXT(TERMES(IND+1))
             TRABUF(TVNIBBB) = ISHFT(TEMP1,8) + TEMP2
             IND = IND + 2

             TEMP1 = ZEXT(TERMES(IND+0))
             TEMP2 = ZEXT(TERMES(IND+1))
             TRABUF(TVNIBBO) = ISHFT(TEMP1,8) + TEMP2
             IND = IND + 2

             I1NIB(5) = ZEXT(TERMES(IND+0))
             I1NIB(4) = ZEXT(TERMES(IND+1))
             I1NIB(3) = ZEXT(TERMES(IND+2))
             I1NIB(2) = ZEXT(TERMES(IND+3))
             I1NIB(1) = ZEXT(TERMES(IND+4))
             IND = IND + 5
             TRABUF(TVNIBBA1) = NIB/100
             TRABUF(TVNIBBA2) = MOD(NIB,100)

             TRABUF(TVNIBCD) = ZEXT(TERMES(IND))
             IND = IND + 1

          ENDIF
        ENDIF
C
C CHECK MESSAGE CHECKSUM
C
	IF(P(SUPSUM).EQ.0) THEN
	  I4CCITT = IAND(BASECHKSUM+TRABUF(TTER),'FFFF'X)
	  TERMES(3) = I1CCITT(2)
	  TERMES(4) = I1CCITT(1)
	  CHKLEN    = MESLEN-1
	  CALL GETCCITT(TERMES,1,CHKLEN,MYCHKSUM)
	  IF(MYCHKSUM.NE.TRABUF(TCHK)) TRABUF(TERR)=CBAD
	ENDIF
C
C CHECK FOR DES ERROR
C
	IF(P(DESACT).EQ.0) THEN
	  ENCMES = ZEXT(TERMES(1))
	  ENCMES = IAND(ENCMES,'08'X)
	  IF(P(DESFLG).EQ.0.AND.BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTDES)) THEN
	    ENCACT = '08'X
	  ELSE
	    ENCACT = 0
	  ENDIF
	  IF(ENCMES.NE.ENCACT) TRABUF(TERR) = DESMOD
	ENDIF

	IF(TRABUF(TERR).NE.NOER.AND.TRABUF(TERR).NE.VINQ) TRABUF(TSTAT)=REJT

	RETURN
	END
