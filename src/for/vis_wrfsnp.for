C WRFSNP.FOR                                                                    
C
C V18 02-DEC-2000 UXN TOTOGOLO ADDED.
C V17 11-APR-2000 OXK Values w/o VALUNITS
C V16 24-MAR-2000 UXN UINT added.
C V15 13-OCT-1999 RXK World Tour added.
C V14 17-MAY-1999 UXN Super Triple added.
C V13 15-AUG-1996 RXK Fix of display of money amounts
C V12 27-JUL-1995 PXB Changed so as V5 game only has one division.
C V11 01-FEB-1995 HXK Fix for removing offline sales for lotto, vakio, kicker
C V10 07-DEC-1994 PXB Initialiazed monsy fields to zero so as games do not
C                     pick up another game's amounts.
C V09 02-NOV-1993 HXK 5 PENNY OUT.
C V08 10-SEP-1993 HXK Changed WRF file variables
C V07 10-JAN-1993 HJK CHANGED FOR VIKING LOTTO 
C V06 26-OCT-1992 HJK AMENDED FOR SPEDEN
C V05 03-MAY-1992 HJK CHANGED USEAMT
C V04 18-APR-1992 HJK FIX FOR MIN COMMAND
C V03 07-APR-1992 HJK FURTHER CHANGES FOR NEW KENO
C V02 16-MAR-1992 MTK CHANGED FOR NEW KENO GAME 
C V01 16-JUL-1991 MTK INITIAL RELEASE FOR FINLAND
C                                                                               
C WIN RESERVE FUND SNAPSHOT                                                     
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
C Copyright 2000 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C 
C=======OPTIONS /CHECK=NOOVERFLOW/EXT                                                                      
        SUBROUTINE WRFSNP(NUM,CLINE)                                              
        IMPLICIT NONE
C                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
                                                               
        INCLUDE  'INCLIB:GLOBAL.DEF'
        INCLUDE  'INCLIB:PRMAGT.DEF'
        INCLUDE  'INCLIB:AGTINF.DEF'
        INCLUDE  'INCLIB:VISCOM.DEF'
        INCLUDE  'INCLIB:CONCOM.DEF'
	INCLUDE  'INCLIB:DATBUF.DEF'

        INCLUDE  'INCLIB:LTOCOM.DEF'
        INCLUDE  'INCLIB:KIKCOM.DEF'
        INCLUDE  'INCLIB:SPTCOM.DEF'
        INCLUDE  'INCLIB:WITCOM.DEF'
        INCLUDE  'INCLIB:TSLCOM.DEF'
        INCLUDE  'INCLIB:SCRCOM.DEF'
        INCLUDE  'INCLIB:BNGCOM.DEF'
        INCLUDE  'INCLIB:CPLCOM.DEF'
        INCLUDE  'INCLIB:DBLCOM.DEF'
        INCLUDE  'INCLIB:SSCCOM.DEF'
        INCLUDE  'INCLIB:TRPCOM.DEF'
        INCLUDE  'INCLIB:STRCOM.DEF'
	INCLUDE  'INCLIB:TGLCOM.DEF'
        INCLUDE  'INCLIB:RWFCOM.DEF'
C                                                                               
        ! arguments
        INTEGER*4  NUM                  !
        INTEGER*4  CLINE(20)            !

        ! variables
        INTEGER*4  CBUF(CDLEN)          !
        INTEGER*4  TAB(5)               !
        INTEGER*4  MOVJAK               !
        INTEGER*4  BALSUM               !       
        INTEGER*4  GNUM                 !
        INTEGER*4  GTYP                 !
        INTEGER*4  GIND                 !
        INTEGER*4  GST                  !
        INTEGER*4  LIM                  !
        INTEGER*4  VALUE                !
        INTEGER*4  POS                  !
        INTEGER*4  KEYNUM               !
        INTEGER*4  ST                   !
        INTEGER*4  K                    !
        INTEGER*4  NEWRES               !
        INTEGER*4  DRAW                 !
        INTEGER*4  MINAMT               !
        INTEGER*4  MV1AMT               !
        INTEGER*4  MV2AMT               !
        INTEGER*4  MV3AMT               !
        INTEGER*4  MV4AMT               !
        INTEGER*4  MV5AMT               !
	INTEGER*4  WEEK,YEAR,CDC
	REAL*8     ROL1AMT,ROL2AMT

        REAL*8     KEYS(7)              !                                  

        CHARACTER*8 STATUS                                                        

        DATA KEYS/'OFFline ','MOV1    ','MOV2    ','MINimum ',                    
     *            'MOV3    ','MOV4    ','MOV5    '/                               
C                                                                               
C                                                                               
        GNUM = NUM                                                                  
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
            WRITE(CLIN23,3040)
            RETURN
        ENDIF

        GTYP = GNTTAB(GAMTYP,GNUM)                                                  
        GIND = GNTTAB(GAMIDX,GNUM)                                                  
        IF(GTYP.LT.1.OR.GIND.LT.1) THEN                                           
            WRITE(CLIN23,3010) GNUM                                                 
            RETURN                                                                  
        ENDIF                                                                     
C                                                                               
        GST = -1                                                                    
        IF(GTYP.EQ.TLTO) THEN
	  GST = LTOSTS(GIND)
	  CDC = LTOESD(GIND)
	ELSEIF(GTYP.EQ.TSPT) THEN
	  GST = SPTSTS(GIND)
	  CDC = SPTESD(GIND)
	ELSEIF(GTYP.EQ.TTGL) THEN
	  GST = TGLSTS(GIND)
	  CDC = TGLESD(GIND)
	ELSEIF(GTYP.EQ.TKIK) THEN
	  GST = KIKSTS(GIND)
	  CDC = KIKESD(GIND)                                         
        ELSEIF(GTYP.EQ.TBNG) THEN
	  GST = BNGSTS(GIND)   
          CDC = BNGESD(GIND)
        ELSEIF(GTYP.EQ.TTSL) THEN
	  GST = TSLSTS(GIND)
	  CDC = TSLESD(GIND) - WEEK_OFFSET  
        ELSEIF(GTYP.EQ.TSCR) THEN
	  GST = SCRSTS(GIND)
	  CDC = SCRESD(GIND) - WEEK_OFFSET   
        ELSEIF(GTYP.EQ.TWIT) THEN 
	  GST = WITSTS(GIND)
	  CDC = WITESD(GIND) - WEEK_OFFSET   
        ELSEIF(GTYP.EQ.TCPL) THEN
	  GST = CPLSTS(GIND)
	  CDC = CPLESD(GIND) - WEEK_OFFSET   
        ELSEIF(GTYP.EQ.TDBL) THEN
	  GST = DBLSTS(GIND)
	  CDC = DBLESD(GIND) - WEEK_OFFSET   
        ELSEIF(GTYP.EQ.TSSC) THEN
	  GST = SSCSTS(GIND)
	  CDC = SSCESD(GIND) - WEEK_OFFSET   
        ELSEIF(GTYP.EQ.TTRP) THEN
	  GST = TRPSTS(GIND)
	  CDC = TRPESD(GIND) - WEEK_OFFSET   
        ELSEIF(GTYP.EQ.TSTR) THEN
	  GST = STRSTS(GIND)
	  CDC = STRESD(GIND) - WEEK_OFFSET   
        ENDIF                                                                                                   
C                                                                               
	CALL FIGWEK(CDC,WEEK,YEAR)
C
        VALUE = 0                                                                   
        POS   = 1                                                                     
        LIM   = 4       
        IF(GTYP .EQ. TTGL) LIM=6  ! TOTOGOLO
        IF(GTYP .EQ. TSPT) LIM=6  ! SPORTS                                                     
        IF(GTYP .EQ. TLTO) LIM=7  ! LOTTO'S
        CALL KEY(CLINE,KEYS,LIM,POS,KEYNUM)                                       
C                                                                               
        IF(POS.GT.40) GOTO 100                  !NO INPUT                      
        IF(KEYNUM.EQ.0) THEN                                                      
            WRITE(CLIN23,3040)                                                      
            RETURN                                                                  
        ENDIF                                                                     
        IF(GTYP.EQ.TBNG.AND.KEYNUM.NE.4) THEN                                                      
            WRITE(CLIN23,3040)                                                      
            RETURN                                                                  
        ENDIF                                                                     
C                                                                               
C                                                                               
        CALL WRFNUMB(CLINE,POS,VALUE)           !GET VALUE                        
        IF(VALUE.LT.0) THEN                                                       
            WRITE(CLIN23,3050)                                                      
            RETURN                                                                  
        ENDIF                                                                     
        CALL FASTSET(0,CBUF,CDLEN)                                                
C                                                                               
C CHANGE OFFLINE AMOUNT (but not for VIKING or ODDSET)                                                        
C                                                                               
        IF(KEYNUM.EQ.1)THEN   
            IF(   GTYP.EQ.TLTO .OR.
     *            GTYP.EQ.TSPT .OR. GTYP.EQ.TTGL .OR.
     *            GTYP.EQ.TTSL .OR.
     *            GTYP.EQ.TWIT .OR.
     *            GTYP.EQ.TSCR)  THEN
               CONTINUE
            ELSE             
               CBUF(1) = 9                                                               
               CBUF(2) = VALUE                                                           
               CBUF(3) = TCGEN                                                           
               CBUF(5) = GNUM                                                            
               GOTO 10 
            ENDIF                                                    
        ENDIF                                                                     
C                                                                               
C CHANGE MOVE 1 AMOUNT  (but not for  VIKING)                                                        
C                                                                               
        IF(KEYNUM.EQ.2) THEN   
            IF(GTYP.EQ.TTSL .OR.
     *         GTYP.EQ.TWIT .OR.
     *         GTYP.EQ.TSCR)  THEN
                CONTINUE
            ELSE             
                CBUF(1) = 10                                                               
                CBUF(2) = VALUE                                                           
                CBUF(3) = TCGEN                                                           
                CBUF(5) = GNUM                                                            
                CBUF(8) = 1                                                               
                GOTO 10                                                                 
            ENDIF                                                                     
        ENDIF                                                                        
C
C CHANGE MOVE 2 AMOUNT                                                          
C                                                                               
        IF(KEYNUM.EQ.3) THEN                                                      
            IF(GTYP.EQ.TTSL .OR.
     *            GTYP.EQ.TWIT .OR.
     *            GTYP.EQ.TSCR)  THEN
                CONTINUE
            ELSE
                CBUF(1) = 10                                                              
                CBUF(2) = VALUE                                                           
                CBUF(3) = TCGEN                                                           
                CBUF(5) = GNUM                                                            
                CBUF(8) = 2                                                               
                GOTO 10
            ENDIF                                                                 
        ENDIF                                                                     
C                                                                               
C CHANGE MINIMUM JACKPOT AMOUNT (DIV 2 IF GNUM=7; i.e.VIKING)                   
C                                                                               
        IF(KEYNUM.EQ.4 .AND. GST.LE.GAMBFD) THEN                                    
            IF(GTYP.EQ.TTSL .OR.
     *            GTYP.EQ.TWIT .OR.
     *            GTYP.EQ.TSCR)  THEN
                CONTINUE
            ELSE
                CBUF(1) = 11                                                              
                CBUF(2) = VALUE                                                           
                CBUF(3) = TCGEN                                                           
                CBUF(5) = GNUM                                                            
                GOTO 10
            ENDIF                                                                 
        ENDIF                                                                     
C                                                                               
C CHANGE MOVE 3 AMOUNT                                                        
C                                                                               
        IF(KEYNUM.EQ.5 .AND. 
     *     (GTYP.EQ.TLTO .OR.GTYP.EQ.TSPT .OR. GTYP.EQ.TTGL) ) THEN                         
            CBUF(1) = 10                                                          
            CBUF(2) = VALUE                                                           
            CBUF(3) = TCGEN                                                           
            CBUF(5) = GNUM                                                            
            CBUF(8) = 3                                                               
            GOTO 10                                                                 
        ENDIF                                                                     
C                                                                               
C CHANGE MOVE 4 AMOUNT    
C                                                                               
        IF(KEYNUM.EQ.6 .AND. 
     *     (GTYP.EQ.TLTO .OR. GTYP.EQ.TSPT .OR. GTYP.EQ.TTGL) )THEN  
            CBUF(1) = 10                                                              
            CBUF(2) = VALUE                                                           
            CBUF(3) = TCGEN                                                           
            CBUF(5) = GNUM                                                            
            CBUF(8) = 4                                                               
            GOTO 10                                                                 
        ENDIF                                                                     
C                                                                               
C CHANGE MOVE 5 AMOUNT                                                         
C                                                                               
        IF(KEYNUM.EQ.7 .AND. GTYP .EQ. TLTO) THEN                                        
            CBUF(1) = 10                                                              
            CBUF(2) = VALUE                                                           
            CBUF(3) = TCGEN                                                           
            CBUF(5) = GNUM                                                            
            CBUF(8) = 5                                                               
            GOTO 10                                                                 
        ENDIF                                                                     
        GOTO 100                                                                  
C                                                                               
C GENERATE COMMAND                                                              
C                                                                               
10      CONTINUE                                                                  
        CBUF(6)=IDNUM                                                             
        CALL VISCMD(CBUF,ST)                                                      
        CALL XWAIT(2,2,ST)                                                         
C                                                                               
100     CONTINUE                                                                  
        CALL FASTMOV(RWF_WRFGAM(1,GNUM),TAB,5)                                        
        NEWRES=TAB(RESAMT)+TAB(ONPAMT)+TAB(OFPAMT)+                               
     *                     TAB(PENAMT)-TAB(USEAMT)                                
C                                                                               
C                                                                               
        DRAW   = 1                                                                    
        MOVJAK = 0                                                                  
        BALSUM = 0                                                                  

        MINAMT = 0
        MV1AMT = 0
        MV2AMT = 0
        MV3AMT = 0
        MV4AMT = 0
        MV5AMT = 0
C                                                                               
C                                                                               
        IF(GTYP.EQ.TLTO) THEN       ! LOTTO                                              
            DRAW   = LTODRW(GIND)                                                       
            MINAMT = LTOMIN(GIND)                                                     
            IF(GIND.EQ.1) MV1AMT = LTOASH(1,GIND)                                       
            MV2AMT = LTOASH(2,GIND)                                                   
            MV3AMT = LTOASH(3,GIND)                                     
            MV4AMT = LTOASH(4,GIND)                                     
            MV5AMT = LTOASH(5,GIND)                                     
        ENDIF                                                                     
C                                                                               
C                                                                               
        IF(GTYP.EQ.TSPT) THEN       ! SPORTS (VAKIO)                                              
            DRAW   = SPTDRW(GIND)                                                       
            MINAMT = SPTMIN(GIND)                                                     
            MV1AMT = SPTASH(1,GIND)                                                   
            MV2AMT = SPTASH(2,GIND)
            MV3AMT = SPTASH(3,GIND)
            MV4AMT = SPTASH(4,GIND)                                                   
        ENDIF                                                                     
C                                                                               
        IF(GTYP.EQ.TTGL) THEN    
            DRAW   = TGLDRW(GIND)                                                       
            MINAMT = TGLMIN(GIND)                                                     
            MV1AMT = TGLASH(1,GIND)                                                   
            MV2AMT = TGLASH(2,GIND)
            MV3AMT = TGLASH(3,GIND)
            MV4AMT = TGLASH(4,GIND)                                                   
        ENDIF                                                                     
C                                                                               
C                                                                               
        IF(GTYP.EQ.TKIK) THEN       ! Joker                                             
            DRAW   = KIKDRW(GIND)                                                       
            MINAMT = KIKMIN(GIND)                                                     
            MV1AMT = KIKASH(1,GIND)                                                   
            MV2AMT = KIKASH(2,GIND)                                                   
            ROL1AMT = DFLOAT(KIKPOL(1,1,GIND))*DFLOAT(DYN_BETUNIT)+
     *                DFLOAT(KIKPOL(2,1,GIND))                                                   
            ROL2AMT = DFLOAT(KIKPOL(1,2,GIND))*DFLOAT(DYN_BETUNIT)+
     *                DFLOAT(KIKPOL(2,2,GIND))                                                   
        ENDIF                                                                     

        IF(GTYP.EQ.TBNG) THEN       ! Bingo
            DRAW   = BNGDRW(GIND)                                                       
            MINAMT = BNGMIN(GIND)                                                     
        ENDIF                                                                     

C                                                                               
        STATUS='closed  '                                                         
        IF(GST.EQ.GAMOPN) STATUS='open    '                                       
        IF(GST.EQ.GAMDON) STATUS='done    '                                       
        IF(GST.EQ.-1)     STATUS='unknown '                                       
C                                                                               
C                                                                               
        WRITE(CLIN1,901)
        WRITE(CLIN2,999)
        IF(GTYP.EQ.TKIK) THEN
           WRITE(CLIN3,9031) (GLNAMES(K,GNUM),K=1,4),ROL1AMT/100.0
           WRITE(CLIN4,9032) ROL2AMT/100.0
        ELSE
           WRITE(CLIN3,903) (GLNAMES(K,GNUM),K=1,4),MOVJAK/100,MOD(MOVJAK,100)
           WRITE(CLIN4,904)   
        ENDIF
      	WRITE(CLIN5,905) STATUS,
     *			 DFLOAT(TAB(RESAMT))/100.0
      	WRITE(CLIN6,906) DRAW,
     *			 DFLOAT(TAB(ONPAMT))/100.0
      	WRITE(CLIN7,907) WEEK,YEAR,
     *			 DFLOAT(TAB(OFPAMT))/100.0
      	WRITE(CLIN8,908) DFLOAT(BALSUM*DYN_VALUNIT)/100.0     
      	WRITE(CLIN9,909) DFLOAT(TAB(USEAMT))/100.0 
      	WRITE(CLIN10,910) DFLOAT(TAB(PENAMT))/100.0
      	WRITE(CLIN11,911)             
      	WRITE(CLIN12,912) DFLOAT(NEWRES)/100.0 
      	WRITE(CLIN13,999)

        IF(GTYP.EQ.TBNG) THEN
           WRITE(CLIN14,9141) KEYS(1), DFLOAT(TAB(OFPAMT))
        ELSE
           WRITE(CLIN14,914)  KEYS(1), DFLOAT(TAB(OFPAMT))
        ENDIF

        WRITE(CLIN15,999)

        IF (GTYP .EQ. TLTO .AND. GIND .EQ. 2) THEN
            WRITE(CLIN16,9161) KEYS(4),
     *                         CMONY(MINAMT,12,VALUNIT)   
        ELSEIF(GTYP .EQ. TBNG) THEN
            WRITE(CLIN16,9162) KEYS(4),
     *                        CMONY(MINAMT,12,VALUNIT) 
        ELSE
            WRITE(CLIN16,916) KEYS(4),
     *                        CMONY(MINAMT,12,VALUNIT) 
        ENDIF                                          

        WRITE(CLIN17,999)

        IF (GTYP .EQ. TLTO .AND. GIND .EQ. 2) THEN
            WRITE(CLIN18,999)                          
        ELSEIF(GTYP.EQ.TBNG) THEN
            WRITE(CLIN18,999)
        ELSE
            WRITE(CLIN18,918) KEYS(2),
     *                        CMONY(MV1AMT,12,VALUNIT) 
        ENDIF                        
        IF(GTYP.EQ.TBNG) THEN
            WRITE(CLIN19,999)
	ELSE
          WRITE(CLIN19,919) KEYS(3),
     *                      CMONY(MV2AMT,12,VALUNIT)    
	END IF

                                
        IF(GTYP.EQ.TLTO .OR. GTYP.EQ.TSPT .OR. GTYP.EQ.TTGL) THEN   
            WRITE(CLIN20,920) KEYS(5),CMONY(MV3AMT,12,VALUNIT)                                  
            WRITE(CLIN21,921) KEYS(6),CMONY(MV4AMT,12,VALUNIT)                                  
        ELSE                                                                      
            WRITE(CLIN20,999)                                                       
            WRITE(CLIN21,999)                                                       
        ENDIF      
                                                               
        IF(GTYP .EQ. TLTO) THEN
            WRITE(CLIN22,922) KEYS(7),CMONY(MV5AMT,12,VALUNIT)                                  
        ELSE                                                                      
            WRITE(CLIN22,999)                                                       
        ENDIF                                                                     

        RETURN                                                                    
C                                                                               
C                                                                               
3010    FORMAT('Game ',I2,' not currently active')                                
3040    FORMAT('Invalid input')                                                   
3050    FORMAT('Value error, use format MMM.PP')                                  
C                                                                               
C                                                                               
901     FORMAT('Win reserve fund')                                                
903     FORMAT(2X,4A4,2X,T20,'   Moving jackpot amount',T62,I7,'.',I2.2,                
     *         ' mk')                                                             
9031    FORMAT(2X,4A4,2X,T20,'   Moving Div 1 amount (roll)',T60,F12.2,                
     *         ' mk')                                                             
9032    FORMAT(2X,'Status',T20,'   Moving Div 2 amount (roll)',T60,F12.2,' mk')
904     FORMAT(2X,'Status')                                                       
905     FORMAT(2X,A8,1X,1X,T20,'   Previous reserve ',T60,F12.2,' mk')            
906     FORMAT(2X,'Draw ',I3,T20,' + Purged uncashed online wins',                
     *        T60,F12.2,' mk')     
907     FORMAT(2X,'Round ',I2.2,'/',I4,T20,' + Purged uncashed offline ',           
     *         'wins',T60,F12.2,' mk')                                            
908     FORMAT(T20,'-+ Win balance sum',T60,F12.2,' mk')                          
909     FORMAT(T20,'-+ Used for wins',T60,F12.2,' mk')                            
910     FORMAT(T20,' + Penny money',T60,F12.2,' mk')                              
911     FORMAT(T55,'====================')                                        
912     FORMAT(T20,'   New reserve fund',T60,F12.2,' mk')                         
914     FORMAT(2X,'Purged uncashed wins from offline',                            
     *         T51,'*',A8,F12.2,' mk')                                            
9141    FORMAT(2X,'Purged uncashed wins from offline',                            
     *         T51,' ',A8,F12.2,' mk')                                            
916     FORMAT(2X,'Minimum amount in the first division',                         
     *         T51,'*',A8,A12,' mk')                                            
9161    FORMAT(2X,'Minimum amount in the second division',                        
     *         T51,'*',A8,A12,' mk')                                            
9162    FORMAT(2X,'Minimum pot',T51,'*',A8,A12,' mk')                                            
918     FORMAT(2X,'Amount moved to division 1',                                   
     *         T51,'*',A8,A12,' mk')                                            
919     FORMAT(2X,'Amount moved to division 2',                                   
     *         T51,'*',A8,A12,' mk')                                            
920     FORMAT(2X,'Amount moved to division 3',                                   
     *         T51,'*',A8,A12,' mk')                                            
921     FORMAT(2X,'Amount moved to division 4',                                   
     *         T51,'*',A8,A12,' mk')                                            
922     FORMAT(2X,'Amount moved to division 5',                                   
     *         T51,'*',A8,A12,' mk')                                            
999     FORMAT(80(' '))                                                           

        END                                                                       
