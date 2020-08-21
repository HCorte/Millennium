C LDCART.FOR
C
C V05 13-NOV-97 UXN Fix for agent number.
C
C $Log:   GXAFIP:[GOLS]LDCART.FOV  $
C  
C     Rev 1.1   06 Mar 1997 13:12:30   RXK
C  Fix for the case when cartel number is not set
C  
C     Rev 1.0   17 Apr 1996 13:48:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   17 Oct 1993 14:59:24   HXK
C  FIX FOR CARTELS.
C  
C     Rev 1.1   17 Oct 1993 14:50:06   HXK
C  FIX FOR CARTELS.
C  
C     Rev 1.0   07 Jul 1993 18:33:30   HXN
C  Initial revision.
C  
C LOAD CARTEL TABLE.  
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

C=======OPTIONS/CHECK=NOOVERFLOW/EXT
      SUBROUTINE LDCART ( CARTAB )                                              
      IMPLICIT NONE

	INCLUDE 'INCLIB: SYSPARAM.DEF'
        INCLUDE 'INCLIB: SYSEXTRN.DEF'

	INCLUDE 'INCLIB: GLOBAL.DEF'
	INCLUDE 'INCLIB: CONCOM.DEF'
C	INCLUDE 'INCLIB: DESTRA.DEF'
C	INCLUDE 'INCLIB: PRMLOG.DEF'
        INCLUDE 'INCLIB: AGTINF.DEF'
	INCLUDE 'INCLIB: AGTCOM.DEF'
        INCLUDE 'INCLIB: RECAGT.DEF'


        INTEGER*4 CARTAB(2,NUMAGT)                                                

        INTEGER*4 MAXCART,
     *            CARTEL,AGENT,
     *            I,REC,ST
        PARAMETER(MAXCART=10)                                                   

        TYPE*,IAM(),' Loading cartel table...'
                        
        CALL FASTSET( 0,CARTAB,2*NUMAGT )                                         

C LOAD CARTEL TABLE                                                             
C -----------------                                                                               
        CALL OPENASF(ASF)                                                         

      DO 100 REC = 1, NUMAGT                                                    
	 IF (MOD(REC,500).EQ.1) THEN
	    TYPE*,IAM(),' Loading Cartel table in progress ...',REC
	 ENDIF

         CALL READASF(REC,ASFREC,ST)                                            
         IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),1,ST,0)

         CALL ASCBIN(ASFINF,SCHAN,LCHAN,CARTEL,ST)
         IF(ST.NE.0)            GOTO 100

         IF (CARTEL.LT.0)       GOTO 100
         IF (CARTEL.GE.MAXCART) GOTO 100

         CARTAB(1,REC) = CARTEL                                                 

         CALL ASCBIN(ASFINF,SAGNO,LAGNO,AGENT,ST)
	 IF(ST.NE.0) GOTO 100

         CARTAB(2,REC) = AGENT                                               

100   CONTINUE                                                                  

      CALL CLOSASF                                                              

      RETURN                                                                    
      END     ! LDCART.FCC
