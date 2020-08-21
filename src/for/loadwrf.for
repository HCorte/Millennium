C Subroutine LOADWRF
C 
C V02 22-MAR-2000 OXK *_SPTPOLDIV added
C V01 10 Sep 1993 HXK Initial revision.
C
C SUBROUTINE TO LOAD WIN RESERVE FUND FILE                                    
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
C       
C=======OPTIONS /CHECK=NOOVERFLOW/EXT                                         
        SUBROUTINE LOADWRF                                                    
        IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'                                           
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RWFCOM.DEF'                                           
        INCLUDE 'INCLIB:RECRDF.DEF'                                           

        INTEGER*4  FDB(7)                           !                         
        INTEGER*4  ST,I                             !
                                            
                                                                             
        TYPE*,IAM(),'Loading reserve fund / roll pool file'

        CALL OPENW(3,SFNAMES(1,RDF),4,0,0,ST)                                 
        CALL IOINIT(FDB,3,RDFSEC*256)                        
        IF(ST.NE.0) THEN                                                      
            CALL FILERR(SFNAMES(1,RDF),1,ST,0)                                
            RETURN                                                            
        ENDIF                                                                 

        CALL READW(FDB,1,RDFREC,ST)                                           
        IF(ST.NE.0) THEN                                                      
            CALL FILERR(SFNAMES(1,RDF),2,ST,1)                                
            CLOSE(UNIT=3)                                                     
            RETURN                                                            
        ENDIF                                                                 
        CLOSE(UNIT=3)                                                         
                                                                             

C NOTE: WINUPD WILL POST ADDITIONAL PENNY MONEY AND USED AMOUNTS              
C       CALCULATE DURING SHARE CALCULATION WHEN FINAL RESULTS                 
C       ARE POSTED TO THE GAME/VALIDATION FILES.                              
                                                                             
	CALL FASTMOV(RDF_SPTPOLDIV(1,1),RWF_SPTPOLDIV(1,1),NUMSPT*SPGDIV)

        CALL FASTMOV(RDF_WRFTAB(1,1),RWF_WRFGAM(1,1),10*MAXGAM)
        CALL FASTMOV(RDF_WRFCUD(1),RWF_WRFDRW(1),MAXGAM)     
        DO I = 1, MAXGAM                                                      
            IF(RWF_WRFDRW(I).NE.DAYDRW(I)) THEN      
                RWF_WRFDRW(I)=DAYDRW(I)                    
                RWF_WRFGAM(RESAMT,I) = RWF_WRFGAM(RESAMT,I) + 
     *                                 RWF_WRFGAM(ONPAMT,I) +      
     *                                 RWF_WRFGAM(OFPAMT,I) - 
     *                                 RWF_WRFGAM(USEAMT,I) +      
     *                                 RWF_WRFGAM(PENAMT,I)    
                RWF_WRFGAM(ONPAMT,I) = 0              
                RWF_WRFGAM(OFPAMT,I) = 0        
                RWF_WRFGAM(PENAMT,I) = 0       
                RWF_WRFGAM(USEAMT,I) = 0       
            ENDIF                                                             
        ENDDO

        RETURN                                                                

        END                                                                   
