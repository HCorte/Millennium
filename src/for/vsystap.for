C
C PROGRAM VSYSTAP
C $Log:   GXAFXT:[GOLS]VSYSTAP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:56:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   18 Jun 1993 20:12:04   HXK
C  SEPERATED SUBROUTINES FOR PVCS
C
C ** Source - vsystap.fcc **
C
C  VSYSTAP.FOR
C                                                                               
C  V01 10-DEC-92 HHN Initial release for FINLAND                                  
C                                                                               
C This program                                                                  
C 1. reads (system tables) data from LTOSYS.FIL,                          
C    loads these data into LSYSCOM area.                                        
C 2. reads reduced system tables data from files,                               
C    loads these data into LSYSCOM area.                                        
C 3. save LSYSCOM data back to LTOSYS.FIL.                                      
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
C Copyright 1991 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C -------------------------------------------------                             
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
      PROGRAM VSYSTAP                                                           
      IMPLICIT NONE
                                                                                
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:LSYSCOM.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
                                                                                
                                                                                
                                                                                
      INTEGER*4  CLU          /5/,!console unit                                 
     *           LSF_LU       /2/,!Lotto System file LU
     *           VSTAT                                                          
                                                                                
      INTEGER*4  FDB(7)                                                         
      INTEGER*4  FUN                                                            
      CHARACTER*20 LSF_FILENAME                                                 
      EQUIVALENCE (SCFSFN(1,LSF),LSF_FILENAME)                                  
                                                                                


C Begin Code ---------------------------------------------
                                                                                
      TYPE *,'<<<<<<<<<< VSYSTAP 01.00 >>>>>>>>>>'                              
      TYPE *,' '

      CALL FASTSET(0,LSYS_ATR,LSYS_COMMON_LENGTH)

      TYPE *,'LSYS_COMMON_LENGTH = ',LSYS_COMMON_LENGTH,' words I4,'
      TYPE *,'Lotto system file must have at least ',
     *        LSYS_COMMON_LENGTH*4,' bytes.'
      TYPE *,'Or ',(LSYS_COMMON_LENGTH*4 + 511 )  /512,' Vax sectors.'
      TYPE *,' '
      TYPE *,' NOTE that LSYS_TAB starts at byte # : ',
     *         (LSYS_COMMON_LENGTH - LSYS_TABMAX - LSYSMAX)*4
      TYPE *,' '

C     CALL OPENW (CLU,'CON:',0,0,0,VSTAT)                                       
                                                                                
                                                                                
C Open Report file                                                              
C ----------------                                                              
C     CALL ROPEN (REP_NAME,REP_LU,VSTAT)                                        
C     IF (VSTAT.NE.0) THEN                                                      
C        TYPE*,'Error when open Report file.',VSTAT                             
C        RETURN                                                                 
C     ENDIF                                                                     
                                                                                
C Get System configuration info                                                 
C -----------------------------                                                 
C      CALL GETSCONF (SCFREC,VSTAT)  !scfrec --> recscf.def                      
C      IF (VSTAT.NE.0) THEN                                                      
C         WRITE (CLU,9100) VSTAT,BELL                                            
C         STOP                                                                   
C      ENDIF                                                                     

      CALL WIMG(CLU,'Enter Lotto System file name : ')
      READ(5,903) LSF_FILENAME                                                  
 903  FORMAT(20A)                                                               

                                                                                
C Open Lotto System file                                                        
C ----------------------                                                        
C      WRITE (CLU,9200) SCFSFN(1,LSF)                                            
      TYPE*,'Lotto System file : ***',LSF_FILENAME,'***'                        

      CALL OPENQW (LSF_LU,SCFSFN(1,LSF),4,0,0,VSTAT)                              
      IF(VSTAT.NE.0) CALL FILERR(SCFSFN(1,LSF),2,VSTAT,0)                                    
                                                                                
C     Read common data from LSF, into common section                            
C     ----------------------------------------------                            
      CALL IOQINIT (FDB,LSF_LU,1*256) !1CC sector of 256 bytes.
      CALL READQIO (FDB,1,LSYS_ATR,LSYS_COMMON_LENGTH*4,VSTAT) !from record 1
      IF (VSTAT.NE.0) CALL FILERR(SCFSFN(1,LSF),2,VSTAT,1)
                                                                                
                                                                                
                                                                                      
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
 200  CONTINUE                                                                  
        TYPE*,' '                                                               
        TYPE*,'0. Exit'                                                         
        TYPE*,'1. Load another table file'                                      
        TYPE*,'2. Save image to file'                                           
        CALL INPNUM ('   Your choice : ',FUN,0,2,VSTAT)                         
                                                                                
        IF (FUN.EQ.1) THEN                                                      
           CALL VLOAD                                                           
           GOTO 200                                                             
        ENDIF                                                                   
                                                                                
        IF (FUN.EQ.2) THEN                                                      
           CALL VSAVE_IM (FDB)                                                  
           GOTO 200                                                             
        ENDIF                                                                   
                                                                                
        IF (FUN.EQ.0) THEN                                                      
           CALL CLOSEFIL (FDB)  !close Lotto System file.                       
           STOP                                                                 
        ENDIF                                                                   
      GOTO 200                                                                  
                                                                                
 9100 FORMAT(1X,' Unable to get System Config info.',                           
     *       1X,' ST = ',I2,A1)                                                 
C9200 FORMAT (1X,' Lotto System file : ***',A20,'***')                          
      END                                                                       
