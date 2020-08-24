C COPVPF.FOR
C
C V02 09-JUN-2011 FJG Add bunch conversion
C V01 01-JAN-2010 FJG ePassive
C
C Program to copy the VPF files for resizing
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the propercntty of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferrorsed from the cuerrorsody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferrorsed,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /check=nooverflow
        program copvpf
        implicit none
C
        include '(lib$routines)'
	include 'inclib:sysparam.def'
	include 'inclib:sysextrn.def'
C	
        include 'inclib:global.def'
        include 'inclib:concom.def'
        include 'inclib:pascom.def'        
        include 'inclib:valpasfil.def'        
        include 'inclib:hshcom.def'
C
        integer*4     tubsiz
        parameter     (TUBSIZ=I4BUCSIZ*5)
        integer*4     vsize
        parameter     (VSIZE=1000000)
C                    
        integer*4     bigvpf(VSIZE)
        integer*4     newbuf(TUBSIZ)
        integer*4     vpfbuf(TUBSIZ)
C                   
        integer*4     vpflun
        integer*4     newlun
        integer*4     vpfnam(5)
        integer*4     newnam(5)
        character*20  cvpfnam
        character*20  cnewnam
        character*20  cwrknam        
        equivalence   (vpfnam,cvpfnam)
        equivalence   (newnam,cnewnam)
C
        integer*4     errors
        integer*4     sector
        integer*4     tempv1
        integer*4     totrea
        integer*4     totwri
C        
        integer*4     vpfmax
        parameter     (vpfmax = 100)
C        
        character*20  cvpfarr(vpfmax)  
        integer*4     vpfind      
        integer*4     lopind
C
        character*255 vpflok
        character*255 namfil         
C
        integer*1     i1temp(4)
        integer*2     i2temp(2)
        integer*4     i4temp
        equivalence   (i4temp,i2temp,i1temp)        
C
        call copyrite
C===============================================================================
C       Open input validation file
C===============================================================================
        type*,iam()
        call wimg(5,'Enter VPF source file: ')
	read(5,'(A20)') cvpfnam        
        if(cvpfnam(1:2).eq.'E '.or.cvpfnam(1:2).eq.'e '.or.cvpfnam(1:2).eq.'  ') call gstop(GEXIT_OPABORT)
        if(cvpfnam(1:4).eq.'ALL '.or.cvpfnam(1:4).eq.'all ') then
!+++++++Let's vpflok for files. First .DAT and later on .0xx++++++++++++++++++++++        
          vpflok = 'VALX:VPF*.FIL;*'
          vpfind = 0
          do while (lib$find_file (vpflok, namfil, errors))
            vpfind = vpfind + 1
            cvpfarr(vpfind) = 'VALX:' // namfil(index(namfil,']')+1:index(namfil,';')-1)
            if(vpfind.eq.vpfmax) then
              type*,iam(),'WARNING: Maximum number of ',vpfmax,' files to load reached'
              type*,iam()
              exit
            endif
          end do               
        else
          vpfind = 1
          cvpfarr(vpfind) = cvpfnam
        endif
        type*,iam()
C+++++++BUILD FILE NAMES
        do lopind = 1,vpfind
          type*,iam(),'========================================================'
          type*,iam()
          type*,iam(),'>>>>>>>> Processing ',lopind,'/',vpfind,' <<<<<<<<'
          type*,iam()          
          cvpfnam = cvpfarr(lopind)
          cwrknam = cvpfnam
          tempv1 = index(cwrknam,'.')-1
          cnewnam = cwrknam(1:tempv1) // '.NEW'
          cwrknam = cwrknam(1:tempv1) // '.OLD'
C+++++++++TRY TO OPEN INPUT FILE       
          call find_available_lun(vpflun,errors)
          if(errors.ne.0) then
            type*,iam(),'Error getting logical unit ',errors
          else
            call iopen(vpfnam(1),vpflun,vpflen*2,VFSCDC,VFSSER*2-1,errors)
            if(errors.ne.0) then
              type*,iam(),'Error opening VPF source file ',errors
            else
              call find_available_lun(newlun,errors)
              if(errors.ne.0) then
                type*,iam(),'Error getting logical unit ',errors
              else              
                call itubsize(vpflun,tubsiz)
                call getsiz_used(vpflun,sector)
                sector = (sector/10000)+1
                sector = (sector*5000)-15
                call newfil(newlun,cnewnam,sector,0,errors)
                call iopen(newnam(1),newlun,vpflen*2,VFSCDC,VFSSER*2-1,errors)
                if(errors.ne.0) then
                  type*,iam(),'Error opening VPF destination file ',errors    
                else         
                  call iinib(newlun,bigvpf,vsize)
                  call inochks(newlun)
                  call itubsize(newlun,tubsiz)                 
                  totrea = 0
                  totwri = 0
C===============> INI BUCLE DE LECTURA              
                  do while (1)                  
                    call isread(v4buf_pas,vpflun,vpfbuf,errors)
                    if(errors.eq.errend) then
                      call iclose(vpflun,vpfbuf,errors)
                      if(errors.ne.0) call filerr(vpfnam(1),4,errors,0)
                      call iclosb(newlun,bigvpf,newbuf,errors)
                      if(errors.ne.0) call filerr(newnam(1),4,errors,0)
                      type*,iam()
                      type*,iam(),'Total records read:  ',totrea
                      type*,iam(),'Total records write: ',totwri    
                      type*,iam()
                      type*,iam(),'Renaming ',cvpfnam,' >> ',cwrknam
                      call lib$rename_file(cvpfnam,cwrknam)                                      
                      type*,iam(),'Renaming ',cnewnam,' >> ',cvpfnam                     
                      call lib$rename_file(cnewnam,cvpfnam)                                                          
                      type*,iam()
                      exit
                    elseif(errors.ne.0) then
                      call filerr(vpfnam(1),2,errors,0)
                    else
                      totrea = totrea + 1
                      call iwribf(v4buf_pas,newlun,bigvpf,newbuf,errors)
                      if(errors.ne.0) then
                        call filerr(newnam(1),3,errors,0)                    
                      else
                        totwri = totwri + 1
                      endif
                    endif
                  enddo
C===============> FIN BUCLE DE LECTURA                            
                endif
              endif  
            endif 
          endif
        enddo
C===============================================================================
C       END OF PROCESSING
C===============================================================================                  
        call gstop(GEXIT_SUCCESS)
        end
