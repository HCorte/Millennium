$!
$! v01 13-feb-95 wxm initial release
$!
$ if "''p1'" .eqs. "" then p1 = "ACTIVE"
$ if "''p1'" .eqs. "*"      then goto START
$ if "''p1'" .eqs. "ACTIVE" then goto START
$ if "''p1'" .eqs. "LIVE"   then goto START
$ if "''p1'" .eqs. "TOOL"   then goto START
$ if "''p1'" .eqs. "JUNK"   then goto START
$ if "''p1'" .eqs. "UPDATE" then goto START
$ write sys$output "Invalid group parameter."
$ goto END
$ START:
$ if f$search("gxsrc:normlib.lis") .nes. "" then del/nolog gxsrc:normlib.lis;*
$ if "''p1'" .eqs. "*"
$ then
$  domain = "library"
$  cms sho element/format="#E"/out=gxsrc:normlib.lis NRM_*.*
$ else
$  domain = "''p1' group"
$  cms sho element/format="#E"/out=gxsrc:normlib.lis 'p1'_code
$ endif
$ if f$search("gxsrc:normlib.lis") .eqs "" then goto NONE
$ open/read/error=erropin infil gxsrc:normlib.lis
$ if f$search("gxsrc:normlib.com") .nes. "" then del/nolog gxsrc:normlib.com;*
$ open/write/error=erropout outfil gxsrc:normlib.com
$ write/err=errwrout outfil "$set noon"
$ write/err=errwrout outfil "$set ver"
$ cnt = 0
$ name1 = ""
$ NEXT:
$  read/end=endin/err=errrdin infil file
$  if (f$parse("''file'",,,"TYPE") .nes. ".FOR") .AND. -
      (f$parse("''file'",,,"TYPE") .nes. ".C") then goto NEXT
$  name = f$parse("''file'",,,"NAME")
$  if f$extract(0,4,"''name'") .nes. "NRM_" then goto NEXT
$  if name1 .nes. ""
$  then
$   if cnt .lt. 25 then name1 = name1 + ",-"
$   write outfil name1
$   name1 = ""
$  endif
$  if cnt .eq. 0
$  then
$   name1 = "$LIBRARY/CREATE/OBJECT GXSRC:NORMLIB.OLB ''name'"
$   cnt = 1
$  else
$   if cnt .eq. 25
$   then
$     name1 = "$LIBRARY/REPLACE/OBJECT GXSRC:NORMLIB.OLB ''name'"
$     cnt = 1
$   else
$     name1 = "''name'"
$     cnt = cnt+1
$   endif
$  endif
$ goto NEXT
$ ERRRDIN:
$ write sys$output "Error reading temporary file GXSRC:NORMLIB.LIS"
$ close infil
$ close outfil
$ goto END
$ ERRWROUT:
$ write sys$output "Error writing output file GXSRC:NORMLIB.COM"
$ close infil
$ close outfil
$ goto END
$ ERROPIN:
$ write sys$output "Error opening temporary file GXSRC:NORMLIB.LIS"
$ goto END
$ ERROPOUT:
$ write sys$output "Error opening output file GXSRC:NORMLIB.COM"
$ close infil
$ goto END
$ ENDIN:
$ if name1 .nes. ""
$ then
$   if f$extract(f$length("''name1'")-1,1,"''name1'") .eqs. "-"
$   then
$     name1 = f$extract(0,f$length("''name1'")-1,"''name1'")
$   endif
$   write outfil name1
$ endif
$ write/err=errwrout outfil "$set nover"
$ close infil
$ close outfil
$! del/nolog gxsrc:normlib.lis;*
$ if cnt .eq. 0
$ then
$  write sys$output "No NRM_*.FOR files found in ''domain'"
$  del/nolog gxsrc:normlib.com;*
$ endif
$ goto END
$ NONE:
$ write sys$output "No NRM_*.FOR files found in ''domain'"
$ END:
$ exit
