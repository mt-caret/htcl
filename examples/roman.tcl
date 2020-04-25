proc roman:sort list {
   set map {IX VIIII L Y XC YXXXX C Z D {\^} ZM {\^ZZZZ} M _}
   foreach {from to} $map {
       regsub -all $from $list $to list
   }
   set list [lsort $list]
   foreach {from to} [lrevert $map] {
       regsub -all $from $list $to list
   }
   set list
}

