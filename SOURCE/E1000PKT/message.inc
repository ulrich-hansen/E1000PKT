;
; Basic packet driver messages

driver_msg db "Packet Driver for Intel(R) PRO/1000 Family of Desktop & Server adapters ",DRV_VER,13,10,'$'
pci_msg    db "PCI BIOS is required for this driver",13,10,'$'
copyright  db "Copyright (C) Intel Corporation 2006. All rights reserved.",13,10,'$'
           public skip_str
skip_str   db 13,10,'$'

           public usage_msg
usage_msg  db 13,10,"usage",20h, DRVNAME
           db " [options] <packet_int_no> [nic_no] [speed] [duplex]",13,10,13,10,
           "speed=[10|100|1000]   duplex=[HALF|FULL]",13,10,13,10
opt_msg label byte
db "   -i -- Force driver to report itself as IEEE 802.3 instead of Ethernet II.",13,10
db "   -n -- NetWare conversion.  Converts 802.3 packets into 8137 packets",13,10
db "   -p -- Promiscuous mode disable",13,10
db "   -u -- Uninstall",13,10,'$'

success_msg db 13,10,"Driver succesfully installed",13,10,'$'
error_msg   db "*** Error initializing packet driver interface ***",13,10,'$'

