;
; 
; PCI BIOS calls and supported DEVICE ID definitions

COMMAND_REGISTER        equ     004h
PCIX_COMMAND_REG        equ     0E6h
PCIX_STATUS_REG_LW      equ     0E8h
PCIX_STATUS_REG_HW      equ     0EAh

PCIX_CMD_MMRBC_MASK     equ     00Ch
PCIX_CMD_MMRBC_SHIFT    equ     002h
PCIX_ST_DMMRBC_MASK     equ     060h
PCIX_ST_DMMRBC_SHIFT    equ     005h

CMD_IO_SPACE            equ     001h
CMD_MEMORY_SPACE        equ     002h
CMD_BUS_MASTER          equ     004h
CMD_MWI_ENABLE          equ     010h


PCI_BIOS_INTERRUPT      equ     01Ah
PCI_FUNCTION_ID         equ     0B1h

;- PCI Bios function list
PCI_BIOS_PRESENT        equ     001h
FIND_PCI_DEVICE         equ     002h
FIND_PCI_CLASS_CODE     equ     003h
GENERATE_SPECIAL_CYCLE  equ     006h
READ_PCI_CONFIG_BYTE    equ     008h
READ_PCI_CONFIG_WORD    equ     009h
READ_PCI_CONFIG_DWORD   equ     00Ah
WRITE_PCI_CONFIG_BYTE   equ     00Bh
WRITE_PCI_CONFIG_WORD   equ     00Ch
WRITE_PCI_CONFIG_DWORD  equ     00Dh
GET_IRQ_ROUTING_OPTIONS equ     00Eh
SET_PCI_IRQ             equ     00Fh

;- PCI Bios return codes
PCI_SUCCESSFULL         equ     000h
PCI_FUNC_NOT_SUPPORTED  equ     081h
PCI_BAD_VENDOR_ID       equ     083h
PCI_DEVICE_NOT_FOUND    equ     086h
PCI_BAD_REGISTER_NUMBER equ     087h
PCI_SET_FAILED          equ     088h
PCI_BUFFER_TOO_SMALL    equ     089h


;- Supported chip list
VENDOR_ID               equ     8086h
i82544EI_COPPER         equ     1008h
i82544EI_FIBER          equ     1009h
i82544GC_COPPER         equ     100Ch
i82544GC_LOM            equ     100Dh
i82540EM                equ     100Eh
i82540EM_LOM            equ     1015h
i82540EP_LOM1           equ     1016h
i82540EP                equ     1017h
i82540EP_LOM2           equ     101Eh
i82545EM_COPPER         equ     100Fh
i82545EM_FIBER          equ     1011h
i82541EI                equ     1013h
i82541ER                equ     1078h
i82541EI_MOBILE         equ     1018h
i82541PI                equ     107Ch
i82547EI                equ     1019h
i82547EI_MOBILE         equ     101Ah
