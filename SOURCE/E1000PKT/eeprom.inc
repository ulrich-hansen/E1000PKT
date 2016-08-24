;

; Some EEPROM word bits masks

; indecate EEPROM address masks
EEPROM_SIZE_64      equ        64
EEPROM_SIZE_256     equ        256

; EEPROM Addres Map
 
EE_IA1              equ        00h           ; IA byte 1
EE_IA2              equ        01h           ; IA byte 2
EE_IA3              equ        02h           ; IA byte 3
EE_IA4              equ        03h           ; IA byte 4
EE_IA5              equ        04h           ; IA byte 5
EE_IA6              equ        05h           ; IA byte 6
EE_COMP             equ        03h           ; Compatability word
EE_PBA12            equ        08h           ; PBA bytes 1=High byte 2=Low byte
EE_PBA34            equ        09h           ; PBA bytes 3=High byte 4=Low byte
EE_INIT1            equ        0ah           ; Init Control 1 word
EE_SUBID            equ        0bh           ; Subsystem ID word
EE_SUBVD            equ        0ch           ; Subsystem Vendor ID word
EE_DEVID            equ        0dh           ; Device ID word
EE_VENID            equ        0eh           ; VEndor ID word
EE_INIT2            equ        0fh           ; Init Control 2 word
EE_SDP_CTRL         equ        20h           ; Software Defined Pins Control word
EE_CCTRL            equ        21h           ; Circuit Control word
EE_D3               equ        22h           ; D3 Power byte
EE_D0               equ        22h           ; D0 Power byte
EE_CH_SUM           equ        3fh           ; Checksum word

; EEPROM Check Sum Value
EE_CHECK_SUM_VALUE  equ        0BABAh

EE_INIT1_LVDID      equ        BIT_0         ; 1=load  PCI Vendor & Device IDs 
                                             ; from the EEPROM
EE_INIT1_SUBID      equ        BIT_1         ; 1=load  PCI Subsystem Vendor &
                                             ; Device IDs from the EEPROM
EE_INIT1_PME        equ        BIT_2         ; Loaded into the PME_Clock bit 
EE_INIT1_PM         equ        BIT_3         ; 1=Full support for Power managment
EE_INIT1_ILOS       equ        BIT_4         ; Default setting for the Loss of
                                             ; signal polarity setting
EE_INIT1_IPS1       equ        BIT_8         ; Default setting for the Power
                                             ; State Output invert bit 1
EE_INIT1_LRST       equ        BIT_9         ; Default setting for link reset
EE_INIT1_FD         equ        BIT_10        ; Default setting for duplex setting
EE_INIT1_FRCSPD     equ        BIT_11        ; Default setting for the Force Speed
                                             ; bit in the Device Control register 
EE_INIT1_IPS0       equ        BIT_12        ; Default setting for the Power
                                             ; State output invert bit 0  
EE_INIT1_64_32      equ        BIT_13        ; 1=Device is mapable in the 32bit space
                                             ; 0=Device is mapable in the 64bit space
EE_INIT1_SIGN_MASK  equ    (BIT_14+BIT_15)   ; Mask for Sinbature bits

; Signature field MUST BE 01. in other cases EEPROM is not valid
EE_SIGN_SHIFT       equ        14
EE_SIGN             equ        0001h


; EEPROM Initialization Control Word 2 bit definitions
EE_INIT2_133        equ        BIT_6         ; 1=Device is 133MHz capable
EE_INIT2_MSI        equ        BIT_7         ; 1=disable MSI in standard PCI mode 
EE_INIT2_FLD        equ        BIT_8         ; 1=Disable Flash logic
                                             ; 0=Enable Flash logic
EE_INIT2_FSMASK     equ    (BIT_9 + BIT_10)  ; Mask for Flash Size Indication bits
EE_INIT2_ANE        equ        BIT_11        ; 1=Auto-Negotiation Enable
                                             ; 0=Auto-Negotiation Disable
EE_INIT2_PC_MASK    equ   (BIT_12 + BIT_13)  ; Desired PAUSE capability for
                                             ; advertised config base page
EE_INIT_ASDE        equ        BIT_14        ; Initial value of the Auto-Speed Detection
                                             ; Enable (ASDE) bit of the Device 
                                             ; Control Register (CTRL)
EE_INIT2_APME       equ        BIT_15        ; Initial value of the Assert PME On 
                                             ; APM Wakeup bit in the Wake Up 
                                             ; Control Register (WUC.APMPME)

; Flash Size Indication values

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   BIT_13   ;   BIT_12  ;  Size  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     0      ;     0     ;   64KB ;
;     0      ;     1     ;  128KB ;
;     1      ;     0     ;  256KB ;
;     1      ;     1     ;  512KB ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FLASH_SIZE_SHIFT    equ        12
FLASH_SIZE_64       equ        0
FLASH_SIZE_128      equ        1
FLASH_SIZE_256      equ        2
FLASH_SIZE_512      equ        3
