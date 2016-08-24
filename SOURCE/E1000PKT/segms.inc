;

; Segments definitions and ordering

_TEXT   segment para public USE16 'CODE'
_TEXT   ends

_INIT   segment para public USE16 'CODE'
_INIT   ends

CGROUP  group   _TEXT, _INIT

        ASSUME   CS:CGROUP, DS:CGROUP, ES:NOTHING, SS:NOTHING
