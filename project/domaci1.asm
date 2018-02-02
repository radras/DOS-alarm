VID_SEG		equ		0b800h
START_POS	equ		320
STR_TERM	equ		0
VID_GREEN	equ		02h

KBD		equ 60h
S_SCAN	equ	1Fh
SPACE   equ 39h
org 100h

segment .code

Start:  jmp InstallTsr
PSP:			db 0
FuncID: 		db 0
IDString: 		db 'RADOICA', 0Ah, '$'
OldInt2FO:    	dw 0
OldInt2FS: 		dw 0
OldInt1CO:		dw 0
OldInt1CS:		dw 0
OldInt9O:		dw 0
OldInt9S:		dw 0
OldInt28O:		dw 0
OldInt28S:		dw 0

RTCtimeHour:	db 1, ':'
RTCtimeMinute:	db 1, ':'
RTCtimeSecond:	db 1, ':'

stopsignal: 	db 0
wakeup: 		db 'WAKE UP!$'

esres: 			dw 0
dires: 			dw 0

hf: 			db 0
hs: 			db 0, ':'
mf: 			db 0
ms: 			db 0, ':'
sf: 			db 0
sse: 			db 0, '/'

hfl: 			db 0
hsl: 			db 0, ':'
mfl: 			db 0
msl: 			db 0, ':'
sfl: 			db 0
ssel: 			db 0, '$'
alarmon: 		db 0
cnt10: 			dw 0

errmsg: 		db 'Can not remove TSR!$'


;-----------------------------------------
; Nas 2Fh
; ulaz: 
;		ah - ID broj 
;		
; izlaz:
;		al - 0 ako nije nas hendler
;			 0FFh ako jeste
;-----------------------------------------
OurInt2F:
		cmp     ah, [cs:FuncID]   ;Is this call for us?
		je      .ItsUs
		push word [cs:OldInt2FS]
		push word [cs:OldInt2FO]
		retf
; Now decode the function value in AL:

.ItsUs:	
		cmp     al, 0           ;Verify presence call?
        jne     .TryOtherFunc
        mov     al, 0FFh        ;Return "present" value in AL.
		push 	cs
		pop 	es
		lea 	di, [IDString]
		iret				;Return to caller.

.TryOtherFunc:
		iret

;-----------------------------------------
; Nas 1Ch
;-----------------------------------------
OurInt1C:
		
		mov 	ah, 34h							;  
		int 	21h								;pozivamo prekid da bismo proverili da li je DOS u upotrebi
		mov 	al, [es:bx]
		cmp 	al, 0
		jne 	.izlaz2
		
		cmp 	byte [cs:stopsignal], 1
		jne		.ispiscrno
		push 	ds
		mov 	ax, cs
		mov 	ds, ax
 
		call 	OkayToRmv
		pop		ds
		
		jc 		.print_error
		push 	ds
		mov 	ax, cs
		mov 	ds, ax
		call 	Remove
		pop		ds
		iret
		
		.print_error:
			mov ax, VID_SEG
			mov es, ax
			
			mov bx, word 530
			
			mov si, errmsg
			.petljaerr:
			mov al, byte [cs:si]  
			
			cmp al, '$'
			je .ispiscrno
			
			mov byte [es:bx], al
			inc bx
			mov [es:bx], byte VID_GREEN
			inc bx
			
			inc si
			
			jmp .petljaerr

		
		.ispiscrno:
			mov ax, VID_SEG
			mov es, ax
			
			mov bx, word 71*2
			
			mov si, hf
			.petljacrno:
			mov al, byte [cs:si]  
			
			cmp al, '$'
			je .al
			cmp al, '/'
			je .newlinecrno
			
			mov byte [es:bx], 0 
			inc bx
			mov [es:bx], byte VID_GREEN
			inc bx
			
			inc si
			
			jmp .petljacrno
			jmp .al

			.newlinecrno:
			mov bx, word 302
			inc si
			jmp .petljacrno
		
		.al:
			cmp byte [cs:alarmon], 1
			je .ispisalarm
		
		.l1:
			mov al,10           	;Get RTC register A
			out 70h,al
			in al,71h
			test al,0x80            ;Is update in progress?
			jne .l1             	; yes, wait

			mov al,0            
			out 70h,al
			in al,71h
			mov [cs:RTCtimeSecond],al
						
			mov al,0x02         
			out 70h,al
			in al,71h
			mov [cs:RTCtimeMinute],al

			mov al,0x04         
			out 70h,al
			in al,71h
			mov [cs:RTCtimeHour],al
			
			.satprva:
			mov al, [cs:RTCtimeHour]
			mov bl, [cs:hf]
			shr al, 4
			add al, 30h
			
			sub bl, al
			add bl, 30h
			mov [cs:hfl], bl
			
			.satdruga:
			mov al, [cs:RTCtimeHour]
			mov bl, [cs:hs]
			and al, 0Fh
			add al, 30h
			
			cmp bl, al
			jge .satdrugaveca
			add bl, 10
			sub byte [cs:hfl], 1
			.satdrugaveca:
			sub bl, al
			add bl, 30h
			mov [cs:hsl], bl
			
			.minutprva:
			mov al, [cs:RTCtimeMinute]
			mov bl, [cs:mf]
			shr al, 4
			add al, 30h
			
			cmp bl, al
			jge .minutprvaveca
			add bl, 6
			sub byte [cs:hsl], 1
			.minutprvaveca:
			sub bl, al
			add bl, 30h
			mov [cs:mfl], bl
			
			.minutdruga:
			mov al, [cs:RTCtimeMinute]
			mov bl, [cs:ms]
			and al, 0Fh
			add al, 30h
			
			cmp bl, al
			jge .minutdrugaveca
			add bl, 10
			sub byte [cs:mfl], 1
			.minutdrugaveca:
			sub bl, al
			add bl, 30h
			mov [cs:msl], bl
			
			.sekundprva:
			mov al, [cs:RTCtimeSecond]
			mov bl, [cs:sf]
			shr al, 4
			add al, 30h
			
			cmp bl, al
			jge .sekundprvaveca
			add bl, 6
			sub byte [cs:msl], 1
			.sekundprvaveca:
			sub bl, al
			add bl, 30h
			mov [cs:sfl], bl
			
			.sekunddruga:
			mov al, [cs:RTCtimeSecond]
			mov bl, [cs:sse]
			and al, 0Fh
			add al, 30h
			
			cmp bl, al
			jge .sekunddrugaveca
			add bl, 10
			sub byte [cs:sfl], 1
			.sekunddrugaveca:
			sub bl, al
			add bl, 30h
			mov [cs:ssel], bl
			
			cmp byte [cs:sfl], '0'
			jl .ispravisekundprvu
			jmp .nastavi
			
			.ispravisekundprvu:
			add byte [cs:sfl], 6
			sub byte [cs:msl], 1
			
			cmp byte [cs:msl], '0'
			jl .ispraviminutdrugu
			jmp .nastavi
			
			.ispraviminutdrugu:
			add byte [cs:msl], 10
			sub byte [cs:mfl], 1
			
			cmp byte [cs:msl], '0'
			jl .ispraviminutprvu
			jmp .nastavi
			
			.ispraviminutprvu:
			add byte [cs:mfl], 6
			sub byte [cs:hsl], 1
			
			cmp byte [cs:hsl], '0'
			jl .ispravisatdrugu
			jmp .nastavi
			
			.ispravisatdrugu:
			add byte [cs:hsl], 10
			sub byte [cs:hfl], 1
		
			
		.nastavi:
			
			cmp byte [cs:hfl], '0'
			jne .ispis
			cmp byte [cs:hsl], '0'
			jne .ispis
			cmp byte [cs:mfl], '0'
			jne .ispis
			cmp byte [cs:msl], '0'
			jne .ispis
			cmp byte [cs:sfl], '0'
			jne .ispis
			cmp byte [cs:ssel], '0'
			jne .ispis
			mov byte [cs:alarmon], 1
			jmp .ispisalarm
			
		.ispis:
			mov ax, VID_SEG
			mov es, ax
			
			mov bx, word 71*2
			
			mov si, hf
			.petlja:
			mov al, byte [cs:si]  
			
			cmp al, '$'
			je .izlaz2
			cmp al, '/'
			je .newline
			
			mov [es:bx], al
			inc bx
			mov [es:bx], byte VID_GREEN
			inc bx
			
			inc si
			
			jmp .petlja
			jmp .izlaz2
				
			.newline:
			mov bx, word 302
			inc si
			jmp .petlja
			
		.ispisalarm:
			cmp word [cs:cnt10], 182
			jge .stopalarm
			add word [cs:cnt10], 1
			
			
			mov ax, VID_SEG
			mov es, ax
			
			mov bx, word 302
			
			mov si, wakeup
			.petlja2:
			mov al, byte [cs:si]  
			
			cmp al, '$'
			je .izlaz2

			
			mov [es:bx], al
			inc bx
			mov [es:bx], byte 82h
			inc bx
			
			inc si
			
			jmp .petlja2
				
			.stopalarm:
			mov byte [cs:stopsignal], 1
			.izlaz2:
			
			
			push word [cs:OldInt1CS]
			push word [cs:OldInt1CO]
			retf
			
;-----------------------------------------
; Nas 9h
;	
;	reaguje na pritisak s ili space
;-----------------------------------------

OurInt09:
			pusha
			
			in 		al, KBD			
			cmp    	al, S_SCAN
			je     	.snooze                   
			cmp		al, SPACE              
			je     	.snooze
			jmp 	.izlaz					
		
		.snooze:
			
			cmp byte [cs:alarmon], 1
			jne .izlaz
			cmp word [cs:cnt10], 182
			jge	.izlaz
			mov byte [cs:alarmon], 0
			mov word [cs:cnt10], 0
			add byte [cs:ms], 1
			
			cmp byte [cs:ms], '9'
			jle .izlaz
			mov byte [cs:ms], '0'
			add byte [cs:mf], 1
			
			cmp byte [cs:mf], '5'
			jle .izlaz
			mov byte [cs:mf], '0'
			add byte [cs:hs], 1
			
			cmp byte [cs:hs], '9'
			jle .test
			mov byte [cs:mf], '0'
			add byte [cs:hf], 1
			
			;test for 2x:xx:xx
			.test:
			cmp byte [cs:hf], '2'
			jl .izlaz
			cmp byte [cs:hs], '4'
			jl .izlaz
			mov byte [cs:hf], '0'
			add byte [cs:hs], '0'
			
		.izlaz:
			popa


			push word [cs:OldInt9S]
			push word [cs:OldInt9O]
			retf
	
;-----------------------------------------
; Nas 28h za resenje reentrancy problema
;-----------------------------------------
OurInt28:	
				pusha
				
				cmp 	byte [cs:stopsignal], 1
				jne		.ispiscrno
				push 	ds
				mov 	ax, cs
				mov 	ds, ax
		 
				call 	OkayToRmv
				pop		ds
				
				jc 		.print_error
				push 	ds
				mov 	ax, cs
				mov 	ds, ax
				call 	Remove
				pop		ds
				iret
		
			.print_error:
				mov ax, VID_SEG
				mov es, ax
				
				mov bx, word 530
				
				mov si, errmsg
				.petljaerr:
				mov al, byte [cs:si]  
				
				cmp al, '$'
				je .ispiscrno
				
				mov byte [es:bx], al
				inc bx
				mov [es:bx], byte VID_GREEN
				inc bx
				
				inc si
				
				jmp .petljaerr

			
			.ispiscrno:
				mov ax, VID_SEG
				mov es, ax
				
				mov bx, word 71*2
				
				mov si, hf
				.petljacrno:
				mov al, byte [cs:si]  
				
				cmp al, '$'
				je .al
				cmp al, '/'
				je .newlinecrno
				
				mov byte [es:bx], 0 ;crno
				inc bx
				mov [es:bx], byte VID_GREEN
				inc bx
				
				inc si
				
				jmp .petljacrno
				jmp .al

				.newlinecrno:
				mov bx, word 302
				inc si
				jmp .petljacrno
			
			.al:
				cmp byte [cs:alarmon], 1
				je .ispisalarm
			
			.l1:
				mov 	al,	10           	;Get RTC register A
				out 	70h, al
				in 		al,71h
				test 	al,0x80            ;Is update in progress?
				jne 	.l1             	; yes, wait

				mov 	al,0            ;Get seconds (00 to 59)
				out 	70h,al
				in 		al,71h
				mov 	[cs:RTCtimeSecond],al
							
				mov 	al,0x02         ;Get minutes (00 to 59)
				out 	70h,al
				in 		al,71h
				mov 	[cs:RTCtimeMinute],al

				mov 	al,0x04         ;Get hours (see notes)
				out 	70h,al
				in 		al,71h
				mov 	[cs:RTCtimeHour],al
				
				.satprva:
				mov 	al, [cs:RTCtimeHour]
				mov 	bl, [cs:hf]
				shr 	al, 4
				add 	al, 30h
				
				sub 	bl, al
				add 	bl, 30h
				mov 	[cs:hfl], bl
				
				.satdruga:
				mov 	al, [cs:RTCtimeHour]
				mov 	bl, [cs:hs]
				and 	al, 0Fh
				add 	al, 30h
				
				cmp 	bl, al
				jge 	.satdrugaveca
				add 	bl, 10
				sub 	byte [cs:hfl], 1
				.satdrugaveca:
				sub 	bl, al
				add 	bl, 30h
				mov 	[cs:hsl], bl
				
				.minutprva:
				mov		al, [cs:RTCtimeMinute]
				mov 	bl, [cs:mf]
				shr 	al, 4
				add 	al, 30h
				
				cmp 	bl, al
				jge 	.minutprvaveca
				add 	bl, 6
				sub 	byte [cs:hsl], 1
				.minutprvaveca:
				sub 	bl, al
				add 	bl, 30h
				mov 	[cs:mfl], bl
				
				.minutdruga:
				mov 	al, [cs:RTCtimeMinute]
				mov 	bl, [cs:ms]
				and 	al, 0Fh
				add		al, 30h
				
				cmp		bl, al
				jge 	.minutdrugaveca
				add 	bl, 10
				sub 	byte [cs:mfl], 1
				.minutdrugaveca:
				sub 	bl, al
				add 	bl, 30h
				mov 	[cs:msl], bl
				
				.sekundprva:
				mov 	al, [cs:RTCtimeSecond]
				mov 	bl, [cs:sf]
				shr 	al, 4
				add 	al, 30h
				
				cmp 	bl, al
				jge .sekundprvaveca
				add 	bl, 6
				sub 	byte [cs:msl], 1
				.sekundprvaveca:
				sub 	bl, al
				add 	bl, 30h
				mov 	[cs:sfl], bl
				
				.sekunddruga:
				mov 	al, [cs:RTCtimeSecond]
				mov 	bl, [cs:sse]
				and 	al, 0Fh
				add 	al, 30h
				
				cmp 	bl, al
				jge .sekunddrugaveca
				add 	bl, 10
				sub 	byte [cs:sfl], 1
				.sekunddrugaveca:
				sub 	bl, al
				add 	bl, 30h
				mov 	[cs:ssel], bl
				
				cmp 	byte [cs:sfl], '0'
				jl 		.ispravisekundprvu
				jmp 	.nastavi
				
				.ispravisekundprvu:
				add 	byte [cs:sfl], 6
				sub 	byte [cs:msl], 1
				
				cmp 	byte [cs:msl], '0'
				jl 		.ispraviminutdrugu
				jmp 	.nastavi
				
				.ispraviminutdrugu:
				add 	byte [cs:msl], 10
				sub 	byte [cs:mfl], 1
				
				cmp 	byte [cs:msl], '0'
				jl 		.ispraviminutprvu
				jmp 	.nastavi
				
				.ispraviminutprvu:
				add 	byte [cs:mfl], 6
				sub 	byte [cs:hsl], 1
				
				cmp 	byte [cs:hsl], '0'
				jl 		.ispravisatdrugu
				jmp 	.nastavi
				
				.ispravisatdrugu:
				add 	byte [cs:hsl], 10
				sub 	byte [cs:hfl], 1
			
				
			.nastavi:
				
				cmp 	byte [cs:hfl], '0'
				jne 	.ispis
				cmp 	byte [cs:hsl], '0'
				jne 	.ispis
				cmp 	byte [cs:mfl], '0'
				jne 	.ispis
				cmp 	byte [cs:msl], '0'
				jne 	.ispis
				cmp 	byte [cs:sfl], '0'
				jne 	.ispis
				cmp 	byte [cs:ssel], '0'
				jne 	.ispis
				mov 	byte [cs:alarmon], 1
				jmp 	.ispisalarm
				
			.ispis:
				mov 	ax, VID_SEG
				mov 	es, ax
				
				mov 	bx, word 71*2
				
				mov 	si, hf
				.petlja:
				mov 	al, byte [cs:si]  
				
				cmp 	al, '$'
				je 		.izlaz2
				cmp 	al, '/'
				je 		.newline
				
				mov 	[es:bx], al
				inc 	bx
				mov 	[es:bx], byte VID_GREEN
				inc		 bx
				
				inc 	si
				
				jmp 	.petlja
				jmp 	.izlaz2
					
				.newline:
				mov 	bx, word 302
				inc 	si
				jmp 	.petlja
				
			.ispisalarm:
				cmp 	word [cs:cnt10], 182
				jge 	.stopalarm
				add 	word [cs:cnt10], 1
				
				
				mov 	ax, VID_SEG
				mov 	es, ax
				
				mov 	bx, word 302
				
				mov 	si, wakeup
				.petlja2:
				mov 	al, byte [cs:si]  
				
				cmp 	al, '$'
				je 		.izlaz2

				
				mov 	[es:bx], al
				inc 	bx
				mov 	[es:bx], byte 82h
				inc 	bx
				
				inc		si
				
				jmp 	.petlja2
					
				.stopalarm:
				mov 	byte [cs:stopsignal], 1
				.izlaz2:
				
				
				push 	word [cs:OldInt1CS]
				push 	word [cs:OldInt1CO]
				retf
					
				popa     
				push 	word [cs:OldInt28S]
				push 	word [cs:OldInt28O]
				retf
			
;-----------------------------------------
; Proverava da li mozemo da uklonimo nas
; TSR iz memorije
;
; OkayToRmv-    This routine returns the carry flag set if it is okay to
;               remove the current TSR from memory. It checks the interrupt
;               vectors for int 2F, int 9, int 28 and int 1C to make sure they
;               are still pointing at our local routines.
;               This code assumes DS is pointing at the resident code's
;               data segment.
;-----------------------------------------
OkayToRmv:
                
				mov 	ds, [cs:esres]
				push    es
                mov     ax, 0                   ;Point ES at interrupt vector
                mov     es, ax                  ; table.
                mov     ax, word [ds:OldInt2FO]
                cmp     ax, [es:2fh*4]
                jne     .CantRemove
                mov     ax, word [ds:OldInt2FS]
                cmp     ax, [es:2Fh*4 + 2]
                jne     .CantRemove

                mov     ax, word [ds:OldInt9O]
                cmp     ax, [es:9h*4]
                jne     .CantRemove
                mov     ax, word [ds:OldInt9S]
                cmp     ax, [es:9h*4 + 2]
                jne     .CantRemove

				mov     ax, word [ds:OldInt1CO]
                cmp     ax, [es:1Ch*4]
                jne     .CantRemove
                mov     ax, word [ds:OldInt1CS]
                cmp     ax, [es:1Ch*4 + 2]
                jne     .CantRemove
				
				mov     ax, word [ds:OldInt28O]
                cmp     ax, [es:28h*4]
                jne     .CantRemove
                mov     ax, word [ds:OldInt28S]
                cmp     ax, [es:28h*4 + 2]
                jne     .CantRemove
				
; We can safely remove this TSR from memory.
				
                stc
                pop     es
                ret

; Someone else is in the way, we cannot remove this TSR.

.CantRemove:    clc
                pop     es
                ret

				
;-----------------------------------------
; Uklanjanje TSRa
;-----------------------------------------
Remove:			
			mov		al, [cs:stopsignal]
			cmp		al, 1
			je		.fromtsr
			push 	word [cs:esres]
			pop		es
			push 	word [cs:dires]
			pop 	di
			lea 	bx, [es:di]
			add 	bx, 9
			jmp 	.fromprog
			.fromtsr:
			push	cs
			pop		es
			lea 	di, [IDString]
			lea		bx, [es:di]
			add 	bx, 9
			
			.fromprog:
			
			mov		ax, [es:bx]
			mov 	word [cs:OldInt2FO], ax
			inc		bx
			inc 	bx
			mov		ax, [es:bx]
			mov 	word [cs:OldInt2FS], ax
			inc		bx
			inc 	bx
			mov		ax, [es:bx]
			mov 	word [cs:OldInt1CO], ax
			inc		bx
			inc 	bx
			mov		ax, [es:bx]
			mov 	word [cs:OldInt1CS], ax
			inc		bx
			inc 	bx
			mov		ax, [es:bx]
			mov 	word [cs:OldInt9O], ax
			inc		bx
			inc 	bx
			mov		ax, [es:bx]
			mov 	word [cs:OldInt9S], ax
			inc		bx
			inc 	bx
			mov		ax, [es:bx]
			mov 	word [cs:OldInt28O], ax
			inc		bx
			inc 	bx
			mov		ax, [es:bx]
			mov 	word [cs:OldInt28S], ax
			
			
			cli
				
			xor ax, ax
			mov es, ax
			mov ax, [cs:OldInt1CS]
			mov [es:1Ch*4+2], ax
			mov dx, [OldInt1CO]
			mov [es:1Ch*4], dx
				
			xor ax, ax
			mov es, ax
			mov ax, [cs:OldInt28S]
			mov [es:28h*4+2], ax
			mov dx, [cs:OldInt28O]
			mov [es:28h*4], dx
				
			xor ax, ax
			mov es, ax
			mov ax, [cs:OldInt2FS]
			mov [es:2Fh*4+2], ax
			mov dx, [cs:OldInt2FO]
			mov [es:2Fh*4], dx
				
			xor ax, ax
			mov es, ax
			mov ax, [cs:OldInt9S]
			mov [es:09h*4+2], ax
			mov dx, [cs:OldInt9O]
			mov [es:09h*4], dx
				
			sti
				
			mov es, word [cs:2ch]
			mov ah, 49h
			int 21h
			push cs
			pop es
			mov ah, 49h
			int 21h
			
			ret
				
;-----------------------------------------
; Instalacija TSRa 
;-----------------------------------------

InstallTsr:		
				call 	check_for_stop


				mov     byte [cs:FuncID], 0       ;Initialize FuncID to zero.
                mov     cx, 0FFh
.SearchLoop:    mov     ah, cl
                push    cx
                mov     al, 0
				int     2Fh
                pop     cx
                cmp     al, 0
                je      .TryNext
				
				mov 	byte bl, [es:di] 
				
				cmp 	bl, 'R'					;proverio sam samo prvi karakter
												;dovoljno je sto sam kucao poredjenje
				je      .AlreadyThere			;za start i stop xD
				loop    .SearchLoop      
                jmp     .NotInstalled

.TryNext:       mov     [cs:FuncID], cl      	;Save possible function ID if this
                loop    .SearchLoop      		; identifier is not in use.
                jmp     .NotInstalled

.AlreadyThere:  
				mov 	word [cs:esres], es
				mov 	word [cs:dires], di
				cmp 	byte [cs:stop_flag], 1
				je		.Ending
				mov  	dx, alreadyInstalled
				mov  	ah,09
				int  	21h
				jmp 	.Ending
				
.NotInstalled:  
				mov 	byte [cs:not_instaled_flag], 1
				cmp     byte [cs:FuncID], 0      ;If there are no available IDs, this
                jne     .GoodID         		 ; will still contain zero.
				mov  	dx, toomanytsrs	
				mov  	ah,09
				int 	 21h
				ret

.GoodID:		
			cmp 	byte [cs:stop_flag], 1
			je		.Ending	
			
			cmp  	byte [is_everything_ok], 0
			je 	 	.Ending
			
			push 	cs                     
			pop  	es

			mov  	dx, notInstalled
			mov  	ah,09
			int  	21h
			
			mov  	dx, installing
			mov  	ah,09
			int  	21h
			
			mov  	es, [002Ch]              
			mov  	ah, 49h                
			int  	21h                   
			
			;int 2Fh
			
			; Izmena prekida
			cli
			xor 	ax, ax
			mov 	es, ax
			mov 	bx, [es:2Fh*4]
			mov 	[OldInt2FO], bx 
			mov 	bx, [es:2Fh*4+2]
			mov 	[OldInt2FS], bx

			mov 	dx, OurInt2F
			mov 	[es:2Fh*4], dx
			mov 	ax, cs
			mov 	[es:2Fh*4+2], ax
				
			xor 	ax, ax
			mov 	es, ax
			mov 	bx, [es:09h*4]
			mov 	[OldInt9O], bx 
			mov 	bx, [es:09h*4+2]
			mov 	[OldInt9S], bx

			mov 	dx, OurInt09
			mov 	[es:09h*4], dx
			mov 	ax, cs
			mov 	[es:09h*4+2], ax
				
			xor 	ax, ax
			mov 	es, ax
			mov 	bx, [es:1Ch*4]
			mov 	[OldInt1CO], bx 
			mov 	bx, [es:1Ch*4+2]
			mov 	[OldInt1CS], bx

			mov 	dx, OurInt1C
			mov 	[es:1Ch*4], dx
			mov 	ax, cs
			mov 	[es:1Ch*4+2], ax
			push 	ds		
			pop 	gs		
			
			xor 	ax, ax
			mov 	es, ax
			mov 	bx, [es:28h*4]
			mov 	[OldInt28O], bx 
			mov 	bx, [es:28h*4+2]
			mov 	[OldInt28S], bx

			mov 	dx, OurInt28
			mov 	[es:28h*4], dx
			mov 	ax, cs
			mov 	[es:28h*4+2], ax
						
			sti         
			
			
			mov  	dx, InstallTsr       
			sub  	dx, Start         
			shr  	dx, 04                    
			;add  dx, 17                  
			mov  	ax, 3100h                
			int  	21h                  
		
			ret
.Ending:
			cmp 	byte [not_instaled_flag], 1
			je		.realend
			cmp 	byte [stop_flag], 1
			jne 	.realend
			
			push	ds
			call 	OkayToRmv
			pop 	ds
			
			jc 		.print_error
			
			call 	Remove
			jmp 	.realend
		
		.print_error:
			
			
			mov  	dx, errmsg
			mov  	ah, 09
			int  	21h
			
				
.realend:
			
			mov 	ax, 4C00h
			int 	21h
		
			ret
	
;-----------------------------------
; Proverava pravilan unos
;-----------------------------------
	
check_for_stop:

			cld
			mov     cx, 0080h                   
			mov     di, 81h                     
			mov     al, ' '                      
	repe    scasb                               

			dec     di                          
			mov     si, di                      
			mov     al, 0dh                     
	repne   scasb                                
			mov 	byte [di-1], 0                    		
			
			mov 	dx, stop_alarm
			mov 	bx, si

	.stop_loop:
			mov 	word ax, [es:bx]
			cmp 	ax, 0
			je 		.flag_stop  
			push 	bx
			mov		bx, dx
			mov 	word cx, [es:bx]
			inc 	dx
			pop 	bx
			inc 	bx
			cmp 	ax, cx
			je 		.stop_loop
			jmp 	check_for_start
	.flag_stop:
			mov 	byte [stop_flag], 1
			jmp 	kraj		

check_for_start:
		
			cld
			mov     cx, 0080h                   
			mov     di, 81h                     
			mov     al, ' '                      
	repe    scasb                               

			dec     di                          
			mov     si, di                      
			mov     al, ' '                     
	repne   scasb                                
			mov byte [di-1], 0

			mov 	bx, start_alarm
			mov 	dx, si

	.start_loop:
			mov 	word ax, [es:bx]
			cmp 	ax, 0
			je 		.flag_start  
			push 	bx
			mov 	bx, dx
			mov 	word cx, [es:bx]
			inc 	dx
			pop 	bx
			inc 	bx
			cmp 	ax, cx
			je 		.start_loop
			jmp 	warning
	.flag_start:
			jmp 	read_time
			
warning:
			mov  	ah, 9       					
			mov  	dx, warning_message  			
			int  	21h         					
			jmp 	kraj

	read_time:
			
			cld
			mov     cx, 0080h                   
			mov     di, 81h                     
			mov     al, ' '                      
	repe    scasb                               

			dec     di                          
			mov     si, di                      
			mov     al, 0                     	; trazimo prethodno postavljenu nulu
	repne   scasb             
			
			
rh:		;hours
			mov 	al, [di]
			mov 	[hf], al
			cmp 	byte [hf], '0'
			jl 		warning_time
			cmp 	byte [hf], '2'
			jg 		warning_time
			
			inc 	di
			
			mov 	al, [di]
			mov 	[hs], al
			cmp 	byte [hs], '0'
			jl 		warning_time
			cmp 	byte [hf], '2'
			je 		.ftwo
			jmp 	.ntwo
		.ftwo:
			cmp 	byte [hs], '3'
			jg 		warning_time
			cmp 	byte [hs], '0'
			jl 		warning_time
		.ntwo:
			cmp 	byte [hs], '9'
			jg 		warning_time
			cmp 	byte [hs], '0'
			jl 		warning_time

			inc 	di
			
			mov 	al, [di]
			cmp 	al, ':'
			jne 	warning_time
			
			inc 	di
			
			; minutes
			mov 	al, [di]
			mov 	[mf], al
			cmp 	byte [mf], '0'
			jl 		warning_time
			cmp 	byte [mf], '5'
			jg 		warning_time
			
			inc 	di
			
			mov 	al, [di]
			mov 	[ms], al
			cmp 	byte [ms], '0'
			jl 		warning_time
			cmp 	byte [ms], '9'
			jg 		warning_time
			
			inc 	di
			;seconds
			
			mov 	al, [di]
			cmp 	al, ':'
			jne 	warning_time
			
			inc 	di
			
			mov 	al, [di]
			mov 	[sf], al
			cmp 	byte [sf], '0'
			jl 		warning_time
			cmp 	byte [sf], '5'
			jg 		warning_time
			
			inc 	di
			
			mov 	al, [di]
			mov 	[sse], al
			cmp 	byte [sse], '0'
			jl 		warning_time
			cmp 	byte [sse], '9'
			jg 		warning_time
			
			inc 	di
			
			mov 	al, [di]
			cmp		al, 0
			jne 	warning_time
						
			mov 	byte [is_everything_ok], 1
			
			jmp 	kraj

warning_time:
			mov  	ah, 9       					
			mov  	dx, warning_message_time  			
			int  	21h         					
			jmp 	kraj
		
kraj:
			ret

			
alreadyInstalled: 	db 'Already installed!',  0Ah, '$'
notInstalled: 		db 'Not installed.',  0Ah, '$'
installing: 		db 'Installing TSR...',  0Ah, '$'
toomanytsrs: 		db 'Too many TSRs already in memory', 0Ah,'$'
dbgmsg: 			db 'DEBUG', 0Ah, '$'
stop_flag: 			db 0
not_instaled_flag: 	db 0


segment .data
start_alarm:		dw '-start', 0
stop_alarm: 		dw '-stop', 0
warning_message: 	db 'Wrong argument format!',  0Ah, '$'
warning_message_time: db 'Wrong time format!', 0Ah, '$'
is_everything_ok: 	db 0