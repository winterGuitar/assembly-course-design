assume cs:setuppg

setuppg segment
start:
	mov ax,initsys
	mov es,ax
	mov bx,0
	mov al,1
	mov ch,0
	mov cl,1
	mov dh,0
	mov dl,0
	mov ah,3
	int 13h

	mov ax,main
	mov es,ax
	mov bx,0
	mov al,3
	mov cl,2
	mov ah,3
	int 13h

	mov ax,4c00h
	int 21h
setuppg ends

initsys segment
assume cs:initsys
	mov ax,2000h
	mov es,ax
	mov bx,0

	mov al,3
	mov ch,0
	mov cl,2
	mov dl,0
	mov dh,0

	mov ah,2
	int 13h

	mov ax,2000h
	push ax
	mov ax,0h
	push ax

	retf
initsys ends

data segment
db 128 dup(0)
data ends

main segment
assume cs:main

	jmp short init
	
	opt_str1 db '1) reset pc','$'
	opt_str2 db '2) start system','$'
	opt_str3 db '3) clock','$'
	opt_str4 db '4) set clock','$'
	opt_str_tbl dw offset opt_str1, offset opt_str2, offset opt_str3, offset opt_str4
	opt_func_tbl dw reset_pc, start_sys, sys_clock, setclock
	
init:
	mov ax,data
	mov ss,ax
	mov sp,128
	call doint9

main_start:
	call clearbuf
	call cls
	call show_opts
	call getinput
	jmp main_start


	mov ax,4c00h
	int 21h

doint9:
	push cs
	pop ds
	mov ax,0
	mov es,ax
	
	mov si,offset int9
	mov di,204h
	mov cx,offset int9end - offset int9
	cld
	rep movsb
	
	push es:[9*4]
	pop es:[200h]
	push es:[9*4+2]
	pop es:[202h]
	
	cli
	mov word ptr es:[9*4],204h
	mov word ptr es:[9*4+2],0
	sti
	
	ret
int9:
	push ax
	push bx
	push cx
	push es
	
	in al,60h
	pushf
	call dword ptr cs:[200h]
	
	cmp al,01h
	je return_main
	cmp al,3bh
	je changecolor
	
	mov ah,1
	int 16h
	jmp short return
	
changecolor:
	mov ax,0b800h
	mov es,ax
	mov di,160*12+30*2+1
	mov cx,17
color_lps:
	mov al,es:[di]
	mov ah,al
	inc al
	and ah,11111000b
	and al,00000111b
	or ah,al
	mov es:[di],ah
	add di,2
	loop color_lps
	jmp short return

return_main:
	mov sp,128
	jmp far ptr main_start
return:
	pop es
	pop cx
	pop bx
	pop ax
	iret
int9end:
	nop


show_opts:
	mov cx,4
	mov bx,0
	mov ax,cs
	mov ds,ax
	mov dh,10
	mov dl,32
show_lps:
	mov ah,2
	mov bh,0
	int 10h

	push dx

	mov dx,opt_str_tbl[bx]
	mov ah,9
	int 21h
	add bx,2

	pop dx
	inc dh
	loop show_lps
	ret


	;清屏
cls:
	mov ax,0b800h
	mov ds,ax
	mov bx,0
	mov cx,24*80*2
cls_s:
	mov byte ptr ds:[bx],0
	add bx,2
	loop cls_s
	mov bx,1
resetcol:
	mov byte ptr ds:[bx],07h
	add bx,2
	loop resetcol
	ret

getinput:
	mov ah,0
	int 16h
	cmp al,'1'
	jb input_ret
	cmp al,'4'
	ja input_ret
	sub al,31h
	mov bx,0
	mov bl,al
	add bx,bx
	call word ptr opt_func_tbl[bx]
input_ret:
	ret

reset_pc:
	mov ax,0ffffh
	push ax
	mov ax,0
	push ax
	retf

start_sys:
	call cls
	mov ax,0
	mov es,ax
	mov bx,7c00h
	mov al,1
	mov ch,0
	mov cl,1
	mov dh,0
	mov dl,80h
	mov ah,2
	int 13h

	mov ax,0
	push ax
	mov ax,7c00h
	push ax
	retf


sys_clock:
	call cls
	jmp short start_sys_clock
	format db '00/00/00 00:00:00','$'
	pos db 9,8,7,4,2,0
start_sys_clock:
	mov di,0
	mov ax,cs
	mov ds,ax
	mov bx,0
	mov cx,6
read_clock_lps:
	mov al,pos[bx]
	out 70h,al
	in al,71h
	mov ah,al
	push cx
	mov cl,4
	shr ah,cl
	pop cx
	and al,00001111b
	add al,30h
	add ah,30h
	mov byte ptr format[di],ah
	mov byte ptr format[di+1],al
	add di,3
	inc bx
	loop read_clock_lps
	
	mov ah,2
	mov bh,0
	mov dh,12
	mov dl,30
	int 10h

	push dx

	mov dx,offset format
	mov ah,9
	int 21h
	pop dx

	jmp start_sys_clock


setclock:
	call clearbuf
	call cls
	jmp short start_setclock
	timeformat db '00/00/00 00:00:00','$'
	bitpos db 9,8,7,4,2,0
start_setclock:
	push ds
	push di
	push cx
	push bx
	mov ax,cs
	mov ds,ax
	mov dx,offset timeformat
	mov si,dx
	call getstr
	mov cx,6
	mov bx,0
	mov di,0
set_lps:
	mov ah,ds:[timeformat+bx]
	cmp ah,0
	je finishrets
	sub ah,30h
	mov al,ds:[timeformat+bx+1]
	sub al,30h
	push cx
	mov cl,4
	shl ah,cl
	pop cx
	and ah,11110000b
	or ah,al
	mov al,ds:[bitpos][di]
	out 70h,al
	mov al,ah
	out 71h,al
	inc di
	add bx,3
	loop set_lps
finishrets:
	pop bx
	pop cx
	pop di
	pop ds
	ret
	

getstr:
	push ax
getstrs:
	mov ah,0
	int 16h
	cmp al,20h
	jb nochar
	mov ah,0
	call charstack
	mov ah,2
	call charstack
	jmp getstrs
nochar:
	cmp ah,0eh
	je backspace
	cmp ah,1ch
	je enter
	jmp getstrs
backspace:
	mov ah,1
	call charstack
	mov ah,2
	call charstack
	jmp getstrs
enter:
	mov al,0
	mov ah,0
	call charstack
	mov ah,2
	call charstack
	pop ax
	ret

charstack:
	jmp short charstart
	table dw charpush,charpop,charshow
	top dw 0
charstart:
	push bx
	push dx
	push di
	push es

	cmp ah,2
	ja sret
	mov bl,ah
	mov bh,0
	add bx,bx
	jmp word ptr table[bx]
charpush:
	mov bx,top
	mov [si][bx],al
	inc top
	jmp sret
charpop:
	cmp top,0
	je sret
	dec top
	mov bx,top
	mov al,[si][bx]
	jmp sret
charshow:
	mov bx,0b800h
	mov es,bx
	mov al,160
	mov ah,0
	mul dh
	mov di,ax
	add dl,dl
	mov dh,0
	add di,dx
	mov bx,0
charshows:
	cmp bx,top
	jne noempty
	mov byte ptr es:[di],' '
	jmp sret
noempty:
	mov al,[si][bx]
	mov es:[di],al
	mov byte ptr es:[di+2],' '
	inc bx
	add di,2
	jmp charshows
sret:
	pop es
	pop di
	pop dx
	pop bx
	ret

clearbuf:
	mov ah,1
	int 16h
	jz endclear
	mov ah,0
	int 16h
	jmp clearbuf
endclear:
	ret

main ends
end start