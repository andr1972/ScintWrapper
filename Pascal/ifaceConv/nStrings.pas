//GNU LESSER GENERAL PUBLIC LICENSE - See lgpl.txt
//this file is part neoPack package
unit nStrings;

{$ifdef FPC}
{$asmmode intel}
{$endif}

interface

type
{$ifndef UNICODE}UnicodeString = WideString;{$endif}
DWORD = LongWord;

function ToUpper(const S: AnsiString): AnsiString;
function ToLower(const S: AnsiString): AnsiString;
function AnsiToLower(const S: AnsiString): AnsiString;
function AnsiToUpper(const S: AnsiString): AnsiString;
function WideToLower(const S: UnicodeString): UnicodeString;
function WideToUpper(const S: UnicodeString): UnicodeString;
function AnsiCompare(const S1,S2: AnsiString; flagsIgnore:DWORD=0): integer;
function AnsiCompareIgnore(const S1,S2: AnsiString): integer;
function WideCompare(const S1,S2: UnicodeString; flagsIgnore:DWORD=0): integer;
function WideCompareIgnore(const S1,S2: UnicodeString): integer;
{$if Defined(MSWINDOWS)}
function WideCompareL(const S1,S2: UnicodeString; MaxLen: integer; flagsIgnore:DWORD=0): integer;
function WideCompareLIgnore(const S1,S2: UnicodeString; MaxLen: integer): integer;
{$ifend}
procedure iStrConvert(StrIn,StrOut,Tab: PAnsiChar); overload;
function iStrConvert(StrIn:Ansistring; Tab:PAnsiChar):AnsiString; overload;
function StrZComp(const Str1,Str2: PAnsiChar): integer; assembler;overload;
function StrZComp(const Str1,Str2,Tab: PAnsiChar): Integer; assembler;overload;
function StrLZComp(const Str1,Str2:PAnsiChar; MaxLen:Cardinal): integer; assembler;overload;
function StrLZComp(const Str1,Str2; MaxLen:Cardinal; Tab:PAnsiChar): integer; assembler;overload;
function StrLLComp(const Str1,Str2: PAnsiChar; L1,L2:Cardinal): integer; assembler;overload;
function StrLLComp(const Str1,Str2: PAnsiChar; L1,L2:Cardinal; Tab:PAnsiChar):integer; assembler;overload;
function StrBComp(const Str1,Str2: PAnsiChar; L1,L2:Cardinal): integer; assembler;overload;
function StrBComp(const Str1,Str2: PAnsiChar; L1,L2:Cardinal; Tab:PAnsiChar):integer; assembler;overload;

implementation

uses
  {$ifdef LINUX}Types,{$else}Windows,{$endif}
  Math;

function ToUpper(const S: AnsiString): AnsiString;
const D=ord('a')-ord('A');
var
  Len: Cardinal;
  i: integer;
  fromP,toP: PAnsiChar;
  c: AnsiChar;
begin
  Len:=Length(S);
  SetLength(result, Len);
  fromP:=PAnsiChar(S);
  toP:=PAnsiChar(result);
  for i:=0 to Len-1 do
  begin
    c:=fromP[i];
    if (c>='a') and (c<='z') then dec(c, D);
    toP[i]:=c;
  end;
end;

function ToLower(const S: AnsiString): AnsiString;
const D=ord('a')-ord('A');
var
  Len: Cardinal;
  i: integer;
  fromP,toP: PAnsiChar;
  c: AnsiChar;
begin
  Len:=Length(S);
  SetLength(result, Len);
  fromP:=PAnsiChar(S);
  toP:=PAnsiChar(result);
  for i:=0 to Len-1 do
  begin
    c:=fromP[i];
    if (c>='A') and (c<='Z') then inc(c, D);
    toP[i]:=c;
  end;
end;

function AnsiToLower(const S: AnsiString): AnsiString;
{$ifdef MSWINDOWS}
var
  Len: Cardinal;
begin
  Len := Length(S);
  if Len>0 then
  begin
    SetString(result, PAnsiChar(S), Len);
    CharLowerBuffA(PAnsiChar(result), Len);
  end else result:='';
end;
{$else}
begin
  result := UCS4StringToUnicodeString(UCS4LowerCase(UnicodeStringToUCS4String(S)));
end;
{$endif}

function AnsiToUpper(const S: AnsiString): AnsiString;
var
  Len: Cardinal;
begin
  Len:=Length(S);
  if Len>0 then
  begin
    SetString(result, PAnsiChar(S), Len);
    CharUpperBuffA(PAnsiChar(result), Len);
  end else result:='';
end;

function WideToLower(const S: UnicodeString): UnicodeString;
var
  Len: Cardinal;
begin
  Len:=Length(S);
  if Len>0 then
  begin
    SetString(result, PWideChar(S), Len);
    CharLowerBuffW(PWideChar(result), Len);
  end else result:='';
end;

function WideToUpper(const S: UnicodeString): UnicodeString;
var
  Len: Cardinal;
begin
  Len:=Length(S);
  if Len>0 then
  begin
    SetString(result, PWideChar(S), Len);
    CharUpperBuffW(PWideChar(result), Len);
  end else result:='';
end;

function AnsiCompare(const S1,S2: AnsiString; flagsIgnore:DWORD=0): integer;
begin
  Result := CompareStringA(LOCALE_USER_DEFAULT, flagsIgnore,
            PAnsiChar(S1), Length(S1), PAnsiChar(S2), Length(S2)) - 2;
end;

function AnsiCompareIgnore(const S1,S2: AnsiString): integer;
begin
  Result := AnsiCompare(S1,S2, NORM_IGNORECASE);
end;


function WideCompare(const S1,S2: UnicodeString; flagsIgnore:DWORD=0): integer;
{$if Defined(MSWINDOWS)}
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, flagsIgnore,
            PWideChar(S1), Length(S1), PWideChar(S2), Length(S2)) - 2;
end;
{$elseif Defined(LINUX)}
const MaxStackLen=128;
var
  len1,len2,i: integer;
  buf1,buf2: array[0..MaxStackLen] of DWORD;
  p1,p2: PDWORDArray;
begin
  len1:=UTF16To32(S1,nil,0,true);
  if len1>MaxStackLen then
    GetMem(p1,4*(len1+1))
  else
    p1:=@buf1;
  UTF16To32(S1,p1,len1+1,true);

  len2=UTF16To32(S2,nil,0,true);
  if len2>MaxStackLen then
    GetMem(p2,4*(len2+1))
  else
    p2:=@buf1;
  UTF16To32(S2,p2,len2+1,true);
try
  if flagsIgnore and NORM_IGNORECASE <> 0 then
  begin
    for i:=0 to len1-1 do p1[i]:=towlower(p1[i]);
    for i:=0 to len2-1 do p2[i]:=towlower(p2[i]);
  end;
  result := wcscoll(p1, p2);
finally
  if p1<>@buf1 then FreeMem(p1);
  if p2<>@buf2 then FreeMem(p2);
end;
end;
{$ifend}

function WideCompareIgnore(const S1,S2: UnicodeString): integer;
begin
  result:=WideCompare(S1,S2, NORM_IGNORECASE);
end;

{$if Defined(MSWINDOWS)}
function WideCompareL(const S1,S2: UnicodeString; MaxLen: integer; flagsIgnore:DWORD=0): integer;
begin
  result := CompareStringW(LOCALE_USER_DEFAULT, flagsIgnore,
            PWideChar(S1), min(Length(S1),MaxLen), PWideChar(S2), min(Length(S2),MaxLen)) - 2;
end;

function WideCompareLIgnore(const S1,S2: UnicodeString; MaxLen: integer): integer;
begin
  result:=WideCompareL(S1,S2, MaxLen, NORM_IGNORECASE);
end;
{$ifend}

//StrIn and StrOut can be the same AnsiString
procedure iStrConvert(StrIn,StrOut,Tab: PAnsiChar); assembler;
asm
    push  edi
    push  esi
    mov   esi,eax
    mov   edi,edx
    mov   al,[ecx]  //secure against bad Tab
    test  al,al
    jnz   @end
    xor   eax,eax
    dec   esi
    dec   edi
@loop:
    inc   esi
    inc   edi
    mov   al,[esi]
    mov   al,[ecx+eax] //with xlat be slower
    mov   [edi],al
    test  al,al
    jnz   @loop
@end:
    pop   esi
    pop   edi
end;

function iStrConvert(StrIn:AnsiString; Tab:PAnsiChar):AnsiString;
begin
  SetLength(result, Length(StrIn));
  iStrConvert(PAnsiChar(StrIn),PAnsiChar(result),Tab);
end;


{
 <0	Str1 < Str2
 =0	Str1 = Str2
 >0	Str1 > Str2
}
//Compare first difference or to #0 when identical
function StrZComp(const Str1,Str2: PAnsiChar): integer; assembler;overload;
asm
    push  edi
    push  esi
    mov   esi,eax
    mov   edi,edx

    // settings some registers (outside of loop!)speedup some slowdown,
    // it is optimized for Duron:
    xor   eax,eax
    xor   edx,edx
    dec   esi
    dec   edi
@loop:
    inc   esi
    inc   edi
    mov   al,[esi]
    test  al,al
    mov   dl,[edi] // not modified flags and mov dl,[edi] must be irrespective to if al=0
    je    @end     // al = 0 ?
    cmp   al,dl
    je    @loop
@end:
    sub   eax,edx
    pop   esi
    pop   edi
end;

function StrZComp(const Str1,Str2,Tab: PAnsiChar): integer; assembler;overload;
asm
    push  edi
    push  esi
    mov   esi,eax
    mov   edi,edx

    xor   eax,eax
    xor   edx,edx
    //in ecx is Tab
    dec   esi
    dec   edi
@loop:
    inc   esi
    inc   edi
    mov   al,[esi]
    test  al,al
    mov   dl,[edi] // not modified flags
    je    @end     // al = 0 ?
    cmp   al,dl
    je    @loop
    mov   al,byte ptr [ecx+eax] //xlat on Duron is slower than this instruction and must use ebx
    mov   dl,byte ptr [ecx+edx]
    cmp   eax,edx
    je    @loop
@end:
    sub   eax,edx
    pop   esi
    pop   edi
end;

//Compare MaxLen chars or to #0 AnsiChar (or first difference)
function StrLZComp(const Str1,Str2:PAnsiChar; MaxLen:Cardinal): integer; assembler;overload;
asm
    push  edi
    push  esi
    mov   esi,eax
    mov   edi,edx

    xor   eax,eax
    xor   edx,edx
    dec   esi
    dec   edi
@loop:
    cmp   ecx,0
    je    @end
    dec   ecx
    inc   esi
    inc   edi
    mov   al,[esi]
    test  al,al
    mov   dl,[edi] // not modified flags and mov dl,[edi] must be irrespective to if al=0
    je    @end     // al = 0 ?
    cmp   al,dl
    je    @loop
@end:
    sub   eax,edx
    pop   esi
    pop   edi
end;


function StrLZComp(const Str1,Str2; MaxLen:Cardinal; Tab:PAnsiChar): integer; assembler;overload;
asm
    push  edi
    push  esi
    push  ebx
    mov   esi,eax
    mov   edi,edx

    xor   eax,eax
    xor   edx,edx
    mov   ebx,Tab
    dec   esi
    dec   edi
@loop:
    cmp   ecx,0
    je    @end
    dec   ecx
    inc   esi
    inc   edi
    mov   al,[esi]
    test  al,al
    mov   dl,[edi] // not modified flags
    je    @end     // al = 0 ?
    cmp   al,dl
    je    @loop
    mov   al,byte ptr [ebx+eax] //xlat on Duron is slower than this instruction
    mov   dl,byte ptr [ebx+edx]
    cmp   eax,edx
    je    @loop
@end:
    sub   eax,edx
    pop   ebx
    pop   esi
    pop   edi
end;

//Compare exact min(L1,L2) chars (or to first difference); warning: #0 is not special treated
//if equal at min(L1,L2) chars and L1<>L2 - shorter AnsiString will less
function StrLLComp(const Str1,Str2: PAnsiChar; L1,L2:Cardinal): integer; assembler;overload;
asm
    push  edi
    push  esi
    push  L1
    push  L2
    mov   esi,eax
    mov   edi,edx
    cmp   L1,L2
    jle   @L1_LE
    mov   ecx,L2
@L1_LE:
    xor   eax,eax
    xor   edx,edx
    dec   esi
    dec   edi
@loop:
    cmp   ecx,0
    je    @end
    dec   ecx
    inc   esi
    inc   edi
    mov   al,[esi]
    mov   dl,[edi]
    cmp   al,dl
    je    @loop
@end:
    sub   eax,edx
    pop   edx // not modified flags
    pop   ecx
    jne   @chardiffs//eax <> 0
    mov   eax,ecx
    sub   eax,edx
@chardiffs:
    pop   esi
    pop   edi
end;

function StrLLComp(const Str1,Str2: PAnsiChar; L1,L2:Cardinal; Tab:PAnsiChar):integer; assembler;overload;
asm
    push  edi
    push  esi
    push  ebx
    push  L1
    push  L2
    mov   esi,eax
    mov   edi,edx
    cmp   L1,L2
    jle   @L1_LE
    mov   ecx,L2
@L1_LE:
    xor   eax,eax
    xor   edx,edx
    mov   ebx,Tab
    dec   esi
    dec   edi
@loop:
    cmp   ecx,0
    je    @end
    dec   ecx
    inc   esi
    inc   edi
    mov   al,[esi]
    mov   dl,[edi]
    cmp   al,dl
    je    @loop
    mov   al,byte ptr [ebx+eax] //xlat on Duron is slower than this instruction and must use ebx
    mov   dl,byte ptr [ebx+edx]
    cmp   eax,edx
    je    @loop
@end:
    sub   eax,edx
    pop   edx // not modified flags
    pop   ecx
    jne   @chardiffs//eax <> 0
    mov   eax,ecx
    sub   eax,edx
@chardiffs:
    pop   ebx
    pop   esi
    pop   edi
end;

//Compare Len1 and Len2 chars back; warning: #0 is not special treated
function StrBComp(const Str1,Str2: PAnsiChar; L1,L2:Cardinal): integer; assembler;overload;
asm
    push  edi
    push  esi
    push  L1
    push  L2
    mov   esi,eax
    add   esi,L1
    mov   edi,edx
    add   edi,L2
    cmp   L1,L2
    jle   @L1_LE
    mov   ecx,L2
@L1_LE:
    cmp   ecx,0
    jge   @ECX_GE
    xor   ecx,ecx
@ECX_GE:
    xor   eax,eax
    xor   edx,edx
@loop:
    cmp   ecx,0
    je    @end
    dec   ecx
    dec   esi
    dec   edi
    mov   al,[esi]
    mov   dl,[edi]
    cmp   al,dl
    je    @loop
@end:
    sub   eax,edx
    pop   edx // not modified flags
    pop   ecx
    jne   @chardiffs//eax <> 0
    mov   eax,ecx
    sub   eax,edx
@chardiffs:
    pop   esi
    pop   edi
end;


function StrBComp(const Str1,Str2: PAnsiChar; L1,L2:Cardinal; Tab:PAnsiChar):integer; assembler;overload;
asm
    push  edi
    push  esi
    push  ebx
    push  L1
    push  L2
    mov   esi,eax
    add   esi,L1
    mov   edi,edx
    add   edi,L2
    cmp   L1,L2
    jle   @L1_LE
    mov   ecx,L2
@L1_LE:
    cmp   ecx,0
    jge   @ECX_GE
    xor   ecx,ecx
@ECX_GE:
    xor   eax,eax
    xor   edx,edx
    mov   ebx,Tab
@loop:
    cmp   ecx,0
    je    @end
    dec   ecx
    dec   esi
    dec   edi
    mov   al,[esi]
    mov   dl,[edi]
    cmp   al,dl
    je    @loop
    mov   al,byte ptr [ebx+eax] //xlat on Duron is slower than this instruction and must use ebx
    mov   dl,byte ptr [ebx+edx]
    cmp   eax,edx
    je    @loop
@end:
    sub   eax,edx
    pop   edx // not modified flags
    pop   ecx
    jne   @chardiffs//eax <> 0
    mov   eax,ecx
    sub   eax,edx
@chardiffs:
    pop   ebx
    pop   esi
    pop   edi
end;

end.
