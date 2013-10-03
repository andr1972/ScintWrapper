// GNU LESSER GENERAL PUBLIC LICENSE - See lgpl.txt
unit nHash;

interface
uses
  nStrings;
  
type
{$ifndef UNICODE}UnicodeString = WideString;{$endif}

  PHashItemII = ^THashItemII;
  THashItemII = record
    hash: LongWord;
    key: integer;
    value: integer;
    next: PHashItemII;
  end;

  PHashItemAI = ^THashItemAI;
  THashItemAI = record
    hash: LongWord;
    key: AnsiString;
    value: integer;
    next: PHashItemAI;
  end;

  PHashItemIA = ^THashItemIA;
  THashItemIA = record
    hash: LongWord;
    key: integer;
    value: AnsiString;
    next: PHashItemIA;
  end;

  PHashItemAA = ^THashItemAA;
  THashItemAA = record
    hash: LongWord;
    key: AnsiString;
    value: AnsiString;
    next: PHashItemAA;
  end;

  PHashItemSI = ^THashItemSI;
  THashItemSI = record
    hash: LongWord;
    key: string;
    value: integer;
    next: PHashItemSI;
  end;

  PHashItemIS = ^THashItemIS;
  THashItemIS = record
    hash: LongWord;
    key: integer;
    value: string;
    next: PHashItemIS;
  end;

  PHashItemSS = ^THashItemSS;
  THashItemSS = record
    hash: LongWord;
    key: string;
    value: string;
    next: PHashItemSS;
  end;

  PHashItemWI = ^THashItemWI;
  THashItemWI = record
    hash: LongWord;
    key: UnicodeString;
    value: integer;
    next: PHashItemAI;
  end;

  PHashItemIW = ^THashItemIW;
  THashItemIW = record
    hash: LongWord;
    key: integer;
    value: UnicodeString;
    next: PHashItemIW;
  end;

  PHashItemWW = ^THashItemWW;
  THashItemWW = record
    hash: LongWord;
    key: UnicodeString;
    value: UnicodeString;
    next: PHashItemWW;
  end;

  PHashItemAW = ^THashItemAW;
  THashItemAW = record
    hash: LongWord;
    key: AnsiString;
    value: UnicodeString;
    next: PHashItemWW;
  end;

  PHashItemWA = ^THashItemWA;
  THashItemWA = record
    hash: LongWord;
    key: UnicodeString;
    value: AnsiString;
    next: PHashItemWW;
  end;

  THashFunc = function(key: integer): LongWord;
  TEqualsFunc = function(keyA, keyB: integer): boolean;

  { behaviour when it meet existing key
    - enable associate multiple values to one key, values can be retrieved
    by GetNext
    - raise Exception - this situation means error in program
    - skip, only first will be stored
    - overwrite, only last will be stored }
  TKeyAgain = (kaEnable, kaException, kaSkip, kaOverwrite);

  THashTableII = class
  private
    FSize: LongWord;
    FCapacity: LongWord;
    FTab: array of PHashItemII;
    FHashFunc: THashFunc;
    FEqualsFunc: TEqualsFunc;
  protected
    procedure AssignKey(item: PHashItemII; key: integer); virtual;
    procedure AssignValue(item: PHashItemII; value: integer); virtual;
    procedure DisposeItem(item: PHashItemII); virtual;
  public
    KeyAgain: TKeyAgain;
    constructor Create(ACapacity: LongWord = 256);
    destructor Destroy; override;
    procedure Put(key, value: integer);
    function Get(key: integer): PHashItemII;
    function GetPrior(item: PHashItemII): PHashItemII;
    function RemoveKey(key: integer): boolean;
    procedure Clear;
    property Size: LongWord read FSize;
    property Capacity: LongWord read FCapacity;
  end;

  THashTableAI = class(THashTableII)
  protected
    procedure AssignKey(item: PHashItemII; key: integer); override;
    procedure DisposeItem(item: PHashItemII); override;
  public
    constructor Create(ACapacity: LongWord = 256);
    procedure Put(key: AnsiString; value: integer);
    function Get(key: AnsiString): PHashItemAI;
    function GetPrior(item: PHashItemAI): PHashItemAI;
    function RemoveKey(key: AnsiString): boolean;
  end;

  THashTableIA = class(THashTableII)
  protected
    procedure AssignValue(item: PHashItemII; value: integer); override;
    procedure DisposeItem(item: PHashItemII); override;
  public
    procedure Put(key: integer; value: AnsiString);
    function Get(key: integer): PHashItemIA;
    function GetPrior(item: PHashItemIA): PHashItemIA;
  end;

  THashTableAA = class(THashTableII)
  protected
    procedure AssignKey(item: PHashItemII; key: integer); override;
    procedure AssignValue(item: PHashItemII; value: integer); override;
    procedure DisposeItem(item: PHashItemII); override;
  public
    constructor Create(ACapacity: LongWord = 256);
    procedure Put(key: AnsiString; value: AnsiString);
    function Get(key: AnsiString): PHashItemAA;
    function GetPrior(item: PHashItemAA): PHashItemAA;
    function RemoveKey(key: AnsiString): boolean;
  end;

  THashTableSI = class(THashTableII)
  protected
    procedure AssignKey(item: PHashItemII; key: integer); override;
    procedure DisposeItem(item: PHashItemII); override;
  public
    constructor Create(ACapacity: LongWord = 256);
    procedure Put(key: string; value: integer);
    function Get(key: string): PHashItemSI;
    function GetPrior(item: PHashItemSI): PHashItemSI;
    function RemoveKey(key: string): boolean;
  end;

  THashTableIS = class(THashTableII)
  protected
    procedure AssignValue(item: PHashItemII; value: integer); override;
    procedure DisposeItem(item: PHashItemII); override;
  public
    procedure Put(key: integer; value: string);
    function Get(key: integer): PHashItemIS;
    function GetPrior(item: PHashItemIS): PHashItemIS;
  end;

  THashTableSS = class(THashTableII)
  protected
    procedure AssignKey(item: PHashItemII; key: integer); override;
    procedure AssignValue(item: PHashItemII; value: integer); override;
    procedure DisposeItem(item: PHashItemII); override;
  public
    constructor Create(ACapacity: LongWord = 256);
    procedure Put(key: string; value: string);
    function Get(key: string): PHashItemSS;
    function GetPrior(item: PHashItemSS): PHashItemSS;
    function RemoveKey(key: string): boolean;
  end;

  THashTableWI = class(THashTableII)
  protected
    procedure AssignKey(item: PHashItemII; key: integer); override;
    procedure DisposeItem(item: PHashItemII); override;
  public
    constructor Create(ACapacity: LongWord = 256);
    procedure Put(key: UnicodeString; value: integer);
    function Get(key: UnicodeString): PHashItemWI;
    function GetPrior(item: PHashItemWI): PHashItemWI;
    function RemoveKey(key: UnicodeString): boolean;
  end;

  THashTableIW = class(THashTableII)
  protected
    procedure AssignValue(item: PHashItemII; value: integer); override;
    procedure DisposeItem(item: PHashItemII); override;
  public
    procedure Put(key: integer; value: UnicodeString);
    function Get(key: integer): PHashItemIW;
    function GetPrior(item: PHashItemIW): PHashItemIW;
  end;

  THashTableWW = class(THashTableII)
  protected
    procedure AssignKey(item: PHashItemII; key: integer); override;
    procedure AssignValue(item: PHashItemII; value: integer); override;
    procedure DisposeItem(item: PHashItemII); override;
  public
    constructor Create(ACapacity: LongWord = 256);
    procedure Put(key: UnicodeString; value: UnicodeString);
    function Get(key: UnicodeString): PHashItemWW;
    function GetPrior(item: PHashItemWW): PHashItemWW;
    function RemoveKey(key: UnicodeString): boolean;
  end;

  THashTableAW = class(THashTableII)
  protected
    procedure AssignKey(item: PHashItemII; key: integer); override;
    procedure AssignValue(item: PHashItemII; value: integer); override;
    procedure DisposeItem(item: PHashItemII); override;
  public
    constructor Create(ACapacity: LongWord = 256);
    procedure Put(key: AnsiString; value: UnicodeString);
    function Get(key: AnsiString): PHashItemAW;
    function GetPrior(item: PHashItemAW): PHashItemAW;
    function RemoveKey(key: AnsiString): boolean;
  end;

  THashTableWA = class(THashTableII)
  protected
    procedure AssignKey(item: PHashItemII; key: integer); override;
    procedure AssignValue(item: PHashItemII; value: integer); override;
    procedure DisposeItem(item: PHashItemII); override;
  public
    constructor Create(ACapacity: LongWord = 256);
    procedure Put(key: UnicodeString; value: AnsiString);
    function Get(key: UnicodeString): PHashItemWA;
    function GetPrior(item: PHashItemWA): PHashItemWA;
    function RemoveKey(key: UnicodeString): boolean;
  end;


implementation

uses
  SysUtils;

var
  TranslTab: array [0 .. 255] of byte;

function H(p: PByte; ByteCount: LongWord): LongWord;
var
  i: LongWord;
begin
  result := $FFFFFFFF;
  if ByteCount > 0 then
    for i := 0 to ByteCount - 1 do
    begin
      // new processors fast multiplies by constant
      result := (result xor p^) * 134775813 + 1;
      inc(p);
    end;
end;

function H_transl(p: PByte; ByteCount: LongWord;
  var LookUp: array of byte): LongWord;
var
  i: LongWord;
begin
  result := $FFFFFFFF;
  if ByteCount > 0 then
    for i := 0 to ByteCount - 1 do
    begin
      // new processors fast multiplies by constant
      result := (result xor LookUp[p^]) * 134775813 + 1;
      inc(p);
    end;
end;

function HashFuncI(key: integer): LongWord;
begin
  result := H(@key, 4);
end;

function HashFuncA(key: integer): LongWord;
var
  AStr: AnsiString absolute key;
begin
  result := H(PByte(PAnsiChar(AStr)), Length(AStr));
end;

function HashFuncS(key: integer): LongWord;
var
  Str: string absolute key;
begin
  result := H(PByte(PChar(Str)), Length(Str));
end;

function HashFuncW(key: integer): LongWord;
var
  WStr: UnicodeString absolute key;
begin
  result := H(PByte(PWideChar(WStr)), Length(WStr));
end;

function HashFuncALower(key: integer): LongWord;
var
  AStr: AnsiString absolute key;
begin
  result := H_transl(PByte(PAnsiChar(AStr)), Length(AStr), TranslTab);
end;

function HashFuncWLower(key: integer): LongWord;
var
  WStr: UnicodeString absolute key;
  LowerWStr: UnicodeString;
begin
  LowerWStr := WideLowerCase(WStr);
  result := H(PByte(PWideChar(LowerWStr)), Length(WStr));
end;

function EqualsFuncI(keyA, keyB: integer): boolean;
begin
  result := keyA = keyB;
end;

function EqualsFuncA(keyA, keyB: integer): boolean;
var
  AStrA: AnsiString absolute keyA;
  AStrB: AnsiString absolute keyB;
begin
  result := AStrA = AStrB;
end;

function EqualsFuncAignore(keyA, keyB: integer): boolean;
var
  AStrA: AnsiString absolute keyA;
  AStrB: AnsiString absolute keyB;
begin
  result := AnsiCompareIgnore(AStrA, AStrB) = 0;
end;

function EqualsFuncS(keyA, keyB: integer): boolean;
var
  StrA: string absolute keyA;
  StrB: string absolute keyB;
begin
  result := StrA = StrB;
end;

function EqualsFuncSignore(keyA, keyB: integer): boolean;
var
  StrA: string absolute keyA;
  StrB: string absolute keyB;
begin
  result := SysUtils.AnsiCompareText(StrA, StrB) = 0;
end;

function EqualsFuncW(keyA, keyB: integer): boolean;
var
  WStrA: UnicodeString absolute keyA;
  WStrB: UnicodeString absolute keyB;
begin
  result := WStrA = WStrB;
end;

function EqualsFuncWignore(keyA, keyB: integer): boolean;
var
  WStrA: UnicodeString absolute keyA;
  WStrB: UnicodeString absolute keyB;
begin
  result := WideCompareIgnore(WStrA, WStrB) = 0;
end;

{ THashTableII }

procedure THashTableII.AssignKey(item: PHashItemII; key: integer);
begin
  item.key := key;
end;

procedure THashTableII.AssignValue(item: PHashItemII; value: integer);
begin
  item.value := value;
end;

procedure THashTableII.Clear;
var
  i: integer;
  item,p: PHashItemII;
begin
  for i := 0 to FCapacity - 1 do
  begin
    item := FTab[i];
    while item <> nil do
    begin
      p := item;
      item := item.next;
      DisposeItem(p);
    end;
  end;
end;

{ Returns the index of most significant set bit; for 0 return 0 }
function FindMostSignSetBit(N: Cardinal): integer;
asm {On entry: eax = ACardinal}
  bsr eax, eax
end;

constructor THashTableII.Create(ACapacity: LongWord = 256);
begin
  FCapacity := 1 shl FindMostSignSetBit(ACapacity);
  KeyAgain := kaEnable;
  SetLength(FTab, FCapacity);
  FHashFunc := HashFuncI;
  FEqualsFunc := EqualsFuncI;
end;

destructor THashTableII.Destroy;
begin
  Clear;
  inherited;
end;

procedure THashTableII.DisposeItem(item: PHashItemII);
begin
  Dispose(item);
end;

procedure THashTableII.Put(key, value: integer);
var
  hash: LongWord;
  bucketIdx: integer;
  item: PHashItemII;
begin
  hash := FHashFunc(key);
  bucketIdx := hash and (Capacity-1);//capacity must be power of two
  New(item);
  item.hash := hash;
  // fields key and value must be zero due to descendants (fields will strings)
  item.key := 0;
  item.value := 0;
  AssignKey(item, key);
  AssignValue(item, value);
  // FEqualsFunc tu w zaleznosci od keyagain
  item.next := FTab[bucketIdx];
  FTab[bucketIdx] := item;
end;

function THashTableII.RemoveKey(key: integer): boolean;
var
  hash: LongWord;
  bucketIdx: integer;
  item: PHashItemII;
  prevItem,nextItem: PHashItemII;
begin
  result:=false;
  hash := FHashFunc(key);
  bucketIdx := hash and (Capacity-1);
  item := FTab[bucketIdx];
  prevItem := nil;
  while item <> nil do
  begin
    if (item.hash = hash) and FEqualsFunc(item.key, key) then
    begin
      nextItem:=item.next;
      writeln('remove value=',item.value);
      if prevItem=nil then
        FTab[bucketIdx]:=item.next
      else
        prevItem.next:=item.next;
      DisposeItem(item);
      item:=nextItem;
      result:=true;
    end
    else
    begin
      writeln('skip value=',item.value);
      prevItem:=item;
      item := item.next;
    end;
  end;
  if result then dec(FSize);
  writeln;writeln('after remove');
  item := FTab[bucketIdx];
  while item <> nil do
  begin
    Assert (not FEqualsFunc(item.key, key));
    writeln('zostalo value=',item.value);
    item := item.next;
  end;
  writeln;writeln;
end;

function THashTableII.Get(key: integer): PHashItemII;
var
  hash: LongWord;
  bucketIdx: integer;
begin
  hash := FHashFunc(key);
  bucketIdx := hash and (Capacity-1);
  result := FTab[bucketIdx];
  while result <> nil do
  begin
    if (result.hash = hash) and FEqualsFunc(result.key, key) then
      exit;
    result := result.next;
  end;
end;

function THashTableII.GetPrior(item: PHashItemII): PHashItemII;
var
  hash: LongWord;
  key: integer;
begin
  hash := item.hash;
  key := item.key;
  result := item.next;
  while result <> nil do
  begin
    if (result.hash = hash) and FEqualsFunc(result.key, key) then
      exit;
    result := result.next;
  end;
end;

procedure THashTableIA.AssignValue(item: PHashItemII; value: integer);
var
  valueA: AnsiString absolute value;
  itemIA: PHashItemIA absolute item;
begin
  itemIA.value := valueA;
end;

procedure THashTableIA.DisposeItem(item: PHashItemII);
begin
  Dispose(PHashItemIA(item));
end;

function THashTableIA.Get(key: integer): PHashItemIA;
begin
  result := PHashItemIA( inherited Get(key));
end;

function THashTableIA.GetPrior(item: PHashItemIA): PHashItemIA;
begin
  result:=PHashItemIA(inherited GetPrior(PHashItemII(item)));
end;

procedure THashTableIA.Put(key: integer; value: AnsiString);
begin
  inherited Put(key, integer(value));
end;

{ THashTableAI }

procedure THashTableAI.AssignKey(item: PHashItemII; key: integer);
var
  keyA: AnsiString absolute key;
  itemAI: PHashItemAI absolute item;
begin
  itemAI.key := keyA;
end;

constructor THashTableAI.Create(ACapacity: LongWord = 256);
begin
  inherited Create(ACapacity);
  FHashFunc := HashFuncA;
  FEqualsFunc := EqualsFuncA;
end;

procedure THashTableAI.DisposeItem(item: PHashItemII);
begin
  Dispose(PHashItemAI(item));
end;

function THashTableAI.Get(key: AnsiString): PHashItemAI;
begin
  result := PHashItemAI( inherited Get(integer(key)));
end;

function THashTableAI.GetPrior(item: PHashItemAI): PHashItemAI;
begin
  result:=PHashItemAI(inherited GetPrior(PHashItemII(item)));
end;

procedure InitTransl;
var
  i: integer;
  AnsiChars: AnsiString;
begin
  SetLength(AnsiChars, 255);
  for i := 1 to 255 do
    AnsiChars[i] := AnsiChar(i);
  AnsiChars := AnsiToLower(AnsiChars);
  TranslTab[0] := 0;
  for i := 1 to 255 do
    TranslTab[i] := ord(AnsiChars[i]);
end;

procedure THashTableAI.Put(key: AnsiString; value: integer);
begin
  inherited Put(integer(key), value);
end;

function THashTableAI.RemoveKey(key: AnsiString): boolean;
begin
  result:=inherited RemoveKey(integer(key));
end;

{ THashTableAA }

procedure THashTableAA.AssignKey(item: PHashItemII; key: integer);
var
  keyA: AnsiString absolute key;
  itemAA: PHashItemAA absolute item;
begin
  itemAA.key := keyA;
end;

procedure THashTableAA.AssignValue(item: PHashItemII; value: integer);
var
  valueA: AnsiString absolute value;
  itemAA: PHashItemAA absolute item;
begin
  itemAA.value := valueA;
end;

constructor THashTableAA.Create(ACapacity: LongWord = 256);
begin
  inherited Create(ACapacity);
  FHashFunc := HashFuncA;
  FEqualsFunc := EqualsFuncA;
end;

procedure THashTableAA.DisposeItem(item: PHashItemII);
begin
  Dispose(PHashItemAA(item));
end;

function THashTableAA.Get(key: AnsiString): PHashItemAA;
begin
  result := PHashItemAA( inherited Get(integer(key)));
end;

function THashTableAA.GetPrior(item: PHashItemAA): PHashItemAA;
begin
  result:=PHashItemAA(inherited GetPrior(PHashItemII(item)));
end;

procedure THashTableAA.Put(key: AnsiString; value: AnsiString);
begin
  inherited Put(integer(key), integer(value));
end;

function THashTableAA.RemoveKey(key: AnsiString): boolean;
begin
  result:=inherited RemoveKey(integer(key));
end;

{ THashTableSI }

procedure THashTableSI.AssignKey(item: PHashItemII; key: integer);
var
  keyS: string absolute key;
  itemSI: PHashItemSI absolute item;
begin
  itemSI.key := keyS;
end;

constructor THashTableSI.Create(ACapacity: LongWord);
begin
  inherited Create(ACapacity);
  FHashFunc := HashFuncS;
  FEqualsFunc := EqualsFuncS;
end;

procedure THashTableSI.DisposeItem(item: PHashItemII);
begin
  Dispose(PHashItemSI(item));
end;

function THashTableSI.Get(key: string): PHashItemSI;
begin
  result := PHashItemSI( inherited Get(integer(key)));
end;

function THashTableSI.GetPrior(item: PHashItemSI): PHashItemSI;
begin
  result:=PHashItemSI(inherited GetPrior(PHashItemII(item)));
end;

procedure THashTableSI.Put(key: string; value: integer);
begin
  inherited Put(integer(key), value);
end;

function THashTableSI.RemoveKey(key: string): boolean;
begin
  result:=inherited RemoveKey(integer(key));
end;

{ THashTableIS }

procedure THashTableIS.AssignValue(item: PHashItemII; value: integer);
var
  valueS: string absolute value;
  itemIS: PHashItemIS absolute item;
begin
  itemIS.value := valueS;
end;

procedure THashTableIS.DisposeItem(item: PHashItemII);
begin
  Dispose(PHashItemIS(item));
end;

function THashTableIS.Get(key: integer): PHashItemIS;
begin
  result := PHashItemIS( inherited Get(key));
end;

function THashTableIS.GetPrior(item: PHashItemIS): PHashItemIS;
begin
  result:=PHashItemIS(inherited GetPrior(PHashItemII(item)));
end;

procedure THashTableIS.Put(key: integer; value: string);
begin
  inherited Put(key, integer(value));
end;

{ THashTableSS }

procedure THashTableSS.AssignKey(item: PHashItemII; key: integer);
var
  keyS: string absolute key;
  itemSS: PHashItemSS absolute item;
begin
  itemSS.key := keyS;
end;

procedure THashTableSS.AssignValue(item: PHashItemII; value: integer);
var
  valueS: string absolute value;
  itemSS: PHashItemSS absolute item;
begin
  itemSS.value := valueS;
end;

constructor THashTableSS.Create(ACapacity: LongWord);
begin
  inherited Create(ACapacity);
  FHashFunc := HashFuncS;
  FEqualsFunc := EqualsFuncS;
end;

procedure THashTableSS.DisposeItem(item: PHashItemII);
begin
  Dispose(PHashItemSS(item));
end;

function THashTableSS.Get(key: string): PHashItemSS;
begin
  result := PHashItemSS( inherited Get(integer(key)));
end;

function THashTableSS.GetPrior(item: PHashItemSS): PHashItemSS;
begin
  result:=PHashItemSS(inherited GetPrior(PHashItemII(item)));
end;

procedure THashTableSS.Put(key, value: string);
begin
  inherited Put(integer(key), integer(value));
end;

function THashTableSS.RemoveKey(key: string): boolean;
begin
  result:=inherited RemoveKey(integer(key));
end;

{ THashTableWI }

procedure THashTableWI.AssignKey(item: PHashItemII; key: integer);
var
  keyW: UnicodeString absolute key;
  itemWI: PHashItemWI absolute item;
begin
  itemWI.key := keyW;
end;

constructor THashTableWI.Create(ACapacity: LongWord = 256);
begin
  inherited Create(ACapacity);
  FHashFunc := HashFuncW;
  FEqualsFunc := EqualsFuncW;
end;

procedure THashTableWI.DisposeItem(item: PHashItemII);
begin
  Dispose(PHashItemWI(item));
end;

function THashTableWI.Get(key: UnicodeString): PHashItemWI;
begin
  result := PHashItemWI( inherited Get(integer(key)));
end;

function THashTableWI.GetPrior(item: PHashItemWI): PHashItemWI;
begin
  result:=PHashItemWI(inherited GetPrior(PHashItemII(item)));
end;

procedure THashTableWI.Put(key: UnicodeString; value: integer);
begin
  inherited Put(integer(key), value);
end;

function THashTableWI.RemoveKey(key: UnicodeString): boolean;
begin
  result:=inherited RemoveKey(integer(key));
end;

{ THashTableIW }

procedure THashTableIW.AssignValue(item: PHashItemII; value: integer);
var
  valueW: UnicodeString absolute value;
  itemIW: PHashItemIW absolute item;
begin
  itemIW.value := valueW;
end;

procedure THashTableIW.DisposeItem(item: PHashItemII);
begin
  Dispose(PHashItemIW(item));
end;

function THashTableIW.Get(key: integer): PHashItemIW;
begin
  result := PHashItemIW( inherited Get(key));
end;

function THashTableIW.GetPrior(item: PHashItemIW): PHashItemIW;
begin
  result:=PHashItemIW(inherited GetPrior(PHashItemII(item)));
end;

procedure THashTableIW.Put(key: integer; value: UnicodeString);
begin
  inherited Put(key, integer(value));
end;

{ THashTableWW }

procedure THashTableWW.AssignKey(item: PHashItemII; key: integer);
var
  keyW: UnicodeString absolute key;
  itemWW: PHashItemWW absolute item;
begin
  itemWW.key := keyW;
end;

procedure THashTableWW.AssignValue(item: PHashItemII; value: integer);
var
  valueW: UnicodeString absolute value;
  itemWW: PHashItemWW absolute item;
begin
  itemWW.value := valueW;
end;

constructor THashTableWW.Create(ACapacity: LongWord = 256);
begin
  inherited Create(ACapacity);
  FHashFunc := HashFuncW;
  FEqualsFunc := EqualsFuncW;
end;

procedure THashTableWW.DisposeItem(item: PHashItemII);
begin
  Dispose(PHashItemWW(item));
end;

function THashTableWW.Get(key: UnicodeString): PHashItemWW;
begin
  result := PHashItemWW( inherited Get(integer(key)));
end;

function THashTableWW.GetPrior(item: PHashItemWW): PHashItemWW;
begin
  result:=PHashItemWW(inherited GetPrior(PHashItemII(item)));
end;

procedure THashTableWW.Put(key, value: UnicodeString);
begin
  inherited Put(integer(key), integer(value));
end;

function THashTableWW.RemoveKey(key: UnicodeString): boolean;
begin
  result:=inherited RemoveKey(integer(key));
end;

{ THashTableAW }

procedure THashTableAW.AssignKey(item: PHashItemII; key: integer);
var
  keyA: AnsiString absolute key;
  itemAW: PHashItemAW absolute item;
begin
  itemAW.key := keyA;
end;

procedure THashTableAW.AssignValue(item: PHashItemII; value: integer);
var
  valueW: UnicodeString absolute value;
  itemAW: PHashItemAW absolute item;
begin
  itemAW.value := valueW;
end;

constructor THashTableAW.Create(ACapacity: LongWord = 256);
begin
  inherited Create(ACapacity);
  FHashFunc := HashFuncA;
  FEqualsFunc := EqualsFuncA;
end;

procedure THashTableAW.DisposeItem(item: PHashItemII);
begin
    Dispose(PHashItemAW(item));
end;

function THashTableAW.Get(key: AnsiString): PHashItemAW;
begin
  result := PHashItemAW( inherited Get(integer(key)));
end;

function THashTableAW.GetPrior(item: PHashItemAW): PHashItemAW;
begin
  result:=PHashItemAW(inherited GetPrior(PHashItemII(item)));
end;

procedure THashTableAW.Put(key: AnsiString; value: UnicodeString);
begin
  inherited Put(integer(key), integer(value));
end;

function THashTableAW.RemoveKey(key: AnsiString): boolean;
begin
  result:=inherited RemoveKey(integer(key));
end;

{ THashTableWA }

procedure THashTableWA.AssignKey(item: PHashItemII; key: integer);
var
  keyW: UnicodeString absolute key;
  itemWA: PHashItemWA absolute item;
begin
  itemWA.key := keyW;
end;

procedure THashTableWA.AssignValue(item: PHashItemII; value: integer);
var
  valueA: AnsiString absolute value;
  itemWA: PHashItemWA absolute item;
begin
  itemWA.value := valueA;
end;

constructor THashTableWA.Create(ACapacity: LongWord = 256);
begin
  inherited Create(ACapacity);
  FHashFunc := HashFuncW;
  FEqualsFunc := EqualsFuncW;
end;

procedure THashTableWA.DisposeItem(item: PHashItemII);
begin
  Dispose(PHashItemWA(item));
end;

function THashTableWA.Get(key: UnicodeString): PHashItemWA;
begin
  result := PHashItemWA( inherited Get(integer(key)));
end;

function THashTableWA.GetPrior(item: PHashItemWA): PHashItemWA;
begin
  result:=PHashItemWA(inherited GetPrior(PHashItemII(item)));
end;

procedure THashTableWA.Put(key: UnicodeString; value: AnsiString);
begin
  inherited Put(integer(key), integer(value));
end;

function THashTableWA.RemoveKey(key: UnicodeString): boolean;
begin
  result:=inherited RemoveKey(integer(key));
end;

initialization
  InitTransl;
end.
