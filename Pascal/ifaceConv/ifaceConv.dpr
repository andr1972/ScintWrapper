{$APPTYPE CONSOLE}
program ifaceConv;

uses
  SysUtils;

procedure read(filename: string);
var
  f: TextFile;
  line: string;
  key: string[3];
begin
  AssignFile(f,filename);
  Reset(f);
  while not eof(f) do
  begin
    readln(f, line);
    if line='' then continue;
    if (Length(line)>=2)and(line[1]='#')and(line[2]='#') then continue;
    key:=Copy(line,1,3);
    if key='cat' then writeln('cat')
    else if key='fun' then writeln('fun')
    else if key='get' then writeln('get')
    else if key='set' then writeln('set')
    else if key='val' then writeln('val')
    else if key='evt' then writeln('evt')
    else if key='enu' then writeln('enu')
    else if key='lex' then writeln('lex');
  end;
  CloseFile(f);
end;

type
  TMiniLexer = class
  private
    FText: string;
    FPtr: PChar;
    FPos: integer;
  public
    procedure LoadText(Text: string);
    function NextToken: string; //empty string = end of text
  end;

procedure TMiniLexer.LoadText(Text: string);
begin
   FText:=Text;
   FPtr:=PChar(FText);
   FPos:=0;
end;

function TMiniLexer.NextToken: string;
var
  Head,Tail: PChar;
begin
  Assert(FPtr<>nil);
  result:='';
  Head := FPtr+FPos;
  if Head^=#0 then exit;
  while (Head^ in[#1..' ']) do inc(Head); //without #0
  if Head^=#0 then exit;
  Tail := Head;
  while not (Tail^ in[#0..' ']) do inc(Tail);//with #0
  SetString(result, Head, Tail-Head);
  while (Tail^ in[#1..' ']) do inc(Tail);//without #0
  FPos:=Tail-FPtr;
end;

var
  Str: string = 'fun void InsertText=2003(position pos, string text)';
  Pos: integer=0;
  Token: string;
  lexer: TMiniLexer;
begin
  lexer:=TMiniLexer.Create;
  lexer.LoadText(Str);
  repeat
    Token:=lexer.NextToken;
    if Token<>'' then writeln(Token);
  until Token='';
  lexer.Free;
  //read('Scintilla.iface');
end.
