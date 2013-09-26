{$APPTYPE CONSOLE}
program ifaceConv;

uses
  SysUtils;

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
  while (Head^ in[#1..' ']) do inc(Head); //white chars without #0
  if Head^=#0 then exit;
  Tail := Head;
  case Tail^ of
    'A'..'Z','a'..'z','_': while Tail^ in['A'..'Z','a'..'z','0'..'9','_'] do inc(Tail); //ident
    '0'..'9': while Tail^ in['0'..'9'] do inc(Tail); //number
    else inc(Tail); //token = one char
  end;
  SetString(result, Head, Tail-Head);
  while (Tail^ in[#1..' ']) do inc(Tail);//white chars without #0
  FPos:=Tail-FPtr;
end;

procedure read(filename: string);
var
  f: TextFile;
  line: string;
  key,name,num: string;
  lexer: TMiniLexer;
begin
  AssignFile(f,filename);
  Reset(f);
  lexer:=TMiniLexer.Create;
  while not eof(f) do
  begin
    readln(f, line);
    if line='' then continue;
    if (Length(line)>=2)and(line[1]='#')and(line[2]='#') then continue;
    lexer.LoadText(line);
    key:=lexer.NextToken;
    if key='val' then
    begin
      name:=lexer.NextToken;
      lexer.NextToken; //=
      num:=lexer.NextToken;
      writeln('#define ',UpperCase(name),' ',num);
    end
    else if (key='fun')or(key='get')or(key='set') then
    begin
      lexer.NextToken; //type
      name:=lexer.NextToken;
      lexer.NextToken; //=
      num:=lexer.NextToken;
      writeln('#define SCI_',UpperCase(name),' ',num);
    end;
    {key:=Copy(line,1,3);
    if key='cat' then writeln('cat')
    else if key='get' then writeln('get')
    else if key='set' then writeln('set')
    else if key='val' then writeln('val')
    else if key='evt' then writeln('evt')
    else if key='enu' then writeln('enu')
    else if key='lex' then writeln('lex');}
  end;
  lexer.Free;
  CloseFile(f);
end;

begin
  read('Scintilla.iface');
end.

