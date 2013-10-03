{$APPTYPE CONSOLE}
program ifaceConv;

uses
  SysUtils,
  nHash,
  Classes;

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
    '0'..'9':
    begin
      if (Tail^='0')and(Tail[1]='x') then
      begin
        inc(Tail,2);
        while Tail^ in['0'..'9','A'..'F','a'..'f'] do inc(Tail);
      end
      else while Tail^ in['0'..'9'] do inc(Tail); //number
    end else
    begin
      if (Tail^='-') and (Tail[1] in['0'..'9']) then
      begin
        inc(Tail);
        while Tail^ in['0'..'9'] do inc(Tail);
      end else
      inc(Tail); //token = one char
    end;
  end;
  SetString(result, Head, Tail-Head);
  while (Tail^ in[#1..' ']) do inc(Tail);//white chars without #0
  FPos:=Tail-FPtr;
end;

type
//if function parameter type stringresult return with zero at end?
  TStringResultZ = class
  private
    listZ: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function ifReturnsZ(funName: string): boolean;
  end;

var
  srz: TStringResultZ;

procedure createH(filename: string);
var
  f: TextFile;
  outF: TextFile;
  line: string;
  key,name,num: string;
  lexer: TMiniLexer;
begin
  AssignFile(f,filename);
  AssignFile(outF,'Scintilla.h.gen');
  Reset(f);
  Rewrite(outF);
  lexer:=TMiniLexer.Create;
  while not eof(f) do
  begin
    readln(f, line);
    if line='' then continue;
    if (Length(line)>=2)and(line[1]='#')and(line[2]='#') then continue;
    if line='# For SciLexer.h' then
    begin
      CloseFile(outF);
      AssignFile(outF,'SciLexer.h.gen');
      Rewrite(outF);
    end;
    if line='# Events' then
    begin
      CloseFile(outF);
      AssignFile(outF,'Scintilla.h.gen');
      Append(outF);
    end;
    if line='cat Provisional' then break;
    lexer.LoadText(line);
    key:=lexer.NextToken;
    if key='val' then
    begin
      name:=lexer.NextToken;
      lexer.NextToken; //=
      num:=lexer.NextToken;
      writeln(outF, '#define ',UpperCase(name),' ',num);
    end
    else if (key='fun')or(key='get')or(key='set') then
    begin
      lexer.NextToken; //type
      name:=lexer.NextToken;
      lexer.NextToken; //=
      num:=lexer.NextToken;
      writeln(outF, '#define SCI_',UpperCase(name),' ',num);
    end else if key='evt' then
    begin
      lexer.NextToken; //type
      name:=lexer.NextToken;
      lexer.NextToken; //=
      num:=lexer.NextToken;
      writeln(outF, '#define SCN_',UpperCase(name),' ',num);
    end;
  end;
  lexer.Free;
  CloseFile(outF);
  CloseFile(f);
end;

procedure dump_types;
var
  f: TextFile;
  line: string;
  type_,key,funname,name,num,str: string;
  lexer: TMiniLexer;
begin
  AssignFile(f,'Scintilla.iface');
  Reset(f);
  lexer:=TMiniLexer.Create;
  while not eof(f) do
  begin
    readln(f, line);
    if line='' then continue;
    if (Length(line)>=2)and(line[1]='#')and(line[2]='#') then continue;
    if line='cat Provisional' then break;
    lexer.LoadText(line);
    key:=lexer.NextToken;
    if (key='fun')or(key='get')or(key='set')or(key='evt') then
    begin
      type_:=lexer.NextToken; //type
      funname:=lexer.NextToken;
      lexer.NextToken; //=
      num:=lexer.NextToken;
      writeln(type_);
      str:=lexer.NextToken;
      Assert(str='(');
      repeat
        type_:=lexer.NextToken;
        if type_=',' then continue;
        if type_=')' then break;
        writeln(type_);
        //if type_='stringresult' then writeln(funname);
        name:=lexer.NextToken;
        if name=',' then continue;
        if name=')' then break;
        str:=lexer.NextToken;
        Assert((str=',')or(str=')'));
      until str=')';
    end;
  end;
  lexer.Free;
  CloseFile(f);
end;

function Hex2Pas(numstr: string): string;
begin
  if (Length(numstr)>2)and(numstr[1]='0')and(numstr[2]='x') then
    result:='$'+Copy(numstr,3,Length(numstr)-2)
  else result:=numstr;
end;


procedure HeaderAutoFile(var f: TExtFile);
begin
  writeln(f,'{ Do not edit. This file is automatically generated from the');
  writeln(f,'Scintilla.iface interface definition }');
  writeln(f);
end;

procedure loadTypemap(filename: string; map: THashTableSS);
var
  f: TextFile;
  line: string;
  lexer: TMiniLexer;
  key, value: string;
begin
  AssignFile(f,filename);
  Reset(f);
  lexer:=TMiniLexer.Create;
  while not eof(f) do
  begin
    readln(f, line);
    lexer.LoadText(line);
    key:=lexer.NextToken;
    value:=lexer.NextToken;
    map.Put(key, value);
  end;
  lexer.Free;
  CloseFile(f);
end;

procedure createPas(filename: string);
var
  f: TextFile;
  constsF: TextFile;
  lexUnitF: TextFile;
  funDeclF: TextFile;
  funBodiesF: TextFile;
  line: string;
  key,funtype,name,num,otherStr: string;
  paramTypes,paramNames: TStringList;
  lexer: TMiniLexer;
  partLexers: boolean;
  typeMap: THashTableSS;
  msgCnt,paramCnt: integer;
  bStringResult,bSep: boolean;
  declStr,declStrB,subDeclStr: string;
  i:integer;
begin
  typeMap:=THashTableSS.Create(16);
  loadTypemap('typemapPas.dat',typeMap);
  AssignFile(f,filename);
  AssignFile(constsF,'..'+PathDelim+'consts.inc');
  AssignFile(funDeclF,'..'+PathDelim+'funDecl.inc');
  AssignFile(funBodiesF,'..'+PathDelim+'funBodies.inc');
  Reset(f);
  Rewrite(constsF);
  Rewrite(funDeclF);
  Rewrite(funBodiesF);
  HeaderAutoFile(constsF);
  writeln(constsF, 'const');
  lexer:=TMiniLexer.Create;
  partLexers:=false;
  paramTypes:=TStringList.Create;
  paramNames:=TStringList.Create;
  while not eof(f) do
  begin
    readln(f, line);
    if line='' then continue;
    if (Length(line)>=2)and(line[1]='#')and(line[2]='#') then continue;
    if line='cat Provisional' then break;
    if line='# For SciLexer.h' then
    begin
      partLexers:=true;
      AssignFile(lexUnitF,'..'+PathDelim+'LexConsts.pas');
      Rewrite(lexUnitF);
      HeaderAutoFile(lexUnitF);
      writeln(lexUnitF, 'unit LexConsts;');
      writeln(lexUnitF);
      writeln(lexUnitF,'interface');
      writeln(lexUnitF);
      writeln(lexUnitF, 'const');
    end
    else if line='# Events' then
    begin
      writeln(lexUnitF);
      writeln(lexUnitF, 'implementation');
      writeln(lexUnitF);
      writeln(lexUnitF, 'end.');
      CloseFile(lexUnitF);
      partLexers:=false;
    end;
    lexer.LoadText(line);
    key:=lexer.NextToken;
    if key='val' then
    begin
      name:=lexer.NextToken;
      lexer.NextToken; //=
      num:=Hex2Pas(lexer.NextToken);
      if partlexers then
        writeln(lexUnitF, '  ',name,' = ',num,';')
      else
        writeln(constsF, '  ',name,' = ',num,';');
    end
    else if (key='fun')or(key='get')or(key='set') then
    begin
      funtype:=lexer.NextToken;
      if funtype='void' then
        declStr:='procedure '
      else
        declStr:='function ';
      name:=lexer.NextToken;
      declStrB:=declStr;
      declStr:=declStr+name+'(';
      declStrB:=declStrB+'TScintilla.'+name+'(';

      lexer.NextToken; //=
      num:=Hex2Pas(lexer.NextToken);
      writeln(constsF, '  SCI_',name,' = ',num,';');
      otherStr:=lexer.NextToken;
      Assert(otherStr='(');
      msgCnt:=0;
      paramCnt:=0;
      bStringResult:=false;
      paramTypes.Clear;
      paramNames.Clear;
      subDeclStr:='';
      repeat
        otherStr:=lexer.NextToken;
        inc(msgCnt);
        if otherStr=',' then
        begin
          paramTypes.Add('');
          paramNames.Add('');
          continue;
        end;
        if otherStr=')' then break;
        paramTypes.Add(otherStr);
        otherStr:=lexer.NextToken;
        Assert(otherStr<>',');
        Assert(otherStr<>')');
        if (otherStr='set')or(otherStr='type')or(otherStr='end') then
          paramNames.Add(otherStr+'_')
        else
          paramNames.Add(otherStr);
        inc(paramCnt);

        if paramCnt>1 then subDeclStr:=subDeclStr+'; ';
        if paramTypes[msgCnt-1]='stringresult' then
        begin
          bStringResult:=true;
          Assert(msgCnt=2);
        end;
        subDeclStr:=subDeclStr+paramNames[msgCnt-1]+': '+typeMap.Get(paramTypes[msgCnt-1]).value;
        otherStr:=lexer.NextToken;
        Assert((otherStr=',')or(otherStr=')'));
      until otherStr=')';
      subDeclStr:=subDeclStr+')';
      if funtype<>'void' then
        subDeclStr:=subDeclStr+': '+typeMap.Get(funtype).value;
      subDeclStr:=subDeclStr+';';
      declStr:=declStr+subDeclStr;
      declStrB:=declStrB+subDeclStr;
      if bStringResult then
        writeln(funDeclF, declStr,' overload;')
      else
        writeln(funDeclF, declStr);
      writeln(funBodiesF, declStrB);
      writeln(funBodiesF, 'begin');
      write(funBodiesF, '  SendEditor(SCI_',name);
      Assert(paramTypes.Count=paramNames.Count);
      if paramCnt>0 then
      for i:=0 to paramNames.Count-1 do
      begin
        write(funBodiesF, ',');
        if paramNames[i]='' then
        begin
          Assert(i=0);
          write(funBodiesF, '0');
        end
        else if paramTypes[i]='int' then write(funBodiesF, paramNames[i])
        else write(funBodiesF, 'Integer(',paramNames[i],')');
      end;
      writeln(funBodiesF, ');');
      writeln(funBodiesF, 'end;');
      if bStringResult then
      begin
        declStr:='function '+name+'(';
        declStrB:='function TScintilla.'+name+'(';
        subDeclStr:='';
        bSep:=false;
        for i:=0 to paramNames.Count-1 do
        begin
          if paramTypes[i]='' then continue;
          if paramTypes[i]='stringresult' then continue;
          if (paramTypes[i]='int')and(paramNames[i]='length') then continue;
          if bSep then subDeclStr:=subDeclStr+'; ';
          subDeclStr:=subDeclStr+paramNames[i]+': '+typeMap.Get(paramTypes[i]).value;
          bSep:=true;
        end;
        subDeclStr:=subDeclStr+'): AnsiString;';
        declStr:=declStr+subDeclStr;
        declStrB:=declStrB+subDeclStr;
        writeln(funDeclF, declStr,' overload;');
        writeln(funBodiesF, declStrB);
        writeln(funBodiesF, 'var');
        writeln(funBodiesF, '  len: integer;');
        writeln(funBodiesF, 'begin');
        write(funBodiesF, '  len:=',name,'(');
        bSep:=false;
        for i:=0 to paramNames.Count-1 do
        begin
          if paramTypes[i]='' then continue;
          if bSep then write(funBodiesF, ',');
          if paramTypes[i]='stringresult' then write(funBodiesF, 'nil')
          else if (paramTypes[i]='int')and(paramNames[i]='length') then
            write(funBodiesF, '0')
          else write(funBodiesF, paramNames[i]);
          bSep:=true;
        end;
        writeln(funBodiesF, ');');
        if srz.ifReturnsZ(name) then
        begin
          writeln(funBodiesF, '  if len<=0 then raise Exception.Create(''',name,' returns 0'');');
          writeln(funBodiesF, '  if len=1 then');
        end else writeln(funBodiesF, '  if len<=0 then');
        writeln(funBodiesF, '  begin');
        writeln(funBodiesF, '    result:='''';');
        writeln(funBodiesF, '    exit;');
        writeln(funBodiesF, '  end;');
        if srz.ifReturnsZ(name) then
          writeln(funBodiesF, '  SetLength(result, len-1);')
        else
          writeln(funBodiesF, '  SetLength(result, len);');
        write(funBodiesF, '  ',name,'(');
        bSep:=false;
        for i:=0 to paramNames.Count-1 do
        begin
          if paramTypes[i]='' then continue;
          if bSep then write(funBodiesF, ',');
          if paramTypes[i]='stringresult' then write(funBodiesF, 'PAnsiChar(result)')
          else if (paramTypes[i]='int')and(paramNames[i]='length') then
            write(funBodiesF, 'len')
          else write(funBodiesF, paramNames[i]);
          bSep:=true;
        end;
        write(funBodiesF, ');');
        if srz.ifReturnsZ(name) then
             writeln(funBodiesF, ' //last byte is #0')
        else writeln(funBodiesF);
        writeln(funBodiesF, 'end;');
      end;
    end else if key='evt' then
    begin
      lexer.NextToken; //type
      name:=lexer.NextToken;
      lexer.NextToken; //=
      num:=Hex2Pas(lexer.NextToken);
      writeln(constsF, '  SCN_',name,' = ',num,';');
    end;
  end;
  paramTypes.Free;
  paramNames.Free;
  lexer.Free;
  typeMap.Free;
  CloseFile(constsF);
  CloseFile(funDeclF);
  CloseFile(funBodiesF);
  CloseFile(f);
end;

{ TStringResultZ }

constructor TStringResultZ.Create;
var
  f: TextFile;
  line,funName: string;
  lexer: TMiniLexer;
begin
  AssignFile(f, 'stringresult.dat');
  Reset(f);
  listZ:=TStringList.Create;
  lexer:=TMiniLexer.Create;
  while not eof(f) do
  begin
    readln(f, line);
    line:=Trim(line);
    if line='' then continue;
    lexer.LoadText(line);
    funName:=lexer.NextToken;
    if lexer.NextToken='zero' then listZ.Add(funName);
  end;
  lexer.Free;
  CloseFile(f);
end;

destructor TStringResultZ.Destroy;
begin
  listZ.Free;
  inherited;
end;

function TStringResultZ.ifReturnsZ(funName: string): boolean;
begin
  result:=listZ.IndexOf(funName)>=0;
end;

begin
  srz:=TStringResultZ.Create;
  //createH('Scintilla.iface');
  createPas('Scintilla.iface');
  //dump_types;
  srz.Free;
end.

