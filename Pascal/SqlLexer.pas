unit SqlLexer;

interface

uses
  Graphics, Scintilla;

type
  TSqlLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation

uses
  LexConsts;

const
  sampleLines: AnsiString =
'set echo on'#10+
#10+
'COLUMN column_name format a26'#10+
'COLUMN low_val format a20'#10+
'COLUMN high_val format a20'#10+
'COLUMN data_type format a20'#10+
#10+
'alter session set nls_date_format=''DD-MON-YYYY HH24:MI:SS'';'#10+
#10+
'PROMPT ****************************************************'#10+
'PROMPT First build the table and index and INSERT some data'#10+
'PROMPT ****************************************************'#10+
#10+
'CREATE TABLE TEST_TAB1'#10+
'('#10+
'  REPORTING_DATE            NUMBER              NOT NULL,'#10+
'  SOURCE_SYSTEM             VARCHAR2(30 CHAR)   NOT NULL,'#10+
'  SEQ_ID                    NUMBER              NOT NULL,'#10+
'  STATUS                    VARCHAR2(1 CHAR)    NOT NULL'#10+
')';


//---"Standard" (ANSI SQL keywords (Version 1, 2 and 3) (www.sql.org)-----------
  sqlKeyWords: PAnsiChar =
    'absolute action active actor add after alias all allocate alter ' +
    'and any are as asc ascending assertion async at attributes auto ' +
    'base_name before begin between bit bit_length boolean both breadth by ' +
    'cache call cascade cascaded case cast catalog char_length ' +
    'character_length check coalesce collate collation column commit ' +
    'committed completion computed conditional connect connection constraint ' +
    'constraints containing convert corresponding count create cross current ' +
    'current_date current_path current_time current_timestamp current_user ' +
    'cursor cycle data database date day deallocate debug declare default ' +
    'deferrable deferred delete depth desc descending describe descriptor ' +
    'destroy diagnostics dictionary disconnect distinct do domain ' +
    'drop each element else elseif end end-exec entry_point equals escape ' +
    'except exception execute exists exit external extract factor false ' +
    'filter first for foreign from full function general generator get ' +
    'global grant group having hold hour identity if ignore immediate in ' +
    'inactive index initially inner input insensitive insert instead ' +
    'intersect interval into is isolation join key last leading leave left ' +
    'less level like limit list local loop lower match merge minute modify ' +
    'month names national natural nchar new new_table next no none not null ' +
    'nullif object octet_length of off old old_table on only operation ' +
    'operator operators or order others outer output overlaps pad ' +
    'parameter parameters partial password path pendant plan position ' +
    'postfix prefix preorder prepare preserve primary prior private ' +
    'privileges procedure protected read recursive ref referencing relative ' +
    'replace resignal restrict retain return returns revoke right role ' +
    'rollback routine row rows savepoint schema scroll search second select ' +
    'sensitive sequence session session_user set shadow shared signal ' +
    'similar size snapshot some space sqlexception sqlstate sqlwarning start ' +
    'state structure substring suspend symbol system_user table temporary ' +
    'term test then there time timestamp timezone_hour timezone_minute to ' +
    'trailing transaction translate translation trigger trim true tuple type ' +
    'uncommitted under union unique unknown update upper usage user using ' +
    'value varchar variable varying view virtual visible wait when where ' +
    'while with without work write year zone';



function TSqlLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TSqlLexer.InitDefaults;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_SQL);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);
  FOwner.SendEditor(SCI_SETKEYWORDS, 0, integer(sqlKeyWords));
  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others
  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_SQL_WORD, 1);
  SetAStyle(SCE_SQL_STRING, clFuchsia);
  SetAStyle(SCE_SQL_NUMBER, TColor($800000));
  SetAStyle(SCE_SQL_SQLPLUS_PROMPT, TColor($000080));
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.sql')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
